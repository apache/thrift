// Licensed to the Apache Software Foundation(ASF) under one
// or more contributor license agreements.See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership.The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.

using System;
using System.Buffers;
using System.Buffers.Binary;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Thrift.Protocol.Entities;
using Thrift.Transport;

namespace Thrift.Protocol
{
    //TODO: implementation of TProtocol

    // ReSharper disable once InconsistentNaming
    public class TCompactProtocol : TProtocol
    {
        private const byte ProtocolId = 0x82;
        private const byte Version = 1;
        private const byte VersionMask = 0x1f; // 0001 1111
        private const byte TypeMask = 0xE0; // 1110 0000
        private const byte TypeBits = 0x07; // 0000 0111
        private const int TypeShiftAmount = 5;

        private const byte NoTypeOverride = 0xFF;

        // ReSharper disable once InconsistentNaming
        private static readonly byte[] TTypeToCompactType = new byte[16];
        private static readonly TType[] CompactTypeToTType = new TType[13];

        /// <summary>
        ///     Used to keep track of the last field for the current and previous structs, so we can do the delta stuff.
        /// </summary>
        private readonly Stack<short> _lastField = new Stack<short>(15);

        /// <summary>
        ///     If we encounter a boolean field begin, save the TField here so it can have the value incorporated.
        /// </summary>
        private TField? _booleanField;

        /// <summary>
        ///     If we Read a field header, and it's a boolean field, save the boolean value here so that ReadBool can use it.
        /// </summary>
        private bool? _boolValue;

        private short _lastFieldId;

        // minimize memory allocations by means of an preallocated bytes buffer
        // The value of 128 is arbitrarily chosen, the required minimum size must be sizeof(long)
        private readonly byte[] PreAllocatedBuffer = new byte[128]; 

        private struct VarInt
        {
            public byte[] bytes;
            public int count;
        }

        // minimize memory allocations by means of an preallocated VarInt buffer
        private VarInt PreAllocatedVarInt = new VarInt()
        {
            bytes = new byte[10], // see Int64ToVarInt()
            count = 0
        };




        public TCompactProtocol(TTransport trans)
            : base(trans)
        {
            TTypeToCompactType[(int) TType.Stop] = Types.Stop;
            TTypeToCompactType[(int) TType.Bool] = Types.BooleanTrue;
            TTypeToCompactType[(int) TType.Byte] = Types.Byte;
            TTypeToCompactType[(int) TType.I16] = Types.I16;
            TTypeToCompactType[(int) TType.I32] = Types.I32;
            TTypeToCompactType[(int) TType.I64] = Types.I64;
            TTypeToCompactType[(int) TType.Double] = Types.Double;
            TTypeToCompactType[(int) TType.String] = Types.Binary;
            TTypeToCompactType[(int) TType.List] = Types.List;
            TTypeToCompactType[(int) TType.Set] = Types.Set;
            TTypeToCompactType[(int) TType.Map] = Types.Map;
            TTypeToCompactType[(int) TType.Struct] = Types.Struct;

            CompactTypeToTType[Types.Stop] = TType.Stop;
            CompactTypeToTType[Types.BooleanTrue] = TType.Bool;
            CompactTypeToTType[Types.BooleanFalse] = TType.Bool;
            CompactTypeToTType[Types.Byte] = TType.Byte;
            CompactTypeToTType[Types.I16] = TType.I16;
            CompactTypeToTType[Types.I32] = TType.I32;
            CompactTypeToTType[Types.I64] = TType.I64;
            CompactTypeToTType[Types.Double] = TType.Double;
            CompactTypeToTType[Types.Binary] = TType.String;
            CompactTypeToTType[Types.List] = TType.List;
            CompactTypeToTType[Types.Set] = TType.Set;
            CompactTypeToTType[Types.Map] = TType.Map;
            CompactTypeToTType[Types.Struct] = TType.Struct;
        }

        public void Reset()
        {
            _lastField.Clear();
            _lastFieldId = 0;
        }

        public override async Task WriteMessageBeginAsync(TMessage message, CancellationToken cancellationToken)
        {
            PreAllocatedBuffer[0] = ProtocolId;
            PreAllocatedBuffer[1] = (byte)((Version & VersionMask) | (((uint)message.Type << TypeShiftAmount) & TypeMask));
            await Trans.WriteAsync(PreAllocatedBuffer, 0, 2, cancellationToken);

            Int32ToVarInt((uint) message.SeqID, ref PreAllocatedVarInt);
            await Trans.WriteAsync(PreAllocatedVarInt.bytes, 0, PreAllocatedVarInt.count, cancellationToken);

            await WriteStringAsync(message.Name, cancellationToken);
        }

        public override Task WriteMessageEndAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            return Task.CompletedTask;
        }

        /// <summary>
        ///     Write a struct begin. This doesn't actually put anything on the wire. We
        ///     use it as an opportunity to put special placeholder markers on the field
        ///     stack so we can get the field id deltas correct.
        /// </summary>
        public override Task WriteStructBeginAsync(TStruct @struct, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            _lastField.Push(_lastFieldId);
            _lastFieldId = 0;

            return Task.CompletedTask;
        }

        public override Task WriteStructEndAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            _lastFieldId = _lastField.Pop();

            return Task.CompletedTask;
        }

        private async Task WriteFieldBeginInternalAsync(TField field, byte fieldType, CancellationToken cancellationToken)
        {
            // if there's a exType override passed in, use that. Otherwise ask GetCompactType().
            if (fieldType == NoTypeOverride)
                fieldType = GetCompactType(field.Type);


            // check if we can use delta encoding for the field id
            if (field.ID > _lastFieldId)
            {
                var delta = field.ID - _lastFieldId;
                if (delta <= 15)
                {
                    // Write them together
                    PreAllocatedBuffer[0] = (byte)((delta << 4) | fieldType);
                    await Trans.WriteAsync(PreAllocatedBuffer, 0, 1, cancellationToken);
                    _lastFieldId = field.ID;
                    return;
                }
            }

            // Write them separate
            PreAllocatedBuffer[0] = fieldType;
            await Trans.WriteAsync(PreAllocatedBuffer, 0, 1, cancellationToken);
            await WriteI16Async(field.ID, cancellationToken);
            _lastFieldId = field.ID;
        }

        public override async Task WriteFieldBeginAsync(TField field, CancellationToken cancellationToken)
        {
            if (field.Type == TType.Bool)
            {
                _booleanField = field;
            }
            else
            {
                await WriteFieldBeginInternalAsync(field, NoTypeOverride, cancellationToken);
            }
        }

        public override Task WriteFieldEndAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            return Task.CompletedTask;
        }

        public override async Task WriteFieldStopAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            PreAllocatedBuffer[0] = Types.Stop;
            await Trans.WriteAsync(PreAllocatedBuffer, 0, 1, cancellationToken);
        }

        protected async Task WriteCollectionBeginAsync(TType elemType, int size, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            /*
            Abstract method for writing the start of lists and sets. List and sets on
             the wire differ only by the exType indicator.
            */

            if (size <= 14)
            {
                PreAllocatedBuffer[0] = (byte)((size << 4) | GetCompactType(elemType));
                await Trans.WriteAsync(PreAllocatedBuffer, 0, 1, cancellationToken);
            }
            else
            {
                PreAllocatedBuffer[0] = (byte)(0xf0 | GetCompactType(elemType));
                await Trans.WriteAsync(PreAllocatedBuffer, 0, 1, cancellationToken);

                Int32ToVarInt((uint) size, ref PreAllocatedVarInt);
                await Trans.WriteAsync(PreAllocatedVarInt.bytes, 0, PreAllocatedVarInt.count, cancellationToken);
            }
        }

        public override async Task WriteListBeginAsync(TList list, CancellationToken cancellationToken)
        {
            await WriteCollectionBeginAsync(list.ElementType, list.Count, cancellationToken);
        }

        public override Task WriteListEndAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            return Task.CompletedTask;
        }

        public override async Task WriteSetBeginAsync(TSet set, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            await WriteCollectionBeginAsync(set.ElementType, set.Count, cancellationToken);
        }

        public override Task WriteSetEndAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            return Task.CompletedTask;
        }

        public override async Task WriteBoolAsync(bool b, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            /*
            Write a boolean value. Potentially, this could be a boolean field, in
            which case the field header info isn't written yet. If so, decide what the
            right exType header is for the value and then Write the field header.
            Otherwise, Write a single byte.
            */

            if (_booleanField != null)
            {
                // we haven't written the field header yet
                var type = b ? Types.BooleanTrue : Types.BooleanFalse;
                await WriteFieldBeginInternalAsync(_booleanField.Value, type, cancellationToken);
                _booleanField = null;
            }
            else
            {
                // we're not part of a field, so just write the value.
                PreAllocatedBuffer[0] = b ? Types.BooleanTrue : Types.BooleanFalse;
                await Trans.WriteAsync(PreAllocatedBuffer, 0, 1, cancellationToken);
            }
        }

        public override async Task WriteByteAsync(sbyte b, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            PreAllocatedBuffer[0] = (byte)b;
            await Trans.WriteAsync(PreAllocatedBuffer, 0, 1, cancellationToken);
        }

        public override async Task WriteI16Async(short i16, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            Int32ToVarInt(IntToZigzag(i16), ref PreAllocatedVarInt);
            await Trans.WriteAsync(PreAllocatedVarInt.bytes, 0, PreAllocatedVarInt.count, cancellationToken);
        }

        private static void Int32ToVarInt(uint n, ref VarInt varint)
        {
            // Write an i32 as a varint. Results in 1 - 5 bytes on the wire.
            varint.count = 0;
            Debug.Assert(varint.bytes.Length >= 5);

            while (true)
            {
                if ((n & ~0x7F) == 0)
                {
                    varint.bytes[varint.count++] = (byte)n;
                    break;
                }

                varint.bytes[varint.count++] = (byte)((n & 0x7F) | 0x80);
                n >>= 7;
            }
        }

        public override async Task WriteI32Async(int i32, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            Int32ToVarInt(IntToZigzag(i32), ref PreAllocatedVarInt);
            await Trans.WriteAsync(PreAllocatedVarInt.bytes, 0, PreAllocatedVarInt.count, cancellationToken);
        }

        static private void Int64ToVarInt(ulong n, ref VarInt varint)
        {
            // Write an i64 as a varint. Results in 1-10 bytes on the wire.
            varint.count = 0;
            Debug.Assert(varint.bytes.Length >= 10);

            while (true)
            {
                if ((n & ~(ulong)0x7FL) == 0)
                {
                    varint.bytes[varint.count++] = (byte)n;
                    break;
                }
                varint.bytes[varint.count++] = (byte)((n & 0x7F) | 0x80);
                n >>= 7;
            }
        }

        public override async Task WriteI64Async(long i64, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            Int64ToVarInt(LongToZigzag(i64), ref PreAllocatedVarInt);
            await Trans.WriteAsync(PreAllocatedVarInt.bytes, 0, PreAllocatedVarInt.count, cancellationToken);
        }

        public override async Task WriteDoubleAsync(double d, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            BinaryPrimitives.WriteInt64LittleEndian(PreAllocatedBuffer, BitConverter.DoubleToInt64Bits(d));
            await Trans.WriteAsync(PreAllocatedBuffer, 0, 8, cancellationToken);
        }

        public override async Task WriteStringAsync(string str, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            var buf = ArrayPool<byte>.Shared.Rent(Encoding.UTF8.GetByteCount(str));
            try
            {
                var numberOfBytes = Encoding.UTF8.GetBytes(str, 0, str.Length, buf, 0);

                Int32ToVarInt((uint)numberOfBytes, ref PreAllocatedVarInt);
                await Trans.WriteAsync(PreAllocatedVarInt.bytes, 0, PreAllocatedVarInt.count, cancellationToken);
                await Trans.WriteAsync(buf, 0, numberOfBytes, cancellationToken);
            }
            finally
            {
                ArrayPool<byte>.Shared.Return(buf);
            }
        }

        public override async Task WriteBinaryAsync(byte[] bytes, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            Int32ToVarInt((uint) bytes.Length, ref PreAllocatedVarInt);
            await Trans.WriteAsync(PreAllocatedVarInt.bytes, 0, PreAllocatedVarInt.count, cancellationToken);
            await Trans.WriteAsync(bytes, 0, bytes.Length, cancellationToken);
        }

        public override async Task WriteMapBeginAsync(TMap map, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            if (map.Count == 0)
            {
                PreAllocatedBuffer[0] = 0;
                await Trans.WriteAsync( PreAllocatedBuffer, 0, 1, cancellationToken);
            }
            else
            {
                Int32ToVarInt((uint) map.Count, ref PreAllocatedVarInt);
                await Trans.WriteAsync(PreAllocatedVarInt.bytes, 0, PreAllocatedVarInt.count, cancellationToken);

                PreAllocatedBuffer[0] = (byte)((GetCompactType(map.KeyType) << 4) | GetCompactType(map.ValueType));
                await Trans.WriteAsync(PreAllocatedBuffer, 0, 1, cancellationToken);
            }
        }

        public override Task WriteMapEndAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            return Task.CompletedTask;
        }

        public override async ValueTask<TMessage> ReadMessageBeginAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            var protocolId = (byte) await ReadByteAsync(cancellationToken);
            if (protocolId != ProtocolId)
            {
                throw new TProtocolException($"Expected protocol id {ProtocolId:X} but got {protocolId:X}");
            }

            var versionAndType = (byte) await ReadByteAsync(cancellationToken);
            var version = (byte) (versionAndType & VersionMask);

            if (version != Version)
            {
                throw new TProtocolException($"Expected version {Version} but got {version}");
            }

            var type = (byte) ((versionAndType >> TypeShiftAmount) & TypeBits);
            var seqid = (int) await ReadVarInt32Async(cancellationToken);
            var messageName = await ReadStringAsync(cancellationToken);

            return new TMessage(messageName, (TMessageType) type, seqid);
        }

        public override Task ReadMessageEndAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            return Task.CompletedTask;
        }

        public override ValueTask<TStruct> ReadStructBeginAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            _lastField.Push(_lastFieldId);
            _lastFieldId = 0;

            return new ValueTask<TStruct>(AnonymousStruct);
        }

        public override Task ReadStructEndAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            /*
            Doesn't actually consume any wire data, just removes the last field for
            this struct from the field stack.
            */

            // consume the last field we Read off the wire.
            _lastFieldId = _lastField.Pop();

            return Task.CompletedTask;
        }

        public override async ValueTask<TField> ReadFieldBeginAsync(CancellationToken cancellationToken)
        {
            // Read a field header off the wire.
            var type = (byte) await ReadByteAsync(cancellationToken);

            // if it's a stop, then we can return immediately, as the struct is over.
            if (type == Types.Stop)
            {
                return StopField;
            }


            // mask off the 4 MSB of the exType header. it could contain a field id delta.
            var modifier = (short) ((type & 0xf0) >> 4);
            var compactType = (byte)(type & 0x0f);

            short fieldId;
            if (modifier == 0)
            {
                fieldId = await ReadI16Async(cancellationToken);
            }
            else
            {
                fieldId = (short) (_lastFieldId + modifier);
            }

            var ttype = GetTType(compactType);
            var field = new TField(string.Empty, ttype, fieldId);

            // if this happens to be a boolean field, the value is encoded in the exType
            if( ttype == TType.Bool)
            {
                _boolValue = (compactType == Types.BooleanTrue);
            }

            // push the new field onto the field stack so we can keep the deltas going.
            _lastFieldId = field.ID;
            return field;
        }

        public override Task ReadFieldEndAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            return Task.CompletedTask;
        }

        public override async ValueTask<TMap> ReadMapBeginAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            /*
            Read a map header off the wire. If the size is zero, skip Reading the key
            and value exType. This means that 0-length maps will yield TMaps without the
            "correct" types.
            */

            var size = (int) await ReadVarInt32Async(cancellationToken);
            var keyAndValueType = size == 0 ? (byte) 0 : (byte) await ReadByteAsync(cancellationToken);
            var map = new TMap(GetTType((byte) (keyAndValueType >> 4)), GetTType((byte) (keyAndValueType & 0xf)), size);
            CheckReadBytesAvailable(map);
            return map;
        }

        public override Task ReadMapEndAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            return Task.CompletedTask;
        }

        public override async ValueTask<TSet> ReadSetBeginAsync(CancellationToken cancellationToken)
        {
            /*
            Read a set header off the wire. If the set size is 0-14, the size will
            be packed into the element exType header. If it's a longer set, the 4 MSB
            of the element exType header will be 0xF, and a varint will follow with the
            true size.
            */

            return new TSet(await ReadListBeginAsync(cancellationToken));
        }

        public override ValueTask<bool> ReadBoolAsync(CancellationToken cancellationToken)
        {
            /*
            Read a boolean off the wire. If this is a boolean field, the value should
            already have been Read during ReadFieldBegin, so we'll just consume the
            pre-stored value. Otherwise, Read a byte.
            */

            if (_boolValue != null)
            {
                var result = _boolValue.Value;
                _boolValue = null;
                return new ValueTask<bool>(result);
            }

            return InternalCall();

            async ValueTask<bool> InternalCall()
            {
                var data = await ReadByteAsync(cancellationToken);
                return (data == Types.BooleanTrue);
            }
        }


        public override async ValueTask<sbyte> ReadByteAsync(CancellationToken cancellationToken)
        {
            // Read a single byte off the wire. Nothing interesting here.
            await Trans.ReadAllAsync(PreAllocatedBuffer, 0, 1, cancellationToken);
            return (sbyte)PreAllocatedBuffer[0];
        }

        public override async ValueTask<short> ReadI16Async(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            return (short) ZigzagToInt(await ReadVarInt32Async(cancellationToken));
        }

        public override async ValueTask<int> ReadI32Async(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            return ZigzagToInt(await ReadVarInt32Async(cancellationToken));
        }

        public override async ValueTask<long> ReadI64Async(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            return ZigzagToLong(await ReadVarInt64Async(cancellationToken));
        }

        public override async ValueTask<double> ReadDoubleAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            await Trans.ReadAllAsync(PreAllocatedBuffer, 0, 8, cancellationToken);
            
            return BitConverter.Int64BitsToDouble(BinaryPrimitives.ReadInt64LittleEndian(PreAllocatedBuffer));
        }

        public override async ValueTask<string> ReadStringAsync(CancellationToken cancellationToken)
        {
            // read length
            var length = (int) await ReadVarInt32Async(cancellationToken);
            if (length == 0)
            {
                return string.Empty;
            }

            // read and decode data
            if (length < PreAllocatedBuffer.Length)
            {
                await Trans.ReadAllAsync(PreAllocatedBuffer, 0, length, cancellationToken);
                return Encoding.UTF8.GetString(PreAllocatedBuffer, 0, length);
            }

            Transport.CheckReadBytesAvailable(length);

            var buf = ArrayPool<byte>.Shared.Rent(length);
            try
            {
                await Trans.ReadAllAsync(buf, 0, length, cancellationToken);
                return Encoding.UTF8.GetString(buf, 0, length);
            }
            finally
            {
                ArrayPool<byte>.Shared.Return(buf);
            }
        }

        public override async ValueTask<byte[]> ReadBinaryAsync(CancellationToken cancellationToken)
        {
            // read length
            var length = (int) await ReadVarInt32Async(cancellationToken);
            if (length == 0)
            {
                return Array.Empty<byte>();
            }

            // read data
            Transport.CheckReadBytesAvailable(length);
            var buf = new byte[length];
            await Trans.ReadAllAsync(buf, 0, length, cancellationToken);
            return buf;
        }

        public override async ValueTask<TList> ReadListBeginAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            /*
            Read a list header off the wire. If the list size is 0-14, the size will
            be packed into the element exType header. If it's a longer list, the 4 MSB
            of the element exType header will be 0xF, and a varint will follow with the
            true size.
            */

            var sizeAndType = (byte) await ReadByteAsync(cancellationToken);
            var size = (sizeAndType >> 4) & 0x0f;
            if (size == 15)
            {
                size = (int) await ReadVarInt32Async(cancellationToken);
            }

            var type = GetTType(sizeAndType);
            var list = new TList(type, size);
            CheckReadBytesAvailable(list);
            return list;
        }

        public override Task ReadListEndAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            return Task.CompletedTask;
        }

        public override Task ReadSetEndAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            return Task.CompletedTask;
        }

        private static byte GetCompactType(TType ttype)
        {
            // Given a TType value, find the appropriate TCompactProtocol.Types constant.
            return TTypeToCompactType[(int) ttype];
        }


        private async ValueTask<uint> ReadVarInt32Async(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            /*
            Read an i32 from the wire as a varint. The MSB of each byte is set
            if there is another byte to follow. This can Read up to 5 bytes.
            */

            uint result = 0;
            var shift = 0;

            while (true)
            {
                var b = (byte) await ReadByteAsync(cancellationToken);
                result |= (uint) (b & 0x7f) << shift;
                if ((b & 0x80) != 0x80)
                {
                    break;
                }
                shift += 7;
            }

            return result;
        }

        private async ValueTask<ulong> ReadVarInt64Async(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            /*
            Read an i64 from the wire as a proper varint. The MSB of each byte is set
            if there is another byte to follow. This can Read up to 10 bytes.
            */

            var shift = 0;
            ulong result = 0;
            while (true)
            {
                var b = (byte) await ReadByteAsync(cancellationToken);
                result |= (ulong) (b & 0x7f) << shift;
                if ((b & 0x80) != 0x80)
                {
                    break;
                }
                shift += 7;
            }

            return result;
        }

        private static int ZigzagToInt(uint n)
        {
            return (int) (n >> 1) ^ -(int) (n & 1);
        }

        private static long ZigzagToLong(ulong n)
        {
            return (long) (n >> 1) ^ -(long) (n & 1);
        }

        private static TType GetTType(byte type)
        {
            // Given a TCompactProtocol.Types constant, convert it to its corresponding TType value.
            return CompactTypeToTType[type & 0x0f];
        }

        private static ulong LongToZigzag(long n)
        {
            // Convert l into a zigzag long. This allows negative numbers to be represented compactly as a varint
            return (ulong) (n << 1) ^ (ulong) (n >> 63);
        }

        private static uint IntToZigzag(int n)
        {
            // Convert n into a zigzag int. This allows negative numbers to be represented compactly as a varint
            return (uint) (n << 1) ^ (uint) (n >> 31);
        }

        // Return the minimum number of bytes a type will consume on the wire
        public override int GetMinSerializedSize(TType type)
        {
            switch (type)
            {
                case TType.Stop:    return 0;
                case TType.Void:    return 0;
                case TType.Bool:   return sizeof(byte);
                case TType.Double: return 8;  // uses fixedLongToBytes() which always writes 8 bytes
                case TType.Byte: return sizeof(byte);
                case TType.I16:     return sizeof(byte);  // zigzag
                case TType.I32:     return sizeof(byte);  // zigzag
                case TType.I64:     return sizeof(byte);  // zigzag
                case TType.String: return sizeof(byte);  // string length
                case TType.Struct:  return 0;             // empty struct
                case TType.Map:     return sizeof(byte);  // element count
                case TType.Set:    return sizeof(byte);  // element count
                case TType.List:    return sizeof(byte);  // element count
                default: throw new TTransportException(TTransportException.ExceptionType.Unknown, "unrecognized type code");
            }
        }

        public class Factory : TProtocolFactory
        {
            public override TProtocol GetProtocol(TTransport trans)
            {
                return new TCompactProtocol(trans);
            }
        }

        /// <summary>
        ///     All of the on-wire exType codes.
        /// </summary>
        private static class Types
        {
            public const byte Stop = 0x00;
            public const byte BooleanTrue = 0x01;
            public const byte BooleanFalse = 0x02;
            public const byte Byte = 0x03;
            public const byte I16 = 0x04;
            public const byte I32 = 0x05;
            public const byte I64 = 0x06;
            public const byte Double = 0x07;
            public const byte Binary = 0x08;
            public const byte List = 0x09;
            public const byte Set = 0x0A;
            public const byte Map = 0x0B;
            public const byte Struct = 0x0C;
        }
    }
}
