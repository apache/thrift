/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.thrift.transport;

import org.apache.thrift.TApplicationException;
import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TCompactProtocol;
import org.apache.thrift.protocol.TMessage;
import org.apache.thrift.protocol.TMessageType;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.zip.DataFormatException;
import java.util.zip.Deflater;
import java.util.zip.Inflater;
//import org.iq80.snappy.CorruptionException;
//import org.iq80.snappy.Snappy;

public class THeaderTransport extends TFramedTransport {
    public static final int HEADER_MAGIC_MASK = 0xFFFF0000;
    public static final int HEADER_FLAGS_MASK = 0x0000FFFF;

    // 16th and 32nd bits must be 0 to differentiate framed vs unframed.
    public static final int HEADER_MAGIC = 0x0FFF0000;

    // HTTP has different magic
    public static final int HTTP_SERVER_MAGIC = 0x504F5354; // 'POST'

    // Note max frame size is slightly less than HTTP_SERVER_MAGIC
    public static final int MAX_FRAME_SIZE = 0x3FFFFFFF;

    private int zlibBufferSize = 512;

    // Transforms
    public enum Transforms {
        ZLIB_TRANSFORM(0x01),
        HMAC_TRANSFORM(0x02), // Deprecated and no longer supported
        SNAPPY_TRANSFORM(0x03);

        private int value;

        private Transforms(int value) {
            this.value = value;
        }

        public int getValue() {
            return value;
        }
    }

    // Infos
    public enum Infos {
        INFO_KEYVALUE(0x01),
        INFO_PKEYVALUE(0x02);

        private int value;

        private Infos(int value) {
            this.value = value;
        }

        public int getValue() {
            return value;
        }
    }

    // Client types
    public enum ClientTypes {
        HEADERS(0),
        FRAMED_DEPRECATED(1),
        UNFRAMED_DEPRECATED(2),
        HTTP(3),
        UNKNOWN(4);

        private int value;

        private ClientTypes(int value) {
            this.value = value;
        }

        public int getValue() {
            return value;
        }
    }

    public static final int T_BINARY_PROTOCOL = 0;
    public static final int T_JSON_PROTOCOL = 1;
    public static final int T_COMPACT_PROTOCOL = 2;

    private static final int numClientTypes = 4;

    private int protoId = T_COMPACT_PROTOCOL; // default
    private ClientTypes clientType = ClientTypes.HEADERS;
    private int seqId = 0;
    private int flags = 0;

    private final Boolean[] supportedClients;

    private final List<Transforms> writeTransforms;
    private List<Integer> readTransforms;

    private final HashMap<String, String> readHeaders;
    private final HashMap<String, String> readPersistentHeaders;
    private final HashMap<String, String> writeHeaders;
    private final HashMap<String, String> writePersistentHeaders;

    private static final String IDENTITY_HEADER = "identity";
    private static final String ID_VERSION_HEADER = "id_version";
    private static final String ID_VERSION = "1";

    private String identity;

    public THeaderTransport(TTransport transport) {
        super(transport);
        writeTransforms = new ArrayList<Transforms>();

        // Always supported headers
        supportedClients = new Boolean[numClientTypes];
        supportedClients[ClientTypes.HEADERS.getValue()] = true;
        writeHeaders = new HashMap<String, String>();
        writePersistentHeaders = new HashMap<String, String>();
        readHeaders = new HashMap<String, String>();
        readPersistentHeaders = new HashMap<String, String>();
    }

    public THeaderTransport(TTransport transport, List<ClientTypes> clientTypes) {
        this(transport);

        if (clientTypes != null) {
            for (ClientTypes t : clientTypes) {
                supportedClients[t.getValue()] = true;
            }
        }
    }

    public TTransport getUnderlyingTransport() {
        return this.transport_;
    }

    /**
     * Returns the protocol Id we are reading/writing
     *
     * @return protocol id
     */
    public int getProtocolId() {
        if (clientType == ClientTypes.HEADERS) {
            return protoId;
        } else {
            return 0; // Default to binary for all others.
        }
    }

    /**
     * Sets protocol Id we are writing May be updated on read.
     */
    public void setProtocolId(int protoId) {
        this.protoId = protoId;
    }

    /**
     * Sets the internal buffer size for zlib transform This will work with any value (except 0), but
     * this is provided as an optimization knob.
     *
     * @param sz Block size for decompress
     */
    public void setZlibBufferSize(int sz) {
        zlibBufferSize = sz;
    }

    /**
     * Add a transform to the write transforms list
     */
    public void addTransform(Transforms transform) {
        writeTransforms.add(transform);
    }

    public void setHeader(String key, String value) {
        writeHeaders.put(key, value);
    }

    public HashMap<String, String> getWriteHeaders() {
        return writeHeaders;
    }

    public void setPersistentHeader(String key, String value) {
        writePersistentHeaders.put(key, value);
    }

    public HashMap<String, String> getWritePersistentHeaders() {
        return writePersistentHeaders;
    }

    public HashMap<String, String> getReadPersistentHeaders() {
        return readPersistentHeaders;
    }

    public HashMap<String, String> getHeaders() {
        return readHeaders;
    }

    public void clearHeaders() {
        writeHeaders.clear();
    }

    public void clearPersistentHeaders() {
        writePersistentHeaders.clear();
    }

    public String getPeerIdentity() {
        if (readHeaders.containsKey(IDENTITY_HEADER)
                && ID_VERSION.equals(readHeaders.get(ID_VERSION_HEADER))) {
            return readHeaders.get(IDENTITY_HEADER);
        }

        return null;
    }

    public void setIdentity(String identity) {
        this.identity = identity;
    }

    @Override
    public int read(byte[] buf, int off, int len) throws TTransportException {
        if (readBuffer_ != null) {
            int got = readBuffer_.read(buf, off, len);
            if (got > 0) {
                return got;
            }
        }

        if (clientType == ClientTypes.UNFRAMED_DEPRECATED) {
            return transport_.read(buf, off, len);
        }

        // Read another frame of data
        readFrame(len);

        return readBuffer_.read(buf, off, len);
    }

    /**
     * Should be called from THeaderProtocol at the start of every message
     */
    public void _resetProtocol() throws TTransportException {
        // Set to anything except unframed
        clientType = ClientTypes.HEADERS;
        // Read the header bytes to check which protocol to use
        readFrame(0);
    }

    //@Override
    protected void readFrame() throws TTransportException {
        throw new TTransportException("You must use readFrame(int reqLen)");
    }

    /**
     * Reads another frame
     *
     * @param reqLen Try and read at least reqLen bytes
     */
    protected void readFrame(int reqLen) throws TTransportException {
        transport_.readAll(i32buf, 0, 4);
        int word1 = decodeWord(i32buf);

        if ((word1 & TBinaryProtocol.VERSION_MASK) == TBinaryProtocol.VERSION_1) {
            clientType = ClientTypes.UNFRAMED_DEPRECATED;
            if (reqLen <= 4) {
                readBuffer_.reset(i32buf);
            } else {
                byte[] buff = new byte[reqLen];
                System.arraycopy(i32buf, 0, buff, 0, 4);
                transport_.readAll(buff, 4, reqLen - 4);
                readBuffer_.reset(buff);
            }
        } else if (word1 == HTTP_SERVER_MAGIC) {
            throw new THeaderException("This transport does not support HTTP");
        } else {
            if (word1 - 4 > MAX_FRAME_SIZE) {
                // special case for the most common question in user-group
                // this will probably saves hours of engineering effort.
                int magic1 = 0x61702048; // ASCII "ap H" in little endian
                int magic2 = 0x6C6C6F63; // ASCII "lloc" in little endian
                if (word1 == magic1 || word1 == magic2) {
                    throw new TTransportException(
                            "The Thrift server received an ASCII request and safely ignored it. "
                                    + "In all likelihood, this isn't the reason of your problem "
                                    + "(probably a local daemon sending HTTP content to all listening ports).");
                }
                throw new TTransportException("Framed transport frame " + "is too large");
            }

            // Could be framed or header format.  Check next word.
            transport_.readAll(i32buf, 0, 4);
            int version = decodeWord(i32buf);
            if ((version & TBinaryProtocol.VERSION_MASK) == TBinaryProtocol.VERSION_1) {
                clientType = ClientTypes.FRAMED_DEPRECATED;
                byte[] buff = new byte[word1];
                System.arraycopy(i32buf, 0, buff, 0, 4);
                transport_.readAll(buff, 4, word1 - 4);
                readBuffer_.reset(buff);
            } else if ((version & HEADER_MAGIC_MASK) == HEADER_MAGIC) {
                clientType = ClientTypes.HEADERS;
                if (word1 - 4 < 10) {
                    throw new TTransportException("Header transport frame " + "is too small");
                }
                byte[] buff = new byte[word1];
                System.arraycopy(i32buf, 0, buff, 0, 4);

                // read packet minus version
                transport_.readAll(buff, 4, word1 - 4);
                flags = version & HEADER_FLAGS_MASK;
                // read seqId
                seqId = decodeWord(buff, 4);
                int headerSize = decodeShort(buff, 8);

                readHeaderFormat(headerSize, buff);
            } else {
                clientType = ClientTypes.UNKNOWN;
                throw new THeaderException("Unsupported client type");
            }
        }
    }

    // TODO(davejwatson) potential inclusion in a java util class

    /**
     * Reads a varint from the buffer. frame.data = buffer to use frame.idx = Offset to data in this
     * case, incremented by size of varint
     */
    private int readVarint32Buf(ByteBuffer frame) {
        int result = 0;
        int shift = 0;

        while (true) {
            byte b = frame.get();
            result |= (int) (b & 0x7f) << shift;
            if ((b & 0x80) != 0x80) {
                break;
            }
            shift += 7;
        }

        return result;
    }

    private void writeVarint(ByteBuffer out, int n) {
        while (true) {
            if ((n & ~0x7F) == 0) {
                out.put((byte) n);
                break;
            } else {
                out.put((byte) (n | 0x80));
                n >>>= 7;
            }
        }
    }

    private void writeString(ByteBuffer out, String str) {
        byte[] bytes = str.getBytes(StandardCharsets.UTF_8);
        writeVarint(out, bytes.length);
        out.put(ByteBuffer.wrap(bytes));
    }

    private String readString(ByteBuffer in) throws TTransportException {
        int sz = readVarint32Buf(in);
        byte[] bytearr = new byte[sz];
        in.get(bytearr, 0, sz);
        return new String(bytearr, 0, sz, StandardCharsets.UTF_8);
    }

    private void readHeaderFormat(int headerSize, byte[] buff) throws TTransportException {
        ByteBuffer frame = ByteBuffer.wrap(buff);
        frame.position(10); // Advance past version, flags, seqid

        headerSize = headerSize * 4;
        int endHeader = headerSize + frame.position();
        if (headerSize > frame.remaining()) {
            throw new TTransportException("Header size is larger than frame");
        }
        protoId = readVarint32Buf(frame);
        int numHeaders = readVarint32Buf(frame);

        // Clear out any previous transforms
        readTransforms = new ArrayList<Integer>(numHeaders);

        if (protoId == T_JSON_PROTOCOL && clientType != ClientTypes.HTTP) {
            throw new TTransportException("Trying to recv JSON encoding " + "over binary");
        }

        // Read in the headers.  Data for each varies. See
        // doc/HeaderFormat.txt
        int hmacSz = 0;
        for (int i = 0; i < numHeaders; i++) {
            int transId = readVarint32Buf(frame);
            if (transId == Transforms.ZLIB_TRANSFORM.getValue()) {
                readTransforms.add(transId);
//            } else if (transId == Transforms.SNAPPY_TRANSFORM.getValue()) {
//                readTransforms.add(transId);
            } else if (transId == Transforms.HMAC_TRANSFORM.getValue()) {
                throw new THeaderException("Hmac transform no longer supported");
            } else {
                throw new THeaderException("Unknown transform during recv");
            }
        }

        // Read the info section.
        readHeaders.clear();
        while (frame.position() < endHeader) {
            int infoId = readVarint32Buf(frame);
            if (infoId == Infos.INFO_KEYVALUE.getValue()) {
                int numKeys = readVarint32Buf(frame);
                for (int i = 0; i < numKeys; i++) {
                    String key = readString(frame);
                    String value = readString(frame);
                    readHeaders.put(key, value);
                }
            } else if (infoId == Infos.INFO_PKEYVALUE.getValue()) {
                int numKeys = readVarint32Buf(frame);
                for (int i = 0; i < numKeys; i++) {
                    String key = readString(frame);
                    String value = readString(frame);
                    readPersistentHeaders.put(key, value);
                }
            } else {
                // Unknown info ID, continue on to reading data.
                break;
            }
        }
        readHeaders.putAll(readPersistentHeaders);

        // Read in the data section.
        frame.position(endHeader);
        frame.limit(frame.limit() - hmacSz); // limit to data without mac

        frame = untransform(frame);
        readBuffer_.reset(frame.array(), frame.position(), frame.remaining());
        // adjust for 4 bytes sizeFiller_ in TFramedTransport
        readBuffer_.consumeBuffer(4);
    }

    private ByteBuffer untransform(ByteBuffer data) throws TTransportException {

        if (readTransforms.contains(Transforms.ZLIB_TRANSFORM.getValue())) {
            try {
                Inflater decompressor = new Inflater();
                decompressor.setInput(data.array(), data.position(), data.remaining());
                int length = 0;
                ArrayList<byte[]> outBytes = new ArrayList<byte[]>();
                while (!decompressor.finished()) {
                    byte[] output = new byte[zlibBufferSize];
                    length += decompressor.inflate(output);
                    outBytes.add(output);
                }
                decompressor.end();

                // Ugh output wants to be a list of blocks, we just want a buffer
                if (outBytes.size() == 1) {
                    data = ByteBuffer.wrap(outBytes.get(0));
                } else {
                    ByteBuffer output = ByteBuffer.allocate(length);
                    for (byte[] outBlock : outBytes) {
                        output.put(outBlock, 0, Math.min(zlibBufferSize, length));
                        length -= outBlock.length;
                    }
                    data = output;
                    data.position(0);
                }
            } catch (DataFormatException dfe) {
                throw new THeaderException("Could not inflate data");
            }
            if (!writeTransforms.contains(Transforms.ZLIB_TRANSFORM)) {
                writeTransforms.add(Transforms.ZLIB_TRANSFORM);
            }
//        } else if (readTransforms.contains(Transforms.SNAPPY_TRANSFORM.getValue())) {
//            try {
//                byte[] output = new byte[Snappy.getUncompressedLength(data.array(), data.position())];
//                int length = Snappy.uncompress(data.array(), data.position(), data.remaining(), output, 0);
//                data = ByteBuffer.wrap(output, 0, length);
//            } catch (CorruptionException e) {
//                throw new THeaderException(e);
//            }
//            if (!writeTransforms.contains(Transforms.SNAPPY_TRANSFORM)) {
//                writeTransforms.add(Transforms.SNAPPY_TRANSFORM);
//            }
        }
        return data;
    }

    private ByteBuffer transform(ByteBuffer data) throws TTransportException {

        if (writeTransforms.contains(Transforms.ZLIB_TRANSFORM)) {
            byte[] output = new byte[data.limit() + 512]; // output might be larger
            Deflater compressor = new Deflater();
            try {
                compressor.setInput(data.array(), data.position(), data.remaining());
                compressor.finish();
                int length = compressor.deflate(output);
                if (!compressor.finished()) {
                    // Output buffer was not big enough.  Unlikely.
                    // If you hit this, you probably shouldn't be using ZLIB_TRANSFORM :)
                    throw new TTransportException("Output compress buffer not big enough");
                }
                data = ByteBuffer.wrap(output);
                data.limit(length);
            } finally {
                compressor.end();
            }
//        } else if (writeTransforms.contains(Transforms.SNAPPY_TRANSFORM)) {
//            byte[] outputBuffer = new byte[Snappy.maxCompressedLength(data.limit())];
//            int length =
//                    Snappy.compress(data.array(), data.position(), data.remaining(), outputBuffer, 0);
//            data = ByteBuffer.wrap(outputBuffer, 0, length);
        }

        return data;
    }

    private int getWriteHeadersSize(Map<String, String> headers) {
        if (headers.size() == 0) {
            return 0;
        }

        int len = 10; // 5 bytes varint for info header type
        // 5 bytes varint for info headers count
        for (Map.Entry<String, String> header : headers.entrySet()) {
            len += 10; // 5 bytes varint for key size and
            // 5 bytes varint for value size
            len += header.getKey().length();
            len += header.getValue().length();
        }
        return len;
    }

    private ByteBuffer flushInfoHeaders(Infos info, Map<String, String> headers) {
        ByteBuffer infoData = ByteBuffer.allocate(getWriteHeadersSize(headers));
        if (!headers.isEmpty()) {
            writeVarint(infoData, info.getValue());
            writeVarint(infoData, headers.size());
            for (Map.Entry<String, String> pairs : headers.entrySet()) {
                writeString(infoData, pairs.getKey());
                writeString(infoData, pairs.getValue());
            }
            headers.clear();
        }
        infoData.limit(infoData.position());
        infoData.position(0);
        return infoData;
    }

    /* Writes the output buffer in header format, or format
     * client responded with (framed, unframed, http)
     */
    @Override
    public void flush() throws TTransportException {
        flushImpl(false);
    }

    @Override
    public void onewayFlush() throws TTransportException {
        flushImpl(true);
    }

    public void flushImpl(boolean oneway) throws TTransportException {
        try {
            // Check if this is a TApplicationException
            TApplicationException tae = null;
            byte[] buf = writeBuffer_.get();
            int len = writeBuffer_.len();
            if (len >= 2
                    && buf[0] == TCompactProtocol.PROTOCOL_ID
                    && ((buf[1] >> TCompactProtocol.TYPE_SHIFT_AMOUNT) & 0x03) == TMessageType.EXCEPTION) {
                // Compact
                TCompactProtocol proto = new TCompactProtocol(new TMemoryInputTransport(buf));
                @SuppressWarnings("unused")
                TMessage msg = proto.readMessageBegin();
                tae = new TApplicationException();
                tae.read(proto);
            } else if (len >= 4
                    && ((buf[0] << 24) | (buf[1] << 16)) == TBinaryProtocol.VERSION_1
                    && buf[3] == TMessageType.EXCEPTION) {
                // Binary
                TBinaryProtocol proto = new TBinaryProtocol(new TMemoryInputTransport(buf));
                @SuppressWarnings("unused")
                TMessage msg = proto.readMessageBegin();
                tae = new TApplicationException();
                tae.read(proto);
            }

            if (tae != null) {
                if (!writeHeaders.containsKey("uex")) {
                    writeHeaders.put("uex", "TApplicationException");
                }
                if (!writeHeaders.containsKey("uexw")) {
                    writeHeaders.put("uexw", tae.getMessage() == null ? "[null]" : tae.getMessage());
                }
            }
        } catch (TException e) {
            // Failed parsing a TApplicationException, so don't write headers
        }

        ByteBuffer frame = ByteBuffer.wrap(writeBuffer_.get());
        frame.limit(writeBuffer_.len());
        writeBuffer_.reset();
        if (clientType == ClientTypes.HEADERS) {
            frame = transform(frame);
        }

        if (frame.remaining() > MAX_FRAME_SIZE) {
            throw new TTransportException(
                    "Attempting to send frame that is "
                            + "too large: "
                            + Integer.toString(frame.remaining()));
        }

        if (protoId == T_JSON_PROTOCOL && clientType != ClientTypes.HTTP) {
            throw new TTransportException("Trying to send JSON encoding" + " over binary");
        }

        if (clientType == ClientTypes.HEADERS) {

            // Each varint could be up to 5 in size.
            ByteBuffer transformData = ByteBuffer.allocate(writeTransforms.size() * 5);

            // For now, no transforms require data.
            int numTransforms = writeTransforms.size();
            for (Transforms trans : writeTransforms) {
                writeVarint(transformData, trans.getValue());
            }
            transformData.limit(transformData.position());
            transformData.position(0);

            if (identity != null && identity.length() > 0) {
                writeHeaders.put(ID_VERSION_HEADER, ID_VERSION);
                writeHeaders.put(IDENTITY_HEADER, identity);
            }

            ByteBuffer infoData1 = flushInfoHeaders(Infos.INFO_PKEYVALUE, writePersistentHeaders);
            ByteBuffer infoData2 = flushInfoHeaders(Infos.INFO_KEYVALUE, writeHeaders);

            ByteBuffer headerData = ByteBuffer.allocate(10);
            writeVarint(headerData, protoId);
            writeVarint(headerData, numTransforms);
            headerData.limit(headerData.position());
            headerData.position(0);

            int headerSize =
                    transformData.remaining()
                            + infoData1.remaining()
                            + infoData2.remaining()
                            + headerData.remaining();
            int paddingSize = 4 - headerSize % 4;
            headerSize += paddingSize;

            // Allocate buffer for the headers.
            // 14 bytes for sz, magic , flags , seqId , headerSize
            ByteBuffer out = ByteBuffer.allocate(headerSize + 14);

            // See thrift/doc/HeaderFormat.txt for more info on wire format
            encodeInt(out, 10 + headerSize + frame.remaining());
            encodeShort(out, HEADER_MAGIC >> 16);
            encodeShort(out, flags);
            encodeInt(out, seqId);
            encodeShort(out, headerSize / 4);

            out.put(headerData);
            out.put(transformData);
            out.put(infoData1);
            out.put(infoData2);

            // There are no info headers for this version
            // Pad out the header with 0x00
            for (int i = 0; i < paddingSize; i++) {
                out.put((byte) 0x00);
            }
            out.position(0);

            transport_.write(out.array(), out.position(), out.remaining());
            transport_.write(frame.array(), frame.position(), frame.remaining());
        } else if (clientType == ClientTypes.FRAMED_DEPRECATED) {
            ByteBuffer out = ByteBuffer.allocate(4);
            encodeInt(out, frame.remaining());
            out.position(0);
            transport_.write(out.array(), out.position(), out.remaining());
            transport_.write(frame.array(), frame.position(), frame.remaining());
        } else if (clientType == ClientTypes.UNFRAMED_DEPRECATED) {
            transport_.write(frame.array(), frame.position(), frame.remaining());
        } else if (clientType == ClientTypes.HTTP) {
            throw new TTransportException("HTTP is unimplemented in this language");
        } else {
            throw new TTransportException("Unknown client type on send");
        }

        if (oneway) {
            transport_.onewayFlush();
        } else {
            transport_.flush();
        }
    }

    // TODO(davejwatson) potential inclusion in a java util class

    private final byte[] i32buf = new byte[4];

    private void encodeInt(ByteBuffer out, final int val) {
        encodeWord(val, i32buf);
        out.put(i32buf, 0, 4);
    }

    private final byte[] i16buf = new byte[2];

    private void encodeShort(ByteBuffer out, final int val) {
        encodeShort(val, i16buf);
        out.put(i16buf, 0, 2);
    }

    public static class Factory extends TFramedTransport.Factory {
        List<ClientTypes> clientTypes;

        public Factory(List<ClientTypes> clientTypes) {
            this.clientTypes = clientTypes;
        }

        @Override
        public TTransport getTransport(TTransport base) {
            return new THeaderTransport(base, clientTypes);
        }
    }
}