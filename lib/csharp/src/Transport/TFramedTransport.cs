/**
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

using System.IO;

namespace Thrift.Transport
{
	public class TFramedTransport : TTransport
	{
		protected TTransport transport = null;
		protected MemoryStream writeBuffer = new MemoryStream(1024);
		protected MemoryStream readBuffer = null;

		public class Factory : TTransportFactory
		{
			public override TTransport GetTransport(TTransport trans)
			{
				return new TFramedTransport(trans);
			}
		}

		public TFramedTransport(TTransport transport)
		{
			this.transport = transport;
		}

		public override void Open()
		{
			transport.Open();
		}

		public override bool IsOpen
		{
			get
			{
				return transport.IsOpen;
			}
		}

		public override void Close()
		{
			transport.Close();
		}

		public override int Read(byte[] buf, int off, int len)
		{
			if (readBuffer != null)
			{
				int got = readBuffer.Read(buf, off, len);
				if (got > 0)
				{
					return got;
				}
			}

			// Read another frame of data
			ReadFrame();

			return readBuffer.Read(buf, off, len);
		}

		private void ReadFrame()
		{
			byte[] i32rd = new byte[4];
			transport.ReadAll(i32rd, 0, 4);
			int size =
				((i32rd[0] & 0xff) << 24) |
				((i32rd[1] & 0xff) << 16) |
				((i32rd[2] & 0xff) <<  8) |
				((i32rd[3] & 0xff));

			byte[] buff = new byte[size];
			transport.ReadAll(buff, 0, size);
			readBuffer = new MemoryStream(buff);
		}

		public override void Write(byte[] buf, int off, int len)
		{
			writeBuffer.Write(buf, off, len);
		}

		public override void Flush()
		{
			byte[] buf = writeBuffer.GetBuffer();
			int len = (int)writeBuffer.Length;
			writeBuffer = new MemoryStream(writeBuffer.Capacity);

			byte[] i32out = new byte[4];
			i32out[0] = (byte)(0xff & (len >> 24));
			i32out[1] = (byte)(0xff & (len >> 16));
			i32out[2] = (byte)(0xff & (len >> 8));
			i32out[3] = (byte)(0xff & (len));
			transport.Write(i32out, 0, 4);
			transport.Write(buf, 0, len);
			transport.Flush();
		}
	}
}
