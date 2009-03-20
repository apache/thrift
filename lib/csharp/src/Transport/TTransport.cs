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

using System;

namespace Thrift.Transport
{
	public abstract class TTransport
	{
		public abstract bool IsOpen
		{
			get;
		}

		public bool Peek()
		{
			return IsOpen;
		}

		public abstract void Open();

		public abstract void Close();

		public abstract int Read(byte[] buf, int off, int len);

		public int ReadAll(byte[] buf, int off, int len)
		{
			int got = 0;
			int ret = 0;

			while (got < len)
			{
				ret = Read(buf, off + got, len - got);
				if (ret <= 0)
				{
					throw new TTransportException("Cannot read, Remote side has closed");
				}
				got += ret;
			}

			return got;
		}

		public abstract void Write(byte[] buf, int off, int len);

		public virtual void Flush()
		{
		}
	}
}
