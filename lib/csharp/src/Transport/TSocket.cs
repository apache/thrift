//
//  TSocket.cs
//
//  Begin:  Aug 19, 2007
//  Authors: 
//		Todd Berman <tberman@imeem.com>
//
//  Copyright (C) 2007 imeem, inc. <http://www.imeem.com>
//  All rights reserved.
//

using System;
using System.Collections.Generic;
using System.Text;
using System.Net.Sockets;

namespace Thrift.Transport
{
	public class TSocket : TStreamTransport
	{
		private TcpClient client = null;
		private string host = null;
		private int port = 0;
		private int timeout = 0;

		public TSocket(TcpClient client)
		{
			this.client = client;

			if (IsOpen)
			{
				inputStream = client.GetStream();
				outputStream = client.GetStream();
			}
		}

		public TSocket(string host, int port) : this(host, port, 0)
		{
		}

		public TSocket(string host, int port, int timeout)
		{
			this.host = host;
			this.port = port;
			this.timeout = timeout;

			InitSocket();
		}

		private void InitSocket()
		{
			client = new TcpClient();
			client.ReceiveTimeout = client.SendTimeout = timeout;
		}

		public int Timeout
		{
			set
			{
				client.ReceiveTimeout = client.SendTimeout = timeout = value;
			}
		}

		public TcpClient TcpClient
		{
			get
			{
				return client;
			}
		}

		public override bool IsOpen
		{
			get
			{
				if (client == null)
				{
					return false;
				}

				return client.Connected;
			}
		}

		public override void Open()
		{
			if (IsOpen)
			{
				throw new TTransportException(TTransportException.ExceptionType.AlreadyOpen, "Socket already connected");
			}

			if (String.IsNullOrEmpty(host))
			{
				throw new TTransportException(TTransportException.ExceptionType.NotOpen, "Cannot open null host");
			}

			if (port <= 0)
			{
				throw new TTransportException(TTransportException.ExceptionType.NotOpen, "Cannot open without port");
			}

			if (client == null)
			{
				InitSocket();
			}

			client.Connect(host, port);
			inputStream = client.GetStream();
			outputStream = client.GetStream();
		}

		public override void Close()
		{
			base.Close();
			if (client != null)
			{
				client.Close();
				client = null;
			}
		}
	}
}
