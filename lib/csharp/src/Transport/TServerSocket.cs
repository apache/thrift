//
//  TServerTransport.cs
//
//  Begin:  Dec 3, 2007
//  Authors:
//		Will Palmeri <wpalmeri@imeem.com>
//
//  Copyright (C) 2007 imeem, inc. <http://www.imeem.com>
//  All rights reserved.

using System;
using System.Net.Sockets;


namespace Thrift.Transport
{
	public class TServerSocket : TServerTransport
	{
		/**
		* Underlying server with socket
		*/
		private TcpListener server = null;

		/**
		 * Port to listen on
		 */
		private int port = 0;

		/**
		 * Timeout for client sockets from accept
		 */
		private int clientTimeout = 0;

		/**
		 * Whether or not to wrap new TSocket connections in buffers
		 */
		private bool useBufferedSockets = false;

		/**
		 * Creates a server socket from underlying socket object
		 */
		public TServerSocket(TcpListener listener)
			:this(listener, 0)
		{
		}

		/**
		 * Creates a server socket from underlying socket object
		 */
		public TServerSocket(TcpListener listener, int clientTimeout)
		{
			this.server = listener;
			this.clientTimeout = clientTimeout;
		}

		/**
		 * Creates just a port listening server socket
		 */
		public TServerSocket(int port)
			: this(port, 0)
		{
		}

		/**
		 * Creates just a port listening server socket
		 */
		public TServerSocket(int port, int clientTimeout)
			:this(port, clientTimeout, false)
		{
		}

		public TServerSocket(int port, int clientTimeout, bool useBufferedSockets)
		{
			this.port = port;
			this.clientTimeout = clientTimeout;
			this.useBufferedSockets = useBufferedSockets;
			try
			{
				// Make server socket
				server = new TcpListener(System.Net.IPAddress.Any, this.port);
			}
			catch (Exception)
			{
				server = null;
				throw new TTransportException("Could not create ServerSocket on port " + port + ".");
			}
		}

		public override void Listen()
		{
			// Make sure not to block on accept
			if (server != null)
			{
				try
				{
					server.Start();
				}
				catch (SocketException sx)
				{
					throw new TTransportException("Could not accept on listening socket: " + sx.Message);
				}
			}
		}

		protected override TTransport AcceptImpl()
		{
			if (server == null)
			{
				throw new TTransportException(TTransportException.ExceptionType.NotOpen, "No underlying server socket.");
			}
			try
			{
				TcpClient result = server.AcceptTcpClient();
				TSocket result2 = new TSocket(result);
				result2.Timeout = clientTimeout;
				if (useBufferedSockets)
				{
					TBufferedTransport result3 = new TBufferedTransport(result2);
					return result3;
				}
				else
				{
					return result2;
				}
			}
			catch (Exception ex)
			{
				throw new TTransportException(ex.ToString());
			}
		}

		public override void Close()
		{
			if (server != null)
			{
				try
				{
					server.Stop();
				}
				catch (Exception ex)
				{
					throw new TTransportException("WARNING: Could not close server socket: " + ex);
				}
				server = null;
			}
		}
	}
}
