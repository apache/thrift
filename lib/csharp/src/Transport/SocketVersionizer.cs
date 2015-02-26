using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Sockets;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace Thrift.Transport
{
	/**
	* PropertyInfo for the DualMode property of the System.Net.Sockets.Socket class. Used to determine if the sockets are capable of
	* automatic IPv4 and IPv6 handling. If DualMode is present the sockets automatically handle IPv4 and IPv6 connections.
	* If the DualMode is not available the system configuration determines whether IPv4 or IPv6 is used.
	*/
	internal static class SocketVersionizer
	{
		/*
		* PropertyInfo for the DualMode property of System.Net.Sockets.Socket.
		*/
		private static PropertyInfo DualModeProperty = typeof(Socket).GetProperty("DualMode");

		/*
		* Indicates whether the used framework supports DualMode on sockets or not.
		*/
		internal static Boolean SupportsDualMode
		{
			get
			{
				return SocketVersionizer.DualModeProperty != null;
			}
		}

		/*
		* Creates a TcpClient according to the capabilitites of the used framework
		*/
		internal static TcpClient CreateTcpClient()
		{
			TcpClient client = null;

			if (SocketVersionizer.SupportsDualMode)
			{
				client = new TcpClient(AddressFamily.InterNetworkV6);
				SocketVersionizer.DualModeProperty.SetValue(client.Client, true);
			}
			else
			{
				client = new TcpClient(AddressFamily.InterNetwork);
			}

			return client;
		}

		/*
		* Creates a TcpListener according to the capabilitites of the used framework
		*/
		internal static TcpListener CreateTcpListener(Int32 port)
		{
			TcpListener listener = null;

			if (SocketVersionizer.SupportsDualMode)
			{
				listener = new TcpListener(System.Net.IPAddress.IPv6Any, port);
				SocketVersionizer.DualModeProperty.SetValue(listener.Server, true);
			}
			else
			{
				listener = new TcpListener(System.Net.IPAddress.Any, port);
			}

			return listener;
		}
	}
}
