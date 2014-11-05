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

package;

import org.apache.thrift.*;
import org.apache.thrift.protocol.*;
import org.apache.thrift.transport.*;
import org.apache.thrift.server.*;
import org.apache.thrift.meta_data.*;

using StringTools;


enum Prot {
	binary;
	json;
}

enum Trns {
	socket;
	http;
}


class Arguments
{
	public var server(default,null) : Bool = false;
	public var framed(default,null) : Bool = false;
	public var buffered(default,null) : Bool = false;
	public var protocol(default,null) : Prot = binary;
	public var transport(default,null) : Trns = socket;
	
	public var host(default,null) : String = "localhost";
	public var port(default,null) : Int = 9090;

	public var numIterations(default,null) : Int = 1;
	public var numThreads(default,null) : Int = 1;
	
	
	public function new() {
		#if sys
		try {
  			ParseArgs();
		} catch (e : String) {
			trace(GetHelp());
			throw e;
		}
		#else
		trace("WN: Platform does not support program arguments, using defaults.");
		#end
	}
	
	#if sys
		
	private static function GetHelp() : String {
		return "\n"
			+Sys.executablePath()+"  [client|server]  [options]\n"
			+"Modus: Either client or server, the default is client.\n"
			+"\n"
			+"Options:\n"
			+"  -f, --framed         framed transport (supersedes buffered)\n"
			+"  -b, --buffered       buffered transport\n"
			+"  --json               JSON protocol\n"
			+"  --protocol=<prot>    Choose protocol: json, binary (default binary).\n"
			+"  --port=<port>        Port number for socket transport, default 9090\n"
			+"\n"
			+"Client only options:\n"
			+"  --host=<host>        Host name, IP or URL, default localhost\n"
			+"  -n=<iterations>      Number of test iterations\n"
			+"  -t=<threads>         Number of test threads\n"
			+"  -u=<url>             Target Host/URL (same as --host)\n"
			+"\n"
			+"All arguments are optional.\n";
	}
	

	private function ParseArgs() : Void {
		var step = 0;
		for (arg in Sys.args()) {
			
			// server|client
			switch(step) {
			case 0:
				++step;
				if ( arg == "client") 
					server = false;
				else if ( arg == "server") 
					server = true;
				else
					throw "First argument must be 'server' or 'client'";
					
			case 1:					
				if ( (arg == "-f") || (arg == "--framed")) {
					framed = true;
				} else if (( arg == "-b") || ( arg == "--buffered")) {
					buffered = true;
				} else if (( arg == "--json") || (arg == "--protocol=json")){
					protocol = json;
				} else if (( arg == "--protocol=binary")){
					protocol = binary;
				} else if (arg.startsWith("--host=")) {
					ClientOnlyOption(arg);
					host = arg.substr(arg.indexOf("=") + 1);
				} else if (arg.startsWith("--port=")) {
					var tmp = Std.parseInt(arg.substr(arg.indexOf("=")+1));
					if( tmp != null)
						port = tmp;
					else
						throw "Invalid port number "+arg;
				} else if (arg == "-n") {
					ClientOnlyOption(arg);
					step = 2;
				} else if (arg == "-t") {
					ClientOnlyOption(arg);
					step = 3;
				} else if (arg == "-u") {
					ClientOnlyOption(arg);
					step = 4;
				} else {
					throw "Unexpected argument "+arg;
				}					
					
			case 2:  // num iterations
				step = 1;
				var tmp = Std.parseInt(arg);
				if( tmp != null)
					numIterations = tmp;
				else
					throw "Invalid numeric value "+arg;
					
			case 3: // num threads
				step = 1;
				var tmp = Std.parseInt(arg);
				if( tmp != null)
					numThreads = tmp;
				else
					throw "Invalid numeric value "+arg;
					
			case 4:  // url
				step = 1;
				host = arg;
					
			default:
				throw "Unexpected state";
			}

			
			if ( framed && buffered)
			{
				trace("WN: framed supersedes buffered transport");
			}

		}
	}

	#end
		
		
	private function ClientOnlyOption( arg : String) {
		if( server) {
			throw "Unexpected argument in client mode: "+arg;
		}
	}
}
