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
import org.apache.thrift.meta_data.*;

import tutorial.*;
import shared.*;


enum Prot {
	binary;
	json;
}

enum Trns {
	socket;
	http;
}

class Main {
	
	private static var server : Bool = false;
	private static var framed : Bool = false;
	private static var buffered : Bool = false;
	private static var prot : Prot = binary;
	private static var trns : Trns = socket;
	
	private static var targetHost : String = "localhost";
	private static var targetPort : Int = 80;
	
    static function main() {
		try {
  			ParseArgs();
		} catch (e : String) {
			println(e);
			println(GetHelp());
			return;
		}
			

		try {
			if (server)
				RunServer();
			else 
				RunClient();
		} catch (e : String) {
			println(e);
		}

		println("Completed.");
    }
	
	
	private static function println(txt : String) {
		Sys.println(txt);
	}
	
	
	private static function GetHelp() : String {
		return Sys.executablePath()+"  modus  trnsOption  transport  protocol\n"
		+"Options:\n"
		+"  modus:       client, server   (default: client)\n"
		+"  trnsOption:  framed, buffered (default: none)\n"
		+"  transport:   socket, http     (default: socket)\n"
		+"  protocol:    binary, json     (default: binary)\n"
		+"\n"
		+"All arguments are optional.\n";
	}
	
	
	private static function ParseArgs() : Void {
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
				if ( arg == "framed") {
					framed = true;
				} else if ( arg == "buffered") {
					buffered = true;
				} else if ( arg == "socket") {
					trns = socket;
					++step;
				} else if ( arg == "http") {
					trns = http;
					++step;
				} else {
					throw "Unknown transport "+arg;
				}					

			case 2:					
				if ( arg == "binary") {
					prot = binary;
					++step;
				} else if ( arg == "json") {
					prot = json;
					++step;
				} else {
					throw "Unknown protocol "+arg;
				}					

			default:
				throw "Unexpected argument "+arg;
			}

			if ( framed && buffered)
			{
				println("WN: framed supersedes buffered");
			}

		}
	}
	
	
	private static function ClientSetup() : Calculator {

		// endpoint transport
		var transport : TTransport;
		switch(trns)
		{
		case socket:
		 	transport = new TSocket( targetHost, targetPort);
        case http:
		 	transport = new THttpClient( targetHost);
		default:
			throw "Unhandled transport";
		}

		
		// optinal layered transport
		if ( framed) {
			transport = new TFramedTransport(transport);
		} else if ( buffered) {
			throw "TBufferedTransport not implemented yet";
			//transport = new TBufferedTransport(transport);
		}

		
		// protocol
		var protocol : TProtocol;
		switch(prot)
		{
		case binary:
		 	protocol = new TBinaryProtocol( transport);
		/*
        case json:
		 	protocol = new TJsonProtocol( transport);
		*/
		default:
			throw "Unhandled protocol";
		}

		
		// put everything together
		return new CalculatorImpl(protocol,protocol);
	}
	
	
	private static function RunClient() : Void {
		var client = ClientSetup();
		
		client.ping();
		println("ping()");

		client.add( 1, 1, 
			null,     
			function( sum : haxe.Int32) : Void {
				println('1+1= $sum');
			});
		

		var work = new tutorial.Work();
		work.op = tutorial.Operation.DIVIDE;
		work.num1 = 1;
		work.num2 = 0;
		try {
			client.calculate( 1, work, 
				function(error : Error) : Void { 
					println("Error: "+error.message); 
				}, 
				function(quotient : haxe.Int32) : Void { 
					println('Whoa we can divide by 0! Result = $quotient'); 
				}); 

		} 
		catch(e : Error) {
			println("TODO: exception");
			/*
			switch v := err.(type) {
			case *tutorial.InvalidOperation:;
				println("Invalid operation:", v);
			default:;
				println("Error during operation:", err);
			}
			*/
		}

		work.op = tutorial.Operation.SUBTRACT;
		work.num1 = 15;
		work.num2 = 10;
		client.calculate( 1, work,
			null,     
			function( diff : haxe.Int32) : Void {
				println('15-10=$diff\n');
			});
		

		client.getStruct( 1,
			null,     
			function( log : SharedStruct) : Void {
				println('Check log: $(log.Value)');
			});
	}
	
	
	private static function RunServer() : Void {
		throw "Not implemented";
	}

}
