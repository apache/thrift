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
import org.apache.thrift.meta_data.*;
import org.apache.thrift.protocol.*;
import org.apache.thrift.server.*;
import org.apache.thrift.transport.*;
import tests.ConstantsTest;
import tests.MultiplexTest;
import tests.StreamTest;
import thrift.test.*;

enum WhatTests
{
	Normal;
	Multiplex;
	Constants;
}

class Main
{
	static private var what : WhatTests = Normal;
	static private var server : Bool = false;

	static private inline var CMDLINEHELP : String
		= "\nHaxeTests  [client|server]  [multiplex]\n"
		  + "  client|server  ... determines run mode for some tests, default is client\n"
		  + "  multiplex ........ run multiplex test server or client\n"
		  + "  constants ........ run constants and conformity tests\n"
		  ;

	static private function ParseArgs()
	{
		#if sys

		var args = Sys.args();
		if ( args != null)
		{
			for ( arg in args)
			{
				switch (arg.toLowerCase())
				{
					case "client":
						server = false;
					case "server" :
						server = true;
					case "multiplex" :
						what = Multiplex;
					case "constants" :
						what = Constants;
					default:
						throw 'Invalid argument "$arg"\n'+CMDLINEHELP;
				}
			}
		}

		#end
	}

	static public function main()
	{
		try
		{
			ParseArgs();

			switch ( what)
			{
				case Normal:
					#if sys
					tests.StreamTest.Run(server);
					#end
				case Multiplex:
					#if ! (flash || html5 || js)
					tests.MultiplexTest.Run(server);
					#end
				case Constants:
					tests.ConstantsTest.Run(server);
				default:
					throw 'Unhandled test mode $what';
			}

			trace("All tests completed.");
		}
		catch ( e: Dynamic)
		{
			trace('$e');
			#if sys
			Sys.exit(1);  // indicate error
			#end
		}
	}
}