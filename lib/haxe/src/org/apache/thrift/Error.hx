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
 
package org.apache.thrift;
  
// there seems no built-in equivalent for the Error object in Haxe (or it is very well hidden)
// http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/Error.html
class Error {

	private var _id : Int;
	private var _msg : String;
	
	public var errorID(get,never) : Int;
	public var message(get,never) : String;
	private var name(default,null) : String;  	// NOT IMPLEMENTED

	
	function new(message : String = "", id : Int = 0) {
		//super();
		_id = id;
		_msg = message;
	}
	
	function get_errorID() : Int {
		return _id;
	}
	
	function get_message() : String {
		return _msg;
	}
	

	// NOT IMPLEMENTED
	// see http://haxe.org/manual/cr-rtti-structure.html
	private function get_name() : String {
		throw "not implemented";
	}
	
}
