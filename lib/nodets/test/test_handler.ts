/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * 'License'); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * 'AS IS' BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

//This is the server side Node test handler for the standard
//  Apache Thrift test service.

import ttypes = require("./gen-nodets/ThriftTest_types");
import ThriftTest = require("./gen-nodets/ThriftTest");
import thrift = require("thrift");



export class SyncThriftTestHandler implements ThriftTest.ThriftTest.ThriftTestLike {
  testVoid(): thrift.Q.IPromise<void> {
    //console.log('testVoid()');
    return thrift.Q.resolve<void>(undefined);
  }
  testMapMap(hello: number) {
    //console.log('testMapMap(' + hello + ')');

    var mapmap: {[key: number]: {[key: number]: number; }} = [];
    var pos: {[key: number]: number; } = [];
    var neg: {[key: number]: number; } = [];
    for (var i = 1; i < 5; i++) {
      pos[i] = i;
      neg[-i] = -i;
    }
    mapmap[4] = pos;
    mapmap[-4] = neg;

    return thrift.Q.resolve(mapmap);
  }
  testInsanity(argument: ttypes.ThriftTest.Insanity): Q.IPromise<{ [k: number]: any; }> {
    //console.log('testInsanity(');
    //console.log(argument);
    //console.log(')');

    var hello = new ttypes.ThriftTest.Xtruct();
    hello.string_thing = 'Hello2';
    hello.byte_thing = 2;
    hello.i32_thing = 2;
    hello.i64_thing = 2;

    var goodbye = new ttypes.ThriftTest.Xtruct();
    goodbye.string_thing = 'Goodbye4';
    goodbye.byte_thing = 4;
    goodbye.i32_thing = 4;
    goodbye.i64_thing = 4;

    var crazy = new ttypes.ThriftTest.Insanity();
    crazy.userMap = [];
    crazy.userMap[ttypes.ThriftTest.Numberz.EIGHT] = 8;
    crazy.userMap[ttypes.ThriftTest.Numberz.FIVE] = 5;
    crazy.xtructs = [goodbye, hello];

    var first_map: any = [];
    var second_map: any = [];

    first_map[ttypes.ThriftTest.Numberz.TWO] = crazy;
    first_map[ttypes.ThriftTest.Numberz.THREE] = crazy;

    var looney = new ttypes.ThriftTest.Insanity();
    second_map[ttypes.ThriftTest.Numberz.SIX] = looney;

    var insane: { [k: number]: any; } = [];
    insane[1] = first_map;
    insane[2] = second_map;

    //console.log('insane result:');
    //console.log(insane);
    return thrift.Q.resolve(insane);
  }
  testMulti(arg0: any, arg1: number, arg2: number, arg3: { [k: number]: string; }, arg4: ttypes.ThriftTest.Numberz, arg5: number) {
    //console.log('testMulti()');

    var hello = new ttypes.ThriftTest.Xtruct();
    hello.string_thing = 'Hello2';
    hello.byte_thing = arg0;
    hello.i32_thing = arg1;
    hello.i64_thing = arg2;
    return thrift.Q.resolve(hello);
  }
  testException(arg: string): Q.IPromise<void> {
    //console.log('testException('+arg+')');
    if (arg === 'Xception') {
      var x = new ttypes.ThriftTest.Xception();
      x.errorCode = 1001;
      x.message = arg;
      throw x;
    } else if (arg === 'TException') {
      throw new thrift.Thrift.TException(arg);
    } else {
      return;
    }
  }
  testMultiException(arg0: string, arg1: string) {
    //console.log('testMultiException(' + arg0 + ', ' + arg1 + ')');
    if (arg0 === ('Xception')) {
      var x = new ttypes.ThriftTest.Xception();
      x.errorCode = 1001;
      x.message = 'This is an Xception';
      throw x;
    } else if (arg0 === ('Xception2')) {
      var x2 = new ttypes.ThriftTest.Xception2();
      x2.errorCode = 2002;
      x2.struct_thing = new ttypes.ThriftTest.Xtruct();
      x2.struct_thing.string_thing = 'This is an Xception2';
      throw x2;
    }

    var res = new ttypes.ThriftTest.Xtruct();
    res.string_thing = arg1;
    return thrift.Q.resolve(res);
  }
  testOneway(sleepFor: number) {
    //console.log('testOneway(' + sleepFor + ') => JavaScript (like Rust) never sleeps!');
  }

  testString(thing: string) {
    return thrift.Q.resolve(thing);
  }
  testByte(thing: number) {
    return thrift.Q.resolve(thing);
  }
  testI32(thing: number) {
    return thrift.Q.resolve(thing);
  }
  testI64(thing: number) {
    return thrift.Q.resolve(thing);
  }
  testDouble(thing: number) {
    return thrift.Q.resolve(thing);
  }
  testBinary(thing: Buffer) {
    return thrift.Q.resolve(thing);
  }
  testStruct(thing: ttypes.ThriftTest.Xtruct) {
    return thrift.Q.resolve(thing);
  }
  testNest(thing: ttypes.ThriftTest.Xtruct2) {
    return thrift.Q.resolve(thing);
  }
  testMap(thing: { [k: number]: number; }) {
    return thrift.Q.resolve(thing);
  }
  testStringMap(thing: { [k: string]: string; }) {
    return thrift.Q.resolve(thing);
  }
  testSet(thing: number[]) {
    return thrift.Q.resolve(thing);
  }
  testList(thing: number[]) {
    return thrift.Q.resolve(thing);
  }
  testEnum(thing: ttypes.ThriftTest.Numberz) {
    return thrift.Q.resolve(thing);
  }
  testTypedef(thing: number) {
    return thrift.Q.resolve(thing);
  }
}

export class AsyncThriftTestHandler implements ThriftTest.ThriftTest.ThriftTestLike {
  private syncHandler: SyncThriftTestHandler;
  constructor() {
    this.syncHandler = new SyncThriftTestHandler();
  }

  testVoid(callback: (result: void) => void): Q.IPromise<void> {
    callback(undefined);
    return;
  }
  testMapMap(hello: number,
    callback: (err: any, result: { [k: number]: { [k: number]: number; }; }) => void):
     Q.IPromise<{ [k: number]: { [k: number]: number; }; }> {
    //console.log('testMapMap(' + hello + ')');

    var mapmap: {[key: number]: {[key: number]: number; }} = [];
    var pos: {[key: number]: number; } = [];
    var neg: {[key: number]: number; } = [];
    for (var i = 1; i < 5; i++) {
      pos[i] = i;
      neg[-i] = -i;
    }
    mapmap[4] = pos;
    mapmap[-4] = neg;

    callback(null, mapmap);
    return;
  }
  testInsanity(argument: ttypes.ThriftTest.Insanity, callback?: (err: any, result: { [k: number]: any; }) => void): Q.IPromise<{ [k: number]: any; }> {
    //console.log('testInsanity(');
    //console.log(argument);
    //console.log(')');

    var hello = new ttypes.ThriftTest.Xtruct();
    hello.string_thing = 'Hello2';
    hello.byte_thing = 2;
    hello.i32_thing = 2;
    hello.i64_thing = 2;

    var goodbye = new ttypes.ThriftTest.Xtruct();
    goodbye.string_thing = 'Goodbye4';
    goodbye.byte_thing = 4;
    goodbye.i32_thing = 4;
    goodbye.i64_thing = 4;

    var crazy = new ttypes.ThriftTest.Insanity();
    crazy.userMap = [];
    crazy.userMap[ttypes.ThriftTest.Numberz.EIGHT] = 8;
    crazy.userMap[ttypes.ThriftTest.Numberz.FIVE] = 5;
    crazy.xtructs = [goodbye, hello];

    var first_map: any = [];
    var second_map: any = [];

    first_map[ttypes.ThriftTest.Numberz.TWO] = crazy;
    first_map[ttypes.ThriftTest.Numberz.THREE] = crazy;

    var looney = new ttypes.ThriftTest.Insanity();
    second_map[ttypes.ThriftTest.Numberz.SIX] = looney;

    var insane: { [k: number]: any; } = [];
    insane[1] = first_map;
    insane[2] = second_map;

    //console.log('insane result:');
    //console.log(insane);
    callback(null, insane);
    return;
  }
  testMulti(arg0: any, arg1: number, arg2: number, arg3: { [k: number]: string; }, arg4: ttypes.ThriftTest.Numberz, arg5: number, result: Function): Q.IPromise<ttypes.ThriftTest.Xtruct> {
    var hello = this.syncHandler.testMulti(arg0, arg1, arg2, arg3, arg4, arg5);
    hello.then(hello => result(null, hello));
    return;
  }
  testException(arg: string, result: (err: any) => void): Q.IPromise<void> {
    //console.log('testException('+arg+')');
    if (arg === 'Xception') {
      var x = new ttypes.ThriftTest.Xception();
      x.errorCode = 1001;
      x.message = arg;
      result(x);
    } else if (arg === 'TException') {
      result(new thrift.Thrift.TException(arg));
    } else {
      result(null);
    }
    return;
  }
  testMultiException(arg0: string, arg1: string, result: (err: any, res?: ttypes.ThriftTest.Xtruct) => void): thrift.Q.IPromise<ttypes.ThriftTest.Xtruct> {
    //console.log('testMultiException(' + arg0 + ', ' + arg1 + ')');
    if (arg0 === ('Xception')) {
      var x = new ttypes.ThriftTest.Xception();
      x.errorCode = 1001;
      x.message = 'This is an Xception';
      result(x);
    } else if (arg0 === ('Xception2')) {
      var x2 = new ttypes.ThriftTest.Xception2();
      x2.errorCode = 2002;
      x2.struct_thing = new ttypes.ThriftTest.Xtruct();
      x2.struct_thing.string_thing = 'This is an Xception2';
      result(x2);
    } else {
      var res = new ttypes.ThriftTest.Xtruct();
      res.string_thing = arg1;
      result(null, res);
    }
    return;
  }
  testOneway(sleepFor: number, result: Function) {
    this.syncHandler.testOneway(sleepFor);
  }
  testString(thing: string, callback: (err: any, result: string) => void): Q.IPromise<string> {
    callback(null, thing);
    return;
  }
  testByte(thing: number, callback: (err: any, result: number) => void): Q.IPromise<number> {
    callback(null, thing);
    return;
  }
  testI32(thing: number, callback: (err: any, result: number) => void): Q.IPromise<number>  {
    callback(null, thing);
    return;
  }
  testI64(thing: number, callback: (err: any, result: number) => void): Q.IPromise<number>  {
    callback(null, thing);
    return;
  }
  testDouble(thing: number, callback: (err: any, result: number) => void): Q.IPromise<number>  {
    callback(null, thing);
    return;
  }
  testBinary(thing: Buffer, callback: (err: any, result: Buffer) => void): Q.IPromise<Buffer> {
    callback(null, thing);
    return;
  }
  testStruct(thing: ttypes.ThriftTest.Xtruct, callback: (err: any, result: ttypes.ThriftTest.Xtruct) => void): Q.IPromise<ttypes.ThriftTest.Xtruct> {
    callback(null, thing);
    return;
  }
  testNest(thing: ttypes.ThriftTest.Xtruct2, callback: (err: any, result: ttypes.ThriftTest.Xtruct2) => void): Q.IPromise<ttypes.ThriftTest.Xtruct2> {
    callback(null, thing);
    return;
  }
  testMap(thing: { [k: number]: number; }, callback: (err: any, result: { [k: number]: number; }) => void): Q.IPromise<{ [k: number]: number; }> {
    callback(null, thing);
    return;
  }
  testStringMap(thing: { [k: string]: string; }, callback: (err: any, result: { [k: string]: string; }) => void): Q.IPromise<{ [k: string]: string; }> {
    callback(null, thing);
    return;
  }
  testSet(thing: number[], callback: (err: any, result: number[]) => void): Q.IPromise<number[]> {
    callback(null, thing);
    return;
  }
  testList(thing: number[], callback: (err: any, result: number[]) => void): Q.IPromise<number[]> {
    callback(null, thing);
    return;
  }
  testEnum(thing: ttypes.ThriftTest.Numberz, callback: (err: any, result: ttypes.ThriftTest.Numberz) => void): Q.IPromise<ttypes.ThriftTest.Numberz> {
    callback(null, thing);
    return;
  }
  testTypedef(thing: number, callback: (err: any, result: number) => void): Q.IPromise<number> {
    callback(null, thing);
    return;
  }
}
