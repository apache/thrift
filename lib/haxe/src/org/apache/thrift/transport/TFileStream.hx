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

package org.apache.thrift.transport;

import haxe.io.Bytes;
import haxe.io.BytesBuffer;
import haxe.io.Input;
import haxe.io.Output;

#if js
import js.node.Fs;
#end

enum TFileMode {
    CreateNew;
    Append;
    Read;
}


class TFileStream implements TStream {

    public var FileName(default,null) : String;

    #if !js
    private var Input  : sys.io.FileInput;
    private var Output : sys.io.FileOutput;
    #else
    private var Input  : Null<Int>;
    private var Output  : Null<Int>;
    private var lastInputOffset : Int = 0;
    #end


    public function new( fname : String, mode : TFileMode) {
        FileName = fname;
        switch ( mode)
        {
            case TFileMode.CreateNew:
                #if !js
                Output = sys.io.File.write( fname, true);
                #else
                Output = Fs.openSync(fname, WriteCreate);
                #end

            case TFileMode.Append:
                #if !js
                Output = sys.io.File.append( fname, true);
                #else
                Output = Fs.openSync(fname, AppendCreate);
                #end

            case TFileMode.Read:
                #if !js
                Input = sys.io.File.read( fname, true);
                #else
                Input = Fs.openSync(fname, js.node.FsOpenFlag.Read);
                #end

            default:
                throw new TTransportException( TTransportException.UNKNOWN,
                                               "Unsupported mode");
        }

    }

    public function Close() : Void {
        if( Input != null) {
            #if !js
            Input.close();
            #else
            Fs.closeSync(Input);
            #end
            Input = null;
        }
        if( Output != null) {
            #if !js
            Output.close();
            #else
            Fs.closeSync(Output);
            #end
            Output = null;
        }
    }

    public function Peek() : Bool {
        if( Input == null)
            throw new TTransportException( TTransportException.NOT_OPEN, "File not open for input");

        #if !js
        return (! Input.eof());
        #else
        var bytesRead = 0;
        var copyBuf = new js.node.Buffer(1);
        return ((bytesRead = Fs.readSync(Input, copyBuf, 0, 1, lastInputOffset)) > 0);
        #end
        
    }

    public function Read( buf : Bytes, offset : Int, count : Int) : Int {
        if( Input == null)
            throw new TTransportException( TTransportException.NOT_OPEN, "File not open for input");

        #if !js
        return Input.readBytes( buf, offset, count);
        #else
        var bytesRead = 0;
        var copyJsBuf = new js.node.Buffer(count);
        bytesRead = Fs.readSync(Input, copyJsBuf, 0, count, lastInputOffset);
        if(bytesRead > 0) {
            lastInputOffset += bytesRead;
            var readBuf = copyJsBuf.hxToBytes();
            buf.blit(offset, readBuf, 0, bytesRead);
        }
        return bytesRead;
        #end
    }

    public function Write( buf : Bytes, offset : Int, count : Int) : Void {
        if( Output == null)
            throw new TTransportException( TTransportException.NOT_OPEN, "File not open for output");

        #if !js
        Output.writeBytes( buf, offset, count);
        #else
        var writeBuf = js.node.buffer.Buffer.hxFromBytes(buf);
        Fs.writeSync(Output, writeBuf, offset, count);
        #end
    }

    public function Flush() : Void {
        if( Output != null) {
            #if !js
            Output.flush();
            #else
            Fs.fsyncSync(Output);
            #end
        }
    }

}
 
