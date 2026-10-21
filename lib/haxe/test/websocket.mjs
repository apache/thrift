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

// Runs a compiled WebSocketTest against a WebSocket server that answers each
// ThriftTest call (binary protocol) with its own argument:
//
//   node websocket.mjs bin/WebSocketTest.js
//
// Needs no packages. The server is a minimal WebSocket implementation of its
// own; the client uses the WebSocket built into Node, which Node 20 and 21
// only provide with --experimental-websocket, passed when needed.
import { spawn } from 'node:child_process';
import crypto from 'node:crypto';
import http from 'node:http';

const WEBSOCKET_GUID = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';

// Size of a field value on the wire, by Thrift type id.
const FIXED_SIZE = { 2: 1, 3: 1, 4: 8, 6: 2, 8: 4, 10: 8 };
const STRING = 11;

function frame(payload) {
  let header;
  if (payload.length < 126) {
    header = Buffer.from([0x82, payload.length]);
  } else {
    header = Buffer.from([0x82, 126, payload.length >> 8, payload.length & 0xff]);
  }
  return Buffer.concat([header, payload]);
}

// A call with one argument, field 1, becomes a reply whose result, field 0,
// is that argument: the ThriftTest echo methods.
function reply(call) {
  let pos = 4; // strict header: version and message type
  const nameLength = call.readInt32BE(pos);
  pos += 4;
  const name = call.subarray(pos, pos + nameLength);
  pos += nameLength;
  const seqid = call.readInt32BE(pos);
  pos += 4;
  const type = call[pos];
  pos += 3; // type and field id
  const size = type === STRING ? 4 + call.readInt32BE(pos) : FIXED_SIZE[type];
  const value = call.subarray(pos, pos + size);

  const head = Buffer.alloc(8);
  head.writeUInt32BE(0x80010002, 0); // version 1, REPLY
  head.writeInt32BE(nameLength, 4);
  const seq = Buffer.alloc(4);
  seq.writeInt32BE(seqid, 0);
  return Buffer.concat([head, name, seq, Buffer.from([type, 0, 0]), value, Buffer.from([0])]);
}

function serve(socket) {
  let pending = Buffer.alloc(0);
  socket.on('data', (chunk) => {
    pending = Buffer.concat([pending, chunk]);
    for (;;) {
      if (pending.length < 2) return;
      const opcode = pending[0] & 0x0f;
      let length = pending[1] & 0x7f;
      let pos = 2;
      if (length === 126) {
        if (pending.length < 4) return;
        length = pending.readUInt16BE(2);
        pos = 4;
      }
      // Client frames are always masked.
      if (pending.length < pos + 4 + length) return;
      const mask = pending.subarray(pos, pos + 4);
      const payload = Buffer.from(pending.subarray(pos + 4, pos + 4 + length));
      for (let i = 0; i < payload.length; i++) {
        payload[i] ^= mask[i & 3];
      }
      pending = pending.subarray(pos + 4 + length);

      if (opcode === 0x8) {
        socket.end(Buffer.from([0x88, 0]));
        return;
      }
      if (opcode === 0x2) {
        socket.write(frame(reply(payload)));
      }
    }
  });
  socket.on('error', () => {});
}

const script = process.argv[2];
const server = http.createServer((request, response) => {
  response.writeHead(426);
  response.end();
});
server.on('upgrade', (request, socket) => {
  const accept = crypto.createHash('sha1')
    .update(request.headers['sec-websocket-key'] + WEBSOCKET_GUID)
    .digest('base64');
  socket.write('HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\n' +
               `Connection: Upgrade\r\nSec-WebSocket-Accept: ${accept}\r\n\r\n`);
  serve(socket);
});

server.listen(0, '127.0.0.1', () => {
  const url = `ws://127.0.0.1:${server.address().port}`;
  const flags = typeof WebSocket === 'undefined' ? ['--experimental-websocket'] : [];
  const client = spawn(process.execPath, [...flags, script, url], { stdio: 'inherit' });
  const timer = setTimeout(() => {
    console.error('WebSocketTest did not finish within 30 s');
    client.kill();
  }, 30000);
  client.on('exit', (code) => {
    clearTimeout(timer);
    process.exit(code === null ? 1 : code);
  });
});
