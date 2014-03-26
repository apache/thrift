//
// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements. See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership. The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.
//

#include <lua.h>
#include <lauxlib.h>
#include <string.h>
#include <inttypes.h>
#include <netinet/in.h>

extern int64_t lualongnumber_checklong(lua_State *L, int index);
extern int64_t lualongnumber_pushlong(lua_State *L, int64_t *val);

// host order to network order (64-bit)
static int64_t T_htonll(uint64_t data) {
  uint32_t d1 = htonl((uint32_t)data);
  uint32_t d2 = htonl((uint32_t)(data >> 32));
  return ((uint64_t)d1 << 32) + (uint64_t)d2;
}

// network order to host order (64-bit)
static int64_t T_ntohll(uint64_t data) {
  uint32_t d1 = ntohl((uint32_t)data);
  uint32_t d2 = ntohl((uint32_t)(data >> 32));
  return ((uint64_t)d1 << 32) + (uint64_t)d2;
}

/**
 * bpack(type, data)
 *  c - Signed Byte
 *  s - Signed Short
 *  i - Signed Int
 *  l - Signed Long
 *  d - Double
 */
static int l_bpack(lua_State *L) {
  const char *code = luaL_checkstring(L, 1);
  luaL_argcheck(L, code[1] == '\0', 0, "Format code must be one character.");
  luaL_Buffer buf;
  luaL_buffinit(L, &buf);

  switch (code[0]) {
    case 'c': {
      int8_t data = luaL_checknumber(L, 2);
      luaL_addlstring(&buf, (void*)&data, sizeof(data));
      break;
    }
    case 's': {
      int16_t data = luaL_checknumber(L, 2);
      data = (int16_t)htons(data);
      luaL_addlstring(&buf, (void*)&data, sizeof(data));
      break;
    }
    case 'i': {
      int32_t data = luaL_checkinteger(L, 2);
      data = (int32_t)htonl(data);
      luaL_addlstring(&buf, (void*)&data, sizeof(data));
      break;
    }
    case 'l': {
      int64_t data = lualongnumber_checklong(L, 2);
      data = (int64_t)T_htonll(data);
      luaL_addlstring(&buf, (void*)&data, sizeof(data));
      break;
    }
    case 'd': {
      double data = luaL_checknumber(L, 2);
      luaL_addlstring(&buf, (void*)&data, sizeof(data));
      break;
    }
    default:
      luaL_argcheck(L, 0, 0, "Invalid format code.");
  }

  luaL_pushresult(&buf);
  return 1;
}

/**
 * bunpack(type, data)
 *  c - Signed Byte
 *  s - Signed Short
 *  i - Signed Int
 *  l - Signed Long
 *  d - Double
 */
static int l_bunpack(lua_State *L) {
  const char *code = luaL_checkstring(L, 1);
  luaL_argcheck(L, code[1] == '\0', 0, "Format code must be one character.");
  const char *data = luaL_checkstring(L, 2);
  size_t len = lua_rawlen(L, 2);

  switch (code[0]) {
    case 'c': {
      int8_t val;
      luaL_argcheck(L, len == sizeof(val), 1, "Invalid input string size.");
      memcpy(&val, data, sizeof(val));
      lua_pushnumber(L, val);
      break;
    }
    case 's': {
      int16_t val;
      luaL_argcheck(L, len == sizeof(val), 1, "Invalid input string size.");
      memcpy(&val, data, sizeof(val));
      val = (int16_t)ntohs(val);
      lua_pushnumber(L, val);
      break;
    }
    case 'i': {
      int32_t val;
      luaL_argcheck(L, len == sizeof(val), 1, "Invalid input string size.");
      memcpy(&val, data, sizeof(val));
      val = (int32_t)ntohl(val);
      lua_pushnumber(L, val);
      break;
    }
    case 'l': {
      int64_t val;
      luaL_argcheck(L, len == sizeof(val), 1, "Invalid input string size.");
      memcpy(&val, data, sizeof(val));
      val = (int64_t)T_ntohll(val);
      lualongnumber_pushlong(L, &val);
      break;
    }
    case 'd': {
      double val;
      luaL_argcheck(L, len == sizeof(val), 1, "Invalid input string size.");
      memcpy(&val, data, sizeof(val));
      lua_pushnumber(L, val);
      break;
    }
    default:
      luaL_argcheck(L, 0, 0, "Invalid format code.");
  }
  return 1;
}

static const struct luaL_Reg lua_bpack[] = {
  {"bpack", l_bpack},
  {"bunpack", l_bunpack},
  {NULL, NULL}
};

int luaopen_libluabpack(lua_State *L) {
  luaL_register(L, "libluabpack", lua_bpack);
  return 1;
}
