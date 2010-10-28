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

package org.apache.thrift.test;

import thrift.test.*;

/**
 */
public class ToStringTest {
  public static void main(String[] args) throws Exception {
    JavaTestHelper object = new JavaTestHelper();
    object.req_int = 0;
    object.req_obj = "";


    object.req_bin = new byte[] {
      0, -1, 2, -3, 4, -5, 6, -7, 8, -9, 10, -11, 12, -13, 14, -15,
      16, -17, 18, -19, 20, -21, 22, -23, 24, -25, 26, -27, 28, -29,
      30, -31, 32, -33, 34, -35, 36, -37, 38, -39, 40, -41, 42, -43, 44,
      -45, 46, -47, 48, -49, 50, -51, 52, -53, 54, -55, 56, -57, 58, -59,
      60, -61, 62, -63, 64, -65, 66, -67, 68, -69, 70, -71, 72, -73, 74,
      -75, 76, -77, 78, -79, 80, -81, 82, -83, 84, -85, 86, -87, 88, -89,
      90, -91, 92, -93, 94, -95, 96, -97, 98, -99, 100, -101, 102, -103,
      104, -105, 106, -107, 108, -109, 110, -111, 112, -113, 114, -115,
      116, -117, 118, -119, 120, -121, 122, -123, 124, -125, 126, -127,
    };

    if (!object.toString().equals(
        "JavaTestHelper(req_int:0, req_obj:, req_bin:"+
        "00 FF 02 FD 04 FB 06 F9 08 F7 0A F5 0C F3 0E F1 10 EF 12 ED 14 "+
        "EB 16 E9 18 E7 1A E5 1C E3 1E E1 20 DF 22 DD 24 DB 26 D9 28 D7 "+
        "2A D5 2C D3 2E D1 30 CF 32 CD 34 CB 36 C9 38 C7 3A C5 3C C3 3E "+
        "C1 40 BF 42 BD 44 BB 46 B9 48 B7 4A B5 4C B3 4E B1 50 AF 52 AD "+
        "54 AB 56 A9 58 A7 5A A5 5C A3 5E A1 60 9F 62 9D 64 9B 66 99 68 "+
        "97 6A 95 6C 93 6E 91 70 8F 72 8D 74 8B 76 89 78 87 7A 85 7C 83 "+
        "7E 81)")) {
      throw new RuntimeException();
    }
 
    object.req_bin = new byte[] {
      0, -1, 2, -3, 4, -5, 6, -7, 8, -9, 10, -11, 12, -13, 14, -15,
      16, -17, 18, -19, 20, -21, 22, -23, 24, -25, 26, -27, 28, -29,
      30, -31, 32, -33, 34, -35, 36, -37, 38, -39, 40, -41, 42, -43, 44,
      -45, 46, -47, 48, -49, 50, -51, 52, -53, 54, -55, 56, -57, 58, -59,
      60, -61, 62, -63, 64, -65, 66, -67, 68, -69, 70, -71, 72, -73, 74,
      -75, 76, -77, 78, -79, 80, -81, 82, -83, 84, -85, 86, -87, 88, -89,
      90, -91, 92, -93, 94, -95, 96, -97, 98, -99, 100, -101, 102, -103,
      104, -105, 106, -107, 108, -109, 110, -111, 112, -113, 114, -115,
      116, -117, 118, -119, 120, -121, 122, -123, 124, -125, 126, -127,
      0,
    };

    if (!object.toString().equals(
        "JavaTestHelper(req_int:0, req_obj:, req_bin:"+
        "00 FF 02 FD 04 FB 06 F9 08 F7 0A F5 0C F3 0E F1 10 EF 12 ED 14 "+
        "EB 16 E9 18 E7 1A E5 1C E3 1E E1 20 DF 22 DD 24 DB 26 D9 28 D7 "+
        "2A D5 2C D3 2E D1 30 CF 32 CD 34 CB 36 C9 38 C7 3A C5 3C C3 3E "+
        "C1 40 BF 42 BD 44 BB 46 B9 48 B7 4A B5 4C B3 4E B1 50 AF 52 AD "+
        "54 AB 56 A9 58 A7 5A A5 5C A3 5E A1 60 9F 62 9D 64 9B 66 99 68 "+
        "97 6A 95 6C 93 6E 91 70 8F 72 8D 74 8B 76 89 78 87 7A 85 7C 83 "+
        "7E 81 ...)")) {
      throw new RuntimeException();
    } 

    object.req_bin = new byte[] {};
    object.setOpt_binIsSet(true);


    if (!object.toString().equals(
        "JavaTestHelper(req_int:0, req_obj:, req_bin:)")) {
      throw new RuntimeException();
    } 
  }
}

