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

import Foundation

public class TBase64Utils {
    private static let EncodeTable: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    private static let NA: UInt8 = UInt8(255)

    private static let DecodeTable: [UInt8] = [
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 62, NA, NA, NA, 63,
        52, 53, 54, 55, 56, 57, 58, 59, 60, 61, NA, NA, NA, NA, NA, NA,
        NA, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
        15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, NA, NA, NA, NA, NA,
        NA, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
        41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    ]

    static func encode(src: [UInt8], srcOff: Int, len: Int, dst: inout [UInt8], dstOff: Int) {
        if (src.count == 0) {
            return
        }
        var index: UInt8 = 0

        index = src[srcOff] >> 2 & 0x3F
        dst[dstOff] = EncodeTable[Int(index)].asciiValue!

        if (len == 3) {
            index = ((src[srcOff] << 4) & 0x30) | ((src[srcOff + 1] >> 4) & 0x0F)
            dst[dstOff + 1] = EncodeTable[Int(index)].asciiValue!

            index = ((src[srcOff + 1] << 2) & 0x3C) | ((src[srcOff + 2] >> 6) & 0x03)
            dst[dstOff + 2] = EncodeTable[Int(index)].asciiValue!

            index = (src[srcOff + 2]  & 0x3F)
            dst[dstOff + 3] = EncodeTable[Int(index)].asciiValue!
        } else if (len == 2) {
            index = ((src[srcOff] << 4) & 0x30) | ((src[srcOff + 1] >> 4) & 0x0F)
            dst[dstOff + 1] = EncodeTable[Int(index)].asciiValue!

            index = ((src[srcOff + 1] << 2) & 0x3C)
            dst[dstOff + 2] = EncodeTable[Int(index)].asciiValue!
        } else {
            // len == 1
            index = ((src[srcOff] << 4) & 0x30)
            dst[dstOff + 1] = EncodeTable[Int(index)].asciiValue!
        }
    }

    static func decode(src: [UInt8], srcOff: Int, len: Int, dst: inout [UInt8], dstOff: Int) {
        if (src.count == 0) {
            return
        }

        dst[dstOff] =  (DecodeTable[Int(src[srcOff] & 0x0FF)] << 2) | (DecodeTable[Int(src[srcOff + 1] & 0x0FF)] >> 4)
        if (len > 2) {
            dst[dstOff + 1] =  ((DecodeTable[Int(src[srcOff + 1] & 0x0FF)] << 4) & 0xF0) | (DecodeTable[Int(src[srcOff + 2] & 0x0FF)] >> 2)
            if (len > 3) {
                dst[dstOff + 2] =  ((DecodeTable[Int(src[srcOff + 2] & 0x0FF)] << 6) & 0xC0) | (DecodeTable[Int(src[srcOff + 3] & 0x0FF)])
            }
        }
    }
}
