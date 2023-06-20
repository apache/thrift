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

/**
    JSON protocol implementation for thrift.
    This is a full-feature protocol supporting Write and Read.
    Please see the C++ class header for a detailed description of the protocol's wire format
    Adapted from netstd C# version
 */
public class TJSONProtocol: TProtocol {
    static let Version: Int = 1

    public var transport: TTransport

    // Temporary buffer used by several methods
    private var tempBuffer: [UInt8] = [0,0,0,0]
    private var contextStack: JSONContextStack = JSONContextStack()
    private var currentContext: JSONBaseContext?
    private var context: JSONBaseContext {
        get throws {
            if (currentContext != nil) {
                return currentContext!
            }
            throw TProtocolError(error: .depthLimit, message: "Current context is nil")
        }
    }

    /**
     Reader that manages a 1-byte buffer
     */
    private var optionalReader: LookaheadReader?
    private var reader: LookaheadReader {
        get throws {
            if (optionalReader != nil) {
                return optionalReader!
            }
            throw TProtocolError(error: .depthLimit, message: "Lookahead reader is nil")
        }
    }

    // MARK: TJSONProtocol Constructor
    public required init(on transport: TTransport) {
        self.transport = transport
        currentContext = JSONBaseContext(on: self)
        optionalReader = LookaheadReader(on: self)
    }

    // MARK: TJSONProtocol helpers
    /**
     Push a new JSON context onto the context stack
     */
    func pushContext(_ context: JSONBaseContext) {
        contextStack.push(context)
        currentContext = context
    }

    /**
     Pop current JSON context from the context stack
     */
    func popContext() {
        _ = contextStack.pop()
        currentContext = contextStack.isEmpty() ? JSONBaseContext(on: self) : contextStack.peek()
    }

    /**
     Reset context stack to pristine state. Allows for reusal of the protocol even in cases where the protocol instance
     was in an undefined state due to dangling/stale/obsolete contexts
     */
    func resetContext() {
        contextStack.clear()
        currentContext = JSONBaseContext(on: self)
    }

    /**
     Read a byte that must match bytes[0]; otherwise an exception is thrown.
     - bytes: Input bytes array
     */
    func readJsonSyntaxChar(bytes: [UInt8]) throws {
        let ch: UInt8 = try reader.read()
        if (ch != bytes[0]) {
            throw TProtocolError(error: .invalidData, message: "Unexpected character: \(ch.asCharacter())")
        }
    }

    /**
     Write the bytes in array buffer as a JSON characters, escaping as needed
     */
    func writeJsonString(bytes: [UInt8]) throws {
        try context.writeConditionalDelimiter()
        try transport.writeJSONQuote()

        let len: Int = bytes.count
        for i in 0..<len {
            if (bytes[i] & 0x00FF >= 0x30) {
                if (bytes[i] == TJSONProtocolConstants.Backslash[0]) {
                    try transport.writeJSONBackslash()
                    try transport.writeJSONBackslash()
                } else {
                    try transport.write(data: Data(bytes: [bytes[i]], count: 1))
                }
            } else {
                tempBuffer[0] = TJSONProtocolConstants.JsonCharTable[Int(bytes[i])]
                if (tempBuffer[0] == 1) {
                    try transport.write(data: Data(bytes: [bytes[i]], count: 1))
                } else if (tempBuffer[0] > 1) {
                    try transport.writeJSONBackslash()
                    try transport.write(data: Data(bytes: [tempBuffer[0]], count: 1))
                } else {
                    try transport.writeJSONEscSequences()
                    tempBuffer[0] = (bytes[i] >> 4).toHexChar()
                    tempBuffer[1] = (bytes[i]).toHexChar()
                    try transport.write(data: Data(bytes: [tempBuffer[0], tempBuffer[1]], count:2))
                }
            }
        }
        try transport.writeJSONQuote()
    }

    /**
     Write out number as a JSON value. If the context dicates so, it will be wrapped in quotes to output as a JSON string.
     */
    func writeJsonInteger(num: Int64) throws {
        try context.writeConditionalDelimiter()
        let str: String = String(num)

        let escapeNum: Bool = try context.escapeNumbers()
        if (escapeNum) {
            try transport.write(data: Data(bytes: TJSONProtocolConstants.Quote, count: TJSONProtocolConstants.Quote.count))
        }

        let strData: Data = str.data(using: .utf8)!
        try transport.write(data: strData)

        if (escapeNum) {
            try transport.write(data: Data(bytes: TJSONProtocolConstants.Quote, count: TJSONProtocolConstants.Quote.count))
        }
    }

    /**
     Write out a double as a JSON value. If it is Nan or Infinity or if the context dictates escaping, write out as JSON string.
     */
    func writeJsonDouble(num: Double) throws {
        try context.writeConditionalDelimiter()
        let str = String(num)
        var special = false

        switch(str[0]) {
        case "N", "I":
            // Nan or Infinity
            special = true
        case "-":
            if (str[1] == "I") {
                // -Infinity
                special = true
            }
        default:
            special = false
        }

        let escapeNum = try context.escapeNumbers()
        let escapeNumOrSpecial = special || escapeNum
        if (escapeNumOrSpecial) {
            try transport.writeJSONQuote()
        }

        if let strData = str.data(using: .utf8) {
            try transport.write(data: strData)
        } else {
            throw TProtocolError(error: .invalidData, message: "Cannot convert double number to data bytes")
        }

        if (escapeNumOrSpecial) {
            try transport.writeJSONQuote()
        }
    }

    /**
     Write out contents of byte array as a JSON string with base-64 encoded data
     */
    func writeJsonBase64(bytes: [UInt8]) throws {
        try context.writeConditionalDelimiter()
        try transport.writeJSONQuote()

        var len = bytes.count
        var off = 0
        while (len >= 3) {
            // Encode 3 bytes at a time
            TBase64Utils.encode(src: bytes, srcOff: off, len: 3, dst: &tempBuffer, dstOff: 0)
            try transport.write(data: Data(bytes: tempBuffer, count: 4))
            off += 3
            len -= 3
        }

        if (len > 0) {
            // Encode remainder
            TBase64Utils.encode(src: bytes, srcOff: off, len: len, dst: &tempBuffer, dstOff: 0)
            try transport.write(data: Data(bytes: tempBuffer, count: len + 1))
        }

        try transport.writeJSONQuote()
    }

    func writeJsonObjectStart() throws {
        try context.writeConditionalDelimiter()
        try transport.writeJSONLeftBrace()
        pushContext(JSONPairContext(on: self))
    }

    func writeJsonObjectEnd() throws {
        popContext()
        try transport.writeJSONRightBrace()
    }

    func writeJsonArrayStart() throws {
        try context.writeConditionalDelimiter()
        try transport.writeJSONLeftBracket()
        pushContext(JSONListContext(on: self))
    }

    func writeJsonArrayEnd() throws {
        popContext()
        try transport.writeJSONRightBracket()
    }

    /**
     Read in a JSON string, unescaping as appropriate. Skip reading from the context if skipContext is true.
     */
    func readJsonString(skipContext: Bool) throws -> [UInt8] {
        var codeunits: [Character] = []

        if (!skipContext) {
            try context.readConditionalDelimiter()
        }
        try readJsonSyntaxChar(bytes: TJSONProtocolConstants.Quote)

        var dataBuffer = Data()
        while (true) {
            var ch: UInt8 = try reader.read()
            if (ch == TJSONProtocolConstants.Quote[0]) {
                break
            }

            // Escaped?
            if (ch != TJSONProtocolConstants.EscSequences[0]) {
                dataBuffer.append([ch], count: 1)
                continue
            }

            // distinguish between \uXXXX and \?
            ch = try reader.read()
            if (ch != TJSONProtocolConstants.EscSequences[1]) { // control chars like \n
                guard let off: Int = TJSONProtocolConstants.EscSequences.firstIndex(of: ch) else {
                    throw TProtocolError(error: .invalidData, message: "Expected control char")
                }
                ch = TJSONProtocolConstants.EscapeCharValues[off]
                dataBuffer.append([ch], count: 1)
                continue
            }

            // It's \uXXXX
            let tempData: Data = try transport.readAll(size: 4)
            let wch = Int16( ((tempData[0]).toHexChar() << 12) +
                             ((tempData[1]).toHexChar() << 8) +
                             ((tempData[2]).toHexChar() << 4) +
                             ((tempData[3]).toHexChar()) )
            guard let wchScalar = UnicodeScalar(Int(wch)) else {
                throw TProtocolError(error: .invalidData, message: "Expected Unicode character")
            }

            if (try wch.magnitude.isHighSurrogate()) {
                if (codeunits.count > 0) {
                    throw TProtocolError(error: .invalidData, message: "Exptected low surrogate char")
                }
                codeunits.append(Character(wchScalar))
            } else if (try wch.magnitude.isLowSurrogate()) {
                if (codeunits.count == 0) {
                    throw TProtocolError(error: .invalidData, message: "Exptected high surrogate char")
                }
                codeunits.append(Character(wchScalar))
                guard let codeunitsData = String(codeunits).data(using: .utf8) else {
                    throw TProtocolError(error: .invalidData, message: "Codeunits cannot be converted to string bytes")
                }
                dataBuffer.append(codeunitsData)
                codeunits.removeAll()
            } else {
                let bytesArray: [UInt8] = withUnsafeBytes(of: wch.bigEndian, Array.init)
                dataBuffer.append(Data(bytes: bytesArray, count: bytesArray.count))
            }
         }

        if (codeunits.count > 0) {
            throw TProtocolError(error: .invalidData, message: "Expected low surrogate char")
        }

        let bytesResult: [UInt8] = dataBuffer.map { $0 }
        return bytesResult
    }

    /**
     Read in a sequence of characters that are all valid in JSON numbers. Does not do a complete regex check
     to validate that this is actually a number.
     */
    func readJsonNumericChars() throws -> String {
        var str = ""
        while(true) {
            // TODO: Workaround for primitive types with TJSONProtocol: think - how to rewrite into more easy from without exception
            do {
                let ch: UInt8 = try reader.peek()
                if (!ch.isJsonNumeric()) {
                    break
                }
                let c = try reader.read()
                str.append(c.asCharacter())
            } catch is TTransportError {
                break
            }
            catch let error {
                throw error
            }
        }
        return str
    }

    /**
     Read in a JSON number.  If the context dictates, read in enclosing quotes.
     */
    func readJsonInteger() throws -> Int64 {
        try context.readConditionalDelimiter()
        let escapeNum = try context.escapeNumbers()
        if (escapeNum) {
            try readJsonSyntaxChar(bytes: TJSONProtocolConstants.Quote)
        }

        let str: String = try readJsonNumericChars()
        if (escapeNum) {
            try readJsonSyntaxChar(bytes: TJSONProtocolConstants.Quote)
        }

        guard let result = Int64(str) else { throw TProtocolError(error: .invalidData, message: "Cannot convert \(str) to Int64") }
        return result
    }

    /**
     Read in a JSON double value. Throw if the value is not wrapped in quotes when expected or if wrapped in quotes when not expected.
     */
    func readJsonDouble() throws -> Double {
        try context.readConditionalDelimiter()

        let escapeNum = try context.escapeNumbers()
        if (try reader.peek() == TJSONProtocolConstants.Quote[0]) {
            let arr: [UInt8] = try readJsonString(skipContext: true)
            if let str: String = String(data: Data(arr), encoding: .utf8),
               let dub = Double(str) {
                if (!escapeNum && !dub.isNaN && !dub.isInfinite) {
                    throw TProtocolError(error: .invalidData, message: "Numeric data unexpectedly quoted")
                }
                return dub
            } else {
                throw TProtocolError(error: .invalidData, message: "Numeric data convertion to double failed")
            }
        }

        if (escapeNum) {
            try readJsonSyntaxChar(bytes: TJSONProtocolConstants.Quote)
        }

        let str: String = try readJsonNumericChars()
        if let dub = Double(str) {
            return dub
        } else {
            throw TProtocolError(error: .invalidData, message: "Numeric data convertion to double failed")
        }
    }

    /**
     Read in a JSON string containing base-64 encoded data and decode it.
     */
    func readJsonBase64() throws -> [UInt8] {
        var b = try readJsonString(skipContext: false)
        var len = b.count
        var off = 0
        var size = 0

        // Reduce len to ignore fill bytes
        while( (len > 0) && (b[len - 1] == "=".asciiBytes()[0]) ) {
            len -= 1
        }

        // Read & decode full byte triplets = 4 source bytes
        while (len > 4) {
            // Decode 4 bytes at a time
            TBase64Utils.decode(src: b, srcOff: off, len: 4, dst: &b, dstOff: size) // Nb: decode in place
            off += 4
            len -= 4
            size += 3
        }

        // Don't decode if we hit the end or got a single leftover byte
        // (invalid base64 but legal for skip of reqular string exType)
        if (len > 1) {
            // Decode remainder
            TBase64Utils.decode(src: b, srcOff: off, len: len, dst: &b, dstOff: size) // NB: decode in place
            size += len - 1
        }

        let result: [UInt8] = Array(b[0..<size])
        return result
    }

    func readJsonObjectStart() throws {
        try context.readConditionalDelimiter()
        try readJsonSyntaxChar(bytes: TJSONProtocolConstants.LeftBrace)
        pushContext(JSONPairContext(on: self))
    }

    func readJsonObjectEnd() throws {
        try readJsonSyntaxChar(bytes: TJSONProtocolConstants.RightBrace)
        popContext()
    }

    func readJsonArrayStart() throws {
        try context.readConditionalDelimiter()
        try readJsonSyntaxChar(bytes: TJSONProtocolConstants.LeftBracket)
        pushContext(JSONListContext(on: self))
    }

    func readJsonArrayEnd() throws {
        try readJsonSyntaxChar(bytes: TJSONProtocolConstants.RightBracket)
        popContext()
    }

    // MARK: - TProtocol
    public func readMessageBegin() throws -> (String, TMessageType, Int32) {
        resetContext()
        try readJsonArrayStart()

        let version = try readJsonInteger()
        if (version != TJSONProtocol.Version) {
            throw TProtocolError(error: .badVersion(expected: "\(TJSONProtocol.Version)", got: "\(version)"), message: "Bad version")
        }

        let buf = try readJsonString(skipContext: false)
        guard let name = String(bytes: buf, encoding: .utf8) else {
            throw TProtocolError(error: .invalidData, message: "Invalid message name")
        }
        guard let type = TMessageType(rawValue: Int32(try readJsonInteger())) else {
            throw TProtocolError(error: .invalidData, message: "Invalid message type")
        }
        let seqID = try readJsonInteger()

        return (name, type, Int32(seqID))
    }

    public func readMessageEnd() throws {
        try readJsonArrayEnd()
    }

    public func readStructBegin() throws -> String {
        try readJsonObjectStart()
        return ""
    }

    public func readStructEnd() throws {
        try readJsonObjectEnd()
    }

    public func readFieldBegin() throws -> (String, TType, Int32) {
        let ch = try reader.peek()
        if (ch == TJSONProtocolConstants.RightBrace[0]) {
            return ("", TType.stop, 0)
        }

        let fieldID = try readJsonInteger()
        try readJsonObjectStart()
        let fieldName: [UInt8] = try readJsonString(skipContext: false)
        let fieldType: TType = try TType.getTypeIdForTypeName(fieldName)
        guard let name = String(bytes: fieldName, encoding: .utf8) else {
            throw TProtocolError(error: .invalidData, message: "Invalid field name")
        }
        return (name, fieldType, Int32(fieldID))
    }

    public func readFieldEnd() throws {
        try readJsonObjectEnd()
    }

    public func readMapBegin() throws -> (TType, TType, Int32) {
        try readJsonArrayStart()
        let keyTypeName = try readJsonString(skipContext: false)
        let keyType = try TType.getTypeIdForTypeName(keyTypeName)

        let valueTypeName = try readJsonString(skipContext: false)
        let valueType = try TType.getTypeIdForTypeName(valueTypeName)

        let count = try readJsonInteger()

        try checkReadBytesAvailable(keyType: keyType, valueType: valueType, count: Int32(count))
        try readJsonObjectStart()
        return (keyType, valueType, Int32(count))
    }

    public func readMapEnd() throws {
        try readJsonObjectEnd()
        try readJsonArrayEnd()
    }

    public func readSetBegin() throws -> (TType, Int32) {
        try readJsonArrayStart()

        let elementTypeName = try readJsonString(skipContext: false)
        let elementType = try TType.getTypeIdForTypeName(elementTypeName)

        let count = try readJsonInteger()

        try checkReadBytesAvailable(elementType, Int32(count))

        return (elementType, Int32(count))
    }

    public func readSetEnd() throws {
        try readJsonArrayEnd()
    }

    public func readListBegin() throws -> (TType, Int32) {
        try readJsonArrayStart()

        let elementTypeName = try readJsonString(skipContext: false)
        let elementType = try TType.getTypeIdForTypeName(elementTypeName)

        let count = try readJsonInteger()

        try checkReadBytesAvailable(elementType, Int32(count))
        return (elementType, Int32(count))
    }

    public func readListEnd() throws {
        try readJsonArrayEnd()
    }

    public func read() throws -> String {
        let buf = try readJsonString(skipContext: false)
        guard let str = String(bytes: buf, encoding: .utf8) else {
            throw TProtocolError(error: .invalidData, message: "Cannot convert bytes to string")
        }
        return str
    }

    public func read() throws -> Bool {
        let intValue = try readJsonInteger()
        return intValue == 0 ? false : true
    }

    public func read() throws -> UInt8 {
        return UInt8(try readJsonInteger())
    }

    public func read() throws -> Int8 {
        return Int8(try readJsonInteger())
    }

    public func read() throws -> Int16 {
        return Int16(try readJsonInteger())
    }

    public func read() throws -> Int32 {
        return Int32(try readJsonInteger())
    }

    public func read() throws -> Int64 {
        return try readJsonInteger()
    }

    public func read() throws -> Double {
        return try readJsonDouble()
    }

    public func read() throws -> Data {
        let base64Bytes = try readJsonBase64()
        return Data(bytes: base64Bytes, count: base64Bytes.count)
    }

    public func read() throws -> UUID {
        let buf = try readJsonString(skipContext: false)
        guard let id = String(bytes: buf, encoding: .utf8) else {
            throw TProtocolError(error: .invalidData, message: "Cannot convert bytes to string")
        }
        guard let uuid =  UUID(uuidString: id) else {
            throw TProtocolError(error: .invalidData, message: "Cannot convert string to uuid")
        }
        return uuid
    }

    public func writeMessageBegin(name: String, type messageType: TMessageType, sequenceID: Int32) throws {
        resetContext()
        try writeJsonArrayStart()
        try writeJsonInteger(num: Int64(TJSONProtocol.Version))

        guard let nameData = name.data(using: .utf8) else {
            throw TProtocolError(error: .invalidData, message: "Cannot convert message name to bytes data")
        }
        try writeJsonString(bytes: [UInt8] (nameData))

        try writeJsonInteger(num: Int64(messageType.rawValue))
        try writeJsonInteger(num: Int64(sequenceID))
    }

    public func writeMessageEnd() throws {
        try writeJsonArrayEnd()
    }

    public func writeStructBegin(name: String) throws {
        try writeJsonObjectStart()
    }

    public func writeStructEnd() throws {
        try writeJsonObjectEnd()
    }

    public func writeFieldBegin(name: String, type fieldType: TType, fieldID: Int32) throws {
        try writeJsonInteger(num: Int64(fieldID))

        try writeJsonObjectStart()

        let fieldTypeName = try fieldType.getTypeNameForTypeId()
        try writeJsonString(bytes: fieldTypeName)
    }

    public func writeFieldStop() throws {
        // Nop
    }

    public func writeFieldEnd() throws {
        try writeJsonObjectEnd()
    }

    public func writeMapBegin(keyType: TType, valueType: TType, size: Int32) throws {
        try writeJsonArrayStart()

        let mapKeyTypeName = try keyType.getTypeNameForTypeId()
        try writeJsonString(bytes: mapKeyTypeName)

        let mapValueTypeName = try valueType.getTypeNameForTypeId()
        try writeJsonString(bytes: mapValueTypeName)

        try writeJsonInteger(num: Int64(size))

        try writeJsonObjectStart()
    }

    public func writeMapEnd() throws {
        try writeJsonObjectEnd()
        try writeJsonArrayEnd()
    }

    public func writeSetBegin(elementType: TType, size: Int32) throws {
        try writeJsonArrayStart()

        let elementTypeName = try elementType.getTypeNameForTypeId()
        try writeJsonString(bytes: elementTypeName)

        try writeJsonInteger(num: Int64(size))
    }

    public func writeSetEnd() throws {
        try writeJsonArrayEnd()
    }

    public func writeListBegin(elementType: TType, size: Int32) throws {
        try writeJsonArrayStart()

        let elementTypeName = try elementType.getTypeNameForTypeId()
        try writeJsonString(bytes: elementTypeName)

        try writeJsonInteger(num: Int64(size))
    }

    public func writeListEnd() throws {
        try writeJsonArrayEnd()
    }

    public func write(_ value: String) throws {
        guard let strData = value.data(using: .utf8) else {
            throw TProtocolError(error: .invalidData, message: "Cannot convert string value to bytes data")
        }

        try writeJsonString(bytes: [UInt8](strData))
    }

    public func write(_ value: Bool) throws {
        try writeJsonInteger(num: value ? 1 : 0)
    }

    public func write(_ value: UInt8) throws {
        try writeJsonInteger(num: Int64(value))
    }

    public func write(_ value: Int8) throws {
        try writeJsonInteger(num: Int64(value))
    }

    public func write(_ value: Int16) throws {
        try writeJsonInteger(num: Int64(value))
    }

    public func write(_ value: Int32) throws {
        try writeJsonInteger(num: Int64(value))
    }

    public func write(_ value: Int64) throws {
        try writeJsonInteger(num: value)
    }

    public func write(_ value: Double) throws {
        try writeJsonDouble(num: value)
    }

    public func write(_ value: Data) throws {
        try writeJsonBase64(bytes: [UInt8](value))
    }

    public func write(_ value: UUID) throws {
        guard let strData = value.uuidString.data(using: .utf8) else {
            throw TProtocolError(error: .invalidData, message: "Cannot convert UUID value to bytes data")
        }

        try writeJsonString(bytes: [UInt8](strData))
    }

    // MARK: - Private functions
    private func checkReadBytesAvailable(keyType: TType, valueType: TType, count: Int32) throws {
        let elmSize = try getMinSerializedSize(keyType) + getMinSerializedSize(valueType)
        _ = count * elmSize
        // TODO: implement checkReadBytesAvailable in TTransport
        // transport.checkReadBytesAvailable(size: count * elmSize)
    }

    private func checkReadBytesAvailable(_ elementType: TType, _ count: Int32) throws {
        let elmSize = try getMinSerializedSize(elementType)
        _ = count * elmSize
        // TODO: implement checkReadBytesAvailable in TTransport
        // transport.checkReadBytesAvailable(size: count * elmSize)
    }

    private func getMinSerializedSize(_ type: TType) throws -> Int32  {
        switch(type) {
        case .stop, .void: return 0
        case .bool, .i8, .i16, .i32, .i64, .double: return 1
        case .string, .struct, .map, .set, .list: return 2 // empty object
        default:
            throw TProtocolError(error: .invalidData, message: "Invalid TType")
        }
    }

    // MARK: - TJSONProtocol inner classes
    /*
     Base class for tracking JSON contexts that may require
     inserting/reading additional JSON syntax characters
     This base context does nothing
     */
    class JSONBaseContext {
        var proto: TJSONProtocol

        init(on proto: TJSONProtocol) {
            self.proto = proto
        }

        func writeConditionalDelimiter() throws {
        }

        func readConditionalDelimiter() throws {
        }

        func escapeNumbers() -> Bool {
            return false
        }
    }

    /*
     Context for JSON lists. will insert/read commas before each item except for the first one
     */
    class JSONListContext: JSONBaseContext {
        private var first: Bool = true

        override init(on proto: TJSONProtocol) {
            super.init(on: proto)
        }

        override func writeConditionalDelimiter() throws {
            if (first) {
                first = false
            } else {
                try proto.transport.writeJSONComma()
            }
        }

        override func readConditionalDelimiter() throws {
            if (first) {
                first = false
            } else {
                try proto.readJsonSyntaxChar(bytes: TJSONProtocolConstants.Comma)
            }
        }
    }

    /*
     Context for JSON records. Will insert/read colons before the value portion of each record pair,
     and commas before each key except the first. In addition, will indicate that numbers in the key position
     need to be escaped in quotes (since JSON keys must be strings).
     */
    class JSONPairContext : JSONBaseContext {
        private var colon: Bool = true
        private var first: Bool = true

        override init(on proto: TJSONProtocol) {
            super.init(on: proto)
        }

        override func writeConditionalDelimiter() throws {
            if (first) {
                first = false
                colon = true
            } else {
                if (colon) {
                    try proto.transport.writeJSONColon()
                } else {
                    try proto.transport.writeJSONComma()
                }
                self.colon = !self.colon
            }
        }

        override func readConditionalDelimiter() throws {
            if (first) {
                first = false
                colon = true
            } else {
                try proto.readJsonSyntaxChar(bytes: colon ? TJSONProtocolConstants.Colon : TJSONProtocolConstants.Comma)
                self.colon = !self.colon
            }
        }

        override func escapeNumbers() -> Bool {
            return colon
        }
    }

    class JSONContextStack {
        private var items: [JSONBaseContext] = []

        func peek() -> JSONBaseContext {
            guard let topElement = items.first else { fatalError("This stack is empty.") }
            return topElement
        }

        func pop() -> JSONBaseContext {
            return items.removeFirst()
        }

        func push(_ element: JSONBaseContext) {
            items.insert(element, at: 0)
        }

        func clear() {
            items.removeAll()
        }

        func isEmpty() -> Bool {
            return items.count == 0
        }
    }

    class LookaheadReader {
        private var byteData: UInt8?
        private var hasData: Bool = false
        var proto: TJSONProtocol

        init(on proto: TJSONProtocol) {
            self.proto = proto
        }

        func read() throws -> UInt8 {
            if (hasData) {
                hasData = false
            } else {
                let data = try proto.transport.readAll(size: 1)
                byteData = Array(data)[0]
            }
            if let byte = byteData {
                return byte
            }
            throw TProtocolError(error: .invalidData, message: "Reader does not have data to read")
        }

        func peek() throws -> UInt8 {
            if (!hasData) {
                let data = try proto.transport.readAll(size: 1)
                byteData = Array(data)[0]
                hasData = true
            }
            if let byte = byteData {
                return byte
            }
            throw TProtocolError(error: .invalidData, message: "Reader does not have data to peek")
        }
    }
}

// MARK: TJSONProtocolConstants
/**
 TJSONProtocol Constants properties/fields
 */
public struct TJSONProtocolConstants {
    public static let Comma: [UInt8] = ",".asciiBytes()
    public static let Colon: [UInt8] = ":".asciiBytes()
    public static let LeftBrace: [UInt8] = "{".asciiBytes()
    public static let RightBrace: [UInt8] = "}".asciiBytes()
    public static let LeftBracket: [UInt8] = "[".asciiBytes()
    public static let RightBracket: [UInt8] = "]".asciiBytes()
    public static let Quote: [UInt8] = "\"".asciiBytes()
    public static let Backslash: [UInt8] = "\\".asciiBytes()

    public static let JsonCharTable: [UInt8] = [
        0, 0, 0, 0, 0, 0, 0, 0, b, t, n, 0, f, r, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        1, 1, qt, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
    ]

    // \b -> \u{0008}
    // \f -> \u{000C}
    public static let EscapeChars: [Character] = ["\"", "\\", "/", "\u{0008}", "\u{000C}", "\n", "\r", "\t" ]
    public static let EscapeCharValues: [UInt8] = "\"\\/\u{0008}\u{000C}\n\r\t".asciiBytes()
    public static let EscSequences: [UInt8] = "\\u00".asciiBytes()

    public struct TypeNames {
        public static let NameBool: [UInt8] = "tf".asciiBytes()
        public static let NameByte: [UInt8] = "i8".asciiBytes()
        public static let NameI16: [UInt8] = "i16".asciiBytes()
        public static let NameI32: [UInt8] = "i32".asciiBytes()
        public static let NameI64: [UInt8] = "i64".asciiBytes()
        public static let NameDouble: [UInt8] = "dbl".asciiBytes()
        public static let NameStruct: [UInt8] = "rec".asciiBytes()
        public static let NameString: [UInt8] = "str".asciiBytes()
        public static let NameMap: [UInt8] = "map".asciiBytes()
        public static let NameList: [UInt8] = "lst".asciiBytes()
        public static let NameSet: [UInt8] = "set".asciiBytes()
    }

    // MARK: private fields helpers
    private static let b: UInt8 = "b".asciiBytes()[0]
    private static let t: UInt8 = "t".asciiBytes()[0]
    private static let n: UInt8 = "n".asciiBytes()[0]
    private static let f: UInt8 = "f".asciiBytes()[0]
    private static let r: UInt8 = "r".asciiBytes()[0]
    private static let qt: UInt8 = "\"".asciiBytes()[0]
}

// MARK: Extensions
extension String {
    public func asciiBytes() -> [UInt8] {
        var result: [UInt8] = []
        for char in self {
            result.append(char.asciiValue!)
        }
        return result
    }

    subscript(offset: Int) -> Character {
        self[index(startIndex, offsetBy: offset)]
    }
}

extension Character {
    public func asciiByte() -> UInt8 {
       return self.asciiValue!
    }
}

extension UInt8 {
    /**
     Convert a byte containing a hex value to its corresponding hex character
     */
    public func toHexChar() -> UInt8 {
        var value = self & 0x0F
        if (value < 10) {
            let zeroChar = Character("0").asciiValue!
            return value + zeroChar
        }
        value -= 10
        let aChar = Character("a").asciiValue!
        return value + aChar
    }

    public func isJsonNumeric() -> Bool {
        let numberBytes = "+-.0123456789Ee".asciiBytes()
        if (numberBytes.contains(self)) {
            return true
        }
        return false
    }

    public func asCharacter() -> Character {
        let scalar = UnicodeScalar(self)
        return Character(scalar)
    }
}

extension UInt16 {
    public func isHighSurrogate() throws -> Bool {
        let wch = self
        if let d800 = UInt16("D800", radix: 16),
           let dbff = UInt16("DBFF", radix: 16) {
            return wch >= d800 && wch <= dbff
        } else {
            throw TProtocolError(error: .invalidData, message: "isHighSurrogate failed")
        }
    }

    public func isLowSurrogate() throws -> Bool{
        let wch = self
        if let dc00 = UInt16("DC00", radix: 16),
           let dfff = UInt16("DFFF", radix: 16) {
            return wch >= dc00 && wch <= dfff
        } else {
            throw TProtocolError(error: .invalidData, message: "isLowSurrogate failed")
        }
    }
}

extension TType {
    public static func getTypeIdForTypeName(_ name: [UInt8]) throws -> TType {
        var result = TType.stop
        if (name.count > 1) {
            switch(name[0]) {
            case "t".asciiBytes()[0]:
                result = TType.bool
            case "i".asciiBytes()[0]:
                switch(name[1]) {
                case "8".asciiBytes()[0]:
                    result = TType.i8
                case "1".asciiBytes()[0]:
                    result = TType.i16
                case "3".asciiBytes()[0]:
                    result = TType.i32
                case "6".asciiBytes()[0]:
                    result = TType.i64
                default:
                    result = TType.stop
                }
            case "d".asciiBytes()[0]:
                result = TType.double
            case "l".asciiBytes()[0]:
                result = TType.list
            case "m".asciiBytes()[0]:
                result = TType.map
            case "r".asciiBytes()[0]:
                result = TType.struct
            case "s".asciiBytes()[0]:
                if (name[1] == "t".asciiBytes()[0]) {
                    result = TType.string
                } else if (name[1] == "e".asciiBytes()[0]) {
                    result = TType.set
                }
            default:
                result = TType.stop
            }
        }

        if (result == TType.stop) {
            throw TProtocolError(error: .notImplemented, message: "Unrecognized exType")
        }

        return result
    }

    public func getTypeNameForTypeId() throws -> [UInt8] {
        let typeId = self
        switch(typeId) {
        case .bool:
            return TJSONProtocolConstants.TypeNames.NameBool
        case .i8:
            return TJSONProtocolConstants.TypeNames.NameByte
        case .i16:
            return TJSONProtocolConstants.TypeNames.NameI16
        case .i32:
            return TJSONProtocolConstants.TypeNames.NameI32
        case .i64:
            return TJSONProtocolConstants.TypeNames.NameI64
        case .double:
            return TJSONProtocolConstants.TypeNames.NameDouble
        case .string:
            return TJSONProtocolConstants.TypeNames.NameString
        case .struct:
            return TJSONProtocolConstants.TypeNames.NameStruct
        case .map:
            return TJSONProtocolConstants.TypeNames.NameMap
        case .set:
            return TJSONProtocolConstants.TypeNames.NameSet
        case .list:
            return TJSONProtocolConstants.TypeNames.NameList
        default:
            throw TProtocolError(error: .invalidData, message: "TypeId: \(typeId) does not have mapping Name")
        }
    }
}

extension TTransport {
    func writeJSONColon() throws {
        try self.write(data: Data(bytes: TJSONProtocolConstants.Colon, count: TJSONProtocolConstants.Colon.count))
    }

    func writeJSONComma() throws {
        try self.write(data: Data(bytes: TJSONProtocolConstants.Comma, count: TJSONProtocolConstants.Comma.count))
    }

    func writeJSONQuote() throws {
        try self.write(data: Data(bytes: TJSONProtocolConstants.Quote, count: TJSONProtocolConstants.Quote.count))
    }

    func writeJSONBackslash() throws {
        try self.write(data: Data(bytes: TJSONProtocolConstants.Backslash, count: TJSONProtocolConstants.Backslash.count))
    }

    func writeJSONEscSequences() throws {
        try self.write(data: Data(bytes: TJSONProtocolConstants.EscSequences, count: TJSONProtocolConstants.EscSequences.count))
    }

    func writeJSONLeftBrace() throws {
        try self.write(data: Data(bytes: TJSONProtocolConstants.LeftBrace, count: TJSONProtocolConstants.LeftBrace.count))
    }

    func writeJSONRightBrace() throws {
        try self.write(data: Data(bytes: TJSONProtocolConstants.RightBrace, count: TJSONProtocolConstants.RightBrace.count))
    }

    func writeJSONLeftBracket() throws {
        try self.write(data: Data(bytes: TJSONProtocolConstants.LeftBracket, count: TJSONProtocolConstants.LeftBracket.count))
    }

    func writeJSONRightBracket() throws {
        try self.write(data: Data(bytes: TJSONProtocolConstants.RightBracket, count: TJSONProtocolConstants.RightBracket.count))
    }
}
