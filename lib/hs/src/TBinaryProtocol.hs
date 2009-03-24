module TBinaryProtocol (TBinaryProtocol(..)) where
    import Thrift
    import Data.Bits
    import Data.Int
    import GHC.Exts
    import GHC.Prim
    import GHC.Word
    import Control.Exception

    data TBinaryProtocol a = TTransport a => TBinaryProtocol a

    version_mask = 0xffff0000
    version_1 = 0x80010000;

    getByte :: Bits a => a -> Int -> a
    getByte i b = 255 .&. (shiftR i (8*b))

    getBytes :: (Bits a, Integral a) => a -> Int -> String
    getBytes i 0 = []
    getBytes i n = (toEnum $ fromIntegral $ getByte i (n-1)):(getBytes i (n-1))

    floatBits :: Double -> Word64
    floatBits (D# d#) = W64# (unsafeCoerce# d#)

    floatOfBits :: Word64 -> Double
    floatOfBits (W64# b#) = D# (unsafeCoerce# b#)

    composeBytesH :: [Char] -> Int -> Word32
    composeBytesH [] n = 0
    composeBytesH (h:t) n = (shiftL (fromIntegral (fromEnum h) :: Word32) (8*n)) .|. (composeBytesH t (n-1))
    compBytes :: [Char] -> Word32
    compBytes b = composeBytesH b ((length b)-1)

    composeBytes64H :: [Char] -> Int -> Word64
    composeBytes64H [] n = 0
    composeBytes64H (h:t) n = (shiftL (fromIntegral (fromEnum h) :: Word64) (8*n)) .|. (composeBytes64H t (n-1))
    compBytes64 :: [Char] -> Word64
    compBytes64 b = composeBytes64H b ((length b)-1)
    instance Protocol TBinaryProtocol where
        getTransport (TBinaryProtocol t) = t
        writeBool (TBinaryProtocol tr) b = twrite tr (if b then [toEnum 1::Char] else [toEnum 0::Char])
        writeByte (TBinaryProtocol tr) b = twrite tr (getBytes b 1)
        writeI16 (TBinaryProtocol tr) b = twrite tr (getBytes b 2)
        writeI32 (TBinaryProtocol tr) b = twrite tr (getBytes b 4)
        writeI64 (TBinaryProtocol tr) b = twrite tr (getBytes b 8)
        writeDouble (TBinaryProtocol tr) b = writeI64 (TBinaryProtocol tr) (fromIntegral (floatBits b) :: Int64)
        writeString (TBinaryProtocol tr) s = do twrite tr (getBytes (length s) 4)
                                                twrite tr s
        writeBinary = writeString
        writeMessageBegin (TBinaryProtocol tr) (n,t,s) = do writeI32 (TBinaryProtocol tr) (version_1 .|. (fromEnum t))
                                                            writeString (TBinaryProtocol tr) n
                                                            writeI32 (TBinaryProtocol tr) s
        writeMessageEnd (TBinaryProtocol tr) = return ()
        writeStructBegin (TBinaryProtocol tr) s = return ()
        writeStructEnd (TBinaryProtocol tr) = return ()
        writeFieldBegin a (n,t,i) = do writeByte a (fromEnum t)
                                       writeI16 a i
        writeFieldEnd a = return ()
        writeFieldStop a = writeByte a (fromEnum T_STOP)
        writeMapBegin a (k,v,s) = do writeByte a (fromEnum k)
                                     writeByte a (fromEnum v)
                                     writeI32 a s
        writeMapEnd a = return ()
        writeListBegin a (t,s) = do writeByte a (fromEnum t)
                                    writeI32 a s
        writeListEnd a = return ()
        writeSetBegin = writeListBegin
        writeSetEnd a = return ()
        readByte (TBinaryProtocol tr) = do b <- treadAll tr 1
                                           return $ (fromIntegral (fromIntegral (compBytes b) :: Int8) :: Int)
        readI16 (TBinaryProtocol tr) = do b <- treadAll tr 2
                                          return $ (fromIntegral (fromIntegral (compBytes b) :: Int16) :: Int)
        readI32 (TBinaryProtocol tr) = do b <- treadAll tr 4
                                          return $ (fromIntegral (fromIntegral (compBytes b) :: Int32) :: Int)
        readI64 (TBinaryProtocol tr) = do b <- treadAll tr 8
                                          return $ (fromIntegral (compBytes64 b) :: Int64)
        readDouble (TBinaryProtocol tr) = do b <- readI64 (TBinaryProtocol tr)
                                             return $ floatOfBits (fromIntegral b :: Word64)
        readBool (TBinaryProtocol tr) = do b <- readByte (TBinaryProtocol tr)
                                           return $ b == 1
        readString (TBinaryProtocol tr) = do l <- readI32 (TBinaryProtocol tr)
                                             treadAll tr l
        readBinary = readString
        readMessageBegin (TBinaryProtocol tr) = do ver <- readI32 (TBinaryProtocol tr)
                                                   if (ver .&. version_mask /= version_1) then
                                                       throwDyn (ProtocolExn PE_BAD_VERSION "Missing version identifier")
                                                       else do
                                                         s <- readString (TBinaryProtocol tr)
                                                         sz <- readI32 (TBinaryProtocol tr)
                                                         return (s,toEnum (ver .&. 0xFF) :: Message_type,fromIntegral sz :: Int)
        readMessageEnd (TBinaryProtocol tr) = return ()
        readStructBegin (TBinaryProtocol tr) = return ""
        readStructEnd (TBinaryProtocol tr) = return ()
        readFieldBegin (TBinaryProtocol tr) = do t <- readByte (TBinaryProtocol tr)
                                                 if (toEnum t :: T_type) /= T_STOP then
                                                     do s <- readI16 (TBinaryProtocol tr)
                                                        return ("",toEnum t :: T_type,fromIntegral s :: Int)
                                                     else return ("",toEnum t :: T_type,0)
        readFieldEnd (TBinaryProtocol tr) = return ()
        readMapBegin a = do kt <- readByte a
                            vt <- readByte a
                            s <- readI32 a
                            return (toEnum kt :: T_type,toEnum vt :: T_type,fromIntegral s :: Int)
        readMapEnd a = return ()
        readListBegin a = do b <- readByte a
                             s <- readI32 a
                             return (toEnum b :: T_type,fromIntegral s :: Int)
        readListEnd a = return ()
        readSetBegin = readListBegin
        readSetEnd = readListEnd



