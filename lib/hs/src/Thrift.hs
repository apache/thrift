module Thrift (TransportExn(..),TransportExn_Type(..),TTransport(..), T_type(..), Message_type(..), Protocol(..), AE_type(..), AppExn(..), readAppExn,writeAppExn,Thrift_exception(..), ProtocolExn(..), PE_type(..)) where
  import Data.Generics
  import Data.Int
  import Control.Exception

  data Thrift_exception = Thrift_Error deriving Typeable

  data TransportExn_Type = TE_UNKNOWN
                          | TE_NOT_OPEN
                          | TE_ALREADY_OPEN
                          | TE_TIMED_OUT
                          | TE_END_OF_FILE
                            deriving (Eq,Typeable,Show)

  data TransportExn = TransportExn [Char] TransportExn_Type  deriving (Show,Typeable)

  class TTransport a where
      tisOpen :: a -> Bool
      topen :: a -> IO a
      tclose :: a -> IO a
      tread :: a  -> Int -> IO [Char]
      twrite :: a -> [Char] ->IO ()
      tflush :: a -> IO ()
      treadAll :: a -> Int -> IO [Char]
      treadAll a 0 = return []
      treadAll a len =
          do ret <- tread a len
             case ret of
               [] -> throwDyn (TransportExn "Cannot read. Remote side has closed." TE_UNKNOWN)
               _ -> do
                   rl <- return (length ret)
                   if len <= rl then
                       return ret
                       else do r <- treadAll a (len-rl)
                               return (ret++r)


  data T_type = T_STOP
              | T_VOID
              | T_BOOL
              | T_BYTE
              | T_I08
              | T_I16
              | T_I32
              | T_U64
              | T_I64
              | T_DOUBLE
              | T_STRING
              | T_UTF7
              | T_STRUCT
              | T_MAP
              | T_SET
              | T_LIST
              | T_UTF8
              | T_UTF16
              | T_UNKNOWN
                deriving (Eq)
  instance Enum T_type where
      fromEnum t = case t of
                     T_STOP       -> 0
                     T_VOID       -> 1
                     T_BOOL       -> 2
                     T_BYTE       -> 3
                     T_I08        -> 3
                     T_I16        -> 6
                     T_I32        -> 8
                     T_U64        -> 9
                     T_I64        -> 10
                     T_DOUBLE     -> 4
                     T_STRING     -> 11
                     T_UTF7       -> 11
                     T_STRUCT     -> 12
                     T_MAP        -> 13
                     T_SET        -> 14
                     T_LIST       -> 15
                     T_UTF8       -> 16
                     T_UTF16      -> 17
                     T_UNKNOWN    -> -1
      toEnum t = case t of
                   0 -> T_STOP
                   1 -> T_VOID
                   2 -> T_BOOL
                   3 -> T_BYTE
                   6->  T_I16
                   8 -> T_I32
                   9 -> T_U64
                   10 -> T_I64
                   4 -> T_DOUBLE
                   11 -> T_STRING
                   12 -> T_STRUCT
                   13 -> T_MAP
                   14 -> T_SET
                   15 -> T_LIST
                   16 -> T_UTF8
                   17 -> T_UTF16
                   _ -> T_UNKNOWN


  data Message_type = M_CALL
                    | M_REPLY
                    | M_EXCEPTION
                    | M_UNKNOWN
                      deriving Eq
  instance Enum Message_type where

      fromEnum t = case t of
                     M_CALL -> 1
                     M_REPLY -> 2
                     M_EXCEPTION -> 3
                     M_UNKNOWN -> -1

      toEnum t = case t of
                   1 -> M_CALL
                   2 -> M_REPLY
                   3 -> M_EXCEPTION
                   _ -> M_UNKNOWN




  class Protocol a where
      pflush ::  a -> IO ()
      writeMessageBegin :: a -> ([Char],Message_type,Int) -> IO ()
      writeMessageEnd :: a -> IO ()
      writeStructBegin :: a -> [Char] -> IO ()
      writeStructEnd :: a -> IO ()
      writeFieldBegin :: a -> ([Char], T_type,Int) -> IO ()
      writeFieldEnd :: a -> IO ()
      writeFieldStop :: a -> IO ()
      writeMapBegin :: a -> (T_type,T_type,Int) -> IO ()
      writeMapEnd :: a -> IO ()
      writeListBegin :: a -> (T_type,Int) -> IO ()
      writeListEnd :: a -> IO ()
      writeSetBegin :: a -> (T_type,Int) -> IO ()
      writeSetEnd :: a -> IO ()
      writeBool :: a -> Bool -> IO ()
      writeByte :: a -> Int -> IO ()
      writeI16 :: a -> Int -> IO ()
      writeI32 :: a -> Int -> IO ()
      writeI64 :: a -> Int -> IO ()
      writeDouble :: a -> Double -> IO ()
      writeString :: a -> [Char] -> IO ()
      writeBinary :: a -> [Char] -> IO ()
      readMessageBegin :: a -> IO ([Char],Message_type,Int)
      readMessageEnd :: a -> IO ()
      readStructBegin :: a -> IO [Char]
      readStructEnd :: a -> IO ()
      readFieldBegin :: a -> IO ([Char],T_type,Int)
      readFieldEnd :: a -> IO ()
      readMapBegin :: a -> IO (T_type,T_type,Int)
      readMapEnd :: a -> IO ()
      readListBegin :: a -> IO (T_type,Int)
      readListEnd :: a -> IO ()
      readSetBegin :: a -> IO (T_type,Int)
      readSetEnd :: a -> IO ()
      readBool :: a -> IO Bool
      readByte :: a -> IO Int
      readI16 :: a -> IO Int
      readI32 :: a -> IO Int
      readI64 :: a -> IO Int
      readDouble :: a -> IO Double
      readString :: a -> IO [Char]
      readBinary :: a -> IO [Char]
      skipFields :: a -> IO ()
      skipMapEntries :: a -> Int -> T_type -> T_type -> IO ()
      skipSetEntries :: a -> Int -> T_type -> IO ()
      skip :: a -> T_type -> IO ()
      skipFields a = do (_,ty,_) <- readFieldBegin a
                        if ty == T_STOP then
                              return ()
                              else do skip a ty
                                      readFieldEnd a
                                      skipFields a
      skipMapEntries a n k v= if n == 0 then
                                    return ()
                                    else do skip a k
                                            skip a v
                                            skipMapEntries a (n-1) k v
      skipSetEntries a n k = if n == 0 then
                                   return ()
                                   else do skip a k
                                           skipSetEntries a (n-1) k
      skip a typ = case typ of
                       T_STOP -> return ()
                       T_VOID -> return ()
                       T_BOOL -> do readBool a
                                    return ()
                       T_BYTE -> do readByte a
                                    return ()
                       T_I08 -> do readByte a
                                   return ()
                       T_I16 -> do readI16 a
                                   return ()
                       T_I32 -> do readI32 a
                                   return ()
                       T_U64 -> do readI64 a
                                   return ()
                       T_I64 -> do readI64 a
                                   return ()
                       T_DOUBLE -> do readDouble a
                                      return ()
                       T_STRING -> do readString a
                                      return ()
                       T_UTF7 -> return ()
                       T_STRUCT -> do readStructBegin a
                                      skipFields a
                                      readStructEnd a
                                      return ()
                       T_MAP -> do (k,v,s) <- readMapBegin a
                                   skipMapEntries a s k v
                                   readMapEnd a
                                   return ()
                       T_SET -> do (ty,s) <- readSetBegin a
                                   skipSetEntries a s ty
                                   readSetEnd a
                                   return ()
                       T_LIST -> do (ty,s) <- readListBegin a
                                    skipSetEntries a s ty
                                    readListEnd a
                                    return ()
                       T_UTF8       -> return ()
                       T_UTF16      -> return ()
                       T_UNKNOWN    -> return ()


  data PE_type = PE_UNKNOWN
               | PE_INVALID_DATA
               | PE_NEGATIVE_SIZE
               | PE_SIZE_LIMIT
               | PE_BAD_VERSION
                 deriving (Eq, Data, Typeable)

  data ProtocolExn = ProtocolExn PE_type [Char] deriving (Typeable, Data)

  data AE_type = AE_UNKNOWN
               | AE_UNKNOWN_METHOD
               | AE_INVALID_MESSAGE_TYPE
               | AE_WRONG_METHOD_NAME
               | AE_BAD_SEQUENCE_ID
               | AE_MISSING_RESULT
                 deriving (Eq, Data, Typeable)

  instance Enum AE_type where
      toEnum i = case i of
                   0 -> AE_UNKNOWN
                   1 -> AE_UNKNOWN_METHOD
                   2 -> AE_INVALID_MESSAGE_TYPE
                   3 -> AE_WRONG_METHOD_NAME
                   4 -> AE_BAD_SEQUENCE_ID
                   5 -> AE_MISSING_RESULT
                   _ -> AE_UNKNOWN
      fromEnum t = case t of
                     AE_UNKNOWN -> 0
                     AE_UNKNOWN_METHOD -> 1
                     AE_INVALID_MESSAGE_TYPE -> 2
                     AE_WRONG_METHOD_NAME -> 3
                     AE_BAD_SEQUENCE_ID -> 4
                     AE_MISSING_RESULT -> 5

  data AppExn = AppExn {ae_type :: AE_type, ae_message :: [Char]} deriving (Typeable, Data)

  readAppExnFields pt rec = do (n,ft,id) <- readFieldBegin pt
                               if ft == T_STOP then return rec
                                      else
                                        case id of
                                          1 -> if ft == T_STRING then
                                                   do s <- readString pt
                                                      readAppExnFields pt rec{ae_message = s}
                                                   else do skip pt ft
                                                           readAppExnFields pt rec
                                          2 -> if ft == T_I32 then
                                                   do i <- readI32 pt
                                                      readAppExnFields pt rec{ae_type = (toEnum  i)}
                                                   else do skip pt ft
                                                           readAppExnFields pt rec
                                          _ -> do skip pt ft
                                                  readFieldEnd pt
                                                  readAppExnFields pt rec

  readAppExn pt = do readStructBegin pt
                     rec <- readAppExnFields pt (AppExn {ae_type = undefined, ae_message = undefined})
                     readStructEnd pt
                     return rec


  writeAppExn pt ae = do writeStructBegin pt "TApplicationException"
                         if ae_message ae /= "" then
                             do writeFieldBegin pt ("message",T_STRING,1)
                                writeString pt (ae_message ae)
                                writeFieldEnd pt
                             else return ()
                         writeFieldBegin pt ("type",T_I32,2);
                         writeI32 pt (fromEnum (ae_type ae))
                         writeFieldEnd pt
                         writeFieldStop pt
                         writeStructEnd pt


