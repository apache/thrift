namespace java thrift.test.voidmethexceptions

exception TExampleException {
  1: required string message;
}

service TAppService01 {
  string returnString(1: string msg, 2: bool throwException) throws (1:TExampleException error);
  void returnVoidThrows(1: string msg, 2: bool throwException) throws (1:TExampleException error);
  void returnVoidNoThrowsRuntimeException(1: string msg, 2: bool throwException);
  void returnVoidNoThrowsTApplicationException(1: string msg, 2: bool throwException);
  oneway void onewayVoidNoThrows(1: string msg, 2: bool throwException);
}
