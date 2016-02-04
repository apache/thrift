
namespace go demo.rpc
namespace cpp demo.rpc
struct ArgStruct {
    1:byte argByte,
    2:string argString
    3:i16  argI16,
    4:i32  argI32,
    5:i64  argI64,
    6:double argDouble,
    7:bool   argBool,
}

service RpcService {
    list<string> funCall(
        1:ArgStruct argStruct,
        2:byte argByte,
        3:i16  argI16,
        4:i32  argI32,
        5:i64  argI64,
        6:double argDouble,
        7:string argString,
        8:map<string, string> paramMapStrStr,
        9:map<i32, string> paramMapI32Str,
        10:set<string> paramSetStr,
        11:set<i64> paramSetI64,
        12:list<string> paramListStr,
        13:bool   argBool,
        ),
}