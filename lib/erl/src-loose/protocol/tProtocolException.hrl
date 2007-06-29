-record(tProtocolException, {message, type}).

-define(tProtocolException_UNKNOWN, 0).
-define(tProtocolException_INVALID_DATA, 1).
-define(tProtocolException_NEGATIVE_SIZE, 2).
-define(tProtocolException_SIZE_LIMIT, 3).
-define(tProtocolException_BAD_VERSION, 4).

