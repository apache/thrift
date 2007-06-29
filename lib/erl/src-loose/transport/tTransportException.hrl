-define(tTransportException_UNKNOWN, 0).
-define(tTransportException_NOT_OPEN, 1).
-define(tTransportException_ALREADY_OPEN, 2).
-define(tTransportException_TIMED_OUT, 3).
-define(tTransportException_END_OF_FILE, 4).

-record(tTransportException, {type, message}).
