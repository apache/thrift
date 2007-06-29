% TApplicationException
-define(tApplicationException_UNKNOWN, 0).
-define(tApplicationException_UNKNOWN_METHOD, 1).
-define(tApplicationException_INVALID_MESSAGE_TYPE, 2).
-define(tApplicationException_WRONG_METHOD_NAME, 3).
-define(tApplicationException_BAD_SEQUENCE_ID, 4).
-define(tApplicationException_MISSING_RESULT, 5).

-record(tApplicationException, {message, type=?tApplicationException_UNKNOWN}).
