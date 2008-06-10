-ifndef(THRIFT_PROTOCOL_INCLUDED).
-define(THRIFT_PROTOCOL_INCLUDED, yea).

-record(protocol_message_begin, {name, type, seqid}).
-record(protocol_struct_begin, {name}).
-record(protocol_field_begin, {name, type, id}).
-record(protocol_map_begin, {ktype, vtype, size}).
-record(protocol_list_begin, {etype, size}).
-record(protocol_set_begin, {etype, size}).


-endif.
