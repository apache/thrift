#!/bin/sh
cd /data/users/cpiro/thrift/trunk/lib/erl/release/thrift_rel/local
erl -name cpiro_thrift_rel -boot thrift_rel -config thrift_rel.config $@
