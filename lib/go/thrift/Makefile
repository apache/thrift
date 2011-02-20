# Copyright 2009 The Go Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

include $(GOROOT)/src/Make.inc

TARG=thrift
GOFILES=\
        tapplication_exception.go\
        tbase.go\
        tbinary_protocol.go\
        tcompact_protocol.go\
        tcompare.go\
        tcontainer.go\
        texception.go\
        tfield.go\
        tframed_transport.go\
        thttp_client.go\
        tiostream_transport.go\
        tlist.go\
        tjson_protocol.go\
        tmap.go\
        tmemory_buffer.go\
        tmessage.go\
        tmessagetype.go\
        tnonblocking_server.go\
        tnonblocking_server_socket.go\
        tnonblocking_socket.go\
        tnonblocking_transport.go\
        tnumeric.go\
        tprocessor.go\
        tprocessor_factory.go\
        tprotocol.go\
        tprotocol_exception.go\
        tprotocol_factory.go\
        tserver.go\
        tserver_socket.go\
        tserver_transport.go\
        tset.go\
        tsimple_server.go\
        tsimple_json_protocol.go\
        tsocket.go\
        tstruct.go\
        ttransport.go\
        ttransport_exception.go\
        ttransport_factory.go\
        ttype.go

DIRS=\

include $(GOROOT)/src/Make.pkg

check:
	gomake test

-include ../Make.deps


