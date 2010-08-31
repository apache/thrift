THRIFT = thrift
CXXFLAGS = `pkg-config --cflags libzmq thrift`
LDLIBS = `pkg-config --libs libzmq thrift`

CXXFLAGS += -g3 -O0

GENNAMES = Storage storage_types
GENHEADERS = $(addsuffix .h, $(GENNAMES))
GENSRCS = $(addsuffix .cpp, $(GENNAMES))
GENOBJS = $(addsuffix .o, $(GENNAMES))

PYLIBS = storage/__init__.py

PROGS =  test-client test-server test-sender test-receiver

all: $(PYLIBS) $(PROGS)

test-client: test-client.o TZmqClient.o $(GENOBJS)
test-server: test-server.o TZmqServer.o $(GENOBJS)
test-sender: test-sender.o TZmqClient.o $(GENOBJS)
test-receiver: test-receiver.o TZmqServer.o $(GENOBJS)

test-client.o test-server.o test-sender.o test-receiver.o: $(GENSRCS)

storage/__init__.py: storage.thrift
	$(RM) $(dir $@)
	$(THRIFT) --gen py:newstyle $<
	mv gen-py/$(dir $@) .

$(GENSRCS): storage.thrift
	$(THRIFT) --gen cpp $<
	mv $(addprefix gen-cpp/, $(GENSRCS) $(GENHEADERS)) .

clean:
	$(RM) -r *.o $(PROGS) storage $(GENSRCS) $(GENHEADERS)

.PHONY: clean
