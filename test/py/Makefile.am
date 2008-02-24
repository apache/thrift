THRIFT = $(top_srcdir)/compiler/cpp/thrift

py_unit_tests =                                 \
        SerializationTest.py                    \
        TestEof.py                              \
        RunClientServer.py

thrift_gen =                                    \
        gen-py/ThriftTest/__init__.py

helper_scripts=                                 \
        TestClient.py                           \
        TestServer.py

check_SCRIPTS=                                  \
        $(thrift_gen)                           \
        $(py_unit_tests)                        \
        $(helper_scripts)

TESTS= $(py_unit_tests)


gen-py/ThriftTest/__init__.py: ../ThriftTest.thrift
	$(THRIFT) -py $<

clean-local:
	$(RM) -r gen-py
