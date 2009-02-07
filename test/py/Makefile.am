THRIFT = $(top_srcdir)/compiler/cpp/thrift

py_unit_tests =                                 \
        SerializationTest.py                    \
        TestEof.py                              \
        TestSyntax.py                           \
        RunClientServer.py

thrift_gen =                                    \
        gen-py/ThriftTest/__init__.py           \
        gen-py/DebugProtoTest/__init__.py

helper_scripts=                                 \
        TestClient.py                           \
        TestServer.py

check_SCRIPTS=                                  \
        $(thrift_gen)                           \
        $(py_unit_tests)                        \
        $(helper_scripts)

TESTS= $(py_unit_tests)


gen-py/%/__init__.py: ../%.thrift
	$(THRIFT) --gen py $<

clean-local:
	$(RM) -r gen-py
