macro(GENERATE FILENAME GENERATOR OUTPUTDIR)
  file(MAKE_DIRECTORY ${MY_CURRENT_SOURCE_DIR}/${OUTPUTDIR})
  execute_process(COMMAND ${THRIFTCOMPILER} --gen ${GENERATOR} -out ${OUTPUTDIR} ${FILENAME}
                  WORKING_DIRECTORY ${MY_CURRENT_SOURCE_DIR}
                  RESULT_VARIABLE CMD_RESULT)
  if(CMD_RESULT)
        message(FATAL_ERROR "Error generating ${FILENAME} with generator ${GENERATOR}")
  endif()
endmacro(GENERATE)

generate(${MY_PROJECT_DIR}/test/ThriftTest.thrift py gen-py-default)
generate(${MY_PROJECT_DIR}/test/ThriftTest.thrift py:slots gen-py-slots)
generate(${MY_PROJECT_DIR}/test/ThriftTest.thrift py:new_style gen-py-newstyle)
generate(${MY_PROJECT_DIR}/test/ThriftTest.thrift py:new_style,slots gen-py-newstyleslots)
generate(${MY_PROJECT_DIR}/test/ThriftTest.thrift py:dynamic gen-py-dynamic)
generate(${MY_PROJECT_DIR}/test/ThriftTest.thrift py:dynamic,slots gen-py-dynamicslots)

generate(${MY_PROJECT_DIR}/test/DebugProtoTest.thrift py gen-py-default)
generate(${MY_PROJECT_DIR}/test/DebugProtoTest.thrift py:slots gen-py-slots)
generate(${MY_PROJECT_DIR}/test/DebugProtoTest.thrift py:new_style gen-py-newstyle)
generate(${MY_PROJECT_DIR}/test/DebugProtoTest.thrift py:new_style,slots gen-py-newstyleslots)
generate(${MY_PROJECT_DIR}/test/DebugProtoTest.thrift py:dynamic gen-py-dynamic)
generate(${MY_PROJECT_DIR}/test/DebugProtoTest.thrift py:dynamic,slots gen-py-dynamicslots)

