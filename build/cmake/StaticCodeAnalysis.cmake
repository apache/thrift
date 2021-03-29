find_package(ClangTools QUIET)
if(clang-tidy_FOUND AND run-clang-tidy_FOUND AND NOT TARGET do_run_clang_tidy)
    add_custom_target(
      do_run_clang_tidy
      COMMAND ClangTools::run-clang-tidy -clang-tidy-binary "$<TARGET_FILE:ClangTools::clang-tidy>" -p ${CMAKE_BINARY_DIR} "-quiet" > ./run-clang-tidy.txt
      DEPENDS ${CMAKE_BINARY_DIR}/compile_commands.json
      WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
      )
endif()
