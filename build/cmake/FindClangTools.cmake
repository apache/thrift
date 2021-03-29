# - Try to find Clang tools
#
# The following are set after configuration is done:
#  clang-tidy_FOUND
#  ClangTools::clang-tidy
#  clang-apply-replacements_FOUND
#  ClangTools::clang-apply-replacements
#  run-clang-tidy_FOUND
#  ClangTools::run-clang-tidy

include_guard()
include(FindPackageHandleStandardArgs)

foreach(program_name IN ITEMS clang-tidy clang-apply-replacements)
  find_program(${program_name}_BINARY NAMES ${program_name}-devel ${program_name}-8 ${program_name} PATH_SUFFIXES "LLVM/bin")
  find_package_handle_standard_args(${program_name} DEFAULT_MSG ${program_name}_BINARY)
  if(${program_name}_FOUND AND NOT TARGET ClangTools::${program_name})
    add_executable(ClangTools::${program_name} IMPORTED)
    set_property(TARGET ClangTools::${program_name} PROPERTY IMPORTED_LOCATION "${${program_name}_BINARY}")
  endif()
endforeach()

find_program(run-clang-tidy_BINARY NAMES run-clang-tidy run-clang-tidy.py PATH_SUFFIXES "LLVM/bin" "llvm-devel/share/clang")
find_package_handle_standard_args(run-clang-tidy DEFAULT_MSG run-clang-tidy_BINARY)
if(run-clang-tidy_FOUND AND NOT TARGET ClangTools::run-clang-tidy)
  add_executable(ClangTools::run-clang-tidy IMPORTED)
  set_property(TARGET ClangTools::run-clang-tidy PROPERTY IMPORTED_LOCATION "${run-clang-tidy_BINARY}")
endif()
