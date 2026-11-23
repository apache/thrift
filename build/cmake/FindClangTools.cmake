#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

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

# The module finds several tools, so the names passed to
# find_package_handle_standard_args() differ from the module name by design.
set(FPHSA_NAME_MISMATCHED TRUE)

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

unset(FPHSA_NAME_MISMATCHED)
