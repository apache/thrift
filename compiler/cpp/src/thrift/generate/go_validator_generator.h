/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

#ifndef T_GO_VALIDATOR_GENERATOR_H
#define T_GO_VALIDATOR_GENERATOR_H

#include "thrift/generate/t_generator.h"
#include "thrift/generate/t_go_generator.h"
#include "thrift/generate/validator_parser.h"
#include <fstream>
#include <iostream>
#include <limits>
#include <string>
#include <vector>

class go_validator_generator {
public:
  go_validator_generator(t_go_generator* gg) : go_generator(gg){};
  void generate_struct_validator(std::ostream& out, t_struct* tstruct);

  struct generator_context {
    std::string field_symbol;
    std::string src;
    std::string tgt;
    bool opt;
    t_type* type;
    std::vector<validation_rule*> rules;
  };

private:
  void generate_field_validator(std::ostream& out, const generator_context& context);
  void generate_enum_field_validator(std::ostream& out, const generator_context& context);
  void generate_bool_field_validator(std::ostream& out, const generator_context& context);
  void generate_integer_field_validator(std::ostream& out, const generator_context& context);
  void generate_double_field_validator(std::ostream& out, const generator_context& context);
  void generate_string_field_validator(std::ostream& out, const generator_context& context);
  void generate_list_field_validator(std::ostream& out, const generator_context& context);
  void generate_set_field_validator(std::ostream& out, const generator_context& context);
  void generate_map_field_validator(std::ostream& out, const generator_context& context);
  void generate_struct_field_validator(std::ostream& out, const generator_context& context);

  void indent_up() { go_generator->indent_up(); }
  void indent_down() { go_generator->indent_down(); }
  std::string indent() { return go_generator->indent(); }

  //std::string get_field_name(t_field* field);  -- no impl?
  std::string get_field_reference_name(t_field* field);

  std::string GenID(std::string id) { return id + std::to_string(tmp_[id]++); };

  t_go_generator* go_generator;

  std::map<std::string, int> tmp_;
};

#endif
