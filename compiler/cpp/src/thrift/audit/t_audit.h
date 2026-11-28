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

#ifndef T_AUDIT_H
#define T_AUDIT_H

void compare_namespace(t_program* newProgram, t_program* oldProgram);
void compare_enums(const std::vector<t_enum*>& newEnumList,
                   const std::vector<t_enum*>& oldEnumList);
bool compare_defaults(t_const_value* newStructDefault, t_const_value* oldStructDefault);
void compare_structs(const std::vector<t_struct*>& newStructList,
                     const std::vector<t_struct*>& oldStructList);
void compare_services(const std::vector<t_service*>& newServices,
                      const std::vector<t_service*>& oldServices);
void compare_consts(const std::vector<t_const*>& newConst, const std::vector<t_const*>& oldConst);

#endif
