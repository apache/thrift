// Licensed to the Apache Software Foundation(ASF) under one
// or more contributor license agreements.See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership.The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.

#include <list>
#include <memory>
#include "../catch/catch.hpp"
#include <thrift/generate/t_ocaml_generator.cc>

using defs_t = std::list<t_type*>;

/** This subclass is meant to allow accessing the Thrift generated OCaml outputs
    and keep the tests stable across Thrift versions (as much as possible), but
    otherwise is identical to the standard OCaml generator. */
class t_test_ocaml_generator : public t_ocaml_generator {
public:
    t_test_ocaml_generator(t_program* program) : t_ocaml_generator(program, {}, "") {}

    /** Override and turn off comment generation which contains a version number
        to make tests version-independent. */
    std::string ocaml_autogen_comment() override { return ""; }

    // Allow inspecting the generated code.

    string types() { return f_types_.str(); }
    string consts() { return f_consts_.str(); }
    string service() { return f_service_.str(); }
    string types_i() { return f_types_i_.str(); }
    string service_i() { return f_service_i_.str(); }
};

/** Helper to add a list of definitions to a Thrift 'program' (i.e.
    representation of the IDL) and generate the OCaml outputs. */
void gen_program(t_generator& gen, t_program& program, defs_t defs) {
    for (auto def : defs) {
        if (def->is_typedef()) program.add_typedef(static_cast<t_typedef*>(def));
        else if (def->is_enum()) program.add_enum(static_cast<t_enum*>(def));
        else if (def->is_struct()) program.add_struct(static_cast<t_struct*>(def));
        else if (def->is_xception()) program.add_xception(static_cast<t_struct*>(def));
        else if (def->is_service()) program.add_service(static_cast<t_service*>(def));
    }

    gen.generate_program();
}

TEST_CASE( "t_ocaml_generator - typedefs", "[functional]" )
{
    t_program program("Typedefs.thrift", "Typedefs");
    t_base_type ty_string("string", t_base_type::TYPE_STRING);
    t_typedef tydef_decimal(&program, &ty_string, "Decimal");
    t_test_ocaml_generator gen(&program);

    gen_program(gen, program, defs_t {
        &tydef_decimal
    });

    #include "snapshot_typedefs.cc"
    REQUIRE( snapshot == gen.types() );
}

TEST_CASE( "t_ocaml_generator - handle exception from different module", "[functional]" )
{
    t_program errors_thrift("Errors.thrift", "Errors");
    t_struct server_error(&errors_thrift, "ServerError");
    server_error.set_xception(true);

    t_test_ocaml_generator errors_gen(&errors_thrift);
    gen_program(errors_gen, errors_thrift, defs_t {
        &server_error
    });

    {
        #include "snapshot_exception_types_i.cc"
        REQUIRE( snapshot == errors_gen.types_i() );
    }

    t_program service_thrift("Service.thrift", "Service");
    t_service service(&service_thrift);
    service.set_name("Service");
    t_base_type ret_type("void", t_base_type::TYPE_VOID);
    t_struct args(&service_thrift, "ping_args");
    t_struct throws(&service_thrift, "ping_throws");
    t_field ex_server_error(&server_error, "serverError", 1);
    throws.append(&ex_server_error);
    t_function ping(&ret_type, "ping", &args, &throws);
    service.add_function(&ping);

    t_test_ocaml_generator service_gen(&service_thrift);

    gen_program(service_gen, service_thrift, defs_t {
        &service
    });

    {
        #include "snapshot_service_handle_ex.cc"
        REQUIRE( snapshot == service_gen.service() );
    }
}
