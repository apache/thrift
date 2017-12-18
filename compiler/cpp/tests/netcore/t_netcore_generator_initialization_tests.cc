#include "../catch/catch.hpp"
#include <thrift/parse/t_program.h>
#include <thrift/generate/t_netcore_generator.h>

TEST_CASE( "t_netcore_generator should throw error with unknown options", "[initialization]" )
{
    string path = "CassandraTest.thrift";
    string name = "netcore";
    map<string, string> parsed_options = { { "keys", "keys" } };
    string option_string = "";

    t_program* program = new t_program(path, name);
    t_netcore_generator* gen = nullptr;

    REQUIRE_THROWS(gen = new t_netcore_generator(program, parsed_options, option_string));	

    delete gen;
    delete program;	
}

TEST_CASE("t_netcore_generator should create valid instance with valid options", "[initialization]")
{
    string path = "CassandraTest.thrift";
    string name = "netcore";
    map<string, string> parsed_options = { { "wcf", "wcf" }, { "nullable", "nullable"} };
    string option_string = "";

    t_program* program = new t_program(path, name);
    t_netcore_generator* gen = nullptr;

    REQUIRE_NOTHROW(gen = new t_netcore_generator(program, parsed_options, option_string));
    REQUIRE(gen != nullptr);
    REQUIRE(gen->is_wcf_enabled());
    REQUIRE(gen->is_nullable_enabled());
    REQUIRE_FALSE(gen->is_hashcode_enabled());
    REQUIRE_FALSE(gen->is_serialize_enabled());
    REQUIRE_FALSE(gen->is_union_enabled());

    delete gen;
    delete program;
}

TEST_CASE("t_netcore_generator should pass init succesfully", "[initialization]")
{
    string path = "CassandraTest.thrift";
    string name = "netcore";
    map<string, string> parsed_options = { { "wcf", "wcf" },{ "nullable", "nullable" } };
    string option_string = "";

    t_program* program = new t_program(path, name);
    t_netcore_generator* gen = new t_netcore_generator(program, parsed_options, option_string);

    REQUIRE_NOTHROW(gen->init_generator());

    delete gen;
    delete program;
}
