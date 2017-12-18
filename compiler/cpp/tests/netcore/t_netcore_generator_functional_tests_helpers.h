#include <thrift/parse/t_program.h>

class TestDataGenerator
{
public:
    static const string DEFAULT_FILE_HEADER;

    static std::pair<string, t_enum*> get_test_enum_data(t_program* program);
    static std::pair<string, t_const*> get_test_void_const_data(t_netcore_generator* gen);
    static std::pair<string, t_const*> get_test_string_const_data(t_netcore_generator* gen);
    static std::pair<string, t_const*> get_test_bool_const_data(t_netcore_generator* gen);
    static std::pair<string, t_const*> get_test_i8_const_data(t_netcore_generator* gen);
    static std::pair<string, t_const*> get_test_i16_const_data(t_netcore_generator* gen);
    static std::pair<string, t_const*> get_test_i32_const_data(t_netcore_generator* gen);
    static std::pair<string, t_const*> get_test_i64_const_data(t_netcore_generator* gen);
    static std::pair<string, t_const*> get_test_double_const_data(t_netcore_generator* gen);
};
