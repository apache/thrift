#include <sys/stat.h>
#include "t_cpp_generator.h"
#include "globals.h"
using namespace std;

/**
 * Prepares for file generation by opening up the necessary file output
 * streams.
 *
 * @param tprogram The program to generate
 */
void t_cpp_generator::init_generator(t_program* tprogram) {
  // Make output directory
  mkdir(T_CPP_DIR, S_IREAD | S_IWRITE | S_IEXEC);

  // Make output file
  string f_types_name = string(T_CPP_DIR)+"/"+tprogram->get_name()+"_types.h";
  f_types_.open(f_types_name.c_str());

  // Print header
  f_types_ <<
    autogen_comment();

  // Start ifndef
  f_types_ <<
    "#ifndef thrift_" << tprogram->get_name() << "_types_h" << endl <<
    "#define thrift_" << tprogram->get_name() << "_types_h" << endl <<
    endl;
  
  // Include base types
  f_types_ <<
    "#include <sys/types.h>" << endl <<
    endl;
}

/**
 * Closes the output files.
 */
void t_cpp_generator::close_generator() {
  // Close ifndef
  f_types_ <<
    "#endif" << endl;
  
  // Close output file
  f_types_.close();
}


/**
 * Generates a typedef. This is just a simple 1-liner in C++
 *
 * @param ttypedef The type definition
 */
void t_cpp_generator::generate_typedef(t_typedef* ttypedef) {
  f_types_ <<
    indent() << "typedef " << type_name(ttypedef->get_type()) << " " << 
    ttypedef->get_symbolic() << ";" << endl <<
    endl;
}

/**
 * Generates code for an enumerated type. In C++, this is essentially the same
 * as the thrift definition itself, using the enum keyword in C++.
 *
 * @param tenum The enumeration
 */
void t_cpp_generator::generate_enum(t_enum* tenum) {
  f_types_ <<
    indent() << "enum " << tenum->get_name() << " {" << endl;

  indent_up();

  vector<t_constant*> constants = tenum->get_constants();
  vector<t_constant*>::iterator c_iter;
  bool first = true;
  for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    if (first) {
      first = false;
    } else {
      f_types_ <<
        "," << endl;
    }
    f_types_ <<
      indent() << (*c_iter)->get_name();
    if ((*c_iter)->has_value()) {
      f_types_ <<
        " = " << (*c_iter)->get_value();
    }
  }

  indent_down();

  f_types_ <<
    endl <<
    "};" << endl <<
    endl;
}

/**
 * Generates a struct definition for a thrift data type. In C++, this is just
 * simple C struct with basic data members. There are no constructors,
 * initializers, etc.
 *
 * @param tstruct The struct definition
 */
void t_cpp_generator::generate_struct(t_struct* tstruct) {
  f_types_ <<
    indent() << "struct " << tstruct->get_name() << " {" << endl;
  
  indent_up();

  const t_list* memberlist = tstruct->get_members();
  vector<t_field*> members = memberlist->elems();
  vector<t_field*>::iterator m_iter; 
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    f_types_ <<
      indent() << (*m_iter)->get_type()->get_name() << " " <<
      (*m_iter)->get_name() << ";" << endl;
  }
  
  indent_down();
  
  f_types_ <<
    indent() << "};" << endl <<
    endl;
}

/**
 * Generates a thrift service. In C++, this comprises an entirely separate
 * header and source file. The header file defines the methods and includes
 * the data types defined in the main header file, and the implementation
 * file contains implementations of the basic printer and default interfaces.
 *
 * @param tservice The service definition
 */
void t_cpp_generator::generate_service(t_service* tservice) {
  // Make output files
  string f_header_name = string(T_CPP_DIR)+"/"+tservice->get_name()+".h";
  f_header_.open(f_header_name.c_str());
  string f_service_name = string(T_CPP_DIR)+"/"+tservice->get_name()+".cc";
  f_service_.open(f_service_name.c_str());

  // Print header file includes
  f_header_ << autogen_comment();
  f_header_ <<
    "#ifndef " << tservice->get_name() << "_h" << endl <<
    "#define " << tservice->get_name() << "_h" << endl <<
    endl <<
    "#include \"TInterface.h\"" << endl <<
    "#include \"TDispatcher.h\"" << endl <<
    "#include \"TProtocol.h\"" << endl <<   
    endl;
  f_service_ << autogen_comment();
  f_service_ <<
    "#include \"" << tservice->get_name() << "_h\"" << endl << endl;

  // Generate the three main parts of the service
  generate_service_interface(tservice);
  generate_service_server(tservice);
  generate_service_client(tservice);

  f_header_ <<
    "#endif" << endl;

  // Close files
  f_header_.close();
  f_service_.close();
}

/**
 * Generates a service interface definition.
 *
 * @param tservice The service to generate a header definition for
 */
void t_cpp_generator::generate_service_interface(t_service* tservice) {
  f_header_ <<
    "class " << tservice->get_name() << " : public TInterface {" << endl <<
    " public: " << endl;
  indent_up(); 
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter; 
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    f_header_ <<
      indent() << "virtual " << function_signature(*f_iter) << " = 0;" << endl;
  }
  f_header_ <<
    indent() << "virtual ~" << tservice->get_name() << "() = 0;" << endl;
  indent_down();
  f_header_ <<
    "}; " << endl << endl;
}

/**
 * Generates a service server definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_cpp_generator::generate_service_server(t_service* tservice) {
  // Generate the header portion
  f_header_ <<
    "class " << tservice->get_name() << "Server : " <<
    "public " << tservice->get_name() << ", " <<
    "public TDispatcher {" << endl <<
    " public: " << endl;
  indent_up();
  f_header_ << 
    indent() << "std::string dispatch(const std::string& buff);" << endl <<
    indent() << "virtual ~" << tservice->get_name() << "Server();" << endl;
  indent_down();
  f_header_ <<
    "};" << endl << endl;

  // Generate the dispatch methods
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter; 
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    generate_dispatch_function(tservice, *f_iter);
  }
}

/**
 * Generates a dispatch function definition.
 *
 * @param tfunction The function to write a dispatcher for
 */
void t_cpp_generator::generate_dispatch_function(t_service* tservice,
                                                 t_function* tfunction) {
  f_service_ <<
    "std::string dispatch_" << tfunction->get_name() <<
    "(const char *_tbuf, " <<
    tservice->get_name() << " *dispatcher, " <<
    "TProtocol *protocol) {" << endl;
  indent_up();

  // Create a field to represent this function's arguments
  t_field arg_field(tfunction->get_arglist(), "_targs", 0);

  generate_deserialize_struct(&arg_field);

  // Generate the function call
  indent(f_service_) <<
    type_name(tfunction->get_returntype()) << " _tresult = dispatcher." <<
    tfunction->get_name() << "(";
  vector<t_field*> fields = tfunction->get_arglist()->get_members()->elems();
  vector<t_field*>::iterator f_iter;
  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
    } else {
      f_service_ << ", ";
    }
    f_service_ << "_targs." << (*f_iter)->get_name();
  }
  f_service_ << ");" << endl;

  // Serialize the result
  indent(f_service_) <<
    "std::string _tout = "";" << endl;
  t_field out_field(tfunction->get_returntype(), "_tout", 0);
  generate_serialize_field(&out_field);
  
  indent_down();
  f_service_ <<
    "}" << endl <<
    endl;
}

/**
 * Generates an unserializer for a variable. This makes two key assumptions,
 * first that there is a const char* variable named data that points to the
 * buffer for deserialization, and that there is a variable protocol which
 * is a reference to a TProtocol serialization object.
 */
void t_cpp_generator::generate_deserialize_struct(t_field* tfield) {
    t_struct* tstruct = (t_struct*)(tfield->get_type());
  vector<t_field*> fields = tstruct->get_members()->elems();
  vector<t_field*>::iterator f_iter;

  indent(f_service_) <<
    declare_field(tfield) << endl;
  indent(f_service_) <<
    "map<uint32_t,const char*> fmap = protocol.readFieldMap(_tbuf);" << endl;  

  // Declare the fields up front
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    indent(f_service_) <<
      declare_field(*f_iter) << endl;
    indent(f_service_) <<
      "if (fmap.find(" << tfield->get_key() << ") != fmap.end()) {" << endl;
    indent_up();
    indent(f_service_) <<
      "_tbuf = fmap[" << tfield->get_key() << "];" << endl;
    generate_deserialize_field(*f_iter);
    indent_down();
    indent(f_service_) <<
      "}" << endl;
  }
}

/**
 * Deserializes a field of any type.
 */
void t_cpp_generator::generate_deserialize_field(t_field* tfield) {
  t_type* type = tfield->get_type();
  while (type->is_typedef()) {
    type = ((t_typedef*)type)->get_type();
  }

  if (type->is_struct()) {
    generate_deserialize_struct(tfield);
  }

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      return;
    case t_base_type::TYPE_STRING:
      return;
    case t_base_type::TYPE_BYTE:
      return;
    case t_base_type::TYPE_I32:
      return;
    case t_base_type::TYPE_U32:
      return;
    case t_base_type::TYPE_I64:
      return;
    case t_base_type::TYPE_U64:
      return;
    default:
      throw "compiler error: no C++ name for base type " + tbase;
    }
  } else if (type->is_enum()) {
    
  }
}

/**
 * Generates a service client definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_cpp_generator::generate_service_client(t_service* tservice) {
  // Generate the header portion
  f_header_ <<
    "class " << tservice->get_name() << "Client : " <<
    "public " << tservice->get_name() << " {" << endl <<
    " public:" << endl;
  
  indent_up();
  f_header_ <<
    indent() << tservice->get_name() <<
    "(TDispatcher& dispatcher, TProtocol& protocol);" << endl;
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter; 
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    f_header_ <<
      indent() << function_signature(*f_iter) << ";" << endl;
  }
  indent_down();
  
  f_header_ <<
    " private:" << endl;
  indent_up();
  f_header_ <<
    indent() << "TDispatcher& dispatcher;" << endl <<
    indent() << "TProtocol& protocol;" << endl;
  indent_down();  
  f_header_ <<
    "};" << endl <<
    endl;
}

/**
 * Deserializes a field of any type.
 */
void t_cpp_generator::generate_serialize_field(t_field* tfield) {
  t_type* type = tfield->get_type();
  while (type->is_typedef()) {
    type = ((t_typedef*)type)->get_type();
  }

  if (type->is_struct()) {

  }

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      return;
    case t_base_type::TYPE_STRING:
      return;
    case t_base_type::TYPE_BYTE:
      return;
    case t_base_type::TYPE_I32:
      return;
    case t_base_type::TYPE_U32:
      return;
    case t_base_type::TYPE_I64:
      return;
    case t_base_type::TYPE_U64:
      return;
    default:
      throw "compiler error: no C++ name for base type " + tbase;
    }
  } else if (type->is_enum()) {
    
  }
}


/**
 * Generates a comment about this code being autogenerated.
 *
 * @return C-style comment mentioning that this file is autogenerated.
 */
string t_cpp_generator::autogen_comment() {
  string result = "";
  return
    result +
    "/**\n" +
    " * Autogenerated by Thrift\n" +
    " * " + g_time_str +
    " *\n" +
    " * DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING\n" +
    " */\n";
}

/**
 * Returns a C++ type name
 *
 * @param ttype The type
 */
string t_cpp_generator::type_name(t_type* ttype) {
  if (ttype->is_base_type()) {
    return base_type_name(((t_base_type*)ttype)->get_base());
  } else {
    return ttype->get_name();
  }
}

/**
 * Returns the C++ type that corresponds to the thrift type.
 *
 * @param tbase The base type
 */
string t_cpp_generator::base_type_name(t_base_type::t_base tbase) {
  switch (tbase) {
  case t_base_type::TYPE_VOID:
    return "void";
  case t_base_type::TYPE_STRING:
    return "std::string";
  case t_base_type::TYPE_BYTE:
    return "uint8_t";
  case t_base_type::TYPE_I32:
    return "int32_t";
  case t_base_type::TYPE_U32:
    return "uint32_t";
  case t_base_type::TYPE_I64:
    return "int64_t";
  case t_base_type::TYPE_U64:
    return "uint64_t";
  default:
    throw "compiler error: no C++ name for base type " + tbase;
  }
}

/**
 * Declares a field, which may include initialization as necessary.
 *
 * @param ttype The type
 */
string t_cpp_generator::declare_field(t_field* tfield) {
  // TODO(mcslee): do we ever need to initialize the field?
  return type_name(tfield->get_type()) + " " + tfield->get_name() + ";";
}

/**
 * Renders a function signature of the form 'type name(args)'
 *
 * @param tfunction Function definition
 * @return String of rendered function definition
 */
string t_cpp_generator::function_signature(t_function* tfunction) {
  return
    type_name(tfunction->get_returntype()) + " " + tfunction->get_name() +
    "(" + field_list(tfunction->get_arglist()->get_members()) + ")";
}

/**
 * Renders a field list
 */
string t_cpp_generator::field_list(t_list* tlist) {
  string result = "";

  vector<t_field*> fields = tlist->elems();
  vector<t_field*>::iterator f_iter;
  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
    } else {
      result += ", ";
    }
    result += type_name((*f_iter)->get_type()) + " " + (*f_iter)->get_name();
  }
  return result;
}
