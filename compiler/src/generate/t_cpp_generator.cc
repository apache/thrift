#include <stdlib.h>
#include <sys/stat.h>
#include <sstream>
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
  string f_types_name = string(T_CPP_DIR)+"/"+program_name_+"Types.h";
  f_types_.open(f_types_name.c_str());

  // Print header
  f_types_ <<
    autogen_comment();

  // Start ifndef
  f_types_ <<
    "#ifndef " << program_name_ << "_TYPES_H" << endl <<
    "#define " << program_name_ << "_TYPES_H" << endl <<
    endl;
  
  // Include base types
  f_types_ <<
    "#include \"Thrift.h\"" << endl <<
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

  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter; 
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    indent(f_types_) <<
      declare_field(*m_iter) << endl;
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
  string f_header_name = string(T_CPP_DIR)+"/"+service_name_+".h";
  f_header_.open(f_header_name.c_str());
  string f_service_name = string(T_CPP_DIR)+"/"+service_name_+".cc";
  f_service_.open(f_service_name.c_str());

  // Print header file includes
  f_header_ << autogen_comment();
  f_header_ <<
    "#ifndef " << service_name_ << "_H" << endl <<
    "#define " << service_name_ << "_H" << endl <<
    endl <<
    "#include \"TDispatcher.h\"" << endl <<
    "#include \"protocol/TProtocol.h\"" << endl <<
    "#include \"" << program_name_ << "Types.h\"" << endl <<
    endl;
  f_service_ << autogen_comment();
  f_service_ <<
    "#include \"" << service_name_ << ".h\"" << endl << endl;

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
    "class " << service_name_ << "If {" << endl <<
    " public: " << endl;
  indent_up(); 
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter; 
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    f_header_ <<
      indent() << "virtual " << function_signature(*f_iter) << " = 0;" << endl;
  }
  f_header_ <<
    indent() << "virtual ~" << service_name_ << "If() {}" << endl;
  indent_down();
  f_header_ <<
    "}; " << endl << endl;
}

/**
 * Generates a service client definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_cpp_generator::generate_service_client(t_service* tservice) {
  // Generate the header portion
  f_header_ <<
    "class " << service_name_ << "Client : " <<
    "public " << service_name_ << "If {" << endl <<
    " public:" << endl; 
  indent_up();
  f_header_ <<
    indent() << service_name_ << "Client" <<
    "(TDispatcher* dispatcher, TProtocol* protocol) : " <<
    "_dispatcher(dispatcher), _protocol(protocol) {}" << endl;
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter; 
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    f_header_ <<
      indent() << function_signature(*f_iter) << ";" << endl;
  }
  indent_down();
  
  f_header_ <<
    " protected:" << endl;
  indent_up();
  f_header_ <<
    indent() << "TDispatcher* _dispatcher;" << endl <<
    indent() << "TProtocol* _protocol;" << endl;
  indent_down();  
  f_header_ <<
    "};" << endl <<
    endl;
  
  // Generate client method implementations
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    string prefix = service_name_ + "Client::";
    f_service_ <<
      function_signature(*f_iter, prefix) << " {" << endl;
    indent_up();

    indent(f_service_) <<
      "std::string _argbuf = \"\";" << endl;
    generate_serialize_struct("_argbuf", (*f_iter)->get_arglist());
    indent(f_service_) <<
      "std::string _sbuf = _protocol->writeFunction(\"" <<
      (*f_iter)->get_name() << "\", _argbuf);" << endl;
    indent(f_service_) <<
      "std::string _rbuf = _dispatcher->dispatch(_sbuf);" << endl;

    if (!(*f_iter)->get_returntype()->is_void()) {
      indent(f_service_) <<
        "TBuf _tbuf((uint8_t*)_rbuf.data(), _rbuf.size());" << endl;
      indent(f_service_) <<
        type_name((*f_iter)->get_returntype()) << " _result;" << endl;
      t_field result_field((*f_iter)->get_returntype(), "_result", 0);
      generate_deserialize_field("_tbuf", &result_field);
      indent(f_service_) <<
        "return _result;" << endl;
    } else {
      indent(f_service_) <<
        "return;" << endl;
    }
   
    indent_down();
    f_service_ <<
      "}" << endl <<
      endl;
  }
}

/**
 * Generates a service server definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_cpp_generator::generate_service_server(t_service* tservice) {
  // Generate the header portion
  f_header_ <<
    "class " << service_name_ << "ServerIf : " <<
    "public " << service_name_ << "If, " <<
    "public TDispatcher {" << endl <<
    " public: " << endl;
  indent_up();
  f_header_ << 
    indent() << service_name_ << "ServerIf(TProtocol* protocol) : " <<
    "_protocol(protocol) {}" << endl <<
    indent() << "std::string dispatch(const std::string& _in);" << endl <<
    indent() << "virtual ~" << service_name_ << "ServerIf() {}" << endl;
  indent_down();
  f_header_ <<
    " protected:" << endl;
  indent_up();
  f_header_ <<
    indent() << "TProtocol* _protocol;" << endl;
  indent_down();  
  f_header_ <<
    "};" << endl << endl;

  // Generate the dispatch methods
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter; 
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    generate_dispatch_function(tservice, *f_iter);
  }

  // Generate the server implementation
  f_service_ <<
    "std::string " << service_name_ << "ServerIf::" <<
    "dispatch(const std::string& _in) {" << endl;
  indent_up();

  f_service_ <<
    indent() << "TBuf _tbuf((uint8_t*)_in.data(), _in.size());" << endl <<
    indent() << "std::string _fname = " <<
                "_protocol->readFunction(_tbuf);" << endl <<
    indent() << "std::string _result;" << endl;

  bool first = true;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    if (!first) {
      f_service_ << " else ";
    } else {
      f_service_ << indent();
      first = false;
    }
    f_service_ <<
      "if (_fname.compare(\"" << (*f_iter)->get_name() <<"\") == 0) {" << endl;
    indent_up();
    indent(f_service_) <<
      "_result = dispatch_" << (*f_iter)->get_name() <<
      "(_tbuf, this, _protocol);" << endl;
    indent_down();
    indent(f_service_) << "}";
  }
  indent(f_service_) <<
    " else {" << endl;
  indent_up();
  f_service_ <<
    indent() << "fprintf(stderr, \"Unknown function: '%s'\\n\", " <<
    "_fname.c_str());" << endl <<
    indent() << "_result = \"\";" << endl;
  indent_down();
  indent(f_service_) <<
    "}" << endl;

  indent(f_service_) <<
    "return _result;" << endl;

  indent_down();
  f_service_ <<
    "}" << endl <<
    endl;
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
    "(TBuf _tbuf, " <<
    service_name_ << "If *_dispatcher, " <<
    "const TProtocol *_protocol) {" << endl;
  indent_up();

  // Get the struct of function call params
  t_struct* arg_struct = tfunction->get_arglist();

  // Declare the function arguments
  const vector<t_field*>& fields = arg_struct->get_members();
  vector<t_field*>::const_iterator f_iter;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    indent(f_service_) <<
      declare_field(*f_iter, true) << endl;
  }

  // Deserialize the function arguments as a struct
  generate_deserialize_struct("_tbuf", arg_struct);
  
  // Generate the function call
  f_service_ << indent();
  if (!tfunction->get_returntype()->is_void()) {
    f_service_ << type_name(tfunction->get_returntype()) << " _result = ";
  }
  f_service_ <<
    "_dispatcher->" << tfunction->get_name() << "(";
  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
    } else {
      f_service_ << ", ";
    }
    f_service_ << (*f_iter)->get_name();
  }
  f_service_ << ");" << endl;

  // Serialize the result into _sbuf
  indent(f_service_) <<
    "std::string _sbuf = \"\";" << endl;

  t_field result_field(tfunction->get_returntype(), "_result", 0);
  generate_serialize_field("_sbuf", &result_field);

  // Return the serialized buffer
  indent(f_service_) <<
    "return _sbuf;" << endl;

  indent_down();
  f_service_ <<
    "}" << endl <<
    endl;
}

/**
 * Deserializes a field of any type.
 */
void t_cpp_generator::generate_deserialize_field(string src,
                                                 t_field* tfield,
                                                 string prefix) {
  t_type* type = tfield->get_type();
  while (type->is_typedef()) {
    type = ((t_typedef*)type)->get_type();
  }

  if (type->is_void()) {
    return;
  }

  if (type->is_struct()) {
    generate_deserialize_struct(src,
                                (t_struct*)(tfield->get_type()),
                                prefix + tfield->get_name() + ".");
  } else if (type->is_container()) {
    generate_deserialize_container(src,
                                   tfield->get_type(),
                                   prefix + tfield->get_name());
  } else if (type->is_base_type() || type->is_enum()) {

    indent(f_service_) <<
      prefix << tfield->get_name() << " = ";
    if (type->is_enum()) {
      f_service_ << "(" << type_name(type) << ")";
    }
    f_service_ <<
      "_protocol->";
    
    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "compiler error: cannot serialize void field in a struct: " +
          src + ":" + tfield->get_name() + ":" + prefix;
        break;
      case t_base_type::TYPE_STRING:
        f_service_ << "readString(" << src << ");";
        break;
      case t_base_type::TYPE_BYTE:
        f_service_ << "readByte(" << src << ");";
        break;
      case t_base_type::TYPE_I32:
        f_service_ << "readI32(" << src << ");";
        break;
      case t_base_type::TYPE_U32:
        f_service_ << "readU32(" << src << ");";
        break;
      case t_base_type::TYPE_I64:
        f_service_ << "readI64(" << src << ");";
        break;
      case t_base_type::TYPE_U64:
        f_service_ << "readU64(" << src << ");";
        break;
      default:
        throw "compiler error: no C++ name for base type " + tbase;
      }
    } else if (type->is_enum()) {
      f_service_ << "readI32(" << src << ");";
    }
    
    f_service_ <<
      endl;
  } else {
    printf("DO NOT KNOW HOW TO DESERIALIZE FIELD '%s' TYPE '%s'\n",
           tfield->get_name().c_str(), type_name(type).c_str());
  }
}

/**
 * Generates an unserializer for a variable. This makes two key assumptions,
 * first that there is a const char* variable named data that points to the
 * buffer for deserialization, and that there is a variable protocol which
 * is a reference to a TProtocol serialization object.
 */
void t_cpp_generator::generate_deserialize_struct(string src,
                                                  t_struct* tstruct,
                                                  string prefix) {
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  // Ensure that there are some fields
  if (fields.size() == 0) {
    return;
  }

  scope_up(f_service_);

  // Read the struct fields from the protocol
  string _struct = tmp("_struct");
  indent(f_service_) <<
    "std::map<uint32_t,TBuf> " << _struct <<
    " = _protocol->readStruct(" << src << ");" << endl;
  indent(f_service_) <<
    "std::map<uint32_t,TBuf>::iterator _field;" << endl;

  // Decleare a temp buffer for working with fields
  string fbuf = tmp("_fbuf");

  // Iterate over the fields and extract them
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    // Check for existence of the field
    indent(f_service_) <<
      "if ((_field = " << _struct << ".find(" << (*f_iter)->get_key() <<
      ")) != " << _struct << ".end()) {" <<
      endl;
    indent_up();

    // Copy the field buffer into temp buffer
    indent(f_service_) <<
      "TBuf& " << fbuf << " = _field->second;" << endl;

    // Deserialize from the field's buffer
    generate_deserialize_field(fbuf, *f_iter, prefix);

    indent_down();
    indent(f_service_) <<
      "}" << endl;
  }

  scope_down(f_service_);
}

void t_cpp_generator::generate_deserialize_container(string src,
                                                     t_type* ttype,
                                                     string prefix) {
  scope_up(f_service_);
  
  string size = tmp("_size");
  indent(f_service_) <<
    "uint32_t " << size << " = _protocol->readU32(" << src << ");" << endl;

  string i = tmp("_i");
  indent(f_service_) <<
    "uint32_t " << i << ";" << endl;
  indent(f_service_) <<
    "for (" <<
    i << " = 0; " << i << " < " << size << "; ++" << i << ") {" << endl;
  indent_up();

  if (ttype->is_map()) {
    generate_deserialize_map_element(src, (t_map*)ttype, prefix);
  } else if (ttype->is_set()) {
    generate_deserialize_set_element(src, (t_set*)ttype, prefix);
  } else if (ttype->is_list()) {
    generate_deserialize_list_element(src, (t_list*)ttype, prefix);
  }

  indent_down();
  indent(f_service_) <<
    "}" << endl;

  scope_down(f_service_);
}


/**
 * Generates code to deserialize a map
 */
void t_cpp_generator::generate_deserialize_map_element(string src,
                                                       t_map* tmap,
                                                       string prefix) {
  string key = tmp("_key");
  string val = tmp("_val");
  t_field fkey(tmap->get_key_type(), key, 0);
  t_field fval(tmap->get_val_type(), val, 0);

  indent(f_service_) <<
    declare_field(&fkey) << endl;
  indent(f_service_) <<
    declare_field(&fval) << endl;

  generate_deserialize_field(src, &fkey, "");
  generate_deserialize_field(src, &fval, "");

  indent(f_service_) <<
    prefix << ".insert(std::make_pair(" << key << ", " << val << "));" << endl;
}

void t_cpp_generator::generate_deserialize_set_element(string src,
                                                       t_set* tset,
                                                       string prefix) {
  string elem = tmp("_elem");
  t_field felem(tset->get_elem_type(), elem, 0);

  indent(f_service_) <<
    declare_field(&felem) << endl;

  generate_deserialize_field(src, &felem, "");

  indent(f_service_) <<
    prefix << ".insert(" << elem << ");" << endl;
}

void t_cpp_generator::generate_deserialize_list_element(string src,
                                                        t_list* tlist,
                                                        string prefix) {
  string elem = tmp("_elem");
  t_field felem(tlist->get_elem_type(), elem, 0);

  indent(f_service_) <<
    declare_field(&felem) << endl;

  generate_deserialize_field(src, &felem, "");

  indent(f_service_) <<
    prefix << ".push_back(" << elem << ");" << endl;
}


/**
 * Serializes a field of any type.
 *
 * @param tfield The field to serialize
 * @param prefix Name to prepend to field name
 */
void t_cpp_generator::generate_serialize_field(string dest,
                                               t_field* tfield,
                                               string prefix) {
  t_type* type = tfield->get_type();
  while (type->is_typedef()) {
    type = ((t_typedef*)type)->get_type();
  }
  
  // Do nothing for void types
  if (type->is_void()) {
    return;
  }

  if (type->is_struct()) {
    generate_serialize_struct(dest,
                              (t_struct*)(tfield->get_type()),
                              prefix + tfield->get_name() + ".");
  } else if (type->is_container()) {
    generate_serialize_container(dest,
                                 tfield->get_type(),
                                 prefix + tfield->get_name());
  } else if (type->is_base_type() || type->is_enum()) {
    indent(f_service_) <<
      dest << " += _protocol->";
    
    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "compiler error: cannot serialize void field in a struct: " +
          dest + ":" + tfield->get_name() + ":" + prefix;
        break;
      case t_base_type::TYPE_STRING:
        f_service_ << "writeString(" << prefix << tfield->get_name() << ");";
        break;
      case t_base_type::TYPE_BYTE:
        f_service_ << "writeByte(" << prefix << tfield->get_name() << ");";
        break;
      case t_base_type::TYPE_I32:
        f_service_ << "writeI32(" << prefix << tfield->get_name() << ");";
        break;
      case t_base_type::TYPE_U32:
        f_service_ << "writeU32(" << prefix << tfield->get_name() << ");";
        break;
      case t_base_type::TYPE_I64:
        f_service_ << "writeI64(" << prefix << tfield->get_name() << ");";
        break;
      case t_base_type::TYPE_U64:
        f_service_ << "writeU64(" << prefix << tfield->get_name() << ");";
        break;
      default:
        throw "compiler error: no C++ name for base type " + tbase;
      }
    } else if (type->is_enum()) {
      f_service_ << "writeI32(" << prefix << tfield->get_name() << ");";
    } 
    f_service_ << endl;
  } else {
    printf("DO NOT KNOW HOW TO SERIALIZE FIELD '%s' TYPE '%s'\n",
           tfield->get_name().c_str(), type_name(type).c_str());
  }
}

/**
 * Serializes all the members of a struct.
 *
 * @param tstruct The struct to serialize
 * @param prefix  String prefix to attach to all fields
 */
void t_cpp_generator::generate_serialize_struct(string dest,
                                                t_struct* tstruct,
                                                string prefix) {
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  // Ensure that there are some fields
  if (fields.size() == 0) {
    return;
  }

  scope_up(f_service_);
  string _struct = tmp("_struct");

  indent(f_service_) <<
    "std::map<uint32_t,std::string> " << _struct << ";" << endl;

  // Serialize each of the fields into the map
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    ostringstream fdest;
    fdest << _struct << "[" << (*f_iter)->get_key() << "]";
    generate_serialize_field(fdest.str(), *f_iter, prefix);
  }

  // Write the struct map
  indent(f_service_) <<
    dest << " += _protocol->writeStruct(" << _struct << ");" << endl;

  scope_down(f_service_);
}

void t_cpp_generator::generate_serialize_container(string dest,
                                                   t_type* ttype,
                                                   string prefix) {
  scope_up(f_service_);
  
  indent(f_service_) <<
    dest << " += _protocol->writeU32(" << prefix << ".size());" << endl;

  string iter = tmp("_iter");
  indent(f_service_) <<
    type_name(ttype) << "::const_iterator " << iter << ";" << endl;
  indent(f_service_) <<
    "for (" << iter << " = " << prefix  << ".begin(); " <<
    iter << " != " << prefix << ".end(); " <<
    "++" << iter << ") {" << endl;
  indent_up();
  
  if (ttype->is_map()) {
    generate_serialize_map_element(dest, (t_map*)ttype, iter);
  } else if (ttype->is_set()) {
    generate_serialize_set_element(dest, (t_set*)ttype, iter);
  } else if (ttype->is_list()) {
    generate_serialize_list_element(dest, (t_list*)ttype, iter);
  }

  indent_down();
  indent(f_service_) <<
    "}" << endl;  
  
  scope_down(f_service_);  
}

/**
 * Serializes the members of a map.
 *
 */
void t_cpp_generator::generate_serialize_map_element(string dest,
                                                     t_map* tmap,
                                                     string iter) {
  t_field kfield(tmap->get_key_type(), iter + "->first", 0);
  generate_serialize_field(dest, &kfield, "");

  t_field vfield(tmap->get_val_type(), iter + "->second", 0);
  generate_serialize_field(dest, &vfield, "");
}

/**
 * Serializes the members of a set.
 */
void t_cpp_generator::generate_serialize_set_element(string dest,
                                                     t_set* tset,
                                                     string iter) {
  t_field efield(tset->get_elem_type(), "(*" + iter + ")", 0);
  generate_serialize_field(dest, &efield, "");
}

/**
 * Serializes the members of a list.
 */
void t_cpp_generator::generate_serialize_list_element(string dest,
                                                      t_list* tlist,
                                                      string iter) {
  t_field efield(tlist->get_elem_type(), "(*" + iter + ")", 0);
  generate_serialize_field(dest, &efield, "");
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
  } else if (ttype->is_map()) {
    t_map* tmap = (t_map*) ttype;
    return "std::map<" +
      type_name(tmap->get_key_type()) + ", " +
      type_name(tmap->get_val_type()) + "> ";
  } else if (ttype->is_set()) {
    t_set* tset = (t_set*) ttype;
    return "std::set<" + type_name(tset->get_elem_type()) + "> ";
  } else if (ttype->is_list()) {
    t_list* tlist = (t_list*) ttype;
    return "std::list<" + type_name(tlist->get_elem_type()) + "> ";
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
string t_cpp_generator::declare_field(t_field* tfield, bool init) {
  // TODO(mcslee): do we ever need to initialize the field?
  string result = type_name(tfield->get_type()) + " " + tfield->get_name();
  if (init) {
    t_type* type = tfield->get_type();
    while (type->is_typedef()) {
      type = ((t_typedef*)type)->get_type();
    }

    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        break;
      case t_base_type::TYPE_STRING:
        result += " = \"\"";
        break;
      case t_base_type::TYPE_BYTE:
      case t_base_type::TYPE_I32:
      case t_base_type::TYPE_U32:
      case t_base_type::TYPE_I64:
      case t_base_type::TYPE_U64:
        result += " = 0";
        break;
      default:
        throw "compiler error: no C++ initializer for base type " + tbase;
      }
    } else if (type->is_enum()) {
      result += " = (" + type_name(type) + ")0";
    }
  }
  return result + ";";
}

/**
 * Renders a function signature of the form 'type name(args)'
 *
 * @param tfunction Function definition
 * @return String of rendered function definition
 */
string t_cpp_generator::function_signature(t_function* tfunction,
                                           string prefix) {
  t_type* ttype = tfunction->get_returntype();
  return
    type_name(ttype) + " " + prefix + tfunction->get_name() +
    "(" + argument_list(tfunction->get_arglist()) + ")";
}

/**
 * Renders a field list
 */
string t_cpp_generator::argument_list(t_struct* tstruct) {
  string result = "";

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
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
