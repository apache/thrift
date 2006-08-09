#include <stdlib.h>
#include <sys/stat.h>
#include <sstream>
#include "t_cpp_generator.h"
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
  f_header_ <<
    autogen_comment();
  f_header_ <<
    "#ifndef " << service_name_ << "_H" << endl <<
    "#define " << service_name_ << "_H" << endl <<
    endl <<
    "#include \"TProcessor.h\"" << endl <<
    "#include \"transport/TTransport.h\"" << endl <<
    "#include \"protocol/TProtocol.h\"" << endl <<
    "#include \"" << program_name_ << "Types.h\"" << endl <<
    endl;
  f_service_ <<
    autogen_comment();
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
    "(boost::shared_ptr<facebook::thrift::transport::TTransport> trans, boost::shared_ptr<facebook::thrift::protocol::TProtocol> prot) : " <<
    "_itrans(trans), _otrans(trans), " <<
    "_iprot(prot), _oprot(prot) {}" << endl;
  f_header_ <<
    indent() << service_name_ << "Client" <<
    "(boost::shared_ptr<facebook::thrift::transport::TTransport> itrans, boost::shared_ptr<facebook::thrift::transport::TTransport> otrans," <<
    " boost::shared_ptr<facebook::thrift::protocol::TProtocol> iprot, boost::shared_ptr<facebook::thrift::protocol::TProtocol> oprot) : " <<
    "_itrans(itrans), _otrans(otrans), " <<
    "_iprot(iprot), _oprot(oprot) {}" << endl;

  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::const_iterator f_iter; 
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    t_function send_function(g_program->get_void_type(),
                             string("send_") + (*f_iter)->get_name(),
                             (*f_iter)->get_arglist());
    indent(f_header_) << function_signature(*f_iter) << ";" << endl;
    indent(f_header_) << function_signature(&send_function) << ";" << endl;
    if (!(*f_iter)->is_async()) {
      t_struct noargs;
      t_function recv_function((*f_iter)->get_returntype(),
                               string("recv_") + (*f_iter)->get_name(),
                               &noargs);
      indent(f_header_) << function_signature(&recv_function) << ";" << endl;
    }
  }
  indent_down();
  
  f_header_ <<
    " protected:" << endl;
  indent_up();
  f_header_ <<
    indent() << "boost::shared_ptr<facebook::thrift::transport::TTransport> _itrans;" << endl <<
    indent() << "boost::shared_ptr<facebook::thrift::transport::TTransport> _otrans;" << endl <<
    indent() << "boost::shared_ptr<facebook::thrift::protocol::TProtocol>  _iprot;"  << endl <<
    indent() << "boost::shared_ptr<facebook::thrift::protocol::TProtocol>  _oprot;"  << endl;
  indent_down();  

  f_header_ <<
    "};" << endl <<
    endl;
  
  string scope = service_name_ + "Client::";

  // Generate client method implementations
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    string funname = (*f_iter)->get_name();

    // Open function
    indent(f_service_) <<
      function_signature(*f_iter, scope) << endl;
    scope_up(f_service_);
    indent(f_service_) <<
      "send_" << funname << "(";

    // Get the struct of function call params
    t_struct* arg_struct = (*f_iter)->get_arglist();
    
    // Declare the function arguments
    const vector<t_field*>& fields = arg_struct->get_members();
    vector<t_field*>::const_iterator fld_iter;
    bool first = true;
    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      if (first) {
        first = false;
      } else {
        f_service_ << ", ";
      }
      f_service_ << (*fld_iter)->get_name();
    }
    f_service_ << ");" << endl;

    if (!(*f_iter)->is_async()) {
      f_service_ << indent();
      if (!(*f_iter)->get_returntype()->is_void()) {
        f_service_ << "return ";
      }
      f_service_ <<
        "recv_" << funname << "();" << endl;
    }
    scope_down(f_service_);
    f_service_ << endl;

    t_function send_function(g_program->get_void_type(),
                             string("send_") + (*f_iter)->get_name(),
                             (*f_iter)->get_arglist());

    // Open function
    indent(f_service_) <<
      function_signature(&send_function, scope) << endl;
    scope_up(f_service_);

    // Serialize the request
    f_service_ <<
      indent() <<
      "_oprot->writeStructBegin(_otrans, \"function\");" << endl <<
      indent() <<
      "_oprot->writeFieldBegin(_otrans, \"name\", facebook::thrift::protocol::T_STRING, 0);" << endl <<
      indent() <<
      "_oprot->writeString(_otrans, \"" << funname << "\");" << endl <<
      indent() <<
      "_oprot->writeFieldEnd(_otrans);" << endl <<
      indent() <<
      "_oprot->writeFieldBegin(_otrans, \"args\", facebook::thrift::protocol::T_STRUCT, 1);" << endl;     
    generate_serialize_struct((*f_iter)->get_arglist());
    f_service_ <<
      indent() <<
      "_oprot->writeFieldEnd(_otrans);" << endl <<
      indent() <<
      "_oprot->writeFieldStop(_otrans);" << endl <<
      indent() <<
      "_oprot->writeStructEnd(_otrans);" << endl;
    
    // Flush the request
    indent(f_service_) <<
      "_otrans->flush();" << endl;
    
    scope_down(f_service_);
    f_service_ << endl;

    if (!(*f_iter)->is_async()) {
      t_struct noargs;
      t_function recv_function((*f_iter)->get_returntype(),
                               string("recv_") + (*f_iter)->get_name(),
                               &noargs);
      // Open function
      indent(f_service_) <<
        function_signature(&recv_function, scope) << endl;
      scope_up(f_service_);

      // Read the response
      t_struct result_struct((*f_iter)->get_name() + "_result");
      t_field result_field((*f_iter)->get_returntype(), "_result");
      
      // Add a field to the return struct if non void
      if (!(*f_iter)->get_returntype()->is_void()) {
        indent(f_service_) <<
          type_name((*f_iter)->get_returntype()) << " _result;" << endl;
        result_struct.append(&result_field);
      }
      
      // Deserialize response struct
      generate_deserialize_struct(&result_struct);
      
      // Careful, only return _result if not a void function
      if (!(*f_iter)->get_returntype()->is_void()) {
        indent(f_service_) <<
          "return _result;" << endl;
      } else {
        indent(f_service_) <<
          "return;" << endl;
      }
      
      // Close function
      scope_down(f_service_);
      f_service_ << endl;
    }

  }
}

/**
 * Generates a service server definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_cpp_generator::generate_service_server(t_service* tservice) {
  // Generate the dispatch methods
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter; 

  // Generate the header portion
  f_header_ <<
    "class " << service_name_ << "ServerIf : " <<
    "public " << service_name_ << "If, " <<
    "public facebook::thrift::TProcessor {" << endl <<
    " public: " << endl;
  indent_up();
  f_header_ << 
    indent() <<
    service_name_ << "ServerIf(boost::shared_ptr<facebook::thrift::protocol::TProtocol> protocol) : " <<
                       "_iprot(protocol), _oprot(protocol) {}" << endl <<
    indent() <<
    service_name_ << "ServerIf(boost::shared_ptr<facebook::thrift::protocol::TProtocol> iprot, boost::shared_ptr<facebook::thrift::protocol::TProtocol> oprot) : " <<
                       "_iprot(iprot), _oprot(oprot) {}" << endl <<
    indent() << "bool process(boost::shared_ptr<facebook::thrift::transport::TTransport> _itrans, " <<
                             "boost::shared_ptr<facebook::thrift::transport::TTransport> _otrans);" << endl <<
    indent() << "virtual ~" << service_name_ << "ServerIf() {}" << endl;
  indent_down();

  // Protected data members
  f_header_ <<
    " protected:" << endl;
  indent_up();
  f_header_ <<
    indent() << "boost::shared_ptr<facebook::thrift::protocol::TProtocol> _iprot;" << endl <<
    indent() << "boost::shared_ptr<facebook::thrift::protocol::TProtocol> _oprot;" << endl;
  indent_down();

  // Process function declarations
  f_header_ <<
    " private:" << endl;
  indent_up();
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    indent(f_header_) <<
      "void process_" << (*f_iter)->get_name() <<
      "(boost::shared_ptr<facebook::thrift::transport::TTransport> _itrans, boost::shared_ptr<facebook::thrift::transport::TTransport> _otrans);" << endl;
  }
  indent_down();
  f_header_ <<
    "};" << endl << endl;

  // Generate the server implementation
  f_service_ <<
    "bool " << service_name_ << "ServerIf::" <<
    "process(boost::shared_ptr<facebook::thrift::transport::TTransport> _itrans, boost::shared_ptr<facebook::thrift::transport::TTransport> _otrans) {" << endl;
  indent_up();

  f_service_ <<
    indent() <<
    "std::string _name;" << endl <<
    indent() <<
    "std::string _fname;" << endl <<
    indent() <<
    "facebook::thrift::protocol::TType _ftype;" << endl <<
    indent() <<
    "uint16_t _fid;" << endl <<
    indent() <<
    "_iprot->readStructBegin(_itrans, _name);" << endl <<
    indent() <<
    "_iprot->readFieldBegin(_itrans, _name, _ftype, _fid);" << endl <<
    indent() <<
    "_iprot->readString(_itrans, _fname);" << endl <<
    indent() <<
    "_iprot->readFieldEnd(_itrans);" << endl <<
    indent() <<
    "_iprot->readFieldBegin(_itrans, _name, _ftype, _fid);" << endl;

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
      "process_" << (*f_iter)->get_name() <<
      "(_itrans, _otrans);" << endl;
    indent_down();
    indent(f_service_) << "}";
  }
  indent(f_service_) <<
    " else {" << endl;
  indent_up();
  indent(f_service_) <<
    "fprintf(stderr, \"Unknown function: '%s'\\n\", " <<
    "_fname.c_str());" << endl;
  indent_down();
  indent(f_service_) <<
    "}" << endl;

  // Read end of args field, the T_STOP, and the struct close
  f_service_ <<
    indent() <<
    "_iprot->readFieldEnd(_itrans);" << endl <<
    indent() <<
    "_iprot->readFieldBegin(_itrans, _name, _ftype, _fid);" << endl <<   
    indent() <<
    "_iprot->readStructEnd(_itrans);" << endl <<
    indent() <<
    "return true;" << endl;

  indent_down();
  f_service_ <<
    "}" << endl <<
    endl;

  // Generate the process subfunctions
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    generate_process_function(tservice, *f_iter);
  }
}

/**
 * Generates a process function definition.
 *
 * @param tfunction The function to write a dispatcher for
 */
void t_cpp_generator::generate_process_function(t_service* tservice,
                                                t_function* tfunction) {
  // Open function
  f_service_ <<
    "void " << tservice->get_name() << "ServerIf::" <<
    "process_" << tfunction->get_name() <<
    "(boost::shared_ptr<facebook::thrift::transport::TTransport> _itrans, boost::shared_ptr<facebook::thrift::transport::TTransport> _otrans)" << endl;
  scope_up(f_service_);

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
  generate_deserialize_struct(arg_struct);
  
  // Generate the function call
  f_service_ << indent();
  if (!tfunction->get_returntype()->is_void()) {
    f_service_ << type_name(tfunction->get_returntype()) << " _result = ";
  }
  f_service_ <<
    tfunction->get_name() << "(";
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

  // Serialize the result into a struct
  t_struct result_struct(tfunction->get_name() + "_result");
  t_field result_field(tfunction->get_returntype(), "_result");

  // Only append the field if non-void
  if (!tfunction->get_returntype()->is_void()) {
    result_struct.append(&result_field);
  }
  generate_serialize_struct(&result_struct);
  indent(f_service_) <<
    "_otrans->flush();" << endl;

  // Close function
  scope_down(f_service_);
  f_service_ << endl;
}

/**
 * Deserializes a field of any type.
 */
void t_cpp_generator::generate_deserialize_field(t_field* tfield,
                                                 string prefix) {
  t_type* type = tfield->get_type();
  while (type->is_typedef()) {
    type = ((t_typedef*)type)->get_type();
  }

  if (type->is_void()) {
    throw "CANNOT GENERATE DESERIALIZE CODE FOR void TYPE: " +
      prefix + tfield->get_name();
  }

  string name = prefix + tfield->get_name();

  if (type->is_struct()) {
    generate_deserialize_struct((t_struct*)(tfield->get_type()),
                                 name + ".");
  } else if (type->is_container()) {
    generate_deserialize_container(tfield->get_type(), name);
  } else if (type->is_base_type() || type->is_enum()) {

    indent(f_service_) <<
      "_iprot->";
    
    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "compiler error: cannot serialize void field in a struct: " +
          name;
        break;
      case t_base_type::TYPE_STRING:
        f_service_ << "readString(_itrans, " << name << ");";
        break;
      case t_base_type::TYPE_BYTE:
        f_service_ << "readByte(_itrans, " << name << ");";
        break;
      case t_base_type::TYPE_I32:
        f_service_ << "readI32(_itrans, " << name << ");";
        break;
      case t_base_type::TYPE_U32:
        f_service_ << "readU32(_itrans, " << name << ");";
        break;
      case t_base_type::TYPE_I64:
        f_service_ << "readI64(_itrans, " << name << ");";
        break;
      case t_base_type::TYPE_U64:
        f_service_ << "readU64(_itrans, " << name << ");";
        break;
      default:
        throw "compiler error: no C++ name for base type " + tbase;
      }
    } else if (type->is_enum()) {
      f_service_ << "readI32(_itrans, (int32_t&)" << name << ");";
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
void t_cpp_generator::generate_deserialize_struct(t_struct* tstruct,
                                                  string prefix) {
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  scope_up(f_service_);

  // Read the struct fields from the protocol
  string fid   = tmp("_fid");
  string ftype = tmp("_ftype");
  string fname = tmp("_name");
  
  // Declare stack tmp variables
  f_service_ <<
    indent() << "std::string " << fname << ";" << endl <<
    indent() << "facebook::thrift::protocol::TType " << ftype << ";" << endl <<
    indent() << "uint16_t " << fid << ";" << endl <<
    indent() << "_iprot->readStructBegin(_itrans, " << fname << ");" << endl;
  
  // Loop over reading in fields
  indent(f_service_) <<
    "while (true)" << endl;

    scope_up(f_service_);
    
    // Read beginning field marker
    indent(f_service_) <<
      "_iprot->readFieldBegin(_itrans, " <<
      fname << ", " << ftype << ", " << fid << ");" << endl;
    
    // Check for field STOP marker
    indent(f_service_) <<
      "if (" << ftype << " == facebook::thrift::protocol::T_STOP) { break; }" << endl;
    
    // Switch statement on the field we are reading
    indent(f_service_) <<
      "switch (" << fid << ")" << endl;

      scope_up(f_service_);
    
      // Generate deserialization code for known cases
      for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
        indent(f_service_) <<
          "case " << (*f_iter)->get_key() << ":" << endl;
        indent_up();
        generate_deserialize_field(*f_iter, prefix);
        indent(f_service_) <<
          "break;" << endl;
        indent_down();
      }
      
      // In the default case we skip the field
      f_service_ <<
        indent() << "default:" << endl <<
        indent() << "  _iprot->skip(_itrans, " << ftype << ");" << endl <<
        indent() << "  break;" << endl;
      
      scope_down(f_service_);

    // Read field end marker
    indent(f_service_) <<
      "_iprot->readFieldEnd(_itrans);" << endl;
    
    scope_down(f_service_);
      
  indent(f_service_) <<
    "_iprot->readStructEnd(_itrans);" << endl;

  scope_down(f_service_);
}

void t_cpp_generator::generate_deserialize_container(t_type* ttype,
                                                     string prefix) {
  scope_up(f_service_);
  
  string size = tmp("_size");
  string ktype = tmp("_ktype");
  string vtype = tmp("_vtype");
  string etype = tmp("_etype");
  
  indent(f_service_) <<
    "int32_t " << size << ";" << endl;
  
  // Declare variables, read header
  if (ttype->is_map()) {
    f_service_ <<
      indent() << "facebook::thrift::protocol::TType " << ktype << ";" << endl <<
      indent() << "facebook::thrift::protocol::TType " << vtype << ";" << endl <<
      indent() << "_iprot->readMapBegin(_itrans, " <<
                   ktype << ", " << vtype << ", " << size << ");" << endl;
  } else if (ttype->is_set()) {
    f_service_ <<
      indent() << "facebook::thrift::protocol::TType " << etype << ";" << endl <<
      indent() << "_iprot->readSetBegin(_itrans, " <<
                   etype << ", " << size << ");" << endl;
  } else if (ttype->is_list()) {
    f_service_ <<
      indent() << "facebook::thrift::protocol::TType " << etype << ";" << endl <<
      indent() << "_iprot->readListBegin(_itrans, " <<
                   etype << ", " << size << ");" << endl;
  }


  // For loop iterates over elements
  string i = tmp("_i");
  indent(f_service_) <<
    "int32_t " << i << ";" << endl;
  indent(f_service_) <<
    "for (" <<
    i << " = 0; " << i << " < " << size << "; ++" << i << ")" << endl;
  
    scope_up(f_service_);
    
    if (ttype->is_map()) {
      generate_deserialize_map_element((t_map*)ttype, prefix);
    } else if (ttype->is_set()) {
      generate_deserialize_set_element((t_set*)ttype, prefix);
    } else if (ttype->is_list()) {
      generate_deserialize_list_element((t_list*)ttype, prefix);
    }
    
    scope_down(f_service_);

  // Read container end
  if (ttype->is_map()) {
    indent(f_service_) << "_iprot->readMapEnd(_itrans);" << endl;
  } else if (ttype->is_set()) {
    indent(f_service_) << "_iprot->readSetEnd(_itrans);" << endl;
  } else if (ttype->is_list()) {
    indent(f_service_) << "_iprot->readListEnd(_itrans);" << endl;
  }

  scope_down(f_service_);
}


/**
 * Generates code to deserialize a map
 */
void t_cpp_generator::generate_deserialize_map_element(t_map* tmap,
                                                       string prefix) {
  string key = tmp("_key");
  string val = tmp("_val");
  t_field fkey(tmap->get_key_type(), key);
  t_field fval(tmap->get_val_type(), val);

  indent(f_service_) <<
    declare_field(&fkey) << endl;
  indent(f_service_) <<
    declare_field(&fval) << endl;

  generate_deserialize_field(&fkey);
  generate_deserialize_field(&fval);

  indent(f_service_) <<
    prefix << ".insert(std::make_pair(" << key << ", " << val << "));" << endl;
}

void t_cpp_generator::generate_deserialize_set_element(t_set* tset,
                                                       string prefix) {
  string elem = tmp("_elem");
  t_field felem(tset->get_elem_type(), elem);

  indent(f_service_) <<
    declare_field(&felem) << endl;

  generate_deserialize_field(&felem);

  indent(f_service_) <<
    prefix << ".insert(" << elem << ");" << endl;
}

void t_cpp_generator::generate_deserialize_list_element(t_list* tlist,
                                                        string prefix) {
  string elem = tmp("_elem");
  t_field felem(tlist->get_elem_type(), elem);

  indent(f_service_) <<
    declare_field(&felem) << endl;

  generate_deserialize_field(&felem);

  indent(f_service_) <<
    prefix << ".push_back(" << elem << ");" << endl;
}


/**
 * Serializes a field of any type.
 *
 * @param tfield The field to serialize
 * @param prefix Name to prepend to field name
 */
void t_cpp_generator::generate_serialize_field(t_field* tfield,
                                               string prefix) {
  t_type* type = tfield->get_type();
  while (type->is_typedef()) {
    type = ((t_typedef*)type)->get_type();
  }

  // Do nothing for void types
  if (type->is_void()) {
    throw "CANNOT GENERATE SERIALIZE CODE FOR void TYPE: " +
      prefix + tfield->get_name();
  }
  
  if (type->is_struct()) {
    generate_serialize_struct((t_struct*)(tfield->get_type()),
                              prefix + tfield->get_name() + ".");
  } else if (type->is_container()) {
    generate_serialize_container(tfield->get_type(),
                                 prefix + tfield->get_name());
  } else if (type->is_base_type() || type->is_enum()) {

    string name = prefix + tfield->get_name();
    indent(f_service_) <<
      "_oprot->";
    
    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw
          "compiler error: cannot serialize void field in a struct: " + name;
        break;
      case t_base_type::TYPE_STRING:
        f_service_ << "writeString(_otrans, " << name << ");";
        break;
      case t_base_type::TYPE_BYTE:
        f_service_ << "writeByte(_otrans, " << name << ");";
        break;
      case t_base_type::TYPE_I32:
        f_service_ << "writeI32(_otrans, " << name << ");";
        break;
      case t_base_type::TYPE_U32:
        f_service_ << "writeU32(_otrans, " << name << ");";
        break;
      case t_base_type::TYPE_I64:
        f_service_ << "writeI64(_otrans, " << name << ");";
        break;
      case t_base_type::TYPE_U64:
        f_service_ << "writeU64(_otrans, " << name << ");";
        break;
      default:
        throw "compiler error: no C++ name for base type " + tbase;
      }
    } else if (type->is_enum()) {
      f_service_ << "writeI32(_otrans, (int32_t)" << name << ");";
    }
    f_service_ << endl;
  } else {
    printf("DO NOT KNOW HOW TO SERIALIZE FIELD '%s%s' TYPE '%s'\n",
           prefix.c_str(),
           tfield->get_name().c_str(),
           type_name(type).c_str());
  }
}

/**
 * Serializes all the members of a struct.
 *
 * @param tstruct The struct to serialize
 * @param prefix  String prefix to attach to all fields
 */
void t_cpp_generator::generate_serialize_struct(t_struct* tstruct,
                                                string prefix) {
  string name = tstruct->get_name();
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  scope_up(f_service_);
  indent(f_service_) <<
    "_oprot->writeStructBegin(_otrans, \"" << name << "\");" << endl;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    // Write field header
    indent(f_service_) <<
      "_oprot->writeFieldBegin(_otrans, " <<
      "\"" << (*f_iter)->get_name() << "\", " <<
      type_to_enum((*f_iter)->get_type()) << ", " <<
      (*f_iter)->get_key() << ");" << endl;
    // Write field contents
    generate_serialize_field(*f_iter, prefix);
    // Write field closer
    indent(f_service_) <<
      "_oprot->writeFieldEnd(_otrans);" << endl;
  }
  // Write the struct map
  f_service_ <<
    indent() << "_oprot->writeFieldStop(_otrans);" << endl <<
    indent() << "_oprot->writeStructEnd(_otrans);" << endl;

  scope_down(f_service_);
}

void t_cpp_generator::generate_serialize_container(t_type* ttype,
                                                   string prefix) {
  scope_up(f_service_);
  
  if (ttype->is_map()) {
    indent(f_service_) <<
      "_oprot->writeMapBegin(_otrans, " <<
      type_to_enum(((t_map*)ttype)->get_key_type()) << ", " <<
      type_to_enum(((t_map*)ttype)->get_val_type()) << ", " <<
      prefix << ".size());" << endl;
  } else if (ttype->is_set()) {
    indent(f_service_) <<
      "_oprot->writeSetBegin(_otrans, " <<
      type_to_enum(((t_set*)ttype)->get_elem_type()) << ", " <<
      prefix << ".size());" << endl;
  } else if (ttype->is_list()) {
    indent(f_service_) <<
      "_oprot->writeListBegin(_otrans, " <<
      type_to_enum(((t_list*)ttype)->get_elem_type()) << ", " <<
      prefix << ".size());" << endl;
  }

  string iter = tmp("_iter");
  indent(f_service_) <<
    type_name(ttype) << "::const_iterator " << iter << ";" << endl;
  indent(f_service_) <<
    "for (" << iter << " = " << prefix  << ".begin(); " <<
    iter << " != " << prefix << ".end(); " <<
    "++" << iter << ")" << endl;

    scope_up(f_service_);

    if (ttype->is_map()) {
      generate_serialize_map_element((t_map*)ttype, iter);
    } else if (ttype->is_set()) {
      generate_serialize_set_element((t_set*)ttype, iter);
    } else if (ttype->is_list()) {
      generate_serialize_list_element((t_list*)ttype, iter);
    }
    
    if (ttype->is_map()) {
      indent(f_service_) <<
        "_oprot->writeMapEnd(_otrans);" << endl;
    } else if (ttype->is_set()) {
      indent(f_service_) <<
        "_oprot->writeSetEnd(_otrans);" << endl;
    } else if (ttype->is_list()) {
      indent(f_service_) <<
        "_oprot->writeListEnd(_otrans);" << endl;
    }
    
    scope_down(f_service_);
 
  scope_down(f_service_);  
}

/**
 * Serializes the members of a map.
 *
 */
void t_cpp_generator::generate_serialize_map_element(t_map* tmap,
                                                     string iter) {
  t_field kfield(tmap->get_key_type(), iter + "->first");
  generate_serialize_field(&kfield, "");

  t_field vfield(tmap->get_val_type(), iter + "->second");
  generate_serialize_field(&vfield, "");
}

/**
 * Serializes the members of a set.
 */
void t_cpp_generator::generate_serialize_set_element(t_set* tset,
                                                     string iter) {
  t_field efield(tset->get_elem_type(), "(*" + iter + ")");
  generate_serialize_field(&efield, "");
}

/**
 * Serializes the members of a list.
 */
void t_cpp_generator::generate_serialize_list_element(t_list* tlist,
                                                      string iter) {
  t_field efield(tlist->get_elem_type(), "(*" + iter + ")");
  generate_serialize_field(&efield, "");
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

/**
 * Converts the parse type to a C++ enum string for the given type.
 */
string t_cpp_generator::type_to_enum(t_type* type) {
  while (type->is_typedef()) {
    type = ((t_typedef*)type)->get_type();
  }
  
  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "NO T_VOID CONSTRUCT";
    case t_base_type::TYPE_STRING:
      return "facebook::thrift::protocol::T_STRING";
    case t_base_type::TYPE_BYTE:
      return "facebook::thrift::protocol::T_BYTE";
    case t_base_type::TYPE_I32:
      return "facebook::thrift::protocol::T_I32";
    case t_base_type::TYPE_U32:
      return "facebook::thrift::protocol::T_U32";
    case t_base_type::TYPE_I64:
      return "facebook::thrift::protocol::T_I64";
    case t_base_type::TYPE_U64:
      return "facebook::thrift::protocol::T_U64";
    }
  } else if (type->is_enum()) {
    return "facebook::thrift::protocol::T_I32";
  } else if (type->is_struct()) {
    return "facebook::thrift::protocol::T_STRUCT";
  } else if (type->is_map()) {
    return "facebook::thrift::protocol::T_MAP";
  } else if (type->is_set()) {
    return "facebook::thrift::protocol::T_SET";
  } else if (type->is_list()) {
    return "facebook::thrift::protocol::T_LIST";
  }

  throw "INVALID TYPE IN type_to_enum: " + type->get_name();
}
