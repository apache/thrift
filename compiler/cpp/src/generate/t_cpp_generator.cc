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
  string f_types_name = string(T_CPP_DIR)+"/"+program_name_+"_types.h";
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
    "#include <Thrift.h>" << endl <<
    endl;

  string open_ns = namespace_open(tprogram->get_namespace());

  f_types_ <<
    open_ns << endl <<
    endl;

  // Make output files
  string f_header_name = string(T_CPP_DIR)+"/"+program_name_+".h";
  f_header_.open(f_header_name.c_str());
  string f_service_name = string(T_CPP_DIR)+"/"+program_name_+".cc";
  f_service_.open(f_service_name.c_str());

  // Print header file includes
  f_header_ <<
    autogen_comment();
  f_header_ <<
    "#ifndef " << program_name_ << "_H" << endl <<
    "#define " << program_name_ << "_H" << endl <<
    endl <<
    "#include <Thrift.h>" << endl <<
    "#include <TProcessor.h>" << endl <<
    "#include <protocol/TProtocol.h>" << endl <<
    "#include <transport/TTransport.h>" << endl <<
    "#include \"" << program_name_ << "_types.h\"" << endl <<
    endl <<
    open_ns << endl <<
    endl;

  // Service implementation file includes
  f_service_ <<
    autogen_comment();
  f_service_ <<
    "#include \"" << program_name_ << ".h\"" << endl << 
    endl <<
    open_ns << endl <<
    endl;
}

/**
 * Closes the output files.
 */
void t_cpp_generator::close_generator(t_program* tprogram) {
  // Close ns
  string close_ns = namespace_close(tprogram->get_namespace());
  f_types_ <<
    close_ns << endl <<
    endl;
  f_header_ <<
    close_ns << endl <<
    endl;
  f_service_ <<
    close_ns << endl <<
    endl;

  // Close ifndef
  f_types_ <<
    "#endif" << endl;
  f_header_ <<
    "#endif" << endl;
  
  // Close output file
  f_types_.close();

  // Close files
  f_header_.close();
  f_service_.close();
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
  generate_struct_definition(f_types_, tstruct);
  generate_struct_reader(f_service_, tstruct);
  generate_struct_writer(f_service_, tstruct);
}

/**
 * Writes the struct def.
 *
 * @param out Output stream
 * @param tstruct The struct
 */
void t_cpp_generator::generate_struct_definition(ofstream& out,
                                                 t_struct* tstruct) {
  // Open struct def
  out <<
    indent() << "typedef struct _" << tstruct->get_name() << " {" << endl;
  
  indent_up();

  // Get members
  vector<t_field*>::const_iterator m_iter; 
  const vector<t_field*>& members = tstruct->get_members();

  // Default constructor
  bool init_ctor = false;
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_type* t = (*m_iter)->get_type();
    while (t->is_typedef()) {
      t = ((t_typedef*)t)->get_type();
    }
    if (t->is_base_type() &&
        ((t_base_type*)t)->get_base() != t_base_type::TYPE_STRING) {
      if (!init_ctor) {
        init_ctor = true;
        indent(out) <<
          "_" << tstruct->get_name() << "() : ";
        out << (*m_iter)->get_name() << "(0)";
      } else
        out << ", " << (*m_iter)->get_name() << "(0)";
    }
  }
  if (init_ctor) {
    out << " {} " << endl;
  }

  // Declare all fields
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    indent(out) <<
      declare_field(*m_iter) << endl;
  }

  // Isset vector
  if (members.size() > 0) {
    indent(out) <<
      "struct __isset {" << endl;
      indent_up();
      
      indent(out) <<
        "__isset() : ";
      bool first = true;
      for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
        if (first) {
          first = false;
          out <<
            (*m_iter)->get_name() << "(false)";
        } else {
          out <<
            ", " << (*m_iter)->get_name() << "(false)";
        }
      }
      out << " {}" << endl;
      
      for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
        indent(out) <<
          "bool " << (*m_iter)->get_name() << ";" << endl;
      }
      indent_down();
    indent(out) <<
      "} __isset;" << endl;  
  }

  indent_down(); 
  indent(out) <<
    "} " << tstruct->get_name() << ";" << endl <<
    endl;
}

/**
 * Makes a helper function to gen a struct reader.
 *
 * @param out Stream to write to
 * @param tstruct The struct
 */
void t_cpp_generator::generate_struct_reader(ofstream& out,
                                             t_struct* tstruct) {
  indent(out) <<
    "uint32_t read_struct_" << tstruct->get_name() << "(" <<
    "boost::shared_ptr<const facebook::thrift::protocol::TProtocol> iprot, " <<
    "boost::shared_ptr<facebook::thrift::transport::TTransport> itrans, " <<
    tstruct->get_name() << "& value) {" << endl;
  indent_up();

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  // Declare stack tmp variables
  out <<
    endl << 
    indent() << "uint32_t xfer = 0;" << endl <<
    indent() << "std::string fname;" << endl <<
    indent() << "facebook::thrift::protocol::TType ftype;" << endl <<
    indent() << "int16_t fid;" << endl <<
    endl <<
    indent() << "xfer += iprot->readStructBegin(itrans, fname);" << endl <<
    endl;
  
  // Loop over reading in fields
  indent(out) <<
    "while (true)" << endl;
    scope_up(out);
    
    // Read beginning field marker
    indent(out) <<
      "xfer += iprot->readFieldBegin(itrans, fname, ftype, fid);" << endl;
    
    // Check for field STOP marker
    out <<
      indent() << "if (ftype == facebook::thrift::protocol::T_STOP) { " << endl <<
      indent() << "  break;" << endl <<
      indent() << "}" << endl;
    
    // Switch statement on the field we are reading
    indent(out) <<
      "switch (fid)" << endl;

      scope_up(out);
    
      // Generate deserialization code for known cases
      for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
        indent(out) <<
          "case " << (*f_iter)->get_key() << ":" << endl;
        indent_up();
        generate_deserialize_field(*f_iter, "value.");
        out <<
          indent() << "value.__isset." << (*f_iter)->get_name() << " = true;" << endl <<
          indent() << "break;" << endl;
        indent_down();
      }
      
      // In the default case we skip the field
      out <<
        indent() << "default:" << endl <<
        indent() << "  xfer += iprot->skip(itrans, ftype);" << endl <<
        indent() << "  break;" << endl;
      
      scope_down(out);

    // Read field end marker
    indent(out) <<
      "xfer += iprot->readFieldEnd(itrans);" << endl;

    scope_down(out);

  out <<
    endl <<
    indent() << "xfer += iprot->readStructEnd(itrans);" << endl <<
    indent() <<"return xfer;" << endl;

  indent_down();
  indent(out) <<
    "}" << endl << endl;
}

/**
 * Makes a helper function to gen a struct writer.
 *
 * @param out Stream to write to
 * @param tstruct The struct
 */
void t_cpp_generator::generate_struct_writer(ofstream& out,
                                             t_struct* tstruct) {
  string name = tstruct->get_name();
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  indent(out) <<
    "uint32_t write_struct_" << name << "(" <<
    "boost::shared_ptr<const facebook::thrift::protocol::TProtocol> oprot, " <<
    "boost::shared_ptr<facebook::thrift::transport::TTransport> otrans, " <<
    "const " << name << "& value) {" << endl;
  indent_up();

  out <<
    endl <<
    indent() << "uint32_t xfer = 0;" << endl <<
    endl;

  indent(out) <<
    "xfer += oprot->writeStructBegin(otrans, \"" << name << "\");" << endl;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    // Write field header
    out <<
      endl <<
      indent() << "xfer += oprot->writeFieldBegin(otrans, " <<
      "\"" << (*f_iter)->get_name() << "\", " <<
      type_to_enum((*f_iter)->get_type()) << ", " <<
      (*f_iter)->get_key() << ");" << endl;
    // Write field contents
    generate_serialize_field(*f_iter, "value.");
    // Write field closer
    indent(out) <<
      "xfer += oprot->writeFieldEnd(otrans);" << endl <<
      endl;
  }
  // Write the struct map
  out <<
    indent() << "xfer += oprot->writeFieldStop(otrans);" << endl <<
    endl <<
    indent() << "xfer += oprot->writeStructEnd(otrans);" << endl <<
    indent() << "return xfer;" << endl;

  indent_down();
  indent(out) <<
    "}" << endl << endl;
}

/**
 * Struct writer for result of a function, which can have only one of its
 * fields set and does a conditional if else look up into the __isset field
 * of the struct.
 *
 * @param out Output stream
 * @param tstruct The result struct
 */
void t_cpp_generator::generate_struct_result_writer(ofstream& out,
                                                    t_struct* tstruct) {
  string name = tstruct->get_name();
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  indent(out) <<
    "uint32_t write_struct_" << name << "(" <<
    "boost::shared_ptr<const facebook::thrift::protocol::TProtocol> oprot, " <<
    "boost::shared_ptr<facebook::thrift::transport::TTransport> otrans, " <<
    "const " << name << "& value) {" << endl;
  indent_up();

  out <<
    endl <<
    indent() << "uint32_t xfer = 0;" << endl <<
    endl;

  indent(out) <<
    "xfer += oprot->writeStructBegin(otrans, \"" << name << "\");" << endl;

  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
      out << 
        endl <<
        indent() << "if ";
    } else {
      out <<
        " else if ";
    }

    out << "(value.__isset." << (*f_iter)->get_name() << ") {" << endl;
    
    indent_up();

    // Write field header
    out <<
      indent() << "xfer += oprot->writeFieldBegin(otrans, " <<
      "\"" << (*f_iter)->get_name() << "\", " <<
      type_to_enum((*f_iter)->get_type()) << ", " <<
      (*f_iter)->get_key() << ");" << endl;
    // Write field contents
    generate_serialize_field(*f_iter, "value.");
    // Write field closer
    indent(out) << "xfer += oprot->writeFieldEnd(otrans);" << endl;

    indent_down();
    indent(out) << "}";
  }

  // Write the struct map
  out <<
    endl <<
    indent() << "xfer += oprot->writeFieldStop(otrans);" << endl <<
    indent() << "xfer += oprot->writeStructEnd(otrans);" << endl <<
    indent() << "return xfer;" << endl;

  indent_down();
  indent(out) <<
    "}" << endl << endl;
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
  generate_service_interface(tservice);
  generate_service_helpers(tservice);
  generate_service_server(tservice);
  generate_service_client(tservice);
}

/**
 * Generates helper functions for a service.
 *
 * @param tservice The service to generate a header definition for
 */
void t_cpp_generator::generate_service_helpers(t_service* tservice) {
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter; 
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    t_struct* ts = (*f_iter)->get_arglist();
    generate_struct_definition(f_service_, ts);
    generate_struct_reader(f_service_, ts);
    generate_struct_writer(f_service_, ts);
    generate_function_helpers(*f_iter);
  }
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
  f_header_ <<
    indent() << "virtual ~" << service_name_ << "If() {}" << endl;
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter; 
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    f_header_ <<
      indent() << "virtual " << function_signature(*f_iter) << " = 0;" << endl;
  }
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
    "(boost::shared_ptr<facebook::thrift::transport::TTransport> trans, boost::shared_ptr<const facebook::thrift::protocol::TProtocol> prot) : " <<
    "_itrans(trans), _otrans(trans), " <<
    "_iprot(prot), _oprot(prot) {}" << endl;
  f_header_ <<
    indent() << service_name_ << "Client" <<
    "(boost::shared_ptr<facebook::thrift::transport::TTransport> itrans, boost::shared_ptr<facebook::thrift::transport::TTransport> otrans," <<
    " boost::shared_ptr<const facebook::thrift::protocol::TProtocol> iprot, boost::shared_ptr<const facebook::thrift::protocol::TProtocol> oprot) : " <<
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
    indent() << "boost::shared_ptr<const facebook::thrift::protocol::TProtocol>  _iprot;"  << endl <<
    indent() << "boost::shared_ptr<const facebook::thrift::protocol::TProtocol>  _oprot;"  << endl;
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

    // Open the send function
    indent(f_service_) <<
      function_signature(&send_function, scope) << endl;
    scope_up(f_service_);

    string argsname = (*f_iter)->get_name() + "_args";
    string resultname = (*f_iter)->get_name() + "_result";

    // Serialize the request
    f_service_ <<
      indent() << "int32_t cseqid = 0;" << endl <<
      indent() << "_oprot->writeMessageBegin(_otrans, \"" << (*f_iter)->get_name() << "\", facebook::thrift::protocol::T_CALL, cseqid);" << endl <<
      endl <<
      indent() << argsname << " __args;" << endl;
    
    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      f_service_ <<
        indent() << "__args." << (*fld_iter)->get_name() << " = " << (*fld_iter)->get_name() << ";" << endl;
    }
      
    f_service_ <<
      indent() << "write_struct_" <<  argsname << "(_oprot, _otrans, __args);" << endl <<
      endl <<
      indent() << "_oprot->writeMessageEnd(_otrans);" << endl <<
      indent() << "_otrans->flush();" << endl;
       
    scope_down(f_service_);
    f_service_ << endl;

    // Generate recv function only if not an async function
    if (!(*f_iter)->is_async()) {
      t_struct noargs;
      t_function recv_function((*f_iter)->get_returntype(),
                               string("recv_") + (*f_iter)->get_name(),
                               &noargs);
      // Open function
      indent(f_service_) <<
        function_signature(&recv_function, scope) << endl;
      scope_up(f_service_);

      f_service_ <<
        endl <<
        indent() << "int32_t rseqid = 0;" << endl <<
        indent() << "std::string fname;" << endl <<
        indent() << "facebook::thrift::protocol::TMessageType mtype;" << endl <<
        endl <<
        indent() << "_iprot->readMessageBegin(_itrans, fname, mtype, rseqid);" << endl <<
        indent() << "if (mtype != facebook::thrift::protocol::T_REPLY || fname.compare(\"" << (*f_iter)->get_name() << "\") != 0) {" << endl;
      indent_up();
        f_service_ <<
          indent() << "throw facebook::thrift::Exception(\"Unexpected message type, name, or id\");" << endl;
      indent_down();
      f_service_ <<
        indent() << "}" << endl;

      f_service_ <<
        endl <<
        indent() << resultname << " __result;" << endl;

      // Add a field to the return struct if non void
      f_service_ <<
        indent() << "read_struct_" << resultname << "(_iprot, _itrans, __result);" << endl <<
        indent() << "_iprot->readMessageEnd(_itrans);" << endl <<
        endl;


      // Careful, only return _result if not a void function
      if (!(*f_iter)->get_returntype()->is_void()) {
        f_service_ <<
          indent() << "if (__result.__isset.success) {" << endl <<
          indent() << "  return __result.success;" << endl <<
          indent() << "}" << endl;
      }

      t_struct* xs = (*f_iter)->get_xceptions();
      const std::vector<t_field*>& xceptions = xs->get_members();
      vector<t_field*>::const_iterator x_iter;
      for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
        f_service_ <<
          indent() << "if (__result.__isset." << (*x_iter)->get_name() << ") {" << endl <<
          indent() << "  throw __result." << (*x_iter)->get_name() << ";" << endl <<
          indent() << "}" << endl;
      }

      // Careful, only return _result if not a void function
      if ((*f_iter)->get_returntype()->is_void()) {
        indent(f_service_) <<
          "return;" << endl;
      } else {
        f_service_ <<
          indent() << "throw facebook::thrift::Exception(\"" << (*f_iter)->get_name() << " failed: unknown result\");" << endl;
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
    service_name_ << "ServerIf(boost::shared_ptr<const facebook::thrift::protocol::TProtocol> protocol) : " <<
                       "_iprot(protocol), _oprot(protocol) {}" << endl <<
    indent() <<
    service_name_ << "ServerIf(boost::shared_ptr<const facebook::thrift::protocol::TProtocol> iprot, boost::shared_ptr<const facebook::thrift::protocol::TProtocol> oprot) : " <<
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
    indent() << "boost::shared_ptr<const facebook::thrift::protocol::TProtocol> _iprot;" << endl <<
    indent() << "boost::shared_ptr<const facebook::thrift::protocol::TProtocol> _oprot;" << endl;
  indent_down();

  // Process function declarations
  f_header_ <<
    " private:" << endl;
  indent_up();
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    indent(f_header_) <<
      "void process_" << (*f_iter)->get_name() <<
      "(int32_t seqid, boost::shared_ptr<facebook::thrift::transport::TTransport> _itrans, boost::shared_ptr<facebook::thrift::transport::TTransport> _otrans);" << endl;
  }
  indent_down();
  f_header_ <<
    "};" << endl << endl;

  // Generate the server implementation
  f_service_ <<
    "bool " << service_name_ << "ServerIf::" <<
    "process(boost::shared_ptr<facebook::thrift::transport::TTransport> itrans, boost::shared_ptr<facebook::thrift::transport::TTransport> otrans) {" << endl;
  indent_up();

  f_service_ <<
    endl <<
    indent() << "std::string fname;" << endl <<
    indent() << "facebook::thrift::protocol::TMessageType mtype;" << endl <<
    indent() << "int32_t seqid;" << endl <<
    endl <<
    indent() << "_iprot->readMessageBegin(itrans, fname, mtype, seqid);" << endl <<
    endl <<
    indent() << "if (mtype != facebook::thrift::protocol::T_CALL) {" << endl <<
    indent() << "  throw facebook::thrift::Exception(\"Unexpected message type\");" << endl <<
    indent() << "}" << endl <<
    endl;
    
  bool first = true;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    if (!first) {
      f_service_ << " else ";
    } else {
      f_service_ << indent();
      first = false;
    }
    f_service_ <<
      "if (fname.compare(\"" << (*f_iter)->get_name() <<"\") == 0) {" << endl;
    indent_up();
    indent(f_service_) <<
      "process_" << (*f_iter)->get_name() <<
      "(seqid, itrans, otrans);" << endl;
    indent_down();
    indent(f_service_) << "}";
  }
  indent(f_service_) <<
    " else {" << endl;
  indent_up();
  indent(f_service_) <<
    "throw facebook::thrift::Exception(\"Unknown function name: '\"+fname+\"'\");" << endl;
  indent_down();
  indent(f_service_) <<
    "}" << endl;

  // Read end of args field, the T_STOP, and the struct close
  f_service_ <<
    indent() << "return true;" << endl;

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
 * Generates a struct and helpers for a function.
 *
 * @param tfunction The function
 */
void t_cpp_generator::generate_function_helpers(t_function* tfunction) {
  t_struct result(tfunction->get_name() + "_result");
  t_field success(tfunction->get_returntype(), "success", 0);
  if (!tfunction->get_returntype()->is_void()) {
    result.append(&success);
  }

  t_struct* xs = tfunction->get_xceptions();
  const vector<t_field*>& fields = xs->get_members();
  vector<t_field*>::const_iterator f_iter;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    result.append(*f_iter);
  }

  generate_struct_definition(f_service_, &result);
  generate_struct_reader(f_service_, &result);
  generate_struct_result_writer(f_service_, &result);
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
    "(int32_t seqid, boost::shared_ptr<facebook::thrift::transport::TTransport> itrans, boost::shared_ptr<facebook::thrift::transport::TTransport> otrans)" << endl;
  scope_up(f_service_);

  string argsname = tfunction->get_name() + "_args";
  string resultname = tfunction->get_name() + "_result";

  f_service_ <<
    indent() << argsname << " __args;" << endl <<
    indent() << "read_struct_" << argsname << "(_iprot, itrans, __args);" << endl <<
    indent() << "_iprot->readMessageEnd(itrans);" << endl <<
    endl <<
    indent() << resultname << " __result;" << endl;
 
  t_struct* xs = tfunction->get_xceptions();
  const std::vector<t_field*>& xceptions = xs->get_members();
  vector<t_field*>::const_iterator x_iter;

  if (xceptions.size() > 0) {
    f_service_ <<
      indent() << "try {" << endl;
    indent_up();
  }

  // Generate the function call
  t_struct* arg_struct = tfunction->get_arglist();
  const std::vector<t_field*>& fields = arg_struct->get_members();
  vector<t_field*>::const_iterator f_iter;

  f_service_ << indent();
  if (!tfunction->get_returntype()->is_void()) {
    f_service_ << "__result.success = ";    
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
    f_service_ << "__args." << (*f_iter)->get_name();
  }
  f_service_ << ");" << endl;

  // Set isset on success field
  if (!tfunction->get_returntype()->is_void()) {
    f_service_ <<
      indent() << "__result.__isset.success = true;" << endl;
  }

  if (xceptions.size() > 0) {
    indent_down();
    f_service_ << indent() << "}";
    for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
      f_service_ << " catch (" << (*x_iter)->get_type()->get_name() << " &" << (*x_iter)->get_name() << ") {" << endl;
      indent_up();
      f_service_ <<
        indent() << "__result." << (*x_iter)->get_name() << " = " << (*x_iter)->get_name() << ";" << endl <<
        indent() << "__result.__isset." << (*x_iter)->get_name() << " = true;" << endl;
      indent_down();
      f_service_ << indent() << "}";
    }
    f_service_ << endl;
  }


  // Serialize the result into a struct
  f_service_ <<
    endl <<
    indent() << "_oprot->writeMessageBegin(otrans, \"" << tfunction->get_name() << "\", facebook::thrift::protocol::T_REPLY, seqid);" << endl <<
    indent() << "write_struct_" << resultname << "(_oprot, otrans, __result);" << endl <<
    indent() << "_oprot->writeMessageEnd(otrans);" << endl <<
    indent() << "otrans->flush();" << endl;
    
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

  if (type->is_struct() || type->is_xception()) {
    generate_deserialize_struct((t_struct*)(tfield->get_type()), name);
  } else if (type->is_container()) {
    generate_deserialize_container(tfield->get_type(), name);
  } else if (type->is_base_type() || type->is_enum()) {

    indent(f_service_) <<
      "xfer += iprot->";
    
    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "compiler error: cannot serialize void field in a struct: " +
          name;
        break;
      case t_base_type::TYPE_STRING:
        f_service_ << "readString(itrans, " << name << ");";
        break;
      case t_base_type::TYPE_BYTE:
        f_service_ << "readByte(itrans, " << name << ");";
        break;
      case t_base_type::TYPE_I16:
        f_service_ << "readI16(itrans, " << name << ");";
        break;
      case t_base_type::TYPE_I32:
        f_service_ << "readI32(itrans, " << name << ");";
        break;
      case t_base_type::TYPE_I64:
        f_service_ << "readI64(itrans, " << name << ");";
        break;
      default:
        throw "compiler error: no C++ reader for base type " + tbase + name;
      }
    } else if (type->is_enum()) {
      f_service_ << "readI32(itrans, (int32_t&)" << name << ");";
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
  indent(f_service_) <<
    "xfer += read_struct_" << tstruct->get_name() << "(iprot, itrans, " << prefix << ");" << endl;
}

void t_cpp_generator::generate_deserialize_container(t_type* ttype,
                                                     string prefix) {
  scope_up(f_service_);
  
  string size = tmp("_size");
  string ktype = tmp("_ktype");
  string vtype = tmp("_vtype");
  string etype = tmp("_etype");
  
  indent(f_service_) <<
    "uint32_t " << size << ";" << endl;
  
  // Declare variables, read header
  if (ttype->is_map()) {
    f_service_ <<
      indent() << "facebook::thrift::protocol::TType " << ktype << ";" << endl <<
      indent() << "facebook::thrift::protocol::TType " << vtype << ";" << endl <<
      indent() << "iprot->readMapBegin(itrans, " <<
                   ktype << ", " << vtype << ", " << size << ");" << endl;
  } else if (ttype->is_set()) {
    f_service_ <<
      indent() << "facebook::thrift::protocol::TType " << etype << ";" << endl <<
      indent() << "iprot->readSetBegin(itrans, " <<
                   etype << ", " << size << ");" << endl;
  } else if (ttype->is_list()) {
    f_service_ <<
      indent() << "facebook::thrift::protocol::TType " << etype << ";" << endl <<
      indent() << "iprot->readListBegin(itrans, " <<
                   etype << ", " << size << ");" << endl;
  }


  // For loop iterates over elements
  string i = tmp("_i");
  indent(f_service_) <<
    "uint32_t " << i << ";" << endl;
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
    indent(f_service_) << "iprot->readMapEnd(itrans);" << endl;
  } else if (ttype->is_set()) {
    indent(f_service_) << "iprot->readSetEnd(itrans);" << endl;
  } else if (ttype->is_list()) {
    indent(f_service_) << "iprot->readListEnd(itrans);" << endl;
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
  
  if (type->is_struct() || type->is_xception()) {
    generate_serialize_struct((t_struct*)(tfield->get_type()),
                              prefix + tfield->get_name());
  } else if (type->is_container()) {
    generate_serialize_container(tfield->get_type(),
                                 prefix + tfield->get_name());
  } else if (type->is_base_type() || type->is_enum()) {

    string name = prefix + tfield->get_name();
    indent(f_service_) <<
      "xfer += oprot->";
    
    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw
          "compiler error: cannot serialize void field in a struct: " + name;
        break;
      case t_base_type::TYPE_STRING:
        f_service_ << "writeString(otrans, " << name << ");";
        break;
      case t_base_type::TYPE_BYTE:
        f_service_ << "writeByte(otrans, " << name << ");";
        break;
      case t_base_type::TYPE_I16:
        f_service_ << "writeI16(otrans, " << name << ");";
        break;
      case t_base_type::TYPE_I32:
        f_service_ << "writeI32(otrans, " << name << ");";
        break;
      case t_base_type::TYPE_I64:
        f_service_ << "writeI64(otrans, " << name << ");";
        break;
      default:
        throw "compiler error: no C++ writer for base type " + tbase + name;
      }
    } else if (type->is_enum()) {
      f_service_ << "writeI32(otrans, (int32_t)" << name << ");";
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
  indent(f_service_) <<
    "xfer += write_struct_" << tstruct->get_name() << "(oprot, otrans, " << prefix << ");" << endl;
}

void t_cpp_generator::generate_serialize_container(t_type* ttype,
                                                   string prefix) {
  scope_up(f_service_);
  
  if (ttype->is_map()) {
    indent(f_service_) <<
      "xfer += oprot->writeMapBegin(otrans, " <<
      type_to_enum(((t_map*)ttype)->get_key_type()) << ", " <<
      type_to_enum(((t_map*)ttype)->get_val_type()) << ", " <<
      prefix << ".size());" << endl;
  } else if (ttype->is_set()) {
    indent(f_service_) <<
      "xfer += oprot->writeSetBegin(otrans, " <<
      type_to_enum(((t_set*)ttype)->get_elem_type()) << ", " <<
      prefix << ".size());" << endl;
  } else if (ttype->is_list()) {
    indent(f_service_) <<
      "xfer += oprot->writeListBegin(otrans, " <<
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

    scope_down(f_service_);
    
    if (ttype->is_map()) {
      indent(f_service_) <<
        "xfer += oprot->writeMapEnd(otrans);" << endl;
    } else if (ttype->is_set()) {
      indent(f_service_) <<
        "xfer += oprot->writeSetEnd(otrans);" << endl;
    } else if (ttype->is_list()) {
      indent(f_service_) <<
        "xfer += oprot->writeListEnd(otrans);" << endl;
    }    

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
 * Opens namespace.
 *
 * @param ns The namepsace, w/ periods in it
 * @return Namespaces
 */
string t_cpp_generator::namespace_open(string ns) {
  if (ns.size() == 0) {
    return "";
  }
  string result = "";
  string::size_type loc;
  while ((loc = ns.find(".")) != string::npos) {
    result += "namespace ";
    result += ns.substr(0, loc);
    result += " { ";
    ns = ns.substr(loc+1);
  }
  if (ns.size() > 0) {
    result += "namespace " + ns + " { ";
  }
  return result;
}

/**
 * Closes namespace.
 *
 * @param ns The namepsace, w/ periods in it
 * @return Namespaces
 */
string t_cpp_generator::namespace_close(string ns) {
  if (ns.size() == 0) {
    return "";
  }
  string result = "}";
  string::size_type loc;
  while ((loc = ns.find(".")) != string::npos) {
    result += "}";
    ns = ns.substr(loc+1);
  }
  result += " // namespace";
  return result;
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
    return "int8_t";
  case t_base_type::TYPE_I16:
    return "int16_t";
  case t_base_type::TYPE_I32:
    return "int32_t";
  case t_base_type::TYPE_I64:
    return "int64_t";
  default:
    throw "compiler error: no C++ base type name for base type " + tbase;
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
      case t_base_type::TYPE_I16:
      case t_base_type::TYPE_I32:
      case t_base_type::TYPE_I64:
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
    case t_base_type::TYPE_I16:
      return "facebook::thrift::protocol::T_I16";
    case t_base_type::TYPE_I32:
      return "facebook::thrift::protocol::T_I32";
    case t_base_type::TYPE_I64:
      return "facebook::thrift::protocol::T_I64";
    }
  } else if (type->is_enum()) {
    return "facebook::thrift::protocol::T_I32";
  } else if (type->is_struct()) {
    return "facebook::thrift::protocol::T_STRUCT";
  } else if (type->is_xception()) {
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
