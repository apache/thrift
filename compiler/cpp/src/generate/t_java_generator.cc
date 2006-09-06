#include <stdlib.h>
#include <sys/stat.h>
#include <sstream>
#include "t_java_generator.h"
using namespace std;

/**
 * Prepares for file generation by opening up the necessary file output
 * streams.
 *
 * @param tprogram The program to generate
 */
void t_java_generator::init_generator(t_program* tprogram) {
  // Make output directory
  mkdir(T_JAVA_DIR, S_IREAD | S_IWRITE | S_IEXEC);
  package_name_ = tprogram->get_namespace();
}

/**
 * Packages the generated file
 */
string t_java_generator::java_package() {
  // TODO(mcslee): Allow destination package to be specified in .thrift file
  return string("package ") + package_name_ + ";\n\n";
}

/**
 * Prints standard java imports
 */
string t_java_generator::java_type_imports() {
  return
    string() +
    "import java.util.ArrayList;\n" +
    "import java.util.HashMap;\n" +
    "import java.util.HashSet;\n" +
    "import com.facebook.thrift.*;\n\n";
}

/**
 * Prints standard java imports
 */
string t_java_generator::java_thrift_imports() {
  return
    string() +
    "import com.facebook.thrift.protocol.*;\n" +
    "import com.facebook.thrift.transport.*;\n\n";
}

/**
 * Does nothing in Java
 */
void t_java_generator::close_generator(t_program *tprogram) {}

/**
 * Generates a typedef. This is not done in Java.
 *
 * @param ttypedef The type definition
 */
void t_java_generator::generate_typedef(t_typedef* ttypedef) {}

/**
 * Generates code for an enumerated type. In C++, this is essentially the same
 * as the thrift definition itself, using the enum keyword in C++.
 *
 * @param tenum The enumeration
 */
void t_java_generator::generate_enum(t_enum* tenum) {
  // Make output file
  string f_enum_name = string(T_JAVA_DIR)+"/"+(tenum->get_name())+".java";
  ofstream f_enum;
  f_enum.open(f_enum_name.c_str());

  f_enum <<
    autogen_comment() <<
    java_package() << endl;

  f_enum <<
    "public class " << tenum->get_name() << " ";
  scope_up(f_enum);

  vector<t_constant*> constants = tenum->get_constants();
  vector<t_constant*>::iterator c_iter;
  int value = -1;
  for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    if ((*c_iter)->has_value()) {
      value = (*c_iter)->get_value();
    } else {
      ++value;
    }

    indent(f_enum) <<
      "public static final int " << (*c_iter)->get_name() <<
      " = " << value << ";" << endl;
  }

  scope_down(f_enum);
}

/**
 * Generates a struct definition for a thrift data type. In C++, this is just
 * simple C struct with basic data members. There are no constructors,
 * initializers, etc.
 *
 * @param tstruct The struct definition
 */
void t_java_generator::generate_struct(t_struct* tstruct) {
  generate_java_struct(tstruct, false);
}

void t_java_generator::generate_xception(t_struct* txception) {
  generate_java_struct(txception, true);
}

void t_java_generator::generate_java_struct(t_struct* tstruct,
                                            bool is_exception) {
  // Make output file
  string f_struct_name = string(T_JAVA_DIR)+"/"+(tstruct->get_name())+".java";
  ofstream f_struct;
  f_struct.open(f_struct_name.c_str());

  f_struct <<
    autogen_comment() <<
    java_package() <<
    java_type_imports() <<
    java_thrift_imports();

  generate_java_struct_definition(f_struct,
                                  tstruct,
                                  is_exception);
  f_struct.close();
}

void t_java_generator::generate_java_struct_definition(ofstream &out,
                                                       t_struct* tstruct,
                                                       bool is_exception,
                                                       bool in_class,
                                                       bool is_result) {
  out <<
    "public " << (in_class ? "static " : "") << "class " << tstruct->get_name() << " ";
  
  if (is_exception) {
    out << "extends Exception ";
  }
  
  scope_up(out);

  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter; 
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    indent(out) <<
      "public " << declare_field(*m_iter, true) << endl;
  }

  if (members.size() > 0) {
    out <<
      endl <<
      indent() << "Isset __isset = new Isset();" << endl <<
      indent() << "public final class Isset {" << endl;
    indent_up();
      for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
        indent(out) <<
          "public boolean " << (*m_iter)->get_name() << " = false;" <<  endl;
      }
    indent_down();
    out <<
      indent() << "}" << endl <<
      endl;
  }
  
  generate_java_struct_reader(out, tstruct);
  if (is_result) {
    generate_java_struct_result_writer(out, tstruct);
  } else {
    generate_java_struct_writer(out, tstruct);
  }
  scope_down(out);
  out << endl;
}

void t_java_generator::generate_java_struct_reader(ofstream& out,
                                                   t_struct* tstruct) {
  out <<
    indent() << "public void read(TProtocol _iprot, TTransport _itrans) throws TException {" << endl;
  indent_up();

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  // Declare stack tmp variables
  out <<
    indent() << "TField _field;" << endl <<
    indent() << "TStruct _struct = _iprot.readStructBegin(_itrans);" << endl;
  
  // Loop over reading in fields
  indent(out) <<
    "while (true)" << endl;

    scope_up(out);
    
    // Read beginning field marker
    indent(out) <<
      "_field = _iprot.readFieldBegin(_itrans);" << endl;
    
    // Check for field STOP marker and break
    indent(out) <<
      "if (_field.type == TType.STOP) { " << endl;
    indent_up();
    indent(out) <<
      "break;" << endl;
    indent_down();
    indent(out) <<
      "}" << endl;
    
    // Switch statement on the field we are reading
    indent(out) <<
      "switch (_field.id)" << endl;

      scope_up(out);
    
      // Generate deserialization code for known cases
      for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
        indent(out) <<
          "case " << (*f_iter)->get_key() << ":" << endl;
        indent_up();
        generate_deserialize_field(out, *f_iter, "this.");
        out <<
          indent() << "this.__isset." << (*f_iter)->get_name() << " = true;" << endl;
        indent(out) <<
          "break;" << endl;
        indent_down();
      }
      
      // In the default case we skip the field
      out <<
        indent() << "default:" << endl <<
        indent() << "  TProtocolUtil.skip(_iprot, _itrans, _field.type);" << endl <<
        indent() << "  break;" << endl;
      
      scope_down(out);

    // Read field end marker
    indent(out) <<
      "_iprot.readFieldEnd(_itrans);" << endl;
    
    scope_down(out);
      
    out <<
      indent() << "_iprot.readStructEnd(_itrans);" << endl;

  indent_down();
  out <<
    indent() << "}" << endl <<
    endl;
}

void t_java_generator::generate_java_struct_writer(ofstream& out,
                                                   t_struct* tstruct) {
  out <<
    indent() << "public void write(TProtocol _oprot, TTransport _otrans) throws TException {" << endl;
  indent_up();

  string name = tstruct->get_name();
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  out <<
    indent() << "TStruct _struct = new TStruct(\"" << name << "\");" << endl <<
    indent() << "TField _field = new TField();" << endl <<
    indent() << "_oprot.writeStructBegin(_otrans, _struct);" << endl;

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    out <<
      indent() << "_field.name = \"" << (*f_iter)->get_name() << "\";" << endl <<
      indent() << "_field.type = " << type_to_enum((*f_iter)->get_type()) << ";" << endl <<
      indent() << "_field.id = " << (*f_iter)->get_key() << ";" << endl <<
      indent() << "_oprot.writeFieldBegin(_otrans, _field);" << endl;

    // Write field contents
    generate_serialize_field(out, *f_iter, "this.");

    // Write field closer
    indent(out) <<
      "_oprot.writeFieldEnd(_otrans);" << endl;
  }
  // Write the struct map
  out <<
    indent() << "_oprot.writeFieldStop(_otrans);" << endl <<
    indent() << "_oprot.writeStructEnd(_otrans);" << endl;

  indent_down();
  out <<
    indent() << "}" << endl;
}

void t_java_generator::generate_java_struct_result_writer(ofstream& out,
                                                          t_struct* tstruct) {
  out <<
    indent() << "public void write(TProtocol _oprot, TTransport _otrans) throws TException {" << endl;
  indent_up();

  string name = tstruct->get_name();
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  out <<
    indent() << "TStruct _struct = new TStruct(\"" << name << "\");" << endl <<
    indent() << "TField _field = new TField();" << endl <<
    indent() << "_oprot.writeStructBegin(_otrans, _struct);" << endl;

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

    out <<
      "(this.__isset." << (*f_iter)->get_name() << ") {" << endl;
    indent_up();

    out <<
      indent() << "_field.name = \"" << (*f_iter)->get_name() << "\";" << endl <<
      indent() << "_field.type = " << type_to_enum((*f_iter)->get_type()) << ";" << endl <<
      indent() << "_field.id = " << (*f_iter)->get_key() << ";" << endl <<
      indent() << "_oprot.writeFieldBegin(_otrans, _field);" << endl;

    // Write field contents
    generate_serialize_field(out, *f_iter, "this.");

    // Write field closer
    indent(out) <<
      "_oprot.writeFieldEnd(_otrans);" << endl;

    indent_down();
    indent(out) << "}";
  }
  // Write the struct map
  out <<
    endl <<
    indent() << "_oprot.writeFieldStop(_otrans);" << endl <<
    indent() << "_oprot.writeStructEnd(_otrans);" << endl;

  indent_down();
  out <<
    indent() << "}" << endl <<
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
void t_java_generator::generate_service(t_service* tservice) {
  // Make output file
  string f_service_name = string(T_JAVA_DIR)+"/"+service_name_+".java";
  f_service_.open(f_service_name.c_str());

  f_service_ <<
    autogen_comment() <<
    java_package() <<
    java_type_imports() <<
    java_thrift_imports();

  f_service_ <<
    "public class " << service_name_ << " {" << endl <<
    endl;
  indent_up();

  // Generate the three main parts of the service
  generate_service_interface(tservice);
  generate_service_client(tservice);
  generate_service_server(tservice);
  generate_service_helpers(tservice);

  indent_down();
  f_service_ <<
    "}" << endl;
  f_service_.close();
}

/**
 * Generates a service interface definition.
 *
 * @param tservice The service to generate a header definition for
 */
void t_java_generator::generate_service_interface(t_service* tservice) {
  f_service_ <<
    indent() << "public interface Iface {" << endl;
  indent_up();
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter; 
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    indent(f_service_) <<
      "public " << function_signature(*f_iter) << ";" << endl;
  }
  indent_down();
  f_service_ <<
    indent() << "}" << endl <<
    endl;
}

void t_java_generator::generate_service_helpers(t_service* tservice) {
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter; 
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    t_struct* ts = (*f_iter)->get_arglist();
    generate_java_struct_definition(f_service_, ts, false, true);
    generate_function_helpers(*f_iter);
  }
}

/**
 * Generates a service client definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_java_generator::generate_service_client(t_service* tservice) {
  f_service_ <<
    "public static class Client implements Iface {" << endl;
  indent_up();

  indent(f_service_) <<
    "public Client(TTransport trans, TProtocol prot)" << endl;
  scope_up(f_service_);
  indent(f_service_) << 
    "this(trans, trans, prot, prot);" << endl;
  scope_down(f_service_);
  f_service_ << endl;

  indent(f_service_) <<
    "public Client(TTransport itrans, TTransport otrans," <<
    " TProtocol iprot, TProtocol oprot)" << endl;
  scope_up(f_service_);
  f_service_ <<
    indent() << "_itrans = itrans;" << endl <<
    indent() << "_otrans = otrans;" << endl <<
    indent() << "_iprot = iprot;" << endl <<
    indent() << "_oprot = oprot;" << endl;
  scope_down(f_service_);
  f_service_ << endl;
 
  f_service_ <<
    indent() << "private TTransport _itrans;" << endl <<
    indent() << "private TTransport _otrans;" << endl <<
    indent() << "private TProtocol  _iprot;"  << endl <<
    indent() << "private TProtocol  _oprot;"  << endl <<
    endl <<
    indent() << "private int _seqid;" << endl <<
    endl;

  // Generate client method implementations
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::const_iterator f_iter; 
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    string funname = (*f_iter)->get_name();

    // Open function
    indent(f_service_) <<
      "public " << function_signature(*f_iter) << endl;
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

    string argsname = (*f_iter)->get_name() + "_args";

    // Open function
    indent(f_service_) <<
      "public " << function_signature(&send_function) << endl;
    scope_up(f_service_);

    // Serialize the request
    f_service_ <<
      indent() << "_oprot.writeMessageBegin(_otrans, new TMessage(\"" << funname << "\", TMessageType.CALL, _seqid));" << endl <<
      indent() << argsname << " __args = new " << argsname << "();" << endl;

    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      f_service_ <<
        indent() << "__args." << (*fld_iter)->get_name() << " = " << (*fld_iter)->get_name() << ";" << endl;
    }

    f_service_ <<
      indent() << "__args.write(_oprot, _otrans);" << endl <<
      indent() << "_oprot.writeMessageEnd(_otrans);" << endl <<
      indent() << "_otrans.flush();" << endl;

    scope_down(f_service_);
    f_service_ << endl;

    if (!(*f_iter)->is_async()) {
      string resultname = (*f_iter)->get_name() + "_result";

      t_struct noargs;
      t_function recv_function((*f_iter)->get_returntype(),
                               string("recv_") + (*f_iter)->get_name(),
                               &noargs,
                               (*f_iter)->get_xceptions());
      // Open function
      indent(f_service_) <<
        "public " << function_signature(&recv_function) << endl;
      scope_up(f_service_);
           
      // TODO(mcslee): Message validation here

      f_service_ <<
        indent() << "TMessage _msg = _iprot.readMessageBegin(_itrans);" << endl <<
        indent() << resultname << " __result = new " << resultname << "();" << endl <<
        indent() << "__result.read(_iprot, _itrans);" << endl <<
        indent() << "_iprot.readMessageEnd(_itrans);" << endl;

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
          indent() << "throw new TException(\"" << (*f_iter)->get_name() << " failed: unknown result\");" << endl;
      }
      
      // Close function
      scope_down(f_service_);
      f_service_ << endl;
    }
  }

  indent_down();
  f_service_ <<
    "}" << endl;
}

/**
 * Generates a service server definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_java_generator::generate_service_server(t_service* tservice) {
  // Generate the dispatch methods
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter; 

  // Generate the header portion
  f_service_ <<
    "public static class Server implements TProcessor {" << endl;
  indent_up();

  indent(f_service_) <<
    "public Server(Iface iface, TProtocol prot)" << endl;
  scope_up(f_service_);
  indent(f_service_) << 
    "this(iface, prot, prot);" << endl;
  scope_down(f_service_);
  f_service_ << endl;

  indent(f_service_) <<
    "public Server(Iface iface, TProtocol iprot, TProtocol oprot)" << endl;
  scope_up(f_service_);
  f_service_ <<
    indent() << "_iface = iface;" << endl <<
    indent() << "_iprot = iprot;" << endl <<
    indent() << "_oprot = oprot;" << endl;
  scope_down(f_service_);
  f_service_ << endl;
 
  f_service_ <<
    indent() << "private Iface _iface;" << endl <<
    indent() << "private TProtocol _iprot;" << endl <<
    indent() << "private TProtocol _oprot;" << endl << endl;
  
  // Generate the server implementation
  indent(f_service_) <<
    "public boolean process(TTransport _itrans, TTransport _otrans) " <<
    "throws TException" << endl;
  scope_up(f_service_);

  f_service_ <<
    indent() << "TMessage _msg = _iprot.readMessageBegin(_itrans);" << endl;

  // TODO(mcslee): validate message

  bool first = true;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    if (!first) {
      f_service_ << " else ";
    } else {
      f_service_ << indent();
      first = false;
    }
    f_service_ <<
      "if (_msg.name.equals(\"" << (*f_iter)->get_name() <<"\")) {" << endl;
    indent_up();
    indent(f_service_) <<
      "process_" << (*f_iter)->get_name() <<
      "(_msg.seqid, _itrans, _otrans);" << endl;
    indent_down();
    indent(f_service_) << "}";
  }
  f_service_ <<
    " else {" << endl;
  indent_up();
  indent(f_service_) <<
    "System.err.println" <<
    "(\"Unknown function: '\" + _msg.name + \"'\");" << endl;
  indent_down();
  indent(f_service_) <<
    "}" << endl;
  
  // Read end of args field, the T_STOP, and the struct close
  f_service_ <<
    indent() << "return true;" << endl;

  scope_down(f_service_);
  f_service_ << endl;

  // Generate the process subfunctions
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    generate_process_function(tservice, *f_iter);
  }

  indent_down();
  f_service_ <<
    "}" << endl <<
    endl;
}

/**
 * Generates a struct and helpers for a function.
 *
 * @param tfunction The function
 */
void t_java_generator::generate_function_helpers(t_function* tfunction) {
  if (tfunction->is_async()) {
    return;
  }

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

  generate_java_struct_definition(f_service_, &result, false, true, true);
}

/**
 * Generates a process function definition.
 *
 * @param tfunction The function to write a dispatcher for
 */
void t_java_generator::generate_process_function(t_service* tservice,
                                                 t_function* tfunction) {
  // Open function
  indent(f_service_) <<
    "private void process_" << tfunction->get_name() <<
    "(int seqid, TTransport _itrans, TTransport _otrans) throws TException" << endl;
  scope_up(f_service_);

  string argsname = tfunction->get_name() + "_args";
  string resultname = tfunction->get_name() + "_result";

  f_service_ <<
    indent() << argsname << " __args = new " << argsname << "();" << endl <<
    indent() << "__args.read(_iprot, _itrans);" << endl <<
    indent() << "_iprot.readMessageEnd(_itrans);" << endl;

  t_struct* xs = tfunction->get_xceptions();
  const std::vector<t_field*>& xceptions = xs->get_members();
  vector<t_field*>::const_iterator x_iter;

  // Declare result for non async function
  if (!tfunction->is_async()) {
    f_service_ <<
      indent() << resultname << " __result = new " << resultname << "();" << endl;
  }

  // Try block for a function with exceptions
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
  if (!tfunction->is_async() && !tfunction->get_returntype()->is_void()) {
    f_service_ << "__result.success = ";
  }
  f_service_ <<
    "_iface." << tfunction->get_name() << "(";
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
  if (!tfunction->is_async() && !tfunction->get_returntype()->is_void()) {
    f_service_ <<
      indent() << "__result.__isset.success = true;" << endl;
  }

  if (!tfunction->is_async() && xceptions.size() > 0) {
    indent_down();
    f_service_ << indent() << "}";
    for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
      f_service_ << " catch (" << (*x_iter)->get_type()->get_name() << " " << (*x_iter)->get_name() << ") {" << endl;
      if (!tfunction->is_async()) {
        indent_up();
        f_service_ <<
          indent() << "__result." << (*x_iter)->get_name() << " = " << (*x_iter)->get_name() << ";" << endl <<
          indent() << "__result.__isset." << (*x_iter)->get_name() << " = true;" << endl;
        indent_down();
        f_service_ << indent() << "}";
      } else {
        f_service_ << "}";
      }
    }
    f_service_ << endl;
  }

  // Shortcut out here for async functions
  if (tfunction->is_async()) {
    f_service_ <<
      indent() << "return;" << endl;
    indent_down();
    f_service_ << "}" << endl <<
      endl;
    return;
  }

  f_service_ <<
    indent() << "_oprot.writeMessageBegin(_otrans, new TMessage(\"" << tfunction->get_name() << "\", TMessageType.REPLY, seqid));" << endl <<
    indent() << "__result.write(_oprot, _otrans);" << endl <<
    indent() << "_oprot.writeMessageEnd(_otrans);" << endl <<
    indent() << "_otrans.flush();" << endl;

  // Close function
  scope_down(f_service_);
  f_service_ << endl;
}

/**
 * Deserializes a field of any type.
 */
void t_java_generator::generate_deserialize_field(ofstream& out,
                                                  t_field* tfield,
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
    generate_deserialize_struct(out,
                                (t_struct*)(tfield->get_type()),
                                name);
  } else if (type->is_container()) {
    generate_deserialize_container(out, tfield->get_type(), name);
  } else if (type->is_base_type() || type->is_enum()) {

    indent(out) <<
      name << " = _iprot.";
    
    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "compiler error: cannot serialize void field in a struct: " +
          name;
        break;
      case t_base_type::TYPE_STRING:        
        out << "readString(_itrans);";
        break;
      case t_base_type::TYPE_BOOL:
        out << "readBool(_itrans);";
        break;
      case t_base_type::TYPE_BYTE:
        out << "readByte(_itrans);";
        break;
      case t_base_type::TYPE_I16:
        out << "readI16(_itrans);";
        break;
      case t_base_type::TYPE_I32:
        out << "readI32(_itrans);";
        break;
      case t_base_type::TYPE_I64:
        out << "readI64(_itrans);";
        break;
      case t_base_type::TYPE_DOUBLE:
        out << "readDouble(_itrans);";
        break;
      default:
        throw "compiler error: no Java name for base type " + tbase;
      }
    } else if (type->is_enum()) {
      out << "readI32(_itrans);";
    }
    out <<
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
void t_java_generator::generate_deserialize_struct(ofstream& out,
                                                   t_struct* tstruct,
                                                   string prefix) {
  out <<
    indent() << prefix << " = new " << tstruct->get_name() << "();" << endl <<
    indent() << prefix << ".read(_iprot, _itrans);" << endl;
}

void t_java_generator::generate_deserialize_container(ofstream& out,
                                                      t_type* ttype,
                                                      string prefix) {
  scope_up(out);
  
  string obj;

  if (ttype->is_map()) {
    obj = tmp("_map");
  } else if (ttype->is_set()) {
    obj = tmp("_set");
  } else if (ttype->is_list()) {
    obj = tmp("_list");
  }

  // Declare variables, read header
  if (ttype->is_map()) {
    out <<
      indent() << "TMap " << obj << " = _iprot.readMapBegin(_itrans);" << endl;
  } else if (ttype->is_set()) {
    out <<
      indent() << "TSet " << obj << " = _iprot.readSetBegin(_itrans);" << endl;
  } else if (ttype->is_list()) {
    out <<
      indent() << "TList " << obj << " = _iprot.readListBegin(_itrans);" << endl;
  }


  // For loop iterates over elements
  string i = tmp("_i");
  indent(out) <<
    "for (int " << i << " = 0; " <<
    i << " < " << obj << ".size" << "; " <<
    "++" << i << ")" << endl;
  
    scope_up(out);
    
    if (ttype->is_map()) {
      generate_deserialize_map_element(out, (t_map*)ttype, prefix);
    } else if (ttype->is_set()) {
      generate_deserialize_set_element(out, (t_set*)ttype, prefix);
    } else if (ttype->is_list()) {
      generate_deserialize_list_element(out, (t_list*)ttype, prefix);
    }
    
    scope_down(out);

  // Read container end
  if (ttype->is_map()) {
    indent(out) << "_iprot.readMapEnd(_itrans);" << endl;
  } else if (ttype->is_set()) {
    indent(out) << "_iprot.readSetEnd(_itrans);" << endl;
  } else if (ttype->is_list()) {
    indent(out) << "_iprot.readListEnd(_itrans);" << endl;
  }

  scope_down(out);
}


/**
 * Generates code to deserialize a map
 */
void t_java_generator::generate_deserialize_map_element(ofstream& out,
                                                        t_map* tmap,
                                                        string prefix) {
  string key = tmp("_key");
  string val = tmp("_val");
  t_field fkey(tmap->get_key_type(), key);
  t_field fval(tmap->get_val_type(), val);

  indent(out) <<
    declare_field(&fkey, true) << endl;
  indent(out) <<
    declare_field(&fval, true) << endl;

  generate_deserialize_field(out, &fkey);
  generate_deserialize_field(out, &fval);

  indent(out) <<
    prefix << ".put(" << key << ", " << val << ");" << endl;
}

void t_java_generator::generate_deserialize_set_element(ofstream& out,
                                                        t_set* tset,
                                                        string prefix) {
  string elem = tmp("_elem");
  t_field felem(tset->get_elem_type(), elem);

  indent(out) <<
    declare_field(&felem, true) << endl;

  generate_deserialize_field(out, &felem);

  indent(out) <<
    prefix << ".add(" << elem << ");" << endl;
}

void t_java_generator::generate_deserialize_list_element(ofstream& out,
                                                         t_list* tlist,
                                                         string prefix) {
  string elem = tmp("_elem");
  t_field felem(tlist->get_elem_type(), elem);

  indent(out) <<
    declare_field(&felem, true) << endl;

  generate_deserialize_field(out, &felem);

  indent(out) <<
    prefix << ".add(" << elem << ");" << endl;
}


/**
 * Serializes a field of any type.
 *
 * @param tfield The field to serialize
 * @param prefix Name to prepend to field name
 */
void t_java_generator::generate_serialize_field(ofstream& out,
                                                t_field* tfield,
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
    generate_serialize_struct(out,
                              (t_struct*)(tfield->get_type()),
                              prefix + tfield->get_name());
  } else if (type->is_container()) {
    generate_serialize_container(out,
                                 tfield->get_type(),
                                 prefix + tfield->get_name());
  } else if (type->is_base_type() || type->is_enum()) {

    string name = prefix + tfield->get_name();
    indent(out) <<
      "_oprot.";
    
    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw
          "compiler error: cannot serialize void field in a struct: " + name;
        break;
      case t_base_type::TYPE_STRING:
        out << "writeString(_otrans, " << name << ");";
        break;
      case t_base_type::TYPE_BOOL:
        out << "writeBool(_otrans, " << name << ");";
        break;
      case t_base_type::TYPE_BYTE:
        out << "writeByte(_otrans, " << name << ");";
        break;
      case t_base_type::TYPE_I16:
        out << "writeI16(_otrans, " << name << ");";
        break;
      case t_base_type::TYPE_I32:
        out << "writeI32(_otrans, " << name << ");";
        break;
      case t_base_type::TYPE_I64:
        out << "writeI64(_otrans, " << name << ");";
        break;
      case t_base_type::TYPE_DOUBLE:
        out << "writeDouble(_otrans, " << name << ");";
        break;
      default:
        throw "compiler error: no Java name for base type " + tbase;
      }
    } else if (type->is_enum()) {
      out << "writeI32(_otrans, " << name << ");";
    }
    out << endl;
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
void t_java_generator::generate_serialize_struct(ofstream& out,
                                                 t_struct* tstruct,
                                                 string prefix) {
  out <<
    indent() << prefix << ".write(_oprot, _otrans);" << endl;
}

void t_java_generator::generate_serialize_container(ofstream& out,
                                                    t_type* ttype,
                                                    string prefix) {
  scope_up(out);
  
  if (ttype->is_map()) {
    indent(out) <<
      "_oprot.writeMapBegin(_otrans, new TMap(" <<
      type_to_enum(((t_map*)ttype)->get_key_type()) << ", " <<
      type_to_enum(((t_map*)ttype)->get_val_type()) << ", " <<
      prefix << ".size()));" << endl;
  } else if (ttype->is_set()) {
    indent(out) <<
      "_oprot.writeSetBegin(_otrans, new TSet(" <<
      type_to_enum(((t_set*)ttype)->get_elem_type()) << ", " <<
      prefix << ".size()));" << endl;
  } else if (ttype->is_list()) {
    indent(out) <<
      "_oprot.writeListBegin(_otrans, new TList(" <<
      type_to_enum(((t_list*)ttype)->get_elem_type()) << ", " <<
      prefix << ".size()));" << endl;
  }

  string iter = tmp("_iter");
  if (ttype->is_map()) {
    indent(out) <<
      "for (" <<
      type_name(((t_map*)ttype)->get_key_type()) << " " << iter <<
      " : " <<
      prefix << ".keySet())";
  } else if (ttype->is_set()) {
    indent(out) <<
      "for (" <<
      type_name(((t_set*)ttype)->get_elem_type()) << " " << iter <<
      " : " <<
      prefix << ")";
  } else if (ttype->is_list()) {
    indent(out) <<
      "for (" <<
      type_name(((t_list*)ttype)->get_elem_type()) << " " << iter <<
      " : " <<
      prefix << ")";
  }

    scope_up(out);

    if (ttype->is_map()) {
      generate_serialize_map_element(out, (t_map*)ttype, iter, prefix);
    } else if (ttype->is_set()) {
      generate_serialize_set_element(out, (t_set*)ttype, iter);
    } else if (ttype->is_list()) {
      generate_serialize_list_element(out, (t_list*)ttype, iter);
    }
    
    if (ttype->is_map()) {
      indent(out) <<
        "_oprot.writeMapEnd(_otrans);" << endl;
    } else if (ttype->is_set()) {
      indent(out) <<
        "_oprot.writeSetEnd(_otrans);" << endl;
    } else if (ttype->is_list()) {
      indent(out) <<
        "_oprot.writeListEnd(_otrans);" << endl;
    }
    
    scope_down(out);
 
  scope_down(out);  
}

/**
 * Serializes the members of a map.
 *
 */ 
void t_java_generator::generate_serialize_map_element(ofstream& out,
                                                      t_map* tmap,
                                                      string iter,
                                                      string map) {
  t_field kfield(tmap->get_key_type(), iter);
  generate_serialize_field(out, &kfield, "");

  t_field vfield(tmap->get_val_type(), map + ".get(" + iter + ")");
  generate_serialize_field(out, &vfield, "");
}

/**
 * Serializes the members of a set.
 */
void t_java_generator::generate_serialize_set_element(ofstream& out,
                                                      t_set* tset,
                                                      string iter) {
  t_field efield(tset->get_elem_type(), iter);
  generate_serialize_field(out, &efield, "");
}

/**
 * Serializes the members of a list.
 */
void t_java_generator::generate_serialize_list_element(ofstream& out,
                                                       t_list* tlist,
                                                       string iter) {
  t_field efield(tlist->get_elem_type(), iter);
  generate_serialize_field(out, &efield, "");
}

/**
 * Returns a Java type name
 *
 * @param ttype The type
 * @param container Is the type going inside a container?
 */
string t_java_generator::type_name(t_type* ttype, bool in_container) {
  // In Java typedefs are just resolved to their real type
  while (ttype->is_typedef()) {
    ttype = ((t_typedef*)ttype)->get_type();
  }

  if (ttype->is_base_type()) {
    return base_type_name(((t_base_type*)ttype)->get_base(), in_container);
  } else if (ttype->is_enum()) {
    return (in_container ? "Integer" : "int");
  } else if (ttype->is_map()) {
    t_map* tmap = (t_map*) ttype;
    return "HashMap<" +
      type_name(tmap->get_key_type(), true) + "," +
      type_name(tmap->get_val_type(), true) + ">";
  } else if (ttype->is_set()) {
    t_set* tset = (t_set*) ttype;
    return "HashSet<" + type_name(tset->get_elem_type(), true) + ">";
  } else if (ttype->is_list()) {
    t_list* tlist = (t_list*) ttype;
    return "ArrayList<" + type_name(tlist->get_elem_type(), true) + ">";
  } else {
    return ttype->get_name();
  }
}

/**
 * Returns the C++ type that corresponds to the thrift type.
 *
 * @param tbase The base type
 * @param container Is it going in a Java container?
 */
string t_java_generator::base_type_name(t_base_type::t_base tbase,
                                        bool in_container) {
  switch (tbase) {
  case t_base_type::TYPE_VOID:
    return "void";
  case t_base_type::TYPE_STRING:
    return "String";
  case t_base_type::TYPE_BOOL:
    return "boolean";
  case t_base_type::TYPE_BYTE:
    return "byte";
  case t_base_type::TYPE_I16:
    return (in_container ? "Short" : "short");
  case t_base_type::TYPE_I32:
    return (in_container ? "Integer" : "int");
  case t_base_type::TYPE_I64:
    return (in_container ? "Long" : "long");
  case t_base_type::TYPE_DOUBLE:
    return (in_container ? "Double" : "double");
  default:
    throw "compiler error: no C++ name for base type " + tbase;
  }
}

/**
 * Declares a field, which may include initialization as necessary.
 *
 * @param ttype The type
 */
string t_java_generator::declare_field(t_field* tfield, bool init) {
  // TODO(mcslee): do we ever need to initialize the field?
  string result = type_name(tfield->get_type()) + " " + tfield->get_name();
  if (init) {
    t_type* ttype = tfield->get_type();
    while (ttype->is_typedef()) {
      ttype = ((t_typedef*)ttype)->get_type();
    }
    if (ttype->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)ttype)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "NO T_VOID CONSTRUCT";
      case t_base_type::TYPE_STRING:
        // result += " = \"\"";
        result += " = null";
        break;
      case t_base_type::TYPE_BOOL:
        result += " = false";
        break;
      case t_base_type::TYPE_BYTE:
      case t_base_type::TYPE_I16:
      case t_base_type::TYPE_I32:
      case t_base_type::TYPE_I64:
        result += " = 0";
        break;
      case t_base_type::TYPE_DOUBLE:
        result += " = (double)0";
        break;
    }

    } else if (ttype->is_enum()) {
      result += " = 0";
    } else if (ttype->is_container()) {
      result += " = new " + type_name(ttype) + "()";
    } else {
      result += " = null";
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
string t_java_generator::function_signature(t_function* tfunction,
                                            string prefix) {
  t_type* ttype = tfunction->get_returntype();
  std::string result =
    type_name(ttype) + " " + prefix + tfunction->get_name() +
    "(" + argument_list(tfunction->get_arglist()) + ") throws ";
  t_struct* xs = tfunction->get_xceptions();
  const std::vector<t_field*>& xceptions = xs->get_members();
  vector<t_field*>::const_iterator x_iter;
  for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
    result += (*x_iter)->get_type()->get_name() + ", ";
  }
  result += "TException";
  return result;
}

/**
 * Renders a field list
 */
string t_java_generator::argument_list(t_struct* tstruct) {
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
string t_java_generator::type_to_enum(t_type* type) {
  while (type->is_typedef()) {
    type = ((t_typedef*)type)->get_type();
  }
  
  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "NO T_VOID CONSTRUCT";
    case t_base_type::TYPE_STRING:
      return "TType.STRING";
    case t_base_type::TYPE_BOOL:
      return "TType.BOOL";
    case t_base_type::TYPE_BYTE:
      return "TType.BYTE";
    case t_base_type::TYPE_I16:
      return "TType.I16";
    case t_base_type::TYPE_I32:
      return "TType.I32";
    case t_base_type::TYPE_I64:
      return "TType.I64";
    case t_base_type::TYPE_DOUBLE:
      return "TType.DOUBLE";
    }
  } else if (type->is_enum()) {
    return "TType.I32";
  } else if (type->is_struct() || type->is_xception()) {
    return "TType.STRUCT";
  } else if (type->is_map()) {
    return "TType.MAP";
  } else if (type->is_set()) {
    return "TType.SET";
  } else if (type->is_list()) {
    return "TType.LIST";
  }

  throw "INVALID TYPE IN type_to_enum: " + type->get_name();
}
