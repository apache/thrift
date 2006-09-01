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
}

/**
 * Packages the generated file
 */
string t_java_generator::java_package() {
  // TODO(mcslee): Allow destination package to be specified in .thrift file
  return string("package ") + program_name_ + ";\n\n";
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
    "import com.facebook.thrift.*;\n" +
    "import com.facebook.thrift.protocol.TString;\n\n";
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
  // Make output file
  string f_struct_name = string(T_JAVA_DIR)+"/"+(tstruct->get_name())+".java";
  ofstream f_struct;
  f_struct.open(f_struct_name.c_str());

  f_struct <<
    autogen_comment() <<
    java_package() <<
    java_type_imports();

  f_struct <<
    "public class " << tstruct->get_name() << " ";
  
  scope_up(f_struct);

  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter; 
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    indent(f_struct) <<
      "public " << declare_field(*m_iter, true) << endl;
  }
  
  scope_down(f_struct);
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
  // Generate the three main parts of the service
  generate_service_interface(tservice);
  generate_service_server(tservice);
  generate_service_client(tservice);
}

/**
 * Generates a service interface definition.
 *
 * @param tservice The service to generate a header definition for
 */
void t_java_generator::generate_service_interface(t_service* tservice) {
  // Make output file
  string f_interface_name = string(T_JAVA_DIR)+"/"+service_name_+"If.java";
  ofstream f_interface;
  f_interface.open(f_interface_name.c_str());

  f_interface <<
    autogen_comment() <<
    java_package() <<
    java_type_imports();

  f_interface <<
    "public abstract class " << service_name_ << "If {" << endl;
  indent_up();
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter; 
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    indent(f_interface) <<
      "public abstract " << function_signature(*f_iter) <<
      " throws TException;" << endl;
  }
  indent_down();
  f_interface <<
    "}" << endl;
  f_interface.close();
}

/**
 * Generates a service client definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_java_generator::generate_service_client(t_service* tservice) {
  string f_client_name = string(T_JAVA_DIR)+"/"+service_name_+"Client.java";
  f_service_.open(f_client_name.c_str());

  f_service_ <<
    autogen_comment() <<
    java_package() <<
    java_type_imports() <<
    java_thrift_imports();

  f_service_ <<
    "public class " << service_name_ << "Client " <<
    "extends " << service_name_ << "If {" << endl;
  indent_up();

  indent(f_service_) <<
    "public " << service_name_ << "Client" <<
    "(TTransport trans, TProtocol prot)" << endl;
  scope_up(f_service_);
  indent(f_service_) << 
    "this(trans, trans, prot, prot);" << endl;
  scope_down(f_service_);
  f_service_ << endl;

  indent(f_service_) <<
    "public " << service_name_ << "Client" <<
    "(TTransport itrans, TTransport otrans," <<
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
    indent() << "private TProtocol  _oprot;"  << endl << endl;

  // Generate client method implementations
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::const_iterator f_iter; 
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    string funname = (*f_iter)->get_name();

    // Open function
    indent(f_service_) <<
      "public " << function_signature(*f_iter) << " throws TException" << endl;
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
      "public " << function_signature(&send_function) << " throws TException" << endl;
    scope_up(f_service_);

    // Serialize the request
    f_service_ <<
      indent() <<
      "_oprot.writeStructBegin(_otrans, " <<
      "new TStruct(\"function\"));" << endl <<
      indent() <<
      "_oprot.writeFieldBegin(_otrans, " <<
      "new TField(\"name\", TType.STRING, 0));" << endl <<
      indent() <<
      "_oprot.writeString(_otrans, " <<
      "\"" << funname << "\");" << endl <<
      indent() <<
      "_oprot.writeFieldEnd(_otrans);" << endl <<
      indent() <<
      "_oprot.writeFieldBegin(_otrans, " <<
      "new TField(\"args\", TType.STRUCT, 1));" << endl;
    generate_serialize_struct((*f_iter)->get_arglist());
    f_service_ <<
      indent() <<
      "_oprot.writeFieldEnd(_otrans);" << endl <<
      indent() <<
      "_oprot.writeFieldStop(_otrans);" << endl <<
      indent() <<
      "_oprot.writeStructEnd(_otrans);" << endl;
    
    // Flush the request
    indent(f_service_) <<
      "_otrans.flush();" << endl;

    scope_down(f_service_);
    f_service_ << endl;

    if (!(*f_iter)->is_async()) {
      t_struct noargs;
      t_function recv_function((*f_iter)->get_returntype(),
                               string("recv_") + (*f_iter)->get_name(),
                               &noargs);
      // Open function
      indent(f_service_) <<
        "public " << function_signature(&recv_function) << " throws TException" << endl;
      scope_up(f_service_);
      
      // Read the response
      t_struct result_struct((*f_iter)->get_name() + "_result");
      t_field result_field((*f_iter)->get_returntype(), "_result");
      
      // Add a field to the return struct if non void
      if (!(*f_iter)->get_returntype()->is_void()) {
        indent(f_service_) <<
          declare_field(&result_field, true) << endl;
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

  indent_down();
  f_service_ <<
    "}" << endl;
  f_service_.close();
}

/**
 * Generates a service server definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_java_generator::generate_service_server(t_service* tservice) {
  string f_server_name = string(T_JAVA_DIR)+"/"+service_name_+"ServerIf.java";
  f_service_.open(f_server_name.c_str());

  f_service_ <<
    autogen_comment() <<
    java_package() <<
    java_type_imports() <<
    java_thrift_imports();

  // Generate the dispatch methods
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter; 

  // Generate the header portion
  f_service_ <<
    "public abstract class " << service_name_ << "ServerIf " <<
    "extends " << service_name_ << "If implements TProcessor {" << endl;
  indent_up();

  indent(f_service_) <<
    "public " << service_name_ << "ServerIf" <<
    "(TProtocol prot)" << endl;
  scope_up(f_service_);
  indent(f_service_) << 
    "this(prot, prot);" << endl;
  scope_down(f_service_);
  f_service_ << endl;

  indent(f_service_) <<
    "public " << service_name_ << "ServerIf" <<
    "(TProtocol iprot, TProtocol oprot)" << endl;
  scope_up(f_service_);
  f_service_ <<
    indent() << "_iprot = iprot;" << endl <<
    indent() << "_oprot = oprot;" << endl;
  scope_down(f_service_);
  f_service_ << endl;
 
  f_service_ <<
    indent() << "private TProtocol _iprot;" << endl <<
    indent() << "private TProtocol _oprot;" << endl << endl;
  
  // Generate the server implementation
  indent(f_service_) <<
    "public boolean process(TTransport _itrans, TTransport _otrans) " <<
    "throws TException" << endl;
  scope_up(f_service_);

  f_service_ <<
    indent() <<
    "String _fname;" << endl <<
    indent() <<
    "TStruct _struct;" << endl <<
    indent() <<
    "TField _field;" << endl <<
    indent() <<
    "_struct = _iprot.readStructBegin(_itrans);" << endl <<
    indent() <<
    "_field = _iprot.readFieldBegin(_itrans);" << endl <<
    indent() <<
    "_fname = _iprot.readString(_itrans);" << endl <<
    indent() <<
    "_iprot.readFieldEnd(_itrans);" << endl <<
    indent() <<
    "_field = _iprot.readFieldBegin(_itrans);" << endl;

  bool first = true;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    if (!first) {
      f_service_ << " else ";
    } else {
      f_service_ << indent();
      first = false;
    }
    f_service_ <<
      "if (_fname.equals(\"" << (*f_iter)->get_name() <<"\")) {" << endl;
    indent_up();
    indent(f_service_) <<
      "process_" << (*f_iter)->get_name() <<
      "(_itrans, _otrans);" << endl;
    indent_down();
    indent(f_service_) << "}";
  }
  f_service_ <<
    " else {" << endl;
  indent_up();
  indent(f_service_) <<
    "System.err.println" <<
    "(\"Unknown function: '\" + _field.name + \"'\");" << endl;
  indent_down();
  indent(f_service_) <<
    "}" << endl;

  // Read end of args field, the T_STOP, and the struct close
  f_service_ <<
    indent() <<
    "_iprot.readFieldEnd(_itrans);" << endl <<
    indent() <<
    "_field = _iprot.readFieldBegin(_itrans);" << endl <<   
    indent() <<
    "_iprot.readStructEnd(_itrans);" << endl <<
    indent() <<
    "return true;" << endl;

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

  f_service_.close();
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
    "(TTransport _itrans, TTransport _otrans) throws TException" << endl;
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
    f_service_ <<
      type_name(tfunction->get_returntype()) << " _result = ";
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
    "_otrans.flush();" << endl;

  // Close function
  scope_down(f_service_);
  f_service_ << endl;
}

/**
 * Deserializes a field of any type.
 */
void t_java_generator::generate_deserialize_field(t_field* tfield,
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
      name << " = _iprot.";
    
    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "compiler error: cannot serialize void field in a struct: " +
          name;
        break;
      case t_base_type::TYPE_STRING:        
        f_service_ << "readString(_itrans);";
        break;
      case t_base_type::TYPE_BYTE:
        f_service_ << "readByte(_itrans);";
        break;
      case t_base_type::TYPE_I32:
        f_service_ << "readI32(_itrans);";
        break;
      case t_base_type::TYPE_I64:
        f_service_ << "readI64(_itrans);";
        break;
      default:
        throw "compiler error: no C++ name for base type " + tbase;
      }
    } else if (type->is_enum()) {
      f_service_ << "readI32(_itrans);";
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
void t_java_generator::generate_deserialize_struct(t_struct* tstruct,
                                                  string prefix) {
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  scope_up(f_service_);

  // Read the struct fields from the protocol
  string _struct = tmp("_struct");
  string _field = tmp("_field");
  
  // Declare stack tmp variables
  f_service_ <<
    indent() << "TField " << _field << ";" << endl <<
    indent() << "TStruct " << _struct << " = _iprot.readStructBegin(_itrans);" << endl;
  
  // Loop over reading in fields
  indent(f_service_) <<
    "while (true)" << endl;

    scope_up(f_service_);
    
    // Read beginning field marker
    indent(f_service_) <<
      _field << " = _iprot.readFieldBegin(_itrans);" << endl;
    
    // Check for field STOP marker and break
    indent(f_service_) <<
      "if (" << _field << ".type == TType.STOP) { " << endl;
    indent_up();
    indent(f_service_) <<
      "break;" << endl;
    indent_down();
    indent(f_service_) <<
      "}" << endl;
    
    // Switch statement on the field we are reading
    indent(f_service_) <<
      "switch ((int)" << _field << ".id)" << endl;

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
        indent() <<
        "default:" << endl <<
        indent() <<
        "  TProtocolUtil.skip(_iprot, _itrans, " << 
        _field << ".type);" << endl <<
        indent() <<
        "  break;" << endl;
      
      scope_down(f_service_);

    // Read field end marker
    indent(f_service_) <<
      "_iprot.readFieldEnd(_itrans);" << endl;
    
    scope_down(f_service_);
      
  indent(f_service_) <<
    "_iprot.readStructEnd(_itrans);" << endl;

  scope_down(f_service_);
}

void t_java_generator::generate_deserialize_container(t_type* ttype,
                                                     string prefix) {
  scope_up(f_service_);
  
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
    f_service_ <<
      indent() << "TMap " << obj << " = _iprot.readMapBegin(_itrans);" << endl;
  } else if (ttype->is_set()) {
    f_service_ <<
      indent() << "TSet " << obj << " = _iprot.readSetBegin(_itrans);" << endl;
  } else if (ttype->is_list()) {
    f_service_ <<
      indent() << "TList " << obj << " = _iprot.readListBegin(_itrans);" << endl;
  }


  // For loop iterates over elements
  string i = tmp("_i");
  indent(f_service_) <<
    "for (int " << i << " = 0; " <<
    i << " < " << obj << ".size" << "; " <<
    "++" << i << ")" << endl;
  
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
    indent(f_service_) << "_iprot.readMapEnd(_itrans);" << endl;
  } else if (ttype->is_set()) {
    indent(f_service_) << "_iprot.readSetEnd(_itrans);" << endl;
  } else if (ttype->is_list()) {
    indent(f_service_) << "_iprot.readListEnd(_itrans);" << endl;
  }

  scope_down(f_service_);
}


/**
 * Generates code to deserialize a map
 */
void t_java_generator::generate_deserialize_map_element(t_map* tmap,
                                                        string prefix) {
  string key = tmp("_key");
  string val = tmp("_val");
  t_field fkey(tmap->get_key_type(), key);
  t_field fval(tmap->get_val_type(), val);

  indent(f_service_) <<
    declare_field(&fkey, true) << endl;
  indent(f_service_) <<
    declare_field(&fval, true) << endl;

  generate_deserialize_field(&fkey);
  generate_deserialize_field(&fval);

  indent(f_service_) <<
    prefix << ".put(" << key << ", " << val << ");" << endl;
}

void t_java_generator::generate_deserialize_set_element(t_set* tset,
                                                        string prefix) {
  string elem = tmp("_elem");
  t_field felem(tset->get_elem_type(), elem);

  indent(f_service_) <<
    declare_field(&felem, true) << endl;

  generate_deserialize_field(&felem);

  indent(f_service_) <<
    prefix << ".add(" << elem << ");" << endl;
}

void t_java_generator::generate_deserialize_list_element(t_list* tlist,
                                                         string prefix) {
  string elem = tmp("_elem");
  t_field felem(tlist->get_elem_type(), elem);

  indent(f_service_) <<
    declare_field(&felem, true) << endl;

  generate_deserialize_field(&felem);

  indent(f_service_) <<
    prefix << ".add(" << elem << ");" << endl;
}


/**
 * Serializes a field of any type.
 *
 * @param tfield The field to serialize
 * @param prefix Name to prepend to field name
 */
void t_java_generator::generate_serialize_field(t_field* tfield,
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
      "_oprot.";
    
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
      case t_base_type::TYPE_I64:
        f_service_ << "writeI64(_otrans, " << name << ");";
        break;
      default:
        throw "compiler error: no C++ name for base type " + tbase;
      }
    } else if (type->is_enum()) {
      f_service_ << "writeI32(_otrans, " << name << ");";
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
void t_java_generator::generate_serialize_struct(t_struct* tstruct,
                                                 string prefix) {
  string name = tstruct->get_name();
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  scope_up(f_service_);
  string _struct = tmp("_struct");
  string _field = tmp("_field");

  f_service_ <<
    indent() <<
    "TStruct " << _struct << " = new TStruct(\"" << name << "\");" << endl <<
    indent() <<
    "TField " << _field << " = new TField();" << endl <<
    indent() <<
    "_oprot.writeStructBegin(_otrans, " << _struct << ");" << endl;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    f_service_ <<
      indent() <<
      _field << ".name = \"" << (*f_iter)->get_name() << "\";" << endl <<
      indent() <<
      _field << ".type = " << type_to_enum((*f_iter)->get_type()) << ";" << endl <<
      indent() <<
      _field << ".id = " << (*f_iter)->get_key() << ";" << endl <<
      indent() <<
      "_oprot.writeFieldBegin(_otrans, " << _field << ");" << endl;
    // Write field contents
    generate_serialize_field(*f_iter, prefix);
    // Write field closer
    indent(f_service_) <<
      "_oprot.writeFieldEnd(_otrans);" << endl;
  }
  // Write the struct map
  f_service_ <<
    indent() << "_oprot.writeFieldStop(_otrans);" << endl <<
    indent() << "_oprot.writeStructEnd(_otrans);" << endl;

  scope_down(f_service_);
}

void t_java_generator::generate_serialize_container(t_type* ttype,
                                                    string prefix) {
  scope_up(f_service_);
  
  if (ttype->is_map()) {
    indent(f_service_) <<
      "_oprot.writeMapBegin(_otrans, new TMap(" <<
      type_to_enum(((t_map*)ttype)->get_key_type()) << ", " <<
      type_to_enum(((t_map*)ttype)->get_val_type()) << ", " <<
      prefix << ".size()));" << endl;
  } else if (ttype->is_set()) {
    indent(f_service_) <<
      "_oprot.writeSetBegin(_otrans, new TSet(" <<
      type_to_enum(((t_set*)ttype)->get_elem_type()) << ", " <<
      prefix << ".size()));" << endl;
  } else if (ttype->is_list()) {
    indent(f_service_) <<
      "_oprot.writeListBegin(_otrans, new TList(" <<
      type_to_enum(((t_list*)ttype)->get_elem_type()) << ", " <<
      prefix << ".size()));" << endl;
  }

  string iter = tmp("_iter");
  if (ttype->is_map()) {
    indent(f_service_) <<
      "for (" <<
      type_name(((t_map*)ttype)->get_key_type()) << " " << iter <<
      " : " <<
      prefix << ".keySet())";
  } else if (ttype->is_set()) {
    indent(f_service_) <<
      "for (" <<
      type_name(((t_set*)ttype)->get_elem_type()) << " " << iter <<
      " : " <<
      prefix << ")";
  } else if (ttype->is_list()) {
    indent(f_service_) <<
      "for (" <<
      type_name(((t_list*)ttype)->get_elem_type()) << " " << iter <<
      " : " <<
      prefix << ")";
  }

    scope_up(f_service_);

    if (ttype->is_map()) {
      generate_serialize_map_element((t_map*)ttype, iter, prefix);
    } else if (ttype->is_set()) {
      generate_serialize_set_element((t_set*)ttype, iter);
    } else if (ttype->is_list()) {
      generate_serialize_list_element((t_list*)ttype, iter);
    }
    
    if (ttype->is_map()) {
      indent(f_service_) <<
        "_oprot.writeMapEnd(_otrans);" << endl;
    } else if (ttype->is_set()) {
      indent(f_service_) <<
        "_oprot.writeSetEnd(_otrans);" << endl;
    } else if (ttype->is_list()) {
      indent(f_service_) <<
        "_oprot.writeListEnd(_otrans);" << endl;
    }
    
    scope_down(f_service_);
 
  scope_down(f_service_);  
}

/**
 * Serializes the members of a map.
 *
 */
void t_java_generator::generate_serialize_map_element(t_map* tmap,
                                                      string iter,
                                                      string map) {
  t_field kfield(tmap->get_key_type(), iter);
  generate_serialize_field(&kfield, "");

  t_field vfield(tmap->get_val_type(), map + ".get(" + iter + ")");
  generate_serialize_field(&vfield, "");
}

/**
 * Serializes the members of a set.
 */
void t_java_generator::generate_serialize_set_element(t_set* tset,
                                                      string iter) {
  t_field efield(tset->get_elem_type(), iter);
  generate_serialize_field(&efield, "");
}

/**
 * Serializes the members of a list.
 */
void t_java_generator::generate_serialize_list_element(t_list* tlist,
                                                      string iter) {
  t_field efield(tlist->get_elem_type(), iter);
  generate_serialize_field(&efield, "");
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
  case t_base_type::TYPE_BYTE:
    return "byte";
  case t_base_type::TYPE_I32:
    return (in_container ? "Integer" : "int");
  case t_base_type::TYPE_I64:
    return (in_container ? "Long" : "long");
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
        result += " = \"\"";
        break;
      case t_base_type::TYPE_BYTE:
      case t_base_type::TYPE_I32:
      case t_base_type::TYPE_I64:
        result += " = 0";
        break;
    }

    } else  if (ttype->is_enum()) {
      result += " = 0";
    } else {
      result += " = new " + type_name(tfield->get_type()) + "()";
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
  return
    type_name(ttype) + " " + prefix + tfunction->get_name() +
    "(" + argument_list(tfunction->get_arglist()) + ")";
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
    case t_base_type::TYPE_BYTE:
      return "TType.BYTE";
    case t_base_type::TYPE_I32:
      return "TType.I32";
    case t_base_type::TYPE_I64:
      return "TType.I64";
    }
  } else if (type->is_enum()) {
    return "TType.I32";
  } else if (type->is_struct()) {
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
