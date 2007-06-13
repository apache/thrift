// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

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
void t_java_generator::init_generator() {
  // Make output directory
  mkdir(T_JAVA_DIR, S_IREAD | S_IWRITE | S_IEXEC);
  package_name_ = program_->get_java_package();

  string dir = package_name_;
  string subdir = T_JAVA_DIR;
  string::size_type loc;
  while ((loc = dir.find(".")) != string::npos) {
    subdir = subdir + "/" + dir.substr(0, loc);
    mkdir(subdir.c_str(), S_IREAD | S_IWRITE | S_IEXEC);
    dir = dir.substr(loc+1);
  }
  if (dir.size() > 0) {
    subdir = subdir + "/" + dir;
    mkdir(subdir.c_str(), S_IREAD | S_IWRITE | S_IEXEC);
  }

  package_dir_ = subdir;
}

/**
 * Packages the generated file
 *
 * @return String of the package, i.e. "package com.facebook.thriftdemo;"
 */
string t_java_generator::java_package() {
  if (!package_name_.empty()) {
    return string("package ") + package_name_ + ";\n\n";
  }
  return "";
}

/**
 * Prints standard java imports
 *
 * @return List of imports for Java types that are used in here
 */
string t_java_generator::java_type_imports() {
  return
    string() +
    "import java.util.ArrayList;\n" +
    "import java.util.AbstractMap;\n" +
    "import java.util.HashMap;\n" +
    "import java.util.HashSet;\n" +
    "import com.facebook.thrift.*;\n\n";
}

/**
 * Prints standard java imports
 *
 * @return List of imports necessary for thrift
 */
string t_java_generator::java_thrift_imports() {
  return
    string() +
    "import com.facebook.thrift.protocol.*;\n" +
    "import com.facebook.thrift.transport.*;\n\n";
}

/**
 * Nothing in Java
 */
void t_java_generator::close_generator() {}

/**
 * Generates a typedef. This is not done in Java, since it does
 * not support arbitrary name replacements, and it'd be a wacky waste
 * of overhead to make wrapper classes.
 *
 * @param ttypedef The type definition
 */
void t_java_generator::generate_typedef(t_typedef* ttypedef) {}

/**
 * Enums are a class with a set of static constants. 
 *
 * @param tenum The enumeration
 */
void t_java_generator::generate_enum(t_enum* tenum) {
  // Make output file
  string f_enum_name = package_dir_+"/"+(tenum->get_name())+".java";
  ofstream f_enum;
  f_enum.open(f_enum_name.c_str());

  // Comment and package it
  f_enum <<
    autogen_comment() <<
    java_package() << endl;

  f_enum <<
    "public class " << tenum->get_name() << " ";
  scope_up(f_enum);

  vector<t_enum_value*> constants = tenum->get_constants();
  vector<t_enum_value*>::iterator c_iter;
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
  f_enum.close();
}

/**
 * Generates a class that holds all the constants.
 */
void t_java_generator::generate_consts(std::vector<t_const*> consts) {
  string f_consts_name = package_dir_+"/Constants.java";
  ofstream f_consts;
  f_consts.open(f_consts_name.c_str());

  // Print header
  f_consts <<
    autogen_comment() <<
    java_package() << 
    java_type_imports();

  f_consts <<
    "public class Constants {" << endl <<
    endl;
  indent_up();
  vector<t_const*>::iterator c_iter;
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    print_const_value(f_consts,
                      (*c_iter)->get_name(),
                      (*c_iter)->get_type(),
                      (*c_iter)->get_value(),
                      false);
  }
  indent_down();
  indent(f_consts) <<
    "}" << endl;
  f_consts.close();
}


/**
 * Prints the value of a constant with the given type. Note that type checking
 * is NOT performed in this function as it is always run beforehand using the
 * validate_types method in main.cc
 */
void t_java_generator::print_const_value(std::ofstream& out, string name, t_type* type, t_const_value* value, bool in_static, bool defval) {

  indent(out);
  if (!defval) {
    out <<
      (in_static ? "" : "public static final ") <<
      type_name(type) << " ";
  }
  if (type->is_base_type()) {
    string v2 = render_const_value(out, name, type, value);
    out << name << " = " << v2 << ";" << endl << endl;
  } else if (type->is_enum()) {
    out << name << " = " << value->get_integer() << ";" << endl << endl;
  } else if (type->is_struct() || type->is_xception()) {
    const vector<t_field*>& fields = ((t_struct*)type)->get_members();
    vector<t_field*>::const_iterator f_iter;
    const map<t_const_value*, t_const_value*>& val = value->get_map();
    map<t_const_value*, t_const_value*>::const_iterator v_iter;
    out << name << " = new " << type_name(type) << "();" << endl;
    if (!in_static) {
      indent(out) << "static {" << endl;
      indent_up();
    }
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      t_type* field_type = NULL;
      for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
        if ((*f_iter)->get_name() == v_iter->first->get_string()) {
          field_type = (*f_iter)->get_type();
        }
      }
      if (field_type == NULL) {
        throw "type error: " + type->get_name() + " has no field " + v_iter->first->get_string();
      }
      string val = render_const_value(out, name, field_type, v_iter->second);
      indent(out) << name << "." << v_iter->first->get_string() << " = " << val << ";" << endl;
      indent(out) << name << ".__isset." << v_iter->first->get_string() << " = true;" << endl;
    }
    if (!in_static) {
      indent_down();
      indent(out) << "}" << endl;
    }
    out << endl;
  } else if (type->is_map()) {
    out << name << " = new " << type_name(type, true, true) << "();" << endl;
    if (!in_static) {
      indent(out) << "static {" << endl;
      indent_up();
    }
    t_type* ktype = ((t_map*)type)->get_key_type();
    t_type* vtype = ((t_map*)type)->get_val_type();
    const map<t_const_value*, t_const_value*>& val = value->get_map();
    map<t_const_value*, t_const_value*>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      string key = render_const_value(out, name, ktype, v_iter->first);
      string val = render_const_value(out, name, vtype, v_iter->second);
      indent(out) << name << ".put(" << key << ", " << val << ");" << endl;
    }
    if (!in_static) {
      indent_down();
      indent(out) << "}" << endl;
    }
    out << endl;
  } else if (type->is_list() || type->is_set()) {
    out << name << " = new " << type_name(type) << "();" << endl;
    if (!in_static) {
      indent(out) << "static {" << endl;
      indent_up();
    }
    t_type* etype;
    if (type->is_list()) {
      etype = ((t_list*)type)->get_elem_type();
    } else {
      etype = ((t_set*)type)->get_elem_type();
    }
    const vector<t_const_value*>& val = value->get_list();
    vector<t_const_value*>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      string val = render_const_value(out, name, etype, *v_iter);
      indent(out) << name << ".add(" << val << ");" << endl;
    }
    if (!in_static) {
      indent_down();
      indent(out) << "}" << endl;
    }
    out << endl;
  }
}

string t_java_generator::render_const_value(ofstream& out, string name, t_type* type, t_const_value* value) {
  std::ostringstream render;

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_STRING:
      render << "\"" + value->get_string() + "\"";
      break;
    case t_base_type::TYPE_BOOL:
      render << ((value->get_integer() > 0) ? "true" : "false");
      break;
    case t_base_type::TYPE_BYTE:
    case t_base_type::TYPE_I16:
    case t_base_type::TYPE_I32:
    case t_base_type::TYPE_I64:
      render << value->get_integer();
      break;
    case t_base_type::TYPE_DOUBLE:
      if (value->get_type() == t_const_value::CV_INTEGER) {
        render << value->get_integer();
      } else {
        render << value->get_double();
      }
      break;
    default:
      throw "compiler error: no const of base type " + tbase;
    }
  } else if (type->is_enum()) {
    render << value->get_integer();
  } else {
    string t = tmp("tmp");
    print_const_value(out, t, type, value, true);
    render << t;
  }

  return render.str();
}

/**
 * Generates a struct definition for a thrift data type. This is a class
 * with data members, read(), write(), and an inner Isset class.
 *
 * @param tstruct The struct definition
 */
void t_java_generator::generate_struct(t_struct* tstruct) {
  generate_java_struct(tstruct, false);
}

/**
 * Exceptions are structs, but they inherit from Exception
 *
 * @param tstruct The struct definition
 */
void t_java_generator::generate_xception(t_struct* txception) {
  generate_java_struct(txception, true);
}


/**
 * Java struct definition.
 *
 * @param tstruct The struct definition
 */
void t_java_generator::generate_java_struct(t_struct* tstruct,
                                            bool is_exception) {
  // Make output file
  string f_struct_name = package_dir_+"/"+(tstruct->get_name())+".java";
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

/**
 * Java struct definition. This has various parameters, as it could be
 * generated standalone or inside another class as a helper. If it
 * is a helper than it is a static class.
 *
 * @param tstruct      The struct definition
 * @param is_exception Is this an exception?
 * @param in_class     If inside a class, needs to be static class
 * @param is_result    If this is a result it needs a different writer
 */
void t_java_generator::generate_java_struct_definition(ofstream &out,
                                                       t_struct* tstruct,
                                                       bool is_exception,
                                                       bool in_class,
                                                       bool is_result) {
  indent(out) <<
    "public " << (in_class ? "static " : "") << "class " << tstruct->get_name() << " ";
  
  if (is_exception) {
    out << "extends Exception ";
  }
  
  scope_up(out);

  // Members are public
  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter; 
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    indent(out) <<
      "public " << declare_field(*m_iter, false) << endl;
  }

  // Inner Isset class
  if (members.size() > 0) {
    out <<
      endl <<
      indent() << "public final Isset __isset = new Isset();" << endl <<
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
  
  // Default constructor
  indent(out) <<
    "public " << tstruct->get_name() << "() {" << endl;
  indent_up();
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_type* t = (*m_iter)->get_type();
    while (t->is_typedef()) {
      t = ((t_typedef*)t)->get_type();
    }
    if (!t->is_base_type() && (*m_iter)->get_value() != NULL) {
      print_const_value(out, "this." + (*m_iter)->get_name(), t, (*m_iter)->get_value(), true, true);
    }
  }
  indent_down();
  indent(out) << "}" << endl << endl;

  generate_java_struct_reader(out, tstruct);
  if (is_result) {
    generate_java_struct_result_writer(out, tstruct);
  } else {
    generate_java_struct_writer(out, tstruct);
  }
  generate_java_struct_tostring(out, tstruct);
  scope_down(out);
  out << endl;
}

/**
 * Generates a function to read all the fields of the struct.
 *
 * @param tstruct The struct definition
 */
void t_java_generator::generate_java_struct_reader(ofstream& out,
                                                   t_struct* tstruct) {
  out <<
    indent() << "public void read(TProtocol iprot) throws TException {" << endl;
  indent_up();

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  // Declare stack tmp variables
  out <<
    indent() << "TField field;" << endl <<
    indent() << "TStruct struct = iprot.readStructBegin();" << endl;
  
  // Loop over reading in fields
  indent(out) <<
    "while (true)" << endl;
    scope_up(out);
    
    // Read beginning field marker
    indent(out) <<
      "field = iprot.readFieldBegin();" << endl;
    
    // Check for field STOP marker and break
    indent(out) <<
      "if (field.type == TType.STOP) { " << endl;
    indent_up();
    indent(out) <<
      "break;" << endl;
    indent_down();
    indent(out) <<
      "}" << endl;
    
    // Switch statement on the field we are reading
    indent(out) <<
      "switch (field.id)" << endl;

      scope_up(out);
    
      // Generate deserialization code for known cases
      for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
        indent(out) <<
          "case " << (*f_iter)->get_key() << ":" << endl;
        indent_up();
        indent(out) <<
          "if (field.type == " << type_to_enum((*f_iter)->get_type()) << ") {" << endl;
        indent_up();

        generate_deserialize_field(out, *f_iter, "this.");
        out <<
          indent() << "this.__isset." << (*f_iter)->get_name() << " = true;" << endl;
        indent_down();
        out <<
          indent() << "} else { " << endl <<
          indent() << "  TProtocolUtil.skip(iprot, field.type);" << endl <<
          indent() << "}" << endl <<
          indent() << "break;" << endl;
        indent_down();
      }
      
      // In the default case we skip the field
      out <<
        indent() << "default:" << endl <<
        indent() << "  TProtocolUtil.skip(iprot, field.type);" << endl <<
        indent() << "  break;" << endl;
      
      scope_down(out);

    // Read field end marker
    indent(out) <<
      "iprot.readFieldEnd();" << endl;
    
    scope_down(out);
      
    out <<
      indent() << "iprot.readStructEnd();" << endl;

  indent_down();
  out <<
    indent() << "}" << endl <<
    endl;
}

/**
 * Generates a function to write all the fields of the struct
 *
 * @param tstruct The struct definition
 */
void t_java_generator::generate_java_struct_writer(ofstream& out,
                                                   t_struct* tstruct) {
  out <<
    indent() << "public void write(TProtocol oprot) throws TException {" << endl;
  indent_up();

  string name = tstruct->get_name();
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  out <<
    indent() << "TStruct struct = new TStruct(\"" << name << "\");" << endl <<
    indent() << "TField field = new TField();" << endl <<
    indent() << "oprot.writeStructBegin(struct);" << endl;

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    bool null_allowed = type_can_be_null((*f_iter)->get_type());
    if (null_allowed) {
      out <<
        indent() << "if (this." << (*f_iter)->get_name() << " != null) {" << endl;
      indent_up();
    }

    out <<
      indent() << "field.name = \"" << (*f_iter)->get_name() << "\";" << endl <<
      indent() << "field.type = " << type_to_enum((*f_iter)->get_type()) << ";" << endl <<
      indent() << "field.id = " << (*f_iter)->get_key() << ";" << endl <<
      indent() << "oprot.writeFieldBegin(field);" << endl;

    // Write field contents
    generate_serialize_field(out, *f_iter, "this.");

    // Write field closer
    indent(out) <<
      "oprot.writeFieldEnd();" << endl;
    
    if (null_allowed) {
      indent_down();
      indent(out) << "}" << endl;
    }
  }
  // Write the struct map
  out <<
    indent() << "oprot.writeFieldStop();" << endl <<
    indent() << "oprot.writeStructEnd();" << endl;

  indent_down();
  out <<
    indent() << "}" << endl <<
    endl;
}

/**
 * Generates a function to write all the fields of the struct,
 * which is a function result. These fields are only written
 * if they are set in the Isset array, and only one of them
 * can be set at a time.
 *
 * @param tstruct The struct definition
 */
void t_java_generator::generate_java_struct_result_writer(ofstream& out,
                                                          t_struct* tstruct) {
  out <<
    indent() << "public void write(TProtocol oprot) throws TException {" << endl;
  indent_up();

  string name = tstruct->get_name();
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  out <<
    indent() << "TStruct struct = new TStruct(\"" << name << "\");" << endl <<
    indent() << "TField field = new TField();" << endl <<
    indent() << "oprot.writeStructBegin(struct);" << endl;

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

    bool null_allowed = type_can_be_null((*f_iter)->get_type());
    if (null_allowed) {
      out <<
        indent() << "if (this." << (*f_iter)->get_name() << " != null) {" << endl;
      indent_up();
    }

    out <<
      indent() << "field.name = \"" << (*f_iter)->get_name() << "\";" << endl <<
      indent() << "field.type = " << type_to_enum((*f_iter)->get_type()) << ";" << endl <<
      indent() << "field.id = " << (*f_iter)->get_key() << ";" << endl <<
      indent() << "oprot.writeFieldBegin(field);" << endl;

    // Write field contents
    generate_serialize_field(out, *f_iter, "this.");

    // Write field closer
    indent(out) <<
      "oprot.writeFieldEnd();" << endl;

    if (null_allowed) {
      indent_down();
      indent(out) << "}" << endl;
    }

    indent_down();
    indent(out) << "}";
  }
  // Write the struct map
  out <<
    endl <<
    indent() << "oprot.writeFieldStop();" << endl <<
    indent() << "oprot.writeStructEnd();" << endl;

  indent_down();
  out <<
    indent() << "}" << endl <<
    endl;
}

/**
 * Generates a toString() method for the given struct
 *
 * @param tstruct The struct definition
 */
void t_java_generator::generate_java_struct_tostring(ofstream& out,
                                                     t_struct* tstruct) {
  out <<
    indent() << "public String toString() {" << endl;
  indent_up();

  out <<
    indent() << "StringBuilder sb = new StringBuilder(\"" << tstruct->get_name() << "(\");" << endl;

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
      indent(out) << "sb.append(\"" << (*f_iter)->get_name() << ":\");" << endl;
    } else {
      indent(out) << "sb.append(\"," << (*f_iter)->get_name() << ":\");" << endl;
    }
    t_type* ttype = (*f_iter)->get_type();
    if (ttype->is_xception() || ttype->is_struct()) {
      indent(out) << "sb.append(this." << (*f_iter)->get_name() << ".toString());" << endl;
    } else {
      indent(out) << "sb.append(this." << (*f_iter)->get_name() << ");" << endl;
    }
  }
  out <<
    indent() << "sb.append(\")\");" << endl <<
    indent() << "return sb.toString();" << endl;

  indent_down();
  indent(out) << "}" << endl <<
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
  string f_service_name = package_dir_+"/"+service_name_+".java";
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
  string extends = "";
  string extends_iface = "";
  if (tservice->get_extends() != NULL) {
    extends = type_name(tservice->get_extends());
    extends_iface = " extends " + extends + ".Iface";
  }

  f_service_ <<
    indent() << "public interface Iface" << extends_iface << " {" << endl;
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

/**
 * Generates structs for all the service args and return types
 *
 * @param tservice The service
 */
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
  string extends = "";
  string extends_client = "";
  if (tservice->get_extends() != NULL) {
    extends = type_name(tservice->get_extends());
    extends_client = " extends " + extends + ".Client";
  }

  indent(f_service_) <<
    "public static class Client" << extends_client << " implements Iface {" << endl;
  indent_up();

  indent(f_service_) <<
    "public Client(TProtocol prot)" << endl;
  scope_up(f_service_);
  indent(f_service_) << 
    "this(prot, prot);" << endl;
  scope_down(f_service_);
  f_service_ << endl;

  indent(f_service_) <<
    "public Client(TProtocol iprot, TProtocol oprot)" << endl;
  scope_up(f_service_);
  if (extends.empty()) {
    f_service_ <<
      indent() << "iprot_ = iprot;" << endl <<
      indent() << "oprot_ = oprot;" << endl;
  } else {
    f_service_ <<
      indent() << "super(iprot, oprot);" << endl;
  }
  scope_down(f_service_);
  f_service_ << endl;
 
  if (extends.empty()) {
    f_service_ <<
      indent() << "protected TProtocol iprot_;"  << endl <<
      indent() << "protected TProtocol oprot_;"  << endl <<
      endl <<
      indent() << "protected int seqid_;" << endl <<
      endl;
  }

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
      
    t_function send_function(g_type_void,
                             string("send_") + (*f_iter)->get_name(),
                             (*f_iter)->get_arglist());

    string argsname = (*f_iter)->get_name() + "_args";

    // Open function
    indent(f_service_) <<
      "public " << function_signature(&send_function) << endl;
    scope_up(f_service_);

    // Serialize the request
    f_service_ <<
      indent() << "oprot_.writeMessageBegin(new TMessage(\"" << funname << "\", TMessageType.CALL, seqid_));" << endl <<
      indent() << argsname << " args = new " << argsname << "();" << endl;

    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      f_service_ <<
        indent() << "args." << (*fld_iter)->get_name() << " = " << (*fld_iter)->get_name() << ";" << endl;
    }

    f_service_ <<
      indent() << "args.write(oprot_);" << endl <<
      indent() << "oprot_.writeMessageEnd();" << endl <<
      indent() << "oprot_.getTransport().flush();" << endl;

    scope_down(f_service_);
    f_service_ << endl;

    if (!(*f_iter)->is_async()) {
      string resultname = (*f_iter)->get_name() + "_result";

      t_struct noargs(program_);
      t_function recv_function((*f_iter)->get_returntype(),
                               string("recv_") + (*f_iter)->get_name(),
                               &noargs,
                               (*f_iter)->get_xceptions());
      // Open function
      indent(f_service_) <<
        "public " << function_signature(&recv_function) << endl;
      scope_up(f_service_);
           
      // TODO(mcslee): Message validation here, was the seqid etc ok?

      f_service_ <<
        indent() << "TMessage msg = iprot_.readMessageBegin();" << endl <<
        indent() << "if (msg.type == TMessageType.EXCEPTION) {" << endl <<
        indent() << "  TApplicationException x = TApplicationException.read(iprot_);" << endl <<
        indent() << "  iprot_.readMessageEnd();" << endl <<
        indent() << "  throw x;" << endl <<
        indent() << "}" << endl <<
        indent() << resultname << " result = new " << resultname << "();" << endl <<
        indent() << "result.read(iprot_);" << endl <<
        indent() << "iprot_.readMessageEnd();" << endl;

      // Careful, only return _result if not a void function
      if (!(*f_iter)->get_returntype()->is_void()) {
        f_service_ <<
          indent() << "if (result.__isset.success) {" << endl <<
          indent() << "  return result.success;" << endl <<
          indent() << "}" << endl;
      }

      t_struct* xs = (*f_iter)->get_xceptions();
      const std::vector<t_field*>& xceptions = xs->get_members();
      vector<t_field*>::const_iterator x_iter;
      for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
        f_service_ <<
          indent() << "if (result.__isset." << (*x_iter)->get_name() << ") {" << endl <<
          indent() << "  throw result." << (*x_iter)->get_name() << ";" << endl <<
          indent() << "}" << endl;
      }

      // If you get here it's an exception, unless a void function
      if ((*f_iter)->get_returntype()->is_void()) {
        indent(f_service_) <<
          "return;" << endl;
      } else {
        f_service_ <<
          indent() << "throw new TApplicationException(TApplicationException.MISSING_RESULT, \"" << (*f_iter)->get_name() << " failed: unknown result\");" << endl;
      }
      
      // Close function
      scope_down(f_service_);
      f_service_ << endl;
    }
  }

  indent_down();
  indent(f_service_) <<
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

  // Extends stuff
  string extends = "";
  string extends_processor = "";
  if (tservice->get_extends() != NULL) {
    extends = type_name(tservice->get_extends());
    extends_processor = " extends " + extends + ".Processor";
  }

  // Generate the header portion
  indent(f_service_) <<
    "public static class Processor" << extends_processor << " implements TProcessor {" << endl;
  indent_up();

  indent(f_service_) <<
    "public Processor(Iface iface)" << endl;
  scope_up(f_service_);
  if (!extends.empty()) {
    f_service_ <<
      indent() << "super(iface);" << endl;
  }
  f_service_ <<
    indent() << "iface_ = iface;" << endl;

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    f_service_ <<
      indent() << "processMap_.put(\"" << (*f_iter)->get_name() << "\", new " << (*f_iter)->get_name() << "());" << endl;
  }
  
  scope_down(f_service_);
  f_service_ << endl;
  
  if (extends.empty()) {
    f_service_ <<
      indent() << "protected static interface ProcessFunction {" << endl <<
      indent() << "  public void process(int seqid, TProtocol iprot, TProtocol oprot) throws TException;" << endl <<
      indent() << "}" << endl <<
      endl;
  }

  f_service_ <<
    indent() << "private Iface iface_;" << endl;

  if (extends.empty()) {
    f_service_ <<
      indent() << "protected final HashMap<String,ProcessFunction> processMap_ = new HashMap<String,ProcessFunction>();" << endl;
  }
  
  f_service_ << endl;
  
  // Generate the server implementation
  indent(f_service_) <<
    "public boolean process(TProtocol iprot, TProtocol oprot) throws TException" << endl;
  scope_up(f_service_);

  f_service_ <<
    indent() << "TMessage msg = iprot.readMessageBegin();" << endl;

  // TODO(mcslee): validate message, was the seqid etc. legit?

  f_service_ <<
    indent() << "ProcessFunction fn = processMap_.get(msg.name);" << endl <<
    indent() << "if (fn == null) {" << endl <<
    indent() << "  TProtocolUtil.skip(iprot, TType.STRUCT);" << endl <<
    indent() << "  iprot.readMessageEnd();" << endl <<
    indent() << "  TApplicationException x = new TApplicationException(TApplicationException.UNKNOWN_METHOD, \"Invalid method name: '\"+msg.name+\"'\");" << endl <<
    indent() << "  oprot.writeMessageBegin(new TMessage(msg.name, TMessageType.EXCEPTION, msg.seqid));" << endl <<
    indent() << "  x.write(oprot);" << endl <<
    indent() << "  oprot.writeMessageEnd();" << endl <<
    indent() << "  oprot.getTransport().flush();" << endl <<
    indent() << "  return true;" << endl <<
    indent() << "}" << endl <<
    indent() << "fn.process(msg.seqid, iprot, oprot);" << endl;
  
  f_service_ <<
    indent() << "return true;" << endl;

  scope_down(f_service_);
  f_service_ << endl;

  // Generate the process subfunctions
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    generate_process_function(tservice, *f_iter);
  }

  indent_down();
  indent(f_service_) <<
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

  t_struct result(program_, tfunction->get_name() + "_result");
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
  // Open class
  indent(f_service_) <<
    "private class " << tfunction->get_name() << " implements ProcessFunction {" << endl;
  indent_up();

  // Open function
  indent(f_service_) <<
    "public void process(int seqid, TProtocol iprot, TProtocol oprot) throws TException" << endl;
  scope_up(f_service_);

  string argsname = tfunction->get_name() + "_args";
  string resultname = tfunction->get_name() + "_result";

  f_service_ <<
    indent() << argsname << " args = new " << argsname << "();" << endl <<
    indent() << "args.read(iprot);" << endl <<
    indent() << "iprot.readMessageEnd();" << endl;

  t_struct* xs = tfunction->get_xceptions();
  const std::vector<t_field*>& xceptions = xs->get_members();
  vector<t_field*>::const_iterator x_iter;

  // Declare result for non async function
  if (!tfunction->is_async()) {
    f_service_ <<
      indent() << resultname << " result = new " << resultname << "();" << endl;
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
    f_service_ << "result.success = ";
  }
  f_service_ <<
    "iface_." << tfunction->get_name() << "(";
  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
    } else {
      f_service_ << ", ";
    }
    f_service_ << "args." << (*f_iter)->get_name();
  }
  f_service_ << ");" << endl;

  // Set isset on success field
  if (!tfunction->is_async() && !tfunction->get_returntype()->is_void()) {
    f_service_ <<
      indent() << "result.__isset.success = true;" << endl;
  }

  if (!tfunction->is_async() && xceptions.size() > 0) {
    indent_down();
    f_service_ << indent() << "}";
    for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
      f_service_ << " catch (" << (*x_iter)->get_type()->get_name() << " " << (*x_iter)->get_name() << ") {" << endl;
      if (!tfunction->is_async()) {
        indent_up();
        f_service_ <<
          indent() << "result." << (*x_iter)->get_name() << " = " << (*x_iter)->get_name() << ";" << endl <<
          indent() << "result.__isset." << (*x_iter)->get_name() << " = true;" << endl;
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
    scope_down(f_service_);

    // Close class
    indent_down();
    f_service_ <<
      indent() << "}" << endl <<
      endl;
    return;
  }

  f_service_ <<
    indent() << "oprot.writeMessageBegin(new TMessage(\"" << tfunction->get_name() << "\", TMessageType.REPLY, seqid));" << endl <<
    indent() << "result.write(oprot);" << endl <<
    indent() << "oprot.writeMessageEnd();" << endl <<
    indent() << "oprot.getTransport().flush();" << endl;

  // Close function
  scope_down(f_service_);
  f_service_ << endl;

  // Close class
  indent_down();
  f_service_ <<
    indent() << "}" << endl <<
    endl;
}

/**
 * Deserializes a field of any type.
 *
 * @param tfield The field
 * @param prefix The variable name or container for this field
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
                                (t_struct*)type,
                                name);
  } else if (type->is_container()) {
    generate_deserialize_container(out, type, name);
  } else if (type->is_base_type() || type->is_enum()) {

    indent(out) <<
      name << " = iprot.";
    
    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "compiler error: cannot serialize void field in a struct: " +
          name;
        break;
      case t_base_type::TYPE_STRING:        
        if (((t_base_type*)type)->is_binary()) {
          out << "readBinary();";
        } else {
          out << "readString();";
        }
        break;
      case t_base_type::TYPE_BOOL:
        out << "readBool();";
        break;
      case t_base_type::TYPE_BYTE:
        out << "readByte();";
        break;
      case t_base_type::TYPE_I16:
        out << "readI16();";
        break;
      case t_base_type::TYPE_I32:
        out << "readI32();";
        break;
      case t_base_type::TYPE_I64:
        out << "readI64();";
        break;
      case t_base_type::TYPE_DOUBLE:
        out << "readDouble();";
        break;
      default:
        throw "compiler error: no Java name for base type " + tbase;
      }
    } else if (type->is_enum()) {
      out << "readI32();";
    }
    out <<
      endl;
  } else {
    printf("DO NOT KNOW HOW TO DESERIALIZE FIELD '%s' TYPE '%s'\n",
           tfield->get_name().c_str(), type_name(type).c_str());
  }
}

/**
 * Generates an unserializer for a struct, invokes read()
 */
void t_java_generator::generate_deserialize_struct(ofstream& out,
                                                   t_struct* tstruct,
                                                   string prefix) {
  out <<
    indent() << prefix << " = new " << type_name(tstruct) << "();" << endl <<
    indent() << prefix << ".read(iprot);" << endl;
}

/**
 * Deserializes a container by reading its size and then iterating
 */
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
    indent(out) << "TMap " << obj << " = iprot.readMapBegin();" << endl;
  } else if (ttype->is_set()) {
    indent(out) << "TSet " << obj << " = iprot.readSetBegin();" << endl;
  } else if (ttype->is_list()) {
    indent(out) << "TList " << obj << " = iprot.readListBegin();" << endl;
  }

  indent(out)
    << prefix << " = new " << type_name(ttype, false, true) 
    // size the collection correctly
    << "(" 
    << (ttype->is_list() ? "" : "2*" )
    << obj << ".size"
    << ");" << endl;

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
    indent(out) << "iprot.readMapEnd();" << endl;
  } else if (ttype->is_set()) {
    indent(out) << "iprot.readSetEnd();" << endl;
  } else if (ttype->is_list()) {
    indent(out) << "iprot.readListEnd();" << endl;
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
    declare_field(&fkey) << endl;
  indent(out) <<
    declare_field(&fval) << endl;

  generate_deserialize_field(out, &fkey);
  generate_deserialize_field(out, &fval);

  indent(out) <<
    prefix << ".put(" << key << ", " << val << ");" << endl;
}

/**
 * Deserializes a set element
 */
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

/**
 * Deserializes a list element
 */
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
                              (t_struct*)type,
                              prefix + tfield->get_name());
  } else if (type->is_container()) {
    generate_serialize_container(out,
                                 type,
                                 prefix + tfield->get_name());
  } else if (type->is_base_type() || type->is_enum()) {

    string name = prefix + tfield->get_name();
    indent(out) <<
      "oprot.";
    
    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw
          "compiler error: cannot serialize void field in a struct: " + name;
        break;
      case t_base_type::TYPE_STRING:
        if (((t_base_type*)type)->is_binary()) {
          out << "writeBinary(" << name << ");";
        } else {
          out << "writeString(" << name << ");";
        }
        break;
      case t_base_type::TYPE_BOOL:
        out << "writeBool(" << name << ");";
        break;
      case t_base_type::TYPE_BYTE:
        out << "writeByte(" << name << ");";
        break;
      case t_base_type::TYPE_I16:
        out << "writeI16(" << name << ");";
        break;
      case t_base_type::TYPE_I32:
        out << "writeI32(" << name << ");";
        break;
      case t_base_type::TYPE_I64:
        out << "writeI64(" << name << ");";
        break;
      case t_base_type::TYPE_DOUBLE:
        out << "writeDouble(" << name << ");";
        break;
      default:
        throw "compiler error: no Java name for base type " + tbase;
      }
    } else if (type->is_enum()) {
      out << "writeI32(" << name << ");";
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
    indent() << prefix << ".write(oprot);" << endl;
}

/**
 * Serializes a container by writing its size then the elements.
 *
 * @param ttype  The type of container
 * @param prefix String prefix for fields
 */
void t_java_generator::generate_serialize_container(ofstream& out,
                                                    t_type* ttype,
                                                    string prefix) {
  scope_up(out);
  
  if (ttype->is_map()) {
    indent(out) <<
      "oprot.writeMapBegin(new TMap(" <<
      type_to_enum(((t_map*)ttype)->get_key_type()) << ", " <<
      type_to_enum(((t_map*)ttype)->get_val_type()) << ", " <<
      prefix << ".size()));" << endl;
  } else if (ttype->is_set()) {
    indent(out) <<
      "oprot.writeSetBegin(new TSet(" <<
      type_to_enum(((t_set*)ttype)->get_elem_type()) << ", " <<
      prefix << ".size()));" << endl;
  } else if (ttype->is_list()) {
    indent(out) <<
      "oprot.writeListBegin(new TList(" <<
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
        "oprot.writeMapEnd();" << endl;
    } else if (ttype->is_set()) {
      indent(out) <<
        "oprot.writeSetEnd();" << endl;
    } else if (ttype->is_list()) {
      indent(out) <<
        "oprot.writeListEnd();" << endl;
    }
    
    scope_down(out);
 
  scope_down(out);  
}

/**
 * Serializes the members of a map.
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
 * @return Java type name, i.e. HashMap<Key,Value>
 */
string t_java_generator::type_name(t_type* ttype, bool in_container, bool in_init) {
  // In Java typedefs are just resolved to their real type
  while (ttype->is_typedef()) {
    ttype = ((t_typedef*)ttype)->get_type();
  }

  if (ttype->is_base_type()) {
    return base_type_name((t_base_type*)ttype, in_container);
  } else if (ttype->is_enum()) {
    return (in_container ? "Integer" : "int");
  } else if (ttype->is_map()) {
    t_map* tmap = (t_map*) ttype;
    string prefix;
    if (in_init) {
      prefix = "HashMap";
    } else {
      prefix = "AbstractMap";
    }
    return prefix + "<" +
      type_name(tmap->get_key_type(), true) + "," +
      type_name(tmap->get_val_type(), true) + ">";
  } else if (ttype->is_set()) {
    t_set* tset = (t_set*) ttype;
    return "HashSet<" + type_name(tset->get_elem_type(), true) + ">";
  } else if (ttype->is_list()) {
    t_list* tlist = (t_list*) ttype;
    return "ArrayList<" + type_name(tlist->get_elem_type(), true) + ">";
  }

  // Check for namespacing
  t_program* program = ttype->get_program();
  if (program != NULL && program != program_) {
    string package = program->get_java_package();
    if (!package.empty()) {
      return package + "." + ttype->get_name();
    }
  }

  return ttype->get_name();
}

/**
 * Returns the C++ type that corresponds to the thrift type.
 *
 * @param tbase The base type
 * @param container Is it going in a Java container?
 */
string t_java_generator::base_type_name(t_base_type* type,
                                        bool in_container) {
  t_base_type::t_base tbase = type->get_base();

  switch (tbase) {
  case t_base_type::TYPE_VOID:
    return "void";
  case t_base_type::TYPE_STRING:
    if (type->is_binary()) {
      return "byte[]";
    } else {
      return "String";
    }
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
    if (ttype->is_base_type() && tfield->get_value() != NULL) {
      ofstream dummy;
      result += " = " + render_const_value(dummy, tfield->get_name(), ttype, tfield->get_value());
    } else if (ttype->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)ttype)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "NO T_VOID CONSTRUCT";
      case t_base_type::TYPE_STRING:
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
      result += " = new " + type_name(ttype, false, true) + "()";
    } else {
      result += " = new " + type_name(ttype, false, true) + "()";;
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
    type_name(ttype) + " " + prefix + tfunction->get_name() + "(" + argument_list(tfunction->get_arglist()) + ") throws ";
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
 * Renders a comma separated field list, with type names
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
