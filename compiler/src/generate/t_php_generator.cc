#include <stdlib.h>
#include <sys/stat.h>
#include <sstream>
#include "t_php_generator.h"
using namespace std;

/**
 * Prepares for file generation by opening up the necessary file output
 * streams.
 *
 * @param tprogram The program to generate
 */
void t_php_generator::init_generator(t_program* tprogram) {
  // Make output directory
  mkdir(T_PHP_DIR, S_IREAD | S_IWRITE | S_IEXEC);

  // Make output file
  string f_types_name = string(T_PHP_DIR)+"/"+program_name_+"Types.php";
  f_types_.open(f_types_name.c_str());

  // Print header
  f_types_ <<
    "<?php" << endl <<
    autogen_comment() <<
    php_includes();
}

/**
 * Prints standard java imports
 */
string t_php_generator::php_includes() {
  return
    string("require_once THRIFT_ROOT.'/Thrift.php';\n\n");
}

/**
 * Does nothing in PHP
 */
void t_php_generator::close_generator() {
  // Close types file
  f_types_ << "?>" << endl;
  f_types_.close();
}

/**
 * Generates a typedef. This is not done in PHP, types are all implicit.
 *
 * @param ttypedef The type definition
 */
void t_php_generator::generate_typedef(t_typedef* ttypedef) {}

/**
 * Generates code for an enumerated type. Since define is expensive to lookup
 * in PHP, we use a global array for this.
 *
 * @param tenum The enumeration
 */
void t_php_generator::generate_enum(t_enum* tenum) {
  f_types_ <<
    "$GLOBALS['E_" << tenum->get_name() << "'] = array(" << endl;
  
  vector<t_constant*> constants = tenum->get_constants();
  vector<t_constant*>::iterator c_iter;
  int value = -1;
  for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    if ((*c_iter)->has_value()) {
      value = (*c_iter)->get_value();
    } else {
      ++value;
    }

    f_types_ <<
      "  '" << (*c_iter)->get_name() << "' => " << value << "," << endl;
  }

  f_types_ <<
    ");" << endl << endl;


  // We're also doing it this way to see how it performs. It's more legible
  // code but you can't do things like an 'extract' on it, which is a bit of
  // a downer.

  f_types_ <<
    "final class " << tenum->get_name() << " {" << endl;
  indent_up();
  
  value = -1;
  for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    if ((*c_iter)->has_value()) {
      value = (*c_iter)->get_value();
    } else {
      ++value;
    }

    indent(f_types_) <<
      "const " << (*c_iter)->get_name() << " = " << value << ";" << endl;
  }

  indent_down();
  f_types_ << "}" << endl << endl;
}

/**
 * Generates a struct definition for a thrift data type. This is nothing in PHP
 * where the objects are all just associative arrays (unless of course we
 * decide to start using objects for them...)
 *
 * @param tstruct The struct definition
 */
void t_php_generator::generate_struct(t_struct* tstruct) {
  f_types_ <<
    "class " << tstruct->get_name() << " {" << endl;
  indent_up();

  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter; 
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    indent(f_types_) <<
      "public " << declare_field(*m_iter, true) << endl;
  }
 
  indent_down();

  f_types_ <<
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
void t_php_generator::generate_service(t_service* tservice) {
  string f_service_name = string(T_PHP_DIR)+"/"+service_name_+".php";
  f_service_.open(f_service_name.c_str());

  f_service_ <<
    "<?php" << endl <<
    autogen_comment() <<
    php_includes();

  f_service_ <<
    "require_once '" << service_name_ << "Types.php';" << endl << endl;

  // Generate the three main parts of the service (well, two for now in PHP)
  generate_service_interface(tservice);
  // generate_service_server(tservice);
  generate_service_client(tservice);
  
  // Close service file
  f_service_ << "?>" << endl;
  f_service_.close();
}

/**
 * Generates a service interface definition.
 *
 * @param tservice The service to generate a header definition for
 */
void t_php_generator::generate_service_interface(t_service* tservice) {
  f_service_ <<
    "abstract class " << service_name_ << "If {" << endl;
  indent_up();
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter; 
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    indent(f_service_) <<
      "public abstract function " << function_signature(*f_iter) << ";" << endl;
  }
  indent_down();
  f_service_ <<
    "}" << endl << endl;
}

/**
 * Generates a service client definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_php_generator::generate_service_client(t_service* tservice) {
  f_service_ <<
    "class " << service_name_ << "Client " <<
    "extends " << service_name_ << "If {" << endl;
  indent_up();

  // Private members
  f_service_ <<
    indent() << "private $_itrans = null;" << endl <<
    indent() << "private $_otrans = null;" << endl <<
    indent() << "private $_iprot = null;"  << endl <<
    indent() << "private $_oprot = null;"  << endl << endl;

  // Constructor function
  f_service_ <<
    indent() << "public function __construct() {" << endl <<
    indent() << "  $argv = func_get_args();" << endl <<
    indent() << "  $argc = count($argv);" << endl <<
    indent() << "  if ($argc == 2) {" << endl <<
    indent() << "    $this->_itrans = $this->_otrans = $argv[0];" << endl <<
    indent() << "    $this->_iprot = $this->_oprot = $argv[1];" << endl <<
    indent() << "  } else if ($argc == 4) {" << endl <<
    indent() << "    $this->_itrans = $argv[0];" << endl <<
    indent() << "    $this->_otrans = $argv[1];" << endl <<
    indent() << "    $this->_iprot = $argv[2];" << endl <<
    indent() << "    $this->_oprot = $argv[3];" << endl <<
    indent() << "  }" << endl <<
    indent() << "}" << endl << endl;   

  // Generate client method implementations
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::const_iterator f_iter; 
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    string funname = (*f_iter)->get_name();

    // Open function
    indent(f_service_) <<
      "public function " << function_signature(*f_iter) << endl;
    scope_up(f_service_);

    // Serialize the request
    f_service_ <<
      indent() <<
      "$this->_oprot->writeStructBegin($this->_otrans, 'function');" << endl <<
      indent() <<
      "$this->_oprot->writeFieldBegin($this->_otrans, 'name', TType::STRING, 0);"  << endl <<
      indent() <<
      "$this->_oprot->writeString($this->_otrans, '" << funname << "');" << endl <<
      indent() <<
      "$this->_oprot->writeFieldEnd($this->_otrans);" << endl <<
      indent() <<
      "$this->_oprot->writeFieldBegin($this->_otrans, 'args', TType::STRUCT, 1);" << endl;
    generate_serialize_struct((*f_iter)->get_arglist());
    f_service_ <<
      indent() <<
      "$this->_oprot->writeFieldEnd($this->_otrans);" << endl <<
      indent() <<
      "$this->_oprot->writeFieldStop($this->_otrans);" << endl <<
      indent() <<
      "$this->_oprot->writeStructEnd($this->_otrans);" << endl;
    
    // Flush the request
    indent(f_service_) <<
      "$this->_otrans->flush();" << endl;

    // Read the response
    t_struct result_struct((*f_iter)->get_name() + "_result");
    t_field result_field((*f_iter)->get_returntype(), "_result");

    // Add a field to the return struct if non void
    if (!(*f_iter)->get_returntype()->is_void()) {
      indent(f_service_) <<
        declare_field(&result_field, true, true) << endl;
      result_struct.append(&result_field);
    }

    // Deserialize response struct
    generate_deserialize_struct(&result_struct);

    // Careful, only return _result if not a void function
    if (!(*f_iter)->get_returntype()->is_void()) {
      indent(f_service_) <<
        "return $_result;" << endl;
    } else {
      indent(f_service_) <<
        "return;" << endl;
    }

    // Close function
    scope_down(f_service_);
    f_service_ << endl;
  }

  indent_down();
  f_service_ <<
    "}" << endl;
  f_service_.close();
}

/**
 * Deserializes a field of any type.
 */
void t_php_generator::generate_deserialize_field(t_field* tfield,
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
                                 name + "->");
  } else if (type->is_container()) {
    generate_deserialize_container(tfield->get_type(), name);
  } else if (type->is_base_type() || type->is_enum()) {

    indent(f_service_) <<
      "$this->_iprot->";
    
    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "compiler error: cannot serialize void field in a struct: " +
          name;
        break;
      case t_base_type::TYPE_STRING:        
        f_service_ << "readString($this->_itrans, $" << name << ");";
        break;
      case t_base_type::TYPE_BYTE:
        f_service_ << "readByte($this->_itrans, $" << name << ");";
        break;
      case t_base_type::TYPE_I32:
        f_service_ << "readI32($this->_itrans, $" << name << ");";
        break;
      case t_base_type::TYPE_U32:
        f_service_ << "readU32($this->_itrans, $" << name << ");";
        break;
      case t_base_type::TYPE_I64:
        f_service_ << "readI64($this->_itrans, $" << name << ");";
        break;
      case t_base_type::TYPE_U64:
        f_service_ << "readU64($this->_itrans, $" << name << ");";
        break;
      default:
        throw "compiler error: no C++ name for base type " + tbase;
      }
    } else if (type->is_enum()) {
      f_service_ << "readI32($this->_itrans, $" << name << ");";
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
void t_php_generator::generate_deserialize_struct(t_struct* tstruct,
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
    indent() << "$" << fname << " = null;" << endl <<
    indent() << "$" << ftype << " = null;" << endl <<
    indent() << "$" << fid << " = 0;" << endl <<
    indent() << "$this->_iprot->readStructBegin($this->_itrans, $" << fname << ");" << endl;
  
  // Loop over reading in fields
  indent(f_service_) <<
    "while (true)" << endl;

    scope_up(f_service_);
    
    // Read beginning field marker
    indent(f_service_) <<
      "$this->_iprot->readFieldBegin($this->_itrans, " << 
      "$" << fname << ", $" << ftype << ", $" << fid << ");" << endl;
    
    // Check for field STOP marker and break
    indent(f_service_) <<
      "if ($" << ftype << " == TType::STOP) { " << endl;
    indent_up();
    indent(f_service_) <<
      "break;" << endl;
    indent_down();
    indent(f_service_) <<
      "}" << endl;
    
    // Switch statement on the field we are reading
    indent(f_service_) <<
      "switch ($" << fid << ")" << endl;

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
        indent() <<  "default:" << endl <<
        indent() <<  "  $this->_iprot->skip($this->_itrans, $" << ftype << ");" << endl <<
        indent() <<  "  break;" << endl;
      
      scope_down(f_service_);

    // Read field end marker
    indent(f_service_) <<
      "$this->_iprot->readFieldEnd($this->_itrans);" << endl;
    
    scope_down(f_service_);
      
  indent(f_service_) <<
    "$this->_iprot->readStructEnd($this->_itrans);" << endl;

  scope_down(f_service_);
}

void t_php_generator::generate_deserialize_container(t_type* ttype,
                                                     string prefix) {
  scope_up(f_service_);
  
  string size = tmp("_size");
  string ktype = tmp("_ktype");
  string vtype = tmp("_vtype");
  string etype = tmp("_etype");
  
  indent(f_service_) <<
    "$" << size << " = 0;" << endl;
  
  // Declare variables, read header
  if (ttype->is_map()) {
    f_service_ <<
      indent() << "$" << ktype << " = 0;" << endl <<
      indent() << "$" << vtype << " = 0;" << endl <<
      indent() << "$this->_iprot->readMapBegin($this->_itrans, " <<
      "$" << ktype << ", $" << vtype << ", $" << size << ");" << endl;
  } else if (ttype->is_set()) {
    f_service_ <<
      indent() << "$" << etype << " = 0;" << endl <<
      indent() << "$this->_iprot->readSetBegin($this->_itrans, " <<
      "$" << etype << ", $" << size << ");" << endl;
  } else if (ttype->is_list()) {
    f_service_ <<
      indent() << "$" << etype << " = 0;" << endl <<
      indent() << "$this->_iprot->readListBegin($this->_itrans, " <<
      "$" << etype << ", $" << size << ");" << endl;
  }

  // For loop iterates over elements
  string i = tmp("_i");
  indent(f_service_) <<
    "for ($" <<
    i << " = 0; $" << i << " < $" << size << "; ++$" << i << ")" << endl;
  
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
    indent(f_service_) << "$this->_iprot->readMapEnd($this->_itrans);" << endl;
  } else if (ttype->is_set()) {
    indent(f_service_) << "$this->_iprot->readSetEnd($this->_itrans);" << endl;
  } else if (ttype->is_list()) {
    indent(f_service_) << "$this->_iprot->readListEnd($this->_itrans);" << endl;
  }

  scope_down(f_service_);
}


/**
 * Generates code to deserialize a map
 */
void t_php_generator::generate_deserialize_map_element(t_map* tmap,
                                                       string prefix) {
  string key = tmp("_key");
  string val = tmp("_val");
  t_field fkey(tmap->get_key_type(), key);
  t_field fval(tmap->get_val_type(), val);

  indent(f_service_) <<
    declare_field(&fkey, true, true) << endl;
  indent(f_service_) <<
    declare_field(&fval, true, true) << endl;

  generate_deserialize_field(&fkey);
  generate_deserialize_field(&fval);

  indent(f_service_) <<
    "$" << prefix << "[$" << key << "] = $" << val << ";" << endl;
}

void t_php_generator::generate_deserialize_set_element(t_set* tset,
                                                       string prefix) {
  string elem = tmp("_elem");
  t_field felem(tset->get_elem_type(), elem);

  indent(f_service_) <<
    "$" << elem << " = null;" << endl;

  generate_deserialize_field(&felem);

  indent(f_service_) <<
    "$" << prefix << " []= $" << elem << ";" << endl;
}

void t_php_generator::generate_deserialize_list_element(t_list* tlist,
                                                        string prefix) {
  string elem = tmp("_elem");
  t_field felem(tlist->get_elem_type(), elem);

  indent(f_service_) <<
    "$" << elem << " = null;" << endl;

  generate_deserialize_field(&felem);

  indent(f_service_) <<
    "$" << prefix << " []= $" << elem << ";" << endl;
}


/**
 * Serializes a field of any type.
 *
 * @param tfield The field to serialize
 * @param prefix Name to prepend to field name
 */
void t_php_generator::generate_serialize_field(t_field* tfield,
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
                              prefix + tfield->get_name() + "->");
  } else if (type->is_container()) {
    generate_serialize_container(tfield->get_type(),
                                 prefix + tfield->get_name());
  } else if (type->is_base_type() || type->is_enum()) {

    string name = prefix + tfield->get_name();
    indent(f_service_) <<
      "$this->_oprot->";
    
    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw
          "compiler error: cannot serialize void field in a struct: " + name;
        break;
      case t_base_type::TYPE_STRING:
        f_service_ << "writeString($this->_otrans, $" << name << ");";
        break;
      case t_base_type::TYPE_BYTE:
        f_service_ << "writeByte($this->_otrans, $" << name << ");";
        break;
      case t_base_type::TYPE_I32:
        f_service_ << "writeI32($this->_otrans, $" << name << ");";
        break;
      case t_base_type::TYPE_U32:
        f_service_ << "writeU32($this->_otrans, $" << name << ");";
        break;
      case t_base_type::TYPE_I64:
        f_service_ << "writeI64($this->_otrans, $" << name << ");";
        break;
      case t_base_type::TYPE_U64:
        f_service_ << "writeU64($this->_otrans, $" << name << ");";
        break;
      default:
        throw "compiler error: no C++ name for base type " + tbase;
      }
    } else if (type->is_enum()) {
      f_service_ << "writeI32($this->_otrans, $" << name << ");";
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
void t_php_generator::generate_serialize_struct(t_struct* tstruct,
                                                 string prefix) {
  string name = tstruct->get_name();
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  scope_up(f_service_);
  indent(f_service_) <<
    "$this->_oprot->writeStructBegin($this->_otrans, '" << name << "');" << endl;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    // Write field header
    indent(f_service_) <<
      "$this->_oprot->writeFieldBegin($this->_otrans, " <<
      "'" << (*f_iter)->get_name() << "', " <<
      type_to_enum((*f_iter)->get_type()) << ", " <<
      (*f_iter)->get_key() << ");" << endl;
    // Write field contents
    generate_serialize_field(*f_iter, prefix);
    // Write field closer
    indent(f_service_) <<
      "$this->_oprot->writeFieldEnd($this->_otrans);" << endl;
  }
  // Write the struct map
  f_service_ <<
    indent() << "$this->_oprot->writeFieldStop($this->_otrans);" << endl <<
    indent() << "$this->_oprot->writeStructEnd($this->_otrans);" << endl;

  scope_down(f_service_);
}

void t_php_generator::generate_serialize_container(t_type* ttype,
                                                   string prefix) {
  scope_up(f_service_);
  
  if (ttype->is_map()) {
    indent(f_service_) <<
      "$this->_oprot->writeMapBegin($this->_otrans, " <<
      type_to_enum(((t_map*)ttype)->get_key_type()) << ", " <<
      type_to_enum(((t_map*)ttype)->get_val_type()) << ", " <<
      "count($" << prefix << "));" << endl;
  } else if (ttype->is_set()) {
    indent(f_service_) <<
      "$this->_oprot->writeSetBegin($this->_otrans, " <<
      type_to_enum(((t_set*)ttype)->get_elem_type()) << ", " <<
      "count($" << prefix << "));" << endl;
  } else if (ttype->is_list()) {
    indent(f_service_) <<
      "$this->_oprot->writeListBegin($this->_otrans, " <<
      type_to_enum(((t_list*)ttype)->get_elem_type()) << ", " <<
      "count($" << prefix << "));" << endl;
  }

    scope_up(f_service_);

    if (ttype->is_map()) {
      string kiter = tmp("_kiter");
      string viter = tmp("_viter");
      indent(f_service_) << 
        "foreach ($" << prefix << " as " <<
        "$" << kiter << " => $" << viter << ")" << endl;
      scope_up(f_service_);     
      generate_serialize_map_element((t_map*)ttype, kiter, viter);
      scope_down(f_service_);
    } else if (ttype->is_set()) {
      string iter = tmp("_iter");
      indent(f_service_) << 
        "foreach ($" << prefix << " as $" << iter << ")" << endl;
      scope_up(f_service_);
      generate_serialize_set_element((t_set*)ttype, iter);
      scope_down(f_service_);
    } else if (ttype->is_list()) {
      string iter = tmp("_iter");
      indent(f_service_) << 
        "foreach ($" << prefix << " as $" << iter << ")" << endl;
      scope_up(f_service_);
      generate_serialize_list_element((t_list*)ttype, iter);
      scope_down(f_service_);
    }
    
    if (ttype->is_map()) {
      indent(f_service_) <<
        "$this->_oprot->writeMapEnd($this->_otrans);" << endl;
    } else if (ttype->is_set()) {
      indent(f_service_) <<
        "$this->_oprot->writeSetEnd($this->_otrans);" << endl;
    } else if (ttype->is_list()) {
      indent(f_service_) <<
        "$this->_oprot->writeListEnd($this->_otrans);" << endl;
    }
    
    scope_down(f_service_);
 
  scope_down(f_service_);  
}

/**
 * Serializes the members of a map.
 *
 */
void t_php_generator::generate_serialize_map_element(t_map* tmap,
                                                      string kiter,
                                                      string viter) {
  t_field kfield(tmap->get_key_type(), kiter);
  generate_serialize_field(&kfield, "");

  t_field vfield(tmap->get_val_type(), viter);
  generate_serialize_field(&vfield, "");
}

/**
 * Serializes the members of a set.
 */
void t_php_generator::generate_serialize_set_element(t_set* tset,
                                                      string iter) {
  t_field efield(tset->get_elem_type(), iter);
  generate_serialize_field(&efield, "");
}

/**
 * Serializes the members of a list.
 */
void t_php_generator::generate_serialize_list_element(t_list* tlist,
                                                      string iter) {
  t_field efield(tlist->get_elem_type(), iter);
  generate_serialize_field(&efield, "");
}

/**
 * Returns a Java type name
 *
 * @param ttype The type
 */
string t_php_generator::type_name(t_type* ttype) {
  // In Java typedefs are just resolved to their real type
  while (ttype->is_typedef()) {
    ttype = ((t_typedef*)ttype)->get_type();
  }

  if (ttype->is_base_type()) {
    return base_type_name(((t_base_type*)ttype)->get_base());
  } else if (ttype->is_enum()) {
    return "Int32";
  } else if (ttype->is_map()) {
    t_map* tmap = (t_map*) ttype;
    return "HashMap<" +
      type_name(tmap->get_key_type()) + "," +
      type_name(tmap->get_val_type()) + ">";
  } else if (ttype->is_set()) {
    t_set* tset = (t_set*) ttype;
    return "HashSet<" + type_name(tset->get_elem_type()) + ">";
  } else if (ttype->is_list()) {
    t_list* tlist = (t_list*) ttype;
    return "ArrayList<" + type_name(tlist->get_elem_type()) + ">";
  } else {
    return ttype->get_name();
  }
}

/**
 * Returns the C++ type that corresponds to the thrift type.
 *
 * @param tbase The base type
 */
string t_php_generator::base_type_name(t_base_type::t_base tbase) {
  switch (tbase) {
  case t_base_type::TYPE_VOID:
    return "void";
  case t_base_type::TYPE_STRING:
    return "TString";
  case t_base_type::TYPE_BYTE:
    return "UInt8";
  case t_base_type::TYPE_I32:
    return "Int32";
  case t_base_type::TYPE_U32:
    return "UInt32";
  case t_base_type::TYPE_I64:
    return "Int64";
  case t_base_type::TYPE_U64:
    return "UInt64";
  default:
    throw "compiler error: no C++ name for base type " + tbase;
  }
}

/**
 * Declares a field, which may include initialization as necessary.
 *
 * @param ttype The type
 */
string t_php_generator::declare_field(t_field* tfield, bool init, bool obj) {
  string result = "$" + tfield->get_name();
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
        result += " = ''";
        break;
      case t_base_type::TYPE_BYTE:
      case t_base_type::TYPE_I32:
      case t_base_type::TYPE_U32:
      case t_base_type::TYPE_I64:
      case t_base_type::TYPE_U64:
        result += " = 0";
        break;
      default:
        throw "compiler error: no PHP initializer for base type " + tbase;
      }
    } else if (type->is_enum()) {
      result += " = 0";
    } else if (type->is_container()) {
      result += " = array()";
    } else if (type->is_struct()) {
      if (obj) {
        result += " = new " + type->get_name() + "()";
      } else {
        result += " = null";
      }
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
string t_php_generator::function_signature(t_function* tfunction,
                                           string prefix) {
  return
    prefix + tfunction->get_name() +
    "(" + argument_list(tfunction->get_arglist()) + ")";
}

/**
 * Renders a field list
 */
string t_php_generator::argument_list(t_struct* tstruct) {
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
    result += "$" + (*f_iter)->get_name();
  }
  return result;
}

/**
 * Converts the parse type to a C++ enum string for the given type.
 */
string t_php_generator ::type_to_enum(t_type* type) {
  while (type->is_typedef()) {
    type = ((t_typedef*)type)->get_type();
  }
  
  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "NO T_VOID CONSTRUCT";
    case t_base_type::TYPE_STRING:
      return "TType::STRING";
    case t_base_type::TYPE_BYTE:
      return "TType::BYTE";
    case t_base_type::TYPE_I32:
      return "TType::I32";
    case t_base_type::TYPE_U32:
      return "TType::U32";
    case t_base_type::TYPE_I64:
      return "TType::I64";
    case t_base_type::TYPE_U64:
      return "TType::U64";
    }
  } else if (type->is_enum()) {
    return "TType::I32";
  } else if (type->is_struct()) {
    return "TType::STRUCT";
  } else if (type->is_map()) {
    return "TType::MAP";
  } else if (type->is_set()) {
    return "TType::SET";
  } else if (type->is_list()) {
    return "TType::LST";
  }

  throw "INVALID TYPE IN type_to_enum: " + type->get_name();
}
