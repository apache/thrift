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
void t_php_generator::init_generator() {
  // Make output directory
  mkdir(T_PHP_DIR, S_IREAD | S_IWRITE | S_IEXEC);

  // Make output file
  string f_types_name = string(T_PHP_DIR)+"/"+program_name_+"_types.php";
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
    string("include_once $GLOBALS['THRIFT_ROOT'].'/Thrift.php';\n\n");
}

/**
 * Close up (or down) some filez.
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
  
  vector<t_enum_value*> constants = tenum->get_constants();
  vector<t_enum_value*>::iterator c_iter;
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

  // Prevent instantiation of this class
  f_types_ <<
    indent() << "private function __construct() {}" << endl;

  indent_down();
  f_types_ << "}" << endl << endl;
}

/**
 * Make a struct
 */
void t_php_generator::generate_struct(t_struct* tstruct) {
  generate_php_struct(tstruct, false);
}

/**
 * Generates a struct definition for a thrift exception. Basically the same
 * as a struct but extends the Exception class.
 *
 * @param txception The struct definition
 */
void t_php_generator::generate_xception(t_struct* txception) {
  generate_php_struct(txception, true);  
}

/**
 * Structs can be normal or exceptions.
 */
void t_php_generator::generate_php_struct(t_struct* tstruct,
                                          bool is_exception) {
  generate_php_struct_definition(f_types_, tstruct, is_exception);
}

/**
 * Generates a struct definition for a thrift data type. This is nothing in PHP
 * where the objects are all just associative arrays (unless of course we
 * decide to start using objects for them...)
 *
 * @param tstruct The struct definition
 */
void t_php_generator::generate_php_struct_definition(ofstream& out,
                                                     t_struct* tstruct,
                                                     bool is_exception) {
  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter; 

  out <<
    "class " << tstruct->get_name();
  if (is_exception) {
    out << " extends Exception";
  }
  out <<
    " {" << endl;
  indent_up();

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    indent(out) <<
      "public $" << (*m_iter)->get_name() << " = null;" << endl;
  }
 
  out << endl;

  // Generate constructor from array
  if (members.size() > 0) {
    out <<
      indent() << "public function __construct($vals=null) {" << endl;
    indent_up();
    out <<
      indent() << "if (is_array($vals)) {" << endl;
    indent_up();
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      out <<
        indent() << "if (isset($vals['" << (*m_iter)->get_name() << "'])) {" << endl <<
        indent() << "  $this->" << (*m_iter)->get_name() << " = $vals['" << (*m_iter)->get_name() << "'];" << endl <<
        indent() << "}" << endl;
    }
    indent_down();
    out <<
      indent() << "}" << endl;
    indent_down();
    out <<
      indent() << "}" << endl <<
      endl;
  }
  
  generate_php_struct_reader(out, tstruct);
  generate_php_struct_writer(out, tstruct);

  indent_down();
  out <<
    indent() << "}" << endl <<
    endl;
}

/**
 * Generates the read() method for a struct
 */
void t_php_generator::generate_php_struct_reader(ofstream& out,
                                                 t_struct* tstruct) {
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  indent(out) <<
    "public function read($input) " << endl;
  scope_up(out);

  out <<
    indent() << "$xfer = 0;" << endl <<
    indent() << "$fname = null;" << endl <<
    indent() << "$ftype = 0;" << endl <<
    indent() << "$fid = 0;" << endl;

  // Declare stack tmp variables
  if (!binary_inline_) {
    indent(out) <<
      "$xfer += $input->readStructBegin($fname);" << endl;   
  }

  // Loop over reading in fields
  indent(out) <<
    "while (true)" << endl;

    scope_up(out);
    
    // Read beginning field marker
    if (binary_inline_) {
      t_field fftype(g_type_byte, "ftype");
      t_field ffid(g_type_i16, "fid");
      generate_deserialize_field(out, &fftype);
      out <<
        indent() << "if ($ftype == TType::STOP) {" << endl <<
        indent() << "  break;" << endl <<
        indent() << "}" << endl;      
      generate_deserialize_field(out, &ffid);
    } else {
      indent(out) <<
        "$xfer += $input->readFieldBegin($fname, $ftype, $fid);" << endl;
      // Check for field STOP marker and break
      indent(out) <<
        "if ($ftype == TType::STOP) {" << endl;
      indent_up();
      indent(out) <<
        "break;" << endl;
      indent_down();
      indent(out) <<
        "}" << endl;
    }   
   
    // Switch statement on the field we are reading
    indent(out) <<
      "switch ($fid)" << endl;

      scope_up(out);
    
      // Generate deserialization code for known cases
      for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
        indent(out) <<
          "case " << (*f_iter)->get_key() << ":" << endl;
        indent_up();
        generate_deserialize_field(out, *f_iter, "this->");
        indent(out) <<
          "break;" << endl;
        indent_down();
      }
      
      // In the default case we skip the field
      indent(out) <<  "default:" << endl;
      if (binary_inline_) {
        indent(out) <<  "  $xfer += TProtocol::skipBinary($input, $ftype);" << endl;
      } else {
        indent(out) <<  "  $xfer += $input->skip($ftype);" << endl;
      }
      indent(out) <<  "  break;" << endl;
      
      scope_down(out);
      
    if (!binary_inline_) {
      // Read field end marker
      indent(out) <<
        "$xfer += $input->readFieldEnd();" << endl;
    }
    
    scope_down(out);
    
  if (!binary_inline_) {
    indent(out) <<
      "$xfer += $input->readStructEnd();" << endl;
  }

  indent(out) <<
    "return $xfer;" << endl;

  indent_down();
  out <<
    indent() << "}" << endl <<
    endl;
}

/**
 * Generates the write() method for a struct
 */
void t_php_generator::generate_php_struct_writer(ofstream& out,
                                                 t_struct* tstruct) {
  string name = tstruct->get_name();
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  if (binary_inline_) {
    indent(out) <<
      "public function write(&$output) {" << endl;
  } else {
    indent(out) <<
      "public function write($output) {" << endl;
  }
  indent_up();
  
  indent(out) <<
    "$xfer = 0;" << endl;

  if (!binary_inline_) {
    indent(out) <<
      "$xfer += $output->writeStructBegin('" << name << "');" << endl;
  }

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {   
    out <<
      indent() << "if ($this->" << (*f_iter)->get_name() << " !== null) {" << endl;
    indent_up();

    // Write field header
    if (binary_inline_) {
      out <<
        indent() << "$output .= pack('c', " << type_to_enum((*f_iter)->get_type()) << ");" << endl <<
        indent() << "$output .= pack('n', " << (*f_iter)->get_key() << ");" << endl;
    } else {
      indent(out) <<
        "$xfer += $output->writeFieldBegin(" <<
        "'" << (*f_iter)->get_name() << "', " <<
        type_to_enum((*f_iter)->get_type()) << ", " <<
        (*f_iter)->get_key() << ");" << endl;
    }

    // Write field contents
    generate_serialize_field(out, *f_iter, "this->");

    // Write field closer
    if (!binary_inline_) {
      indent(out) <<
        "$xfer += $output->writeFieldEnd();" << endl;
    }

    indent_down();
    indent(out) <<
      "}" << endl;
  }

  if (binary_inline_) {
    out <<
      indent() << "$output .= pack('c', TType::STOP);" << endl;
  } else {
    out <<
      indent() << "$xfer += $output->writeFieldStop();" << endl <<
      indent() << "$xfer += $output->writeStructEnd();" << endl;
  }

  out <<
    indent() << "return $xfer;" << endl;

  indent_down();
  out <<
    indent() << "}" << endl <<
    endl;
}

/**
 * Generates a thrift service.
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
    "include_once $GLOBALS['THRIFT_ROOT'].'/packages/" << program_name_ << "/" << program_name_ << "_types.php';" << endl;

  if (tservice->get_extends() != NULL) {
    f_service_ <<
      "include_once $GLOBALS['THRIFT_ROOT'].'/packages/" << tservice->get_extends()->get_program()->get_name() << "/" << tservice->get_extends()->get_name() << ".php';" << endl;
  }
  
  f_service_ <<
    endl;

  // Generate the three main parts of the service (well, two for now in PHP)
  generate_service_interface(tservice);
  generate_service_rest(tservice);
  generate_service_client(tservice);
  generate_service_helpers(tservice);
  generate_service_processor(tservice);
  
  // Close service file
  f_service_ << "?>" << endl;
  f_service_.close();
}

/**
 * Generates a service server definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_php_generator::generate_service_processor(t_service* tservice) {
  // Generate the dispatch methods
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter; 

  string extends = "";
  string extends_processor = "";
  if (tservice->get_extends() != NULL) {
    extends = tservice->get_extends()->get_name();
    extends_processor = " extends " + extends + "Processor";
  }

  // Generate the header portion
  f_service_ <<
    "class " << service_name_ << "Processor" << extends_processor << " {" << endl;
  indent_up();

  if (extends.empty()) {
    f_service_ <<
      indent() << "protected $handler_ = null;" << endl;
  }

  f_service_ <<
    indent() << "public function __construct($handler) {" << endl;
  if (extends.empty()) {
    f_service_ <<
      indent() << "  $this->handler_ = $handler;" << endl;
  } else {
    f_service_ <<
      indent() << "  parent::__construct($handler);" << endl;
  }
  f_service_ <<
    indent() << "}" << endl <<
    endl;

  // Generate the server implementation
  indent(f_service_) <<
    "public function process($input, $output) {" << endl;
  indent_up();

  f_service_ <<
    indent() << "$rseqid = 0;" << endl <<
    indent() << "$fname = null;" << endl <<
    indent() << "$mtype = 0;" << endl <<
    endl;

  if (binary_inline_) {
    t_field ffname(g_type_string, "fname");
    t_field fmtype(g_type_byte, "mtype");
    t_field fseqid(g_type_i32, "rseqid");
    generate_deserialize_field(f_service_, &ffname, "", true);
    generate_deserialize_field(f_service_, &fmtype, "", true);
    generate_deserialize_field(f_service_, &fseqid, "", true);
  } else {
    f_service_ <<
      indent() << "$input->readMessageBegin($fname, $mtype, $rseqid);" << endl;
  }

  // TODO(mcslee): validate message 

  // HOT: check for method implementation
  f_service_ <<
    indent() << "$methodname = 'process_'.$fname;" << endl << 
    indent() << "if (!method_exists($this, $methodname)) {" << endl <<
    indent() << "  throw new Exception('Function '.$fname.' not implemented.');" << endl <<
    indent() << "}" << endl <<
    indent() << "$this->$methodname($rseqid, $input, $output);" << endl;
  indent_down();
  f_service_ <<
    indent() << "}" << endl <<
    endl;

  // Generate the process subfunctions
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    generate_process_function(tservice, *f_iter);
  }

  indent_down();
  f_service_ << "}" << endl;
}

/**
 * Generates a process function definition.
 *
 * @param tfunction The function to write a dispatcher for
 */
void t_php_generator::generate_process_function(t_service* tservice,
                                                t_function* tfunction) {
  // Open function
  indent(f_service_) <<
    "private function process_" << tfunction->get_name() <<
    "($seqid, $input, $output) {" << endl;
  indent_up();

  string argsname = service_name_ + "_" + tfunction->get_name() + "_args";
  string resultname = service_name_ + "_" + tfunction->get_name() + "_result";

  f_service_ <<
    indent() << "$args = new " << argsname << "();" << endl <<
    indent() << "$args->read($input);" << endl;
  if (!binary_inline_) {
    f_service_ <<
      indent() << "$input->readMessageEnd();" << endl;
  }

  t_struct* xs = tfunction->get_xceptions();
  const std::vector<t_field*>& xceptions = xs->get_members();
  vector<t_field*>::const_iterator x_iter;

  // Declare result for non async function
  if (!tfunction->is_async()) {
    f_service_ <<
      indent() << "$result = new " << resultname << "();" << endl;
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
    f_service_ << "$result->success = ";
  }
  f_service_ <<
    "$this->handler_->" << tfunction->get_name() << "(";
  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
    } else {
      f_service_ << ", ";
    }
    f_service_ << "$args->" << (*f_iter)->get_name();
  }
  f_service_ << ");" << endl;

  if (!tfunction->is_async() && xceptions.size() > 0) {
    indent_down();
    for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
      f_service_ <<
        indent() << "} catch (" << (*x_iter)->get_type()->get_name() << " $" << (*x_iter)->get_name() << ") {" << endl;
      if (!tfunction->is_async()) {
        indent_up();
        f_service_ <<
          indent() << "$result->" << (*x_iter)->get_name() << " = $" << (*x_iter)->get_name() << ";" << endl;
        indent_down();
        f_service_ << indent();
      }
    }
    f_service_ << "}" << endl;
  }

  // Shortcut out here for async functions
  if (tfunction->is_async()) {
    f_service_ <<
      indent() << "return;" << endl;
    indent_down();
    f_service_ <<
      indent() << "}" << endl;
    return;
  }

  // Serialize the request header
  if (binary_inline_) {
    f_service_ <<
      indent() << "$buff = '';" << endl <<
      indent() << "$buff .= pack('N', strlen('" << tfunction->get_name() << "'));" << endl <<
      indent() << "$buff .= '" << tfunction->get_name() << "';" << endl <<
      indent() << "$buff .= pack('cN', TMessageType::REPLY, $seqid);" << endl <<
      indent() << "$result->write($buff);" << endl <<
      indent() << "$output->write($buff);" << endl <<
      indent() << "$output->flush();" << endl;
  } else {
    f_service_ <<
      indent() << "$output->writeMessageBegin('" << tfunction->get_name() << "', TMessageType::REPLY, $seqid);" << endl <<
      indent() << "$result->write($output);" << endl <<
      indent() << "$output->getOutputTransport()->flush();" << endl;
  }

  // Close function
  indent_down();
  f_service_ <<
    indent() << "}" << endl;
}

/**
 * Generates helper functions for a service.
 *
 * @param tservice The service to generate a header definition for
 */
void t_php_generator::generate_service_helpers(t_service* tservice) {
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;

  f_service_ <<
    "// HELPER FUNCTIONS AND STRUCTURES" << endl << endl;

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    t_struct* ts = (*f_iter)->get_arglist();
    string name = ts->get_name();
    ts->set_name(service_name_ + "_" + name);
    generate_php_struct_definition(f_service_, ts, false);
    generate_php_function_helpers(*f_iter);
    ts->set_name(name);
  }
}

/**
 * Generates a struct and helpers for a function.
 *
 * @param tfunction The function
 */
void t_php_generator::generate_php_function_helpers(t_function* tfunction) {
  t_struct result(program_, service_name_ + "_" + tfunction->get_name() + "_result");
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

  generate_php_struct_definition(f_service_, &result, false);
}

/**
 * Generates a service interface definition.
 *
 * @param tservice The service to generate a header definition for
 */
void t_php_generator::generate_service_interface(t_service* tservice) {
  string extends = "";
  string extends_if = "";
  if (tservice->get_extends() != NULL) {
    extends = " extends " + tservice->get_extends()->get_name();
    extends_if = " extends " + tservice->get_extends()->get_name() + "If";
  }
  f_service_ <<
    "interface " << service_name_ << "If" << extends_if << " {" << endl;
  indent_up();
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter; 
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    indent(f_service_) <<
      "public function " << function_signature(*f_iter) << ";" << endl;
  }
  indent_down();
  f_service_ <<
    "}" << endl << endl;
}

/**
 * Generates a REST interface
 */
void t_php_generator::generate_service_rest(t_service* tservice) {
  string extends = "";
  string extends_if = "";
  if (tservice->get_extends() != NULL) {
    extends = " extends " + tservice->get_extends()->get_name();
    extends_if = " extends " + tservice->get_extends()->get_name() + "Rest";
  }
  f_service_ <<
    "class " << service_name_ << "Rest" << extends_if << " {" << endl;
  indent_up();
  f_service_ <<
    indent() << "var $impl_;" << endl <<
    endl <<
    indent() << "public function __construct($impl) {" << endl <<
    indent() << "  $this->impl_ = $impl;" << endl <<
    indent() << "}" << endl <<
    endl;

  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter; 
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    indent(f_service_) <<
      "public function " << (*f_iter)->get_name() << "($request) {" << endl;
    indent_up();
    const vector<t_field*>& args = (*f_iter)->get_arglist()->get_members();
    vector<t_field*>::const_iterator a_iter;
    for (a_iter = args.begin(); a_iter != args.end(); ++a_iter) {
      f_service_ <<
        indent() << "$" << (*a_iter)->get_name() << " = $request['" << (*a_iter)->get_name() << "'];" << endl;
      if ((*a_iter)->get_type()->is_list()) {
        f_service_ << 
          indent() << "$" << (*a_iter)->get_name() << " = explode(',', $" << (*a_iter)->get_name() << ");" << endl;
      }      
    }
    f_service_ <<
      indent() << "return $this->impl_->" << (*f_iter)->get_name() << "(" << argument_list((*f_iter)->get_arglist()) << ");" << endl;
    indent_down();
    indent(f_service_) <<
      "}" << endl <<
      endl;
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
  string extends = "";
  string extends_client = "";
  if (tservice->get_extends() != NULL) {
    extends = tservice->get_extends()->get_name();
    extends_client = " extends " + extends + "Client";
  }

  f_service_ <<
    "class " << service_name_ << "Client" << extends_client << " implements " << service_name_ << "If {" << endl;
  indent_up();

  // Private members
  if (extends.empty()) {
    f_service_ <<
      indent() << "protected $input_ = null;" << endl <<
      indent() << "protected $output_ = null;" << endl <<
      endl;
    f_service_ <<
      indent() << "protected $seqid_ = 0;" << endl <<
      endl;
  }

  // Constructor function
  f_service_ <<
    indent() << "public function __construct($input, $output=null) {" << endl;
  if (!extends.empty()) {
    f_service_ <<
      indent() << "  parent::__construct($input, $output);" << endl;
  } else {
    f_service_ <<
      indent() << "  $this->input_ = $input;" << endl <<
      indent() << "  $this->output_ = $output ? $output : $input;" << endl;
  }
  f_service_ <<
    indent() << "}" << endl << endl;   

  // Generate client method implementations
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::const_iterator f_iter;    
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    t_struct* arg_struct = (*f_iter)->get_arglist();
    const vector<t_field*>& fields = arg_struct->get_members();
    vector<t_field*>::const_iterator fld_iter;
    string funname = (*f_iter)->get_name();

    // Open function
    indent(f_service_) <<
      "public function " << function_signature(*f_iter) << endl;
    scope_up(f_service_);
      indent(f_service_) <<
        "$this->send_" << funname << "(";

      bool first = true;
      for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
        if (first) {
          first = false;
        } else {
          f_service_ << ", ";
        }
        f_service_ << "$" << (*fld_iter)->get_name();
      }
      f_service_ << ");" << endl;

      if (!(*f_iter)->is_async()) {
        f_service_ << indent();
        if (!(*f_iter)->get_returntype()->is_void()) {
          f_service_ << "return ";
        }
        f_service_ <<
          "$this->recv_" << funname << "();" << endl;
      }
    scope_down(f_service_);
    f_service_ << endl;

    indent(f_service_) <<
      "public function send_" << function_signature(*f_iter) << endl;
    scope_up(f_service_);  

      std::string argsname = service_name_ + "_" + (*f_iter)->get_name() + "_args";

      // Serialize the request header
      if (binary_inline_) {
        f_service_ <<
          indent() << "$buff = '';" << endl <<
          indent() << "$buff .= pack('N', strlen('" << funname << "'));" << endl <<
          indent() << "$buff .= '" << funname << "';" << endl <<
          indent() << "$buff .= pack('cN', TMessageType::CALL, $this->seqid_);" << endl;
      } else {
        f_service_ <<
          indent() << "$this->output_->writeMessageBegin('" << (*f_iter)->get_name() << "', TMessageType::CALL, $this->seqid_);" << endl;
      }
      
      f_service_ <<
        indent() << "$args = new " << argsname << "();" << endl;
      
      for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
        f_service_ <<
          indent() << "$args->" << (*fld_iter)->get_name() << " = $" << (*fld_iter)->get_name() << ";" << endl;
      }
           
      // Write to the stream
      if (binary_inline_) { 
        f_service_ <<
          indent() << "$args->write($buff);" << endl <<
          indent() << "$this->output_->write($buff);" << endl <<
          indent() << "$this->output_->flush();" << endl;
      } else {
        f_service_ <<
          indent() << "$args->write($this->output_);" << endl <<
          indent() << "$this->output_->writeMessageEnd();" << endl <<
          indent() << "$this->output_->getOutputTransport()->flush();" << endl;
      }   
      
    scope_down(f_service_);
      

    if (!(*f_iter)->is_async()) {
      std::string resultname = service_name_ + "_" + (*f_iter)->get_name() + "_result";
      t_struct noargs(program_);
      
      t_function recv_function((*f_iter)->get_returntype(),
                               string("recv_") + (*f_iter)->get_name(),
                               &noargs);
      // Open function
      f_service_ <<
        endl <<
        indent() << "public function " << function_signature(&recv_function) << endl;
      scope_up(f_service_);

      f_service_ <<
        indent() << "$rseqid = 0;" << endl <<
        indent() << "$fname = null;" << endl <<
        indent() << "$mtype = 0;" << endl <<
        endl;

      if (binary_inline_) {
        t_field ffname(g_type_string, "fname");
        t_field fmtype(g_type_byte, "mtype");
        t_field fseqid(g_type_i32, "rseqid");
        generate_deserialize_field(f_service_, &ffname, "", true);
        generate_deserialize_field(f_service_, &fmtype, "", true);
        generate_deserialize_field(f_service_, &fseqid, "", true);
      } else {
        f_service_ <<
          indent() << "$this->input_->readMessageBegin($fname, $mtype, $rseqid);" << endl;
      }

      // TODO(mcslee): Validate message reply here

      f_service_ <<
        indent() << "$result = new " << resultname << "();" << endl <<
        indent() << "$result->read($this->input_);" << endl;

      if (!binary_inline_) {
        f_service_ <<
          indent() << "$this->input_->readMessageEnd();" << endl <<
          endl;
      }

      // Careful, only return result if not a void function
      if (!(*f_iter)->get_returntype()->is_void()) {
        f_service_ <<
          indent() << "if ($result->success !== null) {" << endl <<
          indent() << "  return $result->success;" << endl <<
          indent() << "}" << endl;
      }

      t_struct* xs = (*f_iter)->get_xceptions();
      const std::vector<t_field*>& xceptions = xs->get_members();
      vector<t_field*>::const_iterator x_iter;
      for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
        f_service_ <<
          indent() << "if ($result->" << (*x_iter)->get_name() << " !== null) {" << endl <<
          indent() << "  throw $result->" << (*x_iter)->get_name() << ";" << endl <<
          indent() << "}" << endl;
      }

      // Careful, only return _result if not a void function
      if ((*f_iter)->get_returntype()->is_void()) {
        indent(f_service_) <<
          "return;" << endl;
      } else {
        f_service_ <<
          indent() << "throw new Exception(\"" << (*f_iter)->get_name() << " failed: unknown result\");" << endl;
      }     

    // Close function
    scope_down(f_service_);
    f_service_ << endl;

    }   
  }

  indent_down();
  f_service_ <<
    "}" << endl << endl;
}

/**
 * Deserializes a field of any type.
 */
void t_php_generator::generate_deserialize_field(ofstream &out,
                                                 t_field* tfield,
                                                 string prefix,
                                                 bool inclass) {
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

    if (binary_inline_) {
      std::string itrans = "$input";

      if (type->is_base_type()) {
        t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
        switch (tbase) {
        case t_base_type::TYPE_VOID:
          throw "compiler error: cannot serialize void field in a struct: " +
            name;
          break;
        case t_base_type::TYPE_STRING:
          out <<
            indent() << "$len = unpack('N', " << itrans << "->readAll(4));" << endl <<
            indent() << "$len = $len[1];" << endl <<
            indent() << "if ($len > 0x7fffffff) {" << endl <<
            indent() << "  $len = 0 - (($len - 1) ^ 0xffffffff);" << endl <<
            indent() << "}" << endl <<
            indent() << "$" << name << " = " << itrans << "->readAll($len);" << endl;
          break;
        case t_base_type::TYPE_BOOL:
          out <<
            indent() << "$" << name << " = unpack('c', " << itrans << "->readAll(1));" << endl <<
            indent() << "$" << name << " = (bool)$" << name << "[1];" << endl;
          break;
        case t_base_type::TYPE_BYTE:
          out <<
            indent() << "$" << name << " = unpack('c', " << itrans << "->readAll(1));" << endl <<
            indent() << "$" << name << " = $" << name << "[1];" << endl;
          break;
        case t_base_type::TYPE_I16:
          out <<
            indent() << "$val = unpack('n', " << itrans << "->readAll(2));" << endl <<
            indent() << "$val = $val[1];" << endl <<
            indent() << "if ($val > 0x7fff) {" << endl <<
            indent() << "  $val = 0 - (($val - 1) ^ 0xffff);" << endl <<
            indent() << "}" << endl <<
            indent() << "$" << name << " = $val;" << endl;
          break;
        case t_base_type::TYPE_I32:
          out <<
            indent() << "$val = unpack('N', " << itrans << "->readAll(4));" << endl <<
            indent() << "$val = $val[1];" << endl <<
            indent() << "if ($val > 0x7fffffff) {" << endl <<
            indent() << "  $val = 0 - (($val - 1) ^ 0xffffffff);" << endl <<
            indent() << "}" << endl <<
            indent() << "$" << name << " = $val;" << endl;
          break;
        case t_base_type::TYPE_I64:
          out <<
            indent() << "$arr = unpack('N2', " << itrans << "->readAll(8));" << endl <<
            indent() << "if ($arr[1] & 0x80000000) {" << endl <<
            indent() << "  $arr[1] = $arr[1] ^ 0xFFFFFFFF;" << endl <<
            indent() << "  $arr[2] = $arr[2] ^ 0xFFFFFFFF;" << endl <<
            indent() << "  $" << name << " = 0 - $arr[1]*4294967296 - $arr[2] - 1;" << endl <<
            indent() << "} else {" << endl <<
            indent() << "  $" << name << " = $arr[1]*4294967296 + $arr[2];" << endl <<
            indent() << "}" << endl;
          break;
        case t_base_type::TYPE_DOUBLE:
          out <<
            indent() << "$arr = unpack('d', strrev(" << itrans << "->readAll(8)));" << endl <<
            indent() << "$" << name << " = $arr[1];" << endl;
          break;
        default:
          throw "compiler error: no PHP name for base type " + tbase + tfield->get_name();
        }
      } else if (type->is_enum()) {
          out <<
            indent() << "$val = unpack('N', " << itrans << "->readAll(4));" << endl <<
            indent() << "$val = $val[1];" << endl <<
            indent() << "if ($val > 0x7fffffff) {" << endl <<
            indent() << "  $val = 0 - (($val - 1) ^ 0xffffffff);" << endl <<
            indent() << "}" << endl <<
            indent() << "$" << name << " = $val;" << endl;
      }
    } else {

      indent(out) <<
        "$xfer += $input->";
      
      if (type->is_base_type()) {
        t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
        switch (tbase) {
        case t_base_type::TYPE_VOID:
          throw "compiler error: cannot serialize void field in a struct: " +
            name;
          break;
        case t_base_type::TYPE_STRING:        
          out << "readString($" << name << ");";
          break;
        case t_base_type::TYPE_BOOL:
          out << "readBool($" << name << ");";
          break;
        case t_base_type::TYPE_BYTE:
          out << "readByte($" << name << ");";
          break;
        case t_base_type::TYPE_I16:
          out << "readI16($" << name << ");";
          break;
        case t_base_type::TYPE_I32:
          out << "readI32($" << name << ");";
          break;
        case t_base_type::TYPE_I64:
          out << "readI64($" << name << ");";
          break;
        case t_base_type::TYPE_DOUBLE:
          out << "readDouble($" << name << ");";
          break;
        default:
          throw "compiler error: no PHP name for base type " + tbase;
        }
      } else if (type->is_enum()) {
        out << "readI32($" << name << ");";
      }
      out << endl;
    }

  } else {
    printf("DO NOT KNOW HOW TO DESERIALIZE FIELD '%s' TYPE '%s'\n",
           tfield->get_name().c_str(), type->get_name().c_str());
  }  
}

/**
 * Generates an unserializer for a variable. This makes two key assumptions,
 * first that there is a const char* variable named data that points to the
 * buffer for deserialization, and that there is a variable protocol which
 * is a reference to a TProtocol serialization object.
 */
void t_php_generator::generate_deserialize_struct(ofstream &out,
                                                  t_struct* tstruct,
                                                  string prefix) {
  out <<
    indent() << "$" << prefix << " = new " << tstruct->get_name() << "();" << endl <<
    indent() << "$xfer += $" << prefix << "->read($input);" << endl;
}

void t_php_generator::generate_deserialize_container(ofstream &out,
                                                     t_type* ttype,
                                                     string prefix) {
  scope_up(out);
  
  string size = tmp("_size");
  string ktype = tmp("_ktype");
  string vtype = tmp("_vtype");
  string etype = tmp("_etype");
  
  t_field fsize(g_type_i32, size);
  t_field fktype(g_type_byte, ktype);
  t_field fvtype(g_type_byte, vtype);
  t_field fetype(g_type_byte, etype);

  out <<
    indent() << "$" << prefix << " = array();" << endl <<
    indent() << "$" << size << " = 0;" << endl;
  
  // Declare variables, read header
  if (ttype->is_map()) {
    out <<
      indent() << "$" << ktype << " = 0;" << endl <<
      indent() << "$" << vtype << " = 0;" << endl;
    if (binary_inline_) {
      generate_deserialize_field(out, &fktype);
      generate_deserialize_field(out, &fvtype);
      generate_deserialize_field(out, &fsize);
    } else {
      out <<
        indent() << "$xfer += $input->readMapBegin(" <<
        "$" << ktype << ", $" << vtype << ", $" << size << ");" << endl;
    }
  } else if (ttype->is_set()) {
    if (binary_inline_) {
      generate_deserialize_field(out, &fetype);
      generate_deserialize_field(out, &fsize);
    } else {
      out <<
        indent() << "$" << etype << " = 0;" << endl <<
        indent() << "$xfer += $input->readSetBegin(" <<
        "$" << etype << ", $" << size << ");" << endl;
    }
  } else if (ttype->is_list()) {
    if (binary_inline_) {
      generate_deserialize_field(out, &fetype);
      generate_deserialize_field(out, &fsize);
    } else {
      out <<
        indent() << "$" << etype << " = 0;" << endl <<
        indent() << "$xfer += $input->readListBegin(" <<
        "$" << etype << ", $" << size << ");" << endl;
    }
  }

  // For loop iterates over elements
  string i = tmp("_i");
  indent(out) <<
    "for ($" <<
    i << " = 0; $" << i << " < $" << size << "; ++$" << i << ")" << endl;
  
    scope_up(out);
    
    if (ttype->is_map()) {
      generate_deserialize_map_element(out, (t_map*)ttype, prefix);
    } else if (ttype->is_set()) {
      generate_deserialize_set_element(out, (t_set*)ttype, prefix);
    } else if (ttype->is_list()) {
      generate_deserialize_list_element(out, (t_list*)ttype, prefix);
    }
    
    scope_down(out);

  if (!binary_inline_) {
    // Read container end
    if (ttype->is_map()) {
      indent(out) << "$xfer += $input->readMapEnd();" << endl;
    } else if (ttype->is_set()) {
      indent(out) << "$xfer += $input->readSetEnd();" << endl;
    } else if (ttype->is_list()) {
      indent(out) << "$xfer += $input->readListEnd();" << endl;
    }
  }

  scope_down(out);
}


/**
 * Generates code to deserialize a map
 */
void t_php_generator::generate_deserialize_map_element(ofstream &out,
                                                       t_map* tmap,
                                                       string prefix) {
  string key = tmp("key");
  string val = tmp("val");
  t_field fkey(tmap->get_key_type(), key);
  t_field fval(tmap->get_val_type(), val);

  indent(out) <<
    declare_field(&fkey, true, true) << endl;
  indent(out) <<
    declare_field(&fval, true, true) << endl;

  generate_deserialize_field(out, &fkey);
  generate_deserialize_field(out, &fval);

  indent(out) <<
    "$" << prefix << "[$" << key << "] = $" << val << ";" << endl;
}

void t_php_generator::generate_deserialize_set_element(ofstream &out,
                                                       t_set* tset,
                                                       string prefix) {
  string elem = tmp("elem");
  t_field felem(tset->get_elem_type(), elem);

  indent(out) <<
    "$" << elem << " = null;" << endl;

  generate_deserialize_field(out, &felem);

  indent(out) <<
    "$" << prefix << " []= $" << elem << ";" << endl;
}

void t_php_generator::generate_deserialize_list_element(ofstream &out,
                                                        t_list* tlist,
                                                        string prefix) {
  string elem = tmp("elem");
  t_field felem(tlist->get_elem_type(), elem);

  indent(out) <<
    "$" << elem << " = null;" << endl;

  generate_deserialize_field(out, &felem);

  indent(out) <<
    "$" << prefix << " []= $" << elem << ";" << endl;
}


/**
 * Serializes a field of any type.
 *
 * @param tfield The field to serialize
 * @param prefix Name to prepend to field name
 */
void t_php_generator::generate_serialize_field(ofstream &out,
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

    if (binary_inline_) {
      if (type->is_base_type()) {
        t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
        switch (tbase) {
        case t_base_type::TYPE_VOID:
          throw
            "compiler error: cannot serialize void field in a struct: " + name;
          break;
        case t_base_type::TYPE_STRING:
          out <<
            indent() << "$output .= pack('N', strlen($" << name << "));" << endl <<
            indent() << "$output .= $" << name << ";" << endl;
          break;
        case t_base_type::TYPE_BOOL:
          out <<
            indent() << "$output .= pack('c', $" << name << " ? 1 : 0);" << endl;
          break;
        case t_base_type::TYPE_BYTE:
          out <<
            indent() << "$output .= pack('c', $" << name << ");" << endl;
          break;
        case t_base_type::TYPE_I16:
          out <<
            indent() << "$output .= pack('n', $" << name << ");" << endl;
          break;
        case t_base_type::TYPE_I32:
          out <<
            indent() << "$output .= pack('N', $" << name << ");" << endl;
          break;
        case t_base_type::TYPE_I64:
          out << 
            indent() << "$output .= pack('N2', $" << name << " >> 32, $" << name << " & 0xFFFFFFFF);" << endl;
          break;
        case t_base_type::TYPE_DOUBLE:
          out << 
            indent() << "$output .= strrev(pack('d', $" << name << "));" << endl;
          break;
        default:
          throw "compiler error: no PHP name for base type " + tbase;
        }
      } else if (type->is_enum()) {
        out <<
          indent() << "$output .= pack('N', $" << name << ");" << endl;
      }
    } else {

      indent(out) <<
        "$xfer += $output->";
      
      if (type->is_base_type()) {
        t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
        switch (tbase) {
        case t_base_type::TYPE_VOID:
          throw
            "compiler error: cannot serialize void field in a struct: " + name;
          break;
        case t_base_type::TYPE_STRING:
          out << "writeString($" << name << ");";
          break;
        case t_base_type::TYPE_BOOL:
          out << "writeBool($" << name << ");";
          break;
        case t_base_type::TYPE_BYTE:
          out << "writeByte($" << name << ");";
          break;
        case t_base_type::TYPE_I16:
          out << "writeI16($" << name << ");";
          break;
        case t_base_type::TYPE_I32:
          out << "writeI32($" << name << ");";
          break;
        case t_base_type::TYPE_I64:
          out << "writeI64($" << name << ");";
          break;
        case t_base_type::TYPE_DOUBLE:
          out << "writeDouble($" << name << ");";
          break;
        default:
          throw "compiler error: no PHP name for base type " + tbase;
        }
      } else if (type->is_enum()) {
        out << "writeI32($" << name << ");";
      }
      out << endl;
    }
  } else {
    printf("DO NOT KNOW HOW TO SERIALIZE FIELD '%s%s' TYPE '%s'\n",
           prefix.c_str(),
           tfield->get_name().c_str(),
           type->get_name().c_str());
  }
}

/**
 * Serializes all the members of a struct.
 *
 * @param tstruct The struct to serialize
 * @param prefix  String prefix to attach to all fields
 */
void t_php_generator::generate_serialize_struct(ofstream &out,
                                                t_struct* tstruct,
                                                string prefix) {
  indent(out) <<
    "$xfer += $" << prefix << "->write($output);" << endl; 
}

/**
 * Writes out a container
 */
void t_php_generator::generate_serialize_container(ofstream &out,
                                                   t_type* ttype,
                                                   string prefix) {
  scope_up(out);
  
  if (ttype->is_map()) {
    if (binary_inline_) {
      out <<
        indent() << "$output .= pack('c', " << type_to_enum(((t_map*)ttype)->get_key_type()) << ");" << endl <<
        indent() << "$output .= pack('c', " << type_to_enum(((t_map*)ttype)->get_val_type()) << ");" << endl <<
        indent() << "$output .= strrev(pack('l', count($" << prefix << ")));" << endl;
    } else {
      indent(out) <<
        "$output->writeMapBegin(" <<
        type_to_enum(((t_map*)ttype)->get_key_type()) << ", " <<
        type_to_enum(((t_map*)ttype)->get_val_type()) << ", " <<
        "count($" << prefix << "));" << endl;
    }
  } else if (ttype->is_set()) {
    if (binary_inline_) {
      out <<
        indent() << "$output .= pack('c', " << type_to_enum(((t_set*)ttype)->get_elem_type()) << ");" << endl <<
        indent() << "$output .= strrev(pack('l', count($" << prefix << ")));" << endl;

    } else {
      indent(out) <<
        "$output->writeSetBegin(" <<
        type_to_enum(((t_set*)ttype)->get_elem_type()) << ", " <<
        "count($" << prefix << "));" << endl;
    }
  } else if (ttype->is_list()) {
    if (binary_inline_) {
      out <<
        indent() << "$output .= pack('c', " << type_to_enum(((t_list*)ttype)->get_elem_type()) << ");" << endl <<
        indent() << "$output .= strrev(pack('l', count($" << prefix << ")));" << endl;

    } else {
      indent(out) <<
        "$output->writeListBegin(" <<
        type_to_enum(((t_list*)ttype)->get_elem_type()) << ", " <<
        "count($" << prefix << "));" << endl;
    }
  }

    scope_up(out);

    if (ttype->is_map()) {
      string kiter = tmp("kiter");
      string viter = tmp("viter");
      indent(out) << 
        "foreach ($" << prefix << " as " <<
        "$" << kiter << " => $" << viter << ")" << endl;
      scope_up(out);     
      generate_serialize_map_element(out, (t_map*)ttype, kiter, viter);
      scope_down(out);
    } else if (ttype->is_set()) {
      string iter = tmp("iter");
      indent(out) << 
        "foreach ($" << prefix << " as $" << iter << ")" << endl;
      scope_up(out);
      generate_serialize_set_element(out, (t_set*)ttype, iter);
      scope_down(out);
    } else if (ttype->is_list()) {
      string iter = tmp("iter");
      indent(out) << 
        "foreach ($" << prefix << " as $" << iter << ")" << endl;
      scope_up(out);
      generate_serialize_list_element(out, (t_list*)ttype, iter);
      scope_down(out);
    }
    
    scope_down(out);

  if (!binary_inline_) {
    if (ttype->is_map()) {
      indent(out) <<
        "$output->writeMapEnd();" << endl;
    } else if (ttype->is_set()) {
      indent(out) <<
        "$output->writeSetEnd();" << endl;
    } else if (ttype->is_list()) {
      indent(out) <<
        "$output->writeListEnd();" << endl;
    }
  }
 
  scope_down(out);  
}

/**
 * Serializes the members of a map.
 *
 */
void t_php_generator::generate_serialize_map_element(ofstream &out,
                                                     t_map* tmap,
                                                     string kiter,
                                                     string viter) {
  t_field kfield(tmap->get_key_type(), kiter);
  generate_serialize_field(out, &kfield, "");

  t_field vfield(tmap->get_val_type(), viter);
  generate_serialize_field(out, &vfield, "");
}

/**
 * Serializes the members of a set.
 */
void t_php_generator::generate_serialize_set_element(ofstream &out,
                                                     t_set* tset,
                                                     string iter) {
  t_field efield(tset->get_elem_type(), iter);
  generate_serialize_field(out, &efield, "");
}

/**
 * Serializes the members of a list.
 */
void t_php_generator::generate_serialize_list_element(ofstream &out,
                                                      t_list* tlist,
                                                      string iter) {
  t_field efield(tlist->get_elem_type(), iter);
  generate_serialize_field(out, &efield, "");
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
        result += " = 0.0";
        break;
      default:
        throw "compiler error: no PHP initializer for base type " + tbase;
      }
    } else if (type->is_enum()) {
      result += " = 0";
    } else if (type->is_container()) {
      result += " = array()";
    } else if (type->is_struct() || type->is_xception()) {
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
    case t_base_type::TYPE_BOOL:
      return "TType::BOOL";
    case t_base_type::TYPE_BYTE:
      return "TType::BYTE";
    case t_base_type::TYPE_I16:
      return "TType::I16";
    case t_base_type::TYPE_I32:
      return "TType::I32";
    case t_base_type::TYPE_I64:
      return "TType::I64";
    case t_base_type::TYPE_DOUBLE:
      return "TType::DOUBLE";
    }
  } else if (type->is_enum()) {
    return "TType::I32";
  } else if (type->is_struct() || type->is_xception()) {
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
