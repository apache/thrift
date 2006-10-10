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
void t_php_generator::close_generator(t_program *tprogram) {
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

  if (binary_inline_) {
    indent(out) <<
      "public function read($itrans) " << endl;
  } else {
    indent(out) <<
      "public function read($iprot, $itrans) " << endl;
  }
  scope_up(out);

  out <<
    indent() << "$xfer = 0;" << endl <<
    indent() << "$fname = null;" << endl <<
    indent() << "$ftype = 0;" << endl <<
    indent() << "$fid = 0;" << endl;

  // Declare stack tmp variables
  if (!binary_inline_) {
    indent(out) <<
      "$xfer += $iprot->readStructBegin($itrans, $fname);" << endl;   
  }

  // Loop over reading in fields
  indent(out) <<
    "while (true)" << endl;

    scope_up(out);
    
    // Read beginning field marker
    if (binary_inline_) {
      t_field fftype(g_program->get_byte_type(), "ftype");
      t_field ffid(g_program->get_i16_type(), "fid");
      generate_deserialize_field(out, &fftype);
      out <<
        indent() << "if ($ftype == TType::STOP) {" << endl <<
        indent() << "  break;" << endl <<
        indent() << "}" << endl;      
      generate_deserialize_field(out, &ffid);
    } else {
      indent(out) <<
        "$xfer += $iprot->readFieldBegin($itrans, $fname, $ftype, $fid);" << endl;
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
        indent(out) <<  "  $xfer += TProtocol::skipBinary($itrans, $ftype);" << endl;
      } else {
        indent(out) <<  "  $xfer += $iprot->skip($itrans, $ftype);" << endl;
      }
      indent(out) <<  "  break;" << endl;
      
      scope_down(out);
      
    if (!binary_inline_) {
      // Read field end marker
      indent(out) <<
        "$xfer += $iprot->readFieldEnd($itrans);" << endl;
    }
    
    scope_down(out);
    
  if (!binary_inline_) {
    indent(out) <<
      "$xfer += $iprot->readStructEnd($itrans);" << endl;
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
      "public function write(&$_output) {" << endl;
  } else {
    indent(out) <<
      "public function write($oprot, $otrans) {" << endl;
  }
  indent_up();
  
  indent(out) <<
    "$xfer = 0;" << endl;

  if (!binary_inline_) {
    indent(out) <<
      "$xfer += $oprot->writeStructBegin($otrans, '" << name << "');" << endl;
  }

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {   
    out <<
      indent() << "if ($this->" << (*f_iter)->get_name() << " !== null) {" << endl;
    indent_up();

    // Write field header
    if (binary_inline_) {
      out <<
        indent() << "$_output .= pack('c', " << type_to_enum((*f_iter)->get_type()) << ");" << endl <<
        indent() << "$_output .= pack('n', " << (*f_iter)->get_key() << ");" << endl;
    } else {
      indent(out) <<
        "$xfer += $oprot->writeFieldBegin($otrans, " <<
        "'" << (*f_iter)->get_name() << "', " <<
        type_to_enum((*f_iter)->get_type()) << ", " <<
        (*f_iter)->get_key() << ");" << endl;
    }

    // Write field contents
    generate_serialize_field(out, *f_iter, "this->");

    // Write field closer
    if (!binary_inline_) {
      indent(out) <<
        "$xfer += $oprot->writeFieldEnd($otrans);" << endl;
    }

    indent_down();
    indent(out) <<
      "}" << endl;
  }

  if (binary_inline_) {
    out <<
      indent() << "$_output .= pack('c', TType::STOP);" << endl;
  } else {
    out <<
      indent() << "$xfer += $oprot->writeFieldStop($otrans);" << endl <<
      indent() << "$xfer += $oprot->writeStructEnd($otrans);" << endl;
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
    "include_once $GLOBALS['THRIFT_ROOT'].'/packages/" << program_name_ << "/" << program_name_ << "_types.php';" << endl << endl;

  // Generate the three main parts of the service (well, two for now in PHP)
  generate_service_interface(tservice);
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

  // Generate the header portion
  f_service_ <<
    "class " << service_name_ << "Processor {" << endl;
  indent_up();

  f_service_ <<
    indent() << "private $_handler = null;" << endl;
  if (!binary_inline_) {
    f_service_ << 
      indent() << "private $_iprot = null;" << endl <<
      indent() << "private $_oprot = null;" << endl <<
      endl;
  }

  if (binary_inline_) {
    f_service_ <<
      indent() << "public function __construct($handler) {" << endl << 
      indent() << "  $this->_handler = $handler;" << endl <<
      indent() << "}" << endl <<
      endl;
  } else {
    f_service_ <<
      indent() << "public function __construct($handler, $iprot, $oprot=null) {" << endl << 
      indent() << "  $this->_handler = $handler;" << endl <<
      indent() << "  $this->_iprot = $iprot;" << endl <<
      indent() << "  $this->_oprot = $oprot ? $oprot : $iprot;" << endl <<
      indent() << "}" << endl <<
      endl;
  }

  // Generate the server implementation
  indent(f_service_) <<
    "public function process($itrans, $otrans) {" << endl;
  indent_up();

  f_service_ <<
    indent() << "$rseqid = 0;" << endl <<
    indent() << "$fname = null;" << endl <<
    indent() << "$mtype = 0;" << endl <<
    endl;

  if (binary_inline_) {
    t_field ffname(g_program->get_string_type(), "fname");
    t_field fmtype(g_program->get_byte_type(), "mtype");
    t_field fseqid(g_program->get_i32_type(), "rseqid");
    generate_deserialize_field(f_service_, &ffname, "", true);
    generate_deserialize_field(f_service_, &fmtype, "", true);
    generate_deserialize_field(f_service_, &fseqid, "", true);
  } else {
    f_service_ <<
      indent() << "$this->_iprot->readMessageBegin($itrans, $fname, $mtype, $rseqid);" << endl;
  }

  // TODO(mcslee): validate message 

  // HOT: check for method implementation
  f_service_ <<
    indent() << "$methodname = 'process_'.$fname;" << endl << 
    indent() << "if (!method_exists($this, $methodname)) {" << endl <<
    indent() << "  throw new Exception('Function '.$fname.' not implemented.');" << endl <<
    indent() << "}" << endl <<
    indent() << "$this->$methodname($rseqid, $itrans, $otrans);" << endl;
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
    "($seqid, $itrans, $otrans) {" << endl;
  indent_up();

  string argsname = service_name_ + "_" + tfunction->get_name() + "_args";
  string resultname = service_name_ + "_" + tfunction->get_name() + "_result";

  f_service_ <<
    indent() << "$__args = new " << argsname << "();" << endl;
  if (binary_inline_) {
    f_service_ <<
      indent() << "$__args->read($itrans);" << endl;
  } else {
    f_service_ <<
      indent() << "$__args->read($this->_iprot, $itrans);" << endl <<
      indent() << "$this->_iprot->readMessageEnd($itrans);" << endl;
  }

  t_struct* xs = tfunction->get_xceptions();
  const std::vector<t_field*>& xceptions = xs->get_members();
  vector<t_field*>::const_iterator x_iter;

  // Declare result for non async function
  if (!tfunction->is_async()) {
    f_service_ <<
      indent() << "$__result = new " << resultname << "();" << endl;
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
    f_service_ << "$__result->success = ";
  }
  f_service_ <<
    "$this->_handler->" << tfunction->get_name() << "(";
  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
    } else {
      f_service_ << ", ";
    }
    f_service_ << "$__args->" << (*f_iter)->get_name();
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
          indent() << "$__result->" << (*x_iter)->get_name() << " = $" << (*x_iter)->get_name() << ";" << endl;
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
    f_service_ << endl;
    return;
  }

  // Serialize the request header
  if (binary_inline_) {
    f_service_ <<
      indent() << "$_output = '';" << endl <<
      indent() << "$_output .= pack('N', strlen('" << tfunction->get_name() << "'));" << endl <<
      indent() << "$_output .= '" << tfunction->get_name() << "';" << endl <<
      indent() << "$_output .= pack('cN', TMessageType::REPLY, $seqid);" << endl;
  } else {
    f_service_ <<
      indent() << "$this->_oprot->writeMessageBegin($otrans, '" << tfunction->get_name() << "', TMessageType::REPLY, $seqid);" << endl <<
      indent() << "$__result->write($this->_oprot, $otrans);" << endl <<
      indent() << "$otrans->flush();" << endl;
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
  t_struct result(service_name_ + "_" + tfunction->get_name() + "_result");
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
    endl;

  if (!binary_inline_) {
    f_service_ <<
      indent() << "private $_iprot = null;" << endl <<
      indent() << "private $_oprot = null;" << endl <<
      endl;
  }

  f_service_ <<
    indent() << "private $_seqid = 0;" << endl <<
    endl;

  // Constructor function
  f_service_ <<
    indent() << "public function __construct() {" << endl <<
    indent() << "  $argv = func_get_args();" << endl <<
    indent() << "  $argc = count($argv);" << endl;

  if (binary_inline_) {
    f_service_ <<
      indent() << "  if ($argc == 1) {" << endl <<
      indent() << "    $this->_itrans = $this->_otrans = $argv[0];" << endl <<
      indent() << "  } else if ($argc == 2) {" << endl <<
      indent() << "    $this->_itrans = $argv[0];" << endl <<
      indent() << "    $this->_otrans = $argv[1];" << endl <<
      indent() << "  }" << endl;
  } else {
    f_service_ <<
      indent() << "  if ($argc == 2) {" << endl <<
      indent() << "    $this->_itrans = $this->_otrans = $argv[0];" << endl <<
      indent() << "    $this->_iprot = $this->_oprot = $argv[1];" << endl <<
      indent() << "  } else if ($argc == 4) {" << endl <<
      indent() << "    $this->_itrans = $argv[0];" << endl <<
      indent() << "    $this->_otrans = $argv[1];" << endl <<
      indent() << "    $this->_iprot = $argv[2];" << endl <<
      indent() << "    $this->_oprot = $argv[3];" << endl <<
      indent() << "  }" << endl;
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
          indent() << "$_output = '';" << endl <<
          indent() << "$_output .= pack('N', strlen('" << funname << "'));" << endl <<
          indent() << "$_output .= '" << funname << "';" << endl <<
          indent() << "$_output .= pack('cN', TMessageType::CALL, $this->seqid);" << endl;
      } else {
        f_service_ <<
          indent() << "$this->_oprot->writeMessageBegin($this->_otrans, '" << (*f_iter)->get_name() << "', TMessageType::CALL, $this->seqid);" << endl;
      }
      
      f_service_ <<
        indent() << "$__args = new " << argsname << "();" << endl;
      
      for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
        f_service_ <<
          indent() << "$__args->" << (*fld_iter)->get_name() << " = $" << (*fld_iter)->get_name() << ";" << endl;
      }
           
      // Write to the stream
      if (binary_inline_) { 
        f_service_ <<
          indent() << "$__args->write($_output, $__args);" << endl <<
          indent() << "$this->_otrans->write($_output);" << endl;
      } else {
        f_service_ <<
          indent() << "$__args->write($this->_oprot, $this->_otrans);" << endl <<
          indent() << "$this->_oprot->writeMessageEnd($this->_otrans);" << endl;
      }
      
      // Flush the request
      indent(f_service_) <<
        "$this->_otrans->flush();" << endl;
      
      
    scope_down(f_service_);
      

    if (!(*f_iter)->is_async()) {
      std::string resultname = service_name_ + "_" + (*f_iter)->get_name() + "_result";
      t_struct noargs;
      
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
        t_field ffname(g_program->get_string_type(), "fname");
        t_field fmtype(g_program->get_byte_type(), "mtype");
        t_field fseqid(g_program->get_i32_type(), "rseqid");
        generate_deserialize_field(f_service_, &ffname, "", true);
        generate_deserialize_field(f_service_, &fmtype, "", true);
        generate_deserialize_field(f_service_, &fseqid, "", true);
      } else {
        f_service_ <<
          indent() << "$this->_iprot->readMessageBegin($this->_itrans, $fname, $mtype, $rseqid);" << endl;
      }

      // TODO(mcslee): Validate message reply here

      f_service_ <<
        indent() << "$__result = new " << resultname << "();" << endl;
      if (binary_inline_) {
        f_service_ <<
          indent() << "$__result->read($this->_otrans);" << endl;
      } else {
        f_service_ <<
          indent() << "$__result->read($this->_iprot, $this->_otrans);" << endl;
      }

      if (!binary_inline_) {
        f_service_ <<
          indent() << "$this->_iprot->readMessageEnd($this->_itrans);" << endl <<
          endl;
      }

      // Careful, only return _result if not a void function
      if (!(*f_iter)->get_returntype()->is_void()) {
        f_service_ <<
          indent() << "if ($__result->success !== null) {" << endl <<
          indent() << "  return $__result->success;" << endl <<
          indent() << "}" << endl;
      }

      t_struct* xs = (*f_iter)->get_xceptions();
      const std::vector<t_field*>& xceptions = xs->get_members();
      vector<t_field*>::const_iterator x_iter;
      for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
        f_service_ <<
          indent() << "if ($__result->" << (*x_iter)->get_name() << " !== null) {" << endl <<
          indent() << "  throw $__result->" << (*x_iter)->get_name() << ";" << endl <<
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
                                (t_struct*)(tfield->get_type()),
                                 name);
  } else if (type->is_container()) {
    generate_deserialize_container(out, tfield->get_type(), name);
  } else if (type->is_base_type() || type->is_enum()) {

    if (binary_inline_) {
      std::string itrans = inclass ? "$this->_itrans" : "$itrans";

      if (type->is_base_type()) {
        t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
        switch (tbase) {
        case t_base_type::TYPE_VOID:
          throw "compiler error: cannot serialize void field in a struct: " +
            name;
          break;
        case t_base_type::TYPE_STRING:
          out <<
            indent() << "$_len = unpack('N', " << itrans << "->readAll(4));" << endl <<
            indent() << "$_len = $_len[1];" << endl <<
            indent() << "if ($_len > 0x7fffffff) {" << endl <<
            indent() << "  $_len = 0 - (($len - 1) ^ 0xffffffff);" << endl <<
            indent() << "}" << endl <<
            indent() << "$" << name << " = " << itrans << "->readAll($_len);" << endl;
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
            indent() << "$_val = unpack('n', " << itrans << "->readAll(2));" << endl <<
            indent() << "$_val = $_val[1];" << endl <<
            indent() << "if ($_val > 0x7fff) {" << endl <<
            indent() << "  $_val = 0 - (($_val - 1) ^ 0xffff);" << endl <<
            indent() << "}" << endl <<
            indent() << "$" << name << " = $_val;" << endl;
          break;
        case t_base_type::TYPE_I32:
          out <<
            indent() << "$_val = unpack('N', " << itrans << "->readAll(4));" << endl <<
            indent() << "$_val = $_val[1];" << endl <<
            indent() << "if ($_val > 0x7fffffff) {" << endl <<
            indent() << "  $_val = 0 - (($_val - 1) ^ 0xffffffff);" << endl <<
            indent() << "}" << endl <<
            indent() << "$" << name << " = $_val;" << endl;
          break;
        case t_base_type::TYPE_I64:
          out <<
            indent() << "$_arr = unpack('N2', " << itrans << "->readAll(8));" << endl <<
            indent() << "if ($_arr[1] & 0x80000000) {" << endl <<
            indent() << "  $_arr[1] = $_arr[1] ^ 0xFFFFFFFF;" << endl <<
            indent() << "  $_arr[2] = $_arr[2] ^ 0xFFFFFFFF;" << endl <<
            indent() << "  $" << name << " = 0 - $_arr[1]*4294967296 - $_arr[2] - 1;" << endl <<
            indent() << "} else {" << endl <<
            indent() << "  $" << name << " = $_arr[1]*4294967296 + $_arr[2];" << endl <<
            indent() << "}" << endl;
          break;
        case t_base_type::TYPE_DOUBLE:
          out <<
            indent() << "$_arr = unpack('d', strrev(" << itrans << "->readAll(8)));" << endl <<
            indent() << "$" << name << " = $_arr[1];" << endl;
          break;
        default:
          throw "compiler error: no PHP name for base type " + tbase + tfield->get_name();
        }
      } else if (type->is_enum()) {
          out <<
            indent() << "$_val = unpack('N', " << itrans << "->readAll(4));" << endl <<
            indent() << "$_val = $_val[1];" << endl <<
            indent() << "if ($_val > 0x7fffffff) {" << endl <<
            indent() << "  $_val = 0 - (($_val - 1) ^ 0xffffffff);" << endl <<
            indent() << "}" << endl <<
            indent() << "$" << name << " = $_val;" << endl;
      }
    } else {

      indent(out) <<
        "$xfer += $iprot->";
      
      if (type->is_base_type()) {
        t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
        switch (tbase) {
        case t_base_type::TYPE_VOID:
          throw "compiler error: cannot serialize void field in a struct: " +
            name;
          break;
        case t_base_type::TYPE_STRING:        
          out << "readString($itrans, $" << name << ");";
          break;
        case t_base_type::TYPE_BOOL:
          out << "readBool($itrans, $" << name << ");";
          break;
        case t_base_type::TYPE_BYTE:
          out << "readByte($itrans, $" << name << ");";
          break;
        case t_base_type::TYPE_I16:
          out << "readI16($itrans, $" << name << ");";
          break;
        case t_base_type::TYPE_I32:
          out << "readI32($itrans, $" << name << ");";
          break;
        case t_base_type::TYPE_I64:
          out << "readI64($itrans, $" << name << ");";
          break;
        case t_base_type::TYPE_DOUBLE:
          out << "readDouble($itrans, $" << name << ");";
          break;
        default:
          throw "compiler error: no PHP name for base type " + tbase;
        }
      } else if (type->is_enum()) {
        out << "readI32($itrans, $" << name << ");";
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
    indent() << "$" << prefix << " = new " << tstruct->get_name() << "();" << endl;
  if (binary_inline_) {
    out <<
      indent() << "$xfer += $" << prefix << "->read($itrans);" << endl;
  } else {
    out <<
      indent() << "$xfer += $" << prefix << "->read($iprot, $itrans);" << endl;
  }
}

void t_php_generator::generate_deserialize_container(ofstream &out,
                                                     t_type* ttype,
                                                     string prefix) {
  scope_up(out);
  
  string size = tmp("_size");
  string ktype = tmp("_ktype");
  string vtype = tmp("_vtype");
  string etype = tmp("_etype");
  
  t_field fsize(g_program->get_i32_type(), size);
  t_field fktype(g_program->get_byte_type(), ktype);
  t_field fvtype(g_program->get_byte_type(), vtype);
  t_field fetype(g_program->get_byte_type(), etype);

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
        indent() << "$xfer += $iprot->readMapBegin($itrans, " <<
        "$" << ktype << ", $" << vtype << ", $" << size << ");" << endl;
    }
  } else if (ttype->is_set()) {
    if (binary_inline_) {
      generate_deserialize_field(out, &fetype);
      generate_deserialize_field(out, &fsize);
    } else {
      out <<
        indent() << "$" << etype << " = 0;" << endl <<
        indent() << "$xfer += $iprot->readSetBegin($itrans, " <<
        "$" << etype << ", $" << size << ");" << endl;
    }
  } else if (ttype->is_list()) {
    if (binary_inline_) {
      generate_deserialize_field(out, &fetype);
      generate_deserialize_field(out, &fsize);
    } else {
      out <<
        indent() << "$" << etype << " = 0;" << endl <<
        indent() << "$xfer += $iprot->readListBegin($itrans, " <<
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
      indent(out) << "$xfer += $iprot->readMapEnd($itrans);" << endl;
    } else if (ttype->is_set()) {
      indent(out) << "$xfer += $iprot->readSetEnd($itrans);" << endl;
    } else if (ttype->is_list()) {
      indent(out) << "$xfer += $iprot->readListEnd($itrans);" << endl;
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
  string key = tmp("_key");
  string val = tmp("_val");
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
  string elem = tmp("_elem");
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
  string elem = tmp("_elem");
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
                              (t_struct*)(tfield->get_type()),
                              prefix + tfield->get_name());
  } else if (type->is_container()) {
    generate_serialize_container(out,
                                 tfield->get_type(),
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
            indent() << "$_output .= pack('N', strlen($" << name << "));" << endl <<
            indent() << "$_output .= $" << name << ";" << endl;
          break;
        case t_base_type::TYPE_BOOL:
          out <<
            indent() << "$_output .= pack('c', $" << name << " ? 1 : 0);" << endl;
          break;
        case t_base_type::TYPE_BYTE:
          out <<
            indent() << "$_output .= pack('c', $" << name << ");" << endl;
          break;
        case t_base_type::TYPE_I16:
          out <<
            indent() << "$_output .= pack('n', $" << name << ");" << endl;
          break;
        case t_base_type::TYPE_I32:
          out <<
            indent() << "$_output .= pack('N', $" << name << ");" << endl;
          break;
        case t_base_type::TYPE_I64:
          out << 
            indent() << "$_output .= pack('N2', $" << name << " >> 32, $" << name << " & 0xFFFFFFFF);" << endl;
          break;
        case t_base_type::TYPE_DOUBLE:
          out << 
            indent() << "$_output .= strrev(pack('d', $" << name << "));" << endl;
          break;
        default:
          throw "compiler error: no PHP name for base type " + tbase;
        }
      } else if (type->is_enum()) {
        out <<
          indent() << "$_output .= pack('N', $" << name << ");" << endl;
      }
    } else {

      indent(out) <<
        "$xfer += $oprot->";
      
      if (type->is_base_type()) {
        t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
        switch (tbase) {
        case t_base_type::TYPE_VOID:
          throw
            "compiler error: cannot serialize void field in a struct: " + name;
          break;
        case t_base_type::TYPE_STRING:
          out << "writeString($otrans, $" << name << ");";
          break;
        case t_base_type::TYPE_BOOL:
          out << "writeBool($otrans, $" << name << ");";
          break;
        case t_base_type::TYPE_BYTE:
          out << "writeByte($otrans, $" << name << ");";
          break;
        case t_base_type::TYPE_I16:
          out << "writeI16($otrans, $" << name << ");";
          break;
        case t_base_type::TYPE_I32:
          out << "writeI32($otrans, $" << name << ");";
          break;
        case t_base_type::TYPE_I64:
          out << "writeI64($otrans, $" << name << ");";
          break;
        case t_base_type::TYPE_DOUBLE:
          out << "writeDouble($otrans, $" << name << ");";
          break;
        default:
          throw "compiler error: no PHP name for base type " + tbase;
        }
      } else if (type->is_enum()) {
        out << "writeI32($otrans, $" << name << ");";
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
  if (binary_inline_) {
    indent(out) <<
      "$xfer += $" << prefix << "->write($_output);" << endl;
  } else {
    indent(out) <<
      "$xfer += $" << prefix << "->write($oprot, $otrans);" << endl;
  }
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
        indent() << "$_output .= pack('c', " << type_to_enum(((t_map*)ttype)->get_key_type()) << ");" << endl <<
        indent() << "$_output .= pack('c', " << type_to_enum(((t_map*)ttype)->get_val_type()) << ");" << endl <<
        indent() << "$_output .= strrev(pack('l', count($" << prefix << ")));" << endl;
    } else {
      indent(out) <<
        "$oprot->writeMapBegin($otrans, " <<
        type_to_enum(((t_map*)ttype)->get_key_type()) << ", " <<
        type_to_enum(((t_map*)ttype)->get_val_type()) << ", " <<
        "count($" << prefix << "));" << endl;
    }
  } else if (ttype->is_set()) {
    if (binary_inline_) {
      out <<
        indent() << "$_output .= pack('c', " << type_to_enum(((t_set*)ttype)->get_elem_type()) << ");" << endl <<
        indent() << "$_output .= strrev(pack('l', count($" << prefix << ")));" << endl;

    } else {
      indent(out) <<
        "$oprot->writeSetBegin($otrans, " <<
        type_to_enum(((t_set*)ttype)->get_elem_type()) << ", " <<
        "count($" << prefix << "));" << endl;
    }
  } else if (ttype->is_list()) {
    if (binary_inline_) {
      out <<
        indent() << "$_output .= pack('c', " << type_to_enum(((t_list*)ttype)->get_elem_type()) << ");" << endl <<
        indent() << "$_output .= strrev(pack('l', count($" << prefix << ")));" << endl;

    } else {
      indent(out) <<
        "$oprot->writeListBegin($otrans, " <<
        type_to_enum(((t_list*)ttype)->get_elem_type()) << ", " <<
        "count($" << prefix << "));" << endl;
    }
  }

    scope_up(out);

    if (ttype->is_map()) {
      string kiter = tmp("_kiter");
      string viter = tmp("_viter");
      indent(out) << 
        "foreach ($" << prefix << " as " <<
        "$" << kiter << " => $" << viter << ")" << endl;
      scope_up(out);     
      generate_serialize_map_element(out, (t_map*)ttype, kiter, viter);
      scope_down(out);
    } else if (ttype->is_set()) {
      string iter = tmp("_iter");
      indent(out) << 
        "foreach ($" << prefix << " as $" << iter << ")" << endl;
      scope_up(out);
      generate_serialize_set_element(out, (t_set*)ttype, iter);
      scope_down(out);
    } else if (ttype->is_list()) {
      string iter = tmp("_iter");
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
        "$oprot->writeMapEnd($otrans);" << endl;
    } else if (ttype->is_set()) {
      indent(out) <<
        "$oprot->writeSetEnd($otrans);" << endl;
    } else if (ttype->is_list()) {
      indent(out) <<
        "$oprot->writeListEnd($otrans);" << endl;
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
