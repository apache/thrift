/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

#include <string>
#include <fstream>
#include <iostream>
#include <vector>
#include <map>

#include <stdlib.h>
#include <sys/stat.h>
#include <sstream>
#include "t_generator.h"
#include "platform.h"
using namespace std;


/**
 * HTML code generator
 *
 * mostly copy/pasting/tweaking from mcslee's work.
 */
class t_html_generator : public t_generator {
 public:
  t_html_generator(
      t_program* program,
      const std::map<std::string, std::string>& parsed_options,
      const std::string& option_string)
    : t_generator(program)
  {
    (void) parsed_options;
    (void) option_string;  
    out_dir_base_ = "gen-html";
    escape_.clear();
    escape_['&']  = "&amp;";
    escape_['<']  = "&lt;";
    escape_['>']  = "&gt;";
    escape_['"']  = "&quot;";
    escape_['\''] = "&apos;";
  }

  void generate_program();
  void generate_program_toc();
  void generate_program_toc_row(t_program* tprog);
  void generate_program_toc_rows(t_program* tprog,
         std::vector<t_program*>& finished);
  void generate_index();
  void generate_css();

  /**
   * Program-level generation functions
   */

  void generate_typedef (t_typedef*  ttypedef);
  void generate_enum    (t_enum*     tenum);
  void generate_const   (t_const*    tconst);
  void generate_struct  (t_struct*   tstruct);
  void generate_service (t_service*  tservice);
  void generate_xception(t_struct*   txception);

  void print_doc        (t_doc* tdoc);
  int  print_type       (t_type* ttype);
  void print_const_value(t_const_value* tvalue);

  std::ofstream f_out_;
};

/**
 * Emits the Table of Contents links at the top of the module's page
 */
void t_html_generator::generate_program_toc() {
  f_out_ << "<table><tr><th>Module</th><th>Services</th>"
   << "<th>Data types</th><th>Constants</th></tr>" << endl;
  generate_program_toc_row(program_);
  f_out_ << "</table>" << endl;
}


/**
 * Recurses through from the provided program and generates a ToC row
 * for each discovered program exactly once by maintaining the list of
 * completed rows in 'finished'
 */
void t_html_generator::generate_program_toc_rows(t_program* tprog,
         std::vector<t_program*>& finished) {
  for (vector<t_program*>::iterator iter = finished.begin();
       iter != finished.end(); iter++) {
    if (tprog->get_path() == (*iter)->get_path()) {
      return;
    }
  }
  finished.push_back(tprog);
  generate_program_toc_row(tprog);
  vector<t_program*> includes = tprog->get_includes();
  for (vector<t_program*>::iterator iter = includes.begin();
       iter != includes.end(); iter++) {
    generate_program_toc_rows(*iter, finished);
  }
}

/**
 * Emits the Table of Contents links at the top of the module's page
 */
void t_html_generator::generate_program_toc_row(t_program* tprog) {
  string fname = tprog->get_name() + ".html";
  f_out_ << "<tr>" << endl << "<td>" << tprog->get_name() << "</td><td>";
  if (!tprog->get_services().empty()) {
    vector<t_service*> services = tprog->get_services();
    vector<t_service*>::iterator sv_iter;
    for (sv_iter = services.begin(); sv_iter != services.end(); ++sv_iter) {
      string name = get_service_name(*sv_iter);
      f_out_ << "<a href=\"" << fname << "#Svc_" << name << "\">" << name
        << "</a><br/>" << endl;
      f_out_ << "<ul>" << endl;
      map<string,string> fn_html;
      vector<t_function*> functions = (*sv_iter)->get_functions();
      vector<t_function*>::iterator fn_iter;
      for (fn_iter = functions.begin(); fn_iter != functions.end(); ++fn_iter) {
        string fn_name = (*fn_iter)->get_name();
        string html = "<li><a href=\"" + fname + "#Fn_" + name + "_" +
          fn_name + "\">" + fn_name + "</a></li>";
        fn_html.insert(pair<string,string>(fn_name, html));
      }
      for (map<string,string>::iterator html_iter = fn_html.begin();
        html_iter != fn_html.end(); html_iter++) {
        f_out_ << html_iter->second << endl;
      }
      f_out_ << "</ul>" << endl;
    }
  }
  f_out_ << "</td>" << endl << "<td>";
  map<string,string> data_types;
  if (!tprog->get_enums().empty()) {
    vector<t_enum*> enums = tprog->get_enums();
    vector<t_enum*>::iterator en_iter;
    for (en_iter = enums.begin(); en_iter != enums.end(); ++en_iter) {
      string name = (*en_iter)->get_name();
      // f_out_ << "<a href=\"" << fname << "#Enum_" << name << "\">" << name
      // <<  "</a><br/>" << endl;
      string html = "<a href=\"" + fname + "#Enum_" + name + "\">" + name +
        "</a>";
      data_types.insert(pair<string,string>(name, html));
    }
  }
  if (!tprog->get_typedefs().empty()) {
    vector<t_typedef*> typedefs = tprog->get_typedefs();
    vector<t_typedef*>::iterator td_iter;
    for (td_iter = typedefs.begin(); td_iter != typedefs.end(); ++td_iter) {
      string name = (*td_iter)->get_symbolic();
      // f_out_ << "<a href=\"" << fname << "#Typedef_" << name << "\">" << name
      // << "</a><br/>" << endl;
      string html = "<a href=\"" + fname + "#Typedef_" + name + "\">" + name +
        "</a>";
      data_types.insert(pair<string,string>(name, html));
    }
  }
  if (!tprog->get_objects().empty()) {
    vector<t_struct*> objects = tprog->get_objects();
    vector<t_struct*>::iterator o_iter;
    for (o_iter = objects.begin(); o_iter != objects.end(); ++o_iter) {
      string name = (*o_iter)->get_name();
      //f_out_ << "<a href=\"" << fname << "#Struct_" << name << "\">" << name
      //<< "</a><br/>" << endl;
      string html = "<a href=\"" + fname + "#Struct_" + name + "\">" + name +
        "</a>";
      data_types.insert(pair<string,string>(name, html));
    }
  }
  for (map<string,string>::iterator dt_iter = data_types.begin();
       dt_iter != data_types.end(); dt_iter++) {
    f_out_ << dt_iter->second << "<br/>" << endl;
  }
  f_out_ << "</td>" << endl << "<td><code>";
  if (!tprog->get_consts().empty()) {
    map<string,string> const_html;
    vector<t_const*> consts = tprog->get_consts();
    vector<t_const*>::iterator con_iter;
    for (con_iter = consts.begin(); con_iter != consts.end(); ++con_iter) {
      string name = (*con_iter)->get_name();
      string html ="<a href=\"" + fname + "#Const_" + name +
        "\">" + name + "</a>";
      const_html.insert(pair<string,string>(name, html));
    }
    for (map<string,string>::iterator con_iter = const_html.begin();
   con_iter != const_html.end(); con_iter++) {
      f_out_ << con_iter->second << "<br/>" << endl;
    }
  }
  f_out_ << "</code></td>" << endl << "</tr>";
}

/**
 * Prepares for file generation by opening up the necessary file output
 * stream.
 */
void t_html_generator::generate_program() {
  // Make output directory
  MKDIR(get_out_dir().c_str());
  string fname = get_out_dir() + program_->get_name() + ".html";
  f_out_.open(fname.c_str());
  f_out_ << "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"" << endl;
  f_out_ << "    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" << endl;
  f_out_ << "<html xmlns=\"http://www.w3.org/1999/xhtml\">" << endl;
  f_out_ << "<head>" << endl;
  f_out_ << "<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />" << endl;
  f_out_ << "<link href=\"style.css\" rel=\"stylesheet\" type=\"text/css\"/>"
   << endl;
  f_out_ << "<title>Thrift module: " << program_->get_name()
   << "</title></head><body>" << endl << "<h1>Thrift module: "
   << program_->get_name() << "</h1>" << endl;

  print_doc(program_);

  generate_program_toc();

  if (!program_->get_consts().empty()) {
    f_out_ << "<hr/><h2 id=\"Constants\">Constants</h2>" << endl;
    vector<t_const*> consts = program_->get_consts();
    f_out_ << "<table>";
    f_out_ << "<tr><th>Constant</th><th>Type</th><th>Value</th></tr>" << endl;
    generate_consts(consts);
    f_out_ << "</table>";
  }

  if (!program_->get_enums().empty()) {
    f_out_ << "<hr/><h2 id=\"Enumerations\">Enumerations</h2>" << endl;
    // Generate enums
    vector<t_enum*> enums = program_->get_enums();
    vector<t_enum*>::iterator en_iter;
    for (en_iter = enums.begin(); en_iter != enums.end(); ++en_iter) {
      generate_enum(*en_iter);
    }
  }

  if (!program_->get_typedefs().empty()) {
    f_out_ << "<hr/><h2 id=\"Typedefs\">Type declarations</h2>" << endl;
    // Generate typedefs
    vector<t_typedef*> typedefs = program_->get_typedefs();
    vector<t_typedef*>::iterator td_iter;
    for (td_iter = typedefs.begin(); td_iter != typedefs.end(); ++td_iter) {
      generate_typedef(*td_iter);
    }
  }

  if (!program_->get_objects().empty()) {
    f_out_ << "<hr/><h2 id=\"Structs\">Data structures</h2>" << endl;
    // Generate structs and exceptions in declared order
    vector<t_struct*> objects = program_->get_objects();
    vector<t_struct*>::iterator o_iter;
    for (o_iter = objects.begin(); o_iter != objects.end(); ++o_iter) {
      if ((*o_iter)->is_xception()) {
        generate_xception(*o_iter);
      } else {
        generate_struct(*o_iter);
      }
    }
  }

  if (!program_->get_services().empty()) {
    f_out_ << "<hr/><h2 id=\"Services\">Services</h2>" << endl;
    // Generate services
    vector<t_service*> services = program_->get_services();
    vector<t_service*>::iterator sv_iter;
    for (sv_iter = services.begin(); sv_iter != services.end(); ++sv_iter) {
      service_name_ = get_service_name(*sv_iter);
      generate_service(*sv_iter);
    }
  }

  f_out_ << "</body></html>" << endl;
  f_out_.close();

  generate_index();
  generate_css();
}

/**
 * Emits the index.html file for the recursive set of Thrift programs
 */
void t_html_generator::generate_index() {
  string index_fname = get_out_dir() + "index.html";
  f_out_.open(index_fname.c_str());
  f_out_ << "<html><head>" << endl;
  f_out_ << "<link href=\"style.css\" rel=\"stylesheet\" type=\"text/css\"/>"
   << endl;
  f_out_ << "<title>All Thrift declarations</title></head><body>"
   << endl << "<h1>All Thrift declarations</h1>" << endl;
  f_out_ << "<table><tr><th>Module</th><th>Services</th><th>Data types</th>"
   << "<th>Constants</th></tr>" << endl;
  vector<t_program*> programs;
  generate_program_toc_rows(program_, programs);
  f_out_ << "</table>" << endl;
  f_out_ << "</body></html>" << endl;
  f_out_.close();
}

void t_html_generator::generate_css() {
  string css_fname = get_out_dir() + "style.css";
  f_out_.open(css_fname.c_str());
  f_out_ << "/* Auto-generated CSS for generated Thrift docs */" << endl;
  f_out_ <<
    "body { font-family: Tahoma, sans-serif; }" << endl;
  f_out_ <<
    "pre { background-color: #dddddd; padding: 6px; }" << endl;
  f_out_ <<
    "h3,h4 { padding-top: 0px; margin-top: 0px; }" << endl;
  f_out_ <<
    "div.definition { border: 1px solid gray; margin: 10px; padding: 10px; }" << endl;
  f_out_ <<
    "div.extends { margin: -0.5em 0 1em 5em }" << endl;
  f_out_ <<
    "table { border: 1px solid grey; border-collapse: collapse; }" << endl;
  f_out_ <<
    "td { border: 1px solid grey; padding: 1px 6px; vertical-align: top; }" << endl;
  f_out_ <<
    "th { border: 1px solid black; background-color: #bbbbbb;" << endl <<
    "     text-align: left; padding: 1px 6px; }" << endl;
  f_out_.close();
}

/**
 * If the provided documentable object has documentation attached, this
 * will emit it to the output stream in HTML format.
 */
void t_html_generator::print_doc(t_doc* tdoc) {
  if (tdoc->has_doc()) {
    string doc = tdoc->get_doc();
    size_t index;
    while ((index = doc.find_first_of("\r\n")) != string::npos) {
      if (index == 0) {
  f_out_ << "<p/>" << endl;
      } else {
  f_out_ << doc.substr(0, index) << endl;
      }
      if (index + 1 < doc.size() && doc.at(index) != doc.at(index + 1) &&
    (doc.at(index + 1) == '\r' || doc.at(index + 1) == '\n')) {
  index++;
      }
      doc = doc.substr(index + 1);
    }
    f_out_ << doc << "<br/>";
  }
}

/**
 * Prints out the provided type in HTML
 */
int t_html_generator::print_type(t_type* ttype) {
  int len = 0;
  f_out_ << "<code>";
  if (ttype->is_container()) {
    if (ttype->is_list()) {
      f_out_ << "list&lt;";
      len = 6 + print_type(((t_list*)ttype)->get_elem_type());
      f_out_ << "&gt;";
    } else if (ttype->is_set()) {
      f_out_ << "set&lt;";
      len = 5 + print_type(((t_set*)ttype)->get_elem_type());
      f_out_ << "&gt;";
    } else if (ttype->is_map()) {
      f_out_ << "map&lt;";
      len = 5 + print_type(((t_map*)ttype)->get_key_type());
      f_out_ << ", ";
      len += print_type(((t_map*)ttype)->get_val_type());
      f_out_ << "&gt;";
    }
  } else if (ttype->is_base_type()) {
    f_out_ << (((t_base_type*)ttype)->is_binary() ? "binary" : ttype->get_name());
    len = ttype->get_name().size();
  } else {
    string prog_name = ttype->get_program()->get_name();
    string type_name = ttype->get_name();
    f_out_ << "<a href=\"" << prog_name << ".html#";
    if (ttype->is_typedef()) {
      f_out_ << "Typedef_";
    } else if (ttype->is_struct() || ttype->is_xception()) {
      f_out_ << "Struct_";
    } else if (ttype->is_enum()) {
      f_out_ << "Enum_";
    } else if (ttype->is_service()) {
      f_out_ << "Svc_";
    }
    f_out_ << type_name << "\">";
    len = type_name.size();
    if (ttype->get_program() != program_) {
      f_out_ << prog_name << ".";
      len += prog_name.size() + 1;
    }
    f_out_ << type_name << "</a>";
  }
  f_out_ << "</code>";
  return len;
}

/**
 * Prints out an HTML representation of the provided constant value
 */
void t_html_generator::print_const_value(t_const_value* tvalue) {
  bool first = true;
  switch (tvalue->get_type()) {
  case t_const_value::CV_INTEGER:
    f_out_ << tvalue->get_integer();
    break;
  case t_const_value::CV_DOUBLE:
    f_out_ << tvalue->get_double();
    break;
  case t_const_value::CV_STRING:
    f_out_ << '"' << get_escaped_string(tvalue) << '"';
    break;
  case t_const_value::CV_MAP:
    {
      f_out_ << "{ ";
      map<t_const_value*, t_const_value*> map_elems = tvalue->get_map();
      map<t_const_value*, t_const_value*>::iterator map_iter;
      for (map_iter = map_elems.begin(); map_iter != map_elems.end(); map_iter++) {
        if (!first) {
          f_out_ << ", ";
        }
        first = false;
        print_const_value(map_iter->first);
        f_out_ << " = ";
        print_const_value(map_iter->second);
      }
      f_out_ << " }";
    }
    break;
  case t_const_value::CV_LIST:
    {
      f_out_ << "{ ";
      vector<t_const_value*> list_elems = tvalue->get_list();;
      vector<t_const_value*>::iterator list_iter;
      for (list_iter = list_elems.begin(); list_iter != list_elems.end(); list_iter++) {
        if (!first) {
          f_out_ << ", ";
        }
        first = false;
        print_const_value(*list_iter);
      }
      f_out_ << " }";
    }
    break;
  default:
    f_out_ << "UNKNOWN";
    break;
  }
}

/**
 * Generates a typedef.
 *
 * @param ttypedef The type definition
 */
void t_html_generator::generate_typedef(t_typedef* ttypedef) {
  string name = ttypedef->get_name();
  f_out_ << "<div class=\"definition\">";
  f_out_ << "<h3 id=\"Typedef_" << name << "\">Typedef: " << name
   << "</h3>" << endl;
  f_out_ << "<p><strong>Base type:</strong>&nbsp;";
  print_type(ttypedef->get_type());
  f_out_ << "</p>" << endl;
  print_doc(ttypedef);
  f_out_ << "</div>" << endl;
}

/**
 * Generates code for an enumerated type.
 *
 * @param tenum The enumeration
 */
void t_html_generator::generate_enum(t_enum* tenum) {
  string name = tenum->get_name();
  f_out_ << "<div class=\"definition\">";
  f_out_ << "<h3 id=\"Enum_" << name << "\">Enumeration: " << name
   << "</h3>" << endl;
  print_doc(tenum);
  vector<t_enum_value*> values = tenum->get_constants();
  vector<t_enum_value*>::iterator val_iter;
  f_out_ << "<br/><table>" << endl;
  for (val_iter = values.begin(); val_iter != values.end(); ++val_iter) {
    f_out_ << "<tr><td><code>";
    f_out_ << (*val_iter)->get_name();
    f_out_ << "</code></td><td><code>";
    f_out_ << (*val_iter)->get_value();
    f_out_ << "</code></td></tr>" << endl;
  }
  f_out_ << "</table></div>" << endl;
}

/**
 * Generates a constant value
 */
void t_html_generator::generate_const(t_const* tconst) {
  string name = tconst->get_name();
  f_out_ << "<tr id=\"Const_" << name << "\"><td><code>" << name
   << "</code></td><td><code>";
  print_type(tconst->get_type());
  f_out_ << "</code></td><td><code>";
  print_const_value(tconst->get_value());
  f_out_ << "</code></td></tr>";
  if (tconst->has_doc()) {
    f_out_ << "<tr><td colspan=\"3\"><blockquote>";
    print_doc(tconst);
    f_out_ << "</blockquote></td></tr>";
  }
}

/**
 * Generates a struct definition for a thrift data type.
 *
 * @param tstruct The struct definition
 */
void t_html_generator::generate_struct(t_struct* tstruct) {
  string name = tstruct->get_name();
  f_out_ << "<div class=\"definition\">";
  f_out_ << "<h3 id=\"Struct_" << name << "\">";
  if (tstruct->is_xception()) {
    f_out_ << "Exception: ";
  } else {
    f_out_ << "Struct: ";
  }
  f_out_ << name << "</h3>" << endl;
  vector<t_field*> members = tstruct->get_members();
  vector<t_field*>::iterator mem_iter = members.begin();
  f_out_ << "<table>";
  f_out_ << "<tr><th>Key</th><th>Field</th><th>Type</th><th>Description</th><th>Requiredness</th><th>Default value</th></tr>"
    << endl;
  for ( ; mem_iter != members.end(); mem_iter++) {
    f_out_ << "<tr><td>" << (*mem_iter)->get_key() << "</td><td>";
    f_out_ << (*mem_iter)->get_name();
    f_out_ << "</td><td>";
    print_type((*mem_iter)->get_type());
    f_out_ << "</td><td>";
    f_out_ << (*mem_iter)->get_doc();
    f_out_ << "</td><td>";
    if ((*mem_iter)->get_req() == t_field::T_OPTIONAL) {
      f_out_ << "optional";
    } else if ((*mem_iter)->get_req() == t_field::T_REQUIRED) {
      f_out_ << "required";
    } else {
      f_out_ << "default";
    }
    f_out_ << "</td><td>";
    t_const_value* default_val = (*mem_iter)->get_value();
    if (default_val != NULL) {
      print_const_value(default_val);
    }
    f_out_ << "</td></tr>" << endl;
  }
  f_out_ << "</table><br/>";
  print_doc(tstruct);
  f_out_ << "</div>";
}

/**
 * Exceptions are special structs
 *
 * @param tstruct The struct definition
 */
void t_html_generator::generate_xception(t_struct* txception) {
  generate_struct(txception);
}

/**
 * Generates the HTML block for a Thrift service.
 *
 * @param tservice The service definition
 */
void t_html_generator::generate_service(t_service* tservice) {
  f_out_ << "<h3 id=\"Svc_" << service_name_ << "\">Service: "
    << service_name_ << "</h3>" << endl;

  if (tservice->get_extends()) {
    f_out_ << "<div class=\"extends\"><em>extends</em> ";
    print_type(tservice->get_extends());
    f_out_ << "</div>\n";
  }
  print_doc(tservice);
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator fn_iter = functions.begin();
  for ( ; fn_iter != functions.end(); fn_iter++) {
    string fn_name = (*fn_iter)->get_name();
    f_out_ << "<div class=\"definition\">";
    f_out_ << "<h4 id=\"Fn_" << service_name_ << "_" << fn_name
      << "\">Function: " << service_name_ << "." << fn_name
      << "</h4>" << endl;
    f_out_ << "<pre>";
    int offset = print_type((*fn_iter)->get_returntype());
    bool first = true;
    f_out_ << " " << fn_name << "(";
    offset += fn_name.size() + 2;
    vector<t_field*> args = (*fn_iter)->get_arglist()->get_members();
    vector<t_field*>::iterator arg_iter = args.begin();
    if (arg_iter != args.end()) {
      for ( ; arg_iter != args.end(); arg_iter++) {
        if (!first) {
          f_out_ << "," << endl;
          for (int i = 0; i < offset; ++i) {
            f_out_ << " ";
          }
        }
        first = false;
        print_type((*arg_iter)->get_type());
        f_out_ << " " << (*arg_iter)->get_name();
        if ((*arg_iter)->get_value() != NULL) {
          f_out_ << " = ";
          print_const_value((*arg_iter)->get_value());
        }
      }
    }
    f_out_ << ")" << endl;
    first = true;
    vector<t_field*> excepts = (*fn_iter)->get_xceptions()->get_members();
    vector<t_field*>::iterator ex_iter = excepts.begin();
    if (ex_iter != excepts.end()) {
      f_out_ << "    throws ";
      for ( ; ex_iter != excepts.end(); ex_iter++) {
        if (!first) {
          f_out_ << ", ";
        }
        first = false;
        print_type((*ex_iter)->get_type());
      }
      f_out_ << endl;
    }
    f_out_ << "</pre>";
    print_doc(*fn_iter);
    f_out_ << "</div>";
  }
}

THRIFT_REGISTER_GENERATOR(html, "HTML", "")

