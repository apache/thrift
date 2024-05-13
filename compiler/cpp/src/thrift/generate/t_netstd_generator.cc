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
 *
 * Contains some contributions under the Thrift Software License.
 * Please see doc/old-thrift-license.txt in the Thrift distribution for
 * details.
 */

#include <cassert>

#include <string>
#include <fstream>
#include <iostream>
#include <vector>
#include <cctype>

#include <stdlib.h>
#include <sys/stat.h>
#include <sstream>

#include "thrift/platform.h"
#include "thrift/generate/t_oop_generator.h"
#include "thrift/generate/t_netstd_generator.h"

using std::map;
using std::ostream;
using std::ostringstream;
using std::string;
using std::stringstream;
using std::vector;

//TODO: check for indentation
//TODO: Do we need seqId_ in generation?

t_netstd_generator::t_netstd_generator(t_program* program, const map<string, string>& parsed_options, const string& option_string)
    : t_oop_generator(program)
{
    (void)option_string;
    target_net_version = 0;
    suppress_deepcopy = false;
    add_async_postfix = false;
    use_pascal_case_properties = false;
    union_ = false;
    serialize_ = false;
    wcf_ = false;

    wcf_namespace_.clear();

    map<string, string>::const_iterator iter;

    for (iter = parsed_options.begin(); iter != parsed_options.end(); ++iter)
    {
        if (iter->first.compare("union") == 0) {
            union_ = true;
        }
        else if (iter->first.compare("serial") == 0) {
            serialize_ = true;
            wcf_namespace_ = iter->second; // since there can be only one namespace
        }
        else if (iter->first.compare("wcf") == 0) {
            wcf_ = true;
            wcf_namespace_ = iter->second;
        }
        else if (iter->first.compare("pascal") == 0) {
          use_pascal_case_properties = true;
        }
        else if (iter->first.compare("no_deepcopy") == 0) {
          suppress_deepcopy = true;
        }
        else if (iter->first.compare("net6") == 0) {
          target_net_version = 6;
        }
        else if (iter->first.compare("net8") == 0) {
          target_net_version = 8;
        }
        else if (iter->first.compare("async_postfix") == 0) {
          add_async_postfix = true;
        }
        else {
          throw "unknown option netstd:" + iter->first;
        }
    }

    out_dir_base_ = "gen-netstd";
}

static bool field_has_default(t_field* tfield) { return tfield->get_value() != nullptr; }

static bool field_is_required(t_field* tfield) { return tfield->get_req() == t_field::T_REQUIRED; }

static t_type* resolve_typedef(t_type* ttype)
{
    while (ttype->is_typedef())
    {
        ttype = static_cast<t_typedef*>(ttype)->get_type();
    }
    return ttype;
}


static bool type_can_be_null(t_type* ttype)
{
    ttype = resolve_typedef(ttype);

    return ttype->is_container() || ttype->is_struct() || ttype->is_xception() || ttype->is_string();
}

bool t_netstd_generator::is_wcf_enabled() const { return wcf_; }

bool t_netstd_generator::is_serialize_enabled() const { return serialize_; }

bool t_netstd_generator::is_union_enabled() const { return union_; }

void t_netstd_generator::init_generator()
{
    MKDIR(get_out_dir().c_str());

    namespace_name_ = program_->get_namespace("netstd");

    string dir = namespace_name_;
    string subdir = get_out_dir().c_str();
    string::size_type loc;

    while ((loc = dir.find(".")) != string::npos)
    {
        subdir = subdir + "/" + dir.substr(0, loc);
        MKDIR(subdir.c_str());
        dir = dir.substr(loc + 1);
    }
    if (dir.size() > 0)
    {
        subdir = subdir + "/" + dir;
        MKDIR(subdir.c_str());
    }

    namespace_dir_ = subdir;

    while (!member_mapping_scopes.empty())
    {
        cleanup_member_name_mapping(member_mapping_scopes.back().scope_member);
    }

    pverbose(".NET Standard options:\n");
    pverbose("- union ................ %s\n", (is_union_enabled() ? "ON" : "off"));
    pverbose("- serialize ............ %s\n", (is_serialize_enabled() ? "ON" : "off"));
    pverbose("- wcf .................. %s\n", (is_wcf_enabled() ? "ON" : "off"));
    pverbose("- pascal ............... %s\n", (use_pascal_case_properties ? "ON" : "off"));
    pverbose("- target NET version ... %d\n", target_net_version);
    pverbose("- no_deepcopy .......... %s\n", (suppress_deepcopy ? "ON" : "off"));
    pverbose("- async_postfix ........ %s\n", (add_async_postfix ? "ON" : "off"));
}

string t_netstd_generator::normalize_name(string name, bool is_arg_name)
{
    string tmp(name);
    transform(tmp.begin(), tmp.end(), tmp.begin(), static_cast<int(*)(int)>(tolower));

    // check for reserved argument names
    if( is_arg_name && (CANCELLATION_TOKEN_NAME == name))
    {
        name += "_";
    }

    // un-conflict keywords by prefixing with "@"
    if (netstd_keywords.find(tmp) != netstd_keywords.end())
    {
        return "@" + name;
    }

    // prevent CS8981 "The type name only contains lower-cased ascii characters"
    if( name.find_first_not_of("abcdefghijklmnopqrstuvwxyz") == std::string::npos)
    {
        return "@" + name;
    }

    // no changes necessary
    return name;
}


void t_netstd_generator::reset_indent() {
  while( indent_count() > 0) {
    indent_down();
  }
}


void t_netstd_generator::pragmas_and_directives(ostream& out)
{
    if( target_net_version >= 6) {
      out << "// Thrift code generated for net" << target_net_version << '\n';
      out << "#nullable enable                 // requires C# 8.0" << '\n';
    }

    // this one must be first
    out << "#pragma warning disable IDE0079  // remove unnecessary pragmas" << '\n';

    if( target_net_version >= 8) {
      out << "#pragma warning disable IDE0290  // use primary CTOR" << '\n';
    } else {
      out << "#pragma warning disable IDE0017  // object init can be simplified" << '\n';
      out << "#pragma warning disable IDE0028  // collection init can be simplified" << '\n';
    }
    out << "#pragma warning disable IDE1006  // parts of the code use IDL spelling" << '\n';
    out << "#pragma warning disable CA1822   // empty " << DEEP_COPY_METHOD_NAME << "() methods still non-static" << '\n';

    if( any_deprecations()) {
      out << "#pragma warning disable CS0618   // silence our own deprecation warnings" << '\n';
    }

    if( target_net_version < 6) {
        out << "#pragma warning disable IDE0083  // pattern matching \"that is not SomeType\" requires net5.0 but we still support earlier versions" << '\n';
    }
    out << '\n';
}


bool t_netstd_generator::any_deprecations()
{
  // enums
  vector<t_enum*> enums = program_->get_enums();
  vector<t_enum*>::iterator en_iter;
  for (en_iter = enums.begin(); en_iter != enums.end(); ++en_iter) {
    if( is_deprecated((*en_iter)->annotations_)) {
      return true;
    }

    // enum values
    vector<t_enum_value*> evals = (*en_iter)->get_constants();
    vector<t_enum_value*>::iterator ev_iter;
    for (ev_iter = evals.begin(); ev_iter != evals.end(); ++ev_iter) {
      if( is_deprecated((*ev_iter)->annotations_)) {
        return true;
      }
    }
  }

  // typedefs
  vector<t_typedef*> typedefs = program_->get_typedefs();
  vector<t_typedef*>::iterator td_iter;
  for (td_iter = typedefs.begin(); td_iter != typedefs.end(); ++td_iter) {
    if( is_deprecated((*td_iter)->annotations_)) {
        return true;
    }
  }

  // structs, exceptions, unions
  vector<t_struct*> objects = program_->get_objects();
  vector<t_struct*>::iterator o_iter;
  for (o_iter = objects.begin(); o_iter != objects.end(); ++o_iter) {
    if( is_deprecated((*o_iter)->annotations_)) {
      return true;
    }

    // struct members
    const vector<t_field*>& members = (*o_iter)->get_members();
    vector<t_field*>::const_iterator m_iter;
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      if( is_deprecated((*m_iter)->annotations_)) {
        return true;
      }
    }
  }

  /* not yet
  // constants
  vector<t_const*> consts = program_->get_consts();
  vector<t_const*>::iterator c_iter;
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    if( is_deprecated((*c_iter)->annotations_)) {
      return true;
    }
  }
  */

  // services
  vector<t_service*> services = program_->get_services();
  vector<t_service*>::iterator sv_iter;
  for (sv_iter = services.begin(); sv_iter != services.end(); ++sv_iter) {
    if( is_deprecated((*sv_iter)->annotations_)) {
      return true;
    }

    // service methods
    vector<t_function*> functions = (*sv_iter)->get_functions();
    vector<t_function*>::iterator f_iter;
    for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
      if( is_deprecated((*f_iter)->annotations_)) {
        return true;
      }
    }
  }

  return false;
}


void t_netstd_generator::start_netstd_namespace(ostream& out)
{
    if (!namespace_name_.empty())
    {
        out << "namespace " << namespace_name_ << '\n';
        scope_up(out);
    }
}

void t_netstd_generator::end_netstd_namespace(ostream& out)
{
    if (!namespace_name_.empty())
    {
        scope_down(out);
    }
}

string t_netstd_generator::netstd_type_usings() const
{
    string namespaces =
        "using System;\n"
        "using System.Collections;\n"
        "using System.Collections.Generic;\n"
        "using System.Text;\n"
        "using System.IO;\n"
        "using System.Linq;\n"
        "using System.Threading;\n"
        "using System.Threading.Tasks;\n"
        "using Microsoft.Extensions.Logging;\n"
        "using Thrift;\n"
        "using Thrift.Collections;\n";

    if (is_wcf_enabled())
    {
        namespaces += "using System.ServiceModel;\n";
        namespaces += "using System.Runtime.Serialization;\n";
    }

    return namespaces;
}

string t_netstd_generator::netstd_thrift_usings() const
{
    string namespaces =
        "using Thrift.Protocol;\n"
        "using Thrift.Protocol.Entities;\n"
        "using Thrift.Protocol.Utilities;\n"
        "using Thrift.Transport;\n"
        "using Thrift.Transport.Client;\n"
        "using Thrift.Transport.Server;\n"
        "using Thrift.Processor;\n";

    return namespaces;
}

void t_netstd_generator::close_generator()
{
    // right at the end, after everything else
    generate_extensions_file();
}

void t_netstd_generator::generate_typedef(t_typedef* ttypedef)
{
    (void)ttypedef;
}

void t_netstd_generator::generate_enum(t_enum* tenum)
{
    int ic = indent_count();
    string f_enum_name = namespace_dir_ + "/" + tenum->get_name() + ".cs";

    ofstream_with_content_based_conditional_update f_enum;
    f_enum.open(f_enum_name.c_str());

    generate_enum(f_enum, tenum);

    f_enum.close();
    indent_validate(ic, "generate_enum");
}

void t_netstd_generator::generate_enum(ostream& out, t_enum* tenum)
{
    reset_indent();
    out << autogen_comment();
    out << "using System;" << '\n' << '\n';  // needed for Obsolete() attribute

    pragmas_and_directives(out);
    start_netstd_namespace(out);
    generate_netstd_doc(out, tenum);

    generate_deprecation_attribute(out, tenum->annotations_);
    out << indent() << "public enum " << type_name(tenum,false) << '\n';
    scope_up(out);

    vector<t_enum_value*> constants = tenum->get_constants();
    vector<t_enum_value*>::iterator c_iter;

    for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter)
    {
        generate_netstd_doc(out, *c_iter);
        int value = (*c_iter)->get_value();
        generate_deprecation_attribute(out, (*c_iter)->annotations_);
        out << indent() << normalize_name((*c_iter)->get_name()) << " = " << value << "," << '\n';
    }

    scope_down(out);
    end_netstd_namespace(out);
}

void t_netstd_generator::generate_consts(vector<t_const*> consts)
{
    if (consts.empty())
    {
        return;
    }

    string f_consts_name = namespace_dir_ + '/' + program_name_ + ".Constants.cs";
    ofstream_with_content_based_conditional_update f_consts;
    f_consts.open(f_consts_name.c_str());

    generate_consts(f_consts, consts);

    f_consts.close();
}

void t_netstd_generator::generate_consts(ostream& out, vector<t_const*> consts)
{
    if (consts.empty())
    {
        return;
    }

    reset_indent();
    out << autogen_comment() << netstd_type_usings() << '\n' << '\n';

    pragmas_and_directives(out);
    start_netstd_namespace(out);

    out << indent() << "public static class " << make_valid_csharp_identifier(program_name_) << "Constants" << '\n';

    scope_up(out);

    vector<t_const*>::iterator c_iter;
    bool need_static_constructor = false;
    for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter)
    {
        generate_netstd_doc(out, *c_iter);
        if (print_const_value(out, normalize_name((*c_iter)->get_name()), (*c_iter)->get_type(), (*c_iter)->get_value(), false))
        {
            need_static_constructor = true;
        }
    }

    if (need_static_constructor)
    {
        print_const_constructor(out, consts);
    }

    scope_down(out);
    end_netstd_namespace(out);
}

void t_netstd_generator::print_const_def_value(ostream& out, string name, t_type* type, t_const_value* value)
{
    if (type->is_struct() || type->is_xception())
    {
        const vector<t_field*>& fields = static_cast<t_struct*>(type)->get_members();
        const map<t_const_value*, t_const_value*, t_const_value::value_compare>& val = value->get_map();
        vector<t_field*>::const_iterator f_iter;
        map<t_const_value*, t_const_value*, t_const_value::value_compare>::const_iterator v_iter;
        collect_extensions_types(static_cast<t_struct*>(type));
        prepare_member_name_mapping(static_cast<t_struct*>(type));

        for (v_iter = val.begin(); v_iter != val.end(); ++v_iter)
        {
            t_field* field = nullptr;

            for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
            {
                if ((*f_iter)->get_name() == v_iter->first->get_string())
                {
                    field = *f_iter;
                }
            }

            if (field == nullptr)
            {
                throw "type error: " + type->get_name() + " has no field " + v_iter->first->get_string();
            }

            t_type* field_type = field->get_type();

            string val = render_const_value(out, name, field_type, v_iter->second);
            out << indent() << name << "." << prop_name(field) << " = " << val << ";" << '\n';
        }

        cleanup_member_name_mapping(static_cast<t_struct*>(type));
    }
    else if (type->is_map())
    {
        t_type* ktype = static_cast<t_map*>(type)->get_key_type();
        t_type* vtype = static_cast<t_map*>(type)->get_val_type();
        const map<t_const_value*, t_const_value*, t_const_value::value_compare>& val = value->get_map();
        map<t_const_value*, t_const_value*, t_const_value::value_compare>::const_iterator v_iter;
        for (v_iter = val.begin(); v_iter != val.end(); ++v_iter)
        {
            string key = render_const_value(out, name, ktype, v_iter->first);
            string val = render_const_value(out, name, vtype, v_iter->second);
            out << indent() << name << "[" << key << "]" << " = " << val << ";" << '\n';
        }
    }
    else if (type->is_list() || type->is_set())
    {
        t_type* etype;
        if (type->is_list())
        {
            etype = static_cast<t_list*>(type)->get_elem_type();
        }
        else
        {
            etype = static_cast<t_set*>(type)->get_elem_type();
        }

        const vector<t_const_value*>& val = value->get_list();
        vector<t_const_value*>::const_iterator v_iter;
        for (v_iter = val.begin(); v_iter != val.end(); ++v_iter)
        {
            string val = render_const_value(out, name, etype, *v_iter);
            out << indent() << name << ".Add(" << val << ");" << '\n';
        }
    }
}

void t_netstd_generator::print_const_constructor(ostream& out, vector<t_const*> consts)
{
    out << indent() << "static " << make_valid_csharp_identifier(program_name_).c_str() << "Constants()" << '\n';
    scope_up(out);

    vector<t_const*>::iterator c_iter;
    for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter)
    {
        string name = (*c_iter)->get_name();
        t_type* type = (*c_iter)->get_type();
        t_const_value* value = (*c_iter)->get_value();

        print_const_def_value(out, name, type, value);
    }
    scope_down(out);
}

bool t_netstd_generator::print_const_value(ostream& out, string name, t_type* type, t_const_value* value, bool in_static, bool defval, bool needtype)
{
    out << indent();
    bool need_static_construction = !in_static;

    type = resolve_typedef( type);

    if (!defval || needtype)
    {
        if(in_static) {
          out << type_name(type) << " ";
        } else if(type->is_base_type()) {
          out << "public const " << type_name(type) << " ";
        } else  {
          out << "public static " << (target_net_version >= 6 ? "readonly " : "") << type_name(type) << " ";
        }
    }

    if (type->is_base_type())
    {
        string v2 = render_const_value(out, name, type, value);
        out << name << " = " << v2 << ";" << '\n';
        need_static_construction = false;
    }
    else if (type->is_enum())
    {
        out << name << " = " << type_name(type) << "." << value->get_identifier_name() << ";" << '\n';
        need_static_construction = false;
    }
    else if (type->is_struct() || type->is_xception())
    {
        if(target_net_version >= 6) {
          out << name << " = new();" << '\n';
        } else {
          out << name << " = new " << type_name(type) << "();" << '\n';
        }
    }
    else if (type->is_map() || type->is_list() || type->is_set())
    {
        if(target_net_version >= 8) {
          out << name << " = [];" << '\n';
        } else if(target_net_version >= 6) {
          out << name << " = new();" << '\n';
        } else {
          out << name << " = new " << type_name(type) << "();" << '\n';
        }
    }

    if (defval && !type->is_base_type() && !type->is_enum())
    {
        print_const_def_value(out, name, type, value);
    }

    return need_static_construction;
}

string t_netstd_generator::render_const_value(ostream& out, string name, t_type* type, t_const_value* value)
{
    (void)name;
    ostringstream render;

    if (type->is_base_type())
    {
        t_base_type::t_base tbase = static_cast<t_base_type*>(type)->get_base();
        switch (tbase)
        {
        case t_base_type::TYPE_STRING:
            if (type->is_binary()) {
                render << "System.Text.Encoding.UTF8.GetBytes(\"" << get_escaped_string(value) << "\")";
            } else {
                render << '"' << get_escaped_string(value) << '"';
            }
            break;
        case t_base_type::TYPE_UUID:
            render << "new System.Guid(\"" << get_escaped_string(value) << "\")";
            break;
        case t_base_type::TYPE_BOOL:
            render << ((value->get_integer() > 0) ? "true" : "false");
            break;
        case t_base_type::TYPE_I8:
        case t_base_type::TYPE_I16:
        case t_base_type::TYPE_I32:
        case t_base_type::TYPE_I64:
            render << value->get_integer();
            break;
        case t_base_type::TYPE_DOUBLE:
            if (value->get_type() == t_const_value::CV_INTEGER)
            {
                render << value->get_integer();
            }
            else
            {
                render << value->get_double();
            }
            break;
        default:
            throw "compiler error: no const of base type " + t_base_type::t_base_name(tbase);
        }
    }
    else if (type->is_enum())
    {
        render << type_name(type) << "." << value->get_identifier_name();
    }
    else
    {
        string t = normalize_name(tmp("tmp"));
        print_const_value(out, t, type, value, true, true, true);
        render << t;
    }

    return render.str();
}

void t_netstd_generator::collect_extensions_types(t_struct* tstruct)
{
    const vector<t_field*>& members = tstruct->get_members();
    vector<t_field*>::const_iterator m_iter;

    // make private members with public Properties
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter)
    {
        collect_extensions_types((*m_iter)->get_type());
    }
}

void t_netstd_generator::collect_extensions_types(t_type* ttype)
{
    ttype = resolve_typedef( ttype);
    string key = type_name(ttype);

    if (ttype->is_struct() || ttype->is_xception())
    {
        if( checked_extension_types.find(key) == checked_extension_types.end())
        {
            checked_extension_types[key] = ttype;    // prevent recursion

            t_struct* tstruct = static_cast<t_struct*>(ttype);
            collect_extensions_types(tstruct);
        }
        return;
    }

    if (ttype->is_container())
    {
        if( collected_extension_types.find(key) == collected_extension_types.end())
        {
            collected_extension_types[key] = ttype;   // prevent recursion

            if( ttype->is_map())
            {
                t_map* tmap = static_cast<t_map*>(ttype);
                collect_extensions_types(tmap->get_key_type());
                collect_extensions_types(tmap->get_val_type());
            }
            else if (ttype->is_set())
            {
                t_set* tset = static_cast<t_set*>(ttype);
                collect_extensions_types(tset->get_elem_type());
            }
            else if (ttype->is_list())
            {
                t_list* tlist = static_cast<t_list*>(ttype);
                collect_extensions_types(tlist->get_elem_type());
            }
            else
            {
                throw "compiler error: unhandled container type " + ttype->get_name();
            }
        }

        return;
    }
}

void t_netstd_generator::generate_extensions_file()
{
    if (collected_extension_types.empty())
    {
        return;
    }

    string f_exts_name = namespace_dir_ + '/' + program_name_ + ".Extensions.cs";
    ofstream_with_content_based_conditional_update f_exts;
    f_exts.open(f_exts_name.c_str());

    generate_extensions(f_exts, collected_extension_types);

    f_exts.close();
}

void t_netstd_generator::generate_extensions(ostream& out, map<string, t_type*> types)
{
    if (types.empty())
    {
        return;
    }

    reset_indent();
    out << autogen_comment() << netstd_type_usings()
        << "using Thrift.Protocol;" << '\n'
        << '\n' << '\n';

    pragmas_and_directives(out);
    start_netstd_namespace(out);

    out << indent() << "public static class " << make_valid_csharp_identifier(program_name_) << "Extensions" << '\n';
    scope_up(out);

    bool needs_typecast = false;
    std::map<string,t_type*>::const_iterator iter;
    for( iter = types.begin(); iter != types.end(); ++iter)
    {
        out << indent() << "public static bool Equals(this " << iter->first << " instance, object that)" << '\n';
        scope_up(out);
        if( target_net_version >= 6) {
            out << indent() << "if (that is not " << iter->first << " other) return false;" << '\n';
        } else {
            out << indent() << "if (!(that is " << iter->first << " other)) return false;" << '\n';
        }
        out << indent() << "if (ReferenceEquals(instance, other)) return true;" << '\n';
        out << '\n';
        out << indent() << "return TCollections.Equals(instance, other);" << '\n';
        scope_down(out);
        out << '\n' << '\n';

        out << indent() << "public static int GetHashCode(this " << iter->first << " instance)" << '\n';
        scope_up(out);
        out << indent() << "return TCollections.GetHashCode(instance);" << '\n';
        scope_down(out);
        out << '\n' << '\n';

        if(! suppress_deepcopy) {
            out << indent() << "public static " << iter->first << nullable_field_suffix(iter->second) << " " << DEEP_COPY_METHOD_NAME << "(this " << iter->first << nullable_field_suffix(iter->second) << " source)" << '\n';
            scope_up(out);
            out << indent() << "if (source == null)" << '\n';
            indent_up();
            out << indent() << "return null;" << '\n' << '\n';
            indent_down();

            string suffix("");
            string tmp_instance = tmp("tmp");
            out << indent() << "var " << tmp_instance << " = new " << iter->first << "(source.Count);" << '\n';
            if( iter->second->is_map())
            {
                t_map* tmap = static_cast<t_map*>(iter->second);
                string copy_key = get_deep_copy_method_call(tmap->get_key_type(), true, needs_typecast, suffix);
                string copy_val = get_deep_copy_method_call(tmap->get_val_type(), true, needs_typecast, suffix);
                bool null_key = type_can_be_null(tmap->get_key_type());
                bool null_val = type_can_be_null(tmap->get_val_type());

                out << indent() << "foreach (var pair in source)" << '\n';
                indent_up();
                if( target_net_version >= 6) {
                    out << indent() << tmp_instance << ".Add(pair.Key" << copy_key;
                    out << ", pair.Value" << copy_val;
                } else {
                    out << indent() << tmp_instance << ".Add(";
                    if( null_key) {
                        out << "(pair.Key != null) ? pair.Key" << copy_key << " : null";
                    } else {
                        out << "pair.Key" << copy_key;
                    }
                    out << ", ";
                    if( null_val) {
                        out << "(pair.Value != null) ? pair.Value" << copy_val << " : null";
                    } else {
                        out << "pair.Value" << copy_val;
                    }
                }
                out << ");" << '\n';
                indent_down();

            } else if( iter->second->is_set() || iter->second->is_list()) {
                string copy_elm;
                bool null_elm = false;
                if (iter->second->is_set())
                {
                    t_set* tset = static_cast<t_set*>(iter->second);
                    copy_elm = get_deep_copy_method_call(tset->get_elem_type(), true, needs_typecast, suffix);
                    null_elm = type_can_be_null(tset->get_elem_type());
                }
                else // list
                {
                    t_list* tlist = static_cast<t_list*>(iter->second);
                    copy_elm = get_deep_copy_method_call(tlist->get_elem_type(), true, needs_typecast, suffix);
                    null_elm = type_can_be_null(tlist->get_elem_type());
                }

                out << indent() << "foreach (var elem in source)" << '\n';
                indent_up();
                if( target_net_version >= 6) {
                    out << indent() << tmp_instance << ".Add(elem" << copy_elm;
                } else {
                    out << indent() << tmp_instance << ".Add(";
                    if( null_elm)
                    {
                        out << "(elem != null) ? elem" << copy_elm << " : null";
                    } else {
                        out << "elem" << copy_elm;
                    }
                }
                out << ");" << '\n';
                indent_down();
            }

            out << indent() << "return " << tmp_instance << ";" << '\n';
            scope_down(out);
            out << '\n' << '\n';
        }
    }


    scope_down(out);
    end_netstd_namespace(out);
}

void t_netstd_generator::generate_struct(t_struct* tstruct)
{
    collect_extensions_types(tstruct);

    if (is_union_enabled() && tstruct->is_union())
    {
        generate_netstd_union(tstruct);
    }
    else
    {
        generate_netstd_struct(tstruct, false);
    }
}

void t_netstd_generator::generate_xception(t_struct* txception)
{
    generate_netstd_struct(txception, true);
}

void t_netstd_generator::generate_netstd_struct(t_struct* tstruct, bool is_exception)
{
    int ic = indent_count();

    string f_struct_name = namespace_dir_ + "/" + (tstruct->get_name()) + ".cs";
    ofstream_with_content_based_conditional_update f_struct;

    f_struct.open(f_struct_name.c_str());

    reset_indent();
    f_struct << autogen_comment() << netstd_type_usings() << netstd_thrift_usings() << '\n' << '\n';

    pragmas_and_directives(f_struct);
    generate_netstd_struct_definition(f_struct, tstruct, is_exception);

    f_struct.close();

    indent_validate(ic, "generate_netstd_struct");
}

void t_netstd_generator::generate_netstd_struct_definition(ostream& out, t_struct* tstruct, bool is_exception, bool in_class, bool is_result)
{
    if (!in_class)
    {
        start_netstd_namespace(out);
    }

    out << '\n';

    generate_netstd_doc(out, tstruct);
    collect_extensions_types(tstruct);
    prepare_member_name_mapping(tstruct);

    if ((is_serialize_enabled() || is_wcf_enabled()) && !is_exception)
    {
        out << indent() << "[DataContract(Namespace=\"" << wcf_namespace_ << "\")]" << '\n';
    }

    bool is_final = tstruct->annotations_.find("final") != tstruct->annotations_.end();

    string sharp_struct_name = type_name(tstruct, false);

    generate_deprecation_attribute(out, tstruct->annotations_);
    out << indent() << "public " << (is_final ? "sealed " : "") << "partial class " << sharp_struct_name << " : ";

    if (is_exception)
    {
        out << "TException, ";
    }

    out << "TBase" << '\n'
        << indent() << "{" << '\n';
    indent_up();

    const vector<t_field*>& members = tstruct->get_members();
    vector<t_field*>::const_iterator m_iter;

    // make private members with public Properties
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter)
    {
        // if the field is required, then we use auto-properties
        if (!field_is_required((*m_iter)))
        {
            out << indent() << "private " << declare_field(*m_iter, false, true, "_") << '\n';
        }
    }
    out << '\n';

    bool has_non_required_fields = false;
    bool has_required_fields = false;
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter)
    {
        generate_netstd_doc(out, *m_iter);
        generate_property(out, *m_iter, true, true);
        bool is_required = field_is_required((*m_iter));
        if (is_required)
        {
            has_required_fields = true;
        }
        else
        {
            has_non_required_fields = true;
        }
    }

    bool generate_isset = has_non_required_fields;
    if (generate_isset)
    {
        out << '\n';
        if (is_serialize_enabled() || is_wcf_enabled())
        {
            out << indent() << "[DataMember(Order = 1)]" << '\n';
        }
        out << indent() << "public Isset __isset;" << '\n';
        if (is_serialize_enabled() || is_wcf_enabled())
        {
            out << indent() << "[DataContract]" << '\n';
        }

        out << indent() << "public struct Isset" << '\n'
            << indent() << "{" << '\n';
        indent_up();

        for (m_iter = members.begin(); m_iter != members.end(); ++m_iter)
        {
            bool is_required = field_is_required((*m_iter));
            // if it is required, don't need Isset for that variable
            // if it is not required, if it has a default value, we need to generate Isset
            if (!is_required)
            {
                if (is_serialize_enabled() || is_wcf_enabled())
                {
                    out << indent() << "[DataMember]" << '\n';
                }
                out << indent() << "public bool " << get_isset_name(normalize_name((*m_iter)->get_name())) << ";" << '\n';
            }
        }

        indent_down();
        out << indent() << "}" << '\n' << '\n';

        if (generate_isset && (is_serialize_enabled() || is_wcf_enabled()))
        {
            out << indent() << "#region XmlSerializer support" << '\n' << '\n';

            for (m_iter = members.begin(); m_iter != members.end(); ++m_iter)
            {
                bool is_required = field_is_required(*m_iter);
                // if it is required, don't need Isset for that variable
                // if it is not required, if it has a default value, we need to generate Isset
                if (!is_required)
                {
                    out << indent() << "public bool ShouldSerialize" << prop_name(*m_iter) << "()" << '\n'
                        << indent() << "{" << '\n';
                    indent_up();
                    out << indent() << "return __isset." << get_isset_name(normalize_name((*m_iter)->get_name())) << ";" << '\n';
                    indent_down();
                    out << indent() << "}" << '\n' << '\n';
                }
            }

            out << indent() << "#endregion XmlSerializer support" << '\n' << '\n';
        }
    }

    // We always want a default, no argument constructor for Reading
    out << indent() << "public " << sharp_struct_name << "()" << '\n'
        << indent() << "{" << '\n';
    indent_up();

    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter)
    {
        t_type* t = (*m_iter)->get_type();
        t = resolve_typedef(t);

        if ((*m_iter)->get_value() != nullptr)
        {
            if (field_is_required((*m_iter)))
            {
                print_const_value(out, "this." + normalize_name(prop_name(*m_iter)), t, (*m_iter)->get_value(), true, true);
            }
            else
            {
                print_const_value(out, "this._" + (*m_iter)->get_name(), t, (*m_iter)->get_value(), true, true);
                // Optionals with defaults are marked set
                out << indent() << "this.__isset." << get_isset_name(normalize_name((*m_iter)->get_name())) << " = true;" << '\n';
            }
        }
    }
    indent_down();
    out << indent() << "}" << '\n' << '\n';

    // if we have required fields, we add that CTOR too
    if (has_required_fields)
    {
        out << indent() << "public " << sharp_struct_name << "(";
        bool first = true;
        for (m_iter = members.begin(); m_iter != members.end(); ++m_iter)
        {
            if (field_is_required(*m_iter))
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                    out << ", ";
                }
                out << type_name((*m_iter)->get_type()) << nullable_field_suffix(*m_iter) << " " << normalize_name((*m_iter)->get_name());
            }
        }
        out << ") : this()" << '\n'
            << indent() << "{" << '\n';
        indent_up();

        for (m_iter = members.begin(); m_iter != members.end(); ++m_iter)
        {
            if (field_is_required(*m_iter))
            {
                out << indent() << "this." << prop_name(*m_iter) << " = " << normalize_name((*m_iter)->get_name()) << ";" << '\n';
            }
        }

        indent_down();
        out << indent() << "}" << '\n' << '\n';
    }

    // DeepCopy()
    generate_netstd_deepcopy_method(out, tstruct, sharp_struct_name);

    generate_netstd_struct_reader(out, tstruct);
    if (is_result)
    {
        generate_netstd_struct_result_writer(out, tstruct);
    }
    else
    {
        generate_netstd_struct_writer(out, tstruct);
    }
    generate_netstd_struct_equals(out, tstruct);
    generate_netstd_struct_hashcode(out, tstruct);
    generate_netstd_struct_tostring(out, tstruct);

    indent_down();
    out << indent() << "}" << '\n' << '\n';

    // generate a corresponding WCF fault to wrap the exception
    if ((is_serialize_enabled() || is_wcf_enabled()) && is_exception)
    {
        generate_netstd_wcffault(out, tstruct);
    }

    cleanup_member_name_mapping(tstruct);
    if (!in_class)
    {
        end_netstd_namespace(out);
    }
}


void t_netstd_generator::generate_netstd_wcffault(ostream& out, t_struct* tstruct)
{
    out << '\n';
    out << indent() << "[DataContract]" << '\n';

    bool is_final = tstruct->annotations_.find("final") != tstruct->annotations_.end();

    generate_deprecation_attribute(out, tstruct->annotations_);
    out << indent() << "public " << (is_final ? "sealed " : "") << "partial class " << type_name(tstruct,false) << "Fault" << '\n'
        << indent() << "{" << '\n';
    indent_up();

    const vector<t_field*>& members = tstruct->get_members();
    vector<t_field*>::const_iterator m_iter;

    // make private members with public Properties
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter)
    {
        // if the field is required, then we use auto-properties
        if (!field_is_required((*m_iter)))
        {
            out << indent() << "private " << declare_field(*m_iter, false, true, "_") << '\n';
        }
    }
    out << '\n';

    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter)
    {
        generate_property(out, *m_iter, true, false);
    }

    indent_down();
    out << indent() << "}" << '\n' << '\n';
}

void t_netstd_generator::generate_netstd_deepcopy_method(ostream& out, t_struct* tstruct, std::string sharp_struct_name)
{
    if( suppress_deepcopy) {
        return;  // feature disabled
    }

    const vector<t_field*>& members = tstruct->get_members();
    vector<t_field*>::const_iterator m_iter;

    out << indent() << "public " << sharp_struct_name << " " << DEEP_COPY_METHOD_NAME << "()" << '\n';
    out << indent() << "{" << '\n';
    indent_up();

    // return directly if there are only required fields
    string tmp_instance = tmp("tmp");
    out << indent() << "var " << tmp_instance << " = new " << sharp_struct_name << "()";
    bool inline_assignment = (target_net_version >= 6);
    if(inline_assignment) {
      out << '\n' << indent() << "{" << '\n';
      indent_up();
    } else {
      out << ";\n";
    }

    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
        bool needs_typecast = false;
        string suffix("");
        t_type* ttype = (*m_iter)->get_type();
        string copy_op = get_deep_copy_method_call(ttype, true, needs_typecast, suffix);

        bool is_required = field_is_required(*m_iter);
        bool null_allowed = type_can_be_null((*m_iter)->get_type());

        if(inline_assignment) {
          if( null_allowed || (!is_required)) {  // = has isset
            indent_down();
            out << indent() << "};" << '\n';
            inline_assignment = false;
          }
        }

        generate_null_check_begin( out, *m_iter);

        out << indent();
        if(!inline_assignment) {
          out << tmp_instance << ".";
        }
        out << prop_name(*m_iter) << " = ";
        if( needs_typecast) {
            out << "(" << type_name(ttype) << ")";
        }
        out << "this." << prop_name(*m_iter) << copy_op;
        out << (inline_assignment ? "," : ";") << '\n';

        generate_null_check_end( out, *m_iter);
        if( !is_required) {
            out << indent();
            if(!inline_assignment) {
              out << tmp_instance << ".";
            }
            out << "__isset." << get_isset_name(normalize_name((*m_iter)->get_name()));
            out << " = this.__isset." << get_isset_name(normalize_name((*m_iter)->get_name()));
            out << (inline_assignment ? "," : ";") << '\n';
        }
    }

    if(inline_assignment) {
      indent_down();
      out << indent() << "};" << '\n';
    }

    out << indent() << "return " << tmp_instance << ";" << '\n';

    indent_down();
    out << indent() << "}" << '\n' << '\n';
}

void t_netstd_generator::generate_netstd_struct_reader(ostream& out, t_struct* tstruct)
{
    out << indent() << "public async global::System.Threading.Tasks.Task ReadAsync(TProtocol iprot, CancellationToken " << CANCELLATION_TOKEN_NAME << ")" << '\n'
        << indent() << "{" << '\n';
    indent_up();
    out << indent() << "iprot.IncrementRecursionDepth();" << '\n'
        << indent() << "try" << '\n'
        << indent() << "{" << '\n';
    indent_up();

    const vector<t_field*>& fields = tstruct->get_members();
    vector<t_field*>::const_iterator f_iter;

    // Required variables aren't in __isset, so we need tmp vars to check them
    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
    {
        if (field_is_required(*f_iter))
        {
            out << indent() << "bool isset_" << (*f_iter)->get_name() << " = false;" << '\n';
        }
    }

    out << indent() << "TField field;" << '\n'
        << indent() << "await iprot.ReadStructBeginAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n'
        << indent() << "while (true)" << '\n'
        << indent() << "{" << '\n';
    indent_up();
    out << indent() << "field = await iprot.ReadFieldBeginAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n'
        << indent() << "if (field.Type == TType.Stop)" << '\n'
        << indent() << "{" << '\n';
    indent_up();
    out << indent() << "break;" << '\n';
    indent_down();
    out << indent() << "}" << '\n' << '\n'
        << indent() << "switch (field.ID)" << '\n'
        << indent() << "{" << '\n';
    indent_up();

    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
    {
        bool is_required = field_is_required(*f_iter);
        out << indent() << "case " << (*f_iter)->get_key() << ":" << '\n';
        indent_up();
        out << indent() << "if (field.Type == " << type_to_enum((*f_iter)->get_type()) << ")" << '\n'
            << indent() << "{" << '\n';
        indent_up();

        generate_deserialize_field(out, *f_iter);
        if (is_required)
        {
            out << indent() << "isset_" << (*f_iter)->get_name() << " = true;" << '\n';
        }

        indent_down();
        out << indent() << "}" << '\n'
            << indent() << "else" << '\n'
            << indent() << "{" << '\n';
        indent_up();
        out << indent() << "await TProtocolUtil.SkipAsync(iprot, field.Type, " << CANCELLATION_TOKEN_NAME << ");" << '\n';
        indent_down();
        out << indent() << "}" << '\n'
            << indent() << "break;" << '\n';
        indent_down();
    }

    out << indent() << "default: " << '\n';
    indent_up();
    out << indent() << "await TProtocolUtil.SkipAsync(iprot, field.Type, " << CANCELLATION_TOKEN_NAME << ");" << '\n'
        << indent() << "break;" << '\n';
    indent_down();
    indent_down();
    out << indent() << "}" << '\n'
        << '\n'
        << indent() << "await iprot.ReadFieldEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';
    indent_down();
    out << indent() << "}" << '\n'
        << '\n'
        << indent() << "await iprot.ReadStructEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';

    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
    {
        if (field_is_required((*f_iter)))
        {
            out << indent() << "if (!isset_" << (*f_iter)->get_name() << ")" << '\n'
                << indent() << "{" << '\n';
            indent_up();
            out << indent() << "throw new TProtocolException(TProtocolException.INVALID_DATA);" << '\n';
            indent_down();
            out << indent() << "}" << '\n';
        }
    }

    indent_down();
    out << indent() << "}" << '\n';
    out << indent() << "finally" << '\n'
        << indent() << "{" << '\n';
    indent_up();
    out << indent() << "iprot.DecrementRecursionDepth();" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
    indent_down();
    out << indent() << "}" << '\n' << '\n';
}


void t_netstd_generator::generate_null_check_begin(ostream& out, t_field* tfield) {
    bool is_required = field_is_required(tfield);
    bool null_allowed = type_can_be_null(tfield->get_type());

    if( null_allowed || (!is_required)) {
        bool first = true;
        out << indent() << "if(";

        if( null_allowed) {
            out << "(" << prop_name(tfield) << " != null)";
            first = false;
        }

        if( !is_required) {
            if( !first) {
                out << " && ";
            }
            out << "__isset." << get_isset_name(normalize_name(tfield->get_name()));
        }

        out << ")" << '\n'
            << indent() << "{" << '\n';
        indent_up();
    }
}


void t_netstd_generator::generate_null_check_end(ostream& out, t_field* tfield) {
    bool is_required = field_is_required(tfield);
    bool null_allowed = type_can_be_null(tfield->get_type());

    if( null_allowed || (!is_required)) {
        indent_down();
        out << indent() << "}" << '\n';
    }
}

void t_netstd_generator::generate_netstd_struct_writer(ostream& out, t_struct* tstruct)
{
    out << indent() << "public async global::System.Threading.Tasks.Task WriteAsync(TProtocol oprot, CancellationToken " << CANCELLATION_TOKEN_NAME << ")" << '\n'
        << indent() << "{" << '\n';
    indent_up();

    out << indent() << "oprot.IncrementRecursionDepth();" << '\n'
        << indent() << "try" << '\n'
        << indent() << "{" << '\n';
    indent_up();

    string name = tstruct->get_name();
    const vector<t_field*>& fields = tstruct->get_sorted_members();
    vector<t_field*>::const_iterator f_iter;

    string tmpvar = tmp("tmp");
    out << indent() << "var " << tmpvar << " = new TStruct(\"" << name << "\");" << '\n'
        << indent() << "await oprot.WriteStructBeginAsync(" << tmpvar << ", " << CANCELLATION_TOKEN_NAME << ");" << '\n';

    if (fields.size() > 0)
    {
        tmpvar = tmp("tmp");
        if(target_net_version >= 8) {
          out << indent() << "#pragma warning disable IDE0017  // simplified init" << '\n';
        }
        out << indent() << "var " << tmpvar << " = new TField();" << '\n';
        for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
        {
            generate_null_check_begin( out, *f_iter);
            out << indent() << tmpvar << ".Name = \"" << (*f_iter)->get_name() << "\";" << '\n'
                << indent() << tmpvar << ".Type = " << type_to_enum((*f_iter)->get_type()) << ";" << '\n'
                << indent() << tmpvar << ".ID = " << (*f_iter)->get_key() << ";" << '\n'
                << indent() << "await oprot.WriteFieldBeginAsync(" << tmpvar << ", " << CANCELLATION_TOKEN_NAME << ");" << '\n';

            generate_serialize_field(out, *f_iter);

            out << indent() << "await oprot.WriteFieldEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';
            generate_null_check_end(out, *f_iter);
        }
        if(target_net_version >= 8) {
          out << indent() << "#pragma warning restore IDE0017  // simplified init" << '\n';
        }
    }

    out << indent() << "await oprot.WriteFieldStopAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n'
        << indent() << "await oprot.WriteStructEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';
    indent_down();
    out << indent() << "}" << '\n'
        << indent() << "finally" << '\n'
        << indent() << "{" << '\n';
    indent_up();
    out << indent() << "oprot.DecrementRecursionDepth();" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
    indent_down();
    out << indent() << "}" << '\n' << '\n';
}

void t_netstd_generator::generate_netstd_struct_result_writer(ostream& out, t_struct* tstruct)
{
    out << indent() << "public async global::System.Threading.Tasks.Task WriteAsync(TProtocol oprot, CancellationToken " << CANCELLATION_TOKEN_NAME << ")" << '\n'
        << indent() << "{" << '\n';
    indent_up();

    out << indent() << "oprot.IncrementRecursionDepth();" << '\n'
        << indent() << "try" << '\n'
        << indent() << "{" << '\n';
    indent_up();

    string name = tstruct->get_name();
    const vector<t_field*>& fields = tstruct->get_sorted_members();
    vector<t_field*>::const_iterator f_iter;

    string tmpvar = tmp("tmp");
    out << indent() << "var " << tmpvar << " = new TStruct(\"" << name << "\");" << '\n'
        << indent() << "await oprot.WriteStructBeginAsync(" << tmpvar << ", " << CANCELLATION_TOKEN_NAME << ");" << '\n';

    if (fields.size() > 0)
    {
        tmpvar = tmp("tmp");
        if(target_net_version >= 8) {
          out << indent() << "#pragma warning disable IDE0017  // simplified init" << '\n';
        }
        out << indent() << "var " << tmpvar << " = new TField();" << '\n';
        bool first = true;
        for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
        {
            if (first)
            {
                first = false;
                out << '\n' << indent() << "if";
            }
            else
            {
                out << indent() << "else if";
            }

            out << "(this.__isset." << get_isset_name(normalize_name((*f_iter)->get_name())) << ")" << '\n'
                << indent() << "{" << '\n';
            indent_up();

            bool null_allowed = type_can_be_null((*f_iter)->get_type());
            if (null_allowed)
            {
                out << indent() << "if (" << prop_name(*f_iter) << " != null)" << '\n'
                    << indent() << "{" << '\n';
                indent_up();
            }

            out << indent() << tmpvar << ".Name = \"" << prop_name(*f_iter) << "\";" << '\n'
                << indent() << tmpvar << ".Type = " << type_to_enum((*f_iter)->get_type()) << ";" << '\n'
                << indent() << tmpvar << ".ID = " << (*f_iter)->get_key() << ";" << '\n'
                << indent() << "await oprot.WriteFieldBeginAsync(" << tmpvar << ", " << CANCELLATION_TOKEN_NAME << ");" << '\n';

            generate_serialize_field(out, *f_iter);

            out << indent() << "await oprot.WriteFieldEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';

            if (null_allowed)
            {
                indent_down();
                out << indent() << "}" << '\n';
            }

            indent_down();
            out << indent() << "}" << '\n';
        }
        if(target_net_version >= 8) {
          out << indent() << "#pragma warning restore IDE0017  // simplified init" << '\n';
        }
    }

    out << indent() << "await oprot.WriteFieldStopAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n'
        << indent() << "await oprot.WriteStructEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';
    indent_down();
    out << indent() << "}" << '\n'
        << indent() << "finally" << '\n'
        << indent() << "{" << '\n';
    indent_up();
    out << indent() << "oprot.DecrementRecursionDepth();" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
    indent_down();
    out << indent() << "}" << '\n' << '\n';
}

void t_netstd_generator::generate_netstd_struct_tostring(ostream& out, t_struct* tstruct)
{
    string tmpvar = tmp("tmp");
    out << indent() << "public override string ToString()" << '\n'
        << indent() << "{" << '\n';
    indent_up();
    out << indent() << "var " << tmpvar << " = new StringBuilder(\"" << tstruct->get_name() << "(\");" << '\n';

    const vector<t_field*>& fields = tstruct->get_members();
    vector<t_field*>::const_iterator f_iter;

    bool useFirstFlag = false;
    string tmp_count = tmp("tmp");
    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
    {
        if (!field_is_required((*f_iter)))
        {
            out << indent() << "int " << tmp_count.c_str() << " = 0;" << '\n';
            useFirstFlag = true;
        }
        break;
    }

    bool had_required = false; // set to true after first required field has been processed

    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
    {
        bool is_required = field_is_required(*f_iter);
        generate_null_check_begin(out, *f_iter);

        if (useFirstFlag && (!had_required))
        {
            out << indent() << "if(0 < " << tmp_count.c_str() << (is_required ? "" : "++") << ") { " << tmpvar << ".Append(\", \"); }" << '\n';
            out << indent() << tmpvar << ".Append(\"" << prop_name(*f_iter) << ": \");" << '\n';
        }
        else
        {
            out << indent() << tmpvar << ".Append(\", " << prop_name(*f_iter) << ": \");" << '\n';
        }

        out << indent() << prop_name(*f_iter) << ".ToString(" << tmpvar << ");" << '\n';

        generate_null_check_end(out, *f_iter);
        if (is_required) {
            had_required = true; // now __count must be > 0, so we don't need to check it anymore
        }
    }

    out << indent() << tmpvar << ".Append(')');" << '\n'
        << indent() << "return " << tmpvar << ".ToString();" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
}

void t_netstd_generator::generate_netstd_union(t_struct* tunion)
{
    int ic = indent_count();

    string f_union_name = namespace_dir_ + "/" + (tunion->get_name()) + ".cs";
    ofstream_with_content_based_conditional_update f_union;

    f_union.open(f_union_name.c_str());

    reset_indent();
    f_union << autogen_comment() << netstd_type_usings() << netstd_thrift_usings() << '\n' << '\n';

    pragmas_and_directives(f_union);
    generate_netstd_union_definition(f_union, tunion);

    f_union.close();

    indent_validate(ic, "generate_netstd_union.");
}

void t_netstd_generator::generate_netstd_union_definition(ostream& out, t_struct* tunion)
{
    // Let's define the class first
    start_netstd_namespace(out);

    generate_deprecation_attribute(out, tunion->annotations_);
    out << indent() << "public abstract partial class " << normalize_name(tunion->get_name()) << " : TUnionBase" << '\n';
    out << indent() << "{" << '\n';
    indent_up();

    out << indent() << "public abstract global::System.Threading.Tasks.Task WriteAsync(TProtocol tProtocol, CancellationToken " << CANCELLATION_TOKEN_NAME << ");" << '\n'
        << indent() << "public readonly int Isset;" << '\n'
        << indent() << "public abstract object" << nullable_suffix() <<" Data { get; }" << '\n'
        << indent() << "protected " << normalize_name(tunion->get_name()) << "(int isset)" << '\n'
        << indent() << "{" << '\n';
    indent_up();
    out << indent() << "Isset = isset;" << '\n';
    indent_down();
    out << indent() << "}" << '\n' << '\n';

    const vector<t_field*>& fields = tunion->get_members();
    vector<t_field*>::const_iterator f_iter;

    out << indent() << "public override bool Equals(object" << nullable_suffix() << " that)" << '\n';
    scope_up(out);
    if( target_net_version >= 6) {
        out << indent() << "if (that is not " << tunion->get_name() << " other) return false;" << '\n';
    } else {
        out << indent() << "if (!(that is " << tunion->get_name() << " other)) return false;" << '\n';
    }
    out << indent() << "if (ReferenceEquals(this, other)) return true;" << '\n';
    out << '\n';
    out << indent() << "if(this.Isset != other.Isset) return false;" << '\n';
    out << '\n';
    if(target_net_version >= 6) {
        out << indent() << "return Isset switch" << '\n';
        scope_up(out);
        for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
        {
            bool needs_typecast = false;
            string suffix("");
            get_deep_copy_method_call((*f_iter)->get_type(), false, needs_typecast, suffix);
            out << indent() << (*f_iter)->get_key() << " => Equals(As_" << (*f_iter)->get_name() << ", other.As_" << (*f_iter)->get_name() << ")," << '\n';
        }
        out << indent() << "_ => true," << '\n';
        indent_down();
        out << indent() << "};" << '\n';
    } else {
        out << indent() << "switch (Isset)" << '\n';
        scope_up(out);
        for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
        {
            bool needs_typecast = false;
            string suffix("");
            get_deep_copy_method_call((*f_iter)->get_type(), false, needs_typecast, suffix);
            out << indent() << "case " << (*f_iter)->get_key() << ":" << '\n';
            indent_up();
            out << indent() << "return Equals(As_" << (*f_iter)->get_name() << ", other.As_" << (*f_iter)->get_name() << ");" << '\n';
            indent_down();
        }
        out << indent() << "default:" << '\n';
        indent_up();
        out << indent() << "return true;" << '\n';
        indent_down();
        scope_down(out);
    }
    scope_down(out);
    out << '\n';

    out << indent() << "public override int GetHashCode()" << '\n';
    out << indent() << "{" << '\n';
    indent_up();
    if(target_net_version >= 6) {
        out << indent() << "return Isset switch" << '\n';
        out << indent() << "{" << '\n';
        indent_up();
        for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
        {
            string null_coalesce(is_nullable_type((*f_iter)->get_type()) ? "?" : "");
            out << indent() << (*f_iter)->get_key() << " => As_" << (*f_iter)->get_name() << null_coalesce << ".GetHashCode()";
            if( null_coalesce.size() > 0) {
              out << " ?? 0";
            }
            out << "," << '\n';
        }
        out << indent() << "_ =>  (new ___undefined()).GetHashCode()" << '\n';
        indent_down();
        out << indent() << "};" << '\n';
    } else {
        out << indent() << "switch (Isset)" << '\n';
        out << indent() << "{" << '\n';
        indent_up();
        for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
        {
            string null_coalesce(is_nullable_type((*f_iter)->get_type()) ? "?" : "");
            out << indent() << "case " << (*f_iter)->get_key() << ":" << '\n';
            indent_up();
            out << indent() << "return As_" << (*f_iter)->get_name() << null_coalesce << ".GetHashCode()";
            if( null_coalesce.size() > 0) {
              out << " ?? 0";
            }
            out << ";" << '\n';
            indent_down();
        }
        out << indent() << "default:" << '\n';
        indent_up();
        out << indent() << "return (new ___undefined()).GetHashCode();" << '\n';
        indent_down();
        indent_down();
        out << indent() << "}" << '\n';
    }
    indent_down();
    out << indent() << "}" << '\n' << '\n';

    if( ! suppress_deepcopy) {
        out << indent() << "public " << tunion->get_name() << " " << DEEP_COPY_METHOD_NAME << "()" << '\n';
        out << indent() << "{" << '\n';
        indent_up();
        if(target_net_version >= 6) {
            out << indent() << "return Isset switch" << '\n';
            out << indent() << "{" << '\n';
            indent_up();
            for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
            {
                bool needs_typecast = false;
                string suffix("");
                string copy_op = get_deep_copy_method_call((*f_iter)->get_type(), false, needs_typecast, suffix);
                out << indent() << (*f_iter)->get_key() << " => new " << (*f_iter)->get_name() << "(As_" << (*f_iter)->get_name() << suffix << copy_op << ")," << '\n';
            }
            out << indent() << "_ => new ___undefined()" << '\n';
            indent_down();
            out << indent() << "};" << '\n';
        } else {
            out << indent() << "switch (Isset)" << '\n';
            out << indent() << "{" << '\n';
            indent_up();
            for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
            {
                bool needs_typecast = false;
                string suffix("");
                string copy_op = get_deep_copy_method_call((*f_iter)->get_type(), false, needs_typecast, suffix);
                out << indent() << "case " << (*f_iter)->get_key() << ":" << '\n';
                indent_up();
                out << indent() << "return new " << (*f_iter)->get_name() << "(As_" << (*f_iter)->get_name() << suffix << copy_op << ");" << '\n';
                indent_down();
            }
            out << indent() << "default:" << '\n';
            indent_up();
            out << indent() << "return new ___undefined();" << '\n';
            indent_down();
            indent_down();
            out << indent() << "}" << '\n';
        }
        indent_down();
        out << indent() << "}" << '\n' << '\n';
    }

    out << indent() << "public class ___undefined : " << tunion->get_name() << '\n';
    out << indent() << "{" << '\n';
    indent_up();

    out << indent() << "public override object" << nullable_suffix() <<" Data { get { return null; } }" << '\n'
        << indent() << "public ___undefined() : base(0) {}" << '\n' << '\n';

    if( ! suppress_deepcopy) {
        out << indent() << "public new ___undefined " << DEEP_COPY_METHOD_NAME << "()" << '\n';
        out << indent() << "{" << '\n';
        indent_up();
        out << indent() << "return new ___undefined();" << '\n';
        indent_down();
        out << indent() << "}" << '\n' << '\n';
    }

    t_struct undefined_struct(program_,"___undefined");
    generate_netstd_struct_equals(out, &undefined_struct);
    generate_netstd_struct_hashcode(out, &undefined_struct);

    out << indent() << "public override global::System.Threading.Tasks.Task WriteAsync(TProtocol oprot, CancellationToken " << CANCELLATION_TOKEN_NAME << ")" << '\n'
        << indent() << "{" << '\n';
    indent_up();
    out << indent() << "throw new TProtocolException( TProtocolException.INVALID_DATA, \"Cannot persist an union type which is not set.\");" << '\n';
    indent_down();
    out << indent() << "}" << '\n' << '\n';
    indent_down();
    out << indent() << "}" << '\n' << '\n';

    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
    {
        generate_netstd_union_class(out, tunion, (*f_iter));
    }

    generate_netstd_union_reader(out, tunion);

    indent_down();
    out << indent() << "}" << '\n' << '\n';

    end_netstd_namespace(out);
}

void t_netstd_generator::generate_netstd_union_class(ostream& out, t_struct* tunion, t_field* tfield)
{
    out << indent() << "public " << type_name(tfield->get_type()) << nullable_field_suffix(tfield) << " As_" << tfield->get_name() << '\n';
    out << indent() << "{" << '\n';
    indent_up();
    out << indent() << "get" << '\n';
    out << indent() << "{" << '\n';
    indent_up();
    out << indent() << "return (" << tfield->get_key() << " == Isset) && (Data != null)"
        << " ? (" << type_name(tfield->get_type()) << nullable_field_suffix(tfield) << ")Data"
        << " : default"
        << (target_net_version >= 6 ? "" : ("(" + type_name(tfield->get_type()) + ")"))
        << ";" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
    indent_down();
    out << indent() << "}" << '\n'
        << '\n';


    out << indent() << "public class " << normalize_name(tfield->get_name()) << " : " << normalize_name(tunion->get_name()) << '\n';
    out << indent() << "{" << '\n';
    indent_up();

    out << indent() << "private readonly " << type_name(tfield->get_type()) << " _data;" << '\n'
        << indent() << "public override object" << nullable_suffix() <<" Data { get { return _data; } }" << '\n'
        << indent() << "public " << normalize_name(tfield->get_name()) << "(" << type_name(tfield->get_type()) << " data) : base("<< tfield->get_key() <<")" << '\n'
        << indent() << "{" << '\n';
    indent_up();
    out << indent() << "this._data = data;" << '\n';
    indent_down();
    out << indent() << "}" << '\n';

    if( ! suppress_deepcopy) {
        out << indent() << "public new " << normalize_name(tfield->get_name()) << " " << DEEP_COPY_METHOD_NAME << "()" << '\n';
        out << indent() << "{" << '\n';
        indent_up();
        bool needs_typecast = false;
        string suffix("");
        string copy_op = get_deep_copy_method_call(tfield->get_type(), true, needs_typecast, suffix);
        out << indent() << "return new " << normalize_name(tfield->get_name()) << "(_data" << copy_op << ");" << '\n';
        indent_down();
        out << indent() << "}" << '\n' << '\n';
    }

    out << indent() << "public override bool Equals(object" << nullable_suffix() << " that)" << '\n';
    out << indent() << "{" << '\n';
    indent_up();
    if(target_net_version >= 6) {
        out << indent() << "if (that is not " << tunion->get_name() << " other) return false;" << '\n';
    } else {
        out << indent() << "if (!(that is " << tunion->get_name() << " other)) return false;" << '\n';
    }
    out << indent() << "if (ReferenceEquals(this, other)) return true;" << '\n';
    out << '\n';
    out << indent() << "return Equals( _data, other.As_" << tfield->get_name() << ");" << '\n';
    indent_down();
    out << indent() << "}" << '\n' << '\n';

    out << indent() << "public override int GetHashCode()" << '\n';
    out << indent() << "{" << '\n';
    indent_up();
    out << indent() << "return _data.GetHashCode();" << '\n';
    indent_down();
    out << indent() << "}" << '\n' << '\n';

    out << indent() << "public override async global::System.Threading.Tasks.Task WriteAsync(TProtocol oprot, CancellationToken " << CANCELLATION_TOKEN_NAME << ") {" << '\n';
    indent_up();

    out << indent() << "oprot.IncrementRecursionDepth();" << '\n'
        << indent() << "try" << '\n'
        << indent() << "{" << '\n';
    indent_up();

    out << indent() << "var struc = new TStruct(\"" << tunion->get_name() << "\");" << '\n'
        << indent() << "await oprot.WriteStructBeginAsync(struc, " << CANCELLATION_TOKEN_NAME << ");" << '\n';

    out << indent() << "var field = new TField()" << '\n';
    out << indent() << "{" << '\n';
    indent_up();
    out << indent() << "Name = \"" << tfield->get_name() << "\"," << '\n'
        << indent() << "Type = " << type_to_enum(tfield->get_type()) << "," << '\n'
        << indent() << "ID = " << tfield->get_key() << '\n';
    indent_down();
    out << indent() << "};" << '\n';
    out << indent() << "await oprot.WriteFieldBeginAsync(field, " << CANCELLATION_TOKEN_NAME << ");" << '\n';

    generate_serialize_field(out, tfield, "_data", true, false);

    out << indent() << "await oprot.WriteFieldEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n'
        << indent() << "await oprot.WriteFieldStopAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n'
        << indent() << "await oprot.WriteStructEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';
    indent_down();
    out << indent() << "}" << '\n'
        << indent() << "finally" << '\n'
        << indent() << "{" << '\n';
    indent_up();
    out << indent() << "oprot.DecrementRecursionDepth();" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
    indent_down();
    out << indent() << "}" << '\n' << '\n';
}

void t_netstd_generator::generate_netstd_struct_equals(ostream& out, t_struct* tstruct)
{
    out << indent() << "public override bool Equals(object" << nullable_suffix() << " that)" << '\n'
        << indent() << "{" << '\n';
    indent_up();
    if(target_net_version >= 6) {
        out << indent() << "if (that is not " << type_name(tstruct,false) << " other) return false;" << '\n';
    } else {
        out << indent() << "if (!(that is " << type_name(tstruct,false) << " other)) return false;" << '\n';
    }
    out << indent() << "if (ReferenceEquals(this, other)) return true;" << '\n';


    const vector<t_field*>& fields = tstruct->get_members();
    vector<t_field*>::const_iterator f_iter;

    bool first = true;

    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
    {
        if (first)
        {
            first = false;
            out << indent() << "return ";
            indent_up();
        }
        else
        {
            out << '\n';
            out << indent() << "&& ";
        }
        if (!field_is_required((*f_iter)))
        {
            out << "((__isset." << get_isset_name(normalize_name((*f_iter)->get_name())) << " == other.__isset."
                << get_isset_name(normalize_name((*f_iter)->get_name())) << ") && ((!__isset."
                << get_isset_name(normalize_name((*f_iter)->get_name())) << ") || (";
        }
        t_type* ttype = (*f_iter)->get_type();
        if (ttype->is_container() || ttype->is_binary())
        {
            out << "TCollections.Equals(";
        }
        else
        {
            out << "global::System.Object.Equals(";
        }
        out << prop_name((*f_iter)) << ", other." << prop_name((*f_iter)) << ")";
        if (!field_is_required((*f_iter)))
        {
            out << ")))";
        }
    }
    if (first)
    {
        out << indent() << "return true;" << '\n';
    }
    else
    {
        out << ";" << '\n';
        indent_down();
    }

    indent_down();
    out << indent() << "}" << '\n' << '\n';
}

void t_netstd_generator::generate_netstd_struct_hashcode(ostream& out, t_struct* tstruct)
{
    out << indent() << "public override int GetHashCode() {" << '\n';
    indent_up();

    out << indent() << "int hashcode = 157;" << '\n';
    out << indent() << "unchecked {" << '\n';
    indent_up();

    const vector<t_field*>& fields = tstruct->get_members();
    vector<t_field*>::const_iterator f_iter;

    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
    {
        t_type* ttype = (*f_iter)->get_type();

        generate_null_check_begin(out, *f_iter);
        out << indent() << "hashcode = (hashcode * 397) + ";
        if (ttype->is_container()) {
            out << "TCollections.GetHashCode(" << prop_name((*f_iter)) << ")";
        }
        else {
            out << prop_name(*f_iter) << ".GetHashCode()";
        }
        out << ";" << '\n';

        generate_null_check_end(out, *f_iter);
    }

    indent_down();
    out << indent() << "}" << '\n';
    out << indent() << "return hashcode;" << '\n';

    indent_down();
    out << indent() << "}" << '\n' << '\n';
}

void t_netstd_generator::generate_service(t_service* tservice)
{
    int ic = indent_count();

    string f_service_name = namespace_dir_ + "/" + service_name_ + ".cs";
    ofstream_with_content_based_conditional_update f_service;
    f_service.open(f_service_name.c_str());

    reset_indent();
    f_service << autogen_comment() << netstd_type_usings() << netstd_thrift_usings() << '\n' << '\n';

    pragmas_and_directives(f_service);
    start_netstd_namespace(f_service);

    f_service << indent() << "public partial class " << normalize_name(service_name_) << '\n'
              << indent() << "{" << '\n';
    indent_up();

    generate_service_interface(f_service, tservice);
    generate_service_client(f_service, tservice);
    generate_service_server(f_service, tservice);
    generate_service_helpers(f_service, tservice);

    indent_down();
    f_service << indent() << "}" << '\n';

    end_netstd_namespace(f_service);
    f_service.close();

    indent_validate(ic, "generate_service.");
}

void t_netstd_generator::generate_service_interface(ostream& out, t_service* tservice)
{
    string extends = "";
    string extends_iface = "";
    if (tservice->get_extends() != nullptr)
    {
        extends = type_name(tservice->get_extends());
        extends_iface = " : " + extends + ".IAsync";
    }

    //out << '\n' << '\n';

    generate_netstd_doc(out, tservice);

    if (is_wcf_enabled())
    {
        out << indent() << "[ServiceContract(Namespace=\"" << wcf_namespace_ << "\")]" << '\n';
    }

    generate_deprecation_attribute(out, tservice->annotations_);
    prepare_member_name_mapping(tservice);
    out << indent() << "public interface IAsync" << extends_iface << '\n'
        << indent() << "{" << '\n';

    indent_up();
    vector<t_function*> functions = tservice->get_functions();
    vector<t_function*>::iterator f_iter;
    for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter)
    {
        generate_netstd_doc(out, *f_iter);

        // if we're using WCF, add the corresponding attributes
        if (is_wcf_enabled())
        {
            out << indent() << "[OperationContract]" << '\n';

            const vector<t_field*>& xceptions = (*f_iter)->get_xceptions()->get_members();
            vector<t_field*>::const_iterator x_iter;
            for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter)
            {
                out << indent() << "[FaultContract(typeof(" + type_name((*x_iter)->get_type()) + "Fault))]" << '\n';
            }
        }

        generate_deprecation_attribute(out, (*f_iter)->annotations_);
        out << indent() << function_signature_async(*f_iter) << ";" << '\n' << '\n';
    }
    indent_down();
    out << indent() << "}" << '\n' << '\n';
    cleanup_member_name_mapping(tservice);
}

bool t_netstd_generator::is_deprecated(std::map<std::string, std::vector<std::string>>& annotations)
{
  auto iter = annotations.find("deprecated");
  return (annotations.end() != iter);
}

void t_netstd_generator::generate_deprecation_attribute(ostream& out, std::map<std::string, std::vector<std::string>>& annotations)
{
  auto iter = annotations.find("deprecated");
  if( annotations.end() != iter) {
    out << indent() << "[Obsolete";
    // empty annotation values end up with "1" somewhere, ignore these as well
    if ((iter->second.back().length() > 0) && (iter->second.back() != "1")) {
      out << "(" << make_csharp_string_literal(iter->second.back()) << ")";
    } else {
      out << "(" << make_csharp_string_literal("This code is deprecated.") << ")";  // generic message to prevent CA1041
    }
    out << "]" << '\n';
  }
}

void t_netstd_generator::generate_service_helpers(ostream& out, t_service* tservice)
{
    vector<t_function*> functions = tservice->get_functions();
    vector<t_function*>::iterator f_iter;

    prepare_member_name_mapping(tservice);
    out << indent() << "public class InternalStructs" << '\n';
    out << indent() << "{" << '\n';
    indent_up();

    for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter)
    {
        t_struct* ts = (*f_iter)->get_arglist();
        collect_extensions_types(ts);
        generate_netstd_struct_definition(out, ts, false, true);
        generate_function_helpers(out, *f_iter);
    }

    indent_down();
    out << indent() << "}" << '\n' << '\n';
    cleanup_member_name_mapping(tservice);
}

void t_netstd_generator::generate_service_client(ostream& out, t_service* tservice)
{
    string extends = "";
    string extends_client = "";
    if (tservice->get_extends() != nullptr)
    {
        extends = type_name(tservice->get_extends());
        extends_client = extends + ".Client, ";
    }
    else
    {
        extends_client = "TBaseClient, IDisposable, ";
    }

    out << '\n';

    generate_netstd_doc(out, tservice);
    generate_deprecation_attribute(out, tservice->annotations_);
    prepare_member_name_mapping(tservice);
    out << indent() << "public class Client : " << extends_client << "IAsync" << '\n'
        << indent() << "{" << '\n';
    indent_up();

    out << indent() << "public Client(TProtocol protocol) : this(protocol, protocol)" << '\n'
        << indent() << "{" << '\n'
        << indent() << "}" << '\n'
        << '\n'
        << indent() << "public Client(TProtocol inputProtocol, TProtocol outputProtocol) : base(inputProtocol, outputProtocol)" << '\n'
        << indent() << "{" << '\n'
        << indent() << "}" << '\n'
        << '\n';

    vector<t_function*> functions = tservice->get_functions();
    vector<t_function*>::const_iterator functions_iterator;

    for (functions_iterator = functions.begin(); functions_iterator != functions.end(); ++functions_iterator)
    {
        string raw_func_name = (*functions_iterator)->get_name();
        string function_name = raw_func_name + (add_async_postfix ? "Async" : "");

        // async
        generate_deprecation_attribute(out, (*functions_iterator)->annotations_);
        out << indent() << "public async " << function_signature_async(*functions_iterator, "") << '\n'
            << indent() << "{" << '\n';
        indent_up();
        out << indent() << "await send_" << function_name << "(";
        string call_args = argument_list((*functions_iterator)->get_arglist(),false);
        if(! call_args.empty()) {
            out << call_args << ", ";
        }
        out << CANCELLATION_TOKEN_NAME << ");" << '\n';
        if(! (*functions_iterator)->is_oneway()) {
            out << indent() << ((*functions_iterator)->get_returntype()->is_void() ? "" : "return ")
                            << "await recv_" << function_name << "(" << CANCELLATION_TOKEN_NAME << ");" << '\n';
        }
        indent_down();
        out << indent() << "}" << '\n' << '\n';

        // async send
        generate_deprecation_attribute(out, (*functions_iterator)->annotations_);
        out << indent() << "public async " << function_signature_async(*functions_iterator, "send_", MODE_NO_RETURN) << '\n'
            << indent() << "{" << '\n';
        indent_up();

        string tmpvar = tmp("tmp");
        string argsname = (*functions_iterator)->get_name() + "_args";

        out << indent() << "await OutputProtocol.WriteMessageBeginAsync(new TMessage(\"" << raw_func_name
            << "\", TMessageType." << ((*functions_iterator)->is_oneway() ? "Oneway" : "Call")
            << ", SeqId), " << CANCELLATION_TOKEN_NAME << ");" << '\n'
            << indent() << '\n'
            << indent() << "var " << tmpvar << " = new InternalStructs." << argsname << "() {" << '\n';
        indent_up();

        t_struct* arg_struct = (*functions_iterator)->get_arglist();
        collect_extensions_types(arg_struct);
        prepare_member_name_mapping(arg_struct);
        const vector<t_field*>& fields = arg_struct->get_members();
        vector<t_field*>::const_iterator fld_iter;

        for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter)
        {
            out << indent() << prop_name(*fld_iter) << " = " << normalize_name((*fld_iter)->get_name(),true) << "," << '\n';
        }

        indent_down();
        out << indent() << "};" << '\n';


        out << indent() << '\n'
            << indent() << "await " << tmpvar << ".WriteAsync(OutputProtocol, " << CANCELLATION_TOKEN_NAME << ");" << '\n'
            << indent() << "await OutputProtocol.WriteMessageEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n'
            << indent() << "await OutputProtocol.Transport.FlushAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';

        indent_down();
        out << indent() << "}" << '\n' << '\n';

        if (!(*functions_iterator)->is_oneway())
        {
            // async recv
            generate_deprecation_attribute(out, (*functions_iterator)->annotations_);
            out << indent() << "public async " << function_signature_async(*functions_iterator, "recv_", MODE_NO_ARGS) << '\n'
                << indent() << "{" << '\n';
            indent_up();

            string resultname = (*functions_iterator)->get_name() + "_result";
            t_struct noargs(program_);
            t_struct* xs = (*functions_iterator)->get_xceptions();
            collect_extensions_types(xs);
            prepare_member_name_mapping(xs, xs->get_members(), resultname);

            tmpvar = tmp("tmp");
            out << indent() << '\n'
                << indent() << "var " << tmpvar << " = await InputProtocol.ReadMessageBeginAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n'
                << indent() << "if (" << tmpvar << ".Type == TMessageType.Exception)" << '\n'
                << indent() << "{" << '\n';
            indent_up();

            tmpvar = tmp("tmp");
            out << indent() << "var " << tmpvar << " = await TApplicationException.ReadAsync(InputProtocol, " << CANCELLATION_TOKEN_NAME << ");" << '\n'
                << indent() << "await InputProtocol.ReadMessageEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n'
                << indent() << "throw " << tmpvar << ";" << '\n';
            indent_down();

            tmpvar = tmp("tmp");
            out << indent() << "}" << '\n'
                << '\n'
                << indent() << "var " << tmpvar << " = new InternalStructs." << resultname << "();" << '\n'
                << indent() << "await " << tmpvar << ".ReadAsync(InputProtocol, " << CANCELLATION_TOKEN_NAME << ");" << '\n'
                << indent() << "await InputProtocol.ReadMessageEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';

            if (!(*functions_iterator)->get_returntype()->is_void())
            {
                out << indent() << "if (" << tmpvar << ".__isset.success)" << '\n'
                    << indent() << "{" << '\n';
                indent_up();
                string nullable_value = nullable_value_access((*functions_iterator)->get_returntype());
                out << indent() << "return " << tmpvar << ".Success" << nullable_value << ";" << '\n';
                indent_down();
                out << indent() << "}" << '\n';
            }

            const vector<t_field*>& xceptions = xs->get_members();
            vector<t_field*>::const_iterator x_iter;
            for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter)
            {
                out << indent() << "if (" << tmpvar << ".__isset." << get_isset_name(normalize_name((*x_iter)->get_name())) << ")" << '\n'
                    << indent() << "{" << '\n';
                indent_up();
                out << indent() << "throw " << tmpvar << "." << prop_name(*x_iter) << nullable_value_access((*x_iter)->get_type()) << ";" << '\n';
                indent_down();
                out << indent() << "}" << '\n';
            }

            if (!(*functions_iterator)->get_returntype()->is_void())
            {
                out << indent() << "throw new TApplicationException(TApplicationException.ExceptionType.MissingResult, \""
                    << function_name << " failed: unknown result\");" << '\n';
            }

            cleanup_member_name_mapping(xs);
            indent_down();
            out << indent() << "}" << '\n' << '\n';
        }

        cleanup_member_name_mapping(arg_struct);
    }

    indent_down();
    out << indent() << "}" << '\n' << '\n';
    cleanup_member_name_mapping(tservice);
}

void t_netstd_generator::generate_service_server(ostream& out, t_service* tservice)
{
    vector<t_function*> functions = tservice->get_functions();
    vector<t_function*>::iterator f_iter;

    string extends = "";
    string extends_processor = "";
    if (tservice->get_extends() != nullptr)
    {
        extends = type_name(tservice->get_extends());
        extends_processor = extends + ".AsyncProcessor, ";
    }

    prepare_member_name_mapping(tservice);
    out << indent() << "public class AsyncProcessor : " << extends_processor << "ITAsyncProcessor" << '\n'
        << indent() << "{" << '\n';

    indent_up();

    out << indent() << "private readonly IAsync _iAsync;" << '\n'
        << indent() << "private readonly ILogger<AsyncProcessor>" << nullable_suffix() << " _logger;" << '\n'
        << '\n'
        << indent() << "public AsyncProcessor(IAsync iAsync, ILogger<AsyncProcessor>" << nullable_suffix() << " logger = default)";

    if (!extends.empty())
    {
        out << " : base(iAsync)";
    }

    out << '\n'
        << indent() << "{" << '\n';
    indent_up();

    out << indent() << "_iAsync = iAsync ?? throw new ArgumentNullException(nameof(iAsync));" << '\n';
    out << indent() << "_logger = logger;" << '\n';
    for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter)
    {
        string raw_func_name = (*f_iter)->get_name();
        out << indent() << "processMap_[\"" << raw_func_name << "\"] = " << raw_func_name << "_ProcessAsync;" << '\n';
    }

    indent_down();
    out << indent() << "}" << '\n'
        << '\n';

    if (extends.empty())
    {
        out << indent() << "protected delegate global::System.Threading.Tasks.Task ProcessFunction(int seqid, TProtocol iprot, TProtocol oprot, CancellationToken " << CANCELLATION_TOKEN_NAME << ");" << '\n';
    }

    if (extends.empty())
    {
        out << indent() << "protected Dictionary<string, ProcessFunction> processMap_ = ";
        if(target_net_version >= 8) {
          out << "[];" << '\n';
        } else if(target_net_version >= 6) {
          out << "new();" << '\n';
        } else {
          out << "new Dictionary<string, ProcessFunction>();" << '\n';
        }
    }

    out << '\n';

    if (extends.empty())
    {
        out << indent() << "public async Task<bool> ProcessAsync(TProtocol iprot, TProtocol oprot)" << '\n'
            << indent() << "{" << '\n';
        indent_up();
        out << indent() << "return await ProcessAsync(iprot, oprot, CancellationToken.None);" << '\n';
        indent_down();
        out << indent() << "}" << '\n' << '\n';

        out << indent() << "public async Task<bool> ProcessAsync(TProtocol iprot, TProtocol oprot, CancellationToken " << CANCELLATION_TOKEN_NAME << ")" << '\n';
    }
    else
    {
        out << indent() << "public new async Task<bool> ProcessAsync(TProtocol iprot, TProtocol oprot)" << '\n'
            << indent() << "{" << '\n';
        indent_up();
        out << indent() << "return await ProcessAsync(iprot, oprot, CancellationToken.None);" << '\n';
        indent_down();
        out << indent() << "}" << '\n' << '\n';

        out << indent() << "public new async Task<bool> ProcessAsync(TProtocol iprot, TProtocol oprot, CancellationToken " << CANCELLATION_TOKEN_NAME << ")" << '\n';
    }

    out << indent() << "{" << '\n';
    indent_up();
    out << indent() << "try" << '\n'
        << indent() << "{" << '\n';
    indent_up();
    out << indent() << "var msg = await iprot.ReadMessageBeginAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n'
        << '\n'
        << indent() << "processMap_.TryGetValue(msg.Name, out var fn);" << '\n'
        << '\n'
        << indent() << "if (fn == null)" << '\n'
        << indent() << "{" << '\n';
    indent_up();
    out << indent() << "await TProtocolUtil.SkipAsync(iprot, TType.Struct, " << CANCELLATION_TOKEN_NAME << ");" << '\n'
        << indent() << "await iprot.ReadMessageEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n'
        << indent() << "var x = new TApplicationException (TApplicationException.ExceptionType.UnknownMethod, \"Invalid method name: '\" + msg.Name + \"'\");" << '\n'
        << indent() << "await oprot.WriteMessageBeginAsync(new TMessage(msg.Name, TMessageType.Exception, msg.SeqID), " << CANCELLATION_TOKEN_NAME << ");" << '\n'
        << indent() << "await x.WriteAsync(oprot, " << CANCELLATION_TOKEN_NAME << ");" << '\n'
        << indent() << "await oprot.WriteMessageEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n'
        << indent() << "await oprot.Transport.FlushAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n'
        << indent() << "return true;" << '\n';
    indent_down();
    out << indent() << "}" << '\n'
        << '\n'
        << indent() << "await fn(msg.SeqID, iprot, oprot, " << CANCELLATION_TOKEN_NAME << ");" << '\n'
        << '\n';
    indent_down();
    out << indent() << "}" << '\n';
    out << indent() << "catch (IOException)" << '\n'
        << indent() << "{" << '\n';
    indent_up();
    out << indent() << "return false;" << '\n';
    indent_down();
    out << indent() << "}" << '\n'
        << '\n'
        << indent() << "return true;" << '\n';
    indent_down();
    out << indent() << "}" << '\n' << '\n';

    for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter)
    {
        generate_process_function_async(out, tservice, *f_iter);
    }

    indent_down();
    out << indent() << "}" << '\n' << '\n';
    cleanup_member_name_mapping(tservice);
}

void t_netstd_generator::generate_function_helpers(ostream& out, t_function* tfunction)
{
    if (tfunction->is_oneway())
    {
        return;
    }

    t_struct result(program_, tfunction->get_name() + "_result");
    t_field success(tfunction->get_returntype(), "success", 0);
    if (!tfunction->get_returntype()->is_void())
    {
        result.append(&success);
    }

    t_struct* xs = tfunction->get_xceptions();
    const vector<t_field*>& fields = xs->get_members();
    vector<t_field*>::const_iterator f_iter;
    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
    {
        result.append(*f_iter);
    }

    collect_extensions_types(&result);
    generate_netstd_struct_definition(out, &result, false, true, true);
}

void t_netstd_generator::generate_process_function_async(ostream& out, t_service* tservice, t_function* tfunction)
{
    (void)tservice;
    out << indent() << "public async global::System.Threading.Tasks.Task " << tfunction->get_name()
        << "_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot, CancellationToken " << CANCELLATION_TOKEN_NAME << ")" << '\n'
        << indent() << "{" << '\n';
    indent_up();

    string argsname = tfunction->get_name() + "_args";
    string resultname = tfunction->get_name() + "_result";

    string args = tmp("tmp");
    out << indent() << "var " << args << " = new InternalStructs." << argsname << "();" << '\n'
        << indent() << "await " << args << ".ReadAsync(iprot, " << CANCELLATION_TOKEN_NAME << ");" << '\n'
        << indent() << "await iprot.ReadMessageEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';

    string tmpResult = tmp("tmp");
    if (!tfunction->is_oneway())
    {
        out << indent() << "var " << tmpResult << " = new InternalStructs." << resultname << "();" << '\n';
    }

    out << indent() << "try" << '\n'
        << indent() << "{" << '\n';
    indent_up();

    t_struct* xs = tfunction->get_xceptions();
    const vector<t_field*>& xceptions = xs->get_members();

    if (xceptions.size() > 0)
    {
        out << indent() << "try" << '\n'
            << indent() << "{" << '\n';
        indent_up();
    }

    t_struct* arg_struct = tfunction->get_arglist();
    const vector<t_field*>& fields = arg_struct->get_members();
    vector<t_field*>::const_iterator f_iter;

    if( is_deprecated(tfunction->annotations_)) {
      out << indent() << "#pragma warning disable CS0618,CS0612" << '\n';
    }

    out << indent();
    if (!tfunction->is_oneway() && !tfunction->get_returntype()->is_void())
    {
        out << tmpResult << ".Success = ";
    }

    out << "await _iAsync." << func_name(normalize_name(tfunction->get_name()) + (add_async_postfix ? "Async" : "")) << "(";

    bool first = true;
    collect_extensions_types(arg_struct);
    prepare_member_name_mapping(arg_struct);
    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
    {
        if (first)
        {
            first = false;
        }
        else
        {
            out << ", ";
        }

        out << args << "." << prop_name(*f_iter);
    }

    cleanup_member_name_mapping(arg_struct);

    if (!first)
    {
        out << ", ";
    }

    out << "" << CANCELLATION_TOKEN_NAME << ");" << '\n';

    if( is_deprecated(tfunction->annotations_)) {
      out << indent() << "#pragma warning restore CS0618,CS0612" << '\n';
    }

    vector<t_field*>::const_iterator x_iter;

    collect_extensions_types(xs);
    prepare_member_name_mapping(xs, xs->get_members(), resultname);
    if (xceptions.size() > 0)
    {
        indent_down();
        out << indent() << "}" << '\n';

        for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter)
        {
            string tmpex = tmp("tmp");
            out << indent() << "catch (" << type_name((*x_iter)->get_type()) << " " << tmpex << ")" << '\n'
                << indent() << "{" << '\n';

            if (!tfunction->is_oneway())
            {
                indent_up();
                out << indent() << tmpResult << "." << prop_name(*x_iter) << " = " << tmpex << ";" << '\n';
                indent_down();
            }
            out << indent() << "}" << '\n';
        }
    }

    if (!tfunction->is_oneway())
    {
        out << indent() << "await oprot.WriteMessageBeginAsync(new TMessage(\""
                << tfunction->get_name() << "\", TMessageType.Reply, seqid), " << CANCELLATION_TOKEN_NAME << "); " << '\n'
            << indent() << "await " << tmpResult << ".WriteAsync(oprot, " << CANCELLATION_TOKEN_NAME << ");" << '\n';
    }
    indent_down();

    cleanup_member_name_mapping(xs);

    string tmpex = tmp("tmp");
    out << indent() << "}" << '\n'
        << indent() << "catch (TTransportException)" << '\n'
        << indent() << "{" << '\n'
        << indent() << "  throw;" << '\n'
        << indent() << "}" << '\n'
        << indent() << "catch (Exception " << tmpex << ")" << '\n'
        << indent() << "{" << '\n';
    indent_up();

    string tmpvar = tmp("tmp");
    out << indent() << "var " << tmpvar << " = $\"Error occurred in {GetType().FullName}: {" << tmpex << ".Message}\";" << '\n';
    out << indent() << "if(_logger != null)" << '\n';
    indent_up();
    out << indent() << "_logger.LogError(\"{Exception}, {Message}\", " << tmpex << ", " << tmpvar << ");" << '\n';
    indent_down();
    out << indent() << "else" << '\n';
    indent_up();
    out << indent() << "Console.Error.WriteLine(" << tmpvar << ");" << '\n';
    indent_down();

    if (tfunction->is_oneway())
    {
        indent_down();
        out << indent() << "}" << '\n';
    }
    else
    {
        tmpvar = tmp("tmp");
        out << indent() << "var " << tmpvar << " = new TApplicationException(TApplicationException.ExceptionType.InternalError,\" Internal error.\");" << '\n'
            << indent() << "await oprot.WriteMessageBeginAsync(new TMessage(\"" << tfunction->get_name()
            << "\", TMessageType.Exception, seqid), " << CANCELLATION_TOKEN_NAME << ");" << '\n'
            << indent() << "await " << tmpvar << ".WriteAsync(oprot, " << CANCELLATION_TOKEN_NAME << ");" << '\n';
        indent_down();

        out << indent() << "}" << '\n'
            << indent() << "await oprot.WriteMessageEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n'
            << indent() << "await oprot.Transport.FlushAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';
    }

    indent_down();
    out << indent() << "}" << '\n' << '\n';
}

void t_netstd_generator::generate_netstd_union_reader(ostream& out, t_struct* tunion)
{
    // Thanks to THRIFT-1768, we don't need to check for required fields in the union
    const vector<t_field*>& fields = tunion->get_members();
    vector<t_field*>::const_iterator f_iter;

    out << indent() << "public static async Task<" << tunion->get_name() << "> ReadAsync(TProtocol iprot, CancellationToken " << CANCELLATION_TOKEN_NAME << ")" << '\n';
    scope_up(out);

    out << indent() << "iprot.IncrementRecursionDepth();" << '\n';
    out << indent() << "try" << '\n';
    scope_up(out);

    string tmpRetval = tmp("tmp");
    out << indent() << tunion->get_name() << " " << tmpRetval << ";" << '\n';
    out << indent() << "await iprot.ReadStructBeginAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';
    out << indent() << "TField field = await iprot.ReadFieldBeginAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';
    // we cannot have the first field be a stop -- we must have a single field defined
    out << indent() << "if (field.Type == TType.Stop)" << '\n';
    scope_up(out);
    out << indent() << "await iprot.ReadFieldEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';
    out << indent() << "" << tmpRetval << " = new ___undefined();" << '\n';
    scope_down(out);
    out << indent() << "else" << '\n';
    scope_up(out);
    out << indent() << "switch (field.ID)" << '\n';
    scope_up(out);

    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
    {
        out << indent() << "case " << (*f_iter)->get_key() << ":" << '\n';
        indent_up();
        out << indent() << "if (field.Type == " << type_to_enum((*f_iter)->get_type()) << ") {" << '\n';
        indent_up();

        string tmpvar = tmp("tmp");
        out << indent() << type_name((*f_iter)->get_type()) << " " << tmpvar << ";" << '\n';
        generate_deserialize_field(out, (*f_iter), tmpvar, true);
        out << indent() << tmpRetval << " = new " << (*f_iter)->get_name() << "(" << tmpvar << ");" << '\n';

        indent_down();
        out << indent() << "} else { " << '\n' << indent() << " await TProtocolUtil.SkipAsync(iprot, field.Type, " << CANCELLATION_TOKEN_NAME << ");"
            << '\n' << indent() << "  " << tmpRetval << " = new ___undefined();" << '\n' << indent() << "}" << '\n'
            << indent() << "break;" << '\n';
        indent_down();
    }

    out << indent() << "default: " << '\n';
    indent_up();
    out << indent() << "await TProtocolUtil.SkipAsync(iprot, field.Type, " << CANCELLATION_TOKEN_NAME << ");" << '\n' << indent()
        << tmpRetval << " = new ___undefined();" << '\n';
    out << indent() << "break;" << '\n';
    indent_down();

    scope_down(out);

    out << indent() << "await iprot.ReadFieldEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';

    out << indent() << "if ((await iprot.ReadFieldBeginAsync(" << CANCELLATION_TOKEN_NAME << ")).Type != TType.Stop)" << '\n';
    scope_up(out);
    out << indent() << "throw new TProtocolException(TProtocolException.INVALID_DATA);" << '\n';
    scope_down(out);

    // end of else for TStop
    scope_down(out);
    out << indent() << "await iprot.ReadStructEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';
    out << indent() << "return " << tmpRetval << ";" << '\n';
    indent_down();

    scope_down(out);
    out << indent() << "finally" << '\n';
    scope_up(out);
    out << indent() << "iprot.DecrementRecursionDepth();" << '\n';
    scope_down(out);

    out << indent() << "}" << '\n' << '\n';
}

void t_netstd_generator::generate_deserialize_field(ostream& out, t_field* tfield, string prefix, bool is_propertyless)
{
    t_type* type = tfield->get_type();
    type = resolve_typedef( type);

    if (type->is_void())
    {
        throw "CANNOT GENERATE DESERIALIZE CODE FOR void TYPE: " + prefix + tfield->get_name();
    }

    string name = prefix + (is_propertyless ? "" : prop_name(tfield));

    if (type->is_struct() || type->is_xception())
    {
        generate_deserialize_struct(out, static_cast<t_struct*>(type), name);
    }
    else if (type->is_container())
    {
        generate_deserialize_container(out, type, name);
    }
    else if (type->is_base_type() || type->is_enum())
    {
        out << indent() << name << " = ";

        if (type->is_enum())
        {
            out << "(" << type_name(type) << ")";
        }

        out << "await iprot.";

        if (type->is_base_type())
        {
            t_base_type::t_base tbase = static_cast<t_base_type*>(type)->get_base();
            switch (tbase)
            {
            case t_base_type::TYPE_VOID:
                throw "compiler error: cannot serialize void field in a struct: " + name;
                break;
            case t_base_type::TYPE_STRING:
                if (type->is_binary())
                {
                    out << "ReadBinaryAsync(" << CANCELLATION_TOKEN_NAME << ");";
                }
                else
                {
                    out << "ReadStringAsync(" << CANCELLATION_TOKEN_NAME << ");";
                }
                break;
            case t_base_type::TYPE_UUID:
                out << "ReadUuidAsync(" << CANCELLATION_TOKEN_NAME << ");";
                break;
            case t_base_type::TYPE_BOOL:
                out << "ReadBoolAsync(" << CANCELLATION_TOKEN_NAME << ");";
                break;
            case t_base_type::TYPE_I8:
                out << "ReadByteAsync(" << CANCELLATION_TOKEN_NAME << ");";
                break;
            case t_base_type::TYPE_I16:
                out << "ReadI16Async(" << CANCELLATION_TOKEN_NAME << ");";
                break;
            case t_base_type::TYPE_I32:
                out << "ReadI32Async(" << CANCELLATION_TOKEN_NAME << ");";
                break;
            case t_base_type::TYPE_I64:
                out << "ReadI64Async(" << CANCELLATION_TOKEN_NAME << ");";
                break;
            case t_base_type::TYPE_DOUBLE:
                out << "ReadDoubleAsync(" << CANCELLATION_TOKEN_NAME << ");";
                break;
            default:
                throw "compiler error: no C# name for base type " + t_base_type::t_base_name(tbase);
            }
        }
        else if (type->is_enum())
        {
            out << "ReadI32Async(" << CANCELLATION_TOKEN_NAME << ");";
        }
        out << '\n';
    }
    else
    {
        printf("DO NOT KNOW HOW TO DESERIALIZE FIELD '%s' TYPE '%s'\n", tfield->get_name().c_str(), type_name(type).c_str());
    }
}

void t_netstd_generator::generate_deserialize_struct(ostream& out, t_struct* tstruct, string prefix)
{
    if (is_union_enabled() && tstruct->is_union())
    {
        out << indent() << prefix << " = await " << type_name(tstruct) << ".ReadAsync(iprot, " << CANCELLATION_TOKEN_NAME << ");" << '\n';
    }
    else
    {
        out << indent() << prefix << " = new " << type_name(tstruct) << "();" << '\n'
            << indent() << "await " << prefix << ".ReadAsync(iprot, " << CANCELLATION_TOKEN_NAME << ");" << '\n';
    }
}

void t_netstd_generator::generate_deserialize_container(ostream& out, t_type* ttype, string prefix)
{
    out << indent() << "{" << '\n';
    indent_up();

    string obj;

    if (ttype->is_map())
    {
        obj = tmp("_map");
    }
    else if (ttype->is_set())
    {
        obj = tmp("_set");
    }
    else if (ttype->is_list())
    {
        obj = tmp("_list");
    }

    if (ttype->is_map())
    {
        out << indent() << "var " << obj << " = await iprot.ReadMapBeginAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';
    }
    else if (ttype->is_set())
    {
        out << indent() << "var " << obj << " = await iprot.ReadSetBeginAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';
    }
    else if (ttype->is_list())
    {
        out << indent() << "var " << obj << " = await iprot.ReadListBeginAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';
    }

    out << indent() << prefix << " = new " << type_name(ttype) << "(" << obj << ".Count);" << '\n';
    string i = tmp("_i");
    out << indent() << "for(int " << i << " = 0; " << i << " < " << obj << ".Count; ++" << i << ")" << '\n'
        << indent() << "{" << '\n';
    indent_up();

    if (ttype->is_map())
    {
        generate_deserialize_map_element(out, static_cast<t_map*>(ttype), prefix);
    }
    else if (ttype->is_set())
    {
        generate_deserialize_set_element(out, static_cast<t_set*>(ttype), prefix);
    }
    else if (ttype->is_list())
    {
        generate_deserialize_list_element(out, static_cast<t_list*>(ttype), prefix);
    }

    indent_down();
    out << indent() << "}" << '\n';

    if (ttype->is_map())
    {
        out << indent() << "await iprot.ReadMapEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';
    }
    else if (ttype->is_set())
    {
        out << indent() << "await iprot.ReadSetEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';
    }
    else if (ttype->is_list())
    {
        out << indent() << "await iprot.ReadListEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';
    }

    indent_down();
    out << indent() << "}" << '\n';
}

void t_netstd_generator::generate_deserialize_map_element(ostream& out, t_map* tmap, string prefix)
{
    string key = tmp("_key");
    string val = tmp("_val");

    t_field fkey(tmap->get_key_type(), key);
    t_field fval(tmap->get_val_type(), val);

    out << indent() << declare_field(&fkey, false, false) << '\n';
    out << indent() << declare_field(&fval, false, false) << '\n';

    generate_deserialize_field(out, &fkey);
    generate_deserialize_field(out, &fval);

    out << indent() << prefix << "[" << key << "] = " << val << ";" << '\n';
}

void t_netstd_generator::generate_deserialize_set_element(ostream& out, t_set* tset, string prefix)
{
    string elem = tmp("_elem");
    t_field felem(tset->get_elem_type(), elem);

    out << indent() << declare_field(&felem, false, false) << '\n';

    generate_deserialize_field(out, &felem);

    out << indent() << prefix << ".Add(" << elem << ");" << '\n';
}

void t_netstd_generator::generate_deserialize_list_element(ostream& out, t_list* tlist, string prefix)
{
    string elem = tmp("_elem");
    t_field felem(tlist->get_elem_type(), elem);

    out << indent() << declare_field(&felem, false, false) << '\n';

    generate_deserialize_field(out, &felem);

    out << indent() << prefix << ".Add(" << elem << ");" << '\n';
}

void t_netstd_generator::generate_serialize_field(ostream& out, t_field* tfield, string prefix, bool is_propertyless, bool allow_nullable)
{
    t_type* type = tfield->get_type();
    type = resolve_typedef( type);

    string name = prefix + (is_propertyless ? "" : prop_name(tfield));
    string nullable_name = name + (allow_nullable ? nullable_value_access(type) : "");

    if (type->is_void())
    {
        throw "CANNOT GENERATE SERIALIZE CODE FOR void TYPE: " + name;
    }

    if (type->is_struct() || type->is_xception())
    {
        generate_serialize_struct(out, static_cast<t_struct*>(type), name);
    }
    else if (type->is_container())
    {
        generate_serialize_container(out, type, name);
    }
    else if (type->is_base_type() || type->is_enum())
    {
        out << indent() << "await oprot.";

        if (type->is_base_type())
        {
            t_base_type::t_base tbase = static_cast<t_base_type*>(type)->get_base();
            switch (tbase)
            {
            case t_base_type::TYPE_VOID:
                throw "compiler error: cannot serialize void field in a struct: " + name;
            case t_base_type::TYPE_STRING:
                if (type->is_binary())
                {
                    out << "WriteBinaryAsync(";
                }
                else
                {
                    out << "WriteStringAsync(";
                }
                out << name << ", " << CANCELLATION_TOKEN_NAME << ");";
                break;
            case t_base_type::TYPE_UUID:
                out << "WriteUuidAsync(" << nullable_name << ", " << CANCELLATION_TOKEN_NAME << ");";
                break;
            case t_base_type::TYPE_BOOL:
                out << "WriteBoolAsync(" << nullable_name << ", " << CANCELLATION_TOKEN_NAME << ");";
                break;
            case t_base_type::TYPE_I8:
                out << "WriteByteAsync(" << nullable_name << ", " << CANCELLATION_TOKEN_NAME << ");";
                break;
            case t_base_type::TYPE_I16:
                out << "WriteI16Async(" << nullable_name << ", " << CANCELLATION_TOKEN_NAME << ");";
                break;
            case t_base_type::TYPE_I32:
                out << "WriteI32Async(" << nullable_name << ", " << CANCELLATION_TOKEN_NAME << ");";
                break;
            case t_base_type::TYPE_I64:
                out << "WriteI64Async(" << nullable_name << ", " << CANCELLATION_TOKEN_NAME << ");";
                break;
            case t_base_type::TYPE_DOUBLE:
                out << "WriteDoubleAsync(" << nullable_name << ", " << CANCELLATION_TOKEN_NAME << ");";
                break;
            default:
                throw "compiler error: no C# name for base type " + t_base_type::t_base_name(tbase);
            }
        }
        else if (type->is_enum())
        {
            out << "WriteI32Async((int)" << name << ", " << CANCELLATION_TOKEN_NAME << ");";
        }
        out << '\n';
    }
    else
    {
        printf("DO NOT KNOW HOW TO SERIALIZE '%s%s' TYPE '%s'\n", prefix.c_str(), tfield->get_name().c_str(), type_name(type).c_str());
    }
}

void t_netstd_generator::generate_serialize_struct(ostream& out, t_struct* tstruct, string prefix)
{
    (void)tstruct;
    out << indent() << "await " << prefix << ".WriteAsync(oprot, " << CANCELLATION_TOKEN_NAME << ");" << '\n';
}

void t_netstd_generator::generate_serialize_container(ostream& out, t_type* ttype, string prefix)
{
    if (ttype->is_map())
    {
        out << indent() << "await oprot.WriteMapBeginAsync(new TMap(" << type_to_enum(static_cast<t_map*>(ttype)->get_key_type())
            << ", " << type_to_enum(static_cast<t_map*>(ttype)->get_val_type()) << ", " << prefix
            << ".Count), " << CANCELLATION_TOKEN_NAME << ");" << '\n';
    }
    else if (ttype->is_set())
    {
        out << indent() << "await oprot.WriteSetBeginAsync(new TSet(" << type_to_enum(static_cast<t_set*>(ttype)->get_elem_type())
            << ", " << prefix << ".Count), " << CANCELLATION_TOKEN_NAME << ");" << '\n';
    }
    else if (ttype->is_list())
    {
        out << indent() << "await oprot.WriteListBeginAsync(new TList("
            << type_to_enum(static_cast<t_list*>(ttype)->get_elem_type()) << ", " << prefix << ".Count), " << CANCELLATION_TOKEN_NAME << ");"
            << '\n';
    }

    string iter = tmp("_iter");
    if (ttype->is_map())
    {
        out << indent() << "foreach (" << type_name(static_cast<t_map*>(ttype)->get_key_type()) << " " << iter
            << " in " << prefix << ".Keys)";
    }
    else if (ttype->is_set())
    {
        out << indent() << "foreach (" << type_name(static_cast<t_set*>(ttype)->get_elem_type()) << " " << iter
            << " in " << prefix << ")";
    }
    else if (ttype->is_list())
    {
        out << indent() << "foreach (" << type_name(static_cast<t_list*>(ttype)->get_elem_type()) << " " << iter
            << " in " << prefix << ")";
    }

    out << '\n';
    out << indent() << "{" << '\n';
    indent_up();

    if (ttype->is_map())
    {
        generate_serialize_map_element(out, static_cast<t_map*>(ttype), iter, prefix);
    }
    else if (ttype->is_set())
    {
        generate_serialize_set_element(out, static_cast<t_set*>(ttype), iter);
    }
    else if (ttype->is_list())
    {
        generate_serialize_list_element(out, static_cast<t_list*>(ttype), iter);
    }

    indent_down();
    out << indent() << "}" << '\n';

    if (ttype->is_map())
    {
        out << indent() << "await oprot.WriteMapEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';
    }
    else if (ttype->is_set())
    {
        out << indent() << "await oprot.WriteSetEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';
    }
    else if (ttype->is_list())
    {
        out << indent() << "await oprot.WriteListEndAsync(" << CANCELLATION_TOKEN_NAME << ");" << '\n';
    }
}

void t_netstd_generator::generate_serialize_map_element(ostream& out, t_map* tmap, string iter, string map)
{
    t_field kfield(tmap->get_key_type(), iter);
    generate_serialize_field(out, &kfield, "", false, false);
    t_field vfield(tmap->get_val_type(), map + "[" + iter + "]");
    generate_serialize_field(out, &vfield, "", false, false);
}

void t_netstd_generator::generate_serialize_set_element(ostream& out, t_set* tset, string iter)
{
    t_field efield(tset->get_elem_type(), iter);
    generate_serialize_field(out, &efield, "", false, false);
}

void t_netstd_generator::generate_serialize_list_element(ostream& out, t_list* tlist, string iter)
{
    t_field efield(tlist->get_elem_type(), iter);
    generate_serialize_field(out, &efield, "", false, false);
}

void t_netstd_generator::generate_property(ostream& out, t_field* tfield, bool isPublic, bool generateIsset)
{
    generate_netstd_property(out, tfield, isPublic, generateIsset, "_");
}

void t_netstd_generator::generate_netstd_property(ostream& out, t_field* tfield, bool isPublic, bool generateIsset, string fieldPrefix)
{
    if ((is_serialize_enabled() || is_wcf_enabled()) && isPublic)
    {
        out << indent() << "[DataMember(Order = 0)]" << '\n';
    }
    generate_deprecation_attribute(out, tfield->annotations_);

    out << indent()
        << (isPublic ? "public " : "private ")
        << type_name(tfield->get_type())
        << nullable_field_suffix(tfield)
        << " "
        << prop_name(tfield)
        ;

    bool is_required = field_is_required(tfield);
    if (is_required)
    {
        out << " { get; set; }";
        if( (target_net_version >= 6) && (!force_member_nullable(tfield))) {
            out << initialize_field(tfield) << ";";
        }
        out << '\n';
    }
    else
    {
        out << '\n'
            << indent() << "{" << '\n';
        indent_up();

        out << indent() << "get" << '\n'
            << indent() << "{" << '\n';
        indent_up();

        out << indent() << "return " << fieldPrefix + tfield->get_name() << ";" << '\n';
        indent_down();
        out << indent() << "}" << '\n'
            << indent() << "set" << '\n'
            << indent() << "{" << '\n';
        indent_up();

        if (generateIsset)
        {
            out << indent() << "__isset." << get_isset_name(normalize_name(tfield->get_name())) << " = true;" << '\n';
        }
        out << indent() << "this." << fieldPrefix + tfield->get_name() << " = value;" << '\n';

        indent_down();
        out << indent() << "}" << '\n';
        indent_down();
        out << indent() << "}" << '\n';
    }
    out << '\n';
}

string t_netstd_generator::make_csharp_string_literal( string const& value)
{
  if (value.length() == 0) {
    return "";
  }

  std::stringstream result;
  result << "\"";
  for (signed char const c: value) {
    if( (c >= 0) && (c < 32)) {  // convert ctrl chars, but leave UTF-8 alone
      int width = std::max( (int)sizeof(c), 4);
      result << "\\x" << std::hex << std::setw(width) << std::setfill('0') << (int)c;
    } else if ((c == '\\') || (c == '"')) {
      result << "\\" << c;
    } else {
      result << c;   // anything else "as is"
    }
  }
  result << "\"";

  return result.str();
}


string t_netstd_generator::make_valid_csharp_identifier(string const& fromName)
{
    string str = fromName;
    if (str.empty())
    {
        return str;
    }

    // tests rely on this
    assert(('A' < 'Z') && ('a' < 'z') && ('0' < '9'));

    // if the first letter is a number, we add an additional underscore in front of it
    char c = str.at(0);
    if (('0' <= c) && (c <= '9'))
    {
        str = "_" + str;
    }

    // following chars: letter, number or underscore
    for (size_t i = 0; i < str.size(); ++i)
    {
        c = str.at(i);
        if (('A' > c || c > 'Z') && ('a' > c || c > 'z') && ('0' > c || c > '9') && '_' != c)
        {
            str.replace(i, 1, "_");
        }
    }

    return str;
}

void t_netstd_generator::cleanup_member_name_mapping(void* scope)
{
    if (member_mapping_scopes.empty())
    {
        throw "internal error: cleanup_member_name_mapping() no scope active";
    }

    member_mapping_scope& active = member_mapping_scopes.back();
    if (active.scope_member != scope)
    {
        throw "internal error: cleanup_member_name_mapping() called for wrong struct";
    }

    member_mapping_scopes.pop_back();
}

string t_netstd_generator::get_mapped_member_name(string name)
{
    if (!member_mapping_scopes.empty())
    {
        member_mapping_scope& active = member_mapping_scopes.back();
        map<string, string>::iterator iter = active.mapping_table.find(name);
        if (active.mapping_table.end() != iter)
        {
            return iter->second;
        }
    }

    pverbose("no mapping for member %s\n", name.c_str());
    return name;
}

void t_netstd_generator::prepare_member_name_mapping(t_service* tservice)
{
    prepare_member_name_mapping(tservice, tservice->get_functions(), tservice->get_name());
}

void t_netstd_generator::prepare_member_name_mapping(t_struct* tstruct)
{
    prepare_member_name_mapping(tstruct, tstruct->get_members(), tstruct->get_name());
}

void t_netstd_generator::prepare_member_name_mapping(t_struct* scope, const vector<t_field*>& members, const string& structname)
{
    // begin new scope
    member_mapping_scopes.emplace_back();
    member_mapping_scope& active = member_mapping_scopes.back();
    active.scope_member = scope;

    // current C# generator policy:
    // - prop names are always rendered with an Uppercase first letter
    // - struct names are used as given
    std::set<string> used_member_names;
    vector<t_field*>::const_iterator iter;

    // prevent name conflicts with struct (CS0542 error + THRIFT-2942)
    used_member_names.insert(structname);
    used_member_names.insert("Isset");
    used_member_names.insert("Read");
    used_member_names.insert("Write");

    for (iter = members.begin(); iter != members.end(); ++iter)
    {
        string oldname = (*iter)->get_name();
        string newname = prop_name(*iter, true);
        while (true)
        {
            // new name conflicts with another member
            if (used_member_names.find(newname) != used_member_names.end())
            {
                pverbose("struct %s: member %s conflicts with another member\n", structname.c_str(), newname.c_str());
                newname += '_';
                continue;
            }

            // add always, this helps us to detect edge cases like
            // different spellings ("foo" and "Foo") within the same struct
            pverbose("struct %s: member mapping %s => %s\n", structname.c_str(), oldname.c_str(), newname.c_str());
            active.mapping_table[oldname] = newname;
            used_member_names.insert(newname);
            break;
        }
    }
}


void t_netstd_generator::prepare_member_name_mapping(t_service* scope, const vector<t_function*>& members, const string& structname)
{
    // begin new scope
    member_mapping_scopes.emplace_back();
    member_mapping_scope& active = member_mapping_scopes.back();
    active.scope_member = scope;

    // current C# generator policy:
    // - prop names are always rendered with an Uppercase first letter
    // - struct names are used as given
    std::set<string> used_member_names;
    vector<t_function*>::const_iterator iter;

    // prevent name conflicts with service/intf
    used_member_names.insert(structname);
    used_member_names.insert("Client");
    used_member_names.insert("IAsync");
    used_member_names.insert("AsyncProcessor");
    used_member_names.insert("InternalStructs");

    for (iter = members.begin(); iter != members.end(); ++iter)
    {
        string oldname = (*iter)->get_name();
        string newname = func_name(*iter, true);
        while (true)
        {
            // new name conflicts with another method
            if (used_member_names.find(newname) != used_member_names.end())
            {
                pverbose("service %s: method %s conflicts with another method\n", structname.c_str(), newname.c_str());
                newname += '_';
                continue;
            }

            // add always, this helps us to detect edge cases like
            // different spellings ("foo" and "Foo") within the same service
            pverbose("service %s: method mapping %s => %s\n", structname.c_str(), oldname.c_str(), newname.c_str());
            active.mapping_table[oldname] = newname;
            used_member_names.insert(newname);
            break;
        }
    }
}


string t_netstd_generator::convert_to_pascal_case(const string& str) {
  string out;
  bool must_capitalize = true;
  bool first_character = true;
  for (auto it = str.begin(); it != str.end(); ++it) {
    if (std::isalnum(*it)) {
      if (must_capitalize) {
        out.append(1, (char)::toupper(*it));
        must_capitalize = false;
      } else {
        out.append(1, *it);
      }
    } else {
      if (first_character) //this is a private variable and should not be PascalCased
        return str;
      must_capitalize = true;
    }
    first_character = false;
  }
  return out;
}


string t_netstd_generator::get_isset_name(const string& str) {
  return ("Isset" != str) ? str : str + "_";
}


string t_netstd_generator::prop_name(t_field* tfield, bool suppress_mapping) {
  string name(tfield->get_name());
  if (suppress_mapping) {
    name[0] = toupper(name[0]);
    if (use_pascal_case_properties)
      name = t_netstd_generator::convert_to_pascal_case(name);
  } else {
    name = get_mapped_member_name(name);
  }

  return name;
}

string t_netstd_generator::func_name(t_function* tfunc, bool suppress_mapping) {
  return func_name(tfunc->get_name(), suppress_mapping);
}

string t_netstd_generator::func_name(std::string fname, bool suppress_mapping) {
  if (suppress_mapping) {
    return fname;
  }

  return get_mapped_member_name(fname);
}

bool t_netstd_generator::is_nullable_type(t_type* ttype) {
  ttype = resolve_typedef(ttype);

  if (ttype->is_enum()) {
    return false;
  }

  if (ttype->is_base_type()) {
    t_base_type::t_base tbase = static_cast<t_base_type*>(ttype)->get_base();
    switch (tbase)
    {
    case t_base_type::TYPE_STRING:
      return true;  // both binary and string
    default:
      return false;
    }
  }

  return true;
}


string t_netstd_generator::nullable_suffix() {
  if(target_net_version >= 6) {
    return "?";
  } else {
    return "";
  }
}


string t_netstd_generator::nullable_field_suffix(t_field* tfield) {
  if(field_is_required(tfield) && (!force_member_nullable(tfield)))
    return "";
  else
    return nullable_field_suffix(tfield->get_type());
}


string t_netstd_generator::nullable_field_suffix(t_type* ttype) {
  if( target_net_version < 6) {
    return "";
  }

  ttype = resolve_typedef(ttype);

  if (ttype->is_enum()) {
    return "";
  }

  if (ttype->is_base_type()) {
    t_base_type::t_base tbase = static_cast<t_base_type*>(ttype)->get_base();
    switch (tbase)
    {
    case t_base_type::TYPE_STRING:
      return nullable_suffix();
    default:
      return "";
    }
  }

  return nullable_suffix();
}

string t_netstd_generator::nullable_value_access(t_type* ttype) {
  if( target_net_version < 6)
    return "";

  ttype = resolve_typedef(ttype);

  // this code uses the null-forgiving operator and therefore assumes that the variable
  // has been properly checked against an isset guard or null
  if (ttype->is_base_type()) {
    t_base_type::t_base tbase = static_cast<t_base_type*>(ttype)->get_base();
    switch (tbase)
    {
    case t_base_type::TYPE_STRING:
      return "!";
    default:
      return "";
    }
  }

  if (ttype->is_container() || ttype->is_struct() || ttype->is_xception()) {
    return "!";
  }

  return "";
}

bool t_netstd_generator::force_member_nullable(t_field* tfield) {
  // IMPORTANT:
  // If tfield is a struct that contains a required field of the same type (directly or indirectly),
  // auto-initializing such a member field would immediately produce an OOM, or at least unexpectedly
  // allocate potentially large amounts of memory -> ALWAYS leave containers and struct members nullable
  t_type* ttype = resolve_typedef(tfield->get_type());
  return ttype->is_struct() || ttype->is_container();
}

string t_netstd_generator::type_name(t_type* ttype, bool with_namespace)
{
    ttype = resolve_typedef(ttype);

    if (ttype->is_base_type())
    {
        return base_type_name(static_cast<t_base_type*>(ttype));
    }

    if (ttype->is_map())
    {
        t_map* tmap = static_cast<t_map*>(ttype);
        return "Dictionary<" + type_name(tmap->get_key_type()) + ", " + type_name(tmap->get_val_type()) + ">";
    }

    if (ttype->is_set())
    {
        t_set* tset = static_cast<t_set*>(ttype);
        return "HashSet<" + type_name(tset->get_elem_type()) + ">";
    }

    if (ttype->is_list())
    {
        t_list* tlist = static_cast<t_list*>(ttype);
        return "List<" + type_name(tlist->get_elem_type()) + ">";
    }

    string the_name = normalize_name(ttype->get_name());

    if(with_namespace)
    {
        t_program* program = ttype->get_program();
        if (program != nullptr)// && program != program_)
        {
            string ns =  program->get_namespace("netstd");
            if (!ns.empty())
            {
                return "global::" + ns + "." + the_name;
            }
        }
    }

    return the_name;
}

string t_netstd_generator::base_type_name(t_base_type* tbase)
{
    switch (tbase->get_base())
    {
    case t_base_type::TYPE_VOID:
        return "void";
    case t_base_type::TYPE_STRING:
        if (tbase->is_binary()) {
            return "byte[]";
        } else {
            return "string";
        }
    case t_base_type::TYPE_UUID:
        return "global::System.Guid";
    case t_base_type::TYPE_BOOL:
        return "bool";
    case t_base_type::TYPE_I8:
        return "sbyte";
    case t_base_type::TYPE_I16:
        return "short";
    case t_base_type::TYPE_I32:
        return "int";
    case t_base_type::TYPE_I64:
        return "long";
    case t_base_type::TYPE_DOUBLE:
        return "double";
    default:
        throw "compiler error: no C# name for base type " + t_base_type::t_base_name(tbase->get_base());
    }
}

string t_netstd_generator::get_deep_copy_method_call(t_type* ttype, bool is_not_null, bool& needs_typecast, string& suffix)
{
    ttype = resolve_typedef(ttype);

    // if is_not_null is set, then the surrounding code already explicitly tests against != null
    string null_check("");

    suffix = "";
    needs_typecast = false;
    if (ttype->is_base_type())
    {
        t_base_type::t_base tbase = static_cast<t_base_type*>(ttype)->get_base();
        switch (tbase)
        {
        case t_base_type::TYPE_STRING:
            if (ttype->is_binary()) {
                suffix = nullable_suffix();
                if( target_net_version >= 8) {
                    null_check = is_not_null ? "!" : " ?? []";
                }
                else if( target_net_version >= 6) {
                    null_check = is_not_null ? "!" : " ?? Array.Empty<byte>()";
                }
                return ".ToArray()" + null_check;
            } else {
                if( target_net_version >= 6) {
                    null_check = is_not_null ? "!" : " ?? string.Empty";
                }
                return null_check;  // simple assignment will do, strings are immutable in C#
            }
            break;
        default:
            return "";  // simple assignment will do
        }
    }
    else if (ttype->is_enum())
    {
        return "";  // simple assignment will do
    }
    else if (is_union_enabled() && ttype->is_struct() && static_cast<t_struct*>(ttype)->is_union())
    {
        needs_typecast = (! ttype->is_container());
        suffix = nullable_suffix();
        if( target_net_version >= 6) {
            null_check = is_not_null ? "!" : " ?? new "+ttype->get_name() +".___undefined()";
        }
        return "." + DEEP_COPY_METHOD_NAME + "()" + null_check;
    }
    else
    {
        needs_typecast = (! ttype->is_container());
        suffix = nullable_suffix();
        if( (target_net_version >= 8) && ttype->is_container()) {
            null_check = is_not_null ? "!" : " ?? []";
        } else if( target_net_version >= 6) {
            null_check = is_not_null ? "!" : " ?? new()";
        }
        return "." + DEEP_COPY_METHOD_NAME + "()" + null_check;
    }

    throw "UNEXPECTED TYPE IN get_deep_copy_method_call: " + ttype->get_name();
}

string t_netstd_generator::declare_field(t_field* tfield, bool init, bool allow_nullable, string prefix)
{
    string result = type_name(tfield->get_type())
                  + (allow_nullable ? nullable_field_suffix(tfield) : "")
                  + " "
                  + prefix + tfield->get_name()
                  ;
    if (init)
    {
        result += initialize_field(tfield);
    }

    return result + ";";
}

string t_netstd_generator::initialize_field(t_field* tfield)
{
    t_type* ttype = tfield->get_type();
    ttype = resolve_typedef(ttype);

    if (ttype->is_base_type() && field_has_default(tfield))
    {
        std::ofstream dummy;
        return " = " + render_const_value(dummy, tfield->get_name(), ttype, tfield->get_value());
    }
    else if (force_member_nullable(tfield))
    {
        return "";  // see force_member_nullable() why this is necessary
    }
    else if (ttype->is_base_type())
    {
        t_base_type::t_base tbase = static_cast<t_base_type*>(ttype)->get_base();
        switch (tbase)
        {
        case t_base_type::TYPE_VOID:
            throw "NO T_VOID CONSTRUCT";
        case t_base_type::TYPE_STRING:
            if((target_net_version >= 6) && field_is_required(tfield)) {
                if (ttype->is_binary()) {
                    return target_net_version >= 8 ? "= []" : " = Array.Empty<byte>()";
                } else {
                    return " = string.Empty";
                }
            } else {
                return " = null";
            }
            break;
        case t_base_type::TYPE_UUID:
            return " = System.Guid.Empty";
            break;
        case t_base_type::TYPE_BOOL:
            return " = false";
            break;
        case t_base_type::TYPE_I8:
        case t_base_type::TYPE_I16:
        case t_base_type::TYPE_I32:
        case t_base_type::TYPE_I64:
            return " = 0";
            break;
        case t_base_type::TYPE_DOUBLE:
            return " = 0.0";
            break;
        }
    }
    else if (ttype->is_enum())
    {
        return " = default";
    }
    else if (ttype->is_container())
    {
        if(target_net_version >= 6) {
            return " = new()";
        } else {
            return " = new " + type_name(ttype) + "()";
        }
    }
    else if (ttype->is_struct())
    {
        t_struct* tstruct = static_cast<t_struct*>(ttype);
        if(target_net_version >= 6) {
            if(tstruct->is_union()) {
                return " = new " + type_name(ttype) + ".___undefined()";
            } else {
                return " = new()";
            }
        } else {
            return " = new " + type_name(ttype) + "()";
        }
    }

    throw "UNEXPECTED TYPE IN initialize_field: " + ttype->get_name();
}

string t_netstd_generator::function_signature(t_function* tfunction, string prefix)
{
    t_type* ttype = tfunction->get_returntype();
    return type_name(ttype) + " " + func_name(normalize_name(prefix + tfunction->get_name())) + "(" + argument_list(tfunction->get_arglist()) + ")";
}

string t_netstd_generator::function_signature_async(t_function* tfunction, string prefix, int mode)
{
    t_type* ttype = tfunction->get_returntype();
    string task = "global::System.Threading.Tasks.Task";
    if ((!ttype->is_void()) && ((mode & MODE_NO_RETURN) == 0))
    {
        task += "<" + type_name(ttype) + ">";
    }

    string result = task + " " + func_name(normalize_name(prefix + tfunction->get_name()) + (add_async_postfix ? "Async" : "")) + "(";
    if((mode & MODE_NO_ARGS) == 0) {
        string args = argument_list(tfunction->get_arglist());
        result += args;
        if (!args.empty())
        {
            result += ", ";
        }
    }
    result += "CancellationToken " + CANCELLATION_TOKEN_NAME + " = default)";

    return result;
}

string t_netstd_generator::argument_list(t_struct* tstruct, bool with_types)
{
    string result = "";
    const vector<t_field*>& fields = tstruct->get_members();
    vector<t_field*>::const_iterator f_iter;
    bool first = true;
    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
    {
        if (first)
        {
            first = false;
        }
        else
        {
            result += ", ";
        }

        if( with_types) {
            result += type_name((*f_iter)->get_type()) + nullable_field_suffix(*f_iter) + " ";
        }

        result += normalize_name((*f_iter)->get_name(),true);
    }
    return result;
}

string t_netstd_generator::type_to_enum(t_type* type)
{
    type = resolve_typedef( type);

    if (type->is_base_type())
    {
        t_base_type::t_base tbase = static_cast<t_base_type*>(type)->get_base();
        switch (tbase)
        {
        case t_base_type::TYPE_VOID:
            throw "NO T_VOID CONSTRUCT";
        case t_base_type::TYPE_STRING:
            return "TType.String";
        case t_base_type::TYPE_UUID:
            return "TType.Uuid";
        case t_base_type::TYPE_BOOL:
            return "TType.Bool";
        case t_base_type::TYPE_I8:
            return "TType.Byte";
        case t_base_type::TYPE_I16:
            return "TType.I16";
        case t_base_type::TYPE_I32:
            return "TType.I32";
        case t_base_type::TYPE_I64:
            return "TType.I64";
        case t_base_type::TYPE_DOUBLE:
            return "TType.Double";
        }
    }
    else if (type->is_enum())
    {
        return "TType.I32";
    }
    else if (type->is_struct() || type->is_xception())
    {
        return "TType.Struct";
    }
    else if (type->is_map())
    {
        return "TType.Map";
    }
    else if (type->is_set())
    {
        return "TType.Set";
    }
    else if (type->is_list())
    {
        return "TType.List";
    }

    throw "INVALID TYPE IN type_to_enum: " + type->get_name();
}

void t_netstd_generator::generate_netstd_docstring_comment(ostream& out, string contents)
{
    docstring_comment(out, "/// <summary>" + string("\n"), "/// ", contents, "/// </summary>" + string("\n"));
}

void t_netstd_generator::generate_netstd_doc(ostream& out, t_field* field)
{
    if (field->get_type()->is_enum())
    {
        string combined_message = field->get_doc() + "\n" + "<seealso cref=\"" + get_enum_class_name(field->get_type()) + "\"/>";
        generate_netstd_docstring_comment(out, combined_message);
    }
    else
    {
        generate_netstd_doc(out, static_cast<t_doc*>(field));
    }
}

void t_netstd_generator::generate_netstd_doc(ostream& out, t_doc* tdoc)
{
    if (tdoc->has_doc())
    {
        generate_netstd_docstring_comment(out, tdoc->get_doc());
    }
}

void t_netstd_generator::generate_netstd_doc(ostream& out, t_function* tfunction)
{
    if (tfunction->has_doc())
    {
        stringstream ps;
        const vector<t_field*>& fields = tfunction->get_arglist()->get_members();
        vector<t_field*>::const_iterator p_iter;
        for (p_iter = fields.begin(); p_iter != fields.end(); ++p_iter)
        {
            t_field* p = *p_iter;
            ps << '\n' << "<param name=\"" << p->get_name() << "\">";
            if (p->has_doc())
            {
                string str = p->get_doc();
                str.erase(remove(str.begin(), str.end(), '\n'), str.end());
                ps << str;
            }
            ps << "</param>";
        }

        docstring_comment(out,
                                   "",
                                   "/// ",
                                   "<summary>" + string("\n") + tfunction->get_doc() + "</summary>" + ps.str(),
                                   "");
    }
}

void t_netstd_generator::docstring_comment(ostream& out, const string& comment_start, const string& line_prefix, const string& contents, const string& comment_end)
{
    if (comment_start != "")
    {
        out << indent() << comment_start;
    }

    stringstream docs(contents, std::ios_base::in);

    while (!(docs.eof() || docs.fail()))
    {
        char line[1024];
        docs.getline(line, 1024);

        // Just prnt a newline when the line & prefix are empty.
        if (strlen(line) == 0 && line_prefix == "" && !docs.eof())
        {
            out << '\n';
        }
        else if (strlen(line) > 0 || !docs.eof())
        { // skip the empty last line
            out << indent() << line_prefix << line << '\n';
        }
    }
    if (comment_end != "")
    {
        out << indent() << comment_end;
    }
}

string t_netstd_generator::get_enum_class_name(t_type* type)
{
    string package = "";
    t_program* program = type->get_program();
    if (program != nullptr) // && program != program_)
    {
        package = program->get_namespace("netstd") + ".";
    }
    return "global::" + package + type->get_name();
}

std::string t_netstd_generator::display_name() const {
  return "C#";
}


THRIFT_REGISTER_GENERATOR(
    netstd,
    "C#",
    "    wcf:             Adds bindings for WCF to generated classes.\n"
    "    serial:          Add serialization support to generated classes.\n"
    "    union:           Use new union typing, which includes a static read function for union types.\n"
    "    pascal:          Generate Pascal Case property names according to Microsoft naming convention.\n"
    "    net6:            Enable features that require net6 and C# 8 or higher.\n"
    "    net8:            Enable features that require net8 and C# 12 or higher.\n"
    "    no_deepcopy:     Suppress generation of " + DEEP_COPY_METHOD_NAME + "() method.\n"
    "    async_postfix:   Append \"Async\" to all service methods (maintains compatibility with existing code).\n"
)
