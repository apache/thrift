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
    suppress_deepcopy = false;
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
        else {
          throw "unknown option netstd:" + iter->first;
        }
    }

    out_dir_base_ = "gen-netstd";
}

static string correct_function_name_for_async(string const& function_name)
{
    string const async_end = "Async";
    size_t i = function_name.find(async_end);
    if (i != string::npos)
    {
        return function_name + async_end;
    }

    return function_name;
}

/**
* \brief Search and replace "_args" substring in struct name if exist (for C# class naming)
* \param struct_name
* \return Modified struct name ("Struct_args" -> "StructArgs") or original name
*/
static string check_and_correct_struct_name(const string& struct_name)
{
    string args_end = "_args";
    size_t i = struct_name.find(args_end);
    if (i != string::npos)
    {
        string new_struct_name = struct_name;
        new_struct_name.replace(i, args_end.length(), "Args");
        return new_struct_name;
    }

    string result_end = "_result";
    size_t j = struct_name.find(result_end);
    if (j != string::npos)
    {
        string new_struct_name = struct_name;
        new_struct_name.replace(j, result_end.length(), "Result");
        return new_struct_name;
    }

    return struct_name;
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

map<string, int> t_netstd_generator::get_keywords_list() const
{
    return netstd_keywords;
}

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
    init_keywords();

    while (!member_mapping_scopes.empty())
    {
        cleanup_member_name_mapping(member_mapping_scopes.back().scope_member);
    }

    pverbose(".NET Standard options:\n");
    pverbose("- union ......... %s\n", (is_union_enabled() ? "ON" : "off"));
    pverbose("- serialize ..... %s\n", (is_serialize_enabled() ? "ON" : "off"));
    pverbose("- wcf ........... %s\n", (is_wcf_enabled() ? "ON" : "off"));
    pverbose("- pascal ........ %s\n", (use_pascal_case_properties ? "ON" : "off"));
    pverbose("- no_deepcopy ... %s\n", (suppress_deepcopy ? "ON" : "off"));
}

string t_netstd_generator::normalize_name(string name)
{
    string tmp(name);
    transform(tmp.begin(), tmp.end(), tmp.begin(), static_cast<int(*)(int)>(tolower));

    // un-conflict keywords by prefixing with "@"
    if (netstd_keywords.find(tmp) != netstd_keywords.end())
    {
        return "@" + name;
    }

    // no changes necessary
    return name;
}

void t_netstd_generator::init_keywords()
{
    netstd_keywords.clear();

    // C# keywords
    netstd_keywords["abstract"] = 1;
    netstd_keywords["as"] = 1;
    netstd_keywords["base"] = 1;
    netstd_keywords["bool"] = 1;
    netstd_keywords["break"] = 1;
    netstd_keywords["byte"] = 1;
    netstd_keywords["case"] = 1;
    netstd_keywords["catch"] = 1;
    netstd_keywords["char"] = 1;
    netstd_keywords["checked"] = 1;
    netstd_keywords["class"] = 1;
    netstd_keywords["const"] = 1;
    netstd_keywords["continue"] = 1;
    netstd_keywords["decimal"] = 1;
    netstd_keywords["default"] = 1;
    netstd_keywords["delegate"] = 1;
    netstd_keywords["do"] = 1;
    netstd_keywords["double"] = 1;
    netstd_keywords["else"] = 1;
    netstd_keywords["enum"] = 1;
    netstd_keywords["event"] = 1;
    netstd_keywords["explicit"] = 1;
    netstd_keywords["extern"] = 1;
    netstd_keywords["false"] = 1;
    netstd_keywords["finally"] = 1;
    netstd_keywords["fixed"] = 1;
    netstd_keywords["float"] = 1;
    netstd_keywords["for"] = 1;
    netstd_keywords["foreach"] = 1;
    netstd_keywords["goto"] = 1;
    netstd_keywords["if"] = 1;
    netstd_keywords["implicit"] = 1;
    netstd_keywords["in"] = 1;
    netstd_keywords["int"] = 1;
    netstd_keywords["interface"] = 1;
    netstd_keywords["internal"] = 1;
    netstd_keywords["is"] = 1;
    netstd_keywords["lock"] = 1;
    netstd_keywords["long"] = 1;
    netstd_keywords["namespace"] = 1;
    netstd_keywords["new"] = 1;
    netstd_keywords["null"] = 1;
    netstd_keywords["object"] = 1;
    netstd_keywords["operator"] = 1;
    netstd_keywords["out"] = 1;
    netstd_keywords["override"] = 1;
    netstd_keywords["params"] = 1;
    netstd_keywords["private"] = 1;
    netstd_keywords["protected"] = 1;
    netstd_keywords["public"] = 1;
    netstd_keywords["readonly"] = 1;
    netstd_keywords["ref"] = 1;
    netstd_keywords["return"] = 1;
    netstd_keywords["sbyte"] = 1;
    netstd_keywords["sealed"] = 1;
    netstd_keywords["short"] = 1;
    netstd_keywords["sizeof"] = 1;
    netstd_keywords["stackalloc"] = 1;
    netstd_keywords["static"] = 1;
    netstd_keywords["string"] = 1;
    netstd_keywords["struct"] = 1;
    netstd_keywords["switch"] = 1;
    netstd_keywords["this"] = 1;
    netstd_keywords["throw"] = 1;
    netstd_keywords["true"] = 1;
    netstd_keywords["try"] = 1;
    netstd_keywords["typeof"] = 1;
    netstd_keywords["uint"] = 1;
    netstd_keywords["ulong"] = 1;
    netstd_keywords["unchecked"] = 1;
    netstd_keywords["unsafe"] = 1;
    netstd_keywords["ushort"] = 1;
    netstd_keywords["using"] = 1;
    netstd_keywords["virtual"] = 1;
    netstd_keywords["void"] = 1;
    netstd_keywords["volatile"] = 1;
    netstd_keywords["while"] = 1;

    // C# contextual keywords
    netstd_keywords["add"] = 1;
    netstd_keywords["alias"] = 1;
    netstd_keywords["ascending"] = 1;
    netstd_keywords["async"] = 1;
    netstd_keywords["await"] = 1;
    netstd_keywords["descending"] = 1;
    netstd_keywords["dynamic"] = 1;
    netstd_keywords["from"] = 1;
    netstd_keywords["get"] = 1;
    netstd_keywords["global"] = 1;
    netstd_keywords["group"] = 1;
    netstd_keywords["into"] = 1;
    netstd_keywords["join"] = 1;
    netstd_keywords["let"] = 1;
    netstd_keywords["orderby"] = 1;
    netstd_keywords["partial"] = 1;
    netstd_keywords["remove"] = 1;
    netstd_keywords["select"] = 1;
    netstd_keywords["set"] = 1;
    netstd_keywords["value"] = 1;
    netstd_keywords["var"] = 1;
    netstd_keywords["where"] = 1;
    netstd_keywords["yield"] = 1;

    netstd_keywords["when"] = 1;
}

void t_netstd_generator::reset_indent() {
  while( indent_count() > 0) { 
    indent_down(); 
  }
}


void t_netstd_generator::start_netstd_namespace(ostream& out)
{
    out << "#pragma warning disable IDE0079  // remove unnecessary pragmas" << endl
        << "#pragma warning disable IDE1006  // parts of the code use IDL spelling" << endl
        << endl;

    if (!namespace_name_.empty())
    {
        out << "namespace " << namespace_name_ << endl;
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

    return namespaces + endl;
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

    return namespaces + endl;
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
    out << autogen_comment() << endl;

    start_netstd_namespace(out);
    generate_netstd_doc(out, tenum);

    out << indent() << "public enum " << tenum->get_name() << endl;
    scope_up(out);

    vector<t_enum_value*> constants = tenum->get_constants();
    vector<t_enum_value*>::iterator c_iter;

    for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter)
    {
        generate_netstd_doc(out, *c_iter);
        int value = (*c_iter)->get_value();
        out << indent() << (*c_iter)->get_name() << " = " << value << "," << endl;
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
    out << autogen_comment() << netstd_type_usings() << endl;

    start_netstd_namespace(out);

    out << indent() << "public static class " << make_valid_csharp_identifier(program_name_) << "Constants" << endl;

    scope_up(out);

    vector<t_const*>::iterator c_iter;
    bool need_static_constructor = false;
    for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter)
    {
        generate_netstd_doc(out, *c_iter);
        if (print_const_value(out, (*c_iter)->get_name(), (*c_iter)->get_type(), (*c_iter)->get_value(), false))
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
            out << indent() << name << "." << prop_name(field) << " = " << val << ";" << endl;
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
            out << indent() << name << "[" << key << "]" << " = " << val << ";" << endl;
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
            out << indent() << name << ".Add(" << val << ");" << endl;
        }
    }
}

void t_netstd_generator::print_const_constructor(ostream& out, vector<t_const*> consts)
{
    out << indent() << "static " << make_valid_csharp_identifier(program_name_).c_str() << "Constants()" << endl;
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
        out << (in_static ? "" : type->is_base_type() ? "public const " : "public static ") << type_name(type) << " ";
    }

    if (type->is_base_type())
    {
        string v2 = render_const_value(out, name, type, value);
        out << normalize_name(name) << " = " << v2 << ";" << endl;
        need_static_construction = false;
    }
    else if (type->is_enum())
    {
        out << name << " = " << type_name(type) << "." << value->get_identifier_name() << ";" << endl;
        need_static_construction = false;
    }
    else if (type->is_struct() || type->is_xception())
    {
        out << name << " = new " << type_name(type) << "();" << endl;
    }
    else if (type->is_map())
    {
        out << name << " = new " << type_name(type) << "();" << endl;
    }
    else if (type->is_list() || type->is_set())
    {
        out << name << " = new " << type_name(type) << "();" << endl;
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
        render << type->get_name() << "." << value->get_identifier_name();
    }
    else
    {
        string t = tmp("tmp");
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
    
    if (ttype->is_map() || ttype->is_set() || ttype->is_list())
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
    out << autogen_comment() << netstd_type_usings() << endl;

    start_netstd_namespace(out);

    out << indent() << "public static class " << make_valid_csharp_identifier(program_name_) << "Extensions" << endl;
    scope_up(out);

    bool needs_typecast = false;
    std::map<string,t_type*>::const_iterator iter;
    for( iter = types.begin(); iter != types.end(); ++iter)
    {
        out << indent() << "public static bool Equals(this " << iter->first << " instance, object that)" << endl;
        scope_up(out);
        out << indent() << "if (!(that is " << iter->first << " other)) return false;" << endl;
        out << indent() << "if (ReferenceEquals(instance, other)) return true;" << endl;
        out << endl;
        out << indent() << "return TCollections.Equals(instance, other);" << endl;
        scope_down(out);
        out << endl << endl;
        
        out << indent() << "public static int GetHashCode(this " << iter->first << " instance)" << endl;
        scope_up(out);
        out << indent() << "return TCollections.GetHashCode(instance);" << endl;
        scope_down(out);
        out << endl << endl;

        if(! suppress_deepcopy) {
            out << indent() << "public static " << iter->first << " " << DEEP_COPY_METHOD_NAME << "(this " << iter->first << " source)" << endl;
            scope_up(out);
            out << indent() << "if (source == null)" << endl;
            indent_up();
            out << indent() << "return null;" << endl << endl;
            indent_down();

            string tmp_instance = tmp("tmp");
            out << indent() << "var " << tmp_instance << " = new " << iter->first << "(source.Count);" << endl;
            if( iter->second->is_map())
            {
                t_map* tmap = static_cast<t_map*>(iter->second);
                string copy_key = get_deep_copy_method_call(tmap->get_key_type(), needs_typecast);
                string copy_val = get_deep_copy_method_call(tmap->get_val_type(), needs_typecast);
                bool null_key = type_can_be_null(tmap->get_key_type());
                bool null_val = type_can_be_null(tmap->get_val_type());
                
                out << indent() << "foreach (var pair in source)" << endl;
                indent_up();
                out << indent() << tmp_instance << ".Add(";
                if( null_key)
                {
                    out << "(pair.Key != null) ? pair.Key" << copy_key << " : null";
                } else {
                    out << "pair.Key" << copy_key;
                }
                out << ", ";
                if( null_val)
                {
                    out << "(pair.Value != null) ? pair.Value" << copy_val << " : null";
                } else {
                    out << "pair.Value" << copy_val;
                }
                out << ");" << endl;
                indent_down();
                
            } else if( iter->second->is_set() || iter->second->is_list()) {
                string copy_elm;
                bool null_elm = false;
                if (iter->second->is_set())
                {
                    t_set* tset = static_cast<t_set*>(iter->second);
                    copy_elm = get_deep_copy_method_call(tset->get_elem_type(), needs_typecast);
                    null_elm = type_can_be_null(tset->get_elem_type());
                }
                else // list
                {
                    t_list* tlist = static_cast<t_list*>(iter->second);
                    copy_elm = get_deep_copy_method_call(tlist->get_elem_type(), needs_typecast);
                    null_elm = type_can_be_null(tlist->get_elem_type());
                }

                out << indent() << "foreach (var elem in source)" << endl;
                indent_up();
                out << indent() << tmp_instance << ".Add(";
                if( null_elm)
                {
                    out << "(elem != null) ? elem" << copy_elm << " : null";
                } else {
                    out << "elem" << copy_elm;
                }
                out << ");" << endl;
                indent_down();
            }

            out << indent() << "return " << tmp_instance << ";" << endl;
            scope_down(out);
            out << endl << endl;
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
    f_struct << autogen_comment() << netstd_type_usings() << netstd_thrift_usings() << endl;

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

    out << endl;

    generate_netstd_doc(out, tstruct);
    collect_extensions_types(tstruct);    
    prepare_member_name_mapping(tstruct);

    if ((is_serialize_enabled() || is_wcf_enabled()) && !is_exception)
    {
        out << indent() << "[DataContract(Namespace=\"" << wcf_namespace_ << "\")]" << endl;
    }

    bool is_final = tstruct->annotations_.find("final") != tstruct->annotations_.end();

    string sharp_struct_name = check_and_correct_struct_name(normalize_name(tstruct->get_name()));

    out << indent() << "public " << (is_final ? "sealed " : "") << "partial class " << sharp_struct_name << " : ";

    if (is_exception)
    {
        out << "TException, ";
    }

    out << "TBase" << endl
        << indent() << "{" << endl;
    indent_up();

    const vector<t_field*>& members = tstruct->get_members();
    vector<t_field*>::const_iterator m_iter;

    // make private members with public Properties
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter)
    {
        // if the field is required, then we use auto-properties
        if (!field_is_required((*m_iter)))
        {
            out << indent() << "private " << declare_field(*m_iter, false, "_") << endl;
        }
    }
    out << endl;

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
        out << endl;
        if (is_serialize_enabled() || is_wcf_enabled())
        {
            out << indent() << "[DataMember(Order = 1)]" << endl;
        }
        out << indent() << "public Isset __isset;" << endl;
        if (is_serialize_enabled() || is_wcf_enabled())
        {
            out << indent() << "[DataContract]" << endl;
        }

        out << indent() << "public struct Isset" << endl
            << indent() << "{" << endl;
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
                    out << indent() << "[DataMember]" << endl;
                }
                out << indent() << "public bool " << get_isset_name(normalize_name((*m_iter)->get_name())) << ";" << endl;
            }
        }

        indent_down();
        out << indent() << "}" << endl << endl;

        if (generate_isset && (is_serialize_enabled() || is_wcf_enabled()))
        {
            out << indent() << "#region XmlSerializer support" << endl << endl;

            for (m_iter = members.begin(); m_iter != members.end(); ++m_iter)
            {
                bool is_required = field_is_required(*m_iter);
                // if it is required, don't need Isset for that variable
                // if it is not required, if it has a default value, we need to generate Isset
                if (!is_required)
                {
                    out << indent() << "public bool ShouldSerialize" << prop_name(*m_iter) << "()" << endl
                        << indent() << "{" << endl;
                    indent_up();
                    out << indent() << "return __isset." << get_isset_name(normalize_name((*m_iter)->get_name())) << ";" << endl;
                    indent_down();
                    out << indent() << "}" << endl << endl;
                }
            }

            out << indent() << "#endregion XmlSerializer support" << endl << endl;
        }
    }

    // We always want a default, no argument constructor for Reading
    out << indent() << "public " << sharp_struct_name << "()" << endl
        << indent() << "{" << endl;
    indent_up();

    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter)
    {
        t_type* t = (*m_iter)->get_type();
        t = resolve_typedef(t);

        if ((*m_iter)->get_value() != nullptr)
        {
            if (field_is_required((*m_iter)))
            {
                print_const_value(out, "this." + prop_name(*m_iter), t, (*m_iter)->get_value(), true, true);
            }
            else
            {
                print_const_value(out, "this._" + (*m_iter)->get_name(), t, (*m_iter)->get_value(), true, true);
                // Optionals with defaults are marked set
                out << indent() << "this.__isset." << get_isset_name(normalize_name((*m_iter)->get_name())) << " = true;" << endl;
            }
        }
    }
    indent_down();
    out << indent() << "}" << endl << endl;

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
                out << type_name((*m_iter)->get_type()) << " " << normalize_name((*m_iter)->get_name());
            }
        }
        out << ") : this()" << endl
            << indent() << "{" << endl;
        indent_up();

        for (m_iter = members.begin(); m_iter != members.end(); ++m_iter)
        {
            if (field_is_required(*m_iter))
            {
                out << indent() << "this." << prop_name(*m_iter) << " = " << normalize_name((*m_iter)->get_name()) << ";" << endl;
            }
        }

        indent_down();
        out << indent() << "}" << endl << endl;
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
    out << indent() << "}" << endl << endl;

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
    out << endl;
    out << indent() << "[DataContract]" << endl;

    bool is_final = tstruct->annotations_.find("final") != tstruct->annotations_.end();

    out << indent() << "public " << (is_final ? "sealed " : "") << "partial class " << tstruct->get_name() << "Fault" << endl
        << indent() << "{" << endl;
    indent_up();

    const vector<t_field*>& members = tstruct->get_members();
    vector<t_field*>::const_iterator m_iter;

    // make private members with public Properties
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter)
    {
        // if the field is required, then we use auto-properties
        if (!field_is_required((*m_iter)))
        {
            out << indent() << "private " << declare_field(*m_iter, false, "_") << endl;
        }
    }
    out << endl;

    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter)
    {
        generate_property(out, *m_iter, true, false);
    }

    indent_down();
    out << indent() << "}" << endl << endl;
}

void t_netstd_generator::generate_netstd_deepcopy_method(ostream& out, t_struct* tstruct, std::string sharp_struct_name)
{
    if( suppress_deepcopy) {
        return;  // feature disabled
    }
        
    const vector<t_field*>& members = tstruct->get_members();
    vector<t_field*>::const_iterator m_iter;

    out << indent() << "public " << sharp_struct_name << " " << DEEP_COPY_METHOD_NAME << "()" << endl;
    out << indent() << "{" << endl;
    indent_up();

    // return directly if there are only required fields
    string tmp_instance = tmp("tmp");
    out << indent() << "var " << tmp_instance << " = new " << sharp_struct_name << "();" << endl;

    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
        bool needs_typecast = false;
        t_type* ttype = (*m_iter)->get_type();
        string copy_op = get_deep_copy_method_call(ttype, needs_typecast);
        
        bool is_required = field_is_required(*m_iter);
        generate_null_check_begin( out, *m_iter);

        out << indent() << tmp_instance << "." << prop_name(*m_iter) << " = ";
        if( needs_typecast) {
            out << "(" << type_name(ttype) << ")";
        }
        out << "this." << prop_name(*m_iter) << copy_op << ";" << endl;

        generate_null_check_end( out, *m_iter);
        if( !is_required) {
            out << indent() << tmp_instance << ".__isset." << get_isset_name(normalize_name((*m_iter)->get_name()))
                 << " = this.__isset." << get_isset_name(normalize_name((*m_iter)->get_name())) << ";" << endl;
        }
    }

    out << indent() << "return " << tmp_instance << ";" << endl;
    
    indent_down();
    out << indent() << "}" << endl << endl;
}

void t_netstd_generator::generate_netstd_struct_reader(ostream& out, t_struct* tstruct)
{
    out << indent() << "public async global::System.Threading.Tasks.Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)" << endl
        << indent() << "{" << endl;
    indent_up();
    out << indent() << "iprot.IncrementRecursionDepth();" << endl
        << indent() << "try" << endl
        << indent() << "{" << endl;
    indent_up();

    const vector<t_field*>& fields = tstruct->get_members();
    vector<t_field*>::const_iterator f_iter;

    // Required variables aren't in __isset, so we need tmp vars to check them
    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
    {
        if (field_is_required(*f_iter))
        {
            out << indent() << "bool isset_" << (*f_iter)->get_name() << " = false;" << endl;
        }
    }

    out << indent() << "TField field;" << endl
        << indent() << "await iprot.ReadStructBeginAsync(cancellationToken);" << endl
        << indent() << "while (true)" << endl
        << indent() << "{" << endl;
    indent_up();
    out << indent() << "field = await iprot.ReadFieldBeginAsync(cancellationToken);" << endl
        << indent() << "if (field.Type == TType.Stop)" << endl
        << indent() << "{" << endl;
    indent_up();
    out << indent() << "break;" << endl;
    indent_down();
    out << indent() << "}" << endl << endl
        << indent() << "switch (field.ID)" << endl
        << indent() << "{" << endl;
    indent_up();

    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
    {
        bool is_required = field_is_required(*f_iter);
        out << indent() << "case " << (*f_iter)->get_key() << ":" << endl;
        indent_up();
        out << indent() << "if (field.Type == " << type_to_enum((*f_iter)->get_type()) << ")" << endl
            << indent() << "{" << endl;
        indent_up();

        generate_deserialize_field(out, *f_iter);
        if (is_required)
        {
            out << indent() << "isset_" << (*f_iter)->get_name() << " = true;" << endl;
        }

        indent_down();
        out << indent() << "}" << endl
            << indent() << "else" << endl
            << indent() << "{" << endl;
        indent_up();
        out << indent() << "await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);" << endl;
        indent_down();
        out << indent() << "}" << endl
            << indent() << "break;" << endl;
        indent_down();
    }

    out << indent() << "default: " << endl;
    indent_up();
    out << indent() << "await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);" << endl
        << indent() << "break;" << endl;
    indent_down();
    indent_down();
    out << indent() << "}" << endl
        << endl
        << indent() << "await iprot.ReadFieldEndAsync(cancellationToken);" << endl;
    indent_down();
    out << indent() << "}" << endl
        << endl
        << indent() << "await iprot.ReadStructEndAsync(cancellationToken);" << endl;

    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
    {
        if (field_is_required((*f_iter)))
        {
            out << indent() << "if (!isset_" << (*f_iter)->get_name() << ")" << endl
                << indent() << "{" << endl;
            indent_up();
            out << indent() << "throw new TProtocolException(TProtocolException.INVALID_DATA);" << endl;
            indent_down();
            out << indent() << "}" << endl;
        }
    }

    indent_down();
    out << indent() << "}" << endl;
    out << indent() << "finally" << endl
        << indent() << "{" << endl;
    indent_up();
    out << indent() << "iprot.DecrementRecursionDepth();" << endl;
    indent_down();
    out << indent() << "}" << endl;
    indent_down();
    out << indent() << "}" << endl << endl;
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
        
        out << ")" << endl
            << indent() << "{" << endl;
        indent_up();
    }        
}


void t_netstd_generator::generate_null_check_end(ostream& out, t_field* tfield) {
    bool is_required = field_is_required(tfield);
    bool null_allowed = type_can_be_null(tfield->get_type());
    
    if( null_allowed || (!is_required)) {
        indent_down();
        out << indent() << "}" << endl;
    }
}

void t_netstd_generator::generate_netstd_struct_writer(ostream& out, t_struct* tstruct)
{
    out << indent() << "public async global::System.Threading.Tasks.Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)" << endl
        << indent() << "{" << endl;
    indent_up();

    out << indent() << "oprot.IncrementRecursionDepth();" << endl
        << indent() << "try" << endl
        << indent() << "{" << endl;
    indent_up();

    string name = tstruct->get_name();
    const vector<t_field*>& fields = tstruct->get_sorted_members();
    vector<t_field*>::const_iterator f_iter;

    out << indent() << "var struc = new TStruct(\"" << name << "\");" << endl
        << indent() << "await oprot.WriteStructBeginAsync(struc, cancellationToken);" << endl;

    if (fields.size() > 0)
    {
        out << indent() << "var field = new TField();" << endl;
        for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
        {
            generate_null_check_begin( out, *f_iter);
            out << indent() << "field.Name = \"" << (*f_iter)->get_name() << "\";" << endl
                << indent() << "field.Type = " << type_to_enum((*f_iter)->get_type()) << ";" << endl
                << indent() << "field.ID = " << (*f_iter)->get_key() << ";" << endl
                << indent() << "await oprot.WriteFieldBeginAsync(field, cancellationToken);" << endl;

            generate_serialize_field(out, *f_iter);

            out << indent() << "await oprot.WriteFieldEndAsync(cancellationToken);" << endl;
            generate_null_check_end(out, *f_iter);
        }
    }

    out << indent() << "await oprot.WriteFieldStopAsync(cancellationToken);" << endl
        << indent() << "await oprot.WriteStructEndAsync(cancellationToken);" << endl;
    indent_down();
    out << indent() << "}" << endl
        << indent() << "finally" << endl
        << indent() << "{" << endl;
    indent_up();
    out << indent() << "oprot.DecrementRecursionDepth();" << endl;
    indent_down();
    out << indent() << "}" << endl;
    indent_down();
    out << indent() << "}" << endl << endl;
}

void t_netstd_generator::generate_netstd_struct_result_writer(ostream& out, t_struct* tstruct)
{
    out << indent() << "public async global::System.Threading.Tasks.Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)" << endl
        << indent() << "{" << endl;
    indent_up();

    out << indent() << "oprot.IncrementRecursionDepth();" << endl
        << indent() << "try" << endl
        << indent() << "{" << endl;
    indent_up();

    string name = tstruct->get_name();
    const vector<t_field*>& fields = tstruct->get_sorted_members();
    vector<t_field*>::const_iterator f_iter;

    out << indent() << "var struc = new TStruct(\"" << name << "\");" << endl
        << indent() << "await oprot.WriteStructBeginAsync(struc, cancellationToken);" << endl;

    if (fields.size() > 0)
    {
        out << indent() << "var field = new TField();" << endl;
        bool first = true;
        for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
        {
            if (first)
            {
                first = false;
                out << endl << indent() << "if";
            }
            else
            {
                out << indent() << "else if";
            }

            out << "(this.__isset." << get_isset_name(normalize_name((*f_iter)->get_name())) << ")" << endl
                << indent() << "{" << endl;
            indent_up();

            bool null_allowed = type_can_be_null((*f_iter)->get_type());
            if (null_allowed)
            {
                out << indent() << "if (" << prop_name(*f_iter) << " != null)" << endl
                    << indent() << "{" << endl;
                indent_up();
            }

            out << indent() << "field.Name = \"" << prop_name(*f_iter) << "\";" << endl
                << indent() << "field.Type = " << type_to_enum((*f_iter)->get_type()) << ";" << endl
                << indent() << "field.ID = " << (*f_iter)->get_key() << ";" << endl
                << indent() << "await oprot.WriteFieldBeginAsync(field, cancellationToken);" << endl;

            generate_serialize_field(out, *f_iter);

            out << indent() << "await oprot.WriteFieldEndAsync(cancellationToken);" << endl;

            if (null_allowed)
            {
                indent_down();
                out << indent() << "}" << endl;
            }

            indent_down();
            out << indent() << "}" << endl;
        }
    }

    out << indent() << "await oprot.WriteFieldStopAsync(cancellationToken);" << endl
        << indent() << "await oprot.WriteStructEndAsync(cancellationToken);" << endl;
    indent_down();
    out << indent() << "}" << endl
        << indent() << "finally" << endl
        << indent() << "{" << endl;
    indent_up();
    out << indent() << "oprot.DecrementRecursionDepth();" << endl;
    indent_down();
    out << indent() << "}" << endl;
    indent_down();
    out << indent() << "}" << endl << endl;
}

void t_netstd_generator::generate_netstd_struct_tostring(ostream& out, t_struct* tstruct)
{
    out << indent() << "public override string ToString()" << endl
        << indent() << "{" << endl;
    indent_up();
    out << indent() << "var sb = new StringBuilder(\"" << tstruct->get_name() << "(\");" << endl;

    const vector<t_field*>& fields = tstruct->get_members();
    vector<t_field*>::const_iterator f_iter;

    bool useFirstFlag = false;
    string tmp_count = tmp("tmp");
    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
    {
        if (!field_is_required((*f_iter)))
        {
            out << indent() << "int " << tmp_count.c_str() << " = 0;" << endl;
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
            out << indent() << "if(0 < " << tmp_count.c_str() << (is_required ? "" : "++") << ") { sb.Append(\", \"); }" << endl;
            out << indent() << "sb.Append(\"" << prop_name(*f_iter) << ": \");" << endl;
        }
        else
        {
            out << indent() << "sb.Append(\", " << prop_name(*f_iter) << ": \");" << endl;
        }

        out << indent() << prop_name(*f_iter) << ".ToString(sb);" << endl;

        generate_null_check_end(out, *f_iter);
        if (is_required) {
            had_required = true; // now __count must be > 0, so we don't need to check it anymore
        }
    }

    out << indent() << "sb.Append(')');" << endl
        << indent() << "return sb.ToString();" << endl;
    indent_down();
    out << indent() << "}" << endl;
}

void t_netstd_generator::generate_netstd_union(t_struct* tunion)
{
    int ic = indent_count();

    string f_union_name = namespace_dir_ + "/" + (tunion->get_name()) + ".cs";
    ofstream_with_content_based_conditional_update f_union;

    f_union.open(f_union_name.c_str());

    reset_indent();
    f_union << autogen_comment() << netstd_type_usings() << netstd_thrift_usings() << endl;

    generate_netstd_union_definition(f_union, tunion);

    f_union.close();

    indent_validate(ic, "generate_netstd_union.");
}

void t_netstd_generator::generate_netstd_union_definition(ostream& out, t_struct* tunion)
{
    // Let's define the class first
    start_netstd_namespace(out);

    out << indent() << "public abstract partial class " << tunion->get_name() << " : TUnionBase" << endl;
    out << indent() << "{" << endl;
    indent_up();

    out << indent() << "public abstract global::System.Threading.Tasks.Task WriteAsync(TProtocol tProtocol, CancellationToken cancellationToken);" << endl
        << indent() << "public readonly int Isset;" << endl
        << indent() << "public abstract object Data { get; }" << endl
        << indent() << "protected " << tunion->get_name() << "(int isset)" << endl
        << indent() << "{" << endl;
    indent_up();
    out << indent() << "Isset = isset;" << endl;
    indent_down();
    out << indent() << "}" << endl << endl;

    const vector<t_field*>& fields = tunion->get_members();
    vector<t_field*>::const_iterator f_iter;

    out << indent() << "public override bool Equals(object that)" << endl;
    scope_up(out);
    out << indent() << "if (!(that is " << tunion->get_name() << " other)) return false;" << endl;
    out << indent() << "if (ReferenceEquals(this, other)) return true;" << endl;
    out << endl;
    out << indent() << "if(this.Isset != other.Isset) return false;" << endl;
    out << endl;
    out << indent() << "switch (Isset)" << endl;
    scope_up(out);
    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
    {
        bool needs_typecast = false;
        string copy_op = get_deep_copy_method_call((*f_iter)->get_type(), needs_typecast);
        out << indent() << "case " << (*f_iter)->get_key() << ":" << endl;
        indent_up();
        out << indent() << "return Equals(As_" << (*f_iter)->get_name() << ", other.As_" << (*f_iter)->get_name() << ");" << endl;
        indent_down();
    }                
    out << indent() << "default:" << endl;
    indent_up();
    out << indent() << "return true;" << endl;
    indent_down();
    indent_down();
    scope_down(out);
    scope_down(out);
    out << endl;

    out << indent() << "public override int GetHashCode()" << endl;
    out << indent() << "{" << endl;
    indent_up();
    out << indent() << "switch (Isset)" << endl;
    out << indent() << "{" << endl;
    indent_up();    
    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
    {
        bool needs_typecast = false;
        string copy_op = get_deep_copy_method_call((*f_iter)->get_type(), needs_typecast);
        out << indent() << "case " << (*f_iter)->get_key() << ":" << endl;
        indent_up();
        out << indent() << "return As_" << (*f_iter)->get_name() << ".GetHashCode();" << endl;
        indent_down();
    }                
    out << indent() << "default:" << endl;
    indent_up();
    out << indent() << "return (new ___undefined()).GetHashCode();" << endl;
    indent_down();
    indent_down();
    out << indent() << "}" << endl;
    indent_down();
    out << indent() << "}" << endl << endl;

    if( ! suppress_deepcopy) {
        out << indent() << "public " << tunion->get_name() << " DeepCopy()" << endl;
        out << indent() << "{" << endl;
        indent_up();
        out << indent() << "switch (Isset)" << endl;
        out << indent() << "{" << endl;
        indent_up();    
        for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
        {
            bool needs_typecast = false;
            string copy_op = get_deep_copy_method_call((*f_iter)->get_type(), needs_typecast);
            out << indent() << "case " << (*f_iter)->get_key() << ":" << endl;
            indent_up();
            out << indent() << "return new " << (*f_iter)->get_name() << "(As_" << (*f_iter)->get_name() << copy_op << ");" << endl;
            indent_down();
        }                
        out << indent() << "default:" << endl;
        indent_up();
        out << indent() << "return new ___undefined();" << endl;
        indent_down();
        indent_down();
        out << indent() << "}" << endl;
        indent_down();
        out << indent() << "}" << endl << endl;
    }

    out << indent() << "public class ___undefined : " << tunion->get_name() << endl;
    out << indent() << "{" << endl;
    indent_up();

    out << indent() << "public override object Data { get { return null; } }" << endl
        << indent() << "public ___undefined() : base(0) {}" << endl << endl;
        
    if( ! suppress_deepcopy) {
        out << indent() << "public new ___undefined DeepCopy()" << endl;
        out << indent() << "{" << endl;
        indent_up();
        out << indent() << "return new ___undefined();" << endl;
        indent_down();
        out << indent() << "}" << endl << endl;
    }

    t_struct undefined_struct(program_,"___undefined");
    generate_netstd_struct_equals(out, &undefined_struct);
    generate_netstd_struct_hashcode(out, &undefined_struct);
    
    out << indent() << "public override global::System.Threading.Tasks.Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)" << endl
        << indent() << "{" << endl;
    indent_up();
    out << indent() << "throw new TProtocolException( TProtocolException.INVALID_DATA, \"Cannot persist an union type which is not set.\");" << endl;
    indent_down();
    out << indent() << "}" << endl << endl;
    indent_down();
    out << indent() << "}" << endl << endl;

    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
    {
        generate_netstd_union_class(out, tunion, (*f_iter));
    }

    generate_netstd_union_reader(out, tunion);

    indent_down();
    out << indent() << "}" << endl << endl;

    end_netstd_namespace(out);
}

void t_netstd_generator::generate_netstd_union_class(ostream& out, t_struct* tunion, t_field* tfield)
{
    out << indent() << "public " << type_name(tfield->get_type()) << " As_" << tfield->get_name() << endl;
    out << indent() << "{" << endl;
    indent_up();
    out << indent() << "get" << endl;
    out << indent() << "{" << endl;
    indent_up();
    out << indent() << "return (" << tfield->get_key() << " == Isset) ? (" << type_name(tfield->get_type()) << ")Data : default(" << type_name(tfield->get_type()) << ");" << endl;
    indent_down();
    out << indent() << "}" << endl;
    indent_down();
    out << indent() << "}" << endl
        << endl;
    
    
    out << indent() << "public class " << tfield->get_name() << " : " << tunion->get_name() << endl;
    out << indent() << "{" << endl;
    indent_up();

    out << indent() << "private " << type_name(tfield->get_type()) << " _data;" << endl
        << indent() << "public override object Data { get { return _data; } }" << endl
        << indent() << "public " << tfield->get_name() << "(" << type_name(tfield->get_type()) << " data) : base("<< tfield->get_key() <<")" << endl
        << indent() << "{" << endl;
    indent_up();
    out << indent() << "this._data = data;" << endl;
    indent_down();
    out << indent() << "}" << endl;

    if( ! suppress_deepcopy) {
        out << indent() << "public new " << tfield->get_name() << " DeepCopy()" << endl;
        out << indent() << "{" << endl;
        indent_up();
        bool needs_typecast = false;
        string copy_op = get_deep_copy_method_call(tfield->get_type(), needs_typecast);
        out << indent() << "return new " << tfield->get_name() << "(_data" << copy_op << ");" << endl;
        indent_down();
        out << indent() << "}" << endl << endl;
    }

    out << indent() << "public override bool Equals(object that)" << endl;
    out << indent() << "{" << endl;
    indent_up();
    out << indent() << "if (!(that is " << tunion->get_name() << " other)) return false;" << endl;
    out << indent() << "if (ReferenceEquals(this, other)) return true;" << endl;
    out << endl;
    out << indent() << "return Equals( _data, other.As_" << tfield->get_name() << ");" << endl;
    indent_down();
    out << indent() << "}" << endl << endl;

    out << indent() << "public override int GetHashCode()" << endl;
    out << indent() << "{" << endl;
    indent_up();
    out << indent() << "return _data.GetHashCode();" << endl;
    indent_down();
    out << indent() << "}" << endl << endl;

    out << indent() << "public override async global::System.Threading.Tasks.Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken) {" << endl;
    indent_up();

    out << indent() << "oprot.IncrementRecursionDepth();" << endl
        << indent() << "try" << endl
        << indent() << "{" << endl;
    indent_up();

    out << indent() << "var struc = new TStruct(\"" << tunion->get_name() << "\");" << endl
        << indent() << "await oprot.WriteStructBeginAsync(struc, cancellationToken);" << endl;

    out << indent() << "var field = new TField();" << endl
        << indent() << "field.Name = \"" << tfield->get_name() << "\";" << endl
        << indent() << "field.Type = " << type_to_enum(tfield->get_type()) << ";" << endl
        << indent() << "field.ID = " << tfield->get_key() << ";" << endl
        << indent() << "await oprot.WriteFieldBeginAsync(field, cancellationToken);" << endl;

    generate_serialize_field(out, tfield, "_data", true);

    out << indent() << "await oprot.WriteFieldEndAsync(cancellationToken);" << endl
        << indent() << "await oprot.WriteFieldStopAsync(cancellationToken);" << endl
        << indent() << "await oprot.WriteStructEndAsync(cancellationToken);" << endl;
    indent_down();
    out << indent() << "}" << endl
        << indent() << "finally" << endl
        << indent() << "{" << endl;
    indent_up();
    out << indent() << "oprot.DecrementRecursionDepth();" << endl;
    indent_down();
    out << indent() << "}" << endl;
    indent_down();
    out << indent() << "}" << endl;
    indent_down();
    out << indent() << "}" << endl << endl;
}

void t_netstd_generator::generate_netstd_struct_equals(ostream& out, t_struct* tstruct)
{
    out << indent() << "public override bool Equals(object that)" << endl
        << indent() << "{" << endl;
    indent_up();
    out << indent() << "if (!(that is " << check_and_correct_struct_name(normalize_name(tstruct->get_name())) << " other)) return false;" << endl
        << indent() << "if (ReferenceEquals(this, other)) return true;" << endl;


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
            out << endl;
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
            out << "System.Object.Equals(";
        }
        out << prop_name((*f_iter)) << ", other." << prop_name((*f_iter)) << ")";
        if (!field_is_required((*f_iter)))
        {
            out << ")))";
        }
    }
    if (first)
    {
        out << indent() << "return true;" << endl;
    }
    else
    {
        out << ";" << endl;
        indent_down();
    }

    indent_down();
    out << indent() << "}" << endl << endl;
}

void t_netstd_generator::generate_netstd_struct_hashcode(ostream& out, t_struct* tstruct)
{
    out << indent() << "public override int GetHashCode() {" << endl;
    indent_up();

    out << indent() << "int hashcode = 157;" << endl;
    out << indent() << "unchecked {" << endl;
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
            out << prop_name((*f_iter)) << ".GetHashCode()";
        }
        out << ";" << endl;

        generate_null_check_end(out, *f_iter);
    }

    indent_down();
    out << indent() << "}" << endl;
    out << indent() << "return hashcode;" << endl;

    indent_down();
    out << indent() << "}" << endl << endl;
}

void t_netstd_generator::generate_service(t_service* tservice)
{
    int ic = indent_count();

    string f_service_name = namespace_dir_ + "/" + service_name_ + ".cs";
    ofstream_with_content_based_conditional_update f_service;
    f_service.open(f_service_name.c_str());

    reset_indent();
    f_service << autogen_comment() << netstd_type_usings() << netstd_thrift_usings() << endl;

    start_netstd_namespace(f_service);

    f_service << indent() << "public partial class " << normalize_name(service_name_) << endl
              << indent() << "{" << endl;
    indent_up();

    generate_service_interface(f_service, tservice);
    generate_service_client(f_service, tservice);
    generate_service_server(f_service, tservice);
    generate_service_helpers(f_service, tservice);

    indent_down();
    f_service << indent() << "}" << endl;

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

    //out << endl << endl;

    generate_netstd_doc(out, tservice);

    if (is_wcf_enabled())
    {
        out << indent() << "[ServiceContract(Namespace=\"" << wcf_namespace_ << "\")]" << endl;
    }

    out << indent() << "public interface IAsync" << extends_iface << endl
        << indent() << "{" << endl;

    indent_up();
    vector<t_function*> functions = tservice->get_functions();
    vector<t_function*>::iterator f_iter;
    for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter)
    {
        generate_netstd_doc(out, *f_iter);

        // if we're using WCF, add the corresponding attributes
        if (is_wcf_enabled())
        {
            out << indent() << "[OperationContract]" << endl;

            const vector<t_field*>& xceptions = (*f_iter)->get_xceptions()->get_members();
            vector<t_field*>::const_iterator x_iter;
            for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter)
            {
                out << indent() << "[FaultContract(typeof(" + type_name((*x_iter)->get_type()) + "Fault))]" << endl;
            }
        }

        out << indent() << function_signature_async(*f_iter) << ";" << endl << endl;
    }
    indent_down();
    out << indent() << "}" << endl << endl;
}

void t_netstd_generator::generate_service_helpers(ostream& out, t_service* tservice)
{
    vector<t_function*> functions = tservice->get_functions();
    vector<t_function*>::iterator f_iter;

    out << indent() << "public class InternalStructs" << endl;
    out << indent() << "{" << endl;
    indent_up();

    for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter)
    {
        t_struct* ts = (*f_iter)->get_arglist();
        collect_extensions_types(ts);
        generate_netstd_struct_definition(out, ts, false, true);
        generate_function_helpers(out, *f_iter);
    }

    indent_down();
    out << indent() << "}" << endl << endl;
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

    out << endl;

    generate_netstd_doc(out, tservice);

    out << indent() << "public class Client : " << extends_client << "IAsync" << endl
        << indent() << "{" << endl;
    indent_up();

    out << indent() << "public Client(TProtocol protocol) : this(protocol, protocol)" << endl
        << indent() << "{" << endl
        << indent() << "}" << endl
        << endl
        << indent() << "public Client(TProtocol inputProtocol, TProtocol outputProtocol) : base(inputProtocol, outputProtocol)"
        << indent() << "{" << endl
        << indent() << "}" << endl;

    vector<t_function*> functions = tservice->get_functions();
    vector<t_function*>::const_iterator functions_iterator;

    for (functions_iterator = functions.begin(); functions_iterator != functions.end(); ++functions_iterator)
    {
        string function_name = correct_function_name_for_async((*functions_iterator)->get_name());

        // async
        out << indent() << "public async " << function_signature_async(*functions_iterator, "") << endl
            << indent() << "{" << endl;
        indent_up();

        string argsname = (*functions_iterator)->get_name() + "Args";

        out << indent() << "await OutputProtocol.WriteMessageBeginAsync(new TMessage(\"" << function_name
            << "\", TMessageType." << ((*functions_iterator)->is_oneway() ? "Oneway" : "Call") 
            << ", SeqId), cancellationToken);" << endl
            << indent() << endl
            << indent() << "var args = new InternalStructs." << argsname << "() {" << endl;
        indent_up();

        t_struct* arg_struct = (*functions_iterator)->get_arglist();
        collect_extensions_types(arg_struct);
        prepare_member_name_mapping(arg_struct);
        const vector<t_field*>& fields = arg_struct->get_members();
        vector<t_field*>::const_iterator fld_iter;

        for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter)
        {
            out << indent() << prop_name(*fld_iter) << " = " << normalize_name((*fld_iter)->get_name()) << "," << endl;
        }

        indent_down();
        out << indent() << "};" << endl;


        out << indent() << endl
            << indent() << "await args.WriteAsync(OutputProtocol, cancellationToken);" << endl
            << indent() << "await OutputProtocol.WriteMessageEndAsync(cancellationToken);" << endl
            << indent() << "await OutputProtocol.Transport.FlushAsync(cancellationToken);" << endl;

        if (!(*functions_iterator)->is_oneway())
        {
            string resultname = (*functions_iterator)->get_name() + "Result";
            t_struct noargs(program_);
            t_struct* xs = (*functions_iterator)->get_xceptions();
            collect_extensions_types(xs);
            prepare_member_name_mapping(xs, xs->get_members(), resultname);

            out << indent() << endl
                << indent() << "var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);" << endl
                << indent() << "if (msg.Type == TMessageType.Exception)" << endl
                << indent() << "{" << endl;
            indent_up();

            out << indent() << "var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);" << endl
                << indent() << "await InputProtocol.ReadMessageEndAsync(cancellationToken);" << endl
                << indent() << "throw x;" << endl;
            indent_down();

            out << indent() << "}" << endl
                << endl
                << indent() << "var result = new InternalStructs." << resultname << "();" << endl
                << indent() << "await result.ReadAsync(InputProtocol, cancellationToken);" << endl
                << indent() << "await InputProtocol.ReadMessageEndAsync(cancellationToken);" << endl;

            if (!(*functions_iterator)->get_returntype()->is_void())
            {
                out << indent() << "if (result.__isset.success)" << endl
                    << indent() << "{" << endl;
                indent_up();
                out << indent() << "return result.Success;" << endl;
                indent_down();
                out << indent() << "}" << endl;
            }

            const vector<t_field*>& xceptions = xs->get_members();
            vector<t_field*>::const_iterator x_iter;
            for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter)
            {
                out << indent() << "if (result.__isset." << get_isset_name(normalize_name((*x_iter)->get_name())) << ")" << endl
                    << indent() << "{" << endl;
                indent_up();
                out << indent() << "throw result." << prop_name(*x_iter) << ";" << endl;
                indent_down();
                out << indent() << "}" << endl;
            }

            if ((*functions_iterator)->get_returntype()->is_void())
            {
                out << indent() << "return;" << endl;
            }
            else
            {
                out << indent() << "throw new TApplicationException(TApplicationException.ExceptionType.MissingResult, \""
                    << function_name << " failed: unknown result\");" << endl;
            }

            cleanup_member_name_mapping((*functions_iterator)->get_xceptions());
            indent_down();
            out << indent() << "}" << endl << endl;
        }
        else
        {
            indent_down();
            out << indent() << "}" << endl;
        }
    }

    indent_down();
    out << indent() << "}" << endl << endl;
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

    out << indent() << "public class AsyncProcessor : " << extends_processor << "ITAsyncProcessor" << endl
        << indent() << "{" << endl;

    indent_up();

    out << indent() << "private readonly IAsync _iAsync;" << endl
        << indent() << "private readonly ILogger<AsyncProcessor> _logger;" << endl
        << endl
        << indent() << "public AsyncProcessor(IAsync iAsync, ILogger<AsyncProcessor> logger = default)";

    if (!extends.empty())
    {
        out << " : base(iAsync)";
    }

    out << endl
        << indent() << "{" << endl;
    indent_up();

    out << indent() << "_iAsync = iAsync ?? throw new ArgumentNullException(nameof(iAsync));" << endl;
    out << indent() << "_logger = logger;" << endl;
    for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter)
    {
        string function_name = (*f_iter)->get_name();
        out << indent() << "processMap_[\"" << correct_function_name_for_async(function_name) << "\"] = " << function_name << "_ProcessAsync;" << endl;
    }

    indent_down();
    out << indent() << "}" << endl
        << endl;

    if (extends.empty())
    {
        out << indent() << "protected delegate global::System.Threading.Tasks.Task ProcessFunction(int seqid, TProtocol iprot, TProtocol oprot, CancellationToken cancellationToken);" << endl;
    }

    if (extends.empty())
    {
        out << indent() << "protected Dictionary<string, ProcessFunction> processMap_ = new Dictionary<string, ProcessFunction>();" << endl;
    }

    out << endl;

    if (extends.empty())
    {
        out << indent() << "public async Task<bool> ProcessAsync(TProtocol iprot, TProtocol oprot)" << endl
            << indent() << "{" << endl;
        indent_up();
        out << indent() << "return await ProcessAsync(iprot, oprot, CancellationToken.None);" << endl;
        indent_down();
        out << indent() << "}" << endl << endl;

        out << indent() << "public async Task<bool> ProcessAsync(TProtocol iprot, TProtocol oprot, CancellationToken cancellationToken)" << endl;
    }
    else
    {
        out << indent() << "public new async Task<bool> ProcessAsync(TProtocol iprot, TProtocol oprot)" << endl
            << indent() << "{" << endl;
        indent_up();
        out << indent() << "return await ProcessAsync(iprot, oprot, CancellationToken.None);" << endl;
        indent_down();
        out << indent() << "}" << endl << endl;

        out << indent() << "public new async Task<bool> ProcessAsync(TProtocol iprot, TProtocol oprot, CancellationToken cancellationToken)" << endl;
    }

    out << indent() << "{" << endl;
    indent_up();
    out << indent() << "try" << endl
        << indent() << "{" << endl;
    indent_up();
    out << indent() << "var msg = await iprot.ReadMessageBeginAsync(cancellationToken);" << endl
        << endl
        << indent() << "processMap_.TryGetValue(msg.Name, out ProcessFunction fn);" << endl
        << endl
        << indent() << "if (fn == null)" << endl
        << indent() << "{" << endl;
    indent_up();
    out << indent() << "await TProtocolUtil.SkipAsync(iprot, TType.Struct, cancellationToken);" << endl
        << indent() << "await iprot.ReadMessageEndAsync(cancellationToken);" << endl
        << indent() << "var x = new TApplicationException (TApplicationException.ExceptionType.UnknownMethod, \"Invalid method name: '\" + msg.Name + \"'\");" << endl
        << indent() << "await oprot.WriteMessageBeginAsync(new TMessage(msg.Name, TMessageType.Exception, msg.SeqID), cancellationToken);" << endl
        << indent() << "await x.WriteAsync(oprot, cancellationToken);" << endl
        << indent() << "await oprot.WriteMessageEndAsync(cancellationToken);" << endl
        << indent() << "await oprot.Transport.FlushAsync(cancellationToken);" << endl
        << indent() << "return true;" << endl;
    indent_down();
    out << indent() << "}" << endl
        << endl
        << indent() << "await fn(msg.SeqID, iprot, oprot, cancellationToken);" << endl
        << endl;
    indent_down();
    out << indent() << "}" << endl;
    out << indent() << "catch (IOException)" << endl
        << indent() << "{" << endl;
    indent_up();
    out << indent() << "return false;" << endl;
    indent_down();
    out << indent() << "}" << endl
        << endl
        << indent() << "return true;" << endl;
    indent_down();
    out << indent() << "}" << endl << endl;

    for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter)
    {
        generate_process_function_async(out, tservice, *f_iter);
    }

    indent_down();
    out << indent() << "}" << endl << endl;
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
        << "_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot, CancellationToken cancellationToken)" << endl
        << indent() << "{" << endl;
    indent_up();

    string argsname = tfunction->get_name() + "Args";
    string resultname = tfunction->get_name() + "Result";

    out << indent() << "var args = new InternalStructs." << argsname << "();" << endl
        << indent() << "await args.ReadAsync(iprot, cancellationToken);" << endl
        << indent() << "await iprot.ReadMessageEndAsync(cancellationToken);" << endl;

    if (!tfunction->is_oneway())
    {
        out << indent() << "var result = new InternalStructs." << resultname << "();" << endl;
    }

    out << indent() << "try" << endl
        << indent() << "{" << endl;
    indent_up();

    t_struct* xs = tfunction->get_xceptions();
    const vector<t_field*>& xceptions = xs->get_members();

    if (xceptions.size() > 0)
    {
        out << indent() << "try" << endl
            << indent() << "{" << endl;
        indent_up();
    }

    t_struct* arg_struct = tfunction->get_arglist();
    const vector<t_field*>& fields = arg_struct->get_members();
    vector<t_field*>::const_iterator f_iter;

    out << indent();
    if (!tfunction->is_oneway() && !tfunction->get_returntype()->is_void())
    {
        out << "result.Success = ";
    }

    out << "await _iAsync." << normalize_name(tfunction->get_name()) << "Async(";

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

        out << "args." << prop_name(*f_iter);
    }

    cleanup_member_name_mapping(arg_struct);

    if (!first)
    {
        out << ", ";
    }

    out << "cancellationToken);" << endl;

    vector<t_field*>::const_iterator x_iter;

    collect_extensions_types(xs);
    prepare_member_name_mapping(xs, xs->get_members(), resultname);
    if (xceptions.size() > 0)
    {
        indent_down();
        out << indent() << "}" << endl;

        for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter)
        {
            out << indent() << "catch (" << type_name((*x_iter)->get_type()) << " " << (*x_iter)->get_name() << ")" << endl
                << indent() << "{" << endl;

            if (!tfunction->is_oneway())
            {
                indent_up();
                out << indent() << "result." << prop_name(*x_iter) << " = " << (*x_iter)->get_name() << ";" << endl;
                indent_down();
            }
            out << indent() << "}" << endl;
        }
    }

    if (!tfunction->is_oneway())
    {
        out << indent() << "await oprot.WriteMessageBeginAsync(new TMessage(\""
                << correct_function_name_for_async(tfunction->get_name()) << "\", TMessageType.Reply, seqid), cancellationToken); " << endl
            << indent() << "await result.WriteAsync(oprot, cancellationToken);" << endl;
    }
    indent_down();

    cleanup_member_name_mapping(xs);

    out << indent() << "}" << endl
        << indent() << "catch (TTransportException)" << endl
        << indent() << "{" << endl
        << indent() << "  throw;" << endl
        << indent() << "}" << endl
        << indent() << "catch (Exception ex)" << endl
        << indent() << "{" << endl;
    indent_up();

    out << indent() << "var sErr = $\"Error occurred in {GetType().FullName}: {ex.Message}\";" << endl;
    out << indent() << "if(_logger != null)" << endl;
    indent_up();
    out << indent() << "_logger.LogError(ex, sErr);" << endl;
    indent_down();
    out << indent() << "else" << endl;
    indent_up();
    out << indent() << "Console.Error.WriteLine(sErr);" << endl;
    indent_down();

    if (tfunction->is_oneway())
    {
        indent_down();
        out << indent() << "}" << endl;
    }
    else
    {
        out << indent() << "var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,\" Internal error.\");" << endl
            << indent() << "await oprot.WriteMessageBeginAsync(new TMessage(\"" << correct_function_name_for_async(tfunction->get_name())
            << "\", TMessageType.Exception, seqid), cancellationToken);" << endl
            << indent() << "await x.WriteAsync(oprot, cancellationToken);" << endl;
        indent_down();

        out << indent() << "}" << endl
            << indent() << "await oprot.WriteMessageEndAsync(cancellationToken);" << endl
            << indent() << "await oprot.Transport.FlushAsync(cancellationToken);" << endl;
    }

    indent_down();
    out << indent() << "}" << endl << endl;
}

void t_netstd_generator::generate_netstd_union_reader(ostream& out, t_struct* tunion)
{
    // Thanks to THRIFT-1768, we don't need to check for required fields in the union
    const vector<t_field*>& fields = tunion->get_members();
    vector<t_field*>::const_iterator f_iter;

    out << indent() << "public static async Task<" << tunion->get_name() << "> ReadAsync(TProtocol iprot, CancellationToken cancellationToken)" << endl;
    scope_up(out);

    out << indent() << "iprot.IncrementRecursionDepth();" << endl;
    out << indent() << "try" << endl;
    scope_up(out);

    out << indent() << tunion->get_name() << " retval;" << endl;
    out << indent() << "await iprot.ReadStructBeginAsync(cancellationToken);" << endl;
    out << indent() << "TField field = await iprot.ReadFieldBeginAsync(cancellationToken);" << endl;
    // we cannot have the first field be a stop -- we must have a single field defined
    out << indent() << "if (field.Type == TType.Stop)" << endl;
    scope_up(out);
    out << indent() << "await iprot.ReadFieldEndAsync(cancellationToken);" << endl;
    out << indent() << "retval = new ___undefined();" << endl;
    scope_down(out);
    out << indent() << "else" << endl;
    scope_up(out);
    out << indent() << "switch (field.ID)" << endl;
    scope_up(out);

    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
    {
        out << indent() << "case " << (*f_iter)->get_key() << ":" << endl;
        indent_up();
        out << indent() << "if (field.Type == " << type_to_enum((*f_iter)->get_type()) << ") {" << endl;
        indent_up();

        out << indent() << type_name((*f_iter)->get_type()) << " temp;" << endl;
        generate_deserialize_field(out, (*f_iter), "temp", true);
        out << indent() << "retval = new " << (*f_iter)->get_name() << "(temp);" << endl;

        indent_down();
        out << indent() << "} else { " << endl << indent() << " await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);"
            << endl << indent() << "  retval = new ___undefined();" << endl << indent() << "}" << endl
            << indent() << "break;" << endl;
        indent_down();
    }

    out << indent() << "default: " << endl;
    indent_up();
    out << indent() << "await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);" << endl << indent()
        << "retval = new ___undefined();" << endl;
    out << indent() << "break;" << endl;
    indent_down();

    scope_down(out);

    out << indent() << "await iprot.ReadFieldEndAsync(cancellationToken);" << endl;

    out << indent() << "if ((await iprot.ReadFieldBeginAsync(cancellationToken)).Type != TType.Stop)" << endl;
    scope_up(out);
    out << indent() << "throw new TProtocolException(TProtocolException.INVALID_DATA);" << endl;
    scope_down(out);

    // end of else for TStop
    scope_down(out);
    out << indent() << "await iprot.ReadStructEndAsync(cancellationToken);" << endl;
    out << indent() << "return retval;" << endl;
    indent_down();

    scope_down(out);
    out << indent() << "finally" << endl;
    scope_up(out);
    out << indent() << "iprot.DecrementRecursionDepth();" << endl;
    scope_down(out);

    out << indent() << "}" << endl << endl;
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
                    out << "ReadBinaryAsync(cancellationToken);";
                }
                else
                {
                    out << "ReadStringAsync(cancellationToken);";
                }
                break;
            case t_base_type::TYPE_BOOL:
                out << "ReadBoolAsync(cancellationToken);";
                break;
            case t_base_type::TYPE_I8:
                out << "ReadByteAsync(cancellationToken);";
                break;
            case t_base_type::TYPE_I16:
                out << "ReadI16Async(cancellationToken);";
                break;
            case t_base_type::TYPE_I32:
                out << "ReadI32Async(cancellationToken);";
                break;
            case t_base_type::TYPE_I64:
                out << "ReadI64Async(cancellationToken);";
                break;
            case t_base_type::TYPE_DOUBLE:
                out << "ReadDoubleAsync(cancellationToken);";
                break;
            default:
                throw "compiler error: no C# name for base type " + t_base_type::t_base_name(tbase);
            }
        }
        else if (type->is_enum())
        {
            out << "ReadI32Async(cancellationToken);";
        }
        out << endl;
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
        out << indent() << prefix << " = await " << type_name(tstruct) << ".ReadAsync(iprot, cancellationToken);" << endl;
    }
    else
    {
        out << indent() << prefix << " = new " << type_name(tstruct) << "();" << endl
            << indent() << "await " << prefix << ".ReadAsync(iprot, cancellationToken);" << endl;
    }
}

void t_netstd_generator::generate_deserialize_container(ostream& out, t_type* ttype, string prefix)
{
    out << indent() << "{" << endl;
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
        out << indent() << "TMap " << obj << " = await iprot.ReadMapBeginAsync(cancellationToken);" << endl;
    }
    else if (ttype->is_set())
    {
        out << indent() << "TSet " << obj << " = await iprot.ReadSetBeginAsync(cancellationToken);" << endl;
    }
    else if (ttype->is_list())
    {
        out << indent() << "TList " << obj << " = await iprot.ReadListBeginAsync(cancellationToken);" << endl;
    }

    out << indent() << prefix << " = new " << type_name(ttype) << "(" << obj << ".Count);" << endl;
    string i = tmp("_i");
    out << indent() << "for(int " << i << " = 0; " << i << " < " << obj << ".Count; ++" << i << ")" << endl
        << indent() << "{" << endl;
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
    out << indent() << "}" << endl;

    if (ttype->is_map())
    {
        out << indent() << "await iprot.ReadMapEndAsync(cancellationToken);" << endl;
    }
    else if (ttype->is_set())
    {
        out << indent() << "await iprot.ReadSetEndAsync(cancellationToken);" << endl;
    }
    else if (ttype->is_list())
    {
        out << indent() << "await iprot.ReadListEndAsync(cancellationToken);" << endl;
    }

    indent_down();
    out << indent() << "}" << endl;
}

void t_netstd_generator::generate_deserialize_map_element(ostream& out, t_map* tmap, string prefix)
{
    string key = tmp("_key");
    string val = tmp("_val");

    t_field fkey(tmap->get_key_type(), key);
    t_field fval(tmap->get_val_type(), val);

    out << indent() << declare_field(&fkey) << endl;
    out << indent() << declare_field(&fval) << endl;

    generate_deserialize_field(out, &fkey);
    generate_deserialize_field(out, &fval);

    out << indent() << prefix << "[" << key << "] = " << val << ";" << endl;
}

void t_netstd_generator::generate_deserialize_set_element(ostream& out, t_set* tset, string prefix)
{
    string elem = tmp("_elem");
    t_field felem(tset->get_elem_type(), elem);

    out << indent() << declare_field(&felem) << endl;

    generate_deserialize_field(out, &felem);

    out << indent() << prefix << ".Add(" << elem << ");" << endl;
}

void t_netstd_generator::generate_deserialize_list_element(ostream& out, t_list* tlist, string prefix)
{
    string elem = tmp("_elem");
    t_field felem(tlist->get_elem_type(), elem);

    out << indent() << declare_field(&felem) << endl;

    generate_deserialize_field(out, &felem);

    out << indent() << prefix << ".Add(" << elem << ");" << endl;
}

void t_netstd_generator::generate_serialize_field(ostream& out, t_field* tfield, string prefix, bool is_propertyless)
{
    t_type* type = tfield->get_type();
    type = resolve_typedef( type);

    string name = prefix + (is_propertyless ? "" : prop_name(tfield));

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

        string nullable_name = name;

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
                out << name << ", cancellationToken);";
                break;
            case t_base_type::TYPE_BOOL:
                out << "WriteBoolAsync(" << nullable_name << ", cancellationToken);";
                break;
            case t_base_type::TYPE_I8:
                out << "WriteByteAsync(" << nullable_name << ", cancellationToken);";
                break;
            case t_base_type::TYPE_I16:
                out << "WriteI16Async(" << nullable_name << ", cancellationToken);";
                break;
            case t_base_type::TYPE_I32:
                out << "WriteI32Async(" << nullable_name << ", cancellationToken);";
                break;
            case t_base_type::TYPE_I64:
                out << "WriteI64Async(" << nullable_name << ", cancellationToken);";
                break;
            case t_base_type::TYPE_DOUBLE:
                out << "WriteDoubleAsync(" << nullable_name << ", cancellationToken);";
                break;
            default:
                throw "compiler error: no C# name for base type " + t_base_type::t_base_name(tbase);
            }
        }
        else if (type->is_enum())
        {
            out << "WriteI32Async((int)" << nullable_name << ", cancellationToken);";
        }
        out << endl;
    }
    else
    {
        printf("DO NOT KNOW HOW TO SERIALIZE '%s%s' TYPE '%s'\n", prefix.c_str(), tfield->get_name().c_str(), type_name(type).c_str());
    }
}

void t_netstd_generator::generate_serialize_struct(ostream& out, t_struct* tstruct, string prefix)
{
    (void)tstruct;
    out << indent() << "await " << prefix << ".WriteAsync(oprot, cancellationToken);" << endl;
}

void t_netstd_generator::generate_serialize_container(ostream& out, t_type* ttype, string prefix)
{
    out << indent() << "{" << endl;
    indent_up();

    if (ttype->is_map())
    {
        out << indent() << "await oprot.WriteMapBeginAsync(new TMap(" << type_to_enum(static_cast<t_map*>(ttype)->get_key_type())
            << ", " << type_to_enum(static_cast<t_map*>(ttype)->get_val_type()) << ", " << prefix
            << ".Count), cancellationToken);" << endl;
    }
    else if (ttype->is_set())
    {
        out << indent() << "await oprot.WriteSetBeginAsync(new TSet(" << type_to_enum(static_cast<t_set*>(ttype)->get_elem_type())
            << ", " << prefix << ".Count), cancellationToken);" << endl;
    }
    else if (ttype->is_list())
    {
        out << indent() << "await oprot.WriteListBeginAsync(new TList("
            << type_to_enum(static_cast<t_list*>(ttype)->get_elem_type()) << ", " << prefix << ".Count), cancellationToken);"
            << endl;
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

    out << endl;
    out << indent() << "{" << endl;
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
    out << indent() << "}" << endl;

    if (ttype->is_map())
    {
        out << indent() << "await oprot.WriteMapEndAsync(cancellationToken);" << endl;
    }
    else if (ttype->is_set())
    {
        out << indent() << "await oprot.WriteSetEndAsync(cancellationToken);" << endl;
    }
    else if (ttype->is_list())
    {
        out << indent() << "await oprot.WriteListEndAsync(cancellationToken);" << endl;
    }

    indent_down();
    out << indent() << "}" << endl;
}

void t_netstd_generator::generate_serialize_map_element(ostream& out, t_map* tmap, string iter, string map)
{
    t_field kfield(tmap->get_key_type(), iter);
    generate_serialize_field(out, &kfield, "");
    t_field vfield(tmap->get_val_type(), map + "[" + iter + "]");
    generate_serialize_field(out, &vfield, "");
}

void t_netstd_generator::generate_serialize_set_element(ostream& out, t_set* tset, string iter)
{
    t_field efield(tset->get_elem_type(), iter);
    generate_serialize_field(out, &efield, "");
}

void t_netstd_generator::generate_serialize_list_element(ostream& out, t_list* tlist, string iter)
{
    t_field efield(tlist->get_elem_type(), iter);
    generate_serialize_field(out, &efield, "");
}

void t_netstd_generator::generate_property(ostream& out, t_field* tfield, bool isPublic, bool generateIsset)
{
    generate_netstd_property(out, tfield, isPublic, generateIsset, "_");
}

void t_netstd_generator::generate_netstd_property(ostream& out, t_field* tfield, bool isPublic, bool generateIsset, string fieldPrefix)
{
    if ((is_serialize_enabled() || is_wcf_enabled()) && isPublic)
    {
        out << indent() << "[DataMember(Order = 0)]" << endl;
    }
    bool is_required = field_is_required(tfield);
    if (is_required)
    {
        out << indent() << (isPublic ? "public " : "private ") << type_name(tfield->get_type()) << " " << prop_name(tfield) << " { get; set; }" << endl;
    }
    else
    {
        out << indent() << (isPublic ? "public " : "private ")  << type_name(tfield->get_type()) << " " << prop_name(tfield) << endl
            << indent() << "{" << endl;
        indent_up();

        out << indent() << "get" << endl
            << indent() << "{" << endl;
        indent_up();

        bool use_nullable = false;

        out << indent() << "return " << fieldPrefix + tfield->get_name() << ";" << endl;
        indent_down();
        out << indent() << "}" << endl
            << indent() << "set" << endl
            << indent() << "{" << endl;
        indent_up();

        if (use_nullable)
        {
            if (generateIsset)
            {
                out << indent() << "__isset." << get_isset_name(normalize_name(tfield->get_name())) << " = value.HasValue;" << endl;
            }
            out << indent() << "if (value.HasValue) this." << fieldPrefix + tfield->get_name() << " = value.Value;" << endl;
        }
        else
        {
            if (generateIsset)
            {
                out << indent() << "__isset." << get_isset_name(normalize_name(tfield->get_name())) << " = true;" << endl;
            }
            out << indent() << "this." << fieldPrefix + tfield->get_name() << " = value;" << endl;
        }

        indent_down();
        out << indent() << "}" << endl;
        indent_down();
        out << indent() << "}" << endl;
    }
    out << endl;
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

void t_netstd_generator::prepare_member_name_mapping(t_struct* tstruct)
{
    prepare_member_name_mapping(tstruct, tstruct->get_members(), tstruct->get_name());
}

void t_netstd_generator::prepare_member_name_mapping(void* scope, const vector<t_field*>& members, const string& structname)
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

    // prevent name conflicts with struct (CS0542 error)
    used_member_names.insert(structname);
    used_member_names.insert("Isset");

    // prevent name conflicts with known methods (THRIFT-2942)
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

string t_netstd_generator::type_name(t_type* ttype)
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
        return "THashSet<" + type_name(tset->get_elem_type()) + ">";
    }

    if (ttype->is_list())
    {
        t_list* tlist = static_cast<t_list*>(ttype);
        return "List<" + type_name(tlist->get_elem_type()) + ">";
    }

    string the_name = check_and_correct_struct_name(normalize_name(ttype->get_name()));

    t_program* program = ttype->get_program();
    if (program != nullptr)// && program != program_)
    {
        string ns =  program->get_namespace("netstd");
        if (!ns.empty())
        {
            return "global::" + ns + "." + the_name;
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
        if (tbase->is_binary())
        {
            return "byte[]";
        } else {
            return "string";
        }
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

string t_netstd_generator::get_deep_copy_method_call(t_type* ttype, bool& needs_typecast)
{
    ttype = resolve_typedef(ttype);

    needs_typecast = false;
    if (ttype->is_base_type())
    {
        t_base_type::t_base tbase = static_cast<t_base_type*>(ttype)->get_base();
        switch (tbase)
        {
        case t_base_type::TYPE_STRING:
            if (ttype->is_binary())
            {
                return ".ToArray()";
            } else {
                return "";  // simple assignment will do, strings are immutable in C#
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
    else 
    {
        needs_typecast = (! ttype->is_container());
        return "." + DEEP_COPY_METHOD_NAME + "()";
    }
}

string t_netstd_generator::declare_field(t_field* tfield, bool init, string prefix)
{
    string result = type_name(tfield->get_type()) + " " + prefix + tfield->get_name();
    if (init)
    {
        t_type* ttype = tfield->get_type();
        ttype = resolve_typedef(ttype);
        if (ttype->is_base_type() && field_has_default(tfield))
        {
            std::ofstream dummy;
            result += " = " + render_const_value(dummy, tfield->get_name(), ttype, tfield->get_value());
        }
        else if (ttype->is_base_type())
        {
            t_base_type::t_base tbase = static_cast<t_base_type*>(ttype)->get_base();
            switch (tbase)
            {
            case t_base_type::TYPE_VOID:
                throw "NO T_VOID CONSTRUCT";
            case t_base_type::TYPE_STRING:
                result += " = null";
                break;
            case t_base_type::TYPE_BOOL:
                result += " = false";
                break;
            case t_base_type::TYPE_I8:
            case t_base_type::TYPE_I16:
            case t_base_type::TYPE_I32:
            case t_base_type::TYPE_I64:
                result += " = 0";
                break;
            case t_base_type::TYPE_DOUBLE:
                result += " = (double)0";
                break;
            }
        }
        else if (ttype->is_enum())
        {
            result += " = (" + type_name(ttype) + ")0";
        }
        else if (ttype->is_container())
        {
            result += " = new " + type_name(ttype) + "()";
        }
        else
        {
            result += " = new " + type_name(ttype) + "()";
        }
    }
    return result + ";";
}

string t_netstd_generator::function_signature(t_function* tfunction, string prefix)
{
    t_type* ttype = tfunction->get_returntype();
    return type_name(ttype) + " " + normalize_name(prefix + tfunction->get_name()) + "(" + argument_list(tfunction->get_arglist()) + ")";
}

string t_netstd_generator::function_signature_async(t_function* tfunction, string prefix)
{
    t_type* ttype = tfunction->get_returntype();
    string task = "global::System.Threading.Tasks.Task";
    if (!ttype->is_void())
    {
        task += "<" + type_name(ttype) + ">";
    }

    string result = task + " " + normalize_name(prefix + tfunction->get_name()) + "Async(";
    string args = argument_list(tfunction->get_arglist());
    result += args;
    if (!args.empty())
    {
        result += ", ";
    }
    result += "CancellationToken cancellationToken = default)";

    return result;
}

string t_netstd_generator::argument_list(t_struct* tstruct)
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
        result += type_name((*f_iter)->get_type()) + " " + normalize_name((*f_iter)->get_name());
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
    docstring_comment(out, "/// <summary>" + endl, "/// ", contents, "/// </summary>" + endl);
}

void t_netstd_generator::generate_netstd_doc(ostream& out, t_field* field)
{
    if (field->get_type()->is_enum())
    {
        string combined_message = field->get_doc() + endl + "<seealso cref=\"" + get_enum_class_name(field->get_type()) + "\"/>";
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
            ps << endl << "<param name=\"" << p->get_name() << "\">";
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
                                   "<summary>" + endl + tfunction->get_doc() + "</summary>" + ps.str(),
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
            out << endl;
        }
        else if (strlen(line) > 0 || !docs.eof())
        { // skip the empty last line
            out << indent() << line_prefix << line << endl;
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

THRIFT_REGISTER_GENERATOR(
    netstd,
    "C#",
    "    wcf:             Adds bindings for WCF to generated classes.\n"
    "    serial:          Add serialization support to generated classes.\n"
    "    union:           Use new union typing, which includes a static read function for union types.\n"
    "    pascal:          Generate Pascal Case property names according to Microsoft naming convention.\n"
    "    no_deepcopy:     Suppress generation of DeepCopy() method.\n"
)
