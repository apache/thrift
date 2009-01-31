#
# simple parser for Thrift.
#

# Note: the scanner module is designed to allow this wildcard import
from scanner import *


def parse(contents):

  scanner = Scanner(contents)
  program = Program()

  while True:

    t = scanner.get()
    if t is None:
      return program

    ### delta: we don't enforce HeaderList followed by DefinitionList
    ### delta: deprecated namespaces are not parsed

    if t == ID_INCLUDE:
      inc = scanner.value_of(TYPE_LIT)
      program.add_include(inc)
    elif t == ID_NAMESPACE:
      lang = scanner.value_of(TYPE_ID)
      ns = scanner.value_of(TYPE_ID)
      program.add_namespace(lang, ns)
    elif t == ID_CPP_INCLUDE:
      inc = scanner.value_of(TYPE_LIT)
      program.add_cpp_include(inc)
    elif t == ID_PHP_NAMESPACE:
      ns = scanner.value_of(TYPE_ID)
      program.set_php_namespace(ns)
    elif t == ID_XSD_NAMESPACE:
      ns = scanner.value_of(TYPE_LIT)
      program.set_xsd_namespace(ns)
    elif t == ID_CONST:
      doc = scanner.doc
      ft = parse_field_type(scanner, True)
      ident = scanner.value_of(TYPE_ID)
      scanner.eat_expected(SYM_EQ)
      value = parse_const_value(scanner)
      scanner.eat_commasemi()
      program.add_const(ident, ft, value, doc)
    elif t == ID_TYPEDEF:
      doc = scanner.doc
      ft = parse_field_type(scanner, False)
      ident = scanner.value_of(TYPE_ID)
      program.add_typedef(ident, ft, doc)
    elif t == ID_ENUM:
      enum_doc = scanner.doc
      enum_ident = scanner.value_of(TYPE_ID)
      scanner.eat_expected(SYM_LBRACE)
      values = [ ]
      while True:
        t = scanner.get(eof_allowed=False)
        if t == SYM_RBRACE:
          break
        if t.ttype != TYPE_ID:
          raise ExpectedType(TYPE_ID, t.ttype, scanner.lineno)
        doc = scanner.doc
        ident = t.tvalue
        t = scanner.get(eof_allowed=False)
        if t == SYM_EQ:
          value = scanner.value_of(TYPE_INT)
        else:
          scanner.pushback(t)
          value = None
        scanner.eat_commasemi()
        values.append(EnumValue(ident, value, doc))
      program.add_enum(enum_ident, values, enum_doc)
    elif t == ID_SENUM:
      doc = scanner.doc
      ident = scanner.value_of(TYPE_ID)
      scanner.eat_expected(SYM_LBRACE)
      values = [ ]
      while True:
        t = scanner.get(eof_allowed=False)
        if t == SYM_RBRACE:
          break
        if t.ttype != TYPE_LIT:
          raise ExpectedType(TYPE_LIT, t.ttype, scanner.lineno)
        scanner.eat_commasemi()
        values.append(t.tvalue)
      program.add_senum(ident, values, doc)
    elif t == ID_STRUCT:
      doc = scanner.doc
      ident = scanner.value_of(TYPE_ID)
      t = scanner.get(eof_allowed=False)
      if t == ID_XSD_ALL:
        xsd_all = True
      else:
        xsd_all = False
        scanner.pushback(t)
      fields = parse_field_list(scanner, SYM_LBRACE, SYM_RBRACE)
      annotations = parse_annotations(scanner)
      program.add_struct(ident, fields, annotations, doc)
    elif t == ID_EXCEPTION:
      doc = scanner.doc
      ident = scanner.value_of(TYPE_ID)
      fields = parse_field_list(scanner, SYM_LBRACE, SYM_RBRACE)
      program.add_exception(ident, fields, doc)
    elif t == ID_SERVICE:
      svc_doc = scanner.doc
      svc_ident = scanner.value_of(TYPE_ID)
      t = scanner.get(eof_allowed=False)
      if t == ID_EXTENDS:
        extends = t.tvalue
        t = scanner.get(eof_allowed=False)
      else:
        extends = None
      if t != SYM_LBRACE:
        raise ExpectedError(SYM_LBRACE, t, scanner.lineno)
      functions = [ ]
      while True:
        t = scanner.get(eof_allowed=False)
        doc = scanner.doc
        if t == SYM_RBRACE:
          break
        if t == ID_ASYNC:
          async = True
          t = scanner.get(eof_allowed=False)
        else:
          async = False
        if t == ID_VOID:
          ft = FieldType(ident=ID_VOID)
        else:
          scanner.pushback(t)
          ft = parse_field_type(scanner, True)
        ident = scanner.value_of(TYPE_ID)
        params = parse_field_list(scanner, SYM_LPAREN, SYM_RPAREN)
        t = scanner.get(eof_allowed=False)
        if t == ID_THROWS:
          throws = parse_field_list(scanner, SYM_LPAREN, SYM_RPAREN)
        else:
          throws = None
          scanner.pushback(t)
        scanner.eat_commasemi()
        functions.append(Function(ident, async, ft, params, throws, doc))
      program.add_service(svc_ident, extends, functions, svc_doc)
    else:
      raise IncorrectSyntax(scanner.lineno)


def parse_field_type(scanner, ident_allowed):
  ident = scanner.get_type(TYPE_ID)
  if ident in BASE_TYPES:
    return FieldType(ident=ident)

  cpp_type = None

  if ident == ID_MAP:
    t = scanner.get(eof_allowed=False)
    if t == ID_CPP_TYPE:
      cpp_type = scanner.value_of(TYPE_LITERAL)
      t = scanner.get()
    if t != SYM_LT:
      raise ExpectedError(SYM_LT, t, scanner.lineno)
    map_from = parse_field_type(scanner, True)
    scanner.eat_expected(SYM_COMMA)
    map_to = parse_field_type(scanner, True)
    scanner.eat_expected(SYM_GT)
    return FieldType(cpp_type=cpp_type, map_from=map_from, map_to=map_to,
                     annotations=parse_annotations(scanner))

  if ident == ID_SET:
    t = scanner.get(eof_allowed=False)
    if t == ID_CPP_TYPE:
      cpp_type = scanner.value_of(TYPE_LITERAL)
      t = scanner.get()
    if t != SYM_LT:
      raise ExpectedError(SYM_LT, t, scanner.lineno)
    set_of = parse_field_type(scanner, True)
    scanner.eat_expected(SYM_GT)
    return FieldType(cpp_type=cpp_type, set_of=set_of,
                     annotations=parse_annotations(scanner))

  if ident == ID_LIST:
    scanner.eat_expected(SYM_LT)
    list_of = parse_field_type(scanner, True)
    scanner.eat_expected(SYM_GT)
    t = scanner.get()
    if t == ID_CPP_TYPE:
      cpp_type = scanner.value_of(TYPE_LITERAL)
    elif t is not None:
      scanner.pushback(t)
    return FieldType(cpp_type=cpp_type, list_of=list_of,
                     annotations=parse_annotations(scanner))

  # random identifiers are allowed for FieldType, but not DefinitionType
  if ident_allowed:
    return FieldType(ident=ident)

  raise IncorrectSyntax(scanner.lineno)


def parse_const_value(scanner):
  value = scanner.get(eof_allowed=False)
  if value.ttype in [TYPE_INT, TYPE_HEX, TYPE_DUB, TYPE_LIT, TYPE_ID]:
    return ConstValue(ConstValue.CTYPE_BASE, value)

  if value == SYM_LBRKT:
    values = [ ]
    while True:
      t = scanner.get(eof_allowed=False)
      if t == SYM_RBRKT:
        return ConstValue(ConstValue.CTYPE_LIST, values)
      scanner.pushback(t)
      scanner.eat_commasemi()
      values.append(parse_const_value(scanner))

  if value == SYM_LBRACE:
    values = [ ]
    while True:
      t = scanner.get(eof_allowed=False)
      if t == SYM_RBRACE:
        return ConstValue(ConstValue.CTYPE_MAP, values)
      scanner.pushback(t)
      key = parse_const_value(scanner)
      scanner.eat_expected(SYM_COLON)
      value = parse_const_value(scanner)
      scanner.eat_commasemi()
      values.append(KeyValuePair(key, value))

  raise IncorrectSyntax(scanner.lineno)


def parse_field_list(scanner, start, end):
  scanner.eat_expected(start)

  fields = [ ]
  while True:
    t = scanner.get(eof_allowed=False)
    if t == end:
      return fields
    doc = scanner.doc
    if t.ttype == TYPE_INT:
      field_id = t.tvalue
      scanner.eat_expected(SYM_COLON)
      t = scanner.get(eof_allowed=False)
    else:
      field_id = None
    if t == ID_REQUIRED or t == ID_OPTIONAL:
      ### delta: we don't warn when this occurs in an arglist
      requiredness = t
    else:
      requiredness = None
      scanner.pushback(t)
    ft = parse_field_type(scanner, True)
    ident = scanner.value_of(TYPE_ID)
    t = scanner.get()
    if t == SYM_EQ:
      value = parse_const_value(scanner)
      t = scanner.get()
    else:
      value = None
    if t == ID_XSD_OPTIONAL:
      xsd_optional = True
      t = scanner.get()
    else:
      xsd_optional = False
    if t == ID_XSD_NILLABLE:
      xsd_nillable = True
      t = scanner.get()
    else:
      xsd_nillable = False
    if t == ID_XSD_ATTRS:
      xsd_attrs = parse_field_list(scanner, SYM_LBRACE, SYM_RBRACE)
    else:
      xsd_attrs = None
      if t is not None:
        scanner.pushback(t)
    scanner.eat_commasemi()
    fields.append(Field(ident, ft, doc, field_id, requiredness, value,
                        xsd_optional, xsd_nillable, xsd_attrs))


def parse_annotations(scanner):
  t = scanner.get()
  if t is None:
    return None
  if t != SYM_LPAREN:
    scanner.pushback(t)
    return None
  annotations = [ ]
  while True:
    ident = scanner.value_of(TYPE_ID)
    scanner.eat_expected(SYM_EQ)
    value = scanner.value_of(TYPE_LIT)
    annotations.append(KeyValuePair(ident, value))

    scanner.eat_commasemi()
    t = scanner.get()
    if t == SYM_RPAREN:
      return annotations
    scanner.pushback(t)


class Program(object):
  def __init__(self):
    self.includes = [ ]
    self.namespaces = [ ]
    self.cpp_includes = [ ]
    self.php_namespace = None
    self.xsd_namespace = None
    self.consts = [ ]
    self.typedefs = [ ]
    self.enums = [ ]
    self.structs = [ ]
    self.exceptions = [ ]
    self.services = [ ]

  def add_include(self, include):
    self.includes.append(include)

  def add_namespace(self, lang, namespace):
    self.namespaces.append(Namespace(lang, namespace))

  def add_cpp_include(self, include):
    self.cpp_includes.append(include)

  def set_php_namespace(self, namespace):
    self.php_namespace = namespace

  def set_xsd_namespace(self, namespace):
    self.xsd_namespace = namespace

  def add_const(self, ident, field_type, value, doc):
    self.consts.append(ConstDef(ident, field_type, value, doc))

  def add_typedef(self, ident, field_type, doc):
    self.typedefs.append(Typedef(ident, field_type, doc))

  def add_enum(self, ident, value, doc):
    self.enums.append(Enum(ident, value, doc))

  def add_senum(self, ident, values, doc):
    self.typedefs.append(Typedef(ident, FieldType(values=values), doc))

  def add_struct(self, ident, fields, annotations, doc):
    self.structs.append(Struct(ident, fields, annotations, doc))

  def add_exception(self, ident, fields, doc):
    self.exceptions.append(Exception(ident, fields, doc))

  def add_service(self, ident, extends, functions, doc):
    self.services.append(Service(ident, extends, functions, doc))


class Service(object):
  def __init__(self, ident, extends, functions, doc):
    self.ident = ident
    self.extends = extends
    self.functions = functions
    self.doc = doc


class Function(object):
  def __init__(self, ident, async, field_type, params, throws, doc):
    self.ident = ident
    self.async = async
    self.field_type = field_type
    self.params = params
    self.throws = throws
    self.doc = doc


class Enum(object):
  def __init__(self, ident, values, doc):
    self.ident = ident
    self.values = values
    self.doc = doc

    for i in range(1, len(values)):
      if values[i].value is None:
        ### keep as integer?
        values[i].value = str(int(values[i - 1].value) + 1)


class EnumValue(object):
  def __init__(self, ident, value, doc):
    self.ident = ident
    self.value = value
    self.doc = doc


class Field(object):
  def __init__(self, ident, field_type, doc, field_id, requiredness, value,
               xsd_optional, xsd_nillable, xsd_attrs):
    assert value is None or isinstance(value, ConstValue)

    self.ident = ident
    self.field_type = field_type
    self.doc = doc
    self.field_id = field_id
    self.requiredness = requiredness
    self.value = value
    self.xsd_optional = xsd_optional
    self.xsd_nillable = xsd_nillable
    self.xsd_attrs = xsd_attrs


class FieldType(object):
  def __init__(self, ident=None, cpp_type=None, map_from=None, map_to=None,
               set_of=None, list_of=None, annotations=None, values=None):
    if map_from is not None:
      self.ident = ID_MAP
    elif set_of is not None:
      self.ident = ID_SET
    elif list_of is not None:
      self.ident = ID_LIST
    elif values is not None:
      self.ident = ID_STRING
    else:
      assert ident is not None
      self.ident = ident
    self.cpp_type = cpp_type
    self.map_from = map_from
    self.map_to = map_to
    self.set_of = set_of
    self.list_of = list_of
    self.annotations = annotations
    self.values = values


class KeyValuePair(object):
  def __init__(self, key, value):
    self.key = key
    self.value = value


class ConstDef(object):
  def __init__(self, ident, field_type, value, doc):
    assert isinstance(value, ConstValue)

    self.ident = ident
    self.field_type = field_type
    self.value = value
    self.doc = doc


class ConstValue(object):
  CTYPE_BASE = 'base'
  CTYPE_LIST = 'list'
  CTYPE_MAP = 'map'

  def __init__(self, ctype, value):
    self.ctype = ctype
    self.value = value


class Typedef(object):
  def __init__(self, ident, field_type, doc):
    self.ident = ident
    self.field_type = field_type
    self.doc = doc


class Struct(object):
  def __init__(self, ident, fields, annotations, doc):
    self.ident = ident
    self.fields = fields
    self.annotations = annotations
    self.doc = doc


class Exception(object):
  def __init__(self, ident, fields, doc):
    self.ident = ident
    self.fields = fields
    self.doc = doc


class Namespace(object):
  def __init__(self, lang, namespace):
    self.lang = lang
    self.namespace = namespace


BASE_TYPES = [
  ID_STRING,
  ID_BINARY,
  ID_SLIST,
  ID_BOOL,
  ID_BYTE,
  ID_I16,
  ID_I32,
  ID_I64,
  ID_DOUBLE,
  ]


if __name__ == '__main__':
  import sys
  parse(open(sys.argv[1]).read())
