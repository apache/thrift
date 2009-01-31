#
# simple generator for Thrift
#

import sys
import os
import cStringIO
import operator

import parser
import ezt


### temporary
PATH = '/Users/gstein/src/asf/thrift/compiler/py/src/templates-py'
t_py = ezt.Template(os.path.join(PATH, 'py.ezt'),
                    compress_whitespace=False)
t_py_ser = ezt.Template(os.path.join(PATH, 'py_ser.ezt'),
                        compress_whitespace=False)
t_py_deser = ezt.Template(os.path.join(PATH, 'py_deser.ezt'),
                          compress_whitespace=False)
t_py_cvalue = ezt.Template(os.path.join(PATH, 'py_cvalue.ezt'),
                           compress_whitespace=False)


def generate(program):
  t_py.generate(sys.stdout, Proxy(program))


class AutoVars(object):
  def __init__(self):
    self._counter = 0
    self._mapping = { }
    self._saved = [ ]

  def open_context(self):
    self._saved.append(self._mapping)
    self._mapping = { }

  def close_context(self):
    self._mapping = self._saved.pop()

  def __getattr__(self, name):
    if name.startswith('__'):
      raise AttributeError(name)

    if name in self._mapping:
      return self._mapping[name]
    var = '%s%d' % (name, self._counter)
    self._counter += 1
    self._mapping[name] = var
    return var


class Proxy(object):
  def __init__(self, ob):
    self._ob = ob

    for name, value in vars(ob).items():
      proxy = custom_proxy(value)
      if proxy:
        value = proxy(value)
      elif isinstance(value, list) and value:
        # lists are homogenous, so check the first item
        proxy = custom_proxy(value[0])
        if proxy:
          value = [proxy(ob) for ob in value]
        elif hasattr(value[0], '__dict__'):
          value = [Proxy(ob) for ob in value]
      setattr(self, name, value)

  def __getattr__(self, name):
    if name == 'auto':
      return g_auto
    raise AttributeError(name)


class ProxyFieldType(Proxy):
  def __getattr__(self, name):
    if name == 'serializer':
      return Subtemplate(t_py_ser, self)
    if name == 'deserializer':
      return Subtemplate(t_py_deser, self)
    return Proxy.__getattr__(self, name)


class Subtemplate(object):
  def __init__(self, template, data):
    self._template = template
    self._data = data

  def __getattr__(self, name):
    # jam the name of the result variable into the data params
    self._data.result_var = getattr(g_auto, name)

    # use a new variable context for this template generation
    g_auto.open_context()
    value = gen_value(self._template, self._data)
    g_auto.close_context()

    return value


class ProxyField(Proxy):
  def __getattr__(self, name):
    if name == 'type_enum':
      return TYPE_ENUM.get(self._ob.field_type.ident,
                           self._ob.field_type.ident.tvalue)
    return Proxy.__getattr__(self, name)


class ProxyStruct(Proxy):
  def __getattr__(self, name):
    if name == 'sorted_fields':
      highest = max(int(f.field_id or -1) for f in self._ob.fields)
      fields = [None] * (highest + 1)
      for field in self._ob.fields:
        if field.field_id:
          id = int(field.field_id)
          if id > 0:
            fields[id] = ProxyField(field)
      return fields
    return Proxy.__getattr__(self, name)


class ProxyConstValue(Proxy):
  def __getattr__(self, name):
    if name == 'cvalue':
      return gen_value(t_py_cvalue, self)
    return Proxy.__getattr__(self, name)


def custom_proxy(value):
  if isinstance(value, parser.FieldType):
    return ProxyFieldType
  if isinstance(value, parser.Field):
    return ProxyField
  if isinstance(value, parser.Struct):
    return ProxyStruct
  if isinstance(value, parser.ConstValue):
    return ProxyConstValue
  return None


TYPE_ENUM = {
  parser.ID_STRING: 'TType.STRING',
  parser.ID_BOOL: 'TType.BOOL',
  parser.ID_BYTE: 'TType.BYTE',
  parser.ID_I16: 'TType.I16',
  parser.ID_I32: 'TType.I32',
  parser.ID_I64: 'TType.I64',
  parser.ID_DOUBLE: 'TType.DOUBLE',
  parser.ID_MAP: 'TType.MAP',
  parser.ID_SET: 'TType.SET',
  parser.ID_LIST: 'TType.LIST',
  # TType.STRUCT and TType.I32 for enums
  }


def gen_value(template, ob):
  buf = cStringIO.StringIO()
  template.generate(buf, ob)
  return buf.getvalue()


if __name__ == '__main__':
  import sys
  program = parser.parse(open(sys.argv[1]).read())
  generate(program)
