#
# simple scanner for Thrift. emits tokens.
#

__all__ = ['Scanner', 'SimpleScanner', 'Token', 'TYPE_INT',
           'ExpectedError', 'ExpectedType', 'UnexpectedEOF',
           'UnknownToken', 'IncorrectSyntax',
           ]

import re

re_int = re.compile('[+-]?[0-9]+$')  # special handling
re_hex = re.compile('0x[0-9A-Fa-f]+')
re_dub = re.compile(r'[+-]?[0-9]*(\.[0-9]+)?([eE][+-]?[0-9]+)?')

re_white = re.compile('[ \t\r\n]+')
re_silly = re.compile(r'/\*+\*/')
re_multi = re.compile(r'/\*[^*]/*([^*/]|[^*]/|\*[^/])*\*+/')
re_comment = re.compile('//[^\n]*')
re_unix = re.compile('#[^\n]*')

re_doc = re.compile(r'/\*\*([^*/]|[^*]/|\*[^/])*\*+/')

re_ident = re.compile('[a-zA-Z_][\.a-zA-Z_0-9]*')
re_symbol = re.compile(r'[:;,{}()=<>\[\]]')
re_dliteral = re.compile('"[^"]*"')
re_sliteral = re.compile("'[^']*'")
re_st_ident = re.compile('[a-zA-Z-][.a-zA-Z_0-9-]*')

skip_re = [re_white, re_silly, re_multi, re_comment, re_unix]

types = [
  ('HEX', re_hex),  # keep before re_dub
  ('DUB', re_dub),
  ('DOC', re_doc),
  ('ID', re_ident),
  ('SYM', re_symbol),
  ('LIT', re_dliteral),
  ('LIT', re_sliteral),
  ('STID', re_st_ident),
  ]

for key, pattern in types:
  globals()['TYPE_' + key] = key
  __all__.append('TYPE_' + key)
TYPE_INT = 'INT'


class SimpleScanner(object):

  def __init__(self, contents):
    self.contents = contents
    self.lineno = 1

  def get(self):
    """Get the next token.

    Consumes and returns the next token. Note that leading whitespace is
    skipped.

    Returns None if there are no more tokens.
    """
    self._skip()

    if not self.contents:
      return None

    for ttype, pattern in types:
      m = pattern.match(self.contents)
      if m:
        if m.end() == 0:
          continue
        tvalue = m.group()
        if pattern is re_dub and re_int.match(tvalue):
          ttype = TYPE_INT
        elif ttype == TYPE_LIT:
          # strip quotes
          tvalue = tvalue[1:-1]
        ### fold TYPE_HEX into TYPE_INT? convert INT/DUB away from string?
        token = Token(ttype, tvalue)
        self._chomp(m.end())
        return token

    raise UnknownToken(self.lineno)

  def _skip(self):
    "Skip over leading whitespace."

    while True:
      for pattern in skip_re:
        m = pattern.match(self.contents)
        if m:
          self._chomp(m.end())
          break
      else:
        # nothing matched. all done.
        return

  def _chomp(self, amt):
    "Chomp AMT bytes off the front of the contents. Count newlines."
    self.lineno += self.contents[:amt].count('\n')
    self.contents = self.contents[amt:]


class Scanner(SimpleScanner):
  def __init__(self, contents):
    SimpleScanner.__init__(self, contents)

    self.doc = None
    self.pending = None

  def get(self, eof_allowed=True):
    if self.pending is not None:
      token = self.pending
      self.pending = None
      return token

    self.doc = None
    while True:
      t = SimpleScanner.get(self)
      if t is None:
        if eof_allowed:
          return None
        raise UnexpectedEOF(self.lineno)
      if t.ttype != TYPE_DOC:
        #print 'TOKEN:', t
        return t
      self.doc = t

  def get_type(self, ttype):
    "Get the next token, ensuring it is of the given type."
    t = self.get(eof_allowed=False)
    if t.ttype != ttype:
      raise ExpectedType(ttype, t.ttype, self.lineno)
    return t

  def value_of(self, ttype):
    "Get the next token's value, ensuring it is of the given type."
    return self.get_type(ttype).tvalue

  def pushback(self, token):
    "Push a token back into the scanner; it was unused."
    assert token is not None
    assert self.pending is None
    self.pending = token

  def eat_commasemi(self):
    "Eat a comma or a semicolon, if present."
    t = self.get()
    if t != SYM_COMMA and t != SYM_SEMI:
      self.pushback(t)

  def eat_expected(self, token):
    "Eat the expected token, or raise a ExpectedError."
    t = self.get()
    if t != token:
      raise ExpectedError(token, t, self.lineno)


class Token(object):
  def __init__(self, ttype, tvalue=None):
    self.ttype = ttype
    self.tvalue = tvalue

  def __str__(self):
    if self.tvalue is None:
      return 'T(%s)' % self.ttype
    return 'T(%s, "%s")' % (self.ttype, self.tvalue)

  def __eq__(self, other):
    return self.ttype == other.ttype and self.tvalue == other.tvalue

  def __ne__(self, other):
    return self.ttype != other.ttype or self.tvalue != other.tvalue

  def __hash__(self):
    return hash((self.ttype, self.tvalue))


for ident in ['namespace',
              'cpp_namespace',
              'cpp_include',
              'cpp_type',
              'java_package',
              'cocoa_prefix',
              'csharp_namespace',
              'php_namespace',
              'py_module',
              'perl_package',
              'ruby_namespace',
              'smalltalk_category',
              'smalltalk_prefix',
              'xsd_all',
              'xsd_optional',
              'xsd_nillable',
              'xsd_namespace',
              'xsd_attrs',
              'include',
              'void',
              'bool',
              'byte',
              'i16',
              'i32',
              'i64',
              'double',
              'string',
              'binary',
              'slist',
              'senum',
              'map',
              'list',
              'set',
              'async',
              'typedef',
              'struct',
              'exception',
              'extends',
              'throws',
              'service',
              'enum',
              'const',
              'required',
              'optional',
              ]:
  name = 'ID_' + ident.upper()
  globals()[name] = Token(TYPE_ID, ident)
  __all__.append(name)


for name, sym in [('COLON', ':'),
                  ('SEMI', ';'),
                  ('COMMA', ','),
                  ('LBRACE', '{'),
                  ('RBRACE', '}'),
                  ('LPAREN', '('),
                  ('RPAREN', ')'),
                  ('LBRKT', '['),
                  ('RBRKT', ']'),
                  ('EQ', '='),
                  ('LT', '<'),
                  ('GT', '>'),
                  ]:
  globals()['SYM_' + name] = Token(TYPE_SYM, sym)
  __all__.append('SYM_' + name)


class ExpectedError(Exception):
  "Expected token was not present."

class ExpectedType(Exception):
  "Expected token type was not present."

class UnexpectedEOF(Exception):
  "EOF reached unexpectedly."

class UnknownToken(Exception):
  "Unknown token encountered."

class IncorrectSyntax(Exception):
  "Incorrect syntax encountered."


if __name__ == '__main__':
  import sys

  s = Scanner(open(sys.argv[1]).read())
  while True:
    token = s.get()
    if token is None:
      break
    print token
