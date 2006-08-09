""" Thrift IDL parser/compiler
    
    This parser uses the Python PLY LALR parser generator to build a parser for the Thrift IDL grammar.

    If a compiles \"thyc\" file exists for a given source \"thrift\" file it computes a hash of the file and determines
    if if it is the source of the \"thyc\" file.  If  so, it simply returns the parse tree previously computed, otherwise it
    parses the source and generates a new \"thyc\" file  (assuming of course the source file contains no errors.)

    When the parser encounters import statements it searches for corresponding \"thrift\" or \"thyc\" files in paths corresponding to
    the specified namespace.

    Author(s): Mark Slee(mclee@facebook.com), Marc Kwiatkowski (marc@facebook.com)

    $Id:
"""

import lex
import os
import pickle
import string
import sys
import yacc

class Error(object):

    def __init__(self, start=0, end=0, message=""):
	if len(message) == 0:
	    raise Exception, "NO MESSAGE"
	self.message = message
	self.start = start
	self.end = end
    
    def __str__(self):
	return str(self.start)+": error: "+self.message

class SyntaxError(Error):
    def __init__(self, lexToken):
	Error.__init__(self, lexToken.lineno, lexToken.lineno, "syntax error "+str(lexToken.value))

class SymanticsError(Error):

    def __init__(self, definition, message):
	Error.__init__(self, definition.start, definition.end, message)
	self.definition = definition

    def __str__(self):
	return str(self.start)+": error: "+self.message

class ErrorException(Exception):

    def __init__(self, errors=None):
	self.errors = errors

class Definition(object):
    """ Abstract thrift IDL definition unit """

    def __init__(self, symbols=None, name="", id=None):
	if symbols:
	    self.lines(symbols)
	self.name = name
	self.id = id

    def validate(self):
	pass

    def lines(self, symbols):
	self.start = symbols.lineno(1)
	self.end = symbols.lineno(len(symbols) - 1)

class Identifier(Definition):
    """ An Identifier - name and optional integer id """

    def __init__(self, symbols, name, id=None):
	Definition.__init__(self, symbols, name, id)

    def __str__(self):
	result = self.name
	if self.id != 0:
	    result+="="+str(self.id)
	return result

class Type(Definition):
    """ Abstract Type definition """

    def __init__(self, symbols, name):
	Definition.__init__(self, symbols, name)
	self.name = name

    def __str__(self):
	return self.name

class TypeDef(Type):

    def __init__(self, symbols, name, definitionType):
	Type.__init__(self, symbols, name)
	self.definitionType = definitionType

    def __str__(self):
	return self.name+"<"+str(self.name)+", "+str(self.definitionType)+">"

""" Primitive Types """

class PrimitiveType(Type):

    def __init__(self, name):
	Type.__init__(self, None, name)


VOID_TYPE =  PrimitiveType("void")
BOOL_TYPE = PrimitiveType("bool")
STRING_TYPE =PrimitiveType("utf7")
UTF7_TYPE = PrimitiveType("utf7")
UTF8_TYPE = PrimitiveType("utf8")
UTF16_TYPE = PrimitiveType("utf16")
BYTE_TYPE = PrimitiveType("u08")
I08_TYPE = PrimitiveType("i08")
I16_TYPE = PrimitiveType("i16")
I32_TYPE = PrimitiveType("i32")
I64_TYPE = PrimitiveType("i64")
U08_TYPE = PrimitiveType("u08")
U16_TYPE = PrimitiveType("u16")
U32_TYPE = PrimitiveType("u32")
U64_TYPE = PrimitiveType("u64")
FLOAT_TYPE = PrimitiveType("float")

PRIMITIVE_MAP = {
    "void" : VOID_TYPE,
    "bool" : BOOL_TYPE,
    "string": UTF7_TYPE,
    "utf7": UTF7_TYPE,
    "utf8": UTF8_TYPE,
    "utf16": UTF16_TYPE,
    "byte" : U08_TYPE,
    "i08": I08_TYPE,
    "i16": I16_TYPE,
    "i32": I32_TYPE,
    "i64": I64_TYPE,
    "u08": U08_TYPE,
    "u16": U16_TYPE,
    "u32": U32_TYPE,
    "u64": U64_TYPE,
    "float": FLOAT_TYPE
}

""" Collection Types """

class CollectionType(Type):

    def __init__(self, symbols, name):
	Type.__init__(self, symbols, name)

class Map(CollectionType):

    def __init__(self, symbols, keyType, valueType):
	CollectionType.__init__(self, symbols, "map<"+keyType.name+","+valueType.name +">")
	self.keyType = keyType
	self.valueType = valueType

class Set(CollectionType):

    def __init__(self, symbols, valueType):
	CollectionType.__init__(self, symbols, "set<"+valueType.name+">")
	self.valueType = valueType

class List(CollectionType):

    def __init__(self, symbols, valueType):
	CollectionType.__init__(self, symbols, "list<"+valueType.name+">")
	self.valueType = valueType

class Enum(Definition):

    def __init__(self, symbols, name, enumDefs):
	Definition.__init__(self, symbols, name)
	self.enumDefs = enumDefs

    def validate(self):
	ids = {}
	names = {}
	errors = []

	for enumDef in self.enumDefs:

	    if enumDef.name in names:
 		errors.append(SymanticsError(enumDef, self.name+"."+str(enumDef.name)+" already defined at line "+str(names[enumDef.name].start)))
	    else:
		names[enumDef.name] = enumDef

	    if enumDef.id != None:
		oldEnumDef = ids.get(enumDef.id)
		if oldEnumDef:
		    errors.append(SymanticsError(enumDef, "enum "+self.name+" \""+str(enumDef.name)+"\" uses constant already assigned to \""+oldEnumDef.name+"\""))
		else:
		    ids[enumDef.id] = enumDef
	
	if len(errors):
	    raise ErrorException(errors)

	def assignId(enumDef, currentId, ids):
	    'Finds the next available id number for an enum definition'

	    id= currentId + 1

	    while id in ids:
		id += 1
	    
	    enumDef.id = id

	    ids[enumDef.id] = enumDef

	# assign ids for all enum defs with unspecified ids
	
	currentId = 0
	
	for enumDef in self.enumDefs:
	    if not enumDef.id:
		assignId(enumDef, currentId, ids)
		currentId = enumDef.id

    def __repr__(self):
	return str(self)
	
    def __str__(self):
	return self.name+"<"+string.join(map(lambda enumDef: str(enumDef), self.enumDefs), ", ")

class EnumDef(Definition):

    def __init__(self, symbols, name, id=None):
	Definition.__init__(self, symbols, name, id)

    def __repr__(self):
	return str(self)

    def __str__(self):
	result = self.name
	if self.id:
	    result+= ":"+str(self.id)
	return result


class Field(Definition):

    def __init__(self, symbols, type, identifier):
	Definition.__init__(self, symbols, identifier.name, identifier.id)
	self.type = type
	self.identifier = identifier

    def __str__(self):
	return "<"+str(self.type)+", "+str(self.identifier)+">"

def validateFieldList(fieldList):

    errors = []
    names = {}
    ids = {}

    for field in fieldList:

	if field.name in names:
	    oldField = names[field.name]
	    errors.append(SymanticsError(field, "field \""+field.name+"\" already defined at "+str(oldField.start)))
	else:
	    names[field.name] = field

	if field.id != None:
	    oldField = ids.get(field.id)
	    if oldField:
		errors.append(SymanticsError(field, "field \""+field.name+"\" uses constant already assigned to \""+oldField.name+"\""))
	    else:
		ids[field.id] = field
	
    if len(errors):
	raise ErrorException(errors)

class Struct(Type):

    def __init__(self, symbols, name, fieldList):
	Type.__init__(self, symbols, name)
	self.fieldList = fieldList

    def validate(self):
	validateFieldList(self.fieldList)

    def __str__(self):
	return self.name+"<"+string.join(map(lambda a: str(a), self.fieldList), ", ")+">"

class Function(Definition):

    def __init__(self, symbols, name, resultType, argFieldList):
	Definition.__init__(self, symbols, name)
	self.resultType = resultType
	self.argFieldList = argFieldList

    def validate(self):
	validateFieldList(self.argFieldList)
    
    def __str__(self):
	return self.name+"("+string.join(map(lambda a: str(a), self.argFieldList), ", ")+") => "+str(self.resultType)

class Service(Definition):

    def __init__(self, symbols, name, functionList):
	Definition.__init__(self, symbols, name)
	self.functionList = functionList

    def validate(self):

	errors = []
	functionNames = {}
	for function in self.functionList:
	    if function.name in functionNames:
		oldFunction = functionName[function.name]
		errors.append(SymanticsError(function, "function "+function.name+" already defined at "+str(oldFunction.start)))
	
	if len(errors):
	    raise ErrorException(errors)

    def __str__(self):
	return self.name+"("+string.join(map(lambda a: str(a), self.functionList), ", ")+")"

class Program(object):

    def __init__(self, symbols=None, name="", definitions=None, serviceMap=None, typedefMap=None, enumMap=None, structMap=None, collectionMap=None,
                 primitiveMap=None):

	self.name = name

        if not definitions:
            definitions = []
        self.definitions = definitions

	if not serviceMap:
	    serviceMap = {}
	self.serviceMap = serviceMap

	if not typedefMap:
	    typedefMap = {}
	self.typedefMap = typedefMap

	if not enumMap:
	    enumMap = {}
	self.enumMap = enumMap

	if not structMap:
	    structMap = {}
	self.structMap = structMap

	if not collectionMap:
	    collectionMap = {}
	self.collectionMap = collectionMap

	if not primitiveMap:
	    primitiveMap = PRIMITIVE_MAP
	self.primitiveMap = primitiveMap

    def addDefinition(self, definition, definitionMap, definitionTypeName):
	
	oldDefinition = definitionMap.get(definition.name)
	if oldDefinition:
	    raise ErrorException([SymanticsError(definition, definitionTypeName+" "+definition.name+" is already defined at "+str(oldDefinition.start))])
	else:
	    definitionMap[definition.name] = definition

        # keep an ordered list of definitions so that stub/skel generators can determine the original order

        self.definitions.append(definition)

    def addStruct(self, struct):
	self.addDefinition(struct, self.structMap, "struct")

    def addTypedef(self, typedef):
	self.addDefinition(typedef, self.typedefMap, "typedef")
	
    def addEnum(self, enum):
	self.addDefinition(enum, self.enumMap, "enum")

    def addService(self, service):
	self.addDefinition(service, self.serviceMap, "service")

    def addCollection(self, collection):
	if collection.name in self.collectionMap:
	    return
	else:
	    self.collectionMap[collection.name] = collection

    def getType(self, parent, symbol):
	""" Get the type definition for a symbol"""
	
	typeName = None

	if isinstance(symbol, Type):
	    return symbol
	elif isinstance(symbol, Field):
	    typeName = symbol.type.name
	elif isinstance(symbol, Identifier):
	    typeName = symbol.name
	else:
	    raise ErrorException([SymanticsError(parent, "unknown symbol \""+str(symbol)+"\"")])

	for map in (self.primitiveMap, self.collectionMap, self.typedefMap, self.enumMap, self.structMap):
	    if typeName in map:
		return map[typeName]

	raise ErrorException([SymanticsError(parent, "\""+typeName+"\"  is not defined.")])

    def hasType(self, parent, symbol):
	""" Determine if a type definition exists for the symbol"""

        return self.getType(parent, symbol) == True

    def validate(self):

	errors = []

        # Verify that struct fields types, collection key and element types, and typedef defined types exists and replaces
        # type names with references to the type objects

	for struct in self.structMap.values():
	    for field in struct.fieldList:
		try:
		    field.type = self.getType(struct, field)
		except ErrorException, e:
		    errors+= e.errors

	for collection in self.collectionMap.values():
            try:
                if isinstance(collection, Map):
                    collection.keyType = self.getType(collection, collection.keyType)

                collection.valueType = self.getType(collection, collection.valueType)
            
            except ErrorException, e:
                errors+= e.errors

	for typedef in self.typedefMap.values():
            try:
                typedef.definitionType = self.getType(self, typedef.definitionType)
            
            except ErrorException, e:
                errors+= e.errors

        # Verify that service fuunction result and arg list types exist and replace type name with reference to definition

	for service in self.serviceMap.values():
	    for function in service.functionList:
		try:
		    function.resultType  = self.getType(service, function.resultType)
		except ErrorException, e:
		    errors+= e.errors

		for field in function.argFieldList:
		    try:
			field.type = self.getType(function, field)
		    except ErrorException, e:
			errors+= e.errors

	if len(errors):
	    raise ErrorException(errors)
		    

class Parser(object):
    
    reserved = ("BYTE",
                # "CONST",
		"DOUBLE",
		"ENUM", 
                # "EXCEPTION",
                # "EXTENDS",
		"I08",
		"I16",
		"I32", 
		"I64",
		"LIST", 
		"MAP",
		"SERVICE", 
		"SET",
                # "STATIC",
		"STRING",
		"STRUCT", 
                # "SYNCHRONIZED",
		"TYPEDEF",
		"U08", 
		"U16",
		"U32", 
		"U64", 
		"UTF16",
		"UTF8",
		"VOID"
                )

    tokens = reserved + (
	# Literals (identifier, integer constant, float constant, string constant, char const)
	'ID', 'ICONST', 'SCONST', 'FCONST', 
	# Operators default=, optional*, variable...
	'ASSIGN',  #'OPTIONAL', 'ELLIPSIS',
	# Delimeters ( ) { } < > , . ; :
	'LPAREN', 'RPAREN',
	'LBRACE', 'RBRACE',
	'LANGLE', 'RANGLE',
	'COMMA' #, 'PERIOD', 'SEMI' , 'COLON'
	)

    precendence = ()

    reserved_map = {}

    for r in reserved:
	reserved_map[r.lower()] = r

    def t_ID(self, t):
	r'[A-Za-z_][\w_]*'
	t.type = self.reserved_map.get(t.value,"ID")
	return t
    
    # Completely ignored characters
    t_ignore           = ' \t\x0c'
    
#    t_OPTIONAL         = r'\*'
    t_ASSIGN           = r'='
	
    # Delimeters
    t_LPAREN           = r'\('
    t_RPAREN           = r'\)'
    t_LANGLE           = r'\<'
    t_RANGLE           = r'\>'
    t_LBRACE           = r'\{'
    t_RBRACE           = r'\}'
    t_COMMA            = r','
#    t_PERIOD           = r'\.'
#    t_SEMI             = r';'
#    t_COLON            = r':'
#    t_ELLIPSIS         = r'\.\.\.'
    
    # Integer literal
    t_ICONST = r'\d+([uU]|[lL]|[uU][lL]|[lL][uU])?'

    # Floating literal
    t_FCONST = r'((\d+)(\.\d+)(e(\+|-)?(\d+))? | (\d+)e(\+|-)?(\d+))([lL]|[fF])?'

    # String literal
    t_SCONST = r'\"([^\\\n]|(\\.))*?\"'

    # Comments
    def t_comment(self, t):
	r'(?:/\*(.|\n)*?\*/)|(?://[^\n]*\n)'
	t.lineno += t.value.count('\n')
	    
    def t_error(self, t):
	print "Illegal character %s" % repr(t.value[0])
	t.skip(1)

    # Newlines
    def t_newline(self, t):
	r'\n+'
	t.lineno += t.value.count("\n")
	
    def p_program(self, p):
	'program : definitionlist'
	pass
    
    def p_definitionlist_1(self, p):
	'definitionlist : definitionlist definition'
	pass

    def p_definitionlist_2(self, p):
	'definitionlist :'
	pass
    
    def p_definition_1(self, p):
	'definition : typedef'
	self.pdebug("p_definition_1", p)
	p[0] = p[1]
	try:
	    self.program.addTypedef(p[0])
	except ErrorException, e:
	    self.errors+= e.errors

    def p_definition_2(self, p):
	'definition : enum'
	self.pdebug("p_definition_2", p)
	p[0] = p[1]
	try:
	    self.program.addEnum(p[0])
	except ErrorException, e:
	    self.errors+= e.errors
    
    def p_definition_3(self, p):
	'definition : struct'
	self.pdebug("p_definition_3", p)
	p[0] = p[1]
	try:
	    self.program.addStruct(p[0])
	except ErrorException, e:
	    self.errors+= e.errors
    
    def p_definition_4(self, p):
	'definition : service'
	self.pdebug("p_definition_4", p)
	p[0] = p[1]
	try:
	    self.program.addService(p[0])
	except ErrorException, e:
	    self.errors+= e.errors

    def p_typedef(self, p):
	'typedef : TYPEDEF definitiontype ID'
	self.pdebug("p_typedef", p)
	p[0] = TypeDef(p, p[3], p[2])
	try:
	    p[0].validate()

	except ErrorException, e:
	    self.errors+= e.errors

#    def p_definition_or_referencye_type_1(self, p):
#       XXX need to all typedef struct foo foo_t by allowing references
#	pass
	    
    def p_enum(self, p):
	'enum : ENUM ID LBRACE enumdeflist RBRACE'
	self.pdebug("p_enum", p)
	p[0] = Enum(p, p[2], p[4])

	try:
	    p[0].validate()
	except ErrorException, e:
	    self.errors+= e.errors
	
    def p_enumdeflist_1(self, p):
	'enumdeflist : enumdeflist COMMA enumdef'
	self.pdebug("p_enumdeflist_1", p)
	p[0] = p[1] + (p[3],)

    def p_enumdeflist_2(self, p):
	'enumdeflist : enumdef' 
	self.pdebug("p_enumdeflist_2", p)
	p[0] = (p[1],)

    def p_enumdef_0(self, p):
	'enumdef : ID ASSIGN ICONST'
	self.pdebug("p_enumdef_0", p)
	p[0] = EnumDef(p, p[1], int(p[3]))

    def p_enumdef_1(self, p):
	'enumdef : ID'
	self.pdebug("p_enumdef_1", p)
	p[0] = EnumDef(p, p[1])

    def p_struct(self, p):
	'struct :  STRUCT ID LBRACE fieldlist RBRACE'
	self.pdebug("p_struct", p)
	p[0] = Struct(p, p[2], p[4])

	try:
	    p[0].validate()
	except ErrorException, e:
	    self.errors+= e.errors

    def p_service(self, p):
	'service : SERVICE ID LBRACE functionlist RBRACE'
	self.pdebug("p_service", p)
	p[0] =  Service(p, p[2], p[4])
	try:
	    p[0].validate()
	except ErrorException, e:
	    self.errors+= e.errors

    def p_functionlist_1(self, p):
        'functionlist : functionlist function'
	self.pdebug("p_functionlist_1", p)
	p[0] = p[1] + (p[2],)

    def p_functionlist_2(self, p):
        'functionlist :'
	self.pdebug("p_functionlist_2", p)
	p[0] = ()

    def p_function(self, p):
	'function : functiontype functionmodifiers ID LPAREN fieldlist RPAREN'
	self.pdebug("p_function", p)
	p[0] = Function(p, p[3], p[1], p[5])
	try:
	    p[0].validate()
	except ErrorException, e:
	    self.errors+= e.errors

    def p_functionmodifiers(self, p):
	'functionmodifiers :'
	self.pdebug("p_functionmodifiers", p)
	p[0] = ()

    def p_fieldlist_1(self, p):
	'fieldlist : fieldlist COMMA field'
	self.pdebug("p_fieldlist_1", p)
	p[0] = p[1] + (p[3],)

    def p_fieldlist_2(self, p):
	'fieldlist : field'
	self.pdebug("p_fieldlist_2", p)
	p[0] = (p[1],)

    def p_fieldlist_3(self, p):
	'fieldlist :'
	self.pdebug("p_fieldlist_3", p)
	p[0] = ()

    def p_field_1(self, p):
        'field : fieldtype ID ASSIGN ICONST'
	self.pdebug("p_field_1", p)
	p[0] = Field(p, p[1], Identifier(None, p[2], int(p[4])))

    def p_field_2(self, p):
        'field : fieldtype ID'
	self.pdebug("p_field_2", p)
	p[0] = Field(p, p[1], Identifier(None, p[2]))

    def p_definitiontype_1(self, p):
        'definitiontype : basetype'
	self.pdebug("p_definitiontype_1", p)
	p[0] = p[1]

    def p_definitiontype_2(self, p):
        'definitiontype : collectiontype'
	self.pdebug("p_definitiontype_2", p)
	p[0] = p[1]

    def p_functiontype_1(self, p):
        'functiontype : fieldtype'
	self.pdebug("p_functiontype_1", p)
	p[0] = p[1]
		   
    def p_functiontype_2(self, p):
        'functiontype : VOID'
	self.pdebug("p_functiontype_2", p)
	p[0] = self.program.primitiveMap[p[1].lower()]

    def p_fieldtype_1(self, p):
        'fieldtype : ID'
	self.pdebug("p_fieldtype_1", p)
	p[0] = Identifier(p, p[1])

    def p_fieldtype_2(self, p):
        'fieldtype : basetype'
	self.pdebug("p_fieldtype_2", p)
	p[0] = p[1]

    def p_fieldtype_3(self, p):
        'fieldtype : collectiontype'
	self.pdebug("p_fieldtype_3", p)
	p[0] = p[1]

    def p_basetype_1(self, p):
        'basetype : STRING'
	self.pdebug("p_basetype_1", p)
	p[0] = self.program.primitiveMap[p[1].lower()]

    def p_basetype_2(self, p):
        'basetype : BYTE'
	self.pdebug("p_basetype_2", p)
	p[0] = self.program.primitiveMap[p[1].lower()]

    def p_basetype_3(self, p):
        'basetype : I08'
	self.pdebug("p_basetype_3", p)
	p[0] = self.program.primitiveMap[p[1].lower()]

    def p_basetype_4(self, p):
        'basetype : U08'
	self.pdebug("p_basetype_4", p)
	p[0] = self.program.primitiveMap[p[1].lower()]

    def p_basetype_5(self, p):
        'basetype : I16'
	self.pdebug("p_basetype_5", p)
	p[0] = self.program.primitiveMap[p[1].lower()]

    def p_basetype_6(self, p):
        'basetype : U16'
	self.pdebug("p_basetype_6", p)
	p[0] = self.program.primitiveMap[p[1].lower()]

    def p_basetype_7(self, p):
        'basetype : I32'
	self.pdebug("p_basetype_7", p)
	p[0] = self.program.primitiveMap[p[1].lower()]

    def p_basetype_8(self, p):
        'basetype : U32'
	self.pdebug("p_basetype_8", p)
	p[0] = self.program.primitiveMap[p[1].lower()]

    def p_basetype_9(self, p):
        'basetype : I64'
	self.pdebug("p_basetype_9", p)
	p[0] = self.program.primitiveMap[p[1].lower()]

    def p_basetype_10(self, p):
        'basetype : U64'
	self.pdebug("p_basetype_10", p)
	p[0] = self.program.primitiveMap[p[1].lower()]

    def p_basetype_11(self, p):
        'basetype : UTF8'
	self.pdebug("p_basetype_11", p)
	p[0] = self.program.primitiveMap[p[1].lower()]

    def p_basetype_12(self, p):
        'basetype : UTF16'
	self.pdebug("p_basetype_12", p)
	p[0] = self.program.primitiveMap[p[1].lower()]

    def p_basetype_13(self, p):
        'basetype : DOUBLE'
	self.pdebug("p_basetype_13", p)
	p[0] = self.program.primitiveMap[p[1].lower()]

    def p_collectiontype_1(self, p):
        'collectiontype : maptype'
	self.pdebug("p_collectiontype_1", p)
	p[0] = p[1]
	self.program.addCollection(p[0])

    def p_collectiontype_2(self, p):
        'collectiontype : settype'
	self.pdebug("p_collectiontype_2", p)
	p[0] = p[1]
	self.program.addCollection(p[0])

    def p_collectiontype_3(self, p):
        'collectiontype : listtype'
	self.pdebug("p_collectiontype_3", p)
	p[0] = p[1]
	self.program.addCollection(p[0])

    def p_maptype(self, p):
        'maptype : MAP LANGLE fieldtype COMMA fieldtype RANGLE'
	self.pdebug("p_maptype", p)
	p[0] = Map(p, p[3], p[5])

    def p_settype(self, p):
        'settype : SET LANGLE fieldtype RANGLE'
	self.pdebug("p_settype", p)
	p[0] = Set(p, p[3])

    def p_listtype(self, p):
        'listtype : LIST LANGLE fieldtype RANGLE'
	self.pdebug("p_listtype", p)
	p[0] = Set(p, p[3])

    def p_error(self, p):
        self.errors.append(SyntaxError(p))

    def pdebug(self, name, p):
	if self.debug:
	    print(name+"("+string.join(map(lambda t: "<<"+str(t)+">>", p), ", ")+")")

    def __init__(self, **kw):
        self.debug = kw.get('debug', 0)
        self.names = { }
	self.program = Program()
	self.errors = []

        try:
            modname = os.path.split(os.path.splitext(__file__)[0])[1] + "_" + self.__class__.__name__
        except:
            modname = "parser"+"_"+self.__class__.__name__
        self.debugfile = modname + ".dbg"
        self.tabmodule = modname + "_" + "parsetab"
        #print self.debugfile, self.tabmodule

        # Build the lexer and parser
        lex.lex(module=self, debug=self.debug)
        yacc.yacc(module=self,
                  debug=self.debug,
                  debugfile=self.debugfile,
                  tabmodule=self.tabmodule)

    def parsestring(self, s, filename=""):
	yacc.parse(s)

	if len(self.errors) == 0:
	    try:
		self.program.validate()
	    except ErrorException, e:
		self.errors+= e.errors
	    
	if len(self.errors):
	    for error in self.errors:
		print(filename+":"+str(error))

    def parse(self, filename, doPickle=True):

	f = file(filename, "r")

	self.parsestring(f.read(), filename)

        if len(self.errors) == 0 and doPickle:

	    outf = file(os.path.splitext(filename)[0]+".thyc", "w")

	    pickle.dump(self.program, outf)

if __name__ == '__main__':

    parser = Parser(debug=True);

    parser.parse(sys.argv[1])
