#!python
import sys
from thrift import cpp_generator
from thrift import generator
from thrift import parser

def thrift(source, cpp=False, perl=False, php=False, python=False, java=False, ruby=False, debug=False):

    generators = []

    if cpp:
	generators.append(cpp_generator.CPPGenerator())
    
    p = parser.Parser(debug=debug)

    p.parse(source, False)

    for generator in generators:
	generator(p.program, source)

    if len(p.errors):
	return -1
    else:
	return 0

def main(args):

    cpp = False
    perl = False
    php = False
    python = False
    java = False
    ruby = False

    debug = False

    if "--cpp" in args:
	cpp = True
	args.remove("--cpp")
    if "--debug" in args:
	debug = True
	args.remove("--debug")

    filename = args[-1]

    result = thrift(filename, cpp, java, perl, php, python, ruby, debug)

    sys.exit(result)

if __name__ == '__main__':
    main(sys.argv)

