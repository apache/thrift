import sys
import generator
import cpp_generator
import parser

if __name__ == '__main__':

    args = sys.argv[1:]

    generators = []

    debug = False

    if "--cpp" in args:
	generators.append(cpp_generator.CPPGenerator())
	args.remove("--cpp")
    if "--debug" in args:
	debug = True
	args.remove("--debug")

    filename = args[-1]

    p = parser.Parser(debug=debug)

    p.parse(filename, False)

    if len(p.errors):
	sys.exit(-1)

    [g(p.program, filename) for g in generators]

