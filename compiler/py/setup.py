from distutils.core import setup

setup(name = 'Thrift',
      version = '1.0',
      description = 'Thrift IDL compiler',
      author = ['Mark Slee', 'Marc Kwiatkowski'],
      author_email = ['mcslee@facebook.com', 'marc@facebook.com'],
      url = 'http://code.facebook.com/thrift',
      package_dir = {'thrift' : 'src'},
      py_modules = ['thrift.parser', 'thrift.cpp_generator', 'thrift.generator', 'thrift.php_generator'],
      scripts = ['src/thrift']
      )

