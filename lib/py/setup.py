from distutils.core import setup

setup(name = 'Thrift',
      version = '1.0',
      description = 'Thrift Python Libraries',
      author = ['Mark Slee'],
      author_email = ['mcslee@facebook.com'],
      url = 'http://code.facebook.com/thrift',
      packages = ['thrift', 'thrift.protocol', 'thrift.transport'],
      package_dir = {'thrift' : 'src'},
      )

