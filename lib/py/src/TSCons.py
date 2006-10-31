from SCons.Builder import Builder

def scons_env(env):
  cppbuild = Builder(action = 'thrift --cpp $SOURCE')
  env.Append(BUILDERS = {'ThriftCpp' : cppbuild})

def gen_cpp(env, dir, file):
  scons_env(env)
  suffixes = ['_types.h', '_types.cpp']
  targets = map(lambda s: 'gen-cpp/' + file + s, suffixes)
  return env.ThriftCpp(targets, dir+file+'.thrift')
