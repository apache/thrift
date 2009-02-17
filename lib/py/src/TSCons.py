# Copyright (c) 2006- Facebook
# Distributed under the Thrift Software License
#
# See accompanying file LICENSE or visit the Thrift site at:
# http://developers.facebook.com/thrift/

from os import path
from SCons.Builder import Builder

def scons_env(env, add=''):
  opath = path.dirname(path.abspath('$TARGET'))
  lstr = 'thrift --gen cpp -o ' + opath + ' ' + add + ' $SOURCE'
  cppbuild = Builder(action = lstr)
  env.Append(BUILDERS = {'ThriftCpp' : cppbuild})

def gen_cpp(env, dir, file):
  scons_env(env)
  suffixes = ['_types.h', '_types.cpp']
  targets = map(lambda s: 'gen-cpp/' + file + s, suffixes)
  return env.ThriftCpp(targets, dir+file+'.thrift')
