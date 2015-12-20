import sys
import logging

if sys.version_info[0] == 2:

  from cStringIO import StringIO as BufferIO

  def binary_to_str(bin_val):
    try:
      return bin_val.decode('utf8')
    except:
      return bin_val

  def str_to_binary(str_val):
    try:
      return str_val.encode('utf8')
    except:
      return str_val

else:

  from io import BytesIO as BufferIO

  def binary_to_str(bin_val):
    try:
      return bin_val.decode('utf8')
    except:
      return bin_val

  def str_to_binary(str_val):
    try:
      return bytearray(str_val, 'utf8')
    except:
      return str_val
