import sys
import logging

if sys.version_info[0] == 2:

  from cStringIO import StringIO as BufferIO

  def binary_to_string(bin_val):
    """
    Change a binary value string b'' to a u''.
    """
    try:
      return bin_val.decode('utf8')
    except:
      return bin_val

  def string_to_binary(str_val):
    """
    Change a string to binary, u'' to b''
    """
    try:
      return str_val.encode('utf8')
    except:
      return str_val

else:

  from io import BytesIO as BufferIO

  def binary_to_string(bin_val):
    """
    Change a binary value to a string, b'' to ''.
    """
    try:
      return bin_val.decode('utf8')
    except:
      return bin_val

  def string_to_binary(str_val):
    """
    Change a string to binary, '' to b''
    """
    try:
      return bytearray(str_val, 'utf8')
    except:
      return str_val
