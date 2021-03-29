import os
import sys

if sys.version_info[0] == 2:
    _ENCODE = sys.getfilesystemencoding()

    def path_join(*args):
        bin_args = map(lambda a: a.decode(_ENCODE), args)
        return os.path.join(*bin_args).encode(_ENCODE)

    def str_join(left, right):
        bin_args = map(lambda a: a.decode(_ENCODE), right)
        b = left.decode(_ENCODE)
        return b.join(bin_args).encode(_ENCODE)

    logfile_open = open

else:

    path_join = os.path.join
    str_join = str.join

    def logfile_open(*args):
        return open(*args, errors='replace')
