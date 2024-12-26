import os

path_join = os.path.join
str_join = str.join

def logfile_open(*args):
    return open(*args, errors='replace')
