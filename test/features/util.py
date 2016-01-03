import argparse


def add_common_args(p):
  p.add_argument('--host', default='localhost')
  p.add_argument('--port', type=int)
  p.add_argument('--protocol')
  p.add_argument('--transport')
  p.add_argument('--ssl', action='store_true')


def parse_common_args(argv):
  p = argparse.ArgumentParser()
  add_common_args(p)
  return p.parse_args(argv)
