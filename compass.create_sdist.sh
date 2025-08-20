#! /bin/sh

cd lib/py
python3 -m venv /Users/ugo/.venvs/thrift
source /Users/ugo/.venvs/thrift/bin/activate
pip install setuptools
python3 setup.py sdist
