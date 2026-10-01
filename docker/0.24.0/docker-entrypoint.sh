#!/bin/sh
set -e

if [ "$#" -eq 0 ]; then
	set -- thrift
elif [ "${1#-}" != "$1" ]; then
	set -- thrift "$@"
fi

exec "$@"
