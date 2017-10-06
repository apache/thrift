#!/bin/bash -e

dup puri -Ufiles.kpe.io -D/home/ftp/puri -C"(umask 022; /home/kevin/bin/remove-old-versions puri latest)" -su $*
