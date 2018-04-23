#!/bin/bash -f

set -e

rm -f namelist.core_to_cama
ln -s namelist.core_to_cama_antarctica  namelist.core_to_cama
./core_to_cama
