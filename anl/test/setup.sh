#!/bin/bash
#- Setup for unit tests.

cd ../lib
make
cd ../linkdir
ln -s /worka/ksakamoto/mxe .
cd ../anl/naotide
sh link_nao99b.sh

exit 0
