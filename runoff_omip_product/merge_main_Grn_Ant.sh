#!/bin/bash -f

set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} year"
  exit
fi

rm -f namelist.merge_main_grn_ant
ln -s namelist.merge_main_grn_ant_${1} namelist.merge_main_grn_ant

target=merge_main_grn_ant
make ${target}

./${target}
