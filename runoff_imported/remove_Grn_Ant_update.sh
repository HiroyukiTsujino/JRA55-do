#!/bin/bash -f

set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} year"
  exit
fi

rm -f namelist_runoff_update
ln -s namelist_runoff_update_${1} namelist_runoff_update

target=remove_Greenland_Antarctica_update
make ${target}

./${target}
