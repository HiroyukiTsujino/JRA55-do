#!/bin/bash

set -e

if [ x${2} = x ]; then
  echo "Usage: ${0} year num_days"
  exit
fi

year=${1}
num_days=${2}

sed -e s%@ibyr@%${year}% \
    -e s%@ieyr@%${year}% \
    -e s%@num_days@%${num_days}% \
    namelist_runoff_update_template > namelist_runoff_update_${year}

rm -f namelist_runoff_update
ln -s namelist_runoff_update_${year} namelist_runoff_update

target=remove_Greenland_Antarctica_update
make ${target}

./${target}
