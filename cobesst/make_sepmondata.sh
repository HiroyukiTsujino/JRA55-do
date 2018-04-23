#!/bin/bash -f

set -e

if [ x${3} == x ]; then
   echo "Usage: ${0} start_year end_year end_mon"
   exit
fi

yearst=${1}
yeared=${2}
moned=${3}

################

sed -e s%@ibyr@%${yearst}% \
    -e s%@ieyr@%${yeared}% \
    -e s%@iemn@%${moned}% \
    namelist.sepmondata_template > namelist.sepmondata

./separate_cobesst_monthly
