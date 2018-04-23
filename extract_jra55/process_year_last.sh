#!/bin/bash -f

set -e

if [ x${3} == x ]; then
   echo "Usage: ${0} year mon day"
   exit
fi

year=${1}
mon=${2}
day=${3}

. ../util/datel_leap.sh

num_days=`ndatey ${year} ${mon} ${day}`

echo "num_days = ${num_days}"

################

sh grib2flatbin_surf.sh  ${year} ${year} 1 ${num_days}
sh grib2flatbin_phy2m.sh ${year} ${year} 1 ${num_days}
sh grib2flatbin_ice.sh   ${year} ${year} 1 ${num_days}
