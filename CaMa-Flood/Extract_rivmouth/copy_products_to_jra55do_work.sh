#!/bin/bash

set -e

if [ x${1} == x ]; then
   echo "Usage: ${0} year"
   exit
fi

year=${1}

ORGDIR=../OUTPUT/riv_v1.0_mri
NEWDIR=../../linkdir/river/river_suzuki_v1_0
RIVMASTER=../../linkdir/river/river_suzuki_v0_6

cp ${ORGDIR}/runoff${year}.grd ${NEWDIR}/.
cp ${ORGDIR}/runoff${year}mon.grd ${NEWDIR}/.
cp ${ORGDIR}/runoff_ice.grd ${NEWDIR}/.

ln -s ${NEWDIR}/runoff${year}.grd ${RIVMASTER}/.
