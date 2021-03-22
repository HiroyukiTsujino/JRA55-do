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

pushd ${NEWDIR}
if [ -e runoff${year}.grd ]; then
  fdate=$( ls -l --time-style=+%Y%m%d runoff${year}.grd | awk '{print $6}' )
  if [ ! -d ${fdate} ]; then
    mkdir ${fdate}
  fi
  mv runoff${year}.grd ${fdate}/.
  mv runoff${year}mon.grd ${fdate}/.
fi
popd

cp ${ORGDIR}/runoff${year}.grd ${NEWDIR}/.
cp ${ORGDIR}/runoff${year}mon.grd ${NEWDIR}/.
cp ${ORGDIR}/runoff_ice.grd ${NEWDIR}/.

ln -sfn ../river_suzuki_v1_0/runoff${year}.grd ${RIVMASTER}/.
