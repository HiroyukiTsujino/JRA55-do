#!/bin/bash

set -e

yearst=${1}
yeared=${2}

cobesst_org=/mri-data/climate_1/Kai/COBE-SST/day
cobesst_dir=../linkdir/COBESST/daily

################

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}
  wgrib -s ${cobesst_org}/sst-glb.${year}.dat | egrep '(:WTMP:)' | wgrib -i -nh -ieee ${cobesst_org}/sst-glb.${year}.dat -o ${cobesst_dir}/grads/sst-glb.${year}
  wgrib -s ${cobesst_org}/sst-glb.${year}.dat 2> ${cobesst_dir}/stderr.${year}.txt | egrep '(:ICEC:)' | wgrib -i -nh -ieee ${cobesst_org}/sst-glb.${year}.dat -o ${cobesst_dir}/grads/ice-glb.${year}
  yearn=`expr ${year} + 1`
  year=${yearn}
done
