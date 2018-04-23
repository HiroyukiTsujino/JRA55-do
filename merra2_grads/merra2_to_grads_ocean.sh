#!/bin/bash

set -e

yearst=${1}
yeared=${2}
var_name=${3}
var_name_out=${4}

indir=/work/a/htsujino/MERRA2/rawdata
outdir=/work/a/htsujino/MERRA2/grads_monthly
################

. ../util/datel_leap.sh

year=${yearst}

while [ ${year} -le ${yeared} ];
do

  yearn=`expr ${year} + 1`

  echo "year = ${year}  year_next = ${yearn}"

  if [ ${year} -le 1991 ]; then
    indx=100
  else
    if [ ${year} -le 2000 ]; then
      indx=200
    else
      if [ ${year} -le 2010 ]; then
        indx=300
      else
        if [ ${year} -le 2015 ]; then
          indx=400
        fi
      fi
    fi
  fi

  sed -e s%@ibyr@%${year}% \
      -e s%@ieyr@%${year}% \
      -e s%@indx@%${indx}% \
      -e s%@indir@%${indir}% \
      -e s%@outdir@%${outdir}% \
      -e s%@var_in@%${var_name}% \
      -e s%@var_out@%${var_name_out}% \
      namelist.merra_ocean_template > namelist.merra_surf

  ./nc2grads_merra2

  year=${yearn}

done
