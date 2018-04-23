#!/bin/bash -evh

set -e

if [ x${2} == x ]; then
   echo "Usage: ${0} start_year end_year end_day"
   exit
fi

yrstr=${1}
yrend=${2}
enddy_last=${3}

source ../util/datel_leap.sh

ln -sf NAMELIST.MXE.JRA55 NAMELIST.MXE

idir=/denkei-shared/og1/htsujino/SURF_FLUX/forcing/jra_for_mricom_slprs/TL319_grid
odir=/denkei-shared/og1/htsujino/SURF_FLUX/forcing/jra_for_mricom_slprs_filled/TL319_grid

exe=fill_land_ctl

yr=${yrstr}

while [ ${yr} -le ${yrend} ];
do

  if [ ${yr} -eq ${yrend} ]; then
    if [ x${enddy_last} == x ]; then
      lists="${yr} ${yr}010100 ${yr}010103 ${yr}123121"
    else
      lists="${yr} ${yr}010100 ${yr}010103"
    fi
  else
    lists="${yr} ${yr}010100 ${yr}010103 ${yr}123121"
  fi    

  nleap=`isleap ${yr}`
  if [ ${yr} -eq ${yrend} ]; then
    if [ x${enddy_last} == x ]; then
      enddy=`expr ${nleap} + 365`
    else
      enddy=${enddy_last}
    fi
  else
    enddy=`expr ${nleap} + 365`
  fi

  for i in ${lists}
  do
      if [ ${i} -eq ${yr} ]; then
	  recnum=`expr ${enddy} \* 8`
      else
	  recnum=1
      fi
      echo "recnum = ${recnum}"
      echo ${odir}/${yr}
      if [ ! -d ${odir}/${yr} ]; then
	  mkdir ${odir}/${yr}
      fi
      for var in slprs
      do
	org_name="${idir}/${yr}/${var}.${i}"
	filled_name="${odir}/${yr}/${var}.${i}"
	echo ${org_name}
	echo ${filled_name}
	sed -e s%@ORG_NAME@%${org_name}% \
            -e s%@FILLED_NAME@%${filled_name}% \
	    -e s%@RECNUM@%${recnum}% \
	    namelist.fill_land_template > namelist.fill_land
	./${exe} < namelist.fill_land
      done
  done

  yr=`expr ${yr} + 1`

done
