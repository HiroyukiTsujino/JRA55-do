#!/bin/bash -f

yyyymmddhh=${1}
yyyymm=${2}

newdir=../linkdir/forcing/jra_latlon

################

. ../util/datel_leap.sh

if [ ! -e ${newdir}/${yyyymm} ]; then
  echo "creating ${yyyymm}"
  mkdir ${newdir}/${yyyymm}
fi

#for item in slprs tmp2m sph2m u10m v10m dswrf dlwrf prcp brtmp evap sensible uflx vflx snow
#for item in evap sensible uflx vflx
#for item in lrain crain snow
#for item in uswrf ulwrf
do
  sed -e s/@yyyymmddhh@/${yyyymmddhh}/ \
      -e s/@yyyymm@/${yyyymm}/ \
      -e s/@item@/${item}/ \
      namelist.rg2latlon_template > namelist.rg2latlon
  ./rgrid2latlon
done
