#!/bin/bash -f

if [ x${2} == x ]; then
   echo "Usage: ./make_runoff_st_ed.sh start_year end_year"
   exit
fi

yearst=${1}
yeared=${2}

################

orgdir=/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5/runoff
newdir=/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5/runoff

################

. ../util/datel_leap.sh

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}
  leap=`isleap ${year}`
  iend=`expr 365 + ${leap}`
  echo "${year} iend = ${iend}"
  sed -e s/@year@/${year}/ \
      -e s/@yyyymmdd@/${year}0101/ \
      -e s/@day_year@/1/ \
      namelist.extract_template > namelist.extract
  ./extract_onerecord
  sed -e s/@year@/${year}/ \
      -e s/@yyyymmdd@/${year}1231/ \
      -e s/@day_year@/${iend}/ \
      namelist.extract_template > namelist.extract
  ./extract_onerecord
  yearn=`expr ${year} + 1`
  year=${yearn}
done
