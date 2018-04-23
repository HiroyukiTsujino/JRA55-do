#!/bin/bash -f

set -e

if [ x${2} == x ]; then
   echo "Usage: ./make_hcurl_interannual.sh start_year end_year"
   exit
fi

yearst=${1}
yeared=${2}

################

# v0_8
#indir=../linkdir/forcing/jra55fcst_v7_rad2_monthly_1x1
#outdir=../linkdir/forcing/jra55fcst_v7_rad2_monthly_1x1

# v1_1
indir=../linkdir/forcing/jra55fcst_monthly_1x1
outdir=../linkdir/forcing/jra55fcst_monthly_1x1

################

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo "YEAR = ${year}"
#  for m in 01
  for m in 01 02 03 04 05 06 07 08 09 10 11 12
  do
    yyyymm=${year}${m}
    sed -e s%@yyyymm@%${yyyymm}% \
        -e s%@indir@%${indir}% \
        -e s%@outdir@%${outdir}% \
    namelist.curl_tau_template > namelist.curl_tau
    ./curltau
  done
  yearn=`expr ${year} + 1`
  year=${yearn}

done
