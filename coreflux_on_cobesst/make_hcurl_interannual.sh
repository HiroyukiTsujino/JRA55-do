#!/bin/bash -f

if [ x${2} == x ]; then
   echo "Usage: ./make_hcurl_interannual.sh start_year end_year"
   exit
fi

yearst=${1}
yeared=${2}

################

indir=/work116/htsujino/CORE/core_cobesst_monthly_dec2015
outdir=/work116/htsujino/CORE/core_cobesst_monthly_dec2015

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
