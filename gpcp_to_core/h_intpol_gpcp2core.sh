#!/bin/bash -f

set -e

yearst=${1}
yeared=${2}

year=${yearst}

orgdir="/work116/htsujino/GPCP-v2_3/grads"
newdir="/work116/htsujino/GPCP-v2_3/grads_monthly_T62"

while [ ${year} -le ${yeared} ];
do
  for mm in 01 02 03 04 05 06 07 08 09 10 11 12
  do
    for item in precip
    do
      flin1="${orgdir}/${item}.${year}${mm}"
      flot1="${newdir}/${item}.${year}${mm}"
      num_data=1
      echo ${flin1} ${flot1}
      sed -e s%@flin1@%"${flin1}"% \
          -e s%@flot1@%"${flot1}"% \
          -e s%@num_data@%${num_data}% \
          namelist_gpcp2core_template > namelist_gpcp2core
      ./gpcp_to_core
    done
  done
  yearn=`expr ${year} + 1`
  year=${yearn}
done
