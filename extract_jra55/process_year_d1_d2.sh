#!/bin/bash -f

yearst=${1}
yeared=${2}

################

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  sh shift_2m_to_10m_tq_d1_d2.sh ${year} ${year}
  yearn=`expr ${year} + 1`
  year=${yearn}
done
