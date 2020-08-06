#!/bin/bash

set -e

yearst=${1}
yeared=${2}
item=${3}

orgdir="/work113/htsujino/20CRv3/grads"
newdir="../linkdir/forcing/20CRv3_3hr_TL319"

flin1_base=${orgdir}/${item}
flot1_base=${item}

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}

  sed -e s%@flin1_base@%"${flin1_base}"% \
      -e s%@flot1_base@%"${flot1_base}"% \
      -e s%@dir_out@%"${newdir}"% \
      -e s%@nyear@%${year}% \
      namelist_20CRv3tojra55_template > namelist_20CRv3tojra55

  ./20CRv3_to_jra

  yearn=`expr ${year} + 1`
  year=${yearn}
done
