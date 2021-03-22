#!/bin/sh

set -e

if [ x${4} = x ]; then
  echo "Usage : ${0} start_year end_year directory(dataset name) file_basename"
  exit  
fi

yearst=${1}
yeared=${2}
#dir=../linkdir/forcing/${3}
#dir=/work115/htsujino/SURF_FLUX/forcing/${3}
#dir=/work116/htsujino/SURF_FLUX/forcing/${3}
#dir=/work116/htsujino/${3}/monthly/grads/ # COBESST
dir=../linkdir/verification/${3}
file_base=${4}

echo "directory = ${dir}, file_base = ${file_base}"

################

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo "Processing year ${year}"
  if [ -e ${dir}/${file_base}.${year} ];then
     rm -f ${dir}/${file_base}.${year}
  fi
  for mon in 01 02 03 04 05 06 07 08 09 10 11 12
  do
    cat ${dir}/${file_base}.${year}${mon} >> ${dir}/${file_base}.${year}
  done
  yearn=`expr ${year} + 1`
  year=${yearn}
done
