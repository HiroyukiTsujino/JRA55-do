#!/bin/sh -f

set -e

i=1

while [ ${i} -lt 23 ];
do
  if [ ${i} -ne 10 ]; then
    for item in nswrf nlwrf latent sensible evapor precip
    do
      ./have_region_select.sh ${item} ${i}
    done
  fi
  i=$((${i}+1))
  echo ${i}
done
