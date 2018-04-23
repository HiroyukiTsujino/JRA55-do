#!/bin/bash -f

set -e

if [ "x${3}" = "x" ]; then
  echo "Usage : ${0} start_year end_year item_name "
  exit
fi


yearst=${1}
yeared=${2}
item=${3}

sed -e s%@nbyr@%${yearst}% \
    -e s%@neyr@%${yeared}% \
    -e s%@item@%${item}% \
    namelist.make_ensemble4_mean_weight_template > namelist.make_ensemble_mean_weight

./make_ensemble_mean_weight
