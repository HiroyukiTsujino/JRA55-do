#!/bin/bash -f

set -e

if [ x${1} == x ]; then
  echo "Usage: ${0} year"
fi

year=${1}

rsync -au front:/mri-data/jra-55/Hist/Daily/fcst_phyland/${year}?? ./fcst_phyland/.
