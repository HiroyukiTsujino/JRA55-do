#!/bin/bash -f

set -e

if [ x${1} == "x" ]; then 
  echo 'Usage: ${0} criteria[loose or strict]'
  exit
fi

set -e

criteria=${1}

ln -sf namelist.daily2monthly_${criteria} namelist.daily2monthly

./daily2monthly

\rm namelist.daily2monthly
