#!/bin/bash -f

set -e

if [ x${3} = x ]; then
  echo "Usage: ${0} start_year end_year var_name"
  exit
fi

ibyr=${1}
ieyr=${2}
item=${3}

indir=../linkdir/forcing/jra55fcst_v1_3_prod1_3hr_TL319
outdir=../linkdir/forcing/jra55fcst_v1_3_prod1_monthly_TL319
nhint=3

sed -e s%@item@%"${item}"% \
    -e s%@ibyr@%"${ibyr}"% \
    -e s%@ieyr@%"${ieyr}"% \
    -e s%@nhint@%"${nhint}"% \
    -e s%@indir@%"${indir}"% \
    -e s%@outdir@%"${outdir}"% \
namelist_org2monthly_scalar_template > namelist_org2monthly_scalar

./make_org_to_monthly_scalar
