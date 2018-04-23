#!/bin/sh -f

set -e

if [ x${3} = x ]; then
  echo "Usage: ${0} start_year end_year version"
fi

styear=${1}
endyear=${2}
ver=${3}

#mondir="../linkdir/forcing/jra_cobesst_monthly_${ver}"
#anndir="../linkdir/forcing/jra_cobesst_annual_${ver}"

#mondir="../linkdir/forcing/jra55fcst_monthly_1x1"
#anndir="../linkdir/forcing/jra55fcst_annual_1x1"

#mondir="../linkdir/forcing/jra55Cfcst_monthly_1x1"
#anndir="../linkdir/forcing/jra55Cfcst_annual_1x1"

#mondir="../linkdir/forcing/jra55fcst_${ver}_monthly_1x1"
#anndir="../linkdir/forcing/jra55fcst_${ver}_annual_1x1"


################

#fpathin="${mondir}/u10m"
#fpathout="${anndir}/u10m"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

#fpathin="${mondir}/v10m"
#fpathout="${anndir}/v10m"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

#fpathin="${mondir}/wind10m"
#fpathout="${anndir}/wind10m"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

#fpathin="${mondir}/wvn10m"
#fpathout="${anndir}/wvn10m"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

#fpathin="${mondir}/dtu10m"
#fpathout="${anndir}/dtu10m"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

#fpathin="${mondir}/dqu10m"
#fpathout="${anndir}/dqu10m"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

#fpathin="${mondir}/tmp10m"
#fpathout="${anndir}/tmp10m"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

#fpathin="${mondir}/sph10m"
#fpathout="${anndir}/sph10m"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

#fpathin="${mondir}/tmp2m"
#fpathout="${anndir}/tmp2m"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

#fpathin="${mondir}/sph2m"
#fpathout="${anndir}/sph2m"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20
