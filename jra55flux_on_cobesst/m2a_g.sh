#!/bin/sh

if [ x${3} = x ]; then
  echo "Usage: ${0} start_year end_year version (org v1_3_01 v1_5 ...)"
fi

styear=${1}
endyear=${2}
ver=${3}

#mondir="../linkdir/forcing/jra55fcst_monthly_1x1"
#anndir="../linkdir/forcing/jra55fcst_annual_1x1"

#mondir="../linkdir/forcing/jra55Cfcst_monthly_1x1"
#anndir="../linkdir/forcing/jra55Cfcst_annual_1x1"

# ocsv011
#mondir="../linkdir/forcing/jra55fcst_${ver}_monthly_1x1"
#anndir="../linkdir/forcing/jra55fcst_${ver}_annual_1x1"

# front
mondir="../linkdir/verification/jra55fcst_${ver}_monthly_1x1"
anndir="../linkdir/verification/jra55fcst_${ver}_annual_1x1"

################

fpathin="${mondir}/taux"
fpathout="${anndir}/taux"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

fpathin="${mondir}/tauy"
fpathout="${anndir}/tauy"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

fpathin="${mondir}/dswrf"
fpathout="${anndir}/dswrf"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

fpathin="${mondir}/dlwrf"
fpathout="${anndir}/dlwrf"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

#fpathin="${mondir}/dswrf_all"
#fpathout="${anndir}/dswrf_all"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

#fpathin="${mondir}/dlwrf_all"
#fpathout="${anndir}/dlwrf_all"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

fpathin="${mondir}/precip"
fpathout="${anndir}/precip"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

fpathin="${mondir}/nswrf"
fpathout="${anndir}/nswrf"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

fpathin="${mondir}/nlwrf"
fpathout="${anndir}/nlwrf"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

fpathin="${mondir}/latent"
fpathout="${anndir}/latent"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

fpathin="${mondir}/sensible"
fpathout="${anndir}/sensible"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

fpathin="${mondir}/evapor"
fpathout="${anndir}/evapor"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

#fpathin="${mondir}/prcp_all"
#fpathout="${anndir}/prcp_all"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

#fpathin="${mondir}/evap_org"
#fpathout="${anndir}/evap_org"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

#fpathin="${mondir}/sens_org"
#fpathout="${anndir}/sens_org"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20
