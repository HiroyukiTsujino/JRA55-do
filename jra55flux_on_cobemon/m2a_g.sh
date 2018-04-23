#!/bin/sh -f

if [ x${2} = x ]; then
  echo 'Usage: m2a_sh start_year end_year'
fi

styear=${1}
endyear=${2}

mondir="../linkdir/forcing/jra_cobemon_monthly_c2"
anndir="../linkdir/forcing/jra_cobemon_annual_c2"


################

#fpathin="${mondir}/taux"
#fpathout="${anndir}/taux"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

#fpathin="${mondir}/tauy"
#fpathout="${anndir}/tauy"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

#fpathin="${mondir}/dswrf"
#fpathout="${anndir}/dswrf"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

#fpathin="${mondir}/dlwrf"
#fpathout="${anndir}/dlwrf"
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
