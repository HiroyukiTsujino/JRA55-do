#!/bin/sh -f

styear=1979
endyear=2014

mondir="/work115/htsujino/ERA-interim/grads_monthly"
anndir="/work115/htsujino/ERA-interim/grads_annual"


################

#fpathin="${mondir}/erai_wind10m"
#fpathout="${anndir}/erai_wind10m"
#./mon2ann_g.sh 480 241 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33
#
fpathin="${mondir}/erai_tmp2m"
fpathout="${anndir}/erai_tmp2m"
./mon2ann_g.sh 480 241 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33
#
fpathin="${mondir}/erai_sph2m"
fpathout="${anndir}/erai_sph2m"
./mon2ann_g.sh 480 241 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33
