#!/bin/sh -f

styear=1958
endyear=2001

mondir="/work115/htsujino/ERA-40/grads_monthly"
anndir="/work115/htsujino/ERA-40/grads_annual"


################

#fpathin="${mondir}/era40_wind10m"
#fpathout="${anndir}/era40_wind10m"
#./mon2ann_g.sh 480 241 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33
#
fpathin="${mondir}/era40_tmp2m"
fpathout="${anndir}/era40_tmp2m"
./mon2ann_g.sh 480 241 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33
#
fpathin="${mondir}/era40_sph2m"
fpathout="${anndir}/era40_sph2m"
./mon2ann_g.sh 480 241 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33
