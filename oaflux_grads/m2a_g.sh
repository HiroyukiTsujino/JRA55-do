#!/bin/sh -f

styear=1958
endyear=2014

mondir="/workd/htsujino/OAflux/grads_monthly"
anndir="/workd/htsujino/OAflux/grads_annual"


################

#fpathin="${mondir}/dswrf"
#fpathout="${anndir}/dswrf"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33
#
#fpathin="${mondir}/dlwrf"
#fpathout="${anndir}/dlwrf"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33

fpathin="${mondir}/latent"
fpathout="${anndir}/latent"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33

fpathin="${mondir}/sensible"
fpathout="${anndir}/sensible"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33
