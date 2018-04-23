#!/bin/sh -f

set -e

styear=1988
endyear=2015

mondir="/work116/htsujino/REMSS_WIND/grads_monthly"
anndir="/work116/htsujino/REMSS_WIND/grads_annual"


################

fpathin="${mondir}/swind"
fpathout="${anndir}/swind"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33
