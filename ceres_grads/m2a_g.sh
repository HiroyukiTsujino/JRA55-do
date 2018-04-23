#!/bin/sh -f

set -e

styear=2001
endyear=2014

mondir="../linkdir/CERES/grads"
anndir="../linkdir/CERES/grads_ann"


################

#fpathin="${mondir}/swdn"
#fpathout="${anndir}/swdn"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33

#fpathin="${mondir}/lwdn"
#fpathout="${anndir}/lwdn"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33

fpathin="${mondir}/swdn_seaice_tropics_filter"
fpathout="${anndir}/swdn_seaice_tropics_filter"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33

fpathin="${mondir}/lwdn_filter"
fpathout="${anndir}/lwdn_filter"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33

#fpathin="${mondir}/nswrf"
#fpathout="${anndir}/nswrf"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33

#fpathin="${mondir}/nlwrf"
#fpathout="${anndir}/nlwrf"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33
