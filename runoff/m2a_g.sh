#!/bin/sh -f

set -e

styear=1958
endyear=2016

mondir="/work116/htsujino/SURF_FLUX/forcing/river_jra55-do-v1_1-monthly-1x1"
anndir="/work116/htsujino/SURF_FLUX/forcing/river_jra55-do-v1_1-annual-1x1"


################

fpathin="${mondir}/runoff_all"
fpathout="${anndir}/runoff_all"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20
