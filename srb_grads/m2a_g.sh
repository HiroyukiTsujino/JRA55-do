#! /bin/sh -f

styear=1984
endyear=2007

mondir="/worke/htsujino/SRB/rel3.0_lpsa_lpla/grads"
anndir="/worke/htsujino/SRB/rel3.0_lpsa_lpla/grads_ann"
#mondir="/worke/htsujino/SRB/rel3.1/grads"
#anndir="/worke/htsujino/SRB/rel3.1/grads_ann"


################

#fpathin="${mondir}/swdn"
#fpathout="${anndir}/swdn"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33

#fpathin="${mondir}/lwdn"
#fpathout="${anndir}/lwdn"
#./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33

fpathin="${mondir}/nswrf"
fpathout="${anndir}/nswrf"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33

fpathin="${mondir}/nlwrf"
fpathout="${anndir}/nlwrf"
./mon2ann_g.sh 360 180 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33
