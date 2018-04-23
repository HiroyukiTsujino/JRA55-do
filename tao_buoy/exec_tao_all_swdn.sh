#!/bin/bash

homedir=`pwd`

# TAO

cd /work116/htsujino/BUOY/TAO

ls rad*cdf | sed 's/_dy.cdf//g' | sed 's/rad//g' > ${homedir}/tao_swdn_list.txt

cd ${homedir}

stnlist=`cat tao_swdn_list.txt`
for i in ${stnlist}
do
  sh monthly_swdn.sh TAO ${i}
done

# PIRATA

cd /work116/htsujino/BUOY/PIRATA

ls rad*cdf | sed 's/_dy.cdf//g' | sed 's/rad//g' > ${homedir}/pirata_swdn_list.txt

cd ${homedir}

stnlist=`cat pirata_swdn_list.txt`
for i in ${stnlist}
do
  sh monthly_swdn.sh PIRATA ${i}
done

# RAMA

cd /work116/htsujino/BUOY/RAMA

ls rad*cdf | sed 's/_dy.cdf//g' | sed 's/rad//g' > ${homedir}/rama_swdn_list.txt

cd ${homedir}

stnlist=`cat rama_swdn_list.txt`
for i in ${stnlist}
do
  sh monthly_swdn.sh RAMA ${i}
done
