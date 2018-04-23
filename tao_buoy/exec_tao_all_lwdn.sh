#!/bin/bash

homedir=`pwd`

# TAO

cd /work116/htsujino/BUOY/TAO

ls lw*cdf | sed 's/_dy.cdf//g' | sed 's/lw//g' > ${homedir}/tao_lwdn_list.txt

cd ${homedir}

stnlist=`cat tao_lwdn_list.txt`
for i in ${stnlist}
do
  sh monthly_lwdn.sh TAO ${i}
done

# PIRATA

cd /work116/htsujino/BUOY/PIRATA

ls lw*cdf | sed 's/_dy.cdf//g' | sed 's/lw//g' > ${homedir}/pirata_lwdn_list.txt

cd ${homedir}

stnlist=`cat pirata_lwdn_list.txt`
for i in ${stnlist}
do
  sh monthly_lwdn.sh PIRATA ${i}
done

# RAMA

cd /work116/htsujino/BUOY/RAMA

ls lw*cdf | sed 's/_dy.cdf//g' | sed 's/lw//g' > ${homedir}/rama_lwdn_list.txt

cd ${homedir}

stnlist=`cat rama_lwdn_list.txt`
for i in ${stnlist}
do
  sh monthly_lwdn.sh RAMA ${i}
done
