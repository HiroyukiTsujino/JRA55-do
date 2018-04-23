#!/bin/bash

homedir=`pwd`

# KEO

cd /work116/htsujino/BUOY/KEO

ls lw*cdf | sed 's/_dy.cdf//g' | sed 's/lw//g' > ${homedir}/keo_lwdn_list.txt

cd ${homedir}

stnlist=`cat keo_lwdn_list.txt`
for i in ${stnlist}
do
  sh monthly_lwdn_comp.sh ${i}
done

# PAPA

cd /work116/htsujino/BUOY/PAPA

ls lw*cdf | sed 's/_dy.cdf//g' | sed 's/lw//g' > ${homedir}/papa_lwdn_list.txt

cd ${homedir}

stnlist=`cat papa_lwdn_list.txt`
for i in ${stnlist}
do
  sh monthly_lwdn_comp.sh ${i}
done
