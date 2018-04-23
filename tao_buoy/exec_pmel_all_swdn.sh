#!/bin/bash

homedir=`pwd`

# KEO

cd /work116/htsujino/BUOY/KEO

ls rad*cdf | sed 's/_dy.cdf//g' | sed 's/rad//g' > ${homedir}/keo_swdn_list.txt

cd ${homedir}

stnlist=`cat keo_swdn_list.txt`
for i in ${stnlist}
do
  sh monthly_swdn.sh KEO ${i}
done

# PAPA

cd /work116/htsujino/BUOY/PAPA

ls rad*cdf | sed 's/_dy.cdf//g' | sed 's/rad//g' > ${homedir}/papa_swdn_list.txt

cd ${homedir}

stnlist=`cat papa_swdn_list.txt`
for i in ${stnlist}
do
  sh monthly_swdn.sh PAPA ${i}
done
