#!/bin/bash

homedir=`pwd`

stnlist="15n051w 22n158w 19s085w"
for i in ${stnlist}
do
  sh monthly_swdn_comp.sh ${i}
done
