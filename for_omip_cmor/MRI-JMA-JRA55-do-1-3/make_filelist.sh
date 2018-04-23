#!/bin/bash -f

set -e

for item in q_10 t_10 u_10 v_10 slp rain snow rsds rlds runoff_all
do
  target=${item}.list
  if [ -e  ${target} ];then
    rm -f ${target}
  fi
  touch ${target}
  filelist=`ls input_atmos | grep ${item}`
  for i in ${filelist}
  do
    echo "        'input_atmos/${i}'," >> ${target}
  done
done

for item in brtmp ice sst.COBESST ice.COBESST
do
  target=${item}.list
  if [ -e  ${target} ];then
    rm -f ${target}
  fi
  touch ${target}
  filelist=`ls input_suppl | grep ${item}`
  for i in ${filelist}
  do
    echo "        'input_suppl/${i}'," >> ${target}
  done
done
