#!/bin/bash

elmlist='taux tauy'

ORGDIR=../jra_monthly_c7

yrs_org=1958
yre_org=2013
yrs_this=1958
yre_this=2013
#yrs_this=1902
#yre_this=1957

echo "This cycle starts  ${yrs_this}"
echo "This cycle ends    ${yre_this}"

echo "original cycle starts  ${yrs_org}"
echo "original cycle ends    ${yre_org}"

read -p "Creating symbolic link: ${yrs_this} to ${yre_this}, OK ? (y/n) " cont

if [ ${cont} != "y" ]; then
  echo "operation canceled"
  exit
fi

#-------------------------------------------

yeart=${yrs_this}
yearo=${yrs_org}

while [ ${yeart} -le ${yre_this} ]
do
   echo "linking ${yeart} to ${yearo}"
   for elm in `echo ${elmlist}`
   do
      for mon in 01 02 03 04 05 06 07 08 09 10 11 12
          do 
             fname1=${ORGDIR}/${elm}.${yearo}${mon}
             fname2=${elm}.${yeart}${mon}
             \rm ${fname2}
             ln -s ${fname1} ${fname2}
          done
   done
   yeart=`expr ${yeart} + 1`
   yearo=`expr ${yearo} + 1`
done
