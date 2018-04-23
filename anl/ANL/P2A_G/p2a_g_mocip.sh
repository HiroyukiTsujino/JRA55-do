#! /bin/sh -f

yearst=$1
yeared=$2

########################################

year=${yearst}
while [ ${year} -le ${yeared} ];
do
 echo ${year}

 ./p2a_g.sh   1  154  207  3  0.0e0  ${year} .false. \
         "../grd5d/mocip4"   "../result/mocip4"

 yearn=`expr ${year} + 1`
 year=${yearn}
done


