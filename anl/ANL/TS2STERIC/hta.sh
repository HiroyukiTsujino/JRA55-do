#! /bin/sh -f

yyyymmst=${1}
yyyymmed=${2}

################

year=${yyyymmst}

while [ ${year} -le ${yyyymmed} ];
do
 echo ${year}
 ./ts2steric.sh "$year"
 yearn=`expr ${year} + 1`
 year=${yearn}
done
