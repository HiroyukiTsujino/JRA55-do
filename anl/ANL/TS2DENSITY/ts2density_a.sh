#! /bin/sh -f

yearst=${1}
yeared=${2}

################

year=${yearst}

while [ ${year} -le ${yeared} ];
do
 echo ${year}
 ./ts2density.sh .true. 200 "../grddat/hs_t.$year" "../grddat/hs_s.$year" "../grddat/density.$year"
 yearn=`expr ${year} + 1`
 year=${yearn}
done
