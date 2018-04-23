#! /bin/sh -f

yearst=${1}
yeared=${2}

################

year=${yearst}

while [ ${year} -le ${yeared} ];
do
 echo ${year}
 ./ts2density.sh .false. 0 "../grddat/hs_t.$year" "../grddat/hs_s.$year" "../grddat/sigma0.$year"
 yearn=`expr ${year} + 1`
 year=${yearn}
done
