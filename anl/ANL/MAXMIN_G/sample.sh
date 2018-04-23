#! /bin/sh -f
#
# sample script for Weddell Gyre
#

yearst=1948
yeared=2006

########################################

year=${yearst}

#-----------------

while [ ${year} -le ${yeared} ];
do
 echo ${year}

 ./maxmin_g.sh 364     368  1   1 -9.99E33           \
               220 280 0 30 1 1                      \
               "../result/btsf.${year}"              \
               "../logs/MAXMIN/Weddell_Gyre.${year}"

 yearn=`expr ${year} + 1`
 year=${yearn}
done



