#! /bin/sh
#
# glb2rgn_ij.sh flin flout undef=$3 tuxy="$4" k_2d=$5 i_begin=$6 i_end=$7 j_begin=$8 j_end=$9
#

yearst=1948
yeared=2006

########################################

year=${yearst}
#-----------------

while [ ${year} -le ${yeared} ];
do
 echo ${year}

 ./glb2rgn_ij.sh  ../result/hs_tau_sfc."$year"          \
                  ../result/SO_tau_sfc."$year"          \
                  -9.99e33   u   2                      \
                  3   362   1       157
#                          78.25S     0.25S

 yearn=`expr ${year} + 1`
 year=${yearn}
done

#-----------------


