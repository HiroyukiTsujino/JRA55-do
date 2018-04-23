#!/bin/bash

elmlist='taux tauy'


# JRA-55

# v0_7_2

#ORGDIR=/work115/htsujino/SURF_FLUX/forcing/jra55fcst_v7_rad2_monthly_1x1
#yrs_org=1958
#yre_org=2015
#yrs_this=1958
#yre_this=2015
#yrs_this=1900
#yre_this=1957

# v1_2

#ORGDIR=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_2_monthly_1x1
#yrs_org=1958
#yre_org=2016
#yrs_this=1958
#yre_this=2016
#yrs_this=1899
#yre_this=1957

# raw, v1_3, v1_4

#ORGDIR=/work115/htsujino/SURF_FLUX/forcing/jra55fcst_monthly_1x1
#ORGDIR=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_3_monthly_1x1
#ORGDIR=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_4pre1_monthly_1x1
#ORGDIR=/work113/htsujino/SURF_FLUX/forcing/jra55fcst_v1_4_monthly_1x1
#yrs_org=1958
#yre_org=2017
#yrs_this=1958
#yre_this=2017
#yrs_this=1898
#yre_this=1957


# CORE
#ORGDIR=/work116/htsujino/CORE/core_cobesst_monthly
#yrs_org=1948
#yre_org=2009
#yrs_this=1948
#yre_this=2009
#yrs_this=1886
#yre_this=1947

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

cd monthly_wind_link

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
	     if [ -e ${fname2} ]; then
               rm -f ${fname2}
             fi
             ln -s ${fname1} ${fname2}
          done
   done
   yeart=`expr ${yeart} + 1`
   yearo=`expr ${yearo} + 1`
done
