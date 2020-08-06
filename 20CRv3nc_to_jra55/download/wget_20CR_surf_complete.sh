#!/bin/bash

set -e

# uncomment the following line if you need to set https_proxy
#export ftp_proxy=http://proxy.mri-jma.go.jp:8080/

wget -o ./get_20CRv3_land.log     -nv -nd -P ./ -c ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/timeInvariantSI/land.nc
wget -o ./get_20CRv3_landsflx.log -nv -nd -P ./ -c ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/timeInvariantSI/land.sflx.nc

yrst=1836
yred=1979

year=${yrst}

while [ ${year} -le ${yred} ];
do
  wget -o ./get_20CRv3_SI_air2m_${year}.log   -nv -nd -P ./ -c ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/2mSI/air.2m.${year}.nc
  wget -o ./get_20CRv3_SI_shum2m_${year}.log  -nv -nd -P ./ -c ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/2mSI/shum.2m.${year}.nc
  wget -o ./get_20CRv3_SI_uwnd10m_${year}.log -nv -nd -P ./ -c ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/10mSI/uwnd.10m.${year}.nc
  wget -o ./get_20CRv3_SI_vwnd10m_${year}.log -nv -nd -P ./ -c ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/10mSI/vwnd.10m.${year}.nc
  wget -o ./get_20CRv3_SI_skt_${year}.log     -nv -nd -P ./ -c ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/sfcSI/skt.${year}.nc
  wget -o ./get_20CRv3_SI_icec_${year}.log    -nv -nd -P ./ -c ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/sfcSI/icec.${year}.nc
  wget -o ./get_20CRv3_SI_dlwrf_${year}.log   -nv -nd -P ./ -c ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/sfcSI/dlwrf.sfc.${year}.nc
  wget -o ./get_20CRv3_SI_dswrf_${year}.log   -nv -nd -P ./ -c ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/sfcSI/dswrf.sfc.${year}.nc
  wget -o ./get_20CRv3_SI_prate_${year}.log   -nv -nd -P ./ -c ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/sfcSI/prate.${year}.nc
  wget -o ./get_20CRv3_SI_prmsl_${year}.log   -nv -nd -P ./ -c ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/miscSI/prmsl.${year}.nc
  year=$((year + 1))
  echo ${year}
done

exit

########

yrst=1980
yred=2015

year=${yrst}

while [ ${year} -le ${yred} ];
do
  wget -o ./get_20CRv3_MO_air2m_${year}.log   -nv -nd -P ./ -c ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/2mMO/air.2m.${year}.nc
  wget -o ./get_20CRv3_MO_shum2m_${year}.log  -nv -nd -P ./ -c ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/2mMO/shum.2m.${year}.nc
  wget -o ./get_20CRv3_MO_uwnd10m_${year}.log -nv -nd -P ./ -c ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/10mMO/uwnd.10m.${year}.nc
  wget -o ./get_20CRv3_MO_vwnd10m_${year}.log -nv -nd -P ./ -c ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/10mMO/vwnd.10m.${year}.nc
  wget -o ./get_20CRv3_MO_skt_${year}.log     -nv -nd -P ./ -c ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/sfcMO/skt.${year}.nc
  wget -o ./get_20CRv3_MO_icec_${year}.log    -nv -nd -P ./ -c ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/sfcMO/icec.${year}.nc
  wget -o ./get_20CRv3_MO_dlwrf_${year}.log   -nv -nd -P ./ -c ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/sfcMO/dlwrf.sfc.${year}.nc
  wget -o ./get_20CRv3_MO_dswrf_${year}.log   -nv -nd -P ./ -c ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/sfcMO/dswrf.sfc.${year}.nc
  wget -o ./get_20CRv3_MO_prate_${year}.log   -nv -nd -P ./ -c ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/sfcMO/prate.${year}.nc
  wget -o ./get_20CRv3_MO_prmsl_${year}.log   -nv -nd -P ./ -c ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/miscMO/prmsl.${year}.nc
  year=$((year + 1))
  echo ${year}
done
