#!/bin/bash -f

set -e

if [ x${2} == x ]; then
   echo "Usage: ./make_hcurl_interannual.sh start_year end_year"
   exit
fi

ln -sf NAMELIST.MXE.JRA55_ocean_monthly NAMELIST.MXE

yearst=${1}
yeared=${2}

#basedir=/work115/htsujino/SURF_FLUX/forcing
################

#indir=../linkdir/forcing/jra_monthly
#outdir=../linkdir/forcing/jra_monthly
#indir=../linkdir/forcing/jra_monthly_c2
#outdir=../linkdir/forcing/jra_monthly_c2
#indir=../linkdir/forcing/jra_monthly_d2
#outdir=../linkdir/forcing/jra_monthly_d2
#indir=../linkdir/forcing/jra_monthly_d3
#outdir=../linkdir/forcing/jra_monthly_d3
#indir=../linkdir/forcing/jra_monthly_d4
#outdir=../linkdir/forcing/jra_monthly_d4
#indir=../linkdir/forcing/jra_monthly_e3
#outdir=../linkdir/forcing/jra_monthly_e3
#indir=${basedir}/jra_monthly_f1
#outdir=${basedir}/jra_monthly_f1
# v0_8
#indir=../linkdir/forcing/jra55fcst_v7_prod4_monthly_TL319
#outdir=../linkdir/forcing/jra55fcst_v7_prod4_monthly_TL319
# v0_1
#indir=../linkdir/forcing/jra55fcst_monthly_TL319
#outdir=../linkdir/forcing/jra55fcst_monthly_TL319
# v1_2
#indir=../linkdir/forcing/jra55fcst_v1_2_prod4_monthly_TL319
#outdir=../linkdir/forcing/jra55fcst_v1_2_prod4_monthly_TL319
# v1_3
#indir=../linkdir/forcing/jra55fcst_v1_3_prod1_monthly_TL319
#outdir=../linkdir/forcing/jra55fcst_v1_3_prod1_monthly_TL319
#indir=../linkdir/forcing/jra55fcst_v1_3_prod4_monthly_TL319
#outdir=../linkdir/forcing/jra55fcst_v1_3_prod4_monthly_TL319
# v1_4
#indir=../linkdir/forcing/jra55fcst_v1_4_prod1_monthly_TL319
#outdir=../linkdir/forcing/jra55fcst_v1_4_prod1_monthly_TL319
#indir=../linkdir/forcing/jra55fcst_v1_4_prod4_monthly_TL319
#outdir=../linkdir/forcing/jra55fcst_v1_4_prod4_monthly_TL319

################

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo "YEAR = ${year}"
  for m in 01 02 03 04 05 06 07 08 09 10 11 12
  do
    yyyymm=${year}${m}
    sed -e s%@yyyymm@%${yyyymm}% \
        -e s%@indir@%${indir}% \
        -e s%@outdir@%${outdir}% \
    namelist.curl_tau_template > namelist.curl_tau
    ./curltau
  done
  yearn=`expr ${year} + 1`
  year=${yearn}

done
