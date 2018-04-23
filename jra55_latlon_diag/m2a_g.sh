#!/bin/sh -f

set -e

if [ x${2} = x ]; then
  echo 'Usage: m2a_sh start_year end_year'
fi

styear=${1}
endyear=${2}

# Wind products
#mondir="/work115/htsujino/SURF_FLUX/forcing/jra_remss_monthly_latlon"
#anndir="/work115/htsujino/SURF_FLUX/forcing/jra_remss_annual_latlon"
#mondir="/work115/htsujino/SURF_FLUX/forcing/jra_qscat_strict_monthly_latlon"
#anndir="/work115/htsujino/SURF_FLUX/forcing/jra_qscat_strict_annual_latlon"

# JRA-55 anl
#mondir="../linkdir/forcing/jra_anl_monthly_latlon"
#anndir="../linkdir/forcing/jra_anl_annual_latlon"

# JRA-55C
#mondir="../linkdir/forcing/jra55c_monthly_latlon"
#anndir="../linkdir/forcing/jra55c_annual_latlon"
#mondir="../linkdir/forcing/jra55c_monthly_latlon_filt"
#anndir="../linkdir/forcing/jra55c_annual_latlon_filt"

# ensemble
#mondir="../linkdir/forcing/ensemble_monthly_TL319"
#anndir="../linkdir/forcing/ensemble_annual_TL319"

# ncep-r1
#mondir="../linkdir/forcing/ncep1_monthly_TL319"
#anndir="../linkdir/forcing/ncep1_annual_TL319"

# ncep-r2
#mondir="../linkdir/forcing/ncep2_monthly_TL319"
#anndir="../linkdir/forcing/ncep2_annual_TL319"

# ncep-cfsr
#mondir="../linkdir/forcing/ncep_cfsr_monthly_TL319"
#anndir="../linkdir/forcing/ncep_cfsr_annual_TL319"

# merra2
#mondir="../linkdir/forcing/merra2_monthly_TL319"
#anndir="../linkdir/forcing/merra2_annual_TL319"

# era-i
#mondir="../linkdir/forcing/erai_monthly_TL319"
#anndir="../linkdir/forcing/erai_annual_TL319"

# 20CRv2
#mondir="../linkdir/forcing/20CRv2_monthly_TL319"
#anndir="../linkdir/forcing/20CRv2_annual_TL319"

# CORE
#mondir="../linkdir/forcing/core_monthly_TL319"
#anndir="../linkdir/forcing/core_annual_TL319"


# JRA-55 v0_1
#mondir="../linkdir/forcing/jra55fcst_monthly_TL319"
#anndir="../linkdir/forcing/jra55fcst_annual_TL319"

# JRA-55 v0_7
#mondir="../linkdir/forcing/jra55fcst_v7_prcp_monthly_TL319"
#anndir="../linkdir/forcing/jra55fcst_v7_prcp_annual_TL319"

#mondir="../linkdir/forcing/jra55fcst_v7_prcp3_monthly_TL319"
#anndir="../linkdir/forcing/jra55fcst_v7_prcp3_annual_TL319"

#mondir="../linkdir/forcing/jra55fcst_v7_prod4_monthly_TL319"
#anndir="../linkdir/forcing/jra55fcst_v7_prod4_annual_TL319"

#mondir="../linkdir/forcing/jra55fcst_v7_prod4_inst_monthly_TL319"
#anndir="../linkdir/forcing/jra55fcst_v7_prod4_inst_annual_TL319"

# JRA-55 v1_2
#mondir="../linkdir/forcing/jra55fcst_v1_2_prod4_monthly_TL319"
#anndir="../linkdir/forcing/jra55fcst_v1_2_prod4_annual_TL319"

# JRA-55 v1_3
#mondir="../linkdir/forcing/jra55fcst_v1_3_prod4_monthly_TL319"
#anndir="../linkdir/forcing/jra55fcst_v1_3_prod4_annual_TL319"

# JRA-55 v1_4
#mondir="../linkdir/forcing/jra55fcst_v1_4_prod4_monthly_TL319"
#anndir="../linkdir/forcing/jra55fcst_v1_4_prod4_annual_TL319"

################

#fpathin="${mondir}/tmp2m"
#fpathout="${anndir}/tmp2m"
#./mon2ann_g.sh 640 320 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

#fpathin="${mondir}/sph2m"
#fpathout="${anndir}/sph2m"
#./mon2ann_g.sh 640 320 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

#fpathin="${mondir}/swind"
#fpathout="${anndir}/swind"
#./mon2ann_g.sh 640 320 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33

#fpathin="${mondir}/swind_neutral"
#fpathout="${anndir}/swind_neutral"
#./mon2ann_g.sh 640 320 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33

#fpathin="${mondir}/prcp"
#fpathout="${anndir}/prcp"
#./mon2ann_g.sh 640 320 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" -9.99e33

fpathin="${mondir}/wvn10m"
fpathout="${anndir}/wvn10m"
./mon2ann_g.sh 640 320 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

fpathin="${mondir}/un10m"
fpathout="${anndir}/un10m"
./mon2ann_g.sh 640 320 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20

fpathin="${mondir}/vn10m"
fpathout="${anndir}/vn10m"
./mon2ann_g.sh 640 320 1 ${styear} ${endyear} "${fpathin}" "${fpathout}" 9.999e20
