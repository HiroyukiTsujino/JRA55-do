#!/bin/bash

set -e

year_st=1979
year_ed=2015

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

basedir=../linkdir/forcing
#basedir=/work115/htsujino/SURF_FLUX/forcing
#basedir=/work116/htsujino/SURF_FLUX/forcing

# jra55-raw

#file_base=${basedir}/jra55fcst_annual_1x1/wvn10m
#fileo=wvn10m_glb_v0_1

# jra55-do-v1.1

file_base=${basedir}/jra55fcst_v1_1_annual_1x1/precip.
rmiss_in=9.999e20
fileo=${basedir}/jra55fcst_v1_1_annual_1x1/precip_stats.${year_st}-${year_ed}.gd
rmiss_out=9.999e20

sed -e s%@file_in_base@%${file_base}% \
    -e s%@rmiss_in@%${rmiss_in}% \
    -e s%@file_out@%${fileo}% \
    -e s%@rmiss_out@%${rmiss_out}% \
    -e s%@year_start@%${year_st}% \
    -e s%@year_end@%${year_ed}% \
namelist.linear_trend_template > namelist.linear_trend

exe=linear_trend

./${exe}

exit 0
