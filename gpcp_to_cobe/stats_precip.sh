#!/bin/bash

set -e

year_st=1979
year_ed=2015

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

basedir=/work116/htsujino/GPCP-v2_3/grads_annual_1x1

file_base=${basedir}/precip.
rmiss_in=-9.99e33
fileo=${basedir}/precip_stats.${year_st}-${year_ed}.gd
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
