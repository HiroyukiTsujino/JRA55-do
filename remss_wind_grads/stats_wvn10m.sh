#!/bin/bash

set -e

year_st=1989
year_ed=2015

ln -sf NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

basedir=/work116/htsujino/REMSS_WIND/grads_annual

file_base=${basedir}/swind.
rmiss_in=-9.99e33
fileo=${basedir}/swind_stats.${year_st}-${year_ed}.gd
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
