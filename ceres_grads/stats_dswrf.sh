#!/bin/bash

set -e

year_st=2001
year_ed=2014

ln -sf NAMELIST.MXE.CERES.annual NAMELIST.MXE

basedir=/work116/htsujino/CERES/grads_ann

file_base=${basedir}/swdn.
rmiss_in=-9.99e33
fileo=${basedir}/swdn_stats.${year_st}-${year_ed}.gd
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
