#!/bin/bash

set -e

yearst=${1}
yeared=${2}
var_name=${3}
var_name_out=${4}

indir=/work/a/htsujino/NCEP_R2/rawdata
outdir=/work/a/htsujino/NCEP_R2/grads_monthly

################

. ../util/datel_leap.sh

sed -e s%@ibyr@%${yearst}% \
    -e s%@ieyr@%${yeared}% \
    -e s%@indir@%${indir}% \
    -e s%@outdir@%${outdir}% \
    -e s%@var_in@%${var_name}% \
    -e s%@var_out@%${var_name_out}% \
    namelist.ncep2_surf_template > namelist.ncep2_surf

./nc2grads_ncep2
