#!/bin/bash

set -e

var_name=${1}
var_name_out=${2}

offset=1.0
scale=1.0
indir=/work/a/htsujino/NCEP_R1/rawdata
outdir=/work/a/htsujino/NCEP_R1/grads_const

################

sed -e s%@indir@%${indir}% \
    -e s%@outdir@%${outdir}% \
    -e s%@var_in@%${var_name}% \
    -e s%@var_out@%${var_name_out}% \
    -e s%@scale@%${scale}% \
    -e s%@offset@%${offset}% \
    namelist.ncep1_const_template > namelist.ncep1_const

./nc2grads_ncep1_const
