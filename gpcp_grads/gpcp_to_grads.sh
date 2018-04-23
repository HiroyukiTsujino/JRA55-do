#!/bin/bash

set -e

yearst=${1}
yeared=${2}
var_name=${3}
var_name_out=${4}

indir=/denkei-shared/og1/htsujino/GPCP-v2_3/rawdata
outdir=/denkei-shared/og1/htsujino/GPCP-v2_3/grads
################

sed -e s%@ibyr@%${yearst}% \
    -e s%@ieyr@%${yeared}% \
    -e s%@indir@%${indir}% \
    -e s%@outdir@%${outdir}% \
    -e s%@var_in@%${var_name}% \
    -e s%@var_out@%${var_name_out}% \
 namelist.gpcp_template > namelist.gpcp_surf

./nc2grads_gpcp
