#!/bin/bash

set -e

yearst=${1}
yeared=${2}
file_base=${3}
var_name=${4}

indir=/data/htsujino/20CRv3
outdir=/data/htsujino/20CRv3/grads

################
# for front, pccl
case `hostname` in
  cx???? | front? )
    module load TCSuite/pc
    module load HDF5/pc
    module load NetCDF/pc
    module load NetCDF-fortran/pc
    export FORT90L=-Wl,-T
esac
#####

sed -e s%@ibyr@%${yearst}% \
    -e s%@ieyr@%${yeared}% \
    -e s%@indir@%${indir}% \
    -e s%@file_in@%${file_base}% \
    -e s%@outdir@%${outdir}% \
    -e s%@var_in@%${var_name}% \
    -e s%@var_out@%${file_base}% \
    namelist.20CRv3_surf_template > namelist.20CRv3_surf

./nc2grads_20CRv3
