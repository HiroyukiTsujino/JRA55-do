#!/bin/bash

set -e

yearst=${1}
yeared=${2}

file_base[1]="icec"
var_name[1]="icec"
file_base[2]="skt"
var_name[2]="skt"
file_base[3]="prmsl"
var_name[3]="prmsl"
file_base[4]="uwnd.10m"
var_name[4]="uwnd"
file_base[5]="vwnd.10m"
var_name[5]="vwnd"
file_base[6]="air.2m"
var_name[6]="air"
file_base[7]="shum.2m"
var_name[7]="shum"
file_base[8]="dlwrf.sfc"
var_name[8]="dlwrf"
file_base[9]="dswrf.sfc"
var_name[9]="dswrf"
file_base[10]="prate"
var_name[10]="prate"

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

for item in `seq 5 5`
do
  sed -e s%@ibyr@%${yearst}% \
      -e s%@ieyr@%${yeared}% \
      -e s%@indir@%${indir}% \
      -e s%@file_in@%${file_base[${item}]}% \
      -e s%@outdir@%${outdir}% \
      -e s%@var_in@%${var_name[${item}]}% \
      -e s%@var_out@%${file_base[${item}]}% \
  namelist.20CRv3_surf_template > namelist.20CRv3_surf

  ./nc2grads_20CRv3
done
