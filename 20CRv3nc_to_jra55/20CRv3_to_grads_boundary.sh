#!/bin/bash

set -e

yearst=${1}
yeared=${2}

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

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  sed -e s%@nyear@%${year}% \
    namelist.20CRv3_boundary_template > namelist.20CRv3_boundary
  ./nc2grads_20CRv3_boundary
  year=$(( year + 1 ))
  echo ${year}
done
