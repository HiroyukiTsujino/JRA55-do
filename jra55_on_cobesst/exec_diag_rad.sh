#! /bin/sh

set -e

if [ x${1} = x ]; then
  echo "Usage: ${0} dataset_name (org c0 c1 c2 ...)"
  exit
fi

dsname=${1}

ln -sf NAMELIST.MXE.COBESST.ocean NAMELIST.MXE.COBESST

ln -sf namelist.diagrad.${dsname} namelist.diagrad

./diag_rad_interannual_on_sst

#export OMP_NUM_THREADS=4

#ln -sf namelist.diagrad.c0 namelist.diagrad
#./diag_rad_interannual_on_sst

#ln -sf namelist.diagrad.c1 namelist.diagrad
#./diag_rad_interannual_on_sst

#ln -sf namelist.diagrad.c2 namelist.diagrad
#./diag_rad_interannual_on_sst

#ln -sf namelist.diagrad_ocn.e3 namelist.diagrad
#./diag_rad_interannual_on_sst

#ln -sf namelist.diagrad_all.e4 namelist.diagrad
#./diag_rad_interannual_on_sst
