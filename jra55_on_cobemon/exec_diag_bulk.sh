#! /bin/sh
#

ln -sf NAMELIST.MXE.JRA55 NAMELIST.MXE
ln -sf NAMELIST.MXE.COBESST.ocean NAMELIST.MXE.COBESST

export OMP_NUM_THREADS=4

./diag_bulk_interannual_on_sst
