dset  ^data_etc/moved_20190209/nextxy_wo_lake_big_endian_noyrev_lon0strt_rm_GA_add_Bamber.bin
undef -9999
title next grid point of river routing model
options big_endian
xdef 1440 linear    0.125 0.25
ydef  720 linear  -89.875 0.25
tdef    1 linear 00Z01jan2000 1dy
zdef    1 linear 1 1
vars 2
nextx 1 -1,40,4       ** downstream x  ! -9  rivermouth or coast 
nexty 1 -1,40,4       ** downstream y  ! -99 lake etc
ENDVARS
* -888 Greenland & CAA
* -777 Antarctica
