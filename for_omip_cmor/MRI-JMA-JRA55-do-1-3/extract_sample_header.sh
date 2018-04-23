#!/bin/bash -f

set -e

homedir=`pwd`

echo ${homedir}

jra55do_base=input4MIPs/CMIP6/OMIP/MRI/MRI-JRA55-do-1-3


source activate cmor331

############
A3hr="rlds rsds prra prsn"

vardir=${jra55do_base}/atmos/3hr

# rlds

var=rlds
year=1970
yearn=$(( ${year} + 1 ))
echo ${var} ${year} ${yearn}
indir=${vardir}/${var}/gn/v20180412
infile_base=${var}_input4MIPs_atmosphericState_OMIP_MRI-JRA55-do-1-3_gn_${year}01010130-${yearn}01010000
ncdump -h ${indir}/${infile_base}.nc > demo/${infile_base}.txt

# rsds

var=rsds
year=1982
yearn=$(( ${year} + 1 ))
echo ${var} ${year} ${yearn}
indir=${vardir}/${var}/gn/v20180412
infile_base=${var}_input4MIPs_atmosphericState_OMIP_MRI-JRA55-do-1-3_gn_${year}01010130-${yearn}01010000
ncdump -h ${indir}/${infile_base}.nc > demo/${infile_base}.txt

# prra

var=prra
year=1975
yearn=$(( ${year} + 1 ))
echo ${var} ${year} ${yearn}
indir=${vardir}/${var}/gn/v20180412
infile_base=${var}_input4MIPs_atmosphericState_OMIP_MRI-JRA55-do-1-3_gn_${year}01010130-${yearn}01010000
ncdump -h ${indir}/${infile_base}.nc > demo/${infile_base}.txt

# prsn

var=prsn
year=1978
yearn=$(( ${year} + 1 ))
echo ${var} ${year} ${yearn}
indir=${vardir}/${var}/gn/v20180412
infile_base=${var}_input4MIPs_atmosphericState_OMIP_MRI-JRA55-do-1-3_gn_${year}01010130-${yearn}01010000
ncdump -h ${indir}/${infile_base}.nc > demo/${infile_base}.txt

############
A3hrPt="tas huss psl uas vas brtmp siconca"

vardir=${jra55do_base}/atmos/3hrPt

# huss

var=huss
year=1990
yearn=$(( ${year} + 1 ))
echo ${var} ${year} ${yearn}
indir=${vardir}/${var}/gn/v20180411
infile_base=${var}_input4MIPs_atmosphericState_OMIP_MRI-JRA55-do-1-3_gn_${year}01010000-${year}12312230
ncdump -h ${indir}/${infile_base}.nc > demo/${infile_base}.txt

# tas

var=tas
year=1992
yearn=$(( ${year} + 1 ))
echo ${var} ${year} ${yearn}
indir=${vardir}/${var}/gn/v20180412
infile_base=${var}_input4MIPs_atmosphericState_OMIP_MRI-JRA55-do-1-3_gn_${year}01010000-${year}12312230
ncdump -h ${indir}/${infile_base}.nc > demo/${infile_base}.txt

# psl

var=psl
year=2005
yearn=$(( ${year} + 1 ))
echo ${var} ${year} ${yearn}
indir=${vardir}/${var}/gn/v20180412
infile_base=${var}_input4MIPs_atmosphericState_OMIP_MRI-JRA55-do-1-3_gn_${year}01010000-${year}12312230
ncdump -h ${indir}/${infile_base}.nc > demo/${infile_base}.txt

# uas

var=uas
year=2000
yearn=$(( ${year} + 1 ))
echo ${var} ${year} ${yearn}
indir=${vardir}/${var}/gn/v20180411
infile_base=${var}_input4MIPs_atmosphericState_OMIP_MRI-JRA55-do-1-3_gn_${year}01010000-${year}12312230
ncdump -h ${indir}/${infile_base}.nc > demo/${infile_base}.txt

# vas

var=vas
year=2015
yearn=$(( ${year} + 1 ))
echo ${var} ${year} ${yearn}
indir=${vardir}/${var}/gn/v20180411
infile_base=${var}_input4MIPs_atmosphericState_OMIP_MRI-JRA55-do-1-3_gn_${year}01010000-${year}12312230
ncdump -h ${indir}/${infile_base}.nc > demo/${infile_base}.txt

# ts

var=ts
year=1999
yearn=$(( ${year} + 1 ))
echo ${var} ${year} ${yearn}
indir=${vardir}/${var}/gn/v20180412
infile_base=${var}_input4MIPs_atmosphericState_OMIP_MRI-JRA55-do-1-3_gn_${year}01010000-${year}12312230
ncdump -h ${indir}/${infile_base}.nc > demo/${infile_base}.txt

#############
# siconca
SI3hrPt="siconca"

vardir=${jra55do_base}/seaIce/3hrPt

var=siconca
year=2008
yearn=$(( ${year} + 1 ))
echo ${var} ${year} ${yearn}
indir=${vardir}/${var}/gn/v20180413
infile_base=${var}_input4MIPs_atmosphericState_OMIP_MRI-JRA55-do-1-3_gn_${year}01010000-${year}12312230
ncdump -h ${indir}/${infile_base}.nc > demo/${infile_base}.txt

############

Oday="friver tos"

var=friver
vardir=${jra55do_base}/ocean/day
year=1958
yearn=$(( ${year} + 1 ))
echo ${var} ${year} ${yearn}
indir=${vardir}/${var}/gn/v20180412
infile_base=${var}_input4MIPs_atmosphericState_OMIP_MRI-JRA55-do-1-3_gn_${year}0101-${yearn}0101
ncdump -h ${indir}/${infile_base}.nc > demo/${infile_base}.txt

var=tos
vardir=${jra55do_base}/ocean/day
year=2002
yearn=$(( ${year} + 1 ))
echo ${var} ${year} ${yearn}
indir=${vardir}/${var}/gn/v20180412
infile_base=${var}_input4MIPs_atmosphericState_OMIP_MRI-JRA55-do-1-3_gn_${year}0101-${yearn}0101
ncdump -h ${indir}/${infile_base}.nc > demo/${infile_base}.txt

############

SIday="siconc"
var=siconc
vardir=${jra55do_base}/seaIce/day
year=1960
yearn=$(( ${year} + 1 ))
echo ${var} ${year} ${yearn}
indir=${vardir}/${var}/gn/v20180412
infile_base=${var}_input4MIPs_atmosphericState_OMIP_MRI-JRA55-do-1-3_gn_${year}0101-${yearn}0101
ncdump -h ${indir}/${infile_base}.nc > demo/${infile_base}.txt


############
OmonC="sos"

vardir=${jra55do_base}/ocean/monC

var=sos
echo "${var} monclim"
indir=${vardir}/${var}/gn/v20180412
infile_base=${var}_input4MIPs_atmosphericState_OMIP_MRI-JRA55-do-1-3_gn_195501-201212-clim
ncdump -h ${indir}/${infile_base}.nc > demo/${infile_base}.txt


############
LIyrC="licalvf"

vardir=${jra55do_base}/landIce/yrC

var=licalvf
echo "${var} clim"
indir=${vardir}/${var}/gn/v20180413
infile_base=${var}_input4MIPs_atmosphericState_OMIP_MRI-JRA55-do-1-3_gn_2007-2008-clim
ncdump -h ${indir}/${infile_base}.nc > demo/${infile_base}.txt



############
AyrC="uo vo"

vardir=${jra55do_base}/ocean/yrC

# uo

var=uo
echo "${var} clim"
indir=${vardir}/${var}/gn/v20180412
infile_base=${var}_input4MIPs_atmosphericState_OMIP_MRI-JRA55-do-1-3_gn_1999-2009-clim
ncdump -h ${indir}/${infile_base}.nc > demo/${infile_base}.txt

# vo

var=vo
echo "${var} clim"
indir=${vardir}/${var}/gn/v20180412
infile_base=${var}_input4MIPs_atmosphericState_OMIP_MRI-JRA55-do-1-3_gn_1999-2009-clim
ncdump -h ${indir}/${infile_base}.nc > demo/${infile_base}.txt


############
Ofx="areacello sftof"

vardir=${jra55do_base}/ocean/fx

# areacello

var=areacello
echo "${var} fx"
indir=${vardir}/${var}/gn/v20180413
infile_base=${var}_input4MIPs_atmosphericState_OMIP_MRI-JRA55-do-1-3_gn
ncdump -h ${indir}/${infile_base}.nc > demo/${infile_base}.txt

# sftof

var=sftof
echo "${var} fx"
indir=${vardir}/${var}/gn/v20180413
infile_base=${var}_input4MIPs_atmosphericState_OMIP_MRI-JRA55-do-1-3_gn
ncdump -h ${indir}/${infile_base}.nc > demo/${infile_base}.txt

############
LIfx="areacellg"

vardir=${jra55do_base}/landIce/fx

# areacellg

var=areacellg
echo "${var} fx"
indir=${vardir}/${var}/gn/v20180413
infile_base=${var}_input4MIPs_atmosphericState_OMIP_MRI-JRA55-do-1-3_gn
ncdump -h ${indir}/${infile_base}.nc > demo/${infile_base}.txt


source deactivate cmor331
