#!/bin/bash

. /etc/profile
. ~/.bash_profile

set -e

pwd

. ./util/datel_leap.sh

if [ x${3} = x ]; then
   echo "Usage: ${0} year month day file_date"
   exit
fi

year=${1}
month=${2}
day=${3}

if [ x${4} = x ]; then
  fdate=`date +%d%b%Y`
else
  fdate=${4}
fi

num_days=`ndatey ${year} ${month} ${day}`

if [ ${month} -eq 12 -a ${day} -eq 31 ]; then
  whole_year=yes
else
  whole_year=no
fi

yred=$( nddate ${year} ${month} ${day} 1 | awk '{print $1 }' )
mned=$( nddate ${year} ${month} ${day} 1 | awk '{print $2 }' )
dyed=$( nddate ${year} ${month} ${day} 1 | awk '{print $3 }' )

echo "start    = ${year} 1 1"
echo "end      = ${yred} ${mned} ${dyed}"
echo "num_days = ${num_days}"
echo "fdate    = ${fdate}"
echo "whole_year = ${whole_year}"

######################################
# (0) Load modules to be used.

case `hostname` in
  cx???? | front?.mrihpc )
    module load intel
    module load hdf5
    module load netcdf
    module load netcdf-fortran
    module load wgrib
esac

######################################
# (1) Processing of raw data.
#-------------------------------------
# (1-1) Download raw latest data

# This is not needed for MRI front end.

echo
echo "(1-1) Original data are read directly from mri-data."

######################################
# (1-2) From grib to flat binary

echo
pushd runoff_mk_input
echo "(1-2) Convert from grib to flat binary."
./process_year.sh ${year} ${num_days}
echo "..........done"
popd

######################################
# (2) Run CaMa-Flood
#-------------------------------------

echo
pushd CaMa-Flood/CaMa-Flood_v3.6_JRA55/gosh
echo "running CaMa-Flood"
./JRA55_mri_front.sh ${year} ${year}
echo "..........done"
popd


######################################
# (3) Extract rivermouth data
#-------------------------------------

echo
pushd CaMa-Flood/OUTPUT
echo "(3) Extract rivermouth data"
if [ -d riv_v1.0_mri ]; then
  pdate=$( ls -ld --time-style=+%Y%m%d riv_v1.0_mri | awk '{print $6}' )
  mv riv_v1.0_mri riv_v1.0_mri.${pdate}
  mkdir riv_v1.0_mri
fi
popd

echo
pushd CaMa-Flood/Extract_rivmouth
./extract_rivmouth_v1_0.sh ${year} ${year}
./copy_products_to_jra55do_work.sh ${year}
echo "..........done"
popd

######################################
# (4) Remove data from Greenland and Antarctica
#-------------------------------------

echo
pushd runoff_imported
echo "(4) Remove data from Greenland and Antarctica"
./remove_Grn_Ant_update.sh ${year} ${num_days}
echo "..........done"
popd

######################################
# (5) Merge CaMa-Flood, Greenland, and Antarctica
#-------------------------------------

echo
pushd runoff_omip_product
echo "(5) Merge CaMa-Flood, Greenland, and Antarctica"
./merge_main_GrnCaaB18_Ant.sh ${year} ${year} ${month} ${day}
echo "..........done"
popd

######################################
# (6) Generate netCDF
#-------------------------------------

echo
pushd for_omip
echo "(6) Generate netCDF."
#####fdate=`date +%d%b%Y`
./create_netcdf_runoff_all_v1_5.sh ${year} ${year} ${fdate} ${num_days}
echo "..........done"
popd
