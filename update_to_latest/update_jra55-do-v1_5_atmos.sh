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
pushd extract_jra55
echo "(1-2) Convert from grib to flat binary."
./process_year_last.sh ${year} ${month} ${day}
echo "..... done"
popd

######################################
# (1-3) Apply 1-2-1 filter to wind vector

echo 
pushd jra55_org_grid_calib
echo "(1-3) Apply 1-2-1 filter to wind vector."
./filter_wind_interannual_latest_exec.sh ${year} 1 1 ${yred} ${mned} ${dyed}
echo "..... done"
popd

######################################
# (1-4) Convert raw data from reduced to regular TL319 grid 

echo 
pushd jra55_org_grid_product
echo "(1-4) Convert raw data from reduced to regular TL319 grid."
./red2reg_exec_raw_v1_0.sh ${year} ${year} 1 ${num_days}
./red2reg_exec_raw_v1_0_wind.sh ${year} ${year} 1 ${num_days}
echo "..... done"
popd

#######################################
# (2) Processing of COBE-SST.
#-------------------------------------
# (2-1) Download raw latest data

# This is not needed for MRI front end.

echo 
echo "(2-1) Original data are read directly from mri-data."

######################################
# (2-2) From grib to flat binary
#
echo 
pushd cobesst
echo "(2-2) Convert from grib to GrADs."
./grib2grads_cobesst_daily.sh ${year} ${year}
./grib2grads_cobesst_monthly.sh ${year} ${year}
pmonth=$(( ${month} - 1 ))
if [ ${pmonth} -ge 0 ]; then
  ./make_sepmondata.sh ${year} ${year} ${pmonth}
fi
if [ x${whole_year} = xyes ]; then
  yearn=$(( ${year} + 1 ))
  ./grib2grads_cobesst_daily.sh ${yearn} ${yearn}
  ./grib2grads_cobesst_monthly.sh ${yearn} ${yearn}
fi
echo "..... done"
popd

######################################
# (2-3) Generate netCDF files.
#
echo 
pushd for_omip
echo "(2-3) Generate netCDF files for COBESST."
./create_netcdf_cobesst.sh ${year} ${year} ${fdate} ${num_days}
echo "..... done"
popd

#####################################################
# (3) Adjust surface meteorological variables
#----------------------------------------------------
# (3-1) Adjust air temperature

echo 
pushd jra55_org_grid_calib
echo "(3-1) Adjust air temperature"
./calib_tmp2m_v1_5_interannual_latest_exec.sh ${year} 1 1 ${yred} ${mned} ${dyed}
echo "..... done"
popd

# (3-2) Adjust specific humidity

echo 
pushd jra55_org_grid_calib
echo "(3-2) Adjust specific humidity"
./calib_sph2m_v1_5_interannual_latest_exec.sh ${year} 1 1 ${yred} ${mned} ${dyed}
echo "..... done"
popd

# (3-3) Calculate equivalent neutral wind.

echo 
pushd jra55_org_grid_anl
echo "(3-3) Calculate equivalent neutral wind"
./jra55fcst_filt_v1_5tq_neutral_wind_exec.sh ${year} ${year} 1 ${num_days}
echo "..... done"
popd

# (3-4) Adjust equivalent neutral wind.

echo 
pushd jra55_org_grid_calib
echo "(3-4) Adjust equivalent neutral wind"
./calib_wind_v1_5_interannual_latest_exec.sh ${year} 1 1 ${yred} ${mned} ${dyed}
echo "..... done"
popd

# (3-5) From equivalent neutral to actual wind.

echo
pushd jra55_org_grid_anl
echo "(3-5) From equivalent neutral to actual wind."
./jra55fcst_filt_v1_5_actual_wind_exec.sh ${year} ${year} 1 ${num_days}
echo "..... done"
popd

# (3-6) From reduced TL319 to regular TL319

echo
pushd jra55_org_grid_product
echo "(3-6) From reduced TL319 to regular TL319."
./red2reg_exec_v1_5.sh ${year} ${year} 1 ${num_days}
echo "..... done"
popd

#############################################
# (4) Adjust short and long wave radiation
#--------------------------------------------
# (4-1) Apply factors to short and long wave radiation

echo 
pushd jra55_org_grid_calib
echo "(4-1) Apply factors to short and long wave radiation"
./calib_rad_interannual_update_exec.sh ${year} 1 1 ${yred} ${mned} ${dyed}
echo "..... done"
popd

# (4-2) From reduced TL319 to regular TL319

echo 
pushd jra55_org_grid_product
echo  "(4-2) From reduced TL319 to regular TL319"
./red2reg_exec_rad.sh ${year} ${year} 1 ${num_days}
echo "..... done"
popd

echo 
pushd ./linkdir/work/jra55fcst_v0_7_rad1_3hr_TL319
echo " (4-3) Make symbolic links."
for mon in `seq -f "%02g" 1 12`
do
  if [ ! -e ${year}${mon} ]; then
    ln -sfn ../jra55fcst_v0_7_rad_3hr_TL319/${year}${mon} ${year}${mon} 
  fi
done
echo "..... done"
popd

#############################################
# (5) Adjust precipitation
#--------------------------------------------
# (5-1) Apply factors to precipitation

echo 
pushd jra55_org_grid_calib
echo "(5-1) Apply factors to precipitation"
./calib_prcp_snow_interannual_update_exec_v1_2.sh ${year} 1 1 ${yred} ${mned} ${dyed}
echo "..... done"
popd

# (5-2) From reduced TL319 to regular TL319

echo 
pushd jra55_org_grid_product
echo  "(5-2) From reduced TL319 to regular TL319"
./red2reg_exec_precip_v1_2.sh ${year} ${year} 1 ${num_days}
echo "..... done"
popd

echo 
pushd ./linkdir/work/jra55fcst_v1_2_prcp0_3hr_TL319
echo " (5-3) Make symbolic links."
for mon in `seq -f "%02g" 1 12`
do
  if [ ! -e ${year}${mon} ]; then
    ln -sfn ../jra55fcst_v1_2_prcp_3hr_TL319/${year}${mon} ${year}${mon} 
  fi
done
echo "..... done"
popd

################################################
# (6) Additional adjustment to temperature and humidity
#-----------------------------------------------
# (6-1) Smoothing around the merginal sea ice zone

echo 
pushd jra55_calib
echo "(6-1)  Smoothing around the merginal sea ice zone"
./calib_tmp10m_iceedge_update_exec_v1_5.sh ${year} 1 1 ${yred} ${mned} ${dyed}
echo "..... done"
popd

# (6-2) Cut-off extremely low temperature around Antarctica

echo 
pushd jra55_calib
echo "(6-2) Cut-off extremely low temperature around Antarctica"
./calib_tmp10m_antarc_update_exec_v1_5.sh ${year} 1 1 ${yred} ${mned} ${dyed}
echo "..... done"
popd

echo 
pushd ./linkdir/work/jra55fcst_v1_5_prod4_3hr_TL319
echo " (6-3) Make symbolic links."
for mon in `seq -f "%02g" 1 12`
do
  if [ ! -e ${year}${mon} ]; then
    ln -sfn ../jra55fcst_v1_5_prod3_3hr_TL319/${year}${mon} ${year}${mon} 
  fi
done
echo "..... done"
popd

################################################
# (7) Apply globally uniform factors
#-----------------------------------------------
# (7-1) Apply globally uniform factor to radiation

echo 
pushd jra55_calib
echo "(7-1) Apply globally uniform factor to radiation"
./calib_rad_const_update_exec.sh v1_3 ${year} 1 1 ${yred} ${mned} ${dyed}
echo "..... done"
popd

echo 
pushd jra55_calib
echo "(7-2) Apply globally uniform factor to precipitation"
./calib_prcp_snow_const_update_exec.sh v1_3 ${year} 1 1 ${yred} ${mned} ${dyed}
echo "..... done"
popd

echo 
pushd ./linkdir/products/version_1_5/grads
if [ ! -e ${year} ]; then
  mkdir -p ${year}
fi
popd

echo 
pushd jra55_calib
echo "(7-3) Create rain data"
./create_rain_update.sh v1_5 ${year} 1 1 ${yred} ${mned} ${dyed}
echo "..... done"
popd

################################################
# (8) Gather data in one file per year per item,
#

echo 
pushd for_mricom
echo "(8-1) Gather data in one file per year per item."
./make_mricom_latest_v1_5.sh ${year} ${num_days}
echo "..... done"
echo "(8-2) Fill land for MRI.COM."
./fill_land_all_v1_5.sh ${year} ${year} ${num_days}
echo "..... done"
popd

#################################################
# (9) Generate netCDF files for atmospheric variables.
#-----------------------------------------------
# (9-1) atmospheric variables

echo 
pushd for_omip
echo "(9-1) Generate netCDF files for atmospheric variables."
./create_netcdf_v1_5.sh ${year} ${year} ${fdate} ${num_days}
echo "..... done"
popd

# (9-2) brightness temperature and sea ice

echo 
pushd for_omip
echo "(9-2) Generate netCDF files for brtmp and ice."
./create_netcdf_brtmp_ice.sh ${year} ${year} ${fdate} ${num_days}
echo "..... done"
popd
