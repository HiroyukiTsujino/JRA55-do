#!/bin/sh

set -e

rm -f NAMELIST.MXE
ln -s NAMELIST.MXE.COBESST.ocean_annual NAMELIST.MXE

orgdir=../linkdir/COBESST/monthly/grads
newdir=../linkdir/COBESST/monthly/netCDF

flin="${orgdir}/sst-glb."
flout="${newdir}/sst-COBESST.nc"
flctl="${newdir}/sst-COBESST.ctl"

##############################
# if mask is written to file, l_mask_out = .true.
# and flibas with valid size must exist

l_mask_out=.false.
flibas="dummy_basin_map.txt"

# for GrADs xdfopen

time_start="jan1870"
time_intv="1mo"

./grd2nc_g_input4mips_all<<EOF
&nml_grd2nc_g
 file_in="${flin}",
 file_out="${flout}",
 tuw="u"
 l_append_ext=.false.,
 cext="no",
 l_one_data=.false.,
 l_mask_out=${l_mask_out},
 l_space_2d=.true.,
 l_x_even=.true.,
 l_y_even=.true.,
 num_vars=1,
 num_var_out=1,
 lon_first=0.5d0,delta_lon=1.0d0,nlons=360,l_lon_model=.true.,lon_units="degrees_east",
 lat_first=-89.5d0,delta_lat=1.0d0,nlats=180,l_lat_model=.true.,lat_units="degrees_north",
 zlev=0.0d0,,,
 nlvls=1,
 l_lev_model=.false.,
 lvl_units="m",
 i_ref_year=1870,
 i_ref_month=1,
 i_ref_day=1,
 i_ref_hour=0,
 i_ref_minute=0,
 i_ref_second=0,
 first_data_relative_to_ref=0.5d0,
 ibyear=1870,
 ieyear=2019,
 intv_indx=2,
 nrecs_per_year=12,
 nrecs_out=12,
 l_leap=.true.,
 file_basin="${flibas}",
 file_ctl="${flctl}"
 time_start="${time_start}"
 time_intv="${time_intv}"
 l_put_global_attributes=.true.
/
&nml_vars
 nth_place_tmp=1,
 vname_tmp='tos'
 vunit_tmp='degrees Celsius',
 vlongname_tmp='Sea Surface Temperature'
 vstandardname_tmp='sea_surface_temperature'
 rconv_tmp=1.0,
 rmin_tmp=-9.99e33,
 rmax_tmp=9.99e33,
 undef_in_tmp=9.999e20,
 undef_out_tmp=-9.99e33,
/
&nml_global_attributes
  global_attributes%title="Sea Surface Temperature of COBESST"
  global_attributes%institution="JMA Meteorological Research Institute"
  global_attributes%version="v1.5.00"
  global_attributes%comment="Raw data of COBESST"
  global_attributes%fill_value="Fill value is -9.99e33"
/
EOF
