#!/bin/sh

flin="../../../linkdir/data_20150605/maskt.gd"
flout="../netCDF/t_point_mask.nc"

##############################
# if mask is written to file, l_mask_out = .true.
# and flibas with valid size must exist

l_mask_out=.true.
flibas="../../../linkdir/data_20150605/basin_map.txt"

# for GrADs xdfopen

flctl="../netCDF/t_point_mask.ctl"
time_start="jan1958"
time_intv="1yr"

./grd2nc_g<<EOF
&nml_grd2nc_g
 file_in="${flin}",
 file_out="${flout}",
 tuw="t"
 l_append_ext=.false.,
 cext="no",
 l_one_data=.true.,
 l_mask_out=${l_mask_out},
 num_vars=1,
 num_var_out=1,
 lon_first=0.0d0,delta_lon=1.0d0,nlons=364,l_lon_model=.true.,lon_units="degrees_east",
 lat_first=-90.0d0,delta_lat=0.5d0,nlats=366,l_lat_model=.true.,lat_units="degrees_north",
 zlev=0.0d0,,,
 nlvls=1,
 l_lev_model=.false.,
 lvl_units="m",
 i_ref_year=1958,
 i_ref_month=1,
 i_ref_day=1,
 i_ref_hour=0,
 i_ref_minute=0,
 i_ref_second=0,
 first_data_relative_to_ref=0.5d0,
 ibyear=1958,
 ieyear=1958,
 intv_indx=1,
 nrecs_per_year=1,
 l_leap=.true.,
 file_basin="${flibas}",
 file_ctl="${flctl}"
 time_start="${time_start}"
 time_intv="${time_intv}"
 l_put_global_attributes=.true.
/
&nml_vars
 nth_place_tmp=1,
 var_kind_tmp='integer',
 vname_tmp='sea'
 vunit_tmp='1',
 vlongname_tmp='Land (0) Sea (1) mask'
 vstandardname_tmp='land_sea_mask'
 rconv_tmp=1.0,
 rmin_tmp=-9.99e33,
 rmax_tmp=9.99e33,
 undef_in_tmp=-9.99e33,
 undef_out_tmp=-9.99e33,
/
&nml_global_attributes
  global_attributes%title="T-point mask of MRI global model"
  global_attributes%institution="JMA Meteorological Research Institute"
  global_attributes%comment="MRI.COM uses Arakawa B-grid arrangement. Tracer points are placed the corners of the grid cells."
  global_attributes%fill_value="Fill value is 0.0 for land."
/
EOF
