#!/bin/sh

flin="../time_series/hs_ice."
flout="../netCDF/sea_ice_JRA55adj.nc"

##############################
# if mask is written to file, l_mask_out = .true.
# and flibas with valid size must exist

l_mask_out=.false.
flibas="../../../linkdir/data_20150605/basin_map.txt"

# for GrADs xdfopen

flctl="../netCDF/sea_ice_JRA55adj.ctl"
time_start="jan1958"
time_intv="1mo"

./grd2nc_g<<EOF
&nml_grd2nc_g
 file_in="${flin}",
 file_out="${flout}",
 tuw="t"
 l_append_ext=.false.,
 cext="no",
 l_one_data=.false.,
 l_mask_out=${l_mask_out},
 num_vars=4,
 num_var_out=4,
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
 ieyear=2014,
 intv_indx=2,
 nrecs_per_year=12,
 l_leap=.true.,
 file_basin="${flibas}",
 file_ctl="${flctl}"
 time_start="${time_start}"
 time_intv="${time_intv}"
 l_put_global_attributes=.true.
/
&nml_vars
 nth_place_tmp=1,
 vname_tmp='sit'
 vunit_tmp='m',
 vlongname_tmp='Sea Ice Thickness'
 vstandardname_tmp='sea_ice_thickness'
 rconv_tmp=1.0,
 rmin_tmp=-9.99e33,
 rmax_tmp=9.99e33,
 undef_in_tmp=-9.99e33,
 undef_out_tmp=-9.99e33,
/
&nml_vars
 nth_place_tmp=2,
 vname_tmp='snd'
 vunit_tmp='m',
 vlongname_tmp='Snow Depth'
 vstandardname_tmp='surface_snow_thicknes'
 rconv_tmp=1.0,
 rmin_tmp=-9.99e33,
 rmax_tmp=9.99e33,
 undef_in_tmp=-9.99e33,
 undef_out_tmp=-9.99e33,
/
&nml_vars
 nth_place_tmp=3,
 vname_tmp='sic'
 vunit_tmp='%',
 vlongname_tmp='Sea Ice Area Fraction'
 vstandardname_tmp='sea_ice_area_fraction'
 rconv_tmp=100.0,
 rmin_tmp=-9.99e33,
 rmax_tmp=9.99e33,
 undef_in_tmp=-9.99e33,
 undef_out_tmp=-9.99e33,
/
&nml_vars
 nth_place_tmp=4,
 vname_tmp='tsice'
 vunit_tmp='degrees Celsius',
 vlongname_tmp='Surface Temperature of Sea Ice'
 vstandardname_tmp='surface_temperature'
 rconv_tmp=1.0,
 rmin_tmp=-9.99e33,
 rmax_tmp=9.99e33,
 undef_in_tmp=-9.99e33,
 undef_out_tmp=-9.99e33,
/
&nml_global_attributes
  global_attributes%title="Sea ice fields from MRI global model forced by an adjusted JRA-55 forcing set"
  global_attributes%institution="JMA Meteorological Research Institute"
  global_attributes%comment="Aggregation/Average of 5 thickness categories. Sea ice density is 900 kg m^-3. Snow density is 330 * 1.036 kg m^-3."
  global_attributes%fill_value="Fill value is -9.99e33 for land, 0 for sea ice free ocean"
/
EOF
