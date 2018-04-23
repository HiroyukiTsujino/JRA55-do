#!/bin/sh

set -e

rm -f NAMELIST.MXE
ln -s NAMELIST.MXE.GLBRIVER NAMELIST.MXE

flin="/work116/htsujino/SURF_FLUX/data_river/runoff_area.gd"
flout="/work116/htsujino/SURF_FLUX/forcing/OMIP_DATA/JRA55-do-v1.1/runoff_cell_area.15Dec2016.nc"
flctl="/work116/htsujino/SURF_FLUX/forcing/OMIP_DATA/JRA55-do-v1.1/runoff_cell_area.15Dec2016.ctl"

##############################
# if mask is written to file, l_mask_out = .true.
# and flibas with valid size must exist

l_mask_out=.false.

# for GrADs xdfopen

time_start="00:00z01jan1958"
time_intv="1yr"

./grd2nc_g_revised<<EOF
&nml_grd2nc_g
 file_in="${flin}",
 file_out="${flout}",
 tuw="u"
 l_append_ext=.false.,
 cext="no",
 l_one_data=.true.,
 l_mask_out=${l_mask_out},
 l_space_2d=.true.,
 l_x_even=.true.,
 l_y_even=.true.,
 num_vars=1,
 num_var_out=1,
 lon_first=0.125d0,delta_lon=0.25d0,nlons=1440,l_lon_model=.true.,lon_units="degrees_east",
 lat_first=-89.875d0,delta_lat=0.25d0,nlats=720,l_lat_model=.true.,lat_units="degrees_north",
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
 first_data_relative_to_ref=0.0d0,
 ibyear=1958,
 ieyear=1958,
 intv_indx=1,
 nrecs_per_year=1,
 nrecs_out=1,
 l_leap=.true.,
 file_basin="${flibas}",
 file_ctl="${flctl}"
 time_start="${time_start}"
 time_intv="${time_intv}"
 l_put_global_attributes=.true.
/
&nml_vars
 nth_place_tmp=1,
 vname_tmp='areacello',
 vunit_tmp='m2',
 vlongname_tmp='Horizontal area of a gridcell',
 vstandardname_tmp='cell_area',
 rconv_tmp=1.0,
 rmin_tmp=-9.99e33,
 rmax_tmp=9.99e33,
 undef_in_tmp=-9.99e33,
 undef_out_tmp=-9.99e33,
/
&nml_global_attributes
  global_attributes%title="Grid cell area of runoff data of 0.25 x 0.25 grid"
  global_attributes%institution="JMA Meteorological Research Institute"
  global_attributes%version="v1.1"
  global_attributes%comment="Radius of the Earth is set to 6375 km for the computation"
  global_attributes%fill_value="Fill value is -9.99e33"
/
EOF
