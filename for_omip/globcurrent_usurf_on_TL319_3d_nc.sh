#!/bin/sh

set -e

flin="/denkei-shared/og1/htsujino/SURF_FLUX/forcing/GlobCurrent_annclim/usurf_nov1999-oct2009.dat"
flout="/work113/htsujino/SURF_FLUX/forcing/OMIP_DATA/JRA55-do-suppl-v1.3.01/uosurf_3d_nov1999-oct2009.nc"
flctl="/work113/htsujino/SURF_FLUX/forcing/OMIP_DATA/JRA55-do-suppl-v1.3.01/uosurf_3d_nov1999-oct2009.ctl"

rm -f NAMELIST.MXE
ln -s NAMELIST.MXE.JRA55 NAMELIST.MXE

##############################
# if mask is written to file, l_mask_out = .true.
# and flibas with valid size must exist

l_mask_out=.false.
flibas="../../data/basin_map.txt"

# for GrADs xdfopen

time_start="00:00z01jan1958"
time_intv="1yr"

./grd2nc_g_input4mips<<EOF
&nml_grd2nc_g
 file_in="${flin}",
 file_out="${flout}",
 tuw="u"
 l_append_ext=.false.,
 cext="no",
 l_one_data=.true.,
 l_mask_out=${l_mask_out},
 l_space_2d=.false.,
 l_x_even=.true.,
 l_y_even=.false.,
 num_vars=1,
 num_var_out=1,
 lon_first=0.0d0,delta_lon=1.0d0,nlons=640,l_lon_model=.true.,lon_units="degrees_east",
 lat_first=-90.0d0,delta_lat=0.5d0,nlats=320,l_lat_model=.true.,lat_units="degrees_north",
 zlev=0.5d0,,,
 zlev_bnd=0.0d0,1.0d0,,
 nlvls=1,
 l_lev_model=.false.,
 lvl_units="m",
 i_ref_year=1900,
 i_ref_month=1,
 i_ref_day=1,
 i_ref_hour=0,
 i_ref_minute=0,
 i_ref_second=0,
 first_data_relative_to_ref=0.0d0,
 ibyear=1999,
 ieyear=1999,
 intv_indx=3,
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
 vname_tmp='uo',
 vunit_tmp='m s-1',
 vlongname_tmp='Sea Water X Velocity at 0 m depth',
 vstandardname_tmp='sea_water_x_velocity',
 rconv_tmp=1.0,
 rmin_tmp=-9.99e33,
 rmax_tmp=9.99e33,
 undef_in_tmp=-9.99e33,
 undef_out_tmp=1.e20,
/
&nml_global_attributes
  global_attributes%title="Sea Water X Velocity at 0 m depth from GlobCurrent Rio et al. (2014)"
  global_attributes%institution="JMA Meteorological Research Institute"
  global_attributes%version="v1.3.1"
  global_attributes%comment="Land values are Zero"
  global_attributes%fill_value="Fill value is 1.e20"
/
EOF
