#!/bin/sh

set -e

orgdir=/work115/htsujino/SURF_FLUX/forcing/jra_cobesst_monthly_e4
newdir=/work115/htsujino/SURF_FLUX/forcing/jra_cobesst_monthly_e4

flin="${orgdir}/nswrf."
flout="${newdir}/nswrf.JRA55v0_3.nc"
flctl="${newdir}/nswrf.JRA55v0_3.ctl"

##############################
# if mask is written to file, l_mask_out = .true.
# and flibas with valid size must exist

l_mask_out=.false.
flibas="dummy_basin_map.txt"

# for GrADs xdfopen

time_start="jan1985"
time_intv="1mo"

./grd2nc_g<<EOF
&nml_grd2nc_g
 file_in="${flin}",
 file_out="${flout}",
 tuw="u"
 l_append_ext=.false.,
 cext="no",
 l_one_data=.false.,
 l_mask_out=${l_mask_out},
 num_vars=1,
 num_var_out=1,
 lon_first=0.5d0,delta_lon=1.0d0,nlons=360,l_lon_model=.true.,lon_units="degrees_east",
 lat_first=-89.5d0,delta_lat=1.0d0,nlats=180,l_lat_model=.true.,lat_units="degrees_north",
 zlev=0.0d0,,,
 nlvls=1,
 l_lev_model=.false.,
 lvl_units="m",
 i_ref_year=1985,
 i_ref_month=1,
 i_ref_day=1,
 i_ref_hour=0,
 i_ref_minute=0,
 i_ref_second=0,
 first_data_relative_to_ref=0.5d0,
 ibyear=1985,
 ieyear=2014,
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
 vname_tmp='rsntds'
 vunit_tmp='W/m2',
 vlongname_tmp='Net Downward Shortwave Radiation at Sea Water Surface'
 vstandardname_tmp='net_downward_shortwave_flux_at_sea_water_surface'
 rconv_tmp=1.0,
 rmin_tmp=-9.99e33,
 rmax_tmp=9.99e33,
 undef_in_tmp=9.999e20,
 undef_out_tmp=-9.99e33,
/
&nml_global_attributes
  global_attributes%title="Adjusted surface net shortwave radiation of JRA-55"
  global_attributes%institution="JMA Meteorological Research Institute"
  global_attributes%version="v0.3"
  global_attributes%comment="An adjusted CERES EBAF_Surface_Ed2.8 is used adjust JRA-55"
  global_attributes%fill_value="Fill value is -9.99e33"
/
EOF