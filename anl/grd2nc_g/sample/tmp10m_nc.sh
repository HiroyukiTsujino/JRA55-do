#!/bin/sh

flin="../linkdir/forcing/jra_for_mricom/TL319_grid/1961/tmp10m."
flout="../linkdir/forcing/jra_netCDF/t_10.1961.10MAR2015.nc"

##############################
# if mask is written to file, l_mask_out = .true.
# and flibas with valid size must exist

l_mask_out=.false.
flibas="../../data/basin_map.txt"

# for GrADs xdfopen

flctl="../linkdir/forcing/jra_netCDF/t_10.1961.ctl"
time_start="00z01jan1961"
time_intv="3hr"

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
 lon_first=0.0d0,delta_lon=1.0d0,nlons=640,l_lon_model=.true.,lon_units="degrees_east",
 lat_first=-90.0d0,delta_lat=0.5d0,nlats=320,l_lat_model=.true.,lat_units="degrees_north",
 zlev=10.0d0,,,
 nlvls=1,
 l_lev_model=.false.,
 lvl_units="m",
 i_ref_year=1961,
 i_ref_month=1,
 i_ref_day=1,
 i_ref_hour=0,
 i_ref_minute=0,
 i_ref_second=0,
 first_data_relative_to_ref=0.0d0,
 ibyear=1961,
 ieyear=1961,
 intv_indx=4,
 nrecs_per_year=2920,
 l_leap=.true.,
 file_basin="${flibas}",
 file_ctl="${flctl}"
 time_start="${time_start}"
 time_intv="${time_intv}"
/
&nml_vars
 nth_place_tmp=1,
 vname_tmp='tas_10m'
 vunit_tmp='K',
 vlongname_tmp='Near-Surface Air Temperature at the 10 meter height'
 vstandardname_tmp='air_temperature'
 rconv_tmp=1.0,
 rmin_tmp=-9.99e33,
 rmax_tmp=9.99e33,
 undef_in_tmp=-9.99e33,
 undef_out_tmp=-9.99e33,
/
