#!/bin/sh

flin="../linkdir/forcing/jra_cobesst_clim_c2/u10m-jan2001_dec2007.dat"
flout="../linkdir/forcing/jra_cobesst_clim_c2/u10m-jan2001_dec2007.nc"

##############################
# if mask is written to file, l_mask_out = .true.
# and flibas with valid size must exist

l_mask_out=.false.
flibas="dummy_basin_map.txt"

# for GrADs xdfopen

flctl="../linkdir/forcing/jra_cobesst_clim_c2/u10m_annclim.ctl"
time_start="jan2001"
time_intv="1yr"

./grd2nc_g<<EOF
&nml_grd2nc_g
 file_in="${flin}",
 file_out="${flout}",
 tuw="u"
 l_append_ext=.false.,
 cext="no",
 l_one_data=.true.,
 l_mask_out=${l_mask_out},
 num_vars=1,
 num_var_out=1,
 lon_first=0.5d0,delta_lon=1.0d0,nlons=360,l_lon_model=.true.,lon_units="degrees_east",
 lat_first=-89.5d0,delta_lat=1.0d0,nlats=180,l_lat_model=.true.,lat_units="degrees_north",
 zlev=0.0d0,,,
 nlvls=1,
 l_lev_model=.false.,
 lvl_units="m",
 i_ref_year=2001,
 i_ref_month=1,
 i_ref_day=1,
 i_ref_hour=0,
 i_ref_minute=0,
 i_ref_second=0,
 first_data_relative_to_ref=0.0d0,
 ibyear=2001,
 ieyear=2001,
 intv_indx=1,
 nrecs_per_year=1,
 nrecs_out=1,
 l_leap=.true.,
 file_basin="${flibas}",
 file_ctl="${flctl}"
 time_start="${time_start}"
 time_intv="${time_intv}"
/
&nml_vars
 nth_place_tmp=1,
 vname_tmp='uas'
 vunit_tmp='m/s',
 vlongname_tmp='Eastward Wind at the 10 meter height'
 vstandardname_tmp='eastward_wind'
 rconv_tmp=1.0e-2,
 rmin_tmp=-9.99e33,
 rmax_tmp=9.99e33,
 undef_in_tmp=-9.99e33,
 undef_out_tmp=-9.99e33,
/
