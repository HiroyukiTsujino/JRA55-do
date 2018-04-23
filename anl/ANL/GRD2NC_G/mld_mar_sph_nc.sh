#! /bin/sh
#
# echo 'Usage: grd2nc_ht.sh flin flout'

#flin="$1"
#flout="$2"

flin="../time_series/mld/hs_mld.spherical."
flout="../wgomd/mld_mar.nc"
flctl="../wgomd/mld_mar.ctl"
time_start="mar1241"
time_intv="1yr"

fltopo="../../data/topo.d"
flvgrid="../../data/vgrid.d"
flibas="../../data/basin_map.txt"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./grd2nc_g<<EOF
&nml_grd2nc_g
 file_in="$flin",
 file_out="$flout",
 tuw="t"
 l_append_ext=.true.,
 cext="03",
 l_one_data=.false.,
 l_mask_out=.false.,
 num_vars=1,
 num_var_out=1,
 lon_first=0.0d0,delta_lon=1.0d0,nlons=360,l_lon_model=.false.,lon_units="degrees_east",
 lat_first=-90.0d0,delta_lat=0.5d0,nlats=361,l_lat_model=.false.,lat_units="degrees_north",
 zlev=0.0d0,,,
 nlvls=1,
 l_lev_model=.false.,
 lvl_units="m",
 i_ref_year=1241,
 ibyear=1241,
 ieyear=1300,
 intv_indx=3,
 nrecs_per_year=1,
 l_leap=.false.,
 file_topo="$fltopo",
 file_vgrid="$flvgrid",
 file_scale="$flsclf",
 file_basin="$flibas",
 file_ctl="${flctl}"
 time_start="${time_start}"
 time_intv="${time_intv}"
/
&nml_vars
 nth_place_tmp=1,
 vname_tmp='mld'
 vunit_tmp='m',
 vlongname_tmp='March-mean mixed layer depth'
 rconv_tmp=1.0,
 rmin_tmp=-9.99e33,
 rmax_tmp=9.99e33,
 undef_in_tmp=-9.99e33,
 undef_out_tmp=-9.99e33,
/
