#! /bin/sh
#
# Usage: monclim.sh  fincore foutcore tuxy dnum styear endyear

finssh=../clim/hs_ssh.1000
fltopo=../topo/topo.d
flvgrd=../topo/vgrid.d
flsclf=../topo/scale_factor.d

floutv=../clim/total_volume.gd
floutm=../clim/total_mass.gd

l_gridinfo=.true.

floutare=../clim/area_t.gd
floutvol=../clim/volume_t.gd
floutdep=../clim/depth_t.gd
flouttp=../clim/lonlat_t.gd
floutup=../clim/lonlat_u.gd

flout_lattg=../clim/lat_t_grp.gd
flout_lontg=../clim/lon_t_grp.gd
flout_latug=../clim/lat_u_grp.gd
flout_lonug=../clim/lon_u_grp.gd
flout_lonlat_t=../clim/lonlat_t_grp.gd
flout_lonlat_u=../clim/lonlat_u_grp.gd

########################################

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./volo<<EOF
&nml_volo
  flin_ssh="${finssh}"
  fltopo="${fltopo}"
  flvgrd="${flvgrd}"
  flsclf="${flsclf}"
  floutv="${floutv}"
  floutm="${floutm}"
  l_gridinfo=${l_gridinfo}
  floutare="${floutare}"
  floutvol="${floutvol}"
  floutdep="${floutdep}"
  flouttp="${flouttp}"
  floutup="${floutup}"
  flout_lattg="${flout_lattg}"
  flout_lontg="${flout_lontg}"
  flout_latug="${flout_latug}"
  flout_lonug="${flout_lonug}"
  flout_lonug="${flout_lonug}"
  flout_lonlat_t="${flout_lonlat_t}"
  flout_lonlat_u="${flout_lonlat_u}"
/
EOF
