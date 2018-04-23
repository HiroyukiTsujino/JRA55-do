#! /bin/sh
#
# 海洋の子午面循環を計算
#
if [ -z $1 ] ; then
  echo 'Usage: totalmoc.sh fn_out'
  exit 1
fi

fn_out=${1}

fltopo="../../data/topo.d"
flvgrid="../../data/vgrid.d"
flibas="../../data/basin_map.txt"
flsclf="../../data/scale_factor.d"
flin_u="../clim/hs_u.$fn_out"
flin_v="../clim/hs_v.$fn_out"
flin_gm_u="../clim/hs_gm_u.$fn_out"
flin_gm_v="../clim/hs_gm_v.$fn_out"
flin_ssh="../clim/hs_ssh.$fn_out"
file_merid="../clim/total_moc.$fn_out"
file_atlmoc="../clim/atlmoc.$fn_out"
file_stdout="../logs/DGOUT/atlmoc_max.$fn_out"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./total_moc<<EOF
&nmlmoc
 flin_u="$flin_u",
 flin_v="$flin_v",
 flin_gm_u="$flin_gm_u",
 flin_gm_v="$flin_gm_v",
 flin_ssh="$flin_ssh",
 flout="$file_merid",
 flout_amoc="$file_atlmoc",
 fltopo="$fltopo",
 flsclf="$flsclf",
 flibas="$flibas",
 flstdout="$file_stdout",
 jmgeo=307,
 slatg=-78.25d0,
 dlatg=0.5d0,
 l_inc_gm=.true.
 /
&inflg
 file_vgrid="$flvgrid"
 /
EOF
