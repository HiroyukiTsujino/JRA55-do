#! /bin/sh
#
#  Usage: hs2moc.sh flin_u flin_v flin_ssh file_moc file_stdout l_cmip5
#

flin_u=$1
flin_v=$2
flin_ssh=$3
file_moc=$4
file_stdout=$5
l_cmip5=$6

fltopo="../../data/topo.d"
flvgrid="../../data/vgrid.d"
flibas="../../data/basin_map.txt"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./hs2moc<<EOF
&nmlmoc
 flin_u="$flin_u",
 flin_v="$flin_v",
 flin_ssh="$flin_ssh",
 flout="$file_moc",
 fltopo="$fltopo",
 flsclf="$flsclf",
 flibas="$flibas",
 flstdout="$file_stdout",
 l_cmip5=$l_cmip5,
 /
&inflg
 file_vgrid="$flvgrid"
 /
EOF

