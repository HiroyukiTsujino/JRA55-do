#! /bin/sh
#
#  Usage: hs2moc.sh flin_u flin_v flin_ssh file_moc file_stdout l_cmip5
#

flin=${1}
flout=${2}
tuw=${3}
item_start=${4}
k_start=${5}
k_end=${6}
undef=${7}
l_read_table=${8}
l_check_table=${9}
file_rmp_table=${10}
file_recv_area=${11}

fltopo="../data/topo.d"
flvgrid="../data/vgrid.d"
flsclf="../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./remap_scalar<<EOF
&nml_rmp_scl
 idiv=20,
 jdiv=20,
 sph_grid="MRICOM1_0x0_5",
 tuw="${tuw}",
 undef_in=${undef},
 undef_out=${undef},
 item_start=${item_start},
 k_start=${k_start},
 k_end=${k_end},
 l_read_table=${l_read_table},
 l_check_table=${l_check_table},
 file_rmp_table="${file_rmp_table}"
 file_recv_area="${file_recv_area}"
 flin="${flin}",
 flout="${flout}",
 fltopo="${fltopo}",
 flsclf="${flsclf}",
 flstdout="stdout.txt",
 /
&inflg
 file_vgrid="$flvgrid"
 /
EOF

# sph_grid="WOA1x1",
