#! /bin/sh
#
#  Usage: hs2moc.sh flin_u flin_v flin_ssh file_moc file_stdout l_cmip5
#

flin=${1}
flout=${2}
tuw=${3}
l_use_core=${4}
item_start=${5}
k_start=${6}
k_end=${7}
undef=${8}
l_read_table=${9}
l_check_table=${10}
file_rmp_table=${11}
file_recv_area=${12}

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./remap_scalar<<EOF
&nml_rmp_scl
 idiv=10,
 jdiv=10,
 sph_grid="WOA1x1",
 tuw="${tuw}",
 l_use_core=${l_use_core},
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
EOF

# sph_grid="WOA1x1",
