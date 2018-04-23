#! /bin/sh
#

flin=${1}
flout=${2}
tuw=${3}
id_start=${4}
id_end=${5}
item_start=${6}
item_end=${7}
item_layer=${8}
item_total=${9}
k_start=${10}
k_end=${11}
undef=${12}
l_read_table=${13}
l_check_table=${14}
file_rmp_table=${15}
file_recv_area=${16}

fltopo="../data/topo.d"
flvgrid="../data/vgrid.d"
flsclf="../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./map_scalar<<EOF
&nml_map_scl
 idiv=20,
 jdiv=10,
 sph_grid="T62",
 tuw="${tuw}",
 undef_in=${undef},
 undef_out=${undef},
 id_start=${id_start},
 id_end=${id_end},
 item_start=${item_start},
 item_end=${item_end},
 item_layer=${item_layer},
 item_total=${item_total},
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
