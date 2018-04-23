#! /bin/sh
#
#  Usage: remap_scalar.sh
#

flin=${1}
flout=${2}
tuw=${3}
l_use_core=${4}
l_use_ocean=${5}
item_start=${6}
k_start=${7}
k_end=${8}
undef=${9}
l_read_table=${10}
l_check_table=${11}
file_rmp_table=${12}
file_recv_area=${13}
input_little=${14}

echo ${input_little}
#F_RECLUNIT=BYTE    ; export F_RECLUNIT

./remap_scalar<<EOF
&nml_rmp_scl
 idiv=10,
 jdiv=10,
 sph_grid="WOA1x1",
 tuw="${tuw}",
 l_use_core=${l_use_core},
 l_use_ocean=${l_use_ocean},
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
 flstdout="stdout.txt",
 l_input_little=${input_little}
 /
EOF
