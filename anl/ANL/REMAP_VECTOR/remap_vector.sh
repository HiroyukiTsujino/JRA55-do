#! /bin/sh
#
#  Usage: hs2moc.sh flin_u flin_v flin_ssh file_moc file_stdout l_cmip5
#

flinu=${1}
flinv=${2}
floutu=${3}
floutv=${4}
itemu_start=${5}
itemv_start=${6}
k_start=${7}
k_end=${8}
undef=${9}
l_read_table=${10}
l_area_weight=${11}
l_bicubic_trn=${12}
l_check_table=${13}
file_rmp_table=${14}
file_recv_area=${15}

fltopo="../data/topo.d"
flvgrid="../data/vgrid.d"
flsclf="../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./remap_vector<<EOF
&nml_rmp_vec
 idiv=20,
 jdiv=20,
 sph_grid="MRICOM1_0x0_5u",
 undef_in=${undef},
 undef_out=${undef},
 itemu_start=${itemu_start},
 itemv_start=${itemv_start},
 k_start=${k_start},
 k_end=${k_end},
 l_read_table=${l_read_table},
 l_area_weight=${l_area_weight},
 l_bicubic_trn=${l_bicubic_trn},
 l_check_table=${l_check_table},
 file_rmp_table="${file_rmp_table}",
 file_recv_area="${file_recv_area}",
 flinu="${flinu}",
 flinv="${flinv}",
 floutu="${floutu}",
 floutv="${floutv}",
 fltopo="${fltopo}",
 flsclf="${flsclf}",
 flstdout="stdout.txt",
 /
&inflg
 file_vgrid="${flvgrid}"
 /
EOF
