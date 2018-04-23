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
itemu_intv=${7}
itemv_intv=${8}
l_read_sep=${9}
l_write_sep=${10}
k_start=${11}
k_end=${12}
undef=${13}
id_start=${14}
id_end=${15}
l_read_table=${16}
file_rmp_table=${17}
file_recv_area=${18}

fltopo="../data/topo.d"
flvgrid="../data/vgrid.d"
flsclf="../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./map_vector<<EOF
&nml_map_vec
 idiv=20,
 jdiv=10,
 sph_grid="TL319",
 undef_in=${undef},
 undef_out=${undef},
 id_start=${id_start},
 id_end=${id_end},
 k_start=${k_start},
 k_end=${k_end},
 l_read_table=${l_read_table},
 file_rmp_table="${file_rmp_table}",
 file_recv_area="${file_recv_area}",
 flinu="${flinu}",
 flinv="${flinv}",
 itemu_start=${itemu_start},
 itemv_start=${itemv_start},
 itemu_intv=${itemu_intv},
 itemv_intv=${itemv_intv},
 l_read_sep=${l_read_sep},
 l_write_sep=${l_write_sep},
 floutu="${floutu}",
 floutv="${floutv}",
 fltopo="${fltopo}",
 flsclf="${flsclf}",
 flstdout="stdout_vector.txt",
 /
&inflg
 file_vgrid="$flvgrid"
 /
EOF
