#! /bin/sh
#
#  Usage: hs2moc.sh flin_u flin_v flin_ssh file_moc file_stdout l_cmip5
#

flinu=$1
flinv=$2
flout=$3
m_recu=$4
m_recv=$5
k_mask=$6
undef=$7
l_out_area=$8
l_out_mask=$9
#flin=../../run-mip-20120214/clim/hs_vat705_anom.1281_1300
#flout=dummy_out.d
#file_stdout=stdout.txt
#tuw=t
#m_rec=1
#k_mask=27
#undef=-9.99d33
#l_out_area=.true.
#l_out_mask=.true.

fltopo="../../data/topo.d"
flvgrid="../../data/vgrid.d"
flibas="../../data/basin_map.txt"
flsclf="../../data/scale_factor.d"

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./remap_uv<<EOF
&nml_rmp_uv
 idiv=20,
 jdiv=10,
 sph_grid="MRICOM1_0x0_5u",
 tran_lat=63.0d0,
 m_recu=${m_recu},
 m_recv=${m_recv},
 undef=${undef},
 k_mask=${k_mask},
 l_out_area=${l_out_area}
 l_out_mask=${l_out_mask}
 flinu="$flinu",
 flinv="$flinv",
 flout="$flout",
 fltopo="$fltopo",
 flsclf="$flsclf",
 flibas="$flibas",
 flstdout="stdout.txt",
 /
&inflg
 file_vgrid="$flvgrid"
 /
EOF
