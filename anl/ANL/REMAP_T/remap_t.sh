#! /bin/sh
#
#  Usage: hs2moc.sh flin_u flin_v flin_ssh file_moc file_stdout l_cmip5
#

flin=$1
flout=$2
tuw=$3
m_rec=$4
k_mask=$5
undef=$6
l_out_area=$7
l_out_mask=$8
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

./remap_t<<EOF
&nml_rmp_t
 idiv=20,
 jdiv=10,
 sph_grid="MRICOM1_0x0_5",
 tran_lat=63.0d0,
 m_rec=${m_rec},
 tuw="${tuw}",
 undef=${undef},
 k_mask=${k_mask},
 l_out_area=${l_out_area}
 l_out_mask=${l_out_mask}
 flin="$flin",
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
