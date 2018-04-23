#! /bin/sh
#
#  Usage: gmmoc_depth.sh file_extension (e.g., ./gmmoc_depth.sh YYYYMMDD)
#

flin_u=../result_glb/hs_gm_u.${1}
flin_v=../result_glb/hs_gm_v.${1}
flin_ssh=../result_glb/hs_ssh.${1}
file_moc=../result_glb/gmmoc_par.${1}
flibas="../../../linkdir/data_glb/basin_map.txt"
file_exclude="../../../linkdir/data_glb_nest/upscale_aggregate.gd"
file_specify="dummy.gd"
file_check_used=../result_glb/gmmoc_used_up.${1}

F_RECLUNIT=BYTE; export F_RECLUNIT

./gmmoc_depth<<EOF
&nml_gmmoc_dep
 flin_u="$flin_u",
 flin_v="$flin_v",
 flin_ssh="$flin_ssh",
 flout="$file_moc",
 l_read_basin=.true.,
 basin_index=0,
 flibas="${flibas}",
 l_exclude=.false.
 file_exclude="${file_exclude}"
 l_specify=.false.
 file_specify="${file_specify}",
 l_check_used=.false.
 file_check_used="${file_check_used}",
 slatg=-78.0d0,
 elatg=75.0d0,
 dlatg=0.5d0
 /
EOF
