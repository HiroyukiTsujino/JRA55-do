#! /bin/sh
#

ipxm=360
ipym=157
iptm=59
irfile="../../result/SO_taux_sfc.grd"
oafile="../../result/SO_taux_sfc_ave.grd"
tpfile="../../result/SO_taux_sfc_tmp.grd"
eofile="../../result/SO_taux_sfc_eof.grd"
etfile="../../result/SO_taux_sfc_eof_time.grd"
undef_val=-9.99e+33

F_RECLUNIT=BYTE    ; export F_RECLUNIT

./eof2<<EOF
 &nml_eof
 ipx=$ipxm
 ipy=$ipym
 ipt=$iptm
 irfile="$irfile"
 oafile="$oafile"
 tpfile="$tpfile"
 eofile="$eofile"
 etfile="$etfile"
 undef=$undef_val
 /
EOF

