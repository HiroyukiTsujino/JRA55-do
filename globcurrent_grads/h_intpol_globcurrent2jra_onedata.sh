#!/bin/sh

set -e

l_mask_out=.true.

for item in u v
do 
  if [ ${item} = "u" ]; then
    irecv=1
  fi

  if [ ${item} = "v" ]; then
    irecv=2
  fi

  flin1="/denkei-shared/og1/htsujino/GlobCurrent/global_025_deg/total_hs/grads_clim/uv_nov1999-oct2009_9p20.dat"
  file_topo_gc="/denkei-shared/og1/htsujino/GlobCurrent/global_025_deg/data/globcurrent_${item}_nov1999-oct2009_9p20_topo.d"
  flot1="/denkei-shared/og1/htsujino/GlobCurrent/TL319_grid/annclim/${item}surf_nov1999-oct2009_9p20.gd"
  file_topo="/denkei-shared/og1/htsujino/GlobCurrent/TL319_grid/data/jra55_globcurrent_${item}_nov1999-oct2009_9p20_topo.d"
  file_mask="/denkei-shared/og1/htsujino/GlobCurrent/TL319_grid/data/jra55_globcurrent_${item}_nov1999-oct2009_9p20_mask.gd"

./globcurrent_to_jra_onedata<<EOF
&nml_hintpol
 flin1="${flin1}",
 flot1="${flot1}",
 irecv=${irecv}
 undef_in=-9.99e33
 undef_out=0.0e0
 file_topo_gc="${file_topo_gc}"
 file_topo="${file_topo}"
 file_mask="${file_mask}"
 l_mask_out=${l_mask_out}
 /
EOF

done
