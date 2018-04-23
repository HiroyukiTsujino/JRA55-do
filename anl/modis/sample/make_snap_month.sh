#!/bin/bash

yymm=$1

dir_raw=/worka/ksakamot/data/satellite/modis/sst_d_raw/20${yymm}
dir_temp=/worka/ksakamot/data/satellite/modis/temp
dir_out=/worka/ksakamot/data/satellite/modis/sst_grads/east_china

prefix=A2GL1${yymm}


sh select_file.sh ${dir_raw} ${prefix}
sh uncompress.sh ${dir_raw} ${dir_temp}
sh make_snap_dir.sh ${dir_temp} ${dir_out} 1 2 1 2


exit 0
