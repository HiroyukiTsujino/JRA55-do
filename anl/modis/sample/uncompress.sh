#!/bin/bash


#dir_in=/worka/ksakamot/data/satellite/modis/sst_d_raw/201105
#dir_out=/worka/ksakamot/data/satellite/modis/temp

dir_in=$1
dir_out=$2


#======

file_list=list.txt


mkdir -p ${dir_out}
rm -f ${dir_out}/*

files=`cat ${file_list}`

for file in ${files}; do
  cp ${dir_in}/${file} ${dir_out}
done

gunzip ${dir_out}/*


exit 0
