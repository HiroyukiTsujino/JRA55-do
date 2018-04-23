#!/bin/bash
#- Convert files in a directory.

#dir_in=/worka/ksakamot/data/satellite/modis/temp
#dir_out=/worka/ksakamot/data/satellite/modis/sst_grads/east_china

#mx_str=1
#mx_end=2
#my_str=1
#my_end=2

dir_in=$1
dir_out=$2

mx_str=$3
mx_end=$4
my_str=$5
my_end=$6


#======


exe=snap_ctl
make ${exe}


#-- List up snap shots.

list_file=list.txt
list_snap=snap.txt

ls ${dir_in} > ${list_file}
uniq -w 25 ${list_file} > ${list_snap}
snaps=`sed -e "s/_[^_]*$//g" ${list_snap}`


#-- Convert!

mkdir -p ${dir_out}

for snap in ${snaps}; do

  file_base=${dir_in}/${snap}
  fileo=${dir_out}/sst.20`echo ${snap} | cut -c 6-11`

  lin00=.false.
  lin01=.false.
  lin02=.false.
  lin03=.false.
  lin04=.false.
  lin05=.false.
  lin06=.false.
  lin07=.false.
  lin08=.false.
  lin09=.false.
  lin10=.false.
  lin11=.false.

  files=`grep ${snap} ${list_file}`
  regions=`echo ${files} | sed -e "s/${snap}//g"`

  if [ "`echo ${regions} | grep 00`" != "" ]; then
    lin00=.true.
  fi
  if [ "`echo ${regions} | grep 01`" != "" ]; then
    lin01=.true.
  fi
  if [ "`echo ${regions} | grep 02`" != "" ]; then
    lin02=.true.
  fi
  if [ "`echo ${regions} | grep 03`" != "" ]; then
    lin03=.true.
  fi
  if [ "`echo ${regions} | grep 04`" != "" ]; then
    lin04=.true.
  fi
  if [ "`echo ${regions} | grep 05`" != "" ]; then
    lin05=.true.
  fi
  if [ "`echo ${regions} | grep 06`" != "" ]; then
    lin06=.true.
  fi
  if [ "`echo ${regions} | grep 07`" != "" ]; then
    lin07=.true.
  fi
  if [ "`echo ${regions} | grep 08`" != "" ]; then
    lin08=.true.
  fi
  if [ "`echo ${regions} | grep 09`" != "" ]; then
    lin09=.true.
  fi
  if [ "`echo ${regions} | grep 10`" != "" ]; then
    lin10=.true.
  fi
  if [ "`echo ${regions} | grep 11`" != "" ]; then
    lin11=.true.
  fi

./${exe}<<EOF
  &snap_lst
    file_base="${file_base}",
    fileo="${fileo}",
    lin= ${lin00},${lin01},${lin02},${lin03},${lin04},${lin05},${lin06},${lin07},${lin08},${lin09},${lin10},${lin11}, 
    mx_str=${mx_str},
    mx_end=${mx_end},
    my_str=${my_str},
    my_end=${my_end},
    lgrads=.false.,
  /
EOF

done


rm ${list_snap}

exit 0
