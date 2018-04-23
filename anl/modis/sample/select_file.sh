#!/bin/bash
#- Select files in a directory

#dir=/worka/ksakamot/data/satellite/modis/sst_d_raw/201105
#prefix=A2GL11105

dir=$1
prefix=$2


#======

#-- List up snap shots.

list_file=list.txt
list_snap=snap.txt

ls ${dir} > ${list_file}
uniq -w 25 ${list_file} > ${list_snap}
sed -i -e "s/_sst.*$//g" ${list_snap}


#-- Delete duplicated files.

nday=1
while [ ${nday} -le 31 ]; do

  day=`printf "%02d" ${nday}`
  hdate=${prefix}${day}

  snaps=`grep ${hdate} ${list_snap}`
  nsnaps=`echo ${snaps} | wc -w`

  if [ ${nsnaps} -gt 1 ]; then

    snap=`echo ${snaps} | cut -d " " -f 1`
    num_snap=`grep ${snap} ${list_file} | wc -l`

    snaps=`echo ${snaps} | cut -d " " -f 2-`
    for snapn in ${snaps}; do

      num_snapn=`grep ${snap} ${list_file} | wc -l`

      if [ ${num_snapn} -le ${num_snap} ]; then 
        snap_del=${snapn}
      else
        snap_del=${snap}
        snap=${snapn}
        num_snap=${num_snapn}
      fi
      echo Delete ${snap_del}
      sed -i "/${snap_del}/d" ${list_file}

    done

  fi

  nday=`expr ${nday} + 1`

done

rm ${list_snap}

echo "Finish! See "${list_file}

exit 0
