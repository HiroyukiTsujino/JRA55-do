#!/usr/bin/bash

source ./datel_leap.sh

year=${1}

leap=`isleap ${year}`

iend=`expr 365 + ${leap}`

echo "iend = ${iend}"

i=1

while [ $i -le $iend ];
do
  yr=$( nydate $year $i | awk '{print $1 }' )
  mn=$( nydate $year $i | awk '{print $2 }' )
  dy=$( nydate $year $i | awk '{print $3 }' )
 
  yr0=$( printf %04d $yr )
  mn0=$( printf %02d $mn )
  dy0=$( printf %02d $dy )

  date=${yr0}${mn0}${dy0} 

  echo ${i} ${date}

  i=$(( $i + 1 ))

done
