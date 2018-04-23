#!/bin/bash
#-- Print text to standard out and "acitivity stream".

if [ $# -lt 1 ]; then
  echo "Usage: sh $0 status" 
  exit 1
fi


echo "$1"


if [ ! -s stream.conf ]; then
  exit 0
fi

stream_file=`cat stream.conf`
touch ${stream_file}

date=`date +"%m/%d %H:%M"`
root_dir=`pwd | sed -e "s@/setting@@g" | sed -e "s@.*/\([^/]*\)@\1@g"`
root_dir=`printf "%-10s" ${root_dir}`
echo "${date} [${root_dir}] $1" >> ${stream_file}

exit 0

