#!/bin/bash


hostname=`hostname -s`

path_org=/worka/ksakamot/nao99b/omap

rm -f omap

if [ ${hostname} = "ocsv001" ]; then
  ln -sf ${path_org} omap
  path_data=${path_org}
else
  if [ $# -lt 1 ]; then
    echo "Specify data directory"
    exit 1
  fi
  path_data=$1
  mkdir -p ${path_data}
  rsync -av ocsv001:${path_org}/ ${path_data}
  ln -sf ${path_data} omap
fi

echo Make omap to ${path_data}

exit 0
