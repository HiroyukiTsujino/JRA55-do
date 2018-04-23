#!/bin/bash

hostname=`hostname`
echo "hostname=$hostname"
case $hostname in
front*|ocsv001|ocsv003|ocsv004|ocsv005|ocsv011|ogsv007)
  rm -f macros.make
  cp sample/macros.make.${hostname} macros.make
  echo "macros.make.${hostname}" is set for macros.make.
  ;;
*)
  echo "Make setting/macros.make by hand."
  exit 1
esac


exit 0
