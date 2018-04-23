#!/bin/bash -f

set -e

rm -f namelist.calibpcp

#for item in prcp
#for item in snow
for item in prcp snow
do
#  for period in 1996_1999
#  for period in 2000_2004
#  for period in 2005_2008 2009_2016
#  for period in 1997_1998 1999_2005 2006_2006 2007_2016 2017_present
  for period in 2017_present
  do
    sed -e s%@item@%${item}% \
        namelist.calibpcp.monthly_v1_2_${period}_template > namelist.calibpcp
    ./calib_precip_interannual > calib_v1_2_${item}_${period}.log
  done
done
