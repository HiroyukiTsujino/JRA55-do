#! /bin/sh

set -e

if [ x${1} == x ]; then
   echo "Usage: ${0} year"
   exit
fi

year=${1}

jra55fcst_dir=/work116/htsujino/jra-55/Hist/Daily

cd ${jra55fcst_dir}

pwd

date > last_update.txt

echo "copying JRA55fcst ice ......"

./rsync_ice.sh ${year}

echo "copying JRA55fcst surf ....."

./rsync_surf.sh ${year}

echo "copying JRA55fcst phy2m ...."

./rsync_phy2m.sh ${year}

echo "copying JRA55fcst phyland ...."

./rsync_phyland.sh ${year}

echo "....... done"
