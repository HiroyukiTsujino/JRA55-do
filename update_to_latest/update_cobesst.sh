#! /bin/sh

set -e

if [ x${2} == x ]; then
   echo "Usage: ${0} year end_month (0 to process only daily data)"
   exit
fi

year=${1}
end_mon=${2}

HOME=`pwd`

cobesst_org=front:/mri-data/climate_1/Kai/COBE-SST
cobesst_dir=/work116/htsujino/COBESST

echo "copying COBESST daily ......"

rsync -auv ${cobesst_org}/day/sst-glb.${year}.dat ${cobesst_dir}/daily/grib/.

echo "grib2grads for daily ......"

cd ${cobesst_dir}/daily

pwd

./grib2grads.sh ${year} ${year}

cd ${HOME}

if [ x${end_mon} == x0 ]; then
  echo " Copy and process only daily data ... Exiting"
  exit
fi

echo "copying COBESST monthly ...."

rsync -auv ${cobesst_org}/mon/sst-glb.${year}.dat ${cobesst_dir}/monthly/grib/.

echo "....... done"

echo "grib2grads for monthly ......"

cd ${cobesst_dir}/monthly

pwd

./grib2grads.sh ${year} ${year}

echo ${HOME}

cd ${HOME}

cd ../cobesst

echo "separate into monthly files"

./make_sepmondata.sh ${year} ${year} ${end_mon}

echo "....... done"
