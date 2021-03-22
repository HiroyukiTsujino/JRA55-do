#!/bin/bash

. /etc/profile
. ~/.bash_profile

monnam[1]="Jan";monnam[2]="Feb";monnam[3]="Mar";monnam[4]="Apr";monnam[5]="May";monnam[6]="Jun"
monnam[7]="Jul";monnam[8]="Aug";monnam[9]="Sep";monnam[10]="Oct";monnam[11]="Nov";monnam[12]="Dec"

set -e

cd /home/htsujino/JRA55-do

pwd

. ./util/datel_leap.sh

#if [ x${3} = x ]; then
#   echo "Usage: ${0} year month day netcdf_file_date"
#   exit
#fi

year=`date +%Y`
month=`date +%m`
day=`date +%d`

dyear=$( nddateb ${year} ${month} ${day} 4 | awk '{print $1 }' )
dmonth=$( nddateb ${year} ${month} ${day} 4 | awk '{print $2 }' )
dday=$( nddateb ${year} ${month} ${day} 4 | awk '{print $3 }' )

pyear=$( nddateb ${year} ${month} ${day} 1 | awk '{print $1 }' )
pmonth=$( nddateb ${year} ${month} ${day} 1 | awk '{print $2 }' )
pday=$( nddateb ${year} ${month} ${day} 1 | awk '{print $3 }' )

fdate=`date +%d%b%Y`
ddate=`date +%Y%m%d`

pyr=$( printf %04d $pyear )
pmn=${monnam[${pmonth}]}
pdy=$( printf %02d $pday )

fdatep=${pdy}${pmn}${pyr}

echo ${dyear} ${dmonth} ${dday} ${ddate} ${fdate} ${fdatep}

#----------------------------------------------------
# remove older files

pushd ./linkdir/products/version_1_5/netCDF
echo "Remove older files (main)"
rm -f *.${dyear}.${fdatep}.nc
popd
pushd ./linkdir/products/support/netCDF
echo "Remove older files (support)"
rm -f *.${dyear}.${fdatep}.nc
popd

#----------------------------------------------------
# surface atmospheric variables

./update_to_latest/update_jra55-do-v1_5_atmos.sh ${dyear} ${dmonth} ${dday} ${fdate}

# river discharge

./update_to_latest/update_jra55-do-v1_5_river.sh ${dyear} ${dmonth} ${dday} ${fdate}

# CMORize

pushd for_omip_cmor/input4MIPs-cmor-tables
ln -sfn input4MIPs_CV.json.MRI-JRA55-do-all-versions input4MIPs_CV.json
popd
pushd for_omip_cmor/MRI-JMA-JRA55-do-1-5-0-1
conda activate mypy38
sed -e s%@filedate@%${fdate}% \
    runCmorAllWrite-1-5-0-1-update.py_template > runCmorAllWrite-1-5-0-1-update.py
python ./runCmorAllWrite-1-5-0-1-update.py
conda deactivate
popd

pushd ./linkdir/products/version_1_5/input4MIPs/CMIP6/OMIP/MRI/MRI-JRA55-do-1-5-0-1

for item in uas vas tas huss psl ts
do
  fname=`find -type d -name "latest" -prune -o -type f -name "*${item}_input4MIPs_*20210101*.nc" -print`
  #echo ${fname}
  if [ x${fname} = x ]; then
    echo "No new file for ${item}"
  else
    rm -f ./atmos/3hrPt/${item}/gr/latest/${item}*
    mv -f ${fname} ./atmos/3hrPt/${item}/gr/latest/.
    rm -fr ./atmos/3hrPt/${item}/gr/v${ddate}
  fi
done

for item in rsds rlds prra prsn
do
  fname=`find -type d -name "latest" -prune -o -type f -name "*${item}_input4MIPs_*20210101*.nc" -print`
  #echo ${fname}
  if [ x${fname} = x ]; then
    echo "No new file for ${item}"
  else
    rm -f ./atmos/3hr/${item}/gr/latest/${item}*
    mv -f ${fname} ./atmos/3hr/${item}/gr/latest/.
    rm -fr ./atmos/3hr/${item}/gr/v${ddate}
  fi
done

for item in friver
do
  fname=`find -type d -name "latest" -prune -o -type f -name "*${item}_input4MIPs_*20210101*.nc" -print`
  #echo ${fname}
  if [ x${fname} = x ]; then
    echo "No new file for ${item}"
  else
    rm -f ./land/day/${item}/gr/latest/${item}*
    mv -f ${fname} ./land/day/${item}/gr/latest/.
    rm -fr ./land/day/${item}/gr/v${ddate}
  fi
done

for item in licalvf
do
  fname=`find -type d -name "latest" -prune -o -type f -name "*${item}_input4MIPs_*20210101*.nc" -print`
  #echo ${fname}
  if [ x${fname} = x ]; then
    echo "No new file for ${item}"
  else
    rm -f ./landIce/day/${item}/gr/latest/${item}*
    mv -f ${fname} ./landIce/day/${item}/gr/latest/.
    rm -fr ./landIce/day/${item}/gr/v${ddate}
  fi
done

for item in siconca
do
  fname=`find -type d -name "latest" -prune -o -type f -name "*${item}_input4MIPs_*20210101*.nc" -print`
  #echo ${fname}
  if [ x${fname} = x ]; then
    echo "No new file for ${item}"
  else
    rm -f ./seaIce/3hrPt/${item}/gr/latest/${item}*
    mv -f ${fname} ./seaIce/3hrPt/${item}/gr/latest/.
    rm -fr ./seaIce/3hrPt/${item}/gr/v${ddate}
  fi
done

for item in siconc
do
  fname=`find -type d -name "latest" -prune -o -type f -name "*${item}_input4MIPs_*20210101*.nc" -print`
  #echo ${fname}
  if [ x${fname} = x ]; then
    echo "No new file for ${item}"
  else
    rm -f ./seaIce/day/${item}/gn/latest/${item}*
    mv -f ${fname} ./seaIce/day/${item}/gn/latest/.
    rm -fr ./seaIce/day/${item}/gn/v${ddate}
  fi
done
for item in tos
do
  fname=`find -type d -name "latest" -prune -o -type f -name "*${item}_input4MIPs_*20210101*.nc" -print`
  #echo ${fname}
  if [ x${fname} = x ]; then
    echo "No new file for ${item}"
  else
    rm -f ./ocean/day/${item}/gn/latest/${item}*
    mv -f ${fname} ./ocean/day/${item}/gn/latest/.
    rm -fr ./ocean/day/${item}/gn/v${ddate}
  fi
done

cp jra55do_latest.html_template tmp.html_template1
for item in uas vas tas huss psl rsds rlds prra prsn friver licalvf ts siconca siconc tos
do
  fname=`find -name "*${item}_input4MIPs_*20210101*.nc"`
  sed -e s%@${item}_2021@%${fname}% \
      tmp.html_template1 > tmp.html_template2
  mv -f tmp.html_template2 tmp.html_template1
done
mv -f tmp.html_template1 jra55do_latest.html

popd

pushd ./linkdir/products/version_1_5/input4MIPs/CMIP6/OMIP/MRI
./rsync_climate-1-5-0-1.sh
popd
