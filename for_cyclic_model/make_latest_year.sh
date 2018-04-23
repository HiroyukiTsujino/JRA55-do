#!/bin/bash -f

if [ x${3} == x ]; then
   echo "Usage: ./make_interannual.sh item start_year end_year"
   exit
fi

item=${1}
yearst=${2}
yeared=${3}
dayed=${4}

. config_${item}.sh

################

indir=/work116/htsujino/SURF_FLUX/forcing/jra_for_mricom/TL319_grid

outdir=/work116/htsujino/TP_NEST/forcing/JRA-55v1/ciaf

################

. ../util/datel_leap.sh

year=${yearst}

while [ ${year} -le ${yeared} ];
do

  if [ ${year} -eq ${yearst} ]; then
    lmakectl=.true.
  else
    lmakectl=.false.
  fi

  echo "YEAR = ${year}"
  iend=${dayed}
  echo "iend = ${iend}"
  rec_num=`expr ${iend} \* 8`
  ext=${year}
  echo "Number of records = ${rec_num}"
  sed -e s%@ext@%${ext}% \
      -e s%@year@%${year}% \
      -e s%@rec_num@%${rec_num}% \
      -e s%@indir@%${indir}% \
      -e s%@outdir@%${outdir}% \
      -e s%@item@%${item}% \
      -e s%@lmakectl@%${lmakectl}% \
      -e s%@short_name@%${short_name}% \
      -e s%@long_name@%${long_name}% \
      namelist.cut_out_template > namelist.cut_out
  ./cut_out

  rec_num=1
  ext=${year}010100
  echo "Number of records = ${rec_num}"
  sed -e s%@ext@%${ext}% \
      -e s%@year@%${year}% \
      -e s%@rec_num@%${rec_num}% \
      -e s%@indir@%${indir}% \
      -e s%@outdir@%${outdir}% \
      -e s%@item@%${item}% \
      -e s%@lmakectl@%${lmakectl}% \
      -e s%@short_name@%${short_name}% \
      -e s%@long_name@%${long_name}% \
      namelist.cut_out_template > namelist.cut_out
  ./cut_out

  rec_num=1
  echo "Number of records = ${rec_num}"
  ext=${year}010103
  sed -e s%@ext@%${ext}% \
      -e s%@year@%${year}% \
      -e s%@rec_num@%${rec_num}% \
      -e s%@indir@%${indir}% \
      -e s%@outdir@%${outdir}% \
      -e s%@item@%${item}% \
      -e s%@lmakectl@%${lmakectl}% \
      -e s%@short_name@%${short_name}% \
      -e s%@long_name@%${long_name}% \
      namelist.cut_out_template > namelist.cut_out
  ./cut_out

  yearn=`expr ${year} + 1`
  year=${yearn}

done
