#! /bin/sh

if [ x${3} == x ]; then
   echo "Usage: ./jra55_to_trp_scalar.sh start_year end_year item"
   exit
fi

yearst=${1}
yeared=${2}
item=${3}

################

undef=9.999d20

ftable="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5/remap/file_rmp_bilin_a2ot.d"
farea="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5/remap/rmp_tpoint_area.gd"

. ../util/datel_leap.sh

year=${yearst}

while [ ${year} -le ${yeared} ];
do
  echo ${year}

  leap=`isleap ${year}`
  iend=`expr 365 + ${leap}`
  id_st=1
  id_ed=$(( ${iend} * 8 ))

  echo ${id_st} ${id_ed}

  flin="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/TL319_grid_v2/${year}/${item}.${year}"
  flout="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5-v2/${year}/${item}.${year}"
  ./map_scalar.sh ${flin} ${flout} t ${id_st} ${id_ed} 1 1 1 1 1 1 ${undef} .true. .false. ${ftable} ${farea}

  flin="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/TL319_grid_v2/${year}/${item}.${year}010100"
  flout="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5-v2/${year}/${item}.${year}010100"
  ./map_scalar.sh ${flin} ${flout} t 1 1 1 1 1 1 1 1 ${undef} .true. .false. ${ftable} ${farea}

  flin="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/TL319_grid_v2/${year}/${item}.${year}010103"
  flout="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5-v2/${year}/${item}.${year}010103"
  ./map_scalar.sh ${flin} ${flout} t 1 1 1 1 1 1 1 1 ${undef} .true. .false. ${ftable} ${farea}

  flin="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/TL319_grid_v2/${year}/${item}.${year}123121"
  flout="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5-v2/${year}/${item}.${year}123121"
  ./map_scalar.sh ${flin} ${flout} t 1 1 1 1 1 1 1 1 ${undef} .true. .false. ${ftable} ${farea}

  yearn=`expr ${year} + 1`
  year=${yearn}

done
