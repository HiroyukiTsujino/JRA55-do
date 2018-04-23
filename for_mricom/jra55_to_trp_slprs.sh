#! /bin/sh
#
# echo 'Usage: jra55_to_trp_tmp2m.sh start_year end_year'

yearst=${1}
yeared=${2}

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

  flin="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/TL319_grid/${year}/tmp2m.${year}"
  flout="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5/${year}/tmp2m.${year}"
  ./map_scalar.sh ${flin} ${flout} t ${id_st} ${id_ed} 1 1 1 1 1 1 ${undef} .false. .true. ${ftable} ${farea}

  yearn=`expr ${year} + 1`
  year=${yearn}

done
