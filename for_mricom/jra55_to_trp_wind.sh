#! /bin/sh
#
# echo 'Usage: jra55_to_trp_wind.sh start_year end_year'

yearst=${1}
yeared=${2}

################

undef=9.999d20

ftable="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5/remap/file_rmp_bicubic_a2ou.d"
farea="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5/remap/rmp_upoint_area.gd"

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

  flinu="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/TL319_grid/${year}/u10m.${year}"
  flinv="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/TL319_grid/${year}/v10m.${year}"
  floutu="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5/${year}/u10m.${year}"
  floutv="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5/${year}/v10m.${year}"
  ./map_vector.sh ${flinu} ${flinv} ${floutu} ${floutv} 1 1 1 1 .true. .true. 1 1 ${undef} ${id_st} ${id_ed} .true. .false. ${ftable} ${farea}

  flinu="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/TL319_grid/${year}/u10m.${year}010100"
  flinv="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/TL319_grid/${year}/v10m.${year}010100"
  floutu="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5/${year}/u10m.${year}010100"
  floutv="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5/${year}/v10m.${year}010100"
  ./map_vector.sh ${flinu} ${flinv} ${floutu} ${floutv} 1 1 1 1 .true. .true. 1 1 ${undef} 1 1 .true. .false. ${ftable} ${farea}

  flinu="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/TL319_grid/${year}/u10m.${year}010103"
  flinv="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/TL319_grid/${year}/v10m.${year}010103"
  floutu="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5/${year}/u10m.${year}010103"
  floutv="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5/${year}/v10m.${year}010103"
  ./map_vector.sh ${flinu} ${flinv} ${floutu} ${floutv} 1 1 1 1 .true. .true. 1 1 ${undef} 1 1 .true. .false. ${ftable} ${farea}

  flinu="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/TL319_grid/${year}/u10m.${year}123121"
  flinv="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/TL319_grid/${year}/v10m.${year}123121"
  floutu="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5/${year}/u10m.${year}123121"
  floutv="/worke/htsujino/SURF_FLUX/forcing/jra_for_mricom/tripolar-1_0-0_5/${year}/v10m.${year}123121"
  ./map_vector.sh ${flinu} ${flinv} ${floutu} ${floutv} 1 1 1 1 .true. .true. 1 1 ${undef} 1 1 .true. .false. ${ftable} ${farea}

  yearn=`expr ${year} + 1`
  year=${yearn}
done
