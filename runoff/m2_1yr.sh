#!/bin/sh -f

set -e

indir="/denkei-shared/og1/htsujino/SURF_FLUX/forcing/bamber_2012_Greenland/clim025x025"
outdir="/denkei-shared/og1/htsujino/SURF_FLUX/forcing/bamber_2012_Greenland/clim025x025"

outfile="${outdir}"/fwf_green_025x025.1961

if [ -e ${outfile} ]; then
  echo "${outfile} exists, please check"
  exit
fi

for mon in `seq -w 1 12`
do
  infile="${indir}/fwf_green_025x025.${mon}"
#  echo ${infile}
  cat ${infile} >> ${outfile}
done
