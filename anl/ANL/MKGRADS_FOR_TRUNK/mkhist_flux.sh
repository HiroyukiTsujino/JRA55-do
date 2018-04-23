#!/bin/bash

extin=${1}
extgd=${2}
nhist=${3}

indir=/work1/htsujino/trp1x05L51bbl/result-0414
outdir=/work1/htsujino/trp1x05L51bbl/result-0414

# flux

file_in=${indir}/hflux.${extin}
file_out=${outdir}/hist_flux.${extgd}

./mkgrads_flux_hist ${file_in} ${file_out} ${nhist}

