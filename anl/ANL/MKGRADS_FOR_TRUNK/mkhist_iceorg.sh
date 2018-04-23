#!/bin/bash

extin=${1}
extgd=${2}
nhist=${3}

indir=/work1/htsujino/trp1x05L51bbl/result-0414
outdir=/work1/htsujino/trp1x05L51bbl/result-0414

# iceorg

file_in=${indir}/ice_hist.${extin}
file_out=${outdir}/hist_ice.${extgd}

./mkgrads_ice_hist ${file_in} ${file_out} ${nhist}
