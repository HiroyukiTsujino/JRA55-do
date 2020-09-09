#!/bin/sh

set -e

# Where you extracted the tarball that contains restart and sample output files.
CaMa_Flood_output=/data/htsujino/CaMa-Flood/OUTPUT_MRI

# This is created by operations at ../runoff_mk_input
CaMa_Flood_input=../linkdir/work/jra55fcst_v1_3_input_runoff_1dy_025x025

rm -f INPUT
rm -f OUTPUT

ln -sfn ${CaMa_Flood_input} INPUT
ln -sfn ${CaMa_Flood_output} OUTPUT
