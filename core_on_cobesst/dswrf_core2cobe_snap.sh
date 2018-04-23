#!/bin/sh

################

ftable="/worke/htsujino/CORE/table/corea2cobe.d"
farea="/worke/htsujino/CORE/table/rmp_corea2cobe_area.gd"

flin="/workc/ocpublic/refdata/CORE/COREv2/ciaf/grads_oneitem/dswrf.1948"
flout="/worke/htsujino/CORE/core_tmp/dswrf_all.19480101"

./remap_scalar.sh ${flin} ${flout} u .false. .false. 1 1 1 -9.99d33 .false. .true. ${ftable} ${farea}
