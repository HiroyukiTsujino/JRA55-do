#!/bin/bash

datadir=../data

#

file_grid=${datadir}/vgrid.d
file_topo=${datadir}/topo.d

linearx=.true.
lineary=.false.
linearz=.false.

./mkgrads_xyzdefs <<EOF
 &ngrdefs
  file_grid="${file_grid}",
  file_topo="${file_topo}",
  linear_x=${linearx}
  linear_y=${lineary}
  linear_z=${linearz}
 /
EOF
