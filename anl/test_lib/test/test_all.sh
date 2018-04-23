#!/bin/bash

set -e

sh test/glb/test_para.sh > log.para-glb.txt
sh test/glb/test_topo.sh > log.topo-glb.txt
sh test/rect/test_para.sh > log.para-rect.txt
sh test/rect/test_topo.sh > log.topo-rect.txt
sh test/rect/test_topo-3_2.sh > log.topo-rect-3_2.txt
sh test/rect/test_grid.sh > log.grid-rect.txt
sh test/test_calendar.sh > log.calendar.txt

sh test/check_log.sh

exit 0
