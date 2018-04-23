#!/bin/bash

set -e

ln -sf test/rect/namelist.test .

driver=diff_driver
make ${driver}
./${driver} <<EOF
  &diff_lst
    file_namelist1 = "test/rect/NAMELIST.MXE-snp",
    file_namelist2 = "test/rect/NAMELIST.MXE",
    file_base1     = "../../linkdir/mxe/rect/snp/hs_snp_ssh",
    file_base2     = "../../linkdir/mxe/rect/hst/hs_ssh",
    diro           = "../../linkdir/mxe/temp",
    fileo          = "hs_ssh_anom",
    l2d            = .true.,
    cgrid          = "T",
    nstr           = 25,
    nstr2          = 2,
    nstep_change_record2 = 24,
  /
EOF
#-- 1: hourly snap record, 2: daily average record
#-     (1901/01/02 1:00)      (1901/01/02)

exit 0
