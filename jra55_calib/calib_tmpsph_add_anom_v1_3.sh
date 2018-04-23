#!/bin/bash -f

set -e

ln -sf namelist.calib_satsph_v1_3 namelist.calib_satsph

./calib_tq10m_add_anom_interannual > tq10m_add_anom_v1_3.1958-1978.log
