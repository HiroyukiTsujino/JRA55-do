#!/bin/bash -f

set -e

ln -sf namelist.calib_satsph_v1_4 namelist.calib_satsph

./calib_tq10m_add_anom_interannual > tq10m_add_anom_v1_4.1958-1978.log
