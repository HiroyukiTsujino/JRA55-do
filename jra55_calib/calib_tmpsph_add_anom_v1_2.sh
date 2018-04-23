#!/bin/bash -f

set -e

ln -sf namelist.calib_satsph_v1_2 namelist.calib_satsph

./calib_tq10m_add_anom_interannual > tq10m_add_anom_v1_2.1958-1978.log

#./calib_tq10m_add_anom_interannual > tq10m_add_anom.1958-1958.log
