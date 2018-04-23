#!/bin/bash -f

set -e

rm -f namelist.calibtmp10m_antarc

ln -sf namelist.calibtmp10m_antarc_v1_3 namelist.calibtmp10m_antarc
./calib_tmp10m_antarc_interannual

rm -f namelist.calibtmp10m_antarc
