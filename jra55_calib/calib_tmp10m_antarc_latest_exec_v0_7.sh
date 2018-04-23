#!/bin/bash -f

set -e

rm -f namelist.calibtmp10m_antarc

ln -sf namelist.calibtmp10m_antarc_latest_v0_7 namelist.calibtmp10m_antarc
./calib_tmp10m_antarc_interannual

rm -f namelist.calibtmp10m_antarc
