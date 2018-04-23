#!/bin/bash -f

set -e

ln -sf namelist.calibtmp10m_antarc_e5 namelist.calibtmp10m_antarc
./calib_tmp10m_antarc_interannual

\rm namelist.calibtmp10m_antarc
