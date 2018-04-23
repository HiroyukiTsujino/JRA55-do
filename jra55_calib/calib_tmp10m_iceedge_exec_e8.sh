#!/bin/bash -f

set -e

ln -sf namelist.calibtmp10m_iceedge_e8 namelist.calibtmp10m_iceedge
./calib_tmp10m_iceedge_had_interannual

\rm namelist.calibtmp10m_iceedge
