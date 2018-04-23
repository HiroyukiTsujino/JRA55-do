#!/bin/bash -f

set -e

ln -sf namelist.calibtmp10m_iceedge_e7 namelist.calibtmp10m_iceedge
./calib_tmp10m_iceedge_mon_interannual

\rm namelist.calibtmp10m_iceedge
