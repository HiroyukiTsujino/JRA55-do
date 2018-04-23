#!/bin/bash -f

set -e

ln -sf namelist.calibtmp10m_iceedge_e6 namelist.calibtmp10m_iceedge
./calib_tmp10m_iceedge_interannual

\rm namelist.calibtmp10m_iceedge
