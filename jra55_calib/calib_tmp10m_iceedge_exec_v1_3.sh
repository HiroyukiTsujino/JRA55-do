#!/bin/bash -f

set -e

rm -f namelist.calibtmp10m_iceedge

ln -sf namelist.calibtmp10m_iceedge_v1_3 namelist.calibtmp10m_iceedge
./calib_tmp10m_iceedge_mon_interannual

rm -f namelist.calibtmp10m_iceedge
