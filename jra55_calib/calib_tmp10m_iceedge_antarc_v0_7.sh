#!/bin/bash -f

set -e

./calib_tmp10m_iceedge_exec_v0_7.sh > tmp10m_iceedge_1958-present.log

./calib_tmp10m_antarc_exec_v0_7.sh  > tmp10m_antarc_1958-present.log
