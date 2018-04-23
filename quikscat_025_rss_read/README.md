SURF_FLUX/quikscat_025_rss_read
========

This directory contains shell srcipts/Fortran programs to process
 Remote Sensing Systems QuikSCAT Ku-2011 daily Ocean Vector Winds
 on 0.25 deg grid, Version 4 (Riciardulli et al. 2011).

  1. Read QuikSCAT 0.25x0.25 data and output daily GrADs file

  2. Compute time series of monthly data

  3. Compute zonal mean


Programs/Scripts
--------
 
  * Create daily data
     Script:   qscat_daily_grads.sh
     Program:  src/qscat_rss_to_daily.F90
     Namelist: namelist.qscat_daily_template

  * Create monthly data
     Script:   make_day2mon_qscat.sh
     Program:  src/daily2monthly.F90
     Namelist: namelist.daily2monthly_[loose,strict]

  * Zonal mean
     Script:   qscat_monthly_zm.sh
     Program:  src/zonal_mean.F90
     Namelist: namelist.zonalmean_template


Development
--------

  * Developed at: Oceanography and Geochemistry Research Department,
                  Meteorological Research Institute,
                  Japan Meteorological Agency
  * Contact: Hiroyuki Tsujino (htsujino@mri-jma.go.jp)
