SURF_FLUX/scow_grads
========

This directory contains shell srcipts/Fortran programs to process
 Scatterometer Climatology of Ocean Wind (SCOW) (Risien and Chelton 2008).

  1. Compute time series of monthly data in GrADS format

  2. Compute zonal mean


Programs/Scripts
--------

  * Create monthly data
     Program:  src/nc2grads_scow.F90
     Namelist: namelist.scow_wind

  * Zonal mean
     Script:   scow_monthly_zm.sh
     Program:  src/zonal_mean.F90
     Namelist: namelist.zonalmean_template


Development
--------

  * Developed at: Oceanography and Geochemistry Research Department,
                  Meteorological Research Institute,
                  Japan Meteorological Agency
  * Contact: Hiroyuki Tsujino (htsujino@mri-jma.go.jp)
