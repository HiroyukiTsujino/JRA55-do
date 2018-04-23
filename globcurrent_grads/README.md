SURF_FLUX/globcurrent_grads
========

This directory contains shell srcipts/Fortran programs
 to process GlobCurrent 0.25x0.25 data.

Document
--------

  - src/nc2grads_globcurrent.F90

       convert raw netCDF data to grads

  - h_intpol_globcurrent2jra.sh (src/globcurrent_to_jra.F90)

       After the first interpolation, a missing grid is filled by
       using surrounding points if at least 3 surrounding points are valid.

Development
--------

  * Developed at: Oceanographical and Geochemincal Research Department,
                  Meteorological Research Institute,
                  Japan Meteorological Agency
  * Contact: Hiroyuki Tsujino (htsujino@mri-jma.go.jp)
