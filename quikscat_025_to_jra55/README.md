SURF_FLUX/quikscat_025_to_jra55
========

This directory contains shell srcipts/Fortran programs
 to interpolate QuikSCAT 0.25x0.25 monthly data 
 to JRA-55 grid

Document
--------

  - h_intpol_qstat2jra.sh (src/qscat_to_jra.F90)

       JRA-55 grid data is valid only when all four corner points
       of QuikSCAT are available

  - h_intpol_qscat2jra_v2.sh (src/qscat_to_jra_v2.F90)

       After the first interpolation, a missing grid is filled by
       using surrounding points if at least 4 out of 8 surrounding
       points have valid.


Development
--------

  * Developed at: Oceanographical and Geochemincal Research Department,
                  Meteorological Research Institute,
                  Japan Meteorological Agency
  * Contact: Hiroyuki Tsujino (htsujino@mri-jma.go.jp)
