grd2nc_g.F90
========

  * Convert GrADs to netCDF
     intended to be general but still ad hoc...

Documents
--------

  * Example of usage may be found at

       sample/hs_ice_nc.sh : time series data
              t_mask_nc.sh : single data

  * Program grd2nc_g.F90 assumes that a file contains all data for one year.
    For example, 12 records for monthly data.
    To create such data, use

       sample/m2_1yr.sh

Development
--------

  * Developer: JMA Meteorological Research Institute
  * Contact: Hiroyuki Tsujino (JMA-MRI)
