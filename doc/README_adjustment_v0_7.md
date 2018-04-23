SURF_FLUX
========

  * Adjust/Calibrate JRA-55 data set to produce surface atmospheric dataset
    for driving Ocean-Sea ice models



Production of reference data set
--------




Adjustment method for each elements
--------

  * Temperature and Specific Humidity

  * Wind vector ( version 3 correction (v3) is used for wind)
    
  * Radiation

  * Precipitation



Reduced TL319 to Regular TL319 grid
--------



Global adjustment for downward fluxes
--------



Flat binary for MRI.COM input
--------

    * Create flat binary for MRI.COM
       -> for_mricom
          ./make_TL319.sh
          ./make_TL319_latestyear.sh

netCDF for OMIP
--------

    * Create netCDF for OMIP
       -> for_omip
          ./create_netcdf_v03.sh


Documents
--------

  * see doc directory


Development
--------

  * Developer: JMA Meteorological Research Institute
  * Contact: Hiroyuki Tsujino (JMA-MRI)
