JRA55-do/README.md
========

  * Programs/scripts to adjust/calibrate the JRA-55 atmospheric field
    to produce a surface atmospheric dataset for driving Ocean-Sea ice models
    (JRA55-do; Tsujino et al. 2018 (in revision))

  * Contents

    - Introduction
    - System Requirements
    - Processing of the raw JRA-55 data
    - Processing CORE data
    - Processing runoff data
    - Produce figures used for the description paper
    - Low-level processing of reference datasets
    - Utilities
    - Evaluation
    - Producing input files to MRI.COM
    - Documents


Introduction
--------

  * This repository gathers programs/scripts to adjust/calibrate
    the raw JRA-55 atmospheric field (Kobayashi et al. 2015)
    to produce a surface atmospheric dataset for driving
    ocean-sea ice models (JRA55-do).

  * This repository contains no data. One may be able to reproduce
    the entire processing by starting from getting the raw JRA-55 dataset
    from an appropriate server (Stage 0).

  * Processes of producing the JRA55-do dataset can be divided in three stages.

    - Stage 1: Main adjustment to all variables of the raw JRA-55 data 
       on the reduced TL319 grid.

       - After the main adjustment, variables are interpolated
         from the reduced TL319 grid to the normal TL319 grid.

    - Stage 2: Additional adjustment to on the normal TL319 grid.

       - This includes the global flux closure by applying
         a globally uniform, time-invariant factor
         on radiation and precipitation.

    - Stage 3: Produce data files in publishable/usable formats.

       - In the 1st and 2nd stage of processing, 
         data files (in flat binary format) contain
         only one record of a single variable
         (e.g., file names for U-10m are u10m.YYYYMMDDHH).

       - In the final stage, data are gathered into one file
         for a single variable every year.

       - Using the annual files, the data files are converted
         to a netCDF format first, then arranged in CMOR format.


System Requirements
--------

  * Programs and scripts in this directory were developed in the following
    computation environments:

    - PGI fortran
    - GrADs
    - Bash
    - CMOR3.4.0 bundled in Anaconda2 (python 2.7)

  * "Setup.sh" is to set user dependent environments.


Processing of the JRA-55 raw data
--------

  * Updating the dataset to the latest (Stage 0)

    - update_to_latest   : Get latest files of JRA-55 and COBESST from a server
    - extract_jra55      : Convert grib to flat binary data format


  * Processing JRA-55 on reduced TL319 grid (native grid of JRA-55) (Stage 1)

    - jra55_org_grid_anl       : For various analytical processing
    - jra55_org_grid_calc_mean : Generate time series of mean data
    - jra55_org_grid_clim      : Compute climatology from mean data
    - jra55_org_grid_corrfac   : Compute adjustment factors
    - jra55_org_grid_calib     : Apply adjustment factors
    - jra55_org_grid_product   : Convert from reduced to normal TL319 grid
    - jra55_org_grid_windanom  : An attempt to calibrate mean as well as anomaly using QuikSCAT data
    - jra55_org_grid_monthly   : Process time series of the monthly JRA-55 product


  * Processing JRA-55 on normal TL319 grid (Stage 2)

    - jra55_topo	 : Edit Land-Sea masks of the native JRA-55
    - jra55_corrfac      : Compute adjustment factors
    - jra55_calib        : Apply adjustment factors
    - jra55_calc_mean    : Produce time series of averaged properties on JRA55 grid
    - jra55_clim	 : Calculate climatology on JRA55 grid
    - jra55_latlon_diag  : Diagnosis on JRA55 grid

  * Adjust precipitation on Mediterranean (Stage 2)

    - adjust_med_prcp    : adjust Mediterranean precipitation using CORE and GPCC


  * Evaluation using SST datasets (Stage 3)

    - jra55_on_cobesst     : Diagnosis of JRA55 on COBESST grid (1x1)
    - jra55flux_on_cobesst : Processing fluxes on COBESST grid
    - jra55surf_on_cobesst : Processing atmospheric variables on COBESST grid

    - jra55_on_cobemon         : Diagnosis of JRA55 on monthly COBESST (grid 1x1)
    - jra55flux_on_cobemon     : Processing fluxes on monthly COBESST (grid 1x1)
    - jra55_on_cobesst_monthly : process annual mean data of the above analysis

    - jra55_on_hurrellsst     : Diagnosis of JRA55 on Hurrell SST grid (1x1)
    - jra55flux_on_hurrellsst : Processing fluxes on Hurrell SST grid
    - jra55surf_on_hurrellsst : Processing atmospheric variables on Hurrell SST grid


  * Producing the dataset (Stage 4)

    - for_omip	       : Produce netCDF files in preparation for CMOR
    - for_omip_cmor    : CMORize data


Processing runoff data
--------

  * bamber_et_al_2018_grads : Process Greenland runoff data provided by Bamber et al. (2018)
                              (Stage 0)
  * runoff_mk_input         : Generate input file for CaMaFlood (river routing model)
                              (Stage 1)
  * runoff_imported         : Process imported runoff data to produce the dataset
                              (Stage 3)
  * runoff_omip_product     : Produce OMIP dataset
                              (Stage 3)
  * runoff_core             : Process CORE runoff
  * runoff                  : Analyze runoff data
  * runoff_to_model_grid    : Sample program to map runoff data to model grid


Produce figures used for the description paper
--------

  * GrADs scripts used for producing figures

    - description_paper

  * Mean fluxes divided in regions

    - compare_region_flux_v1_1 : version 1.1
    - compare_region_flux_v1_2 : version 1.2
    - compare_region_flux_v1_3 : version 1.3


Processing CORE data
--------

  * Basic processing

    - core_org_grid	    : Processing on T62 grid
    - core_to_jra55	    : Convert from T62 to TL319

  * Evaluation using SST datasets

    - core_on_cobesst     : Diagnosis of JRA55 on COBESST grid (1x1)
    - coreflux_on_cobesst : Processing fluxes on COBESST grid
    - coresurf_on_cobesst : Processing atmospheric variables on COBESST grid

    - core_on_hurrellsst     : Diagnosis of JRA55 on Hurrell SST grid (1x1)
    - coreflux_on_hurrellsst : Processing fluxes on Hurrell SST grid
    - coresurf_on_hurrellsst : Processing atmospheric variables on Hurrell SST grid


Low-level processing of reference datasets
--------

  * Atmospheric Reanalyses

    - erai_grads        : ERA-Interim
    - merra2_grads      : MERRA2
    - ncep1_grads       : NCEP-R1
    - ncep2_grads       : NCEP-R2
    - ncep_cfsr_grads   : NCEP-CFSR
    - 20CRv2_grads      : 20CR-v2
    - era40_grads       : ERA40

  * SST datasets

    - cobesst           : COBESST of Ishii et al. (2005)
    - hurrell_grads     : SST dataset of Hurrell et al. (2008) 
    - hadisst_grads     : HadISST

  * Wind datasets

    - quikscat_025_rss_read  : QuikSCAT provided by Remote Sensing Systems (REMSS)
    - quikscat_025_to_jra55  : Convert from 0.25x0.25 (QuikSCAT) to TL319 (JRA-55)
    - remss_wind_grads       : Remote Sensing Systems (REMSS) SSM/I wind speed
    - scow_grads             : Scatteromete Oceanic Wind of Risien and Chelton (2008)
    - quikscat_025_read	     : Obsolete QuikSCAT
    - quikscat_025	     : Obsolete QuikSCAT


  * Surface datasets
    - j-ofuro           : J-OFURO
    - oaflux_grads      : OAflux
    - nocs_grads        : NOCS
    - iabp_npoles_grads : IABP-NPOLES by Rigor et al. (2001)


  * Radiation datasets

    - isccp_grads       : ISCCP-FD
    - ceres_grads       : CERES-EBAF_SURF_Ed2.8
    - srb_grads         : Surface Radiation Budget (SRB) Release-3.0

  * Precipitation datasets

    - gpcc_grads        : GPCC
    - gpcp_grads        : GPCP
    - gpcp_to_cobe      : GPCP interpolation to COBESST grid (1x1)
    - gpcp_to_core      : GPCP interpolation to CORE grid (T62)

  * Buoys

    - tao_buoy    : TAO buoy array
    - whoi_buoy   : Wood Hole buoys
    - aoml_drifer : NOAA-AOML buoys
    - jkeo_buoy   : JKEO buoy

  * WOA13

    - woa13v2     : World Ocean Atlas 2013 version 2

  * GlobCurrent Product

    - glbcurrent_grads : Rio et al. (2014)

  * Simulations of global ocean model conducted by MRI

    - reference_simulation


Utilities
-------

  * anl	              : General purpose programs
  * grads             : User defined functions for GrADs
  * lib	              : Library to define and store the grid system
  * util	      : Utilities for miscellaneous processing
  * linkdir	      : Symbolic links to data where they actually exist
  * wavelet           : Wavelet analysis
  * filter_spector    : Compute response function of smoothing filters
  * coare-3_0         : COARE 3.0 bulk formula
  * shared_progs
     - neutral_to_actual : Sample program to convert from neutral to actual wind


Evaluation
--------

  * flux_compare  : Compare fluxes, draw figures

  * surf_compare  : Compare fluxes, draw figures
  * surf_compare_for_v0_3  : Compare fluxes, draw figures (v0.3)
  * surf_compare_for_v0_7  : Compare fluxes, draw figures (v0.7)
  * surf_compare_winds  : Compare fluxes, draw figures

  * compare_for_v1_1 : Evaluation of v1.1
  * analysis_v1_3    : Supplementary analysis on v1.3

  * comp_atmos    : compare formulae for computation of properties of moist air
  * comp_bulk     : compare bulk formulae

  * sverdrup_pacific : Compute a time series of monthly Sverdrup stream
          functions for the Pacific Ocean by solving a baroclinic Rossby
          wave model forced by monthly wind stress fields.

  * verify_rad    : verify radiation fluxes
  * verify_prcp   : verify precipitation
  * verify_tmpsph : verify temperature and specific humidity
  * verify_wind   : verify wind


Producing input files to MRI.COM
--------

  * for_mricom	     : Produce input files for MRI.COM
  * for_cyclic_model : Produce input files for MRI.COM with cyclic grid configulation


Documents
--------

  * See doc directory


Development
--------

  * Developer: JMA Meteorological Research Institute
  * Contact: Hiroyuki Tsujino (JMA-MRI)
