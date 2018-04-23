SURF_FLUX
========

  * Adjust/Calibrate JRA-55 data set to produce surface atmospheric dataset
    for driving Ocean-Sea ice models

  * The suite of procedures are named "e"-series.


Adjustment method for each elements
--------

  * Wind vector ( version 3 correction (v3) is used for wind)
    
    1. Produce daily QuikSCAT data set 

      -> quikscat_025_rss_read

    2. Produce daily data set from 6-hourly JRA-55 anl_surf and fcst_surf
       
      -> jra55_org_grid_calc_mean

        - make_daily_anl_surf_exec.sh
            make_org_to_daily_surf.F90
            namelist.make_daily_anl_surf_template (edit for this purpose)
            items:
              u10m, v10m, (swind), tmp2m, sph2m (from anl_surf)

        - make_daily_fcst_surf_tmp2m.sh (edit for brtmp, slprs, ice)
            make_org_to_daily_surf.F90
            namelist.make_daily_fcst_surf_template
            items:
              brtmp, slprs, ice (from fcst_surf)


    3. Produce daily data set from a blend of QuikSCAT and JRA-55 anl_surf

      -> jra55_org_grid_anl

        - blend_quikscat_anl_exec.sh
            blend_quikscat_anl.F90
            namelist.blend_quikscat_anl_template


    4. Produce monthly data set from the daily blend data

      -> jra55_org_grid_calc_mean

        - make_day2mon_blend_wind.sh
            make_daily_to_monthly.F90
            namelist.make_daily2monthly_blend_[swind,u10m,v10m]


    5. Apply 1-2-1 filter to JRA-55 fcst_surf

      -> jra55_org_grid_calib

        - filter_wind_interannual_exec.sh
            filter_wind_interannual.F90
            namelist.filterwind


    6. Produce daily and then monthly data of filtered JRA-55 fcst_surf wind

      -> jra55_org_grid_calc_mean

        - make_daily_fcst_e1_exec.sh
            make_org_to_daily_surf.F90
            namelist.make_daily_fcst_e1_template

        - make_day2mon_fcst_e1_wind.sh
            make_daily_to_monthly.F90
            namelist_daily2monthly_fcst_e1_template


    7. Produce monthly climatology of blend and filtered JRA-55

      -> jra55_org_grid_clim

        - make_monclim_blend_wind.sh
            mkmonclim.F90
            namelist.make_monclim_blend_[swind,u10m,v10m]

        - make_monclim_fcst_e1_wind.sh
            mkmonclim.F90
            namelist.make_monclim_fcst_e1_[swind,u10m,v10m]


    8. Determine correction factor

      -> jra55_org_grid_corrfac

        - make_correc_wind_exec.sh
            mk_correc_wvec_ceof_mon.F90
             (namelist.windcorrec_ceof_mon)
            mk_correc_wvec_tanh_mon.F90
             (namelist.windcorrec_tanh_mon)

    9. Apply correction factor

      -> jra55_org_grid_calib

        - calib_wind_interannual_exec.sh
            calib_wind_interannual.F90
            namelist.calibwind

  * Radiation

    1. Produce Reference Data on the basis of CERES-EBAF

      -> ceres_grads

        - make_clim_ceres.sh
            mkmonclim.F90
            namelist.monthly_clim_ceres_[dswrf,dlwrf]

        - reduce_swdn_clim_exec.sh
            reduce_swdn_clim.F90
            namelist.reduce_ceres_swdn

       ##### Long wave of CERES should be filtered around 60S #####

    2. Produce monthly data of JRA-55 fcst_phy2m

      -> jra55_org_grid_calc_mean

        - make_monthly_rad_org.sh
            make_org_to_monthly_flux.F90
            namelist_org2monthly_flux_org_template
        
    3. Produce monthly climatology of JRA-55 fcst_phy2m

      -> jra55_org_grid_clim

        - make_monclim_fcst_rad.sh
            mkmonclim.F90
            namelist.make_monclim_fcst_[dswrf,dlwrf]


    4. Determine correction factor

      -> jra55_org_grid_corrfac

        - make_correc_rad_exec.sh
            mk_correc_rad.F90
            namelist.make_[dswrf,dlwrf]_correc

    5. Apply correction factor

      -> jra55_org_grid_calib

        - calib_rad_interannual_exec.sh
            calib_rad_interannual.F90
            namelist.calibrad.monthly


  * Precipitation

    1. Produce Reference Data on the basis of CORE

      -> core_org_grid

        - mkmonclim.F90
          ioinfmncl.precip.dat

    2. Produce monthly data of JRA-55 fcst_phy2m

      -> jra55_org_grid_calc_mean

        - make_monthly_precip_org.sh
            make_org_to_monthly_flux.F90
            namelist_org2monthly_flux_org_template
        
    3. Produce monthly climatology of JRA-55 fcst_phy2m

      -> jra55_org_grid_clim

        - make_monclim_fcst_precip.sh
            mkmonclim.F90
            namelist.make_monclim_fcst_precip


    4. Determine correction factor

      -> jra55_org_grid_corrfac

        - make_correc_precip_exec.sh
            mk_correc_precip.F90
            namelist.make_precip_correc


    5. Apply correction factor

      -> jra55_org_grid_calib

        - calib_prcp_snow_interannual_exec.sh
            calib_precip_interannual.F90
            namelist.calibpcp.prcp_monthly
            namelist.calibpcp.snow_monthly


  * Temperature and Specific Humidity

    1. Produce monthly IABP-NPOLES data set 

      -> iabp_npoles_grads


    2. Produce daily and then monthly tmp2m and sph2m from 6-hourly JRA-55 anl_surf
       
      -> jra55_org_grid_calc_mean

        - make_daily_anl_surf_exec.sh
            make_org_to_daily_surf.F90
            namelist.make_daily_anl_surf_template (edit for this purpose)
            items:
              tmp2m, sph2m

        - make_day2mon_anl_[tmp2m,sph2m].sh
            make_daily_to_monthly.F90
            namelist.make_daily2monthly_template


    3. Produce daily and then monthly tmp2m and ice from 3-hourly JRA-55 fcst_surf
       
      -> jra55_org_grid_calc_mean

        - make_daily_fcst_surf_tmp2m.sh (edit for tmp2m and ice)
            make_org_to_daily_surf.F90
            namelist.make_daily_fcst_surf_template
            items:
              tmp2m, ice

        - make_day2mon_fcst_tmp_ice.sh
            make_daily_to_monthly.F90
            namelist_daily2monthly_fcst_template


    4. Produce monthly reference tmp2m data
       from a blend of IABP-NPOLES and JRA-55 anl_surf

      -> jra55_org_grid_anl

        - blend_iabp_nples_anl_exec.sh
            blend_iabp_nples_anl.F90
            namelist.blend_iabp_anl_template


    5. Produce monthly climatology of blend and fcst tmp2m (on ice and other surfaces)

      -> jra55_org_grid_clim

        - make_monclim_on_ice.sh
            mkmonclim_on_ice.F90
            namelist.make_monclim_on_ice_[blend,fcst]


    6. Determine correction factor for tmp2m

      -> jra55_org_grid_corrfac

        - make_correc_tmp2m_exec.sh
            mk_correc_tmp2m.F90
            namelist.make_tmp2m_[ice,all]_correc


    7. Apply correction factor for tmp2m
        and adjust sph2m by keeping relative humidity

      -> jra55_org_grid_calib

        - calib_tmp2m_interannual_exec.sh
            calib_tmp2m_interannual.F90
            namelist.calibsat

    8. Produce daily and then monthly sph2m
       from adjusted 3-hourly fcst_surf_e2
       
      -> jra55_org_grid_calc_mean

        - make_daily_fcst_e2_exec.sh
            make_org_to_daily_surf.F90
            namelist.make_daily_fcst_e2_template

        - make_day2mon_fcst_e2_sph2m.sh
            make_daily_to_monthly.F90
            namelist_daily2monthly_fcst_e2_template


    9. Produce monthly climatology of anl_surf and fcst_e2 sph2m

      -> jra55_org_grid_clim

        - make_monclim_anl_sph2m.sh
            mkmonclim.F90
            namelist.make_monclim_anl_sph2m

        - make_monclim_fcst_e2_sph2m.sh
            mkmonclim.F90
            namelist.make_monclim_fcst_e2_sph2m
            

   10. Determine correction factor

      -> jra55_org_grid_corrfac

        - make_correc_sph2m_exec.sh
            mk_correc_sph2m.F90
            namelist.make_sph2m_correc


   11. Apply Correction factor for sph2m

      -> jra55_org_grid_calib

        - calib_sph2m_interannual_exec.sh
            calib_sph2m_interannual.F90
            namelist.calibsph


   12. Shift height from 2 m to 10 m

      -> jra55_org_grid_produce

        - produce_tmp_sph_10m_exec.sh
            produce_tmp_sph_10m.F90
            namelist.tmp_sph_10m_template



Reduced TL319 to Regular TL319 grid
--------

    -> jra55_org_grid_produce

       ./make_tables.sh
       ./red2reg_exec_e[0,2,3].sh
       ./red2reg_exec_wind.sh     (u10m, v10m)
       ./red2reg_exec_tmp_sph.sh  (tmp10m, sph10m)
       ./red2reg_exec_rad.sh      (dswrf, dlwrf)
       ./red2reg_exec_precip.sh   (prcp, snow)


Global adjustment for downward fluxes
--------

    1. Evaluate monthly heat/fresh water fluxes on COBESST grid
       -> jra55_on_cobesst
          ./exec_diag_bulk.sh (namelist.diagflux.e3)

    2. Evaluate annual mean and global mean fluxes
       -> jra55flux_on_cobesst
          ./m2a.sh
          ./have_[nswrf,nlwrf,latent,sensible,precip,evapor]_ocean.sh

    3. determine globally constant factor
       -> jra55_calib
          ./calc_const_calib_prcp (namelist.const_calib_prcp_e3)
              namelist.const_calib_rad_e3
          ./calc_const_calib_rad  (namelist.const_calib_rad_e3)
              namelist.const_calib_rad_e3

    4. Apply factor
       -> jra55_calib
          ./calib_rad_const_exec.sh
             namelist.calibrad.const_e3
          ./calib_prcp_snow_const_exec.sh
             namelist.calibpcp.const_e3
             namelist.calibpcp.snow_const_e3

    5. Create rain fall data

       -> jra55_calib
          ./create_rain.sh


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
