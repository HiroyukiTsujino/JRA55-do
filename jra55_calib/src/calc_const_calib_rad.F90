! -*-F90-*-
!=================================================================
program calc_const_calib_radiation

  implicit none

  real(8) :: total_shortwave, downward_longwave
  real(8) :: total_water_flux, total_ice_thermodynamics
  real(8) :: total_other, residual
  real(8) :: area_ocean

  real(8) :: calibration_factor

!-----------------------------------------------------------------

  namelist /nml_calib_rad/ &
       &  total_shortwave, downward_longwave, &
       &  total_water_flux, total_ice_thermodynamics, &
       &  total_other, &
       &  area_ocean

!-----------------------------------------------------------------

  open (10,file='namelist.const_calib_rad')
  read (10,nml_calib_rad) 
  close(10)

  write(6,*) ' Heat flux due to SW+LW+LA+SE : ', total_other
  write(6,*) ' Heat flux due to water flux  : ', total_water_flux
  write(6,*) ' Heat flux due to sea ice     : ', total_ice_thermodynamics

  residual = total_other + total_ice_thermodynamics + total_water_flux

  write(6,*) ' Residual Heat flux    : ', residual
  write(6,*) ' Total short wave      : ', total_shortwave
  write(6,*) ' Downward long wave    : ', downward_longwave

  calibration_factor = &
       & (total_shortwave + downward_longwave - residual) &
       & / (total_shortwave + downward_longwave)

  write(6,*) ' Calibration factor for downward radiation : ',&
       & calibration_factor

!-----------------------------------------------------------------

end program calc_const_calib_radiation
