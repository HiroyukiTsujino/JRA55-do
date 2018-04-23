! -*-F90-*-
!=================================================================
program calc_const_calib_prcp

  implicit none

  real(8) :: sublimation, precipitation, evaporation, river_runoff
  real(8) :: area_ocean

  real(8) :: calibration_factor

  logical :: l_sublim_integrated
  logical :: l_river_integrated

  real(8) :: sublimation_per_area, river_runoff_per_area

!-----------------------------------------------------------------

  namelist /nml_calib_prcp/ &
       &  sublimation, precipitation, &
       &  evaporation, river_runoff, &
       &  area_ocean, &
       &  l_sublim_integrated, &
       &  l_river_integrated

!-----------------------------------------------------------------

  l_sublim_integrated = .false.
  l_river_integrated  = .true.

  open (10,file='namelist.const_calib_prcp')
  read (10,nml_calib_prcp) 
  close(10)

  if (l_sublim_integrated) then
    sublimation_per_area = sublimation / area_ocean
  else
    sublimation_per_area = sublimation
  end if

  if (l_river_integrated) then
    river_runoff_per_area = river_runoff / area_ocean
  else
    river_runoff_per_area = river_runoff
  end if

  write(6,*) ' Sublimation   : ', sublimation_per_area
  write(6,*) ' Precipitation : ', precipitation
  write(6,*) ' Evaporation   : ', evaporation
  write(6,*) ' River run off : ', river_runoff_per_area

  calibration_factor = &
       & (evaporation + sublimation_per_area - river_runoff_per_area) &
       & / precipitation

  write(6,*) ' Calibration factor for precipitaion : ', calibration_factor

!-----------------------------------------------------------------

end program calc_const_calib_prcp
