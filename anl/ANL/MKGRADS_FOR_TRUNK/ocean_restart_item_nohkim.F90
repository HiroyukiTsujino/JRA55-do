
  integer(4), parameter :: nitems = 11

  integer(4), parameter :: ndims(nitems) = (/&
       & km,km,km,km,0,0,0,km,km,km,km &
       & /)

  integer(4), parameter :: nuvts(nitems) = (/&
       & 1,1,0,0,0,1,1,0,0,0,0 &
       & /)

!         <-  9  -><3><----  
  character(len=256) :: varname(nitems) = (/ &
       & "u         km 99 Eastward Velocity  [cm/s]                            ", &
       & "v         km 99 Northward Velocity [cm/s]                            ", &
       & "t         km 99 Temperature [degC]                                   ", &
       & "s         km 99 Salinity [psu]                                       ", &
       & "h          0 99 Sea Surface Height [cm]                              ", &
       & "um         0 99 vertically integrated zonal transport [cm^2/s]       ", &
       & "vm         0 99 vertically integrated meridioal transport [cm^2/s]   ", &
       & "avd       km 99 vertical diffusivity for tracers [cm^2/s]            ", &
       & "avm       km 99 vertical viscosity for horizontal velocity [cm^2/s]  ", &
       & "avq       km 99 vertical diffusivity for turbulent K.E. [m^2/s]      ", &
       & "eb        km 99 turbulent kinetic energy [m^2/s^2]                   "  &
       & /)
