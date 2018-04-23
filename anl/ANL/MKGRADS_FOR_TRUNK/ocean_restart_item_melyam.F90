
  integer(4), parameter :: nitems = 12

  integer(4), parameter :: ndims(nitems) = (/&
       & km,km,km,km,0,0,0,km,km,km,km,0 &
       & /)

  integer(4), parameter :: nuvts(nitems) = (/&
       & 1,1,0,0,0,1,1,0,0,0,0,0 &
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
       & "q         km 99 velocity scale of turbulent kinetic energy [m/s]     ", &
       & "alo        0 99 mixing length [m]                                    "  &
       & /)
