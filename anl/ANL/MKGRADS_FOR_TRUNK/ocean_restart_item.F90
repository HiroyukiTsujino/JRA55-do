
  integer(4), parameter :: nitems = 7

  integer(4), parameter :: ndims(nitems) = (/&
       & km,km,km,km,0,0,0 &
       & /)

  integer(4), parameter :: nuvts(nitems) = (/&
       & 1,1,0,0,0,1,1 &
       & /)

!         <-  9  -><3><----  
  character(len=256) :: varname(nitems) = (/ &
       & "u         km 99 Eastward Velocity  [cm/s]                            ", &
       & "v         km 99 Northward Velocity [cm/s]                            ", &
       & "t         km 99 Temperature [degC]                                   ", &
       & "s         km 99 Salinity [psu]                                       ", &
       & "h          0 99 Sea Surface Height [cm]                              ", &
       & "um         0 99 vertically integrated zonal transport [cm^2/s]       ", &
       & "vm         0 99 vertically integrated meridioal transport [cm^2/s]   "  &
       & /)
