
  integer(4), parameter :: nitems = 5

  integer(4), parameter :: ndims(nitems) = (/&
       & km,km,km,km,0 &
       & /)

  integer(4), parameter :: nuvts(nitems) = (/&
       & 1,1,0,0,0 &
       & /)

!         <-  9  -><3><----  
  character(len=256) :: varname(nitems) = (/ &
       & "u         km 99 Eastward Velocity  [cm/s]                            ", &
       & "v         km 99 Northward Velocity [cm/s]                            ", &
       & "t         km 99 Temperature [degC]                                   ", &
       & "s         km 99 Salinity [psu]                                       ", &
       & "h          0 99 Sea Surface Height [cm]                              "  &
       & /)
