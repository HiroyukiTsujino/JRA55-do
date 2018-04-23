  integer(4), parameter :: nitems = 15

  integer(4), parameter :: ndims(nitems) = (/&
       & 1,1,1,1,1,1,1,1,5,5, &
       & 5,5,5,5,5 &
       & /)

  integer(4), parameter :: nuvts(nitems) = (/&
       & 0,0,0,1,1,0,0,0,0,0, &
       & 0,0,0,0,0 &
       & /)

!         <-  9  -><3><----  
  character(len=256) :: varname(nitems+1) = (/ &
       & "sit        0 99 Sea Ice Thickness [m]                                                      (CMIP5-P1)", &
       & "snd        0 99 Snow Depth [m]                                                             (CMIP5-P1)", &
       & "sic        0 99 Sea Ice Area Fraction [%]                                                  (CMIP5-P1)", &
       & "uice       0 99 Sea Ice Eastward Drifting Velocity  [m/s]                                            ", &
       & "vice       0 99 Sea Ice Northward Drifting Velocity [m/s]                                            ", &
       & "aopen      0 99 Open Water Area Fraction [%]                                                         ", &
       & "t0icel     0 99 Open Water Surface Skin Temperature [degC]                                           ", &
       & "s0icel     0 99 Open Water Surface Skin Salinity [psu]                                               ", &
       & "hi         5 99 Mean Ice Thickness in Each Category [m]                                              ", &
       & "hs         5 99 Mean Snow Thickness in Each Category [m]                                             ", &
       & "ar         5 99 Area Fraction of Each Category [%]                                                   ", &
       & "t3ice      5 99 Surface Temperature of Each Category [degC]                                          ", &
       & "t1ice      5 99 Interior Temperature of Each Category [degC]                                         ", &
       & "t0ice      5 99 Bottom Temperature of Each Category [degC]                                           ", &
       & "s0ice      5 99 Bottom Salinity of Each Category [degC]                                              ", &
       & "sim        0 99 Sea Icea plus Surface Snow Amount [kg/m/m]                                 (CMIP5-P1)"  &
       & /)
