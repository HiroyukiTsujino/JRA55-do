
  integer(4), parameter :: nitems = 19

  integer(4), parameter :: ndims(nitems) = (/&
       & ncat+1,ncat+1,ncat+1,ncat+1,ncat+1, &
       & ncat+1,ncat+1,ncat+1,ncat+1,1, &
       & 1,1,1,1,1,1,1,1,1 &
       & /)

  integer(4), parameter :: nuvts(nitems) = (/&
       & 0,0,0,0,0,0,0,0,0,0, &
       & 0,0,0,0,1,1,0,0,0 &
       & /)

!         <-  9  -><3><----  
  character(len=256) :: varname(nitems) = (/ &
       & "ar         6 99 Area Fraction of Each Category [%]                                                   ", &
       & "hi         6 99 Mean Ice Thickness in Each Category [m]                                              ", &
       & "sn         6 99 Mean Snow Thickness in Each Category [m]                                             ", &
       & "hh         6 99 Ice Thickness in Each Category [m]    f                                              ", &
       & "ss         6 99 Snow Thickness in Each Category [m]                                                  ", &
       & "ts         6 99 Surface Temperature of Each Category [degC]                                          ", &
       & "t1         6 99 Interior Temperature of Each Category [degC]                                         ", &
       & "tb         6 99 Bottom Temperature of Each Category [degC]                                           ", &
       & "sb         6 99 Bottom Salinity of Each Category [degC]                                              ", &
       & "ara        0 99 Sea Ice Area Fraction [%]                                                            ", &
       & "hia        0 99 Grid average Sea Ice Thickness [m]                                                   ", &
       & "ssa        0 99 Grid average snow depth [m]                                                          ", &
       & "hha        0 99 Category Average Sea Ice Thickness [m]                                               ", &
       & "sha        0 99 Category Average Snow Depth [m]                                                      ", &
       & "ui         0 99 Sea Ice Eastward Drifting Velocity  [m/s]                                            ", &
       & "vi         0 99 Sea Ice Northward Drifting Velocity [m/s]                                            ", &
       & "sigma1     0 99 Stress tensor 11 component [N/m^2]                                                   ", &
       & "sigma2     0 99 Stress tensor 22 component [N/m^2]                                                   ", &
       & "sigma3     0 99 Stress tensor 12(21) component [N/m^2]                                               "  &
       & /)
