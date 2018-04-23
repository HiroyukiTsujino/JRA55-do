  integer(4), parameter :: nitems = 41

  integer(4), parameter :: ndims(nitems) = (/&
       & 1,1,1,1,1,1,1,1,5,5, &
       & 5,5,5,5,5,5,5,5,5,5, &
       & 5,5,5,5,5,5,5,5,5,5, &
       & 5,5,5,5,5,5,5,1,1,1, &
       & 1 /)

  integer(4), parameter :: nuvts(nitems) = (/&
       & 0,0,0,1,1,0,0,0,0,0, &
       & 0,0,0,0,0,0,0,0,0,0, &
       & 0,0,0,0,0,0,0,0,0,0, &
       & 0,0,0,0,0,0,0,0,0,0, &
       & 0 /)

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
       & "ssws       5 99 Net short wave on snow surface [W/m^2]                                               ", &
       & "sswi       5 99 Net short wave on ice surface [W/m^2]                                                ", &
       & "sswb       5 99 Net short wave on ocean surface [W/m^2]                                              ", &
       & "slws       5 99 Net long wave on snow surface [W/m^2]                                                ", &
       & "slas       5 99 latent heat on snow surface [W/m^2]                                                  ", &
       & "ssns       5 99 sensible heat on snow surface [W/m^2]                                                ", &
       & "shiu       5 99 heat flux in upper ice [W/m^2]                                                       ", &
       & "shid       5 99 heat flux in lower ice [W/m^2]                                                       ", &
       & "shio       5 99 heat flux on 1st layer of OGCM [W/m^2]                                               ", &
       & "smlti      5 99 ice melt at upper surface [m/s of water]                                             ", &
       & "smlts      5 99 snow melt at upper surface [m/s of water]                                            ", &
       & "smfii      5 99 ice melt in ice interior [m/s of water]                                              ", &
       & "smfib      5 99 ice melt at bottom ice [m/s of water]                                                ", &
       & "srmpi      5 99 ice volume change by remapping [m/s of water]                                        ", &
       & "srmps      5 99 snow volume change by remapping [m/s of water]                                       ", &
       & "srdgi      5 99 ice volume change by ridging [m/s of water]                                          ", &
       & "srdgs      5 99 snow volume change by ridging [m/s of water]                                         ", &
       & "sadvi      5 99 ice volume change by advection [m/s of water]                                        ", &
       & "sadvs      5 99 snow volume change by advection [m/s of water]                                       ", &
       & "sws2i      5 99 snow to ice change [m/s of water]                                                    ", &
       & "sadji      5 99 ice volume change by adjustment [m/s of water]                                       ", &
       & "sadjs      5 99 snow volume change by adjustment [m/s of water]                                      ", &
       & "sswl       0 99 short wave on open water [W/m^2]                                                     ", & 
       & "sotl       0 99 heat flux on open water [W/m^2]                                                      ", & 
       & "shao       0 99 heat flux on 1st layer of OGCM  [W/m^2]                                              ", & 
       & "sfrzl      0 99 freezing in open water [m/s of water]                                                ", &
       & "sim        0 99 Sea Icea plus Surface Snow Amount [kg/m/m]                                 (CMIP5-P1)"  & 
       & /)
