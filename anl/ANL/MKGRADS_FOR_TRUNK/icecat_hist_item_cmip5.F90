  integer(4), parameter :: nitems = 41

  integer(4), parameter :: ndims(nitems) = (/&
       & 1,1,1,1,1,1,1,1,5,5, &
       & 5,5,5,5,5,1,1,1,1,1, &
       & 1,1,1,1,1,1,1,1,1,1, &
       & 1,1,1,1,1,1,1,1,1,1, &
       & 1 /)

  integer(4), parameter :: nuvts(nitems) = (/&
       & 0,0,0,1,1,0,0,0,0,0, &
       & 0,0,0,0,0,0,0,0,0,0, &
       & 0,0,0,0,0,0,0,0,0,0, &
       & 0,0,1,1,1,1,1,1,0,0, &
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
       & "tsice      0 99 Surface Temperature of Sea Ice [degC]                                      (CMIP5-P1)", &
       & "tsnint     0 99 Temperature at Interface Between Sea Ice and Snow [degC]                   (CMIP5-P1)", &
       & "prsn       0 99 Surface Snowfall Rate into the Sea Ice Portion of the Grid Cell [kg/m^2/s] (CMIP5-P1)", &
       & "grFrazil   0 99 Frazil Sea Ice Growth (Leads) Rate [kg/m^2/s]                              (CMIP5-P1)", &
       & "grCongel   0 99 Congelation Sea Ice Growth Rate [kg/m^2/s]                                 (CMIP5-P1)", &
       & "snoToIce   0 99 Snow-Ice Formation Rate [kg/m^2/s]                                         (CMIP5-P1)", &
       & "snomelt    0 99 Snow Melt Rate [kg/m^2/s]                                                  (CMIP5-P1)", &
       & "tmelt      0 99 Rate of Melt at Upper Surface of Sea Ice [kg/m^2/s]                        (CMIP5-P1)", &
       & "bmelt      0 99 Rate of Melt at Sea Ice Base [kg/m^2/s]                                    (CMIP5-P1)", &
       & "hcice      0 99 Sea Ice Total Heat Content [J]                                             (CMIP5-P2)", &
       & "rsdssi     0 99 Downward Shortwave over Sea Ice [W/m^2]                                    (CMIP5-P1)", &
       & "rsussi     0 99 Upward Shortwave over Sea Ice [W/m^2]                                      (CMIP5-P1)", &
       & "rldssi     0 99 Downward Longwave over Sea Ice [W/m^2]                                     (CMIP5-P2)", &
       & "rlussi     0 99 Upward Longwave over Sea Ice [W/m^2]                                       (CMIP5-P2)", &
       & "hfssi      0 99 Surface Upward Sensible Heat Flux over Sea Ice [W/m^2]                     (CMIP5-P2)", &
       & "hflssi     0 99 Surface Upward Latent Heat Flux over Sea Ice [W/m^2]                       (CMIP5-P2)", &
       & "sblsi      0 99 Sublimation over Sea Ice [kg/m^2/s]                                        (CMIP5-P2)", &
       & "transix    0 99 Eastward Sea Ice Transport [kg/s]                                          (CMIP5-P1)", &
       & "transiy    0 99 Northward Sea Ice Transport [kg/s]                                         (CMIP5-P1)", &
       & "strairx    0 99 Eastward Atmospheric Stress On Sea Ice [N/m^2]                             (CMIP5-P2)", &
       & "strairy    0 99 Northward Atmospheric Stress On Sea Ice [N/m^2]                            (CMIP5-P2)", &
       & "strocnx    0 99 Eastward Ocean Stress On Sea Ice [N/m^2]                                   (CMIP5-P2)", &
       & "strocny    0 99 Northward Ocean Stress On Sea Ice [N/m^2]                                  (CMIP5-P2)", &
       & "streng     0 99 Compressive Sea Ice Strength [N/m]                                         (CMIP5-P2)", &
       & "divice     0 99 Strain Rate Divergence of Sea Ice [1/s]                                    (CMIP5-P2)", &
       & "ridgice    0 99 Sea Ice Ridging Rate [1/s]                                                 (CMIP5-P2)", &
       & "sim        0 99 Sea Icea plus Surface Snow Amount [kg/m/m]                                 (CMIP5-P1)"  &
       & /)
