! -*-F90-*-
program rivermouth_ctl
  use rivermouth

  call ini_OGCM
  call ini_river
  call main
  call write_result

end program rivermouth_ctl
