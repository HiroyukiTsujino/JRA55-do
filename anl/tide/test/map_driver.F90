! -*-F90-*-
program main
  use map_test
  use fruit, only: init_fruit, set_unit_name, fruit_summary
  implicit none

  call ini_test
  call init_fruit
  call set_unit_name(trim(testname))
  call test_seek_nearest_sea_igrid
  call fruit_summary

end program main
