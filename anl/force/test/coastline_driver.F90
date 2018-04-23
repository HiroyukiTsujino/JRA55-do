! -*-F90-*-
program main
  use coastline_test
  use fruit, only: init_fruit, set_unit_name, fruit_summary
  implicit none

  call ini_test
  call init_fruit
  call set_unit_name(trim(testname))
  call test_seek_nearest_sea
  call test_seek_nearest_sea2
  call fruit_summary

end program main
