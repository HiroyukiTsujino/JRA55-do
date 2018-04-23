! -*-F90-*-
program main
  use remap_test
  use fruit, only: init_fruit, set_unit_name, fruit_summary
  implicit none

  call ini_test
  call init_fruit
  call set_unit_name(trim(testname))
  call test_nlink
  call test_isrc
  call test_idst
  call fruit_summary

end program main
