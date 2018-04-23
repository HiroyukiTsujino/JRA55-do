! -*-F90-*-
program main
  use regrid_test
  use fruit, only: init_fruit, set_unit_name, fruit_summary
  implicit none

  call ini_test
  call init_fruit
  call set_unit_name(trim(testname))
  call test_h2d_t2u
  call test_h2d_t2u_zero
  call test_h2d_t2u_one
  call fruit_summary

end program main
