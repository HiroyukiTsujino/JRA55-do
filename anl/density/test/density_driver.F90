! -*-F90-*-
program main
  use density_test
  use fruit, only: init_fruit, set_unit_name, fruit_summary
  implicit none

  call init_fruit
  call set_unit_name(trim(testname))
  call test_dens
  call fruit_summary

end program main
