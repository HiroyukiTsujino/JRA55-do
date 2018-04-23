! -*-F90-*-
program main
  use have_test
  use fruit, only: init_fruit, set_unit_name, fruit_summary
  implicit none

  call ini_test
  call init_fruit
  call set_unit_name(trim(testname))
  call test_calc
  call fruit_summary

end program main
