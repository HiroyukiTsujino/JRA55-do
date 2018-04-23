! -*-F90-*-
program main
  use energy_test
  use fruit, only: init_fruit, set_unit_name, fruit_summary
  implicit none

  call ini_test
  call init_fruit
  call set_unit_name(trim(testname))
  call test_w
  call test_px
  call test_py
  call test_nablap
  call fruit_summary

end program main
