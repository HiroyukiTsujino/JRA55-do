! -*-F90-*-
program main
  use libmxe_calendar_test
  use fruit, only: init_fruit, set_unit_name, fruit_summary
  implicit none

  call init_fruit
  call set_unit_name(trim(testname))
  call test_addmonth_plus_0
  call test_addmonth_plus_1
  call test_addmonth_plus_14
  call test_addmonth_minus_1
  call test_l_leap_year
  call test_diffsec
  call test_addcal
  call fruit_summary

end program main
