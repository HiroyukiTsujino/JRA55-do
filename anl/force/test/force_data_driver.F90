! -*-F90-*-
program main
  use force_data_test
  use fruit, only: init_fruit, set_unit_name, fruit_summary
  implicit none

  call ini_test
  call init_fruit
  call set_unit_name(trim(testname))
  call test_lon
  call test_io_nm
  call test_read_data
  call fruit_summary

end program main
