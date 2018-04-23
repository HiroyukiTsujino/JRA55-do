! -*-F90-*-
program main
  use gradient_test
  use fruit, only: init_fruit, set_unit_name, fruit_summary
  implicit none

  call ini_test
  call init_fruit
  call set_unit_name(trim(testname))
  call test_h2d_dtdxu
  call test_h2d_dtdxu_zero
  call test_h2d_dtdxu_one
  call test_h2d_dudxu
  call test_h2d_dudyu
  call fruit_summary

end program main
