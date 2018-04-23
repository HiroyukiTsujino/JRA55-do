! -*-F90-*-
program main
  use integ_test
  use fruit, only: init_fruit, set_unit_name, fruit_summary
  implicit none

  call ini_test
  call init_fruit
  call set_unit_name(trim(testname))
  call test_area_t_2d
  call test_area_t_zero
  call test_area_t_one
  call test_area_u_2d
  call test_area_u_zero
  call test_area_u_one
  call test_vol_u
  call test_vert_t
  call test_section_u_zonal
  call fruit_summary

end program main
