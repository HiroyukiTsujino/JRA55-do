! -*-F90-*-
program main
  use libmxe_topo_test
  use fruit, only: init_fruit, set_unit_name, fruit_summary
  implicit none

  call ini_test
  call init_fruit
  call set_unit_name(trim(testname))
  call test_ho4
  call test_exnn
  call test_depth_t_cm
!  call write_topo_grads
  call fruit_summary

end program main
