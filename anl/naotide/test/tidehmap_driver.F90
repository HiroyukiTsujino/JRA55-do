! -*-F90-*-
program main
  use tidehmap_test
  use fruit, only: init_fruit, set_unit_name, fruit_summary
  implicit none

  call init_fruit
  call set_unit_name(trim(testname))
  call read_answer
  call test_calc
  call fruit_summary

end program main
