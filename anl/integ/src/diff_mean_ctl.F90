! -*-F90-*-
program main
  use diff_mean
  implicit none

  call ini
  call calc
  call write_result
  call write_ave

end program main
