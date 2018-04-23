! -*-F90-*-
program main
  use mean
  implicit none

  call ini
  call calc
  call write_result
  call write_ave

end program main
