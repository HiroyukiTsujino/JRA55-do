! -*-F90-*-
program main
  use wlwl
  implicit none

  call ini
  do while ( has_next() )
    call calc
    call write_wlwl
    call write_w2
    call next
  enddo

end program main
