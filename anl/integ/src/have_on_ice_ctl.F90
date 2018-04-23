! -*-F90-*-
program main
  use have_on_ice
  implicit none

  call ini
  do while ( has_next() )
    call calc
    call next
  enddo
  call write_result

end program main
