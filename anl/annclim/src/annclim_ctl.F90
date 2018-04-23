! -*-F90-*-
program main
  use annclim
  implicit none

  call ini
  do while ( has_next() )
    call calc
    call next
  end do
  call write_result

end program main
