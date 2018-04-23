! -*-F90-*-
program main
  use zave
  implicit none

  call ini
  do while ( has_next() )
    call calc
    call write_result
    call next
  enddo

end program main
