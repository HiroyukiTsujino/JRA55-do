! -*-F90-*-
program main
  use tidehmap
  implicit none

  call init
  do while ( has_next() )
    call calc
    call write_result
    call next
  enddo

end program main
