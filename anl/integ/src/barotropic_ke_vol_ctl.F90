! -*-F90-*-
program main
  use barotropic_ke_vol
  implicit none

  call ini
  do while ( has_next() )
    call calc
    call next
  enddo
  call write_result

end program main
