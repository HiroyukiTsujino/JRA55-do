! -*-F90-*-
program main
  use fill_land
  implicit none
  integer(4) :: nrec_str, nrec_end, nrec

  call ini ( nrec_str, nrec_end )

  do nrec = nrec_str, nrec_end
    call read_data  (nrec)
    call modify_data  
    call write_data (nrec)
  end do

  call deallocate_arrays

end program main

