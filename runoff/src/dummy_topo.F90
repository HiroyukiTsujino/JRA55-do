!-*-F90-*-
program dummy_topo

  integer(4), parameter :: imut = 1440
  integer(4), parameter :: jmut = 720
  integer(4), parameter :: mtin1 = 81

  integer(4),dimension(imut,jmut) :: ho4, exn

  integer(4) :: i,j,k,mi,mj
  integer(4), parameter :: mtgrd=20

  character(128) :: file_mask

  !--------------------------------------------------------------------------

  ho4(:,:) = 100
  exn(:,:) = 1

  file_mask='topo_dummy.d'
  open(24,file=file_mask,form='unformatted')
  write(24) ho4, exn
  close(24)

end program dummy_topo
