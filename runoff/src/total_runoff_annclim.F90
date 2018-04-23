!-*-F90-*-
program main

  implicit none

  integer(4), parameter :: nx = 1440, ny = 720
  real(4) :: roff(nx,ny), mask(nx,ny), undef
  integer(4) :: tmp(0:nx+1,0:ny+1)
  real(8) :: clim, annual

  character(256) :: cfriv, cdate
  character(256) :: file_river_in
  character(256) :: file_riv_ann

  integer(4) :: nt, nyr, nm, nd, irec, i, j, tday, ii, jj
  
  integer(4) :: irec1, irec2, irec3
  character(256) :: dir_out

  data undef / 1e20 /

  !--------------------------------------------------------------------

  namelist /nml_total_runoff_annclim/ file_river_in, dir_out

  !--------------------------------------------------------------------
 
  open (10, file='namelist.total_runoff_annclim')
  read (10, nml=nml_total_runoff_annclim)
  close(10)

  !--------------------------------------------------------------------

  annual = 0.d0
  open (10,file=file_river_in,form='unformatted',status='old',access='direct',recl=4*nx*ny)
  irec = 0

  read(10,rec=irec) roff
  do j = 1, ny
    do i = 1, nx
      if ( roff(i,j) == undef ) roff(i,j) = 0.
      annual = annual + real(roff(i,j),8)
    end do
  end do

  write(file_riv_ann,'(1a,1a)') trim(dir_out),'/annual.txt'
  open (40,file=file_riv_ann)
  write(40,*) annual
  write(6,*) ' Annual mean = ', real(annual,4)
  close(40)

end program main
