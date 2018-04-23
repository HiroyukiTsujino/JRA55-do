!-*-F90-*-
program main

  implicit none

  integer(4), parameter :: nx = 301, ny = 561
  real(4) :: roff(nx,ny), mask(nx,ny), undef
  real(4) :: alon(nx,ny), alat(nx,ny)
  integer(4) :: tmp(0:nx+1,0:ny+1)

  character(256) :: file_fwf_in
  character(256) :: file_riv_mon

  integer(4) :: nt, nyr, nm, nd, irec, i, j, ii, jj
  
  integer(4) :: irec1, irec2, irec3

  character(128) :: fl_in_fwf, fl_in_lonlat, dir_out

  integer(4) :: ibyr, ieyr

  !--------------------------------------------------------------------

  namelist /nml_bamber_runoff/ fl_in_fwf, fl_in_lonlat, dir_out, ibyr, ieyr

  !--------------------------------------------------------------------
 
  open(10, file='namelist.bamber_runoff')
  read(10, nml=nml_bamber_runoff)
  close(10)

  !--------------------------------------------------------------------

  open(10,file=fl_in_fwf,form='unformatted',status='old',access='direct', &
       & convert='little_endian',recl=4*nx*ny)
  irec = 0

  open(11,file=fl_in_lonlat)
  do j = 1, ny
    do i = 1, nx
      read(11,*) alon(i,j), alat(i,j)
    end do
  end do
  close(11)

  do nyr = ibyr, ieyr

    write(*,*) nyr

    do nm = 1, 12

      irec = irec + 1
      read(10,rec=irec) roff

      write(file_riv_mon,'(1a,1a,i4.4,i2.2)') trim(dir_out),'/fwf_green.',nyr,nm
      open(30,file=file_riv_mon,form='unformatted',access='direct',convert='big_endian',recl=4*nx*ny)
      write(30,rec=1) real(roff,4)
      write(30,rec=2) real(alon,4)
      write(30,rec=3) real(alat,4)
      close(30)

    end do

  end do

end program main
