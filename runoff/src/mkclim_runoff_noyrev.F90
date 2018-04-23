!-*-F90-*-
program main

  implicit none

  integer(4), parameter :: nx = 1440, ny = 720
  real(4) :: roff(nx,ny), work4(nx,ny), undef_in
  integer(4) :: tmp(0:nx+1,0:ny+1)
  integer(4) :: iwork(nx,ny)

  character(256) :: cfriv, cdate
  character(256) :: file_river_in
  character(256) :: file_river_clim
  character(256) :: file_river_leap
  character(256) :: riv_in_base

  integer(4) :: ibyr, ieyr

  integer(4) :: nt, nyr, nm, nd, irec, i, j, tday, ii, jj
  
  integer(4) :: irec1, irec2, irec3

  integer(4),parameter :: nmonyr = 12
  integer(4) :: ndmon(nmonyr) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  real(8) :: roff_clim(nx,ny,365)
  real(8) :: roff_clim_leap(nx,ny)

  !--------------------------------------------------------------------------

  namelist /nml_river_mkclim/ ibyr, ieyr, riv_in_base, undef_in, &
       &                      file_river_clim, file_river_leap

  !--------------------------------------------------------------------------

  open(10,file='namelist.mkclim')
  read(10,nml=nml_river_mkclim)
  close(10)

  !--------------------------------------------------------------------------

  roff_clim(:,:,:) = 0.0d0

  do nyr = ibyr, ieyr

    write(*,*) nyr

    write(file_river_in,'(1a,i4.4,1a)') trim(riv_in_base),nyr,'.bin'
    open(10,file=file_river_in,form='unformatted',status='old',access='direct',action='read',recl=4*nx*ny)
    write(6,*) ' reading from ', trim(file_river_in)

    irec = 0

    if ( lleap(nyr) ) then
      ndmon(2) = 29
    else
      ndmon(2) = 28
    end if

    tday = 0

    do nm = 1, nmonyr
      do nd = 1, ndmon(nm)
        if (lleap(nyr) .and. (nm == 2) .and. (nd == 29)) cycle
        irec = irec + 1
        read(10,rec=irec) roff
        tday = tday + 1
        do j = 1, ny
          do i = 1, nx
            roff_clim(i,j,tday) = roff_clim(i,j,tday) + real(roff(i,j),8)
          end do
        end do
      end do
    end do

    write(6,*) ' closing file, total record = ', irec
    write(6,*) '               total day = ', tday

    close(10)

  end do
    
  open (20,file=file_river_clim,form='unformatted',access='direct',action='write',recl=4*nx*ny)
  write(6,*) ' writing to ', trim(file_river_clim)

  do nd = 1, 365
    do j = 1, ny
      do i = 1, nx
        roff_clim(i,j,nd) = roff_clim(i,j,nd) / real(ieyr-ibyr+1,8)
      end do
    end do
    write(20,rec=nd) real(roff_clim(1:nx,1:ny,nd),4)
  end do

  close(20)

  roff_clim_leap(:,:) = 0.5d0 * (roff_clim(:,:,59) + roff_clim(:,:,60))

  open (20,file=file_river_leap,form='unformatted',access='direct',action='write',recl=4*nx*ny)
  write(6,*) ' writing to ', trim(file_river_leap)
  write(20,rec=1) real(roff_clim_leap(1:nx,1:ny),4)
  close(20)

contains

  logical function lleap( year )
    implicit none
    integer(4) :: year
    logical :: lflg
    if ( mod(year,4) == 0 ) then
      if ( mod(year,100) == 0 ) then
        if ( mod(year,400) == 0 ) then
          lflg = .true.
        else
          lflg = .false.
        end if
      else
        lflg = .true.
      end if
    else
      lflg = .false.
    end if
    lleap = lflg
  end function lleap

end program main
