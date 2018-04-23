!-*-F90-*-
program main

  implicit none

  integer(4), parameter :: nx = 1440, ny = 720
  real(4) :: roff(nx,ny), work4(nx,ny), mask(nx,ny), undef_in
  integer(4) :: tmp(0:nx+1,0:ny+1)
  integer(4) :: iwork(nx,ny)
  real(4) :: factor

  character(256) :: cfriv, cdate
  character(256) :: file_river_in
  character(256) :: file_river_out
  character(256) :: riv_in_base
  character(256) :: riv_out_base
  character(256) :: file_mask

  integer(4) :: ibyr, ieyr

  integer(4) :: nt, nyr, nm, nd, irec, i, j, tday, ii, jj
  
  integer(4) :: irec1, irec2, irec3

  integer(4),parameter :: nmonyr = 12
  integer(4) :: ndmon(nmonyr) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  !--------------------------------------------------------------------------

  namelist /nml_river_factor/ factor, ibyr, ieyr, &
       & file_mask, &
       & riv_in_base, riv_out_base, undef_in

  !--------------------------------------------------------------------------

  open(10, file='namelist.apply_factor')
  read(10, nml=nml_river_factor)
  close(10)

  !--------------------------------------------------------------------------

  cfriv='/worke/htsujino/RUNOFF_YOSHIMURA/nextxy_wo_lake.bin'

  open(10,file=cfriv,form='unformatted',status='old',access='direct',recl=4*nx*ny)
  read(10,rec=1) iwork
  close(10)

  do j = 1, ny
    do i = 1, nx
      if (i > nx/2) then
        tmp(i-nx/2,ny-j+1) = iwork(i,j)
      else
        tmp(i+nx/2,ny-j+1) = iwork(i,j)
      end if
    end do
  end do

  tmp(1:nx,0) = -9999
  tmp(1:nx,ny+1) = 1
  tmp(0,1:ny) = tmp(nx,1:ny)
  tmp(nx+1,1:ny) = tmp(1,1:ny)

  do j = 1, ny
    do i = 1, nx
      if ( tmp(i,j) == -9 ) then
        mask(i,j) = 1.
      else
        mask(i,j) = 0.
      end if
    end do
  end do

  open(10,file=file_mask,form='unformatted',access='direct',action='write',recl=4*nx*ny)
  write(6,*) ' Writing MASK to ', trim(file_mask)
  write(10,rec=1) mask
  close(10)

  !-------------------------------------------------------------------------

  do nyr = ibyr, ieyr

    write(*,*) nyr

    write(file_river_in,'(1a,i4.4,1a)') trim(riv_in_base),nyr,'.bin'
    open(10,file=file_river_in,form='unformatted',status='old',access='direct',action='read',recl=4*nx*ny)
    write(6,*) ' reading from ', trim(file_river_in)

    write(file_river_out,'(1a,i4.4,1a)') trim(riv_out_base),nyr,'.bin'
    open(20,file=file_river_out,form='unformatted',access='direct',action='write',recl=4*nx*ny)
    write(6,*) ' writing to ', trim(file_river_out)

    irec = 0

    if ( lleap(nyr) ) then
      ndmon(2) = 29
    else
      ndmon(2) = 28
    end if

    do nm = 1, nmonyr

      do nd = 1, ndmon(nm)

        irec = irec + 1
        read(10,rec=irec) work4
        do j = 1, ny
          do i = 1, nx
            if (i > nx/2) then
              roff(i-nx/2,ny-j+1) = work4(i,j)
            else
              roff(i+nx/2,ny-j+1) = work4(i,j)
            end if
          end do
        end do

        do j = 1, ny
          do i = 1, nx
            if ( roff(i,j) == undef_in ) then
              roff(i,j) = 0.0
            else
              roff(i,j) = max(roff(i,j),0.0) * mask(i,j) * factor
            end if
          end do
        end do
        write(20,rec=irec) roff
      end do
    end do

    write(6,*) ' closing file, total record = ', irec

    close(20)
    close(10)

  end do
    
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
