!-*-F90-*-
program main

  implicit none

  integer(4), parameter :: nx = 1440, ny = 720
  real(4) :: roff(nx,ny), work4(nx,ny), undef_in
  integer(4) :: tmp(0:nx+1,0:ny+1)
  integer(4) :: iwork(nx,ny)
  real(4) :: factor

  character(256) :: cfriv, cdate
  character(256) :: file_river_in
  character(256) :: file_river_out
  character(256) :: riv_in_base
  character(256) :: riv_out_base

  integer(4) :: ibyr, ieyr

  integer(4) :: nt, nyr, nm, nd, irec, i, j, tday, ii, jj
  
  integer(4) :: irec1, irec2, irec3

  integer(4),parameter :: nmonyr = 12
  integer(4) :: ndmon(nmonyr) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  !--------------------------------------------------------------------------

  namelist /nml_river_factor_noyrev/ factor, ibyr, ieyr, &
       & riv_in_base, riv_out_base, undef_in

  !--------------------------------------------------------------------------

  open(10, file='namelist.apply_factor_noyrev')
  read(10, nml=nml_river_factor_noyrev)
  close(10)

  !--------------------------------------------------------------------------

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
            roff(i,j) = work4(i,j)
          end do
        end do

        do j = 1, ny
          do i = 1, nx
            if ( roff(i,j) == undef_in ) then
              roff(i,j) = 0.0
            else
              roff(i,j) = roff(i,j) * factor
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
