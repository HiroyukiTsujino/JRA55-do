!-*-F90-*-
program main

  implicit none

  integer(4), parameter :: nx = 752, ny = 785

  real(4) :: work4a(nx,ny), work4b(nx,ny), work4c(nx,ny), work4d(nx,ny)
  real(4) :: undef_in, hl1

  integer(4) :: imask (nx,ny)

  character(256) :: cfriv, cdate
  character(256) :: file_river_in
  character(256) :: file_mask
  character(256) :: riv_in_base

  integer(4) :: ibyr, ieyr

  integer(4) :: nt, nyr, nm, nd, irec, i, j, tday, ii, jj
  integer(4) :: num_data
  
  integer(4) :: irec1, irec2, irec3

  integer(4),parameter :: nmonyr = 12
  integer(4) :: ndmon(nmonyr) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  !--------------------------------------------------------------------------

  namelist /nml_river_scan/ ibyr, ieyr, riv_in_base, file_mask, undef_in

  !--------------------------------------------------------------------------

  write(6,*) 'reading namelist '

  open (10,file='namelist.scan_river')
  read (10,nml=nml_river_scan)
  close(10)

  !--------------------------------------------------------------------------

  imask(:,:) = 0

  do nyr = ibyr, ieyr

    write(6,*) nyr

    do nm = 1, nmonyr

      write(file_river_in,'(1a,i4.4,i2.2)') trim(riv_in_base),nyr,nm

      open(10,file=file_river_in,form='unformatted',status='old',access='direct',action='read',recl=4*nx*ny)

      write(6,*) ' reading from ', trim(file_river_in)

      read(10,rec=1) work4a
      read(10,rec=2) work4b
      read(10,rec=3) work4c
      read(10,rec=6) work4d ! Greenland mask

      do j = 1, ny
        do i = 1, nx
          hl1 = work4a(i,j) + work4b(i,j) + work4c(i,j) 
          if (hl1 > 0.0) then
            if (work4d(i,j) == 1.0) then
              imask(i,j) = 1
            else
              imask(i,j) = 5
            end if
          end if
        end do
      end do

      close(10)

    end do


  end do

  open (20,file=file_mask,form='unformatted',access='direct',action='write',recl=4*nx*ny)
  write(6,*) ' writing to ', trim(file_mask)
  write(20,rec=1) real(imask(1:nx,1:ny),4)
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
