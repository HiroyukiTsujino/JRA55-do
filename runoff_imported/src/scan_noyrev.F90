!-*-F90-*-
program main

  implicit none

  integer(4), parameter :: nx = 1440, ny = 720

  real(4) :: work4(nx,ny), undef_in

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

  logical    :: l_clim
  integer(4) :: num_rec_clim

  !--------------------------------------------------------------------------

  namelist /nml_river_scan_noyrev/ ibyr, ieyr, l_clim, num_rec_clim, &
       & riv_in_base, file_mask, undef_in

  !--------------------------------------------------------------------------

  l_clim = .false.
  num_rec_clim = 0

  write(6,*) 'reading namelist '

  open (10,file='namelist.scan_noyrev')
  read (10,nml=nml_river_scan_noyrev)
!  write(6,nml=nml_river_scan_noyrev) 
  close(10)

  if (l_clim) then
    if (num_rec_clim <= 0) then
      write(6,*) ' ERROR ! If l_clim = .true., you must specify positive integer num_rec_clim'
      stop
    end if
    ibyr = 1 ! dummy
    ieyr = 1 ! dummy
  end if

  !--------------------------------------------------------------------------

  imask(:,:) = 0

  do nyr = ibyr, ieyr

    write(6,*) nyr
    irec = 0

    if (l_clim) then
      open(10,file=riv_in_base,form='unformatted',status='old',access='direct',action='read',recl=4*nx*ny)
      write(6,*) ' reading from ', trim(riv_in_base)
      num_data = num_rec_clim
    else
      write(file_river_in,'(1a,i4.4)') trim(riv_in_base),nyr
      open( 10,file=file_river_in,form='unformatted',status='old',access='direct',action='read',recl=4*nx*ny)
      write(6,*) ' reading from ', trim(file_river_in)
      if ( lleap(nyr) ) then
        num_data = 366
      else
        num_data = 365
      end if
    end if

    do nd = 1, num_data
      irec = irec + 1
      read(10,rec=irec) work4
      do j = 1, ny
        do i = 1, nx
          if ((work4(i,j) /= undef_in) .and. (work4(i,j) > 0.0))then
            imask(i,j) = 1
          end if
        end do
      end do
    end do

    write(6,*) ' closing file, total record = ', irec

    close(10)

  end do

  open(20,file=file_mask,form='unformatted',access='direct',action='write',recl=4*nx*ny)
  write(6,*) ' writing to ', trim(file_mask)
  write(20,rec=1) imask
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
