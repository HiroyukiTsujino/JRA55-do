!-*-f90-*-
program lonlat_to_dxdy

  implicit none

  integer(4) :: imut, jmut
  real(8),allocatable :: alont(:), alatt(:)
  real(8),allocatable :: alont_chk(:), alatt_chk(:)

  real(8) :: slon0, slat0
  real(8) :: tmp

  integer(4) :: i, j
  integer(4) :: ios

  logical :: l_xlinear, l_ylinear
  real(8) :: xstart, dx, ystart, dy

  character(len=128) :: file_xdef, file_ydef
  character(len=128) :: file_grid

  integer(4), parameter :: lun = 10

  !-------------------------------------------
  namelist /nml_lonlat_force/ l_xlinear, l_ylinear, &
       & imut, jmut, &
       & xstart, dx, ystart, dy, &
       & file_xdef, file_ydef, &
       & file_grid
  !-------------------------------------------

  imut = 0
  jmut = 0
  dx = 0.0d0
  dy = 0.0d0
  xstart = 0.0d0
  ystart = 0.0d0

  open(lun,file='namelist.lonlat_for_force')
  read(lun,nml=nml_lonlat_force)
  close(lun)

  !-------------------------------------------

  if (l_xlinear) then
    if (imut == 0) then
      write(6,*) ' if longitude is linear, imut should be given by namelist '
      stop
    end if
    allocate(alont(imut))
    do i = 1, imut
      alont(i) = xstart + dx * (i-1)
    end do
  else
    open(lun,file=file_xdef)
    imut = 0
    do
      read(lun,*,iostat=ios) tmp
      write(6,*) tmp, ios
      if (ios /= 0) exit
      imut = imut + 1
    end do
    close(lun)
    write(6,*) 'X-direction data:', imut

    allocate(alont(imut))
    
    open(lun,file=file_xdef)
    do i = 1, imut
      read(lun,*,iostat=ios) alont(i)
      if (ios /= 0) then
        write(6,*) ' Error : inconsistent number of data ', imut
        stop
      end if
    end do
    close(lun)
  end if

  !-----

  if (l_ylinear) then
    if (jmut == 0) then
      write(6,*) ' if longitude is linear, jmut should be given by namelist '
      stop
    end if
    allocate(alatt(jmut))
    do j = 1, jmut
      alatt(j) = ystart + dy * (j-1)
    end do
  else
    open(lun,file=file_ydef)
    jmut = 0
    do
      read(lun,*,iostat=ios) tmp
      write(6,*) tmp, ios
      if (ios /= 0) exit
      jmut = jmut + 1
    end do
    close(lun)

    write(6,*) 'Y-direction data:', jmut

    allocate(alatt(jmut))

    open(lun,file=file_ydef)
    do j = 1, jmut
      read(lun,*,iostat=ios) alatt(j)
      if (ios /= 0) then
        write(6,*) ' Error : inconsistent number of data ', jmut
        stop
      end if
    end do
    close(lun)
  end if

  open(lun,file=file_grid,form='unformatted',action='write')
  write(6,*) ' lon-lat data for force_data.F90 is written to ', trim(file_grid)
  write(lun) alont, alatt
  close(lun)

end program lonlat_to_dxdy
