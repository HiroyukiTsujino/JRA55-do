!-*-f90-*-
program lonlat_to_dxdy

  implicit none

  integer(4) :: imut, jmut
  real(8),allocatable :: alont(:), alatt(:)
  real(8),allocatable :: alont_chk(:), alatt_chk(:)
  real(8),allocatable :: alonu(:), alatu(:)
  real(8),allocatable :: dxtdeg(:), dytdeg(:)
  real(8),allocatable :: dxudeg(:), dyudeg(:)

  real(8) :: slon0, slat0
  real(8) :: tmp

  integer(4) :: i, j
  integer(4) :: ios

  logical :: l_xlinear, l_ylinear
  real(8) :: xstart, dx, ystart, dy

  character(len=128) :: file_xdef, file_ydef
  character(len=128) :: file_dxdy

  integer(4), parameter :: lun = 10

  !-------------------------------------------
  namelist /nml_make_dxdy/ l_xlinear, l_ylinear, &
       & imut, jmut, &
       & xstart, dx, ystart, dy, &
       & file_xdef, file_ydef
  !-------------------------------------------

  imut = 0
  jmut = 0
  dx = 0.0d0
  dy = 0.0d0
  xstart = 0.0d0
  ystart = 0.0d0

  open(lun,file='namelist.lonlat2dxdy')
  read(lun,nml=nml_make_dxdy)
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
      write(6,*) ' if latitude is linear, jmut should be given by namelist '
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

  !----

  allocate(alonu(1:imut), alatu(1:jmut))
  allocate(alont_chk(1:imut), alatt_chk(1:jmut))
  allocate(dxtdeg(1:imut), dytdeg(1:jmut))
  allocate(dxudeg(1:imut), dyudeg(1:jmut))

  slon0 = alont(2)
  dxtdeg(1) = alont(2) - alont(1)
  do i = 2, imut
    dxtdeg(i) = 2.0d0 * (alont(i) - alont(i-1) - 0.5d0 * dxtdeg(i-1))
  end do

  alonu(1) = slon0 - 0.5d0 * dxtdeg(1)
  do i = 2, imut
    alonu(i) = alonu(i-1) + dxtdeg(i) 
  enddo
  do i = 1, imut
    alont_chk(i) = alonu(i) - 0.5d0 * dxtdeg(i)
    write(6,*) 'check ', alont_chk(i), alont(i)
  enddo

  slat0 = alatt(2)
  dytdeg(1) = alatt(2) - alatt(1)
  do j = 2, jmut
    dytdeg(j)  = 2.0d0 * (alatt(j) - alatt(j-1) - 0.5d0 * dytdeg(j-1))
  end do

  alatu(1) = slat0 - 0.5d0 * dytdeg(1)
  do j = 2, jmut
    alatu(j) = alatu(j-1) + dytdeg(j) 
  enddo
  do j = 1, jmut
    alatt_chk(j) = alatu(j) - 0.5d0 * dytdeg(j)
    write(6,*) 'check ', alatt_chk(j), alatt(j)
  enddo

  file_dxdy='dxdy_jra55.d'
  open(lun,file=file_dxdy,form='unformatted',action='write')
  write(lun) imut, jmut
  write(lun) dxtdeg
  write(lun) dytdeg
  close(lun)

end program lonlat_to_dxdy
