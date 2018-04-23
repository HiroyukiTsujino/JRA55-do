!-*-f90-*-
program lonlat_to_dxdy_up

  ! set data point to the U-points

  implicit none

  integer(4) :: imut, jmut
  real(8),allocatable :: alont(:), alatt(:)
  real(8),allocatable :: alonu_chk(:), alatu_chk(:)
  real(8),allocatable :: alonu(:), alatu(:)
  real(8),allocatable :: dxtdeg(:), dytdeg(:)
  real(8),allocatable :: dxudeg(:), dyudeg(:)

  real(8) :: slon0, slat0
  real(8) :: tmp1, tmp2
  integer(4) :: ii, jj

  integer(4) :: i, j
  integer(4) :: ios

  logical :: l_xlinear, l_ylinear
  real(8) :: xstart, dx, ystart, dy

  character(len=128) :: file_xdef, file_ydef
  character(len=128) :: file_dxdy, file_lat

  integer(4), parameter :: lun = 10

  !-------------------------------------------
  namelist /nml_make_dxdy/ l_xlinear, l_ylinear, &
       & imut, jmut, &
       & xstart, dx, ystart, dy, &
       & file_xdef, file_ydef, &
       & file_lat
  !-------------------------------------------

  imut = 0
  jmut = 0
  dx = 0.0d0
  dy = 0.0d0
  xstart = 0.0d0
  ystart = 0.0d0

  open(lun,file='namelist.lonlat2dxdy_up_exact')
  read(lun,nml=nml_make_dxdy)
  close(lun)

  !-------------------------------------------

  if (l_xlinear) then
    if (imut == 0) then
      write(6,*) ' if longitude is linear, imut should be given by namelist '
      stop
    end if
    allocate(alonu(imut))
    alonu(1) = xstart + 0.5d0 * dx
    do i = 2, imut
      alonu(i) = alonu(i-1) + dx
    end do
  else
    open(lun,file=file_xdef)
    imut = 0
    do
      read(lun,*,iostat=ios) ii, tmp1, tmp2, jj
      write(6,*) ii, tmp1, ios
      if (ios /= 0) exit
      imut = imut + 1
    end do
    close(lun)
    write(6,*) 'X-direction data:', imut

    allocate(alonu(imut))
    
    open(lun,file=file_xdef)
    do i = 1, imut
      read(lun,*,iostat=ios) alonu(i)
      if (ios /= 0) then
        write(6,*) ' Error : inconsistent number of data (X) ', imut
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
    allocate(alatu(jmut))
    alatu(j) = ystart + 0.5d0 * dy
    do j = 1, jmut
      alatu(j) = alatu(j-1) + dy
    end do
  else
    open(lun,file=file_ydef)
    read(lun,*) 
    jmut = 0
    do
      read(lun,*,iostat=ios) ii, tmp1, tmp2, jj
      write(6,*) ii, tmp1, ios
      if (ios /= 0) exit
      jmut = jmut + 1
    end do
    close(lun)

    jmut = jmut * 2

    write(6,*) 'Y-direction data:', jmut
    
    allocate(alatu(jmut))
    
    open(lun,file=file_ydef)
    read(lun,*) 
    do j = 1, jmut / 2
      read(lun,*,iostat=ios) ii, tmp1, tmp2, jj
      alatu(jmut - j + 1) = tmp1
      alatu(j) = - tmp1
      if (ios /= 0) then
        write(6,*) ' Error : inconsistent number of data (Y) ', jmut
        stop
      end if
    end do
    close(lun)
  end if

  !----

  allocate(alont(1:imut), alatt(1:jmut))
  allocate(alonu_chk(1:imut), alatu_chk(1:jmut))
  allocate(dxtdeg(1:imut), dytdeg(1:jmut))
  allocate(dxudeg(1:imut), dyudeg(1:jmut))

  dxtdeg(1) = 2.0d0 * (alonu(1) - xstart)
  do i = 2, imut
    dxtdeg(i) = (alonu(i) - alonu(i-1))
  end do

  alont(1) = alonu(1) - 0.5d0 * dxtdeg(1)
  do i = 2, imut
    alont(i) = alonu(i-1) + 0.5d0 * dxtdeg(i)
  enddo

  alonu_chk(1) = alont(2) - 0.5d0 * dxtdeg(2)
  write(6,*) 'check ', alonu_chk(1), alonu(1)
  do i = 2, imut
    alonu_chk(i) = alonu(i-1) + dxtdeg(i)
    write(6,*) 'check ', alonu_chk(i), alonu(i)
  enddo

  !-----

  dytdeg(1) = 2.0d0 * (alatu(1) - ystart)
  do j = 2, jmut
    dytdeg(j) = (alatu(j) - alatu(j-1))
  end do

  alatt(1) = alatu(1) - 0.5d0 * dytdeg(1)
  do j = 2, jmut
    alatt(j) = alatu(j-1) + 0.5d0 * dytdeg(j)
  enddo

  alatu_chk(1) = alatt(2) - 0.5d0 * dytdeg(2)
  write(6,*) 'check ', alatu_chk(1), alatu(1)
  do j = 2, jmut
    alatu_chk(j) = alatu_chk(j-1) + dytdeg(j)
    write(6,*) 'check ', alatu_chk(j), alatu(j)
  enddo

  !-----

  write(6,*) 'T-point longitude'
  write(6,'(7f14.8)') (alont(i),i=1,imut)
  write(6,*) 'T-point latitude'
  write(6,'(7f14.8)') (alatt(j),j=1,jmut)

  write(6,*) 'DXT'
  write(6,'(7f14.8)') (dxtdeg(i),i=1,jmut)
  write(6,*) 'DYT'
  write(6,'(7f14.8)') (dytdeg(j),j=1,jmut)

  file_dxdy='dxdy_jra55_up_exact.d'
  open(lun,file=file_dxdy,form='unformatted',action='write')
  write(lun) imut, jmut
  write(lun) dxtdeg
  write(lun) dytdeg
  close(lun)

  open(lun,file=file_lat,form='formatted',action='write')
  do j = 1, jmut
    write(lun,*) alatu(j)
  end do
  close(lun)

  write(6,*) ' lon_west_end_of_core  = ', alont(2)
  write(6,*) ' lat_south_end_of_core = ', alatt(2)

end program lonlat_to_dxdy_up
