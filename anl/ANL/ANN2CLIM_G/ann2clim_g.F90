!-*-F90-*-
!ann2clim_g.F90
!====================================================
!
!  Monthly Data to Annual Data
!
!====================================================
program ann2clim_g

  implicit none

  integer(4), save :: imut, jmut, km
  !
  real(4), save :: UNDEF = -9.99e33
  !
  integer(4) :: sttyear, endyear
  !
  character(len=256)    :: fpathin
  character(len=256)    :: fpathout
  !
  character(len=256)    :: flin
  character(len=256)    :: fclim
  character(len=256)    :: flout
  !
  integer(4), parameter :: mtfile = 80
  integer(4), parameter :: mtin   = 81
  integer(4), parameter :: mtout  = 82
  integer(4) :: nth_data
  logical :: l_subclim
  !
  real(4), allocatable :: dat3in(:,:,:)
  real(4), allocatable :: dat3out(:,:,:)
  real(8), allocatable :: dat8(:,:,:)
  real(8), allocatable :: dat8clim(:,:,:)
  !
  real(4), allocatable :: dat3inorg(:,:,:)
  !
  integer(4) :: iyear, m
  integer(4) :: i, j, k, n
  !
  !==========================================
  namelist /nml_ann2clim_g/ imut, jmut, km, undef, &
       & sttyear, endyear, fpathin, fpathout, nth_data, &
       & l_subclim, fclim
  !-----------------------------------------
  read(unit=5, nml=nml_ann2clim_g)
  print *,'imut       :', imut
  print *,'jmut       :', jmut
  print *,'km         :', km
  print *,'UNDEF      :', undef
  print *,'start year :', sttyear
  print *,'end   year :', endyear
  print *,'fpath in   :', trim(fpathin)
  print *,'fpath out  :', trim(fpathout)
  print *,'nth_data   :', nth_data
  print *,'l_subclim  :', l_subclim
  print *,'fclim      :', trim(fclim)

  allocate(dat3in(1:imut, 1:jmut, 1:km))
  allocate(dat3inorg(1:imut, 1:jmut, 1:km))
  allocate(dat8  (1:imut, 1:jmut, 1:km))
  allocate(dat8clim(1:imut, 1:jmut, 1:km))
  allocate(dat3out(1:imut, 1:jmut, 1:km))

  !------------------------------------------
  if (l_subclim) then

    open(mtin, file=fclim, form='unformatted', access='direct', recl=4*imut*jmut*km)
    write(*,'(a, a)') 'climatology read from :', trim(fclim)
    read (mtin, rec=nth_data) dat3in(:,:,:)
    close(mtin)
    where(dat3in(1:imut, 1:jmut, 1:km) == undef)
      dat3in(1:imut, 1:jmut, 1:km) = 0.0e0
    end where
    dat8clim(1:imut, 1:jmut, 1:km) = real(dat3in(1:imut, 1:jmut, 1:km), 8)

  end if

  !------------------------------------------

  write(flout, '(a, a, i4.4, a, i4.4)' ) trim(fpathout), '.', sttyear,'_', endyear
  open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
  write(6,*) 'Output : ', trim(flout)
  dat8(1:imut, 1:jmut, 1:km) = 0.d0

  !------------------------------------------

  do iyear = sttyear, endyear
    write(flin, '(a, a, i4.4)' ) trim(fpathin), '.', iyear
    write(*,'(a, a)') 'file in :', trim(flin)
    open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)
    read (mtin, rec=nth_data) dat3in(:,:,:)
    if (iyear == sttyear) then
      dat3inorg(1:imut, 1:jmut, 1:km) = dat3in(1:imut, 1:jmut, 1:km)
    end if
    do k=1, km
      where(dat3in(1:imut, 1:jmut, k) == UNDEF)
        dat3in(1:imut, 1:jmut, k) = 0.0
      end where
    end do
    dat8(1:imut, 1:jmut, 1:km) = dat8(1:imut, 1:jmut, 1:km) &
         &    + real(dat3in(1:imut, 1:jmut, 1:km),8)
    close(mtin)
  end do

  dat8(1:imut, 1:jmut, 1:km) = dat8(1:imut, 1:jmut, 1:km) / real(endyear-sttyear+1,8)

  if (l_subclim) then
    dat8(1:imut, 1:jmut, 1:km) = dat8(1:imut, 1:jmut, 1:km) - dat8clim(1:imut, 1:jmut, 1:km)
  end if

  write(*,'(a, a)') 'file out :', trim(flout)
  dat3out(1:imut, 1:jmut, 1:km) = real(dat8(1:imut, 1:jmut, 1:km),4)
  do k=1, km
    where(dat3inorg(1:imut, 1:jmut, k) == undef)
      dat3out(1:imut, 1:jmut, k) = UNDEF
    end where
  end do
  write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:km)
  close(mtout)

!====================================================
end program ann2clim_g
