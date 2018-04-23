!-*-F90-*-
!mon2ann_g.F90
!====================================================
!
!  Monthly Data to Annual Data
!
!====================================================
program mon2ann_g
  !
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
  character(len=256)    :: flout
  !
  integer(4), parameter :: mtfile = 80
  integer(4), parameter :: mtin   = 81
  integer(4), parameter :: mtout  =82
  !
  real(4), allocatable :: dat3in(:,:,:)
  real(4), allocatable :: nundef(:,:,:)
  real(4), allocatable :: dat3out(:,:,:)
  real(8), allocatable :: dat8(:,:,:)
  !
  real(8), save :: daymonth(12)
  real(8), save :: day_year
  !
  integer(4) :: iyear, m
  integer(4) :: i, j, k, n

  logical l_leap_valid

  !==========================================
  ! 
  namelist /nml_mon2ann_g/ imut, jmut, km, undef, &
       & l_leap_valid, sttyear, endyear, &
       & fpathin, fpathout
  !
  !-------------------------------------------
  !
  k_max=0
  !
  read(unit=5, nml=nml_mon2ann_g)
  print *,'imut       :', imut
  print *,'jmut       :', jmut
  print *,'km         :', km
  print *,'UNDEF      :', undef
  print *,'start year :', sttyear
  print *,'end   year :', endyear
  print *,'fpath in   :', fpathin
  print *,'fpath out  :', fpathout
  !
  allocate(dat3in(1:imut, 1:jmut, 1:km))
  allocate(nundef(1:imut, 1:jmut, 1:km))
  allocate(dat8(1:imut, 1:jmut, 1:km))
  allocate(dat3out(1:imut, 1:jmut, 1:km))
  !------------------------------------------
  !
  daymonth( 1)=31.d0
  daymonth( 2)=28.d0
  daymonth( 3)=31.d0
  daymonth( 4)=30.d0
  daymonth( 5)=31.d0
  daymonth( 6)=30.d0
  daymonth( 7)=31.d0
  daymonth( 8)=31.d0
  daymonth( 9)=30.d0
  daymonth(10)=31.d0
  daymonth(11)=30.d0
  daymonth(12)=31.d0
  !
  day_year = 365.0
  !
  !-------------------------
  do iyear = sttyear, endyear

    if (l_leap_valid) then
      if(mod(iyear,4) == 0) then
        if(mod(iyear,100) == 0) then
          if(mod(iyear,400) == 0) then
            daymonth(2) = 29.d0
            day_year = 366.d0
          else
            daymonth(2) = 28.d0
            day_year = 365.d0
          end if
        else
          daymonth(2) = 29.d0
          day_year = 366.d0
        end if
      else
        daymonth(2) = 28.d0
        day_year = 365.d0
      end if
    end if

    nundef(1:imut, 1:jmut, 1:km) = 0

    write(flout, '(a, a, i4.4)' ) trim(fpathout), '.', iyear
    open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
    dat8(1:imut, 1:jmut, 1:km) = 0.d0
    do m=1, 12
      write(flin, '(a, a, i4.4, i2.2)' ) trim(fpathin), '.', iyear, m
      write(*,'(a, a)') 'file in :', trim(flin)
      open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)
      !
      read (mtin, rec=1) dat3in(:,:,:)
      do k=1, km
        do j = 1, jmut
          do i = 1, imut
            if (dat3in(i,j,k) == UNDEF) then
              dat3in(i,j,k) = 0.0
              nundef(i,j,k) = nundef(i,j,k) + 1
            end if
          end do
        end do
      end do
      dat8(1:imut, 1:jmut, 1:km) = dat8(1:imut, 1:jmut, 1:km) &
            &    + daymonth(m) * dat3in(1:imut, 1:jmut, 1:km)
      !
      close(mtin)
    end do
    !
    write(*,'(a, a)') 'file out :', trim(flout)
    dat3out(1:imut, 1:jmut, 1:km) = real(dat8(1:imut, 1:jmut, 1:km),4)/day_year
    do k=1, km
      where(nundef(1:imut, 1:jmut, k) > 0)
        dat3out(1:imut, 1:jmut, k) = UNDEF
      end where
    end do
    write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:km)
    close(mtout)
    !-----------------------------------------------------------------
  end do
  !
!====================================================
end program mon2ann_g
