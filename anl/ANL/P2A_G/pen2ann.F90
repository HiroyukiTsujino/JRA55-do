!-*-F90-*-
!mon2ann.F90
!====================================================
!
!  Pentad Data to Annual Data
!
!====================================================
program pen2ann
  !
  use oc_mod_param, only : &
  &   imut, jmut, km
  !
  use oc_structure, only  : &
  &   read_topo,            &
  &   aexl, atexl,          &
  &   coefx, coefy
  !
  character(len=256)    :: fltopo
  !
  real(4), parameter :: UNDEF = -9.99e33
  !
  real(4) :: dat3in(imut, jmut, km) ! 読み出し用3次元配列
  !
  real(4) :: dat3out(imut, jmut, km)
  !
  integer(4), save :: daymonth(12)
  integer(4), save :: day_year
  integer(4), parameter :: num_data = 73 ! = 365 / 5
  integer(4), save :: day_pen(num_data)
  !
  character(len=256)    :: flin
  character(len=256)    :: flout
  !
  integer(4), parameter :: mtfile = 80
  integer(4), parameter :: mtin   = 81
  integer(4), parameter :: mtout  = 82
  !
  integer(4) :: styear, endyear
  integer(4) :: iyear
  integer(4) :: i, j, k
  integer(4) :: nday, month, nd
  !
  character(len=256)    :: fpathin
  character(len=256)    :: fpathout
  character(len=  1)    :: tuxy
  integer(4)            :: k_max
  logical               :: l_leap
  !
  namelist /nml_pen2ann/ fltopo, styear, endyear, l_leap, fpathin, fpathout, tuxy, k_max
  !
  !==========================================
  !
  k_max=0
  !
  read(unit=5, nml=nml_pen2ann)
  print *,'fltopo     :', fltopo
  print *,'start year :', styear
  print *,'end   year :', endyear
  print *,'leap year  :', l_leap
  print *,'fpath in   :', fpathin
  print *,'fpath out  :', fpathout
  print *,'tuxy       :', tuxy
  print *,'k_max      :', k_max
  !
  call read_topo(fltopo)
  !
  !------------------------------------------
  !
  daymonth( 1)=31
  daymonth( 2)=28
  daymonth( 3)=31
  daymonth( 4)=30
  daymonth( 5)=31
  daymonth( 6)=30
  daymonth( 7)=31
  daymonth( 8)=31
  daymonth( 9)=30
  daymonth(10)=31
  daymonth(11)=30
  daymonth(12)=31

  day_year = 365

  day_pen(1:num_data) = 5

  !-------------------------
  do iyear = styear, endyear

    if (l_leap) then
      if(mod(iyear,4) == 0) then
        if(mod(iyear,100) == 0) then
          if(mod(iyear,400) == 0) then
            daymonth(2) = 29
            day_pen(12) = 6
            day_year = 366
          else
            daymonth(2) = 28
            day_pen(12) = 5
            day_year = 365
          end if
        else
          daymonth(2) = 29
          day_pen(12) = 6
          day_year = 366
        end if
      else
        daymonth(2) = 28
        day_pen(12) = 5
        day_year = 365
      end if
    end if

    nday = 0
    month = 1

    select case (tuxy)
      case('T') !-------------------------------------------------
        write(flout, '(a, a, i4.4)' ) trim(fpathout), '.', iyear
        open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
        dat3out(1:imut, 1:jmut, 1:km) = 0.0
        do nd = 1, num_data

          nday = nday + day_pen(nd)

          if (nday > daymonth(month)) then
            nday = nday - daymonth(month)
            month = month + 1
          end if

          write(6,*) 'Year = ', iyear, ' Month = ', month, ' Day = ', nday

          write(flin, '(a, a, i4.4, i2.2, i2.2)' ) trim(fpathin), '.', iyear, month, nday
          write(*,'(a, a)') 'file in :', trim(flin)
          open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)
          !
          read (mtin, rec=1) dat3in(:,:,:)
          do k=1, km
            where(atexl(1:imut, 1:jmut, k) == 0.d0)
              dat3in(1:imut, 1:jmut, k) = 0.0
            end where
          end do
          dat3out(1:imut, 1:jmut, 1:km) = dat3out(1:imut, 1:jmut, 1:km) &
            &             + real(day_pen(nd),4) * dat3in(1:imut, 1:jmut, 1:km)
          !
          close(mtin)
        end do
        write(*,'(a, a)') 'file out :', trim(flout)
        dat3out(1:imut, 1:jmut, 1:km)=dat3out(1:imut, 1:jmut, 1:km)/ real(day_year,4)
        do k=1, km
          where(atexl(1:imut, 1:jmut, k) == 0.d0)
            dat3out(1:imut, 1:jmut, k) = UNDEF
          end where
        end do
        write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:km)
        close(mtout)
      case('t') !-------------------------------------------------
        write(flout, '(a, a, i4.4)' ) trim(fpathout), '.', iyear
        open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
        dat3out(1:imut, 1:jmut, 1:k_max) = 0.0
        do nd = 1, num_data

          nday = nday + day_pen(nd)

          if (nday > daymonth(month)) then
            nday = nday - daymonth(month)
            month = month + 1
          end if

          write(6,*) 'Year = ', iyear, ' Month = ', month, ' Day = ', nday

          write(flin, '(a, a, i4.4, i2.2, i2.2)' ) trim(fpathin), '.', iyear, month, nday
          write(*,'(a, a)') 'file in :', trim(flin)
          open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
          !
          read (mtin, rec=1) dat3in(:,:,1:k_max)
          do k=1, k_max
            where(atexl(1:imut, 1:jmut, 1) == 0.d0)
              dat3in(1:imut, 1:jmut, k) = 0.0
            end where
          end do
          dat3out(1:imut, 1:jmut, 1:k_max) = dat3out(1:imut, 1:jmut, 1:k_max) &
            &       + real(day_pen(nd),4) * dat3in(1:imut, 1:jmut, 1:k_max)
          !
          close(mtin)
        end do
        write(*,'(a, a)') 'file out :', trim(flout)
        dat3out(1:imut, 1:jmut, 1:k_max)=dat3out(1:imut, 1:jmut, 1:k_max) / real(day_year,4)
        do k=1, k_max
          where(atexl(1:imut, 1:jmut, 1) == 0.d0)
            dat3out(1:imut, 1:jmut, k) = UNDEF
          end where
        end do
        write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:k_max)
        close(mtout)
      case('U') !-------------------------------------------------
        write(flout, '(a, a, i4.4)' ) trim(fpathout), '.', iyear
        open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
        dat3out(1:imut, 1:jmut, 1:km) = 0.0
        do nd = 1, num_data

          nday = nday + day_pen(nd)

          if (nday > daymonth(month)) then
            nday = nday - daymonth(month)
            month = month + 1
          end if

          write(6,*) 'Year = ', iyear, ' Month = ', month, ' Day = ', nday

          write(flin, '(a, a, i4.4, i2.2, i2.2)' ) trim(fpathin), '.', iyear, month, nday
          write(*,'(a, a)') 'file in :', trim(flin)
          open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)

          read (mtin, rec=1) dat3in(:,:,:)
          do k=1, km
            where(aexl(1:imut, 1:jmut, k) == 0.d0)
              dat3in(1:imut, 1:jmut, k) = 0.0
            end where
          end do
          dat3out(1:imut, 1:jmut, 1:km) = dat3out(1:imut, 1:jmut, 1:km) &
            &    + real(day_pen(nd),4) * dat3in(1:imut, 1:jmut, 1:km)
          !
          close(mtin)
        end do
        write(*,'(a, a)') 'file out :', trim(flout)
        dat3out(1:imut, 1:jmut, 1:km)=dat3out(1:imut, 1:jmut, 1:km)/ real(day_year,4)
        do k=1, km
          where(aexl(1:imut, 1:jmut, k) == 0.d0)
            dat3out(1:imut, 1:jmut, k) = UNDEF
          end where
        end do
        write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:km)
        close(mtout)
      case('u') !-------------------------------------------------
        write(flout, '(a, a, i4.4)' ) trim(fpathout), '.', iyear
        open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
        dat3out(1:imut, 1:jmut, 1:k_max) = 0.0
        do nd = 1, num_data

          nday = nday + day_pen(nd)

          if (nday > daymonth(month)) then
            nday = nday - daymonth(month)
            month = month + 1
          end if

          write(6,*) 'Year = ', iyear, ' Month = ', month, ' Day = ', nday

          write(flin, '(a, a, i4.4, i2.2, i2.2)' ) trim(fpathin), '.', iyear, month, nday
          write(*,'(a, a)') 'file in :', trim(flin)
          open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
          !
          read (mtin, rec=1) dat3in(:,:,1:k_max)
          do k=1, k_max
            where(aexl(1:imut, 1:jmut, 1) == 0.d0)
              dat3in(1:imut, 1:jmut, k) = 0.0
            end where
          end do
          dat3out(1:imut, 1:jmut, 1:k_max) = dat3out(1:imut, 1:jmut, 1:k_max) &
            &       + real(day_pen(nd),4) * dat3in(1:imut, 1:jmut, 1:k_max)
          !
          close(mtin)
        end do
        write(*,'(a, a)') 'file out :', trim(flout)
        dat3out(1:imut, 1:jmut, 1:k_max)=dat3out(1:imut, 1:jmut, 1:k_max) / real(day_year,4)
        do k=1, k_max
          where(aexl(1:imut, 1:jmut, 1) == 0.d0)
            dat3out(1:imut, 1:jmut, k) = UNDEF
          end where
        end do
        write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:k_max)
        close(mtout)
      case('X') !-------------------------------------------------
        write(flout, '(a, a, i4.4)' ) trim(fpathout), '.', iyear
        open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
        dat3out(1:imut, 1:jmut, 1:km) = 0.0
        do nd = 1, num_data

          nday = nday + day_pen(nd)

          if (nday > daymonth(month)) then
            nday = nday - daymonth(month)
            month = month + 1
          end if

          write(6,*) 'Year = ', iyear, ' Month = ', month, ' Day = ', nday

          write(flin, '(a, a, i4.4, i2.2, i2.2)' ) trim(fpathin), '.', iyear, month, nday
          write(*,'(a, a)') 'file in :', trim(flin)
          open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)
          !
          read (mtin, rec=1) dat3in(:,:,:)
          do k=1, km
            where(coefx(1:imut, 1:jmut, k) == 0.d0)
              dat3in(1:imut, 1:jmut, k) = 0.0
            end where
          end do
          dat3out(1:imut, 1:jmut, 1:km) = dat3out(1:imut, 1:jmut, 1:km) &
            &    + real(day_pen(nd),4) * dat3in(1:imut, 1:jmut, 1:km)
          !
          close(mtin)
        end do
        write(*,'(a, a)') 'file out :', trim(flout)
        do k=1, km
          dat3out(1:imut, 1:jmut, k)=dat3out(1:imut, 1:jmut, k) / real(day_year,4)
        end do
        do k=1, km
          where(coefx(1:imut, 1:jmut, k) == 0.d0)
            dat3out(1:imut, 1:jmut, k) = UNDEF
          end where
        end do
        write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:km)
        close(mtout)
      case('x') !-------------------------------------------------
        write(flout, '(a, a, i4.4)' ) trim(fpathout), '.', iyear
        open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
        dat3out(1:imut, 1:jmut, 1:k_max) = 0.0
        do nd = 1, num_data

          nday = nday + day_pen(nd)

          if (nday > daymonth(month)) then
            nday = nday - daymonth(month)
            month = month + 1
          end if

          write(6,*) 'Year = ', iyear, ' Month = ', month, ' Day = ', nday

          write(flin, '(a, a, i4.4, i2.2, i2.2)' ) trim(fpathin), '.', iyear, month, nday

          write(*,'(a, a)') 'file in :', trim(flin)
          open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
          !
          read (mtin, rec=1) dat3in(:,:,1:k_max)
          do k=1, k_max
            where(coefx(1:imut, 1:jmut, 1) == 0.d0)
              dat3in(1:imut, 1:jmut, k) = 0.0
            end where
          end do
          dat3out(1:imut, 1:jmut, 1:k_max) = dat3out(1:imut, 1:jmut, 1:k_max) &
            &       + real(day_pen(nd),4) * dat3in(1:imut, 1:jmut, 1:k_max)
          !
          close(mtin)
        end do
        write(*,'(a, a)') 'file out :', trim(flout)
        dat3out(1:imut, 1:jmut, 1:k_max)=dat3out(1:imut, 1:jmut, 1:k_max) / real(day_year,4)
        do k=1, k_max
          where(coefx(1:imut, 1:jmut, 1) == 0.d0)
            dat3out(1:imut, 1:jmut, k) = UNDEF
          end where
        end do
        write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:k_max)
        close(mtout)
      case('Y') !-------------------------------------------------
        write(flout, '(a, a, i4.4)' ) trim(fpathout), '.', iyear
        open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
        dat3out(1:imut, 1:jmut, 1:km) = 0.0
        do nd = 1, num_data

          nday = nday + day_pen(nd)

          if (nday > daymonth(month)) then
            nday = nday - daymonth(month)
            month = month + 1
          end if

          write(6,*) 'Year = ', iyear, ' Month = ', month, ' Day = ', nday

          write(flin, '(a, a, i4.4, i2.2, i2.2)' ) trim(fpathin), '.', iyear, month, nday

          write(*,'(a, a)') 'file in :', trim(flin)
          open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)

          read (mtin, rec=1) dat3in(:,:,:)
          do k=1, km
            where(coefy(1:imut, 1:jmut, k) == 0.d0)
              dat3in(1:imut, 1:jmut, k) = 0.0
            end where
          end do
          dat3out(1:imut, 1:jmut, 1:km) = dat3out(1:imut, 1:jmut, 1:km) &
            &    + real(day_pen(nd),4) * dat3in(1:imut, 1:jmut, 1:km)
          !
          close(mtin)
        end do
        write(*,'(a, a)') 'file out :', trim(flout)
        dat3out(1:imut, 1:jmut, 1:km)=dat3out(1:imut, 1:jmut, 1:km) / real(day_year,4)
        do k=1, km
          where(coefy(1:imut, 1:jmut, k) == 0.d0)
            dat3out(1:imut, 1:jmut, k) = UNDEF
          end where
        end do
        write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:km)
        close(mtout)
      case('y') !-------------------------------------------------
        write(flout, '(a, a, i4.4)' ) trim(fpathout), '.', iyear
        open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
        dat3out(1:imut, 1:jmut, 1:k_max) = 0.0
        do nd = 1, num_data

          nday = nday + day_pen(nd)

          if (nday > daymonth(month)) then
            nday = nday - daymonth(month)
            month = month + 1
          end if

          write(6,*) 'Year = ', iyear, ' Month = ', month, ' Day = ', nday

          write(flin, '(a, a, i4.4, i2.2, i2.2)' ) trim(fpathin), '.', iyear, month, nday

          write(*,'(a, a)') 'file in :', trim(flin)
          open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
          !
          read (mtin, rec=1) dat3in(:,:,1:k_max)
          do k=1, k_max
           where(coefy(1:imut, 1:jmut, 1) == 0.d0)
              dat3in(1:imut, 1:jmut, k) = 0.0
            end where
          end do
          dat3out(1:imut, 1:jmut, 1:k_max) = dat3out(1:imut, 1:jmut, 1:k_max) &
            &       + real(day_pen(nd),4) * dat3in(1:imut, 1:jmut, 1:k_max)
          !
          close(mtin)
        end do
        write(*,'(a, a)') 'file out :', trim(flout)
        dat3out(1:imut, 1:jmut, 1:k_max)=dat3out(1:imut, 1:jmut, 1:k_max) / real(day_year,4)
        do k=1, k_max
          where(coefy(1:imut, 1:jmut, 1) == 0.d0)
            dat3out(1:imut, 1:jmut, k) = UNDEF
          end where
        end do
        write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:k_max)
        close(mtout)
      case default !-----------------------------------------------
    end select
    !-----------------------------------------------------------------
  end do
  !
!====================================================
end program pen2ann
