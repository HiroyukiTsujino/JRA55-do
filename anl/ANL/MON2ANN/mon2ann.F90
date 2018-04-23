!mon2ann.F90
!====================================================
!
!  Monthly Data to Annual Data
!
!====================================================
program mon2ann
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
  real(4) :: dat3out(imut,jmut,km)
  !
  real(4), save :: daymonth(12)
  real(4), save :: day_year
  !
  character(len=256)    :: flin
  character(len=256)    :: flout
  !
  integer(4), parameter :: mtfile = 80
  integer(4), parameter :: mtin   = 81
  integer(4), parameter :: mtout  =82
  !
  integer(4) :: styear, endyear
  integer(4) :: iyear, m
  integer(4) :: i, j, k, n
  !
  character(len=256)    :: fpathin
  character(len=256)    :: fpathout
  character(len=  1)    :: tuxy
  integer(4)            :: k_max
  !
  namelist /nml_mon2ann/ fltopo styear, endyear, fpathin, fpathout, tuxy, k_max
  !
  !==========================================
  !
  k_max=0
  !
  read(unit=5, nml=nml_mon2ann)
  print *,'fltopo     :', fltopo
  print *,'start year :', styear
  print *,'end   year :', endyear
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
  !
  day_year = 365.0
  !
  !-------------------------
  do iyear = styear, endyear
#ifndef OGCM_Y365D
    if(mod(iyear,4) == 0) then
      if(mod(iyear,100) == 0) then
        if(mod(iyear,400) == 0) then
          daymonth(2) = 29
          day_year = 366.0
        else
          daymonth(2) = 28
          day_year = 365.0
        end if
      else
        daymonth(2) = 29
        day_year = 366.0
      end if
    else
      daymonth(2) = 28
      day_year = 365.0
    end if
#endif /* OGCM_Y365D */
    select case (tuxy)
      case('T') !-------------------------------------------------
        write(flout, '(a, a, i4.4)' ) trim(fpathout), '.', iyear
        open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
        dat3out(1:imut, 1:jmut, 1:km) = 0.0
        do m=1, 12
          write(flin, '(a, a, i4.4, i2.2)' ) trim(fpathin), '.', iyear, m
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
            &             + daymonth(m) * dat3in(1:imut, 1:jmut, 1:km)
          !
          close(mtin)
        end do
        write(*,'(a, a)') 'file out :', trim(flout)
        dat3out(1:imut, 1:jmut, 1:km)=dat3out(1:imut, 1:jmut, 1:km)/day_year
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
        do m=1, 12
          write(flin, '(a, a, i4.4, i2.2)' ) trim(fpathin), '.', iyear, m
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
            &       + daymonth(m) * dat3in(1:imut, 1:jmut, 1:k_max)
          !
          close(mtin)
        end do
        write(*,'(a, a)') 'file out :', trim(flout)
        dat3out(1:imut, 1:jmut, 1:k_max)=dat3out(1:imut, 1:jmut, 1:k_max)/day_year
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
        do m=1, 12
          write(flin, '(a, a, i4.4, i2.2)' ) trim(fpathin), '.', iyear, m
          write(*,'(a, a)') 'file in :', trim(flin)
          open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)
          !
          read (mtin, rec=1) dat3in(:,:,:)
          do k=1, km
            where(aexl(1:imut, 1:jmut, k) == 0.d0)
              dat3in(1:imut, 1:jmut, k) = 0.0
            end where
          end do
          dat3out(1:imut, 1:jmut, 1:km) = dat3out(1:imut, 1:jmut, 1:km) &
            &    + daymonth(m) * dat3in(1:imut, 1:jmut, 1:km)
          !
          close(mtin)
        end do
        write(*,'(a, a)') 'file out :', trim(flout)
        dat3out(1:imut, 1:jmut, 1:km)=dat3out(1:imut, 1:jmut, 1:km)/day_year
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
        do m=1, 12
          write(flin, '(a, a, i4.4, i2.2)' ) trim(fpathin), '.', iyear, m
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
            &       + daymonth(m) * dat3in(1:imut, 1:jmut, 1:k_max)
          !
          close(mtin)
        end do
        write(*,'(a, a)') 'file out :', trim(flout)
        dat3out(1:imut, 1:jmut, 1:k_max)=dat3out(1:imut, 1:jmut, 1:k_max)/day_year
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
        do m=1, 12
          write(flin, '(a, a, i4.4, i2.2)' ) trim(fpathin), '.', iyear, m
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
            &    + daymonth(m) * dat3in(1:imut, 1:jmut, 1:km)
          !
          close(mtin)
        end do
        write(*,'(a, a)') 'file out :', trim(flout)
        do k=1, km
          dat3out(1:imut, 1:jmut, k)=dat3out(1:imut, 1:jmut, k)/day_year
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
        do m=1, 12
          write(flin, '(a, a, i4.4, i2.2)' ) trim(fpathin), '.', iyear, m
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
            &       + daymonth(m) * dat3in(1:imut, 1:jmut, 1:k_max)
          !
          close(mtin)
        end do
        write(*,'(a, a)') 'file out :', trim(flout)
        dat3out(1:imut, 1:jmut, 1:k_max)=dat3out(1:imut, 1:jmut, 1:k_max)/day_year
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
        do m=1, 12
          write(flin, '(a, a, i4.4, i2.2)' ) trim(fpathin), '.', iyear, m
          write(*,'(a, a)') 'file in :', trim(flin)
          open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)
          !
          read (mtin, rec=1) dat3in(:,:,:)
          do k=1, km
            where(coefy(1:imut, 1:jmut, k) == 0.d0)
              dat3in(1:imut, 1:jmut, k) = 0.0
            end where
          end do
          dat3out(1:imut, 1:jmut, 1:km) = dat3out(1:imut, 1:jmut, 1:km) &
            &    + daymonth(m) * dat3in(1:imut, 1:jmut, 1:km)
          !
          close(mtin)
        end do
        write(*,'(a, a)') 'file out :', trim(flout)
        dat3out(1:imut, 1:jmut, 1:km)=dat3out(1:imut, 1:jmut, 1:km)/day_year
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
        do m=1, 12
          write(flin, '(a, a, i4.4, i2.2)' ) trim(fpathin), '.', iyear, m
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
            &       + daymonth(m) * dat3in(1:imut, 1:jmut, 1:k_max)
          !
          close(mtin)
        end do
        write(*,'(a, a)') 'file out :', trim(flout)
        dat3out(1:imut, 1:jmut, 1:k_max)=dat3out(1:imut, 1:jmut, 1:k_max)/day_year
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
end program mon2ann
