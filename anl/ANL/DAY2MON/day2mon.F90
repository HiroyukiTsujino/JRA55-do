!day2mon.F90
!====================================================
!
!  Daily Data to Monthly Data
!
!====================================================
program day2mon
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
  real(4) :: dat3in(imut, jmut, km) ! 読み出し用3次元配列
  !
  real(4) :: dat3out(imut,jmut,km)
  !
  real(4), save :: daymonth(12)
  !
  character(len=256)    :: fpathin
  character(len=256)    :: flin
  character(len=256)    :: fpathout
  character(len=256)    :: flout
  character(len=  1)    :: tuxy
  integer(4)            :: k_max
  !
  integer(4), parameter :: mtfile = 80
  integer(4), parameter :: mtin   = 81
  integer(4), parameter :: mtout  =82
  !
  real(4), save :: UNDEF = -9.99e33
  !
  integer(4) :: styear, endyear
  integer(4) :: iyear, m, d
  integer(4) :: i, j, k, n
  !
  namelist /nml_day2mon/ fltopo, styear, endyear, fpathin, fpathout, tuxy, k_max, UNDEF
  !
  !==========================================
  !
  k_max=0
  !
  read(unit=5, nml=nml_day2mon)
  print *,'start year :', styear
  print *,'end   year :', endyear
  print *,'fpath in   :', fpathin
  print *,'fpath out  :', fpathout
  print *,'tuxy       :', tuxy
  print *,'k_max      :', k_max
  print *,'UNDEF      :', UNDEF
  !
  call read_topo(fltopo)
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
  !-------------------------
  do iyear = styear, endyear
#ifndef OGCM_Y365D
    if(mod(iyear,4) == 0) then
      if(mod(iyear,100) == 0) then
        if(mod(iyear,400) == 0) then
          daymonth(2) = 29
        else
          daymonth(2) = 28
        end if
      else
        daymonth(2) = 29
      end if
    else
      daymonth(2) = 28
    end if
#endif /* OGCM_Y365D */
    select case (tuxy)
      case('T') !-------------------------------------------------
        do m=1, 12
          write(flout, '(a, a, i4.4, i2.2)' ) trim(fpathout), '.', iyear, m
          open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
          dat3out(1:imut, 1:jmut, 1:km) = 0.0
          do d=1, daymonth(m)
            write(flin, '(a, a, i4.4, i2.2, i2.2)' ) trim(fpathin), '.', iyear, m, d
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
              &                            + dat3in(1:imut, 1:jmut, 1:km)
            !
            close(mtin)
          end do
          write(*,'(a, a)') 'file out :', trim(flout)
          dat3out(1:imut, 1:jmut, 1:km)=dat3out(1:imut, 1:jmut, 1:km)/real(daymonth(m),4)
          do k=1, km
            where(atexl(1:imut, 1:jmut, k) == 0.d0)
              dat3out(1:imut, 1:jmut, k) = UNDEF
            end where
          end do
          write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:km)
          close(mtout)
        end do
      case('t') !-------------------------------------------------
        do m=1, 12
          write(flout, '(a, a, i4.4, i2.2)' ) trim(fpathout), '.', iyear, m
          open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
          dat3out(1:imut, 1:jmut, 1:k_max) = 0.0
          do d=1, daymonth(m)
            write(flin, '(a, a, i4.4, i2.2, i2.2)' ) trim(fpathin), '.', iyear, m, d
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
              &                               + dat3in(1:imut, 1:jmut, 1:k_max)
            !
            close(mtin)
          end do
          write(*,'(a, a)') 'file out :', trim(flout)
          dat3out(1:imut, 1:jmut, 1:k_max)=dat3out(1:imut, 1:jmut, 1:k_max)/real(daymonth(m),4)
          do k=1, k_max
            where(atexl(1:imut, 1:jmut, 1) == 0.d0)
              dat3out(1:imut, 1:jmut, k) = UNDEF
            end where
          end do
          write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:k_max)
          close(mtout)
        end do
      case('U') !-------------------------------------------------
        do m=1, 12
          write(flout, '(a, a, i4.4, i2.2)' ) trim(fpathout), '.', iyear, m
          open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
          dat3out(1:imut, 1:jmut, 1:km) = 0.0
          do d=1, daymonth(m)
            write(flin, '(a, a, i4.4, i2.2, i2.2)' ) trim(fpathin), '.', iyear, m, d
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
              &                            + dat3in(1:imut, 1:jmut, 1:km)
            !
            close(mtin)
          end do
          write(*,'(a, a)') 'file out :', trim(flout)
          dat3out(1:imut, 1:jmut, 1:km)=dat3out(1:imut, 1:jmut, 1:km)/real(daymonth(m),4)
          do k=1, km
            where(aexl(1:imut, 1:jmut, k) == 0.d0)
              dat3out(1:imut, 1:jmut, k) = UNDEF
            end where
          end do
          write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:km)
          close(mtout)
        end do
      case('u') !-------------------------------------------------
        do m=1, 12
          write(flout, '(a, a, i4.4, i2.2)' ) trim(fpathout), '.', iyear, m
          open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
          dat3out(1:imut, 1:jmut, 1:k_max) = 0.0
          do d=1, daymonth(m)
            write(flin, '(a, a, i4.4, i2.2, i2.2)' ) trim(fpathin), '.', iyear, m, d
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
              &                               + dat3in(1:imut, 1:jmut, 1:k_max)
            !
            close(mtin)
          end do
          write(*,'(a, a)') 'file out :', trim(flout)
          dat3out(1:imut, 1:jmut, 1:k_max)=dat3out(1:imut, 1:jmut, 1:k_max)/real(daymonth(m),4)
          do k=1, k_max
            where(aexl(1:imut, 1:jmut, 1) == 0.d0)
              dat3out(1:imut, 1:jmut, k) = UNDEF
            end where
          end do
          write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:k_max)
          close(mtout)
        end do
      case('X') !-------------------------------------------------
        do m=1, 12
          write(flout, '(a, a, i4.4, i2.2)' ) trim(fpathout), '.', iyear, m
          open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
          dat3out(1:imut, 1:jmut, 1:km) = 0.0
          do d=1, daymonth(m)
            write(flin, '(a, a, i4.4, i2.2, i2.2)' ) trim(fpathin), '.', iyear, m, d
            write(*,'(a, a)') 'file in :', trim(flin)
            open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)
            !
            read (mtin, rec=1) dat3in(:,:,:)
            do k=1, km
              where(dat3in(1:imut, 1:jmut, k) == UNDEF)
                dat3in(1:imut, 1:jmut, k) = 0.0
              end where
            end do
            dat3out(1:imut, 1:jmut, 1:km) = dat3out(1:imut, 1:jmut, 1:km) &
              &                            + dat3in(1:imut, 1:jmut, 1:km)
            !
            close(mtin)
          end do
          write(*,'(a, a)') 'file out :', trim(flout)
          dat3out(1:imut, 1:jmut, 1:km)=dat3out(1:imut, 1:jmut, 1:km)/real(daymonth(m),4)
          do k=1, km
            where(coefx(1:imut, 1:jmut, k) == 0.d0)
              dat3out(1:imut, 1:jmut, k) = UNDEF
            end where
          end do
          write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:km)
          close(mtout)
        end do
      case('x') !-------------------------------------------------
        do m=1, 12
          write(flout, '(a, a, i4.4, i2.2)' ) trim(fpathout), '.', iyear, m
          open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
          dat3out(1:imut, 1:jmut, 1:k_max) = 0.0
          do d=1, daymonth(m)
            write(flin, '(a, a, i4.4, i2.2, i2.2)' ) trim(fpathin), '.', iyear, m, d
            write(*,'(a, a)') 'file in :', trim(flin)
            open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
            !
            read (mtin, rec=1) dat3in(:,:,1:k_max)
            do k=1, k_max
              where(dat3in(1:imut, 1:jmut, 1) == UNDEF)
                dat3in(1:imut, 1:jmut, k) = 0.0
              end where
            end do
            dat3out(1:imut, 1:jmut, 1:k_max) = dat3out(1:imut, 1:jmut, 1:k_max) &
              &                               + dat3in(1:imut, 1:jmut, 1:k_max)
            !
            close(mtin)
          end do
          write(*,'(a, a)') 'file out :', trim(flout)
          dat3out(1:imut, 1:jmut, 1:k_max)=dat3out(1:imut, 1:jmut, 1:k_max)/real(daymonth(m),4)
          do k=1, k_max
            where(coefx(1:imut, 1:jmut, 1) == 0.d0)
              dat3out(1:imut, 1:jmut, k) = UNDEF
            end where
          end do
          write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:k_max)
          close(mtout)
        end do
      case('Y') !-------------------------------------------------
        do m=1, 12
          write(flout, '(a, a, i4.4, i2.2)' ) trim(fpathout), '.', iyear, m
          open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
          dat3out(1:imut, 1:jmut, 1:km) = 0.0
          do d=1, daymonth(m)
            write(flin, '(a, a, i4.4, i2.2, i2.2)' ) trim(fpathin), '.', iyear, m, d
            write(*,'(a, a)') 'file in :', trim(flin)
            open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)
            !
            read (mtin, rec=1) dat3in(:,:,:)
            do k=1, km
              where(dat3in(1:imut, 1:jmut, k) == UNDEF)
                dat3in(1:imut, 1:jmut, k) = 0.0
              end where
            end do
            dat3out(1:imut, 1:jmut, 1:km) = dat3out(1:imut, 1:jmut, 1:km) &
              &                            + dat3in(1:imut, 1:jmut, 1:km)
            !
            close(mtin)
          end do
          write(*,'(a, a)') 'file out :', trim(flout)
          dat3out(1:imut, 1:jmut, 1:km)=dat3out(1:imut, 1:jmut, 1:km)/real(daymonth(m),4)
          do k=1, km
            where(coefy(1:imut, 1:jmut, k) == 0.d0)
              dat3out(1:imut, 1:jmut, k) = UNDEF
            end where
          end do
          write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:km)
          close(mtout)
        end do
      case('y') !-------------------------------------------------
        do m=1, 12
          write(flout, '(a, a, i4.4, i2.2)' ) trim(fpathout), '.', iyear, m
          open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
          dat3out(1:imut, 1:jmut, 1:k_max) = 0.0
          do d=1, daymonth(m)
            write(flin, '(a, a, i4.4, i2.2, i2.2)' ) trim(fpathin), '.', iyear, m, d
            write(*,'(a, a)') 'file in :', trim(flin)
            open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
            !
            read (mtin, rec=1) dat3in(:,:,1:k_max)
            do k=1, k_max
              where(dat3in(1:imut, 1:jmut, 1) == UNDEF)
                dat3in(1:imut, 1:jmut, k) = 0.0
              end where
            end do
            dat3out(1:imut, 1:jmut, 1:k_max) = dat3out(1:imut, 1:jmut, 1:k_max) &
              &                               + dat3in(1:imut, 1:jmut, 1:k_max)
            !
            close(mtin)
          end do
          write(*,'(a, a)') 'file out :', trim(flout)
          dat3out(1:imut, 1:jmut, 1:k_max)=dat3out(1:imut, 1:jmut, 1:k_max)/real(daymonth(m),4)
          do k=1, k_max
            where(coefy(1:imut, 1:jmut, 1) == 0.d0)
              dat3out(1:imut, 1:jmut, k) = UNDEF
            end where
          end do
          write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:k_max)
          close(mtout)
        end do
      case default !-----------------------------------------------
    end select
    !-----------------------------------------------------------------
  end do
  !
!====================================================
end program day2mon
