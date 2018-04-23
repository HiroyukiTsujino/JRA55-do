!monclim.F90
!====================================================
!
!  Interannual Monthly Data to Monthly Climatology
!
!====================================================
program monclim
  !
  use oc_mod_param, only : &
  &   imut, jmut, km
  !
  use oc_structure, only : &
  &  read_topo,            &
  &  aexl, atexl
  !
  implicit none
  !
  real(4), parameter :: UNDEF = -9.99e33
  !
  integer(4), parameter :: mtfile = 80
  integer(4), parameter :: mtin   = 81
  integer(4), parameter :: mtout  =82
  !
  real(4), allocatable :: datin(:,:,:)
  real(4), allocatable :: datout(:,:,:)
  real(8), allocatable :: data(:,:,:)
  !
  character(len=256)    :: fincore
  character(len=256)    :: foutcore
  character(len=256)    :: fltopo
  character(len=256)    :: flin
  character(len=256)    :: flout
  character(len=4)      :: tuxy
  integer(4) :: dnum  !  for 2D data
  !
  integer(4) :: styear, endyear
  integer(4) :: iyear, m
  integer(4) :: i, j, k, n
  real(8)    :: nyear
  !
  namelist /nml_monclim/ fincore, foutcore, fltopo, styear, endyear, tuxy, dnum
  !
  !==========================================
  !
  read(unit=5, nml_monclim)
  write(*, *) 'fincore : ',  fincore
  write(*, *) 'foutcore: ',  foutcore
  write(*, *) 'fltopo  : ',  fltopo
  write(*, *) 'styear : ', styear
  write(*, *) 'endyear: ', endyear
  write(*, *) 'tuxy   : ', tuxy
  write(*, *) 'dnum   : ', dnum
  !------------------------------------------
  !
  call read_topo(fltopo)
  !
  if(tuxy == 't' .or. tuxy == 'u') then
    allocate(datin(1:imut, 1:jmut, 1:dnum))
    allocate(datout(1:imut, 1:jmut, 1:dnum))
    allocate(data(1:imut, 1:jmut, 1:dnum))
  else
    allocate(datin(1:imut, 1:jmut, 1:km))
    allocate(datout(1:imut, 1:jmut, 1:km))
    allocate(data(1:imut, 1:jmut, 1:km))
  end if
  !
  !-------------------------
  select case(tuxy)
    !-----------------------------------------------------------------
    case('u') !  Surface Flux at UV grids
      do m=1, 12
        write(flout, '(a, a, i2.2)' ) trim(foutcore), '.', m
        write(*,'(a, a)') 'file out :', trim(flout)
        open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*dnum)
        !
        datout(1:imut, 1:jmut, 1:dnum) = 0.0
        nyear=0.d0
        data(1:imut, 1:jmut, 1:dnum) = 0.d0
        do iyear = styear, endyear
          write(flin, '(a, a, i4.4, i2.2)' ) trim(fincore), '.', iyear, m
          write(*,'(a, a)') 'file in :', trim(flin)
          open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*dnum)
          !
          read (mtin, rec=1) datin(1:imut,1:jmut,1:dnum)
          do n = 1, dnum
            where(aexl(1:imut, 1:jmut, 1) == 0.d0)
              datin(1:imut, 1:jmut, n) = 0.0
            end where
          end do
          data(1:imut, 1:jmut, 1:dnum) = data(1:imut, 1:jmut, 1:dnum) &
            &    + real(datin(1:imut, 1:jmut, 1:dnum), 8)
          nyear = nyear + 1.d0
          close(mtin)
        end do
        !
        datout(1:imut, 1:jmut, 1:dnum) = real(data(1:imut, 1:jmut, 1:dnum)/nyear, 4)
        do n = 1, dnum
          where(aexl(1:imut, 1:jmut, 1) == 0.d0)
            datout(1:imut, 1:jmut, n) = UNDEF
          end where
        end do
        write(mtout, rec=1) datout(1:imut, 1:jmut, 1:dnum)
        close(mtout)
      end do
    !-----------------------------------------------------------------
    case('t') !  Surface Flux at TS grids
      do m=1, 12
        write(flout, '(a, a, i2.2)' ) trim(foutcore), '.', m
        write(*,'(a, a)') 'file out :', trim(flout)
        open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*dnum)
        !
        datout(1:imut, 1:jmut, 1:dnum) = 0.0
        nyear=0.d0
        data(1:imut, 1:jmut, 1:dnum) = 0.d0
        do iyear = styear, endyear
          write(flin, '(a, a, i4.4, i2.2)' ) trim(fincore), '.', iyear, m
          write(*,'(a, a)') 'file in :', trim(flin)
          open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*dnum)
          !
          read (mtin, rec=1) datin(1:imut,1:jmut,1:dnum)
          do n = 1, dnum
            where(atexl(1:imut, 1:jmut, 1) == 0.d0)
              datin(1:imut, 1:jmut, n) = 0.0
            end where
          end do
          data(1:imut, 1:jmut, 1:dnum) = data(1:imut, 1:jmut, 1:dnum) &
            &    + real(datin(1:imut, 1:jmut, 1:dnum), 8)
          nyear = nyear + 1.d0
          close(mtin)
        end do
        !
        datout(1:imut, 1:jmut, 1:dnum) = real(data(1:imut, 1:jmut, 1:dnum)/nyear, 4)
        do n = 1, dnum
          where(atexl(1:imut, 1:jmut, 1) == 0.d0)
            datout(1:imut, 1:jmut, n) = UNDEF
          end where
        end do
        write(mtout, rec=1) datout(1:imut, 1:jmut, 1:dnum)
        close(mtout)
      end do
    !-----------------------------------------------------------------
    case('U') !  3D data UV grids
      do m=1, 12
        write(flout, '(a, a, i2.2)' ) trim(foutcore), '.', m
        write(*,'(a, a)') 'file out :', trim(flout)
        open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
        !
        datout(1:imut, 1:jmut, 1:km) = 0.0
        nyear=0.d0
        data(1:imut, 1:jmut, 1:km) = 0.d0
        do iyear = styear, endyear
          write(flin, '(a, a, i4.4, i2.2)' ) trim(fincore), '.', iyear, m
          write(*,'(a, a)') 'file in :', trim(flin)
          open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)
          !
          read (mtin, rec=1) datin(1:imut,1:jmut,1:km)
          do k = 1, km
            where(aexl(1:imut, 1:jmut, k) == 0.d0)
              datin(1:imut, 1:jmut, k) = 0.0
            end where
          end do
          data(1:imut, 1:jmut, 1:km) = data(1:imut, 1:jmut, 1:km) &
            &    + real(datin(1:imut, 1:jmut, 1:km), 8)
          nyear = nyear + 1.d0
          close(mtin)
        end do
        !
        datout(1:imut, 1:jmut, 1:km) = real(data(1:imut, 1:jmut, 1:km)/nyear, 4)
        do k = 1, km
          where(aexl(1:imut, 1:jmut, k) == 0.d0)
            datout(1:imut, 1:jmut, k) = UNDEF
          end where
        end do
        write(mtout, rec=1) datout(1:imut, 1:jmut, 1:km)
        close(mtout)
      end do
    !-----------------------------------------------------------------
    case('T') !  3D data TS grids
      do m=1, 12
        write(flout, '(a, a, i2.2)' ) trim(foutcore), '.', m
        write(*,'(a, a)') 'file out :', trim(flout)
        open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
        !
        datout(1:imut, 1:jmut, 1:km) = 0.0
        nyear=0.d0
        data(1:imut, 1:jmut, 1:km) = 0.d0
        do iyear = styear, endyear
          write(flin, '(a, a, i4.4, i2.2)' ) trim(fincore), '.', iyear, m
          write(*,'(a, a)') 'file in :', trim(flin)
          open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)
          !
          read (mtin, rec=1) datin(1:imut,1:jmut,1:km)
          do k = 1, km
            where(atexl(1:imut, 1:jmut, k) == 0.d0)
              datin(1:imut, 1:jmut, k) = 0.0
            end where
          end do
          data(1:imut, 1:jmut, 1:km) = data(1:imut, 1:jmut, 1:km) &
            &    + real(datin(1:imut, 1:jmut, 1:km), 8)
          nyear = nyear + 1.d0
          close(mtin)
        end do
        !
        datout(1:imut, 1:jmut, 1:km) = real(data(1:imut, 1:jmut, 1:km)/nyear, 4)
        do k = 1, km
          where(atexl(1:imut, 1:jmut, k) == 0.d0)
            datout(1:imut, 1:jmut, k) = UNDEF
          end where
        end do
        write(mtout, rec=1) datout(1:imut, 1:jmut, 1:km)
        close(mtout)
      end do
    !-----------------------------------------------------------------
    case('W') !  3D data W point
      do m=1, 12
        write(flout, '(a, a, i2.2)' ) trim(foutcore), '.', m
        write(*,'(a, a)') 'file out :', trim(flout)
        open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
        !
        datout(1:imut, 1:jmut, 1:km) = 0.0
        nyear=0.d0
        data(1:imut, 1:jmut, 1:km) = 0.d0
        do iyear = styear, endyear
          write(flin, '(a, a, i4.4, i2.2)' ) trim(fincore), '.', iyear, m
          write(*,'(a, a)') 'file in :', trim(flin)
          open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)
          !
          read (mtin, rec=1) datin(1:imut,1:jmut,1:km)
          datin(1:imut, 1:jmut, km) = 0.0
          do k = 1, km-1
            where(atexl(1:imut, 1:jmut, k+1) == 0.d0)
              datin(1:imut, 1:jmut, k) = 0.0
            end where
          end do
          data(1:imut, 1:jmut, 1:km) = data(1:imut, 1:jmut, 1:km) &
            &    + real(datin(1:imut, 1:jmut, 1:km), 8)
          nyear = nyear + 1.d0
          close(mtin)
        end do
        !
        datout(1:imut, 1:jmut, 1:km) = real(data(1:imut, 1:jmut, 1:km)/nyear, 4)
        do k = 1, km
          where(atexl(1:imut, 1:jmut, k+1) == 0.d0)
            datout(1:imut, 1:jmut, k) = UNDEF
          end where
        end do
        write(mtout, rec=1) datout(1:imut, 1:jmut, 1:km)
        close(mtout)
      end do
      !-----------------------------------------------------------------
    case default
      write(*,*) 'tuxy is not correct !'
  end select
  !
!====================================================
end program monclim
