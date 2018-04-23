!regcor.F90
!====================================================
!
!  Interannual Monthly Data to Monthly Climatology
!
!====================================================
program regcor
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
  integer(4), parameter :: mtin   = 81
  integer(4), parameter :: mtout  =82
  !
  real(4), allocatable :: series_in(:)
  real(8)              :: mean_series
  real(8)              :: var_series
  real(8)              :: stddev
  real(8)              :: inv_stddev
  real(8), allocatable :: nrm_series(:)
  real(4), allocatable :: series_out(:)
  !
  real(4), allocatable :: data_in(:,:,:)
  real(8), allocatable :: data(:,:,:)
  real(8), allocatable :: mean_data(:,:,:)
  real(8), allocatable :: variance(:,:,:)
  !
  real(8), allocatable :: covarian(:,:,:)
  real(8), allocatable :: corr(:,:,:)
  real(8), allocatable :: regr(:,:,:)
  real(4), allocatable :: data_out(:,:,:)
  !
  character(len=256)   :: fseriescore
  character(len=256)   :: fseries
  character(len=256)   :: fseriesout
  !
  character(len=256)   :: fincore
  character(len=256)   :: flin
  character(len=256)   :: foutcore
  character(len=256)   :: flout
  !
  character(len=256)   :: fltopo
  character(len=4)     :: tuxy
  integer(4), save :: snum = 1  !  for sequential data
  integer(4), save :: dnum = 1  !  for 2D data
  !
  integer(4) :: styear, endyear, lag
  integer(4) :: numdata
  real(8)    :: rnumdata
  real(8)    :: inv_numdata
  integer(4) :: iyear, m
  integer(4) :: i, j, k, n
  real(8)    :: nyear
  !
  namelist /nml_regcor/ fseriescore, snum, fincore, foutcore, tuxy, dnum, fltopo, &
    &                   styear, endyear, lag
  !
  !==========================================
  !
  read(unit=5, nml_regcor)
  write(*, *) 'fseriescore : ',  fseriescore
  write(*, *) 'snum   : ', snum
  write(*, *) 'fincore     : ',  fincore
  write(*, *) 'foutcore    : ',  foutcore
  write(*, *) 'tuxy   : ', tuxy
  write(*, *) 'dnum   : ', dnum
  write(*, *) 'fltopo : ',  fltopo
  write(*, *) 'styear   : ', styear
  write(*, *) 'endyear  : ', endyear
  write(*, *) 'lag      : ', lag
  write(*, *) '-----------'
  numdata = endyear - styear +1
  write(*, *) 'numdata  : ', numdata
  rnumdata = real(numdata, 8)
  inv_numdata = 1.d0 / rnumdata
  !------------------------------------------
  !
  call read_topo(fltopo)
  !
  if(tuxy == 't' .or. tuxy == 'u') then
    allocate(data_in(1:imut, 1:jmut, 1:dnum))
    allocate(data_out(1:imut, 1:jmut, 1:dnum))
    allocate(data(1:imut, 1:jmut, 1:dnum))
    allocate(mean_data(1:imut, 1:jmut, 1:dnum))
    allocate(variance(1:imut, 1:jmut, 1:dnum))
    allocate(covarian(1:imut, 1:jmut, 1:dnum))
    allocate(corr(1:imut, 1:jmut, 1:dnum))
    allocate(regr(1:imut, 1:jmut, 1:dnum))
  else
    allocate(data_in(1:imut, 1:jmut, 1:km))
    allocate(data_out(1:imut, 1:jmut, 1:km))
    allocate(data(1:imut, 1:jmut, 1:km))
    allocate(mean_data(1:imut, 1:jmut, 1:km))
    allocate(variance(1:imut, 1:jmut, 1:km))
    allocate(covarian(1:imut, 1:jmut, 1:km))
    allocate(corr(1:imut, 1:jmut, 1:km))
    allocate(regr(1:imut, 1:jmut, 1:km))
  end if
  !-----------------------------
  allocate(series_in(1:numdata))
  allocate(nrm_series(1:numdata))
  allocate(series_out(1:numdata))
  !-----------------------------
  !
  mean_series = 0.d0
  do iyear = styear, endyear
    write(fseries, '(a, a, i4.4)' ) trim(fseriescore), '.', iyear
    write(*,'(a, a)') 'series file in :', trim(fseries)
    open(mtin, file=fseries, form='unformatted', access='direct', recl=4)
    read (mtin, rec=snum) series_in(iyear)
    mean_series = mean_series + real(series_in(iyear), 8)
    close(mtin)
  end do
  mean_series = mean_series * inv_numdata
  write(*, *) 'mean : ', mean_series
  !
  var_series = 0.d0
  do iyear = styear, endyear
    var_series = var_series + (real(series_in(iyear), 8) - mean_series)**2
  end do
  var_series = var_series * inv_numdata
  stddev = sqrt(var_series) 
  write(*, *) 'standart deviation : ', stddev
  inv_stddev = 1.d0 / stddev
  !
  do iyear = styear, endyear
    write(fseriesout, '(a, a, i4.4)' ) trim(fseriescore), '.normalized.', iyear
    write(*,'(a, a)') 'series file out :', trim(fseriesout)
    open(mtout, file=fseriesout, form='unformatted', access='direct', recl=4)
    nrm_series(iyear) = (real(series_in(iyear), 8) -mean_series) * inv_stddev
    write(mtout, rec = 1) real(nrm_series(iyear), 4)
    write(*, *)                nrm_series(iyear)
    close(mtout)
  end do
  !
  !-------------------------
  select case(tuxy)
    !-----------------------------------------------------------------
    case('u') !  Surface Data at UV grids
      mean_data(1:imut, 1:jmut, 1:dnum) = 0.d0
      do iyear = styear, endyear
        write(flin, '(a, a, i4.4)' ) trim(fincore), '.', iyear + lag
        write(*,'(a, a)') 'file in :', trim(flin)
        open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*dnum)
        !
        read (mtin, rec=1) data_in(1:imut,1:jmut,1:dnum)
        do n = 1, dnum
          where(aexl(1:imut, 1:jmut, 1) == 0.d0)
            data_in(1:imut, 1:jmut, n) = 0.0
          end where
        end do
        mean_data(1:imut, 1:jmut, 1:dnum) = mean_data(1:imut, 1:jmut, 1:dnum) &
          &    + real(data_in(1:imut, 1:jmut, 1:dnum), 8)
        close(mtin)
      end do
      mean_data(1:imut, 1:jmut, 1:dnum) = mean_data(1:imut, 1:jmut, 1:dnum) * inv_numdata
      !
      variance(1:imut, 1:jmut, 1:dnum) = 0.d0
      covarian(1:imut, 1:jmut, 1:dnum) = 0.d0
      do iyear = styear, endyear
        write(flin, '(a, a, i4.4)' ) trim(fincore), '.', iyear + lag
        write(*,'(a, a)') 'file in :', trim(flin)
        open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*dnum)
        !
        read (mtin, rec=1) data_in(1:imut,1:jmut,1:dnum)
        close(mtin)
        do n = 1, dnum
          where(aexl(1:imut, 1:jmut, 1) == 0.d0)
            data_in(1:imut, 1:jmut, n) = 0.0
          end where
        end do
        data(1:imut, 1:jmut, 1:dnum) = real(data_in(1:imut, 1:jmut, 1:dnum), 8)  &
          &                          - mean_data(1:imut, 1:jmut, 1:dnum)
        variance(1:imut, 1:jmut, 1:dnum) = variance(1:imut, 1:jmut, 1:dnum)   &
          &   + data(1:imut, 1:jmut, 1:dnum) * data(1:imut, 1:jmut, 1:dnum)
        covarian(1:imut, 1:jmut, 1:dnum) = covarian(1:imut, 1:jmut, 1:dnum)   &
          &   +      nrm_series(iyear) * data(1:imut, 1:jmut, 1:dnum)
      end do
      covarian(1:imut, 1:jmut, 1:dnum) = covarian(1:imut, 1:jmut, 1:dnum) * inv_numdata
      variance(1:imut, 1:jmut, 1:dnum) = variance(1:imut, 1:jmut, 1:dnum) * inv_numdata
      !
      write(flout, '(a, a, i4.4)' ) trim(foutcore), '.lag', lag +1000
      write(*,'(a, a)') 'file out :', trim(flout)
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*dnum)
      !
      !  regression
      !
      data_out(1:imut, 1:jmut, 1:dnum) = real(covarian(1:imut, 1:jmut, 1:dnum), 4)
      do n = 1, dnum
        where(aexl(1:imut, 1:jmut, 1) == 0.d0)
          data_out(1:imut, 1:jmut, n) = UNDEF
        end where
      end do
      write(mtout, rec=1) data_out(1:imut, 1:jmut, 1:dnum)
      !
      !  correlation
      !
      do n = 1, dnum
        do j = 1, jmut
          do i = 1, imut
            if(variance(i, j, n) > 0.0d0) then
              data_out(i, j, n) = real(covarian(i, j, n) / sqrt(variance(i, j, n)), 4)
            else
              if(aexl(i, j, 1) == 0.d0) then
                data_out(i, j, n) = UNDEF
              else
                data_out(i, j, n) = 0.0
              end if
            end if
          end do
        end do
      end do
      write(mtout, rec=2) data_out(1:imut, 1:jmut, 1:dnum)
      close(mtout)
    !-----------------------------------------------------------------
    case('t') !  Surface Data at TS grids
      mean_data(1:imut, 1:jmut, 1:dnum) = 0.d0
      do iyear = styear, endyear
        write(flin, '(a, a, i4.4)' ) trim(fincore), '.', iyear + lag
        write(*,'(a, a)') 'file in :', trim(flin)
        open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*dnum)
        !
        read (mtin, rec=1) data_in(1:imut,1:jmut,1:dnum)
        do n = 1, dnum
          where(atexl(1:imut, 1:jmut, 1) == 0.d0)
            data_in(1:imut, 1:jmut, n) = 0.0
          end where
        end do
        mean_data(1:imut, 1:jmut, 1:dnum) = mean_data(1:imut, 1:jmut, 1:dnum) &
          &    + real(data_in(1:imut, 1:jmut, 1:dnum), 8)
        close(mtin)
      end do
      mean_data(1:imut, 1:jmut, 1:dnum) = mean_data(1:imut, 1:jmut, 1:dnum) * inv_numdata
      !
      variance(1:imut, 1:jmut, 1:dnum) = 0.d0
      covarian(1:imut, 1:jmut, 1:dnum) = 0.d0
      do iyear = styear, endyear
        write(flin, '(a, a, i4.4)' ) trim(fincore), '.', iyear + lag
        write(*,'(a, a)') 'file in :', trim(flin)
        open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*dnum)
        !
        read (mtin, rec=1) data_in(1:imut,1:jmut,1:dnum)
        close(mtin)
        do n = 1, dnum
          where(atexl(1:imut, 1:jmut, 1) == 0.d0)
            data_in(1:imut, 1:jmut, n) = 0.0
          end where
        end do
        data(1:imut, 1:jmut, 1:dnum) = real(data_in(1:imut, 1:jmut, 1:dnum), 8)  &
          &                          - mean_data(1:imut, 1:jmut, 1:dnum)
        variance(1:imut, 1:jmut, 1:dnum) = variance(1:imut, 1:jmut, 1:dnum)   &
          &   + data(1:imut, 1:jmut, 1:dnum) * data(1:imut, 1:jmut, 1:dnum)
        covarian(1:imut, 1:jmut, 1:dnum) = covarian(1:imut, 1:jmut, 1:dnum)   &
          &   +      nrm_series(iyear) * data(1:imut, 1:jmut, 1:dnum)
      end do
      covarian(1:imut, 1:jmut, 1:dnum) = covarian(1:imut, 1:jmut, 1:dnum) * inv_numdata
      variance(1:imut, 1:jmut, 1:dnum) = variance(1:imut, 1:jmut, 1:dnum) * inv_numdata
      !
      write(flout, '(a, a, i4.4)' ) trim(foutcore), '.lag', lag +1000
      write(*,'(a, a)') 'file out :', trim(flout)
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*dnum)
      !
      !  regression
      !
      data_out(1:imut, 1:jmut, 1:dnum) = real(covarian(1:imut, 1:jmut, 1:dnum), 4)
      do n = 1, dnum
        where(atexl(1:imut, 1:jmut, 1) == 0.d0)
          data_out(1:imut, 1:jmut, n) = UNDEF
        end where
      end do
      write(mtout, rec=1) data_out(1:imut, 1:jmut, 1:dnum)
      !
      !  correlation
      !
      do n = 1, dnum
        do j = 1, jmut
          do i = 1, imut
            if(variance(i, j, n) > 0.d0) then
              data_out(i, j, n) = real(covarian(i, j, n) / sqrt(variance(i, j, n)), 4)
            else
              if(atexl(i, j, 1) == 0.d0) then
                data_out(i, j, n) = UNDEF
              else
                data_out(i, j, n) = 0.0
              end if
            end if
          end do
        end do
      end do
      write(mtout, rec=2) data_out(1:imut, 1:jmut, 1:dnum)
      close(mtout)
    !-----------------------------------------------------------------
    case('U') !  3D data at UV grids
      mean_data(1:imut, 1:jmut, 1:km) = 0.d0
      do iyear = styear, endyear
        write(flin, '(a, a, i4.4)' ) trim(fincore), '.', iyear + lag
        write(*,'(a, a)') 'file in :', trim(flin)
        open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)
        !
        read (mtin, rec=1) data_in(1:imut,1:jmut,1:km)
        do k = 1, km
          where(aexl(1:imut, 1:jmut, k) == 0.d0)
            data_in(1:imut, 1:jmut, k) = 0.0
          end where
        end do
        mean_data(1:imut, 1:jmut, 1:km) = mean_data(1:imut, 1:jmut, 1:km) &
          &    + real(data_in(1:imut, 1:jmut, 1:km), 8)
        close(mtin)
      end do
      mean_data(1:imut, 1:jmut, 1:km) = mean_data(1:imut, 1:jmut, 1:km) * inv_numdata
      !
      variance(1:imut, 1:jmut, 1:km) = 0.d0
      covarian(1:imut, 1:jmut, 1:km) = 0.d0
      do iyear = styear, endyear
        write(flin, '(a, a, i4.4)' ) trim(fincore), '.', iyear + lag
        write(*,'(a, a)') 'file in :', trim(flin)
        open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)
        !
        read (mtin, rec=1) data_in(1:imut,1:jmut,1:km)
        close(mtin)
        do k = 1, km
          where(aexl(1:imut, 1:jmut, k) == 0.d0)
            data_in(1:imut, 1:jmut, k) = 0.0
          end where
        end do
        data(1:imut, 1:jmut, 1:km) = real(data_in(1:imut, 1:jmut, 1:km), 8)  &
          &                           - mean_data(1:imut, 1:jmut, 1:km)
        variance(1:imut, 1:jmut, 1:km) = variance(1:imut, 1:jmut, 1:km)   &
          &   + data(1:imut, 1:jmut, 1:km) * data(1:imut, 1:jmut, 1:km)
        covarian(1:imut, 1:jmut, 1:km) = covarian(1:imut, 1:jmut, 1:km)   &
          &   +      nrm_series(iyear) * data(1:imut, 1:jmut, 1:km)
      end do
      covarian(1:imut, 1:jmut, 1:km) = covarian(1:imut, 1:jmut, 1:km) * inv_numdata
      variance(1:imut, 1:jmut, 1:km) = variance(1:imut, 1:jmut, 1:km) * inv_numdata
      !
      write(flout, '(a, a, i4.4)' ) trim(foutcore), '.lag', lag +1000
      write(*,'(a, a)') 'file out :', trim(flout)
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
      !
      !  regression
      !
      data_out(1:imut, 1:jmut, 1:km) = real(covarian(1:imut, 1:jmut, 1:km), 4)
      do k = 1, km
        where(aexl(1:imut, 1:jmut, k) == 0.d0)
          data_out(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      write(mtout, rec=1) data_out(1:imut, 1:jmut, 1:km)
      !
      !  correlation
      !
      do k = 1, km
        do j = 1, jmut
          do i = 1, imut
            if(variance(i, j, k) > 0.0d0) then
              data_out(i, j, k) = real(covarian(i, j, k) / sqrt(variance(i, j, k)), 4)
            else
              if(aexl(i, j, k) == 0.d0) then
                data_out(i, j, k) = UNDEF
              else
                data_out(i, j, k) = 0.0
              end if
            end if
          end do
        end do
      end do
      write(mtout, rec=2) data_out(1:imut, 1:jmut, 1:km)
      close(mtout)
    !-----------------------------------------------------------------
    case('T') !  3D data at TS grids
      mean_data(1:imut, 1:jmut, 1:km) = 0.d0
      do iyear = styear, endyear
        write(flin, '(a, a, i4.4)' ) trim(fincore), '.', iyear + lag
        write(*,'(a, a)') 'file in :', trim(flin)
        open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)
        !
        read (mtin, rec=1) data_in(1:imut,1:jmut,1:km)
        do k = 1, km
          where(atexl(1:imut, 1:jmut, k) == 0.d0)
            data_in(1:imut, 1:jmut, k) = 0.0
          end where
        end do
        mean_data(1:imut, 1:jmut, 1:km) = mean_data(1:imut, 1:jmut, 1:km) &
          &    + real(data_in(1:imut, 1:jmut, 1:km), 8)
        close(mtin)
      end do
      mean_data(1:imut, 1:jmut, 1:km) = mean_data(1:imut, 1:jmut, 1:km) * inv_numdata
      !
      variance(1:imut, 1:jmut, 1:km) = 0.d0
      covarian(1:imut, 1:jmut, 1:km) = 0.d0
      do iyear = styear, endyear
        write(flin, '(a, a, i4.4)' ) trim(fincore), '.', iyear + lag
        write(*,'(a, a)') 'file in :', trim(flin)
        open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)
        !
        read (mtin, rec=1) data_in(1:imut,1:jmut,1:km)
        close(mtin)
        do k = 1, km
          where(atexl(1:imut, 1:jmut, k) == 0.d0)
            data_in(1:imut, 1:jmut, k) = 0.0
          end where
        end do
        data(1:imut, 1:jmut, 1:km) = real(data_in(1:imut, 1:jmut, 1:km), 8)  &
          &                           - mean_data(1:imut, 1:jmut, 1:km)
        variance(1:imut, 1:jmut, 1:km) = variance(1:imut, 1:jmut, 1:km)   &
          &   + data(1:imut, 1:jmut, 1:km) * data(1:imut, 1:jmut, 1:km)
        covarian(1:imut, 1:jmut, 1:km) = covarian(1:imut, 1:jmut, 1:km)   &
          &   +      nrm_series(iyear) * data(1:imut, 1:jmut, 1:km)
      end do
      covarian(1:imut, 1:jmut, 1:km) = covarian(1:imut, 1:jmut, 1:km) * inv_numdata
      variance(1:imut, 1:jmut, 1:km) = variance(1:imut, 1:jmut, 1:km) * inv_numdata
      !
      write(flout, '(a, a, i4.4)' ) trim(foutcore), '.lag', lag +1000
      write(*,'(a, a)') 'file out :', trim(flout)
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
      !
      !  regression
      !
      data_out(1:imut, 1:jmut, 1:km) = real(covarian(1:imut, 1:jmut, 1:km), 4)
      do k = 1, km
        where(atexl(1:imut, 1:jmut, k) == 0.d0)
          data_out(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      write(mtout, rec=1) data_out(1:imut, 1:jmut, 1:km)
      !
      !  correlation
      !
      do k = 1, km
        do j = 1, jmut
          do i = 1, imut
            if(variance(i, j, k) > 0.0d0) then
              data_out(i, j, k) = real(covarian(i, j, k) / sqrt(variance(i, j, k)), 4)
            else
              if(atexl(i, j, k) == 0.d0) then
                data_out(i, j, k) = UNDEF
              else
                data_out(i, j, k) = 0.0
              end if
            end if
          end do
        end do
      end do
      write(mtout, rec=2) data_out(1:imut, 1:jmut, 1:km)
      close(mtout)
    !-----------------------------------------------------------------
    case default
      write(*,*) 'tuxy is not correct !'
  end select
  !
!====================================================
end program regcor
