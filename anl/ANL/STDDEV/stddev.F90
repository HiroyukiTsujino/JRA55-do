!stddev.F90
!====================================================
!
!  Standard Deviation
!
!====================================================
program stddev
  !
  implicit none
  !
  !
  integer(4), parameter :: mtin   = 81
  integer(4), parameter :: mtout  =82
  !
  real(4), allocatable :: data_in(:,:,:,:)
  real(8), allocatable :: data(:,:,:,:)
  real(8), allocatable :: mean_data(:,:,:,:)
  real(8), allocatable :: variance(:,:,:,:)
  real(4), allocatable :: data_out(:,:,:,:)
  !
  character(len=256)   :: fincore
  character(len=256)   :: flout
  real(4), save  :: UNDEF = 0.0e0
  logical, save  :: l_monthly = .false.
  integer(4) :: inum, jnum, knum, vnum
  integer(4) :: sttyear, endyear
  namelist /nml_stddev/ fincore, flout, undef, l_monthly, &
    &                   inum, jnum, knum, vnum,           &
    &                   sttyear, endyear
  !
  character(len=256)   :: flin
  integer(4) :: numdata
  real(8)    :: rnumdata
  real(8)    :: inv_numdata
  integer(4) :: iyear, m
  integer(4) :: i, j, k, n
  real(8)    :: nyear
  !
  !
  !==========================================
  !
  read(unit=5, nml_stddev)
  write(*, *) 'fincore  : ', fincore
  write(*, *) 'flout    : ', flout
  write(*, *) 'inum     : ', inum
  write(*, *) 'jnum     : ', jnum
  write(*, *) 'knum     : ', knum
  write(*, *) 'vnum     : ', vnum
  write(*, *) 'undef    : ', undef
  write(*, *) 'sttyear  : ', sttyear
  write(*, *) 'endyear  : ', endyear
  write(*, *) 'l_monthly: ', l_monthly
  write(*, *) '-----------'
  numdata = endyear - sttyear +1
  write(*, *) 'numdata  : ', numdata
  rnumdata = real(numdata, 8)
  inv_numdata = 1.d0 / rnumdata
  !------------------------------------------
  !
  allocate(data_in(1:inum, 1:jnum, 1:knum, 1:vnum))
  allocate(data(1:inum, 1:jnum, 1:knum, 1:vnum))
  allocate(mean_data(1:inum, 1:jnum, 1:knum, 1:vnum))
  allocate(variance(1:inum, 1:jnum, 1:knum, 1:vnum))
  allocate(data_out(1:inum, 1:jnum, 1:knum, 1:vnum))
  !--------------------------------------------------------------------------------------------
  !
  mean_data(1:inum, 1:jnum, 1:knum, 1:vnum) = 0.d0
  variance(1:inum, 1:jnum, 1:knum, 1:vnum) = 0.d0
  !--------------------------------------------------------------------------------------------
  if(l_monthly) then
    do iyear = sttyear, endyear
      do m = 1, 12
        write(flin, '(a, a, i4.4, i2.2)' ) trim(fincore), '.', iyear, m
        write(*,'(a, a)') 'file in :', trim(flin)
        open(mtin, file=flin, form='unformatted', access='direct', recl=4*inum*jnum*knum*vnum)
        !
        read (mtin, rec=1) data_in(1:inum,1:jnum,1:knum,1:vnum)
        close(mtin)
        do n = 1, vnum
          do k = 1, knum
            where(data_in(1:inum, 1:jnum, k, n) == UNDEF)
              data_in(1:inum, 1:jnum, k, n) = 0.e0
            end where
          end do
        end do
        mean_data(1:inum, 1:jnum, 1:knum, 1:vnum) = mean_data(1:inum, 1:jnum, 1:knum, 1:vnum) &
          &    + real(data_in(1:inum, 1:jnum, 1:knum, 1:vnum), 8)
      end do
    end do
    mean_data(1:inum, 1:jnum, 1:knum, 1:vnum) = mean_data(1:inum, 1:jnum, 1:knum, 1:vnum) * inv_numdata /12.d0
    !
    do iyear = sttyear, endyear
      do m = 1, 12
        write(flin, '(a, a, i4.4, i2.2)' ) trim(fincore), '.', iyear, m
        write(*,'(a, a)') 'file in :', trim(flin)
        open(mtin, file=flin, form='unformatted', access='direct', recl=4*inum*jnum*knum*vnum)
        !
        read (mtin, rec=1) data_in(1:inum,1:jnum,1:knum,1:vnum)
        close(mtin)
        do n = 1, vnum
          do k = 1, knum
            where(data_in(1:inum, 1:jnum, k, n) == UNDEF)
              data_in(1:inum, 1:jnum, k, n) = 0.e0
            end where
          end do
        end do
        data(1:inum, 1:jnum, 1:knum, 1:vnum) = real(data_in(1:inum, 1:jnum, 1:knum, 1:vnum), 8)  &
          &                           - mean_data(1:inum, 1:jnum, 1:knum, 1:vnum)
        variance(1:inum, 1:jnum, 1:knum, 1:vnum) = variance(1:inum, 1:jnum, 1:knum, 1:vnum)   &
          &   + data(1:inum, 1:jnum, 1:knum, 1:vnum) * data(1:inum, 1:jnum, 1:knum, 1:vnum)
      end do
    end do
    variance(1:inum, 1:jnum, 1:knum, 1:vnum) = variance(1:inum, 1:jnum, 1:knum, 1:vnum) * inv_numdata /12.d0
  else !---------------------------------------------------------------------------------------
    do iyear = sttyear, endyear
      write(flin, '(a, a, i4.4)' ) trim(fincore), '.', iyear
      write(*,'(a, a)') 'file in :', trim(flin)
      open(mtin, file=flin, form='unformatted', access='direct', recl=4*inum*jnum*knum*vnum)
      !
      read (mtin, rec=1) data_in(1:inum,1:jnum,1:knum,1:vnum)
      close(mtin)
      do n = 1, vnum
        do k = 1, knum
          where(data_in(1:inum, 1:jnum, k, n) == UNDEF)
            data_in(1:inum, 1:jnum, k, n) = 0.e0
          end where
        end do
      end do
      mean_data(1:inum, 1:jnum, 1:knum, 1:vnum) = mean_data(1:inum, 1:jnum, 1:knum, 1:vnum) &
        &    + real(data_in(1:inum, 1:jnum, 1:knum, 1:vnum), 8)
    end do
    mean_data(1:inum, 1:jnum, 1:knum, 1:vnum) = mean_data(1:inum, 1:jnum, 1:knum, 1:vnum) * inv_numdata
    !
    do iyear = sttyear, endyear
      write(flin, '(a, a, i4.4)' ) trim(fincore), '.', iyear
      write(*,'(a, a)') 'file in :', trim(flin)
      open(mtin, file=flin, form='unformatted', access='direct', recl=4*inum*jnum*knum*vnum)
      !
      read (mtin, rec=1) data_in(1:inum,1:jnum,1:knum,1:vnum)
      close(mtin)
      do n = 1, vnum
        do k = 1, knum
          where(data_in(1:inum, 1:jnum, k, n) == UNDEF)
            data_in(1:inum, 1:jnum, k, n) = 0.e0
          end where
        end do
      end do
      data(1:inum, 1:jnum, 1:knum, 1:vnum) = real(data_in(1:inum, 1:jnum, 1:knum, 1:vnum), 8)  &
        &                           - mean_data(1:inum, 1:jnum, 1:knum, 1:vnum)
      variance(1:inum, 1:jnum, 1:knum, 1:vnum) = variance(1:inum, 1:jnum, 1:knum, 1:vnum)   &
        &   + data(1:inum, 1:jnum, 1:knum, 1:vnum) * data(1:inum, 1:jnum, 1:knum, 1:vnum)
    end do
    variance(1:inum, 1:jnum, 1:knum, 1:vnum) = variance(1:inum, 1:jnum, 1:knum, 1:vnum) * inv_numdata
  end if
  !--------------------------------------------------------------------------------------------
  !
  open(mtout, file=flout, form='unformatted', access='direct', recl=4*inum*jnum*knum*vnum)
  !
  data_out(1:inum, 1:jnum, 1:knum, 1:vnum) = real(sqrt(variance(1:inum, 1:jnum, 1:knum, 1:vnum)), 4)
  do n = 1, vnum
    do k = 1, knum
      where(variance(1:inum, 1:jnum, k, n) == 0.d0)
        data_out(1:inum, 1:jnum, k, n) = UNDEF
      end where
    end do
  end do
  write(mtout, rec=1) data_out(1:inum, 1:jnum, 1:knum, 1:vnum)
  close(mtout)
  !
!====================================================
end program stddev
