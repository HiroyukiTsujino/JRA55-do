! -*-F90-*-
!==============================================================
program make_correction_for_wind_magnitude_annual
!==============================================================

  use libmxe_para, only: pi, radian, radian_r, radius
  use file_open_close_manager

  implicit none

  integer(4) :: imut, jmut
  integer(4) :: lreclen_in
  integer(4) :: lreclen_out

  integer(4) :: i, j, k, nyear, month, klev, mday
  integer(4) :: nbyr, neyr, ibmn, iemn

  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  integer(4),parameter :: mtin1 = 31, mtin2 = 32, mtin3 = 33
  integer(4),parameter :: mtin4 = 34, mtin5 = 35, mtin6 = 36
  integer(4),parameter :: mtot1 = 51, mtot2 = 52, mtot3 = 53, mtot4 = 54

  real(4) :: undef_jra
  real(4) :: undef_obs

  integer(4) :: total_grid_1d
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: ibgn, iend, i0

  real(4),allocatable :: dat2(:)

  real(8),allocatable :: swind_obs(:)
  real(8),allocatable :: mask_obs (:)

  real(8),allocatable :: swind_jra(:)
  real(8),allocatable :: mask_jra (:)

  real(8),allocatable :: corr_mag(:)

  real(8),allocatable :: mask_wind(:)

  ! for interpolation

  real(8),allocatable :: data_mag_latlon(:,:)
  real(8),allocatable :: data_mag_org(:), data_mag_new(:)
  real(8),allocatable :: mask_org(:)
  real(8),allocatable :: lon_org(:), lon_new(:)
  real(8) :: dlon, dlon_rg
  real(8) :: weight
  integer(4) :: ii

  !---------

  character(len=256) :: flnin1, flnin2, flnin3
  character(len=256) :: flnin4, flnin5, flnin6
  character(len=256) :: flnot1, flnot2, flnot3, flnot4
  character(len=256) :: file_mask

  integer(4) :: irecs_obs, irecu_obs, irecv_obs

  real(8) :: offset_max_ratio

!-----------------------------------------------------------------

  namelist /nml_windcorrec_mag_offset_ann/ &
       &  flnin1, &
       &  irecs_obs, &
       &  undef_obs, &
       &  flnin4, &
       &  undef_jra, &
       &  file_mask, &
       &  imut, jmut, grid_name, dlon, &
       &  flnot1, flnot3, &
       &  offset_max_ratio

!-----------------------------------------------------------------

  offset_max_ratio = 0.5d0
  open (10,file='namelist.windcorrec_mag_offset_ann')
  read (10,nml_windcorrec_mag_offset_ann) 
  close(10)

!-----------------------------------------------------------------
! set grid points

  allocate(num_xgrid(1:jmut))
  allocate(data_mag_latlon(1:imut,1:jmut))
  allocate(data_mag_new(1:imut))
  allocate(lon_new(1:imut))

  do i = 1, imut
    lon_new(i) = dlon * real(i-1,8)
  end do

  call set_reduced_grid(grid_name,num_xgrid,jmut,total_grid_1d)

  lreclen_in = 4 * total_grid_1d
  lreclen_out = 4 * total_grid_1d

!-----------------------------------------------------------------

  allocate(dat2(1:total_grid_1d))

  allocate(swind_obs(1:total_grid_1d))
  allocate(mask_obs (1:total_grid_1d))

  allocate(swind_jra(1:total_grid_1d))
  allocate(mask_jra (1:total_grid_1d))

  allocate(corr_mag(1:total_grid_1d))

  allocate(mask_wind(1:total_grid_1d))

  swind_obs(:) = 0.0d0
  mask_obs (:) = 0.0d0

  swind_jra(:) = 0.0d0
  mask_jra (:) = 0.0d0

  mask_wind(:) = 0.0d0

!-----------------------------------------------------------------

  open(mtin1, file=file_mask, form='unformatted', &
       & access='direct', convert='little_endian', recl=lreclen_in)
  write(6,*) ' file : ', trim(file_mask), ' opened '
  read(mtin1,rec=1) dat2
  mask_wind(1:total_grid_1d) = 1.0d0 - real(dat2(1:total_grid_1d),8)
  close(mtin1)

!-----------------------------------------------------------------

  write(6,*) ' Correction factor of speed = ',trim(flnot1)
  open (mtot1,file=flnot1,access='direct',form='unformatted', &
       & convert='little_endian',recl=lreclen_out)
  write(6,*) ' Offset ratio MAX = ', offset_max_ratio

  write(6,*) ' Correction factor of magnitute (latlon) = ',trim(flnot3)
  open (mtot3,file=flnot3,access='direct',form='unformatted', &
       & recl=4*imut*jmut)

  !---------------------------------------------------------------------

  open(mtin1, file=flnin1, form='unformatted', access='direct', &
       & convert='little_endian',recl=lreclen_in)
  write(6,*) ' file : ', trim(flnin1), ' opened '
  read(mtin1,rec=irecs_obs) dat2
  do i = 1, total_grid_1d
    swind_obs(i) = real(dat2(i),8)
  end do
  close(mtin1)

  !-------

  open(mtin4, file=flnin4, form='unformatted', access='direct', &
       & convert='little_endian', recl=lreclen_in)
  write(6,*) ' file : ', trim(flnin4), ' opened '
  read(mtin4,rec=1) dat2
  do i = 1, total_grid_1d
    swind_jra(i) = real(dat2(i),8)
  end do
  close(mtin4)

  !--------------------------------------------

  do i = 1, total_grid_1d
    if (mask_wind(i) /= 0.0d0) then
      if ((swind_obs(i) == real(undef_obs,8)) .and. (swind_jra(i) == real(undef_jra,8))) then
        write(6,*) ' mask and input data are inconsistent, terminating ....'
        stop
      else
        corr_mag(i) = min(swind_obs(i) - swind_jra(i), offset_max_ratio * swind_jra(i))
      end if
    else
      corr_mag(i) = 0.0d0 ! land : no adjustment
    end if
  end do

  write(mtot1,rec=1) real(corr_mag(1:total_grid_1d),4)
  close(mtot1)

  !--------------------------------------------
  ! reduced grid to lat-lon grid for check

  i0 = 0

  do j = 1, jmut

    ibgn = i0 + 1
    iend = i0 + num_xgrid(j)

    if (num_xgrid(j) == imut) then

      data_mag_latlon(1:imut,jmut-j+1) = corr_mag(ibgn:iend)

    else

      allocate(data_mag_org(1:num_xgrid(j)+1))
      allocate(lon_org     (1:num_xgrid(j)+1))

      data_mag_org(1:num_xgrid(j)) = corr_mag(ibgn:iend)
      data_mag_org(num_xgrid(j)+1) = corr_mag(ibgn)

      dlon_rg = 360.0 / real(num_xgrid(j),8)

      do ii = 1, num_xgrid(j) + 1
        lon_org(ii) = dlon_rg * real(ii-1,8)
      end do
      
      do i = 1, imut
        do ii = 1, num_xgrid(j)
          if (abs(lon_org(ii) - lon_new(i)) < 1.0d-4) then
            data_mag_new(i) = data_mag_org(ii)
            exit
          else if ( (lon_org(ii) < lon_new(i)) .and. (lon_new(i) <= lon_org(ii+1)) ) then
            weight = (lon_new(i) - lon_org(ii)) / (lon_org(ii+1) - lon_org(ii))
            data_mag_new(i) = (1.0d0 - weight) * data_mag_org(ii) + weight * data_mag_org(ii+1)
            exit
          end if
        end do
      end do
      data_mag_latlon(1:imut,jmut-j+1) = data_mag_new(1:imut)
      deallocate(data_mag_org)
      deallocate(lon_org)
    end if
    i0 = iend
  end do
  
  write(mtot3,rec=1) real(data_mag_latlon,4)
  close(mtot3)

end program make_correction_for_wind_magnitude_annual
