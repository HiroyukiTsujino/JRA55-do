! -*-F90-*-
!==============================================================
program make_correction_for_wind_vector_annual
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
  real(8),allocatable :: uwind_obs(:)
  real(8),allocatable :: vwind_obs(:)
  real(8),allocatable :: angle_obs(:)
  real(8),allocatable :: mask_obs (:)

  real(8),allocatable :: swind_jra(:)
  real(8),allocatable :: uwind_jra(:)
  real(8),allocatable :: vwind_jra(:)
  real(8),allocatable :: angle_jra(:)
  real(8),allocatable :: mask_jra (:)

  real(8),allocatable :: corr_mag(:)
  real(8),allocatable :: corr_rot(:)
  real(8),allocatable :: corr_rot_org(:)
  real(8),allocatable :: swind_vec_obs(:)
  real(8),allocatable :: swind_vec_jra(:)

  real(8),allocatable :: mask_wind(:)

  ! for interpolation

  real(8),allocatable :: data_mag_latlon(:,:)
  real(8),allocatable :: data_rot_latlon(:,:)
  real(8),allocatable :: data_rot_raw_latlon(:,:)
  real(8),allocatable :: data_mag_org(:), data_mag_new(:)
  real(8),allocatable :: data_rot_org(:), data_rot_new(:)
  real(8),allocatable :: data_rot_raw_org(:), data_rot_raw_new(:)
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

  real(8) :: mag_min, mag_max

!-----------------------------------------------------------------

  namelist /nml_windcorrec_tanh_ann/ &
       &  flnin1, flnin2, flnin3, &
       &  irecs_obs, irecu_obs, irecv_obs, &
       &  undef_obs, &
       &  flnin4, flnin5, flnin6, &
       &  undef_jra, &
       &  file_mask, &
       &  imut, jmut, grid_name, dlon, &
       &  flnot1, flnot2, flnot3, flnot4, &
       &  mag_min, mag_max

!-----------------------------------------------------------------

  mag_min = 0.0d0
  mag_max = 1.0d2
  open (10,file='namelist.windcorrec_tanh_ann')
  read (10,nml_windcorrec_tanh_ann) 
  close(10)

!-----------------------------------------------------------------
! set grid points

  allocate(num_xgrid(1:jmut))
  allocate(data_mag_latlon(1:imut,1:jmut))
  allocate(data_rot_latlon(1:imut,1:jmut))
  allocate(data_rot_raw_latlon(1:imut,1:jmut))
  allocate(data_mag_new(1:imut))
  allocate(data_rot_new(1:imut))
  allocate(data_rot_raw_new(1:imut))
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
  allocate(uwind_obs(1:total_grid_1d))
  allocate(vwind_obs(1:total_grid_1d))
  allocate(angle_obs(1:total_grid_1d))
  allocate(mask_obs (1:total_grid_1d))

  allocate(swind_jra(1:total_grid_1d))
  allocate(uwind_jra(1:total_grid_1d))
  allocate(vwind_jra(1:total_grid_1d))
  allocate(angle_jra(1:total_grid_1d))
  allocate(mask_jra (1:total_grid_1d))

  allocate(corr_mag(1:total_grid_1d))
  allocate(corr_rot(1:total_grid_1d))
  allocate(corr_rot_org(1:total_grid_1d))
  allocate(swind_vec_obs(1:total_grid_1d))
  allocate(swind_vec_jra(1:total_grid_1d))

  allocate(mask_wind(1:total_grid_1d))

  swind_obs(:) = 0.0d0
  uwind_obs(:) = 0.0d0
  vwind_obs(:) = 0.0d0
  mask_obs (:) = 0.0d0

  swind_jra(:) = 0.0d0
  uwind_jra(:) = 0.0d0
  vwind_jra(:) = 0.0d0
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
  write(6,*) ' MAX = ', mag_max
  write(6,*) ' MIN = ', mag_min

  write(6,*) ' Rotation = ',trim(flnot2)
  open (mtot2,file=flnot2,access='direct',form='unformatted', &
       & convert='little_endian',recl=lreclen_out)

  write(6,*) ' Correction factor of magnitute (latlon) = ',trim(flnot3)
  open (mtot3,file=flnot3,access='direct',form='unformatted', &
       & recl=4*imut*jmut)

  write(6,*) ' Correction factor of direction (latlon) = ',trim(flnot4)
  open (mtot4,file=flnot4,access='direct',form='unformatted', &
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

  open(mtin2, file=flnin2, form='unformatted', access='direct', &
       & convert='little_endian',recl=lreclen_in)
  write(6,*) ' file : ', trim(flnin2), ' opened '
  read(mtin2,rec=irecu_obs) dat2
  do i = 1, total_grid_1d
    uwind_obs(i) = real(dat2(i),8)
  end do
  close(mtin2)

  open(mtin3, file=flnin3, form='unformatted', access='direct', &
       & convert='little_endian',recl=lreclen_in)
  write(6,*) ' file : ', trim(flnin3), ' opened '
  read(mtin3,rec=irecv_obs) dat2
  do i = 1, total_grid_1d
    vwind_obs(i) = real(dat2(i),8)
  end do
  close(mtin3)

  !-------

  open(mtin4, file=flnin4, form='unformatted', access='direct', &
       & convert='little_endian', recl=lreclen_in)
  write(6,*) ' file : ', trim(flnin4), ' opened '
  read(mtin4,rec=1) dat2
  do i = 1, total_grid_1d
    swind_jra(i) = real(dat2(i),8)
  end do
  close(mtin4)

  open(mtin5, file=flnin5, form='unformatted', access='direct', &
       & convert='little_endian', recl=lreclen_in)
  write(6,*) ' file : ', trim(flnin5), ' opened '
  read(mtin5,rec=1) dat2
  do i = 1, total_grid_1d
    uwind_jra(i) = real(dat2(i),8)
  end do
  close(mtin5)
  
  open(mtin6, file=flnin6, form='unformatted', access='direct', &
       & convert='little_endian',recl=lreclen_in)
  write(6,*) ' file : ', trim(flnin6), ' opened '
  read(mtin6,rec=1) dat2
  do i = 1, total_grid_1d
    vwind_jra(i) = real(dat2(i),8)
  end do
  close(mtin6)

  !--------------------------------------------

  do i = 1, total_grid_1d
    if (mask_wind(i) /= 0.0d0) then
      if ((swind_obs(i) == real(undef_obs,8)) .and. (swind_jra(i) == real(undef_jra,8))) then
        write(6,*) ' mask and input data are inconsistent, terminating ....'
        stop
      else
        corr_mag(i) = min(max(swind_obs(i) / swind_jra(i), mag_min), mag_max)
      end if
    else
      !corr_mag(i) = real(undef_jra,8)
      corr_mag(i) = 1.0d0
    end if
  end do

  write(mtot1,rec=1) real(corr_mag(1:total_grid_1d),4)

  !--------------------------------------------

  swind_vec_obs(:) = 0.0d0
  do i = 1, total_grid_1d
    if (mask_wind(i) /= 0.0d0) then
      if (uwind_obs(i) /= 0.0d0) then
        angle_obs(i) = 180.d0 / pi * atan(vwind_obs(i)/uwind_obs(i)) ! [-90, 90]
        if ((uwind_obs(i) < 0.0d0) .and. (vwind_obs(i) > 0.0d0)) then
          angle_obs(i) = angle_obs(i) + 180.0d0
        end if
        if ((uwind_obs(i) < 0.0d0) .and. (vwind_obs(i) < 0.0d0)) then
          angle_obs(i) = angle_obs(i) - 180.0d0
        end if
      else
        if (vwind_obs(i) > 0.0d0) then
          angle_obs(i) = 90.0d0
        else
          angle_obs(i) = -90.0d0
        end if
      end if
      swind_vec_obs(i) = sqrt(uwind_obs(i)**2 + vwind_obs(i)**2)
    else
      angle_obs(i) = real(undef_obs,8)
    end if
  end do

  swind_vec_jra(:) = 0.0d0
  do i = 1, total_grid_1d
    if (mask_wind(i) /= 0.0d0) then
      if (uwind_jra(i) /= 0.0d0) then
        angle_jra(i) = 180.d0 / pi * atan(vwind_jra(i)/uwind_jra(i))
        if ((uwind_jra(i) < 0.0d0) .and. (vwind_jra(i) > 0.0d0)) then
          angle_jra(i) = angle_jra(i) + 180.0d0
        end if
        if ((uwind_jra(i) < 0.0d0) .and. (vwind_jra(i) < 0.0d0)) then
          angle_jra(i) = angle_jra(i) - 180.0d0
        end if
      else
        if (vwind_jra(i) > 0.0d0) then
          angle_jra(i) = 90.0d0
        else
          angle_jra(i) = -90.0d0
        end if
      end if
      swind_vec_jra(i) = sqrt(uwind_jra(i)**2 + vwind_jra(i)**2)
    else
      angle_jra(i) = real(undef_jra,8)
    end if
  end do
  
  do i = 1, total_grid_1d
    if (mask_wind(i) /= 0.0d0) then
      corr_rot(i) = angle_obs(i) - angle_jra(i)
      if (corr_rot(i) > 180.0d0) then
        corr_rot(i) = corr_rot(i) - 360.0d0
      end if
      if (corr_rot(i) < -180.0d0) then
        corr_rot(i) = 360.0d0 + corr_rot(i)
      end if

      corr_rot_org(i) = corr_rot(i)
      ! smooth transition
      corr_rot(i) = corr_rot_org(i) * 0.5d0 * (1.0d0 + tanh(swind_vec_jra(i)-3.0d0)) 

    else
      swind_vec_obs(i) = real(undef_jra,8)
      swind_vec_jra(i) = real(undef_jra,8)
      corr_rot(i) = 0.0d0
      !corr_rot(i) = real(undef_jra,8)
      corr_rot_org(i) = corr_rot(i)
    end if
  end do

  write(mtot2,rec=1) real(corr_rot     (1:total_grid_1d),4)
  write(mtot2,rec=2) real(angle_obs    (1:total_grid_1d),4)
  write(mtot2,rec=3) real(angle_jra    (1:total_grid_1d),4)
  write(mtot2,rec=4) real(corr_rot_org (1:total_grid_1d),4)
  write(mtot2,rec=5) real(swind_vec_obs(1:total_grid_1d),4)
  write(mtot2,rec=6) real(swind_vec_jra(1:total_grid_1d),4)
  
  !-----------------------------------------------------------
  ! reduced grid to lat-lon grid for check

  i0 = 0

  do j = 1, jmut

    ibgn = i0 + 1
    iend = i0 + num_xgrid(j)

    if (num_xgrid(j) == imut) then

      data_mag_latlon(1:imut,jmut-j+1) = corr_mag(ibgn:iend)
      data_rot_latlon(1:imut,jmut-j+1) = corr_rot(ibgn:iend)
      data_rot_raw_latlon(1:imut,jmut-j+1) = corr_rot_org(ibgn:iend)

    else

      allocate(data_mag_org(1:num_xgrid(j)+1))
      allocate(data_rot_org(1:num_xgrid(j)+1))
      allocate(data_rot_raw_org(1:num_xgrid(j)+1))
      allocate(lon_org     (1:num_xgrid(j)+1))

      data_mag_org(1:num_xgrid(j)) = corr_mag(ibgn:iend)
      data_mag_org(num_xgrid(j)+1) = corr_mag(ibgn)
      data_rot_org(1:num_xgrid(j)) = corr_rot(ibgn:iend)
      data_rot_org(num_xgrid(j)+1) = corr_rot(ibgn)
      data_rot_raw_org(1:num_xgrid(j)) = corr_rot_org(ibgn:iend)
      data_rot_raw_org(num_xgrid(j)+1) = corr_rot_org(ibgn)

      dlon_rg = 360.0 / real(num_xgrid(j),8)

      do ii = 1, num_xgrid(j) + 1
        lon_org(ii) = dlon_rg * real(ii-1,8)
      end do
      
      do i = 1, imut
        do ii = 1, num_xgrid(j)
          if (abs(lon_org(ii) - lon_new(i)) < 1.0d-4) then
            data_mag_new(i) = data_mag_org(ii)
            data_rot_new(i) = data_rot_org(ii)
            data_rot_raw_new(i) = data_rot_raw_org(ii)
            exit
          else if ( (lon_org(ii) < lon_new(i)) .and. (lon_new(i) <= lon_org(ii+1)) ) then
            weight = (lon_new(i) - lon_org(ii)) / (lon_org(ii+1) - lon_org(ii))
            data_mag_new(i) = (1.0d0 - weight) * data_mag_org(ii) + weight * data_mag_org(ii+1)
            data_rot_new(i) = (1.0d0 - weight) * data_rot_org(ii) + weight * data_rot_org(ii+1)
            data_rot_raw_new(i) = (1.0d0 - weight) * data_rot_raw_org(ii) + weight * data_rot_raw_org(ii+1)
            exit
          end if
        end do
      end do
      data_mag_latlon(1:imut,jmut-j+1) = data_mag_new(1:imut)
      data_rot_latlon(1:imut,jmut-j+1) = data_rot_new(1:imut)
      data_rot_raw_latlon(1:imut,jmut-j+1) = data_rot_raw_new(1:imut)
      deallocate(data_mag_org)
      deallocate(data_rot_org)
      deallocate(data_rot_raw_org)
      deallocate(lon_org)
    end if
    i0 = iend
  end do
  
  write(mtot3,rec=1) real(data_mag_latlon,4)
  write(mtot4,rec=1) real(data_rot_latlon,4)
  write(mtot4,rec=2) real(data_rot_raw_latlon,4)
  
  close(mtot2)
  close(mtot1)

end program make_correction_for_wind_vector_annual
