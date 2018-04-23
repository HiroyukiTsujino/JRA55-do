! -*-F90-*-
program shift_2m_to_10m_hybrid

  ! shift 2m air temperature and specific humidity to those at 10 m

  implicit none

  integer(4) :: imut, jmut
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: total_grid_1d
  real(8),parameter :: tab = 273.15

  integer(4) :: ibgn, iend, i0

  real(4),allocatable :: work4(:)

  real(8),allocatable :: data_tmp2m(:), data_sph2m(:)
  real(8),allocatable :: data_slprs(:), data_brtmp(:)
  real(8),allocatable :: data_zrough(:)
  real(8),allocatable :: data_u10m(:), data_v10m(:)

  real(8),allocatable :: data_evap(:), data_sens(:)
  real(8),allocatable :: data_evapa(:), data_sensa(:)
  real(8),allocatable :: data_evapb(:), data_sensb(:)

  real(8),allocatable :: data_uflx(:), data_vflx(:)
  real(8),allocatable :: data_uflxa(:), data_vflxa(:)
  real(8),allocatable :: data_uflxb(:), data_vflxb(:)

  real(8),allocatable :: data_tau(:), data_wdv(:)
  real(8),allocatable :: data_ice(:)
  real(8),allocatable :: data_mask(:)
  real(8),allocatable :: data_tmp10m(:), data_sph10m(:)
  real(8),allocatable :: data_sphsrf(:)

  real(8),allocatable :: tmp10m_latlon(:,:)
  real(8),allocatable :: sph10m_latlon(:,:)

  real(8),allocatable :: tmp10m_org(:), tmp10m_new(:)
  real(8),allocatable :: sph10m_org(:), sph10m_new(:)
  real(8),allocatable :: lon_org(:), lon_new(:)
  real(8) :: dlon, dlon_rg

  character(256) :: file_tmp2m
  character(256) :: file_sph2m
  character(256) :: file_slprs
  character(256) :: file_brtmp
  character(256) :: file_zrough

  character(256) :: file_uflxa
  character(256) :: file_vflxa
  character(256) :: file_uflxb
  character(256) :: file_vflxb
  character(256) :: file_evapa
  character(256) :: file_sensa
  character(256) :: file_evapb
  character(256) :: file_sensb

  character(256) :: file_u10m
  character(256) :: file_v10m
  character(256) :: file_ice
  character(256) :: file_mask
  character(256) :: file_tmp10m
  character(256) :: file_sph10m

  integer(4),parameter :: lun=10

  integer(4) :: i, j, ii, n
  real(8) :: weight
  real(8) :: altu, altt, altq, alt_target

  integer(4) :: iii, m, m_left, m_right, i_left, i_right
  real(8),parameter :: sphmin = 3.0d-4

  !---------------------------------------------
 
  namelist /nml_shift_height/ &
       & file_tmp2m, file_sph2m, &
       & file_slprs, file_zrough, &
       & file_brtmp, &
       & file_evapa, file_sensa, &
       & file_evapb, file_sensb, &
       & file_uflxa, file_vflxa, &
       & file_uflxb, file_vflxb, &
       & file_u10m, file_v10m, &
       & file_ice, &
       & file_mask, &
       & file_tmp10m, file_sph10m, &
       & altu, altt, altq, alt_target, &
       & imut, jmut, dlon, grid_name

  !---------------------------------------------

  open(lun,file='namelist.shift_height_zrough')
  read(lun,nml=nml_shift_height)
  close(lun)

  !---------------------------------------------

  allocate(num_xgrid(1:jmut))
  allocate(tmp10m_latlon(1:imut,1:jmut))
  allocate(sph10m_latlon(1:imut,1:jmut))
  allocate(tmp10m_new(1:imut))
  allocate(sph10m_new(1:imut))
  allocate(lon_new(1:imut))

  do i = 1, imut
    lon_new(i) = dlon * real(i-1,8)
  end do

  call set_reduced_grid(grid_name,num_xgrid,total_grid_1d)

  allocate(work4(1:total_grid_1d))

  allocate(data_tmp2m(1:total_grid_1d))
  allocate(data_sph2m(1:total_grid_1d))
  allocate(data_slprs(1:total_grid_1d))
  allocate(data_brtmp(1:total_grid_1d))
  allocate(data_zrough(1:total_grid_1d))
  allocate(data_sphsrf(1:total_grid_1d))
  allocate(data_u10m (1:total_grid_1d))
  allocate(data_v10m (1:total_grid_1d))

  allocate(data_evap (1:total_grid_1d))
  allocate(data_sens (1:total_grid_1d))
  allocate(data_evapa(1:total_grid_1d))
  allocate(data_sensa(1:total_grid_1d))
  allocate(data_evapb(1:total_grid_1d))
  allocate(data_sensb(1:total_grid_1d))

  allocate(data_uflx (1:total_grid_1d))
  allocate(data_vflx (1:total_grid_1d))
  allocate(data_uflxa(1:total_grid_1d))
  allocate(data_vflxa(1:total_grid_1d))
  allocate(data_uflxb(1:total_grid_1d))
  allocate(data_vflxb(1:total_grid_1d))

  allocate(data_wdv (1:total_grid_1d))
  allocate(data_tau (1:total_grid_1d))

  allocate(data_ice (1:total_grid_1d))
  allocate(data_mask(1:total_grid_1d))

  allocate(data_tmp10m(1:total_grid_1d))
  allocate(data_sph10m(1:total_grid_1d))

  !------
  open(lun,file=file_tmp2m,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_tmp2m(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  open(lun,file=file_sph2m,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_sph2m(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  !------
  open(lun,file=file_slprs,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_slprs(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  open(lun,file=file_brtmp,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_brtmp(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  open(lun,file=file_zrough,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_zrough(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  !------
  open(lun,file=file_evapb,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_evapb(1:total_grid_1d) = real(work4(1:total_grid_1d),8) ! positive for "gain" by the atmosphere
  close(lun)

  open(lun,file=file_sensb,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_sensb(1:total_grid_1d) = real(work4(1:total_grid_1d),8) ! positive for "gain" by the atmosphere
  close(lun)

  open(lun,file=file_evapa,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_evapa(1:total_grid_1d) = real(work4(1:total_grid_1d),8) ! positive for "gain" by the atmosphere
  close(lun)

  open(lun,file=file_sensa,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_sensa(1:total_grid_1d) = real(work4(1:total_grid_1d),8) ! positive for "gain" by the atmosphere
  close(lun)

  !------
  open(lun,file=file_uflxb,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_uflxb(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  open(lun,file=file_vflxb,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_vflxb(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  open(lun,file=file_uflxa,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_uflxa(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  open(lun,file=file_vflxa,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_vflxa(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  !------
  !open(lun,file=file_u10m,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  !read(lun,rec=1) work4
  !data_u10m(1:total_grid_1d) = real(work4(1:total_grid_1d),8) ! positive for "gain" by the atmosphere
  !close(lun)
  !
  !open(lun,file=file_v10m,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  !read(lun,rec=1) work4
  !data_v10m(1:total_grid_1d) = real(work4(1:total_grid_1d),8) ! positive for "gain" by the atmosphere
  !close(lun)
  !
  !------
  open(lun,file=file_ice,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_ice(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  !------
  open(lun,file=file_mask,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_mask(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  do n = 1, total_grid_1d
    ! [mm/day] -> [kg/s/m2], assuming (pure) water density = 1000 kg / m^3
    data_evap(n) = - 0.5d0 * (data_evapb(n) + data_evapa(n)) / 86400.d0
    data_sens(n) = - 0.5d0 * (data_sensb(n) + data_sensa(n))
    data_uflx(n) = - 0.5d0 * (data_uflxb(n) + data_uflxa(n))
    data_vflx(n) = - 0.5d0 * (data_vflxb(n) + data_vflxa(n))
    data_mask(n) = 1.0d0 - data_mask(n) ! 0 for land, 1 for sea
    data_brtmp(n) = data_brtmp(n) - tab ! Celsius
    data_tmp2m(n) = data_tmp2m(n) - tab ! Celsius
    data_slprs(n) = data_slprs(n) * 1.0d-2 ! [hPa]
    data_tau(n) = max(sqrt(data_uflx(n)**2 + data_vflx(n)**2),1.0d-2) ! 0.01 [N/m^2] floor for wind stress
    data_wdv(n) = 0.0d0
    !data_wdv(n) = sqrt(data_u10m(n)**2 + data_v10m(n)**2)
  end do

  call bulk_shift(data_tmp10m, data_sph10m, &
     & data_tmp2m, data_sph2m, data_wdv, data_tau, data_slprs, data_brtmp, &
     & data_zrough, data_sphsrf, data_sens, data_evap, &
     & data_mask, data_ice, total_grid_1d, altu, altt, altq, alt_target)

  !-----------------------------------------------------------------------------------------

  i0 = 0

  do j = 1, jmut

    ibgn = i0 + 1
    iend = i0 + num_xgrid(j)

    if (num_xgrid(j) == imut) then

      tmp10m_latlon(1:imut,jmut-j+1) = data_tmp10m(ibgn:iend)
      sph10m_latlon(1:imut,jmut-j+1) = data_sph10m(ibgn:iend)

    else

      allocate(tmp10m_org(1:num_xgrid(j)+1))
      allocate(sph10m_org(1:num_xgrid(j)+1))
      allocate(lon_org (1:num_xgrid(j)+1))

      tmp10m_org(1:num_xgrid(j)) = data_tmp10m(ibgn:iend)
      tmp10m_org(num_xgrid(j)+1) = data_tmp10m(ibgn)
      sph10m_org(1:num_xgrid(j)) = data_sph10m(ibgn:iend)
      sph10m_org(num_xgrid(j)+1) = data_sph10m(ibgn)

      !do ii = 1, num_xgrid(j)
        !if (sph10m_org(ii) < 0.0d0) then
        !  m_left  = 0
        !  m_right = 0
        !  i_left  = 0
        !  i_right = 0
        !  do m = 1, num_xgrid(j)/2
        !    iii = ii - m
        !    if (iii < 1) then
        !      iii = num_xgrid(j) - abs(iii)
        !    end if
        !    if (sph10m_org(iii) > 0.0d0) then
        !      i_left = iii
        !      m_left = m
        !      exit
        !    end if
        !  end do
        !  do m = 1, num_xgrid(j)/2
        !    iii = ii + m
        !    if (iii > num_xgrid(j)) then
        !      iii = iii - num_xgrid(j)
        !    end if
        !    if (sph10m_org(iii) > 0.0d0) then
        !      i_right = iii
        !      m_right = m
        !      exit
        !    end if
        !  end do
        !  if (m_left == 0 .or. m_right == 0) then
        !    write(6,*) ' could not find valid specific humidity for j = ', j, ii
        !    write(6,*) ' replacing with ', sphmin
        !    sph10m_org(ii) = sphmin
        !  else
        !    sph10m_org(ii) = (real(m_right,8) * sph10m_org(i_left) &
        !         & + real(m_left,8) *  sph10m_org(i_right)) / real(m_right+m_left,8)
        !    write(6,*) ' replacing with ', sph10m_org(ii)
        !    write(6,*) '             at ', i_left, m_left, i_right, m_right
        !  end if
        !end if
      !end do

      dlon_rg = 360.0 / real(num_xgrid(j),8)
      do ii = 1, num_xgrid(j) + 1
        lon_org(ii) = dlon_rg * real(ii-1,8)
      end do
      do i = 1, imut
        do ii = 1, num_xgrid(j)
          if (abs(lon_org(ii) - lon_new(i)) < 1.0d-4) then
            tmp10m_new(i) = tmp10m_org(ii)
            sph10m_new(i) = sph10m_org(ii)
            exit
          else if ( (lon_org(ii) < lon_new(i)) .and. (lon_new(i) <= lon_org(ii+1)) ) then
            weight = (lon_new(i) - lon_org(ii)) / (lon_org(ii+1) - lon_org(ii))
            tmp10m_new(i) = (1.0d0 - weight) * tmp10m_org(ii) + weight * tmp10m_org(ii+1)
            sph10m_new(i) = (1.0d0 - weight) * sph10m_org(ii) + weight * sph10m_org(ii+1)
            exit
          end if
        end do
      end do
      tmp10m_latlon(1:imut,jmut-j+1) = tmp10m_new(1:imut)
      sph10m_latlon(1:imut,jmut-j+1) = sph10m_new(1:imut)
      deallocate(tmp10m_org, sph10m_org)
      deallocate(lon_org)
    end if
    i0 = iend
  end do

  tmp10m_latlon(1:imut,1:jmut) = tmp10m_latlon(1:imut,1:jmut) + tab

  open(lun,file=file_tmp10m,form='unformatted',access='direct',recl=4*imut*jmut)
  write(lun,rec=1) real(tmp10m_latlon,4)
  close(lun)

  open(lun,file=file_sph10m,form='unformatted',access='direct',recl=4*imut*jmut)
  write(lun,rec=1) real(sph10m_latlon,4)
  close(lun)

  !---------------------------------------------

contains

  subroutine set_reduced_grid(gname,numx,num_grid_1d)
    character(len=*),intent(in) :: gname
    integer(4), intent(out) :: numx(jmut)
    integer(4), intent(out) :: num_grid_1d
    logical :: l_found
    integer(4) :: j
    integer(4) :: num_grid_1d_truth=157792

    l_found = .false.

    if (gname=='TL319') then
      l_found = .true.
      num_grid_1d_truth=157792
      numx(1)      = 48
      numx(2)      = 64
      numx(3:4)    = 80
      numx(5)      = 96
      numx(6:7)    = 112
      numx(8:9)    = 128
      numx(10:11)  = 144
      numx(12:13)  = 160
      numx(14:17)  = 192
      numx(18:21)  = 224
      numx(22:23)  = 240
      numx(24:25)  = 256
      numx(26:30)  = 288
      numx(31:35)  = 320
      numx(36:37)  = 336
      numx(38:45)  = 384
      numx(46:48)  = 400
      numx(49:54)  = 432
      numx(55:57)  = 448
      numx(58:63)  = 480
      numx(64:70)  = 512
      numx(71:81)  = 560
      numx(82:86)  = 576
      numx(87:160) = 640
      do j = 1, 160
        numx(jmut-j+1) = numx(j)
      end do
      num_grid_1d = 0
      do j = 1, jmut
        num_grid_1d = num_grid_1d + numx(j)
      end do
      if (num_grid_1d_truth /= num_grid_1d) then
        write(6,*) ' Grid name = ',trim(gname), ', 1d total grid number = ',num_grid_1d
        stop
      end if
    end if

    if (.not. l_found) then
      write(6,*) ' Name of the grid is not found in the table '
    end if

  end subroutine set_reduced_grid

end program shift_2m_to_10m_hybrid
