! -*-F90-*-
program make_correction_for_radiation_monthly

  use libmxe_para, only: libmxe_para__register &
                     & , clen, pi, radian, radian_r, radius &
                     & , type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo
  use libmxe_io, only: libmxe_io__register, type_libmxe_io

  use file_open_close_manager

  implicit none

  integer(4) :: imut, jmut
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: total_grid_1d
  real(4) :: undef4_jra
  real(8) :: undef8_jra

  integer(4) :: ibgn, iend, i0

  real(4),allocatable :: work4(:)

  real(8),allocatable :: data_rad(:)
  real(8),allocatable :: data_rad_ref(:)
  real(8),allocatable :: data_rad_correc(:)

  real(8),allocatable :: rad_correc_latlon(:,:)

  real(8),allocatable :: rad_org(:), rad_new(:)
  real(8),allocatable :: mask_org(:)
  real(8),allocatable :: rad_blend_org(:)
  real(8),allocatable :: lon_org(:), lon_new(:)
  real(8) :: dlon, dlon_rg
  real(8),allocatable :: lat_org(:)

  character(256) :: file_rad_base
  character(256) :: file_rad
  character(256) :: file_mask

  character(256) :: file_rad_correc_org
  character(256) :: file_rad_correc_latlon
  character(len=258) :: file_ydef

  integer(4),parameter :: lun=10
  integer(4) :: mtin1, mtin2
  integer(4) :: mtot1, mtot2

  integer(4) :: i, j, ii, jj, n, month
  real(8) :: weight

  integer(4) :: iii, m, m_left, m_right, i_left, i_right

  integer(4) :: iceres, jceres, kceres
  integer(4) :: iw, ie, js, jn, ios
  real(8) :: wgt_x, wgt_y, tmp_ref, undef8_ref
  real(8) :: model_lat, model_lon
  real(4) :: undef4_ref
  character(len=256) :: file_ref_base
  character(len=256) :: file_ref
  real(8),allocatable :: rad_ref(:,:)
  real(4),allocatable :: work4_2d(:,:)
  real(8),allocatable :: lon_ref(:), lat_ref(:)

  type(type_libmxe_para) :: orgp
  type(type_libmxe_grid) :: orgg
  type(type_libmxe_topo) :: orgt

  !---------------------------------------------

  namelist /nml_make_radcorrec/ &
       & file_ref_base, &
       & undef4_ref, &
       & file_rad_base, &
       & file_rad_correc_org, &
       & file_rad_correc_latlon, &
       & undef4_jra, &
       & imut, jmut, dlon, grid_name, &
       & file_ydef

  !---------------------------------------------

  open(lun,file='namelist.make_radcorrec')
  read(lun,nml=nml_make_radcorrec)
  close(lun)

  !---------------------------------------------
  ! set reference data grid

  call libmxe_para__register(orgp, file_namelist='NAMELIST.MXE.CERES')

  iceres = orgp%imut
  jceres = orgp%jmut
  kceres = orgp%km
  write(6,*) iceres, jceres, kceres
  allocate(work4_2d(1:iceres,1:jceres))
  allocate(rad_ref(0:iceres+1,1:jceres))
  allocate(lon_ref(0:iceres+1),lat_ref(1:jceres))

  call libmxe_grid__register(orgg,orgp)

  undef8_ref = real(undef4_ref,8)

  lon_ref(1:iceres) = orgg%lonu(1:iceres)
  lon_ref(0) = orgg%lonu(iceres) - 360.0d0
  lon_ref(iceres+1) = orgg%lonu(1) + 360.0d0

  lat_ref(1:jceres) = orgg%latu(1:jceres)

  !---------------------------------------------
  ! set JRA-55 grid
 
  allocate(num_xgrid(1:jmut))
  allocate(lat_org(1:jmut))
  allocate(rad_correc_latlon(1:imut,1:jmut))
  allocate(rad_new(1:imut))
  allocate(lon_new(1:imut))

  do i = 1, imut
    lon_new(i) = dlon * real(i-1,8)
  end do

  open(lun,file=file_ydef)
  do j = 1, jmut
    read(lun,*,iostat=ios) lat_org(j)
    if (ios /= 0) then
      write(6,*) ' Error : inconsistent number of data ', jmut
      stop
    else
      !write(6,*) lat_org(j)
    end if
  end do
  close(lun)

  call set_reduced_grid(grid_name,num_xgrid,jmut,total_grid_1d)

  allocate(work4(1:total_grid_1d))

  allocate(data_rad(1:total_grid_1d))
  allocate(data_rad_ref(1:total_grid_1d))
  allocate(data_rad_correc(1:total_grid_1d))

  undef8_jra = real(undef4_jra,8)

  !------

  call open_file_direct(mtot1,file_rad_correc_org,4*total_grid_1d, &
       & convert_mode='little_endian',action='write')

  call open_file_direct(mtot2,file_rad_correc_latlon,4*imut*jmut, &
       & action='write')

  do month = 1, 12

    write(file_ref,'(1a,i2.2)') trim(file_ref_base),month
    open(lun,file=file_ref,form='unformatted',access='direct',recl=4*iceres*jceres)
    write(6,*) ' file : ', trim(file_ref), ' opened '
    read(lun,rec=1) work4_2d
    rad_ref(1:iceres,1:jceres) = real(work4_2d(1:iceres,1:jceres),8)
    close(lun)
    rad_ref(0       ,1:jceres) = rad_ref(iceres,1:jceres) 
    rad_ref(iceres+1,1:jceres) = rad_ref(1     ,1:jceres) 

    !-------

    write(file_rad,'(1a,i2.2)') trim(file_rad_base),month
    open(lun,file=file_rad,form='unformatted',access='direct', &
         & convert='little_endian',recl=4*total_grid_1d)
    write(6,*) ' file : ', trim(file_rad), ' opened '
    read(lun,rec=1) work4
    data_rad(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
    close(lun)

    !----------------------------------------------------------------------------
    ! interplate reference data to JRA-55 reduced grid

    i0 = 0

    do j = 1, jmut

      model_lat = lat_org(jmut-j+1)

      ibgn = i0 + 1
      iend = i0 + num_xgrid(j)

      do jj = 2, jceres
        if ((lat_ref(jj-1) <= model_lat) .and. (model_lat < lat_ref(jj))) then
          jn = jj
          js = jj-1
          wgt_y = (model_lat - lat_ref(js)) / (lat_ref(jn) - lat_ref(js))
          exit
        end if
      end do
      if (model_lat < lat_ref(1)) then
        jn = 1
        js = 1
        wgt_y = 1.0d0
      end if
      if (lat_ref(jceres) <= model_lat) then
        jn = jceres
        js = jceres
        wgt_y = 1.0d0
      end if

      allocate(rad_org(1:num_xgrid(j)))
      allocate(lon_org(1:num_xgrid(j)))

      dlon_rg = 360.0 / real(num_xgrid(j),8)

      do i = 1, num_xgrid(j)
        lon_org(i) = dlon_rg * real(i-1,8)
      end do

      do i = 1, num_xgrid(j)
        model_lon = lon_org(i)
        loop_ceres_i: do ii = 1, iceres + 1
          if ((lon_ref(ii-1) <= model_lon) .and. (model_lon < lon_ref(ii))) then
            iw = ii - 1
            ie = ii
            wgt_x = (model_lon - lon_ref(iw)) / (lon_ref(ie) - lon_ref(iw))
            if ( (rad_ref(iw,js) /= undef8_ref) .and. &
               & (rad_ref(ie,js) /= undef8_ref) .and. &
               & (rad_ref(iw,jn) /= undef8_ref) .and. &
               & (rad_ref(ie,jn) /= undef8_ref)) then
              tmp_ref = (1.0d0-wgt_x)*(1.0d0-wgt_y)*rad_ref(iw,js) &
                   & +         wgt_x *(1.0d0-wgt_y)*rad_ref(ie,js) &
                   & +  (1.0d0-wgt_x)* wgt_y       *rad_ref(iw,jn) &
                   & +         wgt_x * wgt_y       *rad_ref(ie,jn)
            else
              tmp_ref = undef8_ref
            end if
            exit loop_ceres_i
          end if
        end do loop_ceres_i

        rad_org(i) = tmp_ref

      end do

      data_rad_ref(ibgn:iend) = rad_org(1:num_xgrid(j))

      deallocate(rad_org)
      deallocate(lon_org)

      i0 = iend

    end do

    do i = 1, total_grid_1d
      !if (data_mask(i,j) == 1.0d0) then
        if ((data_rad_ref(i) > 5.0d0) .and. (data_rad(i) > 5.0d0)) then
          data_rad_correc(i) = data_rad_ref(i) / data_rad(i)
        else
          data_rad_correc(i) = 1.0d0
        end if
      !else
      !  data_rad_correc(i,j) = undef8_jra
      !end if
    end do

    write(mtot1,rec=month) real(data_rad_correc(1:total_grid_1d),4)

    !--------------------------------------------
    ! reduced grid to lat-lon grid for check

    i0 = 0

    do j = 1, jmut

      ibgn = i0 + 1
      iend = i0 + num_xgrid(j)

      if (num_xgrid(j) == imut) then

        rad_correc_latlon(1:imut,jmut-j+1) = data_rad_correc(ibgn:iend)

      else

        allocate(rad_org(1:num_xgrid(j)+1))
        allocate(lon_org(1:num_xgrid(j)+1))

        rad_org(1:num_xgrid(j)) = data_rad_correc(ibgn:iend)
        rad_org(num_xgrid(j)+1) = data_rad_correc(ibgn)

        dlon_rg = 360.0 / real(num_xgrid(j),8)
        do ii = 1, num_xgrid(j) + 1
          lon_org(ii) = dlon_rg * real(ii-1,8)
        end do
        do i = 1, imut
          do ii = 1, num_xgrid(j)
            if (abs(lon_org(ii) - lon_new(i)) < 1.0d-4) then
              rad_new(i) = rad_org(ii)
              exit
            else if ( (lon_org(ii) < lon_new(i)) .and. (lon_new(i) <= lon_org(ii+1)) ) then
              weight = (lon_new(i) - lon_org(ii)) / (lon_org(ii+1) - lon_org(ii))
              rad_new(i) = (1.0d0 - weight) * rad_org(ii) + weight * rad_org(ii+1)
              exit
            end if
          end do
        end do
        rad_correc_latlon(1:imut,jmut-j+1) = rad_new(1:imut)
        deallocate(rad_org)
        deallocate(lon_org)
      end if
      i0 = iend
    end do

    write(mtot2,rec=month) real(rad_correc_latlon,4)

    !---------------------------

  end do

  close(lun)

  !---------------------------------------------

end program make_correction_for_radiation_monthly
