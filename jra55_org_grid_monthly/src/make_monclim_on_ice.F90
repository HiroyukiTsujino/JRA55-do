! -*-F90-*-
program make_monthly_climatology_on_ice

  implicit none

  integer(4) :: imut, jmut
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: total_grid_1d

  integer(4) :: ibgn, iend, i0

  real(4),allocatable :: work4(:)

  real(8),allocatable :: data_in(:)
  real(8),allocatable :: data_ice(:)
  real(8),allocatable :: data_mask(:)
  real(8),allocatable :: data_cl1(:), data_cl2(:)
  integer(4),allocatable :: nvalid1(:)
  integer(4),allocatable :: nvalid2(:)

  real(8),allocatable :: data_cl_latlon1(:,:)
  real(8),allocatable :: data_cl_latlon2(:,:)

  real(8),allocatable :: data_org1(:), data_new1(:)
  real(8),allocatable :: data_org2(:), data_new2(:)
  real(8),allocatable :: mask_org(:)
  real(8),allocatable :: lon_org(:), lon_new(:)
  real(8) :: dlon, dlon_rg

  character(256) :: file_mask
  character(256) :: file_ice, file_ice_base
  character(256) :: file_monthly1, file_monthly_base1
  character(256) :: file_monthly2, file_monthly_base2

  character(256) :: file_clim_org1, file_clim_org_base1
  character(256) :: file_clim_org2, file_clim_org_base2
  character(256) :: file_clim_latlon1, file_clim_latlon_base1
  character(256) :: file_clim_latlon2, file_clim_latlon_base2

  integer(4),parameter :: lun=10

  integer(4) :: i, j, ii, jj, n
  real(8) :: weight

  integer(4) :: iii, m, m_left, m_right, i_left, i_right

  integer(4) :: nbyr, neyr, month, nyear
       
  real(4) :: undef4_in, undef4_out
  real(8) :: undef8_in, undef8_out

  !---------------------------------------------

  namelist /nml_make_monclim_on_ice/ &
       & file_mask,              &
       & file_monthly_base1,     &
       & file_monthly_base2,     &
       & file_ice_base,          &
       & undef4_in,              &
       & file_clim_org_base1,    &
       & file_clim_org_base2,    &
       & file_clim_latlon_base1, &
       & file_clim_latlon_base2, &
       & undef4_out,             &
       & imut, jmut, dlon, grid_name, &
       & nbyr, neyr

  !---------------------------------------------

  open(lun,file='namelist.make_monclim_on_ice')
  read(lun,nml=nml_make_monclim_on_ice)
  close(lun)

  undef8_in  = real(undef4_in,8)
  undef8_out = real(undef4_out,8)

  !---------------------------------------------

  allocate(num_xgrid(1:jmut))
  allocate(data_cl_latlon1(1:imut,1:jmut))
  allocate(data_cl_latlon2(1:imut,1:jmut))
  allocate(data_new1(1:imut),data_new2(1:imut))
  allocate(lon_new(1:imut))

  do i = 1, imut
    lon_new(i) = dlon * real(i-1,8)
  end do

  call set_reduced_grid(grid_name,num_xgrid,jmut,total_grid_1d)

  allocate(work4(1:total_grid_1d))

  allocate(data_in  (1:total_grid_1d))
  allocate(data_ice (1:total_grid_1d))
  allocate(data_cl1 (1:total_grid_1d))
  allocate(data_cl2 (1:total_grid_1d))
  allocate(nvalid1  (1:total_grid_1d))
  allocate(nvalid2  (1:total_grid_1d))
  allocate(data_mask(1:total_grid_1d))

  !----------------------------------------------

  open(lun,file=file_mask,form='unformatted',access='direct', &
       & convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_mask(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)
  do n = 1, total_grid_1d
    data_mask(n) = 1.0d0 - data_mask(n) ! 0 for land, 1 for water
  end do

  !----------------------------------------------

  do month = 1, 12

    data_cl1(:) = 0.0d0
    data_cl2(:) = 0.0d0
    nvalid1(:) = 0
    nvalid2(:) = 0

    do nyear = nbyr, neyr

      write(6,*) 'Year = ', nyear, ' Month = ', month

      write(file_ice,'(1a,i4.4,i2.2)') trim(file_ice_base),nyear,month
      open(lun,file=file_ice,form='unformatted',access='direct',&
           & convert='little_endian',recl=4*total_grid_1d)
      read(lun,rec=1) work4
      close(lun)

      do n = 1, total_grid_1d
        if (work4(n) /= undef4_in) then
          data_ice(n) = real(work4(n),8)
        end if
      end do

      write(file_monthly1,'(1a,i4.4,i2.2)') trim(file_monthly_base1),nyear,month
      open(lun,file=file_monthly1,form='unformatted',access='direct',&
           & convert='little_endian',recl=4*total_grid_1d)
      read(lun,rec=1) work4
      close(lun)
      do n = 1, total_grid_1d
        if ((data_mask(n) > 0.0d0) .and. (data_ice(n) > 0.0d0) &
             & .and. (work4(n) /= undef4_in) .and. (n < total_grid_1d/2)) then
          data_cl1(n) = data_cl1(n) + real(work4(n),8)
          nvalid1(n) = nvalid1(n) + 1
        end if
      end do

      write(file_monthly2,'(1a,i4.4,i2.2)') trim(file_monthly_base2),nyear,month
      open(lun,file=file_monthly2,form='unformatted',access='direct',&
           & convert='little_endian',recl=4*total_grid_1d)
      read(lun,rec=1) work4
      close(lun)
      do n = 1, total_grid_1d
        if (work4(n) /= undef4_in) then
          data_cl2(n) = data_cl2(n) + real(work4(n),8)
          nvalid2(n) = nvalid2(n) + 1
        end if
      end do

    end do

    do n = 1, total_grid_1d
      if (nvalid2(n) == (neyr-nbyr+1) ) then
        data_cl2(n) = data_cl2(n) / real(nvalid2(n),8)
      else
        data_cl2(n) = undef8_out
      end if
    end do

    do n = 1, total_grid_1d
      if (nvalid1(n) >= (neyr-nbyr+1)/2 ) then
        data_cl1(n) = data_cl1(n) / real(nvalid1(n),8)
      else
        data_cl1(n) = data_cl2(n)
      end if
    end do

    ! reduced grid to lat-lon grid for check

    i0 = 0

    do j = 1, jmut

      ibgn = i0 + 1
      iend = i0 + num_xgrid(j)

      if (num_xgrid(j) == imut) then

        data_cl_latlon1(1:imut,jmut-j+1) = data_cl1(ibgn:iend)
        data_cl_latlon2(1:imut,jmut-j+1) = data_cl2(ibgn:iend)

      else

        allocate(data_org1(1:num_xgrid(j)+1))
        allocate(data_org2(1:num_xgrid(j)+1))
        allocate(lon_org  (1:num_xgrid(j)+1))

        data_org1(1:num_xgrid(j)) = data_cl1(ibgn:iend)
        data_org1(num_xgrid(j)+1) = data_cl1(ibgn)
        data_org2(1:num_xgrid(j)) = data_cl2(ibgn:iend)
        data_org2(num_xgrid(j)+1) = data_cl2(ibgn)

        dlon_rg = 360.0 / real(num_xgrid(j),8)

        do ii = 1, num_xgrid(j) + 1
          lon_org(ii) = dlon_rg * real(ii-1,8)
        end do

        do i = 1, imut
          do ii = 1, num_xgrid(j)
            if (abs(lon_org(ii) - lon_new(i)) < 1.0d-4) then
              data_new1(i) = data_org1(ii)
              data_new2(i) = data_org2(ii)
              exit
            else if ( (lon_org(ii) < lon_new(i)) .and. (lon_new(i) <= lon_org(ii+1)) ) then
              weight = (lon_new(i) - lon_org(ii)) / (lon_org(ii+1) - lon_org(ii))
              data_new1(i) = (1.0d0 - weight) * data_org1(ii) + weight * data_org1(ii+1)
              data_new2(i) = (1.0d0 - weight) * data_org2(ii) + weight * data_org2(ii+1)
              exit
            end if
          end do
        end do
        data_cl_latlon1(1:imut,jmut-j+1) = data_new1(1:imut)
        data_cl_latlon2(1:imut,jmut-j+1) = data_new2(1:imut)
        deallocate(data_org1,data_org2)
        deallocate(lon_org)
      end if
      i0 = iend
    end do

    write(file_clim_org1,'(1a,i2.2)') trim(file_clim_org_base1),month
    write(6,*) ' outfile = ',trim(file_clim_org1)
    open (lun,file=file_clim_org1,form='unformatted',access='direct', &
         & convert='little_endian',recl=4*total_grid_1d)
    write(lun,rec=1) real(data_cl1,4)
    close(lun)

    write(file_clim_org2,'(1a,i2.2)') trim(file_clim_org_base2),month
    write(6,*) ' outfile = ',trim(file_clim_org2)
    open (lun,file=file_clim_org2,form='unformatted',access='direct', &
         & convert='little_endian',recl=4*total_grid_1d)
    write(lun,rec=1) real(data_cl2,4)
    close(lun)

    write(file_clim_latlon1,'(1a,i2.2)') trim(file_clim_latlon_base1),month
    write(6,*) ' outfile = ',trim(file_clim_latlon1)
    open (lun,file=file_clim_latlon1,form='unformatted',access='direct',recl=4*imut*jmut)
    write(lun,rec=1) real(data_cl_latlon1,4)
    close(lun)

    write(file_clim_latlon2,'(1a,i2.2)') trim(file_clim_latlon_base2),month
    write(6,*) ' outfile = ',trim(file_clim_latlon2)
    open (lun,file=file_clim_latlon2,form='unformatted',access='direct',recl=4*imut*jmut)
    write(lun,rec=1) real(data_cl_latlon2,4)
    close(lun)

  end do

end program make_monthly_climatology_on_ice
