! -*-F90-*-
program make_ensemble_mean_weight

  implicit none

  type :: type_reanl_data
    character(len=256) :: file_base
    character(len=256) :: file_name
    real(4) :: undef4
    real(8) :: weight
    real(8),pointer :: data_in(:)
    logical :: l_for_land
    logical :: l_for_ice
  end type type_reanl_data

  type(type_reanl_data),allocatable :: reanl_data(:)

  ! JRA-55 Grid

  integer(4) :: imut, jmut
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: total_grid_1d

  integer(4) :: ibgn, iend, i0

  real(4),allocatable :: work4(:)

  integer(4),parameter :: num_ens_max = 10
  integer(4) :: num_ens, ens_for_land, ens_for_ice

  character(len=256) :: file_base_tmp
  real(4) :: undef_tmp
  real(8) :: weight_tmp
  logical :: l_for_land_tmp
  logical :: l_for_ice_tmp

  real(8),allocatable :: dat_sort(:)
  real(8),allocatable :: wgt_sort(:)

  real(8),allocatable :: data_ens(:)
  real(8),allocatable :: data_mask(:)
  real(8),allocatable :: data_weight(:)
  real(8),allocatable :: data_ice(:)

  real(8),allocatable :: data_ens_latlon(:,:)

  character(256) :: file_monthly
  character(256) :: file_mask
  character(256) :: file_mask_weight
  character(256) :: file_ice, file_ice_base
  character(256) :: file_ens_org, file_ens_org_base

  character(256) :: file_ens_latlon, file_ens_latlon_base

  integer(4) :: i, j, ii, jj, n
  real(8) :: data_sum, total_weight

  integer(4) :: m

  integer(4) :: nbyr, neyr, month, nyear
  integer(4) :: nst, ned
       
  real(4) :: undef4_out
  real(8) :: undef8_out

  logical :: l_out_latlon

  ! for interpolation

  real(8),allocatable :: data_org(:), data_new(:)
  real(8),allocatable :: mask_org(:)
  real(8),allocatable :: lon_org(:), lon_new(:)
  real(8) :: dlon, dlon_rg, weight
  real(8),allocatable :: lat_org(:)

  integer(4),parameter :: lun=10

  !---------------------------------------------

  namelist /nml_make_ensemble_mean_weight/  &
       & num_ens,                     &
       & nbyr, neyr,                  &
       & file_mask,                   &
       & file_mask_weight,            &
       & file_ice_base,               &
       & imut, jmut, dlon, grid_name, &
       & file_ens_org_base,           &
       & file_ens_latlon_base,        &
       & undef4_out,                  &
       & l_out_latlon

  namelist /nml_reanl_data/ &
       & file_base_tmp,     &
       & undef_tmp,         &
       & weight_tmp,        &
       & l_for_land_tmp,    &
       & l_for_ice_tmp

  !---------------------------------------------

  l_out_latlon = .false.

  open(lun,file='namelist.make_ensemble_mean_weight')
  read(lun,nml=nml_make_ensemble_mean_weight)
  close(lun)

  undef8_out = real(undef4_out,8)

  !---------------------------------------------

  allocate(num_xgrid(1:jmut))
  allocate(data_ens_latlon(1:imut,1:jmut))
  allocate(data_new(1:imut))
  allocate(lon_new(1:imut))

  do i = 1, imut
    lon_new(i) = dlon * real(i-1,8)
  end do

  call set_reduced_grid(grid_name,num_xgrid,jmut,total_grid_1d)

  allocate(work4(1:total_grid_1d))

  allocate(data_ens (1:total_grid_1d))
  allocate(data_mask(1:total_grid_1d))
  allocate(data_weight(1:total_grid_1d))
  allocate(data_ice (1:total_grid_1d))

  !---------------------------------------------

  if (num_ens > num_ens_max) then
    write(6,*) ' num_ens must be <= ', num_ens_max
    stop
  end if

  allocate(reanl_data(1:num_ens))
  allocate(dat_sort(1:num_ens))
  allocate(wgt_sort(1:num_ens))

  do m = 1, num_ens
    allocate(reanl_data(m)%data_in(1:total_grid_1d))
  end do

  open(lun,file='namelist.make_ensemble_mean_weight')
  do m = 1, num_ens
    read(lun,nml=nml_reanl_data)
    reanl_data(m)%file_base = trim(file_base_tmp)
    reanl_data(m)%undef4 = undef_tmp
    reanl_data(m)%weight = weight_tmp
    reanl_data(m)%l_for_land = l_for_land_tmp
    reanl_data(m)%l_for_ice = l_for_ice_tmp
  end do
  close(lun)

  ens_for_land = 0
  ens_for_ice = 0
  do m = 1, num_ens
    if (reanl_data(m)%l_for_land) then
      ens_for_land = m
    end if
    if (reanl_data(m)%l_for_ice) then
      ens_for_ice = m
    end if
    write(6,'(1a,i4,1x,1a)') 'file base for   ', m , trim(reanl_data(m)%file_base)
    write(6,'(1a,i4,1x,e12.5)') 'undef value for ', m , reanl_data(m)%undef4
    write(6,'(1a,i4,1x,f12.5)') 'weight for      ', m , reanl_data(m)%weight
    write(6,'(1a,i4,1x,l4)') 'used for land   ', m , reanl_data(m)%l_for_land
    write(6,'(1a,i4,1x,l4)') 'used for ice    ', m , reanl_data(m)%l_for_ice
  end do

  if (ens_for_land == 0) then
    write(6,*) ' one of data set should be designated for land '
    stop
  else
    write(6,*) 'For land :  ',ens_for_land
  end if

  if (ens_for_ice == 0) then
    write(6,*) 'Ensemble is used over sea ice'
  else
    write(6,*) 'For sea ice :  ',ens_for_ice
  end if

  !----------------------------------------------

  open(lun,file=file_mask,form='unformatted',access='direct', &
       & convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_mask(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)
  do n = 1, total_grid_1d
    data_mask(n) = 1.0d0 - data_mask(n) ! 0 for land, 1 for water
  end do

  open(lun,file=file_mask_weight,form='unformatted',access='direct', &
       & convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_weight(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  !----------------------------------------------

  nst = nbyr
  ned = neyr

  do nyear = nst, ned

    do month = 1, 12

      data_ens(:) = 0.0d0

      write(6,*) 'Year = ', nyear, ' Month = ', month

      write(file_ice,'(1a,i4.4,i2.2)') trim(file_ice_base),nyear,month
      open(lun,file=file_ice,form='unformatted',access='direct', &
           & convert='little_endian',recl=4*total_grid_1d)
      write(6,*) 'reading ice from ', trim(file_ice)
      read(lun,rec=1) work4
      close(lun)
      data_ice(1:total_grid_1d) = real(work4(1:total_grid_1d),8)

      do m = 1, num_ens

        reanl_data(m)%data_in(n) = 0.0d0

        write(file_monthly,'(1a,i4.4,i2.2)') trim(reanl_data(m)%file_base),nyear,month
        open(lun,file=file_monthly,form='unformatted',access='direct', &
             & convert='little_endian',recl=4*total_grid_1d)
        write(6,*) 'reading from ', trim(file_monthly)
        read(lun,rec=1) work4
        close(lun)
        do n = 1, total_grid_1d
          if ((work4(n) /= reanl_data(m)%undef4) .and. (abs(work4(n)) < 1.0e10)) then
            reanl_data(m)%data_in(n) = real(work4(n),8)
          end if
        end do
      end do
      
      do n = 1, total_grid_1d

        if (data_mask(n) == 1.0d0) then

          do m = 1, num_ens
            dat_sort(m) = reanl_data(m)%data_in(n)
            if (reanl_data(m)%weight == 0.5d0) then
              wgt_sort(m) = reanl_data(m)%weight * data_weight(n)
            else
              wgt_sort(m) = reanl_data(m)%weight
            end if
          end do

          call hpsort2(num_ens,dat_sort,wgt_sort)

          data_sum = 0.0d0
          total_weight = 0.0d0

          data_sum = data_sum + dat_sort(1) * wgt_sort(1) * 0.5d0
          total_weight = total_weight + wgt_sort(1) * 0.5d0

          data_sum = data_sum + dat_sort(num_ens) * wgt_sort(num_ens) * 0.5d0
          total_weight = total_weight + wgt_sort(num_ens) * 0.5d0

          do m = 2, num_ens - 1
            data_sum = data_sum + dat_sort(m) * wgt_sort(m)
            total_weight = total_weight + wgt_sort(m)
          end do

          if (total_weight /= 0.0d0) then
            data_ens(n) = data_sum / total_weight
          else
            data_ens(n) = undef8_out
          end if

          !if (n == total_grid_1d/2) then
          !  do m = 1, num_ens
          !    write(6,*) m, dat_sort(m), wgt_sort(m)
          !  end do
          !  write(6,*) data_ens(n), total_weight
          !end if

          if (ens_for_ice /= 0) then
            if (data_ice(n) > 0.0d0) then ! if any sea ice
              data_ens(n) = reanl_data(ens_for_ice)%data_in(n)
            end if
          end if

        else

          data_ens(n) = reanl_data(ens_for_land)%data_in(n)

        end if

      end do
        
      write(file_ens_org,'(1a,i4.4,i2.2)') &
           & trim(file_ens_org_base),nyear,month
      write(6,*) ' outfile = ',trim(file_ens_org)
      open (lun,file=file_ens_org,form='unformatted',access='direct', &
           & convert='little_endian',recl=4*total_grid_1d)
      write(lun,rec=1) real(data_ens,4)
      close(lun)

      !-------------------------------------------------
      ! reduced grid to lat-lon grid for check
      ! Following is just for checking, do not use for scientific quality computation
    
      IF_LATLON: if (l_out_latlon) then 

        i0 = 0

        do j = 1, jmut

          ibgn = i0 + 1
          iend = i0 + num_xgrid(j)

          if (num_xgrid(j) == imut) then
            
            data_ens_latlon(1:imut,jmut-j+1) = data_ens(ibgn:iend)

          else

            allocate(data_org(1:num_xgrid(j)+1))
            allocate(lon_org (1:num_xgrid(j)+1))

            data_org(1:num_xgrid(j)) = data_ens(ibgn:iend)
            data_org(num_xgrid(j)+1) = data_ens(ibgn)

            dlon_rg = 360.0 / real(num_xgrid(j),8)

            do ii = 1, num_xgrid(j) + 1
              lon_org(ii) = dlon_rg * real(ii-1,8)
            end do

            do i = 1, imut
              do ii = 1, num_xgrid(j)
                if (abs(lon_org(ii) - lon_new(i)) < 1.0d-4) then
                  data_new(i) = data_org(ii)
                  exit
                else if ( (lon_org(ii) < lon_new(i)) .and. (lon_new(i) <= lon_org(ii+1)) ) then
                  weight = (lon_new(i) - lon_org(ii)) / (lon_org(ii+1) - lon_org(ii))
                  if ((data_org(ii) /= undef8_out) .and. (data_org(ii+1) /= undef8_out)) then
                    data_new(i) = (1.0d0 - weight) * data_org(ii) + weight * data_org(ii+1)
                  else
                    data_new(i) = undef8_out
                  end if
                  exit
                end if
              end do
            end do
            data_ens_latlon(1:imut,jmut-j+1) = data_new(1:imut)
            deallocate(data_org)
            deallocate(lon_org)
          end if
          i0 = iend
        end do

        write(file_ens_latlon,'(1a,i4.4,i2.2)') &
             & trim(file_ens_latlon_base),nyear,month
        write(6,*) ' outfile = ',trim(file_ens_latlon)
        open (lun,file=file_ens_latlon,form='unformatted',access='direct', &
             & recl=4*imut*jmut)
        write(lun,rec=1) real(data_ens_latlon,4)
        close(lun)
        
      end if IF_LATLON

      !---------------------------------------------      

    end do
  end do

end program make_ensemble_mean_weight
