!-*-F90-*-
program make_annual_climatology_to_replace

  implicit none

  integer(4) :: imut, jmut
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: total_grid_1d

  integer(4) :: ibgn, iend, i0

  real(4),allocatable :: work4(:)

  real(8),allocatable :: data_mask(:)

  real(8),allocatable :: data_u_cl1(:), data_u_cl2(:)
  real(8),allocatable :: data_u_cl3(:), data_u_cl4(:) 

  real(8),allocatable :: data_v_cl1(:), data_v_cl2(:)
  real(8),allocatable :: data_v_cl3(:), data_v_cl4(:) 

  real(8),allocatable :: data_u_cl_latlon1(:,:)
  real(8),allocatable :: data_u_cl_latlon2(:,:)
  real(8),allocatable :: data_u_cl_latlon3(:,:)
  real(8),allocatable :: data_u_cl_latlon4(:,:)

  real(8),allocatable :: data_v_cl_latlon1(:,:)
  real(8),allocatable :: data_v_cl_latlon2(:,:)
  real(8),allocatable :: data_v_cl_latlon3(:,:)
  real(8),allocatable :: data_v_cl_latlon4(:,:)

  real(8),allocatable :: data_u_org1(:), data_u_new1(:)
  real(8),allocatable :: data_u_org2(:), data_u_new2(:)
  real(8),allocatable :: data_u_org3(:), data_u_new3(:)
  real(8),allocatable :: data_u_org4(:), data_u_new4(:)

  real(8),allocatable :: data_v_org1(:), data_v_new1(:)
  real(8),allocatable :: data_v_org2(:), data_v_new2(:)
  real(8),allocatable :: data_v_org3(:), data_v_new3(:)
  real(8),allocatable :: data_v_org4(:), data_v_new4(:)

  real(8),allocatable :: lon_org(:), lon_new(:)
  real(8) :: dlon, dlon_rg

  character(256) :: file_mask

  character(256) :: file_u_clim_in_org1
  character(256) :: file_u_clim_in_org2
  character(256) :: file_u_clim_in_org3
  character(256) :: file_u_clim_out_org4

  character(256) :: file_v_clim_in_org1
  character(256) :: file_v_clim_in_org2
  character(256) :: file_v_clim_in_org3
  character(256) :: file_v_clim_out_org4

  character(256) :: file_u_clim_out_latlon
  character(256) :: file_v_clim_out_latlon

  integer(4),parameter :: lun=10

  integer(4) :: i, j, ii, jj, n
  real(8) :: weight

  integer(4) :: iii, m, m_left, m_right, i_left, i_right

  real(4) :: undef4_in, undef4_out
  real(8) :: undef8_in, undef8_out

  logical :: l_out_latlon

  !---------------------------------------------

  namelist /nml_make_annclim_replace/ &
       & file_mask,              &
       & undef4_in,              &
       & file_u_clim_in_org1,    &
       & file_u_clim_in_org2,    &
       & file_u_clim_in_org3,    &
       & file_u_clim_out_org4,   &
       & file_v_clim_in_org1,    &
       & file_v_clim_in_org2,    &
       & file_v_clim_in_org3,    &
       & file_v_clim_out_org4,   &
       & file_u_clim_out_latlon, &
       & file_v_clim_out_latlon, &
       & undef4_out,             &
       & imut, jmut, dlon, grid_name, &
       & l_out_latlon

  !---------------------------------------------

  l_out_latlon = .false.

  open(lun,file='namelist.make_annclim_to_replace')
  read(lun,nml=nml_make_annclim_replace)
  close(lun)

  undef8_in  = real(undef4_in,8)
  undef8_out = real(undef4_out,8)

  !---------------------------------------------

  allocate(num_xgrid(1:jmut))
  allocate(data_u_cl_latlon1(1:imut,1:jmut))
  allocate(data_u_cl_latlon2(1:imut,1:jmut))
  allocate(data_u_cl_latlon3(1:imut,1:jmut))
  allocate(data_u_cl_latlon4(1:imut,1:jmut))
  allocate(data_v_cl_latlon1(1:imut,1:jmut))
  allocate(data_v_cl_latlon2(1:imut,1:jmut))
  allocate(data_v_cl_latlon3(1:imut,1:jmut))
  allocate(data_v_cl_latlon4(1:imut,1:jmut))
  allocate(data_u_new1(1:imut),data_u_new2(1:imut))
  allocate(data_u_new3(1:imut),data_u_new4(1:imut))
  allocate(data_v_new1(1:imut),data_v_new2(1:imut))
  allocate(data_v_new3(1:imut),data_v_new4(1:imut))
  allocate(lon_new(1:imut))

  do i = 1, imut
    lon_new(i) = dlon * real(i-1,8)
  end do

  call set_reduced_grid(grid_name,num_xgrid,jmut,total_grid_1d)

  allocate(work4(1:total_grid_1d))

  allocate(data_u_cl1(1:total_grid_1d))
  allocate(data_u_cl2(1:total_grid_1d))
  allocate(data_u_cl3(1:total_grid_1d))
  allocate(data_u_cl4(1:total_grid_1d))

  allocate(data_v_cl1(1:total_grid_1d))
  allocate(data_v_cl2(1:total_grid_1d))
  allocate(data_v_cl3(1:total_grid_1d))
  allocate(data_v_cl4(1:total_grid_1d))

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
  ! Org 1973-1996

  open(lun,file=file_u_clim_in_org1,form='unformatted',access='direct', &
       & convert='little_endian', action='read', recl=4*total_grid_1d)
  read(lun,rec=1) work4
  close(lun)
  data_u_cl1(1:total_grid_1d) = real(work4(1:total_grid_1d),8)

  open(lun,file=file_v_clim_in_org1,form='unformatted',access='direct', &
       & convert='little_endian', action='read', recl=4*total_grid_1d)
  read(lun,rec=1) work4
  close(lun)
  data_v_cl1(1:total_grid_1d) = real(work4(1:total_grid_1d),8)

  !----------------------------------------------
  ! Org 1999-2009

  open(lun,file=file_u_clim_in_org2,form='unformatted',access='direct', &
       & convert='little_endian', action='read', recl=4*total_grid_1d)
  read(lun,rec=1) work4
  close(lun)
  data_u_cl2(1:total_grid_1d) = real(work4(1:total_grid_1d),8)

  open(lun,file=file_v_clim_in_org2,form='unformatted',access='direct', &
       & convert='little_endian', action='read', recl=4*total_grid_1d)
  read(lun,rec=1) work4
  close(lun)
  data_v_cl2(1:total_grid_1d) = real(work4(1:total_grid_1d),8)

  !----------------------------------------------
  ! QuikSCAT 1999-2009

  open(lun,file=file_u_clim_in_org3,form='unformatted',access='direct', &
       & convert='little_endian', action='read', recl=4*total_grid_1d)
  read(lun,rec=1) work4
  close(lun)
  data_u_cl3(1:total_grid_1d) = real(work4(1:total_grid_1d),8)

  open(lun,file=file_v_clim_in_org3,form='unformatted',access='direct', &
       & convert='little_endian', action='read', recl=4*total_grid_1d)
  read(lun,rec=1) work4
  close(lun)
  data_v_cl3(1:total_grid_1d) = real(work4(1:total_grid_1d),8)

  !----------------------------------------------

  do n = 1, total_grid_1d
    data_u_cl4(n) = data_u_cl1(n) - data_u_cl2(n) + data_u_cl3(n)
    data_v_cl4(n) = data_v_cl1(n) - data_v_cl2(n) + data_v_cl3(n)
  end do

  !----------------------------------------------

  write(6,*) ' outfile = ',trim(file_u_clim_out_org4)
  open (lun,file=file_u_clim_out_org4,form='unformatted',access='direct', &
       & convert='little_endian', action='write',recl=4*total_grid_1d)
  write(lun,rec=1) real(data_u_cl4,4)
  close(lun)

  write(6,*) ' outfile = ',trim(file_v_clim_out_org4)
  open (lun,file=file_v_clim_out_org4,form='unformatted',access='direct', &
       & convert='little_endian', action='write',recl=4*total_grid_1d)
  write(lun,rec=1) real(data_v_cl4,4)
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

        data_u_cl_latlon1(1:imut,jmut-j+1) = data_u_cl1(ibgn:iend)
        data_u_cl_latlon2(1:imut,jmut-j+1) = data_u_cl2(ibgn:iend)
        data_u_cl_latlon3(1:imut,jmut-j+1) = data_u_cl3(ibgn:iend)
        data_u_cl_latlon4(1:imut,jmut-j+1) = data_u_cl4(ibgn:iend)

        data_v_cl_latlon1(1:imut,jmut-j+1) = data_v_cl1(ibgn:iend)
        data_v_cl_latlon2(1:imut,jmut-j+1) = data_v_cl2(ibgn:iend)
        data_v_cl_latlon3(1:imut,jmut-j+1) = data_v_cl3(ibgn:iend)
        data_v_cl_latlon4(1:imut,jmut-j+1) = data_v_cl4(ibgn:iend)

      else

        allocate(data_u_org1(1:num_xgrid(j)+1))
        allocate(data_u_org2(1:num_xgrid(j)+1))
        allocate(data_u_org3(1:num_xgrid(j)+1))
        allocate(data_u_org4(1:num_xgrid(j)+1))

        allocate(data_v_org1(1:num_xgrid(j)+1))
        allocate(data_v_org2(1:num_xgrid(j)+1))
        allocate(data_v_org3(1:num_xgrid(j)+1))
        allocate(data_v_org4(1:num_xgrid(j)+1))

        allocate(lon_org  (1:num_xgrid(j)+1))

        data_u_org1(1:num_xgrid(j)) = data_u_cl1(ibgn:iend)
        data_u_org1(num_xgrid(j)+1) = data_u_cl1(ibgn)
        data_v_org1(1:num_xgrid(j)) = data_v_cl1(ibgn:iend)
        data_v_org1(num_xgrid(j)+1) = data_v_cl1(ibgn)

        data_u_org2(1:num_xgrid(j)) = data_u_cl2(ibgn:iend)
        data_u_org2(num_xgrid(j)+1) = data_u_cl2(ibgn)
        data_v_org2(1:num_xgrid(j)) = data_v_cl2(ibgn:iend)
        data_v_org2(num_xgrid(j)+1) = data_v_cl2(ibgn)

        data_u_org3(1:num_xgrid(j)) = data_u_cl3(ibgn:iend)
        data_u_org3(num_xgrid(j)+1) = data_u_cl3(ibgn)
        data_v_org3(1:num_xgrid(j)) = data_v_cl3(ibgn:iend)
        data_v_org3(num_xgrid(j)+1) = data_v_cl3(ibgn)

        data_u_org4(1:num_xgrid(j)) = data_u_cl4(ibgn:iend)
        data_u_org4(num_xgrid(j)+1) = data_u_cl4(ibgn)
        data_v_org4(1:num_xgrid(j)) = data_v_cl4(ibgn:iend)
        data_v_org4(num_xgrid(j)+1) = data_v_cl4(ibgn)

        dlon_rg = 360.0 / real(num_xgrid(j),8)

        do ii = 1, num_xgrid(j) + 1
          lon_org(ii) = dlon_rg * real(ii-1,8)
        end do

        do i = 1, imut
          do ii = 1, num_xgrid(j)
            if (abs(lon_org(ii) - lon_new(i)) < 1.0d-4) then
              data_u_new1(i) = data_u_org1(ii)
              data_v_new1(i) = data_v_org1(ii)
              data_u_new2(i) = data_u_org2(ii)
              data_v_new2(i) = data_v_org2(ii)
              data_u_new3(i) = data_u_org3(ii)
              data_v_new3(i) = data_v_org3(ii)
              data_u_new4(i) = data_u_org4(ii)
              data_v_new4(i) = data_v_org4(ii)
              exit
            else if ( (lon_org(ii) < lon_new(i)) .and. (lon_new(i) <= lon_org(ii+1)) ) then
              weight = (lon_new(i) - lon_org(ii)) / (lon_org(ii+1) - lon_org(ii))
              data_u_new1(i) = (1.0d0 - weight) * data_u_org1(ii) + weight * data_u_org1(ii+1)
              data_v_new1(i) = (1.0d0 - weight) * data_v_org1(ii) + weight * data_v_org1(ii+1)
              data_u_new2(i) = (1.0d0 - weight) * data_u_org2(ii) + weight * data_u_org2(ii+1)
              data_v_new2(i) = (1.0d0 - weight) * data_v_org2(ii) + weight * data_v_org2(ii+1)
              data_u_new3(i) = (1.0d0 - weight) * data_u_org3(ii) + weight * data_u_org3(ii+1)
              data_v_new3(i) = (1.0d0 - weight) * data_v_org3(ii) + weight * data_v_org3(ii+1)
              data_u_new4(i) = (1.0d0 - weight) * data_u_org4(ii) + weight * data_u_org4(ii+1)
              data_v_new4(i) = (1.0d0 - weight) * data_v_org4(ii) + weight * data_v_org4(ii+1)
              exit
            end if
          end do
        end do
        data_u_cl_latlon1(1:imut,jmut-j+1) = data_u_new1(1:imut)
        data_v_cl_latlon1(1:imut,jmut-j+1) = data_v_new1(1:imut)
        data_u_cl_latlon2(1:imut,jmut-j+1) = data_u_new2(1:imut)
        data_v_cl_latlon2(1:imut,jmut-j+1) = data_v_new2(1:imut)
        data_u_cl_latlon3(1:imut,jmut-j+1) = data_u_new3(1:imut)
        data_v_cl_latlon3(1:imut,jmut-j+1) = data_v_new3(1:imut)
        data_u_cl_latlon4(1:imut,jmut-j+1) = data_u_new4(1:imut)
        data_v_cl_latlon4(1:imut,jmut-j+1) = data_v_new4(1:imut)
        deallocate(data_u_org1,data_v_org1,data_u_org2,data_v_org2)
        deallocate(data_u_org3,data_v_org3,data_u_org4,data_v_org4)
        deallocate(lon_org)
      end if
      i0 = iend
    end do

    open (lun,file=trim(file_u_clim_out_latlon),form='unformatted',access='direct', action='write',recl=4*imut*jmut)
    write(lun,rec=1) real(data_u_cl_latlon1,4)
    write(lun,rec=2) real(data_u_cl_latlon2,4)
    write(lun,rec=3) real(data_u_cl_latlon3,4)
    write(lun,rec=4) real(data_u_cl_latlon4,4)
    close(lun)

    open (lun,file=trim(file_v_clim_out_latlon),form='unformatted',access='direct', action='write',recl=4*imut*jmut)
    write(lun,rec=1) real(data_v_cl_latlon1,4)
    write(lun,rec=2) real(data_v_cl_latlon2,4)
    write(lun,rec=3) real(data_v_cl_latlon3,4)
    write(lun,rec=4) real(data_v_cl_latlon4,4)
    close(lun)

  end if IF_LATLON

end program make_annual_climatology_to_replace
