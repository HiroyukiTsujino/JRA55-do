! -*-F90-*-
program make_monthly_correction_radiation

  implicit none

  integer(4) :: imut, jmut
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: total_grid_1d

  integer(4) :: ibgn, iend, i0

  real(4),allocatable :: work4(:)

  real(8),allocatable :: data_ref(:)
  real(8),allocatable :: data_jra(:)
  real(8),allocatable :: data_correc(:)
  real(8),allocatable :: data_mask(:)

  real(8),allocatable :: data_correc_latlon(:,:)

  real(8) :: hl1, hl2

  ! for interpolation

  real(8),allocatable :: data_org(:), data_new(:)
  real(8),allocatable :: mask_org(:)
  real(8),allocatable :: lon_org(:), lon_new(:)
  real(8) :: dlon, dlon_rg

  character(256) :: file_ref, file_ref_base
  character(256) :: file_jra, file_jra_base
  character(256) :: file_mask

  character(256) :: file_correc
  character(256) :: file_correc_latlon

  integer(4),parameter :: lun=10, lun1=11, lun2=12

  integer(4) :: i, j, ii, jj, n
  real(8) :: weight

  integer(4) :: iii, m, m_left, m_right, i_left, i_right

  integer(4) :: nbyr, neyr, month, nyear
       
  real(4) :: undef4_jra, undef4_ref, undef4_out
  real(8) :: undef8_jra, undef8_ref, undef8_out

  !---------------------------------------------

  namelist /nml_make_radcorrec_nat/ &
       &  file_mask,              &
       &  file_ref_base,          &
       &  file_jra_base,          &
       &  undef4_jra, undef4_ref, &
       &  file_correc,            &
       &  file_correc_latlon,     &
       &  undef4_out,             &
       &  imut, jmut, dlon, grid_name

  !---------------------------------------------

  open (lun,file='namelist.make_radcorrec_nat')
  read (lun,nml=nml_make_radcorrec_nat)
  close(lun)

  undef8_jra = real(undef4_jra,8)
  undef8_ref = real(undef4_ref,8)
  undef8_out = real(undef4_out,8)

  !---------------------------------------------

  allocate(num_xgrid(1:jmut))
  allocate(data_correc_latlon(1:imut,1:jmut))
  allocate(data_new(1:imut))
  allocate(lon_new(1:imut))

  do i = 1, imut
    lon_new(i) = dlon * real(i-1,8)
  end do

  call set_reduced_grid(grid_name,num_xgrid,jmut,total_grid_1d)

  write(6,*) ' total_grid_1d of ', trim(grid_name),' = ', total_grid_1d

  allocate(work4(1:total_grid_1d))

  allocate(data_ref   (1:total_grid_1d))
  allocate(data_jra   (1:total_grid_1d))
  allocate(data_mask  (1:total_grid_1d))
  allocate(data_correc(1:total_grid_1d))

  !----------------------------------------------

  open(lun,file=file_mask,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_mask(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)
  do n = 1, total_grid_1d
    data_mask(n) = 1.0d0 - data_mask(n) ! 0 for land, 1 for water
  end do

  write(6,*) ' outfile = ',trim(file_correc)
  open (lun1,file=file_correc,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)

  write(6,*) ' outfile = ',trim(file_correc_latlon)
  open (lun2,file=file_correc_latlon,form='unformatted',access='direct',recl=4*imut*jmut)

  !----------------------------------------------

  do month = 1, 12

    data_ref(:) = 0.0d0
    data_jra(:) = 0.0d0
    data_correc(:) = 0.0d0

    write(file_ref,'(1a,i2.2)') trim(file_ref_base),month
    open(lun,file=file_ref,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
    read(lun,rec=1) work4
    close(lun)

    do n = 1, total_grid_1d
      if (work4(n) /= undef4_ref) then
        data_ref(n) = real(work4(n),8)
      else
        data_ref(n) = 0.0d0
      end if
    end do

    write(file_jra,'(1a,i2.2)') trim(file_jra_base),month
    open(lun,file=file_jra,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
    read(lun,rec=1) work4
    close(lun)

    do n = 1, total_grid_1d
      if (work4(n) /= undef4_jra) then
        data_jra(n) = real(work4(n),8)
      else
        data_jra(n) = 0.0d0
      end if
    end do

    do n = 1, total_grid_1d
!      if ((data_ref(n) /= 0.0d0) .and. (data_jra(n) /= 0.0d0)) then
      if ((data_ref(n) > 5.0d0) .and. (data_jra(n) > 5.0d0)) then
        data_correc(n) = data_ref(n) / data_jra(n)
      else
        data_correc(n) = 1.0d0
      end if
    end do

    write(lun1,rec=month) real(data_correc,4)

    !----------------------------------------------------------------
    ! reduced grid to lat-lon grid for check

    i0 = 0

    do j = 1, jmut

      ibgn = i0 + 1
      iend = i0 + num_xgrid(j)

      if (num_xgrid(j) == imut) then

        data_correc_latlon(1:imut,jmut-j+1) = data_correc(ibgn:iend)

      else

        allocate(data_org(1:num_xgrid(j)+1))
        allocate(lon_org (1:num_xgrid(j)+1))

        data_org(1:num_xgrid(j)) = data_correc(ibgn:iend)
        data_org(num_xgrid(j)+1) = data_correc(ibgn)

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
        data_correc_latlon(1:imut,jmut-j+1) = data_new(1:imut)
        deallocate(data_org)
        deallocate(lon_org)
      end if
      i0 = iend
    end do

    write(lun2,rec=month) real(data_correc_latlon,4)

  end do

  close(lun2)
  close(lun1)

  !---------------------------------------------

end program make_monthly_correction_radiation
