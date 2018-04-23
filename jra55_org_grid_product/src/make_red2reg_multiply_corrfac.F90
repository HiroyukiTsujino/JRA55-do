! -*-F90-*-
program make_reduced_to_regular_multiplicative_corrfac

  implicit none

  integer(4) :: imut, jmut
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: total_grid_1d

  integer(4) :: ibgn, iend, i0

  real(4),allocatable :: work4(:)

  real(8),allocatable :: data_in(:)
  real(8),allocatable :: data_latlon(:,:)
  real(8),allocatable :: data_weight(:,:)
  real(8),allocatable :: data_ocean(:,:)

  real(4),allocatable :: work2d(:,:)

  real(8),allocatable :: data_mask(:)
  real(8),allocatable :: data_land_mask(:,:)

  integer(4) :: nlink
  integer(4),allocatable :: isrc(:)
  integer(4),allocatable :: idst(:)
  real(8),allocatable    :: wgt(:)

  character(256) :: file_table

  character(256) :: file_in
  character(256) :: file_latlon
  character(256) :: file_mask
  character(256) :: file_mask_latlon
  character(256) :: file_weight

  real(4) :: undef4_in, undef4_out
  real(8) :: undef8_in, undef8_out

  logical :: l_out_weight

  integer(4),parameter :: lun_i = 10, lun_o = 11, lun_m = 12

  integer(4) :: i, j, m, n, mon
  integer(4) :: num_data
  

  !---------------------------------------------

  namelist /nml_make_red2reg_mult_corrfac/ &
       & file_in,             &
       & file_latlon,         &
       & file_mask,           &
       & file_mask_latlon,    &
       & num_data,            &
       & l_out_weight,        &
       & file_weight,         &
       & file_table,          &
       & undef4_in,           &
       & undef4_out,          &
       & imut, jmut, grid_name

  !---------------------------------------------

  open(lun_i,file='namelist.make_red2reg_mult_corrfac')
  read(lun_i,nml=nml_make_red2reg_mult_corrfac)
  close(lun_i)

  undef8_in  = real(undef4_in,8)
  undef8_out = real(undef4_out,8)

  !---------------------------------------------

  allocate(data_latlon(1:imut,1:jmut))
  allocate(data_weight(1:imut,1:jmut))
  allocate(data_ocean (1:imut,1:jmut))
  allocate(data_land_mask(1:imut,1:jmut))
  allocate(work2d(1:imut,1:jmut))

  allocate(num_xgrid(1:jmut))

  call set_reduced_grid(grid_name,num_xgrid,jmut,total_grid_1d)

  allocate(work4(1:total_grid_1d))
  allocate(data_in(1:total_grid_1d))
  allocate(data_mask(1:total_grid_1d))

  !----------------------------------------------

  open (lun_i,file=file_table,form='unformatted')
  read(lun_i) nlink
  allocate(isrc(1:nlink))
  allocate(idst(1:nlink))
  allocate(wgt(1:nlink))
  read(lun_i) isrc
  read(lun_i) idst
  read(lun_i) wgt
  close(lun_i)

  !----------------------------------------------

  open (lun_m,file=file_mask,form='unformatted', &
       & access='direct',convert='little_endian',recl=4*total_grid_1d)
  read (lun_m,rec=1) work4
  data_mask(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun_m)
  data_mask(1:total_grid_1d) = 1.0d0 - data_mask(1:total_grid_1d)

  open (lun_m,file=file_mask_latlon,form='unformatted', &
       & access='direct',recl=4*imut*jmut)
  read (lun_m,rec=1) work2d
  close(lun_m)
  data_land_mask(1:imut,1:jmut) = real(work2d(1:imut,1:jmut),8)


  open (lun_i,file=file_in,form='unformatted',access='direct', &
       & convert='little_endian',recl=4*total_grid_1d)

  open (lun_o,file=file_latlon,form='unformatted',access='direct', &
       & recl=4*imut*jmut)

  do mon = 1, num_data

    read(lun_i,rec=mon) work4

    data_in(1:total_grid_1d) = real(work4(1:total_grid_1d),8)

    data_latlon(:,:) = 0.d0
    data_weight(:,:) = 0.d0

    do n = 1, nlink
      j = (idst(n)-1)/imut + 1
      i = idst(n) - imut * (j-1)
      m = isrc(n)
      if ((data_latlon(i,j) == 1.0d0) .and. (data_in(m) == undef8_in)) then
        write(6,*) 'table and data is inconsistent, please check '
        stop
      end if
      if (data_in(m) /= undef8_in) then
        data_latlon(i,j) = data_latlon(i,j) + wgt(n) * data_in(m)
        data_weight(i,j) = data_weight(i,j) + wgt(n)
      end if
    end do

    do j = 1, jmut
      do i = 1, imut
        if (data_land_mask(i,j) > 0.0d0) then
          if (data_weight(i,j) >= 0.5d0) then
            data_latlon(i,j) = data_latlon(i,j) / data_weight(i,j)
          else
            data_latlon(i,j) = 1.0d0      ! correction factor is missing
          end if
        else
          data_latlon(i,j) = undef8_out ! land
        end if
      end do
    end do
    
    !write(6,*) ' outfile = ',trim(file_latlon)

    ! NOTE: default endian is big

    write(lun_o,rec=mon) real(data_latlon,4)

    !if (l_out_weight) then
    !  write(6,*) ' outfile = ',trim(file_weight)
    !  open (lun,file=file_weight,form='unformatted',access='direct', &
    !       & recl=4*imut*jmut)
    !  write(lun,rec=1) real(data_weight,4)
    !  close(lun)
    !end if

  end do

  close(lun_o)
  close(lun_i)

  !---------------------------------------------

end program make_reduced_to_regular_multiplicative_corrfac
