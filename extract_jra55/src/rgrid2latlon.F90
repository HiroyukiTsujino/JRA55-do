! -*-F90-*-
program reduced_grid_to_latlon_grid

  implicit none

  integer(4) :: imut, jmut
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: total_grid_1d

  integer(4) :: ibgn, iend, i0
  real(4),allocatable :: data_rg(:)
  real(4),allocatable :: data_latlon(:,:)

  real(8),allocatable :: data_org(:), data_new(:)
  real(8),allocatable :: lon_org(:), lon_new(:)
  real(8) :: dlon, dlon_rg

  character(256) :: file_in
  character(256) :: file_out
  integer(4),parameter :: lun=10

  integer(4) :: i, j, ii
  real(8) :: weight

  !---------------------------------------------
 
  namelist /nml_rg2latlon/ file_in, file_out, imut, jmut, dlon, grid_name

  !---------------------------------------------

  open(lun,file='namelist.rg2latlon')
  read(lun,nml=nml_rg2latlon)
  close(lun)

  !---------------------------------------------

  allocate(num_xgrid(1:jmut))

  call set_reduced_grid(grid_name,num_xgrid,total_grid_1d)

  allocate(data_rg(1:total_grid_1d))
  allocate(data_latlon(1:imut,1:jmut))
  allocate(data_new(1:imut))
  allocate(lon_new(1:imut))

  do i = 1, imut
    lon_new(i) = dlon * real(i-1,8)
  end do

  open(lun,file=file_in,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) data_rg
  close(lun)

  i0 = 0

  do j = 1, jmut

    ibgn = i0 + 1
    iend = i0 + num_xgrid(j)

    if (num_xgrid(j) == imut) then
      data_latlon(1:imut,jmut-j+1) = data_rg(ibgn:iend)
    else
      allocate(data_org(1:num_xgrid(j)+1))
      allocate(lon_org (1:num_xgrid(j)+1))
      data_org(1:num_xgrid(j)) = real(data_rg(ibgn:iend),8)
      data_org(num_xgrid(j)+1) = real(data_rg(ibgn),8)
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
            data_new(i) = (1.0d0 - weight) * data_org(ii) + weight * data_org(ii+1)
            exit
          end if
        end do
      end do
      data_latlon(1:imut,jmut-j+1) = data_new(1:imut)
      deallocate(data_org)
      deallocate(lon_org)
    end if
    i0 = iend
  end do

  open(lun,file=file_out,form='unformatted',access='direct',recl=4*imut*jmut)
  write(lun,rec=1) real(data_latlon,4)
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

end program reduced_grid_to_latlon_grid
