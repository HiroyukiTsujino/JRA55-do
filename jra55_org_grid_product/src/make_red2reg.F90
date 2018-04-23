! -*-F90-*-
program make_reduced_to_regular

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

  integer(4) :: nlink
  integer(4),allocatable :: isrc(:)
  integer(4),allocatable :: idst(:)
  real(8),allocatable    :: wgt(:)

  character(256) :: file_table

  character(256) :: file_in
  character(256) :: file_latlon
  character(256) :: file_weight

  real(4) :: undef4_in, undef4_out
  real(8) :: undef8_in, undef8_out

  logical :: l_out_weight

  integer(4),parameter :: lun=10

  integer(4) :: i, j, m, n

  !---------------------------------------------

  namelist /nml_make_red2reg/ &
       & file_in,             &
       & file_latlon,         &
       & l_out_weight,        &
       & file_weight,         &
       & file_table,          &
       & undef4_in,           &
       & undef4_out,          &
       & imut, jmut, grid_name

  !---------------------------------------------

  open(lun,file='namelist.make_red2reg')
  read(lun,nml=nml_make_red2reg)
  close(lun)

  undef8_in  = real(undef4_in,8)
  undef8_out = real(undef4_out,8)

  !---------------------------------------------

  allocate(data_latlon(1:imut,1:jmut))
  allocate(data_weight(1:imut,1:jmut))
  allocate(num_xgrid(1:jmut))

  call set_reduced_grid(grid_name,num_xgrid,jmut,total_grid_1d)

  allocate(work4(1:total_grid_1d))
  allocate(data_in(1:total_grid_1d))

  !----------------------------------------------

  open (lun,file=file_table,form='unformatted')
  read(lun) nlink
  allocate(isrc(1:nlink))
  allocate(idst(1:nlink))
  allocate(wgt(1:nlink))
  read(lun) isrc
  read(lun) idst
  read(lun) wgt
  close(lun)

  !----------------------------------------------

  open(lun,file=file_in,form='unformatted',access='direct', &
       & convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  close(lun)

  data_in(1:total_grid_1d) = real(work4(1:total_grid_1d),8)

  data_latlon(:,:) = 0.d0
  !data_weight(:,:) = 0.d0

  do n = 1, nlink
    j = (idst(n)-1)/imut + 1
    i = idst(n) - imut * (j-1)
    m = isrc(n)
    data_latlon(i,j) = data_latlon(i,j) + wgt(n) * data_in(m)
    !data_weight(i,j) = data_weight(i,j) + wgt(n)
  end do

  !write(6,*) ' outfile = ',trim(file_latlon)
  open (lun,file=file_latlon,form='unformatted',access='direct', &
       & recl=4*imut*jmut)
  write(lun,rec=1) real(data_latlon,4)
  close(lun)

  !if (l_out_weight) then
  !  write(6,*) ' outfile = ',trim(file_weight)
  !  open (lun,file=file_weight,form='unformatted',access='direct', &
  !       & recl=4*imut*jmut)
  !  write(lun,rec=1) real(data_weight,4)
  !  close(lun)
  !end if

  !---------------------------------------------

end program make_reduced_to_regular
