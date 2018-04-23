! -*-F90-*-
program zonal_mean

  implicit none

  integer(4) :: imut, jmut

  real(4),allocatable :: work4(:,:)

  real(8),allocatable :: data_org(:,:)
  real(8),allocatable :: data_mask(:,:)

  real(8),allocatable :: data_zm(:)

  real(8),allocatable :: lon_org(:)
  real(8) :: dlon, slon
  real(8),allocatable :: lat_org(:)
  real(8) :: dlat, slat

  character(256) :: file_in
  character(256) :: file_out

  real(4) :: undef_in, undef_out
  real(8) :: undef8_in, undef8_out

  character(len=258) :: file_ydef

  integer(4),parameter :: lun=10

  integer(4) :: i, j, ii, jj, n, ios

  integer(4) :: n_valid

  !---------------------------------------------

  namelist /nml_zonalmean/ &
       & file_in, &
       & undef_in, &
       & file_out, &
       & undef_out, &
       & imut, jmut, slon, dlon, slat, dlat

  !---------------------------------------------

  open(lun,file='namelist.zonalmean')
  read(lun,nml=nml_zonalmean)
  close(lun)

  undef8_in = real(undef_in,8)
  undef8_out = real(undef_out,8)

  !---------------------------------------------

  allocate(data_zm(1:jmut))
  allocate(lat_org(1:jmut))

  !---------------------------------------------
  ! set regular grid

  allocate(lon_org(1:imut))
  allocate(lat_org(1:jmut))

  do i = 1, imut
    lon_org(i) = slon + dlon * (i-1)
  end do

  do j = 1, jmut
    lat_org(j) = slat + dlat * (j-1)
  end do

  !--------------------------------

  allocate(work4(1:imut,1:jmut))
  allocate(data_org(1:imut,1:jmut))

  open(lun,file=file_in,form='unformatted',access='direct',recl=4*imut*jmut)
  read(lun,rec=1) work4
  close(lun)

  data_org(1:imut,1:jmut) = real(work4(1:imut,1:jmut),8)

  !----------------------------------------------------------------------------

  do j = 1, jmut
    n_valid = 0
    data_zm(j) = 0.0d0
    do i = 1, imut
      if ((data_org(i,j) /= undef8_in)) then
        n_valid = n_valid + 1
        data_zm(j) = data_zm(j) + data_org(i,j)
      end if
    end do
    if (n_valid >= 1) then
      data_zm(j) = data_zm(j) / real(n_valid,8)
    else
      data_zm(j) = undef8_out
    end if
  end do

  !-------------------------------------------------------------

  write(6,*) 'Product written to ', trim(file_out)

  open (lun,file=file_out,form='unformatted',access='direct',recl=4*jmut)
  write(lun,rec=1) real(data_zm,4)
  close(lun)

  !-------------------------------------------------------------

end program zonal_mean
