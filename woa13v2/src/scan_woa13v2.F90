!-*-F90-*-
!-  Scan data and create land-sea mask
program scan_data
  use libmxe_para, only: libmxe_para__register &
                     & , clen, pi, radian, radian_r, radius &
                     & , type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_calendar
  use libmxe_io, only: libmxe_io__register, type_libmxe_io

  implicit none


  !-- arguments --

  character(len=clen) :: file_base   !- base name of original data
  character(len=clen) :: file_mask   !- output file name
  character(len=clen) :: file_topo   !- topography file
  character(len=clen) :: file_of_namelist   !- namelist file name

  integer(4),parameter :: lun = 10

  character(len=clen) :: filei

  integer(4) :: i, j, k, n, imut, jmut, km

  real(4),   allocatable :: data_org(:,:,:)
  integer(4),allocatable :: data_msk(:,:,:)
  integer(4),allocatable :: ho(:,:), exn(:,:)
  real(4),   allocatable :: data_out(:,:)
  real(4) :: undef_data

  integer(4) :: istat, irec
  integer(4) :: nyear, month, id, mdays

  type(type_libmxe_para) :: orgp
  type(type_libmxe_grid) :: orgg

  logical :: l_leap_year
  integer(4),parameter :: nday_of_month(12) =(/31,28,31,30,31,30,31,31,30,31,30,31/) 

  !---------------------------

  namelist /nml_scandata/ file_base, file_mask, file_topo, file_of_namelist, undef_data

  !------ preprocessing ------

  !---- arguments ----

  open(lun,file='namelist.scandata',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(*,*) ' cannot find file : namelist.scandata'
    stop
  end if

  read(lun,nml=nml_scandata,iostat=istat)

  if ( istat /= 0 ) then
    write(*,*) ' Read error : namelist.scandata'
    stop
  end if

  close(lun)

  !---- specifics of dataset ----

  call libmxe_para__register(orgp, file_namelist=file_of_namelist)
  call libmxe_grid__register(orgg, orgp)

  !-- Make mask data by scanning original river runoff data --

  imut = orgp%imut
  jmut = orgp%jmut
  km   = orgp%km + 1

  write(6,*) ' 3D data size : ',imut,jmut,km
  allocate(data_org(imut,jmut,km))
  allocate(data_msk(imut,jmut,km))
  allocate(ho (1:imut,1:jmut))
  allocate(exn(1:imut,1:jmut))

  data_org(:,:,:) = 0.0
  data_msk(:,:,:) = 0

  do month = 1, 12

    write(filei,'(1a,i2.2,1a)') trim(file_base), month, '.gd'
    write(6,*) 'Reading ',trim(filei),' ...'
    open(lun,file=filei,form='unformatted',action='read' &
         &    , access='direct',recl=4*imut*jmut*km)
    irec = 0
    irec = irec + 1
    read(lun,rec=irec) data_org(1:imut,1:jmut,1:km)

    do k = 1, km
      do j = 1, jmut
        do i = 1, imut
          if (data_org(i,j,k) /= undef_data) then
            data_msk(i,j,k) = data_msk(i,j,k) + 1
          endif
        end do
      end do
    end do

    close(lun)

  end do

  deallocate(data_org)

  allocate(data_out(imut,jmut))

  open(lun,file=file_mask,form='unformatted',access='direct',&
       & action='write',recl=4*imut*jmut)

  do j = 1, jmut
    do i = 1, imut
      if (data_msk(i,j,1) == 0) then
        data_out(i,j) = 0.0
      else
        if (data_msk(i,j,1) == 12) then
          data_out(i,j) = 1.0
        else
          write(6,*) ' There may be missing data in time series of this grid ', i,j
          data_out(i,j) = 0.0
        end if
      end if
    end do
  end do

  write(lun,rec=1) real(data_out(1:imut,1:jmut))
  data_out(:,:) = real(data_msk(:,:,1),4)
  write(lun,rec=2) real(data_out(1:imut,1:jmut))

  close(lun)

  open(lun,file=file_topo,form='unformatted',action='write')

  do j = 1, jmut
    do i = 1, imut
      if (data_msk(i,j,1) == 12) then
        ho (i,j) = orgg%dep(2)
        exn(i,j) = 1
        do k = 2, km
          if (data_msk(i,j,k) == 12) then
            ho (i,j) = nint(orgg%dep(k))
            exn(i,j) = k - 1
          else
            exit
          end if
        end do
      end if
    end do
  end do

  write(lun) ho, exn
  close(lun)

  deallocate(data_msk)

end program scan_data
