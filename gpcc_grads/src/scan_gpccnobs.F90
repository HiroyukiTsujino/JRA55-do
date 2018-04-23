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
  character(len=clen) :: file_of_namelist   !- namelist file name

  integer(4),parameter   :: lun = 10

  character(len=clen) :: filei

  integer :: i, j, n, imut, jmut

  real(4),   allocatable :: data_org(:,:)
  integer(4),allocatable :: data_msk(:,:)
  integer(4),allocatable :: ho(:,:), exn(:,:)
  real(4),   allocatable :: data_out(:,:)
  real(4) :: undef_data

  integer(4) :: istat, irec
  integer(4) :: nyear, month, id, mdays

  type(type_libmxe_para) :: orgp
  type(type_libmxe_io)   :: orgio

  logical :: l_leap_year
  integer(4),parameter :: nday_of_month(12) =(/31,28,31,30,31,30,31,31,30,31,30,31/) 

  !---------------------------

  namelist /nml_scandata/ file_base, file_mask, file_of_namelist, undef_data

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

  !-- Make mask data by scanning original river runoff data --

  imut = orgp%imut
  jmut = orgp%jmut

  write(6,*) ' 2D data size : ',imut,jmut
  allocate(data_org(imut,jmut))
  allocate(data_msk(imut,jmut))
  allocate(ho(1:imut,1:jmut))
  allocate(exn(1:imut,1:jmut))

  data_org(:,:) = 0.0
  data_msk(:,:) = 0

  call libmxe_io__register(orgio,orgp)

  write(6,*) 'Number of data will be ', orgio%nm

  do nyear = orgio%calrec(1)%year, orgio%calrec(orgio%nm)%year

    l_leap_year = libmxe_calendar__l_leap_year( nyear )

    do month = 1, 12

      write(filei,'(1a,i4.4,i2.2)') trim(file_base),nyear,month
      write(6,*) 'Reading ',trim(filei),' ...'
      open(lun,file=filei,form='unformatted',action='read' &
           &    , access='direct',recl=4*imut*jmut)
      irec = 0

      irec = irec + 1
      read(lun,rec=irec) data_org(1:imut,1:jmut)
      do j = 1, jmut
        do i = 1, imut
          if (data_org(i,j) > 0.0) then ! at least one observation
            data_msk(i,j) = data_msk(i,j) + 1
          endif
        end do
      end do

      close(lun)

    end do

  end do

  deallocate(data_org)

  allocate(data_out(imut,jmut))

  open(lun,file=file_mask,form='unformatted',access='direct',&
       & action='write',recl=4*imut*jmut)
  write(6,*) ' Mask output to ', trim(file_mask)

  do j = 1, jmut
    do i = 1, imut
      if (data_msk(i,j) == 0) then
        data_out(i,j) = 0.0
      else
        if (data_msk(i,j) == orgio%nm) then
          data_out(i,j) = 1.0
        else
          !write(6,'(1a,i4,i4)')  ' There may be missing data in time series of this grid ', i,j
          !write(6,*) data_msk(i,j), orgio%nm
          data_out(i,j) = 0.0
        end if
      end if
    end do
  end do

  write(lun,rec=1) real(data_out(1:imut,1:jmut))
  data_out(:,:) = real(data_msk(:,:),4)
  write(lun,rec=2) real(data_out(1:imut,1:jmut))

  close(lun)

  deallocate(data_msk)

end program scan_data
