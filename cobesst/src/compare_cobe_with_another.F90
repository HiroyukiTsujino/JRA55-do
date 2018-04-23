!-*-F90-*-
!-  Compare COBESST with another SST dataset.
program compare_cobesst_with_another_monthly

  use libmxe_para, only: libmxe_para__register &
                     & , clen, pi, radian, radian_r, radius &
                     & , type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, &
       & type_libmxe_topo, &
       & libmxe_topo__aexl
  use libmxe_calendar
  use libmxe_io, only: libmxe_io__register, type_libmxe_io

  implicit none


  !-- arguments --

  character(len=clen) :: file_base_cobe, file_base_ref   !- base name of original data
  character(len=clen) :: file_of_namelist   !- namelist file name
  character(len=clen) :: filei, file_out

  integer(4),parameter   :: lun = 10

  integer :: i, j, n, imut, jmut

  real(4),   allocatable :: data_cobe(:,:)
  real(4),   allocatable :: data_ref(:,:)
  integer(4),allocatable :: total_days(:,:)
  real(8),   allocatable :: diff(:,:), rmsd(:,:)
  real(4) :: undef_cobe, undef_ref, undef_out
  real(8) :: hl1

  integer(4) :: istat, irec
  integer(4) :: nyear, month, id, mdays
  integer(4) :: ibyr, ieyr

  type(type_libmxe_para) :: orgp
  type(type_libmxe_grid) :: orgg
  type(type_libmxe_topo) :: orgt
  type(type_libmxe_io)   :: orgio

  logical :: l_leap_year
  integer(4),parameter :: nday_of_month(12) =(/31,28,31,30,31,30,31,31,30,31,30,31/) 

  !---------------------------

  namelist /nml_compare/ file_base_cobe, file_base_ref, &
       & file_out, file_of_namelist, &
       & undef_cobe, undef_ref, undef_out, ibyr, ieyr

  !------ preprocessing ------

  !---- arguments ----

  open(lun,file='namelist.compare',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(*,*) ' cannot find file : namelist.compare'
    stop
  end if

  read(lun,nml=nml_compare,iostat=istat)

  if ( istat /= 0 ) then
    write(*,*) ' Read error : namelist.compare'
    stop
  end if

  close(lun)

  !---- specifics of dataset ----

  call libmxe_para__register(orgp, file_namelist=file_of_namelist)
  call libmxe_grid__register(orgg, orgp)
  call libmxe_topo__register(orgt, orgp)
  call libmxe_topo__aexl(orgt, orgp)

  !-- Make mask data by scanning original river runoff data --

  imut = orgp%imut
  jmut = orgp%jmut

  write(6,*) ' 2D data size : ',imut,jmut

  allocate(total_days(1:imut,1:jmut))
  allocate(diff(1:imut,1:jmut), rmsd(1:imut,1:jmut))
  allocate(data_cobe(1:imut,1:jmut))
  allocate(data_ref(1:imut,1:jmut))

  call libmxe_io__register(orgio,orgp)

  write(6,*) 'Number of data will be ', orgio%nm

  total_days(:,:) = 0
  diff(:,:) = 0.0d0
  rmsd(:,:) = 0.0d0

  do nyear = ibyr, ieyr

    l_leap_year = libmxe_calendar__l_leap_year( nyear )

    do month = 1, 12

      if (month == 2) then
        if (l_leap_year) then
          mdays = nday_of_month(month) + 1
        else
          mdays = nday_of_month(month)
        end if
      else
        mdays = nday_of_month(month)
      end if

      write(filei,'(1a,i4.4,i2.2)') trim(file_base_cobe),nyear,month
      write(6,*) 'Reading (COBESST) ',trim(filei),' ...'
      open (lun,file=filei,form='unformatted',action='read' &
           &    , access='direct',recl=4*imut*jmut)
      irec = 1
      read (lun,rec=irec) data_cobe(1:imut,1:jmut)
      close(lun)
      
      write(filei,'(1a,i4.4,i2.2)') trim(file_base_ref),nyear,month
      write(6,*) 'Reading (Reference) ',trim(filei),' ...'
      open (lun,file=filei,form='unformatted',action='read' &
           &    , access='direct',recl=4*imut*jmut)
      irec = 1
      read (lun,rec=irec) data_ref(1:imut,1:jmut)
      close(lun)
      
      do j = 1, jmut
        do i = 1, imut
          if ((data_cobe(i,j) /= undef_cobe) .and. (data_ref(i,j) /= undef_ref)) then
            total_days(i,j) = total_days(i,j) + mdays * int(orgt%aexl(i,j,1)+1.0d-6)
            hl1 = orgt%aexl(i,j,1) * (data_cobe(i,j) - data_ref(i,j))
            diff(i,j) = diff(i,j) + real(mdays,8) * hl1
            rmsd(i,j) = rmsd(i,j) + real(mdays,8) * sqrt(hl1**2)
          end if
        end do
      end do

    end do

  end do

  do j = 1, jmut
    do i = 1, imut
      if (total_days(i,j) > 0) then
        diff(i,j) = diff(i,j) / real(total_days(i,j),8)
        rmsd(i,j) = rmsd(i,j) / real(total_days(i,j),8)
      else
        diff(i,j) = undef_out
        rmsd(i,j) = undef_out
      end if
    end do
  end do

  open(lun,file=trim(file_out),form='unformatted',access='direct',&
       & action='write',recl=4*imut*jmut)

  write(lun,rec=1) real(diff(1:imut,1:jmut),4)
  write(lun,rec=2) real(rmsd(1:imut,1:jmut),4)

  close(lun)

end program compare_cobesst_with_another_monthly
