! -*-F90-*-
!==================================================================
program read_netcdt_output_grads_runoff
  !----------------------------------------------------------------
  ! Information:
  !   Create direct access file (GrADS) of
  !     the corrected normal year forcing (CNYF) of CORE.
  !
  !   For river run off data.
  !
  !                                         2007.5.9 H.Tsujino
  !----------------------------------------------------------------

  implicit none

  include '/usr/local/netcdf/include/netcdf.inc'

  integer(4), parameter :: imut = 360, jmut = 180
  integer(4), parameter :: mtot1 = 91, mtot2 = 92, mtot3 = 93

!  real(8), parameter :: ro = 1.036d3 ! density of sea water [kg/m3]
  real(4) :: dat4(imut,jmut)
  real(8) :: dat8(imut,jmut)

  real(4) :: rof(imut,jmut)
  real(4) :: are(imut,jmut)
  real(4) :: work4(imut,jmut)

  real(4) :: mask(imut,jmut)

  integer(4) :: idmon(12)

  integer(4) :: i, j, k, l, m, n

  integer(4) :: nmocount
  integer(4) :: ireco1
  integer(4) :: nyear

  ! for netCDF

  integer(4), parameter :: nvars = 2, nfiles = 1
  integer(4) :: var(nvars), sta(nvars)
  integer(4) :: start(3), range(3)
  character(128) :: flnin(nfiles)
  integer(4) :: ifiles(nfiles)

  character(128) :: fl_in, fl_out_base
  character(128) :: flot1
  character(128) :: file_mask

  !--------------------------------------------------------------------

  namelist /nml_core_runoff_cnyf/ fl_in, fl_out_base, file_mask

  !--------------------------------------------------------------------
 
  open(10, file='namelist.core_runoff_cnyf')
  read(10, nml=nml_core_runoff_cnyf)
  close(10)

  !--------------------------------------------------------------------
  ! general settings

  idmon(1:12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  !--------------------------------------------------------------------
  ! open netcdf file

  flnin(1) = trim(fl_in)

  do n = 1, nfiles
    sta(n) = nf_open(flnin(n),nf_nowrite,ifiles(n)) 
    if (sta(n) /= 0) then
      write(6,*) 'nf_open error for file number ',n
      stop
    end if
  end do

  sta(1) = nf_inq_varid(ifiles(1),'runoff',var(1))
  sta(2) = nf_inq_varid(ifiles(1),'area',var(2))

  do n = 1, nvars
    if (sta(n) /= 0) then
      write(6,*) 'nf_inq_varid error for variable ',n, var(n)
      stop
    end if
  end do

  mask(:,:) = 0.0

  !---------------------------------------------------------------------
  ! main year
  !

  start(1:3) = (/ 1, 1, 1 /)
  range(1:3) = (/ 360, 180, 1 /)

  nmocount = 0

  do nyear = 1000, 1000

    ! open output file

    do m = 1, 12

      write(flot1,'(1a,i4.4,i2.2)') trim(fl_out_base),nyear,m
      open(mtot1,file=flot1,form='unformatted',access='direct',recl=4*imut*jmut)
      write(6,*) 'DATA written to ',trim(flot1)
      ireco1 = 0

      nmocount = nmocount + 1
      start(3) = nmocount

      sta(1) = nf_get_vara_double(ifiles(1),var(1),start,range,dat8)
      rof(1:imut,1:jmut) = real(dat8(1:imut,1:jmut),4)

      start(3) = 1
      sta(2) = nf_get_vara_double(ifiles(1),var(2),start,range,dat8)
      are(1:imut,1:jmut) = real(dat8(1:imut,1:jmut),4)

      work4(1:imut,1:jmut) = 0.0

      do i = 1, 180
        work4(i+180,1:jmut) = rof(i,1:jmut) ! [kg/m^2/s] of pure water
      end do
      do i = 181, 360
        work4(i-180,1:jmut) = rof(i,1:jmut) ! [kg/m^2/s] of pure water
      end do

      ireco1 = ireco1 + 1
      write(mtot1,rec=ireco1) work4

      do j = 1, jmut
        do i = 1, imut
          if (work4(i,j) > 0.0) then
            mask(i,j) = 1.0
          end if
        end do
      end do

!      do i = 1, 180
!        work4(i+180,1:jmut) = are(i,1:jmut)
!      end do
!      do i = 181, 360
!        work4(i-180,1:jmut) = are(i,1:jmut)
!      end do
!
!      ireco1 = ireco1 + 1
!      write(mtot1,rec=ireco1) work4

    end do

    close(mtot1)
  
  end do

  !file_mask = '/worke/htsujino/CORE/data/runoff_mask.gd'
  !open(mtot2,file=file_mask,form='unformatted',access='direct',recl=4*imut*jmut)
  !write(6,*) 'DATA written to ',trim(file_mask)
  !write(mtot2,rec=1) real(mask(1:imut,1:jmut),4)
  !close(mtot2)

  do n = 1, nfiles
    write(6,*) ifiles(n)
    sta(n) = nf_close(ifiles(n))
    if (sta(n) /= 0) then
      write(6,*) 'nf_close error for file number ',n, ifiles(n), sta(n)
    end if
  end do

end program read_netcdt_output_grads_runoff
