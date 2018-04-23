! -*-F90-*-
!==================================================================
program read_netcdf_output_grads_marchmld
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

  integer(4), parameter :: imut = 364, jmut = 368
  integer(4), parameter :: mtot1 = 91

  real(4) :: dat4(imut,jmut)
  real(8) :: dat8(imut,jmut)

  real(4) :: mld(imut,jmut)
  real(4) :: work4(imut,jmut)

  character(128) :: flot1

  integer(4) :: i, j, k, l, m, n

  integer(4) :: nmocount
  integer(4) :: ireco1
  integer(4) :: nyear

  ! for netCDF

  integer(4), parameter :: nvars = 1, nfiles = 1
  integer(4) :: var(nvars), sta(nvars)
  integer(4) :: start(4), range(4)
  character(128) :: flnin(nfiles)
  integer(4) :: ifiles(nfiles)

  !--------------------------------------------------------------------
  ! open netcdf file

!  flnin(1) = '../assimilation-201203/time_series/mld_ferret/march_mld_003_M.nc'
  flnin(1) = '../run-mip-20120214/time_series/mld_ferret/september_mld_003.nc'

  do n = 1, nfiles
    sta(n) = nf_open(flnin(n),nf_nowrite,ifiles(n)) 
    if (sta(n) /= 0) then
      write(6,*) 'nf_open error for file number ',n
      stop
    end if
  end do

!  sta(1) = nf_inq_varid(ifiles(1),'MARCH_MLD',var(1))
  sta(1) = nf_inq_varid(ifiles(1),'SEPTEMBER_MLD',var(1))

  do n = 1, nvars
    if (sta(n) /= 0) then
      write(6,*) 'nf_inq_varid error for variable ',n, var(n)
      stop
    end if
  end do

  !---------------------------------------------------------------------
  ! main year
  !

  start(1:4) = (/ 1, 1, 1, 1 /)
  range(1:4) = (/ 364, 368, 1, 1 /)

  nmocount = 0

  do nyear = 1241, 1300

    ! open output file

!    write(flot1,'(1a,i4.4,1a)') &
!         & '../assimilation-201203/time_series/mld_ferret/march_mld_003_M.',nyear,'03'
    write(flot1,'(1a,i4.4,1a)') &
         & '../run-mip-20120214/time_series/mld_ferret/september_mld_003.',nyear,'09'
    open(mtot1,file=flot1,form='unformatted',access='direct',action='write',recl=4*imut*jmut)
    write(6,*) 'DATA written to ',trim(flot1)
    ireco1 = 0

    nmocount = nmocount + 1
    start(4) = nmocount

    sta(1) = nf_get_vara_real(ifiles(1),var(1),start,range,dat4)

    write(6,*) sta(1), start(1), start(2), start(3), start(4)

    mld(1:imut,1:jmut) = dat4(1:imut,1:jmut)

    ireco1 = ireco1 + 1
    write(mtot1,rec=ireco1) mld

    close(mtot1)
  
  end do

  do n = 1, nfiles
    write(6,*) ifiles(n)
    sta(n) = nf_close(ifiles(n))
    if (sta(n) /= 0) then
      write(6,*) 'nf_close error for file number ',n, ifiles(n), sta(n)
    end if
  end do

end program read_netcdf_output_grads_marchmld
