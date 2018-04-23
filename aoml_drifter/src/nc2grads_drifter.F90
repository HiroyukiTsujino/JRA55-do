! -*-F90-*-
!==================================================================
program read_netcdf_output_grads
  !----------------------------------------------------------------
  ! Information:
  !   Create direct access file (GrADS) of NOAA-AOML drifter data
  !----------------------------------------------------------------

  implicit none

  include '/usr/local/netcdf-fortran-4.4.3/include/netcdf.inc'

  integer(4), parameter :: imut = 1440, jmut = 632, jmut_wide = 720
  integer(4), parameter :: mtot1 = 91, mtot2 = 92

  real(8) :: dat8(jmut,imut)
  real(4) :: dat4(imut,jmut)

  real(4) :: wind(imut,jmut)
  real(4) :: work4(imut,jmut)

  integer(4) :: idmon(12)

  character(128) :: flot1, flot2

  integer(4) :: i, j, k, l, m, n, jw

  integer(4) :: nmocount
  integer(4) :: ireco1, ireco2
  integer(4) :: nyear
  integer(4) :: mst, med

  real(4) :: undef_in = 0.0, undef_out = 0.0
  real(4),parameter :: undef_drifter = -9999.0
  real(4),parameter :: undef_mxe = -9.99e33

  ! for netCDF

  integer(4), parameter :: nvars = 12, nfiles = 1
  integer(4) :: var(nvars), sta(nvars)
  integer(4) :: start(3), range(3)
  character(128) :: flnin(nfiles)
  integer(4) :: ifiles(nfiles)

  character(128) :: flnin_drifter, flnot_base

  logical :: isnan

  !--------------------------------------------------------------------

  namelist /nml_drifter/ &
       & flnin_drifter, undef_in, &
       & flnot_base, undef_out, mst, med

  mst = 1
  med = 1

  open (11,file='namelist.drifter')
  read (11,nml=nml_drifter)
  close(11)

  !--------------------------------------------------------------------
  ! general settings

  idmon(1:12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  !---------------------------------------------------------------------
  ! main year
  !

  start(1:3) = (/ 1, 1, 1 /)
  range(1:3) = (/ jmut, imut, 1 /)

  ! open netcdf file

  write(flnin(1),'(1a)') trim(flnin_drifter)

  do n = 1, nfiles
    write(6,*) 'Openning ',trim(flnin(n))
    sta(n) = nf_open(flnin(n),nf_nowrite,ifiles(n)) 
    write(6,*) 'ifiles = ', ifiles(1)
    if (sta(n) /= 0) then
      write(6,*) 'nf_open error for file number ',n, sta(n)
      stop
    end if
  end do

  sta(1) = nf_inq_varid(ifiles(1),'U'   ,var(1))
  sta(2) = nf_inq_varid(ifiles(1),'V'   ,var(2))
  sta(3) = nf_inq_varid(ifiles(1),'SST' ,var(3))
  sta(4) = nf_inq_varid(ifiles(1),'eU'  ,var(4))
  sta(5) = nf_inq_varid(ifiles(1),'eV'  ,var(5))
  sta(6) = nf_inq_varid(ifiles(1),'eSST',var(6))
  sta(7) = nf_inq_varid(ifiles(1),'N'   ,var(7))

  do n = 1, 7
    if (sta(n) /= 0) then
      write(6,*) 'nf_inq_varid error for variable ',n, var(n)
      stop
    else
      write(6,*) ' var sta = ', var(n), sta(n)
    end if
  end do

  write(flot1,'(1a)') trim(flnot_base)
  open(mtot1,file=flot1,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'Wind speed written to ',trim(flot1)
  ireco1 = 0

  do m = mst, med

    start(3) = m

    !---------------------------------------------------------------------------

    do n = 1, 7

      sta(n) = nf_get_vara_double(ifiles(1),var(n),start,range,dat8)

      if (sta(n) /= 0) then
        write(6,*) 'nf_get_vara_double error for variable ',n, var(n), sta(n), ifiles(1)
        stop
      end if

      do j = 1, jmut
        do i = 1, imut
          if (isnan(dat8(j,i))) then
            wind(i,j) = undef_mxe
          else
            wind(i,j) = real(dat8(j,i),4)
          end if
        end do
      end do

      ! open output file

      ireco1 = ireco1 + 1
      write(mtot1,rec=ireco1) wind

    end do

  end do

  close(mtot1)

  do n = 1, nfiles
    !write(6,*) ifiles(n)
    sta(n) = nf_close(ifiles(n))
    if (sta(n) /= 0) then
      write(6,*) 'nf_close error for file number ',n, ifiles(n), sta(n)
    end if
  end do

contains

  logical function isnan(a) 
    real(8) ::  a 
    if (a.ne.a) then 
      isnan = .true. 
    else 
      isnan = .false. 
    end if
    return 
  end function isnan

end program read_netcdf_output_grads
