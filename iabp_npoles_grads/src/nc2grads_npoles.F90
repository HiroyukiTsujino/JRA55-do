! -*-F90-*-
!==================================================================
program read_netcdf_output_grads
  !----------------------------------------------------------------
  ! Information:
  !   Create direct access file (GrADS) of
  !     the NOCS surface data
  !----------------------------------------------------------------

  implicit none

  include '/usr/local/netcdf/include/netcdf.inc'

  integer(4), parameter :: imut = 360, jmut = 180
  integer(4), parameter :: jmut_data = 40
  integer(4), parameter :: mtot1 = 91, mtot2 = 92

  integer(2) :: idat2(imut,jmut_data)

  real(4) :: tair(imut,jmut)
  real(4) :: work4(imut,jmut)

  integer(4) :: idmon(12)

  character(128) :: flot1, flot2

  integer(4) :: i, j, k, l, m, n

  integer(4) :: nmocount
  integer(4) :: ireco1, ireco2
  integer(4) :: nyear

  integer(2),parameter :: undef_npoles = 32767
  real(4),parameter :: undef_mxe = -9.99e33

  ! for netCDF

  integer(4), parameter :: nvars = 1, nfiles = 1
  integer(4) :: var(nvars), sta(nvars)
  integer(4) :: start(3), range(3)
  character(128) :: flnin(nfiles)
  integer(4) :: ifiles(nfiles)

  !--------------------------------------------------------------------
  ! general settings

  idmon(1:12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  !---------------------------------------------------------------------
  ! open netcdf file

  start(1:3) = (/ 1, 1, 1 /)
  range(1:3) = (/ 360, jmut_data, 1 /)

  nmocount = 0

  write(flnin(1),'(1a)') '/worke/htsujino/IABP_NPOLES/orgdata/satiabppoles7998.nc'

  do n = 1, nfiles
    sta(n) = nf_open(flnin(n),nf_nowrite,ifiles(n)) 
    if (sta(n) /= 0) then
      write(6,*) 'nf_open error for file number ',n
      stop
    end if
  end do

  sta(1) = nf_inq_varid(ifiles(1),'data',var(1))

  do n = 1, nvars
    if (sta(n) /= 0) then
      write(6,*) 'nf_inq_varid error for variable ',n, var(n)
      stop
    end if
  end do

  nmocount = 0

  do nyear = 1979, 1998

    do m = 1, 12

      nmocount = nmocount + 1

      start(3) = nmocount

      sta(1) = nf_get_vara_int2(ifiles(1),var(1),start,range,idat2)

      tair(:,:) = real(undef_npoles,4)

      do j = 1, jmut_data
        do i = 1, imut
          tair(i,jmut-j+1) = real(idat2(i,j),4)
        end do
      end do

      do j = 1, jmut
        do i = 1, imut
          if (tair(i,j) == real(undef_npoles,4)) then
            tair(i,j) = undef_mxe
          else
            tair(i,j) = tair(i,j) * 1.0e-2
          end if
        end do
      end do

      ! open output file

      write(flot1,'(1a,i4.4,i2.2)') &
           & '/worke/htsujino/IABP_NPOLES/grads/tair.',nyear,m
      open(mtot1,file=flot1,form='unformatted',access='direct',recl=4*imut*jmut)
      write(6,*) 'Tair written to ',trim(flot1)
      ireco1 = 0

      ireco1 = ireco1 + 1

      write(mtot1,rec=ireco1) tair

      close(mtot1)

    end do

  end do

  do n = 1, nfiles
    write(6,*) ifiles(n)
    sta(n) = nf_close(ifiles(n))
    if (sta(n) /= 0) then
      write(6,*) 'nf_close error for file number ',n, ifiles(n), sta(n)
    end if
  end do

end program read_netcdf_output_grads
