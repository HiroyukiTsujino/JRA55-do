! -*-F90-*-
!==================================================================
program read_netcdf_output_grads_d2m
  !----------------------------------------------------------------
  ! Information:
  !   Create direct access file (GrADS) of
  !     the NOCS surface data
  !----------------------------------------------------------------

  implicit none

  include '/usr/local/netcdf/include/netcdf.inc'

  integer(4), parameter :: imut = 360, jmut = 180
  integer(4), parameter :: mtot1 = 91, mtot2 = 92

  real(4) :: dat4(imut,jmut)
  real(8) :: dat8(imut,jmut)

  real(8) :: hum2m(imut,jmut)
  integer(4) :: num_valid(imut,jmut)
  real(4) :: work4(imut,jmut)

  integer(4) :: idmon(12)

  character(128) :: flot1, flot2

  integer(4) :: i, j, k, l, m, n

  integer(4) :: ndycount
  integer(4) :: ireco1, ireco2
  integer(4) :: nyear
  integer(4) :: mst, med
  integer(4) :: nday, nd

  real(4),parameter :: undef_oaflux = 32766.0
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
  ! main year
  !

  start(1:3) = (/ 1, 1, 1 /)
  range(1:3) = (/ 360, 180, 1 /)


  do nyear = 1985, 2014

    mst = 1
    med = 12

    hum2m(:,:) = 0.0d0
    num_valid(:,:) = 0

    write(flnin(1),'(1a,i4.4,1a)') &
         & '/workd/htsujino/OAflux/daily/qa_oaflux_',nyear,'.nc'

    do n = 1, nfiles
      sta(n) = nf_open(flnin(n),nf_nowrite,ifiles(n)) 
      if (sta(n) /= 0) then
        write(6,*) 'nf_open error for file number ',n
        stop
      end if
    end do

    sta(1) = nf_inq_varid(ifiles(1),'hum2m',var(1))

    do n = 1, nvars
      if (sta(n) /= 0) then
        write(6,*) 'nf_inq_varid error for variable ',n, var(n)
        stop
      end if
    end do

    ndycount = 0

    do m = mst, med

      if (m == 2) then
        if (mod(nyear,4) == 0) then
          if (mod(nyear,100) == 0) then
            if (mod(nyear,400) == 0) then
              nday = idmon(m) + 1
            else
              nday = idmon(m)
            end if
          else
            nday = idmon(m) + 1
          end if
        else
          nday = idmon(m)
        end if
      else
        nday = idmon(m)
      end if

      do nd = 1, nday

        ndycount = ndycount + 1

        start(3) = ndycount

        sta(1) = nf_get_vara_real(ifiles(1),var(1),start,range,dat4)

        do j = 1, jmut
          do i = 1, imut
            if (dat4(i,j) /= undef_oaflux) then
              num_valid(i,j) = num_valid(i,j) + 1
              hum2m(i,j) = hum2m(i,j) + real(dat4(i,j),8) * 0.01d0
            end if
          end do
        end do
      end do

    end do

    do j = 1, jmut
      do i = 1, imut
        if (num_valid(i,j) == ndycount) then
          hum2m(i,j) = hum2m(i,j) / real(num_valid(i,j))
        else
          hum2m(i,j) = undef_mxe
        end if
      end do
    end do

    do n = 1, nfiles
      write(6,*) ifiles(n)
      sta(n) = nf_close(ifiles(n))
      if (sta(n) /= 0) then
        write(6,*) 'nf_close error for file number ',n, ifiles(n), sta(n)
      end if
    end do

    write(6,*) nyear, ndycount

    ! open output file

    write(flot1,'(1a,i4.4)') '/workd/htsujino/OAflux/grads_annual/sph2m.',nyear
    open(mtot1,file=flot1,form='unformatted',access='direct',recl=4*imut*jmut)
    write(6,*) 'Specific humidity written to ',trim(flot1)
    ireco1 = 0
    ireco1 = ireco1 + 1
    write(mtot1,rec=ireco1) real(hum2m(1:imut,1:jmut),4)
    close(mtot1)

  end do

end program read_netcdf_output_grads_d2m
