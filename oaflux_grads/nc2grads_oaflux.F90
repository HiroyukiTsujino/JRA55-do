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
  integer(4), parameter :: mtot1 = 91, mtot2 = 92

  real(4) :: dat4(imut,jmut)
  real(8) :: dat8(imut,jmut)

  real(4) :: latent(imut,jmut)
  real(4) :: sensible(imut,jmut)
  real(4) :: work4(imut,jmut)

  integer(4) :: idmon(12)

  character(128) :: flot1, flot2

  integer(4) :: i, j, k, l, m, n

  integer(4) :: nmocount
  integer(4) :: ireco1, ireco2
  integer(4) :: nyear
  integer(4) :: mst, med

  real(4),parameter :: undef_oaflux_lh = 32766.0
  real(4),parameter :: undef_oaflux_sh = 32766.0
  real(4),parameter :: undef_mxe = -9.99e33

  ! for netCDF

  integer(4), parameter :: nvars = 2, nfiles = 2
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


  do nyear = 1958, 2014

    ! open netcdf file

    write(flnin(1),'(1a,i4.4,1a)') '/workd/htsujino/OAflux/monthly/lh_oaflux_',nyear,'.nc'
    write(flnin(2),'(1a,i4.4,1a)') '/workd/htsujino/OAflux/monthly/sh_oaflux_',nyear,'.nc'

    do n = 1, nfiles
      sta(n) = nf_open(flnin(n),nf_nowrite,ifiles(n)) 
      if (sta(n) /= 0) then
        write(6,*) 'nf_open error for file number ',n
        stop
      end if
    end do

    sta(1) = nf_inq_varid(ifiles(1),'lhtfl',var(1))
    sta(2) = nf_inq_varid(ifiles(2),'shtfl',var(2))

    do n = 1, nvars
      if (sta(n) /= 0) then
        write(6,*) 'nf_inq_varid error for variable ',n, var(n)
        stop
      end if
    end do

    mst = 1
    med = 12

    nmocount = 0

    do m = mst, med

      nmocount = nmocount + 1

      start(3) = nmocount

      !---------------------------------------------------------------------------

      sta(1) = nf_get_vara_real(ifiles(1),var(1),start,range,dat4)

      do j = 1, jmut
        do i = 1, imut
          if (dat4(i,j) == undef_oaflux_lh) then
            latent(i,j) = undef_mxe
          else
            latent(i,j) = dat4(i,j) * 0.1
          end if
        end do
      end do

      ! open output file

      write(flot1,'(1a,i4.4,i2.2)') &
           & '/workd/htsujino/OAflux/grads_monthly/latent.',nyear,m
      open(mtot1,file=flot1,form='unformatted',access='direct',recl=4*imut*jmut)
      write(6,*) 'Latent heat flux written to ',trim(flot1)
      ireco1 = 0
      ireco1 = ireco1 + 1
      write(mtot1,rec=ireco1) latent
      close(mtot1)

      !---------------------------------------------------------------------------

      sta(2) = nf_get_vara_real(ifiles(2),var(2),start,range,dat4)

      do j = 1, jmut
        do i = 1, imut
          if (dat4(i,j) == undef_oaflux_sh) then
            sensible(i,j) = undef_mxe
          else
            sensible(i,j) = dat4(i,j) * 0.1
          end if
        end do
      end do

      ! open output file

      write(flot2,'(1a,i4.4,i2.2)') &
           & '/workd/htsujino/OAflux/grads_monthly/sensible.',nyear,m
      open(mtot2,file=flot2,form='unformatted',access='direct',recl=4*imut*jmut)
      write(6,*) 'Sensible heat flux written to ',trim(flot2)
      ireco2 = 0
      ireco2 = ireco2 + 1
      write(mtot2,rec=ireco2) sensible
      close(mtot2)

      !---------------------------------------------------------------------------

    end do

    do n = 1, nfiles
      write(6,*) ifiles(n)
      sta(n) = nf_close(ifiles(n))
      if (sta(n) /= 0) then
        write(6,*) 'nf_close error for file number ',n, ifiles(n), sta(n)
      end if
    end do

  end do

end program read_netcdf_output_grads
