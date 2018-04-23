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

  real(4) :: sst(imut,jmut)
  real(4) :: ice(imut,jmut)
  real(4) :: mask(imut,jmut)

  integer(4) :: idmon(12)

  character(128) :: flot1, flot2

  integer(4), parameter :: mtmsk = 93
  character(128) :: file_mask

  integer(4) :: i, j, k, l, m, n

  integer(4) :: nmocount
  integer(4) :: ireco1, ireco2
  integer(4) :: nyear
  integer(4) :: mst, med

  real(4),parameter :: undef_hurrell = 1.0e20
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
  ! open netcdf file

  write(flnin(1),'(1a)') &
       & '../linkdir/Hurrell_SST_ICE/orgdata/MODEL.SST.HAD187001-198110.OI198111-201203.nc'
  write(flnin(2),'(1a)') &
       & '../linkdir/Hurrell_SST_ICE/orgdata/MODEL.ICE.HAD187001-198110.OI198111-201203.nc'

  do n = 1, nfiles
    sta(n) = nf_open(flnin(n),nf_nowrite,ifiles(n)) 
    if (sta(n) /= 0) then
      write(6,*) 'nf_open error for file number ',n
      stop
    end if
  end do

  sta(1) = nf_inq_varid(ifiles(1),'SST',var(1))
  sta(2) = nf_inq_varid(ifiles(2),'SEAICE',var(2))

  do n = 1, nvars
    if (sta(n) /= 0) then
      write(6,*) 'nf_inq_varid error for variable ',n, var(n)
      stop
    end if
  end do

  start(1:3) = (/ 1, 1, 1 /)
  range(1:3) = (/ 360, 180, 1 /)

  nmocount = 0

  do nyear = 1870, 2012

    mst = 1
    med = 12

    if (nyear == 2012) med = 3

    do m = mst, med

      nmocount = nmocount + 1

      start(3) = nmocount

      !---------------------------------------------------------------------------

      sta(1) = nf_get_vara_real(ifiles(1),var(1),start,range,dat4)

      do j = 1, jmut
        do i = 1, imut
          if (dat4(i,j) == undef_hurrell) then
            sst(i,j) = undef_mxe
          else
!            if (mask(i,j) == 1.0) then
            sst(i,j) = dat4(i,j)
!            else
!              sst(i,j) = undef_mxe
             !end if
          end if
        end do
      end do

      ! open output file

      write(flot1,'(1a,i4.4,i2.2)') &
           & '../linkdir/Hurrell_SST_ICE/grads/sst-glb.',nyear,m
      open(mtot1,file=flot1,form='unformatted',access='direct',recl=4*imut*jmut)
      write(6,*) 'SST written to ',trim(flot1)
      write(mtot1,rec=1) sst
      close(mtot1)

      !---------------------------------------------------------------------------

      sta(2) = nf_get_vara_real(ifiles(2),var(2),start,range,dat4)

      do j = 1, jmut
        do i = 1, imut
          if (dat4(i,j) == undef_hurrell) then
            ice(i,j) = undef_mxe
          else
!            if (mask(i,j) == 1.0) then
            ice(i,j) = min(dat4(i,j) * 1.0e-2, 1.0)
!            else
!              ice(i,j) = undef_mxe
!            end if
          end if
        end do
      end do

      ! open output file

      write(flot2,'(1a,i4.4,i2.2)') &
           & '../linkdir/Hurrell_SST_ICE/grads/ice-glb.',nyear,m
      open(mtot2,file=flot2,form='unformatted',access='direct',recl=4*imut*jmut)
      write(6,*) 'SEA ICE written to ',trim(flot2)
      write(mtot2,rec=1) ice
      close(mtot2)

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
