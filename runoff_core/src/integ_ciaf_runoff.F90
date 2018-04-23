! -*-F90-*-
!==================================================================
program integ_runoff
  !----------------------------------------------------------------
  ! Information:
  !
  !----------------------------------------------------------------

  implicit none

  include '/usr/local/netcdf/include/netcdf.inc'

  integer(4), parameter :: imut = 360, jmut = 180
  integer(4), parameter :: mtot1 = 91, mtot2 = 92

  real(4) :: dat4(imut,jmut)
  real(8) :: dat8(imut,jmut)

  real(8) :: rof(imut,jmut)
  real(8) :: are(imut,jmut)

  real(8) :: annual_mean
  real(8) :: monthly_mean

  integer(4) :: idmon(12)

  character(256) :: flot1, flot2

  integer(4) :: i, j, k, l, m, n

  integer(4) :: nmocount
  integer(4) :: ireco1, ireco2
  integer(4) :: nyear

  integer(4) :: ndyear

  ! for netCDF

  integer(4), parameter :: nvars = 2, nfiles = 1
  integer(4) :: var(nvars), sta(nvars)
  integer(4) :: start(3), range(3)
  character(128) :: flnin(nfiles)
  integer(4) :: ifiles(nfiles)

  character(128) :: fl_in

  integer(4) :: ibyr, ieyr

  !--------------------------------------------------------------------

  namelist /nml_core_runoff_integ/ fl_in, ibyr, ieyr

  !--------------------------------------------------------------------
 
  open(10, file='namelist.core_runoff_integ')
  read(10, nml=nml_core_runoff_integ)
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

  !---------------------------------------------------------------------
  ! main year
  !

  start(1:3) = (/ 1, 1, 1 /)
  range(1:3) = (/ 360, 180, 1 /)

  nmocount = 0

  do nyear = ibyr, ieyr

    write(flot1,'(1a,i4.4)') 'result/runoff.',nyear
    open(mtot1,file=flot1,form='unformatted',access='direct',recl=4)
    write(6,*) 'DATA written to ',trim(flot1)
    ireco1 = 0

    annual_mean = 0.0d0
    ndyear = 0

    do m = 1, 12

      write(flot2,'(1a,i4.4,i2.2)') 'result/runoff.',nyear, m
      open(mtot2,file=flot2,form='unformatted',access='direct',recl=4)
      write(6,*) 'DATA written to ',trim(flot2)
      ireco2 = 0

      nmocount = nmocount + 1

      start(3) = nmocount
      sta(1) = nf_get_vara_double(ifiles(1),var(1),start,range,rof)

      start(3) = 1
      sta(2) = nf_get_vara_double(ifiles(1),var(2),start,range,are)

      monthly_mean = 0.0d0
      do j = 1, jmut
        do i = 1, imut
          monthly_mean = monthly_mean + rof(i,j) * are(i,j)
        end do
      end do

      write(mtot2,rec=1) real(monthly_mean,4)
      close(mtot2)

      ndyear = ndyear + idmon(m)
      annual_mean = annual_mean + monthly_mean * real(idmon(m),8)

    end do

    annual_mean = annual_mean / real(ndyear,8)
    ireco1 = ireco1 + 1
    write(6,*) annual_mean * 1.0d-9
    write(mtot1,rec=1) real(annual_mean,4)
    close(mtot1)
  
  end do

  do n = 1, nfiles
    write(6,*) ifiles(n)
    sta(n) = nf_close(ifiles(n))
    if (sta(n) /= 0) then
      write(6,*) 'nf_close error for file number ',n, ifiles(n), sta(n)
    end if
  end do

end program integ_runoff
