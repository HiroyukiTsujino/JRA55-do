! -*-F90-*-
!==================================================================
program read_netcdf_output_grads
  !----------------------------------------------------------------
  ! Information:
  !   Create direct access file (GrADS) of
  !     the MERRA2 surface data
  !----------------------------------------------------------------

  implicit none

  include 'netcdf.inc'

  integer(4), parameter :: imut = 576, jmut = 361
  integer(4), parameter :: mtot1 = 91, mtot2 = 92

  real(4) :: dat4(imut,jmut)
  real(8) :: dat8(imut,jmut)

  real(4) :: qair(imut,jmut)
  real(4) :: work4(imut,jmut)

  integer(4) :: idmon(12)

  character(128) :: flot1, flot2

  integer(4) :: i, j, k, l, m, n

  integer(4) :: nmocount
  integer(4) :: ireco1, ireco2
  integer(4) :: nyear

  real(4),parameter :: undef_merra = 1.0e15
  real(4),parameter :: undef_mxe = -9.99e33

  ! for netCDF

  integer(4), parameter :: nvars = 1, nfiles = 1
  integer(4) :: var(nvars), sta(nvars)
  integer(4) :: start(3), range(3)
  character(128) :: flnin(nfiles)
  integer(4) :: ifiles(nfiles)

  character(128) :: file_in, file_out
  character(128) :: var_in

  !--------------------------------------------------------------------

  namelist /nml_merra_const/ &
       & file_in, file_out, &
       & var_in

  !--------------------------------------------------------------------

  open (10,file='namelist.merra_const')
  read (10,nml=nml_merra_const)
  close(10)

  !--------------------------------------------------------------------
  ! general settings

  idmon(1:12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  !---------------------------------------------------------------------
  ! main year
  !

  start(1:3) = (/ 1, 1, 1 /)
  range(1:3) = (/ 576, 361, 1 /)

  !--------------------------------------------------------------------
  ! open netcdf file

  write(flnin(1),'(1a)') trim(file_in)

  write(6,*) 'reading from ', flnin(1)

  do n = 1, nfiles
    sta(n) = nf_open(flnin(n),nf_nowrite,ifiles(n)) 
    if (sta(n) /= 0) then
      write(6,*) 'nf_open error for file number ',n
      stop
    end if
  end do

  sta(1) = nf_inq_varid(ifiles(1),trim(var_in),var(1))

  do n = 1, nvars
    if (sta(n) /= 0) then
      write(6,*) 'nf_inq_varid error for variable ',n, var(n)
      stop
    end if
  end do

  nmocount = 1

  start(3) = nmocount
  sta(1) = nf_get_vara_real(ifiles(1),var(1),start,range,dat4)

  do n = 1, nvars
    if (sta(n) /= 0) then
      write(6,*) 'nf_get_vara_real error for variable ',n, var(n)
      stop
    end if
  end do

  do j = 1, jmut
    do i = 1, imut
      if (i > imut/2) then
        qair(i-imut/2,j) = real(dat4(i,j),4)
      else
        qair(i+imut/2,j) = real(dat4(i,j),4)
      end if
    end do
  end do

  do j = 1, jmut
    do i = 1, imut
      if (qair(i,j) == undef_merra) then
        qair(i,j) = undef_mxe
      end if
    end do
  end do

  ! open output file
  write(flot1,'(1a)') trim(file_out)
  open(mtot1,file=flot1,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'DATA written to ',trim(flot1)
  ireco1 = 0

  ireco1 = ireco1 + 1

  write(mtot1,rec=ireco1) qair

  close(mtot1)

  do n = 1, nfiles
    !write(6,*) ifiles(n)
    sta(n) = nf_close(ifiles(n))
    if (sta(n) /= 0) then
      write(6,*) 'nf_close error for file number ',n, ifiles(n), sta(n)
    end if
  end do

end program read_netcdf_output_grads
