! -*-F90-*-
!==================================================================
program read_netcdf_output_grads
  !----------------------------------------------------------------
  ! Information:
  !   Create direct access file (GrADS) of SCOW wind
  !----------------------------------------------------------------

  implicit none

  include '/usr/local/netcdf/include/netcdf.inc'

  integer(4), parameter :: imut = 1440, jmut = 560, jmut_wide = 720
  integer(4), parameter :: mtot1 = 91, mtot2 = 92

  real(4) :: dat4(imut,jmut)

  real(4) :: wind(imut,jmut_wide)
  real(4) :: work4(imut,jmut)

  integer(4) :: idmon(12)

  character(128) :: flot1, flot2

  integer(4) :: i, j, k, l, m, n, jw

  integer(4) :: nmocount
  integer(4) :: ireco1, ireco2
  integer(4) :: nyear
  integer(4) :: mst, med

  real(4) :: undef_in = 0.0, undef_out = 0.0
  real(4),parameter :: undef_scow = -9999.0
  real(4),parameter :: undef_mxe = -9.99e33

  ! for netCDF

  integer(4), parameter :: nvars = 12, nfiles = 1
  integer(4) :: var(nvars), sta(nvars)
  integer(4) :: start(3), range(3)
  character(128) :: flnin(nfiles)
  integer(4) :: ifiles(nfiles)

  character(128) :: flnin_scow, flnot_base

  !--------------------------------------------------------------------

  namelist /nml_scow_wind/ &
       & flnin_scow, undef_in, &
       & flnot_base, undef_out

  open (11,file='namelist.scow')
  read (11,nml=nml_scow_wind)
  close(11)

  !--------------------------------------------------------------------
  ! general settings

  idmon(1:12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  !---------------------------------------------------------------------
  ! main year
  !

  start(1:3) = (/ 1, 1, 1 /)
  range(1:3) = (/ imut, jmut, 1 /)

  ! open netcdf file

  write(flnin(1),'(1a)') trim(flnin_scow)

  do n = 1, nfiles
    sta(n) = nf_open(flnin(n),nf_nowrite,ifiles(n)) 
    if (sta(n) /= 0) then
      write(6,*) 'nf_open error for file number ',n
      stop
    end if
  end do

  sta(1)  = nf_inq_varid(ifiles(1),'january'  ,var(1))
  sta(2)  = nf_inq_varid(ifiles(1),'february' ,var(2))
  sta(3)  = nf_inq_varid(ifiles(1),'march'    ,var(3))
  sta(4)  = nf_inq_varid(ifiles(1),'april'    ,var(4))
  sta(5)  = nf_inq_varid(ifiles(1),'may'      ,var(5))
  sta(6)  = nf_inq_varid(ifiles(1),'june'     ,var(6))
  sta(7)  = nf_inq_varid(ifiles(1),'july'     ,var(7))
  sta(8)  = nf_inq_varid(ifiles(1),'august'   ,var(8))
  sta(9)  = nf_inq_varid(ifiles(1),'september',var(9))
  sta(10) = nf_inq_varid(ifiles(1),'october'  ,var(10))
  sta(11) = nf_inq_varid(ifiles(1),'november' ,var(11))
  sta(12) = nf_inq_varid(ifiles(1),'december' ,var(12))

  do n = 1, nvars
    if (sta(n) /= 0) then
      write(6,*) 'nf_inq_varid error for variable ',n, var(n)
      stop
    end if
  end do

  mst = 1
  med = 12

  do m = mst, med

    start(3) = 1

    !---------------------------------------------------------------------------

    sta(m) = nf_get_vara_real(ifiles(1),var(m),start,range,dat4)

    wind(:,:) = undef_mxe

    do j = 1, jmut
      jw = j + (jmut_wide - jmut) / 2
      do i = 1, imut
        if (dat4(i,j) == undef_scow) then
          wind(i,jw) = undef_mxe
        else
          wind(i,jw) = dat4(i,j)
        end if
      end do
    end do

    ! open output file

    write(flot1,'(1a,i2.2)') trim(flnot_base),m
    open(mtot1,file=flot1,form='unformatted',access='direct',recl=4*imut*jmut_wide)
    write(6,*) 'Wind speed written to ',trim(flot1)
    ireco1 = 0
    ireco1 = ireco1 + 1
    write(mtot1,rec=ireco1) wind
    close(mtot1)

  end do

  do n = 1, nfiles
    !write(6,*) ifiles(n)
    sta(n) = nf_close(ifiles(n))
    if (sta(n) /= 0) then
      write(6,*) 'nf_close error for file number ',n, ifiles(n), sta(n)
    end if
  end do

end program read_netcdf_output_grads
