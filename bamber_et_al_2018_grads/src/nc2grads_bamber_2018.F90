! -*-F90-*-
!==================================================================
program read_netcdf_output_grads
  !----------------------------------------------------------------
  ! Information:
  !   Create direct access file (GrADS) of NOAA-AOML drifter data
  !----------------------------------------------------------------

  implicit none

  include 'netcdf.inc'

  integer(4), parameter :: imut = 752, jmut = 785, ndata = 708
  integer(4), parameter :: mtot1 = 91, mtot2 = 92

  ! For GrADs output

  real(4) :: work4(imut,jmut)

  ! For reading netCDF file

  real(8) :: dat8(imut,jmut)
  real(4) :: dat4(imut,jmut)
  integer(2) :: idat2(imut, jmut)
  integer(1) :: idat1(imut, jmut)

!  real(8) :: runoff_tundra(imut,jmut)
!  real(8) :: runoff_ice_sheet(imut,jmut)
!  real(8) :: runoff_solid_ice(imut,jmut)
!  real(8) :: latitude(imut,jmut)
!  real(8) :: longitude(imut,jmut)
!  real(8) :: lsmgr(imut,jmut)
!  real(8) :: ocean_basins(imut,jmut)

  integer(4) :: idmon(12)

  character(128) :: flot1, flot2

  integer(4) :: i, j, k, l, m, n, jw

  integer(4) :: nmocount
  integer(4) :: ireco1, ireco2
  integer(4) :: nyear
  integer(4) :: nyst, nyed
  integer(4) :: mst, med
  integer(4) :: ms, me, ny

  real(4) :: undef_in = 0.0, undef_out = 0.0
  real(4),parameter :: undef_bamber = -9999.0
  real(4),parameter :: undef_mxe = -9.99e33

  ! for netCDF

  integer(4), parameter :: nvars = 12, nfiles = 1
  integer(4) :: var(nvars), sta(nvars)
  integer(4) :: start(3), range(3)
  character(128) :: flnin(nfiles)
  integer(4) :: ifiles(nfiles)

  integer(1) :: itmp_int1
  integer(2) :: itmp_int2
  real(4)    :: rtmp_r4
  real(8)    :: rtmp_r8

  integer(4) :: ifill_value(nvars)
  real(8) :: rfill_value(nvars)
  real(4) :: scale_factor(nvars)

  integer(4) :: iostat

  character(128) :: flnin_bamber, flnot_base

  logical :: isnan

  integer(4) :: irec_nc

  !--------------------------------------------------------------------

  namelist /nml_bamber_2018/ &
       & flnin_bamber, undef_in, &
       & flnot_base, undef_out, nyst, nyed, mst, med

  mst = 1
  med = 1

  open (11,file='namelist.bamber_2018')
  read (11,nml=nml_bamber_2018)
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

  write(flnin(1),'(1a)') trim(flnin_bamber)

  do n = 1, nfiles
    write(6,*) 'Openning ',trim(flnin(n))
    sta(n) = nf_open(flnin(n),nf_nowrite,ifiles(n)) 
    write(6,*) 'ifiles = ', ifiles(1)
    if (sta(n) /= 0) then
      write(6,*) 'nf_open error for file number ',n, sta(n)
      stop
    end if
  end do

  sta(1) = nf_inq_varid(ifiles(1),'runoff_tundra',var(1))
  sta(2) = nf_inq_varid(ifiles(1),'runoff_ice'   ,var(2))
  sta(3) = nf_inq_varid(ifiles(1),'solid_ice'    ,var(3))
  sta(4) = nf_inq_varid(ifiles(1),'lon'          ,var(4))
  sta(5) = nf_inq_varid(ifiles(1),'lat'          ,var(5))
  sta(6) = nf_inq_varid(ifiles(1),'LSMGr'        ,var(6))
  sta(7) = nf_inq_varid(ifiles(1),'ocean_basins' ,var(7))

  do n = 1, 7
    if (sta(n) /= 0) then
      write(6,*) 'nf_inq_varid error for variable ',n, var(n)
      stop
    else
      write(6,*) ' var sta = ', var(n), sta(n)
    end if
  end do

  ! var 1

  sta(1) = 0
  iostat = nf_get_att_int2(ifiles(1),var(1),'_FillValue',itmp_int2)
  sta(1) = sta(1) + iostat
  ifill_value(1) = int(itmp_int2,4)
  iostat = nf_get_att_real(ifiles(1),var(1),'scale_factor',rtmp_r4)
  sta(1) = sta(1) + iostat
  scale_factor(1) = rtmp_r4
  write(6,*) ' '
  write(6,*) ' Summary for Variable 1 '
  write(6,*) '    _FillValue for variable    = ',ifill_value(1)
  write(6,*) '    scale_factor for variable  = ',scale_factor(1)
  write(6,*) ' '

  ! var 2

  sta(2) = 0
  iostat = nf_get_att_int2(ifiles(1),var(2),'_FillValue',itmp_int2)
  sta(2) = sta(2) + iostat
  ifill_value(2) = int(itmp_int2,4)
  iostat = nf_get_att_real(ifiles(1),var(2),'scale_factor',rtmp_r4)
  sta(2) = sta(2) + iostat
  scale_factor(2) = rtmp_r4
  write(6,*) ' '
  write(6,*) ' Summary for Variable 2 '
  write(6,*) '    _FillValue for variable    = ',ifill_value(2)
  write(6,*) '    scale_factor for variable  = ',scale_factor(2)
  write(6,*) ' '

  ! var 3

  sta(3) = 0
  iostat = nf_get_att_int2(ifiles(1),var(3),'_FillValue',itmp_int2)
  sta(3) = sta(3) + iostat
  ifill_value(3) = int(itmp_int2,4)
  iostat = nf_get_att_real(ifiles(1),var(3),'scale_factor',rtmp_r4)
  sta(3) = sta(3) + iostat
  scale_factor(3) = rtmp_r4
  write(6,*) ' '
  write(6,*) ' Summary for Variable 3 '
  write(6,*) '    _FillValue for variable    = ',ifill_value(3)
  write(6,*) '    scale_factor for variable  = ',scale_factor(3)
  write(6,*) ' '

  ! var 4

  sta(4) = 0
  iostat = nf_get_att_double(ifiles(1),var(4),'_FillValue',rtmp_r8)
  sta(4) = sta(4) + iostat
  rfill_value(4) = rtmp_r8
  write(6,*) ' '
  write(6,*) ' Summary for Variable 4 '
  write(6,*) '    _FillValue for variable    = ',rfill_value(4)
  write(6,*) ' '

  ! var 5

  sta(5) = 0
  iostat = nf_get_att_double(ifiles(1),var(5),'_FillValue',rtmp_r8)
  sta(5) = sta(5) + iostat
  rfill_value(5) = rtmp_r8
  write(6,*) ' '
  write(6,*) ' Summary for Variable 5 '
  write(6,*) '    _FillValue for variable    = ',rfill_value(5)
  write(6,*) ' '

  ! var 6

  sta(6) = 0
  iostat = nf_get_att_int1(ifiles(1),var(6),'_FillValue',itmp_int1)
  sta(6) = sta(6) + iostat
  ifill_value(6) = int(itmp_int1,4)
  write(6,*) ' '
  write(6,*) ' Summary for Variable 6 '
  write(6,*) '    _FillValue for variable    = ',ifill_value(6)
  write(6,*) ' '

  ! var 7

  sta(7) = 0
  iostat = nf_get_att_int1(ifiles(1),var(7),'_FillValue',itmp_int1)
  sta(7) = sta(7) + iostat
  ifill_value(7) = int(itmp_int1,4)
  write(6,*) ' '
  write(6,*) ' Summary for Variable 7 '
  write(6,*) '    _FillValue for variable    = ',ifill_value(7)
  write(6,*) ' '

  do n = 1, 7
    if (sta(n) /= 0) then
      write(6,*) 'read attribute error for variable ',n, var(n)
      stop
    else
      write(6,*) ' var sta = ', var(n), sta(n)
    end if
  end do

  irec_nc = 0

  LOOP_YEAR: do ny = nyst, nyed

    if (ny == nyst) then
      ms = mst
    else
      ms = 1
    end if
    if (ny == nyed) then
      me = med
    else
      me = 12
    end if

    LOOP_MONTH: do m = mst, med

      irec_nc = irec_nc + 1

      write(flot1,'(1a,i4.4,i2.2)') trim(flnot_base),ny,m
      open(mtot1,file=flot1,form='unformatted',access='direct',recl=4*imut*jmut)
      write(6,*) 'Greenland runoff written to ',trim(flot1)
      ireco1 = 0
      write(6,*) ' netCDF record number = ', irec_nc

      start(3) = irec_nc

      !---------------------------------------------------------------------------

      write(6,*) ' Getting variable 1'
      n = 1
      write(6,*) ' Scale factor = ', scale_factor(n)
      sta(n) = nf_get_vara_int2(ifiles(1),var(n),start,range,idat2)
      do j = 1, jmut
        do i = 1, imut
          if (idat2(i,j) == int(ifill_value(n),2)) then
            dat4(i,jmut-j+1) = undef_mxe
          else
            dat4(i,jmut-j+1) = real(idat2(i,j),4) * scale_factor(n)
          end if
        end do
      end do
      ireco1 = ireco1 + 1
      write(mtot1,rec=ireco1) dat4

      write(6,*) ' Getting variable 2'
      n = 2
      write(6,*) ' Scale factor = ', scale_factor(n)
      sta(n) = nf_get_vara_int2(ifiles(1),var(n),start,range,idat2)
      do j = 1, jmut
        do i = 1, imut
          if (idat2(i,j) == int(ifill_value(n),2)) then
            dat4(i,jmut-j+1) = undef_mxe
          else
            dat4(i,jmut-j+1) = real(idat2(i,j),4) * scale_factor(n)
          end if
        end do
      end do
      ireco1 = ireco1 + 1
      write(mtot1,rec=ireco1) dat4

      write(6,*) ' Getting variable 3'
      n = 3
      write(6,*) ' Scale factor = ', scale_factor(n)
      sta(n) = nf_get_vara_int2(ifiles(1),var(n),start,range,idat2)
      do j = 1, jmut
        do i = 1, imut
          if (idat2(i,j) == int(ifill_value(n),2)) then
            dat4(i,jmut-j+1) = undef_mxe
          else
            dat4(i,jmut-j+1) = real(idat2(i,j),4) * scale_factor(n)
          end if
        end do
      end do
      ireco1 = ireco1 + 1
      write(mtot1,rec=ireco1) dat4

      write(6,*) ' Getting variable 4'
      n = 4
      sta(n) = nf_get_vara_double(ifiles(1),var(n),start,range,dat8)
      do j = 1, jmut
        do i = 1, imut
          if (isnan(dat8(i,j))) then
            dat4(i,jmut-j+1) = undef_mxe
          else
            dat4(i,jmut-j+1) = real(dat8(i,j),4)
          end if
        end do
      end do
      ireco1 = ireco1 + 1
      write(mtot1,rec=ireco1) dat4

      write(6,*) ' Getting variable 5'
      n = 5
      sta(n) = nf_get_vara_double(ifiles(1),var(n),start,range,dat8)
      do j = 1, jmut
        do i = 1, imut
          if (isnan(dat8(i,j))) then
            dat4(i,jmut-j+1) = undef_mxe
          else
            dat4(i,jmut-j+1) = real(dat8(i,j),4)
          end if
        end do
      end do
      ireco1 = ireco1 + 1
      write(mtot1,rec=ireco1) dat4

      write(6,*) ' Getting variable 6'
      n = 6
      sta(n) = nf_get_vara_int1(ifiles(1),var(n),start,range,idat1)
      do j = 1, jmut
        do i = 1, imut
          if (idat1(i,j) == int(ifill_value(n),1)) then
            dat4(i,jmut-j+1) = undef_mxe
          else
            dat4(i,jmut-j+1) = real(idat1(i,j),4)
          end if
        end do
      end do
      ireco1 = ireco1 + 1
      write(mtot1,rec=ireco1) dat4

      write(6,*) ' Getting variable 7'
      n = 7
      sta(n) = nf_get_vara_int1(ifiles(1),var(n),start,range,idat1)
      do j = 1, jmut
        do i = 1, imut
          if (idat1(i,j) == int(ifill_value(n),1)) then
            dat4(i,jmut-j+1) = undef_mxe
          else
            dat4(i,jmut-j+1) = real(idat1(i,j),4)
          end if
        end do
      end do
      ireco1 = ireco1 + 1
      write(mtot1,rec=ireco1) dat4

      do n = 1, 7
        if (sta(n) /= 0) then
          write(6,*) 'nf_get_vara_type error for variable ',n, var(n), sta(n), ifiles(1)
          stop
        end if
      end do

      close(mtot1)
    end do LOOP_MONTH
  end do LOOP_YEAR


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
