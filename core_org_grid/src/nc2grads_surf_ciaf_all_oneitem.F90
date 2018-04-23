! -*-F90-*-
!==================================================================
program read_netcdf_write_grads
  !----------------------------------------------------------------
  ! Information:
  !   Create direct access file (GrADS) of
  !     the corrected normal year forcing (CNYF) of CORE.
  !
  !   Except for river run off data (on different grid points).
  !
  !                                         2007.5.9 H.Tsujino
  !----------------------------------------------------------------

  implicit none

  include '/usr/local/netcdf/include/netcdf.inc'

  integer(4), parameter :: imut = 192, jmut = 94
  integer(4), parameter :: mtot1 = 91, mtot2 = 92, mtot3 = 93, mtot4 = 94, mtot5 = 95
  integer(4), parameter :: mtot6 = 96, mtot7 = 97, mtot8 = 98, mtot9 = 99

  real(4) :: dat4(imut,jmut)
  real(8) :: dat8(imut,jmut)

  real(4) :: us  (imut,jmut)
  real(4) :: vs  (imut,jmut)
  real(4) :: sat (imut,jmut)
  real(4) :: qa  (imut,jmut)
  real(4) :: slp (imut,jmut)
  real(4) :: dsw (imut,jmut)
  real(4) :: dlw (imut,jmut)
  real(4) :: rain(imut,jmut)
  real(4) :: snow(imut,jmut)

  real(4) :: wdv(imut,jmut)
  real(4) :: pcp(imut,jmut)

  integer(4) :: idmon(12)

  integer(4) :: nyear

  character(128) :: flin1, flin2, flin3, flin4, flin5
  character(128) :: flot1, flot2, flot3, flot4, flot5
  character(128) :: flot6, flot7, flot8, flot9

  integer(4) :: i, j, k, l, m, n
  integer(4) :: nmocount, ndycount, n6hcount
  integer(4) :: ireco1, ireco2, ireco3, ireco4, ireco5
  integer(4) :: ireco6, ireco7, ireco8, ireco9

  ! for netCDF

  integer(4), parameter :: nvars = 9, nfiles = 7
  integer(4) :: var(nvars), sta(nvars)
  integer(4) :: start(3), range(3)
  character(128) :: flnin(nfiles)
  integer(4) :: ifiles(nfiles)

  !--------------------------------------------------------------------
  ! general settings

  idmon(1:12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  !--------------------------------------------------------------------

  LOOP_YEAR: do nyear = 1948, 2004
!  LOOP_YEAR: do nyear = 1997, 2004
!  LOOP_YEAR: do nyear = 2005, 2005
!  LOOP_YEAR: do nyear = 2006, 2007
!  LOOP_YEAR: do nyear = 2008, 2009

  ! open output file

  write(flot1,'(1A,I4.4)') &
       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/grads_oneitem/u10m.',nyear
  open(mtot1,file=flot1,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'U10m data written to ',trim(flot1)
  ireco1 = 0

  write(flot2,'(1A,I4.4)') &
       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/grads_oneitem/v10m.',nyear
  open(mtot2,file=flot2,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'V10m data written to ',trim(flot2)
  ireco2 = 0

  write(flot3,'(1A,I4.4)') &
       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/grads_oneitem/dswrf.',nyear
  open(mtot3,file=flot3,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'Short wave data written to ',trim(flot3)
  ireco3 = 0

  write(flot4,'(1A,I4.4)') &
       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/grads_oneitem/dlwrf.',nyear
  open(mtot4,file=flot4,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'Long wave data written to ',trim(flot4)
  ireco4 = 0

  write(flot5,'(1A,I4.4)') &
       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/grads_oneitem/tmp10m.',nyear
  open(mtot5,file=flot5,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'Temperature (2m) written to ',trim(flot5)
  ireco5 = 0

  write(flot6,'(1A,I4.4)') &
       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/grads_oneitem/sph10m.',nyear
  open(mtot6,file=flot6,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'Specific humidity (2m) written to ',trim(flot6)
  ireco6 = 0

  write(flot7,'(1A,I4.4)') &
       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/grads_oneitem/wind10m.',nyear
  open(mtot7,file=flot7,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'Wind speed (10m) written to ',trim(flot7)
  ireco7 = 0

  write(flot8,'(1A,I4.4)') &
       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/grads_oneitem/slprs.',nyear
  open(mtot8,file=flot8,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'Sea level pressure written to ',trim(flot8)
  ireco8 = 0

  write(flot9,'(1A,I4.4)') &
       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/grads_oneitem/prcp.',nyear
  open(mtot9,file=flot9,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'Precipitation data written to ',trim(flot9)
  ireco9 = 0

  !--------------------------------------------------------------------
  ! open netcdf file

  write(flnin(1),'(1A,I4.4,1A)') &
       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/orgdata/u_10.',nyear,'.05APR2010.nc'
!  write(flnin(1),'(1A,I4.4,1A)') &
!       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/orgdata/u_10.',nyear,'.06JUN2011.nc'
!  write(flnin(1),'(1A,I4.4,1A)') &
!       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/orgdata/u_10.',nyear,'.23OCT2012.nc'

  write(flnin(2),'(1A,I4.4,1A)') &
       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/orgdata/v_10.',nyear,'.05APR2010.nc'
!  write(flnin(2),'(1A,I4.4,1A)') &
!       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/orgdata/v_10.',nyear,'.06JUN2011.nc'
!  write(flnin(2),'(1A,I4.4,1A)') &
!       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/orgdata/v_10.',nyear,'.23OCT2012.nc'

  write(flnin(3),'(1A,I4.4,1A)') &
       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/orgdata/t_10.',nyear,'.05APR2010.nc'
!  write(flnin(3),'(1A,I4.4,1A)') &
!       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/orgdata/t_10.',nyear,'.10FEB1011.nc'
!  write(flnin(3),'(1A,I4.4,1A)') &
!       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/orgdata/t_10.',nyear,'.06JUN2011.nc'
!  write(flnin(3),'(1A,I4.4,1A)') &
!       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/orgdata/t_10.',nyear,'.23OCT2012.nc'


  write(flnin(4),'(1A,I4.4,1A)') &
       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/orgdata/q_10.',nyear,'.05APR2010.nc'
!  write(flnin(4),'(1A,I4.4,1A)') &
!       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/orgdata/q_10.',nyear,'.06JUN2011.nc'
!  write(flnin(4),'(1A,I4.4,1A)') &
!       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/orgdata/q_10.',nyear,'.23OCT2012.nc'

  write(flnin(5),'(1A,I4.4,1A)') &
       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/orgdata/slp.',nyear,'.05APR2010.nc'
!  write(flnin(5),'(1A,I4.4,1A)') &
!       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/orgdata/slp.',nyear,'.06JUN2011.nc'
!  write(flnin(5),'(1A,I4.4,1A)') &
!       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/orgdata/slp.',nyear,'.23OCT2012.nc'

  write(flnin(6),'(1A,I4.4,1A)') &
       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/orgdata/ncar_precip.',nyear,'.05APR2010.nc'
!  write(flnin(6),'(1A,I4.4,1A)') &
!       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/orgdata/ncar_precip.',nyear,'.06JUN2011.nc'
!  write(flnin(6),'(1A,I4.4,1A)') &
!       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/orgdata/ncar_precip.',nyear,'.23OCT2012.nc'

  write(flnin(7),'(1A,I4.4,1A)') &
       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/orgdata/ncar_rad.',nyear,'.05APR2010.nc'
!  write(flnin(7),'(1A,I4.4,1A)') &
!       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/orgdata/ncar_rad.',nyear,'.06JUN2011.nc'
!  write(flnin(7),'(1A,I4.4,1A)') &
!       & '/workc/ocpublic/refdata/CORE/COREv2/ciaf/orgdata/ncar_rad.',nyear,'.23OCT2012.nc'

  !------------------------------------------------

  do n = 1, nfiles
    sta(n) = nf_open(flnin(n),nf_nowrite,ifiles(n))
    if (sta(n) /= 0) then
      write(6,*) 'nf_open error for file number ',n
      stop
    end if
  end do

  sta(1) = nf_inq_varid(ifiles(1),'U_10_MOD',var(1))
  sta(2) = nf_inq_varid(ifiles(2),'V_10_MOD',var(2))
  sta(3) = nf_inq_varid(ifiles(3),'T_10_MOD',var(3))
  sta(4) = nf_inq_varid(ifiles(4),'Q_10_MOD',var(4))
  sta(5) = nf_inq_varid(ifiles(5),'SLP',var(5))

  sta(6) = nf_inq_varid(ifiles(6),'RAIN',var(6))
  sta(7) = nf_inq_varid(ifiles(6),'SNOW',var(7))

  sta(8) = nf_inq_varid(ifiles(7),'SWDN_MOD',var(8))
  sta(9) = nf_inq_varid(ifiles(7),'LWDN_MOD',var(9))

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
  range(1:3) = (/ 192, 94, 1 /)

  nmocount = 0

  ! ===== PRECIPITATION ===== !

  do m = 1, 12

    nmocount = nmocount + 1
    start(3) = nmocount

    sta(6) = nf_get_vara_real(ifiles(6),var(6),start,range,dat4)
    rain(1:imut,1:jmut) = dat4(1:imut,1:jmut)
    sta(7) = nf_get_vara_real(ifiles(6),var(7),start,range,dat4)
    snow(1:imut,1:jmut) = dat4(1:imut,1:jmut)

    pcp(1:imut,1:jmut) = (rain(1:imut,1:jmut) + snow(1:imut,1:jmut))

    do k = 6, 7
      if (sta(k) /= 0) then
        write(6,*) 'nf_get_vara_real error for file number ',&
             & n6hcount, k, var(k), sta(k)
        stop
      end if
    end do

    ireco9 = ireco9 + 1
!    write(6,*) ireco9
    write(mtot9,rec=ireco9) pcp

  end do

  ! ===== RADIATION ===== !

  ndycount = 0

  do m = 1, 12

    do n = 1, idmon(m)

      ndycount = ndycount + 1
      start(3) = ndycount

      sta(8) = nf_get_vara_real(ifiles(7),var(8),start,range,dat4)
      dsw(1:imut,1:jmut) = dat4(1:imut,1:jmut)

      sta(9) = nf_get_vara_real(ifiles(7),var(9),start,range,dat4)
      dlw(1:imut,1:jmut) = dat4(1:imut,1:jmut)

      do k = 8, 9
        if (sta(k) /= 0) then
          write(6,*) 'nf_get_vara_real error for file number ',&
               & n6hcount, k, var(k), sta(k)
          stop
        end if
      end do

      ireco3 = ireco3 + 1
      write(mtot3,rec=ireco3) dsw
      ireco4 = ireco4 + 1
      write(mtot4,rec=ireco4) dlw

!      write(6,*) ireco3, ireco4

    end do
  end do

  ! ===== WIND and ATMOSPHERE ===== !

  n6hcount = 0

  do m = 1, 12

    do n = 1, idmon(m)

      do l = 1, 4

        n6hcount = n6hcount + 1
        start(3) = n6hcount

        sta(1) = nf_get_vara_real(ifiles(1),var(1),start,range,dat4)
        us(1:imut,1:jmut) = dat4(1:imut,1:jmut)

        sta(2) = nf_get_vara_real(ifiles(2),var(2),start,range,dat4)
        vs(1:imut,1:jmut) = dat4(1:imut,1:jmut)

        sta(3) = nf_get_vara_real(ifiles(3),var(3),start,range,dat4)
        sat(1:imut,1:jmut) = dat4(1:imut,1:jmut)

        sta(4) = nf_get_vara_real(ifiles(4),var(4),start,range,dat4)
        qa(1:imut,1:jmut) = dat4(1:imut,1:jmut)

        sta(5) = nf_get_vara_real(ifiles(5),var(5),start,range,dat4)
        slp(1:imut,1:jmut) = dat4(1:imut,1:jmut)

        do k = 1, 5
          if (sta(k) /= 0) then
            write(6,*) 'nf_get_vara_real error for file number ',&
                 & n6hcount, k, var(k), sta(k), ifiles(k)
            stop
          end if
        end do

        do j = 1, jmut
          do i = 1, imut
            wdv(i,j) = sqrt(us(i,j)**2 + vs(i,j)**2)
          end do
        end do

        ireco1 = ireco1 + 1
        write(mtot1,rec=ireco1) us
        ireco2 = ireco2 + 1
        write(mtot2,rec=ireco2) vs

        ireco5 = ireco5 + 1
        write(mtot5,rec=ireco5) sat
        ireco6 = ireco6 + 1
        write(mtot6,rec=ireco6) qa
        ireco7 = ireco7 + 1
        write(mtot7,rec=ireco7) wdv
        ireco8 = ireco8 + 1
        write(mtot8,rec=ireco8) slp
        
      end do
    end do
  end do

  do n = 1, nfiles
    sta(n) = nf_close(ifiles(n))
    if (sta(n) /= 0) then
      write(6,*) 'nf_close error for file number ',n, ifiles(n), sta(n)
    end if
  end do

  close(mtot9)
  close(mtot8)
  close(mtot7)
  close(mtot6)
  close(mtot5)
  close(mtot4)
  close(mtot3)
  close(mtot2)
  close(mtot1)

  end do LOOP_YEAR

end program read_netcdf_write_grads
