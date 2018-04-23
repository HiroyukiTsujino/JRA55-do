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

  integer(4), parameter :: imut = 480, jmut = 241
  integer(4), parameter :: mtot1 = 91, mtot2 = 92, mtot3 = 93, mtot4 = 94
  integer(4), parameter :: mtot5 = 95, mtot6 = 96, mtot7 = 97, mtot8 = 98
  real(8), parameter    :: pi = 3.141592653589793d0

  real(4) :: dat4(imut,jmut)
  real(8) :: dat8(imut,jmut)

  real(8) :: ci(imut,jmut)
  real(8) :: tmp2m(imut,jmut)
  real(8) :: tmp2m_org(imut,jmut)
  real(8) :: dew2m(imut,jmut)
  real(8) :: sph2m(imut,jmut)
  real(8) :: sph2m_org(imut,jmut)
  real(8) :: slprs(imut,jmut)
  real(8) :: rlhf(imut,jmut)

  real(8) :: work8(imut,jmut)
  real(8) :: workp8(imut,jmut) ! sea level pressure
  real(8) :: workd8(imut,jmut) ! dew point temperature

  real(8) :: alon_era40(imut)
  real(8) :: alat_era40(jmut)
  real(8) :: alat_tmp(jmut)

  integer(4) :: idmon(12)

  integer(4) :: i, j, k, l, m, n

  integer(4) :: nrecord, nhrcount
  integer(4) :: ireco1, ireco2, ireco3, ireco4
  integer(4) :: ireco5, ireco6, ireco7, ireco8
  integer(4) :: mst, med
  integer(4) :: nday, nd, nh

  !------------
  ! low temperature cut-off south of 60S

  real(8), parameter :: avg_c0 = 61.846d0
  real(8), parameter :: avg_c1 =  1.107d0
  real(8), parameter :: amp_c0 = -21.841d0
  real(8), parameter :: amp_c1 = -0.447d0
  real(8), parameter :: phs_c0 = 0.298d0
  real(8) :: rsec
  real(8) :: cosfactor
  real(8) :: tmin
  integer(4) :: current_year_sec, this_year_sec, ileap

  !------------

  real(8) :: lat_fac(jmut)
  real(8),parameter :: tmp_offset = 0.25d0

  !------------

  real(4),parameter :: undef_era40 = -32767.0
  real(4),parameter :: undef_mxe = -9.99e33

  integer(4) :: nyear
  character(128) :: file_in1
  character(128) :: file_out_tmp_year, file_out_sph_year
  character(128) :: file_out_tmp_s1, file_out_tmp_s2, file_out_tmp_e1
  character(128) :: file_out_sph_s1, file_out_sph_s2, file_out_sph_e1

  ! for netCDF

  integer(4), parameter :: nvars = 6, nfiles = 1
  integer(4) :: var(nfiles,nvars), sta(nfiles)
  integer(4) :: iostat
  integer(4) :: start1d(2), range1d(2)
  integer(4) :: start(3), range(3)
  character(128) :: flnin(nfiles)
  integer(4) :: ifiles(nfiles)

  real(8) :: ci_offset(nfiles), ci_scale(nfiles), ci_miss(nfiles)
  real(8) :: t2m_offset(nfiles), t2m_scale(nfiles), t2m_miss(nfiles)
  real(8) :: msl_offset(nfiles), msl_scale(nfiles), msl_miss(nfiles)
  real(8) :: d2m_offset(nfiles), d2m_scale(nfiles), d2m_miss(nfiles)

  !--------------------------------------------------------------------

  real(8), parameter :: ems = 1.00d0
  real(8), parameter :: stfblz = 5.67d-5
  real(8), parameter :: tab = 273.15d0
  real(8), parameter :: sst_freeze = -1.8d0
  real(8), parameter :: sst_bad = 40.0d0

  real(8), parameter :: molw = 18.016d0
  real(8), parameter :: mola = 28.966d0
  real(8), parameter :: epsilon = molw/mola

  ! CORE (LY04)

  !real(8), parameter :: rhoa = 1.22d0 ! air density [kg/m3]
  !real(8), parameter :: q0 = 0.98d0    ! dimensionless factor
  !real(8), parameter :: q1a = 640380.d0 ! [kg/m3]
  !real(8), parameter :: q2a = -5107.4d0 ! [K]
  !real(8), parameter :: q1i = 11637800.d0 ! L-Y p16
  !real(8), parameter :: q2i = -5897.8d0
  !real(8), parameter :: rgas = 287.04d0
  !real(8), parameter :: r0 = 0.6078d0

  ! Gill (1982)

  real(8), parameter :: g1 = 0.7859d0
  real(8), parameter :: g2 = 0.03477d0
  real(8), parameter :: g3 = 0.00412d0

  real(8), parameter :: h1 = 4.5d0
  real(8), parameter :: h2 = 0.0006d0

  real(8), parameter :: i1 = 0.00422d0

  ! temporal variables

  real(8) :: d2m, slp, ice_conc
  real(8) :: qsat, esat

  real(8) :: hl1, hl2, hl3

  real(8), parameter :: sphmin = 2.0d-5 ! minimum of specific humidity

  !--------------------------------------------------------------------

  namelist /nml_era40_org2grads_tmpcalib/ nyear, &
       & file_in1, &
       & file_out_tmp_year, file_out_sph_year, &
       & file_out_tmp_s1, file_out_tmp_s2, file_out_tmp_e1, &
       & file_out_sph_s1, file_out_sph_s2, file_out_sph_e1

  !--------------------------------------------------------------------
  
  open(10,file='namelist.era40_org2grads_tmpcalib')
  read(10,nml=nml_era40_org2grads_tmpcalib)
  close(10)

  !--------------------------------------------------------------------
  ! general settings

  idmon(1:12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  !---------------------------------------------------------------------

  flnin(1)=file_in1

  write(6,*) 'file1: ',trim(flnin(1))

  sta(1:nfiles) = 0

  do n = 1, nfiles

    iostat = nf_open(flnin(n),nf_nowrite,ifiles(n)) 
    if (iostat /= 0) then
      write(6,*) 'nf_open error for file number ',n, ' iostat = ', iostat
      stop
    end if

    iostat = nf_inq_varid(ifiles(n),'ci',var(n,1))
    if (iostat /= 0) then
      write(6,*) 'nf_inq_varid error for variable 1 in file ', n, iostat
      stop
    end if

    iostat = nf_inq_varid(ifiles(n),'t2m',var(n,2))
    if (iostat /= 0) then
      write(6,*) 'nf_inq_varid error for variable 2 in file ', n, iostat
      stop
    end if

    iostat = nf_inq_varid(ifiles(n),'msl',var(n,3))
    if (iostat /= 0) then
      write(6,*) 'nf_inq_varid error for variable 2 in file', n, iostat
      stop
    end if

    iostat = nf_inq_varid(ifiles(n),'d2m',var(n,4))
    if (iostat /= 0) then
      write(6,*) 'nf_inq_varid error for variable 3 in file', n, iostat
      stop
    end if


    iostat = nf_inq_varid(ifiles(n),'longitude',var(n,5))
    if (iostat /= 0) then
      write(6,*) 'nf_inq_varid error for longitude in file ', n, iostat
      stop
    end if

    iostat = nf_inq_varid(ifiles(n),'latitude ',var(n,6))
    if (iostat /= 0) then
      write(6,*) 'nf_inq_varid error for latitude in file ', n, iostat
      stop
    end if

    sta(n) = 0
    iostat = nf_get_att_double(ifiles(n),var(n,1),'add_offset'   ,ci_offset(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,1),'scale_factor' ,ci_scale(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,1),'missing_value',ci_miss(n))
    sta(n) = sta(n) + iostat
    if (sta(n) /= 0) then
      write(6,*) 'nf_inq_varid error for variable 1 in file', n, sta(n)
      stop
    end if

    sta(n) = 0
    iostat = nf_get_att_double(ifiles(n),var(n,2),'add_offset'   ,t2m_offset(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,2),'scale_factor' ,t2m_scale(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,2),'missing_value',t2m_miss(n))
    sta(n) = sta(n) + iostat
    if (sta(n) /= 0) then
      write(6,*) 'nf_inq_varid error for variable 2 in file', n, sta(n)
      stop
    end if

    sta(n) = 0
    iostat = nf_get_att_double(ifiles(n),var(n,3),'add_offset'   ,msl_offset(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,3),'scale_factor' ,msl_scale(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,3),'missing_value',msl_miss(n))
    sta(n) = sta(n) + iostat
    if (sta(n) /= 0) then
      write(6,*) 'nf_inq_varid error for variable 3 in file', n, sta(n)
      stop
    end if

    sta(n) = 0
    iostat = nf_get_att_double(ifiles(n),var(n,4),'add_offset'   ,d2m_offset(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,4),'scale_factor' ,d2m_scale(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,4),'missing_value',d2m_miss(n))
    sta(n) = sta(n) + iostat
    if (sta(n) /= 0) then
      write(6,*) 'nf_inq_varid error for variable 3 in file', n, sta(n)
      stop
    end if

    write(6,*) 'CI add_offset   ', n, ci_offset(n)
    write(6,*) 'CI scale_factor ', n, ci_scale(n)
    write(6,*) 'CI missing_value', n, ci_miss(n)

    write(6,*) 'T2M add_offset   ', n, t2m_offset(n)
    write(6,*) 'T2M scale_factor ', n, t2m_scale(n)
    write(6,*) 'T2M missing_value', n, t2m_miss(n)

    write(6,*) 'MSL add_offset   ', n, msl_offset(n)
    write(6,*) 'MSL scale_factor ', n, msl_scale(n)
    write(6,*) 'MSL missing_value', n, msl_miss(n)

    write(6,*) 'D2M add_offset   ', n, d2m_offset(n)
    write(6,*) 'D2M scale_factor ', n, d2m_scale(n)
    write(6,*) 'D2M missing_value', n, d2m_miss(n)

  end do

  start1d(1:2) = (/ 1, 1 /)
  range1d(1:2) = (/ 480, 1 /)
  iostat = nf_get_vara_double(ifiles(1),var(1,5),start1d,range1d,alon_era40)
  write(6,'(5f10.4)') (alon_era40(i),i=1,imut)

  start1d(1:2) = (/   1, 1 /)
  range1d(1:2) = (/ 241, 1 /)
  iostat = nf_get_vara_double(ifiles(1),var(1,6),start1d,range1d,alat_tmp)

  do j = 1, jmut
    alat_era40(jmut-j+1) = alat_tmp(j)
  end do
  write(6,'(5f10.4)') (alat_era40(j),j=1,jmut)

  lat_fac(1:jmut) = 0.0d0
  do j = 1, jmut
    if ((50.0d0 < alat_era40(j)) .and. (alat_era40(j) < 55.0d0)) then
      lat_fac(j) = (alat_era40(j) - 50.0d0) / 5.0d0
    end if
    if ((55.0d0 <= alat_era40(j)) .and. (alat_era40(j) <= 65.0d0)) then
      lat_fac(j) = 1.0d0
    end if
    if ((65.0d0 < alat_era40(j)) .and. (alat_era40(j) < 70.0d0)) then
      lat_fac(j) = 1.0d0 - (alat_era40(j)-65.0d0) / 5.0d0
    end if
  end do

  ! open output file

  open(mtot1,file=file_out_tmp_year,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'T2M written to ',trim(file_out_tmp_year)
  ireco1 = 0

  open(mtot2,file=file_out_tmp_s1,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'T2M written to ',trim(file_out_tmp_s1)
  ireco2 = 0

  open(mtot3,file=file_out_tmp_s2,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'T2M written to ',trim(file_out_tmp_s2)
  ireco3 = 0

  open(mtot4,file=file_out_tmp_e1,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'T2M written to ',trim(file_out_tmp_e1)
  ireco4 = 0

  open(mtot5,file=file_out_sph_year,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'Q2M written to ',trim(file_out_sph_year)
  ireco5 = 0

  open(mtot6,file=file_out_sph_s1,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'Q2M written to ',trim(file_out_sph_s1)
  ireco6 = 0

  open(mtot7,file=file_out_sph_s2,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'Q2M written to ',trim(file_out_sph_s2)
  ireco7 = 0

  open(mtot8,file=file_out_sph_e1,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'Q2M written to ',trim(file_out_sph_e1)
  ireco8 = 0

  start(1:3) = (/ 1, 1, 1 /)
  range(1:3) = (/ 480, 241, 1 /)

  ci (:,:) = 0.0d0
  tmp2m(:,:) = 0.0d0
  tmp2m_org(:,:) = 0.0d0

  if (mod(nyear,4) == 0) then
    if (mod(nyear,100) == 0) then
      if (mod(nyear,400) == 0) then
        ileap = 1
      else
        ileap = 0
      end if
    else
      ileap = 1
    end if
  else
    ileap = 0
  end if

  this_year_sec = (365 + ileap) * 86400
  current_year_sec = 0

  mst = 1
  med = 12
  nrecord = 0

  do m = mst, med

    if (m == 2) then
      nday = idmon(m) + ileap
    else
      nday = idmon(m)
    end if

    write(6,*) nyear, m, nday

    do nd = 1, nday
      do nh = 1, 4 ! 6-hourly

        rsec = real(current_year_sec,8) / real(this_year_sec,8)
        cosfactor = cos(2.d0*pi*rsec - phs_c0)

        current_year_sec = current_year_sec + 3600 * 6 ! must be added afterwards

        if ((0.0d0 <= rsec) .and. (rsec < 1.0d0)) then
          !write(6,*) ' rsec      = ', rsec
          !write(6,*) ' cosfactor = ', cosfactor
        else
          write(6,*) ' Error in rsec, rsec = ', rsec
          stop
        end if

        nrecord  = nrecord + 1

        start(3) = nrecord
        iostat = nf_get_vara_double(ifiles(1),var(1,1),start,range,work8)
        sta(1) = sta(1) + iostat
        start(3) = nrecord
        iostat = nf_get_vara_double(ifiles(1),var(1,3),start,range,workp8)
        sta(1) = sta(1) + iostat
        start(3) = nrecord
        iostat = nf_get_vara_double(ifiles(1),var(1,4),start,range,workd8)
        sta(1) = sta(1) + iostat

        do j = 1, jmut
          do i = 1, imut
            if (work8(i,j) /= ci_miss(1)) then
              ci(i,jmut-j+1) = work8(i,j) * ci_scale(1) + ci_offset(1)
            else
              ci(i,jmut-j+1) = undef_mxe
            end if
            if ((workp8(i,j) /= msl_miss(1)) .and. (workd8(i,j) /= d2m_miss(1))) then
              slp = (workp8(i,j) * msl_scale(1) + msl_offset(1)) * 1.0d-2
              d2m = (workd8(i,j) * d2m_scale(1) + d2m_offset(1)) - tab
              if (ci(i,jmut-j+1) > 0.55d0) then
                hl1 = (g1 + g2 * d2m) / (1.0d0 + g3 * d2m) + i1 * d2m
                hl2 = 10 ** hl1
                hl3 = 1.0d0 + 1.0d-6 * slp * (h1 + h2 * d2m**2)
                esat = hl2 * hl3 
                qsat = epsilon * esat / (slp - (1.0d0 - epsilon) * esat)
              else
                hl1 = (g1 + g2 * d2m) / (1.0d0 + g3 * d2m)
                hl2 = 10 ** hl1
                hl3 = 1.0d0 + 1.0d-6 * slp * (h1 + h2 * d2m**2)
                esat = hl2 * hl3
                qsat = epsilon * esat / (slp - (1.0d0 - epsilon) * esat)
              end if
              sph2m(i,jmut-j+1) = qsat
              sph2m_org(i,jmut-j+1) = qsat
              slprs(i,jmut-j+1) = slp
            else
              sph2m(i,jmut-j+1) = undef_mxe
              sph2m_org(i,jmut-j+1) = undef_mxe
              slprs(i,jmut-j+1) = undef_mxe
            end if
          end do
        end do

        start(3) = nrecord
        iostat = nf_get_vara_double(ifiles(1),var(1,2),start,range,work8)
        sta(1) = sta(1) + iostat

        do j = 1, jmut
          do i = 1, imut
            if (work8(i,j) /= t2m_miss(1)) then
              tmp2m    (i,jmut-j+1) = work8(i,j) * t2m_scale(1) + t2m_offset(1) - tab
              tmp2m_org(i,jmut-j+1) = work8(i,j) * t2m_scale(1) + t2m_offset(1) - tab
            else
              tmp2m    (i,jmut-j+1) = undef_mxe
              tmp2m_org(i,jmut-j+1) = undef_mxe
            end if
          end do
        end do

        do j = 1, jmut
          do i = 1, imut
            if (ci(i,j) /= undef_mxe) then ! ocean grid
              if (ci(i,j) > 0.55d0) then ! on sea ice
                hl1 = (g1 + g2 * tmp2m_org(i,j)) / (1.0d0 + g3 * tmp2m_org(i,j)) + i1 * tmp2m_org(i,j)
                hl2 = 10 ** hl1
                hl3 = 1.0d0 + 1.0d-6 * slprs(i,j)* (h1 + h2 * tmp2m_org(i,j)**2)
                esat = hl2 * hl3
                qsat = epsilon * esat / (slprs(i,j) - (1.0d0 - epsilon) * esat)
              else
                hl1 = (g1 + g2 * tmp2m_org(i,j)) / (1.0d0 + g3 * tmp2m_org(i,j))
                hl2 = 10 ** hl1
                hl3 = 1.0d0 + 1.0d-6 * slprs(i,j) * (h1 + h2 * tmp2m_org(i,j)**2)
                esat = hl2 * hl3
                qsat = epsilon * esat / (slprs(i,j) - (1.0d0 - epsilon) * esat)
              end if
            else ! land
              if (alat_era40(j) < -60.0d0) then ! ice is assumed
                hl1 = (g1 + g2 * tmp2m_org(i,j)) / (1.0d0 + g3 * tmp2m_org(i,j)) + i1 * tmp2m_org(i,j)
                hl2 = 10 ** hl1
                hl3 = 1.0d0 + 1.0d-6 * slprs(i,j)* (h1 + h2 * tmp2m_org(i,j)**2)
                esat = hl2 * hl3
                qsat = epsilon * esat / (slprs(i,j) - (1.0d0 - epsilon) * esat)
              else ! same as water
                hl1 = (g1 + g2 * tmp2m_org(i,j)) / (1.0d0 + g3 * tmp2m_org(i,j))
                hl2 = 10 ** hl1
                hl3 = 1.0d0 + 1.0d-6 * slprs(i,j) * (h1 + h2 * tmp2m_org(i,j)**2)
                esat = hl2 * hl3
                qsat = epsilon * esat / (slprs(i,j) - (1.0d0 - epsilon) * esat)
              end if
            end if
            rlhf(i,j) = sph2m_org(i,j) * (1.0d0 - qsat) / (qsat * (1.0d0 - sph2m_org(i,j)))
          end do
        end do

        ! temperature adjustment

        do j = 1, jmut
          if (alat_era40(j) < -60.d0) then
            tmin = (avg_c0 + avg_c1 * alat_era40(j)) + (amp_c0 + amp_c1 * alat_era40(j)) * cosfactor
            !write(6,*) tmin
            do i = 1, imut
              if (tmp2m(i,j) /= undef_mxe) then
                tmp2m(i,j) = max(tmp2m(i,j),tmin)
              end if
            end do
          end if
        end do

        do j = 1, jmut
          !write(6,*) lat_fac(j)
          do i = 1, imut
            if (tmp2m(i,j) /= undef_mxe) then
              tmp2m(i,j) = tmp2m(i,j) + lat_fac(j) * tmp_offset
            end if
          end do
        end do

        ! specific humidity is made consistent with temperature

        do j = 1, jmut
          do i = 1, imut
            if (tmp2m(i,j) /= tmp2m_org(i,j)) then ! if cut-off applied
              !if (alat_era40(j) > 70.0d0) then
              !  write(6,*) alat_era40(j), tmp2m(i,j), tmp2m_org(i,j)
              !end if
              if (ci(i,j) /= undef_mxe) then
                if (ci(i,j) > 0.55d0) then
                  hl1 = (g1 + g2 * tmp2m(i,j)) / (1.0d0 + g3 * tmp2m(i,j)) + i1 * tmp2m(i,j)
                  hl2 = 10 ** hl1
                  hl3 = 1.0d0 + 1.0d-6 * slprs(i,j)* (h1 + h2 * tmp2m(i,j)**2)
                  esat = hl2 * hl3 
                  qsat = epsilon * esat / (slprs(i,j) - (1.0d0 - epsilon) * esat)
                else
                  hl1 = (g1 + g2 * tmp2m(i,j)) / (1.0d0 + g3 * tmp2m(i,j))
                  hl2 = 10 ** hl1
                  hl3 = 1.0d0 + 1.0d-6 * slprs(i,j) * (h1 + h2 * tmp2m(i,j)**2)
                  esat = hl2 * hl3
                  qsat = epsilon * esat / (slprs(i,j) - (1.0d0 - epsilon) * esat)
                end if
              else ! land
                if (alat_era40(j) < -60.0d0) then ! ice is assumed
                  hl1 = (g1 + g2 * tmp2m(i,j)) / (1.0d0 + g3 * tmp2m(i,j)) + i1 * tmp2m(i,j)
                  hl2 = 10 ** hl1
                  hl3 = 1.0d0 + 1.0d-6 * slprs(i,j)* (h1 + h2 * tmp2m(i,j)**2)
                  esat = hl2 * hl3 
                  qsat = epsilon * esat / (slprs(i,j) - (1.0d0 - epsilon) * esat)
                else ! same as water
                  hl1 = (g1 + g2 * tmp2m(i,j)) / (1.0d0 + g3 * tmp2m(i,j))
                  hl2 = 10 ** hl1
                  hl3 = 1.0d0 + 1.0d-6 * slprs(i,j) * (h1 + h2 * tmp2m(i,j)**2)
                  esat = hl2 * hl3
                  qsat = epsilon * esat / (slprs(i,j) - (1.0d0 - epsilon) * esat)
                end if
              end if
              hl1 = 1.0d0 - qsat * (1.0d0 - rlhf(i,j))
              hl2 = qsat * rlhf(i,j)
              sph2m(i,j) = max(hl2 / hl1, sphmin)
            end if
          end do
        end do

        do j = 1, jmut
          do i = 1, imut
            tmp2m(i,j) = tmp2m(i,j) + tab
          end do
        end do

        ireco1 = ireco1 + 1
        ireco5 = ireco5 + 1

        write(6,*) ' org ', nrecord, ' grads ', ireco1
        write(mtot1,rec=ireco1) real(tmp2m(1:imut,1:jmut),4)
        write(mtot5,rec=ireco5) real(sph2m(1:imut,1:jmut),4)

        if ((m == 1) .and. (nd == 1) .and. (nh == 1)) then
          write(mtot2,rec=1) real(tmp2m(1:imut,1:jmut),4)
          write(6,*) ' Closing ', trim(file_out_tmp_s1)
          close(mtot2)
          write(mtot6,rec=1) real(sph2m(1:imut,1:jmut),4)
          write(6,*) ' Closing ', trim(file_out_sph_s1)
          close(mtot6)
        end if

        if ((m == 1) .and. (nd == 1) .and. (nh == 2)) then
          write(mtot3,rec=1) real(tmp2m(1:imut,1:jmut),4)
          write(6,*) ' Closing ', trim(file_out_tmp_s2)
          close(mtot3)
          write(mtot7,rec=1) real(sph2m(1:imut,1:jmut),4)
          write(6,*) ' Closing ', trim(file_out_sph_s2)
          close(mtot7)
        end if

        if ((m == 12) .and. (nd == 31) .and. (nh == 4)) then
          write(mtot4,rec=1) real(tmp2m(1:imut,1:jmut),4)
          write(6,*) ' Closing ', trim(file_out_tmp_e1)
          close(mtot4)
          write(mtot8,rec=1) real(sph2m(1:imut,1:jmut),4)
          write(6,*) ' Closing ', trim(file_out_sph_e1)
          close(mtot8)
        end if

      end do
    end do
  end do

  close(mtot1)

end program read_netcdf_output_grads
