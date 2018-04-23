! -*-F90-*-
!==================================================================
program read_netcdf_output_grads_monthly_spfh
  !----------------------------------------------------------------
  ! Information:
  !   Create direct access file (GrADS) of
  !     the ERA-Interim surface analysis data
  !----------------------------------------------------------------

  implicit none

  include '/usr/local/netcdf/include/netcdf.inc'

  integer(4), parameter :: imut = 480, jmut = 241
  integer(4), parameter :: mtot1 = 91, mtot2 = 92

  real(8) :: ice(imut,jmut,12)
  real(8) :: q2m(imut,jmut,12)

  integer(4) :: num_valid_ice(imut,jmut,12)
  integer(4) :: num_valid_q2m(imut,jmut,12)
  integer(4) :: ho4(imut,jmut)

  real(8) :: work8(imut,jmut)
  real(8) :: workp8(imut,jmut) ! sea level pressure
  real(8) :: workd8(imut,jmut) ! dew point temperature

  integer(4) :: idmon(12)

  character(128) :: flot1, flot2

  integer(4) :: i, j, k, l, m, n

  integer(4) :: nrecord, nhrcount, nhinc
  integer(4) :: ireco1, ireco2
  integer(4) :: mst, med
  integer(4) :: nday, nd, nh

  real(4),parameter :: undef_erai = 32766.0
  real(4),parameter :: undef_mxe = -9.99e33

  integer(4) :: nyear
  character(128) :: var_name
  character(128) :: file_in1, file_in2
  character(128) :: file_mask
  character(128) :: file_out_base, file_out

  logical :: l_mask_out

  ! for netCDF

  integer(4), parameter :: nvars = 3, nfiles = 2
  integer(4) :: var(nfiles,nvars), sta(nfiles)
  integer(4) :: iostat
  integer(4) :: start(3), range(3)
  character(128) :: flnin(nfiles)
  integer(4) :: ifiles(nfiles)

  real(8) :: ice_offset(nfiles), ice_scale(nfiles), ice_miss(nfiles)
  real(8) :: msl_offset(nfiles), msl_scale(nfiles), msl_miss(nfiles)
  real(8) :: d2m_offset(nfiles), d2m_scale(nfiles), d2m_miss(nfiles)
  real(8) :: hl1, hl2, hl3

  integer(4) :: nfiles_tmp

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

  !--------------------------------------------------------------------

  namelist /nml_erai_spfh_org2mon/ nyear, &
       & file_in1, file_in2, file_out_base, &
       & l_mask_out, file_mask

  !--------------------------------------------------------------------
  
  open (10,file='namelist.erai_spfh_org2mon')
  read (10,nml=nml_erai_spfh_org2mon)
  close(10)

  !--------------------------------------------------------------------
  ! general settings

  idmon(1:12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  !---------------------------------------------------------------------

  flnin(1)=file_in1
  flnin(2)=file_in2

  start(1:3) = (/ 1, 1, 1 /)
  range(1:3) = (/ 480, 241, 1 /)


  write(6,*) 'file1: ',trim(flnin(1))
  write(6,*) 'file2: ',trim(flnin(2))

  sta(1:nfiles) = 0

  if (nyear == 2015) then
    nfiles_tmp = 1
  else
    nfiles_tmp = nfiles
  end if

  do n = 1, nfiles_tmp

    iostat = nf_open(flnin(n),nf_nowrite,ifiles(n)) 
    if (iostat /= 0) then
      write(6,*) 'nf_open error for file number ',n, ' iostat = ', iostat
      stop
    end if

    iostat = nf_inq_varid(ifiles(n),'ci',var(n,1))
    if (iostat /= 0) then
      write(6,*) 'nf_inq_varid error for variable 1 in file', n, iostat
      stop
    end if

    iostat = nf_inq_varid(ifiles(n),'msl',var(n,2))
    if (iostat /= 0) then
      write(6,*) 'nf_inq_varid error for variable 2 in file', n, iostat
      stop
    end if

    iostat = nf_inq_varid(ifiles(n),'d2m',var(n,3))
    if (iostat /= 0) then
      write(6,*) 'nf_inq_varid error for variable 3 in file', n, iostat
      stop
    end if

    sta(n) = 0
    iostat = nf_get_att_double(ifiles(n),var(n,1),'add_offset'   ,ice_offset(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,1),'scale_factor' ,ice_scale(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,1),'missing_value',ice_miss(n))
    sta(n) = sta(n) + iostat
    if (sta(n) /= 0) then
      write(6,*) 'nf_inq_varid error for variable 1 in file', n, sta(n)
      stop
    end if

    sta(n) = 0
    iostat = nf_get_att_double(ifiles(n),var(n,2),'add_offset'   ,msl_offset(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,2),'scale_factor' ,msl_scale(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,2),'missing_value',msl_miss(n))
    sta(n) = sta(n) + iostat
    if (sta(n) /= 0) then
      write(6,*) 'nf_inq_varid error for variable 2 in file', n, sta(n)
      stop
    end if

    sta(n) = 0
    iostat = nf_get_att_double(ifiles(n),var(n,3),'add_offset'   ,d2m_offset(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,3),'scale_factor' ,d2m_scale(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,3),'missing_value',d2m_miss(n))
    sta(n) = sta(n) + iostat
    if (sta(n) /= 0) then
      write(6,*) 'nf_inq_varid error for variable 3 in file', n, sta(n)
      stop
    end if

    write(6,*) 'ICE add_offset   ', n, ice_offset(n)
    write(6,*) 'ICE scale_factor ', n, ice_scale(n)
    write(6,*) 'ICE missing_value', n, ice_miss(n)

    write(6,*) 'MSL add_offset   ', n, msl_offset(n)
    write(6,*) 'MSL scale_factor ', n, msl_scale(n)
    write(6,*) 'MSL missing_value', n, msl_miss(n)

    write(6,*) 'D2M add_offset   ', n, d2m_offset(n)
    write(6,*) 'D2M scale_factor ', n, d2m_scale(n)
    write(6,*) 'D2M missing_value', n, d2m_miss(n)

  end do

  ice(:,:,:) = 0.0d0
  num_valid_ice(:,:,:) = 0

  q2m(:,:,:) = 0.0d0
  num_valid_q2m(:,:,:) = 0

  mst = 1
  med = 12
  nrecord = 0

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

    write(6,*) nyear, m, nday

    do nd = 1, nday

      do nh = 1, 4 ! 6-hourly

        nrecord  = nrecord + 1

        if ((nd == 1) .and. (nh == 1)) then
          nhinc = 3
        else
          nhinc = 6
        end if

        start(3) = nrecord
        iostat = nf_get_vara_double(ifiles(1),var(1,1),start,range,work8)
        sta(1) = sta(1) + iostat

        start(3) = nrecord
        iostat = nf_get_vara_double(ifiles(1),var(1,2),start,range,workp8)
        sta(1) = sta(1) + iostat

        start(3) = nrecord
        iostat = nf_get_vara_double(ifiles(1),var(1,3),start,range,workd8)
        sta(1) = sta(1) + iostat

        do j = 1, jmut
          do i = 1, imut

            if (work8(i,j) /= ice_miss(1)) then
              ice_conc = work8(i,j) * ice_scale(1) + ice_offset(1)
              num_valid_ice(i,jmut-j+1,m) = num_valid_ice(i,jmut-j+1,m) + nhinc
              ice(i,jmut-j+1,m) = ice(i,jmut-j+1,m) + ice_conc * real(nhinc,8)
              if ((nd == 1) .and. (nh == 1) .and. (m > 1)) then
                num_valid_ice(i,jmut-j+1,m-1) = num_valid_ice(i,jmut-j+1,m-1) + nhinc
                ice(i,jmut-j+1,m-1) = ice(i,jmut-j+1,m-1) + ice_conc * real(nhinc,8)
              end if
            else
              ice_conc = 0.0d0
            end if

            if ((workp8(i,j) /= msl_miss(1)) .and. (workd8(i,j) /= d2m_miss(1))) then
              num_valid_q2m(i,jmut-j+1,m) = num_valid_q2m(i,jmut-j+1,m) + nhinc
              slp = (workp8(i,j) * msl_scale(1) + msl_offset(1)) * 1.0d-2
              d2m = (workd8(i,j) * d2m_scale(1) + d2m_offset(1)) - tab

              if (ice_conc > 0.55d0) then
                hl1 = (g1 + g2 * d2m) / (1.0d0 + g3 * d2m) + i1 * d2m
                hl2 = 10 ** hl1
                hl3 = 1.0d0 + 1.0d-6 * slp * (h1 + h2 * d2m**2)
                esat = hl2 * hl3 
                qsat = epsilon * esat / (slp - (1.0d0 - epsilon) * esat)
                !qsat = q1i * exp(q2i / (satc(i,j) + tab)) / rhoa
              else
                hl1 = (g1 + g2 * d2m) / (1.0d0 + g3 * d2m)
                hl2 = 10 ** hl1
                hl3 = 1.0d0 + 1.0d-6 * slp * (h1 + h2 * d2m**2)
                esat = hl2 * hl3
                qsat = epsilon * esat / (slp - (1.0d0 - epsilon) * esat)
                !qsat = q0 * q1a * exp(q2a / (satc(i,j) + tab)) / rhoa
              end if

              q2m(i,jmut-j+1,m) = q2m(i,jmut-j+1,m) + qsat * real(nhinc,8)
              if ((nd == 1) .and. (nh == 1) .and. (m > 1)) then
                num_valid_q2m(i,jmut-j+1,m-1) = num_valid_q2m(i,jmut-j+1,m-1) + nhinc
                q2m(i,jmut-j+1,m-1) = q2m(i,jmut-j+1,m-1) + qsat * real(nhinc,8)
              end if
            end if

          end do
        end do

      end do
    end do
  end do

  !------------------------------------------------------------------------------
  ! 1st of January of the next year

  if (nyear == 2015) then

    m = 12
    nrecord = 365 * 4
    nhinc = 3

    start(3) = nrecord
    iostat = nf_get_vara_double(ifiles(1),var(1,1),start,range,work8)
    sta(1) = sta(1) + iostat
    
    start(3) = nrecord
    iostat = nf_get_vara_double(ifiles(1),var(1,2),start,range,workp8)
    sta(1) = sta(1) + iostat

    start(3) = nrecord
    iostat = nf_get_vara_double(ifiles(1),var(1,3),start,range,workd8)
    sta(1) = sta(1) + iostat

    do j = 1, jmut
      do i = 1, imut
        if (work8(i,j) /= ice_miss(1)) then
          num_valid_ice(i,jmut-j+1,m) = num_valid_ice(i,jmut-j+1,m) + nhinc
          ice_conc = work8(i,j) * ice_scale(1) + ice_offset(1) 
          ice(i,jmut-j+1,m) = ice(i,jmut-j+1,m) + ice_conc * real(nhinc,8)
        else
          ice_conc = 0.0d0
        end if
        if ((workp8(i,j) /= msl_miss(2)) .and. (workd8(i,j) /= d2m_miss(1))) then
          num_valid_q2m(i,jmut-j+1,m) = num_valid_q2m(i,jmut-j+1,m) + nhinc
          slp = (workp8(i,j) * msl_scale(1) + msl_offset(1)) * 1.d-2
          d2m = (workd8(i,j) * d2m_scale(1) + d2m_offset(1)) - tab
          if (ice_conc > 0.55d0) then ! needs check
            hl1 = (g1 + g2 * d2m) / (1.0d0 + g3 * d2m) + i1 * d2m
            hl2 = 10 ** hl1
            hl3 = 1.0d0 + 1.0d-6 * slp * (h1 + h2 * d2m**2)
            esat = hl2 * hl3 
            qsat = epsilon * esat / (slp - (1.0d0 - epsilon) * esat)
            !qsat = q1i * exp(q2i / (satc(i,j) + tab)) / rhoa
          else
            hl1 = (g1 + g2 * d2m) / (1.0d0 + g3 * d2m)
            hl2 = 10 ** hl1
            hl3 = 1.0d0 + 1.0d-6 * slp * (h1 + h2 * d2m**2)
            esat = hl2 * hl3
            qsat = epsilon * esat / (slp - (1.0d0 - epsilon) * esat)
            !qsat = q0 * q1a * exp(q2a / (satc(i,j) + tab)) / rhoa
          end if
          q2m(i,jmut-j+1,m) = q2m(i,jmut-j+1,m) + qsat * real(nhinc,8)
        end if
      end do
    end do

  else

    m = 12
    nrecord = 1
    nhinc = 3

    start(3) = nrecord
    iostat = nf_get_vara_double(ifiles(2),var(2,1),start,range,work8)
    sta(2) = sta(2) + iostat

    start(3) = nrecord
    iostat = nf_get_vara_double(ifiles(2),var(2,2),start,range,workp8)
    sta(2) = sta(2) + iostat

    start(3) = nrecord
    iostat = nf_get_vara_double(ifiles(2),var(2,3),start,range,workd8)
    sta(2) = sta(2) + iostat

    do j = 1, jmut
      do i = 1, imut
        if (work8(i,j) /= ice_miss(2)) then
          num_valid_ice(i,jmut-j+1,m) = num_valid_ice(i,jmut-j+1,m) + nhinc
          ice_conc = work8(i,j) * ice_scale(2) + ice_offset(2) 
          ice(i,jmut-j+1,m) = ice(i,jmut-j+1,m) + ice_conc * real(nhinc,8)
        else
          ice_conc = 0.0d0
        end if
        if ((workp8(i,j) /= msl_miss(2)) .and. (workd8(i,j) /= d2m_miss(2))) then
          num_valid_q2m(i,jmut-j+1,m) = num_valid_q2m(i,jmut-j+1,m) + nhinc
          slp = (workp8(i,j) * msl_scale(2) + msl_offset(2)) * 1.d-2
          d2m = (workd8(i,j) * d2m_scale(2) + d2m_offset(2)) - tab
          if (ice_conc > 0.55d0) then ! needs check
            hl1 = (g1 + g2 * d2m) / (1.0d0 + g3 * d2m) + i1 * d2m
            hl2 = 10 ** hl1
            hl3 = 1.0d0 + 1.0d-6 * slp * (h1 + h2 * d2m**2)
            esat = hl2 * hl3 
            qsat = epsilon * esat / (slp - (1.0d0 - epsilon) * esat)
            !qsat = q1i * exp(q2i / (satc(i,j) + tab)) / rhoa
          else
            hl1 = (g1 + g2 * d2m) / (1.0d0 + g3 * d2m)
            hl2 = 10 ** hl1
            hl3 = 1.0d0 + 1.0d-6 * slp * (h1 + h2 * d2m**2)
            esat = hl2 * hl3
            qsat = epsilon * esat / (slp - (1.0d0 - epsilon) * esat)
            !qsat = q0 * q1a * exp(q2a / (satc(i,j) + tab)) / rhoa
          end if
          q2m(i,jmut-j+1,m) = q2m(i,jmut-j+1,m) + qsat * real(nhinc,8)
        end if
      end do
    end do
  end if

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

    do j = 1, jmut
      do i = 1, imut
        if (num_valid_ice(i,j,m) == 24 * nday) then
          ice(i,j,m) = ice(i,j,m) / real(num_valid_ice(i,j,m),8)
        else
          ice(i,j,m) = real(undef_mxe,8)
        end if
        if (num_valid_q2m(i,j,m) == 24 * nday) then
          q2m(i,j,m) = q2m(i,j,m) / real(num_valid_q2m(i,j,m),8)
        else
          q2m(i,j,m) = real(undef_mxe,8)
        end if
      end do
    end do

    write(6,'(1a,f12.5,2i6)') &
         & 'ice ', ice(400,210,m), num_valid_ice(400,210,m), 24 * nday
    write(6,'(1a,f12.5,2i6)') &
         & 'q2m ', q2m(400,210,m), num_valid_q2m(400,210,m), 24 * nday

    ! open output file

    write(file_out,'(1a,i4.4,i2.2)') trim(file_out_base),nyear,m
    open(mtot1,file=file_out,form='unformatted',access='direct',recl=4*imut*jmut)
    write(6,*) 'Specific humidty written to ',trim(file_out)
    ireco1 = 0
    ireco1 = ireco1 + 1
    write(mtot1,rec=ireco1) real(q2m(1:imut,1:jmut,m),4)
    close(mtot1)

  end do

  do n = 1, nfiles_tmp
    iostat = nf_close(ifiles(n))
    if (iostat /= 0) then
      write(6,*) 'nf_close error for file number ',n, ifiles(n), iostat
    end if
  end do

  if (l_mask_out) then
    do j = 1, jmut
      do i = 1, imut
        if (num_valid_ice(i,j,1) == 24 * 31) then
          ho4(i,j) = 1
        else
          ho4(i,j) = 0
        end if
      end do
    end do

    open(mtot1,file=file_mask,form='unformatted')
    write(6,*) 'topography written to ',trim(file_mask)
    write(mtot1) ho4,ho4
    close(mtot1)

  end if

end program read_netcdf_output_grads_monthly_spfh
