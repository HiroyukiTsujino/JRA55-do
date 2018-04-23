! -*-F90-*-
!==================================================================
program read_netcdf_output_grads_monthly
  !----------------------------------------------------------------
  ! Information:
  !   Create direct access file (GrADS) of
  !     the ERA-Interim surface analysis data
  !----------------------------------------------------------------

  implicit none

  include '/usr/local/netcdf/include/netcdf.inc'

  integer(4), parameter :: imut = 480, jmut = 241
  integer(4), parameter :: mtot1 = 91, mtot2 = 92

  real(8) :: un10m (imut,jmut,12)
  real(8) :: vn10m (imut,jmut,12)
  real(8) :: wn10m (imut,jmut,12)
  real(8) :: tmp10m(imut,jmut,12)
  real(8) :: sph10m(imut,jmut,12)

  real(8) :: un10m_tmp (imut,jmut)
  real(8) :: vn10m_tmp (imut,jmut)
  real(8) :: wn10m_tmp (imut,jmut)
  real(8) :: tmp10m_tmp(imut,jmut)
  real(8) :: sph10m_tmp(imut,jmut)

  real(8) :: sst_in(imut,jmut)
  real(8) :: ice_in(imut,jmut)

  integer(4) :: num_valid_wind(imut,jmut,12)

  real(8) :: aexl(imut,jmut)
  real(8) :: wind_mask(imut,jmut)
  real(8) :: tmp2m(imut,jmut)
  real(8) :: dew2m(imut,jmut)
  real(8) :: sph2m(imut,jmut)
  real(8) :: slprs(imut,jmut)

  real(8) :: u10m(imut,jmut)
  real(8) :: v10m(imut,jmut)
  real(8) :: w10m(imut,jmut)

  real(8) :: sst(imut,jmut)
  real(8) :: ice(imut,jmut)
  real(8) :: skt(imut,jmut)

  integer(4) :: idmon(12)

  character(128) :: flot1, flot2

  integer(4) :: i, j, k, l, m, n

  integer(4) :: nrecord, nhrcount, nhinc
  integer(4) :: ireco1, ireco2
  integer(4) :: mst, med
  integer(4) :: nday, nd, nh

  real(4),parameter :: undef_mxe = -9.99e33

  integer(4) :: nyear

  character(128) :: file_in1a, file_in1b
  character(128) :: file_in2a, file_in2b
  character(128) :: file_mask

  character(128) :: file_out_base
  character(128) :: file_u10m, file_v10m, file_w10m

  ! for netCDF

  integer(4), parameter :: nvars1 = 7, nfiles1 = 2
  integer(4), parameter :: nvars2 = 1, nfiles2 = 2

  character(16) :: var_name1(nvars1)
  character(16) :: var_name2(nvars2)

  integer(4) :: var1(nfiles1,nvars1), sta1(nfiles1)
  integer(4) :: var2(nfiles2,nvars2), sta2(nfiles2)

  integer(4) :: iostat1, iostat2
  integer(4) :: start1(3), range1(3)
  integer(4) :: start2(3), range2(3)

  character(128) :: flnin1(nfiles1)
  character(128) :: flnin2(nfiles2)
  integer(4) :: ifiles1(nfiles1)
  integer(4) :: ifiles2(nfiles2)

  real(8) :: offset1(nvars1), scale1(nvars1), miss1(nvars1)
  real(8) :: offset2(nvars1), scale2(nvars1), miss2(nvars1)

  real(8) :: sst_offset (nfiles1), sst_scale (nfiles1), sst_miss (nfiles1)
  real(8) :: ice_offset (nfiles1), ice_scale (nfiles1), ice_miss (nfiles1)
  real(8) :: msl_offset (nfiles1), msl_scale (nfiles1), msl_miss (nfiles1)
  real(8) :: t2m_offset (nfiles1), t2m_scale (nfiles1), t2m_miss (nfiles1)
  real(8) :: d2m_offset (nfiles1), d2m_scale (nfiles1), d2m_miss (nfiles1)
  real(8) :: u10m_offset(nfiles1), u10m_scale(nfiles1), u10m_miss(nfiles1)
  real(8) :: v10m_offset(nfiles1), v10m_scale(nfiles1), v10m_miss(nfiles1)

  real(8) :: skt_offset(nfiles2), skt_scale(nfiles2), skt_miss(nfiles2)

  real(8) :: work1(imut,jmut,nvars1)
  real(8) :: work2(imut,jmut,nvars2)

  real(8) :: hl1, hl2, hl3

  integer(4) :: nfiles1_tmp, nfiles2_tmp

  !--------------------------------------------------------------------

  real(8), parameter :: ems = 1.00d0
  real(8), parameter :: stfblz = 5.67d-5
  real(8), parameter :: tab = 273.15d0
  real(8), parameter :: sst_freeze = -1.8d0
  real(8), parameter :: sst_bad = 40.0d0

  real(8), parameter :: sphmin = 2.0d-5 ! minimum of specific humidity

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

  real(8) :: d2m_tmp, slp_tmp
  real(8) :: qsat, esat

  real(8) :: altu, altt, altq, alt_target

  !--------------------------------------------------------------------

  namelist /nml_erai_neutral_org2mon/ &
       & nyear,                       &
       & file_in1a, file_in1b,        &
       & file_in2a, file_in2b,        &
       & file_out_base

  !--------------------------------------------------------------------
  
  open (10,file='namelist.erai_neutral_org2mon')
  read (10,nml=nml_erai_neutral_org2mon)
  close(10)

  !--------------------------------------------------------------------
  ! general settings

  idmon(1:12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  var_name1(1:nvars1) = (/ "ci ", "sst ", "msl ", "u10 ", "v10 ", "t2m ", "d2m " /)
  var_name2(1:nvars2) = (/ "skt " /)

  altu = 10.d0
  altt = 2.0d0
  altq = 2.0d0
  alt_target = 10.d0

  !---------------------------------------------------------------------

  flnin1(1)=file_in1a
  flnin1(2)=file_in1b
  flnin2(1)=file_in2a
  flnin2(2)=file_in2b

  start1(1:3) = (/ 1, 1, 1 /)
  range1(1:3) = (/ 480, 241, 1 /)

  start2(1:3) = (/ 1, 1, 1 /)
  range2(1:3) = (/ 480, 241, 1 /)

  write(6,*) 'file1a: ',trim(flnin1(1))
  write(6,*) 'file1b: ',trim(flnin1(2))

  write(6,*) 'file2a: ',trim(flnin2(1))
  write(6,*) 'file2b: ',trim(flnin2(2))

  sta1(1:nfiles1) = 0
  sta2(1:nfiles2) = 0

  if (nyear == 2015) then
    nfiles1_tmp = 1
    nfiles2_tmp = 1
  else
    nfiles1_tmp = nfiles1
    nfiles2_tmp = nfiles2
  end if

  !--------------------------------------------------------------------------
  ! file 1

  do n = 1, nfiles1_tmp

    iostat1 = nf_open(flnin1(n),nf_nowrite,ifiles1(n)) 
    if (iostat1 /= 0) then
      write(6,*) 'nf_open error for file number ',n, ' iostat1 = ', iostat1
      stop
    end if

    do m = 1, nvars1
      iostat1 = nf_inq_varid(ifiles1(n),trim(var_name1(m)),var1(n,m))
      if (iostat1 /= 0) then
        write(6,*) 'nf_inq_varid error for variable',m, trim(var_name1(m)),' in file', n, iostat1
        stop
      end if
    end do

    do m = 1, nvars1
      sta1(n) = 0
      iostat1 = nf_get_att_double(ifiles1(n),var1(n,m),'add_offset'   ,offset1(m))
      sta1(n) = sta1(n) + iostat1
      iostat1 = nf_get_att_double(ifiles1(n),var1(n,m),'scale_factor' ,scale1 (m))
      sta1(n) = sta1(n) + iostat1
      iostat1 = nf_get_att_double(ifiles1(n),var1(n,m),'missing_value',miss1  (m))
      sta1(n) = sta1(n) + iostat1
      if (sta1(n) /= 0) then
        write(6,*) 'nf_inq_varid error for variable ',m,' in file', n, sta1(n)
        stop
      end if
    end do

    do m = 1, nvars1
      write(6,*) trim(var_name1(m)), ' add_offset   ', n, offset1(m)
      write(6,*) trim(var_name1(m)), ' scale_factor ', n, scale1 (m)
      write(6,*) trim(var_name1(m)), ' missing_value', n, miss1  (m)
    end do

    ice_offset(n) = offset1(1)
    ice_scale (n) = scale1 (1)
    ice_miss  (n) = miss1  (1)

    sst_offset(n) = offset1(2)
    sst_scale (n) = scale1 (2)
    sst_miss  (n) = miss1  (2)

    msl_offset(n) = offset1(3)
    msl_scale (n) = scale1 (3)
    msl_miss  (n) = miss1  (3)

    u10m_offset(n) = offset1(4)
    u10m_scale (n) = scale1 (4)
    u10m_miss  (n) = miss1  (4)

    v10m_offset(n) = offset1(5)
    v10m_scale (n) = scale1 (5)
    v10m_miss  (n) = miss1  (5)

    t2m_offset(n) = offset1(6)
    t2m_scale (n) = scale1 (6)
    t2m_miss  (n) = miss1  (6)

    d2m_offset(n) = offset1(7)
    d2m_scale (n) = scale1 (7)
    d2m_miss  (n) = miss1  (7)

  end do

  ! file 2

  do n = 1, nfiles2_tmp

    iostat2 = nf_open(flnin2(n),nf_nowrite,ifiles2(n)) 
    if (iostat2 /= 0) then
      write(6,*) 'nf_open error for file number ',n, ' iostat2 = ', iostat2
      stop
    end if

    do m = 1, nvars2
      iostat2 = nf_inq_varid(ifiles2(n),trim(var_name2(m)),var2(n,m))
      if (iostat2 /= 0) then
        write(6,*) 'nf_inq_varid error for variable',m,' in file', n, iostat2
        stop
      end if
    end do

    do m = 1, nvars2
      sta2(n) = 0
      iostat2 = nf_get_att_double(ifiles2(n),var2(n,m),'add_offset'   ,offset2(m))
      sta2(n) = sta2(n) + iostat2
      iostat2 = nf_get_att_double(ifiles2(n),var2(n,m),'scale_factor' ,scale2(m))
      sta2(n) = sta2(n) + iostat2
      iostat2 = nf_get_att_double(ifiles2(n),var2(n,m),'missing_value',miss2(m))
      sta2(n) = sta2(n) + iostat2
      if (sta2(n) /= 0) then
        write(6,*) 'nf_inq_varid error for variable',m,' in file', n, sta2(n)
        stop
      end if
    end do

    do m = 1, nvars2
      write(6,*) trim(var_name2(m)), ' add_offset   ', n, offset2(m)
      write(6,*) trim(var_name2(m)), ' scale_factor ', n, scale2 (m)
      write(6,*) trim(var_name2(m)), ' missing_value', n, miss2  (m)
    end do

    skt_offset(n) = offset2(1)
    skt_scale (n) = scale2 (1)
    skt_miss  (n) = miss2  (1)

  end do

  !-------------------------------------------------

  un10m(:,:,:) = 0.0d0
  vn10m(:,:,:) = 0.0d0
  wn10m(:,:,:) = 0.0d0
  aexl(:,:) = 0.0d0
  num_valid_wind(:,:,:) = 0

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

        start1(3) = nrecord

        iostat1 = nf_get_vara_double(ifiles1(1),var1(1,1),start1,range1,work1(1,1,1))
        sta1(1) = sta1(1) + iostat1

        iostat1 = nf_get_vara_double(ifiles1(1),var1(1,2),start1,range1,work1(1,1,2))
        sta1(1) = sta1(1) + iostat1

        iostat1 = nf_get_vara_double(ifiles1(1),var1(1,3),start1,range1,work1(1,1,3))
        sta1(1) = sta1(1) + iostat1

        iostat1 = nf_get_vara_double(ifiles1(1),var1(1,4),start1,range1,work1(1,1,4))
        sta1(1) = sta1(1) + iostat1

        iostat1 = nf_get_vara_double(ifiles1(1),var1(1,5),start1,range1,work1(1,1,5))
        sta1(1) = sta1(1) + iostat1

        iostat1 = nf_get_vara_double(ifiles1(1),var1(1,6),start1,range1,work1(1,1,6))
        sta1(1) = sta1(1) + iostat1

        iostat1 = nf_get_vara_double(ifiles1(1),var1(1,7),start1,range1,work1(1,1,7))
        sta1(1) = sta1(1) + iostat1

        start2(3) = nrecord

        iostat2 = nf_get_vara_double(ifiles2(1),var2(1,1),start2,range2,work2(1,1,1))
        sta2(1) = sta2(1) + iostat2

        wind_mask(:,:) = 0.0d0

        do j = 1, jmut
          do i = 1, imut

            if (work1(i,jmut-j+1,1) /= ice_miss(1)) then
              ice(i,j) = work1(i,jmut-j+1,1) * ice_scale(1) + ice_offset(1)
            else
              ice(i,j) = 0.0d0
            end if

            if (work1(i,jmut-j+1,2) /= sst_miss(1)) then
              sst(i,j) = work1(i,jmut-j+1,2) * sst_scale(1) + sst_offset(1)
              aexl(i,j) = 1.0d0
            else
              sst(i,j) = 0.0d0
            end if

            if (work1(i,jmut-j+1,3) /= msl_miss(1)) then
              slprs(i,j) = work1(i,jmut-j+1,3) * msl_scale(1) + msl_offset(1)
            else
              slprs(i,j) = 0.0d0
            end if

            if ((work1(i,jmut-j+1,4) /= u10m_miss(1)) .and. (work1(i,jmut-j+1,5) /= v10m_miss(1))) then
              u10m(i,j) = work1(i,jmut-j+1,4) * u10m_scale(1) + u10m_offset(1)
              v10m(i,j) = work1(i,jmut-j+1,5) * v10m_scale(1) + v10m_offset(1)
              w10m(i,j) = sqrt(u10m(i,j)**2 + v10m(i,j)**2)
              wind_mask(i,j) = 1.0d0
            else
              u10m(i,j) = 0.0d0
              v10m(i,j) = 0.0d0
              w10m(i,j) = 0.0d0
            end if

            if (work1(i,jmut-j+1,6) /= t2m_miss(1)) then
              tmp2m(i,j) = work1(i,jmut-j+1,6) * t2m_scale(1) + t2m_offset(1)
            else
              tmp2m(i,j) = 0.0d0
            end if

            if (work1(i,jmut-j+1,7) /= d2m_miss(1)) then
              dew2m(i,j) = work1(i,jmut-j+1,7) * d2m_scale(1) + d2m_offset(1)
            else
              dew2m(i,j) = 0.0d0
            end if

            if (work2(i,jmut-j+1,1) /= skt_miss(1)) then
              skt(i,j) = work2(i,jmut-j+1,1) * skt_scale(1) + skt_offset(1)
            else
              skt(i,j) = 0.0d0
            end if

            if ((aexl(i,j) == 1.0d0) .and. (ice(i,j) <= 0.55d0)) then
              sst_in(i,j) = sst(i,j)
            else
              sst_in(i,j) = skt(i,j)
            end if

            if (aexl(i,j) == 1.0d0) then
              if (ice(i,j) <= 0.55d0) then
                ice_in(i,j) = 0.0d0
              else
                ice_in(i,j) = 1.0d0
              end if
            else
              ice_in(i,j) = 0.0d0
            end if

            if ((slprs(i,j) /= 0.0d0) .and. (dew2m(i,j) /= 0.0d0)) then

              slp_tmp = slprs(i,j) * 1.0d-2 ! [hPa]
              d2m_tmp = dew2m(i,j) - tab

              if (ice(i,j) > 0.55d0) then
                hl1 = (g1 + g2 * d2m_tmp) / (1.0d0 + g3 * d2m_tmp) + i1 * d2m_tmp
                hl2 = 10 ** hl1
                hl3 = 1.0d0 + 1.0d-6 * slp_tmp * (h1 + h2 * d2m_tmp**2)
                esat = hl2 * hl3 
                qsat = epsilon * esat / (slp_tmp - (1.0d0 - epsilon) * esat)
                !qsat = q1i * exp(q2i / (satc(i,j) + tab)) / rhoa
              else
                hl1 = (g1 + g2 * d2m_tmp) / (1.0d0 + g3 * d2m_tmp)
                hl2 = 10 ** hl1
                hl3 = 1.0d0 + 1.0d-6 * slp_tmp * (h1 + h2 * d2m_tmp**2)
                esat = hl2 * hl3
                qsat = epsilon * esat / (slp_tmp - (1.0d0 - epsilon) * esat)
                !qsat = q0 * q1a * exp(q2a / (satc(i,j) + tab)) / rhoa
              end if

              sph2m(i,j) = qsat

            else

              sph2m(i,j) = 0.0d0

            end if

          end do
        end do

        !write(6,*) ice(240,160),  sst(240,160),  skt(240,160) 
        !write(6,*) u10m(240,160),  v10m(240,160),  slprs(240,160)
        !write(6,*) tmp2m(240,160),  dew2m(240,160),  sph2m(240,160)

        call shift_wind_real2neutral( &
             & wn10m_tmp,tmp10m_tmp,sph10m_tmp, &
             & w10m,tmp2m,sph2m,slprs,sst_in,ice_in,&
             & imut,jmut,aexl,altu,altt,altq,alt_target,sphmin)

        do j = 1, jmut
          do i = 1, imut
            un10m_tmp(i,j) = u10m(i,j) * wn10m_tmp(i,j) / w10m(i,j)
            vn10m_tmp(i,j) = v10m(i,j) * wn10m_tmp(i,j) / w10m(i,j)
          end do
        end do

        !write(6,*) w10m(240,160),   wn10m_tmp(240,160)
        !write(6,*) tmp10m_tmp(240,160), tmp2m(240,160),  sst_in(240,160)

        do j = 1, jmut
          do i = 1, imut
            if (wind_mask(i,j) /= 0.0d0) then
              num_valid_wind(i,j,m) = num_valid_wind(i,j,m) + nhinc
              un10m(i,j,m) = un10m(i,j,m) + un10m_tmp(i,j) * real(nhinc,8)
              vn10m(i,j,m) = vn10m(i,j,m) + vn10m_tmp(i,j) * real(nhinc,8)
              wn10m(i,j,m) = wn10m(i,j,m) + wn10m_tmp(i,j) * real(nhinc,8)
              if ((nd == 1) .and. (nh == 1) .and. (m > 1)) then
                num_valid_wind(i,j,m-1) = num_valid_wind(i,j,m-1) + nhinc
                un10m(i,j,m-1) = un10m(i,j,m-1) + un10m_tmp(i,j) * real(nhinc,8)
                vn10m(i,j,m-1) = vn10m(i,j,m-1) + vn10m_tmp(i,j) * real(nhinc,8)
                wn10m(i,j,m-1) = wn10m(i,j,m-1) + wn10m_tmp(i,j) * real(nhinc,8)
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

    start1(3) = nrecord

    iostat1 = nf_get_vara_double(ifiles1(1),var1(1,1),start1,range1,work1(1,1,1))
    sta1(1) = sta1(1) + iostat1

    iostat1 = nf_get_vara_double(ifiles1(1),var1(1,2),start1,range1,work1(1,1,2))
    sta1(1) = sta1(1) + iostat1

    iostat1 = nf_get_vara_double(ifiles1(1),var1(1,3),start1,range1,work1(1,1,3))
    sta1(1) = sta1(1) + iostat1

    iostat1 = nf_get_vara_double(ifiles1(1),var1(1,4),start1,range1,work1(1,1,4))
    sta1(1) = sta1(1) + iostat1

    iostat1 = nf_get_vara_double(ifiles1(1),var1(1,5),start1,range1,work1(1,1,5))
    sta1(1) = sta1(1) + iostat1

    iostat1 = nf_get_vara_double(ifiles1(1),var1(1,6),start1,range1,work1(1,1,6))
    sta1(1) = sta1(1) + iostat1

    iostat1 = nf_get_vara_double(ifiles1(1),var1(1,7),start1,range1,work1(1,1,7))
    sta1(1) = sta1(1) + iostat1

    start2(3) = nrecord

    iostat2 = nf_get_vara_double(ifiles2(1),var2(1,1),start2,range2,work2(1,1,1))
    sta2(1) = sta2(1) + iostat2

    do j = 1, jmut
      do i = 1, imut

        if (work1(i,jmut-j+1,1) /= ice_miss(1)) then
          ice(i,j) = work1(i,jmut-j+1,1) * ice_scale(1) + ice_offset(1)
        else
          ice(i,j) = 0.0d0
        end if

        if (work1(i,jmut-j+1,2) /= sst_miss(1)) then
          sst(i,j) = work1(i,jmut-j+1,2) * sst_scale(1) + sst_offset(1)
          aexl(i,j) = 1.0d0
        else
          sst(i,j) = 0.0d0
        end if
        
        if (work1(i,jmut-j+1,3) /= msl_miss(1)) then
          slprs(i,j) = work1(i,jmut-j+1,3) * msl_scale(1) + msl_offset(1)
        else
          slprs(i,j) = 0.0d0
        end if
        
        if ((work1(i,jmut-j+1,4) /= u10m_miss(1)) .and. (work1(i,jmut-j+1,5) /= v10m_miss(1))) then
          u10m(i,j) = work1(i,jmut-j+1,4) * u10m_scale(1) + u10m_offset(1)
          v10m(i,j) = work1(i,jmut-j+1,5) * v10m_scale(1) + v10m_offset(1)
          w10m(i,j) = sqrt(u10m(i,j)**2 + v10m(i,j)**2)
          wind_mask(i,j) = 1.0d0
        else
          u10m(i,j) = 0.0d0
          v10m(i,j) = 0.0d0
          w10m(i,j) = 0.0d0
        end if
        
        if (work1(i,jmut-j+1,6) /= t2m_miss(1)) then
          tmp2m(i,j) = work1(i,jmut-j+1,6) * t2m_scale(1) + t2m_offset(1)
        else
          tmp2m(i,j) = 0.0d0
        end if

        if (work1(i,jmut-j+1,7) /= d2m_miss(1)) then
          dew2m(i,j) = work1(i,jmut-j+1,7) * d2m_scale(1) + d2m_offset(1)
        else
          dew2m(i,j) = 0.0d0
        end if

        if (work2(i,jmut-j+1,1) /= skt_miss(1)) then
          skt(i,j) = work2(i,jmut-j+1,1) * skt_scale(1) + skt_offset(1)
        else
          skt(i,j) = 0.0d0
        end if

        if ((aexl(i,j) == 1.0d0) .and. (ice(i,j) <= 0.55d0)) then
          sst_in(i,j) = sst(i,j)
        else
          sst_in(i,j) = skt(i,j)
        end if

        if (aexl(i,j) == 1.0d0) then
          if (ice(i,j) <= 0.55d0) then
            ice_in(i,j) = 0.0d0
          else
            ice_in(i,j) = 1.0d0
          end if
        else
          ice_in(i,j) = 0.0d0
        end if

        if ((slprs(i,j) /= 0.0d0) .and. (dew2m(i,j) /= 0.0d0)) then
          slp_tmp = slprs(i,j) * 1.0d-2 ! [hPa]
          d2m_tmp = dew2m(i,j) - tab

          if (ice(i,j) > 0.55d0) then
            hl1 = (g1 + g2 * d2m_tmp) / (1.0d0 + g3 * d2m_tmp) + i1 * d2m_tmp
            hl2 = 10 ** hl1
            hl3 = 1.0d0 + 1.0d-6 * slp_tmp * (h1 + h2 * d2m_tmp**2)
            esat = hl2 * hl3 
            qsat = epsilon * esat / (slp_tmp - (1.0d0 - epsilon) * esat)
            !qsat = q1i * exp(q2i / (satc(i,j) + tab)) / rhoa
          else
            hl1 = (g1 + g2 * d2m_tmp) / (1.0d0 + g3 * d2m_tmp)
            hl2 = 10 ** hl1
            hl3 = 1.0d0 + 1.0d-6 * slp_tmp * (h1 + h2 * d2m_tmp**2)
            esat = hl2 * hl3
            qsat = epsilon * esat / (slp_tmp - (1.0d0 - epsilon) * esat)
            !qsat = q0 * q1a * exp(q2a / (satc(i,j) + tab)) / rhoa
          end if

          sph2m(i,j) = qsat

        else

          sph2m(i,j) = 0.0d0

        end if

      end do
    end do

    call shift_wind_real2neutral( &
         & wn10m_tmp,tmp10m_tmp,sph10m_tmp, &
         & w10m,tmp2m,sph2m,slprs,sst_in,ice_in,&
         & imut,jmut,aexl,altu,altt,altq,alt_target,sphmin)

    do j = 1, jmut
      do i = 1, imut
        un10m_tmp(i,j) = u10m(i,j) * wn10m_tmp(i,j) / w10m(i,j)
        vn10m_tmp(i,j) = v10m(i,j) * wn10m_tmp(i,j) / w10m(i,j)
      end do
    end do

    do j = 1, jmut
      do i = 1, imut
        if (wind_mask(i,j) /= 0.0d0) then
          num_valid_wind(i,j,m) = num_valid_wind(i,j,m) + nhinc
          un10m(i,j,m) = un10m(i,j,m) + un10m_tmp(i,j) * real(nhinc,8)
          vn10m(i,j,m) = vn10m(i,j,m) + vn10m_tmp(i,j) * real(nhinc,8)
          wn10m(i,j,m) = wn10m(i,j,m) + wn10m_tmp(i,j) * real(nhinc,8)
        end if
      end do
    end do

  else

    m = 12
    nrecord = 1
    nhinc = 3

    start1(3) = nrecord

    iostat1 = nf_get_vara_double(ifiles1(2),var1(2,1),start1,range1,work1(1,1,1))
    sta1(2) = sta1(2) + iostat1

    iostat1 = nf_get_vara_double(ifiles1(2),var1(2,2),start1,range1,work1(1,1,2))
    sta1(2) = sta1(2) + iostat1

    iostat1 = nf_get_vara_double(ifiles1(2),var1(2,3),start1,range1,work1(1,1,3))
    sta1(2) = sta1(2) + iostat1

    iostat1 = nf_get_vara_double(ifiles1(2),var1(2,4),start1,range1,work1(1,1,4))
    sta1(2) = sta1(2) + iostat1

    iostat1 = nf_get_vara_double(ifiles1(2),var1(2,5),start1,range1,work1(1,1,5))
    sta1(2) = sta1(2) + iostat1

    iostat1 = nf_get_vara_double(ifiles1(2),var1(2,6),start1,range1,work1(1,1,6))
    sta1(2) = sta1(2) + iostat1

    iostat1 = nf_get_vara_double(ifiles1(2),var1(2,7),start1,range1,work1(1,1,7))
    sta1(2) = sta1(2) + iostat1

    start2(3) = nrecord

    iostat2 = nf_get_vara_double(ifiles2(2),var2(2,1),start2,range2,work2(1,1,1))
    sta2(2) = sta2(2) + iostat2

    do j = 1, jmut
      do i = 1, imut

        if (work1(i,jmut-j+1,1) /= ice_miss(2)) then
          ice(i,j) = work1(i,jmut-j+1,1) * ice_scale(2) + ice_offset(2)
        else
          ice(i,j) = 0.0d0
        end if

        if (work1(i,jmut-j+1,2) /= sst_miss(2)) then
          sst(i,j) = work1(i,jmut-j+1,2) * sst_scale(2) + sst_offset(2)
          aexl(i,j) = 1.0d0
        else
          sst(i,j) = 0.0d0
        end if
        
        if (work1(i,jmut-j+1,3) /= msl_miss(2)) then
          slprs(i,j) = work1(i,jmut-j+1,3) * msl_scale(2) + msl_offset(2)
        else
          slprs(i,j) = 0.0d0
        end if
        
        if ((work1(i,jmut-j+1,4) /= u10m_miss(2)) .and. (work1(i,jmut-j+1,5) /= v10m_miss(2))) then
          u10m(i,j) = work1(i,jmut-j+1,4) * u10m_scale(2) + u10m_offset(2)
          v10m(i,j) = work1(i,jmut-j+1,5) * v10m_scale(2) + v10m_offset(2)
          w10m(i,j) = sqrt(u10m(i,j)**2 + v10m(i,j)**2)
          wind_mask(i,j) = 1.0d0
        else
          u10m(i,j) = 0.0d0
          v10m(i,j) = 0.0d0
          w10m(i,j) = 0.0d0
        end if
        
        if (work1(i,jmut-j+1,6) /= t2m_miss(2)) then
          tmp2m(i,j) = work1(i,jmut-j+1,6) * t2m_scale(2) + t2m_offset(2)
        else
          tmp2m(i,j) = 0.0d0
        end if

        if (work1(i,jmut-j+1,7) /= d2m_miss(2)) then
          dew2m(i,j) = work1(i,jmut-j+1,7) * d2m_scale(2) + d2m_offset(2)
        else
          dew2m(i,j) = 0.0d0
        end if

        if (work2(i,jmut-j+1,1) /= skt_miss(2)) then
          skt(i,j) = work2(i,jmut-j+1,1) * skt_scale(2) + skt_offset(2)
        else
          skt(i,j) = 0.0d0
        end if

        if ((aexl(i,j) == 1.0d0) .and. (ice(i,j) <= 0.55d0)) then
          sst_in(i,j) = sst(i,j)
        else
          sst_in(i,j) = skt(i,j)
        end if

        if (aexl(i,j) == 1.0d0) then
          if (ice(i,j) <= 0.55d0) then
            ice_in(i,j) = 0.0d0
          else
            ice_in(i,j) = 1.0d0
          end if
        else
          ice_in(i,j) = 0.0d0
        end if

        if ((slprs(i,j) /= 0.0d0) .and. (dew2m(i,j) /= 0.0d0)) then
          slp_tmp = slprs(i,j) * 1.0d-2 ! [hPa]
          d2m_tmp = dew2m(i,j) - tab

          if (ice(i,j) > 0.55d0) then
            hl1 = (g1 + g2 * d2m_tmp) / (1.0d0 + g3 * d2m_tmp) + i1 * d2m_tmp
            hl2 = 10 ** hl1
            hl3 = 1.0d0 + 1.0d-6 * slp_tmp * (h1 + h2 * d2m_tmp**2)
            esat = hl2 * hl3 
            qsat = epsilon * esat / (slp_tmp - (1.0d0 - epsilon) * esat)
            !qsat = q1i * exp(q2i / (satc(i,j) + tab)) / rhoa
          else
            hl1 = (g1 + g2 * d2m_tmp) / (1.0d0 + g3 * d2m_tmp)
            hl2 = 10 ** hl1
            hl3 = 1.0d0 + 1.0d-6 * slp_tmp * (h1 + h2 * d2m_tmp**2)
            esat = hl2 * hl3
            qsat = epsilon * esat / (slp_tmp - (1.0d0 - epsilon) * esat)
            !qsat = q0 * q1a * exp(q2a / (satc(i,j) + tab)) / rhoa
          end if

          sph2m(i,j) = qsat

        else

          sph2m(i,j) = 0.0d0

        end if

      end do
    end do

    call shift_wind_real2neutral( &
         & wn10m_tmp,tmp10m_tmp,sph10m_tmp, &
         & w10m,tmp2m,sph2m,slprs,sst_in,ice_in,&
         & imut,jmut,aexl,altu,altt,altq,alt_target,sphmin)

    do j = 1, jmut
      do i = 1, imut
        un10m_tmp(i,j) = u10m(i,j) * wn10m_tmp(i,j) / w10m(i,j)
        vn10m_tmp(i,j) = v10m(i,j) * wn10m_tmp(i,j) / w10m(i,j)
      end do
    end do

    do j = 1, jmut
      do i = 1, imut
        if (wind_mask(i,j) /= 0.0d0) then
          num_valid_wind(i,j,m) = num_valid_wind(i,j,m) + nhinc
          un10m(i,j,m) = un10m(i,j,m) + un10m_tmp(i,j) * real(nhinc,8)
          vn10m(i,j,m) = vn10m(i,j,m) + vn10m_tmp(i,j) * real(nhinc,8)
          wn10m(i,j,m) = wn10m(i,j,m) + wn10m_tmp(i,j) * real(nhinc,8)
        end if
      end do
    end do

  end if

  !----------------------------------------------------------

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
        if (num_valid_wind(i,j,m) == 24 * nday) then
          un10m(i,j,m) = un10m(i,j,m) / real(num_valid_wind(i,j,m),8)
          vn10m(i,j,m) = vn10m(i,j,m) / real(num_valid_wind(i,j,m),8)
          wn10m(i,j,m) = wn10m(i,j,m) / real(num_valid_wind(i,j,m),8)
        else
          write(6,*) ' there is at least one missing value for ', i, j
          un10m(i,j,m) = real(undef_mxe,8)
          vn10m(i,j,m) = real(undef_mxe,8)
          wn10m(i,j,m) = real(undef_mxe,8)
        end if
      end do
    end do

    ! open output file

    write(file_u10m,'(1a,1a,i4.4,i2.2)') trim(file_out_base),'/erai_un10m.',nyear,m
    open(mtot1,file=file_u10m,form='unformatted',access='direct',recl=4*imut*jmut)
    write(6,*) 'Neutral wind (U) written to ',trim(file_u10m)
    write(mtot1,rec=1) real(un10m(1:imut,1:jmut,m),4)
    close(mtot1)

    write(file_v10m,'(1a,1a,i4.4,i2.2)') trim(file_out_base),'/erai_vn10m.',nyear,m
    open(mtot1,file=file_v10m,form='unformatted',access='direct',recl=4*imut*jmut)
    write(6,*) 'Neutral wind (V) written to ',trim(file_v10m)
    write(mtot1,rec=1) real(vn10m(1:imut,1:jmut,m),4)
    close(mtot1)

    write(file_w10m,'(1a,1a,i4.4,i2.2)') trim(file_out_base),'/erai_wn10m.',nyear,m
    open(mtot1,file=file_w10m,form='unformatted',access='direct',recl=4*imut*jmut)
    write(6,*) 'Neutral wind (W) written to ',trim(file_w10m)
    write(mtot1,rec=1) real(wn10m(1:imut,1:jmut,m),4)
    close(mtot1)

  end do

  do n = 1, nfiles1_tmp
    iostat1 = nf_close(ifiles1(n))
    if (iostat1 /= 0) then
      write(6,*) 'nf_close error for file number ',n, ifiles1(n), iostat1
    end if
  end do

  do n = 1, nfiles2_tmp
    iostat2 = nf_close(ifiles2(n))
    if (iostat2 /= 0) then
      write(6,*) 'nf_close error for file number ',n, ifiles2(n), iostat2
    end if
  end do

end program read_netcdf_output_grads_monthly
