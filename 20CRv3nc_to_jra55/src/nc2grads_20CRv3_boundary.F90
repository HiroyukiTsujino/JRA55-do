! -*-F90-*-
!==================================================================
program read_netcdf_output_grads_monthly
  !----------------------------------------------------------------
  ! Information:
  !   Create direct access file (GrADS) of
  !     the ERA-Interim surface analysis data
  !----------------------------------------------------------------

  implicit none

  include 'netcdf.inc'

  integer(4), parameter :: imut = 360, jmut = 181
  integer(4), parameter :: mtot1 = 91, mtot2 = 92, mtot3 = 93

  real(8) :: un10m (imut,jmut)
  real(8) :: vn10m (imut,jmut)
  real(8) :: wn10m (imut,jmut)
  real(8) :: tmp10m(imut,jmut)
  real(8) :: sph10m(imut,jmut)

  real(8) :: sst_in(imut,jmut)
  real(8) :: ice_in(imut,jmut)

  real(8) :: aexl(imut,jmut)
  real(8) :: tmp2m(imut,jmut)
  real(8) :: sph2m(imut,jmut)
  real(8) :: slprs(imut,jmut)

  real(8) :: u10m(imut,jmut)
  real(8) :: v10m(imut,jmut)
  real(8) :: w10m(imut,jmut)

  real(8) :: ice(imut,jmut)
  real(8) :: skt(imut,jmut)

  integer(4) :: idmon(12)

  integer(4) :: i, j, k, l, m, n

  integer(4) :: nrecord
  integer(4) :: ireco
  integer(4) :: mst, med
  integer(4) :: nday, nd, nh

  real(4),parameter :: undef_mxe = -9.99e33

  integer(4) :: nyear

  character(128) :: file_in_base
  character(128) :: file_out_base
  character(128) :: file_t10m, file_q10m, file_w10m

  ! for netCDF

  integer(4), parameter :: nvars1 = 7 ! variable
  integer(4), parameter :: nvars2 = 1 ! constant

  character(16) :: var_name1(nvars1)
  character(16) :: var_name2(nvars2)
  character(16) :: file_name1(nvars1)
  character(16) :: file_name2(nvars2)

  integer(4) :: var1(nvars1), sta1(nvars1)
  integer(4) :: var2(nvars2), sta2(nvars2)

  integer(4) :: iostat1, iostat2
  integer(4) :: start1(3), range1(3)
  integer(4) :: start2(3), range2(3)

  character(128) :: flnin1(nvars1)
  character(128) :: flnin2(nvars2)
  integer(4) :: ifiles1(nvars1)
  integer(4) :: ifiles2(nvars2)

  real(4) :: miss1(nvars1)
  real(4) :: miss2(nvars1)

  real(4) :: ice_miss
  real(4) :: skt_miss
  real(4) :: msl_miss
  real(4) :: t2m_miss
  real(4) :: q2m_miss
  real(4) :: u10m_miss
  real(4) :: v10m_miss
  real(4) :: land_miss

  real(4) :: work1(imut,jmut,nvars1)
  real(4) :: work2(imut,jmut,nvars2)

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

  real(8) :: altu, altt, altq, alt_target

  !--------------------------------------------------------------------

  namelist /nml_20CRv3_boundary/ &
       & nyear,             &
       & file_in_base,      &
       & file_out_base

  !--------------------------------------------------------------------
  
  open (10,file='namelist.20CRv3_boundary')
  read (10,nml=nml_20CRv3_boundary)
  close(10)

  !--------------------------------------------------------------------
  ! general settings

  idmon(1:12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  var_name1 (1:nvars1) = (/ "icec ", "skt ", "prmsl ", "uwnd", "vwnd ", "air ", "shum " /)
  file_name1(1:nvars1) = (/ "icec ", "skt ", "prmsl ", "uwnd.10m ", "vwnd.10m ", "air.2m ", "shum.2m " /)

  var_name2 (1:nvars2) = (/ "land" /)
  file_name2(1:nvars2) = (/ "land" /)

  altu = 10.d0
  altt = 2.0d0
  altq = 2.0d0
  alt_target = 10.d0

  !---------------------------------------------------------------------

  start1(1:3) = (/ 1, 1, 1 /)
  range1(1:3) = (/ 360, 181, 1 /)

  start2(1:3) = (/ 1, 1, 1 /)
  range2(1:3) = (/ 360, 181, 1 /)

  sta1(1:nvars1) = 0
  sta2(1:nvars2) = 0

  !--------------------------------------------------------------------------
  ! file 1

  do n = 1, nvars1

    write(flnin1(n),'(1a,1a,1a,1a,i4.4,1a)') &
         & trim(file_in_base),'/',trim(file_name1(n)),'.',nyear,'.nc'
    write(6,*) 'reading from ', trim(flnin1(n))

    iostat1 = nf_open(flnin1(n),nf_nowrite,ifiles1(n)) 
    if (iostat1 /= 0) then
      write(6,*) 'nf_open error for file number ',n, ' iostat1 = ', iostat1
      stop
    end if

    iostat1 = nf_inq_varid(ifiles1(n),trim(var_name1(n)),var1(n))
    if (iostat1 /= 0) then
      write(6,*) 'nf_inq_varid error for variable',n, trim(var_name1(n)),' in file', n, iostat1
      stop
    end if

    sta1(n) = 0
    iostat1 = nf_get_att_double(ifiles1(n),var1(n),'missing_value',miss1(n))
    sta1(n) = sta1(n) + iostat1
    if (sta1(n) /= 0) then
      write(6,*) 'nf_inq_varid error for variable ',m,' in file', n, sta1(n)
      stop
    end if

    write(6,*) trim(var_name1(n)), ' missing_value', n, miss1  (n)

  end do

  ice_miss  = miss1(1)
  skt_miss  = miss1(2)
  msl_miss  = miss1(3)
  u10m_miss = miss1(4)
  v10m_miss = miss1(5)
  t2m_miss  = miss1(6)
  q2m_miss  = miss1(7)

  ! file 2 (time invariant)

  do n = 1, nvars2

    write(flnin2(n),'(1a,1a,1a,1a)') &
       & trim(file_in_base),'/',trim(file_name2(n)),'.nc'
    write(6,*) 'reading from ', trim(flnin2(n))

    iostat2 = nf_open(flnin2(n),nf_nowrite,ifiles2(n)) 
    if (iostat2 /= 0) then
      write(6,*) 'nf_open error for file number ',n, ' iostat2 = ', iostat2
      stop
    end if

    iostat2 = nf_inq_varid(ifiles2(n),trim(var_name2(n)),var2(n))
    if (iostat2 /= 0) then
      write(6,*) 'nf_inq_varid error for variable',n,' in file', n, iostat2
      stop
    end if

    sta2(n) = 0
    iostat2 = nf_get_att_real(ifiles2(n),var2(n),'missing_value',miss2(n))
    sta2(n) = sta2(n) + iostat2
    if (sta2(n) /= 0) then
      write(6,*) 'nf_get_att_real error for variable',n,' in file', n, sta2(n)
      stop
    end if

    write(6,*) trim(var_name2(n)), ' missing_value', n, miss2  (n)

  end do

  land_miss = miss2(1)

  start2(1) = 1
  iostat2 = nf_get_vara_real(ifiles2(1),var2(1),start2,range2,work2(1,1,1))
  sta2(1) = sta2(1) + iostat2
  if (sta2(n) /= 0) then
    write(6,*) 'nf_get_vara_real error for variable 1 in file 2: ', sta2(n)
    stop
  end if

  do j = 1, jmut
    do i = 1, imut
      if (work1(i,j,1) /= land_miss) then
        aexl(i,j) = 1.0d0 - real(work1(i,j,1),8)
      else
        aexl(i,j) = 0.0d0
      end if
    end do
  end do

  !--------------------------------------------------
  ! open output file

  write(file_t10m,'(1a,1a,i4.4)') trim(file_out_base),'/tmp10m.',nyear
  open(mtot1,file=file_t10m,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'Neutral wind (U) written to ',trim(file_t10m)

  write(file_q10m,'(1a,1a,i4.4)') trim(file_out_base),'/sph10m.',nyear
  open(mtot2,file=file_q10m,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'Neutral wind (V) written to ',trim(file_q10m)

  write(file_w10m,'(1a,1a,i4.4)') trim(file_out_base),'/wdn10m.',nyear
  open(mtot3,file=file_w10m,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'Neutral wind (W) written to ',trim(file_w10m)

  !-------------------------------------------------

  un10m(:,:) = 0.0d0
  vn10m(:,:) = 0.0d0
  wn10m(:,:) = 0.0d0

  mst = 1
  med = 12
  nrecord = 0
  ireco = 0

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

      do nh = 1, 8 ! 3-hourly

        nrecord  = nrecord + 1

        start1(3) = nrecord

        iostat1 = nf_get_vara_real(ifiles1(1),var1(1),start1,range1,work1(1,1,1))
        sta1(1) = sta1(1) + iostat1

        iostat1 = nf_get_vara_real(ifiles1(2),var1(2),start1,range1,work1(1,1,2))
        sta1(1) = sta1(1) + iostat1

        iostat1 = nf_get_vara_real(ifiles1(3),var1(3),start1,range1,work1(1,1,3))
        sta1(1) = sta1(1) + iostat1

        iostat1 = nf_get_vara_real(ifiles1(4),var1(4),start1,range1,work1(1,1,4))
        sta1(1) = sta1(1) + iostat1

        iostat1 = nf_get_vara_real(ifiles1(5),var1(5),start1,range1,work1(1,1,5))
        sta1(1) = sta1(1) + iostat1

        iostat1 = nf_get_vara_real(ifiles1(6),var1(6),start1,range1,work1(1,1,6))
        sta1(1) = sta1(1) + iostat1

        iostat1 = nf_get_vara_real(ifiles1(7),var1(7),start1,range1,work1(1,1,7))
        sta1(1) = sta1(1) + iostat1

        do j = 1, jmut
          do i = 1, imut

            if (work1(i,j,1) /= ice_miss) then
              ice(i,j) = real(work1(i,j,1),8)
            else
              ice(i,j) = 0.0d0
            end if

            if (work1(i,j,2) /= skt_miss) then
              skt(i,j) = real(work1(i,j,2),8)
            else
              skt(i,j) = 0.0d0
            end if

            if (work1(i,j,3) /= msl_miss) then
              slprs(i,j) = real(work1(i,j,3),8)
            else
              slprs(i,j) = 0.0d0
            end if

            if ((work1(i,j,4) /= u10m_miss) .and. (work1(i,j,5) /= v10m_miss)) then
              u10m(i,j) = real(work1(i,j,4),8)
              v10m(i,j) = real(work1(i,j,5),8)
              w10m(i,j) = sqrt(u10m(i,j)**2 + v10m(i,j)**2)
            else
              u10m(i,j) = 0.0d0
              v10m(i,j) = 0.0d0
              w10m(i,j) = 0.0d0
            end if

            if (work1(i,j,6) /= t2m_miss) then
              tmp2m(i,j) = real(work1(i,j,6),8)
            else
              tmp2m(i,j) = 0.0d0
            end if

            if (work1(i,j,7) /= q2m_miss) then
              sph2m(i,j) = real(work1(i,j,7),8)
            else
              sph2m(i,j) = 0.0d0
            end if

            if (aexl(i,j) == 1.0d0) then
              ice_in(i,j) = real(ice(i,j),8)
            else
              ice_in(i,j) = 0.0d0
            end if

            sst_in(i,j) = skt(i,j)

          end do
        end do

        call shift_wind_real2neutral( &
             & wn10m,tmp10m,sph10m, &
             & w10m,tmp2m,sph2m,slprs,sst_in,ice_in,&
             & imut,jmut,aexl,altu,altt,altq,alt_target,sphmin)

        do j = 1, jmut
          do i = 1, imut
            un10m(i,j) = u10m(i,j) * wn10m(i,j) / w10m(i,j)
            vn10m(i,j) = v10m(i,j) * wn10m(i,j) / w10m(i,j)
          end do
        end do

        ireco = ireco + 1
        write(mtot1,rec=ireco) real(tmp10m(1:imut,1:jmut),4)
        write(mtot2,rec=ireco) real(sph10m(1:imut,1:jmut),4)
        write(mtot3,rec=ireco) real(wn10m (1:imut,1:jmut),4)

      end do
    end do
  end do
  
  close(mtot3)
  close(mtot2)
  close(mtot1)

  do n = 1, nvars1
    iostat1 = nf_close(ifiles1(n))
    if (iostat1 /= 0) then
      write(6,*) 'nf_close error for file number ',n, ifiles1(n), iostat1
    end if
  end do

  do n = 1, nvars2
    iostat2 = nf_close(ifiles2(n))
    if (iostat2 /= 0) then
      write(6,*) 'nf_close error for file number ',n, ifiles2(n), iostat2
    end if
  end do

end program read_netcdf_output_grads_monthly
