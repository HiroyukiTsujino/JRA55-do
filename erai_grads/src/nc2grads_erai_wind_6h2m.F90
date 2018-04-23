! -*-F90-*-
!==================================================================
program read_netcdf_output_grads_d2m
  !----------------------------------------------------------------
  ! Information:
  !   Create direct access file (GrADS) of
  !     the ERA-Iterim surface data
  !----------------------------------------------------------------

  implicit none

  include '/usr/local/netcdf/include/netcdf.inc'

  integer(4), parameter :: imut = 480, jmut = 241
  integer(4), parameter :: mtot1 = 91, mtot2 = 92, mtot3 = 93

  real(8) :: sst(imut,jmut,12)
  real(8) :: u10(imut,jmut,12)
  real(8) :: v10(imut,jmut,12)
  real(8) :: w10(imut,jmut,12)

  integer(4) :: num_valid_sst(imut,jmut,12)
  integer(4) :: num_valid_w10(imut,jmut,12)
  integer(4) :: ho4(imut,jmut)

  real(8) :: work8(imut,jmut)
  real(8) :: worku8(imut,jmut)
  real(8) :: workv8(imut,jmut)

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
  character(128) :: file_out1_base, file_out1
  character(128) :: file_out2_base, file_out2
  character(128) :: file_out3_base, file_out3

  logical :: l_mask_out

  ! for netCDF

  integer(4), parameter :: nvars = 3, nfiles = 2
  integer(4) :: var(nfiles,nvars), sta(nfiles)
  integer(4) :: iostat
  integer(4) :: start(3), range(3)
  character(128) :: flnin(nfiles)
  integer(4) :: ifiles(nfiles)

  real(8) :: sst_offset(nfiles), sst_scale(nfiles), sst_miss(nfiles)
  real(8) :: u10_offset(nfiles), u10_scale(nfiles), u10_miss(nfiles)
  real(8) :: v10_offset(nfiles), v10_scale(nfiles), v10_miss(nfiles)
  real(8) :: hl1, hl2

  integer(4) :: nfiles_tmp

  !--------------------------------------------------------------------

  namelist /nml_erai_wind_org2mon/ nyear, &
       & file_in1, file_in2, &
       & file_out1_base, file_out2_base, file_out3_base, &
       & l_mask_out, file_mask

  !--------------------------------------------------------------------
  
  open (10,file='namelist.erai_wind_org2mon')
  read (10,nml=nml_erai_wind_org2mon)
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

    iostat = nf_inq_varid(ifiles(n),'sst',var(n,1))
    if (iostat /= 0) then
      write(6,*) 'nf_inq_varid error for variable 1 in file', n, iostat
      stop
    end if

    iostat = nf_inq_varid(ifiles(n),'u10',var(n,2))
    if (iostat /= 0) then
      write(6,*) 'nf_inq_varid error for variable 2 in file', n, iostat
      stop
    end if

    iostat = nf_inq_varid(ifiles(n),'v10',var(n,3))
    if (iostat /= 0) then
      write(6,*) 'nf_inq_varid error for variable 3 in file', n, iostat
      stop
    end if

    sta(n) = 0
    iostat = nf_get_att_double(ifiles(n),var(n,1),'add_offset'   ,sst_offset(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,1),'scale_factor' ,sst_scale(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,1),'missing_value',sst_miss(n))
    sta(n) = sta(n) + iostat
    if (sta(n) /= 0) then
      write(6,*) 'nf_inq_varid error for variable 1 in file', n, sta(n)
      stop
    end if

    sta(n) = 0
    iostat = nf_get_att_double(ifiles(n),var(n,2),'add_offset'   ,u10_offset(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,2),'scale_factor' ,u10_scale(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,2),'missing_value',u10_miss(n))
    sta(n) = sta(n) + iostat
    if (sta(n) /= 0) then
      write(6,*) 'nf_inq_varid error for variable 2 in file', n, sta(n)
      stop
    end if

    sta(n) = 0
    iostat = nf_get_att_double(ifiles(n),var(n,3),'add_offset'   ,v10_offset(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,3),'scale_factor' ,v10_scale(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,3),'missing_value',v10_miss(n))
    sta(n) = sta(n) + iostat
    if (sta(n) /= 0) then
      write(6,*) 'nf_inq_varid error for variable 3 in file', n, sta(n)
      stop
    end if

    write(6,*) 'SST add_offset   ', n, sst_offset(n)
    write(6,*) 'SST scale_factor ', n, sst_scale(n)
    write(6,*) 'SST missing_value', n, sst_miss(n)

    write(6,*) 'U10 add_offset   ', n, u10_offset(n)
    write(6,*) 'U10 scale_factor ', n, u10_scale(n)
    write(6,*) 'U10 missing_value', n, u10_miss(n)

    write(6,*) 'V10 add_offset   ', n, v10_offset(n)
    write(6,*) 'V10 scale_factor ', n, v10_scale(n)
    write(6,*) 'V10 missing_value', n, v10_miss(n)

  end do

  sst(:,:,:) = 0.0d0
  num_valid_sst(:,:,:) = 0

  u10(:,:,:) = 0.0d0
  v10(:,:,:) = 0.0d0
  w10(:,:,:) = 0.0d0
  num_valid_w10(:,:,:) = 0

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

        do j = 1, jmut
          do i = 1, imut
            if (work8(i,j) /= sst_miss(1)) then
              num_valid_sst(i,jmut-j+1,m) = &
                   & num_valid_sst(i,jmut-j+1,m) + nhinc
              sst(i,jmut-j+1,m) = sst(i,jmut-j+1,m) &
                   & + (work8(i,j) * sst_scale(1) + sst_offset(1)) * real(nhinc,8)
              if ((nd == 1) .and. (nh == 1) .and. (m > 1)) then
                num_valid_sst(i,jmut-j+1,m-1) = &
                     & num_valid_sst(i,jmut-j+1,m-1) + nhinc
                sst(i,jmut-j+1,m-1) = sst(i,jmut-j+1,m-1) &
                     & + (work8(i,j) * sst_scale(1) + sst_offset(1)) * real(nhinc,8)
              end if
            end if
          end do
        end do

        start(3) = nrecord
        iostat = nf_get_vara_double(ifiles(1),var(1,2),start,range,worku8)
        sta(1) = sta(1) + iostat
        iostat = nf_get_vara_double(ifiles(1),var(1,3),start,range,workv8)
        sta(1) = sta(1) + iostat

        do j = 1, jmut
          do i = 1, imut
            if ((worku8(i,j) /= u10_miss(1)) .and. (workv8(i,j) /= v10_miss(1))) then
              num_valid_w10(i,jmut-j+1,m) = &
                   & num_valid_w10(i,jmut-j+1,m) + nhinc
              hl1 = worku8(i,j) * u10_scale(1) + u10_offset(1)
              hl2 = workv8(i,j) * v10_scale(1) + v10_offset(1)
              u10(i,jmut-j+1,m) = u10(i,jmut-j+1,m) + hl1 * real(nhinc,8)
              v10(i,jmut-j+1,m) = v10(i,jmut-j+1,m) + hl2 * real(nhinc,8)
              w10(i,jmut-j+1,m) = w10(i,jmut-j+1,m) &
                 & + sqrt(hl1**2 + hl2**2) * real(nhinc,8)
              if ((nd == 1) .and. (nh == 1) .and. (m > 1)) then
                num_valid_w10(i,jmut-j+1,m-1) = &
                     & num_valid_w10(i,jmut-j+1,m-1) + nhinc
                u10(i,jmut-j+1,m-1) = u10(i,jmut-j+1,m-1) + hl1 * real(nhinc,8)
                v10(i,jmut-j+1,m-1) = v10(i,jmut-j+1,m-1) + hl2 * real(nhinc,8)
                w10(i,jmut-j+1,m-1) = w10(i,jmut-j+1,m-1) &
                     & + sqrt(hl1**2 + hl2**2) * real(nhinc,8)
              end if
            end if
          end do
        end do

      end do
    end do
  end do

  !----------------------------------------------------------------------
  ! 1st of January of the next year to be added to 31st of December of this year

  m = 12
  nhinc = 3

  if (nyear == 2015) then ! if 2016 is not available

    nrecord = 365 * 4

    start(3) = nrecord
    iostat = nf_get_vara_double(ifiles(1),var(1,1),start,range,work8)
    sta(1) = sta(1) + iostat

    do j = 1, jmut
      do i = 1, imut
        if (work8(i,j) /= sst_miss(1)) then
          num_valid_sst(i,jmut-j+1,m) = &
               & num_valid_sst(i,jmut-j+1,m) + nhinc
          sst(i,jmut-j+1,m) = sst(i,jmut-j+1,m) &
               & + (work8(i,j) * sst_scale(1) + sst_offset(1)) * real(nhinc,8)
        end if
      end do
    end do

    start(3) = nrecord
    iostat = nf_get_vara_double(ifiles(1),var(1,2),start,range,worku8)
    sta(1) = sta(1) + iostat

    start(3) = nrecord
    iostat = nf_get_vara_double(ifiles(1),var(1,3),start,range,workv8)
    sta(1) = sta(1) + iostat

    do j = 1, jmut
      do i = 1, imut
        if ((worku8(i,j) /= u10_miss(1)) .and. (workv8(i,j) /= v10_miss(1))) then
          num_valid_w10(i,jmut-j+1,m) = &
               & num_valid_w10(i,jmut-j+1,m) + nhinc
          hl1 = worku8(i,j) * u10_scale(1) + u10_offset(1)
          hl2 = workv8(i,j) * v10_scale(1) + v10_offset(1)
          u10(i,jmut-j+1,m) = u10(i,jmut-j+1,m) + hl1 * real(nhinc,8)
          v10(i,jmut-j+1,m) = v10(i,jmut-j+1,m) + hl2 * real(nhinc,8)
          w10(i,jmut-j+1,m) = w10(i,jmut-j+1,m) &
               & + sqrt(hl1**2 + hl2**2) * real(nhinc,8)
        end if
      end do
    end do

  else

    nrecord = 1

    start(3) = nrecord
    iostat = nf_get_vara_double(ifiles(2),var(2,1),start,range,work8)
    sta(2) = sta(2) + iostat

    do j = 1, jmut
      do i = 1, imut
        if (work8(i,j) /= sst_miss(2)) then
          num_valid_sst(i,jmut-j+1,m) = &
               & num_valid_sst(i,jmut-j+1,m) + nhinc
          sst(i,jmut-j+1,m) = sst(i,jmut-j+1,m) &
               & + (work8(i,j) * sst_scale(2) + sst_offset(2)) * real(nhinc,8)
        end if
      end do
    end do

    start(3) = nrecord
    iostat = nf_get_vara_double(ifiles(2),var(2,2),start,range,worku8)
    sta(2) = sta(2) + iostat

    start(3) = nrecord
    iostat = nf_get_vara_double(ifiles(2),var(2,3),start,range,workv8)
    sta(2) = sta(2) + iostat

    do j = 1, jmut
      do i = 1, imut
        if ((worku8(i,j) /= u10_miss(2)) .and. (workv8(i,j) /= v10_miss(2))) then
          num_valid_w10(i,jmut-j+1,m) = &
               & num_valid_w10(i,jmut-j+1,m) + nhinc
          hl1 = worku8(i,j) * u10_scale(2) + u10_offset(2)
          hl2 = workv8(i,j) * v10_scale(2) + v10_offset(2)
          u10(i,jmut-j+1,m) = u10(i,jmut-j+1,m) + hl1 * real(nhinc,8)
          v10(i,jmut-j+1,m) = v10(i,jmut-j+1,m) + hl2 * real(nhinc,8)
          w10(i,jmut-j+1,m) = w10(i,jmut-j+1,m) &
               & + sqrt(hl1**2 + hl2**2) * real(nhinc,8)
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
        if (num_valid_sst(i,j,m) == 24 * nday) then
          sst(i,j,m) = sst(i,j,m) / real(num_valid_sst(i,j,m),8)
        else
          sst(i,j,m) = real(undef_mxe,8)
        end if
        if (num_valid_w10(i,j,m) == 24 * nday) then
          u10(i,j,m) = u10(i,j,m) / real(num_valid_w10(i,j,m),8)
          v10(i,j,m) = v10(i,j,m) / real(num_valid_w10(i,j,m),8)
          w10(i,j,m) = w10(i,j,m) / real(num_valid_w10(i,j,m),8)
        else
          u10(i,j,m) = real(undef_mxe,8)
          v10(i,j,m) = real(undef_mxe,8)
          w10(i,j,m) = real(undef_mxe,8)
        end if
      end do
    end do

    !write(6,'(1a,f12.5,2i6)') &
    !     & 'sst ', sst(230,121,m), num_valid_sst(230,121,m), 24 * nday
    !write(6,'(1a,f12.5,2i6)') &
    !     & 'w10 ', w10(230,121,m), num_valid_w10(230,121,m), 24 * nday

    ! open output file

    write(file_out1,'(1a,i4.4,i2.2)') trim(file_out1_base),nyear,m
    open (mtot1,file=file_out1,form='unformatted',access='direct',recl=4*imut*jmut)
    write(6,*) 'wind10m written to ',trim(file_out1)
    write(mtot1,rec=1) real(u10(1:imut,1:jmut,m),4)
    close(mtot1)

    write(file_out2,'(1a,i4.4,i2.2)') trim(file_out2_base),nyear,m
    open (mtot2,file=file_out2,form='unformatted',access='direct',recl=4*imut*jmut)
    write(6,*) 'wind10m written to ',trim(file_out2)
    write(mtot2,rec=1) real(v10(1:imut,1:jmut,m),4)
    close(mtot2)

    write(file_out3,'(1a,i4.4,i2.2)') trim(file_out3_base),nyear,m
    open (mtot3,file=file_out3,form='unformatted',access='direct',recl=4*imut*jmut)
    write(6,*) 'wind10m written to ',trim(file_out3)
    write(mtot3,rec=1) real(w10(1:imut,1:jmut,m),4)
    close(mtot3)

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
        if (num_valid_sst(i,j,1) == 24 * 31) then
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

end program read_netcdf_output_grads_d2m
