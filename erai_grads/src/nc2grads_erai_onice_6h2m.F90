! -*-F90-*-
!==================================================================
program read_netcdf_output_grads_6h2m
  !----------------------------------------------------------------
  ! Information:
  !   Create direct access file (GrADS)
  !   of ERA-Interim monthly data
  !----------------------------------------------------------------

  implicit none

  include '/usr/local/netcdf/include/netcdf.inc'

  integer(4), parameter :: imut = 480, jmut = 241
  integer(4), parameter :: mtot1 = 91, mtot2 = 92

  real(4) :: dat4(imut,jmut)
  real(8) :: dat8(imut,jmut)
  integer(4) :: ho4(imut,jmut)

  real(8) :: sst(imut,jmut)
  real(8) :: ice(imut,jmut)
  real(8) :: dat(imut,jmut)

  real(8) :: dat_ocn(imut,jmut,12) ! on sea water
  real(8) :: dat_ice(imut,jmut,12) ! on sea ice
  real(8) :: dat_all(imut,jmut,12) ! all conditions
  integer(4) :: num_ocn(imut,jmut,12) ! on sea water
  integer(4) :: num_ice(imut,jmut,12) ! on sea ice
  integer(4) :: num_all(imut,jmut,12) ! all conditions

  real(8) :: work8(imut,jmut)

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
  character(128) :: file_out
  character(128) :: file_out_base_all
  character(128) :: file_out_base_ocn
  character(128) :: file_out_base_ice

  logical :: l_mask_out

  ! for netCDF

  integer(4), parameter :: nvars = 3, nfiles = 2
  integer(4) :: var(nfiles,nvars), sta(nfiles)
  integer(4) :: iostat
  integer(4) :: start(3), range(3)
  character(128) :: flnin(nfiles)
  integer(4) :: ifiles(nfiles)

  real(8) :: sst_offset(nfiles), sst_scale(nfiles), sst_miss(nfiles)
  real(8) :: ice_offset(nfiles), ice_scale(nfiles), ice_miss(nfiles)
  real(8) :: var_offset(nfiles), var_scale(nfiles), var_miss(nfiles)
  real(8) :: scale_tmp, offset_tmp

  integer(4) :: nfiles_tmp

  real(8) :: frac_ratio

  !--------------------------------------------------------------------

  namelist /nml_erai_org2mon_ice/ nyear, var_name, &
       & file_in1, file_in2, &
       & file_out_base_all, &
       & file_out_base_ocn, &
       & file_out_base_ice, &
       & frac_ratio,        &
       & l_mask_out, file_mask

  !--------------------------------------------------------------------
  
  frac_ratio = 1.0d0

  open(10,file='namelist.erai_org2mon_onice')
  read(10,nml=nml_erai_org2mon_ice)
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

    iostat = nf_inq_varid(ifiles(n),var_name,var(n,2))
    if (iostat /= 0) then
      write(6,*) 'nf_inq_varid error for variable 2 in file', n, iostat
      stop
    end if

    iostat = nf_inq_varid(ifiles(n),'ci',var(n,3))
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
    iostat = nf_get_att_double(ifiles(n),var(n,2),'add_offset'   ,var_offset(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,2),'scale_factor' ,var_scale(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,2),'missing_value',var_miss(n))
    sta(n) = sta(n) + iostat
    if (sta(n) /= 0) then
      write(6,*) 'nf_inq_varid error for variable 2 in file', n, sta(n)
      stop
    end if

    sta(n) = 0
    iostat = nf_get_att_double(ifiles(n),var(n,3),'add_offset'   ,ice_offset(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,3),'scale_factor' ,ice_scale(n))
    sta(n) = sta(n) + iostat
    iostat = nf_get_att_double(ifiles(n),var(n,3),'missing_value',ice_miss(n))
    sta(n) = sta(n) + iostat
    if (sta(n) /= 0) then
      write(6,*) 'nf_inq_varid error for variable 3 in file', n, sta(n)
      stop
    end if

    write(6,*) 'SST add_offset   ', n, sst_offset(n)
    write(6,*) 'SST scale_factor ', n, sst_scale(n)
    write(6,*) 'SST missing_value', n, sst_miss(n)

    write(6,*) trim(var_name), ' add_offset   ', n, var_offset(n)
    write(6,*) trim(var_name), ' scale_factor ', n, var_scale(n)
    write(6,*) trim(var_name), ' missing_value', n, var_miss(n)

    write(6,*) 'ICE add_offset   ', n, ice_offset(n)
    write(6,*) 'ICE scale_factor ', n, ice_scale(n)
    write(6,*) 'ICE missing_value', n, ice_miss(n)

  end do

  !---------------------------------------------------------------

  sst(:,:) = 0.0d0
  dat(:,:) = 0.0d0
  ice(:,:) = 0.0d0

  dat_ocn(:,:,:) = 0.0d0
  dat_ice(:,:,:) = 0.0d0
  dat_all(:,:,:) = 0.0d0

  num_ocn(:,:,:) = 0
  num_ice(:,:,:) = 0
  num_all(:,:,:) = 0

  !---------------------------------------------------------------

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

    write(6,'(1a,i6,1a,i3,1a,i4)') 'YEAR = ',nyear, ' MONTH = ', m, ' DAYS of this month = ',nday

    do nd = 1, nday
      do nh = 1, 4 ! 6-hourly

        nrecord  = nrecord + 1

        if ((nd == 1) .and. (nh == 1)) then
          nhinc = 3
        else
          nhinc = 6
        end if

        ! SST

        start(3) = nrecord
        iostat = nf_get_vara_double(ifiles(1),var(1,1),start,range,work8)
        sta(1) = sta(1) + iostat

        do j = 1, jmut
          do i = 1, imut
            if (work8(i,j) /= sst_miss(1)) then
              sst(i,jmut-j+1) = work8(i,j) * sst_scale(1) + sst_offset(1)
            else
              sst(i,jmut-j+1) = real(undef_mxe,8)
            end if
          end do
        end do

        ! Target variable

        start(3) = nrecord
        iostat = nf_get_vara_double(ifiles(1),var(1,2),start,range,work8)
        sta(1) = sta(1) + iostat

        do j = 1, jmut
          do i = 1, imut
            if (work8(i,j) /= var_miss(1)) then
              dat(i,jmut-j+1) = work8(i,j) * var_scale(1) + var_offset(1)
            else
              dat(i,jmut-j+1) = real(undef_mxe,8)
            end if
          end do
        end do

        ! Sea ice

        start(3) = nrecord
        iostat = nf_get_vara_double(ifiles(1),var(1,3),start,range,work8)
        sta(1) = sta(1) + iostat

        do j = 1, jmut
          do i = 1, imut
            if (work8(i,j) /= ice_miss(1)) then
              ice(i,jmut-j+1) = work8(i,j) * ice_scale(1) + ice_offset(1)
            else
              ice(i,jmut-j+1) = real(undef_mxe,8)
            end if
          end do
        end do

        do j = 1, jmut
          do i = 1, imut
            if (dat(i,j) /= real(undef_mxe,8)) then
              num_all(i,j,m) = num_all(i,j,m) + nhinc
              dat_all(i,j,m) = dat_all(i,j,m) + dat(i,j) * real(nhinc,8)
              if ((nd == 1) .and. (nh == 1) .and. (m > 1)) then
                num_all(i,j,m-1) = num_all(i,j,m-1) + nhinc
                dat_all(i,j,m-1) = dat_all(i,j,m-1) + dat(i,j) * real(nhinc,8)
              end if
              if (ice(i,j) /= real(undef_mxe,8)) then ! this also works as a land-sea mask
                if (ice(i,j) >= 0.55d0) then ! definition employed by JRA55
                  num_ice(i,j,m) = num_ice(i,j,m) + nhinc
                  dat_ice(i,j,m) = dat_ice(i,j,m) + dat(i,j) * real(nhinc,8)
                  if ((nd == 1) .and. (nh == 1) .and. (m > 1)) then
                    num_ice(i,j,m-1) = num_ice(i,j,m-1) + nhinc
                    dat_ice(i,j,m-1) = dat_ice(i,j,m-1) + dat(i,j) * real(nhinc,8)
                  end if
                else
                  num_ocn(i,j,m) = num_ocn(i,j,m) + nhinc
                  dat_ocn(i,j,m) = dat_ocn(i,j,m) + dat(i,j) * real(nhinc,8)
                  if ((nd == 1) .and. (nh == 1) .and. (m > 1)) then
                    num_ocn(i,j,m-1) = num_ocn(i,j,m-1) + nhinc
                    dat_ocn(i,j,m-1) = dat_ocn(i,j,m-1) + dat(i,j) * real(nhinc,8)
                  end if
                end if
              end if
            end if
          end do
        end do

      end do
    end do
  end do

  !-----------------------------------------
  ! 1st of January of the next year

  m = 12
  nhinc = 3

  if (nyear == 2015) then
    nrecord = 365 * 4
    start(3) = nrecord
    iostat = nf_get_vara_double(ifiles(1),var(1,1),start,range,work8)
    sta(1) = sta(1) + iostat
    scale_tmp  = sst_scale(1)
    offset_tmp = sst_offset(1)
  else
    nrecord = 1
    start(3) = nrecord
    iostat = nf_get_vara_double(ifiles(2),var(2,1),start,range,work8)
    sta(2) = sta(2) + iostat
    scale_tmp  = sst_scale(2)
    offset_tmp = sst_offset(2)
  end if

  do j = 1, jmut
    do i = 1, imut
      if (work8(i,j) /= var_miss(1)) then
        sst(i,jmut-j+1) = work8(i,j) * scale_tmp + offset_tmp
      else
        sst(i,jmut-j+1) = real(undef_mxe,8)
      end if
    end do
  end do

  if (nyear == 2015) then
    nrecord = 365 * 4
    start(3) = nrecord
    iostat = nf_get_vara_double(ifiles(1),var(1,2),start,range,work8)
    sta(1) = sta(1) + iostat
    scale_tmp  = var_scale(1)
    offset_tmp = var_offset(1)
  else
    nrecord = 1
    start(3) = nrecord
    iostat = nf_get_vara_double(ifiles(2),var(2,2),start,range,work8)
    sta(2) = sta(2) + iostat
    scale_tmp  = var_scale(2)
    offset_tmp = var_offset(2)
  end if

  do j = 1, jmut
    do i = 1, imut
      if (work8(i,j) /= var_miss(1)) then
        dat(i,jmut-j+1) = work8(i,j) * scale_tmp + offset_tmp
      else
        dat(i,jmut-j+1) = real(undef_mxe,8)
      end if
    end do
  end do

  if (nyear == 2015) then
    nrecord = 365 * 4
    start(3) = nrecord
    iostat = nf_get_vara_double(ifiles(1),var(1,3),start,range,work8)
    sta(1) = sta(1) + iostat
    scale_tmp  = ice_scale(1)
    offset_tmp = ice_offset(1)
  else
    nrecord = 1
    start(3) = nrecord
    iostat = nf_get_vara_double(ifiles(2),var(2,3),start,range,work8)
    sta(2) = sta(2) + iostat
    scale_tmp  = ice_scale(2)
    offset_tmp = ice_offset(2)
  end if

  do j = 1, jmut
    do i = 1, imut
      if (work8(i,j) /= ice_miss(1)) then
        ice(i,jmut-j+1) = work8(i,j) * scale_tmp + offset_tmp
      else
        ice(i,jmut-j+1) = real(undef_mxe,8)
      end if
    end do
  end do

  do j = 1, jmut
    do i = 1, imut
      if (dat(i,j) /= real(undef_mxe,8)) then
        num_all(i,j,m) = num_all(i,j,m) + nhinc
        dat_all(i,j,m) = dat_all(i,j,m) + dat(i,j) * real(nhinc,8)
        if (ice(i,j) /= real(undef_mxe,8)) then ! land-sea mask
          if (ice(i,j) >= 0.55d0) then ! definition employed by JRA55
            num_ice(i,j,m) = num_ice(i,j,m) + nhinc
            dat_ice(i,j,m) = dat_ice(i,j,m) + dat(i,j) * real(nhinc,8)
          else
            num_ocn(i,j,m) = num_ocn(i,j,m) + nhinc
            dat_ocn(i,j,m) = dat_ocn(i,j,m) + dat(i,j) * real(nhinc,8)
          end if
        end if
      end if
    end do
  end do

  !----------------------------------

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
        if (num_all(i,j,m) == 24 * nday) then
          dat_all(i,j,m) = dat_all(i,j,m) / real(num_all(i,j,m),8)
        else
          dat_all(i,j,m) = real(undef_mxe,8)
        end if
        if (num_ocn(i,j,m) >= int(frac_ratio * real(24 * nday,8) + 1.0d-6)) then
          dat_ocn(i,j,m) = dat_ocn(i,j,m) / real(num_ocn(i,j,m),8)
        else
          dat_ocn(i,j,m) = real(undef_mxe,8)
        end if
        if (num_ice(i,j,m) >= int(frac_ratio * real(24 * nday,8) + 1.0d-6)) then
          dat_ice(i,j,m) = dat_ice(i,j,m) / real(num_ice(i,j,m),8)
        else
          dat_ice(i,j,m) = real(undef_mxe,8)
        end if
      end do
    end do

    write(file_out,'(1a,i4.4,i2.2)') trim(file_out_base_all),nyear,m
    open(mtot1,file=file_out,form='unformatted',access='direct',recl=4*imut*jmut)
    write(6,*) trim(var_name),' written to ',trim(file_out)
    ireco1 = 0
    ireco1 = ireco1 + 1
    write(mtot1,rec=ireco1) real(dat_all(1:imut,1:jmut,m),4)
    close(mtot1)

    write(file_out,'(1a,i4.4,i2.2)') trim(file_out_base_ocn),nyear,m
    open(mtot1,file=file_out,form='unformatted',access='direct',recl=4*imut*jmut)
    write(6,*) trim(var_name),' written to ',trim(file_out)
    ireco1 = 0
    ireco1 = ireco1 + 1
    write(mtot1,rec=ireco1) real(dat_ocn(1:imut,1:jmut,m),4)
    close(mtot1)

    write(file_out,'(1a,i4.4,i2.2)') trim(file_out_base_ice),nyear,m
    open(mtot1,file=file_out,form='unformatted',access='direct',recl=4*imut*jmut)
    write(6,*) trim(var_name),' written to ',trim(file_out)
    ireco1 = 0
    ireco1 = ireco1 + 1
    write(mtot1,rec=ireco1) real(dat_ice(1:imut,1:jmut,m),4)
    close(mtot1)

  end do

  do n = 1, nfiles_tmp
    iostat = nf_close(ifiles(n))
    if (iostat /= 0) then
      write(6,*) 'nf_close error for file number ',n, ifiles(n), iostat
    end if
  end do

end program read_netcdf_output_grads_6h2m
