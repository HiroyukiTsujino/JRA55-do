! -*-F90-*-
!==================================================================
program read_netcdf_output_grads_d2m
  !----------------------------------------------------------------
  ! Information:
  !   Create direct access file (GrADS) of
  !     the NOCS surface data
  !----------------------------------------------------------------

  implicit none

  include '/usr/local/netcdf/include/netcdf.inc'

  integer(4), parameter :: imut = 480, jmut = 241
  integer(4), parameter :: mtot1 = 91, mtot2 = 92

  real(4) :: dat4(imut,jmut)
  real(8) :: dat8(imut,jmut)
  integer(4) :: ho4(imut,jmut)

  real(8) :: ci(imut,jmut,12)
  real(8) :: dat(imut,jmut,12)

  integer(4) :: num_valid_ci(imut,jmut,12)
  integer(4) :: num_valid_var(imut,jmut,12)

  real(8) :: work8(imut,jmut)


  integer(4) :: idmon(12)

  character(128) :: flot1, flot2

  integer(4) :: i, j, k, l, m, n

  integer(4) :: nrecord, nhrcount, nhinc
  integer(4) :: ireco1, ireco2
  integer(4) :: mst, med
  integer(4) :: nday, nd, nh

  real(4),parameter :: undef_era40 = -32767.0
  real(4),parameter :: undef_mxe = -9.99e33

  integer(4) :: nyear
  character(128) :: var_name
  character(128) :: file_in1, file_in2
  character(128) :: file_mask
  character(128) :: file_out_base, file_out
  character(128) :: file_ice_base, file_ice

  logical :: l_mask_out, l_ice_out

  ! for netCDF

  integer(4), parameter :: nvars = 2, nfiles = 2
  integer(4) :: var(nfiles,nvars), sta(nfiles)
  integer(4) :: iostat
  integer(4) :: start(3), range(3)
  character(128) :: flnin(nfiles)
  integer(4) :: ifiles(nfiles)

  real(8) :: ci_offset(nfiles), ci_scale(nfiles), ci_miss(nfiles)
  real(8) :: var_offset(nfiles), var_scale(nfiles), var_miss(nfiles)

  !--------------------------------------------------------------------

  namelist /nml_era40_org2mon/ nyear, var_name, &
       & file_in1, file_in2, file_out_base, &
       & l_ice_out, file_ice_base, &
       & l_mask_out, file_mask

  !--------------------------------------------------------------------
  
  l_ice_out = .false.
  open(10,file='namelist.era40_org2mon')
  read(10,nml=nml_era40_org2mon)
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

  do n = 1, nfiles

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

    iostat = nf_inq_varid(ifiles(n),var_name,var(n,2))
    if (iostat /= 0) then
      write(6,*) 'nf_inq_varid error for variable 2 in file', n, iostat
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

    write(6,*) 'CI add_offset   ', n, ci_offset(n)
    write(6,*) 'CI scale_factor ', n, ci_scale(n)
    write(6,*) 'CI missing_value', n, ci_miss(n)

    write(6,*) trim(var_name), ' add_offset   ', n, var_offset(n)
    write(6,*) trim(var_name), ' scale_factor ', n, var_scale(n)
    write(6,*) trim(var_name), ' missing_value', n, var_miss(n)

  end do

  ci(:,:,:) = 0.0d0
  num_valid_ci(:,:,:) = 0

  dat(:,:,:) = 0.0d0
  num_valid_var(:,:,:) = 0

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
            if (work8(i,j) /= ci_miss(1)) then
              num_valid_ci(i,jmut-j+1,m) = &
                   & num_valid_ci(i,jmut-j+1,m) + nhinc
              ci(i,jmut-j+1,m) = ci(i,jmut-j+1,m) &
                   & + (work8(i,j) * ci_scale(1) + ci_offset(1)) * real(nhinc,8)
              if ((nd == 1) .and. (nh == 1) .and. (m > 1)) then
                num_valid_ci(i,jmut-j+1,m-1) = &
                     & num_valid_ci(i,jmut-j+1,m-1) + nhinc
                ci(i,jmut-j+1,m-1) = ci(i,jmut-j+1,m-1) &
                     & + (work8(i,j) * ci_scale(1) + ci_offset(1)) * real(nhinc,8)
              end if
            end if
          end do
        end do

        start(3) = nrecord
        iostat = nf_get_vara_double(ifiles(1),var(1,2),start,range,work8)
        sta(1) = sta(1) + iostat

        do j = 1, jmut
          do i = 1, imut
            if (work8(i,j) /= var_miss(1)) then
              num_valid_var(i,jmut-j+1,m) = &
                   & num_valid_var(i,jmut-j+1,m) + nhinc
              dat(i,jmut-j+1,m) = dat(i,jmut-j+1,m) &
                   & + (work8(i,j) * var_scale(1) + var_offset(1)) * real(nhinc,8)
              if ((nd == 1) .and. (nh == 1) .and. (m > 1)) then
                num_valid_var(i,jmut-j+1,m-1) = &
                     & num_valid_var(i,jmut-j+1,m-1) + nhinc
                dat(i,jmut-j+1,m-1) = dat(i,jmut-j+1,m-1) &
                     & + (work8(i,j) * var_scale(1) + var_offset(1)) * real(nhinc,8)
              end if
            end if
          end do
        end do

      end do
    end do
  end do

  ! 1st of January of the next year

  m = 12
  nrecord = 1
  nhinc = 3

  start(3) = nrecord
  iostat = nf_get_vara_double(ifiles(2),var(2,1),start,range,work8)
  sta(2) = sta(2) + iostat

  do j = 1, jmut
    do i = 1, imut
      if (work8(i,j) /= ci_miss(2)) then
        num_valid_ci(i,jmut-j+1,m) = num_valid_ci(i,jmut-j+1,m) + nhinc
        ci(i,jmut-j+1,m) = ci(i,jmut-j+1,m) &
             & + (work8(i,j) * ci_scale(2) + ci_offset(2)) * real(nhinc,8)
      end if
    end do
  end do

  start(3) = nrecord
  iostat = nf_get_vara_double(ifiles(2),var(2,2),start,range,work8)
  sta(2) = sta(2) + iostat

  do j = 1, jmut
    do i = 1, imut
      if (work8(i,j) /= var_miss(2)) then
        num_valid_var(i,jmut-j+1,m) = num_valid_var(i,jmut-j+1,m) + nhinc
        dat(i,jmut-j+1,m) = dat(i,jmut-j+1,m) &
             & + (work8(i,j) * var_scale(2) + var_offset(2)) * real(nhinc,8)
      end if
    end do
  end do

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
        if (num_valid_ci(i,j,m) == 24 * nday) then
          ci(i,j,m) = ci(i,j,m) / real(num_valid_ci(i,j,m),8)
        else
          ci(i,j,m) = real(undef_mxe,8)
        end if
        if (num_valid_var(i,j,m) == 24 * nday) then
          dat(i,j,m) = dat(i,j,m) / real(num_valid_var(i,j,m),8)
        else
          dat(i,j,m) = real(undef_mxe,8)
        end if
      end do
    end do

    write(6,'(1a,f12.5,2i6)') &
         & 'ci ',         ci(230,121,m), num_valid_ci(230,121,m), 24 * nday
    write(6,'(1a,f12.5,2i6)') &
         & trim(var_name), dat(230,121,m), num_valid_var(230,121,m), 24 * nday

    ! open output file

    write(file_out,'(1a,i4.4,i2.2)') trim(file_out_base),nyear,m
    open(mtot1,file=file_out,form='unformatted',access='direct',recl=4*imut*jmut)
    write(6,*) trim(var_name),' written to ',trim(file_out)
    ireco1 = 0
    ireco1 = ireco1 + 1
    write(mtot1,rec=ireco1) real(dat(1:imut,1:jmut,m),4)
    close(mtot1)

    if (l_ice_out) then
      write(file_ice,'(1a,i4.4,i2.2)') trim(file_ice_base),nyear,m
      open(mtot2,file=file_ice,form='unformatted',access='direct',recl=4*imut*jmut)
      write(6,*) 'sea ice written to ',trim(file_ice)
      write(mtot2,rec=1) real(ci(1:imut,1:jmut,m),4)
      close(mtot2)
    end if

  end do

  do n = 1, nfiles
    iostat = nf_close(ifiles(n))
    if (iostat /= 0) then
      write(6,*) 'nf_close error for file number ',n, ifiles(n), iostat
    end if
  end do

  if (l_mask_out) then
    do j = 1, jmut
      do i = 1, imut
        if (num_valid_ci(i,j,1) == 24 * 31) then
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
