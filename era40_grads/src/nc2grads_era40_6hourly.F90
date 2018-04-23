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

  real(4) :: dat4(imut,jmut)
  real(8) :: dat8(imut,jmut)

  real(8) :: ci(imut,jmut)
  real(8) :: dat(imut,jmut)

  real(8) :: work8(imut,jmut)

  integer(4) :: idmon(12)

  integer(4) :: i, j, k, l, m, n

  integer(4) :: nrecord, nhrcount
  integer(4) :: ireco1, ireco2, ireco3, ireco4
  integer(4) :: mst, med
  integer(4) :: nday, nd, nh

  real(4),parameter :: undef_era40 = -32767.0
  real(4),parameter :: undef_mxe = -9.99e33

  integer(4) :: nyear
  character(128) :: var_name
  character(128) :: file_in1
  character(128) :: file_out_year
  character(128) :: file_out_s1, file_out_s2, file_out_e1

  ! for netCDF

  integer(4), parameter :: nvars = 2, nfiles = 1
  integer(4) :: var(nfiles,nvars), sta(nfiles)
  integer(4) :: iostat
  integer(4) :: start(3), range(3)
  character(128) :: flnin(nfiles)
  integer(4) :: ifiles(nfiles)

  real(8) :: ci_offset(nfiles), ci_scale(nfiles), ci_miss(nfiles)
  real(8) :: var_offset(nfiles), var_scale(nfiles), var_miss(nfiles)

  !--------------------------------------------------------------------

  namelist /nml_era40_org2grads/ nyear, var_name, &
       & file_in1, file_out_year, &
       & file_out_s1, file_out_s2, file_out_e1

  !--------------------------------------------------------------------
  
  open(10,file='namelist.era40_org2grads')
  read(10,nml=nml_era40_org2grads)
  close(10)

  !--------------------------------------------------------------------
  ! general settings

  idmon(1:12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  !---------------------------------------------------------------------

  flnin(1)=file_in1

  start(1:3) = (/ 1, 1, 1 /)
  range(1:3) = (/ 480, 241, 1 /)


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

  ! open output file

  open(mtot1,file=file_out_year,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) trim(var_name),' written to ',trim(file_out_year)
  ireco1 = 0

  open(mtot2,file=file_out_s1,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) trim(var_name),' written to ',trim(file_out_s1)
  ireco2 = 0

  open(mtot3,file=file_out_s2,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) trim(var_name),' written to ',trim(file_out_s2)
  ireco3 = 0

  open(mtot4,file=file_out_e1,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) trim(var_name),' written to ',trim(file_out_e1)
  ireco4 = 0

  ci (:,:) = 0.0d0
  dat(:,:) = 0.0d0

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

        start(3) = nrecord
        iostat = nf_get_vara_double(ifiles(1),var(1,1),start,range,work8)
        sta(1) = sta(1) + iostat

        do j = 1, jmut
          do i = 1, imut
            if (work8(i,j) /= ci_miss(1)) then
              ci(i,jmut-j+1) = work8(i,j) * ci_scale(1) + ci_offset(1)
            else
              ci(i,jmut-j+1) = undef_mxe
            end if
          end do
        end do

        start(3) = nrecord
        iostat = nf_get_vara_double(ifiles(1),var(1,2),start,range,work8)
        sta(1) = sta(1) + iostat

        do j = 1, jmut
          do i = 1, imut
            if (work8(i,j) /= var_miss(1)) then
              dat(i,jmut-j+1) = work8(i,j) * var_scale(1) + var_offset(1)
            else
              dat(i,jmut-j+1) = undef_mxe
            end if
          end do
        end do

        ireco1 = ireco1 + 1
        write(6,*) ' org ', nrecord, ' grads ', ireco1
        write(mtot1,rec=ireco1) real(dat(1:imut,1:jmut),4)

        if ((m == 1) .and. (nd == 1) .and. (nh == 1)) then
          write(mtot2,rec=1) real(dat(1:imut,1:jmut),4)
          write(6,*) ' Closing ', trim(file_out_s1)
          close(mtot2)
        end if

        if ((m == 1) .and. (nd == 1) .and. (nh == 2)) then
          write(mtot3,rec=1) real(dat(1:imut,1:jmut),4)
          write(6,*) ' Closing ', trim(file_out_s2)
          close(mtot3)
        end if

        if ((m == 12) .and. (nd == 31) .and. (nh == 4)) then
          write(mtot4,rec=1) real(dat(1:imut,1:jmut),4)
          write(6,*) ' Closing ', trim(file_out_e1)
          close(mtot4)
        end if

      end do
    end do
  end do

  close(mtot1)

end program read_netcdf_output_grads
