! -*-F90-*-
!==================================================================
program read_netcdf_output_grads
  !----------------------------------------------------------------
  ! Information:
  !   Create direct access file (GrADS) of
  !     the 20CRv3 surface data
  !----------------------------------------------------------------

  implicit none

  include 'netcdf.inc'

  integer(4), parameter :: imut = 360, jmut = 181
  integer(4), parameter :: mtot1 = 91, mtot2 = 92
  integer(4), parameter :: start_year = 1836
  integer(4), parameter :: start_month= 1

  real(4) :: dat4(imut,jmut)
  real(8) :: dat8(imut,jmut)

  real(4) :: dat4x(imut)
  real(4) :: dat4y(jmut)

  real(4) :: qair(imut,jmut)
  real(4) :: work4(imut,jmut)

  integer(4) :: idmon(12)

  character(128) :: flot1, flot2

  integer(4) :: i, j, k, l, m, n

  integer(4) :: ncount
  integer(4) :: ireco1, ireco2
  integer(4) :: nyear, nmon, nday, nh
  integer(4) :: monday

  real(4),parameter :: undef_20CRv3 = -9.96921e36
  real(4),parameter :: undef_mxe = -9.99e33

  ! for netCDF

  integer(4), parameter :: nvars = 1, nfiles = 1
  integer(4) :: var(nvars), sta(nvars)
  integer(4) :: start(3), range(3)
  character(128) :: flnin(nfiles)
  integer(4) :: ifiles(nfiles)

  integer(4) :: stalat, varlat
  character(128) :: var_lat
  integer(4) :: starty(1), rangey(1)

  integer(4) :: stalon, varlon
  character(128) :: var_lon
  integer(4) :: startx(1), rangex(1)

  character(128) :: file_in_base, file_out_base
  character(128) :: var_in, var_out 
  integer(4) :: ibyr, ieyr
  logical :: l_leap

  !--------------------------------------------------------------------

  namelist /nml_20CRv3_surf/ &
       & file_in_base, file_out_base, &
       & ibyr, ieyr, &
       & var_in, var_out 

  !--------------------------------------------------------------------

  open (10,file='namelist.20CRv3_surf')
  read (10,nml=nml_20CRv3_surf)
  close(10)

  var_lon='lon'
  var_lat='lat'

  !--------------------------------------------------------------------
  ! general settings

  idmon(1:12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  !---------------------------------------------------------------------
  ! main year

  start(1:3) = (/ 1, 1, 1 /)
  range(1:3) = (/ 360, 181, 1 /)

  startx(1) = 1
  rangex(1) = 360

  starty(1) = 1
  rangey(1) = 181

  !--------------------------------------------------------------------

  do nyear = ibyr, ieyr

    write(flnin(1),'(1a,i4.4,1a)') trim(file_in_base),nyear,'.nc'
    write(6,*) 'reading from ', trim(flnin(1))

    ! open netcdf file
    do n = 1, nfiles
      sta(n) = nf_open(trim(flnin(n)),nf_nowrite,ifiles(n)) 
      ncount = 0
      if (sta(n) /= 0) then
        write(6,*) 'nf_open error for file number ',n
        stop
      end if
    end do

    sta(1) = nf_inq_varid(ifiles(1),trim(var_in),var(1))

    do n = 1, nvars
      if (sta(n) /= 0) then
        write(6,*) 'nf_inq_varid error for variable ',n, var(n)
        stop
      end if
    end do

    stalon = nf_inq_varid(ifiles(1),trim(var_lon),varlon)
    stalon = nf_get_vara_real(ifiles(1),varlon,startx,rangex,dat4x)

    do i = 1, imut
      write(6,*) dat4x(i)
    end do

    stalat = nf_inq_varid(ifiles(1),trim(var_lat),varlat)
    stalat = nf_get_vara_real(ifiles(1),varlat,starty,rangey,dat4y)

    do j = 1, jmut
      write(6,*) dat4y(j)
    end do

    ! open output file
    write(flot1,'(1a,1a,1a,1a,i4.4)') &
         & trim(file_out_base),'/',trim(var_out),'.',nyear
    open(mtot1,file=trim(flot1),form='unformatted',access='direct',recl=4*imut*jmut)
    write(6,*) 'DATA written to ',trim(flot1)
    ireco1 = 0

    l_leap = .false.
    if (mod(nyear,4) == 0) then
      if (mod(nyear,100) == 0) then
        if (mod(nyear,400) == 0) then
          l_leap = .true.
        else
          l_leap = .false.
        end if
      else
        l_leap = .true.
      end if
    else
      l_leap = .false.
    end if

    do nmon = 1, 12

      if (nmon == 2) then
        if (l_leap) then
          monday = idmon(nmon) + 1
        else
          monday = idmon(nmon)
        end if
      else
        monday = idmon(nmon)
      end if

      do nday = 1, monday

        write(6,*) nmon, nday

        do nh = 1, 8

          ncount = ncount + 1

          start(3) = ncount
          sta(1) = nf_get_vara_real(ifiles(1),var(1),start,range,dat4)

          do n = 1, nvars
            if (sta(n) /= 0) then
              write(6,*) 'nf_get_vara_real error for variable ',n, var(n)
              stop
            end if
          end do

          do j = 1, jmut
            do i = 1, imut
              qair(i,j) = real(dat4(i,j),4)
            end do
          end do
        
          do j = 1, jmut
            do i = 1, imut
              if (qair(i,j) == undef_20CRv3) then
                qair(i,j) = undef_mxe
              end if
            end do
          end do
          
          ireco1 = ireco1 + 1
          write(mtot1,rec=ireco1) qair

        end do
      end do
    end do

    write(6,*) ' written ', ireco1, ' records '
    close(mtot1)

    do n = 1, nfiles
      !write(6,*) ifiles(n)
      sta(n) = nf_close(ifiles(n))
      if (sta(n) /= 0) then
        write(6,*) 'nf_close error for file number ',n, ifiles(n), sta(n)
      end if
    end do

  end do

end program read_netcdf_output_grads
