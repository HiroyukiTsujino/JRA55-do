! -*-F90-*-
!==================================================================
program read_netcdf_output_grads
  !----------------------------------------------------------------
  ! Information:
  !   Create direct access file (GrADS) of the TAO-Buoy data
  !----------------------------------------------------------------

  use libmxe_calendar

  implicit none

  include '/usr/local/netcdf/include/netcdf.inc'

  integer(4), parameter :: imut = 1, jmut = 1, km = 1
  integer(4), parameter :: mtot1 = 91, mtot2 = 92

  real(4) :: dat4a(imut,jmut,km), dat4b(imut,jmut,km)
  real(4) :: time_var
  real(8) :: time_new, time_old, time_factor
  real(8) :: day_from_start
  integer(4) :: iday_from_start
  real(8) :: dat8(imut,jmut)

  real(8) :: rad(imut,jmut)
  real(8) :: qrad(imut,jmut)
  integer(4) :: nvalid(imut,jmut)
  real(4) :: work4(imut,jmut)
  real(4) :: lon,lat

  integer(4) :: idmon(12)
  character(3) :: nammon(12)

  character(128) :: base_dir
  character(128) :: out_dir
  character(128) :: buoy
  character(128) :: location
  character(128) :: element_in, element_out
  character(128) :: interval
  character(128) :: flot1, flot2

  integer(4) :: i, j, k, l, m, n

  integer(4) :: ndcount
  integer(4) :: ireco1, ireco2
  integer(4) :: nyear, month, day, hour, minute, second
  integer(4) :: mst, med
  integer(4) :: num_data
  integer(4) :: stmon, styear

  real(4),parameter :: undef_tao = 1.e35
  real(4),parameter :: undef_mxe = -9.99e33
  real(4) :: undef_in

  ! for netCDF

  integer(4), parameter :: nvars = 5, nfiles = 1
  integer(4) :: staf(nfiles)
  integer(4) :: var(nvars), sta(nvars)
  integer(4) :: start(4), range(4)
  character(128) :: flnin(nfiles)
  integer(4) :: ifiles(nfiles)
  integer(4) :: xid(nfiles)
  integer(4) :: yid(nfiles)
  integer(4) :: timid(nfiles)
  character(len=nf_max_name) :: TIME_DIM, LON_DIM, LAT_DIM, DEP_DIM
  character(len=nf_max_name) :: TIME_NAM, LON_NAM, LAT_NAM, DEP_NAM
  integer(4) :: TIME_LEN, LON_LEN, LAT_LEN, DEP_LEN
  character(len=1),allocatable :: TIME_UNIT_tmp(:)
  character(len=nf_max_name) :: TIME_UNIT
  integer(4) :: unit_nam_len
  character(len=nf_max_name) :: var_nam, qvar_nam

  type(type_calendar) :: start_date, current_date, prev_date
  type(type_calendar) :: caladd

  !--------------------------------------------------------------------

  namelist /nml_buoyrad_mon/ base_dir, out_dir, &
       & buoy, location, undef_in, &
       & var_nam, qvar_nam, &
       & element_in, element_out, interval

  !--------------------------------------------------------------------

  undef_in = undef_tao
  read(5,nml=nml_buoyrad_mon)

  ! general settings

  idmon(1:12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
  nammon(1:12) = (/ 'JAN','FEB','MAR','APR','MAY','JUN',&
                  & 'JUL','AUG','SEP','OCT','NOV','DEC'/)
  TIME_DIM = 'time'
  LON_DIM  = 'lon'
  LAT_DIM  = 'lat'
  DEP_DIM  = 'depth'

  !---------------------------------------------------------------------
  ! open netcdf file

  write(flnin(1),'(8a)') &
       & trim(base_dir),'/',trim(buoy),'/',&
       & trim(element_in),trim(location),trim(interval),'.cdf'

  write(6,*) ' read from ', flnin(1)

  do n = 1, nfiles
    staf(n) = nf_open(flnin(n),nf_nowrite,ifiles(n)) 
    if (staf(n) /= 0) then
      write(6,*) 'nf_open error for file number ',n
      stop
    end if
    staf(n) = nf_inq_dimid(ifiles(n),TIME_DIM,timid(n))
    staf(n) = nf_inq_dimname(ifiles(n),timid(n),TIME_NAM)
    staf(n) = nf_inq_dimlen (ifiles(n),timid(n),TIME_LEN)
    staf(n) = nf_inq_dimid(ifiles(n),LON_DIM,xid(n))
    staf(n) = nf_inq_dimname(ifiles(n),xid(n),LON_NAM)
    staf(n) = nf_inq_dimid(ifiles(n),LAT_DIM,yid(n))
    staf(n) = nf_inq_dimname(ifiles(n),yid(n),LAT_NAM)
    write(6,*) 'TIME_NAME = ', trim(TIME_NAM)
    write(6,*) 'TIME_LEN  = ', TIME_LEN
  end do

  start(1:4) = (/ 1, 1, 1, 1 /)
  range(1:4) = (/ 1, 1, 1, 1 /)

  ndcount = 0

  sta(1) = nf_inq_varid(ifiles(1), var_nam   ,var(1))
  sta(2) = nf_inq_varid(ifiles(1), qvar_nam  ,var(2))
  sta(3) = nf_inq_varid(ifiles(1), TIME_NAM  ,var(3))
  sta(4) = nf_inq_varid(ifiles(1), LON_NAM   ,var(4))
  sta(5) = nf_inq_varid(ifiles(1), LAT_NAM   ,var(5))


  do n = 1, nvars
    if (sta(n) /= nf_noerr) then
      write(6,*) 'nf_inq_varid error for variable ',n, var(n)
      stop
    end if
  end do

  sta(4) = nf_get_vara_real(ifiles(1),var(4),1,1,lon)
  sta(5) = nf_get_vara_real(ifiles(1),var(5),1,1,lat)

  sta(3) = nf_inq_attlen(ifiles(1), var(3), 'units' , unit_nam_len)
  write(6,*) 'length of TIME_UNIT =', unit_nam_len
  sta(3) = nf_get_att_text(ifiles(1), var(3), 'units', TIME_UNIT)
  write(6,*) ' TIME UNIT = ', TIME_UNIT(1:unit_nam_len)

  if (TIME_UNIT(1:3)=='day') then
    time_factor = 1.0
  else
    write(6,*) ' time unit ', TIME_UNIT(1:3),&
         & ' is not supported, terminating... '
    stop
  end if

  do m = 1, unit_nam_len - 4
    if (TIME_UNIT(m:m+4)=='since')  then
      read(TIME_UNIT(m+6:m+9),  *) nyear
      read(TIME_UNIT(m+11:m+12),*) month
      read(TIME_UNIT(m+14:m+15),*) day
      read(TIME_UNIT(m+17:m+18),*) hour
      read(TIME_UNIT(m+20:m+21),*) minute
      read(TIME_UNIT(m+23:m+24),*) second
      exit
    end if
  end do

  start_date%year   = nyear
  start_date%month  = month
  start_date%day    = day
  start_date%hour   = hour
  start_date%minute = minute
  start_date%second = second

  write(6,'(1a,6i6)') 'reference date : ', start_date

  current_date = start_date

  nvalid(1,1) = 0
  rad(1,1) = 0.0d0

  ndcount = 0

  num_data = 0

  do n = 1, TIME_LEN

    ndcount = ndcount + 1
    start(4) = ndcount

    sta(3) = nf_get_vara_real(ifiles(1),var(3),n,1,time_var)

    day_from_start = real(time_var,8) * time_factor
    iday_from_start = int(day_from_start + 1.d-6)
    caladd%year = 0
    caladd%month = 0
    caladd%day = iday_from_start
    caladd%hour = 0
    caladd%minute = 0
    caladd%second = 0

    prev_date = current_date
    current_date = libmxe_calendar__addcal( start_date, caladd, l_leap=.true. )

    if (n > 1) then
      if ((current_date%month /= prev_date%month) .or. (n == TIME_LEN)) then

        num_data = num_data + 1
        if (num_data == 1) then
          stmon = prev_date%month
          styear = prev_date%year
        end if

        if (nvalid(1,1) > 15) then
          rad(1,1) = rad(1,1) / real(nvalid(1,1),8)
        else
          rad(1,1) = real(undef_mxe,8)
        end if

        write(6,*) prev_date%year, prev_date%month, rad(1,1), nvalid(1,1)
        
        ! open output file
        
        write(flot1,'(8a,i4.4,i2.2)') &
             & trim(base_dir),'/',trim(out_dir),'/',&
             & trim(element_out),'_',trim(location),'.', &
             & prev_date%year,prev_date%month
        open(mtot1,file=flot1,form='unformatted',access='direct',recl=4)
        write(6,*) 'Downward radiation written to ',trim(flot1)
        ireco1 = 0
        ireco1 = ireco1 + 1
        write(mtot1,rec=ireco1) real(rad(1,1),4)
        close(mtot1)

        nvalid(1,1) = 0
        rad(1,1) = 0.0d0
      end if
    end if

    sta(2) = nf_get_vara_real(ifiles(1),var(2),start,range,dat4b)
    sta(1) = nf_get_vara_real(ifiles(1),var(1),start,range,dat4a)

    if (dat4a(1,1,1) /= undef_in) then
      rad(1,1) = rad(1,1) + real(dat4a(1,1,1),8)
      nvalid(1,1) = nvalid(1,1) + 1
    end if

  end do

  do n = 1, nfiles
    sta(n) = nf_close(ifiles(n))
    if (sta(n) /= 0) then
      write(6,*) 'nf_close error for file number ',n, ifiles(n), sta(n)
    end if
  end do

  write(flot2,'(1a)') 'tmp.txt'
  open(mtot2,file=flot2,form='formatted')
  write(6,*) 'Data attributes written to ',trim(flot2)
  write(mtot2,*) 'lon = ',lon
  write(mtot2,*) 'lat = ',lat
  write(mtot2,*) 'numdat = ', num_data
  write(mtot2,*) 'stmon = ', trim(nammon(stmon))
  write(mtot2,*) 'styear = ', styear
  write(mtot2,*)
  close(mtot2)
  
end program read_netcdf_output_grads
