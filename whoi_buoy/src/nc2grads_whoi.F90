! -*-F90-*-
!==================================================================
program read_netcdf_output_grads
  !----------------------------------------------------------------
  ! Information:
  !   Create direct access file (GrADS) of the WHOI-Buoy data
  !----------------------------------------------------------------

  use libmxe_calendar

  implicit none

  include '/usr/local/netcdf/include/netcdf.inc'

  integer(4), parameter :: imut = 1, jmut = 1, km = 1
  integer(4), parameter :: mtot1 = 91, mtot2 = 92

  real(8) :: time_var
  real(8) :: time_factor
  real(8) :: day_from_start
  integer(4) :: iday_from_start

  real(8) :: dat8a, dat8b
  real(8) :: rad_day, rad_mon
  real(8) :: qrad
  integer(4) :: nvalid_mon, nvalid_day
  real(8) :: lon, lat

  integer(4) :: idmon(12)
  character(3) :: nammon(12)

  character(128) :: base_dir
  character(128) :: out_dir
  character(128) :: buoy
  character(128) :: location
  character(128) :: element_out
  character(128) :: interval
  character(128) :: flot1, flot2

  integer(4) :: i, j, k, l, m, n

  integer(4) :: ndcount, nhcount
  integer(4) :: ireco1, ireco2
  integer(4) :: nyear, month, day, hour, minute, second
  integer(4) :: mst, med
  integer(4) :: num_data
  integer(4) :: stmon, styear

  real(4),parameter :: undef_whoi = 1.e35
  real(4),parameter :: undef_mxe  = -9.99e33
  real(4) :: undef_in

  ! for netCDF

  integer(4), parameter :: nvars = 4, nfiles = 1
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
  character(len=nf_max_name) :: file_in
  character(len=nf_max_name) :: var_nam_in
  integer(4) :: unit_nam_len

  type(type_calendar) :: start_date, current_date, prev_date
  type(type_calendar) :: caladd

  !--------------------------------------------------------------------

  namelist /nml_buoyrad_mon/ base_dir, out_dir, buoy, file_in, &
       & undef_in, var_nam_in, element_out

  !--------------------------------------------------------------------

  undef_in = undef_whoi
  read(5,nml=nml_buoyrad_mon)

  ! general settings

  idmon(1:12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
  nammon(1:12) = (/ 'JAN','FEB','MAR','APR','MAY','JUN',&
                  & 'JUL','AUG','SEP','OCT','NOV','DEC'/)
  TIME_DIM = 'TIME'
  LON_DIM  = 'LON'
  LAT_DIM  = 'LAT'
  DEP_DIM  = 'DEPTH'

  !---------------------------------------------------------------------
  ! open netcdf file

  write(flnin(1),'(5a)') trim(base_dir),'/',trim(buoy),'/',trim(file_in)

  write(6,*) ' read from ', trim(flnin(1))

  do n = 1, nfiles
    staf(n) = nf_open(flnin(n),nf_nowrite,ifiles(n)) 
    if (staf(n) /= 0) then
      write(6,*) 'nf_open error for file number ',n
      stop
    end if
    staf(n) = nf_inq_dimid(ifiles(n),TIME_DIM,timid(n))
    staf(n) = nf_inq_dimname(ifiles(n),timid(n),TIME_NAM)
    staf(n) = nf_inq_dimlen (ifiles(n),timid(n),TIME_LEN)
    write(6,*) 'TIME_NAME = ', trim(TIME_NAM)
    write(6,*) 'TIME_LEN  = ', TIME_LEN
  end do

  sta(1) = nf_inq_varid(ifiles(1), var_nam_in  ,var(1))
  sta(2) = nf_inq_varid(ifiles(1), TIME_NAM    ,var(2))
  sta(3) = nf_inq_varid(ifiles(1), 'LONGITUDE' ,var(3))
  sta(4) = nf_inq_varid(ifiles(1), 'LATITUDE'  ,var(4))


  do n = 1, nvars
    if (sta(n) /= nf_noerr) then
      write(6,*) 'nf_inq_varid error for variable ',n, var(n)
      stop
    end if
  end do

  sta(3) = nf_get_vara_double(ifiles(1),var(3),1,1,lon)
  sta(4) = nf_get_vara_double(ifiles(1),var(4),1,1,lat)


  if (lon > 0.0d0) then
    if (lat > 0.0d0) then 
      write(location,'(i2.2,1a,i3.3,1a)') int(abs(lat)),'n',int(abs(lon)),'e'
    else
      write(location,'(i2.2,1a,i3.3,1a)') int(abs(lat)),'s',int(abs(lon)),'e'
    end if
  else
    if (lat > 0.0d0) then 
      write(location,'(i2.2,1a,i3.3,1a)') int(abs(lat)),'n',int(abs(lon)),'w'
    else
      write(location,'(i2.2,1a,i3.3,1a)') int(abs(lat)),'s',int(abs(lon)),'w'
    end if
    lon = lon + 360.d0
  end if

  sta(2) = nf_inq_attlen(ifiles(1), var(2), 'units' , unit_nam_len)
  write(6,*) 'length of TIME_UNIT =', unit_nam_len
  sta(2) = nf_get_att_text(ifiles(1), var(2), 'units', TIME_UNIT)
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

  nvalid_day = 0
  rad_day = 0.0d0
  nvalid_mon = 0
  rad_mon = 0.0d0

  nhcount = 0
  ndcount = 0

  num_data = 0

  do n = 1, TIME_LEN

    ndcount = ndcount + 1

    sta(2) = nf_get_vara_double(ifiles(1),var(2),n,1,time_var)

    day_from_start = time_var * time_factor
    iday_from_start = int(day_from_start + 1.d-6)
    caladd%year = 0
    caladd%month = 0
    caladd%day = iday_from_start
    caladd%hour = 0
    caladd%minute = 0
    caladd%second = 0

    prev_date = current_date
    current_date = libmxe_calendar__addcal( start_date, caladd, l_leap=.true. )

    !write(6,*) current_date ! check

    if (n > 1) then

      if ((current_date%day /= prev_date%day) .or. (n == TIME_LEN)) then

        if (nvalid_day == 24) then
          rad_day = rad_day / real(nvalid_day,8)
          rad_mon = rad_mon + rad_day
          nvalid_mon = nvalid_mon + 1
        else
          rad_day = real(undef_mxe,8)
        end if
        nvalid_day = 0
        rad_day = 0

        if ((current_date%month /= prev_date%month) .or. (n == TIME_LEN)) then

          num_data = num_data + 1
          if (num_data == 1) then
            stmon = prev_date%month
            styear = prev_date%year
          end if

          if (nvalid_mon > 15) then
            rad_mon = rad_mon / real(nvalid_mon,8)
          else
            rad_mon = real(undef_mxe,8)
          end if

          write(6,*) prev_date%year, prev_date%month, rad_mon, nvalid_mon
        
          ! open output file
        
          write(flot1,'(8a,i4.4,i2.2)') &
             & trim(base_dir),'/',trim(out_dir),'/',&
             & trim(element_out),'_',trim(location),'.', &
             & prev_date%year,prev_date%month
          open(mtot1,file=flot1,form='unformatted',access='direct',recl=4)
          write(6,*) 'Downward radiation written to ',trim(flot1)
          ireco1 = 0
          ireco1 = ireco1 + 1
          write(mtot1,rec=ireco1) real(rad_mon,4)
          close(mtot1)

          nvalid_mon = 0
          rad_mon = 0.0d0
        end if
      end if
    end if

    sta(1) = nf_get_vara_double(ifiles(1),var(1),n,1,dat8a)

    if (dat8a /= real(undef_whoi,8)) then
      rad_day = rad_day + dat8a
      nvalid_day = nvalid_day + 1
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
  write(mtot2,*) 'location = ', trim(location)
  write(mtot2,*) 'stmon = ', trim(nammon(stmon))
  write(mtot2,*) 'styear = ', styear
  write(mtot2,*)
  close(mtot2)
  
end program read_netcdf_output_grads
