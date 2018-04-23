!-*-F90-*-
Program  qscat_rss_to_daily
	
  use file_open_close_manager

  implicit none

  character(len=256) :: filename

  integer(4), parameter :: xgrid = 1440, ygrid = 720
  real(4)    :: windspd(xgrid,ygrid,2)
  real(4)    :: winddir(xgrid,ygrid,2)
  real(4)    :: mingmt (xgrid,ygrid,2)
  real(4)    :: radrain(xgrid,ygrid,2)
  integer(4) :: scatflag(xgrid,ygrid,2)

  integer(4) :: lyear,jday,imonth,idaymonth,ierr,iasc,ilat,ilon
	
  real(4) :: swind(XGRID,YGRID)
  real(4) :: uwind(XGRID,YGRID)
  real(4) :: vwind(XGRID,YGRID)
  real(4) :: asc_valid(XGRID,YGRID)
  real(4) :: des_valid(XGRID,YGRID)
  real(4) :: total_valid
  real(4) :: undef_out = 0.0

  real(4) :: u1, v1, u2, v2

  integer(4) :: lun_out1, lun_out2
  integer(4) :: i, j
  integer(4) :: lreclen
  character(len=128) :: file_out = 'no_file'
  character(len=128) :: l3_file = 'no_file'

  logical :: l_strict_exclusion=.true.

  !-------------------------------------------------------------------

  namelist /nml_qscat_daily/ filename, file_out, undef_out, l_strict_exclusion

  !-------------------------------------------------------------------
  !     Read the input filename.

  open(10,file='namelist.qscat_daily')
  read(10,nml=nml_qscat_daily) 

  !--------------------------------------------------------

  if (filename .eq. 'no_file') then
    write(6,*) 'Usage: read_qscat3 <Level 3 file>'
    stop
  endif
  if (filename .eq. 'missing_file') then
    lreclen = 4 * XGRID * YGRID
    swind(:,:) = undef_out
    call open_file_direct(lun_out1,file_out,lreclen,convert_mode='big_endian')
    write(lun_out1,rec=1) swind
    write(lun_out1,rec=2) swind
    write(lun_out1,rec=3) swind
    call close_file(lun_out1)
    stop
  endif

  !--------------------------------------------------------

  WRITE(6,*) 'reading from ', trim(filename)

  CALL GET_SCAT_DAILY_V4(filename,mingmt,windspd,winddir,scatflag,radrain,IERR,xgrid,ygrid)

  IF (IERR .ne. 0) THEN
    WRITE(*,*) 'no qscat data: ', trim(filename)
    STOP
  ENDIF

  !------------------------------------------------------------------

  do j = 1, YGRID
    do i = 1, XGRID
      ! Ascend
      asc_valid(i,j) = 0.0
      if (l_strict_exclusion) then
        if (scatflag(i,j,1) == 0) then
          if ((radrain(i,j,1) == -999.0) .or. (radrain(i,j,1) == 0.0)) then
            asc_valid(i,j) = 1.0
          end if
        end if
      else
        if ((scatflag(i,j,1) == 0) .or. (radrain(i,j,1) == 0.0)) then
          asc_valid(i,j) = 1.0
        end if
      end if
      ! Descend
      des_valid(i,j) = 0.0
      if (l_strict_exclusion) then
        if (scatflag(i,j,2) == 0) then
          if ((radrain(i,j,2) == -999.0) .or. (radrain(i,j,2) == 0.0)) then
            des_valid(i,j) = 1.0
          end if
        end if
      else
        if ((scatflag(i,j,2) == 0) .or. (radrain(i,j,2) == 0.0)) then
          des_valid(i,j) = 1.0
        end if
      end if
    end do
  end do

  do j = 1, YGRID
    do i = 1, XGRID
      total_valid = asc_valid(i,j) + des_valid(i,j) 
      if (total_valid > 0.0) then
        if ((windspd(i,j,1) == -999.0) .and. (windspd(i,j,2) == -999.0) &
             & .and. (winddir(i,j,1) == -999.0) .and. (winddir(i,j,2) == -999.0)) then
          write(6,*) ' inconsistent flag :', i, j
          stop
        else
          if ((windspd(i,j,1) /= -999.0) .or. (windspd(i,j,2) /= -999.0)) then
            swind(i,j) = (asc_valid(i,j) * windspd(i,j,1) &
                 &      + des_valid(i,j) * windspd(i,j,2)) / total_valid
          else
            write(6,*) ' inconsistent flag and speed:', i, j
            stop
          end if
          if ((windspd(i,j,1) /= -999.0) .and. (winddir(i,j,1) /= -999.0)) then
            u1 = windspd(i,j,1) * SIND(winddir(i,j,1))
            v1 = windspd(i,j,1) * COSD(winddir(i,j,1))
          else
            if (asc_valid(i,j) == 1.0) then
              write(6,*) ' inconsistent flag and direction:', i, j
              stop
            else
              u1 = 0.0
              v1 = 0.0
            end if
          end if
          if ((windspd(i,j,2) /= -999.0) .and. (winddir(i,j,2) /= -999.0)) then
            u2 = windspd(i,j,2) * SIND(winddir(i,j,2))
            v2 = windspd(i,j,2) * COSD(winddir(i,j,2))
          else
            if (des_valid(i,j) == 1.0) then
              write(6,*) ' inconsistent flag and direction:', i, j
              stop
            else
              u2 = 0.0
              v2 = 0.0
            end if
          end if
          uwind(i,j) = (asc_valid(i,j) * u1 + des_valid(i,j) * u2) / total_valid
          vwind(i,j) = (asc_valid(i,j) * v1 + des_valid(i,j) * v2) / total_valid
        end if
      else
        swind(i,j) = undef_out
        uwind(i,j) = undef_out
        vwind(i,j) = undef_out
      end if
    end do
  end do

  !------------------------------------------------------------------

  lreclen = 4 * XGRID * YGRID
  call open_file_direct(lun_out1,file_out,lreclen,convert_mode='big_endian')
  write(lun_out1,rec=1) swind
  write(lun_out1,rec=2) uwind
  write(lun_out1,rec=3) vwind
  call close_file(lun_out1)

END Program Qscat_rss_to_daily
