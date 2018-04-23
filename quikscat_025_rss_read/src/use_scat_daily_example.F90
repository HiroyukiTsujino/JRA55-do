!-*-F90-*-
Program  Use_Scat_Daily_Example
	
  character(len=256) :: filename

  integer(4), parameter :: xgrid = 1440, ygrid = 720
  real(4) :: windspd(xgrid,ygrid,2)
  real(4) :: winddir(xgrid,ygrid,2)
  real(4) :: mingmt (xgrid,ygrid,2)
  real(4) :: radrain(xgrid,ygrid,2)
  integer(4) :: scatflag(xgrid,ygrid,2)

  integer(4) :: lyear,jday,imonth,idaymonth,ierr,iasc,ilat,ilon
	
  !--------------------------------------------------------
  
  DO lyear =2000,2000	!qscat verification 2000,2000	!seawinds verification  2003,2003	!full qscat data range  1999,2004 (or current year)
    DO jday=11,11 	!qscat verification	11,11		!seawinds verification  115,115		!full year  1,366
      IF (lyear.eq.1999.and.jday.lt.200) cycle ! before qscat in orbit
      IF (lyear.ne.4*INT(lyear/4).and.jday.eq.366) cycle ! not a leap year

      CALL find_month_day(lyear,jday, imonth,idaymonth)

      ! Construct file name
      ! ADJUST FOR YOUR DIRECTORY STRUCTURE and INSTRUMENT!
      ! This subroutine assumes you have unzipped files with no extention

      WRITE(filename,'(1a,i4.4,1a,i2.2,1a,i4.4,i2.2,i2.2,1a)') &
           & '/work115/htsujino/QuikSCAT/qscat/bmaps_v04/y',lyear,'/m',imonth,&
           & '/qscat_',lyear,imonth,idaymonth,'v4'
      WRITE(6,*) 'reading from ', trim(filename)

      CALL GET_SCAT_DAILY_V4(filename,mingmt,windspd,winddir,scatflag,radrain,IERR,xgrid,ygrid)

      IF (IERR .ne. 0) THEN
        WRITE(*,*) 'no scat data: ', trim(filename)
        STOP
      ENDIF

      !!  write out scat verification file data for comparison
      DO iasc = 1,1        !range  ! 1,2(asc,dsc)  !seawinds and qscat verification !1,1
        DO ilat = 274,278  !1,720  ! 274,278
          DO ilon=170,175  !1,1440 ! 170,175
            ! XLAT=0.25*ILAT-90.125
            ! XLON=0.25*ILON-0.125
            write(*,'(2i5,f6.0,2f8.2,2x,i4,f11.2)') ilon,ilat, &
                 &  mingmt(ilon,ilat,iasc),windspd(ilon,ilat,iasc), &
                 &  winddir(ilon,ilat,iasc),scatflag(ilon,ilat,iasc), &
                 &  radrain(ilon,ilat,iasc)
            
          ENDDO  !ilon
        ENDDO  !ilat
      ENDDO  !iasc

    ENDDO	!jday
  ENDDO	!lyear

contains

!! write the location of the qscat data subroutine
!! include 'YourDrive:\YourDirectory\GET_SCAT_DAILY_v4.F'

!!include 'GET_SCAT_DAILY_V4.F90'

!************************************************************************************

SUBROUTINE FIND_MONTH_DAY(LYEAR,IDAYJL, IMON,IDAY)

  INTEGER(4) :: IDAYFX(12,0:1)

  DATA IDAYFX/1,32,60,91,121,152,182,213,244,274,305,335, &
       &      1,32,61,92,122,153,183,214,245,275,306,336/           
                                                  
  ILEAP=0
  IF(LYEAR.EQ.4*INT(LYEAR/4)) ILEAP=1

  DO JMON=2,12
    IF(IDAYFX(JMON,ILEAP).GT.IDAYJL) THEN
      IMON=JMON-1
      exit
    ENDIF
    IMON=12
  end DO

  IDAY=1+IDAYJL-IDAYFX(IMON,ILEAP)
  RETURN

end SUBROUTINE FIND_MONTH_DAY

END Program Use_Scat_Daily_Example
