	Program  Use_Scat_Averaged_Example
	
	character(120) file3day, fileweek, filemonth

	real(4) windspd(1440,720)
	real(4) winddir(1440,720)
	real(4) radrain(1440,720)
	integer(4) scatflag(1440,720)

	integer lyear,jday,imonth,idaymonth,ierr,ilat,ilon


c     ****************************************************************
c	Uncomment this code to loop through days and writing file names. 
c     ****************************************************************
c	DO lyear =1999, current year
c 	  DO jday=1,366
c		IF(lyear.eq.1999.and.jday.lt.200)			 cycle	!before qscat put in orbit
c		IF(lyear.ne.4*INT(lyear/4).and.jday.eq.366)  cycle	!not a leap year
c		
c	    CALL find_month_day(lyear,jday, imonth,idaymonth)
c
c		! Construct file name
c		! ADJUST FOR YOUR DIRECTORY STRUCTURE!
c
c		3-day
c		WRITE(file3day,9001) lyear,imonth,lyear,imonth,idaymonth
c9001		FORMAT('InsertYourDrive:\InsertYourLocation\y',i4.4,'\m',i2.2,'\qscat_',i4.4,i2.2,i2.2,'v4_3day')
c
c		weekly
c		WRITE(fileweek,9002) lyear,imonth,idaymonth
c9002		FORMAT('InsertYourDrive:\InsertYourLocation\weekly\qscat_',i4.4,i2.2,i2.2,'v4')
c
c		monthly
c		WRITE(filemonth,9003) lyear,imonth
c9003		FORMAT('InsertYourDrive:\InsertYourLocation\y',i4.4,'\m',i2.2,'\qscat_',i4.4,i2.2,'v4')



c	THIS CODE IS TO WRITE THE DATA IN THE VERIFICATION FILE

	write(*,*) '***********3-day******************'
c	file3day='your drive:\your directory\qscat_20000111v4_3day'	!change to match your system or use loop structure above
	file3day='qscat_20000111v4_3day'   !for qscat verification
c	file3day='20030425_3day'   !for seawinds verification

	call GET_SCAT_AVERAGED_V4(file3day, windspd,winddir,scatflag,radrain, IERR)	
	IF (IERR.ne.0) THEN
		WRITE(*,*) 'no scat data: ', file3day
		STOP
	ENDIF

	!!  write out verification file data for comparison
		  DO ilat=274,278	 !range=	!1,720
		    DO ilon=170,175				!1,1440

		!		XLAT=0.25*ILAT-90.125
		!		XLON=0.25*ILON-0.125

				write(*,'(2i6,2f9.2,i5,f12.2)') ilon,ilat,
     .				windspd(ilon,ilat),winddir(ilon,ilat),
     .				scatflag(ilon,ilat),radrain(ilon,ilat)

			ENDDO  !ilon
		  ENDDO  !ilat



	write(*,*) '***********weekly******************'
c	fileweek='your drive:\your directory\qscat_20000115v4'	!change to match your system or use loop structure above
	fileweek='qscat_20000115v4'		 !for qscat verification
c	fileweek='20030426'		 !for seawinds verification

	call GET_SCAT_AVERAGED_V4(fileweek, windspd,winddir,scatflag,radrain, IERR)	
	IF (IERR.ne.0) THEN
		WRITE(*,*) 'no scat data: ', fileweek
		STOP
	ENDIF

	!!  write out verification file data for comparison
		  DO ilat=274,278		!range  !1,720
		    DO ilon=170,175				!1,1440

		!		XLAT=0.25*ILAT-90.125
		!		XLON=0.25*ILON-0.125

				write(*,'(2i6,2f9.2,i5,f12.2)') ilon,ilat,
     .				windspd(ilon,ilat),winddir(ilon,ilat),
     .				scatflag(ilon,ilat),radrain(ilon,ilat)

			ENDDO  !ilon
		  ENDDO  !ilat


	write(*,*) '***********month******************'
c	filemonth='your drive:\your directory\qscat_200001v4'	!change to match your system or use loop structure above
	filemonth='qscat_200001v4'		!for qscat verification
c	filemonth='200304'		!for seawinds verification

	call GET_SCAT_AVERAGED_V4(filemonth, windspd,winddir,scatflag,radrain, IERR)	
	IF (IERR.ne.0) THEN
		WRITE(*,*) 'no scat data: ', filemonth
		STOP
	ENDIF

	!!  write out verification file data for comparison
		  DO ilat=274,278		!1,720
		    DO ilon=170,175		!1,1440

		!		XLAT=0.25*ILAT-90.125
		!		XLON=0.25*ILON-0.125

				write(*,'(2i6,2f9.2,i5,f12.2)') ilon,ilat,
     .				windspd(ilon,ilat),winddir(ilon,ilat),
     .				scatflag(ilon,ilat),radrain(ilon,ilat)

			ENDDO  !ilon
		  ENDDO  !ilat

					

c	  ENDDO	!jday
c	ENDDO	!lyear

	END

c	  !! write the location of the qscat data subroutine
c  	  !! include 'YourDrive:\YourDirectory\GET_SCAT_AVERAGED_v4.F'
	include 'GET_SCAT_AVERAGED_v4.F'


************************************************************************************
      SUBROUTINE FIND_MONTH_DAY(LYEAR,IDAYJL, IMON,IDAY)

	INTEGER(4) IDAYFX(12,0:1)
      DATA IDAYFX/1,32,60,91,121,152,182,213,244,274,305,335,           
     1            1,32,61,92,122,153,183,214,245,275,306,336/           
                                                  
      ILEAP=0
	IF(LYEAR.EQ.4*INT(LYEAR/4)) ILEAP=1

      DO 10 JMON=2,12
	IF(IDAYFX(JMON,ILEAP).GT.IDAYJL) THEN
      IMON=JMON-1
	GO TO 20
	ENDIF
   10 CONTINUE
      IMON=12
   20 CONTINUE

      IDAY=1+IDAYJL-IDAYFX(IMON,ILEAP)
      RETURN
	END
