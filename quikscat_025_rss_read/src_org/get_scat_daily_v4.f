	SUBROUTINE GET_SCAT_DAILY_V4(FILENAME, MINGMT,WINDSPD,WINDDIR,SCATFLAG,RADRAIN, IERR)

	!This program reads Remote Sensing Systems Scatterometer Level-4 gridded bytemap daily files, Version-4, GMF: Ku-2011
	!	 The data are returned in 1440 x 720 x 4 x 2 arrays.
	!		the first index of the array is the longitude in 1/4 degrees
	!		the second index of the array is the latitude in 1/4 degrees
	!		the third index of the array is the data type  (min gmt,wind speed,wind direction and rain bitflag)
	!		and the fourth is the morning/evening (ascending/descending) orbit segments
	!
	!The center of the first cell of the 1440 column and 720 row map is at 0.125 E longitude and -89.875 latitude. 
	!The center of the second cell is 0.375 E longitude, -89.875 latitude.
	!		XLAT=0.25*ILAT-90.125
	!		XLON=0.25*ILON-0.125
	!
      !This main program can be adapted to your needs.  It also calls a subroutine that extracts the rain data.
	!		Data Types:
	!		IVAL = ICHAR(data_array(ilon,ilat,parameter,iasc)
	!			1   GMT Time		  : mingmt  = IVAL * 6  (Minute of day GMT)
	!			2   Wind Speed		  : windspd = IVAL*0.2  (10m surface wind speed in m/s)
	!			3   Wind Direction	  : winddir = IVAL*1.5  (direction wind is flowing to, North is 0 deg)
	!			4   Combination Rain Data : use bit extraction to access all data
	!				            bit pos 0       SCATEROMETER rainflag (0=no rain, 1=rain)
	!				            bit pos 1       collocated SSMI, TMI or AMSR observation within 60 min=1, else=0
	!				            bit pos 2-7     0-63: radiometer rain where:
	!							0= absolutely no rain 
	!							1= rain in one of the adjacent cells
	!							2= Radiometer columnar RR = 0.5  km*mm/hr
	!							3= Radiometer columnar RR = 1.0
	!							4: Radiometer columnar RR = 1.5	 etc.
	!							Rain subroutine will extract these bit flags and return 2 arrays
	!							  scatflag (0=no rain, 1 = rain) and 
	!							  radrain:  -999.0 = no rad data
	!									  -1.0 = adjacent rain exists
	!								    0.0-31.0 = radiometer columnar rain rate in km*mm/hr
	!
	!Please make sure your system/compiler reads the data as an array of 8-bit unsigned integer values (0-255)
	!the code in this routine reads the data as a character(1) array to accomplish this.  Adjust for your achitecture if needed.
	!
	! Please check www.remss.com for more information
	! If you have questions, contact RSS support:
	! http://www.remss.com/support
	!
	!
	! Written by D. Smith  January 2002
	! Amended by D. Smith to new format  December 2002
	! Renamed to get_qscat_daily_v03.f by D. Smith   August 2003
	! Altered to read both qscat and seawinds files and -99. changed to -999. for consistency by D. Smith  July 2004
	! Added cell definition information and signed integer stop   D.Smith  March 2005
	! Fixed error with signed integer stop code  D.Smith  Feb 2006
	! added comment for v3a data  D.Smith Jan 2007
	! updated to version-4, only text changes  D.Smith April 2011
	! updated comments only  D.Smith Aug 2014
!*******************************************************************************************************************
	logical(4)      lexist

	character(1)	data_array(1440,720,4,2)
	character(120)  filename

	real(4)			windspd(1440,720,2),winddir(1440,720,2)
	real(4)			uu(1440,720,2),vv(1440,720,2)
	real(4)         mingmt(1440,720,2), radrain(1440,720,2)
	integer(4)      scatflag(1440,720,2)
	integer(4)      ival(1440,720)
	integer(4)		iasc
	integer(4)      IERR
									

************************************************************************************************

	IERR=0

	! Determine if file exists
      INQUIRE(FILE=filename,EXIST=lexist)
  	IF (.not.(lexist)) THEN
		IERR = -1
		RETURN
	ENDIF


	! Open file and Read Data
	OPEN(5,FILE=filename,STATUS='old',RECL=8294400,
     .	ACCESS='direct',FORM='unformatted')
	READ(5,rec=1) data_array
	CLOSE(5)

 

	! Assign data to arrays
	DO iasc=1,2		

		!time 
    		ival   = ICHAR(data_array(:,:,1,iasc))
		if(MINVAL(ival).lt.0) stop 'ival must be 0-255'
		mingmt(:,:,iasc) = ival*6.0

		!wind speed 
		ival   = ICHAR(data_array(:,:,2,iasc))
		if(MINVAL(ival).lt.0) stop 'ival must be 0-255'
		WHERE (ival .le. 250) 
			windspd(:,:,iasc) = ival*0.2
		ELSEWHERE
			windspd(:,:,iasc) = -999.0
		ENDWHERE

		!wind direction
		ival  = ICHAR(data_array(:,:,3,iasc))
		if(MINVAL(ival).lt.0) stop 'ival must be 0-255'
		WHERE (ival.le.250) 
			winddir(:,:,iasc) = ival*1.5
			uu(:,:,iasc)=windspd(:,:,iasc)*SIND(winddir(:,:,iasc))
			vv(:,:,iasc)=windspd(:,:,iasc)*COSD(winddir(:,:,iasc))
		ELSEWHERE
			winddir	(:,:,iasc) = -999.0
			uu		(:,:,iasc) = -999.0
			vv		(:,:,iasc) = -999.0
		ENDWHERE

	ENDDO	!iasc


	!decode combination rain flag
	CALL get_rain(data_array, scatflag,radrain)

	
	RETURN
	END

	


***************************************************************************************
	SUBROUTINE get_rain(data_array, scatflag,radrain)

      character(1) data_array(1440,720,4,2)
	integer(4) scatflag(1440,720,2)
	real(4)    radrain (1440,720,2)

	integer(4) ilon,ilat,iasc,ival,radflag,krain


	do iasc=1,2

	do ilat=1,720
	do ilon=1,1440

	itime= ICHAR(data_array (ilon,ilat,1,iasc))
	if(itime.lt.0) stop 'itime must be 0-255'

	if (itime.gt.250) then
		scatflag(ilon,ilat,iasc)= -999.
		radrain (ilon,ilat,iasc)= -999.
		cycle
	endif
	ival = ICHAR(data_array (ilon,ilat,4,iasc))
	if(ival.lt.0) stop 'ival must be 0-255'
	scatflag(ilon,ilat,iasc) = IBITS(ival,0,1)  !start at zero, extract 1 bit	 	!independant scatterometer rain flag 
	radflag = IBITS(ival,1,1)  !start at one,  extract 1 bit
	krain   = IBITS(ival,2,6)  !start at two,  extract 6 bits

	IF (radflag  == 0)  radrain(ilon,ilat,iasc)  = -999.0			!no collocated radiometer data

	IF (radflag  == 1) THEN
		IF(krain == 0) 	radrain(ilon,ilat,iasc)  =    0.0			!collocated radiometer data show no rain
	
		IF(krain == 1)  radrain(ilon,ilat,iasc)  =   -1.0			!collocated radiometer data show rain in adjacent cells

	    IF(krain  > 1)  radrain(ilon,ilat,iasc)  = krain/2. - 0.5	!collocated radiometer columnar rain rate (0.5 kg.mm/hr resolution) 
	ENDIF

	enddo	!ilon
	enddo	!ilat

	enddo	!iasc

	RETURN
	END