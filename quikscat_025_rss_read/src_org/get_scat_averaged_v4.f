	SUBROUTINE GET_SCAT_AVERAGED_V4(FILENAME, WINDSPD,WINDDIR,SCATFLAG,RADRAIN, IERR)

	!This program reads Remote Sensing Systems Scatterometer Level-4 gridded bytemap files, Version-4, GMF: Ku-2011
	!	 The data are returned in 1440 x 720 x 3 arrays.
	!		  the first index of the array is the longitude in 1/4 degrees
	!		  the second index of the array is the latitude in 1/4 degrees
	!		  and the third index of the array is the data type  (wind speed,wind direction and rain bitflag)
	!		  
	!The center of the first cell of the 1440 column and 720 row map is at 0.125 E longitude and -89.875 latitude. 
	!The center of the second cell is 0.375 E longitude, -89.875 latitude.
	!		XLAT=0.25*ILAT-90.125
	!		XLON=0.25*ILON-0.125
	!
      !This main program can be adapted to your needs.  It also calls a subroutine that extracts the rain data.
	!		Data Types:
	!		 IVAL = ICHAR(data_array(ilon,ilat,parameter)
	!			1   Wind Speed				    : windspd = IVAL*0.2  (10m surface wind speed in m/s)
	!			2	Wind Direction			    : winddir = IVAL*1.5  (direction wind is flowing to, North is 0 deg)
	!			3	Combination Rain Data		: use bit extraction to access all data
	!				            bit pos 0       SCATEROMETER rainflag (0=no rain, 1=rain)
	!											this flag is turned on for a cell if it is on greater than a preset minimum 
	!											number of times within the composite period  (3-day:2, week:5, month;20)
	!				            bit pos 1       collocated SSMI, TMI or AMSR observation within 60 min=1, else=0
	!				            bit pos 2-7     0-63: average of radiometer rain where:
	!												0= absolutely no rain 
	!												1= rain in one of the adjacent cells within those 3 days
	!												2= Average Radiometer columnar RR = 0.5  km*mm/hr
	!												3= Average Radiometer columnar RR = 1.0
	!												4: Average Radiometer columnar RR = 1.5	 etc.
	!											Rain subroutine will extract these bit flags and return 2 arrays
	!											scatflag (0=no rain, 1 = rain) and 
	!											radrain:  -999.0 = no rad data
	!													    -1.0 = adjacent rain exists
	!												    0.0-31.0 = average radiometer columnar rain rate in km*mm/hr
	!
	! Data from the daily files are scalar (wind speed) and vector (wind direction) averaged
	! to produce the values in the time-averaged files.  A data value for a given cell is only provided in a 
	! time-averaged file if a minimum number of pass data exist within the time period being produced
	! (3-day maps, 2 obs;  week maps, 5 obs;  month maps, 20 obs)
	!  3-day = (average of 3 days ending on file date)
	!  weekly= (average of 7 days ending on Saturday of file date)
	!  monthly=(average of all days in month)
	!
	!Please make sure your system/compiler reads the data as an array of 8-bit unsigned integer values (0-255)
	!the code in this routine reads the data as a character(1) array to accomplish this.  Adjust for your achitecture if needed.
	!
	!If you have questions, contact RSS support:
	! http://www.remss.com/support
	!
	!
	! Written by D. Smith  March 2003
	! Made ready for ftp site by D.Smith  August 2003
	! Adjusted for both scatterometers and -99. changed to -999. for consistency by D.Smith July 2004
	! Added cell definition information and signed integer stop   D.Smith  March 2005
	! Fixed code error in signed integer stop  D. Smith Feb 2006
	! Added comment for version 3a  D.Smith  Jan 2007
	! Added definition of averaged files D.Smith Feb 2009
	! Adjusted text for version 4, D.Smith April 2011
!*******************************************************************************************************************
	logical(4)      lexist

	character(1)	data_array(1440,720,3)
	character(120)  filename

	real(4)			windspd(1440,720),winddir(1440,720)
	real(4)			uu(1440,720),vv(1440,720)
	real(4)         radrain(1440,720)
	integer(4)      scatflag(1440,720)
	integer(4)      ival(1440,720)
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
	OPEN(5,FILE=filename,STATUS='old',RECL=3110400,
     .	ACCESS='direct',FORM='unformatted')
	READ(5,rec=1) data_array
	CLOSE(5)

 

	! Assign data to arrays

		!wind speed 
		ival   = ICHAR(data_array(:,:,1))
		if(MINVAL(ival).lt.0) stop 'ival must be 0-255'
		WHERE (ival .le. 250) 
			windspd(:,:) = ival*0.2
		ELSEWHERE
			windspd(:,:) = -999.0
		ENDWHERE

		!wind direction
		ival  = ICHAR(data_array(:,:,2))
		if(MINVAL(ival).lt.0) stop 'ival must be 0-255'
		WHERE (ival.le.250) 
			winddir(:,:) = ival*1.5
			uu(:,:)=windspd(:,:)*SIND(winddir(:,:))
			vv(:,:)=windspd(:,:)*COSD(winddir(:,:))
		ELSEWHERE
			winddir	(:,:) = -999.0
			uu		(:,:) = -999.0
			vv		(:,:) = -999.0
		ENDWHERE


	!decode combination rain flag
	CALL get_rain(data_array, scatflag,radrain)

	
	RETURN
	END

	


***************************************************************************************
	SUBROUTINE get_rain(data_array, scatflag,radrain)

      character(1) data_array(1440,720,3)
	integer(4) scatflag(1440,720)
	real(4)    radrain (1440,720)

	integer(4) ilon,ilat,ival,radflag,krain


	do ilat=1,720
	do ilon=1,1440

	ok= ICHAR(data_array (ilon,ilat,1))
	if(ok.lt.0) stop 'data must be 0-255'
	if (ok.gt.250) then
		scatflag(ilon,ilat)= -999.
		radrain (ilon,ilat)= -999.
		cycle
	endif
	ival = ICHAR(data_array (ilon,ilat,3))
	if(ival.lt.0) stop 'ival must be 0-255'
	scatflag(ilon,ilat) = IBITS(ival,0,1)  !start at zero, extract 1 bit	  !independant scatterometer rain flag 
	radflag = IBITS(ival,1,1)  !start at one,  extract 1 bit
	krain   = IBITS(ival,2,6)  !start at two,  extract 6 bits


	IF (radflag  == 0)  radrain(ilon,ilat)  = -999.0			!no collocated radiometer data

	IF (radflag  == 1) THEN
		IF(krain == 0) 	radrain(ilon,ilat)  =    0.0			!collocated radiometer data show no rain
	
		IF(krain == 1)  radrain(ilon,ilat)  =   -1.0			!collocated radiometer data show rain in adjacent cells

	    IF(krain  > 1)  radrain(ilon,ilat)  = krain/2. - 0.5	!collocated radiometer columnar rain rate (0.5 km*mm/hr resolution) 
	ENDIF

	enddo	!ilon
	enddo	!ilat


	RETURN
	END