! -*-F90-*-
!==============================================================
program make_correction_for_wind_magnitude_monthly
!==============================================================

  implicit none

  integer(4),parameter :: imax = 640
  integer(4),parameter :: jmax = 320

  integer(4) :: i, j, k, nyear, month, klev, mday
  integer(4) :: nbyr, neyr, ibmn, iemn

  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  integer(4),parameter :: mtin1 = 31, mtin2 = 32, mtin3 = 33
  integer(4),parameter :: mtin4 = 34, mtin5 = 35, mtin6 = 36
  integer(4),parameter :: mtot1 = 51, mtot2 = 52

  integer(4),parameter :: lrec=imax*jmax*4

  real(4) :: undef_jra
  real(4) :: undef_obs

  real(4) :: dat2(imax,jmax)

  real(8) :: swind_obs(imax,jmax)
  real(8) :: mask_obs (imax,jmax)

  real(8) :: swind_jra(imax,jmax)
  real(8) :: mask_jra (imax,jmax)

  real(8) :: corr_mag(imax,jmax)

  character(len=256) :: flnin1_base, flnin2_base, flnin3_base
  character(len=256) :: flnin1, flnin2, flnin3
  character(len=256) :: file_mask_wind
  character(len=256) :: flnot1, flnot2

  real(8), parameter :: PI = 3.141592653589793D0

!-----------------------------------------------------------------

  namelist /nml_windmagcorrec_mon/ &
       &  flnin1_base, undef_obs,  &
       &  flnin2_base, undef_jra,  &
       &  file_mask_wind,          &
       &  flnot1

!-----------------------------------------------------------------

  open (10,file='namelist.windmagcorrec_mon')
  read (10,nml_windmagcorrec_mon) 
  close(10)

!-----------------------------------------------------------------

  swind_obs(:,:) = 0.0d0
  mask_obs (:,:) = 0.0d0

  swind_jra(:,:) = 0.0d0
  mask_jra (:,:) = 0.0d0

  open(mtin6, file=file_mask_wind, form='unformatted', access='direct', recl=lrec)
  write(6,*) ' file : ', trim(file_mask_wind), ' opened '
  read(mtin6,rec=1) dat2
  do j = 1, jmax
    do i = 1, imax
      mask_jra(i,j) = real(dat2(i,j),8)
    end do
  end do
  close(mtin6)

  write(6,*) ' Correction factor of speed = ',trim(flnot1)
  open (mtot1,file=flnot1,access='direct',form='unformatted',recl=lrec)

  !---------------------------------------------------------------

  do month = 1, 12

    write(flnin1,'(1a,i2.2)') trim(flnin1_base),month
    open(mtin1, file=flnin1, form='unformatted', access='direct', recl=lrec)
    write(6,*) ' file : ', trim(flnin1), ' opened '
    read(mtin1,rec=1) dat2
    do j = 1, jmax
      do i = 1, imax
        swind_obs(i,j) = real(dat2(i,j),8)
      end do
    end do
    close(mtin1)

    write(flnin2,'(1a,i2.2)') trim(flnin2_base),month
    open(mtin2, file=flnin2, form='unformatted', access='direct', recl=lrec)
    write(6,*) ' file : ', trim(flnin2), ' opened '
    read(mtin2,rec=1) dat2
    do j = 1, jmax
      do i = 1, imax
        swind_jra(i,j) = real(dat2(i,j),8)
      end do
    end do
    close(mtin2)

    !--------------------------------------------

    do j = 1, jmax
      do i = 1, imax
        if (mask_jra(i,j) /= 0.0d0) then
          if ((swind_obs(i,j) == real(undef_obs,8)) .and. (swind_jra(i,j) == real(undef_jra,8))) then
            write(6,*) ' mask and input data are inconsistent, teminating ....'
            stop
          else
            corr_mag(i,j) = swind_obs(i,j) / swind_jra(i,j)
          end if
        else
          corr_mag(i,j) = real(undef_jra,8)
        end if
      end do
    end do

    write(mtot1,rec=month) real(corr_mag(1:imax,1:jmax),4)

  end do

  close(mtot1)

end program make_correction_for_wind_magnitude_monthly
