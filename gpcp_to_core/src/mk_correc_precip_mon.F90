! -*-F90-*-
!==============================================================
program make_correction_for_precipitation_monthly
!==============================================================

  implicit none

  integer(4),parameter :: imax = 192
  integer(4),parameter :: jmax = 94

  integer(4) :: i, j, k, nyear, month, klev, mday
  integer(4) :: nbyr, neyr, ibmn, iemn

  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)
  integer(4),parameter :: nday_year = 365

  integer(4),parameter :: mtin1 = 31, mtin2 = 32, mtin3 = 33
  integer(4),parameter :: mtin4 = 34, mtin5 = 35, mtin6 = 36
  integer(4),parameter :: mtot1 = 51, mtot2 = 52

  integer(4),parameter :: lrec=imax*jmax*4

  real(4) :: undef_ref
  real(4) :: undef_raw

  real(4) :: dat2(imax,jmax)

  real(8) :: precip_ref(imax,jmax)

  real(8) :: precip_raw(imax,jmax)

  real(8) :: precip_mag(imax,jmax)

  character(len=256) :: flnin1, flnin2, flnin3
  character(len=256) :: flnin4, flnin5, flnin6
  character(len=256) :: flnin1_base, flnin2_base
  character(len=256) :: flnin4_base, flnin5_base
  character(len=256) :: file_mask_raw
  character(len=256) :: flnot1, flnot2

  real(8), parameter :: PI = 3.141592653589793D0

!-----------------------------------------------------------------

  namelist /nml_precipcorrmon/ &
       &  flnin1_base, undef_ref, &
       &  flnin4_base, undef_raw, &
       &  flnot1

!-----------------------------------------------------------------

  open (10,file='namelist.prcpcorrmon')
  read (10,nml_precipcorrmon) 
  close(10)

!-----------------------------------------------------------------

  write(6,*) ' Correction factor of precipitation = ',trim(flnot1)
  open (mtot1,file=flnot1,access='direct',form='unformatted',recl=lrec)

  precip_ref(:,:) = 0.0d0
  precip_raw(:,:) = 0.0d0

  do month = 1, 12

    write(flnin1,'(1a,i2.2)') trim(flnin1_base),month
    open(mtin1, file=flnin1, form='unformatted', access='direct', recl=lrec)
    write(6,*) ' file : ', trim(flnin1), ' opened '
    read(mtin1,rec=1) dat2
    close(mtin1)

    do j = 1, jmax
      do i = 1, imax
        if (dat2(i,j) /= undef_ref) then
          precip_ref(i,j) = real(dat2(i,j),8)
        else
          precip_ref(i,j) = 0.0d0
        end if
      end do
    end do

    write(flnin4,'(1a,i2.2)') trim(flnin4_base),month
    open(mtin4, file=flnin4, form='unformatted', access='direct', recl=lrec)
    write(6,*) ' file : ', trim(flnin4), ' opened '
    read(mtin4,rec=1) dat2
    close(mtin4)

    do j = 1, jmax
      do i = 1, imax
        if (dat2(i,j) /= undef_raw) then
          precip_raw(i,j) = real(dat2(i,j),8) ! [kg/m2/sec]
        else
          precip_raw(i,j) = 0.0d0
        end if
      end do
    end do

    do j = 1, jmax
      do i = 1, imax
        if ((precip_ref(i,j) /= 0.0d0) .and. (precip_raw(i,j) /= 0.0d0)) then
          precip_mag(i,j) = precip_ref(i,j) / precip_raw(i,j)
          precip_mag(i,j) = max(min(precip_mag(i,j),5.0d0),0.2d0)
        else
          precip_mag(i,j) = 1.0d0
        end if
      end do
    end do

    write(mtot1,rec=month) real(precip_mag(1:imax,1:jmax),4)

  end do

  close(mtot1)

  !--------------------------------------------

end program make_correction_for_precipitation_monthly
