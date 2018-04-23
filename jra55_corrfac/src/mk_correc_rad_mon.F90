! -*-F90-*-
!==============================================================
program make_correction_for_radiation_monthly
!==============================================================

  implicit none

  integer(4),parameter :: imax = 640
  integer(4),parameter :: jmax = 320

  integer(4) :: i, j, k, nyear, month, klev, mday
  integer(4) :: nbyr, neyr, ibmn, iemn

  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)
  integer(4),parameter :: nday_year = 365

  integer(4),parameter :: mtin1 = 31, mtin2 = 32, mtin3 = 33
  integer(4),parameter :: mtin4 = 34, mtin5 = 35, mtin6 = 36
  integer(4),parameter :: mtot1 = 51, mtot2 = 52

  integer(4),parameter :: lrec=imax*jmax*4

  real(4) :: undef_jra
  real(4) :: undef_obs

  real(4) :: dat2(imax,jmax)

  real(8) :: dswrf_obs(imax,jmax)
  real(8) :: dlwrf_obs(imax,jmax)
  real(8) :: mask_obs (imax,jmax)

  real(8) :: dswrf_jra(imax,jmax)
  real(8) :: dlwrf_jra(imax,jmax)
  real(8) :: mask_jra (imax,jmax)

  real(8) :: dswrf_mag(imax,jmax)
  real(8) :: dlwrf_mag(imax,jmax)

  character(len=256) :: flnin1, flnin2
  character(len=256) :: flnin1_base, flnin2_base
  character(len=256) :: flnin4, flnin5
  character(len=256) :: flnin4_base, flnin5_base
  character(len=256) :: flnot1, flnot2
  character(len=256) :: file_mask_jra

  real(8), parameter :: PI = 3.141592653589793D0

!-----------------------------------------------------------------

  namelist /nml_radcorrmon/ &
       &  flnin1_base, flnin2_base, undef_obs, &
       &  flnin4_base, flnin5_base, undef_jra, &
       &  file_mask_jra, &
       &  flnot1, flnot2

!-----------------------------------------------------------------

  open (10,file='namelist.radcorrmon')
  read (10,nml_radcorrmon) 
  close(10)

!-----------------------------------------------------------------

  mask_obs (:,:) = 0.0d0

  open(mtin6, file=file_mask_jra, form='unformatted', access='direct', recl=lrec)
  write(6,*) ' file : ', trim(file_mask_jra), ' opened '
  read(mtin6,rec=1) dat2
  do j = 1, jmax
    do i = 1, imax
      mask_jra(i,j) = real(dat2(i,j),8)
    end do
  end do
  close(mtin6)

  !---------------------------------------------------------------

  write(6,*) ' Correction factor of short wave = ',trim(flnot1)
  open (mtot1,file=flnot1,access='direct',form='unformatted',recl=lrec)

  write(6,*) ' Correction factor of long wave = ',trim(flnot2)
  open (mtot2,file=flnot2,access='direct',form='unformatted',recl=lrec)

  dswrf_obs(:,:) = 0.0d0
  dlwrf_obs(:,:) = 0.0d0
!  mask_obs (:,:) = 0.0d0

  dswrf_jra(:,:) = 0.0d0
  dlwrf_jra(:,:) = 0.0d0

  do month = 1, 12

    write(flnin1,'(1a,i2.2)') trim(flnin1_base),month
    open(mtin1, file=flnin1, form='unformatted', access='direct', recl=lrec)
    write(6,*) ' file : ', trim(flnin1), ' opened '
    read(mtin1,rec=1) dat2
    close(mtin1)

    do j = 1, jmax
      do i = 1, imax
        if (dat2(i,j) /= undef_obs) then
          dswrf_obs(i,j) = real(dat2(i,j),8)
        end if
      end do
    end do

    write(flnin2,'(1a,i2.2)') trim(flnin2_base),month
    open(mtin2, file=flnin2, form='unformatted', access='direct', recl=lrec)
    write(6,*) ' file : ', trim(flnin2), ' opened '
    read(mtin2,rec=1) dat2
    close(mtin2)

    do j = 1, jmax
      do i = 1, imax
        if (dat2(i,j) /= undef_obs) then
          dlwrf_obs(i,j) = real(dat2(i,j),8)
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
        if (dat2(i,j) /= undef_jra) then
          dswrf_jra(i,j) = real(dat2(i,j),8)
        end if
      end do
    end do

    write(flnin5,'(1a,i2.2)') trim(flnin5_base),month
    open(mtin5, file=flnin5, form='unformatted', access='direct', recl=lrec)
    write(6,*) ' file : ', trim(flnin5), ' opened '
    read(mtin5,rec=1) dat2
    close(mtin5)

    do j = 1, jmax
      do i = 1, imax
        if (dat2(i,j) /= undef_jra) then
          dlwrf_jra(i,j) = real(dat2(i,j),8)
        end if
      end do
    end do

    !--------------------------------------------

    do j = 1, jmax
      do i = 1, imax
        !if (mask_jra(i,j) == 1.0d0) then
          if ((dswrf_obs(i,j) > 5.0d0) .and. (dswrf_jra(i,j) > 5.0d0)) then
            dswrf_mag(i,j) = dswrf_obs(i,j) / dswrf_jra(i,j)
          else
            dswrf_mag(i,j) = 1.0d0
          end if
        !else
        !  dswrf_mag(i,j) = real(undef_jra,8)
        !end if
      end do
    end do

    write(mtot1,rec=month) real(dswrf_mag(1:imax,1:jmax),4)

    !--------------------------------------------

    do j = 1, jmax
      do i = 1, imax
        !if (mask_jra(i,j) == 1.0d0) then
          if ((dlwrf_obs(i,j) > 5.0d0) .and. (dlwrf_jra(i,j) > 5.0d0)) then
            dlwrf_mag(i,j) = dlwrf_obs(i,j) / dlwrf_jra(i,j)
          else
            dlwrf_mag(i,j) = 1.0d0
          end if
        !else
        !  dlwrf_mag(i,j) = real(undef_jra,8)
        !end if
      end do
    end do

    write(mtot2,rec=month) real(dlwrf_mag(1:imax,1:jmax),4)

  end do

  close(mtot1)
  close(mtot2)

end program make_correction_for_radiation_monthly
