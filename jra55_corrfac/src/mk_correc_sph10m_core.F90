! -*-F90-*-
!==============================================================
program make_correction_for_specific_humidity_monthly
!==============================================================

  implicit none

  integer(4),parameter :: imax = 640
  integer(4),parameter :: jmax = 320

  integer(4) :: i, j, k, nyear, month, klev, mday
  integer(4) :: nbyr, neyr, ibmn, iemn

  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)
  integer(4),parameter :: nday_year = 365

  integer(4),parameter :: mtin1 = 31, mtin2 = 32, mtin3 = 33
  integer(4),parameter :: mtot1 = 51, mtot2 = 52

  integer(4),parameter :: lrec=imax*jmax*4

  real(4) :: undef_jra
  real(4) :: undef_obs

  real(4) :: dat2(imax,jmax)

  real(8) :: sph10m_obs(imax,jmax)

  real(8) :: sph10m_jra(imax,jmax)
  real(8) :: mask_jra (imax,jmax)

  real(8) :: sph10m_mag(imax,jmax)

  character(255) :: flnin1, flnin2, flnin3
  character(255) :: flnin1_base, flnin2_base
  character(255) :: flnot1

  real(8), parameter :: PI = 3.141592653589793D0
  real(8), parameter :: sphmin = 5.d-5

!-----------------------------------------------------------------

  namelist /nml_sphcorrmon/ &
       &  flnin1_base, undef_obs, &
       &  flnin2_base, undef_jra, &
       &  flnin3, &
       &  flnot1

!-----------------------------------------------------------------

  open (10,file='namelist.sph10mcorrec')
  read (10,nml_sphcorrmon) 
  close(10)

!-----------------------------------------------------------------

  mask_jra(:,:) = 0.0d0
  open(mtin3, file=flnin3, form='unformatted', access='direct', recl=lrec)
  write(6,*) ' file : ', trim(flnin3), ' opened '
  read(mtin3,rec=1) dat2
  do j = 1, jmax
    do i = 1, imax
      mask_jra(i,j) = real(dat2(i,j),8)
    end do
  end do
  close(mtin3)

!-----------------------------------------------------------------

  write(6,*) ' Correction factor of specific humidity = ',trim(flnot1)
  open (mtot1,file=flnot1,access='direct',form='unformatted',recl=lrec)

  sph10m_obs(:,:) = 0.0d0
  sph10m_jra(:,:) = 0.0d0

  do month = 1, 12

    write(flnin1,'(1a,i2.2)') trim(flnin1_base),month
    open(mtin1, file=flnin1, form='unformatted', access='direct', recl=lrec)
    write(6,*) ' file : ', trim(flnin1), ' opened '
    read(mtin1,rec=1) dat2
    close(mtin1)

    do j = 1, jmax
      do i = 1, imax
        if (dat2(i,j) /= undef_obs) then
          sph10m_obs(i,j) = max(real(dat2(i,j),8),sphmin)
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
        if (dat2(i,j) /= undef_jra) then
          sph10m_jra(i,j) = real(dat2(i,j),8)
        end if
      end do
    end do

    do j = 1, jmax
      do i = 1, imax
        if (mask_jra(i,j) == 1.0d0) then
          if ((sph10m_obs(i,j) /= 0.0d0) .and. (sph10m_jra(i,j) /= 0.0d0)) then
            sph10m_mag(i,j) = sph10m_obs(i,j) / sph10m_jra(i,j)
          else
            sph10m_mag(i,j) = 1.0d0
          end if
        else
          sph10m_mag(i,j) = real(undef_jra,8)
        end if
      end do
    end do
    
    write(mtot1,rec=month) real(sph10m_mag(1:imax,1:jmax),4)

  end do

  close(mtot1)

  !--------------------------------------------

end program make_correction_for_specific_humidity_monthly
