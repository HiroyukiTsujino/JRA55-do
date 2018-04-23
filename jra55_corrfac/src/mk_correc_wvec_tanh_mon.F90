! -*-F90-*-
!==============================================================
program make_correction_for_wind_vector_monthly
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
  real(8) :: uwind_obs(imax,jmax)
  real(8) :: vwind_obs(imax,jmax)
  real(8) :: angle_obs(imax,jmax)
  real(8) :: mask_obs (imax,jmax)

  real(8) :: swind_jra(imax,jmax)
  real(8) :: uwind_jra(imax,jmax)
  real(8) :: vwind_jra(imax,jmax)
  real(8) :: angle_jra(imax,jmax)
  real(8) :: mask_jra (imax,jmax)

  real(8) :: corr_mag(imax,jmax)
  real(8) :: corr_rot(imax,jmax)
  real(8) :: corr_rot_org(imax,jmax)
  real(8) :: swind_vec_obs(imax,jmax)
  real(8) :: swind_vec_jra(imax,jmax)

  real(8) :: mask_wind(imax,jmax)

  character(len=256) :: flnin1, flnin2, flnin3
  character(len=256) :: flnin4, flnin5, flnin6
  character(len=256) :: flnin1_base, flnin2_base, flnin3_base
  character(len=256) :: flnin4_base, flnin5_base, flnin6_base
  character(len=256) :: flnot1, flnot2
  character(len=256) :: file_mask_wind

  real(8), parameter :: PI = 3.141592653589793D0

  integer(4) :: irecs_obs, irecu_obs, irecv_obs

!-----------------------------------------------------------------

  namelist /nml_windcorrec_tanh_mon/ &
       &  flnin1_base, flnin2_base, flnin3_base, &
       &  irecs_obs, irecu_obs, irecv_obs, &
       &  undef_obs, &
       &  flnin4_base, flnin5_base, flnin6_base, &
       &  undef_jra, &
       &  file_mask_wind, &
       &  flnot1, flnot2

!-----------------------------------------------------------------

  open (10,file='namelist.windcorrec_tanh_mon')
  read (10,nml_windcorrec_tanh_mon) 
  close(10)

!-----------------------------------------------------------------

  swind_obs(:,:) = 0.0d0
  uwind_obs(:,:) = 0.0d0
  vwind_obs(:,:) = 0.0d0
  mask_obs (:,:) = 0.0d0

  swind_jra(:,:) = 0.0d0
  uwind_jra(:,:) = 0.0d0
  vwind_jra(:,:) = 0.0d0
  mask_jra (:,:) = 0.0d0

  mask_wind(:,:) = 0.0d0

!-----------------------------------------------------------------

  open(mtin1, file=file_mask_wind, form='unformatted', access='direct', recl=lrec)
  write(6,*) ' file : ', trim(file_mask_wind), ' opened '
  read(mtin1,rec=1) dat2
  do j = 1, jmax
    do i = 1, imax
      mask_wind(i,j) = real(dat2(i,j),8)
    end do
  end do
  close(mtin1)

!-----------------------------------------------------------------

  write(6,*) ' Correction factor of speed = ',trim(flnot1)
  open (mtot1,file=flnot1,access='direct',form='unformatted',recl=lrec)

  write(6,*) ' Rotation = ',trim(flnot2)
  open (mtot2,file=flnot2,access='direct',form='unformatted',recl=lrec)

  do month = 1, 12

    write(flnin1,'(1a,i2.2)') trim(flnin1_base),month
    open(mtin1, file=flnin1, form='unformatted', access='direct', recl=lrec)
    write(6,*) ' file : ', trim(flnin1), ' opened '
    read(mtin1,rec=irecs_obs) dat2
    do j = 1, jmax
      do i = 1, imax
        swind_obs(i,j) = real(dat2(i,j),8)
      end do
    end do
    close(mtin1)

    write(flnin2,'(1a,i2.2)') trim(flnin2_base),month
    open(mtin2, file=flnin2, form='unformatted', access='direct', recl=lrec)
    write(6,*) ' file : ', trim(flnin2), ' opened '
    read(mtin2,rec=irecu_obs) dat2
    do j = 1, jmax
      do i = 1, imax
        uwind_obs(i,j) = real(dat2(i,j),8)
      end do
    end do
    close(mtin2)

    write(flnin3,'(1a,i2.2)') trim(flnin3_base),month
    open(mtin3, file=flnin3, form='unformatted', access='direct', recl=lrec)
    write(6,*) ' file : ', trim(flnin3), ' opened '
    read(mtin3,rec=irecv_obs) dat2
    do j = 1, jmax
      do i = 1, imax
        vwind_obs(i,j) = real(dat2(i,j),8)
      end do
    end do
    close(mtin3)

    !-------

    write(flnin4,'(1a,i2.2)') trim(flnin4_base),month
    open(mtin4, file=flnin4, form='unformatted', access='direct', recl=lrec)
    write(6,*) ' file : ', trim(flnin4), ' opened '
    read(mtin4,rec=1) dat2
    do j = 1, jmax
      do i = 1, imax
        swind_jra(i,j) = real(dat2(i,j),8)
      end do
    end do
    close(mtin4)

    write(flnin5,'(1a,i2.2)') trim(flnin5_base),month
    open(mtin5, file=flnin5, form='unformatted', access='direct', recl=lrec)
    write(6,*) ' file : ', trim(flnin5), ' opened '
    read(mtin5,rec=1) dat2
    do j = 1, jmax
      do i = 1, imax
        uwind_jra(i,j) = real(dat2(i,j),8)
      end do
    end do
    close(mtin5)

    write(flnin6,'(1a,i2.2)') trim(flnin6_base),month
    open(mtin6, file=flnin6, form='unformatted', access='direct', recl=lrec)
    write(6,*) ' file : ', trim(flnin6), ' opened '
    read(mtin6,rec=1) dat2
    do j = 1, jmax
      do i = 1, imax
        vwind_jra(i,j) = real(dat2(i,j),8)
      end do
    end do
    close(mtin6)

    !--------------------------------------------

    do j = 1, jmax
      do i = 1, imax
        if (mask_wind(i,j) /= 0.0d0) then
          if ((swind_obs(i,j) == real(undef_obs,8)) .and. (swind_jra(i,j) == real(undef_jra,8))) then
            write(6,*) ' mask and input data are inconsistent, terminating ....'
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

    !--------------------------------------------

    swind_vec_obs(:,:) = 0.0d0

    do j = 1, jmax
      do i = 1, imax
        if (mask_wind(i,j) /= 0.0d0) then
          if (uwind_obs(i,j) /= 0.0d0) then
            angle_obs(i,j) = 180.d0 / pi * atan(vwind_obs(i,j)/uwind_obs(i,j)) ! [-90, 90]
            if ((uwind_obs(i,j) < 0.0d0) .and. (vwind_obs(i,j) > 0.0d0)) then
              angle_obs(i,j) = angle_obs(i,j) + 180.0d0
            end if
            if ((uwind_obs(i,j) < 0.0d0) .and. (vwind_obs(i,j) < 0.0d0)) then
              angle_obs(i,j) = angle_obs(i,j) - 180.0d0
            end if
          else
            if (vwind_obs(i,j) > 0.0d0) then
              angle_obs(i,j) = 90.0d0
            else
              angle_obs(i,j) = -90.0d0
            end if
          end if
          swind_vec_obs(i,j) = sqrt(uwind_obs(i,j)**2 + vwind_obs(i,j)**2)
        else
          angle_obs(i,j) = real(undef_obs,8)
        end if
      end do
    end do

    swind_vec_jra(:,:) = 0.0d0
    do j = 1, jmax
      do i = 1, imax
        if (mask_wind(i,j) /= 0.0d0) then
          if (uwind_jra(i,j) /= 0.0d0) then
            angle_jra(i,j) = 180.d0 / pi * atan(vwind_jra(i,j)/uwind_jra(i,j))
            if ((uwind_jra(i,j) < 0.0d0) .and. (vwind_jra(i,j) > 0.0d0)) then
              angle_jra(i,j) = angle_jra(i,j) + 180.0d0
            end if
            if ((uwind_jra(i,j) < 0.0d0) .and. (vwind_jra(i,j) < 0.0d0)) then
              angle_jra(i,j) = angle_jra(i,j) - 180.0d0
            end if
          else
            if (vwind_jra(i,j) > 0.0d0) then
              angle_jra(i,j) = 90.0d0
            else
              angle_jra(i,j) = -90.0d0
            end if
          end if
          swind_vec_jra(i,j) = sqrt(uwind_jra(i,j)**2 + vwind_jra(i,j)**2)
        else
          angle_jra(i,j) = real(undef_jra,8)
        end if
      end do
    end do
    
    do j = 1, jmax
      do i = 1, imax
        if (mask_wind(i,j) /= 0.0d0) then
          corr_rot(i,j) = angle_obs(i,j) - angle_jra(i,j)
          if (corr_rot(i,j) > 180.0d0) then
            corr_rot(i,j) = corr_rot(i,j) - 360.0d0
          end if
          if (corr_rot(i,j) < -180.0d0) then
            corr_rot(i,j) = 360.0d0 + corr_rot(i,j)
          end if

          corr_rot_org(i,j) = corr_rot(i,j)
!          corr_rot(i,j) = corr_rot_org(i,j) * 0.5d0 * (1.0d0 + tanh(min(swind_vec_obs(i,j),swind_vec_jra(i,j))-3.0d0))  ! smooth transition
          corr_rot(i,j) = corr_rot_org(i,j) * 0.5d0 * (1.0d0 + tanh(swind_vec_jra(i,j)-3.0d0))  ! smooth transition

          !if (swind_vec(i,j) <= 2.0d0) then ! less than 2 m/s
          !  corr_rot(i,j) = 0.0d0
          !else
          !  if (swind_vec(i,j) <= 3.0d0) then
          !    corr_rot(i,j) = corr_rot(i,j) * (swind_vec(i,j)-2.0d0)  ! smooth transition
          !  end if
          !end if
        else
          swind_vec_obs(i,j) = real(undef_jra,8)
          swind_vec_jra(i,j) = real(undef_jra,8)
          corr_rot(i,j) = real(undef_jra,8)
          corr_rot_org(i,j) = corr_rot(i,j)
        end if
      end do
    end do

    write(mtot2,rec=6*(month-1)+1) real(corr_rot     (1:imax,1:jmax),4)
    write(mtot2,rec=6*(month-1)+2) real(angle_obs    (1:imax,1:jmax),4)
    write(mtot2,rec=6*(month-1)+3) real(angle_jra    (1:imax,1:jmax),4)
    write(mtot2,rec=6*(month-1)+4) real(corr_rot_org (1:imax,1:jmax),4)
    write(mtot2,rec=6*(month-1)+5) real(swind_vec_obs(1:imax,1:jmax),4)
    write(mtot2,rec=6*(month-1)+6) real(swind_vec_jra(1:imax,1:jmax),4)

  end do

  close(mtot2)
  close(mtot1)

end program make_correction_for_wind_vector_monthly
