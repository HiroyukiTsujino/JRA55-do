! -*-F90-*-
!
!  Calculate annual mean climatology
!
!==============================================================
program make_annual_mean_climatology_mask

  use file_open_close_manager

  implicit none

  integer(4) :: imax = 640
  integer(4) :: jmax = 320
  integer(4) :: kmax = 3

  integer(4) :: i, j, k, nyear, month, klev, mday
  integer(4) :: nbyr, neyr, nbmn, nemn, ibmn, iemn

  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  real(4) :: undef_in
  real(4) :: undef_out

  integer(4),allocatable :: maskt(:,:,:)
  real(4),allocatable :: dat2(:,:)
  integer(4),allocatable :: nvalid_cl(:,:,:)
  real(8),allocatable :: dim3_cl(:,:,:)

  character(255) :: infile, infile_base
  character(255) :: outfile, outfile_base
  character(255) :: file_namelist_anncl
  character(255) :: file_mask

  integer(4) :: mtnam, nf_in, nf_out
  integer(4) :: lrec

  logical :: l_leap, l_leap_valid

!-----------------------------------------------------------------

  namelist /nml_anncl/ imax, jmax, kmax, &
       &  infile_base, undef_in, file_mask, &
       &  outfile_base, undef_out, &
       &  nbyr, nbmn, neyr, nemn, klev, l_leap_valid

!-----------------------------------------------------------------

  file_namelist_anncl='namelist.annual_clim'
  call open_file_plain(mtnam,file_namelist_anncl)
  read (mtnam,nml_anncl) 
  close(mtnam)

  write(6,*) ' Leap is taken into consideration : ', l_leap_valid

  lrec = 4 * imax * jmax

  allocate(maskt(1:imax,1:jmax,1:kmax))
  allocate(dat2(1:imax,1:jmax))
  allocate(nvalid_cl(1:imax,1:jmax,1:kmax))
  allocate(dim3_cl(1:imax,1:jmax,1:kmax))

!-----------------------------------------------------------------

  call open_file_direct(nf_in,file_mask,lrec,action='read')
  write(6,*) ' file : ', trim(file_mask), ' opened '
  do k = 1, klev
    read(nf_in,rec=k) ((dat2(i,j),i=1,imax),j=1,jmax)
    do j = 1, jmax
      do i = 1, imax
        maskt(i,j,k) = int(dat2(i,j)+1.e-4)
      end do
    end do
  end do
  call close_file(nf_in)

!-----------------------------------------------------------------

  dim3_cl(:,:,:) = 0.0d0
  nvalid_cl(:,:,:) = 0

  do nyear = nbyr, neyr

    l_leap = .false.

    if (l_leap_valid) then
      if (mod(nyear,4)   == 0) l_leap = .true.
      if (mod(nyear,100) == 0) l_leap = .false.
      if (mod(nyear,400) == 0) l_leap = .true.
    end if

    ibmn = 1
    iemn = 12

    if (nyear == nbyr) ibmn = nbmn
    if (nyear == neyr) iemn = nemn
    
    do month = ibmn, iemn

      write(6,*) 'Year = ', nyear, ' Month = ', month

      if (month /= 2) then
        mday = ndmon(month)
      else
        if (l_leap) then
          mday = ndmon(month) + 1
        else
          mday = ndmon(month)
        end if
      end if

      write(infile,'(1a,i4.4,i2.2)') trim(infile_base),nyear,month

      call open_file_direct(nf_in,infile,lrec,action='read')

      write(6,*) ' file : ', trim(infile), ' opened '

      do k = 1, klev
        read(nf_in,rec=k) ((dat2(i,j),i=1,imax),j=1,jmax)
        dat2(1:imax,1:jmax) = dat2(1:imax,1:jmax) &
             & * real(maskt(1:imax,1:jmax,k),4)
        do j = 1, jmax
          do i = 1, imax
            if (dat2(i,j) /= 0.0e0) then
              dim3_cl(i,j,k) = dim3_cl(i,j,k) + real(dat2(i,j),8) * real(mday,8)
              nvalid_cl(i,j,k) = nvalid_cl(i,j,k) + mday
            end if
          end do
        end do
      end do

      call close_file(nf_in)

    end do

  end do

  do k = 1, klev
    do j = 1, jmax
      do i = 1, imax
        if (nvalid_cl(i,j,k) > 0) then
          dim3_cl(i,j,k) = dim3_cl(i,j,k) / real(nvalid_cl(i,j,k),8)
        else
          dim3_cl(i,j,k) = real(undef_out,8)
        end if
      end do
    end do
  end do

  write(outfile,'(1a)') trim(outfile_base)

  call open_file_direct(nf_out,outfile,lrec,action='write')
  do k = 1, klev
    write(nf_out,rec=0*klev+k) real(dim3_cl(1:imax,1:jmax,k),4)
  end do
  do k = 1, klev
    write(nf_out,rec=1*klev+k) real(nvalid_cl(1:imax,1:jmax,k),4)
  end do

  call close_file(nf_out)

end program make_annual_mean_climatology_mask
