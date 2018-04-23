! -*-F90-*-
!
!  Calculate annual mean climatology from monthly climatology
!
!  2014/08/06
!==============================================================
program make_annual_mean_from_monthly_mean

  implicit none

  integer(4),parameter :: imax = 360
  integer(4),parameter :: jmax = 180
  integer(4),parameter :: kmax = 1

  integer(4) :: i, j, k, nyear, month, nday, klev, nd
  integer(4) :: nbyr, neyr

  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  integer(4),parameter :: nf_in  = 31
  integer(4),parameter :: nf_out = 41

  integer(4),parameter :: lrec=imax*jmax*4

  real(4) :: undef_in, undef_out
  real(4) :: dat2d(imax,jmax)
  real(4) :: dat2n(imax,jmax)

  real(8) :: dim3_ocn(imax,jmax,kmax)
  real(8) :: num_ocn (imax,jmax,kmax)

  character(255) :: infile, infile_base
  character(255) :: outfile

!-----------------------------------------------------------------

  namelist /namanclmncl/ &
       &  infile_base, undef_in, &
       &  outfile, undef_out, &
       &  klev

!-----------------------------------------------------------------

  open(10,file='ioinfanclmncl.dat')
  read(10,namanclmncl) 
  close(10)

!-----------------------------------------------------------------

  dim3_ocn(:,:,:) = 0.0d0

  num_ocn(:,:,:) = 0.0d0

  do month = 1, 12

    write(6,*) ' Month = ', month

    ! read data

    write(infile,'(1a,i2.2)') trim(infile_base),month
    write(6,*) trim(infile)
    open(nf_in,file=infile,form='unformatted',access='direct',recl=lrec)
    write(6,*) ' file : ', trim(infile), ' opened '
    do k = 1, klev
      read(nf_in,rec=0*klev+k) ((dat2d(i,j),i=1,imax),j=1,jmax)
      read(nf_in,rec=1*klev+k) ((dat2n(i,j),i=1,imax),j=1,jmax)
      do j = 1, jmax
        do i = 1, imax
          if (dat2n(i,j) > 0.0) then
            dim3_ocn(i,j,k) = dim3_ocn(i,j,k) + real(dat2d(i,j),8) * real(ndmon(month),8) * real(dat2n(i,j),8)
            num_ocn(i,j,k) = num_ocn(i,j,k) + real(ndmon(month),8) * real(dat2n(i,j),8)
          end if
        end do
      end do
    end do

    close(nf_in)

  end do

  !-----

  do k = 1, klev
    do j = 1, jmax
      do i = 1, imax
        if (num_ocn(i,j,k) > 0.0d0) then
          dim3_ocn(i,j,k) = dim3_ocn(i,j,k) / num_ocn(i,j,k)
        else
          dim3_ocn(i,j,k) = real(undef_out,8)
        end if
      end do
    end do
  end do

  !-----

  write(6,*) ' outfile = ',trim(outfile)
  open (nf_out,file=outfile,access='direct',form='unformatted',recl=lrec)
  do k = 1, klev
    write(nf_out,rec=0*klev+k) ((real(dim3_ocn(i,j,k),4),i=1,imax),j=1,jmax)
  end do
  !do k = 1, klev
  !  write(nf_out,rec=1*klev+k) ((real(num_ocn(i,j,k),4),i=1,imax),j=1,jmax)
  !end do
  close(nf_out)

end program make_annual_mean_from_monthly_mean
