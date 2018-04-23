! -*-F90-*-
!
!  Calculate monthly climatology
!
!  2008/03/14
!==============================================================
program make_monthly_climatology

  implicit none

  integer(4),parameter :: imax = 360
  integer(4),parameter :: jmax = 180
  integer(4),parameter :: kmax = 1

  integer(4) :: i, j, k, nyear, month, nday, klev, nd
  integer(4) :: nbyr, neyr

  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  integer(4),parameter :: nf_in = 31
  integer(4),parameter :: nf_out = 41

  integer(4),parameter :: lrec=imax*jmax*4

  integer(4) :: maskt(imax,jmax,kmax)
  integer(4) :: nvalid(imax,jmax,kmax)

  real(4),parameter :: undef=-9.99e33
!  real(4),parameter :: undef=-9999.0
  real(4) :: gd(imax,jmax)
  real(4) :: dat2(imax,jmax)
  real(8) :: dim3(imax,jmax,kmax)

  character(255) :: infile, infile_base
  character(255) :: outfile, outfile_base

!-----------------------------------------------------------------

  namelist /nammoncl/ &
       &  infile_base, outfile_base, nbyr, neyr, klev

!-----------------------------------------------------------------

  open(10,file='ioinfmncl.dat')
  read(10,nammoncl) 
  close(10)

!-----------------------------------------------------------------

  do month = 1, 12
    dim3(:,:,:) = 0.0d0
    nvalid(:,:,:) = 0
    do nyear = nbyr, neyr
      write(6,*) 'Year = ', nyear, ' Month = ', month
      write(infile,'(1a,i4.4,i2.2)') trim(infile_base),nyear,month
      open(nf_in,file=infile,form='unformatted',access='direct',recl=lrec)
      write(6,*) ' file : ', trim(infile), ' opened '
      do k = 1, klev
        read(nf_in,rec=k) ((dat2(i,j),i=1,imax),j=1,jmax)
        do j = 1, jmax
          do i = 1, imax
            if (dat2(i,j) /= undef) then
              dim3(i,j,k) = dim3(i,j,k) + real(dat2(i,j),8)
              nvalid(i,j,k) = nvalid(i,j,k) + 1
            end if
          end do
        end do
      end do
      close(nf_in)
    end do

    do k = 1, klev
      do j = 1, jmax
        do i = 1, imax
          if (nvalid(i,j,k) > 0) then
            dim3(i,j,k) = dim3(i,j,k) / real(nvalid(i,j,k),8)
          else
            dim3(i,j,k) = real(undef,8)
          end if
        end do
      end do
    end do

    write(outfile,'(1a,i2.2)') trim(outfile_base),month
    write(6,*) ' outfile = ',trim(outfile)
    open (nf_out,file=outfile,access='direct',form='unformatted',recl=lrec)
    do k = 1, klev
      write(nf_out,rec=0*klev+k) ((real(dim3(i,j,k),4),i=1,imax),j=1,jmax)
    end do
    do k = 1, klev
      write(nf_out,rec=1*klev+k) ((real(nvalid(i,j,k),4),i=1,imax),j=1,jmax)
    end do
    close(nf_out)

  end do

end program make_monthly_climatology
