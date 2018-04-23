! -*-F90-*-
!
!  Calculate monthly climatology
!
!  2008/03/14
!==============================================================
program calibration_precip_on_core

  implicit none

  integer(4),parameter :: imax = 192
  integer(4),parameter :: jmax = 94
  integer(4),parameter :: kmax = 1

  integer(4) :: i, j, k, nyear, month, nday, klev, nd
  integer(4) :: nbyr, neyr

  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  integer(4),parameter :: nf_in  = 31, nf_fac = 32
  integer(4),parameter :: nf_out = 41

  integer(4),parameter :: lrec = imax*jmax*4

  real(4) :: undef_in, undef_out

  real(4) :: gd(imax,jmax)
  real(4) :: dat2(imax,jmax)
  real(4) :: dat3(imax,jmax,kmax)
  real(8) :: dim3(imax,jmax,kmax)
  real(8) :: precip_fac(imax,jmax,kmax,12)

  character(256) :: infile, infile_base
  character(256) :: outfile, outfile_base
  character(256) :: flnin_factor

!-----------------------------------------------------------------

  namelist /nml_calib_prcp/ &
       &  infile_base, undef_in, outfile_base, undef_out, &
       &  flnin_factor, &
       &  nbyr, neyr, klev

!-----------------------------------------------------------------

  open (10,file='namelist.calib_prcp')
  read (10,nml_calib_prcp) 
  close(10)

!-----------------------------------------------------------------

  write(6,*) ' Correction factor of precipitation = ',trim(flnin_factor)
  open (nf_fac,file=flnin_factor,access='direct',form='unformatted',recl=4*imax*jmax*klev)

  do month = 1, 12
    read(nf_fac,rec=month) dat3(1:imax,1:jmax,1:klev)
    precip_fac(1:imax,1:jmax,1:klev,month) = real(dat3(1:imax,1:jmax,1:klev),8)
  end do

  close(nf_fac)

  !-----------------------------------------------------------------

  do nyear = nbyr, neyr

    do month = 1, 12

      write(6,*) 'Year = ', nyear, ' Month = ', month
      write(infile,'(1a,i4.4,i2.2)') trim(infile_base),nyear,month

      open(nf_in,file=infile,form='unformatted',access='direct',recl=lrec)

      write(6,*) ' file : ', trim(infile), ' opened '

      do k = 1, klev
        read(nf_in,rec=k) ((dat2(i,j),i=1,imax),j=1,jmax)
        do j = 1, jmax
          do i = 1, imax
            dim3(i,j,k) = real(dat2(i,j),8) * precip_fac(i,j,k,month)
          end do
        end do
      end do

      close(nf_in)

      write(outfile,'(1a,i4.4,i2.2)') trim(outfile_base),nyear,month
      write(6,*) ' outfile = ',trim(outfile)
      open (nf_out,file=outfile,access='direct',form='unformatted',recl=lrec)
      do k = 1, klev
        write(nf_out,rec=0*klev+k) ((real(dim3(i,j,k),4),i=1,imax),j=1,jmax)
      end do
      close(nf_out)

    end do

  end do

end program calibration_precip_on_core
