! -*-F90-*-
!
!  Calculate monthly climatology on ocean and ice separately
!
!  2014/08/06
!==============================================================
program make_monthly_climatology_on_ice

  implicit none

  integer(4),parameter :: imax = 360
  integer(4),parameter :: jmax = 180
  integer(4),parameter :: kmax = 1

  integer(4) :: i, j, k, nyear, month, nday, klev, nd
  integer(4) :: nbyr, neyr

  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  integer(4),parameter :: nf_in  = 31
  integer(4),parameter :: nf_ice = 32
  integer(4),parameter :: nf_out = 41

  integer(4),parameter :: lrec=imax*jmax*4

  integer(4) :: num_ocn(imax,jmax,kmax)
  integer(4) :: num_ice(imax,jmax,kmax)

  real(4) :: undef_in, undef_ice, undef_out
  real(4) :: dat2(imax,jmax)
  real(4) :: aice(imax,jmax) ! sea ice area fraction
  real(8) :: dim3_ocn(imax,jmax,kmax)
  real(8) :: dim3_ice(imax,jmax,kmax)
  real(4) :: def_ice
  real(4) :: frac_valid

  character(255) :: infile, infile_base
  character(255) :: infile_ice, infile_base_ice
  character(255) :: outfile_ocn, outfile_base_ocn
  character(255) :: outfile_ice, outfile_base_ice

!-----------------------------------------------------------------

  namelist /nammonclice/ &
       &  infile_base, undef_in, &
       &  infile_base_ice, undef_ice, &
       &  outfile_base_ocn, outfile_base_ice, undef_out, &
       &  nbyr, neyr, klev, def_ice, &
       &  frac_valid

!-----------------------------------------------------------------

  def_ice=0.55
  frac_valid = 1.0

  open(10,file='ioinfmnclice.dat')
  read(10,nammonclice) 
  close(10)

!-----------------------------------------------------------------

  do month = 1, 12

    dim3_ocn(:,:,:) = 0.0d0
    dim3_ice(:,:,:) = 0.0d0

    num_ocn(:,:,:) = 0
    num_ice(:,:,:) = 0

    do nyear = nbyr, neyr

      write(6,*) 'Year = ', nyear, ' Month = ', month

      ! read sea ice concentration

      write(infile_ice,'(1a,i4.4,i2.2)') trim(infile_base_ice),nyear,month
      open(nf_ice,file=infile_ice,form='unformatted',access='direct',recl=lrec)
      write(6,*) ' file : ', trim(infile_ice), ' opened '
      read(nf_ice,rec=1) ((dat2(i,j),i=1,imax),j=1,jmax)
      do j = 1, jmax
        do i = 1, imax
          if (dat2(i,j) /= undef_ice) then
            aice(i,j) = dat2(i,j)
          else
            aice(i,j) = 0.0
          end if
        end do
      end do
      close(nf_ice)

      ! read data

      write(infile,'(1a,i4.4,i2.2)') trim(infile_base),nyear,month
      write(6,*) trim(infile)
      open(nf_in,file=infile,form='unformatted',access='direct',recl=lrec)
      write(6,*) ' file : ', trim(infile), ' opened '
      do k = 1, klev
        read(nf_in,rec=k) ((dat2(i,j),i=1,imax),j=1,jmax)
        do j = 1, jmax
          do i = 1, imax
            if (dat2(i,j) /= undef_in) then
              if (aice(i,j) < def_ice) then
                dim3_ocn(i,j,k) = dim3_ocn(i,j,k) + real(dat2(i,j),8)
                num_ocn(i,j,k) = num_ocn(i,j,k) + 1
              else
                dim3_ice(i,j,k) = dim3_ice(i,j,k) + real(dat2(i,j),8)
                num_ice(i,j,k) = num_ice(i,j,k) + 1
              end if
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
          if (num_ocn(i,j,k) >= int(frac_valid * real(neyr-nbyr+1,4) + 1.0e-6)) then
            dim3_ocn(i,j,k) = dim3_ocn(i,j,k) / real(num_ocn(i,j,k),8)
          else
            dim3_ocn(i,j,k) = real(undef_out,8)
          end if
        end do
      end do
    end do

    do k = 1, klev
      do j = 1, jmax
        do i = 1, imax
          if (num_ice(i,j,k) >= int(frac_valid * real(neyr-nbyr+1,4) + 1.0e-6)) then
            dim3_ice(i,j,k) = dim3_ice(i,j,k) / real(num_ice(i,j,k),8)
          else
            dim3_ice(i,j,k) = real(undef_out,8)
          end if
        end do
      end do
    end do

    write(outfile_ocn,'(1a,i2.2)') trim(outfile_base_ocn),month
    write(6,*) ' outfile = ',trim(outfile_ocn)
    open (nf_out,file=outfile_ocn,access='direct',form='unformatted',recl=lrec)
    do k = 1, klev
      write(nf_out,rec=0*klev+k) ((real(dim3_ocn(i,j,k),4),i=1,imax),j=1,jmax)
    end do
    do k = 1, klev
      write(nf_out,rec=1*klev+k) ((real(num_ocn(i,j,k),4),i=1,imax),j=1,jmax)
    end do
    close(nf_out)

    write(outfile_ice,'(1a,i2.2)') trim(outfile_base_ice),month
    write(6,*) ' outfile = ',trim(outfile_ice)
    open (nf_out,file=outfile_ice,access='direct',form='unformatted',recl=lrec)
    do k = 1, klev
      write(nf_out,rec=0*klev+k) ((real(dim3_ice(i,j,k),4),i=1,imax),j=1,jmax)
    end do
    do k = 1, klev
      write(nf_out,rec=1*klev+k) ((real(num_ice(i,j,k),4),i=1,imax),j=1,jmax)
    end do
    close(nf_out)

  end do

end program make_monthly_climatology_on_ice
