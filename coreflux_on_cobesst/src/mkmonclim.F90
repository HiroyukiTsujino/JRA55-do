! -*-F90-*-
!
!  Calculate monthly climatology
!
!==============================================================
program make_monthly_climatology

  use file_open_close_manager

  implicit none

  integer(4) :: imax = 640
  integer(4) :: jmax = 320
  integer(4) :: kmax = 3

  integer(4) :: i, j, k, nyear, month, nday, klev, nd
  integer(4) :: nbyr, neyr
  integer(4) :: nbmn, nemn
  integer(4) :: ibyr(12), ieyr(12)

  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  real(4) :: undef_in, undef_out

  integer(4),allocatable :: maskt(:,:,:)
  integer(4),allocatable :: nvalid(:,:,:)
  real(4),allocatable :: dat2(:,:)
  real(8),allocatable :: dim3(:,:,:)

  character(255) :: infile, infile_base
  character(255) :: outfile, outfile_base
  character(255) :: file_namelist_moncl

  integer(4) :: mtnam, nf_in, nf_out
  integer(4) :: lrec

!-----------------------------------------------------------------

  namelist /nml_moncl/ imax, jmax, kmax, &
       &  infile_base, undef_in, outfile_base, undef_out, &
       &  nbyr, neyr, nbmn, nemn, klev

!-----------------------------------------------------------------

  file_namelist_moncl='namelist.monthly_clim'
  nbmn = 1  ! default value
  nemn = 12 ! default value
  call open_file_plain(mtnam,file_namelist_moncl)
  read (mtnam,nml_moncl) 
  close(mtnam)

  ibyr(:) = 0
  ieyr(:) = 0
  do month = 1, 12
    if (month < nbmn) then
      ibyr(month) = nbyr + 1
    else
      ibyr(month) = nbyr
    end if
  end do

  do month = 1, 12
    if (month > nemn) then
      ieyr(month) = neyr - 1
    else
      ieyr(month) = neyr
    end if
  end do

  lrec = 4 * imax * jmax

  allocate(maskt(1:imax,1:jmax,1:kmax))
  allocate(nvalid(1:imax,1:jmax,1:kmax))
  allocate(dat2(1:imax,1:jmax))
  allocate(dim3(1:imax,1:jmax,1:kmax))

!-----------------------------------------------------------------

  do month = 1, 12
    dim3(:,:,:) = 0.0d0
    nvalid(:,:,:) = 0
    do nyear = ibyr(month), ieyr(month)
      write(6,*) 'Year = ', nyear, ' Month = ', month
      write(infile,'(1a,i4.4,i2.2)') trim(infile_base),nyear,month
      call open_file_direct(nf_in,infile,lrec,action='read')
      write(6,*) ' file : ', trim(infile), ' opened '
      do k = 1, klev
        read(nf_in,rec=k) ((dat2(i,j),i=1,imax),j=1,jmax)
        do j = 1, jmax
          do i = 1, imax
            if (dat2(i,j) /= undef_in) then
              dim3(i,j,k) = dim3(i,j,k) + real(dat2(i,j),8)
              nvalid(i,j,k) = nvalid(i,j,k) + 1
            end if
          end do
        end do
      end do
      call close_file(nf_in)
    end do

    do k = 1, klev
      do j = 1, jmax
        do i = 1, imax
          if (nvalid(i,j,k) == (ieyr(month)-ibyr(month)+1) ) then
            dim3(i,j,k) = dim3(i,j,k) / real(nvalid(i,j,k),8)
          else
            dim3(i,j,k) = real(undef_out,8)
          end if
        end do
      end do
    end do

    write(outfile,'(1a,i2.2)') trim(outfile_base),month
    write(6,*) ' outfile = ',trim(outfile)
    call open_file_direct(nf_out,outfile,lrec,action='write')
    do k = 1, klev
      write(nf_out,rec=0*klev+k) ((real(dim3(i,j,k),4),i=1,imax),j=1,jmax)
    end do
    call close_file(nf_out)

  end do

end program make_monthly_climatology
