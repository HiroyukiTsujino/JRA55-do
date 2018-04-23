! -*-F90-*-
!
!  Calculate RMSD from monthly climatology
!
!==============================================================
program make_rmsd_from_monthly_climatology

  use file_open_close_manager

  implicit none

  integer(4) :: imax = 640
  integer(4) :: jmax = 320
  integer(4) :: kmax = 3

  integer(4) :: i, j, k, nyear, month, nday, klev, nd
  integer(4) :: nbyr, neyr
  integer(4) :: nbmn, nemn
  integer(4) :: ibmn, iemn

  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  real(4) :: undef_in, undef_out, undef_moncl

  integer(4),allocatable :: maskt(:,:,:)
  integer(4),allocatable :: nvalid(:,:,:)
  real(4),allocatable :: dat2(:,:)
  real(8),allocatable :: dim3(:,:,:)
  real(8),allocatable :: moncl(:,:,:,:)

  character(255) :: infile, infile_base
  character(255) :: outfile, outfile_base
  character(255) :: file_namelist_rmsd_moncl

  character(255) :: infile_moncl, infile_moncl_base

  integer(4) :: mtnam, nf_in, nf_out
  integer(4) :: lrec

!-----------------------------------------------------------------

  namelist /nml_rmsd_moncl/ imax, jmax, kmax, &
       &  infile_moncl_base, undef_moncl,     &
       &  infile_base, undef_in, outfile_base, undef_out, &
       &  nbyr, neyr, nbmn, nemn, klev

!-----------------------------------------------------------------

  file_namelist_rmsd_moncl='namelist.rmsd_monthly_clim'
  nbmn = 1  ! default value
  nemn = 12 ! default value
  call open_file_plain(mtnam,file_namelist_rmsd_moncl)
  read (mtnam,nml_rmsd_moncl) 
  close(mtnam)

  lrec = 4 * imax * jmax

  allocate(maskt(1:imax,1:jmax,1:kmax))
  allocate(nvalid(1:imax,1:jmax,1:kmax))
  allocate(dat2(1:imax,1:jmax))
  allocate(dim3(1:imax,1:jmax,1:kmax))
  allocate(moncl(1:imax,1:jmax,1:kmax,1:12))

!-----------------------------------------------------------------
! read monthly climatology

  do month = 1, 12
    write(infile_moncl,'(1a,i2.2)') trim(infile_moncl_base),month
    write(6,*) ' infile (monthly climatology) = ',trim(infile_moncl)
    call open_file_direct(nf_in,infile_moncl,lrec,action='read')
    do k = 1, klev
      read(nf_in,rec=0*klev+k) ((dat2(i,j),i=1,imax),j=1,jmax)
      moncl(1:imax,1:jmax,k,month) = real(dat2(1:imax,1:jmax),8)
    end do
    call close_file(nf_in)
  end do

!-----------------------------------------------------------------

  dim3(:,:,:) = 0.0d0
  nvalid(:,:,:) = 0

  do nyear = nbyr, neyr

    if (nyear == nbyr) then
      ibmn = nbmn
    else
      ibmn = 1
    end if

    if (nyear == neyr) then
      iemn = nemn
    else
      iemn = 12
    end if

    do month = ibmn, iemn
      write(6,*) 'Year = ', nyear, ' Month = ', month
      write(infile,'(1a,i4.4,i2.2)') trim(infile_base),nyear,month
      call open_file_direct(nf_in,infile,lrec,action='read')
      write(6,*) ' file : ', trim(infile), ' opened '
      do k = 1, klev
        read(nf_in,rec=k) ((dat2(i,j),i=1,imax),j=1,jmax)
        do j = 1, jmax
          do i = 1, imax
            if ((dat2(i,j) /= undef_in) .and. (moncl(i,j,k,month) /= undef_moncl)) then
              dim3(i,j,k) = dim3(i,j,k) &
                   & + sqrt((moncl(i,j,k,month) - real(dat2(i,j),8))**2)
              nvalid(i,j,k) = nvalid(i,j,k) + 1
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
        if (nvalid(i,j,k) > 0) then
          dim3(i,j,k) = dim3(i,j,k) / real(nvalid(i,j,k),8)
        else
          dim3(i,j,k) = real(undef_out,8)
        end if
      end do
    end do
  end do

  call open_file_direct(nf_out,outfile_base,lrec,action='write')
  do k = 1, klev
    write(nf_out,rec=0*klev+k) ((real(dim3(i,j,k),4),i=1,imax),j=1,jmax)
  end do
  call close_file(nf_out)

end program make_rmsd_from_monthly_climatology
