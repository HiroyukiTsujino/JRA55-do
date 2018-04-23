! -*-F90-*-
!
!  Apply 1-2-1 filter to monthly time series
!
!==============================================================
program make_monthly_climatology

  use file_open_close_manager

  implicit none

  integer(4) :: imax = 640
  integer(4) :: jmax = 320
  integer(4) :: kmax = 3

  integer(4) :: i, j, k, nyear, month, nday, klev, nd, y_tmp, m_tmp
  integer(4) :: nbyr, neyr
  integer(4) :: nbmn, nemn
  integer(4) :: ibmn, iemn

  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  real(4) :: undef_in, undef_out

  integer(4),allocatable :: maskt(:,:,:)
  integer(4),allocatable :: nvalid(:,:,:)
  real(4),allocatable :: dat2(:,:)
  real(8),allocatable :: dim3(:,:,:)

  character(255) :: infile, infile_base
  character(255) :: outfile, outfile_base
  character(255) :: file_namelist_mon_filter

  integer(4) :: mtnam, nf_in, nf_out
  integer(4) :: lrec

!-----------------------------------------------------------------

  namelist /nml_mon_filter/ imax, jmax, kmax, &
       &  infile_base, undef_in, outfile_base, undef_out, &
       &  nbyr, neyr, nbmn, nemn, klev

!-----------------------------------------------------------------

  file_namelist_mon_filter='namelist.monthly_filter'
  nbmn = 1  ! default value
  nemn = 12 ! default value
  call open_file_plain(mtnam,file_namelist_mon_filter)
  read (mtnam,nml_mon_filter) 
  close(mtnam)

  lrec = 4 * imax * jmax

  allocate(maskt(1:imax,1:jmax,1:kmax))
  allocate(nvalid(1:imax,1:jmax,1:kmax))
  allocate(dat2(1:imax,1:jmax))
  allocate(dim3(1:imax,1:jmax,1:kmax))

!-----------------------------------------------------------------

  do nyear = nbyr, neyr

    if (nyear == nbyr) then
      ibmn = nbmn + 1
    else
      ibmn = 1
    end if

    if (nyear == neyr) then
      iemn = nemn - 1
    else
      iemn = 12
    end if

    if ((ibmn == 13) .or. (iemn == 0)) cycle

    do month = ibmn, iemn

      write(6,*) 'Year = ', nyear, ' Month = ', month

      dim3(:,:,:) = 0.0d0
      nvalid(:,:,:) = 0

      ! - 1

      m_tmp = month - 1
      y_tmp = nyear
      if (m_tmp == 0) then
        y_tmp = nyear - 1
        m_tmp = 12
      end if

      write(infile,'(1a,i4.4,i2.2)') trim(infile_base),y_tmp,m_tmp
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

      ! 0

      m_tmp = month
      y_tmp = nyear

      write(infile,'(1a,i4.4,i2.2)') trim(infile_base),y_tmp,m_tmp
      call open_file_direct(nf_in,infile,lrec,action='read')
      write(6,*) ' file : ', trim(infile), ' opened '
      do k = 1, klev
        read(nf_in,rec=k) ((dat2(i,j),i=1,imax),j=1,jmax)
        do j = 1, jmax
          do i = 1, imax
            if (dat2(i,j) /= undef_in) then
              dim3(i,j,k) = dim3(i,j,k) + real(dat2(i,j),8) * 2.0d0
              nvalid(i,j,k) = nvalid(i,j,k) + 2
            end if
          end do
        end do
      end do
      call close_file(nf_in)

      ! 1

      m_tmp = month + 1
      y_tmp = nyear
      if (m_tmp == 13) then
        y_tmp = nyear + 1
        m_tmp = 1
      end if
      write(infile,'(1a,i4.4,i2.2)') trim(infile_base),y_tmp,m_tmp
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

      write(outfile,'(1a,i4.4,i2.2)') trim(outfile_base),nyear,month
      write(6,*) ' outfile = ',trim(outfile)
      call open_file_direct(nf_out,outfile,lrec,action='write')
      do k = 1, klev
        write(nf_out,rec=0*klev+k) ((real(dim3(i,j,k),4),i=1,imax),j=1,jmax)
      end do
      call close_file(nf_out)

    end do

  end do

end program make_monthly_climatology
