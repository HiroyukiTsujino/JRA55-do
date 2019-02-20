! -*-F90-*-
!
!  Calculate annual mean climatology
!
!==============================================================
program make_annual_mean_climatology

  use file_open_close_manager

  implicit none

  integer(4) :: imax = 640
  integer(4) :: jmax = 320

  integer(4) :: i, j, k, nyear, month, mday
  integer(4) :: nbyr, neyr, nbmn, nemn, ibmn, iemn

  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  real(4) :: undef_in
  real(4) :: undef_out

  integer(4),allocatable :: maskt(:,:)
  real(4),allocatable :: datu(:,:)
  real(4),allocatable :: datv(:,:)
  real(4),allocatable :: navu(:,:)
  real(4),allocatable :: navv(:,:)

  real(8),allocatable :: u_cl(:,:)
  real(8),allocatable :: v_cl(:,:)
  real(4),allocatable :: nvalid_u(:,:)
  real(4),allocatable :: nvalid_v(:,:)

  character(255) :: infile, infile_base
  character(255) :: outfile, outfile_base
  character(255) :: file_namelist_anncl

  integer(4) :: mtnam, nf_in, nf_out
  integer(4) :: lrec

  logical :: l_leap, l_leap_valid

!-----------------------------------------------------------------

  namelist /nml_anncl/ imax, jmax, &
       &  infile_base, undef_in, outfile_base, undef_out, &
       &  nbyr, nbmn, neyr, nemn, l_leap_valid

!-----------------------------------------------------------------

  file_namelist_anncl='namelist.annual_clim'
  call open_file_plain(mtnam,file_namelist_anncl)
  read (mtnam,nml_anncl) 
  close(mtnam)

  write(6,*) ' Leap is taken into consideration : ', l_leap_valid

  lrec = 4 * imax * jmax

  allocate(maskt(1:imax,1:jmax))

  allocate(datu(1:imax,1:jmax),datv(1:imax,1:jmax))
  allocate(navu(1:imax,1:jmax),navv(1:imax,1:jmax))

  allocate(u_cl(1:imax,1:jmax),v_cl(1:imax,1:jmax))
  allocate(nvalid_u(1:imax,1:jmax),nvalid_v(1:imax,1:jmax))

!-----------------------------------------------------------------

  u_cl(:,:) = 0.0d0
  v_cl(:,:) = 0.0d0
  nvalid_u(:,:) = 0.0e0
  nvalid_v(:,:) = 0.0e0

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

      read(nf_in,rec=1) ((datu(i,j),i=1,imax),j=1,jmax)
      read(nf_in,rec=2) ((datv(i,j),i=1,imax),j=1,jmax)
      read(nf_in,rec=3) ((navu(i,j),i=1,imax),j=1,jmax)
      read(nf_in,rec=4) ((navv(i,j),i=1,imax),j=1,jmax)

      do j = 1, jmax
        do i = 1, imax
          if ((datu(i,j) /= undef_in) .and. (navu(i,j) > 0.0e0)) then
            u_cl(i,j) = u_cl(i,j) + real(datu(i,j),8) * real(navu(i,j),8)
            nvalid_u(i,j) = nvalid_u(i,j) + navu(i,j)
          end if
          if ((datv(i,j) /= undef_in) .and. (navv(i,j) > 0.0e0)) then
            v_cl(i,j) = v_cl(i,j) + real(datv(i,j),8) * real(navv(i,j),8)
            nvalid_v(i,j) = nvalid_v(i,j) + navv(i,j)
          end if
        end do
      end do

      call close_file(nf_in)

    end do

  end do

  do j = 1, jmax
    do i = 1, imax
      if ((nvalid_u(i,j) > 0.0e0) .and. (nvalid_v(i,j) > 0.0e0)) then
        if (nvalid_u(i,j) == nvalid_v(i,j)) then
          u_cl(i,j) = u_cl(i,j) / real(nvalid_u(i,j),8)
          v_cl(i,j) = v_cl(i,j) / real(nvalid_v(i,j),8)
        else
          write(6,*) ' nvalid_u /= nvalid_v, check the input data '
          stop
        end if
      else
        if (nvalid_u(i,j) == nvalid_v(i,j)) then
          u_cl(i,j) = real(undef_out,8)
          v_cl(i,j) = real(undef_out,8)
        else
          write(6,*) ' nvalid_u /= nvalid_v, check the input data '
          stop
        end if
      end if
    end do
  end do

  write(outfile,'(1a)') trim(outfile_base)

  call open_file_direct(nf_out,outfile,lrec,action='write')
  write(nf_out,rec=1) real(u_cl(1:imax,1:jmax),4)
  write(nf_out,rec=2) real(v_cl(1:imax,1:jmax),4)
  write(nf_out,rec=3) nvalid_u(1:imax,1:jmax)
  write(nf_out,rec=4) nvalid_v(1:imax,1:jmax)
  call close_file(nf_out)

end program make_annual_mean_climatology
