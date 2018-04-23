! -*-F90-*-
!
!  Output monthly data with mask
!
!  mask = 1 : there are valid data for the whole observation priod
!  mask = 0 : there is at least one missing value for this grid point
!
!====================================================================
program make_mask_of_monthly_data

  use file_open_close_manager

  implicit none

  integer(4) :: imax, jmax

  integer(4) :: i, j, k, nyear, month, nday, nd
  integer(4) :: nbyr, neyr

  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)
  integer(4) :: num_day_of_month, ileap
  integer(4) :: ibmn, iemn

  integer(4) :: nf_in = 31
  integer(4) :: nf_out = 41

  integer(4) :: lrec

  integer(4),allocatable :: nvalid(:,:)

  integer(4),allocatable :: imask(:,:)
  real(4),allocatable :: maskt(:,:)
  real(4),allocatable :: work4(:,:)
  real(8),allocatable :: swind(:,:)
  real(8),allocatable :: uwind(:,:)
  real(8),allocatable :: vwind(:,:)

  real(4) :: undef_in, undef_out
  real(4) :: min_ratio
  integer(4) :: min_valid

  character(255) :: infile, infile_dir
  character(255) :: outfile, outfile_dir
  character(255) :: mask_file
  character(255) :: topo_file

  real(4),parameter :: tiny = 1.0e-4

!-----------------------------------------------------------------

  namelist /nml_mask_of_mon/ &
       &  imax, jmax, &
       &  infile_dir, undef_in, outfile_dir, undef_out, &
       &  mask_file, &
       &  topo_file, &
       &  min_ratio, &
       &  nbyr, neyr

!-----------------------------------------------------------------

  open(10,file='namelist.mkmaskmonthly')
  read(10,nml_mask_of_mon) 
  close(10)

  lrec = 4 * imax * jmax

  allocate(imask(1:imax,1:jmax))
  allocate(maskt(1:imax,1:jmax))
  allocate(nvalid(1:imax,1:jmax))
  allocate(work4(1:imax,1:jmax))
  allocate(swind(1:imax,1:jmax))
  allocate(uwind(1:imax,1:jmax))
  allocate(vwind(1:imax,1:jmax))

!-----------------------------------------------------------------

  maskt(1:imax,1:jmax) = 1.0

  LOOP_YEAR_1: do nyear = nbyr, neyr

    ibmn = 1
    iemn = 12

    if (nyear == 1999) ibmn = 8
    if (nyear == 2009) iemn = 10

    LOOP_MONTH_1: do month = ibmn, iemn

      if (month == 2) then
        ileap = 0
        if (mod(nyear, 4)   == 0) ileap = 1
        if (mod(nyear, 100) == 0) ileap = 0
        if (mod(nyear, 400) == 0) ileap = 1
        num_day_of_month = ndmon(month) + ileap
      else
        num_day_of_month = ndmon(month)
      end if

      min_valid = int(min_ratio * real(num_day_of_month,4))

      write(6,*) 'Year = ', nyear, ' Month = ', month, ' min_valid = ', min_valid

      write(infile,'(1a,1a,i4.4,i2.2)') trim(infile_dir),'/qscat_wind_qc.',nyear,month
      write(6,*) ' input file : ', trim(infile), ' opened '
      call open_file_direct(nf_in,infile,lrec)

      read(nf_in,rec=4) ((work4(i,j),i=1,imax),j=1,jmax)
      do j = 1, jmax
        do i = 1, imax
          nvalid(i,j) = int(work4(i,j) + tiny)
        end do
      end do

      call close_file(nf_in)

      do j = 1, jmax
        do i = 1, imax
          if (nvalid(i,j) <= min_valid) then
            maskt(i,j) = 0.0
          end if
        end do
      end do

    end do LOOP_MONTH_1

  end do LOOP_YEAR_1

  !--------------------------------------------------------------------

  write(6,*) ' mask file = ',trim(mask_file)
  call open_file_direct(nf_out,mask_file,lrec)
  write(nf_out,rec=1) maskt
  call close_file(nf_out)

  imask(1:imax,1:jmax) = nint(maskt(1:imax,1:jmax))
  write(6,*) ' topo file = ',trim(topo_file)
  call open_file_sequential(nf_out,topo_file)
  write(nf_out) imask, imask
  call close_file(nf_out)

  !--------------------------------------------------------------------

  LOOP_YEAR_2: do nyear = nbyr, neyr

    ibmn = 1
    iemn = 12

    if (nyear == 1999) ibmn = 8
    if (nyear == 2009) iemn = 10

    LOOP_MONTH_2: do month = ibmn, iemn

      write(6,*) 'Year = ', nyear, ' Month = ', month

      write(infile,'(1a,1a,i4.4,i2.2)') trim(infile_dir),'/qscat_wind_qc.',nyear,month
      write(6,*) ' input file : ', trim(infile), ' opened '
      call open_file_direct(nf_in,infile,lrec)

      read(nf_in,rec=1) ((work4(i,j),i=1,imax),j=1,jmax)
      do j = 1, jmax
        do i = 1, imax
          swind(i,j) = real(work4(i,j),8)
        end do
      end do

      read(nf_in,rec=2) ((work4(i,j),i=1,imax),j=1,jmax)
      do j = 1, jmax
        do i = 1, imax
          uwind(i,j) = real(work4(i,j),8)
        end do
      end do

      read(nf_in,rec=3) ((work4(i,j),i=1,imax),j=1,jmax)
      do j = 1, jmax
        do i = 1, imax
          vwind(i,j) = real(work4(i,j),8)
        end do
      end do

      call close_file(nf_in)

      do j = 1, jmax
        do i = 1, imax
          if (maskt(i,j) == 0.0) then
            swind(i,j) = real(undef_out,8)
            uwind(i,j) = real(undef_out,8)
            vwind(i,j) = real(undef_out,8)
          else
            if (swind(i,j) == real(undef_in,8)) then
              write(6,*) ' mask is not consistent for scalar wind '
              stop
            end if
            if (uwind(i,j) == real(undef_in,8)) then
              write(6,*) ' mask is not consistent for zonal wind '
              stop
            end if
            if (vwind(i,j) == real(undef_in,8)) then
              write(6,*) ' mask is not consistent for meridional wind '
              stop
            end if
          end if
        end do
      end do

      write(outfile,'(1a,1a,i4.4,i2.2)') trim(outfile_dir),'/qscat_wind_mask.',nyear,month
      write(6,*) ' outfile = ',trim(outfile)
      call open_file_direct(nf_out,outfile,lrec)
      write(nf_out,rec=1) ((real(swind(i,j),4),i=1,imax),j=1,jmax)
      write(nf_out,rec=2) ((real(uwind(i,j),4),i=1,imax),j=1,jmax)
      write(nf_out,rec=3) ((real(vwind(i,j),4),i=1,imax),j=1,jmax)
      call close_file(nf_out)

    end do LOOP_MONTH_2

  end do LOOP_YEAR_2

end program make_mask_of_monthly_data
