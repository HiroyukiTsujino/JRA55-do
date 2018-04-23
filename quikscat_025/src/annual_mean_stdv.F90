! -*-F90-*-
!==============================================================
program make_annual_mean_standard_deviation

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

  real(4),allocatable :: work4(:,:)

  integer(4),allocatable :: nvalid_s(:,:)
  integer(4),allocatable :: nvalid_u(:,:)
  integer(4),allocatable :: nvalid_v(:,:)

  real(8),allocatable :: swind_mean(:,:)
  real(8),allocatable :: uwind_mean(:,:)
  real(8),allocatable :: vwind_mean(:,:)

  real(8),allocatable :: swind_stdv(:,:)
  real(8),allocatable :: uwind_stdv(:,:)
  real(8),allocatable :: vwind_stdv(:,:)

  real(4) :: undef_in, undef_out

  character(255) :: infile, infile_dir
  character(255) :: outfile, outfile_dir

!-----------------------------------------------------------------

  namelist /nml_mean_stdv/ &
       &  imax, jmax, &
       &  infile_dir, undef_in, &
       &  outfile, undef_out, &
       &  nbyr, neyr

!-----------------------------------------------------------------

  open(10,file='namelist.meanstdv')
  read(10,nml_mean_stdv) 
  close(10)

  lrec = 4 * imax * jmax

  allocate(work4(1:imax,1:jmax))

  allocate(nvalid_s(1:imax,1:jmax))
  allocate(nvalid_u(1:imax,1:jmax))
  allocate(nvalid_v(1:imax,1:jmax))

  allocate(swind_mean(1:imax,1:jmax))
  allocate(uwind_mean(1:imax,1:jmax))
  allocate(vwind_mean(1:imax,1:jmax))
  allocate(swind_stdv(1:imax,1:jmax))
  allocate(uwind_stdv(1:imax,1:jmax))
  allocate(vwind_stdv(1:imax,1:jmax))

!-----------------------------------------------------------------

  swind_mean(:,:) = 0.0d0
  uwind_mean(:,:) = 0.0d0
  vwind_mean(:,:) = 0.0d0
  nvalid_s(:,:) = 0
  nvalid_u(:,:) = 0
  nvalid_v(:,:) = 0

  LOOP_YEAR_1: do nyear = nbyr, neyr

    ibmn = 1
    iemn = 12

!    if (nyear == 1999) ibmn = 8
    if (nyear == 1999) ibmn = 11
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

      write(6,*) 'Year = ', nyear, ' Month = ', month

      LOOP_DAY_1: do nday = 1, num_day_of_month

        write(infile,'(1a,1a,i4.4,1a,i4.4,i2.2,i2.2)') &
             & trim(infile_dir),'/',nyear,'/qscat_wind.',nyear,month,nday
        write(6,*) ' input file : ', trim(infile), ' opened '
        call open_file_direct(nf_in,infile,lrec)

        read(nf_in,rec=1) ((work4(i,j),i=1,imax),j=1,jmax)
        do j = 1, jmax
          do i = 1, imax
            if (work4(i,j) /= undef_in) then
              swind_mean(i,j) = swind_mean(i,j) + real(work4(i,j),8)
              nvalid_s(i,j) = nvalid_s(i,j) + 1
            end if
          end do
        end do

        read(nf_in,rec=2) ((work4(i,j),i=1,imax),j=1,jmax)
        do j = 1, jmax
          do i = 1, imax
            if (work4(i,j) /= undef_in) then
              uwind_mean(i,j) = uwind_mean(i,j) + real(work4(i,j),8)
              nvalid_u(i,j) = nvalid_u(i,j) + 1
            end if
          end do
        end do

        read(nf_in,rec=3) ((work4(i,j),i=1,imax),j=1,jmax)
        do j = 1, jmax
          do i = 1, imax
            if (work4(i,j) /= undef_in) then
              vwind_mean(i,j) = vwind_mean(i,j) + real(work4(i,j),8)
              nvalid_v(i,j) = nvalid_v(i,j) + 1
            end if
          end do
        end do

        call close_file(nf_in)

      end do LOOP_DAY_1

    end do LOOP_MONTH_1

  end do LOOP_YEAR_1

  do j = 1, jmax
    do i = 1, imax

      if (nvalid_s(i,j) /= nvalid_u(i,j)) then
        write(6,*) ' the number of valid data is different between scalar and zonal'
        stop
      end if
      if (nvalid_s(i,j) /= nvalid_v(i,j)) then
        write(6,*) ' the number of valid data is different between scalar and meridional'
        stop
      end if

      if (nvalid_s(i,j) > 0) then
        swind_mean(i,j) = swind_mean(i,j) / real(nvalid_s(i,j),8)
      else
        swind_mean(i,j) = real(undef_out,8)
      end if
      if (nvalid_u(i,j) > 0) then
        uwind_mean(i,j) = uwind_mean(i,j) / real(nvalid_u(i,j),8)
      else
        uwind_mean(i,j) = real(undef_out,8)
      end if
      if (nvalid_v(i,j) > 0) then
        vwind_mean(i,j) = vwind_mean(i,j) / real(nvalid_v(i,j),8)
      else
        vwind_mean(i,j) = real(undef_out,8)
      end if
    end do
  end do


  !-------------------------------------------------------------

  swind_stdv(:,:) = 0.0d0
  uwind_stdv(:,:) = 0.0d0
  vwind_stdv(:,:) = 0.0d0
  nvalid_s(:,:) = 0
  nvalid_u(:,:) = 0
  nvalid_v(:,:) = 0

  LOOP_YEAR_2: do nyear = nbyr, neyr

    ibmn = 1
    iemn = 12

!    if (nyear == 1999) ibmn = 8
    if (nyear == 1999) ibmn = 11
    if (nyear == 2009) iemn = 10

    LOOP_MONTH_2: do month = ibmn, iemn

      if (month == 2) then
        ileap = 0
        if (mod(nyear, 4)   == 0) ileap = 1
        if (mod(nyear, 100) == 0) ileap = 0
        if (mod(nyear, 400) == 0) ileap = 1
        num_day_of_month = ndmon(month) + ileap
      else
        num_day_of_month = ndmon(month)
      end if

      write(6,*) 'Year = ', nyear, ' Month = ', month

      LOOP_DAY_2: do nday = 1, num_day_of_month

        write(infile,'(1a,1a,i4.4,1a,i4.4,i2.2,i2.2)') &
             & trim(infile_dir),'/',nyear,'/qscat_wind.',nyear,month,nday
        write(6,*) ' input file : ', trim(infile), ' opened '
        call open_file_direct(nf_in,infile,lrec)

        read(nf_in,rec=1) ((work4(i,j),i=1,imax),j=1,jmax)
        do j = 1, jmax
          do i = 1, imax
            if (work4(i,j) /= undef_in) then
              swind_stdv(i,j) = swind_stdv(i,j) &
                   & + (swind_mean(i,j) - real(work4(i,j),8))**2
              nvalid_s(i,j) = nvalid_s(i,j) + 1
            end if
          end do
        end do

        read(nf_in,rec=2) ((work4(i,j),i=1,imax),j=1,jmax)
        do j = 1, jmax
          do i = 1, imax
            if (work4(i,j) /= undef_in) then
              uwind_stdv(i,j) = uwind_stdv(i,j) &
                   & + (uwind_mean(i,j) - real(work4(i,j),8))**2
              nvalid_u(i,j) = nvalid_u(i,j) + 1
            end if
          end do
        end do

        read(nf_in,rec=3) ((work4(i,j),i=1,imax),j=1,jmax)
        do j = 1, jmax
          do i = 1, imax
            if (work4(i,j) /= undef_in) then
              vwind_stdv(i,j) = vwind_stdv(i,j) &
                   & + (vwind_stdv(i,j) - real(work4(i,j),8))**2
              nvalid_v(i,j) = nvalid_v(i,j) + 1
            end if
          end do
        end do

        call close_file(nf_in)

      end do LOOP_DAY_2

    end do LOOP_MONTH_2

  end do LOOP_YEAR_2

  !-------------------------------------------------------------

  do j = 1, jmax
    do i = 1, imax
      if (nvalid_s(i,j) > 0) then
        swind_stdv(i,j) = sqrt(swind_stdv(i,j) / real(nvalid_s(i,j),8))
      else
        swind_stdv(i,j) = real(undef_out,8)
      end if
      if (nvalid_u(i,j) > 0) then
        uwind_stdv(i,j) = sqrt(uwind_stdv(i,j) / real(nvalid_u(i,j),8))
      else
        uwind_stdv(i,j) = real(undef_out,8)
      end if
      if (nvalid_v(i,j) > 0) then
        vwind_stdv(i,j) = sqrt(vwind_stdv(i,j) / real(nvalid_v(i,j),8))
      else
        vwind_stdv(i,j) = real(undef_out,8)
      end if
    end do
  end do

  write(6,*) ' outfile = ',trim(outfile)
  call open_file_direct(nf_out,outfile,lrec)
  write(nf_out,rec=1) ((real(swind_mean(i,j),4),i=1,imax),j=1,jmax)
  write(nf_out,rec=2) ((real(uwind_mean(i,j),4),i=1,imax),j=1,jmax)
  write(nf_out,rec=3) ((real(vwind_mean(i,j),4),i=1,imax),j=1,jmax)
  write(nf_out,rec=4) ((real(swind_stdv(i,j),4),i=1,imax),j=1,jmax)
  write(nf_out,rec=5) ((real(uwind_stdv(i,j),4),i=1,imax),j=1,jmax)
  write(nf_out,rec=6) ((real(vwind_stdv(i,j),4),i=1,imax),j=1,jmax)
  call close_file(nf_out)

end program make_annual_mean_standard_deviation
