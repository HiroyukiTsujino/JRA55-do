! -*-F90-*-
!
!  monthly data from daily data
!
!==============================================================
program make_monthly_from_daily

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

  real(4),allocatable :: work4(:,:)
  real(4),allocatable :: qc_passed(:,:)

  real(8),allocatable :: swind(:,:)
  real(8),allocatable :: uwind(:,:)
  real(8),allocatable :: vwind(:,:)

  real(8),allocatable :: swind_mean(:,:)
  real(8),allocatable :: uwind_mean(:,:)
  real(8),allocatable :: vwind_mean(:,:)
  real(8),allocatable :: swind_stdv(:,:)
  real(8),allocatable :: uwind_stdv(:,:)
  real(8),allocatable :: vwind_stdv(:,:)

  real(4) :: undef_in, undef_out
  real(4) :: min_ratio
  integer(4) :: min_valid

  character(255) :: infile, infile_dir
  character(255) :: outfile, outfile_dir
  character(255) :: file_mnsd

!-----------------------------------------------------------------

  namelist /nml_day_qc_mon/ &
       &  imax, jmax, &
       &  infile_dir, undef_in, outfile_dir, undef_out, &
       &  file_mnsd, &
       &  min_ratio, &
       &  nbyr, neyr

!-----------------------------------------------------------------

  open(10,file='namelist.qcmonthly')
  read(10,nml_day_qc_mon) 
  close(10)

  lrec = 4 * imax * jmax

  allocate(work4(1:imax,1:jmax))

  allocate(nvalid(1:imax,1:jmax))

  allocate(swind(1:imax,1:jmax))
  allocate(uwind(1:imax,1:jmax))
  allocate(vwind(1:imax,1:jmax))

  allocate(qc_passed(1:imax,1:jmax))

  allocate(swind_mean(1:imax,1:jmax))
  allocate(uwind_mean(1:imax,1:jmax))
  allocate(vwind_mean(1:imax,1:jmax))

  allocate(swind_stdv(1:imax,1:jmax))
  allocate(uwind_stdv(1:imax,1:jmax))
  allocate(vwind_stdv(1:imax,1:jmax))

!-----------------------------------------------------------------

  write(6,*) ' Mean and Standard deviation read from ... ',trim(file_mnsd)
  call open_file_direct(nf_in,file_mnsd,lrec)
  read(nf_in,rec=1) work4
  swind_mean(1:imax,1:jmax) = real(work4(1:imax,1:jmax),4)
  read(nf_in,rec=2) work4
  uwind_mean(1:imax,1:jmax) = real(work4(1:imax,1:jmax),4)
  read(nf_in,rec=3) work4
  vwind_mean(1:imax,1:jmax) = real(work4(1:imax,1:jmax),4)
  read(nf_in,rec=4) work4
  swind_stdv(1:imax,1:jmax) = real(work4(1:imax,1:jmax),4)
  read(nf_in,rec=5) work4
  uwind_stdv(1:imax,1:jmax) = real(work4(1:imax,1:jmax),4)
  read(nf_in,rec=6) work4
  vwind_stdv(1:imax,1:jmax) = real(work4(1:imax,1:jmax),4)
  call close_file(nf_in)

!-----------------------------------------------------------------

  LOOP_YEAR: do nyear = nbyr, neyr

    ibmn = 1
    iemn = 12

    if (nyear == 1999) ibmn = 8
    if (nyear == 2009) iemn = 10

    LOOP_MONTH: do month = ibmn, iemn

      if (month == 2) then
        ileap = 0
        if (mod(nyear, 4)   == 0) ileap = 1
        if (mod(nyear, 100) == 0) ileap = 0
        if (mod(nyear, 400) == 0) ileap = 1
        num_day_of_month = ndmon(month) + ileap
      else
        num_day_of_month = ndmon(month)
      end if

      swind(:,:) = 0.0d0
      uwind(:,:) = 0.0d0
      vwind(:,:) = 0.0d0
      nvalid(:,:) = 0

      min_valid = int(min_ratio * real(num_day_of_month,4))

      write(6,*) 'Year = ', nyear, ' Month = ', month, ' min_valid = ', min_valid

      LOOP_DAY: do nday = 1, num_day_of_month

        qc_passed(:,:) = 0.0

        write(infile,'(1a,1a,i4.4,1a,i4.4,i2.2,i2.2)') &
             & trim(infile_dir),'/',nyear,'/qscat_wind.',nyear,month,nday
        write(6,*) ' input file : ', trim(infile), ' opened '
        call open_file_direct(nf_in,infile,lrec)

        read(nf_in,rec=1) ((work4(i,j),i=1,imax),j=1,jmax)
        do j = 1, jmax
          do i = 1, imax
            if (work4(i,j) /= undef_in) then
              if (real(work4(i,j),8) < (swind_mean(i,j) + 3.0d0 * swind_stdv(i,j))) then
                qc_passed(i,j) = 1.0
                swind(i,j) = swind(i,j) + real(work4(i,j),8)
                nvalid(i,j) = nvalid(i,j) + 1
              end if
            end if
          end do
        end do

        read(nf_in,rec=2) ((work4(i,j),i=1,imax),j=1,jmax)
        do j = 1, jmax
          do i = 1, imax
            if ((work4(i,j) /= undef_in) .and. (qc_passed(i,j) == 1.0)) then
              uwind(i,j) = uwind(i,j) + real(work4(i,j),8)
            end if
          end do
        end do

        read(nf_in,rec=3) ((work4(i,j),i=1,imax),j=1,jmax)
        do j = 1, jmax
          do i = 1, imax
            if ((work4(i,j) /= undef_in) .and. (qc_passed(i,j) == 1.0)) then
              vwind(i,j) = vwind(i,j) + real(work4(i,j),8)
            end if
          end do
        end do

        call close_file(nf_in)

      end do LOOP_DAY

      do j = 1, jmax
        do i = 1, imax
          if (nvalid(i,j) > min_valid) then
            swind(i,j) = swind(i,j) / real(nvalid(i,j),8)
            uwind(i,j) = uwind(i,j) / real(nvalid(i,j),8)
            vwind(i,j) = vwind(i,j) / real(nvalid(i,j),8)
          else
            swind(i,j) = real(undef_out,8)
            uwind(i,j) = real(undef_out,8)
            vwind(i,j) = real(undef_out,8)
          end if
        end do
      end do

      write(outfile,'(1a,1a,i4.4,i2.2)') trim(outfile_dir),'/qscat_wind_qc.',nyear,month
      write(6,*) ' outfile = ',trim(outfile)
      call open_file_direct(nf_out,outfile,lrec)
      write(nf_out,rec=1) ((real(swind(i,j),4),i=1,imax),j=1,jmax)
      write(nf_out,rec=2) ((real(uwind(i,j),4),i=1,imax),j=1,jmax)
      write(nf_out,rec=3) ((real(vwind(i,j),4),i=1,imax),j=1,jmax)
      write(nf_out,rec=4) ((real(nvalid(i,j),4),i=1,imax),j=1,jmax)
      call close_file(nf_out)

    end do LOOP_MONTH

  end do LOOP_YEAR

end program make_monthly_from_daily
