!-*-F90-*-
module force_process

  use libmxe_calendar
  use file_open_close_manager

  implicit none

  type :: type_force
    character(len=16) :: item
    character(len=256) :: file_name_base   ! for example, file_name_base.YYYYMMDDHH
    integer(4) :: lunit
    type(type_calendar) :: file_name_date
    integer(4) :: file_intv_type    ! 0: all data in one file, 1: annual, 2: monthly, 3: daily, 4: hourly, 5: minute, 6: second
    integer(4) :: file_intv
    integer(4) :: num_total_record
    integer(4) :: data_intv_sec     ! -1: monthly, or integer second
    type(type_calendar) :: first_data
    type(type_calendar) :: latest_data
    integer(4) :: latest_record
    integer(4) :: im, jm
    real(8) :: ar, br
    real(4) :: rundef
    real(8) :: dundef
    real(8),pointer :: dat_a(:,:), dat_b(:,:)
  end type type_force

  integer(4),parameter :: num_month = 12
  integer(4),save :: idmon(num_month)

  logical(4),save :: l_use_leap

contains

  !============================================================
  subroutine force_process__ini(l_leap_valid)

    logical, intent(in) :: l_leap_valid

    l_use_leap = l_leap_valid

    idmon(1:num_month) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  end subroutine force_process__ini
  !============================================================
  subroutine register_file( &
       &  item, &
       &  file_data,  &
       &  file_base,  &
       &  file_first, &
       &  file_intv_type, &
       &  file_intv, &
       &  total_rec, &
       &  data_intv_sec, &
       &  first_data, &
       &  im, jm,  &
       &  undef )

    character(len=*),intent(in) :: item
    type(type_force),intent(inout) :: file_data
    character(len=*),intent(in) ::  file_base
    integer(4),intent(in) :: file_first(6)
    integer(4),intent(in) :: file_intv_type
    integer(4),intent(in) :: file_intv
    integer(4),intent(in) :: total_rec
    integer(4),intent(in) :: data_intv_sec
    integer(4),intent(in) :: first_data(6)
    integer(4),intent(in) :: im, jm
    real(4),intent(in)    :: undef

    integer(4) :: this_month_sec
    type(type_calendar) :: first_month_start

    file_data%item = item
    file_data%file_name_base = file_base
    file_data%file_name_date%year   = file_first(1)
    file_data%file_name_date%month  = file_first(2)
    file_data%file_name_date%day    = file_first(3)
    file_data%file_name_date%hour   = file_first(4)
    file_data%file_name_date%minute = file_first(5)
    file_data%file_name_date%second = file_first(6)
    file_data%file_intv_type  = file_intv_type
    file_data%file_intv  = file_intv
    file_data%num_total_record = total_rec
    file_data%data_intv_sec = data_intv_sec


    file_data%first_data%year   = first_data(1)
    file_data%first_data%month  = first_data(2)
    file_data%first_data%day    = first_data(3)
    file_data%first_data%hour   = first_data(4)
    file_data%first_data%minute = first_data(5)
    file_data%first_data%second = first_data(6)

    if (file_data%data_intv_sec == -1) then
      first_month_start%year   = file_data%first_data%year
      first_month_start%month  = file_data%first_data%month
      first_month_start%day    = 1
      first_month_start%hour   = 0
      first_month_start%minute = 0
      first_month_start%second = 0
      this_month_sec = days_of_month(first_month_start) * 86400
      file_data%first_data = libmxe_calendar__addsec(first_month_start, this_month_sec/2, l_use_leap)
      write(6,*) ' Date of the first data for item ', trim(file_data%item),' (monthly) is reset. '
      write(6,*) file_data%first_data%year
      write(6,*) file_data%first_data%month
      write(6,*) file_data%first_data%day
      write(6,*) file_data%first_data%hour
      write(6,*) file_data%first_data%minute
      write(6,*) file_data%first_data%second
    end if

    file_data%rundef = real(undef,4)
    file_data%dundef = real(undef,8)
    file_data%im = im
    file_data%jm = jm
    allocate(file_data%dat_a(1:file_data%im,1:file_data%jm))
    allocate(file_data%dat_b(1:file_data%im,1:file_data%jm))

  end subroutine register_file
  !============================================================
  subroutine update_data(present,file_data,base_dir,l_ymdir)

    type(type_calendar),intent(in) :: present
    type(type_force),intent(inout) :: file_data
    character(len=*),intent(in) :: base_dir
    logical,intent(in) :: l_ymdir

    character(len=128) :: base_dir_tmp

    type(type_calendar) :: prev_data
    type(type_calendar) :: latest_month_start
    type(type_calendar) :: prev_month_start
    integer(4) :: this_month_sec
    integer(4) :: prev_month_sec
    integer(4) :: latest_month_sec
    integer(4) :: until_latest_data
    integer(4) :: total_rec
    integer(4) :: newrec
    character(len=128) :: file_name
    integer(4) :: mttmp
    integer(4) :: lreclen
    real(4),allocatable :: work4(:,:)
    logical :: l_proceed

    !----------------------------------------------------------

    if (file_data%file_intv_type == 0) then

      write(6,*) ' Sorry, all data in one file is not supported '

    else

      until_latest_data = libmxe_calendar__diffsec(file_data%latest_data,present,l_use_leap)

      if (until_latest_data > 0) then ! should read new data

        allocate(work4(1:file_data%im,1:file_data%jm))

        file_data%dat_b(1:file_data%im,1:file_data%jm) = file_data%dat_a(1:file_data%im,1:file_data%jm)
        newrec = file_data%latest_record + 1 ! read new data
        prev_data = file_data%latest_data

        if (newrec > file_data%num_total_record) then ! => new file

          l_proceed = .true.

          call create_file_date(file_data, l_proceed)
          if (l_ymdir) then
            write(base_dir_tmp,'(1a,1a,i4.4,i2.2)') &
                 & trim(base_dir),'/',file_data%file_name_date%year,file_data%file_name_date%month
          else
            write(base_dir_tmp,'(1a)') trim(base_dir)
          end if
          call create_file_name(file_name,file_data,base_dir_tmp)
          call calc_total_rec_file(file_data)
          newrec = 1
          write(6,*) ' New File   ', trim(file_name)
          write(6,*) '  total rec ', file_data%num_total_record
          write(6,*) '       read ', newrec

          call close_file(file_data%lunit)
          lreclen = 4 * file_data%im * file_data%jm
          call open_file_direct(mttmp, file_name, lreclen)
          file_data%lunit = mttmp

          read(file_data%lunit,rec=newrec) work4
          file_data%dat_a(1:file_data%im,1:file_data%jm) = real(work4(1:file_data%im,1:file_data%jm),8)
          where(file_data%dat_a(1:file_data%im,1:file_data%jm) == file_data%dundef) &
               & file_data%dat_a(1:file_data%im,1:file_data%jm) = 0.0d0
          
          if (file_data%data_intv_sec > 0) then
            file_data%latest_data = libmxe_calendar__addsec(prev_data, file_data%data_intv_sec, l_use_leap)
          else
            if (prev_data%month == 12) then
              latest_month_start%year   = prev_data%year + 1
              latest_month_start%month  = 1
            else
              latest_month_start%year   = prev_data%year
              latest_month_start%month  = prev_data%month + 1
            end if
            latest_month_start%day    = 1
            latest_month_start%hour   = 0
            latest_month_start%minute = 0
            latest_month_start%second = 0
            this_month_sec = days_of_month(latest_month_start) * 86400
            file_data%latest_data = libmxe_calendar__addsec(latest_month_start, this_month_sec/2, l_use_leap)
            file_data%first_data = file_data%latest_data
          end if

          if ( (file_data%latest_data%year == file_data%first_data%year) &
               .and. (file_data%latest_data%month == file_data%first_data%month) &
               .and. (file_data%latest_data%day == file_data%first_data%day) &
               .and. (file_data%latest_data%hour == file_data%first_data%hour) &
               .and. (file_data%latest_data%minute == file_data%first_data%minute) &
               .and. (file_data%latest_data%second == file_data%first_data%second) ) then
            file_data%latest_data = file_data%first_data
            write(6,*) ' ===== data and calendar consistency check ', trim(file_data%item), ' O.K. ====='
          else
            write(6,*) ' ====== data and calendar inconsistent ', trim(file_data%item), ' terminating =====' 
            stop
          end if

        else ! => read next record

          write(6,*) trim(file_data%item),' read new record ', newrec

          read(file_data%lunit,rec=newrec) work4
          file_data%dat_a(1:file_data%im,1:file_data%jm) = real(work4(1:file_data%im,1:file_data%jm),8)
          where(file_data%dat_a(1:file_data%im,1:file_data%jm) == file_data%dundef) &
               & file_data%dat_a(1:file_data%im,1:file_data%jm) = 0.0d0

          if (file_data%data_intv_sec > 0) then
            file_data%latest_data = libmxe_calendar__addsec(prev_data, file_data%data_intv_sec, l_use_leap)
          else ! monthly
            if (prev_data%month == 12) then
              latest_month_start%year   = prev_data%year + 1
              latest_month_start%month  = 1
            else
              latest_month_start%year   = prev_data%year
              latest_month_start%month  = prev_data%month + 1
            end if
            latest_month_start%day    = 1
            latest_month_start%hour   = 0
            latest_month_start%minute = 0
            latest_month_start%second = 0
            this_month_sec = days_of_month(latest_month_start) * 86400
            file_data%latest_data = libmxe_calendar__addsec(latest_month_start, this_month_sec/2, l_use_leap)
          end if

        end if

        file_data%latest_record = newrec

        deallocate(work4)

      end if

      if (file_data%data_intv_sec > 0) then

        until_latest_data = libmxe_calendar__diffsec(present,file_data%latest_data,l_use_leap)
        file_data%br = real(until_latest_data,8) / real(file_data%data_intv_sec,8)
        file_data%ar = 1.0d0 - file_data%br

      else

        if (file_data%latest_data%month == 1) then
          prev_month_start%year   = file_data%latest_data%year - 1
          prev_month_start%month  = 12
        else
          prev_month_start%year   = file_data%latest_data%year
          prev_month_start%month  = file_data%latest_data%month - 1
        end if
        prev_month_start%day    = 1
        prev_month_start%hour   = 0
        prev_month_start%minute = 0
        prev_month_start%second = 0

        prev_month_sec = days_of_month(prev_month_start) * 86400
        latest_month_sec = days_of_month(file_data%latest_data) * 86400
        until_latest_data = libmxe_calendar__diffsec(present,file_data%latest_data,l_use_leap)
        !write(6,*) trim(file_data%item), ' until_latest_data ', until_latest_data
        file_data%br = 2.0d0 * real(until_latest_data,8) / real(prev_month_sec+latest_month_sec,8)
        file_data%ar = 1.0d0 - file_data%br

      end if

      write(6,*) trim(file_data%item), ' br  ar ', file_data%br, file_data%ar

    end if

  end subroutine update_data
  !============================================================
  subroutine get_first_two_data(present,file_data,base_dir,l_ymdir)

    type(type_calendar),intent(in) :: present
    type(type_force),intent(inout) :: file_data
    character(len=*),intent(in) :: base_dir
    logical,intent(in) :: l_ymdir

    character(128) :: base_dir_tmp

    type(type_calendar) :: prev_data

    type(type_calendar) :: present_month_start
    type(type_calendar) :: next_month_start
    type(type_calendar) :: prev_month_start
    integer(4) :: this_month_sec
    integer(4) :: next_month_sec
    integer(4) :: prev_month_sec
    integer(4) :: latest_month_sec
    integer(4) :: present_sec_of_month

    integer(4) :: total_rec
    integer(4) :: from_first_data
    integer(4) :: until_latest_data
    integer(4) :: from_prev_first_data
    integer(4) :: data_intv_sec
    integer(4) :: itmp
    integer(4) :: recnum
    integer(4) :: mttmp
    integer(4) :: lreclen
    character(len=128) :: file_name
    real(4),allocatable :: work4(:,:)
    logical :: l_proceed

    !-----
    
    allocate(work4(1:file_data%im,1:file_data%jm))

    if (file_data%file_intv_type == 0) then

      write(6,*) ' Sorry, all data in one file is not supported '

    else

      from_first_data = libmxe_calendar__diffsec(file_data%first_data,present,l_use_leap)

      !write(6,*) file_data%first_data%year
      !write(6,*) file_data%first_data%month
      !write(6,*) file_data%first_data%day
      !write(6,*) file_data%first_data%hour
      !write(6,*) file_data%first_data%minute
      !write(6,*) file_data%first_data%second

      if (from_first_data >= 0) then
        
        if (file_data%data_intv_sec > 0) then

          recnum = from_first_data / file_data%data_intv_sec + 1

          prev_data = libmxe_calendar__addsec(file_data%first_data, (recnum-1)*file_data%data_intv_sec, l_use_leap)
          file_data%latest_data = libmxe_calendar__addsec(prev_data, file_data%data_intv_sec, l_use_leap)

        else ! monthly data

          present_month_start%year   = present%year
          present_month_start%month  = present%month
          present_month_start%day    = 1
          present_month_start%hour   = 0
          present_month_start%minute = 0
          present_month_start%second = 0

          if (present%month == 12) then
            next_month_start%year   = present%year + 1
            next_month_start%month  = 1
          else
            next_month_start%year   = present%year
            next_month_start%month  = present%month + 1
          end if
          next_month_start%day    = 1
          next_month_start%hour   = 0
          next_month_start%minute = 0
          next_month_start%second = 0

          this_month_sec = days_of_month(present_month_start) * 86400
          next_month_sec = days_of_month(next_month_start) * 86400

          present_sec_of_month = libmxe_calendar__diffsec(present_month_start, present, l_use_leap)

          if (present_sec_of_month < 0) then
            write(6,*) ' Program error, present_sec_of_month is negative'
            stop
          end if

          if (present_sec_of_month > this_month_sec/2) then
            recnum = (present%year - file_data%first_data%year) * 12 &
                 & + (present%month - file_data%first_data%month) + 1
            file_data%latest_data = libmxe_calendar__addsec(next_month_start, next_month_sec/2, l_use_leap)
          else
            recnum = (present%year - file_data%first_data%year) * 12 &
                 & + (present%month - file_data%first_data%month)
            file_data%latest_data = libmxe_calendar__addsec(present_month_start, this_month_sec/2, l_use_leap)
          end if

          if (recnum <= 0) then
            write(6,*) ' Program error, recnum is not positive integer '
            stop
          end if

        end if

        if (l_ymdir) then
          write(base_dir_tmp,'(1a,1a,i4.4,i2.2)') &
               & trim(base_dir),'/',file_data%file_name_date%year,file_data%file_name_date%month
        else
          write(base_dir_tmp,'(1a)') trim(base_dir)
        end if

        call create_file_name(file_name,file_data,base_dir_tmp)
        call calc_total_rec_file(file_data)

        if (file_data%num_total_record > recnum + 1) then
            
          write(6,*) ' File (b,a) ', trim(file_name)
          write(6,*) '  total rec ', file_data%num_total_record
          write(6,*) '       read ', recnum
          write(6,*) '       read ', recnum + 1

          lreclen = 4 * file_data%im * file_data%jm
          call open_file_direct(mttmp, file_name, lreclen)
          file_data%lunit = mttmp
          read(file_data%lunit,rec=recnum) work4
          file_data%dat_b(1:file_data%im,1:file_data%jm) = real(work4(1:file_data%im,1:file_data%jm),8)
          where(file_data%dat_b(1:file_data%im,1:file_data%jm) == file_data%dundef) &
               & file_data%dat_b(1:file_data%im,1:file_data%jm) = 0.0d0
          read(file_data%lunit,rec=recnum+1) work4
          file_data%dat_a(1:file_data%im,1:file_data%jm) = real(work4(1:file_data%im,1:file_data%jm),8)
          where(file_data%dat_a(1:file_data%im,1:file_data%jm) == file_data%dundef) &
               & file_data%dat_a(1:file_data%im,1:file_data%jm) = 0.0d0
          

        else if (file_data%num_total_record == recnum) then

          write(6,*) ' File 1 (b) ', trim(file_name)
          write(6,*) '  total rec ', file_data%num_total_record
          write(6,*) '       read ', recnum

          lreclen = 4 * file_data%im * file_data%jm
          call open_file_direct(mttmp, file_name, lreclen)
          file_data%lunit = mttmp
          read(file_data%lunit,rec=recnum) work4
          file_data%dat_b(1:file_data%im,1:file_data%jm) = real(work4(1:file_data%im,1:file_data%jm),8)
          call close_file(file_data%lunit)
          where(file_data%dat_b(1:file_data%im,1:file_data%jm) == file_data%dundef) &
               & file_data%dat_b(1:file_data%im,1:file_data%jm) = 0.0d0
          
          l_proceed = .true.
          call create_file_date(file_data, l_proceed)
          if (l_ymdir) then
            write(base_dir_tmp,'(1a,1a,i4.4,i2.2)') &
                 & trim(base_dir),'/',file_data%file_name_date%year,file_data%file_name_date%month
          else
            write(base_dir_tmp,'(1a)') trim(base_dir)
          end if
          call create_file_name(file_name,file_data,base_dir_tmp)
          call calc_total_rec_file(file_data)
          recnum = 1
          write(6,*) ' File 2 (a) ', trim(file_name)
          write(6,*) '  total rec ', file_data%num_total_record
          write(6,*) '       read ', recnum
          
          lreclen = 4 * file_data%im * file_data%jm
          call open_file_direct(mttmp, file_name, lreclen)
          file_data%lunit = mttmp
          read(file_data%lunit,rec=recnum) work4
          file_data%dat_a(1:file_data%im,1:file_data%jm) = real(work4(1:file_data%im,1:file_data%jm),8)
          where(file_data%dat_a(1:file_data%im,1:file_data%jm) == file_data%dundef) &
               & file_data%dat_a(1:file_data%im,1:file_data%jm) = 0.0d0
          
        end if

        if (file_data%data_intv_sec > 0) then

          file_data%latest_record = recnum + 1

          until_latest_data = libmxe_calendar__diffsec(present,file_data%latest_data,l_use_leap)
          file_data%br = real(until_latest_data,8) / real(file_data%data_intv_sec,8)
          file_data%ar = 1.0d0 - file_data%br

        else ! monthly

          if (file_data%latest_data%month == 1) then
            prev_month_start%year   = file_data%latest_data%year - 1
            prev_month_start%month  = 12
          else
            prev_month_start%year   = file_data%latest_data%year
            prev_month_start%month  = file_data%latest_data%month + 1
          end if
          prev_month_start%day    = 1
          prev_month_start%hour   = 0
          prev_month_start%minute = 0
          prev_month_start%second = 0

          prev_month_sec = days_of_month(prev_month_start) * 86400
          latest_month_sec = days_of_month(file_data%latest_data) * 86400
          until_latest_data = libmxe_calendar__diffsec(present,file_data%latest_data,l_use_leap)
          file_data%br = 2.0d0 * real(until_latest_data,8) / real(prev_month_sec+latest_month_sec,8)
          file_data%ar = 1.0d0 - file_data%br

        end if

        write(6,*) trim(file_data%item),' br  ar ', file_data%br, file_data%ar

      else

        if (file_data%data_intv_sec > 0) then

          if (abs(from_first_data) > file_data%data_intv_sec) then

            write(6,*) ' Your date specification for ',trim(file_data%item),' is incorrect, please check '
            write(6,*) '   From first data : ', from_first_data
            write(6,*) '   Data interval   : ', file_data%data_intv_sec
            write(6,'(1a,i4.4,i2.2,i2.2,i2.2,i2.2,i2.2)') ' First data :',&
                 & file_data%first_data%year, &
                 & file_data%first_data%month, &
                 & file_data%first_data%day, &
                 & file_data%first_data%hour, &
                 & file_data%first_data%minute, &
                 & file_data%first_data%second
            write(6,'(1a,i4.4,i2.2,i2.2,i2.2,i2.2,i2.2)') '    Present :',&
                 & present%year, &
                 & present%month, &
                 & present%day, &
                 & present%hour, &
                 & present%minute, &
                 & present%second
            stop
            
          else
            
            ! from previous file
            
            l_proceed = .false.
            call create_file_date(file_data, l_proceed)
            if (l_ymdir) then
              write(base_dir_tmp,'(1a,1a,i4.4,i2.2)') &
                   & trim(base_dir),'/',file_data%file_name_date%year,file_data%file_name_date%month
            else
              write(base_dir_tmp,'(1a)') trim(base_dir)
            end if
            from_prev_first_data = libmxe_calendar__diffsec(file_data%first_data,present,l_use_leap)
            recnum = from_prev_first_data / file_data%data_intv_sec + 1
            call create_file_name(file_name,file_data,base_dir_tmp)
            write(6,*) ' File 1 (b) ', trim(file_name)
            write(6,*) '       read ', recnum
            
            lreclen = 4 * file_data%im * file_data%jm
            call open_file_direct(mttmp, file_name, lreclen)
            file_data%lunit = mttmp
            read(file_data%lunit,rec=recnum) work4
            file_data%dat_b(1:file_data%im,1:file_data%jm) = real(work4(1:file_data%im,1:file_data%jm),8)
            call close_file(file_data%lunit)
            where(file_data%dat_b(1:file_data%im,1:file_data%jm) == file_data%dundef) &
                 & file_data%dat_b(1:file_data%im,1:file_data%jm) = 0.0d0
            
            ! latest file
            
            recnum = 1
            l_proceed = .true.
            call create_file_date(file_data, l_proceed)
            if (l_ymdir) then
              write(base_dir_tmp,'(1a,1a,i4.4,i2.2)') &
                   & trim(base_dir),'/',file_data%file_name_date%year,file_data%file_name_date%month
            else
              write(base_dir_tmp,'(1a)') trim(base_dir)
            end if
            call create_file_name(file_name,file_data,base_dir_tmp)
            call calc_total_rec_file(file_data)
            
            write(6,*) ' File 2 (a) ', trim(file_name)
            write(6,*) '  total rec ', file_data%num_total_record
            write(6,*) '       read ', recnum
            
            lreclen = 4 * file_data%im * file_data%jm
            call open_file_direct(mttmp, file_name, lreclen)
            file_data%lunit = mttmp
            read(file_data%lunit,rec=recnum) work4
            file_data%dat_a(1:file_data%im,1:file_data%jm) = real(work4(1:file_data%im,1:file_data%jm),8)
            where(file_data%dat_a(1:file_data%im,1:file_data%jm) == file_data%dundef) &
                 & file_data%dat_a(1:file_data%im,1:file_data%jm) = 0.0d0
            
            file_data%latest_data = file_data%first_data
            file_data%latest_record = recnum
            file_data%br = real(abs(from_first_data),8) / real(file_data%data_intv_sec,8)
            file_data%ar = 1.0d0 - file_data%br
            write(6,*) trim(file_data%item),' br  ar ', file_data%br, file_data%ar
            
          end if

        else ! monthly

          if (file_data%first_data%month /= present%month) then

            write(6,*) ' Your date specification for ',trim(file_data%item),' is incorrect, please check '
            write(6,*) '   From first data : ', from_first_data
            write(6,'(1a,i4.4,i2.2,i2.2,i2.2,i2.2,i2.2)') ' First data :',&
                 & file_data%first_data%year, &
                 & file_data%first_data%month, &
                 & file_data%first_data%day, &
                 & file_data%first_data%hour, &
                 & file_data%first_data%minute, &
                 & file_data%first_data%second
            write(6,'(1a,i4.4,i2.2,i2.2,i2.2,i2.2,i2.2)') '    Present :',&
                 & present%year, &
                 & present%month, &
                 & present%day, &
                 & present%hour, &
                 & present%minute, &
                 & present%second
            stop
            
          else
            
            ! from previous file
            
            l_proceed = .false.
            call create_file_date(file_data, l_proceed)
            if (l_ymdir) then
              write(base_dir_tmp,'(1a,1a,i4.4,i2.2)') &
                   & trim(base_dir),'/',file_data%file_name_date%year,file_data%file_name_date%month
            else
              write(base_dir_tmp,'(1a)') trim(base_dir)
            end if

            recnum = (present%year - file_data%first_data%year) * 12 &
                 & + (present%month - file_data%first_data%month)

            call create_file_name(file_name,file_data,base_dir_tmp)
            write(6,*) ' File 1 (b) ', trim(file_name)
            write(6,*) '       read ', recnum
            
            lreclen = 4 * file_data%im * file_data%jm
            call open_file_direct(mttmp, file_name, lreclen)
            file_data%lunit = mttmp
            read(file_data%lunit,rec=recnum) work4
            file_data%dat_b(1:file_data%im,1:file_data%jm) = real(work4(1:file_data%im,1:file_data%jm),8)
            call close_file(file_data%lunit)
            where(file_data%dat_b(1:file_data%im,1:file_data%jm) == file_data%dundef) &
                 & file_data%dat_b(1:file_data%im,1:file_data%jm) = 0.0d0
            
            ! latest file
            
            recnum = 1
            l_proceed = .true.
            call create_file_date(file_data, l_proceed)
            if (l_ymdir) then
              write(base_dir_tmp,'(1a,1a,i4.4,i2.2)') &
                   & trim(base_dir),'/',file_data%file_name_date%year,file_data%file_name_date%month
            else
              write(base_dir_tmp,'(1a)') trim(base_dir)
            end if
            call create_file_name(file_name,file_data,base_dir_tmp)
            call calc_total_rec_file(file_data)
            
            write(6,*) ' File 2 (a) ', trim(file_name)
            write(6,*) '  total rec ', file_data%num_total_record
            write(6,*) '       read ', recnum
            
            lreclen = 4 * file_data%im * file_data%jm
            call open_file_direct(mttmp, file_name, lreclen)
            file_data%lunit = mttmp
            read(file_data%lunit,rec=recnum) work4
            file_data%dat_a(1:file_data%im,1:file_data%jm) = real(work4(1:file_data%im,1:file_data%jm),8)
            where(file_data%dat_a(1:file_data%im,1:file_data%jm) == file_data%dundef) &
                 & file_data%dat_a(1:file_data%im,1:file_data%jm) = 0.0d0
            
            file_data%latest_data = file_data%first_data
            present_month_start%year   = file_data%latest_data%year
            present_month_start%month  = file_data%latest_data%month
            present_month_start%day    = 1
            present_month_start%hour   = 0
            present_month_start%minute = 0
            present_month_start%second = 0
            this_month_sec = days_of_month(present_month_start) * 86400
            file_data%latest_data = libmxe_calendar__addsec(present_month_start, this_month_sec/2, l_use_leap)
            file_data%first_data = file_data%latest_data

            file_data%latest_record = recnum

            if (file_data%latest_data%month == 1) then
              prev_month_start%year   = file_data%latest_data%year - 1
              prev_month_start%month  = 12
            else
              prev_month_start%year   = file_data%latest_data%year
              prev_month_start%month  = file_data%latest_data%month + 1
            end if
            prev_month_start%day    = 1
            prev_month_start%hour   = 0
            prev_month_start%minute = 0
            prev_month_start%second = 0

            prev_month_sec = days_of_month(prev_month_start) * 86400
            latest_month_sec = days_of_month(file_data%latest_data) * 86400

            until_latest_data = libmxe_calendar__diffsec(present,file_data%latest_data,l_use_leap)
            if (until_latest_data /= abs(from_first_data)) then
              write(6,*) ' program error (until_latest_data /= abs(from_first_data) '
              write(6,*) until_latest_data, from_first_data
              !write(6,*) file_data%latest_data%year
              !write(6,*) file_data%latest_data%month
              !write(6,*) file_data%latest_data%day
              !write(6,*) file_data%latest_data%hour
              !write(6,*) file_data%latest_data%minute
              !write(6,*) file_data%latest_data%second
              stop
            end if

            file_data%br = 2.0d0 * real(until_latest_data,8) / real(prev_month_sec+latest_month_sec,8)
            file_data%ar = 1.0d0 - file_data%br

            write(6,*) trim(file_data%item),' br  ar ', file_data%br, file_data%ar
            
          end if

        end if
        
      end if

    end if

    deallocate(work4)

  end subroutine get_first_two_data
  !============================================================
  subroutine calc_total_rec_file(file_data)

    type(type_force),intent(inout) :: file_data
    type(type_calendar) :: next_first_data
    integer(4) :: time_coverage
    integer(4) :: days_year
    integer(4) :: days, hours
    integer(4) :: mm, dd
    logical :: l_leap

    if (l_use_leap) then
      l_leap = libmxe_calendar__l_leap_year( file_data%first_data%year )
    else
      l_leap = .false.
    end if

    next_first_data = file_data%first_data

    select case (file_data%file_intv_type)
    case (0)
      write(6,*) ' Sorry, all data in one file is not supported '
    case (1)
      next_first_data%year = file_data%first_data%year + file_data%file_intv
      time_coverage = libmxe_calendar__diffsec(file_data%first_data,next_first_data,l_use_leap)
    case (2)
      next_first_data%month = file_data%first_data%month + file_data%file_intv
      if (next_first_data%month == 13) then
        next_first_data%year = file_data%first_data%year + 1
        next_first_data%month = 1
      end if
      time_coverage = libmxe_calendar__diffsec(file_data%first_data,next_first_data,l_use_leap)
    case (3)
      time_coverage = file_data%file_intv * 86400
    case (4)
      time_coverage = file_data%file_intv * 3600
    case (5)
      time_coverage = file_data%file_intv * 60
    end select

    if (file_data%data_intv_sec > 0) then
      file_data%num_total_record = time_coverage / file_data%data_intv_sec
    else ! -1 : monthly
      file_data%num_total_record = (next_first_data%year - file_data%first_data%year) * 12 &
           & + (next_first_data%month - file_data%first_data%month)
    end if

  end subroutine calc_total_rec_file
  !============================================================
  subroutine create_file_date(file_data, l_proceed)

    type(type_force),intent(inout)  :: file_data
    logical,intent(in) :: l_proceed
    integer(4) :: file_interval, this_month_sec
    type(type_calendar) :: tmp_date

    !-----------------------------------------------------

    select case (file_data%file_intv_type)
    case (0)
      write(6,*) ' Sorry, all data in one file is not supported '
    case (1)
      if (l_proceed) then
        file_data%first_data%year = file_data%first_data%year + file_data%file_intv
        file_data%file_name_date%year = file_data%file_name_date%year + file_data%file_intv
      else
        file_data%first_data%year = file_data%first_data%year - file_data%file_intv
        file_data%file_name_date%year = file_data%file_name_date%year - file_data%file_intv
      end if
    case (2)
      if (l_proceed) then
        file_data%first_data%month = file_data%first_data%month + file_data%file_intv
        if (file_data%first_data%month >= 13) then
          file_data%first_data%year = file_data%first_data%year + 1
          file_data%first_data%month = file_data%first_data%month - 12
        end if
        file_data%file_name_date%month = file_data%file_name_date%month + file_data%file_intv
        if (file_data%file_name_date%month >= 13) then
          file_data%file_name_date%year = file_data%file_name_date%year + 1
          file_data%file_name_date%month = file_data%file_name_date%month - 12
        end if
        tmp_date%year   = file_data%first_data%year
        tmp_date%month  = file_data%first_data%month
        tmp_date%day    = 1
        tmp_date%hour   = 0
        tmp_date%minute = 0
        tmp_date%second = 0
        this_month_sec = days_of_month(tmp_date) * 86400
        file_data%first_data = libmxe_calendar__addsec(tmp_date, this_month_sec/2, l_use_leap)
      else
        file_data%first_data%month = file_data%first_data%month - file_data%file_intv
        if (file_data%first_data%month <= 0) then
          file_data%first_data%year = file_data%first_data%year - 1
          file_data%first_data%month = file_data%first_data%month + 12
        end if
        file_data%file_name_date%month = file_data%file_name_date%month - file_data%file_intv
        if (file_data%file_name_date%month <= 0) then
          file_data%file_name_date%year = file_data%file_name_date%year - 1
          file_data%file_name_date%month = file_data%file_name_date%month + 12
        end if
      end if
    case (3)
      if (l_proceed) then
        file_interval = file_data%file_intv * 86400
      else
        file_interval = - file_data%file_intv * 86400
      end if
      tmp_date = file_data%first_data
      file_data%first_data = libmxe_calendar__addsec( tmp_date, file_interval, l_use_leap )
      tmp_date = file_data%file_name_date
      file_data%file_name_date = libmxe_calendar__addsec( tmp_date, file_interval, l_use_leap )
    case (4)
      if (l_proceed) then
        file_interval = file_data%file_intv * 3600
        file_data%file_name_date = file_data%file_name_date
      else
        file_interval = - file_data%file_intv * 3600
      end if
      tmp_date = file_data%first_data
      file_data%first_data = libmxe_calendar__addsec( tmp_date, file_interval, l_use_leap )
      tmp_date = file_data%file_name_date
      file_data%file_name_date = libmxe_calendar__addsec( tmp_date, file_interval, l_use_leap )
    case (5)
      if (l_proceed) then
        file_interval = file_data%file_intv * 60
        file_data%file_name_date = file_data%file_name_date
      else
        file_interval = - file_data%file_intv * 60
      end if
      tmp_date = file_data%first_data
      file_data%first_data = libmxe_calendar__addsec( tmp_date, file_interval, l_use_leap )
      tmp_date = file_data%file_name_date
      file_data%file_name_date = libmxe_calendar__addsec( tmp_date, file_interval, l_use_leap )
    end select

  end subroutine create_file_date
  !============================================================
  subroutine create_file_name(filename,file_data,bsdir)

    type(type_force),intent(inout)  :: file_data
    character(len=*),intent(out)   :: filename
    character(len=*),intent(in)    :: bsdir
    type(type_calendar) :: tmp_date

    !-----------------------------------------------------

    tmp_date = file_data%file_name_date

    select case (file_data%file_intv_type)
    case (0)
      write(6,*) ' Sorry, all data in one file is not supported '
    case (1)
      write(filename,'(1a,1a,1a,1a,i4.4)') &
           & trim(bsdir),'/',trim(file_data%file_name_base),'.',tmp_date%year
    case (2)
      write(filename,'(1a,1a,1a,1a,i4.4,i2.2)') &
           & trim(bsdir),'/',trim(file_data%file_name_base),'.',tmp_date%year, tmp_date%month
    case (3)
      write(filename,'(1a,1a,1a,1a,i4.4,i2.2,i2.2)') &
           & trim(bsdir),'/',trim(file_data%file_name_base),'.',tmp_date%year, tmp_date%month, tmp_date%day
    case (4)
      write(filename,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') &
           & trim(bsdir),'/',trim(file_data%file_name_base),'.',tmp_date%year, tmp_date%month, tmp_date%day, tmp_date%hour
    case (5)
      write(filename,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
           & trim(bsdir),'/',trim(file_data%file_name_base),'.',tmp_date%year, tmp_date%month, tmp_date%day, tmp_date%hour, tmp_date%minute
    end select

  end subroutine create_file_name
  !============================================================
  function day_of_year(today,l_leap)

    integer(4) :: day_of_year
    type(type_calendar),intent(in) :: today
    logical,intent(in) :: l_leap

    integer(4) :: mon, day
    integer(4) :: m, itmp

    !---------------------------------------------------------

    itmp = 0

    mon = today%month
    day = today%day

    do m = 1, mon - 1
      if (m == 2) then
        if (l_leap) then
          itmp = itmp + idmon(m) + 1
        else
          itmp = itmp + idmon(m)
        end if
      else
        itmp = itmp + idmon(m)
      end if
    end do

    itmp = itmp + day

    day_of_year = itmp

  end function day_of_year
  !============================================================
  !============================================================
  function days_of_month(today)

    integer(4) :: days_of_month
    type(type_calendar),intent(in) :: today
    logical :: l_leap

    integer(4) :: mon
    integer(4) :: itmp

    !---------------------------------------------------------

    if (l_use_leap) then
      l_leap = libmxe_calendar__l_leap_year( today%year )
    else
      l_leap = .false.
    end if

    mon = today%month

    if (mon == 2) then
      if (l_leap) then
        itmp = idmon(mon) + 1
      else
        itmp = idmon(mon)
      end if
    else
      itmp = idmon(mon)
    end if

    days_of_month = itmp

  end function days_of_month
  !============================================================
  !============================================================
  subroutine date_of_year(day,l_leap,mm,dd)

    integer(4),intent(in) :: day
    logical,intent(in) :: l_leap
    integer(4),intent(out) :: mm, dd

    integer(4) :: m, itmp, mday

    !---------------------------------------------------------

    itmp = day

    do m = 1, num_month
      if ((m == 2) .and. (l_leap))  then
        mday = idmon(m) + 1
      else
        mday = idmon(m)
      end if
      itmp = itmp - mday
      if (itmp <= 0) then
        dd = itmp + mday
        mm = m
        exit
      end if
    end do

  end subroutine date_of_year
  !============================================================
end module force_process
