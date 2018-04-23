! -*-F90-*-
!---------------------------------------------------------------
! Information:
!   File Open-Close Manager
!---------------------------------------------------------------
module file_open_close_manager
  !
  use file_unit_manager
  !
  implicit none
  !
contains
!===============================================================
  subroutine open_file_direct(file_unit,file_name,reclen,l_succeed,convert_mode,action)

    character(len=*), intent(in) :: file_name
    integer(4), intent(in)       :: reclen
    integer(4), intent(inout)    :: file_unit
    logical,intent(out),optional :: l_succeed
    character(len=*), intent(in),optional :: convert_mode
    character(len=*), intent(in),optional :: action

    character(len=128) :: conv_mode
    integer(4) :: ios
    !----------------------------------------------------------

    call allocate_unit(file_unit)

!    conv_mode = 'native'
    if (present(convert_mode)) then
      conv_mode = trim(convert_mode)
      if (present(action)) then
        open(file_unit, file=file_name, form='unformatted', &
             & access = 'direct', convert = convert_mode, recl = reclen, action=action, iostat=ios )
      else
        open(file_unit, file=file_name, form='unformatted', &
             & access = 'direct', convert = convert_mode, recl = reclen, iostat=ios )
      end if
    else
      if (present(action)) then
        open(file_unit, file=file_name, form='unformatted', &
             & access = 'direct', recl = reclen, action=action, iostat=ios )
      else
        open(file_unit, file=file_name, form='unformatted', &
             & access = 'direct', recl = reclen, iostat=ios )
      end if
    end if
    if (ios == 0) then
      write(6,*) 'Direct access file ', trim(file_name), &
           &     '   with unit number ', file_unit, ' opened'
      if (present(l_succeed)) then
        l_succeed = .true.
      end if
    else
      write(6,*) 'Opening ', trim(file_name), ' with unit number ', file_unit, ' was not successful'
      call release_unit(file_unit)
      if (present(l_succeed)) then
        l_succeed = .false.
      end if
    end if

  end subroutine open_file_direct
!===============================================================
  subroutine open_file_sequential(file_unit,file_name,convert_mode,action)

    character(len=*), intent(in) :: file_name
    integer(4), intent(inout)    :: file_unit
    character(len=*), intent(in),optional :: convert_mode
    character(len=*), intent(in),optional :: action

    character(len=128) :: conv_mode

    !----------------------------------------------------------

!    conv_mode = 'native'

    call allocate_unit(file_unit)

    if (present(convert_mode)) then
      conv_mode = trim(convert_mode)
      if (present(action)) then
        open(file_unit, file=file_name, form='unformatted', convert=convert_mode, action=action)
      else
        open(file_unit, file=file_name, form='unformatted', convert=convert_mode)
      end if
    else
      if (present(action)) then
        open(file_unit, file=file_name, form='unformatted', action=action)
      else
        open(file_unit, file=file_name, form='unformatted')
      end if
    end if

    write(6,*) 'Sequential access file ', trim(file_name), &
         &     '   with unit number ', file_unit, ' opened'

  end subroutine open_file_sequential
!===============================================================
  subroutine open_file_plain(file_unit,file_name,action)
    !
    character(len=*), intent(in) :: file_name
    integer(4), intent(inout)    :: file_unit
    character(len=*), intent(in),optional :: action
    !----------------------------------------------------------

    call allocate_unit(file_unit)

    if (present(action)) then
      open(file_unit, file=file_name, action=action)
    else
      open(file_unit, file=file_name)
    end if

    write(6,*) 'plain file ', trim(file_name), &
         &     '   with unit number ', file_unit, ' opened'

  end subroutine open_file_plain
!===============================================================
  subroutine close_file(file_unit)

    integer(4), intent(in)      :: file_unit

    !----------------------------------------------------------

    call release_unit(file_unit)

    close(file_unit)

    write(6,*) 'Unit number ', file_unit, ' closed'

  end subroutine close_file
  !
end module file_open_close_manager
