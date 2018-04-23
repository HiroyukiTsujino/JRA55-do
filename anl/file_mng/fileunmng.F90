! -*-F90-*-
!---------------------------------------------------------------
! Information:
!   File Unit Manager
!---------------------------------------------------------------
module file_unit_manager
  !
  implicit none
  !
  private
  !
  public allocate_unit
  public release_unit
  !
  integer(4), parameter :: MIN_UNIT = 20
  integer(4), parameter :: MAX_UNIT = 99
  !
  type :: funitmng
    logical,pointer :: l_open(:)
  end type funitmng
  !
  type(funitmng) :: g_unit_manager
  logical :: g_unit_manager_initialized = .false.
  !
contains
  !====================================================
  subroutine init_unit_manager

    integer(4) :: i

    if (g_unit_manager_initialized) return

    allocate(g_unit_manager%l_open(MIN_UNIT:MAX_UNIT))

    do i = MIN_UNIT, MAX_UNIT
      g_unit_manager%l_open(i) = .false.
    enddo

    g_unit_manager_initialized = .true.

  end subroutine init_unit_manager
  !====================================================
  subroutine allocate_unit(ounit)
    !
    integer(4), intent(out) :: ounit
    integer(4) :: i
    !
    call init_unit_manager
    !
    do i = MIN_UNIT, MAX_UNIT
      if (.not. g_unit_manager%l_open(i)) then
        ounit = i
        g_unit_manager%l_open(i) = .true.
        exit
      end if
      if (i == MAX_UNIT) then
        write(6,*) ' Units not available '
        stop
      end if
    enddo

  end subroutine allocate_unit
  !====================================================
  subroutine release_unit(iunit)
    !
    integer(4), intent(in) :: iunit
    integer(4) :: i
    !
    call init_unit_manager
    !
    if (g_unit_manager%l_open(iunit)) then
      g_unit_manager%l_open(iunit) = .false.
    else
      write(6,*) ' Unit number ', iunit ,' has not been opened'
      stop
    end if

  end subroutine release_unit
  !
end module file_unit_manager
