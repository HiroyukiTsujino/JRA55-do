! -*-F90-*-
!- Set margin region following PARA.
module libmxe_stmrgn
  use libmxe_para, only: type_libmxe_para, itspnt, iuvpnt
  implicit none
  private


  !-- subroutine --
  public :: libmxe_stmrgn__var2_x, libmxe_stmrgn__var3_x
    !- apply cyclic boundary condition (var2: 2D array, var3: 3D)

  public :: libmxe_stmrgn__var2_n, libmxe_stmrgn__var3_n
    !- apply folding condition along northern boundary


contains
!-----------------------------------------------------------------


subroutine libmxe_stmrgn__var3_x(buf,kdim,iuvts,para)
  implicit none

  type(type_libmxe_para), intent(in) :: para
  integer(4), intent(in) :: kdim
  integer(4), intent(in) :: iuvts  !- T/U grid
  real(8), intent(inout) :: buf(para%imut,para%jmut,kdim)

  integer(4) :: i, j, k

 
  !---- check ----
  if ( .not. para%ldef )  then
    write(*,*) 'Error at libmxe_stmrgn__var3_x'
    write(*,*) '  para is not registered.'
    stop
  endif
  if ( .not. para%lcyclic )  then
    write(*,*) 'Warning at libmxe_stmrgn__var3_x'
    write(*,*) '  Zonal B.C. is not cyclic.'
    return
  endif


  if ( iuvts == itspnt ) then
    do k = 1, kdim
      do j = 1, para%jmut
        buf(para%ibt-2,j,k) = buf(para%iet-1,j,k)
        buf(para%iet+2,j,k) = buf(para%ibt+1,j,k)
        buf(para%ibt-1,j,k) = buf(para%iet  ,j,k)
        buf(para%iet+1,j,k) = buf(para%ibt  ,j,k)
      end do
    end do
  end if
  if ( iuvts == iuvpnt ) then
    do k = 1, kdim
      do j = 1, para%jmut
        buf(para%ibu-2,j,k) = buf(para%ieu-1,j,k)
        buf(para%ieu+2,j,k) = buf(para%ibu+1,j,k)
        buf(para%ibu-1,j,k) = buf(para%ieu,  j,k)
        buf(para%ieu+1,j,k) = buf(para%ibu,  j,k)
      end do
    end do
  end if


end subroutine libmxe_stmrgn__var3_x
!-----------------------------------------------------------------


subroutine libmxe_stmrgn__var2_x(buf,iuvts,para)
  implicit none

  type(type_libmxe_para), intent(in) :: para
  integer(4), intent(in) :: iuvts
  real(8), intent(inout) :: buf(para%imut,para%jmut)

  integer(4) :: i, j


  !---- check ----
  if ( .not. para%ldef )  then
    write(*,*) 'Error at libmxe_stmrgn__var2_x'
    write(*,*) '  para is not registered.'
    stop
  endif
  if ( .not. para%lcyclic )  then
    write(*,*) 'Warning at libmxe_stmrgn__var2_x'
    write(*,*) '  Zonal B.C. is not cyclic.'
    return
  endif


  if ( iuvts == itspnt ) then
    do j = 1, para%jmut
      buf(para%ibt-2,j) = buf(para%iet-1,j)
      buf(para%iet+2,j) = buf(para%ibt+1,j)
      buf(para%ibt-1,j) = buf(para%iet  ,j)
      buf(para%iet+1,j) = buf(para%ibt  ,j)
    end do
  end if

  if ( iuvts == iuvpnt ) then
    do j = 1, para%jmut
      buf(para%ibu-2,j) = buf(para%ieu-1,j)
      buf(para%ieu+2,j) = buf(para%ibu+1,j)
      buf(para%ibu-1,j) = buf(para%ieu,  j)
      buf(para%ieu+1,j) = buf(para%ibu,  j)
    end do
  end if


end subroutine libmxe_stmrgn__var2_x
!-----------------------------------------------------------------


subroutine libmxe_stmrgn__var3_n(buf, kdim, iuvts, ireverse, para)
  implicit none

  type(type_libmxe_para), intent(in) :: para
  integer(4), intent(in) :: kdim
  real(8), intent(inout) :: buf(para%imut,para%jmut,kdim)
  integer(4), intent(in) :: iuvts      !- U/T grid
  integer(4), intent(in) :: ireverse
     !- -1: use sign-inversion values (for velocity), 1: not


  integer(4) :: i, j, k
  integer(4) :: ibt, iet, jbt, jet ! core-region (T-points)
  integer(4) :: ibu, ieu, jbu, jeu ! core-region (U-points)
  integer(4) :: nmc                ! used for tripolar/jot

  
  !---- check ----
  if ( .not. para%ldef )  then
    write(*,*) 'Error at libmxe_stmrgn__var3_n'
    write(*,*) '  para is not registered.'
    stop
  endif
  if ( .not. para%lfoldnp )  then
    write(*,*) 'Warning at libmxe_stmrgn__var3_n'
    write(*,*) '  lfoldnp = .false. '
    return
  endif


  nmc = para%nmc
  ibt = para%ibt
  ibu = para%ibu
  iet = para%iet
  ieu = para%ieu
  jbt = para%jbt
  jbu = para%jbu
  jet = para%jet
  jeu = para%jeu


  if (ireverse == 1) then
    if (iuvts == itspnt) then
      do k = 1, kdim
        do i = 1, nmc
          buf(ibt+i-1,jet+1,k) = buf(iet-i+2,jet-1,k)
          buf(ibt+i-1,jet+2,k) = buf(iet-i+2,jet-2,k)
        end do
        do i = nmc/2 + 2, nmc
          buf(ibt+i-1,jet,k) = buf(iet-i+2,jet,k)
        end do
      end do
    end if
    if (iuvts == iuvpnt) then
      do k = 1, kdim
        do i = 1, nmc
          buf(ibu+i-1,jeu  ,k) = buf(ieu-i+1,jeu-1,k) ! 念のため
          buf(ibu+i-1,jeu+1,k) = buf(ieu-i+1,jeu-2,k)
          buf(ibu+i-1,jeu+2,k) = buf(ieu-i+1,jeu-3,k)
        end do
      end do
    end if
    
  else if (ireverse == -1) then
      
    if (iuvts == itspnt) then
      do k = 1, kdim
        do i = 1, nmc
          buf(ibt+i-1,jet+1,k) = - buf(iet-i+2,jet-1,k)
          buf(ibt+i-1,jet+2,k) = - buf(iet-i+2,jet-2,k)
        end do
      end do
      do k = 1, kdim
        do i = nmc/2 +2, nmc
          buf(ibt+i-1,jet,k) = - buf(iet-i+2,jet,k)
        end do
      end do
    end if
    if (iuvts == iuvpnt) then
      do k = 1, kdim
        do i = 1, nmc
          buf(ibu+i-1,jeu  ,k) = - buf(ieu-i+1,jeu-1,k) ! 念のため
          buf(ibu+i-1,jeu+1,k) = - buf(ieu-i+1,jeu-2,k)
          buf(ibu+i-1,jeu+2,k) = - buf(ieu-i+1,jeu-3,k)
        end do
      end do
    end if
  end if


end subroutine libmxe_stmrgn__var3_n
!-----------------------------------------------------------------


subroutine libmxe_stmrgn__var2_n(buf, iuvts, ireverse, para)

  implicit none

  type(type_libmxe_para), intent(in) :: para
  real(8), intent(inout) :: buf(para%imut,para%jmut)
  integer(4), intent(in) :: iuvts
  integer(4), intent(in) :: ireverse

  integer(4) :: i, j, k

  integer(4) :: ibt, iet, jbt, jet ! core-region (T-points)
  integer(4) :: ibu, ieu, jbu, jeu ! core-region (U-points)
  integer(4) :: nmc                ! used for tripolar/jot

  
  !---- check ----
  if ( .not. para%ldef )  then
    write(*,*) 'Error at libmxe_stmrgn__var2_n'
    write(*,*) '  para is not registered.'
    stop
  endif
  if ( .not. para%lfoldnp )  then
    write(*,*) 'Warning at libmxe_stmrgn__var2_n'
    write(*,*) '  lfoldnp = .false. '
    return
  endif


  nmc = para%nmc
  ibt = para%ibt
  ibu = para%ibu
  iet = para%iet
  ieu = para%ieu
  jbt = para%jbt
  jbu = para%jbu
  jet = para%jet
  jeu = para%jeu


  if (ireverse == 1) then
    if (iuvts == itspnt) then
      do i = 1, nmc
        buf(ibt+i-1,jet+1) = buf(iet-i+2,jet-1)
        buf(ibt+i-1,jet+2) = buf(iet-i+2,jet-2)
      end do
      do i = nmc/2 + 2, nmc
        buf(ibt+i-1,jet) = buf(iet-i+2,jet)
      end do
    end if
    if (iuvts == iuvpnt) then
      do i = 1, nmc
        buf(ibu+i-1,jeu  ) = buf(ieu-i+1,jeu-1) ! 念のため
        buf(ibu+i-1,jeu+1) = buf(ieu-i+1,jeu-2)
        buf(ibu+i-1,jeu+2) = buf(ieu-i+1,jeu-3)
      end do
    end if
    
  else if (ireverse == -1) then
      
    if (iuvts == itspnt) then
      do i = 1, nmc
        buf(ibt+i-1,jet+1) = - buf(iet-i+2,jet-1)
        buf(ibt+i-1,jet+2) = - buf(iet-i+2,jet-2)
      end do
      do i = nmc/2 +2, nmc
        buf(ibt+i-1,jet) = - buf(iet-i+2,jet)
      end do
    end if
    if (iuvts == iuvpnt) then
      do i = 1, nmc
        buf(ibu+i-1,jeu  ) = - buf(ieu-i+1,jeu-1) ! 念のため
        buf(ibu+i-1,jeu+1) = - buf(ieu-i+1,jeu-2)
        buf(ibu+i-1,jeu+2) = - buf(ieu-i+1,jeu-3)
      end do
    end if
  end if


end subroutine libmxe_stmrgn__var2_n


end module libmxe_stmrgn
