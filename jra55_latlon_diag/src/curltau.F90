! -*-F90-*-
!
!===================================================================!
!                                                                   !
!                      subroutine curltau                           !
! Information:                                                      !
!     calculate curl of wind stress on T-points of the model.       !
!                                                                   !
!-------------------------------------------------------------------!
subroutine curltau(wsx,wsy,curlt)
  !
  use basin_param
  use grid_common
  !
  implicit none
  !
  real(4), intent(in)    :: wsx(imf,jmf), wsy(imf,jmf)
  real(4), intent(inout) :: curlt(imf,jmf)
  real(4) :: areatmp
  !
  integer(4) :: i, j
  !
  !-----------------------------------------------------------------
  !
  curlt(1:imf,1:jmf) = 0.0
  !
  do j = 2, jmf
    do i = 2, imf
      areatmp = 2.0 * (anhf(i,j) + ashf(i,j))
      curlt(i,j) = 0.5 * atexl(i,j) * (&
           &      (wsy(i,j) + wsy(i,j-1)) * dyt(j) &
           &    - (wsx(i,j) + wsx(i-1,j)) * csu(j) * dxt(i) &
           &    - (wsy(i-1,j) + wsy(i-1,j-1)) * dyt(j) &
           &    + (wsx(i,j-1) + wsx(i-1,j-1)) * csu(j-1) * dxt(i)) &
           &    / areatmp
    enddo
  enddo
  !
end subroutine curltau
