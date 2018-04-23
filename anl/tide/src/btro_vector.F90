! -*-F90-*-
module btro_vector
  use libmxe_para, only: type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  implicit none
  private


  public :: btro_vector__divergence


contains 


!-----------------------------------------------------------------
!- (um,vm) (U-point) => nabla dot (um,vm) (U-point)
subroutine btro_vector__divergence( um, vm, &
                       & para,grid,topo,div)
  implicit none

  type(type_libmxe_para),intent(in) :: para
  type(type_libmxe_grid),intent(in) :: grid
  type(type_libmxe_topo),intent(in) :: topo
  real(8),intent(in)  :: um(para%imut,para%jmut)
  real(8),intent(in)  :: vm(para%imut,para%jmut)
  real(8),intent(out) :: div(para%imut,para%jmut) !- output

  integer :: i, j, im, jm
  real(8) :: hu1, hu2, hv1, hv2, d

  im = para%imut
  jm = para%jmut
  div(:,:) = dble( para%rundefout )

  do i = 2, im - 1
    do j = 2, jm - 1

      hu1 = 0.5d0*( um(i-1,j) + um(i,j) ) * topo%aexl(i-1,j,1) &
           & * ( grid%dy_tl(i,j) + grid%dy_bl(i,j) )
      hu2 = 0.5d0*( um(i,j) + um(i+1,j) ) * topo%aexl(i+1,j,1) &
           & * ( grid%dy_tl(i+1,j) + grid%dy_bl(i+1,j) )
      hv1 = 0.5d0*( vm(i,j-1) + vm(i,j) ) * topo%aexl(i,j-1,1) &
           & * ( grid%dx_bl(i,j) + grid%dx_br(i,j) )
      hv2 = 0.5d0*( vm(i,j) + vm(i,j+1) ) * topo%aexl(i,j+1,1) &
           & * ( grid%dx_bl(i,j+1) + grid%dx_br(i,j+1) )

      d = ( hu2 - hu1 + hv2 - hv1 ) &
         & / ( grid%areau(i,j) + ( 1.d0 - topo%aexl(i,j,1) ) )

      div(i,j) = d * topo%aexl(i,j,1) &
                & + dble( para%rundefout ) * ( 1.d0 - topo%aexl(i,j,1) )

    enddo
  enddo

end subroutine btro_vector__divergence


end module btro_vector
