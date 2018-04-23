! -*-F90-*-
module density_test
  use density
  use libmxe_ut
  use fruit, only: assert_equals
  implicit none
  private

  character(*),parameter,public :: testname='density'

  public :: test_dens


contains


subroutine test_dens
  implicit none

  integer,parameter :: im = 1, jm = 1, km = 1

  real(8) :: potential_t(im,jm,km)
  real(8) :: sal_psu(im,jm,km)
  real(8) :: pressure_bar(km)
  real(8) :: rho_local_gpcm3(im,jm,km)

  potential_t(1,1,1) = 2.d0
  sal_psu(1,1,1) = 35.d0
  pressure_bar(1) = 100.d0  !- 1000m depth

  call dens( im, jm, km, potential_t, sal_psu, &
           & pressure_bar, rho_local_gpcm3 )

  call assert_equals( 3.262243417655143d-2, &
                 & rho_local_gpcm3(1,1,1), 1.d-12, 'dens' )

!see http://www.csgnetwork.com/water_density_calculator.html

end subroutine test_dens


end module density_test
