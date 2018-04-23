! -*-F90-*-
!
module basin_param

  ! model grid number

  integer(4), parameter :: imut = 364, jmut = 368, km = 51
  integer(4), parameter :: ksgm = 5
  integer(4), parameter :: kbbl = 1

  ! start point of model core region

  real(8), parameter :: slon0 = 0.0d0
  real(8), parameter :: slat0 = -78.0d0

#ifndef OGCM_VARIABLE
  real(8), parameter :: dxtdgc = 1.0d0
  real(8), parameter :: dytdgc = 0.5d0
#endif /* OGCM_VARIABLE */

#include "dz.F90"

  real(8), parameter :: ro = 1.036d0        ! 水の密度 [cgs]
  real(8), parameter :: ro0 = ro * 1.0d3    ! 海水の参照密度 [MKS]
  real(8), parameter :: rice = 900.d0       ! 海氷の標準密度 [MKS]
  real(8), parameter :: rdsw = 0.33d0       ! 雪の密度/水の密度
  real(8), parameter :: rsnow = rdsw * ro0  ! 雪の密度 [MKS]

end module basin_param
