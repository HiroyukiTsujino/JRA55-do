! -*-F90-*-
!
!======================================================================
!
!     basin_param: 定数の設定
!
!     Basic Parameters of the Model
!
!     If the region to be analyzed is changed,
!       never forget to change parameter "slat".
!
!----------------------------------------------------------------------
!
module basin_param

  implicit none

  integer(4), parameter :: imut = 364, jmut = 368, km = 51
  integer(4), parameter :: ksgm = 5

  real(8), parameter :: PI =       3.141592653589793D0  ! 円周率
  real(8), parameter :: RADIAN =   180.D0/PI
  real(8), parameter :: radian_r = 1.d0/radian
  real(8), parameter :: OMEGA =    PI/43082.D0   ! 地球の回転角速度
  real(8), parameter :: RADIUS =   6375.D3       ! 地球の半径
  real(8), parameter :: RADIUR =   1.D0/RADIUS   ! 地球の半径の逆数
  real(8), parameter :: GRAV =     9.81D0        ! 重力加速度

  real(8), parameter :: ro   =     1.036d3       ! 海水の密度
  real(8), parameter :: cp   =     3.99d3       ! 海水の密度

  real(8), parameter :: SLAT0 = -78.D0  !  海域の南端緯度
  real(8), parameter :: SLON0 = 0.D0    !  海域の東端経度

  real(8), parameter :: NPLAT = 64.D0   ! モデルの北極点の地理座標における緯度
  real(8), parameter :: NPLON = 80.D0   ! モデルの北極点の地理座標における経度
  real(8), parameter :: SPLAT = 64.D0   ! モデルの北極点の地理座標における緯度
  real(8), parameter :: SPLON = 260.D0  ! モデルの北極点の地理座標における経度

  include 'dz.F90'

end module basin_param
