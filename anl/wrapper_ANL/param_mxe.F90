! -*-F90-*-
! param_mxe.F90
!======================================================================
!
!     oc_mod_param: 定数の設定
!
module oc_mod_param   !  cgs単位系

  use libmxe_para, only: type_libmxe_para
  use libmxe_para, only: radius, rho

  implicit none

  integer(4),save :: IMUT, JMUT, KM
  integer(4),save :: KSGM
  integer(4),save :: KBBL

  integer(4),save :: IJMTT
  integer(4),save :: KMM1
  integer(4),save :: KMP1

  real(8),save :: SLAT0, SLON0
  real(8),save :: NPLAT, NPLON
  real(8),save :: SPLAT, SPLON

  ! 基本的なパラメータ

  real(8),parameter :: RADIUR =   1.D0/RADIUS   ! 地球の半径の逆数
  real(8),parameter :: unit_bar = 1.d-6         ! dyn/cm^2 -> bar への変換係数

  real(8),parameter :: TAB = 273.16D0  !  0℃の絶対温度
  real(8),parameter :: RO = rho        !  水の密度
  real(8),parameter :: CP = 3.99D7     !  水の比熱
  real(8),parameter :: RROCP = 1.D0 / (RO * CP)

  real(8),parameter :: abtm = 1.225D-3 ! 海底摩擦の係数

  real(8),parameter :: stfblz = 5.67d-5 ! Stefan-Boltzman Constant (cgs)
  real(8),parameter :: cpa_mks = 1004.67d0 ! 大気比熱 (J/Kg/K)
  real(8),parameter :: ems = 1.00d0     ! emissivity of sea water
  real(8),parameter :: rhoa_mks = 1.205d0 ! 大気密度 (kg/m^3)

  real(8),parameter :: RHO0 = rho

  integer(4),parameter :: NSMIN = 60    ! 1分の秒数
  integer(4),parameter :: NSHOUR = NSMIN * 60  ! 1時間の秒数
  integer(4),parameter :: NSDAY = 24 * NSHOUR  ! 1日の秒数
  integer(4),parameter :: IS30 = NSDAY * 30    ! 1か月(30日)の秒数
  integer(4),parameter :: IMN = 12             ! 1年の月数
  integer(4),parameter :: NDYEAR = 365
  integer(4),parameter :: NSYEAR = NDYEAR * NSDAY

  real(8), allocatable :: dz(:)

  type(type_libmxe_para),save :: para

contains

  subroutine param_mxe__ini

    use libmxe_para, only: libmxe_para__register

    write(6,*) ' Registering PARAM '

    call libmxe_para__register(para)

    write(6,*) ' ..... done '

    imut = para%imut
    jmut = para%jmut
    km   = para%km
    KSGM = para%ksgm
    KBBL = para%kbbl

    IJMTT = IMUT * JMUT ! 各層の格子点総数
    KMM1 = KM - 1
    KMP1 = KM + 1

    allocate(dz(1:km))
    dz(1:km) = para%dz(1:km)

    write(6,*) ' Exiting param_mxe__ini '

  end subroutine param_mxe__ini

end module oc_mod_param
