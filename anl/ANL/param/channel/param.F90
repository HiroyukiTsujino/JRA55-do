! -*-F90-*-
!
!J     MRI.COM(気象研究所統合海洋モデル) param.F90.in
! MRI.COM (Meteorological Research Institute Community Ocean Model)
!         Copyright 2002 Ocenographic Research Dept., MRI-JMA
!
!======================================================================
!
!J     oc_mod_param: 定数の設定
!
module oc_mod_param   !J  cgs単位系
  !
  implicit none
  !
  integer(4),parameter :: max_char_len = 16
  character(len=max_char_len),save :: name_model = 'OGCM'
  !
  !J     定数の定義
  !
  !Jコンパイル前にモデルの設定に合わせて以下のパラメータの値をセットする必要がある
  !
  !J         IMUT, JMUT, KM
  !J         PARALLEL の場合; NPARTA
  !
  !
  integer(4), parameter:: IMUT = 304   !J  格子点数
  integer(4), parameter:: JMUT = 102
  integer(4), parameter:: KM = 29
  integer(4), parameter:: KSGM = 1
#ifdef OGCM_BBL
  integer(4), parameter:: KBBL = 
#endif /* ! OGCM_BBL */
  !
  real(8), parameter:: SLAT0 = -65.D0  !J  海域の南端緯度
  real(8), parameter:: SLON0 = 0.D0  !J  海域の東端経度
  !
  real(8), parameter :: NPLAT = 90.D0 !J モデルの北極点の地理座標における緯度
  real(8), parameter :: NPLON = 0.D0 !J モデルの北極点の地理座標における経度
#ifdef OGCM_SPHERICAL
  real(8), parameter :: SPLAT = -NPLAT !J モデルの南極点の地理座標における緯度
  real(8), parameter :: SPLON = NPLON+180.d0 !J モデルの南極点の地理座標における経度
#ifdef OGCM_PLANE
  real(8), parameter :: base_lat = -60.d0
#endif /* OGCM_PLANE */
#else /* OGCM_SPHERICAL */
  real(8), parameter :: SPLAT = -90.d0 !J モデルの南極点の地理座標における緯度
  real(8), parameter :: SPLON = 0.d0 !J モデルの南極点の地理座標における経度
#endif /* OGCM_SPHERICAL */
  !
  !
  !J 基本的なパラメータ
  !
  real(8), parameter :: PI =       3.141592653589793D0  !J 円周率
  real(8), parameter :: RADIAN =   180.D0/PI
  real(8), parameter :: radian_r = 1.d0/radian
  real(8), parameter :: OMEGA =    PI/43082.D0   !J 地球の回転角速度
  real(8), parameter :: RADIUS =   6375.D5       !J 地球の半径
  real(8), parameter :: RADIUR =   1.D0/RADIUS   !J 地球の半径の逆数
  real(8), parameter :: GRAV =     981.D0        !J 重力加速度
  real(8), parameter :: unit_bar = 1.d-6     !J dyn/cm^2 -> bar への変換係数
  real(8), parameter :: C24 =      1.D0/24.D0    !J 荒川スキームで使用
  !
  real(8), parameter :: TAB = 273.16D0  !J  0℃の絶対温度
  real(8), parameter :: RO = 1.036D0       !J  水の密度
  !J!!!!real(8), parameter :: RO = 1.D0       !  水の密度
  real(8), parameter :: CP = 3.99D7     !J  水の比熱
  real(8), parameter :: RROCP = 1.D0 / (RO * CP)
  !
!!real(8), parameter :: abtm = 1.225D-3 !J 海底摩擦の係数
  real(8), parameter :: abtm = 0.0d0    !J 海底摩擦の係数
  !
  real(8), parameter :: stfblz = 5.67d-5 ! Stefan-Boltzman Constant (cgs)
  real(8), parameter :: cpa_mks = 1004.67d0 !J 大気比熱 (J/Kg/K)
#ifdef OGCM_TESTING
  real(8), parameter :: rhoa_mks = 1.27d0 !J 大気密度 (kg/m^3)
#else /* OGCM_TESTING */
  real(8), parameter :: rhoa_mks = 1.205d0 !J 大気密度 (kg/m^3)
#endif /* OGCM_TESTING */
  !
!!!!!  real(8),    parameter:: RTMSC = $RTMSC   !J 海面での水温塩分への緩和日数
!J!!!!  real(8),    parameter:: RTMSCB = $RTMSCB ! body forcing 領域での水温塩分への緩和日数
  !
!!!!!#ifdef OGCM_SMAGOR
!!!!!  !
!!!!!  !J cscl : Smagorinsky 粘性係数決定用のパラメータ
!!!!!  !J        Laplacian では 3.0 - 4.0
!!!!!  !J        Bihamonic では 2.0 - 3.0
!!!!!  !J        が良いらしい（Griffies and Hallberg, 2000）が、
!!!!!  !J        モデルの解像度等に合わせてチューニングをする必要がある。
!!!!!  !
!!!!!  real(8), parameter :: cscl = $CSCL
!!!!!  !
!!!!!#ifdef OGCM_SMAGHD
!!!!!  real(8), parameter :: smaghd_ratio = $SMAGHD_RATIO
!!!!!#endif /* OGCM_SMAGHD */
!!!!!#endif /* OGCM_SMAGOR */
  !
#ifndef OGCM_VARIABLE
  real(8),    parameter:: DXTDGC = 0.2D0
  real(8),    parameter:: DYTDGC = 0.1D0
  real(8),    parameter:: dmu = DXTDGC*radian_r ! モデルの格子間隔
  real(8),    parameter:: dpsi= DYTDGC*radian_r
#endif /* ! OGCM_VARIABLE */
  !
  !J 鉛直格子厚さの設定
  !
#include "dz.F90"
  !
  !J     RHO0: 密度の基準値
  !
#ifdef OGCM_CALPP
  !
  !J     OGCM_CALPP: サブルーチン CALPP で圧力の水平平均を計算し、状態方程
  !J         式で圧力の変化を考慮する
  !
  !J     圧力の変化を考慮する場合は、圧力の初期値を計算する際に、密度 1.036
  !J     を用いる
  !
!!real(8),    parameter:: RHO0 = 1.036D0
  real(8),    parameter:: RHO0 = 1.0276D0
#else /* OGCM_CALPP */
  !
  !J     圧力の変化を考慮しない場合、圧力を計算する際に、密度 1.0 を用いる
  !
!!real(8),    parameter:: RHO0 = 1.036D0
  real(8),    parameter:: RHO0 = 1.0276D0
  !J!!!!real(8),    parameter:: RHO0 = 1.D0
#endif /* OGCM_CALPP */
  !
  !
  integer(4), parameter:: IJMTT = IMUT * JMUT !J 各層の格子点総数
!J  integer(4), parameter:: IMUD = IMUT * 32    ! 対流調節(CNVAJS)の作業領域のサイズ
!  integer(4), parameter:: KMM1 = KM - 1
  integer(4), parameter:: KMM1 = MAX(KM - 1,1)
  integer(4), parameter:: KMP1 = KM + 1
  integer(4), parameter:: LL = KM       !J 対流調節の繰り返し回数(LL = KM で不安定を残さない)
  integer(4), parameter:: NSMIN = 60    !J 1分の秒数
  integer(4), parameter:: NSHOUR = NSMIN * 60  !J 1時間の秒数
  integer(4), parameter:: NSDAY = 24 * NSHOUR  !J 1日の秒数
  integer(4), parameter:: IS30 = NSDAY * 30    !J 1か月(30日)の秒数
  integer(4), parameter:: IMN = 12             !J 1年の月数
!!!!!  integer(4), parameter:: ISRSTB = $ISRSTB     !J RSTBND を呼ぶ時間間隔(秒)
  !
#ifdef OGCM_MON30D
  integer(4), parameter:: NDYEAR = 360
#else /* OGCM_MON30D */
  integer(4), parameter:: NDYEAR = 365
#endif /* OGCM_MON30D */

#ifdef OGCM_CLMFRC
  integer(4), parameter:: NSYEAR = NDYEAR * NSDAY
#endif /* OGCM_CLMFRC */

!!!!!#ifdef OGCM_CLMFRC
!!!!!#ifdef OGCM_MONFRC
!!!!!  integer(4), parameter:: IFNMAX = IMN
!!!!!#else /* OGCM_MONFRC */
!!!!!  integer(4), parameter:: IFNMAX = NSYEAR / ISRSTB
!!!!!#endif /* OGCM_MONFRC */
!!!!!#endif /* OGCM_CLMFRC */
  !
  !J     NCPU: CPU 時間カウンタの数
  !
#ifndef OGCM_ICE
  !
  !J     OGCM_ICE 未定義: 海氷モデルなし
  !
  real(8), parameter:: TMICE = -1.7D0 !J 結氷点(水温はこれより低くならない)
  !
#endif /* ! OGCM_ICE */
  !
#ifdef OGCM_PARALLEL
  !
  !J     OGCM_PARALLEL: 並列計算
  !
  integer(4), parameter:: NPARTX = 4  !J  経度方向分割数
  integer(4), parameter:: NPARTY = 1  !J  緯度方向分割数
  integer(4), parameter:: NPARTA = NPARTX * NPARTY !J  プロセッサ数
  !
  !J     NPART: プロセッサ数
  !J     IMX,JMX,IJMX: 各プロセッサの計算領域の格子点数の最大値
  !
  integer(4), parameter:: NPART = NPARTA
!#ifdef OGCM_CMIP5
!  integer(4), parameter:: IMX = (IMUT - 5) / NPARTX + 5
!  integer(4), parameter:: JMX = (JMUT - 3) / NPARTY + 5
!#else /* OGCM_CMIP5 */
  integer(4), parameter:: IMX = (IMUT - 2) / NPARTX + 5
  integer(4), parameter:: JMX = (JMUT - 2) / NPARTY + 5
!#endif /* OGCM_CMIP5 */
  integer(4), parameter:: IJMX = IMX * JMX
  !
#else /* OGCM_PARALLEL */
  !
  !J     OGCM_PARALLEL 未定義: 並列計算を行なわない
  !
  integer(4), parameter:: NPARTA = 1
  integer(4), parameter:: NPART  = 1
  integer(4), parameter:: NPARTX = 1
  integer(4), parameter:: NPARTY = 1
  !
  !J     IMX,JMX,IJMX: 格子点数
  !
  integer(4), parameter:: IMX = IMUT, JMX = JMUT, IJMX = IMX * JMX
  !
#endif /* OGCM_PARALLEL */
  !
#ifdef OGCM_PARALLEL
  integer(4), parameter :: isfmrgn = 2
  integer(4), parameter :: jsfmrgn = 2
#else /* OGCM_PARALLEL */
  integer(4), parameter :: isfmrgn = 0
  integer(4), parameter :: jsfmrgn = 0
#endif /* OGCM_PARALLEL */

  !#ifdef OGCM_ICE
  !
  !J     海氷モデルのパラメータ
  !
  !J real(8), parameter :: si = 4.0D0 !海氷の塩分
  !
  !#endif /* OGCM_ICE */
  !
  integer(4), parameter:: IUVPNT = 0  !J  UV 点
  integer(4), parameter:: ITSPNT = 1  !J  TS 点
  !
  !
  real(4),    parameter:: UNDEF = -99. !J Levitus 気候値で これより小さい値は欠損値とみなす
  !
  !
  !
  integer(4), parameter:: IMF = IMUT, JMF = JMUT
!!!!!#ifdef OGCM_RUNOFF
!!!!!  integer(4), parameter:: IMROF = IMUT, JMROF = JMUT
!!!!!#endif /* OGCM_RUNOFF */
  !
#if defined OGCM_PARENT || defined OGCM_SUB
  integer(4), parameter :: recl_undble = 8
#endif /* OGCM_PARENT || OGCM_SUB */
  !
!!!!!#ifdef OGCM_BULKKARA
!!!!!  real(8), parameter :: close_budgets = $KARACBGT ! 1.1925D0 for OMIP
!!!!!#ifdef OGCM_ICE
!!!!!  real(8), parameter ::  corr_factor_n = $KARACFN, corr_factor_s = $KARACFS
!!!!!#endif /* OGCM_ICE */
!!!!!#endif /* OGCM_BULKKARA */
!!!!!
!!!!!#ifdef OGCM_GUST
!!!!!  real(8), parameter :: gust_slope = 1.d2 ! subgrid gustiness  [cm/s/K]
!!!!!  real(8), parameter :: gust_limit = 2.d2 ! limit of gustiness [cm/s]
!!!!!#endif /* OGCM_GUST */
  !
  integer(4), parameter :: numtrc_a = 2
  integer(4), parameter :: numtrc_p = 0
  integer(4), parameter :: numtrc = numtrc_a + numtrc_p
  !
#ifdef OGCM_OFLTALL
  integer(4), parameter :: ntrc_strt = numtrc_a + 1
#else /* OGCM_OFLTALL */
  integer(4), parameter :: ntrc_strt = 1
#endif /* OGCM_OFLTALL */
  !
end module oc_mod_param
