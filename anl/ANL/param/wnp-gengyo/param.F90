! -*-F90-*-
!
!     MRI.COM(気象研究所統合海洋モデル) param.F90.in
!         Copyright 2002 Ocenographic Research Dept., MRI-JMA
!
!======================================================================
!
!     oc_mod_param: 定数の設定
!
module oc_mod_param   !  cgs単位系
  !
  implicit none
  !
  !     定数の定義
  !
  !コンパイル前にモデルの設定に合わせて以下のパラメータの値をセットする必要がある
  !
  !         IMUT, JMUT, KM
  !         PARALLEL の場合; NPARTA
  !
  !
  integer(4), parameter:: IMUT = 673   !  格子点数
  integer(4), parameter:: JMUT = 442
  integer(4), parameter:: KM = 54
  integer(4), parameter:: KSGM = 6
#ifdef OGCM_BBL
  integer(4), parameter:: KBBL = 
#endif /* ! OGCM_BBL */
  !
  real(8), parameter:: SLAT0 = 15.d0  !  海域の南端緯度
  real(8), parameter:: SLON0 = 117.d0  !  海域の東端経度
  !
  real(8), parameter :: NPLAT = 90.d0 ! モデルの北極点の地理座標における緯度
  real(8), parameter :: NPLON = 0.d0 ! モデルの北極点の地理座標における経度
#ifdef OGCM_SPHERICAL
  real(8), parameter :: SPLAT = -NPLAT ! モデルの南極点の地理座標における緯度
  real(8), parameter :: SPLON = NPLON+180.d0 ! モデルの南極点の地理座標における経度
#else /* OGCM_SPHERICAL */
  real(8), parameter :: SPLAT = -90.d0 ! モデルの南極点の地理座標における緯度
  real(8), parameter :: SPLON = 0.d0 ! モデルの南極点の地理座標における経度
#endif /* OGCM_SPHERICAL */
  !
  !
  ! 基本的なパラメータ
  !
  real(8), parameter :: PI =       3.141592653589793D0  ! 円周率
  real(8), parameter :: RADIAN =   180.D0/PI
  real(8), parameter :: radian_r = 1.d0/radian
  real(8), parameter :: OMEGA =    PI/43082.D0   ! 地球の回転角速度
  real(8), parameter :: RADIUS =   6375.D5       ! 地球の半径
  real(8), parameter :: RADIUR =   1.D0/RADIUS   ! 地球の半径の逆数
  real(8), parameter :: GRAV =     981.D0        ! 重力加速度
  real(8), parameter :: unit_bar = 1.d-6     ! dyn/cm^2 -> bar への変換係数
  real(8), parameter :: C24 =      1.D0/24.D0    ! 荒川スキームで使用
  !
  real(8), parameter :: TAB = 273.16D0  !  0℃の絶対温度
  real(8), parameter :: RO = 1.D0       !  水の密度
  real(8), parameter :: CP = 3.99D7     !  水の比熱
  real(8), parameter :: RROCP = 1.D0 / (RO * CP)
  !
  real(8), parameter :: abtm = 1.225D-3 ! 海底摩擦の係数
  !
  real(8), parameter :: stfblz = 5.67d-5 ! Stefan-Boltzman Constant (cgs)
  real(8), parameter :: cpa_mks = 1004.67d0 ! 大気比熱 (J/Kg/K)
  real(8), parameter :: ems = 0.97d0     ! emissivity of sea water
#ifdef OGCM_TESTING
  real(8), parameter :: rhoa_mks = 1.27d0 ! 大気密度 (kg/m^3)
#else /* OGCM_TESTING */
  real(8), parameter :: rhoa_mks = 1.205d0 ! 大気密度 (kg/m^3)
#endif /* OGCM_TESTING */
  !
#ifdef OGCM_SMAGOR
  !
  ! cscl : Smagorinsky 粘性係数決定用のパラメータ
  !        Laplacian では 3.0 - 4.0
  !        Bihamonic では 2.0 - 3.0
  !        が良いらしい（Griffies and Hallberg, 2000）が、
  !        モデルの解像度等に合わせてチューニングをする必要がある。
  !
  real(8), parameter :: cscl = 2.5d0
  !
#ifdef OGCM_SMAGHD
  real(8), parameter :: smaghd_ratio = $SMAGHD_RATIO
#endif /* OGCM_SMAGHD */
#endif /* OGCM_SMAGOR */
#ifdef OGCM_VISANISO
  real(8), parameter :: cc0 = 0.2d0
#endif /* OGCM_VISANISO */
#ifndef OGCM_VARIABLE
  real(8),    parameter:: DXTDGC = $DXTDGC
  real(8),    parameter:: DYTDGC = $DYTDGC
  real(8),    parameter:: dmu = DXTDGC*radian_r ! モデルの格子間隔
  real(8),    parameter:: dpsi= DYTDGC*radian_r
#endif /* ! OGCM_VARIABLE */
  !
  ! 鉛直格子厚さの設定
  !
#include "dz.F90"
  !
  integer(4), parameter:: ITMSC = 1   ! 海面での水温塩分への緩和日数
  integer(4), parameter:: ITMSCB = 10 ! body forcing 領域での水温塩分への緩和日数
  !
  real(4),    parameter:: UNDEF = -99. ! Levitus 気候値で これより小さい値は欠損値とみなす
  !
  !     RHO0: 密度の基準値
  !
#ifdef OGCM_CALPP
  !
  !     OGCM_CALPP: サブルーチン CALPP で圧力の水平平均を計算し、状態方程
  !         式で圧力の変化を考慮する
  !
  !     圧力の変化を考慮する場合は、圧力の初期値を計算する際に、密度 1.036
  !     を用いる
  !
  real(8),    parameter:: RHO0 = 1.036D0
#else /* OGCM_CALPP */
  !
  !     圧力の変化を考慮しない場合、圧力を計算する際に、密度 1.0 を用いる
  !
  real(8),    parameter:: RHO0 = 1.D0
#endif /* OGCM_CALPP */
  !
  !
  integer(4), parameter:: IJMTT = IMUT * JMUT ! 各層の格子点総数
  integer(4), parameter:: IMUD = IMUT * 32    ! 対流調節(CNVAJS)の作業領域のサイズ
  integer(4), parameter:: KMM1 = KM - 1
  integer(4), parameter:: KMP1 = KM + 1
  integer(4), parameter:: LL = KM       ! 対流調節の繰り返し回数(LL = KM で不安定を残さない)
  integer(4), parameter:: NSMIN = 60    ! 1分の秒数
  integer(4), parameter:: NSHOUR = NSMIN * 60  ! 1時間の秒数
  integer(4), parameter:: NSDAY = 24 * NSHOUR  ! 1日の秒数
  integer(4), parameter:: IS30 = NSDAY * 30    ! 1か月(30日)の秒数
  integer(4), parameter:: IMN = 12             ! 1年の月数
  integer(4), parameter:: ISRSTB = 86400     ! RSTBND を呼ぶ時間間隔(秒)
  !
#ifdef OGCM_MON30D
  integer(4), parameter:: NDYEAR = 360
#else /* OGCM_MON30D */
  integer(4), parameter:: NDYEAR = 365
#endif /* OGCM_MON30D */
#ifdef OGCM_CLMFRC
#ifdef OGCM_MONFRC
  integer(4), parameter:: IFNMAX = IMN
#else /* OGCM_MONFRC */
  integer(4), parameter:: NSYEAR = NDYEAR * NSDAY
  integer(4), parameter:: IFNMAX = NSYEAR / ISRSTB
#endif /* OGCM_MONFRC */
#endif /* OGCM_CLMFRC */
  !
  !     NCPU: CPU 時間カウンタの数
  !
#ifndef OGCM_ICE
  !
  !     OGCM_ICE 未定義: 海氷モデルなし
  !
  real(8), parameter:: TMICE = -1.7D0 ! 結氷点(水温はこれより低くならない)
  !
#endif /* ! OGCM_ICE */
  !
#ifdef OGCM_PARALLEL
  !
  !     OGCM_PARALLEL: 並列計算
  !
  integer(4), parameter:: NPARTA = 64  !  プロセッサ数
  !
  !     NPART: プロセッサ数
  !     IMX,JMX,IJMX: 各プロセッサの計算領域の格子点数の最大値
  !
  integer(4), parameter:: NPART = NPARTA
  integer(4), parameter:: IMX = IMUT, JMX = (JMUT - 3) / NPART + 5, IJMX = IMX * JMX
#else /* OGCM_PARALLEL */
  !
  !     OGCM_PARALLEL 未定義: 並列計算を行なわない
  !
  !     IMX,JMX,IJMX: 格子点数
  !
  integer(4), parameter:: IMX = IMUT, JMX = JMUT, IJMX = IMX * JMX
#endif /* OGCM_PARALLEL */
  !
  !#ifdef OGCM_ICE
  !
  !     海氷モデルのパラメータ
  !
  !real(8), parameter :: si = 4.0D0 !海氷の塩分
  !
  !#endif /* OGCM_ICE */
  !
  integer(4), parameter:: IUVPNT = 0  !  UV 点
  integer(4), parameter:: ITSPNT = 1  !  TS 点
  !
#ifdef OGCM_NPOLE
  !
  !     極フィルター
  !
  integer(4), parameter:: NPFMAX =  100    !  フィルターをかける回数の最大値
  real(8),    parameter:: PFLAT  =  84.5D0 !  フィルターをかける緯度の南限
#endif
  !
#ifdef OGCM_TSINTPOL
  !
  !     TSINTPOL: 水温塩分気候値をモデル格子に補間する
  !
  integer(4), parameter:: NUMBF = 1
  real(8),    parameter:: DLATC = 1.0d0
  real(8),    parameter:: DLONC = 1.0d0
  real(8),    parameter:: SLATC = -89.5d0
  real(8),    parameter:: SLONC = 0.5d0 
  !
  !     IMT, JMT, KK: 水温・塩分気候値の格子点数
  !
  integer(4), parameter:: IMT = 360, JMT = 180, KK = 33
  !
#else /* OGCM_TSINTPOL */
  !
  !     TSINTPOLオフの場合水温塩分気候値はモデル格子点全体に亘って読み込む
  !
  !     IMT, JMT, KK: 水温・塩分気候値の格子点数
  !
  integer(4), parameter:: IMT = IMUT, JMT = JMUT, KK = KM
  !
#endif /* OGCM_TSINTPOL */
  !
#ifdef OGCM_FLXINTPOL
  !
  !     FLXINTPOL: フラックスデータをモデル格子に補間する
  !
  integer(4), parameter:: IMF = 320, JMF = 160
#ifdef OGCM_RUNOFF
  integer(4), parameter:: IMROF = $IMROF, JMROF = $JMROF
#endif /* OGCM_RUNOFF */
  integer(4), parameter:: INTPWIND = 2 ! 1: 風応力は線型補間、2: 3次のスプライン補間
  !
#else /* OGCM_FLXINTPOL */
  !
  !     !FLXINTPOLの場合フラックスデータはモデル格子点全体に亘って読み込む
  !
  integer(4), parameter:: IMF = IMUT, JMF = JMUT
#ifdef OGCM_RUNOFF
  integer(4), parameter:: IMROF = IMUT, JMROF = JMUT
#endif /* OGCM_RUNOFF */
  !
#endif /* OGCM_FLXINTPOL */

#ifdef OGCM_SPLITREST
#ifdef OGCM_VVDIMP
#ifdef OGCM_MELYAM
  integer(4),parameter :: nvars = 10
#elif defined OGCM_NOHKIM
  integer(4),parameter :: nvars = 9
#else /* OGCM_MELYAM OGCM_NOHKIM */
  integer(4),parameter :: nvars = 7
#endif /* OGCM_MELYAM OGCM_NOHKIM */
#else /* OGCM_VVDIMP */
  integer(4),parameter :: nvars = 5
#endif /* OGCM_VVDIMP */
#endif /* OGCM_SPLITREST */

#ifdef OGCM_SPLITHIST
  integer(4),parameter :: nvars2 = 5
!!! commented out by ishikawa 2007-06-06
!!!#ifdef OGCM_DIAGNUMDIF
!!!  integer(4),parameter :: nvars6 = 4
!!!#endif /* OGCM_DIAGNUMDIF */
#endif /* OGCM_SPLITHIST */
  !
#if defined OGCM_PARENT || defined OGCM_SUB
  integer(4), parameter :: recl_undble = 8
#endif /* OGCM_PARENT || OGCM_SUB */
  !
#ifdef OGCM_BULKKARA
  real(8), parameter :: close_budgets = $KARACBGT ! 1.1925D0 for OMIP
#ifdef OGCM_ICE
  real(8), parameter ::  corr_factor_n = $KARACFN, corr_factor_s = $KARACFS
#endif /* OGCM_ICE */
#endif /* OGCM_BULKKARA */

#ifdef OGCM_GUST
  real(8), parameter :: gust_slope = 1.d2 ! subgrid gustiness  [cm/s/K]
  real(8), parameter :: gust_limit = 2.d2 ! limit of gustiness [cm/s]
#endif /* OGCM_GUST */
  !
  integer(4), parameter :: numtrc_a = 2
  integer(4), parameter :: numtrc_p = 0
  integer(4), parameter :: numtrc = numtrc_a + numtrc_p
  !
end module oc_mod_param
