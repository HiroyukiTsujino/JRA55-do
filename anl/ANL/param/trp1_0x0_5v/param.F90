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
  integer(4),parameter :: max_char_len = 16
  character(len=max_char_len),save :: name_model = 'OGCM'
  !
  !     定数の定義
  !
  !コンパイル前にモデルの設定に合わせて以下のパラメータの値をセットする必要がある
  !
  !         IMUT, JMUT, KM
  !         PARALLEL の場合; NPARTA
  !
  !
  integer(4), parameter:: IMUT = 364   !  格子点数
  integer(4), parameter:: JMUT = 368
  integer(4), parameter:: KM = 51
  integer(4), parameter:: KSGM = 5
#ifdef OGCM_BBL
  integer(4), parameter:: KBBL = 1
#endif /* ! OGCM_BBL */
  !
  real(8), parameter:: SLAT0 = -78.D0  !  海域の南端緯度
  real(8), parameter:: SLON0 = 0.D0  !  海域の東端経度
  !
  real(8), parameter :: NPLAT = 64.D0 ! モデルの北極点の地理座標における緯度
  real(8), parameter :: NPLON = 80.D0 ! モデルの北極点の地理座標における経度
#ifdef OGCM_SPHERICAL
  real(8), parameter :: SPLAT = -NPLAT ! モデルの南極点の地理座標における緯度
  real(8), parameter :: SPLON = NPLON+180.d0 ! モデルの南極点の地理座標における経度
#else /* OGCM_SPHERICAL */
  real(8), parameter :: SPLAT = 64.D0 ! モデルの南極点の地理座標における緯度
  real(8), parameter :: SPLON = 260.D0 ! モデルの南極点の地理座標における経度
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
  real(8), parameter :: RO = 1.036D0    !  水の密度
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
#ifndef OGCM_VARIABLE
  real(8),    parameter:: DXTDGC = 1.0d0
  real(8),    parameter:: DYTDGC = 0.5d0
  real(8),    parameter:: dmu = DXTDGC*radian_r ! モデルの格子間隔
  real(8),    parameter:: dpsi= DYTDGC*radian_r
#endif /* ! OGCM_VARIABLE */
  !
  ! 鉛直格子厚さの設定
  !
#include "dz.F90"
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
  real(8),    parameter:: RHO0 = 1.036D0
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
  !
#ifdef OGCM_MON30D
  integer(4), parameter:: NDYEAR = 360
#else /* OGCM_MON30D */
  integer(4), parameter:: NDYEAR = 365
#endif /* OGCM_MON30D */

#ifdef OGCM_CLMFRC
  integer(4), parameter:: NSYEAR = NDYEAR * NSDAY
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
  integer(4), parameter:: NPARTX = 4  !  経度方向分割数
  integer(4), parameter:: NPARTY = 16  !  緯度方向分割数
  integer(4), parameter:: NPARTA = NPARTX * NPARTY !  プロセッサ数
  !
  !     NPART: プロセッサ数
  !     IMX,JMX,IJMX: 各プロセッサの計算領域の格子点数の最大値
  !
  integer(4), parameter:: NPART = NPARTA
  integer(4), parameter:: IMX = (IMUT - 5) / NPARTX + 5
  integer(4), parameter:: JMX = (JMUT - 3) / NPARTY + 5
  integer(4), parameter:: IJMX = IMX * JMX
#else /* OGCM_PARALLEL */
  !
  !     OGCM_PARALLEL 未定義: 並列計算を行なわない
  !
  integer(4), parameter:: NPART = 1
  integer(4), parameter:: NPARTX = 1
  integer(4), parameter:: NPARTY = 1
  !
  !     IMX,JMX,IJMX: 格子点数
  !
  integer(4), parameter:: IMX = IMUT, JMX = JMUT, IJMX = IMX * JMX
#endif /* OGCM_PARALLEL */
  !
#ifdef OGCM_PARALLEL
  integer(4), parameter :: isfmrgn = 2
  integer(4), parameter :: jsfmrgn = 2
#else /* OGCM_PARALLEL */
  integer(4), parameter :: isfmrgn = 0
  integer(4), parameter :: jsfmrgn = 0
#endif /* OGCM_PARALLEL */
  integer(4), parameter :: imxsf = imx + isfmrgn * 2
  integer(4), parameter :: jmxsf = jmx + jsfmrgn * 2

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
#ifdef OGCM_TSINTPOL
  !
  !     TSINTPOL: 水温塩分気候値をモデル格子に補間する
  !
  real(8),    parameter:: DLATC = $DLATC
  real(8),    parameter:: DLONC = $DLONC
  real(8),    parameter:: SLATC = $SLATC
  real(8),    parameter:: SLONC = $SLONC 
  !
  !     IMT, JMT, KK: 水温・塩分気候値の格子点数
  !
  integer(4), parameter:: IMT = $IMT, JMT = $JMT, KK = $KK
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
  real(4),    parameter:: UNDEF = -99. ! Levitus 気候値で これより小さい値は欠損値とみなす
  !
#ifdef OGCM_FLXINTPOL
  !
  !     FLXINTPOL: フラックスデータをモデル格子に補間する
  !
  integer(4), parameter:: IMF = $IMF, JMF = $JMF
#ifdef OGCM_RUNOFF
  integer(4), parameter:: IMROF = $IMROF, JMROF = $JMROF
#endif /* OGCM_RUNOFF */
  integer(4), parameter:: INTPWIND = $INTPWIND ! 1: 風応力は線型補間、2: 3次のスプライン補間
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
#endif /* OGCM_SPLITHIST */
  !
#if defined OGCM_PARENT || defined OGCM_SUB
  integer(4), parameter :: recl_undble = 8
#endif /* OGCM_PARENT || OGCM_SUB */
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
  integer(4), parameter :: num_hstopts = 1
  !
#ifdef OGCM_NEMURO
  integer(4),parameter :: nTemp  =  1
  integer(4),parameter :: nSal   =  2
  integer(4),parameter :: nPHYS  =  3  ! 植物プランクトン [mol/m^3]   PhyPl
  integer(4),parameter :: nPHYL  =  4
  integer(4),parameter :: nZOOS  =  5  ! 動物プランクトン [mol/m^3]   ZooPl
  integer(4),parameter :: nZOOL  =  6  
  integer(4),parameter :: nZOOP  =  7  
  integer(4),parameter :: nNO3   =  8  ! 硝酸             [mol/m^3]   NO3
  integer(4),parameter :: nNH4   =  9  
  integer(4),parameter :: nPON   = 10  ! 有機廃棄物       [mol/m^3]   Detritus
  integer(4),parameter :: nDON   = 11 
  integer(4),parameter :: nSIL   = 12 
  integer(4),parameter :: nOPL   = 13 
  integer(4),parameter :: nO2    = 14  ! 溶存酸素     [mol/m^3]   O2
  integer(4),parameter :: nCaCO3 = nO2+1
  integer(4),parameter :: nTCO2  = nO2+2 ! 溶存無機炭素 [mol/m^3]   DIC
  integer(4),parameter :: nAlk   = nO2+3 ! アルカリ度   [mol/m^3]   ALK
!  integer(4),parameter :: nCa    = nO2+4
#endif /* OGCM_NEMURO */
  !
end module oc_mod_param
