runoff_imported
========

  外部から提供された河川データの処理


Note for JRA55-do-v1.4
--------

  * Monthly data from Bamber et al. (2018) is mapped on 0.25x0.25 grid
    and then temporary interpolated to daily interval.

    During this processing, a mask file for JRA55-do-v1.4 river discharge
    data is generated.

    - src/mk_GrnCaaFreshFlux_Bamber_2018.F90
      Be careful not to overwrite existing
      "nextxy_wo_lake_big_endian_noyrev_lon0strt_rm_GAC_add_Bamber_2018.bin".
      (no overwrite by default)


処理の説明
--------

A. 流路網mapの修正

A1. endian, 座標の修正
 元dataはlittle endian + -180E, 90N から startするdataであるので、
 big endian + 0E, 90S から startする data に修正。

         実行 ./transfer_endian   

         入力 data_etc/nextxy_little_endian.bin
              data_etc/out_xy_little_endian.bin
              data_etc/flwdir_little_endian.bin@

         出力 data_etc/nextxy_big_endian_noyrev_lon0strt.bin
              data_etc/out_xy_big_endian_noyrev_lon0strt.bin
              data_etc/flwdir_big_endian_noyrev_lon0strt.bin

A2. 湖等の消去
         準備 ./ulimit -s unlimited
         実行 ./delete_lake

         入力 data_etc/nextxy_big_endian_noyrev_lon0strt.bin
         出力 data_etc/nextxy_wo_lake_big_endian_noyrev_lon0strt.bin

A3. 源流の同定
         実行 ./find_waterhead
         入力 data_etc/nextxy_big_endian_noyrev_lon0strt.bin
              data_etc/out_xy_big_endian_noyrev_lon0strt.bin
              data_etc/flwdir_big_endian_noyrev_lon0strt.bin
         出力 date_etc/waterhead_xy_big_endian_noyrev_lon0strt.bin
              date_etc/flw.gs  (作画用)
              date_etc/flw2.gs (作画用)


以下でのfresh water flux の出力は全て 
 空間分布 1/4x1/4 単位 m^3 /s 
 全球積算値の単位は Sv
  

B. Antarctica 周りのCalving Flux (CF), Basal Melt Flux (BMF) の作成

         実行  ./mk_AntFreshFlux_Depoorter

         入力 data_Antarctica/Ant_ICESat_MODIS_Mask_1km.ascii

         出力 data_Antarctica/CF_025x025.dat
              data_Antarctica/BMF_025x025.dat

B.1 data_Antarctica のdataをdat_new にcopyしておく
         cp data_Antarctica/BMF_025x025.dat data_new/BMF_025x025.dat 
         cp data_Antarctica/CF_025x025.dat  data_new/CF_025x025.dat 

C. Greenland の流出を別fileにしてrunoff_ice.grdにある値と足す。
   また、 河口のうちGreenlandとAntarcticaの周りのものを-9ではなく、
   それぞれ-888,-777とする。
   また、全球およびGreenlandの合計のrunoffを出力する。

         実行 ./remove_Greenland_Antarctica

         入力 data_etc/nextxy_wo_lake_big_endian_noyrev_lon0strt.bin         
              data_org/runoffYYYY.grd     YYYY = [1958-2015]
              data_org/runoff_ice.grd          
              data_Antarctica/CF_025x025.dat   ! total Antarctica 算出のみに用いる
              data_Antarctica/BMF_025x025.dat  ! total Antarctica 算出のみに用いる

         出力 data_etc/nextxy_wo_lake_big_endian_noyrev_lon0strt_rm_GA.bin
              data_new/runoff.YYYY
              data_new/runoff_Greenland.YYYY          (D で置き換えるため使わない)
	      data_new/total_runoff.YYYY             ! Greenlandと南極以外の合計
	      data_new/total_runoff_Greenland.YYYY    (D で置き換えるため使わない)
	      data_new/total_Antarctica.cnst         ! 南極周りでの合計

D.  Bamber et al. (2012) のGreenland回りの total fresh water flux (5km mesh, monthly)
    の 1961-1990 の平均を用いて、0.25x0.25 の total fresh water flux のmonthly気候値
    を作成する。また、Greenland の河口のindex file としてこのデータと整合するもの
    を作成する。killworth (1996, JPO) の時間フィルターを用いてdaily の気候値も作る.
     (月平均dataの線形時間では使わない場合には場合により10%を越すエラーが生じるため）
    normal year のみデータを作成し、閏年の 2/29 は 2/28 と 3/1 の平均とする。

         実行 ./mk_GrnFreshFlux_Bamber

         入力 data_Grn_Bamber_org/monthly/fwf_green.YYYYMM   YYYYMM:195801-201012
              data_etc/nextxy_wo_lake_big_endian_noyrev_lon0strt_rm_GA.bin

         出力 data_Grn_Bamber_new/monthly/fwf_green_025x025.YYYYMM          (中間file)
              data_Grn_Bamber_new/monthly/total_fwf_green_025x025.YYYYMM    (中間file)

              data_Grn_Bamber_new/clim/fwf_green_025x025.MM        MM:01-12(monthly)  
              data_Grn_Bamber_new/clim/total_fwf_green.MM          MM:01-12(monthly)

              data_Grn_Bamber_new/clim/fwf_green_025x025_365dy (通常年)
              data_Grn_Bamber_new/clim/fwf_green_025x025_366dy (閏年)
              data_Grn_Bamber_new/clim/total_fwf_green_365dy

              data_etc/nextxy_wo_lake_big_endian_noyrev_lon0strt_rm_GA_add_Bamber.bin

E. CORE 河川データを Greenland 用に使うとき

   runoff_core で既に 0.25x0.25 格子の気候値(1961-1990)が計算されている


         実行 ./mk_GrnFreshFlux_CORE

         入力 data_Grn_CORE_org/monclim_025x025/fwf_green_025x025.MM
              data_etc/nextxy_wo_lake_big_endian_noyrev_lon0strt_rm_GA.bin

         出力 data_Grn_Bamber_new/clim/total_fwf_green.MM          MM:01-12(monthly)

              data_Grn_Bamber_new/clim/fwf_green_025x025_365dy (通常年)
              data_Grn_Bamber_new/clim/fwf_green_025x025_366dy (閏年)
              data_Grn_Bamber_new/clim/total_fwf_green_365dy

              data_etc/nextxy_wo_lake_big_endian_noyrev_lon0strt_rm_GA_add_CORE.bin


F. Merino et al. (2016) のIceberg model のmonthly気候値を 0.25x0.25 のmonthly気候値を作成。
   月毎に場所が異るので killworth の時間filterを使わずに単純線形補間でdailyの気候値を作成。

         実行 ./mk_IcebergFlux_Merino

         入力 data_Iceberg_org/mmc2.dat <= mmc2.nc から writemmc2.gs を使って作成

         出力 data_Iceberg/Merino_icb_025x025.MM           MM:01-12(monthly)  
              data_Iceberg/Merino_icb_025x025.365dy        365dy で回すとき
              data_Iceberg/Merino_icb_025x025.366dy        366dy で回すとき
              data_Iceberg/iceberg_index                   icebergに値がある時-666が入っている

G. まとめ

結局、OGCM への fresh water flux の算出に用いるのは以下の4つ(または7つ)の外力データ
空間分布 1/4x1/4 単位 m^3 /s  (ただし 全球積算値の単位は Sv )
  
   ! 時間によらない南極周りのfresh water flux
   data_Antarctica/CF_025x025.dat   
   data_Antarctica/BMF_025x025.dat  
   (両者の積算値は data_Grn_new/total_Antarctica.cnst)

   ! 月平均気候値のGreenland周りのfresh water flux
   data_Grn_Bamber_new/clim/fwf_green_025x025.MM    MM (0:12) 
   (積算値は data_Grn_Bamber_new/clim/total_fwf_green.??)
   または上を時間で線形補間したデータ (prep/rivermouth ではこちらを使う)
     data_Grn_Bamber_new/clim/fwf_green_025x025_365dy
     data_Grn_Bamber_new/clim/fwf_green_025x025_366dy


   ! daily の経年変動含む上記以外の河川による fresh water flux
   data_new/runoff.YYYY                  YYYY (1958-2015)
   (全球積算値は data_new/total_runoff.????)


及び、地形・流路に関する以下の３つのデータ

   data_etc/nextxy_wo_lake_big_endian_noyrev_lon0strt_rm_GA_add_Bamber.bin 

   data_etc/out_xy_big_endian_noyrev_lon0strt.bin (水源)
   data_etc/flwdir_big_endian_noyrev_lon0strt.bin (流路)

このうち最後の二つの流路網に関するデータは runoff.YYYY にのみ適用する。
南極、グリーンランドまわりは、nextx, が-777, -888 で判断し最も近くの海岸にばらまく。




付録
--------

   * src/read_bamber_fwf.F90, namelist.bamber_runoff で
     Bamber 教授提供のデータを処理する。

   * src/scan_noyrev.F90, make_mask_[region]_[dataset].sh で
     0.25x0.25 格子のマスク(1/0)を作成
