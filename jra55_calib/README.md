jra55_calib
========

  JRA-55 の TL319 格子データを較正する



プログラム・スクリプトの説明
--------

  * filter_wind_interannual : 風に東西 1-2-1 のフィルターをかける

  * 空間・時間分布のあるファクターをかける


  * 全球一様のファクターを計算する
     calc_const_calib_rad.F90
       namelist.const_calib_rad_c2
       namelist.const_calib_rad_e3
     calc_const_calib_prcp.F90
       namelist.const_calib_prcp_c2
       namelist.const_calib_prcp_e3

  * 全球一様のファクターをかける

    calib_rad_const_exec.sh
       namelist.calibrad.const_e3

    calib_prcp_snow_const_exec.sh
       namelist.calibpcp.const_e3
       namelist.calibpcp.snow_const_e3

  * 降雨データを作成する

     create_rain.sh
 

開発者
--------

  * 辻野 博之 (気象研究所 海洋・地球化学研究部)

