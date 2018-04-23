jra55_org_grid_anl
========

  JRA-55 の予測値(fcst)や解析値(anl)をオリジナル格子(reduced TL319 格子)
  で処理する


プログラム・スクリプトの説明
--------

  * GRIB から flat binary へ

     - grid2flatbin_anl_surf.sh start_year end_year


  * IABP-NPOLES と JRA-55 anl_surf (月毎データ) をブレンドする

     - blend_iabp_npoles_anl_exec.sh 


  * QuikSCAT と JRA-55 anl_surf (日毎データ) をブレンドする
    QuikSCAT を equivalent neutral 10m wind から real 10 m wind に換算する

     - blend_quikscat_anl_exec.sh


  * JRA55 の風速を 10 m 中立成層と同等(equivalent 10 m neutral wind)の
    ものに換算する

     - jra55_neutral_wind_exec.sh
       namelist.jra55_neutral_wind_template
       src/diag_neutral_wind.F90

  * ERA-Interim を JRA55 reduced TL319 格子に内挿する

     - erai_on_jra55_exec.sh

  * Remote Sensing Systems の風を JRA55 reduced TL319 格子に内挿する

     - remsswind_on_jra55_exec.sh
     - remss_zm_on_jra55_exec.sh (東西平均をとる)

開発者
--------

  * 辻野 博之 (気象研究所 海洋・地球化学研究部)
