jra55_org_grid_calc_mean
========

  JRA-55 の平均値をオリジナル格子(reduced TL319 格子)で求める



プログラム・スクリプトの説明
--------

  * make_org_to_daily_surf.F90 (namelist.make_daily_surf)
    オリジナルの時間間隔データから日平均データを作成
    (０時と２４時のデータがあることを考慮している)

    - anl_surf
        シェルスクリプト      : make_daily_anl_surf_exec.sh (fcst と一括)
        namelist テンプレート : namelist.make_daily_anl_surf_template

    - fcst_surf
        シェルスクリプト      : make_daily_fcst_surf_tmp2m.sh
        namelist テンプレート : namelist.make_daily_fcst_surf_template

    - fcst_e1 (1-2-1 フィルターをかけた風速ベクトル)
        シェルスクリプト      : make_daily_fcst_e1_exec.sh
        namelist テンプレート : namelist.make_daily_fcst_e1_template

    - fcst_e2 (気温修正後の比湿)
        シェルスクリプト      : make_daily_fcst_e2_exec.sh
        namelist テンプレート : namelist.make_daily_fcst_e2_template


  * make_daily_to_monthly.F90
    日平均データから月平均データを作成する

    - make_day2mon_anl_tmp2m.sh
    - make_day2mon_anl_sph2m.sh
    - make_day2mon_blend_wind.sh
    - make_day2mon_fcst_e1_wind.sh
    - make_day2mon_fcst_e2_sph2m.sh
    - make_day2mon_fcst_tmp_ice.sh


  * make_org_to_monthly_flux.F90 (namelist_org2monthly_flux_org_template)
    オリジナルの時間間隔フラックスーデータから月平均データを作成

    - make_monthly_precip_org.sh ... fcst_phy2m の降水
    - make_monthly_rad_org.sh    ... fcst_phy2m の放射


開発者
--------

  * 辻野 博之 (気象研究所 海洋・地球化学研究部)
