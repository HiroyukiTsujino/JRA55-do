jra55_org_grid_corrfac
========

  JRA-55 の較正ファクターをオリジナル格子(reduced TL319 格子)で求める
  


プログラム・スクリプトの説明
--------

  * 下向き短波・長波放射量の調整

    - make_correc_rad_exec.sh*
        namelist.make_dswrf_correc
        namelist.make_dlwrf_correc

  * 降水量の調整
    - make_correc_precip_exec.sh*
        namelist.make_precip_correc

  * 風速ベクトルの調整
    - make_correc_wind_exec.sh*
        namelist.windcorrec_ceof_mon
        namelist.windcorrec_tanh_mon

  * 気温の調整
    - make_correc_tmp2m_exec.sh*
        namelist.make_tmp2m_all_correc
        namelist.make_tmp2m_ice_correc （海氷上. 10年以上海氷が存在する場合）

  * 比湿の調整
    - make_correc_sph2m_exec.sh*
        namelist.make_sph2m_correc


開発者
--------

  * 辻野 博之 (気象研究所 海洋・地球化学研究部)
