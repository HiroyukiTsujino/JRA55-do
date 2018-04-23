jra55_corrfac
========

JRA-55 のバイアスを調整するファクターを求める


風（風速・方向）
-------

  * QuikSCAT のデータがある点で調整ファクターを求める

    - calc_wind_ceof_mon        (風向)
    - mk_correct_wvec_tanh_mon  (風速)
      ... 風向がメインのプログラムだが副産物として
          風速ファクタも出力するので利用
          逆に風向は利用しないほうが良い
          calc_wind_ceof_mon の確認に使う


  * QuikSCAT のデータが得られないJRA-55海洋格子の調整ファクターを
    決める
    
    - fill_wind_correc_antarc   
        QuikSCAT の正しいデータで４角を囲まれたデータのみで
        構成されるデータを使う場合にはこちらを使う

    - fill_wind_correc_antarc_v2
        QuikSCAT データの得られる格子を斜め外挿によって増やした
        場合にはこちらを使う

  * 調整ファクターのスムージング
    ５点フィルターを用いると沿岸付近の修正が良くなくなる
    フィルターはあまり使わないほうが良い

    - smooth_wind_mag           (風速に対する5点フィルター)
    - smooth_wind_rot           (風向に対する5点フィルター)
