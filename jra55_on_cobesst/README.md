jra55_on_cobesst
========

  CORESST 格子上でのフラックスの評価

作業
--------

  * 変換テーブル作り
     - tmp2m_jra55toCOBE_snap_all.sh   (area weighted)
     - tmp2m_jra55toCOBE_snap_ocean.sh (area weighted)
     - uv10m_jra55toCOBE_snap_ocean.sh (bicubic spline)

  * 熱フラックスの評価
     - exec_diag_bulk.sh dataset_name
       これによりデータセット調整に必要なフラックスは全て得られる

開発
-------

  * 気象研究所 海洋・地球化学研究部 辻野

