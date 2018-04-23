jra55_org_grid_product
========

  JRA-55 データセットの仕上げ


プログラム・スクリプトの説明
--------

  * 気温・比湿の 2m から 10m へのシフト
     - produce_tmp_sph_10m_exec.sh

  * reduced TL319 -> regular TL319 の変換テーブルの作成
     - make_tables.sh
     - check_tables.sh

  * 個々の要素の reduced TL319 -> regular TL319 の変換作業
     - red2reg_exec_e[0,2,3].sh


開発者
--------

  * 辻野 博之 (気象研究所 海洋・地球化学研究部)
