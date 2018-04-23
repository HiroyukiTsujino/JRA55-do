COBESST
========

  COBESST の処理を行う


作業手順解説
--------

  * COBESST をスキャンして海陸マスクを作成する
     - src/scan_cobesst.F90
       namelist.scandata

  * JRA-55 の海陸マスク作成方法については ../README_topo_v3.md を参照


  * 湖などを埋めて海陸分布を調整する

     - divide_to_text.sh
         ----> ocean
     - unite_to_bindary.sh
