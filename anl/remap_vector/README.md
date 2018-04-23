プログラム: remap_vector.F90 
========

  ベクトル量の 一般直交座標海洋モデル => 緯度・経度格子への変換


説明
--------


-  一般直交座標海洋モデルの地形等の設定は、NAMELIST.MXE で与える。

-  緯度経度格子は、sph_grid （リストは src/gaussgrid.F90にあるものから選択）
   で指定


実行スクリプト例: remap_vector.sh 
--------

  namelist /nml_rmp_vec/
    sph_grid: 緯度・経度格子名 ... gaussgrid.F90 のリストから指定する。
    l_read_table: テーブルを読み込むか(.true.)新たに作成するか(.false.)
    l_area_weight: 格子分割を基本とする補間を行う。（サポートしていない）
    l_bicubic_trn: 双３次スプラインを基本とする補間を行う。
    l_check_table: 変換をチェックする(.true.)か否か(.false.)
    file_rmp_table: テーブルファイル名（scup タイプ）ベクトル用
    file_rmp_table_scl: テーブルファイル名（scup タイプ）スカラー用
    file_recv_area: 変換チェックの結果を書き出すファイル名
    flinu: インプットデータ(X成分ベクトル)のファイル名
    flinv: インプットデータ(Y成分ベクトル)のファイル名
    undef_in: インプットデータの未定義値
    l_input_little: 入力ファイルが little endian の場合 .true.
    itemu_start: 処理を行うデータ(X成分ベクトル)の１層目のレコード番号
    itemv_start: 処理を行うデータ(Y成分ベクトル)の１層目のレコード番号
    k_start: 処理する鉛直層の開始値
    k_end: 処理する鉛直層の終了値
    floutu: アウトプットデータ(X成分ベクトル)のファイル名
    floutv: アウトプットデータ(Y成分ベクトル)のファイル名
    undef_out: アウトプットデータの未定義値
    file_recv_area: 緯度・経度格子で変換の際受け取った重みの合計、平均 cos, 平均 sin 値を書き出す。
    flstdout: 実行ログの出力ファイル名


開発
--------

- 気象研究所 海洋・地球化学研究部 辻野 (htsujino@mri-jma.go.jp)
