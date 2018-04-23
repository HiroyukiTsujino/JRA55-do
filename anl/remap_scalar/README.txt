プログラム: remap_scalar.F90 

  スカラー量の 一般直交座標海洋モデル ⇒ 緯度・経度格子への変換

  注. CMIP5 で使用した scup 変換テーブルを用いるときは、
      コンパイル時に CPP オプション OGCM_CMIP5 を指定してください。
      Makefile 参照


実行スクリプト例: remap_scalar.sh 

  namelist /nml_rmp_scl/
    l_read_table: テーブルを読み込むか(.true.)新たに作成するか(.false.)
    file_rmp_table: テーブルファイル名（scup タイプ）
    l_check_table: 変換をチェックする(.true.)か否か(.false.)
    file_recv_area: 変換チェックの結果を書き出すファイル名
    idiv: テーブルを作成する際、海洋格子を X 方向に idiv 分割する。l_read_table=.false. のとき必要。
    jdiv: テーブルを作成する際、海洋格子を Y 方向に jdiv 分割する。l_read_table=.false. のとき必要。
    sph_grid: 緯度・経度格子名 ... gaussgrid.F90 のリストから指定する。
    tuw: 海洋モデルの t (トレーサー)点, u (水平速度)点、w (鉛直速度)点のスカラー量を変換する
    flin: インプットデータのファイル名
    undef_in: インプットデータの未定義値
    item_start: 処理を行うデータの１層目のレコード番号
    k_start: 処理する鉛直層の開始値
    k_end: 処理する鉛直層の終了値
    flout: アウトプットデータのファイル名
    undef_out: アウトプットデータの未定義値
    file_recv_area: 緯度・経度格子で受け取った重みの合計を出力（チェック用）
    fltopo: 海洋モデルの地形情報ファイル
    flsclf: 海洋モデルの格子面積情報ファイル
    flstdout: 実行ログの出力ファイル名

海洋モデルが可変格子の場合（VARIABLE）は、格子情報ファイルを指定する。

  namelist /inflg/ file_vgrid
