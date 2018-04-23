プログラム: map_vector.F90 

  ベクトル量の 緯度・経度格子 ⇒ 一般直交座標海洋モデルへの変換
  既存の変換テーブルを読み込むか、bi-cubic spline を行なう。

  注. CMIP5 で使用した scup 変換テーブルを用いるときは、
      コンパイル時に CPP オプション OGCM_CMIP5 を指定してください。
      Makefile 参照

実行スクリプト例: map_vector.sh 

  namelist /nml_map_vec/
    l_read_table: テーブルを読み込むか(.true.)新たに作成するか(.false.)
    file_rmp_table: テーブルファイル名（scup タイプ）
    idiv: 格子分割によるテーブルを作成する際、データ格子を X 方向に idiv 分割する。
    jdiv: 格子分割によるテーブルを作成する際、データ格子を Y 方向に jdiv 分割する。
          (現在のところ、格子分割による変換には未対応)  
    sph_grid: 緯度・経度格子名 ... gaussgrid.F90 のリストから指定する。
    tuw: 海洋モデルの t (トレーサー)点, u (水平速度)点、w (鉛直速度)点のスカラー量を変換する
          (現在のところ、水平速度点(u)への内挿のみ対応している)
    flinu: インプットデータ(X成分ベクトル)のファイル名
    flinv: インプットデータ(Y成分ベクトル)のファイル名
    l_read_sep: X,Y 成分ベクトルを別ファイルから入力
    id_start: 処理したいデータの開始番号
    id_end:   処理したいデータの終了番号
    undef_in: インプットデータの未定義値
    itemu_start: 処理を行うデータ(X成分ベクトル)の１層目のレコード番号
    itemv_start: 処理を行うデータ(Y成分ベクトル)の１層目のレコード番号
    itemu_intv: 処理を行うデータ(X成分ベクトル)のレコードインターバル
    itemv_intv: 処理を行うデータ(Y成分ベクトル)のレコードインターバル
    k_start: 処理する鉛直層の開始値
    k_end: 処理する鉛直層の終了値
    floutu: アウトプットデータ(X成分ベクトル)のファイル名
    floutv: アウトプットデータ(Y成分ベクトル)のファイル名
    l_write_sep: X,Y 成分ベクトルを別ファイルに出力
    undef_out: アウトプットデータの未定義値
    file_recv_area: 海洋格子で変換の際受け取った重みの合計、平均 cos, 平均 sin 値を書き出す。
    fltopo: 海洋モデルの地形情報ファイル
    flsclf: 海洋モデルの格子面積情報ファイル
    flstdout: 実行ログの出力ファイル名

  海洋モデルが可変格子の場合（VARIABLE）は、格子情報ファイルを指定する。

  namelist /inflg/ file_vgrid
