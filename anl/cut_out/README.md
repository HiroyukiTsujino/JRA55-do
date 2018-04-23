cut_out
========

任意の連結領域データの切り出し


ドキュメント
--------

  * src/cut_out.F90 : namelist.cut_out を読み込んでデータの切り出しを行う
  * sample/namelist.cut_out : namelist /nml_cut_out/ を指定
  * nml_cut_out
     flin='入力ファイル',
     undef_in        : 未定義値の指定
     var_num=1,      : データの要素数
     rec_num=1,      : データのレコード数
     tuxy_in='u',    : データの定義点
     iw_core_in=1,   : データの東西コア（袖を除く）領域の西端
     ie_core_in=640, : データの東西コア（袖を除く）領域の東端
     lon_stt=80.0d0, : 切り出し領域の西端経度
     lon_end=440.0d0,: 切り出し領域の東端経度
     lat_stt=-30.0d0,: 切り出し領域の南端緯度
     lat_end=30.0d0, : 切り出し領域の北端緯度
     dep_stt=0.0d0,  : 切り出し領域の上端深度
     dep_end=0.0d0,  : 切り出し領域の下端深度
     var_stt=1,      : 切り出し要素の開始番号
     var_end=1,      : 切り出し要素の終了番号
     rec_stt=1,      : 切り出しレコードの開始番号
     rec_end=1,      : 切り出しレコードの開始番号
     i_append_west=3,: 切り出し領域の西側に付け足すデータ数
     i_append_east=3,: 切り出し領域の西側に付け足すデータ数
     flout='出力ファイル',
     fllonlat='londat.d',
     flctl='出力ファイルのGrADsコントロールファイル',
     lmakectl=.true., : GrADsコントロールファイルを出力するか否か
     var_short='pr',
     var_long='Precipitation_both_liquid_and_solid_phases_units_kg_per_square_meter_per_second',
     linear_x=.true.,
     linear_y=.false.,
     linear_z=.false.


開発
-------

  * 開発：気象研究所 海洋・地球化学研究部 第一研究室
  * 窓口：辻野 htsujino@mri-jma.go.jp
