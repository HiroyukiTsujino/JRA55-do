
JRA55-do riverデータ作成手引き． 鈴木立郎　2017.12.26

 CaMa-Floodを用いて河川データ作成を行う手引き

1.JRA55の3hourly陸面データ(yyyymmは日付）を気象庁より取得する．
 場所　ftp://ds.data.jma.go.jp/JRA-55/Hist
　/Daily/fcst_phyland/(yyyymm)/fcst_phyland.（yyyymmddhh）

2.陸面データから河川流出データをバイナリファイルとして切り出す．
　CONV_GRIB2bin1dy.shを実行し、T319bin1dy/water(yyyymmdd)ファイルを作成．
　注：シェルスクリプト内で　wgribコマンドを使用.あらかじめインストールしておく必要がある．
  この時点でwater(yyyymmdd)ファイルには表面流出データと底面流出データが含まれる．

3.T319座標からCaMa-Floodの座標（0.25度グリット）に、３時間間隔のデータを１日間隔に変換．
　同時に表面流出データおよび底面流出データの和にする．
　CONV_T319toRIV1dyディレクトリ内のシェルスクリプトをしかるべく編集して実行．
　注：src以下はあらかじめ環境にあわせてコンパイルしておく．
　注：TL319.maskの指定を忘れずに（TL319.gribを変換したもの あらかじめ以下のコマンドで切り出しておく）
    ( > wgrib -s TL319.grib | egrep '(:LAND:)' | wgrib -i -bin -ieee TL319.grib -o TL319.mask ）
　シェルスクリプとを実行すると/RIVGRID_1dy/rof(yyyymmdd).grdファイル(little_endian)が作成される．

4.流域ごとにファクターを乗じて流出量の補正を行う.2010年以降は一定．
　factorはあらかじめ計算しておいたdaily factor map (TFACT2_rivmap160805)を用いる。
　RIVGRID_1dy/rof*.grd -> RIVGRID_1dy_tfact2_rivmap160805/rof*.grd.

5.実行プログラムは　JRA55_tfact2_v2_rivmap160805.sh
　入力データはオリジナルではRIVGRID_dy_tfact2_rivmap160805 以下を使用しているが、データ作成の手続きを変更
　したもの　RIVGRID_1dy_tfact2　以下のデータ使用をするのがよい。データの差は計算手続き変更による誤差がのこる？
