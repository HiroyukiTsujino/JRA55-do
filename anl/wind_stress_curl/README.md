wind_stresss_curl
========

風応力カールの計算


ドキュメント
--------

  * src/curltau.F90 : namelist.curl_tau を読み込んで計算を行う
  * sample/namelist.curl_tau : namelist /nml_curltau/ を指定
  * nml_curltau
      flinx : 東西成分風応力データファイル
      fliny : 南北成分風応力データファイル
      irecx : 東西成分風応力データファイルから読み込むレコード番号
      irecy : 南北成分風応力データファイルから読み込むレコード番号
      l_units_cgs : インプットデータが cgs か否か(デフォルト .false. = MKS)
      flout : 出力ファイル
      l_nohalo_cyclic : 周期境界条件を持つデータで、のりしろをもたない場合 .true.


開発
-------

  * 開発：気象研究所 海洋・地球化学研究部 第一研究室
  * 窓口：辻野 <htsujino@mri-jma.go.jp>
