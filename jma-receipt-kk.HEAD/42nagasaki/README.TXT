変更点

※国保請求書で特定の保険者を最初に印刷したい時は、YUSEN.INIに
  半角６桁の保険者番号を設定してください。

※国保総括表・国保請求書の起動ボタンは２にしてください。

１.国保総括表              新規作成  SOKATU4200
   国保請求書              新規作成  SOKATU4210
   04/08/02 2.3.1ver01

２ 国保総括表  diaからmonpeに変更
               起動ボタンを３→２に変更
   国保請求書  diaからmonpeに変更
   その他      再印刷対応修正
   04/09/08  2.3.1ver02

３ 国保総括表  monpe修正
   国保請求書  monpe修正
   04/09/29 2.3.1ver03

４ 国保請求書  ２７老人で０１９、１８６を持っている人を２７老人欄にも集計するよう修正
               ページ番号追加
               monpeの誤字を修正
   その他      Open cobolのバージョンアップによる修正
   05/07/06 2.4.1ver01

５ 国保総括表  Monpe修正
               2.5.0対応による再作成
   国保請求書  Monpe修正
               2.5.0対応による再作成
   05/07/20 2.5.0ver01

６ 国保請求書  記載するデータが有る用紙のみ出力するよう修正
               ２７＋１９の集計方法修正
               Monpeの罫線修正
   05/08/17 2.5.0ver02

７ 国保請求書  ２７＋１９の集計方法修正
   05/09/28 2.5.0ver03

８ 国保請求書  nagasaki.bdファイルの修正
   06/07/20 3.0.0ver01

９ 国保総括表  帳票様式の変更
   国保請求書  帳票様式の変更
   06/10/17 3.1.0ver01

１０ 国保請求書  老人＋原爆の印字条件修正
                 （レセ電と紙レセで単独、併用と印字が異なる）
   07/03/28 3.3.0ver01

１１ 国保請求書  帳票様式の変更に伴う修正
                 公費再掲の法制欄は全公費「００」を印字
   07/05/15 3.4.0ver01

１２ 国保総括表  日レセver4.0対応
                 システム管理マスタ「1901」の
                 医療機関情報を参照
                 長期上位（974）は長期と同様の
                 処理をする
                 返戻対応
     国保請求書  日レセver4.0対応
                 システム管理マスタ「1901」の
                 医療機関情報を参照
                 長期上位（974）は長期と同様の
                 処理をする
   07/08/27 4.0.0ver01

１３ 国保総括表        帳票様式の変更に伴う修正
                       SPA-AREA初期化
     国保請求書        帳票様式の変更に伴う修正
                       SPA-AREA初期化
     後期高齢者請求書  新規作成
   08/04/26 4.2.0ver01

１４ 後期高齢者請求書  医療機関コードバイト数修正
   08/04/29 4.2.0ver02

１５ 後期高齢者請求書  原爆の値が二倍になるのを修正
   08/05/01 4.2.0ver03

１６ 国保総括表        帳票様式の変更に伴う修正
     国保請求書        国組３〜６歳の帳票を分けないように修正
     後期高齢者請求書  原爆・被爆は再掲として集計
                       公費名バイト数修正
                       まとめ公費を使用した時の
                       まとめ公費以外の公費名称修正
   08/05/21 4.2.0ver04

１７ 国保総括表        ３歳未満の年齢判定修正
                       後期高齢のみの場合でも印字する
   08/06/20 4.2.0ver05

１８ 国保請求書        シス管「2007」公費まとめ対応
   08/11/19 4.2.0ver06

１９ 国保総括表        請求書枚数に在宅分も含める
   08/12/15 4.3.0ver01

２０ シェルスクリプトの不要箇所削除
   09/04/10 4.4.0ver01

２１ 国保請求書        シス管「2007」のまとめ公費で
                       6つ目以降の公費がまとまらない不具合を修正
     後期高齢者請求書  シス管「2007」のまとめ公費で
                       6つ目以降の公費がまとまらない不具合を修正
   10/01/07 4.4.0ver02

２２ 国保総括表        オンライン返戻対応
     国保請求書        オンライン返戻対応
     後期高齢者請求書  オンライン返戻対応
                       広域連合の保険者番号指定対応
   10/05/17 4.5.0ver01

２３ 国保総括表        再印刷画面に返戻分は「返戻分」と表示
     国保請求書        再印刷画面に返戻分は「返戻分」と表示
   10/08/17 4.5.0ver02

２４ Lucidでのインストールシェル文字化け対応
   10/09/06 4.5.0ver03

２５ 国保総括表        特別療養費国保対応
     国保請求書        特別療養費国保対応
                       open-cobol1.0対応
                       レイアウト微調整
   11/01/12 4.5.0ver04

２６ 国保総括表        monpe-0.6.20対応
     国保請求書        monpe-0.6.20対応
     後期高齢者請求書  monpe-0.6.20対応
                       シス管「1901」で設定した電話番号が
                       印字されるように修正
                       PERFORMをGOTOに修正
   12/11/19 4.7.0ver01

２７ 国保総括表        帳票様式の変更
                       医療機関情報編集対応
     国保請求書        医療機関情報編集対応
     後期高齢者請求書  医療機関情報編集対応
   13/02/18 4.7.0ver02

２８ 国保総括表        プラグイン対応
     国保請求書        プラグイン対応
     後期高齢者請求書  プラグイン対応
     アンインストール時に削除されないファイルを削除
   13/03/25 1.00

２９ シス管「3004」自動登録対応
   13/05/01 1.01

３０ 国保請求書        長期分を別集計せず一般分に合算するよう修正
                       プログラムオプション対応
     コンパイルエラー時、プラグイン組込を行わないように修正
     シス管「3004」自動削除対応
   14/01/10 1.02

３１ 国保総括表        帳票様式の変更
   14/03/12 1.03

３２ 国保請求書        前期高齢者２割対応
   14/05/15 1.04

３３ プラグイン更新対応
   14/08/06 1.05

３４ 国保総括表        ver4.8.0対応（一時ディレクトリ変更）
     国保請求書        ver4.8.0対応（一時ディレクトリ変更）
     後期高齢者請求書  ver4.8.0対応（一時ディレクトリ変更）
     ruby1.9対応
   14/10/28 2.00

３５ 国保総括表        医療機関編集情報への開設者名追加対応
     国保請求書        医療機関編集情報への開設者名追加対応
     後期高齢者請求書  医療機関編集情報への開設者名追加対応
     プログラムオプション更新対応
     プログラムオプショングループ診療対応
     システム管理マスタ「３００４」の自動登録・削除機能のグループ診療対応
   16/03/11 2.01

３６ 日レセver5.0.0対応
   16/11/30 3.00

３７ 国保総括表        帳票様式の変更
                       医療機関コード変更時、医療機関コード別の作成処理対応
                       バッチログ画面出力対応
     国保請求書        公費番号まとめ情報の公費番号の追加
                       医療機関コード変更時、医療機関コード別の作成処理対応
                       バッチログ画面出力対応
                       ginbee対応
     後期高齢者請求書  公費番号まとめ情報の公費番号の追加
                       医療機関コード変更時、医療機関コード別の作成処理対応
                       バッチログ画面出力対応
   17/08/02 3.01

３８ 日レセver5.1.0対応
   18/11/22 4.00

３９ 国保総括表        修正履歴コメント削除
                       SITEKBN変更（動作に変更なし）
     国保請求書        修正履歴コメント削除
                       SITEKBN変更（動作に変更なし）
                       保険番号情報の取得処理を修正
     後期高齢者請求書  修正履歴コメント削除
                       SITEKBN変更（動作に変更なし）
   19/02/07 4.01

４０ 国保総括表        帳票様式の変更
   19/03/04 4.02

４１ 国保総括表        改元対応
   19/03/04 4.03

４２ 国保総括表        帳票様式の変更
   19/06/12 4.04
