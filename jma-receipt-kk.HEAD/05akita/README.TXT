変更点

※   国保の請求書の起動ボタンは２にしてください。
     国保の請求書以外は３にしてください
※   国保の請求書で、特定の保険者を一番初めに印刷したい時は
     YUSEN.INI  に保険者番号を半角６桁で設定してください
※   国保請求書で診療科名を印字する場合はSRYKA.INIを編集してください。


１ 国保請求書PG追加 03/10/16 1.1.2ver02

２ 国保請求書・保険者番号指定対応
               優先保険者番号指定対応
               在総診の別紙出力対応
               出力順修正
               診療科名追加
   04/01/27 2.0.0ver01

３ 古いPG削除      SOKATU0500 SOKATU0510
   国保請求書・様式変更に伴う新規作成 SOKATU0520
   04/06/02 2.3.0ver01

４ 複写式レセプト・対象公費追加
                   併用区分修正
   国保請求書・国保組合法定外給付時の対応追加
               特定の公費の場合、食事療養を集計しないよう修正
   04/07/16 2.3.1ver01

５ その他  再印刷頁指定対応修正
   04/12/03 2.4.0ver01

６ 複写式レセプト    再作成    SEIKYU0503
   福祉医療費請求票  新規作成  SEIKYU0505
   国保請求書        diaからmonpeに変更
                     福祉医療対象番号追加
                     OpenCOBOLバージョン変更対応
                     PAGE番号追加
   05/02/01 2.4.0ver02

７ 複写レセプト      入院版追加による再作成   SEIKYU0503
   05/05/13 2.4.1.ver01

８ 複写式レセプト    2.5.0対応(個別発行対応含む)
   福祉医療費請求票  2.5.0対応(個別発行対応含む)
   国保請求書        2.5.0対応(返戻対応含む)
   05/06/22 2.5.0.ver01

９ 複写式レセプト    制度変更による再作成
   福祉医療費請求票  対象番号追加(474)
                     一部負担金集計方法修正
   国保請求書        福祉医療対象番号追加(374)
   05/08/24 2.5.0ver02

１０ 複写式レセプト    「食事療養費」欄の「日数」を「回数」に変更
     国保請求書        「食事療養費」欄の「日数」を「回数」に変更
     06/05/15 2.9.0ver01

１１ その他　　　　    akita.bdファイルの修正
     06/08/10 3.1.0ver01

１２ 国保請求書　　    帳票様式の変更
     06/11/18 3.3.0ver01

１３ 複写式レセプト　　高齢者の場合、本人家族欄へ２箇所印字していたのを修正
     06/12/15 3.3.0ver02

１４ 複写式レセプト    帳票様式の変更
                       返戻時の診療年月の修正
                       プレビュー時の患者番号を印字
     07/05/08 3.4.0ver01

１５ 複写式レセプト　　高齢者の場合、本人家族欄へ２箇所印字していたのを再修正
     07/08/19 3.4.0ver02

１６ 国保請求書        日レセver4.0対応
                       システム管理マスタ「1901」の
                       医療機関情報を参照
     複写式レセプト　　日レセver4.0対応
                       システム管理マスタ「1901」の
                       医療機関情報を参照
                       長期上位（974）は長期と同様の
                       処理をする
     福祉医療費請求票  日レセver4.0対応
     07/09/28 4.0.0ver01

１７ 国保請求書        帳票様式の変更に伴う修正
     後期高齢者請求書  新規作成
     08/05/02 4.2.0ver01

１８ 後期高齢者請求書  公費は一箇所に集計する
     複写式レセプト    ４月改正対応
     08/05/16 4.2.0ver02

１９ 国保請求書        国組3〜6歳を分けないように修正
                       SPA-AREA初期化
     複写式レセプト    SPA-AREA初期化
     福祉医療費請求票  SPA-AREA初期化
     08/10/22 4.2.0ver03

２０ 国保請求書        月遅れ分は当月に合算する
     09/01/21 4.3.0ver01

２１ 国保請求書        点数表区分追加
     後期高齢者請求書  点数表区分追加
     シェルスクリプトの不要箇所削除
     09/04/06 4.4.0ver01

２２ 後期高齢者請求書  広域連合の保険者番号指定対応
     10/02/18 4.5.0ver01

２３ 国保請求書        オンライン返戻対応
                       食事療養を記載しない公費の追加
     後期高齢者請求書  オンライン返戻対応
                       食事療養を記載しない公費の追加
     10/07/28 4.5.0ver02

２４ 国保請求書        マル文字の印字ずれ修正
     Lucidでのインストールシェル文字化け対応
     10/09/17 4.5.0ver03

２５ 国保請求書        東北地方太平洋沖地震対応（対象者別集計）
     11/03/24 4.5.0ver04

２６ 国保請求書        印字位置がずれるのを修正
     11/04/19 4.5.0ver05

２７ 国保請求書        特別療養費国保対応
     11/06/15 4.6.0ver01

２８ 国保請求書        monpe-0.6.20対応
     後期高齢者請求書  monpe-0.6.20対応
                       PERFORMをGOTOに修正
     12/11/02 4.7.0ver01

２９ 国保請求書        プラグイン対応
                       医療機関情報編集対応
     後期高齢者請求書  プラグイン対応
                       医療機関情報編集対応
     アンインストール時に削除されないファイルを削除
     13/03/22 1.00

３０ シス管「3004」自動登録対応
     13/04/30 1.01

３１ 国保請求書        日レセDBの文字コードがUTB-8の場合
                       印字されないのを修正（プログラムオプション対応）
     13/09/11 1.02

３２ 国保請求書        前期高齢者２割対応
     コンパイルエラー時、プラグイン組込を行わないように修正
     14/05/21 1.03

３３ 国保請求書        返戻時、再印刷画面のタイトル修正
     シス管「3004」自動削除対応
     14/09/04 1.04

３４ 国保請求書        ver4.8.0対応（一時ディレクトリ変更）
     後期高齢者請求書  ver4.8.0対応（一時ディレクトリ変更）
     ruby1.9対応
     14/10/03 2.00

３５ 国保請求書        医療機関編集情報への開設者名追加対応
     後期高齢者請求書  医療機関編集情報への開設者名追加対応
     プログラムオプション更新対応
     プログラムオプショングループ診療対応
     システム管理マスタ「３００４」の自動登録・削除機能のグループ診療対応
     15/11/12 2.01

３６ 日レセver5.0.0対応
     16/11/30 3.00

３７ 国保請求書        医療機関コード変更時、医療機関コード別の作成処理対応。
                       SITEKBN変更（動作に変更なし）
                       バッチ処理ログ画面の出力対応
     後期高齢者請求書  医療機関コード変更時、医療機関コード別の作成処理対応。
                       SITEKBN変更（動作に変更なし）
                       バッチ処理ログ画面の出力対応
     18/02/07 3.01

３８ 日レセver5.1.0対応
     18/11/22 4.00

３９ 標準総括表印刷指示の設定対応
     複写式レセプト    修正履歴コメント削除
     福祉医療費請求票  修正履歴コメント削除
     国保請求書        修正履歴コメント削除
     後期高齢者請求書  修正履歴コメント削除
     19/12/06 4.01
