変更点

※  県単独情報でSOKATU2700、SOKATU2710を登録する時は２のボタンにチェックを入れて下さい
    その他は３にチェックを入れて下さい

※   国保請求書(府内)で、指定した保険者を一番初めに印刷したい時は
     YUSEN.INI  に保険者番号を半角６桁で設定してください

１０ 一部負担金相当額等助成費請求書  ２７老人の時の市町村名修正
   04/02/05 2.0.0ver04

１１ 障・母・乳医療費請求書  記号、番号を２列で印字するよう変更
                             備考欄の患者負担額算出方法変更
                             患者負担額を印字する対象公費変更
     一部負担金相当額等助成費請求書・一部負担金算出方法変更
     国保請求書(府内)  保険者番号指定対応追加
                       件数、点数等の桁数変更
                       老人＋原爆件数集計方法変更
                       出力順修正
     国保請求書(府外)  件数、点数等の桁数変更
                       老人＋原爆件数集計方法変更
     04/04/12 2.2.2ver01

１２ 一部負担金相当額等助成費請求書  老人＋在宅等の対応修正
     04/10/07 2.4.0ver01

１３ 障・母・乳医療費請求書              様式変更による再作成
     一部負担金相当額等一部助成費請求書  様式変更による再作成
     その他                              再印刷頁指定対応修正
     04/10/14 2.4.0ver02

１４ 障・母・乳医療費請求書              助成が発生しない場合も記載するよう修正
     一部負担金相当額等一部助成費請求書  助成が発生しない場合も記載するよう修正
     04/11/16 2.4.0ver03

１５ 国保請求書(府内)  在宅データの分類方法修正
     04/12/06 2.4.0ver04

１６ 一部負担金相当額等一部助成費請求書  助成がない場合に０円を記載するよう修正
     04/12/16 2.4.0ver05

１７ 国保・請求書集計処理(府内用)  DiaからMonpeへ変更
                                   ページ番号追加
     国保・請求書集計処理(府外用)  帳票変更による再作成
     05/01/28 2.4.0ver06

１８ 国保・請求書(府内)  医療機関コード(Monpe)のフォントサイズを小さくするよう修正
     05/02/15 2.4.0ver07

１９ 障・母・乳医療費請求書          明細のフォント(Monpe)をMinchoに変更
     一部負担金相当額等助成費請求書  月の途中で主保険が切り替わっても負担者番号と受給者番号が同じであれば
                                     一人の人に集計するよう修正
                                     収納読み込みのKEYを２から２５に変更
                                     明細のフォント(Monpe)をMinchoに変更
     国保・請求書集計処理(府内用)    ページ番号削除
                                     明細のフォント(Monpe)をMinchoに変更
                                     明細の点数等一部バイト数変更
     国保・請求書集計処理(府外用)    ページ番号削除
                                     明細のフォント(Monpe)をMinchoに変更
     05/03/09 2.4.1.ver01

２０ 一部負担金相当額等助成費請求書  前回の修正により、一部負担金に不具合が生じてしまったため再び修正
                                     主保険と４１老人が両方変わった場合２行に分かれて印字されるようPTKOHINFを
                                     読み込むKEYを修正
     05/03/25 2.4.1.ver02

２１ 障・母・乳医療費請求書          2.5.0対応(個別発行対応,月途中公費切替え対応含む)
     一部負担金相当額等助成費請求書  2.5.0対応(個別発行対応含む)
                                     主保険の適用期間より以前の老人公費があった場合に、その老人公費は対象と
                                     ならないよう修正(修正内容−保険組合せマスタを読みにいく際のKEYをKEY２
                                     からKEY５に変更。COMB-DELKBNが１だったら読み飛ばす。PTKOHINFを読みに
                                     いく際のPTKOH-TEKSTYMDとPTKOH-TEKEDYMDの条件を変更)
     国保・請求書集計処理(府内用)    2.5.0対応(返戻対応含む)
     国保・請求書集計処理(府外用)    2.5.0対応(返戻対応含む)
     05/05/13 2.5.0.ver01

２２ 障・母・乳医療費請求書          全国公費を併用した場合でも受給者負担金額が１円単位で表示されるよう修正
     一部負担金相当額等助成費請求書  全国公費を併用した場合でも受給者負担金額が１円単位で表示されるよう修正
     05/06/28 2.5.0.ver02

２３ 国保・請求書集計処理(府内用)    堺市政令指定都市化対応
     06/04/10 2.6.0.ver01

２４ 障・母・乳医療費請求書          誤字の修正
     一部負担金相当額等助成費請求書  誤字の修正
     診療報酬請求書(府内用)    「在医総管・在医総」への修正
     診療報酬請求書(他府県用)  「食事療養」欄の「日数」を「回数」に変更
     06/05/26 2.9.0.ver01

２５ 診療報酬請求書(府内用)    「在医総管・在医総」への集計を修正
     06/06/06 2.9.0.ver02

２６ 　　　　　　　　　　　    osaka.bdファイルの修正
     06/07/12 2.9.0.ver03

２７ 一部負担金相当額等一部助成費請求書　　堺市政令指定都市化対応
     06/08/25 3.1.0.ver01

２８ 診療報酬請求書(府内用)    帳票様式の変更に伴う修正
                               食事がない場合でも生活療養費を印字
     診療報酬請求書(他府県用)  帳票様式の変更
                               食事がない場合でも生活療養費を印字
     障・母・乳医療費請求書    帳票様式の変更に伴う修正
     06/11/18 3.3.0.ver01

２９ 診療報酬請求書(府内用)          給付割合表示の修正
     障・母・乳医療費請求書          長期使用時、上限を印字する
     一部負担金相当額等助成費請求書  一部負担金、受給者負担金額欄は０なら０を印字
                                     診療時の保険番号マスタを参照する
     07/01/12 3.3.0.ver02

３０ 一部負担金相当額等助成費請求書  診療時の保険組み合わせを取得する
     07/03/09 3.3.0.ver03

３１ 障・母・乳医療費請求書          21,79等併用時に備考欄に患者負担が
                                     印字されなかったのを修正
     07/07/04 3.4.0.ver01

３２ 診療報酬請求書(府内用)          日レセver4.0対応
                                     システム管理マスタ「1901」の
                                     医療機関情報を参照
     診療報酬請求書(他府県用)        日レセver4.0対応
                                     システム管理マスタ「1901」の
                                     医療機関情報を参照
     障・母・乳医療費請求書          日レセver4.0対応
                                     システム管理マスタ「1901」の
                                     医療機関情報を参照
     一部負担金相当額等助成費請求書  日レセver4.0対応
                                     システム管理マスタ「1901」の
                                     医療機関情報を参照
                                     長期上位（974）は長期と同様の
                                     処理をする
                                     ４１老人の場合、市町村名「豊中市」が
                                     印字されないのを修正
     07/09/17 4.0.0.ver01

３３ 障・母・乳医療費請求書          返戻分は印字対象外とする
     一部負担金相当額等助成費請求書  返戻分は印字対象外とする
     08/02/08 4.0.0.ver02

３４ 診療報酬請求書(府内用)          帳票様式の変更に伴う修正
                                     SPA-AREA初期化
     診療報酬請求書(他府県用)        帳票様式の変更に伴う修正
                                     SPA-AREA初期化
     後期高齢者請求書(府内用)        新規作成
     後期高齢者請求書(他府県用)      新規作成
     08/04/25 4.2.0.ver01

３５ 診療報酬請求書(他府県用)        公費再掲欄一部負担金印字修正
     08/05/07 4.2.0.ver02

３６ 診療報酬請求書(府内用)          線の位置修正
     後期高齢者請求書(府内用)        まとめ公費を使用した時の
                                     まとめ公費以外の公費名称修正
     後期高齢者請求書(他府県用)      まとめ公費を使用した時の
                                     まとめ公費以外の公費名称修正
     08/05/16 4.2.0.ver03

３７ 診療報酬請求書(府内用)          ３月以前の返戻分を診療年月毎でなく
                                     まとめて印字する
     診療報酬請求書(他府県用)        ３月以前の返戻分を診療年月毎でなく
                                     まとめて印字する
     08/06/14 4.2.0.ver04

３８ 障・母・乳医療費請求書          SPA-AREA初期化
     一部負担金相当額等助成費請求書  SPA-AREA初期化
     診療報酬請求書(他府県用)        シス管「2007」公費まとめ対応
     08/11/18 4.2.0.ver05

３９ 診療報酬請求書(府内用)          公費の負担金欄は保険がある場合は
                                     保険、無い場合は公費の値を印字
     09/03/12 4.3.0.ver01

４０ シェルスクリプトの不要箇所削除
     09/04/10 4.4.0.ver01

４１ 診療報酬請求書(他府県用)        シス管「2007」のまとめ公費で
                                     6つ目以降の公費がまとまらない不具合を修正
     後期高齢者請求書(府内用)        シス管「2007」のまとめ公費で
                                     6つ目以降の公費がまとまらない不具合を修正
     後期高齢者請求書(他府県用)      シス管「2007」のまとめ公費で
                                     6つ目以降の公費がまとまらない不具合を修正
     09/12/03 4.4.0.ver02

４２ 診療報酬請求書(府内用)          オンライン返戻対応
     診療報酬請求書(他府県用)        オンライン返戻対応
     後期高齢者請求書(府内用)        広域連合の保険者番号指定対応
                                     オンライン返戻対応
     後期高齢者請求書(他府県用)      広域連合の保険者番号指定対応
                                     オンライン返戻対応
     10/04/06 4.5.0.ver01

４３ 障・母・乳医療費請求書          マル文字の印字ずれ修正
     一部負担金相当額等助成費請求書  マル文字の印字ずれ修正
     Lucidでのインストールシェル文字化け対応
     10/09/17 4.5.0.ver02

４４ 診療報酬請求書(府内用)          特別療養費国保対応
                                     open-cobol1.0対応
     診療報酬請求書(他府県用)        特別療養費国保対応
                                     open-cobol1.0対応
     後期高齢者請求書(府内用)        open-cobol1.0対応
     10/11/24 4.5.0.ver03

４５ 診療報酬請求書(他府県用)        政令指定都市も区毎に集計する
     11/02/02 4.5.0.ver04

４６ 後期高齢者請求書(府内用)        PERFORMをGOTO命令に修正
     後期高齢者請求書(他府県用)      PERFORMをGOTO命令に修正
     12/03/16 4.6.0.ver01

４７ 後期高齢者請求書(府内用)        シス管「1901」で設定した電話番号が
                                     印字されるように修正
     後期高齢者請求書(他府県用)      シス管「1901」で設定した電話番号が
                                     印字されるように修正
     12/04/09 4.6.0.ver02

４８ 診療報酬請求書(府内用)          在宅は一般分にまとめて集計する
     12/06/18 4.6.0.ver03

４９ 診療報酬請求書(府内用)          monpe-0.6.20対応
     診療報酬請求書(他府県用)        monpe-0.6.20対応
     後期高齢者請求書(府内用)        monpe-0.6.20対応
     後期高齢者請求書(他府県用)      monpe-0.6.20対応
     12/11/12 4.7.0.ver01

５０ 診療報酬請求書(府内用)          プラグイン対応
                                     医療機関情報編集対応
     診療報酬請求書(他府県用)        プラグイン対応
                                     医療機関情報編集対応
     後期高齢者請求書(府内用)        プラグイン対応
                                     医療機関情報編集対応
     後期高齢者請求書(他府県用)      プラグイン対応
                                     医療機関情報編集対応
     アンインストール時に削除されないファイルを削除
     13/03/22 1.00

５１ シス管「3004」自動登録対応
     13/04/30 1.01

５２ 診療報酬請求書(府内用)          プログラムオプション対応
     後期高齢者請求書(府内用)        在宅はまとめて集計するように修正
     後期高齢者請求書(他府県用)      在宅はまとめて集計するように修正
     13/06/14 1.02

５３ 診療報酬請求書(府内用)          前期高齢者２割対応
     診療報酬請求書(他府県用)        前期高齢者２割対応
     コンパイルエラー時、プラグイン組込を行わないように修正
     14/05/13 1.03

５４ 診療報酬請求書(府内用)          ver4.8.0対応（一時ディレクトリ変更）
     診療報酬請求書(他府県用)        ver4.8.0対応（一時ディレクトリ変更）
     後期高齢者請求書(府内用)        ver4.8.0対応（一時ディレクトリ変更）
     後期高齢者請求書(他府県用)      ver4.8.0対応（一時ディレクトリ変更）
     シス管「3004」自動削除対応
     ruby1.9対応
     14/10/23 2.00

５５ 診療報酬請求書(府内用)          医療機関編集情報への開設者名追加対応
     診療報酬請求書(他府県用)        医療機関編集情報への開設者名追加対応
     後期高齢者請求書(府内用)        医療機関編集情報への開設者名追加対応
     後期高齢者請求書(他府県用)      医療機関編集情報への開設者名追加対応
     プログラムオプション更新対応
     プログラムオプショングループ診療対応
     シス管「3004」自動登録・削除のグループ診療対応
     16/02/12 2.01

５６ 日レセver5.0.0対応
     16/11/30 3.00

５７ 診療報酬請求書(府内用)          医療機関コード変更時、医療機関コード別の作成処理対応
                                     返戻時、再印刷画面のタイトル修正
                                     バッチログ画面出力対応
     診療報酬請求書(他府県用)        医療機関コード変更時、医療機関コード別の作成処理対応
                                     返戻時、再印刷画面のタイトル修正
                                     公費番号まとめ情報の公費番号の追加
                                     バッチログ画面出力対応
     後期高齢者請求書(府内用)        医療機関コード変更時、医療機関コード別の作成処理対応
                                     公費番号まとめ情報の公費番号の追加
                                     バッチログ画面出力対応
     後期高齢者請求書(他府県用)      医療機関コード変更時、医療機関コード別の作成処理対応
                                     公費番号まとめ情報の公費番号の追加
                                     バッチログ画面出力対応
     17/09/06 3.01

５８ 障・母・乳医療費請求書          修正履歴コメント削除
                                     SITEKBN変更（動作に変更なし）
     一部負担金相当額等助成費請求書  修正履歴コメント削除
                                     SITEKBN変更（動作に変更なし）
     診療報酬請求書(府内用)          修正履歴コメント削除
                                     SITEKBN変更（動作に変更なし）
     診療報酬請求書(他府県用)        修正履歴コメント削除
                                     保険番号マスタの取得の修正
                                     SITEKBN変更（動作に変更なし）
     後期高齢者請求書(府内用)        修正履歴コメント削除
                                     SITEKBN変更（動作に変更なし）
     後期高齢者請求書(他府県用)      修正履歴コメント削除
                                     SITEKBN変更（動作に変更なし）
     18/12/03 3.02

５９ 日レセver5.1.0対応
     18/11/22 4.00

６０ 標準総括表印刷指示の設定対応
     20/02/14 4.01
