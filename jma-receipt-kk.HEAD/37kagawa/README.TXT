変更点

※   国保の請求書の起動ボタンは２にしてください。
     国保の請求書以外は３にしてください
※   国保の請求書で、指定した保険者を一番初めに印刷したい時は
     YUSEN.INI  に保険者番号を半角６桁で設定してください
※   坂出市医療費請求書・入院用、坂出市医療費請求書・老人用に
     振込先を印字する時はKINYU.INIに金融機関情報を設定してく
     ださい
※   宇多津町用ー乳幼児医療費請求書・重度心身障害者等医療費請
     求書・母子家庭等医療費請求書に振込先を印字する時は、KINY
     U_UTAZU.INIに金融機関情報を設定してください

１ 国保送り状PG(食事療養費帳票)追加 03/10/16 ver1.1.2

２ 国保請求書PG追加 03/10/16 ver1.1.2

３ 国保診療報酬請求書送り状・ハッチング箇所を修正、郵便番号削除
   03/11/28 1.1.3ver01

４ 国保請求書・保険者番号指定対応
               優先保険者番号指定対応
               保険者名編集方法修正
               51,91の集計方法修正
   04/01/28 2.0.0ver01

５ 医療費支給申請書(一般用)・公費名にチェックを入れるよう修正
                             月遅れ対応
                             一部負担金集計方法修正
                             他法負担額集計方法修正
                             出力順修正
   医療費支給申請書(老人用)・公費名にチェックを入れるよう修正
                             月遅れ対応
                             一部負担金集計方法修正
                             他法負担額集計方法修正
                             出力順修正
   国保送り状・退職者の分類方法修正
               公費分集計方法修正
               医療機関CDを半角に修正
   食事療養費送り状・医療機関CDを半角に修正
                     公費分集計方法修正
   国保請求書・桁数修正
               公費分集計方法修正
   04/07/08 2.3.1ver01

６ 医療費支給申請書(一般用)   Open cobolのバージョンアップに伴う修正
                              DiaからMonpeへ変更
   医療費支給申請書(老人用)   Open cobolのバージョンアップに伴う修正
                              DiaからMonpeへ変更
   国保送り状                 Open cobolのバージョンアップに伴う修正
                              DiaからMonpeへ変更
                              県内・県外で別々に印刷されるよう修正
   食事療養費送り状           Open cobolのバージョンアップに伴う修正
                              DiaからMonpeへ変更
                              県内・県外で別々に印刷されるよう修正
   国保請求書                 Open cobolのバージョンアップに伴う修正
                              DiaからMonpeへ変更
   05/05/25 2.4.1.ver01

７ 医療費支給申請書(一般用)   2.5.0対応、プログラム一本化による再作成
   医療費支給申請書(老人用)   2.5.0対応、プログラム一本化による再作成
   国保送り状                 2.5.0対応による再作成
   食事療養費送り状           2.5.0対応による再作成
   国保請求書                 2.5.0対応による再作成
   05/07/13 2.5.0.ver01

８ 国保請求書                 在宅分は紙を分けずに合算するよう修正
   05/07/25 2.5.0.ver02

９ 坂出市医療費請求書・入院用   新規作成
   坂出市医療費請求書・老人用   新規作成
   05/09/28 2.5.0.ver03

１０  READMEファイルにKINYU.INIの記載を追加
   05/10/03 2.5.0.ver04

１１ 乳幼児医療費請求書        (宇多津町用)    新規作成
     重度心身障害者医療費請求書(宇多津町用)    新規作成
     母子家庭等医療費請求書    (宇多津町用)    新規作成
   05/10/27 2.5.0.ver05

１２ 国保送り状              ２７＋１９は法別番号「７７」として集計されるよう修正
     食事療養費送り状        ２７＋１９は法別番号「７７」として集計されるよう修正
     国保請求書              ２７＋１９は法別番号「７７」として集計されるよう修正
   05/11/21 2.5.0.ver06

１３ その他    　　　　　　　kagawa.bdファイルの修正
   06/08/08 3.1.0.ver01

１４ 国保送り状              帳票様式の変更に伴う修正
     食事療養費送り状        帳票様式の変更
     国保請求書              帳票様式の変更に伴う修正
   06/11/26 3.3.0.ver01

１５ 国保請求書              入院データの印字ズレを修正
   07/01/09 3.3.0.ver02

１６ 国保送り状                         "051"と"091"は合算して集計する
                                        27+19も19で集計する
     食事療養費送り状                   "051"と"091"は合算して集計する
                                        27+19も19で集計する
     国保請求書                         27+19も19で集計する
     乳幼児医療費請求書 (宇多津町用)　　帳票様式の変更に伴う修正
   07/04/13 3.4.0.ver01

１７ 国保送り状                         "051"と"091"の合算の修正
     食事療養費送り状                   "051"と"091"の合算の修正
   07/06/06 3.4.0.ver02

１８ 国保送り状                              日レセver4.0対応
                                             システム管理マスタ「1901」の
                                             医療機関情報を参照
                                             返戻対応
     食事療養費送り状                        日レセver4.0対応
                                             システム管理マスタ「1901」の
                                             医療機関情報を参照
                                             返戻対応
     国保請求書                              日レセver4.0対応
                                             システム管理マスタ「1901」の
                                             医療機関情報を参照
     医療費支給申請書(一般用)                日レセver4.0対応
                                             システム管理マスタ「1901」の
                                             医療機関情報を参照
                                             長期上位（974）は長期と同様の
                                             処理をする
     医療費支給申請書(老人用)                日レセver4.0対応
                                             システム管理マスタ「1901」の
                                             医療機関情報を参照
                                             長期上位（974）は長期と同様の
                                             処理をする
     坂出市医療費請求書・入院用              日レセver4.0対応
                                             長期上位（974）は長期と同様の
                                             処理をする
     坂出市医療費請求書・老人用              日レセver4.0対応
                                             長期上位（974）は長期と同様の
                                             処理をする
     乳幼児医療費請求書        (宇多津町用)  日レセver4.0対応
                                             システム管理マスタ「1901」の
                                             医療機関情報を参照
                                             長期上位（974）は長期と同様の
                                             処理をする
     重度心身障害者医療費請求書(宇多津町用)  日レセver4.0対応
                                             システム管理マスタ「1901」の
                                             医療機関情報を参照
                                             長期上位（974）は長期と同様の
                                             処理をする
     母子家庭等医療費請求書    (宇多津町用)  日レセver4.0対応
                                             システム管理マスタ「1901」の
                                             医療機関情報を参照
                                             長期上位（974）は長期と同様の
                                             処理をする
   07/09/08 4.0.0.ver01

１９ 国保送り状                              帳票様式の変更に伴う修正
     食事療養費送り状                        帳票様式の変更に伴う修正
     国保請求書                              帳票様式の変更に伴う修正
     後期高齢送り状                          新規作成
     後期高齢者請求書                        新規作成
   08/04/27 4.2.0.ver01

２０ 後期高齢送り状                          県内・県外と分けて印字
                                             まとめ公費を使用した時の
                                             まとめ公費以外の公費名称修正
     後期高齢者請求書                        まとめ公費を使用した時の
                                             まとめ公費以外の公費名称修正
   08/05/13 4.2.0.ver02

２１ 後期高齢送り状                          在宅分もまとめて印字
     後期高齢者請求書                        在宅分もまとめて印字
                                             再印刷画面の帳票タイトル修正
   08/07/11 4.2.0.ver03

２２ 医療費支給申請書(一般用)                SPA-AREA初期化
     医療費支給申請書(老人用)                SPA-AREA初期化
     坂出市医療費請求書・入院用              SPA-AREA初期化
     坂出市医療費請求書・老人用              SPA-AREA初期化
     乳幼児医療費請求書(宇多津町用)          SPA-AREA初期化
     重度心身障害者医療費請求書(宇多津町用)  SPA-AREA初期化
     母子家庭等医療費請求書(宇多津町用)      SPA-AREA初期化
     国保送り状                              シス管「2007」公費まとめ対応
     食事療養費送り状                        シス管「2007」公費まとめ対応
     国保請求書                              シス管「2007」公費まとめ対応
   08/12/17 4.3.0.ver01

２３ シェルスクリプトの不要箇所削除
   09/04/10 4.4.0.ver01

２４ 国保送り状                              精神の一部負担金を表示
   09/07/14 4.4.0.ver02

２５ 国保送り状                              シス管「2007」のまとめ公費で
                                             6つ目以降の公費がまとまらない不具合を修正
     食事療養費送り状                        シス管「2007」のまとめ公費で
                                             6つ目以降の公費がまとまらない不具合を修正
     国保請求書                              シス管「2007」のまとめ公費で
                                             6つ目以降の公費がまとまらない不具合を修正
     後期高齢者送り状                        シス管「2007」のまとめ公費で
                                             6つ目以降の公費がまとまらない不具合を修正
     後期高齢者請求書                        シス管「2007」のまとめ公費で
                                             6つ目以降の公費がまとまらない不具合を修正
   09/12/25 4.4.0.ver03

２６ 国保送り状                              オンライン返戻対応
     食事療養費送り状                        オンライン返戻対応
     国保請求書                              オンライン返戻対応
     後期高齢者送り状                        オンライン返戻対応
                                             広域連合の保険者番号指定対応
     後期高齢者請求書                        オンライン返戻対応
                                             広域連合の保険者番号指定対応
     医療費支給申請書(老人用)                帳票廃止
     坂出市医療費請求書(老人用)              帳票廃止
   10/05/13 4.5.0.ver01

２７ Lucidでのインストールシェル文字化け対応
   10/08/24 4.5.0.ver02

２８ 国保送り状                              主科対応
     食事療養費送り状                        主科対応
     国保請求書                              主科対応
   10/09/24 4.5.0.ver03

２９ 国保送り状                              特別療養費国保対応
     食事療養費送り状                        特別療養費国保対応
     国保請求書                              特別療養費国保対応
     坂出市医療費請求書・入院用              open-cobol1.0対応
   11/01/11 4.5.0.ver04

３０ 国保送り状                              帳票が印刷されない不具合を修正
     食事療養費送り状                        帳票が印刷されない不具合を修正
   11/04/01 4.5.0.ver05

３１ 宇多津町母子家庭等医療費請求書          帳票様式の変更
   11/09/14 4.6.0.ver01

３２ 国保送り状                              monpe-0.6.20対応
     食事療養費送り状                        monpe-0.6.20対応
     国保請求書                              monpe-0.6.20対応
     後期高齢者送り状                        monpe-0.6.20対応
                                             PERFORMをGOTOに修正
     後期高齢者請求書                        monpe-0.6.20対応
                                             シス管「1901」で設定した電話番号が
                                             印字されるように修正
                                             PERFORMをGOTOに修正
     医療費支給申請書(一般用)                monpe-0.6.20対応
     坂出市医療費請求書・入院用              monpe-0.6.20対応
     乳幼児医療費請求書(宇多津町用)          monpe-0.6.20対応
     重度心身障害者医療費請求書(宇多津町用)  monpe-0.6.20対応
     母子家庭等医療費請求書(宇多津町用)      monpe-0.6.20対応
   12/11/19 4.7.0.ver01

３３ 国保送り状                              プラグイン対応
                                             医療機関情報編集対応
     食事療養費送り状                        プラグイン対応
                                             医療機関情報編集対応
     国保請求書                              プラグイン対応
                                             医療機関情報編集対応
     後期高齢者送り状                        プラグイン対応
                                             医療機関情報編集対応
     後期高齢者請求書                        プラグイン対応
                                             医療機関情報編集対応
     医療費支給申請書(一般用)                プラグイン対応
                                             医療機関情報編集対応
     坂出市医療費請求書・入院用              プラグイン対応
                                             医療機関情報編集対応
     乳幼児医療費請求書(宇多津町用)          プラグイン対応
                                             医療機関情報編集対応
     重度心身障害者医療費請求書(宇多津町用)  プラグイン対応
                                             医療機関情報編集対応
     母子家庭等医療費請求書(宇多津町用)      プラグイン対応
                                             医療機関情報編集対応
     アンインストール時に削除されないファイルを削除
   13/03/25 1.00

３４ 国保送り状                              帳票様式の変更
     食事療養費送り状                        帳票様式の変更
     シス管「3004」自動登録対応
   13/05/09 1.01

３５ 国保送り状                              前期高齢者２割対応
     食事療養費送り状                        前期高齢者２割対応
     国保請求書                              前期高齢者２割対応
                                             プログラムオプション対応
     坂出市医療費請求書・入院用              プログラムオプション対応
     乳幼児医療費請求書(宇多津町用)          プログラムオプション対応
     重度心身障害者医療費請求書(宇多津町用)  プログラムオプション対応
     母子家庭等医療費請求書(宇多津町用)      プログラムオプション対応
    コンパイルエラー時、プラグイン組込を行わないように修正
   14/05/14 1.02

３６ 国保送り状                              ver4.8.0対応（一時ディレクトリ変更）
     食事療養費送り状                        ver4.8.0対応（一時ディレクトリ変更）
     国保請求書                              ver4.8.0対応（一時ディレクトリ変更）
     後期高齢者送り状                        ver4.8.0対応（一時ディレクトリ変更）
     後期高齢者請求書                        ver4.8.0対応（一時ディレクトリ変更）
     医療費支給申請書(一般用)                ver4.8.0対応（一時ディレクトリ変更）
     坂出市医療費請求書・入院用              ver4.8.0対応（一時ディレクトリ変更）
     乳幼児医療費請求書(宇多津町用)          ver4.8.0対応（一時ディレクトリ変更）
     重度心身障害者医療費請求書(宇多津町用)  ver4.8.0対応（一時ディレクトリ変更）
     母子家庭等医療費請求書(宇多津町用)      ver4.8.0対応（一時ディレクトリ変更）
     シス管「3004」自動削除対応
     ruby1.9対応
   14/10/27 2.00

３７ 国保送り状                              システム管理マスタ「１９０１」の開設者の参照対応
     食事療養費送り状                        システム管理マスタ「１９０１」の開設者の参照対応
     国保請求書                              システム管理マスタ「１９０１」の開設者の参照対応
     後期高齢者送り状                        システム管理マスタ「１９０１」の開設者の参照対応
     後期高齢者請求書                        システム管理マスタ「１９０１」の開設者の参照対応
     医療費支給申請書(一般用)                システム管理マスタ「１９０１」の開設者の参照対応
     坂出市医療費請求書・入院用              システム管理マスタ「１９０１」の開設者の参照対応
     乳幼児医療費請求書(宇多津町用)          システム管理マスタ「１９０１」の開設者の参照対応
     重度心身障害者医療費請求書(宇多津町用)  システム管理マスタ「１９０１」の開設者の参照対応
     母子家庭等医療費請求書(宇多津町用)      システム管理マスタ「１９０１」の開設者の参照対応
     プログラムオプション更新対応
     プログラムオプショングループ診療対応
     システム管理マスタ「３００４」の自動登録・削除機能のグループ診療対応
   16/02/04 2.01

３８ 日レセver5.0.0対応
   16/11/30 3.00

３９ 医療費支給申請書(一般用)                対象公費はPGオプションから取得する
     坂出市医療費請求書・入院用              医療機関コード変更時、医療機関コード別の作成処理対応
                                             対象公費はPGオプションから取得する
     国保送り状                              医療機関コード変更時、医療機関コード別の作成処理対応
                                             公費番号まとめ情報の公費番号の追加
                                             返戻時、再印刷画面のタイトル修正
     後期高齢者送り状                        医療機関コード変更時、医療機関コード別の作成処理対応
                                             公費番号まとめ情報の公費番号の追加
     食事療養費送り状                        医療機関コード変更時、医療機関コード別の作成処理対応
                                             公費番号まとめ情報の公費番号の追加
                                             返戻時、再印刷画面のタイトル修正
     国保請求書                              医療機関コード変更時、医療機関コード別の作成処理対応
                                             公費番号まとめ情報の公費番号の追加
                                             返戻時、再印刷画面のタイトル修正
     後期高齢者請求書                        医療機関コード変更時、医療機関コード別の作成処理対応
                                             公費番号まとめ情報の公費番号の追加
     バッチログ画面対応
   17/07/10 3.01

４０ 日レセver5.1.0対応
   18/11/22 4.00

４１ 医療費支給申請書(一般用)                修正履歴コメント削除
                                             SITEKBN変更（動作に変更なし）
     坂出市医療費請求書・入院用              修正履歴コメント削除
                                             SITEKBN変更（動作に変更なし）
     乳幼児医療費請求書(宇多津町用)          修正履歴コメント削除
                                             SITEKBN変更（動作に変更なし）
     重度心身障害者医療費請求書(宇多津町用)  修正履歴コメント削除
                                             SITEKBN変更（動作に変更なし）
     母子家庭等医療費請求書(宇多津町用)      修正履歴コメント削除
                                             SITEKBN変更（動作に変更なし）
     国保送り状                              修正履歴コメント削除
                                             SITEKBN変更（動作に変更なし）
                                             保険番号情報の取得処理を修正
     後期高齢者送り状                        修正履歴コメント削除
                                             SITEKBN変更（動作に変更なし）
     食事療養費送り状                        修正履歴コメント削除
                                             SITEKBN変更（動作に変更なし）
                                             保険番号情報の取得処理を修正
     国保請求書                              修正履歴コメント削除
                                             SITEKBN変更（動作に変更なし）
                                             保険番号情報の取得処理を修正
     後期高齢者請求書                        修正履歴コメント削除
                                             SITEKBN変更（動作に変更なし）
   19/01/11 4.01

