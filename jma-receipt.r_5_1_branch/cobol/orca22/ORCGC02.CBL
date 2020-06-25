      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGC02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 病名登録
      *  コンポーネント名  : 病名登録（Ｃ０２）
      *  管理者            : 
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/04/10  保険組合せ削除区分＝１
      *                                     読み飛ばし
      *  01.00.02    MCC-多々納   01/04/18  終了時の更新確認等
      *  01.00.03    MCC-藤原     01/06/06  保険病名フラグの追加
      *  01.00.04    MCC-多々納   01/07/02  日付画面用チェックサブ追加
      *  01.02.01    MCC-藤原     01/08/27  画面修正による項目の追加  
      *  01.02.02    MCC-藤原     01/09/11  転記のチェック修正    
      *  01.02.03    MCC-藤原     01/09/11  選択番号の編集    
      *  01.02.04    MCC-藤原     01/09/12  項目編集時に開始日順に
      *                                     並び替えを行う    
      *  01.02.05    MCC-藤原     01/09/13  基本病名コードの編集追加    
      *  01.03.01    MCC-藤原     01/09/20  JOINの修正      
      *  01.03.02    MCC-藤原     01/10/11  接頭語のみのとき文字数を
      *                                     ゼロにする      
      *  01.03.03    MCC-藤原     01/10/17  疑いフラグ変更のとき
      *                                     病名編集フラグを空白にする
      *                                     転記区分の修正（４→８）       
      *  01.04.01    MCC-藤原     01/11/05  患者番号、診療科を入力項目
      *                                     に変更
      *                                     C01の削除
      *  01.04.02    MCC-藤原     01/11/13  病名コードを７桁に変更     
      *  01.05.01    MCC-藤原     01/11/21  ORCMCPSUBへ変更
      *  01.05.02    MCC-藤原     01/11/27  ボタン・日付の変更     
      *  01.06.01    MCC-藤原     01/12/06  自院病名マスタの追加     
      *  01.06.02    MCC-藤原     01/12/13  疾患区分短縮名の追加     
      *                                     修飾語コードの入力方法の変更    
      *  01.06.03    MCC-藤原     01/12/18  病名のみ入力のとき検索処理を行う     
      *                                     病名コード未入力のとき病名へ移動    
      *  01.06.04    MCC-藤原     02/01/08  病名入力コードの追加     
      *  01.06.05    MCC-藤原     02/01/31  保険組合せの表示の修正     
      *  01.06.06    MCC-藤原     02/02/05  最終受診科の表示を追加     
      *                                     単科のとき初期表示をして入力はしない
      *                                     ようにする     
      *  01.06.07    MCC-藤原     02/02/18  診療開始日と保険組合せの適用日の
      *                                     チェックの追加     
      *  01.06.08    MCC-藤原     02/02/20  慢性疾患の短縮名（０５）の
      *                                     短縮名の修正     
      *  01.06.09    MCC-藤原     02/03/05  転帰日付のチェック追加
      *  01.06.10    MCC-藤原     02/04/04  自院病名の読み込みのＫＥＹ変更
      *  01.06.11    MCC-藤原     02/04/05  自院病名検索画面の追加
      *  01.06.12    MCC-藤原     02/04/11  主病名フラグの重複チェック削除
      *                                     8002"の疑い"の病名コードのとき
      *                                     疑いフラグに１をセット
      *  01.06.13    MCC-藤原     02/05/07  病名コードが削除されるように修正 
      *  01.06.14    MCC-藤原     02/05/08  半角チェック追加
      *                                     転帰日と転帰区分のチェック追加 
      *  01.06.15    MCC-多々納   02/05/08  半角チェック時、半角空白を
      *                                     全角空白へ置換えの追加
      *  01.06.16    MCC-藤原     02/05/09  自院コードのとき病名・カルテ病名に
      *                                     入力できるように修正 
      *                                     同一病名があるときエラーにする
      *  01.06.17    MCC-多々納   02/06/05  患者氏名での検索を追加
      *  01.06.18    MCC-多々納   02/06/21  労災保険追加
      *  01.06.19    MCC-小豆沢   02/06/26  皮膚科特定疾患（３）をコンボから削除
      *  01.06.20    MCC-藤原     02/07/05  病名コード空白の時開始日に移動する
      *  01.06.21    NACL-多々納  02/07/19  患者検索エラーメッセージ修正
      *
      *  01.07.01    NACL-藤原    02/08/20  新傷病名マスタ対応
      *  01.07.02    NACL-藤原    02/08/28  労災のとき保険組合せの名称の修正
      *  01.07.03    NACL-藤原    02/09/17  自賠責とき保険組合せの名称の修正
      *  01.07.04    NACL-藤原    02/10/24  戻り先にｋ０２Ｎを追加
      *  01.07.05    NACL-藤原    02/11/19  編集病名のときの接頭語のチェック
      *                                     の修正
      *                                     病名の長さチェックの修正
      *  01.07.06    NACL-藤原    02/11/25  ＳＰＡパラメタファイルの
      *                                     端末ＩＤ変更
      *  01.07.07    NACL-藤原    02/12/13  病名文字数のチェック修正
      *  01.07.08    NACL-藤原    03/02/14  入力ＣＤ検索から戻ったときの
      *                                     カルテ病名の編集を追加
      *  01.07.09    NACL-藤原    03/02/28  診療科４０に変更
      *  01.07.10    NACL-藤原    03/03/05  排他制御処理追加
      *  01.07.11    NACL-多々納  03/03/14  ファイル削除追加
      *  01.07.12    NACL-藤原    03/03/14  同一開始日の病名の表示順の修正
      *  01.07.13    NACL-藤原    03/03/26  選択番号がエラーのときクリアする
      *  01.07.14    NACL-藤原    03/04/15  診療科から病名を編集しておく
      *  01.07.15    NACL-藤原    03/05/28  変更・削除処理後の選択の解除
      *  01.07.16    NACL-藤原    03/06/03  未コード化傷病名コードのとき
      *                                     病名よりコード検索をする
      *                                     病名からのコード検索方法の修正
      *  01.07.17    NACL-藤原    03/07/03  主病名編集フラグにより主病名
      *                                     のチェックを行う
      *  01.07.18    NACL-藤原    03/07/07  病名コード入力時、病名が変更
      *                                     なければ疑い・慢性区分の変更
      *                                     をしないように修正
      *  01.07.19    NACL-藤原    03/07/15  更新時の入力コードのチェック
      *                                     チェックを追加
      *  01.07.20    NACL-藤原    03/07/20  接頭語＋入力コードの入力時の
      *                                     表示の修正
      *  01.07.21    NACL-藤原    03/08/27  病名から検索したコードがエラー
      *                                     のときの確認画面を表示
      *  01.07.22    NACL-藤原    03/08/28  病名から検索したコードが廃止病名
      *                                     のときの処理を修正
      *  01.07.23    NACL-藤原    03/09/24  転帰日が入力されている病名の
      *                                     同一病名チェックの修正
      *
      *  新患者病名マスタ対応
      *  01.08.01    NACL-藤原    03/08/28  登録数の制限をなくす
      *  01.08.02    NACL-藤原    04/02/24  入外区分の表示切替の機能追加
      *                                     病名文字数のエラー時の表示
      *  01.08.03    NACL-藤原    04/03/08  コラムリストの行制御
      *  01.08.04    NACL-藤原    04/03/09  手術歴の追加
      *  01.08.05    NACL-藤原    04/03/25  レセ電エラーフラグの判定に
      *                                     廃止年月日を追加
      *  01.08.06    NACL-藤原    04/03/29  主病名の確認画面後の更新の修正
      *  01.08.07    NACL-藤原    04/03/30  開始日と保険組合せのチェックの修正
      *  01.08.08    NACL-藤原    04/04/05  ＣＤ検索からの病名コードの
      *                                     編集の修正
      *  01.08.09    NACL-藤原    04/04/13  修飾語コードのみ検索時の病名
      *                                     編集フラグのクリア
      *                                     種別が部位の自院コード入力のとき
      *                                     の病名編集フラグの設定を修正
      *  01.08.10    NACL-藤原    04/04/20  ２１の修飾語コード＋病名コード
      *                                     入力時の病名編集フラグの設定
      *  01.08.11    NACL-藤原    04/05/14  簡単修正時の開始日と
      *                                     保険組合せのチェックの修正
      *  01.08.12    NACL-藤原    04/06/21  アフターケア日の表示修正
      *  01.08.13    NACL-藤原    04/08/19  開始日の未来日を入力したとき
      *                                     警告を表示
      *                                     まとめ入力時の番号の整数以外
      *                                     のチェックを追加
      *  01.08.14    NACL-藤原    04/10/20  自院コード入力時の廃止年月日
      *                                     の警告を追加
      *  01.08.15    NACL-藤原    04/11/15  主病名の確認メッセージの表示
      *                                     をシス管の設定より行う
      *  00.08.16    NACL-藤原    04/11/01  病名入力時修飾語コードのみで
      *                                     構成されていても病名とする
      *  01.08.17    NACL-藤原    05/02/10  訂正日指定対応
      *  ver.2.6.0
      *  02.06.01    NACL-藤原    05/04/20  移行病名警告メッセージの表示、
      *                                     廃止年月日警告メッセージ表示、
      *                                     診療科、表示対象年月の初期表示
      *                                     をシス管の設定より行う
      *  02.06.02    NACL-藤原    05/05/14  同一病名のチェックを修正
      *                                     病名複写機能対応
      *  02.06.03    NACL-藤原    05/07/29  簡単修正の番号指定を「.」も
      *                                     可能とする
      *  02.06.04    NACL-藤原    05/08/25  労災・自賠責の保険限定の場合の
      *                                     同一診療科の同一病名を登録可能
      *                                     とする
      *
      *  02.07.01    NACL-藤原    05/10/31  MONFUNC対応
      *  02.07.02    NACL-藤原    06/01/16  年齢表示を追加
      *  02.07.03    NACL-藤原    06/01/25  疑いフラグからの病名の編集方
      *                                     法の変更
      *  02.07.04    NACL-藤原    06/01/26  保険組合せを増やす
      *
      *  02.08.01    NACL-藤原    06/03/02  「０入院・入院外」指定で診療
      *                                      科指定で削除処理した後の一覧
      *                                      表示が消えるのを修正
      *  02.08.02    NACL-藤原    06/02/01   自院病名コード検索追加
      *  02.08.03    NACL-藤原    06/03/22   改正対応
      *                                      疾患区分の表示を変更
      *  02.08.04    NACL-藤原    06/03/30   旧総合病院のときの診療科の表示
      *  02.08.05    NACL-藤原    06/04/01   自院病名検索時のチェックの修正
      *                                      登録時の変更時の複数病名のチ
      *                                      ェックの追加
      *  02.08.05    NACL-藤原    06/04/04   自院病名検索時の病名種別のセット
      *
      *  03.00.01    NACL-多々納  06/05/16   コメント入力画面変更
      *  03.00.02    NACL-藤原    06/06/19   C06,C07,C08 削除
      *
      *  03.01.01    NACL-藤原    06/06/01  病名入力コードのアルファベッ
      *                                     ト７文字対応
      *  03.01.02    NACL-藤原    06/07/06  自院コード入力時の移行病名の
      *                                     特定疾患の表示をする
      *                                     自院コード検索より戻ったときの
      *                                     移行病名の特定疾患の表示をする
      *  03.01.03    NACL-藤原    06/07/21  保険組合せの名称の編集方法の
      *                                     修正
      *
      *  03.02.01    NACL-藤原    06/07/25  自費による保険限定の病名の重複
      *                                     登録を可能とする
      *  03.02.02    NACL-藤原    06/08/04  自院検索で移行病名がないときの
      *                                     疾患区分の表示方法の修正
      *  03.02.03    NACL-藤原    06/08/16  自院コードのみのときの自院検索
      *                                     遷移がエラーにならないようにする
      *
      *  03.04.01    NACL-藤原    06/11/01  病名一括削除機能対応
      *  03.04.02    NACL-藤原    06/11/27  ＳＰＡデータの削除
      *                                     入外区分、処理区分変更時の入力
      *                                     項目のクリア
      *  03.04.03    NACL-藤原    06/12/01  月途中保険変更時の病名対応
      *  03.04.04    NACL-藤原    07/03/02  月途中保険変更時の病名対応中止
      *  03.04.05    NACL-藤原    07/04/03  カルテ病名の表示の修正
      *  03.04.06    NACL-藤原    07/05/14  カナチェック後の編集の修正
      *  03.04.07    NACL-藤原    07/06/21  処理区分、入外区分変更時の
      *                                     診療科の表示
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/25  複数病名登録時のレセプト表示期間対応
      *  03.05.03    NACL-藤原    07/10/12  ファンクションキー操作による
      *                                     処理区分、入外区分変更時の
      *                                     診療科の表示
      *
      *  04.01.01    NACL-藤原    07/10/11  診療科コピー（移動）機能対応
      *  04.01.02    NACL-藤原    07/10/26  公務災害対応
      *  04.01.03    NACL-藤原    08/02/06  登録年月日の更新時の編集追加
      *  04.01.04    NACL-藤原    08/02/13  診療科チェックの修正
      *
      *  04.02.01    NACL-藤原    08/03/03  特定疾患区分07,08の追加
      *  04.02.02    NACL-藤原    08/05/07  更新時の診療科未入力エラー
      *
      *  04.03.01    NACL-藤原    08/06/11  前回来院日の表示
      *                                     慢性と急性のチェック
      *
      *  04.04.01    NACL-藤原    09/02/16  データチェック後の病名修正対応
      *  04.04.02    NACL-藤原    09/02/16  未コード化傷病名コード入力で
      *                                     病名が変わらないときもコード
      *                                     変換処理を行なうようにする
      *                                     入力のコードクリアも行なう
      *  04.04.03    NACL-藤原    09/03/23  外来まとめからの遷移対応
      *  04.04.04    NACL-藤原    09/10/16  保険組合せの名称の編集方法の変更
      *
      *  04.05.01    NACL-藤原    09/05/27  複数病名登録時の保険組み合わせ対応
      *  04.05.02    NACL-藤原    09/05/28  照会画面からの遷移対応
      *  04.05.03    NACL-藤原    09/06/01  主病名の存在チェックの改善対応
      *                                     システム日付より前に転帰となった
      *                                     病名は主病名としないようにする
      *  04.05.04    NACL-藤原    09/06/01  排他制御時の表示の変更
      *  04.05.05    NACL-藤原    09/06/02  病名の入力順対応
      *  04.05.06    NACL-藤原    09/07/22  照会画面からの遷移対応
      *                                     渡された病名を表示
      *  04.05.07    NACL-藤原    09/11/11  病名の入力順対応を中止
      *  04.05.08    NACL-藤原    11/02/15  誕生日と診療開始日のチェック
      *  04.05.09    NACL-藤原    11/11/14  公害による保険限定の病名の重
      *                                     複登録を可能とする
      *  04.05.10    NACL-藤原    11/12/05  上部の診療科未入力時の診療科表示
      *  04.05.11    NACL-藤原    12/01/13  患者番号未入力時の病名一覧の
      *                                     表示を行わないようにする
      *
      *  04.06.01    NACL-藤原    10/04/28  自院病名に不明な病名コードが
      *                                     登録された場合のチェックを追加
      *  04.06.02    NACL-藤原    10/06/16  疑いフラグ設定対応
      *  04.06.03    NACL-藤原    10/06/20  病名の入力順対応
      *  04.06.04    NACL-藤原    10/08/31  年月省略入力対応
      *  04.06.05    NACL-藤原    10/10/20  難病外来指導管理料対応
      *  04.06.06    NACL-藤原    10/10/26  同義語変換対応
      *  04.06.07    NACL-藤原    10/11/04  補足コメント対応
      *  04.06.08    NACL-藤原    10/12/17  補足コメントの入力があるとき
      *                                     単独使用禁止エラーとしない
      *  04.06.09    NACL-伊藤    11/01/28  リアルタイム送信
      *  04.06.10    NACL-藤原    11/09/02  自院病名で登録した疾患区分を
      *                                     優先する
      *  04.06.11    NACL-藤原    11/10/11  同義語変換時の編集病名フラグ
      *                                     の設定
      *  04.06.12    NACL-藤原    11/12/19  自院病名検索時のチェックと
      *                                     自院病名検索から戻り時の表示の修正
      *  04.06.13    NACL-藤原    12/02/29  補足コメントの登録前チェック追加
      *  04.06.14    NACL-藤原    12/03/22  補足コメントの登録前チェック
      *                                     を複数病名登録時には行わない
      *  04.06.15    NACL-藤原    12/07/04  表示期間によるレセプト記載対応
      *  04.06.16    NACL-藤原    12/10/01  入力病名の入力ＣＤ登録対応
      *  04.06.17    NACL-藤原    12/12/20  自院病名で登録した疾患区分を
      *                                     優先にする
      *  04.06.18    NACL-藤原    13/03/06  同義語変換時の編集病名フラグ
      *                                     の初期化
      *  04.06.19    NACL-藤原    13/03/15  簡単修正時の対象行の設定修正
      *  04.06.20    NACL-藤原    13/05/29  レセ電エラーフラグの判定に
      *                                     主病名フラグを使用しない
      *
      *  04.07.01    NACL-藤原    11/06/10  未コード化病名となる病名の別
      *                                     組み立てへの移行を可能にする
      *  04.07.02    NACL-太田    11/09/15  データチェック業務からの病名
      *                                     引継ぎ対応
      *  04.07.03    NACL-太田    12/02/20  前画面戻り時SPA-PTIDを初期化する
      *  04.07.04    NACL-藤原    13/05/13  病名コード未入力時の病名表示の修正
      *  04.07.05    NACL-藤原    13/06/01  病名コード未入力時のカーソル
      *                                     位置の修正
      *  04.07.06    NACL-太田    13/06/11  データチェック業務からの補足
      *                                     コメント引継ぎ対応
      *  04.07.07    NACL-藤原    13/07/30  自院病名のセット登録対応
      *  04.07.08    NACL-藤原    13/07/12  第三者行為対応
      *  04.07.09    NACL-藤原    13/11/05  補足コメントのコード検索戻り
      *                                     時の病名欄設定の変更
      *  04.07.10    NACL-藤原    13/10/11  更新時のopid 設定
      *  04.07.11    NACL-藤原    13/11/15  病名入力時のコード置換え画面
      *                                     の表示
      *  04.07.12    NACL-藤原    13/12/26  病名入力時のコード置換え画面
      *                                     の表示時の移行先のチェック
      *  04.07.13    NACL-藤原    14/01/29  病名入力時のコード置換え画面
      *                                     の表示時のチェック追加
      *                                     病名コード削除時の修正 
      *  04.07.14    NACL-藤原    14/07/14  第三者行為の重複チェック修正
      *  04.07.15    NACL-藤原    15/04/15  自院病名で登録した疾患区分と
      *                                     同じときは難病の設定を行う
      *  04.07.16    NACL-藤原    15/04/24  疾患区分を外した自院病名入力
      *                                     時の疾患区分の表示の修正
      *  04.07.17    NACL-藤原    15/06/19  診療科表示件数の変更
      *  04.07.02    NACL-藤原    15/07/29  患者病名登録時の登録時間記録
      *                                     と画面表示対応
      *                                     登録時間を追加
      *
      *  04.08.01    NACL-藤原    13/02/05  編集病名の強調表示
      *  04.08.02    NACL-藤原    15/11/02  簡単修正時の死亡区分の更新
      *  04.08.03    NACL-藤原    16/01/12  削除された保険組合せ、第三者
      *                                     行為のコラムリストへの表示
      *  04.08.04    NACL-藤原    16/01/20  転帰区分の死亡変更時の患者
      *                                     情報更新対応
      *  04.08.05    NACL-藤原    16/02/10  自院病名への補足コメント追加
      *                                     対応
      *  04.08.06    NACL-藤原    16/02/17  退院証明書の傷病名欄記載対応
      *                                     合併症に係わる情報追加対応
      *  04.08.07    NACL-藤原    16/02/17  一括入力対応（病名コード除く）
      *  04.08.08    NACL-藤原    16/03/17  セット入力時のエラー表示修正
      *  04.08.09    NACL-藤原    16/06/14  自院コード検索より戻ったときの
      *                                     移行病名の疑いフラグの表示をする
      *  04.08.10    NACL-藤原    16/11/16  病名入力時の置換病名コードへ
      *                                     変更した時の疾患区分の表示
      *  04.08.11    NACL-藤原    17/04/04  自院病名入力時の補足コメント
      *                                     の表示を行う
      *  04.08.12    NACL-藤原    17/07/10  病名コード未入力時の補足コメント
      *                                     のクリア
      *  04.08.13    NACL-藤原    17/07/25  自院コード検索より戻った時の
      *                                     疾患区分等の表示をする
      *  04.08.14    NACL-藤原    17/10/05  原疾患区分が未入力の時
      *                                     合併症区分の登録を可能とする
      *
      *  05.00.01    NACL-藤原    17/10/05  開始日の未来日入力の警告表示
      *                                     の修正
      *  05.00.02    NACL-藤原    18/01/05  登録時の補足コメント半角入力
      *                                     のチェック追加
      *  05.00.03    NACL-藤原    18/06/21  診療科のチェックの修正
      *  05.00.04    NACL-藤原    18/11/22  Warning 対応
      *  05.00.05    NACL-藤原    19/02/13  保険組合せを最大２００に変更
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    病名登録用共通パラメタ
           COPY    "C01COMMON-SPA".
      *
      *    病名登録用画面ＳＰＡ
           COPY    "C02SCR-SPA".
           02  SPA-PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING  //PTBYO-//
                                   BY         //SPA-PTBYO-//.
           02  WRK-HAISIYMD-OCC.
               03  WRK-HAISIYMD-O  PIC 9(08)   OCCURS  6.
           02  WRK-PTBYOMEI-REC    OCCURS  6.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING   //PTBYO-//
                                   BY          //WRK-PTBYO-//.
      *
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-USERBYOMEI      PIC 9(01).
           03  FLG-BYOMEI          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-SYNONYM         PIC 9(01).
           03  FLG-BYOMEIINPUTSET  PIC 9(01).
      *
           03  FLG-ROW             PIC 9(01).
           03  FLG-MANSEI          PIC 9(01).
           03  FLG-KYUSEI          PIC 9(01).
           03  FLG-SYUBYO          PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-CHANGE          PIC 9(01).
           03  FLG-USERBYOMEI-CHK  PIC 9(01).
           03  FLG-BYOMEIINPUTSET-CHK
                                   PIC 9(01).
           03  FLG-TENKIKBN-CHK    PIC 9(01).
           03  FLG-TENKIKBN        PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-BYOMEICD-CHK    PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *     
           03  IDZ                 PIC 9(04).
           03  IDX1                PIC 9(04). 
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
      *
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
      *
           03  IDY1                PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDVV                PIC 9(04).
           03  IDWW                PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDXY                PIC 9(04).
           03  IDYY                PIC 9(04).
           03  IDZZ                PIC 9(04).
           03  IDW1                PIC 9(04).
           03  IDW2                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MOTOPG          PIC X(08).
           03  WRK-SELNO           PIC 9(03). 
           03  WRK-RENNUM          PIC 9(02).
           03  WRK-GMN-TBL         PIC X(1000).
           03  WRK-NAI-TBL         PIC X(1000). 
           03  WRK-WIDGET.
               05  WRK-WIDGET1     PIC X(09).
               05  WRK-WIDGET2     PIC 9(01).  
      *****03  WRK-YMD             PIC X(09).
           03  WRK-YMD             PIC X(10).
           03  WRK-YMDMAX          PIC 9(01). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *         
           03  WRK-BYOMEI-AREA.
               05  WRK-BYOMEI      PIC X(100).
               05  WRK-CHARTBYOMEI PIC X(100).
               05  WRK-RECECD-TBL.
                   07  WRK-RECECD  OCCURS  3
                                   PIC X(01).
               05  WRK-MOJISU      PIC 9(03).
               05  WRK-BYOMEI1     PIC X(800).
      *
           03  WRK-HKNCOMBIMEI     PIC X(100).
           03  WRK-HKNKBN-MEI      PIC X(02).
           03  WRK-ZZ              PIC ZZZZZ.
           03  WRK-YMD-9.
               05  WRK-YY          PIC 9(04).
               05  WRK-MD          PIC 9(04).
               05  WRK-MD-R        REDEFINES   WRK-MD. 
                   07  WRK-MM      PIC 9(02).
                   07  WRK-DD      PIC 9(02). 
           03  WRK-YMD2            PIC 9(08).
           03  WRK-HENFLG          PIC 9(01).
           03  WRK-NUM             PIC ZZZZ9.
           03  WRK-CHKFLG          PIC X(01).
           03  WRK-HAISIYMDH       PIC X(22).
           03  WRK-BANGO           PIC S9(05).
           03  WRK-YM              PIC 9(06).
           03  WRK-SYSYMD          PIC 9(08).
           03  WRK-BUNRUICD        PIC 9(03).
           03  WRK-GMN-BANGO       PIC ZZZZZ.
           03  WRK-BYOMEIHENFLG    PIC X(01).
      *
           03  WRK-KANTAN-HENSYU-AREA.
               05  WRK-HENSYU-NUM  PIC X(40).
               05  WRK-HENSYU-NUM-G    OCCURS  5.
                   07  WRK-HENSYU-NUM-O
                                   PIC X(10).
                   07  WRK-STNUM-O PIC 9(05).
                   07  WRK-EDNUM-O PIC 9(05).
               05  WRK-STNUM       PIC X(05).
               05  WRK-EDNUM       PIC X(05).
               05  WRK-HENSYU-DATA-G.
                   07  WRK-HENSYU-DATA-O
                                   PIC X(10)   OCCURS  9.
               05  WRK-HENSYU-DATA-9
                                   PIC 9(10).
      *
           03  WRK-TEKSTYMD        PIC 9(08).
           03  WRK-TEKEDYMD        PIC 9(08).
           03  WRK-RYOSTYMD        PIC 9(08).
           03  WRK-RYOEDYMD        PIC 9(08).
      *
           03  WRK-TENKIYMD        PIC X(08).
      *
           03  WRK-ERR-BYOMEI      PIC X(80).
           03  WRK-ERR-SRYYMD      PIC X(22).
           03  WRK-ERR-SRYKA       PIC X(20).
      *
      *    傷病名編集フラグ（疑い）
           03  WRK-HENFLG1         PIC X(01).
      *    傷病名編集フラグ（急性）
           03  WRK-HENFLG2         PIC X(01). 
      *
      *
           03  WRK-KBN-HENSYU-AREA.
               05  WRK-KBN         PIC X(04).
               05  WRK-KBNMEI      PIC X(42).
               05  WRK-KBNMEITAN   PIC X(10).                            
               05  WRK-KOHID       PIC 9(10).
      *     
           03  WRK-TANDOKU-AREA.
               05  WRK-TANDOKUKBN-BYOMEICD
                                   PIC X(07).
               05  WRK-TANDOKUKBN-BYOMEI
                                   PIC X(80).
               05  WRK-TANDOKUKBN  PIC 9(01).
      *     
           03  WRK-BYOMEICD        PIC X(07).
           03  WRK-HOSOKUCD        PIC X(12).
      *                             
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-BYOMEI-OLD      PIC X(480).
      * 
           03  WRK-SEIGYO          PIC X(01).
      *
           03  WRK-C02-BYO-ROW     PIC S9(09).
      *
           03  WRK-SRYYMD          PIC X(08).
           03  WRK-ST              PIC 9(03).
      *
           03  WRK-PTID            PIC X(10).
      *
           03  WRK-BYOMEICD-ERRCD  PIC X(05).
           03  WRK-HOSOKUCD-ERRCD  PIC X(05).
      *
           03  WRK-CHK-SRYYMD      PIC X(08).
           03  WRK-CHK-TENKIYMD    PIC X(08).
      *
           03  WRK-ERRCD           PIC X(04).
      *
           03  WRK-BYOMEI-MAX1     PIC 9(02).
           03  WRK-BYOMEI-MAX2     PIC 9(02).
           03  WRK-BYOMEI-INDEX    PIC 9(02).
      *
           03  WRK-LNK-BYO-INDEX   PIC 9(02).
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-GMN-C02-CUR     PIC 9(02).
      *
      *    退避エリア（C02SCR-SPAとあわせる）
           03  WRK-GMN-C02-BYOMEICD-G.
               05  WRK-GMN-C02-BYOMEICD    OCCURS  6
                                   PIC X(12).
           03  WRK-NAI-C02-BYOMEI-TBL.
               05  WRK-NAI-C02-BYOMEI-OCC  OCCURS  21.
                   07  WRK-NAI-C02-BYOMEICD
                                   PIC X(10).
                   07  WRK-NAI-C02-USERBYOMEIFLG
                                   PIC X(01).
                   07  WRK-NAI-C02-BYOMEITOP
                                   PIC X(01).
                   07  WRK-NAI-C02-BYOMEISBT
                                   PIC X(01).
                   07  WRK-NAI-C02-BYOMEI-CHG
                                   PIC X(03).
                   07  WRK-NAI-C02-BYOMEI-G.
                       09  WRK-NAI-C02-BYOMEI  
                                   PIC X(80).
                       09  WRK-NAI-C02-CHARTBYOMEI  
                                   PIC X(80).
                       09  WRK-NAI-C02-UTAGAIFLG
                                   PIC X(01).
                       09  WRK-NAI-C02-TOKSKNCD
                                   PIC 9(02).
                       09  WRK-NAI-C02-NANBYOCD-O
                                   PIC 9(02).
                       09  WRK-NAI-C02-HAISIYMD
                                   PIC 9(08).
                       09  WRK-NAI-C02-US-TOKSKNCD
                                   PIC 9(02).
                       09  WRK-NAI-C02-US-BYOMEIHENFLG
                                   PIC X(01).
                       09  WRK-NAI-C02-US-HKNBYOMEIFLG
                                   PIC X(01).
                       09  WRK-NAI-C02-US-HOSOKU-COMMENT
                                   PIC X(80).
                       09  WRK-NAI-C02-US-UTAGAIFLG
                                   PIC X(01).
                       09  WRK-NAI-C02-BYOMEICD-O
                                   PIC X(07)
                                   OCCURS  21.
                       09  WRK-NAI-C02-BYOMEI-O
                                   PIC X(80)
                                   OCCURS  21.
                       09  WRK-NAI-C02-TANDOKUKBN-O 
                                   PIC 9(02)
                                   OCCURS  21.
                       09  WRK-NAI-C02-HAISIYMD-O
                                   PIC 9(08)
                                   OCCURS  21.
      *
      *    年齢表示
           03  WRK-NENREI-G.
               05  WRK-NENREI-Z    PIC ZZ9.
               05  WRK-NENREI-MEI  PIC X(04).
           03  WRK-NENREI-GR       REDEFINES   WRK-NENREI-G.
               05  WRK-NENREI-ZN   PIC Z9.
               05  WRK-NENREI-MEIN PIC X(04).
               05  WRK-NENREI-FIL  PIC X(01).
      *
      *    行入替用ワーク（C02SCR-SPAとあわせる）
           03  WRK-GMN-C02-TBLO.
               05  WRK-GMN-C02-TBANGO      PIC 9(05).
               05  WRK-GMN-C02-TSRYKA      PIC X(04).
               05  WRK-GMN-C02-TSRYYMD     PIC X(09).
               05  WRK-GMN-C02-TBYOMEI     PIC X(130).
               05  WRK-GMN-C02-TUTAGAIFLG  PIC X(14).
               05  WRK-GMN-C02-TSYUBYOFLG  PIC X(12).
               05  WRK-GMN-C02-TMANSEIKBN  PIC X(42).
               05  WRK-GMN-C02-TNYUGAIKBN  PIC X(06).
               05  WRK-GMN-C02-THKNCOMBI   PIC X(40).
               05  WRK-GMN-C02-TKOHID      PIC X(22).
               05  WRK-GMN-C02-TREZEFLG    PIC X(10).
               05  WRK-GMN-C02-TREZEMM     PIC 9(02).
               05  WRK-GMN-C02-TTENKIKBN   PIC X(08).
               05  WRK-GMN-C02-TTENKIYMD   PIC X(09).
               05  WRK-GMN-C02-THKNBYOMEIFLG
                                           PIC X(08).
               05  WRK-GMN-C02-TDISCHARGEFLG
                                           PIC X(04).
               05  WRK-GMN-C02-TCLASS1     PIC X(08).
               05  WRK-GMN-C02-TCLASS2     PIC X(10).
               05  WRK-GMN-C02-TCREYMD     PIC X(09).
               05  WRK-GMN-C02-TCREHMS     PIC X(08).
               05  WRK-GMN-C02-TUPYMD      PIC X(09).
               05  WRK-GMN-C02-TUPHMS      PIC X(08).
           03  WRK-NAI-C02-TBLO.
               05  WRK-NAI-C02-TSRYYMD     PIC X(08).
               05  WRK-NAI-C02-TRENNUM     PIC 9(02).
               05  WRK-NAI-C02-TSEQNUM     PIC 9(05).
               05  WRK-NAI-C02-TSRYKA      PIC X(02).
               05  WRK-NAI-C02-TNYUGAIKBN  PIC X(01).
               05  WRK-NAI-C02-TTENKIYMD   PIC X(08).
               05  WRK-NAI-C02-THKNCOMBI   PIC X(04).
               05  WRK-NAI-C02-TKOHID      PIC 9(10).
               05  WRK-NAI-C02-TBYOMEI     PIC X(80).
               05  WRK-NAI-C02-THOSOKU-COMMENT
                                           PIC X(40).
               05  WRK-NAI-C02-TUPDOWN     PIC 9(01).
      *
      *    保険組合せ、第三者行為重複チェック用エリア
       01  WRK-CHK-HKNCOMBI-AREA.
           02  WRK-CHK-HKNCOMBI1   PIC 9(04).
           02  WRK-CHK-HKNCOMBI2   PIC 9(04).
      *
           02  WRK-CHK-KOHID1      PIC 9(10).
           02  WRK-CHK-KOHID2      PIC 9(10).
      *
           02  FLG-HKNCOMBI-DABURI PIC 9(01).
           02  WRK-CHK-HKNCOMBI-REC    OCCURS  2.
               COPY    "CPHKNCOMBI.INC"    REPLACING  //COMB-//
                                           BY         //WRK-CHK-COMB-//.
      *
      *    未コード化傷病名コード
       01  WRK-CONS-BYOMEICD       PIC X(07)   VALUE   "0000999". 
      *                                    
      *    自院病名検索画面ＭＡＸ行
       01  WRK-CONS-C04-MAX        PIC 9(03)   VALUE   100.
      *    病名登録画面ＭＡＸ行
       01  WRK-CONS-C02-MAX        PIC 9(03)   VALUE   200.
       01  WRK-CONS-C02-HKNCOMBI-MAX
                                   PIC 9(03)   VALUE   20.
      *
       01  WRK-CONS-BYOMEICD-MAX   PIC 9(02)   VALUE   21.
      *
      *    疑い編集
       01  WRK-UTAGAI-AREA.
      *    の疑い
           03  WRK-CONS-UTAGAI     PIC X(07)   VALUE   "ZZZ8002".                                     
           03  WRK-CONS-UTAGAI-MOJISU
                                   PIC 9(02)   VALUE   6.       
      *    急性
           03  WRK-CONS-KYUSEI-MOJISU
                                   PIC 9(02)   VALUE   4.
      *
       01  SPAN-COLOR-AREA.
           03  SPAN-COLOR-RED      PIC X(18)    VALUE
                                                "<span color=""red"">".  
           03  SPAN-COLOR-BLUE     PIC X(19)    VALUE
                                                "<span color=""blue"">".  
           03  SPAN-COLOR-GREEN    PIC X(20)    VALUE
                                               "<span color=""green"">".  
           03  SPAN-COLOR-END      PIC X(07)    VALUE
                                               "</span>".  
      *
       01  WRK-HEN-AREA.
           03  WRK-LENGTH          PIC 9(02).
           03  WRK-NENGO           PIC X(04).
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *    複写用患者病名マスタ−
       01  WRK-PTBYOMEI-COPY-REC.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING   //PTBYO-//
                                   BY          //WRK-PTBYO-COPY-//.
      *
      *    診療科移動用患者病名マスタ−
       01  WRK-PTBYOMEI-DELETE-REC.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING   //PTBYO-//
                                   BY          //WRK-PTBYO-DLT-//.
      *
      *    患者病名マスタ−（レセ電エラーフラグチェック用）
       01  WRK-PTBYOMEI-CHK-AREA.
           02  WRK-HAISIYMD-CHK    PIC 9(08).
           02  WRK-PTBYOMEI-CHK-REC.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING   //PTBYO-//
                                   BY          //WRK-PTBYO-CHK-//.
      *
      *    患者病名マスタ−（行指定追加処理用）
       01  WRK-PTBYOMEI-INSERT-AREA.
           02  WRK-PTBYOMEI-INSERT-REC         OCCURS  99.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING   //PTBYO-//
                                   BY          //WRK-PTBYO-INS-//.
      *
      *    患者病名マスタ−（開始日別表示用）
       01  WRK-LINE                PIC 9(03).
       01  WRK-PAGE                PIC 9(02).
       01  WRK-PTBYOMEI-SRYYMD-AREA.
           02  WRK-PTBYOMEI-SRYYMD-MAX         PIC 9(03).
      *
           02  WRK-PTBYO-SRY-BANGO             PIC 9(05)
                                               OCCURS  200.
           02  WRK-PTBYOMEI-SRYYMD-REC         OCCURS  200.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING   //PTBYO-//
                                   BY          //WRK-PTBYO-SRY-//.
      *
      *    スパ領域（ワーク）
           COPY    "COMMON-SPA"    REPLACING   //SPA-//
                                   BY          //WRK-SPA-//.
      *
      *
           03  WRK-CIDMSG          PIC X(120).
      *
      *    病名システム予約コードテーブル
           COPY    "CMBYOMEICD.INC".
      *
      *    D04画面からの遷移情報
           COPY    "CPLNKD04.INC".
      *
      *    感染症ファイル名
       01  INFECTION-FILE          PIC X(17)   VALUE
                                   "infectiondata2.sh".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSK2005.INC".
           COPY    "CPSK200501.INC"
                                   REPLACING  //SYS-200501//
                                   BY         //SYS-25//.
      *    病名機能情報
           COPY    "CPSK1042.INC".
      *    診療行為一括入金管理情報
           COPY    "CPSK1038.INC".
      *
      *    病名マスタ−
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    自院病名マスタ−
       01  USERBYOMEI-REC.
           COPY    "CPUSERBYOMEI.INC".
      *
      *    患者病名マスタ−
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    受診履歴マスタ−
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    病名入力セット
       01  BYOMEIINPUTSET-REC.
           COPY    "CPBYOMEIINPUTSET.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    病名コード検索サブ
           COPY    "CPORCSBYOMEI.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    レセプト作成管理マスタ更新サブ
           COPY    "CPORCSRECEUPD.INC".
      *
      *    病名組立サブ
           COPY    "CPORCSMKBYOMEI.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSHKNSET.INC".
      *
      *    前回来院日設定パラメタ
           COPY    "CPORCSLASTYMD.INC".
      *
      *    同義語マスタ
       01  SYNONYM-REC.
           COPY    "CPSYNONYM.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    for Infection
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    MCPAREA.
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA22SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-C02FREE
           MOVE    SPA-WORK        TO  SPA-C01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-C01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-C02FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "CERR"
               MOVE    SPACE               TO  SPA-MOTOPG
      *
               INITIALIZE                  SPA-CIDCD-AREA
                                           SPA-CID1-AREA
               INITIALIZE                  SPA-C02-IKOSAKI-AREA
                                           SPA-C02-CHANGE-AREA
                                           SPA-C02-HAISI-AREA
                                           SPA-ERRCD
                                           WRK-HAISIYMDH                                           
      *
               IF      SPA-C02-BYOSET-INDEX-MAX
                                       >   ZERO
      *            病名入力セットから自院病名編集処理
                   PERFORM 410261-BYOMEIINPUTSET-HEN-SEC
                   IF    ( SPA-CIDCD2      NOT =   SPACE )
                   OR    ( SPA-CIDCD6      NOT =   SPACE )
                   OR    ( SPA-ERRCD       NOT =   SPACE )
                       CONTINUE
                   ELSE
                       ADD     1               TO  SPA-GMN-C02-CUR
                       IF      SPA-GMN-C02-CUR >   7
                           MOVE    9               TO  SPA-GMN-C02-CUR
                       END-IF
                   END-IF
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                       GO  TO  100-INIT-EXT
                   END-IF
                   IF      SPA-CIDCD2      NOT =   SPACE
                       PERFORM 530-CIDSET2-SEC
                       GO  TO  100-INIT-EXT
                   END-IF
                   IF      SPA-CIDCD6      NOT =   SPACE
                       PERFORM 560-CIDSET6-SEC
                       GO  TO  100-INIT-EXT
                   END-IF
               END-IF
      *
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO      TO      100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
               IF      SPA-CIDCD       NOT =   SPACE
                   EVALUATE    SPA-CIDCD
                       WHEN    "0102"
                       WHEN    "0103"
                       WHEN    "0104"
                       WHEN    "0106"
                           PERFORM 520-CIDSET-SEC
                           GO  TO  100-INIT-EXT
                   END-EVALUATE
               END-IF
               IF      SPA-CIDCD2      NOT =   SPACE
                   PERFORM 530-CIDSET2-SEC
                   GO  TO  100-INIT-EXT
               END-IF
               IF      SPA-CIDCD6      NOT =   SPACE
                   PERFORM 560-CIDSET6-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "C02    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
      *        元の画面へ戻す
               IF      WRK-MOTOPG(1:3)     =   "Y01"  OR  "U01"  OR
                                               "P97"  
                                           OR  "C50"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-C02
                   MOVE    SPA-WORK        TO  SPA-C01KYOUTU
               END-IF
      *        予約とエラー表示の時、そのまま画面表示へ
               IF      WRK-MOTOPG(1:3)     =   "Y01" 
                   GO      TO      300-SCREEN-EXT
               END-IF
      *        コメントから
               IF      WRK-MOTOPG(1:3)     =   "C50"
                   GO      TO      300-SCREEN-EXT
               END-IF
      *        外部より選択がある時
               IF      WRK-MOTOPG(1:3)     =   "U01"  OR
                                               "P97"
                   PERFORM 3101-U01SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
      *
               IF      WRK-MOTOPG          =   "M01"  OR  "U02"  OR
                                               "K02"  OR  "K02N" OR
                                               "D04"  OR  "KT01" OR
                                               "Q02"
                   MOVE    SPA-MOTOPG          TO  SPA-C02-MOTOPG
               END-IF
      *                      
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
      *
               INITIALIZE              SPA-C01KYOUTU
      *
               IF    ( WRK-MOTOPG          =   "D04" )
                   MOVE    SPACE           TO  LINKAREA
               END-IF
      *
           END-IF
      *
           EVALUATE    SPA-MOTOPG(1:3)
               WHEN    "C03"
      *            自院病名セット処理
                   PERFORM 320-C03SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "C06"
               WHEN    "C09"
               WHEN    "C11"
               WHEN    "C12"
                   GO      TO      300-SCREEN-EXT
               WHEN    "C04"
      *            自院病名セット処理
                   PERFORM 320-C04SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "C05"
      *            病名セット処理
                   PERFORM 320-C05SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "C10"
      *            簡単修正処理
                   PERFORM 320-C10SPASET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
      *    確認画面より
           IF      SPA-MOTOPG(1:4)     =   "CID1"
               IF      SPA-CIDCD       =   "0101"
                   MOVE    SPACE           TO  SPA-CIDCD
                   IF      SPA-CID1-FLG        =   "OK"
                       MOVE    SPACE               TO  SPA-C02-UPDFLG
                       MOVE    "JOIN"              TO  MCP-PUTTYPE
                       PERFORM 210-BACK
                   ELSE
                       MOVE    "1"                 TO  SPA-C02-UPDFLG
                       MOVE    1                   TO  SPA-GMN-C02-CUR
                   END-IF
                   INITIALIZE                  SPA-CIDCD-AREA
                                               SPA-CID1-AREA  
                   GO      TO      300-SCREEN-EXT
               END-IF
      *
               IF      SPA-CIDCD       =   "0102"
                   IF      SPA-CID1-FLG    =   "OK"
                       PERFORM 4502-TOUROKU-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-C02-CUR
                   END-IF
                   INITIALIZE              SPA-CIDCD-AREA
                                           SPA-CID1-AREA  
                   INITIALIZE              SPA-C02-KEIKOKU-FLG-AREA
                                           SPA-C02-KEIKOKU-MSG-AREA
                   GO      TO      300-SCREEN-EXT
               END-IF
      *
               IF      SPA-CIDCD       =   "0103"
                   IF      SPA-CID1-FLG    =   "OK"
                       IF      SPA-C02-KEIKOKU-FLG3
                                               =   1
                           MOVE    SPACE               TO  SPA-CID1-FLG
                           MOVE    "0104"              TO  SPA-CIDCD
                           GO      TO      300-SCREEN-EXT
                       ELSE 
                           IF      SPA-C02-KEIKOKU-FLG2
                                                   =   1
                               MOVE    SPACE           TO  SPA-CID1-FLG
                               MOVE    "0102"          TO  SPA-CIDCD
                               GO      TO      300-SCREEN-EXT
                           ELSE 
                               PERFORM 4502-TOUROKU-SYORI-SEC
                           END-IF
                       END-IF
                   ELSE
                       MOVE    1               TO  SPA-GMN-C02-CUR
                   END-IF
                   INITIALIZE                  SPA-CIDCD-AREA
                                               SPA-CID1-AREA  
                   INITIALIZE              SPA-C02-KEIKOKU-FLG-AREA
                                           SPA-C02-KEIKOKU-MSG-AREA
                   GO      TO      300-SCREEN-EXT
               END-IF
      *
               IF      SPA-CIDCD       =   "0104"
                   IF      SPA-CID1-FLG    =   "OK"
                       IF      SPA-C02-KEIKOKU-FLG2
                                               =   1
                           MOVE    SPACE               TO  SPA-CID1-FLG
                           MOVE    "0102"              TO  SPA-CIDCD
                           GO      TO      300-SCREEN-EXT
                       ELSE 
                           PERFORM 4502-TOUROKU-SYORI-SEC
                       END-IF
                   ELSE
                       MOVE    1               TO  SPA-GMN-C02-CUR
                   END-IF
                   INITIALIZE              SPA-CIDCD-AREA
                                           SPA-CID1-AREA  
                   INITIALIZE              SPA-C02-KEIKOKU-FLG-AREA
                                           SPA-C02-KEIKOKU-MSG-AREA
                   GO      TO      300-SCREEN-EXT
               END-IF
      *
               IF      SPA-CIDCD       =   "0105"  OR  "0108"
                   IF      SPA-CID1-FLG    =   "OK"
                       IF      SPA-C02-KEIKOKU-FLG1
                                               =   1
                           MOVE    SPACE           TO  SPA-CID1-FLG
                           MOVE    "0103"          TO  SPA-CIDCD
                           GO  TO  300-SCREEN-EXT
                       ELSE 
                           IF      SPA-C02-KEIKOKU-FLG3
                                                   =   1
                               MOVE    SPACE           TO  SPA-CID1-FLG
                               MOVE    "0104"          TO  SPA-CIDCD
                               GO  TO  300-SCREEN-EXT
                           ELSE 
                               IF      SPA-C02-KEIKOKU-FLG2
                                                       =   1
                                   MOVE    SPACE       TO  SPA-CID1-FLG
                                   MOVE    "0102"      TO  SPA-CIDCD
                                   GO  TO  300-SCREEN-EXT
                               ELSE 
                                   PERFORM 4502-TOUROKU-SYORI-SEC
                               END-IF
                           END-IF
                       END-IF
                   ELSE
                       MOVE    1               TO  SPA-GMN-C02-CUR
                   END-IF
                   INITIALIZE                  SPA-CIDCD-AREA
                                               SPA-CID1-AREA  
                   INITIALIZE              SPA-C02-KEIKOKU-FLG-AREA
                                           SPA-C02-KEIKOKU-MSG-AREA
                   GO      TO      300-SCREEN-EXT
               END-IF
      *
               IF      SPA-CIDCD       =   "0106"
                   IF      SPA-CID1-FLG    =   "OK"
                       INITIALIZE              WRK-KANTAN-HENSYU-AREA
                       PERFORM VARYING IDX     FROM    1   BY  1
                               UNTIL   IDX     >       7
                           MOVE    SPA-C01-HENSYU-DATA-O (IDX)
                                           TO  WRK-HENSYU-DATA-O (IDX)
                       END-PERFORM
                       PERFORM VARYING IDX     FROM    1   BY  1
                               UNTIL   IDX     >       5
                           MOVE    SPA-C01-STNUM-O (IDX)
                                                   TO  WRK-STNUM-O (IDX)
                           MOVE    SPA-C01-EDNUM-O (IDX)
                                                   TO  WRK-EDNUM-O (IDX)
                       END-PERFORM
      *                簡単修正時の更新処理
                       PERFORM 410113-KANTAN-KOUSHIN-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-C02-CUR
                   END-IF
                   INITIALIZE                  SPA-CIDCD-AREA
                                               SPA-CID1-AREA  
                   INITIALIZE              SPA-C02-KEIKOKU-FLG-AREA
                                           SPA-C02-KEIKOKU-MSG-AREA
                   INITIALIZE              SPA-C01-HENSYU-NUM-G
                                           SPA-C01-HENSYU-DATA-G
                   GO  TO  300-SCREEN-EXT
               END-IF
      *
               IF      SPA-CIDCD       =   "0107"  OR  "0109"
                   IF      SPA-CID1-FLG    =   "OK"
                       IF      SPA-C02-KEIKOKU-FLG1
                                               =   1
                           MOVE    SPACE           TO  SPA-CID1-FLG
                           MOVE    "0106"          TO  SPA-CIDCD
                           GO  TO  300-SCREEN-EXT
                       ELSE
                           INITIALIZE          WRK-KANTAN-HENSYU-AREA
                           PERFORM VARYING IDX     FROM    1   BY  1
                                   UNTIL   IDX     >       7
                               MOVE    SPA-C01-HENSYU-DATA-O (IDX)
                                           TO  WRK-HENSYU-DATA-O (IDX)
                           END-PERFORM
                           PERFORM VARYING IDX     FROM    1   BY  1
                                   UNTIL   IDX     >       5
                               MOVE    SPA-C01-STNUM-O (IDX)
                                                   TO  WRK-STNUM-O (IDX)
                               MOVE    SPA-C01-EDNUM-O (IDX)
                                                   TO  WRK-EDNUM-O (IDX)
                           END-PERFORM
      *                    簡単修正時の更新処理
                           PERFORM 410113-KANTAN-KOUSHIN-SEC
                       END-IF
                   ELSE
                       MOVE    1               TO  SPA-GMN-C02-CUR
                   END-IF
                   INITIALIZE                  SPA-CIDCD-AREA
                                               SPA-CID1-AREA  
                   INITIALIZE                  SPA-C02-KEIKOKU-FLG-AREA
                                               SPA-C02-KEIKOKU-MSG-AREA
                   INITIALIZE              SPA-C01-HENSYU-NUM-G
                                           SPA-C01-HENSYU-DATA-G
                   GO  TO  300-SCREEN-EXT
               END-IF
           END-IF
      *
      *    病名移行確認画面より
           IF      SPA-MOTOPG(1:4)     =   "CID2"
               MOVE    SPACE               TO  SPA-NAI-C02-BYOMEI-CHG
                                                 (SPA-C02-IKOSAKI-INDEX)
      *
               IF      SPA-CIDCD2          =   "0103"  OR  "0101"
                                           OR  "0102"  OR  "0105"
      *            病名コードの移行
                   IF      SPA-CID2-FLG    =   "OK"
                       MOVE    SPA-C02-IKOSAKI-INDEX
                                           TO  IDZ
                       MOVE    SPA-C02-IKOSAKI-BYOMEICD
                                           TO  SPA-NAI-C02-BYOMEICD(IDZ)
                       IF      SPA-CIDCD2      =   "0103"  OR  "0105"
                           MOVE    SPA-NAI-C02-BYOMEICD(IDZ)
                                           TO  SPA-GMN-C02-BYOMEICD(IDZ)
                       END-IF
                       MOVE    SPA-NAI-C02-BYOMEICD(IDZ)
                                           TO  BYO-BYOMEICD
                       PERFORM 410111-BYOMEI-HENSYU-SEC
                   END-IF
                   PERFORM 41021-BYOMEICD-HENSYU-SEC
                   IF      SPA-CIDCD2      =   "0101"  OR  "0102"
                       MOVE    34              TO  SPA-GMN-C02-CUR
                   END-IF
      *     
                   INITIALIZE                  SPA-CIDCD-AREA
                                               SPA-CID1-AREA
                   INITIALIZE                  SPA-C02-IKOSAKI-AREA
                                               SPA-C02-CHANGE-AREA
                                               SPA-C02-HAISI-AREA
                   GO      TO      300-SCREEN-EXT
               END-IF    
               IF      SPA-CIDCD2          =   "0104"  OR  "0106"
      *            自院病名コードの移行
                   IF      SPA-CID2-FLG    =   "OK"
                       MOVE    SPA-C02-IKOSAKI-INDEX
                                               TO  IDZ
                       MOVE    SPA-C02-IKOSAKI-INDEX1
                                               TO  IDX2
                       MOVE    SPA-CID2-FLG    TO  
                                       SPA-NAI-C02-BYOMEI-CHG (IDZ)
                       INITIALIZE          SPA-NAI-C02-BYOMEI (IDZ)
                                           SPA-NAI-C02-US-BYOMEIHENFLG
                                                              (IDZ)
                                           SPA-NAI-C02-BYOMEICD-O
                                                             (IDZ IDX2)
                                           SPA-NAI-C02-BYOMEI-O
                                                             (IDZ IDX2)
                                           SPA-NAI-C02-HAISIYMD-O
                                                             (IDZ IDX2)
                                           SPA-NAI-C02-TANDOKUKBN-O
                                                             (IDZ IDX2)
      *
                       MOVE    SPA-C02-IKOSAKI-BYOMEICD
                                               TO
                                       SPA-NAI-C02-BYOMEICD-O(IDZ IDX2)
                                       BYO-BYOMEICD
                       PERFORM 900-BYOMEI-READ-SEC
                       IF      FLG-BYOMEI      =   ZERO
                           MOVE    BYO-BYOMEI      TO
                                       SPA-NAI-C02-BYOMEI-O (IDZ IDX2)
                           MOVE    BYO-HAISIYMD    TO
                                       SPA-NAI-C02-HAISIYMD (IDZ)
                                       SPA-NAI-C02-HAISIYMD-O(IDZ IDX2)
                           MOVE    BYO-TOKSKNCD    TO
                                       SPA-NAI-C02-TOKSKNCD (IDZ)
                           MOVE    BYO-NANBYOCD    TO
                                       SPA-NAI-C02-NANBYOCD-O(IDZ)
                           MOVE    BYO-TANDOKUKBN
                                               TO
                                       SPA-NAI-C02-TANDOKUKBN-O
                                                             (IDZ IDX2)
                       END-IF
                       MOVE    SPA-NAI-C02-BYOMEI-O (IDZ 1)
                                               TO
                                           SPA-NAI-C02-BYOMEI (IDZ)
                       PERFORM VARYING IDX2    FROM    2   BY  1
                               UNTIL   IDX2    >       21
                               OR      SPA-NAI-C02-BYOMEICD-O (IDZ IDX2)
                                                   =   SPACE
                           STRING   SPA-NAI-C02-BYOMEI (IDZ)
                                               DELIMITED  BY  SPACE
                                    SPA-NAI-C02-BYOMEI-O (IDZ IDX2)
                                               DELIMITED  BY  SPACE
                                        INTO    SPA-NAI-C02-BYOMEI (IDZ)
                           END-STRING
                       END-PERFORM        
                   END-IF
      *
                   IF      SPA-C02-BYOSET-INDEX-MAX
                                           >   ZERO
                       INITIALIZE              SPA-CIDCD-AREA
                                               SPA-CID1-AREA
                       INITIALIZE              SPA-C02-IKOSAKI-AREA
                                               SPA-C02-CHANGE-AREA
                                               SPA-C02-HAISI-AREA
                                               SPA-ERRCD
                                               WRK-HAISIYMDH
      *                病名入力セットから自院病名編集処理
                       PERFORM 410261-BYOMEIINPUTSET-HEN-SEC
                       IF    ( SPA-CIDCD2      NOT =   SPACE )
                       OR    ( SPA-CIDCD6      NOT =   SPACE )
                       OR    ( SPA-ERRCD       NOT =   SPACE )
                           GO  TO  300-SCREEN-EXT
                       END-IF
                   END-IF
      *
                   PERFORM 41021-BYOMEICD-HENSYU-SEC
      *     
                   INITIALIZE                  SPA-CIDCD-AREA
                                               SPA-CID1-AREA
                   INITIALIZE                  SPA-C02-IKOSAKI-AREA
                                               SPA-C02-CHANGE-AREA
                                               SPA-C02-HAISI-AREA
                   GO      TO      300-SCREEN-EXT
               END-IF    
           END-IF
      *
      *    確認画面４より
           IF      SPA-MOTOPG(1:4)     =   "CID4"
               IF      SPA-CIDCD4      =   "0101"
                   MOVE    SPACE           TO  SPA-CIDCD4
                   IF      SPA-CID4-FLG        =   "OK"
                       PERFORM 440-SAKUJYO-SEC
      *                病名一覧
                       PERFORM 310-SPASET-PTBYO-HENSYU-SEC
                   END-IF
                   MOVE    1                   TO  SPA-GMN-C02-CUR  
                   MOVE    ZERO                TO  FLG-ROW
                   GO      TO      300-SCREEN-EXT
               END-IF
           END-IF              
      *
      *    病名移行確認画面より
           IF      SPA-MOTOPG(1:4)     =   "CID6"
               PERFORM 370-CID6-SET-SEC
               GO  TO  300-SCREEN-EXT
           END-IF
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＬＤ外の項目入力編集処理
      *****************************************************************
       3101-U01SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
           IF      WRK-MOTOPG(1:3)     =   "P97"
               MOVE    SPACE               TO  WRK-SPA-SRYKA
           END-IF
      *
      *    受付、氏名検索より
           IF      WRK-SPA-PTNUM       =   SPA-GMN-C02-PTNUM
               CONTINUE
           ELSE
               IF      WRK-SPA-PTNUM       NOT =   SPACE
                   MOVE    WRK-SPA-PTNUM       TO  C02-PTNUM
                   PERFORM 41015-PTNUM-CHK-SEC
                   IF    ( WRK-SPA-SRYKA   NOT =   SPACE )
                   AND   ( SPA-C02-1042-SRYKA-DSPKBN
                                           NOT =   "1"   )
                       IF    ( WRK-SPA-SRYKA
                                           NOT =   SPA-GMN-C02-SRYKA )
      *****************AND   ( SPA-C02-1042-SRYKA-DSPKBN
      *****************                    NOT =   "1"               )
                           MOVE    WRK-SPA-SRYKA
                                               TO  SPA-GMN-C02-SRYKA
                           MOVE    SPA-GMN-C02-SRYKA
                                               TO  WRK-KBN
                           PERFORM 5001-SRYKA-HENSYU-SEC
                           MOVE    WRK-KBNMEI  TO  SPA-GMN-C02-SRYKAMEI
                           MOVE    SPA-GMN-C02-SRYKA-G
                                               TO  SPA-GMN-C02-BSRYKA-G
      *                    病名一覧
                           MOVE    1           TO  SPA-GMN-C02-PAGE
      *
                           PERFORM 310-SPASET-PTBYO-HENSYU-SEC
                           PERFORM 310-SPASET-HKNCOMBI-HENSYU-SEC
                       END-IF
                       MOVE    1           TO  SPA-GMN-C02-CUR
                   END-IF    
               END-IF
      *********MOVE    1               TO  SPA-GMN-C02-CUR
           END-IF    
           .
       3101-U01SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-C02-AREA
                                   SPA-GMN-C02-TBL
                                   SPA-NAI-C02-TBL
                                   SPA-PTBYOMEI-REC
                                   WRK-PTBYOMEI-REC (1)
                                   WRK-PTBYOMEI-REC (2)
                                   WRK-PTBYOMEI-REC (3)
                                   WRK-PTBYOMEI-REC (4)
                                   WRK-PTBYOMEI-REC (5)
                                   WRK-PTBYOMEI-REC (6)
                                   WRK-HAISIYMD-OCC
      *      
      *    システム日付
           IF    ( SPA-C02-MOTOPG      =   "K02"   OR  "U02"
                                                   OR  "K02N"
                                                   OR  "D04"
                                                   OR  "KT01"
                                                   OR  "Q02" )
               MOVE    SPA-SRYYMDW     TO  SPA-GMN-C02-SYSYMD
               MOVE    SPA-SRYYMD      TO  SPA-NAI-C02-SYSYMD
           ELSE    
               MOVE    SPA-SYSYMDWH    TO  SPA-GMN-C02-SYSYMD
               MOVE    SPA-SYSYMD      TO  SPA-NAI-C02-SYSYMD
           END-IF   
      *
      *    システム日付の前月
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-C02-SYSYMD  TO  LNK-DAY6-KIJUN 
           MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  SPA-NAI-C02-SYSYMD-BACK
      *
           MOVE    "1"             TO  SPA-GMN-C02-TENKIKBNL        (1)
           MOVE    "治ゆ"          TO  SPA-GMN-C02-TENKIKBNMEIL     (1)
           MOVE    "2"             TO  SPA-GMN-C02-TENKIKBNL        (2)
           MOVE    "死亡"          TO  SPA-GMN-C02-TENKIKBNMEIL     (2)
           MOVE    "3"             TO  SPA-GMN-C02-TENKIKBNL        (3)
           MOVE    "中止"          TO  SPA-GMN-C02-TENKIKBNMEIL     (3)
           MOVE    "8"             TO  SPA-GMN-C02-TENKIKBNL        (4)
           MOVE    "移行"          TO  SPA-GMN-C02-TENKIKBNMEIL     (4)
           MOVE    4               TO  SPA-GMN-C02-TENKIKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-C02-UTAGAIFLGL       (1)
           MOVE    "疑い"          TO  SPA-GMN-C02-UTAGAIFLGMEIL    (1)
           MOVE    "2"             TO  SPA-GMN-C02-UTAGAIFLGL       (2)
           MOVE    "急性"          TO  SPA-GMN-C02-UTAGAIFLGMEIL    (2)
           MOVE    "3"             TO  SPA-GMN-C02-UTAGAIFLGL       (3)
           MOVE    "急性かつ疑い"  TO  SPA-GMN-C02-UTAGAIFLGMEIL    (3)
           MOVE    3               TO  SPA-GMN-C02-UTAGAIFLG-MAX
      *
           MOVE    "1"             TO  SPA-GMN-C02-SYUBYOFLGL       (1)
           MOVE    "主病名である"  TO  SPA-GMN-C02-SYUBYOFLGMEIL    (1)
           MOVE    1               TO  SPA-GMN-C02-SYUBYOFLG-MAX
      *
           MOVE    "03"            TO  SPA-GMN-C02-MANSEIKBNL       (1)
           MOVE    "皮膚科特定疾患指導管理料(１)"
                                   TO  SPA-GMN-C02-MANSEIKBNMEIL    (1)
           MOVE    "皮膚（１）"    TO  SPA-GMN-C02-MANSEIKBNTANL    (1)
           MOVE    "04"            TO  SPA-GMN-C02-MANSEIKBNL       (2)
           MOVE    "皮膚科特定疾患指導管理料(２)"
                                   TO  SPA-GMN-C02-MANSEIKBNMEIL    (2)
           MOVE    "皮膚（２）"    TO  SPA-GMN-C02-MANSEIKBNTANL    (2)
           MOVE    "05"            TO  SPA-GMN-C02-MANSEIKBNL       (3)
           MOVE    "特定疾患療養管理料"
                                   TO  SPA-GMN-C02-MANSEIKBNMEIL    (3)
           MOVE    "特定疾患"      TO  SPA-GMN-C02-MANSEIKBNTANL    (3)
           MOVE    "07"            TO  SPA-GMN-C02-MANSEIKBNL       (4)
           MOVE    "てんかん指導料"
                                   TO  SPA-GMN-C02-MANSEIKBNMEIL    (4)
           MOVE    "てんかん"      TO  SPA-GMN-C02-MANSEIKBNTANL    (4)
           MOVE    "08"            TO  SPA-GMN-C02-MANSEIKBNL       (5)
           MOVE    "特定疾患療養管理料又はてんかん指導料"
                                   TO  SPA-GMN-C02-MANSEIKBNMEIL    (5)
           MOVE    "特定・てん"    TO  SPA-GMN-C02-MANSEIKBNTANL    (5)
           MOVE    "09"            TO  SPA-GMN-C02-MANSEIKBNL       (6)
           MOVE    "難病外来指導管理料"
                                   TO  SPA-GMN-C02-MANSEIKBNMEIL    (6)
           MOVE    "難病外来"      TO  SPA-GMN-C02-MANSEIKBNTANL    (6)
           MOVE    6               TO  SPA-GMN-C02-MANSEIKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-C02-NYUGAIKBNL       (1)
           MOVE    "入院"          TO  SPA-GMN-C02-NYUGAIKBNMEIL    (1)
           MOVE    "2"             TO  SPA-GMN-C02-NYUGAIKBNL       (2)
           MOVE    "入院外"        TO  SPA-GMN-C02-NYUGAIKBNMEIL    (2)
           MOVE    2               TO  SPA-GMN-C02-NYUGAIKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-C02-HKNBYOMEIFLGL    (1)
           MOVE    "保険病名"      TO  SPA-GMN-C02-HKNBYOMEIFLGMEIL (1)
           MOVE    1               TO  SPA-GMN-C02-HKNBYOMEIFLG-MAX
      *
      *
           MOVE    "1"             TO  SPA-GMN-C02-REZEFLGL         (1)
           MOVE    "表示しない"    TO  SPA-GMN-C02-REZEFLGMEIL      (1)
           MOVE    1               TO  SPA-GMN-C02-REZEFLG-MAX
      *
           MOVE    "0"             TO  SPA-GMN-C02-DISCHARGEFLGL    (1)
           MOVE    "記載しない"    TO  SPA-GMN-C02-DISCHARGEFLGMEIL (1)
           MOVE    "1"             TO  SPA-GMN-C02-DISCHARGEFLGL    (2)
           MOVE    "記載する"      TO  SPA-GMN-C02-DISCHARGEFLGMEIL (2)
           MOVE    2               TO  SPA-GMN-C02-DISCHARGEFLG-MAX
      *
           MOVE    "01"            TO  SPA-GMN-C02-CLASS1L     (1)
           MOVE    "原疾患ア"      TO  SPA-GMN-C02-CLASS1MEIL  (1)
           MOVE    "02"            TO  SPA-GMN-C02-CLASS1L     (2)
           MOVE    "原疾患イ"      TO  SPA-GMN-C02-CLASS1MEIL  (2)
           MOVE    "03"            TO  SPA-GMN-C02-CLASS1L     (3)
           MOVE    "原疾患ウ"      TO  SPA-GMN-C02-CLASS1MEIL  (3)
           MOVE    "04"            TO  SPA-GMN-C02-CLASS1L     (4)
           MOVE    "原疾患エ"      TO  SPA-GMN-C02-CLASS1MEIL  (4)
           MOVE    "05"            TO  SPA-GMN-C02-CLASS1L     (5)
           MOVE    "原疾患オ"      TO  SPA-GMN-C02-CLASS1MEIL  (5)
           MOVE    5               TO  SPA-GMN-C02-CLASS1-MAX
      *
           MOVE    "01"            TO  SPA-GMN-C02-CLASS2L     (1)
           MOVE    "アの合併症"    TO  SPA-GMN-C02-CLASS2MEIL  (1)
           MOVE    "02"            TO  SPA-GMN-C02-CLASS2L     (2)
           MOVE    "イの合併症"    TO  SPA-GMN-C02-CLASS2MEIL  (2)
           MOVE    "03"            TO  SPA-GMN-C02-CLASS2L     (3)
           MOVE    "ウの合併症"    TO  SPA-GMN-C02-CLASS2MEIL  (3)
           MOVE    "04"            TO  SPA-GMN-C02-CLASS2L     (4)
           MOVE    "エの合併症"    TO  SPA-GMN-C02-CLASS2MEIL  (4)
           MOVE    "05"            TO  SPA-GMN-C02-CLASS2L     (5)
           MOVE    "オの合併症"    TO  SPA-GMN-C02-CLASS2MEIL  (5)
           MOVE    5               TO  SPA-GMN-C02-CLASS2-MAX
      *
           MOVE    "0"             TO  SPA-GMN-C02-SYOKBNL        (1)
           MOVE    "当月"          TO  SPA-GMN-C02-SYOKBNMEIL     (1)
           MOVE    "1"             TO  SPA-GMN-C02-SYOKBNL        (2)
           MOVE    "前月"          TO  SPA-GMN-C02-SYOKBNMEIL     (2)
           MOVE    "2"             TO  SPA-GMN-C02-SYOKBNL        (3)
           MOVE    "全体"          TO  SPA-GMN-C02-SYOKBNMEIL     (3)
           MOVE    3               TO  SPA-GMN-C02-SYOKBN-MAX
      *
           MOVE    SPA-GMN-C02-SYOKBNLST (1)
                                       TO  SPA-GMN-C02-SYOKBN-G
      *
           MOVE    "0"             TO  SPA-GMN-C02-HNYUGAIKBNL     (1)
           MOVE    "入院・入院外"  TO  SPA-GMN-C02-HNYUGAIKBNMEIL  (1)
           MOVE    "1"             TO  SPA-GMN-C02-HNYUGAIKBNL     (2)
           MOVE    "入院"          TO  SPA-GMN-C02-HNYUGAIKBNMEIL  (2)
           MOVE    "2"             TO  SPA-GMN-C02-HNYUGAIKBNL     (3)
           MOVE    "入院外"        TO  SPA-GMN-C02-HNYUGAIKBNMEIL  (3)
           MOVE    3               TO  SPA-GMN-C02-HNYUGAIKBN-MAX
      *
           MOVE    SPA-GMN-C02-HNYUGAIKBNLST (1)
                                   TO  SPA-GMN-C02-HNYUGAIKBN-G
      *         
      *    主病名編集フラグ情報
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
grpsys     MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
           MOVE    "2005"           TO  SYS-KANRICD
           MOVE    "*"              TO  SYS-KBNCD
           MOVE    SPA-NAI-C02-SYSYMD
                                    TO  SYS-STYUKYMD
                                        SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-2005-REC
           ELSE    
               MOVE    SPACE            TO  SYS-2005-REC
           END-IF
           IF      SYS-2005-SYUBYOHENFLG    =   SPACE
               MOVE    ZERO             TO  SYS-2005-SYUBYOHENFLG
           END-IF
           MOVE    SYS-2005-SYUBYOHENFLG
                                       TO  SPA-C02-2005-SYUBYOHENFLG
      *
           MOVE    SPACE           TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "2005"          TO  SYS-KANRICD
           MOVE    "01"            TO  SYS-KBNCD
           MOVE    SPA-NAI-C02-SYSYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-25-REC
           ELSE    
               MOVE    SPACE           TO  SYS-25-REC
           END-IF
           IF      SYS-25-TENKIBYOMEI-KBN
                                   =   SPACE
               MOVE    ZERO            TO  SYS-25-TENKIBYOMEI-KBN
           END-IF
           MOVE    SYS-25-TENKIBYOMEI-KBN
                                       TO
                                       SPA-C02-200501-TENKIBYOMEI-KBN
      *    
           MOVE    SPACE           TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1001"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-NAI-C02-SYSYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           ELSE    
               MOVE    SPACE           TO  SYS-1001-REC
           END-IF
           MOVE    SYS-1001-HOSPSBT1   TO  SPA-C02-1001-HOSPSBT1
      *         
      *    病名機能情報 
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
grpsys     MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
           MOVE    "1038"           TO  SYS-KANRICD
           MOVE    "*"              TO  SYS-KBNCD
           MOVE    SPA-NAI-C02-SYSYMD
                                    TO  SYS-STYUKYMD
                                        SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-1038-REC
           ELSE    
               INITIALIZE                   SYS-1038-REC
           END-IF
           IF      SYS-1038-ERRCOLORS
                                   =   SPACE
               MOVE    "#FFB5C5"       TO  SYS-1038-ERRCOLORS
           END-IF
           MOVE    SYS-1038-ERRCOLORS  TO  SPA-C02-1038-ERRCOLORS
      *         
      *    病名機能情報 
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
grpsys     MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
           MOVE    "1042"           TO  SYS-KANRICD
           MOVE    "*"              TO  SYS-KBNCD
           MOVE    SPA-NAI-C02-SYSYMD
                                    TO  SYS-STYUKYMD
                                        SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-1042-REC
           ELSE    
               INITIALIZE                   SYS-1042-REC
           END-IF
           IF      SYS-1042-MSG1-DSPKBN =   SPACE
               MOVE    ZERO             TO  SYS-1042-MSG1-DSPKBN
           END-IF
           MOVE    SYS-1042-MSG1-DSPKBN
                                       TO  SPA-C02-1042-MSG1-DSPKBN
           IF      SYS-1042-MSG2-DSPKBN =   SPACE
               MOVE    ZERO             TO  SYS-1042-MSG2-DSPKBN
           END-IF
           MOVE    SYS-1042-MSG2-DSPKBN
                                       TO  SPA-C02-1042-MSG2-DSPKBN
           IF      SYS-1042-MSG3-DSPKBN =   SPACE
               MOVE    ZERO             TO  SYS-1042-MSG3-DSPKBN
           END-IF
           MOVE    SYS-1042-MSG3-DSPKBN
                                       TO  SPA-C02-1042-MSG3-DSPKBN
           IF      SYS-1042-MSG4-DSPKBN =   SPACE
               MOVE    ZERO             TO  SYS-1042-MSG4-DSPKBN
           END-IF
           MOVE    SYS-1042-MSG4-DSPKBN
                                       TO  SPA-C02-1042-MSG4-DSPKBN
           IF      SYS-1042-MSG5-DSPKBN =   SPACE
               MOVE    ZERO             TO  SYS-1042-MSG5-DSPKBN
           END-IF
           MOVE    SYS-1042-MSG5-DSPKBN
                                       TO  SPA-C02-1042-MSG5-DSPKBN
           IF      SYS-1042-UTAGAIFLG-KBN
                                    =   SPACE
               MOVE    "0"              TO  SYS-1042-UTAGAIFLG-KBN
           END-IF
           MOVE    SYS-1042-UTAGAIFLG-KBN
                                        TO  SPA-C02-1042-UTAGAIFLG-KBN
      *
      *    旧総合病院の場合は全科表示はなしとする
           IF      SYS-1042-SRYKA-DSPKBN =   SPACE
               MOVE    ZERO             TO  SYS-1042-SRYKA-DSPKBN
           END-IF
           MOVE    SYS-1042-SRYKA-DSPKBN
                                       TO  SPA-C02-1042-SRYKA-DSPKBN
           IF    ( SPA-C02-1001-HOSPSBT1
                                       =   "1" )
           AND   ( SPA-C02-1042-SRYKA-DSPKBN
                                       =   "1" )
               MOVE    ZERO             TO  SPA-C02-1042-SRYKA-DSPKBN
           END-IF
      *
           IF      SYS-1042-SYOKBN-DSPKBN =   SPACE
               MOVE    ZERO             TO  SYS-1042-SYOKBN-DSPKBN
           END-IF
           MOVE    SYS-1042-SYOKBN-DSPKBN
                                       TO  SPA-C02-1042-SYOKBN-DSPKBN
           EVALUATE    SPA-C02-1042-SYOKBN-DSPKBN
               WHEN    ZERO
                   MOVE    SPA-GMN-C02-SYOKBNLST (1)
                                           TO  SPA-GMN-C02-SYOKBN-G
               WHEN    "1"
                   MOVE    SPA-GMN-C02-SYOKBNLST (2)
                                           TO  SPA-GMN-C02-SYOKBN-G
               WHEN    "2"
                   MOVE    SPA-GMN-C02-SYOKBNLST (3)
                                           TO  SPA-GMN-C02-SYOKBN-G
           END-EVALUATE
      *
      *    診療科編集
           PERFORM 3104-SRYKA-SPA-SEC
      *
      *    患者情報（診療行為等で患者番号を入力したとき） 
           IF    ( SPA-C02-MOTOPG      =   "K02"   OR  "U02"
                                                   OR  "K02N"
                                                   OR  "D04"
                                                   OR  "KT01"
                                                   OR  "Q02"  )
           AND   ( SPA-PTNUM       NOT =   SPACE              )
               IF      SPA-C02-MOTOPG      =   "D04"
                   MOVE    SPA-WORK        TO  LNK-D04-AREA
               END-IF
               MOVE    SPA-PTNUM       TO  C02-PTNUM
               PERFORM 41015-PTNUM-CHK-SEC
               MOVE    SPA-HKNCOMBIMEI TO  SPA-GMN-C02-HKNCOMBIMEIH
               MOVE    SPA-FTNRATEMEI  TO  SPA-GMN-C02-FTNRATE
               IF      SPA-ERRCD   NOT =   SPACE
                   MOVE    27              TO  SPA-GMN-C02-CUR
                   MOVE    SPACE           TO  SPA-ERRCD
               ELSE
                   IF    ( SPA-SRYKA   NOT =   SPACE )
                   AND   ( SPA-C02-1042-SRYKA-DSPKBN
                                       NOT =   "1"   )
                       IF    ( SPA-SRYKA   NOT =   SPA-GMN-C02-SRYKA )
      *****************AND   ( SPA-C02-1042-SRYKA-DSPKBN
      *****************                    NOT =   "1"               )
                           MOVE    SPA-SRYKA   TO  SPA-GMN-C02-SRYKA
                           MOVE    SPA-GMN-C02-SRYKA
                                               TO  WRK-KBN
                           PERFORM 5001-SRYKA-HENSYU-SEC
                           MOVE    WRK-KBNMEI  TO  SPA-GMN-C02-SRYKAMEI
                           MOVE    SPA-GMN-C02-SRYKA-G
                                               TO  SPA-GMN-C02-BSRYKA-G
      *                    病名一覧
                           MOVE    1           TO  SPA-GMN-C02-PAGE
      *
                           PERFORM 310-SPASET-PTBYO-HENSYU-SEC
                           PERFORM 310-SPASET-HKNCOMBI-HENSYU-SEC
                       END-IF
                       MOVE    1               TO  SPA-GMN-C02-CUR
                   END-IF    
      *************MOVE    1               TO  SPA-GMN-C02-CUR
               END-IF
               IF      SPA-C02-MOTOPG      =   "D04"
      *
                   MOVE    LNK-D04-BYOMEI
                                       TO  C02-BYOMEI
                   MOVE    LNK-D04-HOSOKU-COMMENT
                                       TO  C02-HOSOKU-COMMENT
      *
                   PERFORM 41025-HOSOKU-COMMENT-CHK-SEC
                   MOVE    SPACE       TO  SPA-ERRCD
      *
                   PERFORM 41013-BYOMEI-CHK-SEC
      *
                   IF    ( FLG-END     =   ZERO )
                    AND  ( SPA-ERRCD   =   SPACE )
                       EVALUATE    TRUE
                       WHEN  ( SPA-GMN-C02-BYOMEI         NOT = SPACE )
                         AND ( SPA-GMN-C02-HOSOKU-COMMENT NOT = SPACE )
                           MOVE    28  TO  SPA-GMN-C02-CUR
                       WHEN  ( SPA-GMN-C02-BYOMEI         NOT = SPACE )
                         AND ( SPA-GMN-C02-HOSOKU-COMMENT     = SPACE )
                           MOVE    34  TO  SPA-GMN-C02-CUR
                       WHEN  ( SPA-GMN-C02-BYOMEI             = SPACE )
                         AND ( SPA-GMN-C02-HOSOKU-COMMENT NOT = SPACE )
                           MOVE    2   TO  SPA-GMN-C02-CUR
                       WHEN  OTHER
                           MOVE    1   TO  SPA-GMN-C02-CUR
                       END-EVALUATE
                   END-IF
      *
               END-IF
           ELSE    
               MOVE    26              TO  SPA-GMN-C02-CUR
           END-IF   
      *
           IF    ( SPA-GMN-C02-SRYKA   NOT =   ZERO  )
           AND   ( SPA-GMN-C02-SRYKAMEI
                                       NOT =   SPACE )
           AND   ( SPA-GMN-C02-BSRYKA      =   SPACE )
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
      *
           MOVE    1               TO  SPA-GMN-C02-PAGE
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険組合せ情報 編集処理
      *****************************************************************
       310-SPASET-HKNCOMBI-HENSYU-SEC  SECTION.
      * 
           MOVE    ZERO                TO  IDY
                                           IDX
      *     
           MOVE    SPA-NAI-C02-PTID    TO  SPA-PTID
           MOVE    SPA-NAI-C02-BIRTHDAY
                                       TO  SPA-BIRTHDAY
           MOVE    "2"                 TO  SPA-NYUGAIKBN
           MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
           INITIALIZE                  ORCSHKNSETAREA
           MOVE    "2"                 TO  ORCSHKN-KBN
           MOVE    "2"                 TO  ORCSHKN-KBN3
           MOVE    "1"                 TO  ORCSHKN-KBN4
           CALL    "ORCSHKNSET"        USING
                                       SPA-AREA
                                       ORCSHKNSETAREA
      *     
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >      ORCSHKN-HKN-MAX
                   OR          IDX     >      200
               PERFORM 3102-HKNCOMBI-SPA-SEC
           END-PERFORM                                
           .
       310-SPASET-HKNCOMBI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    第三者行為情報編集処理
      *****************************************************************
       310-SPASET-KOHID-HENSYU-SEC  SECTION.
      * 
           MOVE    ZERO                TO  IDX
      *
           INITIALIZE                  PTRSIINF-REC
           MOVE    SPA-HOSPNUM     TO  PTRSI-HOSPNUM
           MOVE    SPA-NAI-C02-PTID
                                   TO  PTRSI-PTID
           MOVE    "6"             TO  PTRSI-HKNKBN
           MOVE    PTRSIINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_ptrsiinf"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-PTRSIINF-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
           PERFORM     UNTIL   FLG-PTRSIINF    =   1
                       OR      IDX             >   100
               IF      PTRSI-SAKUJOKBN     NOT =   "1"
                   ADD     1               TO  IDX
                   MOVE    PTRSI-KOHID     TO  SPA-NAI-C02-KOHIDL (IDX)
                   MOVE    PTRSI-RYOSTYMD  TO  SPA-NAI-C02-RYOSTYMDL
                                                                  (IDX)
                   MOVE    PTRSI-RYOEDYMD  TO  SPA-NAI-C02-RYOEDYMDL
                                                                  (IDX)
                   STRING  "第三者（"          DELIMITED   BY  SIZE
                            PTRSI-COMMENT (1:12)
                                               DELIMITED   BY  SPACE
                           "）"                DELIMITED   BY  SIZE
                                               INTO
                                               SPA-GMN-C02-KOHIDL (IDX)
                   END-STRING
               END-IF
      *
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PTRSIINF-READ-N-SEC
           END-PERFORM
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    IDX                 TO  SPA-GMN-C02-KOHID-MAX
      *
           IF      SPA-GMN-C02-KOHID-MAX
                                       >   ZERO
               MOVE    1               TO  SPA-NAI-C02-KOHID-DAISANSYA
           END-IF
           .
       310-SPASET-KOHID-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスター情報 編集処理
      *****************************************************************
       310-SPASET-PTBYO-HENSYU-SEC     SECTION.
      *
      *    患者番号未入力時
           IF      SPA-NAI-C02-PTID   =   0
      *****        MOVE    "0086"             TO  SPA-ERRCD
                   GO  TO  310-SPASET-PTBYO-HENSYU-EXT
           END-IF
      *
            INITIALIZE                 SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-GMN-C02-MAX
                                       SPA-NAI-C02-UPDOWN
      *
           MOVE    SPACE           TO  WRK-SRYYMD
           INITIALIZE                  WRK-PTBYOMEI-SRYYMD-AREA
      *
           MOVE    ZERO                TO  IDX
                                           IDX2
                                           SPA-GMN-C02-MAX
                                           WRK-LINE
                                           WRK-PAGE
      *
           INITIALIZE                  PTBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-SRYKA   TO  PTBYO-SRYKA
      *
      *    月による表示切替えとき
           EVALUATE    SPA-GMN-C02-SYOKBN
               WHEN    ZERO
                   MOVE    SPA-NAI-C02-SYSYMD  TO  PTBYO-SRYYMD
                   MOVE    "31"                TO  PTBYO-SRYYMD (7:2)
               WHEN    "1"
                   MOVE    SPA-NAI-C02-SYSYMD-BACK
                                               TO  PTBYO-SRYYMD
                   MOVE    "31"                TO  PTBYO-SRYYMD (7:2)
               WHEN    "2"
                   MOVE    ALL "9"             TO  PTBYO-SRYYMD
           END-EVALUATE
      *
      *    入外区分による表示切替えとき
           IF      SPA-GMN-C02-HNYUGAIKBN  =   ZERO
               MOVE    SPACE                   TO  PTBYO-NYUGAIKBN
           ELSE
               EVALUATE    SPA-GMN-C02-HNYUGAIKBN
                   WHEN    "1"
                       MOVE    "1"             TO  PTBYO-NYUGAIKBN
                   WHEN    "2"
                       MOVE    "2"             TO  PTBYO-NYUGAIKBN
               END-EVALUATE
           END-IF
      *
      *    全科目指定・入外区分指定のときのキー設定
           IF      SPA-GMN-C02-HNYUGAIKBN  =   ZERO
               IF      SPA-GMN-C02-SRYKA       =   ZERO
                   MOVE    "%%"                TO  PTBYO-SRYKA
               ELSE
                   MOVE    SPA-GMN-C02-SRYKA   TO  PTBYO-SRYKA
               END-IF
               MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
               MOVE    "key37"             TO  WRK-MCP-PATHNAME
           ELSE
               IF      SPA-GMN-C02-SRYKA       =   ZERO
                   MOVE    "%%"                TO  PTBYO-SRYKA
               ELSE
                   MOVE    SPA-GMN-C02-SRYKA   TO  PTBYO-SRYKA
               END-IF
               MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
               MOVE    "key38"             TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    PTBYO-SRYYMD        TO  WRK-SYSYMD
      *
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM UNTIL ( WRK-LINE        >=  WRK-CONS-C02-MAX
                           AND
                           WRK-PAGE        =   SPA-GMN-C02-PAGE )
                   OR    ( FLG-PTBYOMEI    =   1                )
               PERFORM 3101-PTBYO-SPA-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF    ( WRK-PTBYOMEI-SRYYMD-MAX
                                       >   ZERO  )
           OR    ( FLG-PTBYOMEI        =   ZERO  )
               MOVE    1                   TO  SPA-GMN-C02-NEXT
           ELSE
               MOVE    ZERO                TO  SPA-GMN-C02-NEXT
           END-IF
      *
           MOVE    1                   TO  FLG-ROW
           .
       310-SPASET-PTBYO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名画面編集処理
      *****************************************************************
       3101-PTBYO-SPA-SEC         SECTION.
      *
           MOVE    PTBYO-SRYYMD    TO  WRK-SRYYMD
           INITIALIZE                  WRK-PTBYOMEI-SRYYMD-AREA
      *
      *
      *    同一開始日かどうか
           PERFORM UNTIL ( FLG-PTBYOMEI    =   1                )
                   OR    ( WRK-SRYYMD  NOT =   PTBYO-SRYYMD     )
                   OR    ( WRK-PTBYOMEI-SRYYMD-MAX
                                           >=  WRK-CONS-C02-MAX )
               MOVE    ZERO                TO  FLG-CHK
      *        レセプト対象の病名かチェック（全体以外のとき）
               IF      SPA-GMN-C02-SYOKBN   NOT =   "2"
      *          レセプト表示フラグ
                 IF      PTBYO-REZEFLG       =   "1"
                   MOVE    1                   TO  FLG-CHK
                 ELSE
      *
                   IF      PTBYO-SRYYMD(1:6)    =   WRK-SYSYMD (1:6)
                       CONTINUE
                   ELSE
                       IF      PTBYO-TENKIKBN       =   SPACE
                           CONTINUE
                       ELSE 
                           IF    ( SPA-C02-200501-TENKIBYOMEI-KBN
                                                       =   "0"  )
                           OR    ( PTBYO-REZEMM        =   ZERO )
                               IF      PTBYO-TENKIYMD(1:6)
                                                   >=  WRK-SYSYMD (1:6)
                                   CONTINUE
                               ELSE
                                   MOVE    1               TO  FLG-CHK
                               END-IF
                           END-IF
                       END-IF
      *
                       IF      FLG-CHK             =   1
                           CONTINUE
                       ELSE
      *                  レセプト表示月数
                         MOVE    ZERO                TO  WRK-YM
                         IF      PTBYO-REZEMM        >   ZERO
                           INITIALIZE                      STS-AREA-DAY
                           INITIALIZE                      LNK-DAY6-AREA
                           MOVE    "61"            TO  LNK-DAY6-IRAI
                           MOVE    PTBYO-SRYYMD    TO  LNK-DAY6-KIJUN 
                           MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
                           COMPUTE LNK-DAY6-ZOGEN
                                               =   PTBYO-REZEMM    -   1    
                           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                           LNK-DAY6-AREA
                           MOVE    LNK-DAY6-KEISAN(1:6)
                                                   TO  WRK-YM
                           IF    ( SPA-C02-200501-TENKIBYOMEI-KBN
                                                   =   "0"   )
                           OR    ( PTBYO-TENKIKBN  =   SPACE )
                               IF      WRK-YM      >=  WRK-SYSYMD (1:6)
                                   CONTINUE
                               ELSE
                                   MOVE    1               TO  FLG-CHK
                               END-IF
                           END-IF
                         END-IF                    
      *
      *                  レセプト表示期間と転帰日の優先設定による判定
                         IF    ( PTBYO-TENKIKBN      =   SPACE )
                         OR    ( PTBYO-REZEMM        =   ZERO  )
                           CONTINUE
                         ELSE
                           IF      SPA-C02-200501-TENKIBYOMEI-KBN
                                                   =   "1"
                               IF      WRK-YM      >=  WRK-SYSYMD (1:6)
                                   CONTINUE
                               ELSE
                                   MOVE    1           TO  FLG-CHK
                               END-IF
                           END-IF    
                         END-IF
                       END-IF
                   END-IF
                 END-IF
               END-IF
      *
               IF      FLG-CHK         =   ZERO
                   ADD     1               TO  WRK-PTBYOMEI-SRYYMD-MAX
                                               IDX2
                   MOVE    IDX2            TO  WRK-PTBYO-SRY-BANGO
                                               (WRK-PTBYOMEI-SRYYMD-MAX)
                   MOVE    PTBYOMEI-REC    TO  WRK-PTBYOMEI-SRYYMD-REC
                                               (WRK-PTBYOMEI-SRYYMD-MAX)
               END-IF
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
      *    対象がないとき
           IF      WRK-PTBYOMEI-SRYYMD-MAX
                                   =   ZERO
               GO  TO  3101-PTBYO-SPA-EXT
           END-IF
      *
           IF      SPA-GMN-C02-MAX =   ZERO
               ADD     1               TO  WRK-PAGE
           END-IF
      *
      *    表示可能か
           COMPUTE WRK-LINE    =   SPA-GMN-C02-MAX
                               +   WRK-PTBYOMEI-SRYYMD-MAX
           IF      WRK-LINE        >   WRK-CONS-C02-MAX
               IF      WRK-PAGE        <   SPA-GMN-C02-PAGE
                   MOVE    ZERO            TO  WRK-LINE
                                               SPA-GMN-C02-MAX
                   ADD     1               TO  WRK-PAGE
               ELSE
                   GO  TO  3101-PTBYO-SPA-EXT
               END-IF
           END-IF
      *
      *    該当頁に表示するデータかどうか
           IF      WRK-PAGE            =   SPA-GMN-C02-PAGE
      *        同一開始日分を表示
               MOVE    SPA-GMN-C02-MAX TO  IDX
               PERFORM VARYING IDZZ    FROM    1   BY  1
                       UNTIL   IDZZ    >       WRK-PTBYOMEI-SRYYMD-MAX
                   ADD     1           TO  IDX
      *
                   MOVE    WRK-PTBYO-SRY-BANGO   (IDZZ)
                                       TO  SPA-GMN-C02-TBANGO     (IDX)
                   MOVE    WRK-PTBYO-SRY-SRYKA   (IDZZ)
                                       TO  SPA-NAI-C02-TSRYKA     (IDX)
                                           WRK-KBN
                   PERFORM 5001-SRYKA-HENSYU-SEC
                   MOVE    WRK-KBNMEI  TO  SPA-GMN-C02-TSRYKA     (IDX)
                   MOVE    WRK-PTBYO-SRY-RENNUM  (IDZZ)
                                       TO  SPA-NAI-C02-TRENNUM    (IDX)
                   MOVE    WRK-PTBYO-SRY-SEQNUM  (IDZZ)
                                       TO  SPA-NAI-C02-TSEQNUM    (IDX)
                   MOVE    WRK-PTBYO-SRY-BYOMEI  (IDZZ)
                                       TO  SPA-NAI-C02-TBYOMEI    (IDX)
                   MOVE    WRK-PTBYO-SRY-HOSOKU-COMMENT  (IDZZ)
                                       TO  SPA-NAI-C02-THOSOKU-COMMENT
                                                                  (IDX)
                   INITIALIZE              ORCSMKBYOMEIAREA
                   MOVE    SPA-NAI-C02-TBYOMEI         (IDX)
                                       TO  SMKBYO-BYOMEI
                   MOVE    SPA-NAI-C02-THOSOKU-COMMENT (IDX)
                                       TO  SMKBYO-HOSOKU-COMMENT
                   CALL    "ORCSMKBYOMEI"      USING   ORCSMKBYOMEIAREA
                   MOVE    SMKBYO-BYOMEI
                                       TO  SPA-GMN-C02-TBYOMEI (IDX)
      *
                   MOVE    WRK-PTBYO-SRY-SRYYMD  (IDZZ)
                                       TO  SPA-NAI-C02-TSRYYMD    (IDX)
                                       WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG TO  SPA-GMN-C02-TSRYYMD    (IDX)
      *
                   MOVE    WRK-PTBYO-SRY-REZEMM  (IDZZ)
                                       TO  SPA-GMN-C02-TREZEMM    (IDX)
      *
                   MOVE    WRK-PTBYO-SRY-TENKIYMD(IDZZ)
                                       TO  SPA-NAI-C02-TTENKIYMD  (IDX)
                   IF      WRK-PTBYO-SRY-TENKIYMD(IDZZ)
                                   NOT =   SPACE
                       MOVE    WRK-PTBYO-SRY-TENKIYMD(IDZZ)
                                       TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    WRK-HENYMDG TO
                                           SPA-GMN-C02-TTENKIYMD  (IDX)
                   ELSE
                       MOVE    SPACE       TO
                                           SPA-GMN-C02-TTENKIYMD  (IDX)
                   END-IF
      * 
                   MOVE    WRK-PTBYO-SRY-TENKIKBN(IDZZ)
                                       TO  SPA-NAI-C02-TTENKIKBN  (IDX)
                                           WRK-KBN
                   PERFORM 5001-TENKIKBN-HENSYU-SEC
                   MOVE    WRK-KBNMEI  TO  SPA-GMN-C02-TTENKIKBN  (IDX)
      * 
                   MOVE    WRK-PTBYO-SRY-UTAGAIFLG   (IDZZ)
                                       TO  WRK-KBN
                   PERFORM 5001-UTAGAIFLG-HENSYU-SEC
                   MOVE    WRK-KBNMEI  TO  SPA-GMN-C02-TUTAGAIFLG (IDX)
      * 
                   MOVE    WRK-PTBYO-SRY-SYUBYOFLG   (IDZZ)
                                       TO  WRK-KBN
                   PERFORM 5001-SYUBYOFLG-HENSYU-SEC
                   MOVE    WRK-KBNMEI  TO  SPA-GMN-C02-TSYUBYOFLG (IDX)
      *
                   MOVE    WRK-PTBYO-SRY-MANSEIKBN   (IDZZ)
                                       TO  WRK-KBN 
                   PERFORM 5001-MANSEIKBN-HENSYU-SEC
                   MOVE    WRK-KBNMEITAN
                                       TO  SPA-GMN-C02-TMANSEIKBN (IDX)
      *
                   MOVE    WRK-PTBYO-SRY-NYUGAIKBN   (IDZZ)
                                       TO  SPA-NAI-C02-TNYUGAIKBN (IDX)
                                           WRK-KBN 
                   PERFORM 5001-NYUGAIKBN-HENSYU-SEC
                   MOVE    WRK-KBNMEI  TO  SPA-GMN-C02-TNYUGAIKBN (IDX)
      * 
                   MOVE    WRK-PTBYO-SRY-HKNCOMBI    (IDZZ)
                                       TO  SPA-NAI-C02-THKNCOMBI  (IDX)
                                           WRK-KBN 
                   PERFORM 5001-HKNCOMBI-HENSYU-SEC
                   IF      WRK-KBNMEI  =   SPACE
                       PERFORM 31011-HKNCOMBI-HENSYU-SEC
                   ELSE
                       MOVE    WRK-KBNMEI  TO
                                           SPA-GMN-C02-THKNCOMBI  (IDX)
                   END-IF
      * 
                   MOVE    WRK-PTBYO-SRY-KOHID    (IDZZ)
                                       TO  SPA-NAI-C02-TKOHID  (IDX)
                                           WRK-KOHID
                   MOVE    SPACE       TO  WRK-KBNMEI 
                   PERFORM 5001-KOHID-HENSYU-SEC
                   IF      WRK-KBNMEI  =   SPACE
                       PERFORM 31011-PTRSIINF-HENSYU-SEC
                   ELSE
                       MOVE    WRK-KBNMEI  TO  SPA-GMN-C02-TKOHID (IDX)
                   END-IF
      * 
                   MOVE    WRK-PTBYO-SRY-HKNBYOMEIFLG(IDZZ)
                                       TO  WRK-KBN 
                   PERFORM 5001-HKNBYOMEIFLG-HENSYU-SEC
                   MOVE    WRK-KBNMEI  TO
                                       SPA-GMN-C02-THKNBYOMEIFLG  (IDX)
      * 
                   MOVE    WRK-PTBYO-SRY-REZEFLG     (IDZZ)
                                       TO  WRK-KBN 
                   PERFORM 5001-REZEFLG-HENSYU-SEC
                   MOVE    WRK-KBNMEI  TO  SPA-GMN-C02-TREZEFLG   (IDX)
      *
                   MOVE    WRK-PTBYO-SRY-BYOMEIHENFLG   (IDZZ)
                                       TO  SPA-NAI-C02-TBYOMEIHENFLG
                                                                  (IDX)
      *
                   IF      WRK-PTBYO-SRY-CREYMD  (IDZZ)
                                   NOT =   SPACE
                       MOVE    WRK-PTBYO-SRY-CREYMD  (IDZZ)
                                       TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    WRK-HENYMDG
                                       TO
                                       SPA-GMN-C02-TCREYMD (IDX)
                   END-IF
      *
                   IF      WRK-PTBYO-SRY-CREHMS  (IDZZ)
                                   NOT =   SPACE
                       MOVE    WRK-PTBYO-SRY-CREHMS  (IDZZ) (1:2)
                                       TO
                                       SPA-GMN-C02-TCREHMS (IDX) (1:2)
                       MOVE    WRK-PTBYO-SRY-CREHMS  (IDZZ) (3:2)
                                       TO
                                       SPA-GMN-C02-TCREHMS (IDX) (4:2)
                       MOVE    WRK-PTBYO-SRY-CREHMS  (IDZZ) (5:2)
                                       TO
                                       SPA-GMN-C02-TCREHMS (IDX) (7:2)
                       MOVE    ":"     TO
                                       SPA-GMN-C02-TCREHMS (IDX) (3:1)
                                       SPA-GMN-C02-TCREHMS (IDX) (6:1)
                   END-IF
      *
                   IF      WRK-PTBYO-SRY-UPYMD  (IDZZ)
                                   NOT =   SPACE
                       MOVE    WRK-PTBYO-SRY-UPYMD  (IDZZ)
                                       TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    WRK-HENYMDG
                                       TO
                                       SPA-GMN-C02-TUPYMD (IDX)
                   END-IF
      *
                   IF      WRK-PTBYO-SRY-UPHMS  (IDZZ)
                                   NOT =   SPACE
                       MOVE    WRK-PTBYO-SRY-UPHMS  (IDZZ) (1:2)
                                       TO
                                       SPA-GMN-C02-TUPHMS (IDX) (1:2)
                       MOVE    WRK-PTBYO-SRY-UPHMS  (IDZZ) (3:2)
                                       TO
                                       SPA-GMN-C02-TUPHMS (IDX) (4:2)
                       MOVE    WRK-PTBYO-SRY-UPHMS  (IDZZ) (5:2)
                                       TO
                                       SPA-GMN-C02-TUPHMS (IDX) (7:2)
                       MOVE    ":"     TO
                                       SPA-GMN-C02-TUPHMS (IDX) (3:1)
                                       SPA-GMN-C02-TUPHMS (IDX) (6:1)
                   END-IF
      *
                   IF      WRK-PTBYO-SRY-DISCHARGEFLG(IDZZ)
                                       =   "1"
                       MOVE    "記載"  TO
                                       SPA-GMN-C02-TDISCHARGEFLG (IDX)
                   END-IF
      *
                   MOVE    WRK-PTBYO-SRY-CLASS1(IDZZ)
                                       TO  WRK-KBN 
                   PERFORM 5001-CLASS1-HENSYU-SEC
                   MOVE    WRK-KBNMEI  TO
                                       SPA-GMN-C02-TCLASS1  (IDX)
      *
                   MOVE    WRK-PTBYO-SRY-CLASS2(IDZZ)
                                       TO  WRK-KBN 
                   PERFORM 5001-CLASS2-HENSYU-SEC
                   MOVE    WRK-KBNMEI  TO
                                       SPA-GMN-C02-TCLASS2  (IDX)
               END-PERFORM
           END-IF
      *
           ADD     WRK-PTBYOMEI-SRYYMD-MAX
                                   TO  SPA-GMN-C02-MAX
           INITIALIZE                  WRK-PTBYOMEI-SRYYMD-AREA
      *
      *
      *    初期表示のとき警告エラー表示できないのでスキップ
      *
           GO  TO  3101-PTBYO-SPA-EXT
      * 
           IF    ( PTBYO-TENKIKBN      =   SPACE   OR  "8" )
           AND   ( PTBYO-UTAGAIFLG     =   "1"     OR  "2" )
           AND   ( PTBYO-REZEFLG       =   SPACE           )
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    PTBYO-SRYYMD    TO  LNK-DAY6-KIJUN 
               MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
               EVALUATE    PTBYO-UTAGAIFLG
                   WHEN    "1"
                       MOVE    2               TO  LNK-DAY6-ZOGEN 
                   WHEN    "2"
                       MOVE    1               TO  LNK-DAY6-ZOGEN 
               END-EVALUATE         
               CALL    "ORCSDAY"       USING    STS-AREA-DAY
                                                LNK-DAY6-AREA
               IF      STS-DAY-RC1         NOT =   ZERO
                   MOVE    ZERO            TO  LNK-DAY6-KEISAN
               END-IF
               IF      LNK-DAY6-KEISAN     <=  SPA-NAI-C02-SYSYMD
                   MOVE    "0003"              TO  SPA-ERRCD
               END-IF
           END-IF           
           .
       3101-PTBYO-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名称（削除分）編集処理
      *****************************************************************
       31011-HKNCOMBI-HENSYU-SEC   SECTION.
      *
           IF      WRK-PTBYO-SRY-HKNCOMBI (IDZZ)
                                   =   ZERO
               GO  TO  3101-HKNCOMBI-HENSYU-EXT
           END-IF
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM     TO  COMB-HOSPNUM
           MOVE    SPA-NAI-C02-PTID
                                   TO  COMB-PTID
           MOVE    WRK-PTBYO-SRY-HKNCOMBI (IDZZ)
                                   TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-COMB-INV-SEC
           IF      FLG-HKNCOMBI     =   1
               GO  TO  3101-HKNCOMBI-HENSYU-EXT
           END-IF
      *
           MOVE    SPACE           TO  WRK-HKNCOMBIMEI
      *
           STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBIMEI
           END-STRING
      *    主保険がない時
           IF      COMB-SYU-TANSEIDONAME   =   SPACE
               MOVE    SPACE               TO  WRK-HKNCOMBIMEI
               STRING  COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBIMEI
               END-STRING
           END-IF
      *    労災・自賠責保険検索
           IF    ( COMB-HKNNUM         =   "971"   OR  "973" )
           OR    ( COMB-KOH1HKNNUM     =   "970"             )
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
               MOVE    SPA-PTID            TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
      *        第三者行為
               IF      COMB-KOH1HKNNUM     =   "970"
                   MOVE    COMB-KOH1ID         TO  PTRSI-KOHID
                   MOVE    ZERO                TO  PTRSI-HKNID
               END-IF
               MOVE    PTRSIINF-REC    TO  MCPDATA-REC
               MOVE    "tbl_ptrsiinf"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    "tbl_ptrsiinf"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 900-PTRSIINF-READ-N-SEC
               ELSE
                   MOVE    1               TO  FLG-PTRSIINF
               END-IF
               MOVE    "tbl_ptrsiinf"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *        労災の時、コメントの追加
               IF      COMB-HKNNUM         =   "971"  OR  "973"
                   MOVE    SPACE               TO  WRK-HKNKBN-MEI
                   EVALUATE    PTRSI-HKNKBN
                       WHEN    "1"
                           MOVE    "短"        TO  WRK-HKNKBN-MEI
                       WHEN    "2"
                           MOVE    "傷"        TO  WRK-HKNKBN-MEI
                       WHEN    "3"
                           MOVE    "ア"        TO  WRK-HKNKBN-MEI
                       WHEN    "5"
                           MOVE    "公"        TO  WRK-HKNKBN-MEI
                   END-EVALUATE
                   IF      PTRSI-COMMENT       =   SPACE
                       MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                       STRING  COMB-SYU-TANSEIDONAME
                                                   DELIMITED  BY  SPACE
                              " "                  DELIMITED  BY  SIZE
                               WRK-HKNKBN-MEI      DELIMITED  BY  SPACE
                               INTO                WRK-HKNCOMBIMEI
                       END-STRING
                   ELSE
                       MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                       STRING  COMB-SYU-TANSEIDONAME
                                                   DELIMITED  BY  SPACE
                               " "                 DELIMITED  BY  SIZE
                               WRK-HKNKBN-MEI      DELIMITED  BY  SPACE
                               "（"                DELIMITED  BY  SIZE
                               PTRSI-COMMENT(1:24) DELIMITED  BY  SPACE
                               "）"                DELIMITED  BY  SIZE
                               INTO                WRK-HKNCOMBIMEI
                       END-STRING
                   END-IF
               END-IF
      *        第三者行為
               IF      COMB-KOH1HKNNUM     =   "970"
                   IF      PTRSI-COMMENT   NOT =   SPACE
      *                保険＋公費１のみとなる
                       MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                       STRING  COMB-SYU-TANSEIDONAME
                                                   DELIMITED  BY  SPACE
                               " "                 DELIMITED  BY  SIZE
                               COMB-KOH1-TANSEIDONAME
                                                   DELIMITED  BY  SPACE
                               "（"                DELIMITED  BY  SIZE
                               PTRSI-COMMENT(1:24) DELIMITED  BY  SPACE
                               "）"                DELIMITED  BY  SIZE
                               INTO                WRK-HKNCOMBIMEI
                       END-STRING
                   END-IF
              END-IF
           END-IF
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-HKNCOMBIMEI     TO  KANACHK-MAE-INPUT
           MOVE    50                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-HKNCOMBIMEI
           MOVE    WRK-HKNCOMBIMEI     TO  SPA-GMN-C02-THKNCOMBI  (IDX)
           .
       3101-HKNCOMBI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    第三者行為名称（削除分）編集処理
      *****************************************************************
       31011-PTRSIINF-HENSYU-SEC   SECTION.
      *
           IF      WRK-PTBYO-SRY-KOHID (IDZZ)
                                       =   ZERO
               GO  TO  31011-PTRSIINF-HENSYU-EXT
           END-IF
      *
           INITIALIZE                  PTRSIINF-REC
           MOVE    SPA-HOSPNUM     TO  PTRSI-HOSPNUM
           MOVE    SPA-NAI-C02-PTID
                                   TO  PTRSI-PTID
           MOVE    ZERO            TO  PTRSI-HKNID
           MOVE    WRK-PTBYO-SRY-KOHID (IDZZ)
                                   TO  PTRSI-KOHID
           MOVE    PTRSIINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_ptrsiinf"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-PTRSIINF-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
           MOVE    "tbl_ptrsiinf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-PTRSIINF    =   ZERO
               STRING  "第三者（"          DELIMITED   BY  SIZE
                        PTRSI-COMMENT (1:12)
                                           DELIMITED   BY  SPACE
                       "）"                DELIMITED   BY  SIZE
                                           INTO
                                               SPA-GMN-C02-TKOHID (IDX)
               END-STRING
           END-IF
           .
       31011-PTRSIINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せＳＰＡ画面編集処理
      *****************************************************************
       3102-HKNCOMBI-SPA-SEC          SECTION.
      *
      *    自費の時
           IF      ORCSHKN-NAI-HKNNUM (IDX)(1:2)
                                       =   "98"
               MOVE    1                   TO
                                           SPA-NAI-C02-HKNCOMBI-JIHI
           END-IF
      *
      *    公害の時
           IF      ORCSHKN-NAI-HKNNUM (IDX)
                                       =   "975"
               MOVE    1                   TO
                                           SPA-NAI-C02-HKNCOMBI-KOUGAI
           END-IF
      *
      *    労災の時、コメントの追加
           IF      ORCSHKN-NAI-HKNNUM (IDX)
                                       =   "971"  OR  "973"
               MOVE    1                   TO
                                           SPA-NAI-C02-HKNCOMBI-ROUSAI
           END-IF
      *     
           MOVE    ORCSHKN-GMN-HKNCOMBINUM (IDX)
                                       TO  SPA-GMN-C02-HKNCOMBIL   (IDX)
                                           SPA-NAI-C02-THKNCOMBINUM(IDX)
           MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX)
                                       TO  SPA-NAI-C02-THKNCOMBIMEI(IDX)
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI 
           MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX) (1:34)
                                       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-C02-HKNCOMBIMEIL(IDX)
      *                                 
           MOVE    ORCSHKN-NAI-TEKSTYMD (IDX)
                                       TO  SPA-NAI-C02-TEKSTYMD(IDX)
           MOVE    ORCSHKN-NAI-TEKEDYMD (IDX)
                                       TO  SPA-NAI-C02-TEKEDYMD(IDX)
      *
           MOVE    IDX                 TO  SPA-NAI-C02-HKN-MAX
                                           SPA-GMN-C02-HKNCOMBI-MAX
      *
           ADD     1                   TO  IDY
           MOVE    SPA-NAI-C02-THKNCOMBINUM (IDX)
                                   TO  SPA-GMN-C02-THKNCOMBINUM (IDY)
           MOVE    SPA-NAI-C02-THKNCOMBIMEI (IDX)
                                   TO  SPA-GMN-C02-THKNCOMBIMEI (IDY)
           MOVE    SPA-NAI-C02-THKNCOMBINUM (IDX)
                                   TO  SPA-GMN-C02-TNUM (IDY)
      *
           ADD     1               TO  IDY 
           MOVE    ORCSHKN-GMN-TEKSTYMD (IDX)
                                   TO  SPA-GMN-C02-THKNCOMBIMEI (IDY)
           MOVE    ORCSHKN-GMN-TEKEDYMD (IDX)
                                   TO
                                   SPA-GMN-C02-THKNCOMBIMEI (IDY)(15:)
           MOVE    SPA-NAI-C02-THKNCOMBINUM (IDX)
                                   TO  SPA-GMN-C02-TNUM (IDY)
      *
           MOVE    IDY             TO  SPA-GMN-C02-HKN-MAX
      *
           .
       3103-HKNCOMBI-GMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科ＳＰＡ画面編集処理
      *****************************************************************
       3104-SRYKA-SPA-SEC              SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-C02-SRYKA-MAX  
      *
      *    旧総合病院以外のとき”００全科”の表示を追加
           IF      SPA-C02-1001-HOSPSBT1   =   "1"
               CONTINUE
           ELSE
               MOVE    "00"                TO  SPA-GMN-C02-SRYKAL   (1)
               MOVE    "全科"              TO  SPA-GMN-C02-SRYKAMEIL(1)
               MOVE    1                   TO  SPA-GMN-C02-SRYKA-MAX
           END-IF  
      *
      *    診療科の表示    
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-NAI-C02-SYSYMD  TO  SYS-1005-STYUKYMD
                                           SYS-1005-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF 
      *
           MOVE    SPA-GMN-C02-SRYKA-MAX   TO  IDX
           MOVE    ZERO                    TO  IDY
           PERFORM         UNTIL       (IDX            >=  100  )
                           OR          (FLG-SYSKANRI   =   1    )
               ADD     1                     TO  IDX
                                                 IDY
               MOVE    MCPDATA-REC           TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD (1:2)  TO
                                             SPA-GMN-C02-SRYKAL   (IDX)
               MOVE    SYS-1005-SRYKANAME1   TO
                                             SPA-GMN-C02-SRYKAMEIL(IDX)
               MOVE    SPA-GMN-C02-SRYKALST (IDX)
                                             TO  
                                             SPA-GMN-C02-BSRYKALST (IDY)
      *
               MOVE    IDX                   TO  SPA-GMN-C02-SRYKA-MAX
               MOVE    IDY                   TO  SPA-GMN-C02-BSRYKA-MAX  
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    １診療科のとき全科の表示はなし
           IF      SPA-C02-1001-HOSPSBT1   =   "1"
               CONTINUE
           ELSE
               IF      SPA-GMN-C02-SRYKA-MAX
                                           =   2
                   MOVE    SPA-GMN-C02-SRYKALST (2)
                                           TO  SPA-GMN-C02-SRYKALST (1)
                   MOVE    SPACE           TO  SPA-GMN-C02-SRYKALST (2)
                   MOVE    1               TO  SPA-GMN-C02-SRYKA-MAX
               END-IF
           END-IF
      * 
           IF      SPA-GMN-C02-SRYKA-MAX    =   1
               MOVE    SPA-GMN-C02-SRYKALST (1)
                                       TO  SPA-GMN-C02-SRYKA-G
           END-IF                                                             
      *
           .
       3104-SRYKA-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名コードセット処理（自院病名画面より戻り）
      *****************************************************************
       320-C03SPASET-SEC          SECTION.
      *
           IF      SPA-C01KYOUTU       =   SPACE
               GO  TO  320-C03SPASET-EXT
           END-IF
      *
      *    登録した自院病名コードの表示
           INITIALIZE                      SPA-GMN-C02-BYOMEICD-G
           INITIALIZE                      SPA-NAI-C02-BYOMEI-TBL
      *
           MOVE    1                   TO  IDZ
           MOVE    SPA-C01-BYOMEIINPUTCD
                                       TO  SPA-GMN-C02-BYOMEICD (IDZ)
                                           SPA-NAI-C02-BYOMEICD (IDZ)
      *
           INITIALIZE                      USERBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
           MOVE    SPA-NAI-C02-BYOMEICD (IDZ)
                                       TO  USBYO-BYOMEIINPUTCD
           PERFORM 410112-USERBYOMEI-HENSYU-SEC
           PERFORM 41021-BYOMEICD-HENSYU-SEC
      *
           INITIALIZE                      SPA-C01KYOUTU
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    34                  TO  SPA-GMN-C02-CUR
           .
      *
       320-C03SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名セット処理（病名検索画面より戻り）
      *****************************************************************
       320-C05SPASET-SEC          SECTION.
      *
           IF      SPA-C01KYOUTU       =   SPACE
               GO  TO  320-C05SPASET-EXT
           END-IF
      *
      *    コードの画面表示はしない
           INITIALIZE                  SPA-GMN-C02-BYOMEICD-G
      *   
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       21
               MOVE    SPA-C01-TBYOMEICD (IDZ)
                                   TO  SPA-NAI-C02-BYOMEICD (IDZ)
                                       SPA-NAI-C02-BYOMEICD-O (IDZ 1)
               MOVE    SPA-NAI-C02-BYOMEICD (IDZ)
                                   TO  BYO-BYOMEICD
               PERFORM 410111-BYOMEI-HENSYU-SEC
               MOVE    SPACE       TO  SPA-NAI-C02-BYOMEITOP (IDZ) 
           END-PERFORM
           MOVE    "1"             TO  SPA-NAI-C02-BYOMEITOP (1)
      *
           PERFORM 41021-BYOMEI-HENSYU-SEC
      *
      *   入力病名コード数カウント
           PERFORM 410111-BYOMEICDSU-HENSYU-SEC
      *
      *    レセ電エラーフラグ
           PERFORM 41041-RECEDENFLG-CHK-SEC
      *
           MOVE    34                  TO  SPA-GMN-C02-CUR
           .
       320-C05SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名セット処理（自院病名検索画面より戻り）
      *****************************************************************
       320-C04SPASET-SEC          SECTION.
      *
           IF      SPA-C01KYOUTU       =   SPACE
               GO  TO  320-C04SPASET-EXT
           END-IF
      *
           IF      SPA-C01-B10-FLG =   ZERO    OR  1
             IF    ( SPA-C01-INDEX     >   ZERO       )
             AND   ( SPA-C01-WIDGET    =   "HOSOKUCD" )
      *        補足コードのコード検索時は病名の置き換えはしない
               CONTINUE
             ELSE
               PERFORM VARYING IDZ FROM    1   BY  1
                       UNTIL   IDZ >       6      
                   MOVE    SPA-C01-TBYOMEICD (IDZ)
                                       TO  SPA-NAI-C02-BYOMEICD (IDZ)
                                           SPA-GMN-C02-BYOMEICD (IDZ)
                   INITIALIZE                  USERBYOMEI-REC
                   MOVE    SPA-HOSPNUM     TO  USBYO-HOSPNUM
                   MOVE    SPA-NAI-C02-BYOMEICD (IDZ)
                                           TO  USBYO-BYOMEIINPUTCD
                   PERFORM 410112-USERBYOMEI-HENSYU-SEC
                   IF      SPA-NAI-C04-BYOMEI-CHG (IDZ)
                                           NOT =   SPACE
      *??              INITIALIZE      SPA-NAI-C02-BYOMEI-G (IDZ)
                       INITIALIZE      SPA-NAI-C02-BYOMEI   (IDZ)
                                       SPA-NAI-C02-US-BYOMEIHENFLG
                                                            (IDZ)
                                       SPA-NAI-C02-TOKSKNCD (IDZ)
                                       SPA-NAI-C02-NANBYOCD-O
                                                            (IDZ)
                       PERFORM VARYING IDX3    FROM    1   BY  1
                               UNTIL   IDX3    >       21
                           INITIALIZE      SPA-NAI-C02-BYOMEICD-O
                                                           (IDZ IDX3)
                                           SPA-NAI-C02-BYOMEI-O
                                                           (IDZ IDX3)
                                           SPA-NAI-C02-HAISIYMD-O
                                                           (IDZ IDX3)
                                           SPA-NAI-C02-TANDOKUKBN-O
                                                           (IDZ IDX3)
                       END-PERFORM
                   END-IF
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       6
                       IF      SPA-C01-TBYOMEICD-INDEX (IDZ)
                                               =   IDX
                           IF      SPA-NAI-C04-BYOMEI-CHG (IDZ)
                                               NOT =   SPACE
      *??                      INITIALIZE      SPA-NAI-C02-BYOMEI-G(IDZ)
                               INITIALIZE  SPA-NAI-C02-BYOMEI   (IDZ)
                                           SPA-NAI-C02-US-BYOMEIHENFLG
                                                                (IDZ)
                                           SPA-NAI-C02-TOKSKNCD (IDZ)
                                           SPA-NAI-C02-NANBYOCD-O
                                                                (IDZ)
                               PERFORM VARYING IDX3    FROM    1   BY  1
                                       UNTIL   IDX3    >       21
                                   INITIALIZE  SPA-NAI-C02-BYOMEICD-O
                                                           (IDZ IDX3)
                                               SPA-NAI-C02-BYOMEI-O
                                                           (IDZ IDX3)
                                               SPA-NAI-C02-HAISIYMD-O
                                                           (IDZ IDX3)
                                               SPA-NAI-C02-TANDOKUKBN-O
                                                           (IDZ IDX3)
                               END-PERFORM
                           END-IF
                           PERFORM VARYING IDX3    FROM    1   BY  1
                                   UNTIL   IDX3    >       21
                                   OR      SPA-NAI-C04-BYOMEICD-O
                                                           (IDX IDX3)
                                                       =   SPACE
                                   OR      SPA-NAI-C04-BYOMEICD-O
                                                           (IDX IDX3)
                                                       =   ALL "9"
                               MOVE    SPA-NAI-C04-BYOMEICD-O (IDX IDX3)
                                           TO  BYO-BYOMEICD
                               PERFORM 900-BYOMEI-READ-SEC
                               IF      FLG-BYOMEI      =   ZERO
                                   MOVE    BYO-HAISIYMD    TO
                                           SPA-NAI-C02-HAISIYMD (IDZ)
                                           SPA-NAI-C02-HAISIYMD-O
                                                             (IDZ IDX3)
                                   MOVE    BYO-TANDOKUKBN  TO
                                           SPA-NAI-C02-TANDOKUKBN-O
                                                             (IDZ IDX3)
                                   IF      SPA-NAI-C04-BYOMEICD-O
                                                             (IDX IDX3)
                                               NOT =
                                       SPA-NAI-C02-BYOMEICD-O (IDZ IDX3)
                                       MOVE    BYO-TOKSKNCD    TO
                                           SPA-NAI-C02-TOKSKNCD (IDZ)
                                       MOVE    BYO-NANBYOCD    TO
                                           SPA-NAI-C02-NANBYOCD-O(IDZ)
      **                               MOVE    BYO-UTAGAIFLG   TO
      **                                   SPA-NAI-C02-UTAGAIFLG (IDZ)
                                   END-IF 
                               END-IF
                               MOVE    SPA-NAI-C04-BYOMEICD-O (IDX IDX3)
                                       TO  SPA-NAI-C02-BYOMEICD-O
                                                          (IDZ IDX3)
                               MOVE    SPA-NAI-C04-BYOMEI-O (IDX IDX3)
                                       TO  SPA-NAI-C02-BYOMEI-O
                                                          (IDZ IDX3)
                           END-PERFORM 
                           MOVE    SPA-NAI-C04-BYOMEI  (IDX)
                                       TO  SPA-NAI-C02-BYOMEI (IDZ)
                           MOVE    SPA-NAI-C04-BYOMEI-CHG  (IDX)
                                       TO  SPA-NAI-C02-BYOMEI-CHG (IDZ)
                           MOVE    6       TO  IDX
                       END-IF   
                   END-PERFORM
               END-PERFORM
               PERFORM 3201-C04SPASET-BYOMEI-HEN-SEC
             END-IF 
           END-IF
      *
           IF      SPA-C01-INDEX   =   ZERO
      *        自院病名検索時
               IF      SPA-C01-B10-FLG =   ZERO    OR  2
                   IF      SPA-C01-HOSOKU-COMMENT  =   SPACE
                       PERFORM VARYING IDZ FROM    1   BY  1
                               UNTIL   IDZ >       3      
                           MOVE    SPA-C01-THOSOKUCD (IDZ)
                                               TO
                                           SPA-GMN-C02-HOSOKUCD (IDZ)
                           MOVE    SPA-GMN-C02-HOSOKUCD (IDZ)
                                               TO  USBYO-BYOMEIINPUTCD
                           PERFORM 410243-HOSOKU-USERBYO-HEN-SEC
                       END-PERFORM
                       PERFORM 3202-C04SPASET-HOSOKU-HEN-SEC
                   ELSE
                       MOVE    SPA-C01-HOSOKU-COMMENT
                                       TO  SPA-GMN-C02-HOSOKU-COMMENT
                   END-IF
               END-IF  
           ELSE
      *        自院病名コード検索時
               MOVE    SPA-C01-INDEX   TO  IDZ
      *
               IF      SPA-C01-WIDGET  =   "BYOMEICD"
                   CONTINUE
               ELSE
                   MOVE    SPA-C01-BYOMEIINPUTCD
                                               TO
                                           SPA-GMN-C02-HOSOKUCD (IDZ)
                   MOVE    SPA-GMN-C02-HOSOKUCD (IDZ)
                                               TO  USBYO-BYOMEIINPUTCD
                   PERFORM 410243-HOSOKU-USERBYO-HEN-SEC
      *
                   PERFORM 3202-C04SPASET-HOSOKU-HEN-SEC
               END-IF
           END-IF
           .
       320-C04SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名セット処理（病名編集処理）
      *****************************************************************
       3201-C04SPASET-BYOMEI-HEN-SEC    SECTION.
      *
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >       6
               PERFORM 4101111-BYOMEITOP-HENSYU-SEC
           END-PERFORM
           PERFORM 41021-BYOMEI-HENSYU-SEC
           PERFORM 41022-CHARTBYOMEI-HENSYU-SEC
      *
      *   入力病名コード数カウント
           PERFORM 410111-BYOMEICDSU-HENSYU-SEC
      *
      *    レセ電エラーフラグ（複数病名以外）
           IF      SPA-NAI-C02-BYOMEICD-MAX    =   1
               PERFORM 41041-RECEDENFLG-CHK-SEC
           ELSE
               MOVE    SPACE               TO  SPA-NAI-C02-RECEDENFLG
           END-IF
      *    
      *    複数病名のとき補足コメントの入力不可
           IF    ( SPA-NAI-C02-BYOMEICD-MAX    >   1     )
           AND   ( SPA-GMN-C02-HOSOKU-COMMENT
                                           NOT =   SPACE )
               INITIALIZE                  SPA-GMN-C02-HOSOKUCD-G
                                           SPA-NAI-C02-HOSOKU-TBL
               MOVE    SPACE           TO  SPA-GMN-C02-HOSOKU-COMMENT
           END-IF
      *
           MOVE    9               TO  SPA-GMN-C02-CUR
           IF      SPA-C01-INDEX   =   ZERO
               CONTINUE
           ELSE
               IF      SPA-C01-INDEX   =   6
                   MOVE    9               TO  SPA-GMN-C02-CUR
               ELSE
                   PERFORM VARYING IDZ FROM    SPA-C01-INDEX   BY  1
                           UNTIL   IDZ >       6
                       IF      SPA-GMN-C02-BYOMEICD (IDZ)
                                           =   SPACE
                           COMPUTE SPA-GMN-C02-CUR =   IDZ     +   1
                           MOVE    6           TO  IDZ
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
      *    病名コードに存在しないコードがあるとき
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >       21
                   OR      SPA-NAI-C02-BYOMEICD (IDZ)
                                   =   SPACE
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL   IDX3    >       21
                       OR      SPA-NAI-C02-BYOMEICD-O (IDZ IDX3)
                                       =   SPACE
                   IF      SPA-NAI-C02-BYOMEI-O (IDZ IDX3)
                                               =   "■■■"
                       MOVE    "0091"        TO  SPA-ERRCD
                       GO  TO  3201-C04SPASET-BYOMEI-HEN-EXT
                   END-IF
               END-PERFORM       
           END-PERFORM       
           .
       3201-C04SPASET-BYOMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名セット処理（補足コメント編集処理）
      *****************************************************************
       3202-C04SPASET-HOSOKU-HEN-SEC    SECTION.
      *
           MOVE    SPACE           TO  SPA-GMN-C02-HOSOKU-COMMENT
                                       WRK-BYOMEI
      *
           STRING  SPA-NAI-C02-HOSOKU-COMMENT (1)
                                           DELIMITED  BY  SPACE
                   SPA-NAI-C02-HOSOKU-COMMENT (2)
                                           DELIMITED  BY  SPACE
                   SPA-NAI-C02-HOSOKU-COMMENT (3)
                                           DELIMITED  BY  SPACE
                                       INTO    WRK-BYOMEI
           END-STRING
           MOVE    WRK-BYOMEI      TO  SPA-GMN-C02-HOSOKU-COMMENT
      *
           IF      SPA-C01-INDEX   =   ZERO
               MOVE    9               TO  SPA-GMN-C02-CUR
           ELSE
               IF      SPA-C01-INDEX   =   3
                   MOVE    9               TO  SPA-GMN-C02-CUR
               ELSE
                   COMPUTE SPA-GMN-C02-CUR =   SPA-C01-INDEX   +   34
               END-IF
           END-IF
           .
       3202-C04SPASET-HOSOKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    簡単修正更新処理（簡単修正画面より戻り）
      *****************************************************************
       320-C10SPASET-SEC          SECTION.
      *
           IF      SPA-CID1-FLG        =   SPACE
               MOVE    1                   TO  SPA-GMN-C02-CUR
               GO  TO  320-C10SPASET-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-GMN-C02-BANGO-X
                                           SPA-CIDCD
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       9
               MOVE    SPA-C01-HENSYU-DATA-O (IDX)
                                       TO  WRK-HENSYU-DATA-O (IDX)
           END-PERFORM
           MOVE    SPA-C01-HENSYU-DATA-9
                                       TO  WRK-HENSYU-DATA-9
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       5
               MOVE    SPA-C01-STNUM-O (IDX)
                                       TO  WRK-STNUM-O (IDX)
               MOVE    SPA-C01-EDNUM-O (IDX)
                                       TO  WRK-EDNUM-O (IDX)
           END-PERFORM
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    更新処理
           MOVE    ZERO                TO  FLG-TENKIKBN-CHK
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       5
               IF      WRK-STNUM-O (IDXX)  >   ZERO
                   MOVE    WRK-STNUM-O (IDXX)  TO  WRK-STNUM
                   MOVE    WRK-EDNUM-O (IDXX)  TO  WRK-EDNUM
                   PERFORM VARYING IDX     FROM
                                           WRK-STNUM-O (IDXX)  BY  1
                           UNTIL   IDX     >   WRK-EDNUM-O (IDXX)
                       IF      SPA-CID1-FLG        =   "DL"
                           PERFORM 3202-BYOMEI-DELETE-KOUSIN-SEC
                       ELSE
                           IF    ( WRK-HENSYU-DATA-O (8)   =   SPACE )
                           AND   ( WRK-HENSYU-DATA-O (9)   =   SPACE )
                               PERFORM 4101131-KANTAN-SYUSEI-UPD-SEC
                           ELSE
                               IF      WRK-HENSYU-DATA-O (9)   =   SPACE
                                   PERFORM 3201-BYOMEI-COPY-KOUSIN-SEC
                               ELSE
                                   PERFORM
                                       3203-BYOMEI-SRYKA-COPY-UPD-SEC
                               END-IF
                           END-IF
                       END-IF
                       IF      SPA-ERRCD   NOT =   SPACE
                           GO  TO  320-C10SPASET-EXT
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *
      *    患者マスタ更新（死亡区分）
           IF      WRK-HENSYU-DATA-O (1)   =   SPACE
               CONTINUE
           ELSE
               IF      SPA-C02-KEIKOKU-FLG4    =   0
                   CONTINUE
               ELSE
                   INITIALIZE                      PTINF-REC
                   MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                   MOVE    SPA-NAI-C02-PTID    TO  PTINF-PTID
                   PERFORM 900-PTINF-READ-SEC
                   IF      FLG-PTINF           =   ZERO
      *                転帰区分が２で死亡区分が生存のとき
                       IF      SPA-C02-KEIKOKU-FLG4    =   1
                           MOVE    MCPDATA-REC     TO  PTINF-REC
                           MOVE    "1"             TO  PTINF-DEATHKBN 
      *
                           MOVE    SMCNDATE-YMD    TO  PTINF-UPYMD
                           MOVE    SMCNDATE-HMS    TO  PTINF-UPHMS
                           MOVE    PTINF-REC       TO  MCPDATA-REC
                           MOVE    "DBUPDATE"      TO  MCP-FUNC
                           MOVE    "tbl_ptinf"     TO  MCP-TABLE
                           MOVE    "key"           TO  MCP-PATHNAME
                           PERFORM 900-ORCDBMAIN-SEC
                           IF      MCP-RC      NOT =   ZERO 
                               MOVE    "1004"          TO  SPA-ERRCD
                               DISPLAY "UPD PTINF-KEY =>" PTINF-KEY 
                               GO  TO  320-C10SPASET-EXT
                           END-IF
      *???
                           DISPLAY "PTINF=" PTINF-DEATHKBN "#"
      *???
                       ELSE 
      *                    修正で転帰区分を２以外に変更したときに死亡区分が死亡のとき
                           IF      SPA-C02-KEIKOKU-FLG4    =   9
                               MOVE    SPACE           TO
                                                       PTINF-DEATHKBN
                               MOVE    SMCNDATE-YMD    TO  PTINF-UPYMD
                               MOVE    SMCNDATE-HMS    TO  PTINF-UPHMS
                               MOVE    PTINF-REC       TO  MCPDATA-REC
                               MOVE    "DBUPDATE"      TO  MCP-FUNC
                               MOVE    "tbl_ptinf"     TO  MCP-TABLE
                               MOVE    "key"           TO  MCP-PATHNAME
                               PERFORM 900-ORCDBMAIN-SEC
                               IF      MCP-RC      NOT =   ZERO 
                                   MOVE    "1004"          TO  SPA-ERRCD
                                   DISPLAY "UPD PTINF-KEY =>" PTINF-KEY 
                                   GO  TO  320-C10SPASET-EXT
                               END-IF
      *???
                               DISPLAY "PTINF=" PTINF-DEATHKBN "#"
      *???
                           END-IF
                      END-IF
                   END-IF
               END-IF
           END-IF     
       *
           MOVE    SPACE               TO  WRK-HAISIYMDH
                                           SPA-CID1-FLG
                                           SPA-CID3-FLG
           MOVE    ZERO                TO  SPA-C02-IKOFLG
           INITIALIZE                      SPA-GMN-C02-KOUMOKU
                                           SPA-NAI-C02-KOUMOKU
                                           SPA-NAI-C02-SELNO
           MOVE    1                   TO  SPA-GMN-C02-CUR
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
      *
           INITIALIZE                  SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-NAI-C02-SELNO
                                       SPA-PTBYOMEI-REC
      *
           IF    ( SPA-GMN-C02-SRYKA   NOT =   ZERO  )
           AND   ( SPA-GMN-C02-SRYKAMEI
                                       NOT =   SPACE )
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
      *    検索処理
           PERFORM 310-SPASET-PTBYO-HENSYU-SEC
           .
       320-C10SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名複写更新処理
      *****************************************************************
       3201-BYOMEI-COPY-KOUSIN-SEC         SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                      PTBYO-KEY
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-NAI-C02-TSRYKA  (IDX)
                                       TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-TSRYYMD (IDX)
                                       TO  PTBYO-SRYYMD
           MOVE    SPA-NAI-C02-TRENNUM (IDX)
                                       TO  PTBYO-RENNUM 
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM 900-PTBYO-INV-SEC
           IF      FLG-PTBYOMEI        =   1
               MOVE    "1003"              TO  SPA-ERRCD
               DISPLAY "INV PTBYOMEI-KEY =>" PTBYO-KEY 
               GO  TO  3201-BYOMEI-COPY-KOUSIN-EXT
           ELSE
               MOVE    PTBYOMEI-REC        TO  WRK-PTBYOMEI-COPY-REC
           END-IF
      *
      *    転帰区分・転帰日
           INITIALIZE                  WRK-PTBYO-COPY-TENKIYMD
                                       WRK-PTBYO-COPY-TENKIKBN
      *
      *    開始日
           MOVE    WRK-HENSYU-DATA-O (8)
                                   TO  WRK-PTBYO-COPY-SRYYMD
      *
      *    疑いフラグ
           EVALUATE    WRK-HENSYU-DATA-O (3)
               WHEN    SPACE               
                   CONTINUE
               WHEN    "-"               
                   MOVE    SPACE   TO  WRK-PTBYO-COPY-UTAGAIFLG
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (3)
                                   TO  WRK-PTBYO-COPY-UTAGAIFLG
           END-EVALUATE      
      *
      *    主病名フラグ
           EVALUATE    WRK-HENSYU-DATA-O (4)
               WHEN    SPACE               
                   CONTINUE
               WHEN    "-"               
                   MOVE    SPACE   TO  WRK-PTBYO-COPY-SYUBYOFLG
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (4)
                                   TO  WRK-PTBYO-COPY-SYUBYOFLG
           END-EVALUATE      
      *
      *    疾患区分
           EVALUATE    WRK-HENSYU-DATA-O (5)
               WHEN    SPACE               
                   CONTINUE
               WHEN    "-"               
                   MOVE    ZERO    TO  WRK-PTBYO-COPY-MANSEIKBN
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (5)
                                   TO  WRK-PTBYO-COPY-MANSEIKBN
           END-EVALUATE      
      *
      *    入外区分
           EVALUATE    WRK-HENSYU-DATA-O (6)
               WHEN    SPACE               
                   CONTINUE
               WHEN    "-"               
                   MOVE    SPACE   TO  WRK-PTBYO-COPY-NYUGAIKBN
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (6)
                                   TO  WRK-PTBYO-COPY-NYUGAIKBN
           END-EVALUATE      
      *
      *    保険適用
           EVALUATE    WRK-HENSYU-DATA-O (7)
               WHEN    SPACE               
                   CONTINUE
               WHEN    "-"               
                   MOVE    ZERO    TO  WRK-PTBYO-COPY-HKNCOMBI
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (7)
                                   TO  WRK-PTBYO-COPY-HKNCOMBI
           END-EVALUATE      
      *
      *    保険適用（第三者行為）
           EVALUATE    WRK-HENSYU-DATA-9
               WHEN    ZERO
                   CONTINUE
               WHEN    9999999999               
                   MOVE    ZERO    TO  WRK-PTBYO-COPY-KOHID
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-9
                                   TO  WRK-PTBYO-COPY-KOHID
           END-EVALUATE
      *
      *    レセ電エラーフラグ
           INITIALIZE                  WRK-PTBYOMEI-CHK-AREA
           IF      WRK-PTBYO-COPY-BYOMEIHENFLG
                                       =   SPACE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       WRK-CONS-BYOMEICD-MAX
                       OR      WRK-PTBYO-COPY-BYOMEICD (IDY)
                                           =   SPACE
                   MOVE    WRK-PTBYO-COPY-BYOMEICD (IDY)
                                               TO  BYO-BYOMEICD
                   PERFORM 900-BYOMEI-READ-SEC
                   IF      FLG-BYOMEI          =   ZERO
                       IF    ( WRK-HAISIYMD-CHK
                                               =   ZERO                )
                       AND   ( BYO-HAISIYMD
                                         NOT   =   ZERO    AND ALL "9" )
                           MOVE    BYO-HAISIYMD    TO  WRK-HAISIYMD-CHK
                       END-IF
                   END-IF    
               END-PERFORM
           END-IF
           MOVE    WRK-PTBYOMEI-COPY-REC
                                       TO  WRK-PTBYOMEI-CHK-REC
           PERFORM 5001-RECEDENFLG-HENSYU-SEC
           MOVE    WRK-PTBYO-CHK-RECEDENFLG
                                       TO  WRK-PTBYO-COPY-RECEDENFLG
      *
      *    連番取得
           MOVE    ZERO                TO  WRK-RENNUM
      *
           INITIALIZE                  PTBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    WRK-PTBYO-COPY-SRYKA
                                       TO  PTBYO-SRYKA
           MOVE    WRK-PTBYO-COPY-SRYYMD
                                       TO  PTBYO-SRYYMD
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key7"              TO  WRK-MCP-PATHNAME
           PERFORM 900-PTBYO-INV-SEC
           IF      FLG-PTBYOMEI        =   ZERO
               IF      PTBYO-RENNUM        =   99
                   MOVE    "0089"              TO  SPA-ERRCD
                   GO  TO  3201-BYOMEI-COPY-KOUSIN-EXT
               END-IF
               MOVE    PTBYO-RENNUM        TO  WRK-RENNUM
           END-IF
           ADD     1                   TO  WRK-RENNUM
           MOVE    WRK-RENNUM          TO  WRK-PTBYO-COPY-RENNUM
      *
           MOVE    9999                TO  WRK-PTBYO-COPY-SEQNUM
           MOVE    SMCNDATE-YMD        TO  WRK-PTBYO-COPY-CREYMD
           MOVE    SMCNDATE-HMS        TO  WRK-PTBYO-COPY-CREHMS
           MOVE    SMCNDATE-YMD        TO  WRK-PTBYO-COPY-UPYMD
           MOVE    SMCNDATE-HMS        TO  WRK-PTBYO-COPY-UPHMS
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
      *
      *???
           DISPLAY "PTBYO=" WRK-PTBYO-COPY-KEY "#" MCP-FUNC "#"
      *???                
           MOVE    WRK-PTBYOMEI-COPY-REC
                                       TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY MCP-FUNC "PTBYOMEI-KEY =>" WRK-PTBYO-COPY-KEY
           ELSE
               PERFORM 450221-RECEUPD-UPDATE-SEC
           END-IF
           .
       3201-BYOMEI-COPY-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名削除処理
      *****************************************************************
       3202-BYOMEI-DELETE-KOUSIN-SEC   SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                      PTBYO-KEY
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-NAI-C02-TSRYKA  (IDX)
                                       TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-TSRYYMD (IDX)
                                       TO  PTBYO-SRYYMD
           MOVE    SPA-NAI-C02-TRENNUM (IDX)
                                       TO  PTBYO-RENNUM 
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM 900-PTBYO-INV-SEC
           IF      FLG-PTBYOMEI        =   1
               MOVE    "1003"              TO  SPA-ERRCD
               DISPLAY "INV PTBYOMEI-KEY =>" PTBYO-KEY 
               GO  TO  3202-BYOMEI-DELETE-KOUSIN-EXT
           END-IF
      *    削除フラグ
           MOVE    "1"                 TO  PTBYO-DLTFLG
           MOVE    SPA-OPID            TO  PTBYO-DLT-OPID
      *
           MOVE    SPA-OPID            TO  PTBYO-OPID
           MOVE    SMCNDATE-YMD        TO  PTBYO-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTBYO-UPHMS
           MOVE    "DBUPDATE"          TO  MCP-FUNC
      *???
           DISPLAY "PTBYO=" PTBYO-KEY "#" MCP-FUNC "#"
      *???                
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY MCP-FUNC "PTBYOMEI-KEY =>"  PTBYO-KEY 
           ELSE
               PERFORM 450221-RECEUPD-UPDATE-SEC
           END-IF
           .
       3202-BYOMEI-DELETE-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名診療科複写更新処理
      *****************************************************************
       3203-BYOMEI-SRYKA-COPY-UPD-SEC         SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                      PTBYO-KEY
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-NAI-C02-TSRYKA  (IDX)
                                       TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-TSRYYMD (IDX)
                                       TO  PTBYO-SRYYMD
           MOVE    SPA-NAI-C02-TRENNUM (IDX)
                                       TO  PTBYO-RENNUM 
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM 900-PTBYO-INV-SEC
           IF      FLG-PTBYOMEI        =   1
               MOVE    "1003"              TO  SPA-ERRCD
               DISPLAY "INV PTBYOMEI-KEY =>" PTBYO-KEY 
               GO  TO  3203-BYOMEI-SRYKA-COPY-UPD-EXT
           ELSE
               MOVE    PTBYOMEI-REC        TO  WRK-PTBYOMEI-COPY-REC
                                               WRK-PTBYOMEI-DELETE-REC
           END-IF
      *
      *    診療科
           MOVE    WRK-HENSYU-DATA-O (9)
                                   TO  WRK-PTBYO-COPY-SRYKA
      *
      *    連番取得
           MOVE    ZERO                TO  WRK-RENNUM
      *
           INITIALIZE                  PTBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    WRK-PTBYO-COPY-SRYKA
                                       TO  PTBYO-SRYKA
           MOVE    WRK-PTBYO-COPY-SRYYMD
                                       TO  PTBYO-SRYYMD
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key7"              TO  WRK-MCP-PATHNAME
           PERFORM 900-PTBYO-INV-SEC
           IF      FLG-PTBYOMEI        =   ZERO
               IF      PTBYO-RENNUM        =   99
                   MOVE    "0089"              TO  SPA-ERRCD
                   GO  TO  3203-BYOMEI-SRYKA-COPY-UPD-EXT
               END-IF
               MOVE    PTBYO-RENNUM        TO  WRK-RENNUM
           END-IF
           ADD     1                   TO  WRK-RENNUM
           MOVE    WRK-RENNUM          TO  WRK-PTBYO-COPY-RENNUM
      *
           MOVE    9999                TO  WRK-PTBYO-COPY-SEQNUM
           MOVE    SMCNDATE-YMD        TO  WRK-PTBYO-COPY-CREYMD
           MOVE    SMCNDATE-HMS        TO  WRK-PTBYO-COPY-CREHMS
           MOVE    SMCNDATE-YMD        TO  WRK-PTBYO-COPY-UPYMD
           MOVE    SMCNDATE-HMS        TO  WRK-PTBYO-COPY-UPHMS
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
      *
      *???
           DISPLAY "PTBYO=" WRK-PTBYO-COPY-KEY "#" MCP-FUNC "#"
      *???                
           MOVE    WRK-PTBYOMEI-COPY-REC
                                       TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY MCP-FUNC "PTBYOMEI-KEY =>" WRK-PTBYO-COPY-KEY
               GO  TO  3203-BYOMEI-SRYKA-COPY-UPD-EXT
           ELSE
               PERFORM 450221-RECEUPD-UPDATE-SEC
           END-IF
      *
      *    診療科移動のとき元の診療科分を削除する
           IF      SPA-CID1-FLG        =   "MV"
               MOVE    WRK-PTBYOMEI-DELETE-REC
                                           TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
      *???
           DISPLAY "PTBYO=" WRK-PTBYO-COPY-KEY "#" MCP-FUNC "#"
      *???                
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "1002"                  TO  SPA-ERRCD
               DISPLAY MCP-FUNC "PTBYOMEI-KEY =>" WRK-PTBYO-COPY-KEY
                   GO  TO  3203-BYOMEI-SRYKA-COPY-UPD-EXT
               END-IF
           END-IF
           .
       3203-BYOMEI-SRYKA-COPY-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    最終受診診療科情報 編集処理
      *****************************************************************
       330-SPASET-JYURRK-HENSYU-SEC    SECTION.
      *     
grpsys     MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  JYURRK-PTID
           MOVE    SPA-NAI-C02-SYSYMD  TO  JYURRK-SRYYMD
           PERFORM 900-JYURRK-INV-SEC
           IF      FLG-JYURRK          =   ZERO
               MOVE    JYURRK-SRYKA        TO  SPA-GMN-C02-SRYKA
                                               WRK-KBN
               PERFORM 5001-SRYKA-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-SRYKAMEI
           END-IF
           .
       330-SPASET-JYURRK-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面６からの戻り処理
      *****************************************************************
       370-CID6-SET-SEC            SECTION.
      *
           EVALUATE    SPA-CIDCD6
               WHEN    "0101"
               WHEN    "0103"
      *            入力病名コード移行先処理
               WHEN    "0102"
               WHEN    "0105"
      *            入力病名コード推奨コード処理
               WHEN    "0107"
      *            入力病名コード廃止コード処理
                   IF      SPA-CID6-FLG    =   "OK"
                       MOVE    SPA-C02-IKOSAKI-INDEX
                                           TO  IDZ
                       MOVE    SPA-C02-IKOSAKI-BYOMEICD
                                           TO  SPA-NAI-C02-BYOMEICD(IDZ)
                       IF      SPA-CIDCD6      =   "0103"  OR  "0105"
                           MOVE    SPA-NAI-C02-BYOMEICD(IDZ)
                                           TO  SPA-GMN-C02-BYOMEICD(IDZ)
                       END-IF
                       MOVE    SPA-NAI-C02-BYOMEICD(IDZ)
                                           TO  BYO-BYOMEICD
                       PERFORM 410111-BYOMEI-HENSYU-SEC
      *
                       INITIALIZE                  SPA-C02-HAISI-AREA
                       PERFORM 41021-BYOMEICD-HENSYU-SEC
                       IF      SPA-CIDCD6      =   "0101"  OR  "0102"
                           MOVE    34              TO  SPA-GMN-C02-CUR
                       END-IF
                   END-IF
                   IF      SPA-CID6-FLG    =   "CHG"
                       PERFORM VARYING IDXX    FROM
                                       SPA-C02-CHANGE-INDEX-ST   BY  1
                               UNTIL   IDXX    >
                                               SPA-C02-CHANGE-INDEX-ED
                           INITIALIZE      SPA-GMN-C02-BYOMEICD   (IDXX)
                           INITIALIZE      SPA-NAI-C02-BYOMEI-OCC (IDXX)
                       END-PERFORM
      *
                       IF      SPA-C02-CHANGE-MAX  >   6
                         CONTINUE
                       ELSE 
      *                置換えによるコードの移動
                         EVALUATE    TRUE
                           WHEN    SPA-C02-CHANGE-INDEX-MAX
                                             >   SPA-C02-CHANGE-MAX
      *                      減る時
                             COMPUTE IDYY    =  SPA-C02-CHANGE-INDEX-ST
                                             +  SPA-C02-CHANGE-MAX
                             COMPUTE IDZZ    =  SPA-C02-CHANGE-INDEX-MAX
                                             -  SPA-C02-CHANGE-MAX
                             PERFORM VARYING IDX FROM    IDYY   BY  1
                                     UNTIL   IDX >       6
                                 COMPUTE IDW     =   IDX     +   IDZZ
                                 IF      IDW         >   6
                                     INITIALIZE
                                           SPA-NAI-C02-BYOMEI-OCC (IDX)
                                           SPA-GMN-C02-BYOMEICD   (IDX)
                                 ELSE
                                     MOVE    SPA-NAI-C02-BYOMEI-OCC(IDW)
                                       TO  SPA-NAI-C02-BYOMEI-OCC (IDX)
                                     MOVE    SPA-GMN-C02-BYOMEICD  (IDW)
                                       TO  SPA-GMN-C02-BYOMEICD   (IDX)
                                 END-IF                     
                             END-PERFORM                           
                           WHEN    SPA-C02-CHANGE-INDEX-MAX
                                             <   SPA-C02-CHANGE-MAX
      *                      増える時
                             COMPUTE IDYY    =  SPA-C02-CHANGE-INDEX-ED
                                             +  1
                             COMPUTE IDZZ    =  SPA-C02-CHANGE-MAX
                                             -  SPA-C02-CHANGE-INDEX-MAX
                             PERFORM VARYING IDX FROM    6   BY  -1
                                     UNTIL   IDX <       IDYY
                                 COMPUTE IDW     =   IDX     -   IDZZ
                                 IF      IDW     >
                                             SPA-C02-CHANGE-INDEX-ED
                                     MOVE    SPA-NAI-C02-BYOMEI-OCC(IDW)
                                       TO  SPA-NAI-C02-BYOMEI-OCC(IDX)
                                     MOVE    SPA-GMN-C02-BYOMEICD  (IDW)
                                       TO  SPA-GMN-C02-BYOMEICD  (IDX)
                                 ELSE
                                     INITIALIZE
                                           SPA-NAI-C02-BYOMEI-OCC (IDX)
                                           SPA-GMN-C02-BYOMEICD   (IDX)
                                 END-IF
                             END-PERFORM
                         END-EVALUATE
                       END-IF
      *                               
                       COMPUTE IDZ =   SPA-C02-CHANGE-INDEX-ST -   1
                       PERFORM VARYING IDXX    FROM   1   BY  1
                               UNTIL   IDXX    >      SPA-C02-CHANGE-MAX
                           ADD     1           TO  IDZ
                           MOVE    SPA-C02-CHANGE-BYOMEICD (IDXX)
                                           TO  SPA-NAI-C02-BYOMEICD(IDZ)
                           IF      SPA-C02-CHANGE-MAX
                                                   <=  6
                               MOVE    SPA-C02-CHANGE-BYOMEICD (IDXX)
                                           TO  SPA-GMN-C02-BYOMEICD(IDZ)
                           END-IF 
                           MOVE    SPA-NAI-C02-BYOMEICD (IDZ)
                                           TO  BYO-BYOMEICD
                           PERFORM 410111-BYOMEI-HENSYU-SEC
                       END-PERFORM
      *
                       IF       SPA-C02-CHANGE-MAX     >   6
                           MOVE    7               TO  SPA-GMN-C02-CUR
                       ELSE
                           COMPUTE SPA-GMN-C02-CUR     =   1   +   IDZ
                       END-IF 
                       INITIALIZE                  SPA-C02-HAISI-AREA
                       PERFORM 41021-BYOMEICD-HENSYU-SEC
                       IF      SPA-CIDCD6      =   "0101"  OR  "0102"
                           MOVE    34              TO  SPA-GMN-C02-CUR
                       END-IF
                   END-IF
                   IF      SPA-CID6-FLG    =   SPACE
                       INITIALIZE                  SPA-C02-HAISI-AREA
                       PERFORM 41021-BYOMEICD-HENSYU-SEC
                   END-IF
               WHEN    "0111"
      *            入力病名移行先処理
               WHEN    "0112"
      *            入力病名推奨コード処理
               WHEN    "0117"
      *            入力病名廃止コード処理
                   IF      SPA-CID6-FLG    =   "OK"
                       MOVE    SPA-C02-IKOSAKI-INDEX
                                           TO  IDZ
                       MOVE    SPA-C02-IKOSAKI-BYOMEICD
                                           TO  SPA-NAI-C02-BYOMEICD(IDZ)
                       MOVE    SPA-NAI-C02-BYOMEICD(IDZ)
                                           TO  BYO-BYOMEICD
                       PERFORM 410111-BYOMEI-HENSYU-SEC
                   END-IF
                   IF      SPA-CID6-FLG    =   "CHG"
                       PERFORM VARYING IDXX    FROM
                                       SPA-C02-CHANGE-INDEX-ST   BY  1
                               UNTIL   IDXX    >
                                               SPA-C02-CHANGE-INDEX-ED
                           INITIALIZE      SPA-NAI-C02-BYOMEI-OCC (IDXX)
                       END-PERFORM
      *
                       IF      SPA-C02-CHANGE-MAX  >   6
                         CONTINUE
                       ELSE 
      *                  置換えによるコードの移動
                         EVALUATE    TRUE
                           WHEN    SPA-C02-CHANGE-INDEX-MAX
                                             >   SPA-C02-CHANGE-MAX
      *                      減る時
                             COMPUTE IDYY    =  SPA-C02-CHANGE-INDEX-ST
                                             +  SPA-C02-CHANGE-MAX
                             COMPUTE IDZZ    =  SPA-C02-CHANGE-INDEX-MAX
                                             -  SPA-C02-CHANGE-MAX
                             PERFORM VARYING IDX FROM    IDYY   BY  1
                                     UNTIL   IDX >       6
                                 COMPUTE IDW     =   IDX     +   IDZZ
                                 IF      IDW         >   6
                                     INITIALIZE
                                           SPA-NAI-C02-BYOMEI-OCC (IDX)
                                 ELSE
                                     MOVE    SPA-NAI-C02-BYOMEI-OCC(IDW)
                                       TO  SPA-NAI-C02-BYOMEI-OCC (IDX)
                                 END-IF                     
                             END-PERFORM                           
                           WHEN    SPA-C02-CHANGE-INDEX-MAX
                                             <   SPA-C02-CHANGE-MAX
      *                      増える時
                             COMPUTE IDYY    =  SPA-C02-CHANGE-INDEX-ED
                                             +  1
                             COMPUTE IDZZ    =  SPA-C02-CHANGE-MAX
                                             -  SPA-C02-CHANGE-INDEX-MAX
                             PERFORM VARYING IDX FROM    6   BY  -1
                                     UNTIL   IDX <       IDYY
                                 COMPUTE IDW     =   IDX     -   IDZZ
                                 IF      IDW     >
                                             SPA-C02-CHANGE-INDEX-ED
                                     MOVE    SPA-NAI-C02-BYOMEI-OCC(IDW)
                                       TO  SPA-NAI-C02-BYOMEI-OCC(IDX)
                                 ELSE
                                     INITIALIZE
                                           SPA-NAI-C02-BYOMEI-OCC (IDX)
                                 END-IF
                             END-PERFORM
                         END-EVALUATE
                       END-IF
      *                               
                       COMPUTE IDZ =   SPA-C02-CHANGE-INDEX-ST -   1
                       PERFORM VARYING IDXX    FROM   1   BY  1
                               UNTIL   IDXX    >      SPA-C02-CHANGE-MAX
                           ADD     1           TO  IDZ
                           MOVE    SPA-C02-CHANGE-BYOMEICD (IDXX)
                                           TO  SPA-NAI-C02-BYOMEICD(IDZ)
                           MOVE    SPA-NAI-C02-BYOMEICD (IDZ)
                                           TO  BYO-BYOMEICD
                           PERFORM 410111-BYOMEI-HENSYU-SEC
                       END-PERFORM
      *
                       IF       SPA-C02-CHANGE-MAX     >   6
                           MOVE    7               TO  SPA-GMN-C02-CUR
                       ELSE
                           COMPUTE SPA-GMN-C02-CUR     =   1   +   IDZ
                       END-IF 
                   END-IF
      *
                   INITIALIZE                  SPA-C02-HAISI-AREA
                   MOVE    SPACE       TO  SPA-GMN-C02-BYOMEI
                   PERFORM 41021-BYOMEICD-HENSYU-SEC
                   MOVE    34              TO  SPA-GMN-C02-CUR
               WHEN    "0104"
               WHEN    "0106"
      *            自院病名コードの移行
               WHEN    "0109"
      *            自院病名コード廃止処理
               WHEN    "0108"
      *            自院病名コード未コード化病名処理
                   MOVE    SPACE               TO  
                                               SPA-NAI-C02-BYOMEI-CHG
                                                 (SPA-C02-IKOSAKI-INDEX)
      *
                   IF      SPA-CID6-FLG    =   "OK"
                       MOVE    SPA-C02-IKOSAKI-INDEX
                                               TO  IDZ
                       MOVE    SPA-C02-IKOSAKI-INDEX1
                                               TO  IDX2
                       MOVE    SPA-CID6-FLG    TO
                                       SPA-NAI-C02-BYOMEI-CHG  (IDZ)
                       INITIALIZE          SPA-NAI-C02-BYOMEI (IDZ)
                                           SPA-NAI-C02-US-BYOMEIHENFLG
                                                              (IDZ)
                                           SPA-NAI-C02-BYOMEICD-O
                                                             (IDZ IDX2)
                                           SPA-NAI-C02-BYOMEI-O
                                                             (IDZ IDX2)
                                           SPA-NAI-C02-HAISIYMD-O
                                                             (IDZ IDX2)
                                           SPA-NAI-C02-TANDOKUKBN-O
                                                             (IDZ IDX2)
      *
                       MOVE    SPA-C02-IKOSAKI-BYOMEICD
                                               TO
                                       SPA-NAI-C02-BYOMEICD-O(IDZ IDX2)
                                       BYO-BYOMEICD
                       PERFORM 900-BYOMEI-READ-SEC
                       IF      FLG-BYOMEI      =   ZERO
                           MOVE    BYO-BYOMEI      TO
                                       SPA-NAI-C02-BYOMEI-O (IDZ IDX2)
                           MOVE    BYO-HAISIYMD    TO
                                       SPA-NAI-C02-HAISIYMD (IDZ)
                                       SPA-NAI-C02-HAISIYMD-O(IDZ IDX2)
                           MOVE    BYO-TOKSKNCD    TO
                                       SPA-NAI-C02-TOKSKNCD (IDZ)
                           MOVE    BYO-NANBYOCD    TO
                                       SPA-NAI-C02-NANBYOCD-O(IDZ)
                           MOVE    BYO-TANDOKUKBN  TO
                                       SPA-NAI-C02-TANDOKUKBN-O
                                                             (IDZ IDX2)
                       END-IF
                       MOVE    SPA-NAI-C02-BYOMEI-O (IDZ 1)
                                               TO
                                           SPA-NAI-C02-BYOMEI (IDZ)
                       PERFORM VARYING IDX2    FROM    2   BY  1
                               UNTIL   IDX2    >       21
                               OR      SPA-NAI-C02-BYOMEICD-O (IDZ IDX2)
                                                   =   SPACE
                           STRING   SPA-NAI-C02-BYOMEI (IDZ)
                                               DELIMITED  BY  SPACE
                                    SPA-NAI-C02-BYOMEI-O (IDZ IDX2)
                                               DELIMITED  BY  SPACE
                                        INTO    SPA-NAI-C02-BYOMEI (IDZ)
                           END-STRING
                       END-PERFORM        
                   END-IF
                   IF      SPA-CID6-FLG    =   "CHG"
                       PERFORM VARYING IDXX    FROM
                                       SPA-C02-CHANGE-INDEX-ST   BY  1
                               UNTIL   IDXX    >
                                       SPA-C02-CHANGE-INDEX-ED
      ****                 INITIALIZE      SPA-GMN-C02-BYOMEICD (IDXX)
      *??                  INITIALIZE      SPA-NAI-C02-BYOMEI-G (IDXX)
                           INITIALIZE      SPA-NAI-C02-BYOMEI   (IDXX)
                                           SPA-NAI-C02-US-BYOMEIHENFLG
                                                                (IDXX)
                                           SPA-NAI-C02-TOKSKNCD (IDXX)
                                           SPA-NAI-C02-NANBYOCD-O
                                                                (IDXX)
                           PERFORM VARYING IDZ     FROM   1   BY  1
                                   UNTIL   IDZ     >      21
                               INITIALIZE  SPA-NAI-C02-BYOMEICD-O
                                                             (IDXX IDZ)
                                           SPA-NAI-C02-BYOMEI-O
                                                             (IDXX IDZ)
                                           SPA-NAI-C02-HAISIYMD-O
                                                             (IDXX IDZ)
                                           SPA-NAI-C02-TANDOKUKBN-O
                                                             (IDXX IDZ)
                           END-PERFORM
      *??
                           MOVE    SPA-CID6-FLG    TO
                                           SPA-NAI-C02-BYOMEI-CHG (IDXX)
                       END-PERFORM
                       MOVE    SPA-C02-CHANGE-INDEX-ST
                                               TO  IDZ
                       PERFORM VARYING IDXX    FROM   1   BY  1
                               UNTIL   IDXX    >      SPA-C02-CHANGE-MAX
                           MOVE    SPA-C02-CHANGE-BYOMEICD (IDXX)
                                           TO 
                                       SPA-NAI-C02-BYOMEICD-O(IDZ IDXX)
                                       BYO-BYOMEICD
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      FLG-BYOMEI      =   ZERO
                               MOVE    BYO-BYOMEI      TO
                                       SPA-NAI-C02-BYOMEI-O (IDZ IDXX)
                               MOVE    BYO-HAISIYMD    TO
                                       SPA-NAI-C02-HAISIYMD (IDZ)
                                       SPA-NAI-C02-HAISIYMD-O(IDZ IDXX)
                               MOVE    BYO-TOKSKNCD    TO
                                       SPA-NAI-C02-TOKSKNCD (IDZ)
                               MOVE    BYO-NANBYOCD    TO
                                       SPA-NAI-C02-NANBYOCD-O(IDZ)
                               MOVE    BYO-TANDOKUKBN  TO
                                       SPA-NAI-C02-TANDOKUKBN-O
                                                             (IDZ IDXX)
                           END-IF
                       END-PERFORM 
                       MOVE    SPA-NAI-C02-BYOMEI-O (IDZ 1)
                                               TO
                                           SPA-NAI-C02-BYOMEI (IDZ)
                       PERFORM VARYING IDX2    FROM    2   BY  1
                               UNTIL   IDX2    >       21
                               OR      SPA-NAI-C02-BYOMEICD-O
                                                           (IDZ IDX2)
                                               =   SPACE
                           STRING   SPA-NAI-C02-BYOMEI (IDZ)
                                               DELIMITED  BY  SPACE
                                    SPA-NAI-C02-BYOMEI-O (IDZ IDX2)
                                               DELIMITED  BY  SPACE
                                       INTO    SPA-NAI-C02-BYOMEI (IDZ)
                           END-STRING
                       END-PERFORM
                   END-IF
      *
                   IF      SPA-C02-BYOSET-INDEX-MAX
                                           >   ZERO
                       INITIALIZE              SPA-CIDCD-AREA
                                               SPA-CID1-AREA
                       INITIALIZE              SPA-C02-IKOSAKI-AREA
                                               SPA-C02-CHANGE-AREA
                                               SPA-C02-HAISI-AREA
                                               SPA-ERRCD
                                               WRK-HAISIYMDH
      *
      *                病名入力セットから自院病名編集処理
                       PERFORM 410261-BYOMEIINPUTSET-HEN-SEC
                       IF    ( SPA-CIDCD2      NOT =   SPACE )
                       OR    ( SPA-CIDCD6      NOT =   SPACE )
                       OR    ( SPA-ERRCD       NOT =   SPACE )
                           GO  TO  370-CID6-SET-EXT
                       END-IF
                   ELSE
                       COMPUTE SPA-GMN-C02-CUR =   SPA-C02-IKOSAKI-INDEX
                                               +   1
                   END-IF
      * 
                   INITIALIZE                  SPA-C02-HAISI-AREA
                   MOVE    SPACE       TO  SPA-GMN-C02-BYOMEI
                   PERFORM 41021-BYOMEICD-HENSYU-SEC
           END-EVALUATE
      *     
           INITIALIZE                  SPA-CIDCD-AREA
                                       SPA-CID1-AREA
           INITIALIZE                  SPA-C02-IKOSAKI-AREA
                                       SPA-C02-CHANGE-AREA
                                       SPA-C02-HAISI-AREA
           .
       370-CID6-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    "CHANGE"    TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 220-CLEAR-SEC
      *    前回の患者番号
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 230-MAEKPTNUM-SEC
      *    病名検索へ
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 600-C05-SYORI-SEC
      *    前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-MAEGMN-SEC
      *    次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 470-ATOGMN-SEC
      *    削除
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 440-SAKUJYO-GID-SEC
      *    氏名検索
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 480-KENSAKU-SEC
      *    予約登録へ
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 480-YOYAKU-SEC
      *    受付一覧へ
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-UKEICHI-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-TOUROKU-SEC
      *    自院病名検索
               WHEN    "CLICKED"       ALSO    "B04"
                   MOVE    ZERO                TO  WRK-BUNRUICD 
                   PERFORM 490-USERBYOMEI-KENSAKU-SEC
      *    セット登録
               WHEN    "CLICKED"       ALSO    "B15"
                   PERFORM 600-C06-SYORI-SEC
      *    入力ＣＤへ
               WHEN    "CLICKED"       ALSO    "B16"
                   PERFORM 600-C03-SYORI-SEC
      *    コメント処理
               WHEN    "CLICKED"       ALSO    "B17"
                   PERFORM 430-FREEMSG-SEC
      *    手術歴登録処理
               WHEN    "CLICKED"       ALSO    "B18"
                   PERFORM 430-OPECOM-SEC
      *    削除病名一覧表示
               WHEN    "CLICKED"       ALSO    "B20"
                   PERFORM 430-DELETE-LIST-SEC
      *    入外区分表示切替え処理
               WHEN    "CLICKED"       ALSO    "B23"
                   PERFORM 430-HNYUGAIKBN-CHANGE-SEC
      *    表示切替え処理
               WHEN    "CLICKED"       ALSO    "B21"
                   PERFORM 430-SYOKBN-CHANGE-SEC
      *    コード表示
               WHEN    "CLICKED"       ALSO    "B22"
                   PERFORM 490-C09-SYORI-SEC
      *    ↑
               WHEN    "CLICKED"       ALSO    "BYOMEI_UP"
                   PERFORM 490-BYOMEI-UP-SEC
      *    ↓
               WHEN    "CLICKED"       ALSO    "BYOMEI_DOWN"
                   PERFORM 490-BYOMEI-DOWN-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力箇所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-C02-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        病名行選択
               WHEN    ANY             ALSO    "BYOMEILST"
                   PERFORM 41020-BYOMEISEL-SEC
               WHEN    "ACTIVATE"      ALSO    "PTNUM"
                   PERFORM 41015-PTNUM-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SRYKA"
                   PERFORM 41016-SRYKA-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SYOKBN"
                   PERFORM 41022-SYOKBN-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "HNYUGAIKBN"
                   PERFORM 41023-HNYUGAIKBN-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "BANGO"
                   PERFORM 41011-BANGO-HEN-SEC
               WHEN    "ACTIVATE"      ALSO    "BSRYKA"
                   PERFORM 41019-BSRYKA-CHK-SEC
               WHEN    OTHER
                   PERFORM 400-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       400-INPUT-CHK-SEC              SECTION.
      *
      *    複数病名入力時のチェック（更新時以外）
           IF      FLG-TOUROKU         =   ZERO
      *        病名コード入力時はチェックしない
               EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
                   WHEN    "ACTIVATE"      ALSO    "BYOMEICD_1"
                   WHEN    "ACTIVATE"      ALSO    "BYOMEICD_2"
                   WHEN    "ACTIVATE"      ALSO    "BYOMEICD_3"
                   WHEN    "ACTIVATE"      ALSO    "BYOMEICD_4"
                   WHEN    "ACTIVATE"      ALSO    "BYOMEICD_5"
                   WHEN    "ACTIVATE"      ALSO    "BYOMEICD_6"
                       CONTINUE
                   WHEN    OTHER
                       IF      SPA-NAI-C02-BYOMEICD-MAX    >   1
                           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
                               WHEN    "ACTIVATE"  ALSO    "CHARTBYOMEI"
                                   MOVE    "0060"  TO  SPA-ERRCD
                                   MOVE    28      TO  SPA-GMN-C02-CUR
                                   GO  TO  400-INPUT-CHK-EXT
                               WHEN    OTHER
                                   IF    ( C02-HOSOKUCD (1)
                                                       NOT =
                                               SPA-GMN-C02-HOSOKUCD(1) )
                                   OR    ( C02-HOSOKUCD (2)
                                                       NOT =
                                               SPA-GMN-C02-HOSOKUCD(2) )
                                   OR    ( C02-HOSOKUCD (3)
                                                       NOT =
                                               SPA-GMN-C02-HOSOKUCD(3) )
                                   OR    ( C02-HOSOKU-COMMENT
                                                       NOT =
                                           SPA-GMN-C02-HOSOKU-COMMENT  )
                                   OR    ( C02-UTAGAIFLG
                                                       NOT =
                                               SPA-GMN-C02-UTAGAIFLG-G )
                                   OR    ( C02-SYUBYOFLG
                                                       NOT =
                                               SPA-GMN-C02-SYUBYOFLG-G )
                                   OR    ( C02-MANSEIFLG
                                                       NOT =
                                               SPA-GMN-C02-MANSEIKBN-G )
                                   OR    ( C02-NYUGAIKBN
                                                       NOT =
                                               SPA-GMN-C02-NYUGAIKBN-G )
                                   OR    ( C02-REZEFLG
                                                       NOT =
                                               SPA-GMN-C02-REZEFLG-G   )
                                   OR    ( C02-HKNBYOMEIFLG
                                                       NOT =
                                           SPA-GMN-C02-HKNBYOMEIFLG-G  )
                                   OR    ( C02-DISCHARGEFLG
                                                       NOT =
                                           SPA-GMN-C02-DISCHARGEFLG-G  )
                                   OR    ( C02-CLASS1  NOT =
                                               SPA-GMN-C02-CLASS1-G    )
                                   OR    ( C02-CLASS2  NOT =
                                               SPA-GMN-C02-CLASS2-G    )
                                       MOVE    "0060"  TO  SPA-ERRCD
                                       MOVE    2       TO
                                                       SPA-GMN-C02-CUR
                                       GO  TO  400-INPUT-CHK-EXT
                                   END-IF
                           END-EVALUATE
                       END-IF
               END-EVALUATE
           END-IF
      *
      *    移動処理時の入力チェック
           IF      SPA-NAI-C02-UPDOWN  =   1
      ******** IF      C02-BSRYKA      NOT =   SPA-GMN-C02-BSRYKA-G
      ********     MOVE    "0087"              TO  SPA-ERRCD
      ********     MOVE    2                   TO  SPA-GMN-C02-CUR
      ********     GO  TO  400-INPUT-CHK-EXT
      ******** END-IF
               IF      C02-SRYYMD      NOT =   SPA-GMN-C02-SRYYMD
                   MOVE    "0087"              TO  SPA-ERRCD
                   MOVE    17                  TO  SPA-GMN-C02-CUR
                   GO  TO  400-INPUT-CHK-EXT
               END-IF
           END-IF
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    病名コード、病名の入力チェック
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "BYOMEICD_1"
               WHEN    "ACTIVATE"      ALSO    "BYOMEICD_2"
               WHEN    "ACTIVATE"      ALSO    "BYOMEICD_3"
               WHEN    "ACTIVATE"      ALSO    "BYOMEICD_4"
               WHEN    "ACTIVATE"      ALSO    "BYOMEICD_5"
               WHEN    "ACTIVATE"      ALSO    "BYOMEICD_6"
                   PERFORM 4102-BYOMEICD-CHK-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                   OR    ( SPA-CIDCD2      NOT =   SPACE )
                   OR    ( SPA-CIDCD6      NOT =   SPACE )
                       CONTINUE
                   ELSE
                       MOVE    SPA-GMN-C02-HOSOKU-COMMENT
                                               TO  C02-HOSOKU-COMMENT
                       MOVE    SPA-GMN-C02-HOSOKUCD (1)
                                               TO  C02-HOSOKUCD (1)
                       MOVE    SPA-GMN-C02-HOSOKUCD (2)
                                               TO  C02-HOSOKUCD (2)
                       MOVE    SPA-GMN-C02-HOSOKUCD (3)
                                               TO  C02-HOSOKUCD (3)
                   END-IF
               WHEN    "ACTIVATE"      ALSO    "BYOMEI"
                   PERFORM 41013-BYOMEI-CHK-SEC
               WHEN    OTHER
                   IF    ( C02-BYOMEI  NOT =   SPA-GMN-C02-BYOMEI      )
                   AND   ( C02-BYOMEICD (1)
                                           =   SPA-GMN-C02-BYOMEICD (1))
                   AND   ( C02-BYOMEICD (2)
                                           =   SPA-GMN-C02-BYOMEICD (2))
                   AND   ( C02-BYOMEICD (3)
                                           =   SPA-GMN-C02-BYOMEICD (3))
                   AND   ( C02-BYOMEICD (4)
                                           =   SPA-GMN-C02-BYOMEICD (4))
                   AND   ( C02-BYOMEICD (5)
                                           =   SPA-GMN-C02-BYOMEICD (5))
                   AND   ( C02-BYOMEICD (6)
                                           =   SPA-GMN-C02-BYOMEICD (6))
                       PERFORM 41013-BYOMEI-CHK-SEC
                   END-IF
           END-EVALUATE
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
           OR    ( SPA-CIDCD2      NOT =   SPACE )
           OR    ( SPA-CIDCD6      NOT =   SPACE )
               GO      TO      400-INPUT-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-C02-CUR     TO  WRK-GMN-C02-CUR
      *
      *    補足コメントの入力チェック
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "HOSOKUCD_1"
               WHEN    "ACTIVATE"      ALSO    "HOSOKUCD_2"
               WHEN    "ACTIVATE"      ALSO    "HOSOKUCD_3"
                   MOVE    MCP-WIDGET      TO  WRK-WIDGET
                   PERFORM 41024-HOSOKUCD-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "HOSOKU_COMMENT"
                   PERFORM 41025-HOSOKU-COMMENT-CHK-SEC
               WHEN    OTHER
                   IF    ( C02-HOSOKU-COMMENT
                                       NOT =
                                           SPA-GMN-C02-HOSOKU-COMMENT  )
                   AND   ( C02-HOSOKUCD (1)
                                           =   SPA-GMN-C02-HOSOKUCD (1))
                   AND   ( C02-HOSOKUCD (2)
                                           =   SPA-GMN-C02-HOSOKUCD (2))
                   AND   ( C02-HOSOKUCD (3)
                                           =   SPA-GMN-C02-HOSOKUCD (3))
                       PERFORM 41025-HOSOKU-COMMENT-CHK-SEC
                   ELSE
                       IF    ( C02-HOSOKU-COMMENT
                                           =
                                           SPA-GMN-C02-HOSOKU-COMMENT  )
                       AND   ( C02-HOSOKUCD (1)
                                       NOT =   SPA-GMN-C02-HOSOKUCD (1)
                            OR C02-HOSOKUCD (2)
                                       NOT =   SPA-GMN-C02-HOSOKUCD (2)
                            OR C02-HOSOKUCD (3)
                                       NOT =   SPA-GMN-C02-HOSOKUCD (3))
                           MOVE    "HOSOKUCD_1"    TO  WRK-WIDGET
                           PERFORM 41024-HOSOKUCD-CHK-SEC
                       END-IF
                   END-IF
      *
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    WRK-GMN-C02-CUR     TO  SPA-GMN-C02-CUR
                   END-IF
           END-EVALUATE
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
           OR    ( SPA-CIDCD2      NOT =   SPACE )
           OR    ( SPA-CIDCD6      NOT =   SPACE )
               GO      TO      400-INPUT-CHK-EXT
           END-IF
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        保険組合せ行選択
               WHEN    ANY             ALSO    "HKNCOMBILST"
                   PERFORM 41021-HKNCOMBISEL-SEC
           END-EVALUATE
      *
      *    入力箇所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
           OR    ( SPA-CIDCD2      NOT =   SPACE )
           OR    ( SPA-CIDCD6      NOT =   SPACE )
               GO  TO  400-INPUT-CHK-EXT
           END-IF
      *
      *    関連チェック処理
           PERFORM 4103-KANREN-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
           OR    ( SPA-CIDCD2      NOT =   SPACE )
           OR    ( SPA-CIDCD6      NOT =   SPACE )
               GO  TO  400-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU         =   1
      *        更新前チェック処理
               PERFORM 4501-KEIKOKU-CHK-SEC
           END-IF
     *
           .
       400-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
      *****MOVE    SPA-GMN-C02-BSRYKA-G
      *****                            TO  SPA-GMN-C02-BSRYKA-G-OLD
           MOVE    SPA-GMN-C02-SRYYMD  TO  SPA-GMN-C02-SRYYMD-OLD
      *
      *****MOVE    C02-BSRYKA          TO  SPA-GMN-C02-BSRYKA-G
           MOVE    C02-SRYYMD          TO  SPA-GMN-C02-SRYYMD
      **** MOVE    C02-HOSOKU-COMMENT  TO  SPA-GMN-C02-HOSOKU-COMMENT
           MOVE    C02-CHARTBYOMEI     TO  SPA-GMN-C02-CHARTBYOMEI
           MOVE    C02-TENKIKBN        TO  SPA-GMN-C02-TENKIKBN-G
           MOVE    C02-TENKIYMD        TO  SPA-GMN-C02-TENKIYMD
           MOVE    C02-UTAGAIFLG       TO  SPA-GMN-C02-UTAGAIFLG-G
           MOVE    C02-SYUBYOFLG       TO  SPA-GMN-C02-SYUBYOFLG-G
           MOVE    C02-MANSEIFLG       TO  SPA-GMN-C02-MANSEIKBN-G
           MOVE    C02-NYUGAIKBN       TO  SPA-GMN-C02-NYUGAIKBN-G
           MOVE    C02-HKNCOMBI        TO  SPA-GMN-C02-HKNCOMBI-G
           MOVE    C02-KOHID           TO  SPA-GMN-C02-KOHID
           MOVE    C02-REZEFLG         TO  SPA-GMN-C02-REZEFLG-G
           MOVE    C02-REZEMM          TO  SPA-GMN-C02-REZEMM
           MOVE    C02-HKNBYOMEIFLG    TO  SPA-GMN-C02-HKNBYOMEIFLG-G
           MOVE    C02-DISCHARGEFLG    TO  SPA-GMN-C02-DISCHARGEFLG-G
           MOVE    C02-CLASS1          TO  SPA-GMN-C02-CLASS1-G
           MOVE    C02-CLASS2          TO  SPA-GMN-C02-CLASS2-G
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
           PERFORM 41012-SRYYMD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *****PERFORM 41019-BSRYKA-CHK-SEC
      *****IF      SPA-ERRCD       NOT =   SPACE
      *****    GO  TO  4102-KIHON-CHK-EXT
      *****END-IF
           PERFORM 41018-CHARTBYOMEI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41011-TENKIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41012-TENKIYMD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4104-UTAGAI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4105-SYUBYOFLG-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4106-MANSEI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4107-NYUGAIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4108-COMB-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41026-KOHID-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4109-REZEFLG-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41010-REZEMM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41014-HKNBYOMEIFLG-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41027-DISCHARGEFLG-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41028-CLASS1-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41029-CLASS2-CHK-SEC
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       4103-KANREN-CHK-SEC          SECTION.
      *
      *    開始日と転帰日
           IF      SPA-NAI-C02-TENKIYMD    =   SPACE
               CONTINUE
           ELSE
               IF      SPA-NAI-C02-SRYYMD  >   SPA-NAI-C02-TENKIYMD
                   MOVE    "0027"              TO  SPA-ERRCD
                   EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
                       WHEN    "ACTIVATE"      ALSO    "SRYYMD"
                           MOVE    9           TO  SPA-GMN-C02-CUR
                       WHEN    OTHER
                           MOVE    13          TO  SPA-GMN-C02-CUR
                   END-EVALUATE
                   GO  TO  4103-KANREN-CHK-EXT
               END-IF
           END-IF
      *
      *    開始日と保険組合せ
           IF      SPA-GMN-C02-HKNCOMBI    =   SPACE
               CONTINUE
           ELSE
               MOVE    SPA-GMN-C02-HKNCOMBI
                                           TO  WRK-KBN
               PERFORM 5001-HKNCOMBI-HENSYU-SEC
               IF      WRK-KBNMEI      NOT =   SPACE
                   IF      SPA-NAI-C02-SRYYMD
                                           <=   WRK-TEKEDYMD
                       CONTINUE
                   ELSE
                       MOVE    "0011"     TO  SPA-ERRCD
                       EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
                           WHEN    "ACTIVATE"      ALSO    "SRYYMD"
                               MOVE    9           TO  SPA-GMN-C02-CUR
                           WHEN    OTHER
                               MOVE    22          TO  SPA-GMN-C02-CUR
                       END-EVALUATE
                       GO  TO  4103-KANREN-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    開始日と第三者行為
           IF      SPA-GMN-C02-KOHID   =   SPACE
               CONTINUE
           ELSE
               MOVE    SPA-GMN-C02-KOHID   TO  WRK-KBNMEI
               MOVE    ZERO                TO  WRK-KOHID
               PERFORM 5001-KOHID-HENSYU-SEC
               IF      WRK-KOHID           >   ZERO
                   IF      SPA-NAI-C02-SRYYMD
                                               <=  WRK-RYOEDYMD
                       CONTINUE
                   ELSE    
                       MOVE    "0101"          TO  SPA-ERRCD
                       EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
                           WHEN    "ACTIVATE"      ALSO    "SRYYMD"
                               MOVE    9           TO  SPA-GMN-C02-CUR
                           WHEN    OTHER
                               MOVE    41          TO  SPA-GMN-C02-CUR
                       END-EVALUATE
                       GO  TO  4103-KANREN-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    保険組合せと第三者行為
           IF    ( SPA-NAI-C02-KOHID   >   ZERO  )
           AND   ( SPA-GMN-C02-HKNCOMBI
                                   NOT =   SPACE )
               MOVE    "0102"          TO  SPA-ERRCD
               EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
                   WHEN    "ACTIVATE"      ALSO    "KOHID"
                       MOVE    41              TO  SPA-GMN-C02-CUR
                   WHEN    OTHER
                       MOVE    22              TO  SPA-GMN-C02-CUR
               END-EVALUATE
               GO  TO  4103-KANREN-CHK-EXT
           END-IF
      *
      *    レセ電エラーフラグ（複数病名以外）
           IF      SPA-NAI-C02-BYOMEICD-MAX    =   1
               PERFORM 41041-RECEDENFLG-CHK-SEC
           ELSE
               MOVE    SPACE               TO  SPA-NAI-C02-RECEDENFLG
           END-IF
           .
       4103-KANREN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       41015-PTNUM-CHK-SEC            SECTION.
      *
           MOVE    26                  TO  SPA-GMN-C02-CUR
      *
           INITIALIZE                  SPA-GMN-C02-KOUMOKU
                                       SPA-NAI-C02-KOUMOKU
                                       SPA-GMN-C02-HEAD
                                       SPA-NAI-C02-HEAD
                                       SPA-NAI-C02-SELNO
                                       SPA-C02-UPDFLG
                                       SPA-C02-IKOFLG
                                       SPA-NAI-C02-UPDOWN
                                       SPA-GMN-C02-NEXT
      *                              
                                       SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-GMN-C02-MAX
      *                                 
                                       SPA-GMN-C02-HKN-TBL
                                       SPA-NAI-C02-HKN-TBL
                                       SPA-GMN-C02-HKNCOMBILST-G
                                       SPA-GMN-C02-HKN-MAX
                                       SPA-GMN-C02-HKNCOMBI-MAX
                                       SPA-PTBYOMEI-REC
      *
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
           EVALUATE    SPA-C02-1042-SYOKBN-DSPKBN
               WHEN    ZERO
                   MOVE    SPA-GMN-C02-SYOKBNLST (1)
                                           TO  SPA-GMN-C02-SYOKBN-G
               WHEN    "1"
                   MOVE    SPA-GMN-C02-SYOKBNLST (2)
                                           TO  SPA-GMN-C02-SYOKBN-G
               WHEN    "2"
                   MOVE    SPA-GMN-C02-SYOKBNLST (3)
                                           TO  SPA-GMN-C02-SYOKBN-G
           END-EVALUATE
           MOVE    SPA-GMN-C02-HNYUGAIKBNLST (1)
                                       TO  SPA-GMN-C02-HNYUGAIKBN-G
      *
           IF      SPA-GMN-C02-SRYKA-MAX   >   1
               INITIALIZE              SPA-GMN-C02-SRYKA-G
           ELSE
               MOVE    SPA-GMN-C02-SRYKALST (1)
                                       TO  SPA-GMN-C02-SRYKA-G
           END-IF                                      
      *
           MOVE    C02-PTNUM           TO  SPA-GMN-C02-PTNUM
      *
           INITIALIZE                  ORCSPTNUMAREA
           MOVE    SPA-GMN-C02-PTNUM   TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           EVALUATE    SPTNUM-RC
               WHEN    00
                   MOVE    SPTNUM-PTNUM        TO  SPTID-PTNUM
                   MOVE    SPTNUM-PTID         TO  SPTID-PTID
               WHEN   10
                   MOVE    "0031"              TO  SPA-ERRCD
               WHEN    20
      *            氏名検索へ
                   MOVE    SPACE               TO  LINKAREA
                   MOVE    SPA-KYOUTU          TO  LNK-COMMON
                   MOVE    SPA-C02             TO  LNK-SCRSPA
      *
                   MOVE    "C02"               TO  SPA-MOTOPG
                   MOVE    "P97"               TO  SPA-SAKIPG
      *            氏名セット
                   MOVE    SPA-GMN-C02-PTNUM   TO  SPA-NAME
                   MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
                   MOVE    "CHANGE"            TO  MCP-PUTTYPE
                   MOVE    "P97"               TO  MCP-WINDOW
                   PERFORM 900-PUT-WINDOW
                   MOVE    1                   TO  FLG-END
                   GO      TO     41015-PTNUM-CHK-EXT
               WHEN    OTHER
                   MOVE    "0031"          TO  SPA-ERRCD
           END-EVALUATE
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPTID-PTNUM         TO  SPA-GMN-C02-PTNUM
               MOVE    SPTID-PTID          TO  SPA-NAI-C02-PTID
      *
      *        患者マスター存在チェック
grpsys         MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    SPA-NAI-C02-PTID    TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF           NOT =   ZERO
                   MOVE    "0031"            TO  SPA-ERRCD
               ELSE
                   MOVE    PTINF-NAME        TO  SPA-GMN-C02-NAME
                   MOVE    PTINF-KANANAME    TO  SPA-GMN-C02-KANANAME
                   EVALUATE    PTINF-SEX
                       WHEN    "1" 
                           MOVE    "男"          TO  SPA-GMN-C02-SEX
                       WHEN    "2"
                           MOVE    "女"          TO  SPA-GMN-C02-SEX
                   END-EVALUATE
                   MOVE    PTINF-BIRTHDAY    TO  SPA-NAI-C02-BIRTHDAY
                                                 WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG       TO  SPA-GMN-C02-BIRTHDAY
                   MOVE    SPA-NAI-C02-PTID  TO  SPA-LAST-PTID
                   MOVE    SPA-GMN-C02-PTNUM TO  SPA-LAST-PTNUM 
      *            年齢計算
                   INITIALIZE                ORCSAGECHKAREA
grpsys             MOVE    SPA-HOSPNUM       TO  AGECHK-HOSPNUM
                   MOVE    SPA-NAI-C02-PTID  TO  AGECHK-PTID
                   MOVE    SPA-NAI-C02-SYSYMD
                                             TO  AGECHK-SRYYMD
                   MOVE    SPA-NAI-C02-BIRTHDAY
                                             TO  AGECHK-BIRTHDAY
                   CALL    "ORCSAGECHK"        USING
                                               ORCSAGECHKAREA
grpsys                                         SPA-AREA
                   MOVE    SPACE             TO  WRK-NENREI-G
                   IF      AGECHK-NENREI2-YY   =   ZERO
                       MOVE    AGECHK-NENREI2-MM   TO  WRK-NENREI-ZN
                       MOVE    "ヶ月"              TO  WRK-NENREI-MEIN
                   ELSE
                       MOVE    AGECHK-NENREI2-YY   TO  WRK-NENREI-Z
                       MOVE    "才"                TO  WRK-NENREI-MEI
                   END-IF
                   MOVE    WRK-NENREI-G        TO  SPA-GMN-C02-AGE
      *
      *            前回来院日
                   INITIALIZE                  ORCSLASTYMDAREA
                   MOVE    SPA-NAI-C02-PTID    TO  ORCSLASTYMD-PTID
                   CALL    "ORCSLASTYMD"       USING
                                               ORCSLASTYMDAREA
                                               SPA-AREA
      *            入院中は病棟を表示
                   IF      ORCSLASTYMD-BRMNUM  =   SPACE
                       IF      ORCSLASTYMD-LASTYMD NOT =   SPACE
                           STRING  "前回："    DELIMITED  BY  SIZE
                                   ORCSLASTYMD-LASTYMDW
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-GMN-C02-LASTYMD
                           END-STRING
                       END-IF
                   ELSE
                       STRING  "病棟："    DELIMITED  BY  SIZE
                               ORCSLASTYMD-BRMNUM
                                           DELIMITED  BY  SIZE
                                           INTO    SPA-GMN-C02-LASTYMD
                       END-STRING
                   END-IF
      *
      *            排他制御処理
                   PERFORM 990-LOCK-IN-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    2           TO  SPA-NAI-C02-LOCK
                       GO  TO  41015-PTNUM-CHK-EXT
                   END-IF
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 310-SPASET-HKNCOMBI-HENSYU-SEC
               PERFORM 310-SPASET-KOHID-HENSYU-SEC
               IF      SPA-GMN-C02-SRYKA-MAX   =   1
                   MOVE    1                   TO  SPA-GMN-C02-CUR
               ELSE
                   IF      SPA-C02-1042-SRYKA-DSPKBN
                                               =   ZERO
                       PERFORM 330-SPASET-JYURRK-HENSYU-SEC
                       MOVE    27              TO  SPA-GMN-C02-CUR
                   ELSE
                       MOVE    SPA-GMN-C02-SRYKALST (1)
                                               TO  SPA-GMN-C02-SRYKA-G
                       MOVE    1               TO  SPA-GMN-C02-CUR
                   END-IF
               END-IF 
      *
               IF    ( SPA-GMN-C02-SRYKA   NOT =   ZERO  )
               AND   ( SPA-GMN-C02-SRYKAMEI
                                           NOT =   SPACE )
                   MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
               END-IF
      *
               IF      SPA-GMN-C02-SRYKA
                                       NOT =   SPACE 
      *            病名一覧
                   MOVE    1                   TO  SPA-GMN-C02-PAGE
      *
                   PERFORM 310-SPASET-PTBYO-HENSYU-SEC
               END-IF
           END-IF    
           . 
       41015-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科入力処理
      *****************************************************************
       41016-SRYKA-CHK-SEC             SECTION.
      *
           MOVE    27                  TO  SPA-GMN-C02-CUR
      *
           INITIALIZE                  SPA-GMN-C02-KOUMOKU
                                       SPA-NAI-C02-KOUMOKU
                                       SPA-NAI-C02-SELNO
                                       SPA-C02-UPDFLG
                                       SPA-C02-IKOFLG
      *                              
                                       SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-GMN-C02-MAX
                                       SPA-GMN-C02-NEXT
                                       SPA-PTBYOMEI-REC
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
      *
           MOVE    C02-SRYKA           TO  SPA-GMN-C02-SRYKA-G
           MOVE    SPACE               TO  SPA-GMN-C02-SRYKA-G (3:)
      *
           MOVE    SPA-GMN-C02-SRYKA   TO  WRK-KBN
           PERFORM 5001-SRYKA-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-SRYKAMEI
           IF      WRK-KBNMEI          =   SPACE
               MOVE    "0030"              TO  SPA-ERRCD
           ELSE
               IF      SPA-GMN-C02-SRYKA   NOT =   ZERO
                   MOVE    SPA-GMN-C02-SRYKA-G
                                           TO  SPA-GMN-C02-BSRYKA-G
               END-IF
      *        病名一覧
               MOVE    1                   TO  SPA-GMN-C02-PAGE
      *
               PERFORM 310-SPASET-PTBYO-HENSYU-SEC 
               MOVE    1                   TO  SPA-GMN-C02-CUR
           END-IF    
           .
       41016-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       41011-BANGO-HEN-SEC         SECTION.
      *
           MOVE    1                   TO  SPA-GMN-C02-CUR
      *
           INITIALIZE                  SPA-GMN-C02-KOUMOKU
                                       SPA-NAI-C02-KOUMOKU
                                       SPA-NAI-C02-SELNO
                                       SPA-C02-IKOFLG
                                       SPA-PTBYOMEI-REC
      *                                
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
           IF    ( SPA-GMN-C02-SRYKA   NOT =   ZERO  )
           AND   ( SPA-GMN-C02-SRYKAMEI
                                       NOT =   SPACE )
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
      *
           IF      C02-BANGO       =   SPACE
               MOVE    ZERO            TO  SPA-GMN-C02-BANGO
               MOVE    2               TO  SPA-GMN-C02-CUR 
           ELSE
               IF      C02-BANGO (1:1) =   "."
                   PERFORM 410113-KANTAN-SYUSEI-SEC
                   MOVE    1                   TO  SPA-GMN-C02-CUR
                   INITIALIZE                  SPA-GMN-C02-KOUMOKU
                                               SPA-NAI-C02-KOUMOKU
                                               SPA-NAI-C02-SELNO
                                               SPA-C02-IKOFLG
                   MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
                   IF    ( SPA-GMN-C02-SRYKA   NOT =   ZERO  )
                   AND   ( SPA-GMN-C02-SRYKAMEI
                                               NOT =   SPACE )
                       MOVE    SPA-GMN-C02-SRYKA-G
                                               TO  SPA-GMN-C02-BSRYKA-G
                   END-IF
      *
      *            番号のみ指定してあるとき   
                   IF    ( WRK-HENSYU-DATA-G   =   SPACE )
                   AND   ( SPA-ERRCD           =   SPACE )
                       MOVE    C02-BANGO       TO  SPA-GMN-C02-BANGO-X
                       INITIALIZE                  SPA-C01KYOUTU
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       5
                           MOVE    WRK-STNUM-O (IDX)   TO
                                                   SPA-C01-STNUM-O (IDX)
                           MOVE    WRK-EDNUM-O (IDX)   TO
                                                   SPA-C01-EDNUM-O (IDX)
                       END-PERFORM
      *
                       MOVE    SPACE               TO  SPA-MOTOPG
                       MOVE    SPA-C01KYOUTU       TO  SPA-WORK
                       MOVE    SPA-AREA            TO  SPA-COMMON
                       MOVE    SPA-C02FREE         TO  SPA-FREE
      *
                       MOVE    "LINK"              TO  MCP-STATUS
                       MOVE    SPACE               TO  MCP-EVENT
      *
                       CALL    "ORCGC10"           USING
                                                   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA
      *  
                       MOVE    SPA-COMMON          TO  SPA-AREA
                       MOVE    SPA-WORK            TO  SPA-C01KYOUTU
                       MOVE    SPA-FREE            TO  SPA-C02FREE
      *
                       MOVE    "TENKIKBN"          TO  MCP-WIDGET
      *
                       MOVE    "C02"               TO  SPA-MOTOPG
                       MOVE    "C10"               TO  SPA-SAKIPG
      *
                       MOVE    "NEW"               TO  MCP-PUTTYPE
                       MOVE    "C10"               TO  MCP-WINDOW
      *
                       PERFORM 900-PUT-WINDOW
      *
                       MOVE    1                    TO  FLG-END
                       GO  TO  41011-BANGO-HEN-EXT              
                   END-IF
      *
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    C02-BANGO       TO  SPA-GMN-C02-BANGO-X
                   END-IF
               ELSE
                   INITIALIZE                  ORCSNUMAREA
                   MOVE    C02-BANGO       TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF      SNUM-RC             =   ZERO
                       MOVE    SNUM-OUTNUM         TO  SPA-GMN-C02-BANGO
      *
                       PERFORM VARYING     IDX     FROM    1   BY  1
                               UNTIL       IDX     >   SPA-GMN-C02-MAX
                           IF      SPA-GMN-C02-BANGO
                                           =   SPA-GMN-C02-TBANGO (IDX)
                               MOVE    IDX         TO  SPA-NAI-C02-SELNO
                               MOVE    SPA-GMN-C02-MAX
                                                   TO  IDX
                           END-IF    
                       END-PERFORM
      *
                       IF      SPA-NAI-C02-SELNO   =   ZERO
                           MOVE    ZERO            TO  SPA-GMN-C02-BANGO
                           MOVE    "0012"          TO  SPA-ERRCD
                       ELSE
                           PERFORM 410111-BYOMEI-SELECT-SEC     
                       END-IF
                   ELSE
                       MOVE    "0012"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       41011-BANGO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    簡単修正処理
      *****************************************************************
       410113-KANTAN-SYUSEI-SEC         SECTION.
      *
           MOVE    SPACE           TO  WRK-ERR-BYOMEI
                                       WRK-ERR-SRYYMD
                                       WRK-ERR-SRYKA
      *
           INITIALIZE                   WRK-KANTAN-HENSYU-AREA
      *
           UNSTRING    C02-BANGO (2:)   DELIMITED  BY  "/"
                                        INTO    WRK-HENSYU-NUM
                                                WRK-HENSYU-DATA-O (1)
                                                WRK-HENSYU-DATA-O (2)
                                                WRK-HENSYU-DATA-O (3)
                                                WRK-HENSYU-DATA-O (4)
                                                WRK-HENSYU-DATA-O (5)
                                                WRK-HENSYU-DATA-O (6)
                                                WRK-HENSYU-DATA-O (7)
               ON  OVERFLOW
                   MOVE    "0082"      TO  SPA-ERRCD  
                   GO  TO  410113-KANTAN-SYUSEI-EXT
           END-UNSTRING
      *
      *    入力がないとき
           IF    ( WRK-HENSYU-NUM      =   SPACE )
               MOVE    "0080"          TO  SPA-ERRCD
               GO  TO  410113-KANTAN-SYUSEI-EXT
           END-IF
      *
      *    病名番号の入力数
           UNSTRING    WRK-HENSYU-NUM  DELIMITED   BY  "," OR  "."
                                       INTO    WRK-HENSYU-NUM-O (1)
                                               WRK-HENSYU-NUM-O (2)
                                               WRK-HENSYU-NUM-O (3)
                                               WRK-HENSYU-NUM-O (4)
                                               WRK-HENSYU-NUM-O (5)
               ON  OVERFLOW
                   MOVE    "0081"      TO  SPA-ERRCD  
                   GO  TO  410113-KANTAN-SYUSEI-EXT
           END-UNSTRING
      *
      *    入力番号チェック
           IF      WRK-HENSYU-NUM-O (1)    =   "*"
               IF    ( WRK-HENSYU-NUM-O (2)    NOT =   SPACE )
               OR    ( WRK-HENSYU-NUM-O (3)    NOT =   SPACE )
               OR    ( WRK-HENSYU-NUM-O (4)    NOT =   SPACE )
               OR    ( WRK-HENSYU-NUM-O (5)    NOT =   SPACE )
                   MOVE    "0081"      TO  SPA-ERRCD  
                   GO  TO  410113-KANTAN-SYUSEI-EXT
               END-IF
               IF      SPA-GMN-C02-SYOKBN  =   "0" OR  "1"
                   MOVE    1               TO  WRK-STNUM-O (1)
                   MOVE    SPA-GMN-C02-MAX TO  WRK-EDNUM-O (1)
               ELSE
                   MOVE    "0083"          TO  SPA-ERRCD
                   GO  TO  410113-KANTAN-SYUSEI-EXT
               END-IF
           ELSE
             PERFORM VARYING IDXX    FROM    1   BY  1
                     UNTIL   IDXX    >       5
                     OR      SPA-ERRCD   NOT =   SPACE
               MOVE    SPACE               TO  WRK-STNUM
                                               WRK-EDNUM
               IF      WRK-HENSYU-NUM-O (IDXX) =   SPACE
                   CONTINUE
               ELSE
                   UNSTRING    WRK-HENSYU-NUM-O (IDXX)
                                           DELIMITED   BY  "-"
                                           INTO    WRK-STNUM
                                                   WRK-EDNUM
                   END-UNSTRING
      *
                   IF    ( WRK-STNUM           =   SPACE )
                   AND   ( WRK-EDNUM           =   SPACE )
                       MOVE    "0012"          TO  SPA-ERRCD
                       GO  TO  410113-KANTAN-SYUSEI-EXT
                   END-IF
                   IF      WRK-STNUM       NOT =   SPACE
                       INITIALIZE              ORCSNUMAREA
                       MOVE    WRK-STNUM       TO  SNUM-INX
                       CALL    "ORCSNUM"           USING
                                                   ORCSNUMAREA
                       IF    ( SNUM-RC         =   ZERO  )
                       AND   ( SNUM-MINKBN     =   SPACE )
                       AND   ( SNUM-SYOKBN     =   SPACE ) 
                           MOVE    SNUM-OUTNUM     TO  WRK-BANGO
      *
                           PERFORM VARYING IDX     FROM    1   BY  1
                                   UNTIL   IDX     >   SPA-GMN-C02-MAX
                               IF      WRK-BANGO
                                           =   SPA-GMN-C02-TBANGO (IDX)
                                   MOVE    IDX     TO  WRK-STNUM-O(IDXX)
                                   MOVE    SPA-GMN-C02-MAX
                                                   TO  IDX
                               END-IF
                           END-PERFORM
      *
                           IF      WRK-STNUM-O(IDXX)   =   ZERO
                               MOVE    "0012"          TO  SPA-ERRCD
                           ELSE
                               IF      WRK-EDNUM       =   SPACE
                                   MOVE    WRK-STNUM-O(IDXX)
                                                   TO  WRK-EDNUM-O(IDXX)
                               END-IF
                           END-IF
                       ELSE
                           MOVE    "0012"          TO  SPA-ERRCD
                           GO  TO  410113-KANTAN-SYUSEI-EXT
                       END-IF
                   END-IF
                   IF    ( WRK-EDNUM       NOT =   SPACE )
                   AND   ( SPA-ERRCD           =   SPACE )
                       INITIALIZE              ORCSNUMAREA
                       MOVE    WRK-EDNUM       TO  SNUM-INX
                       CALL    "ORCSNUM"           USING
                                                   ORCSNUMAREA
                       IF    ( SNUM-RC         =   ZERO  )
                       AND   ( SNUM-MINKBN     =   SPACE )
                       AND   ( SNUM-SYOKBN     =   SPACE ) 
                           MOVE    SNUM-OUTNUM     TO  WRK-BANGO
      *
                           PERFORM VARYING IDX     FROM    1   BY  1
                                   UNTIL   IDX     >   SPA-GMN-C02-MAX
                               IF      WRK-BANGO
                                           =   SPA-GMN-C02-TBANGO (IDX)
                                   MOVE    IDX     TO  WRK-EDNUM-O(IDXX)
                                   MOVE    SPA-GMN-C02-MAX
                                                   TO  IDX
                               END-IF
                           END-PERFORM
      *
                           IF      WRK-EDNUM-O(IDXX)   =   ZERO
                               MOVE    "0012"          TO  SPA-ERRCD
                           ELSE
                               IF      WRK-STNUM       =   SPACE
                                   MOVE    WRK-EDNUM-O(IDXX)
                                                   TO  WRK-STNUM-O(IDXX)
                               END-IF
                           END-IF
                       ELSE
                           MOVE    "0012"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
             END-PERFORM
             IF      SPA-ERRCD     NOT =   SPACE    
                 GO  TO  410113-KANTAN-SYUSEI-EXT
             END-IF
           END-IF
      *   
           IF    ( WRK-HENSYU-DATA-G   =   SPACE )
               GO  TO  410113-KANTAN-SYUSEI-EXT
           END-IF
      *
      *    転帰区分・転帰日チェック
           IF    ( WRK-HENSYU-DATA-O (1)   =   "-"   )
           AND   ( WRK-HENSYU-DATA-O (2)   =   SPACE )
               MOVE    "-"                     TO  WRK-HENSYU-DATA-O (2)
           END-IF
      *
           IF      WRK-HENSYU-DATA-O (1)   =   SPACE OR  "-"
               IF      WRK-HENSYU-DATA-O (2)   NOT =   SPACE   AND "-"
                   IF      WRK-HENSYU-DATA-O (1)   =   SPACE
                       MOVE    "0037"              TO  SPA-ERRCD
                   ELSE
                       MOVE    "0025"              TO  SPA-ERRCD
                   END-IF
               END-IF
           ELSE
               IF      WRK-HENSYU-DATA-O (1) (2:)
                                           NOT =   SPACE
                   MOVE    "0024"              TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (1)
                                               TO  WRK-KBN        
                   PERFORM 5001-TENKIKBN-HENSYU-SEC
                   IF      WRK-KBNMEI          =   SPACE
                       MOVE    "0024"              TO  SPA-ERRCD
                   ELSE
                       IF      WRK-HENSYU-DATA-O (2)   =   SPACE
                           MOVE    SPA-NAI-C02-SYSYMD  TO
                                               WRK-HENSYU-DATA-O (2)
                       END-IF
                   END-IF
               END-IF 
           END-IF
           IF      SPA-ERRCD           NOT =   SPACE
               GO  TO  410113-KANTAN-SYUSEI-EXT
           END-IF
      *
           IF      WRK-HENSYU-DATA-O (2)   =   SPACE OR  "-"
               IF      WRK-HENSYU-DATA-O (1)   NOT =   SPACE   AND "-"
                   MOVE    "0036"              TO  SPA-ERRCD
                   GO  TO  410113-KANTAN-SYUSEI-EXT
               END-IF
               IF      WRK-HENSYU-DATA-O (2)   =   "-"
                   MOVE    "00000000"          TO  WRK-HENSYU-DATA-O (2)
               END-IF
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               MOVE    WRK-HENSYU-DATA-O (2) 
                                           TO  SGDAY-INDAY
               MOVE    SPA-NAI-C02-SYSYMD  TO  SGDAY-SYSYMD
               MOVE    "1"                 TO  SGDAY-SYSKBN
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   IF      WRK-HENSYU-DATA-O (1)   =   SPACE
                       MOVE    "0037"          TO  SPA-ERRCD
                   END-IF    
               ELSE
                   MOVE    "0016"          TO  SPA-ERRCD
               END-IF
               IF      SPA-ERRCD       NOT =   SPACE       
                   GO  TO  410113-KANTAN-SYUSEI-EXT
               END-IF
               MOVE    SGDAY-SDAY          TO  WRK-HENSYU-DATA-O (2)
           END-IF
      *
      *    疑いフラグチェック
           IF      WRK-HENSYU-DATA-O (3)   =   SPACE   OR  "-"
               CONTINUE
           ELSE
               IF      WRK-HENSYU-DATA-O (3) (2:)
                                           NOT =   SPACE
                   MOVE    "0018"              TO  SPA-ERRCD
                   GO  TO  410113-KANTAN-SYUSEI-EXT
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (3)   TO  WRK-KBN
                   PERFORM 5001-UTAGAIFLG-HENSYU-SEC
                   IF      WRK-KBNMEI          =   SPACE
                       MOVE    "0018"              TO  SPA-ERRCD
                       GO  TO  410113-KANTAN-SYUSEI-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    主病名フラグチェック
           IF      WRK-HENSYU-DATA-O (4)   =   SPACE   OR  "-"
               CONTINUE
           ELSE
               IF      WRK-HENSYU-DATA-O (4) (2:)
                                           NOT =   SPACE
                   MOVE    "0019"              TO  SPA-ERRCD
                   GO  TO  410113-KANTAN-SYUSEI-EXT
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (4)   TO  WRK-KBN
                   PERFORM 5001-UTAGAIFLG-HENSYU-SEC
                   IF      WRK-KBNMEI          =   SPACE
                       MOVE    "0019"              TO  SPA-ERRCD
                       GO  TO  410113-KANTAN-SYUSEI-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    疾患区分チェック
           IF      WRK-HENSYU-DATA-O (5)   =   SPACE   OR  "-"
               CONTINUE
           ELSE
               IF      WRK-HENSYU-DATA-O (5) (3:)
                                           NOT =   SPACE
                   MOVE    "0021"              TO  SPA-ERRCD
                   GO  TO  410113-KANTAN-SYUSEI-EXT
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (5)   TO  WRK-KBN
                   PERFORM 5001-MANSEIKBN-HENSYU-SEC
                   IF      WRK-KBNMEI          =   SPACE
                       MOVE    "0021"              TO  SPA-ERRCD
                       GO  TO  410113-KANTAN-SYUSEI-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    入外区分チェック
           IF      WRK-HENSYU-DATA-O (6)   =   SPACE   OR  "-"
               CONTINUE
           ELSE
               IF      WRK-HENSYU-DATA-O (6) (2:)
                                           NOT =   SPACE
                   MOVE    "0022"              TO  SPA-ERRCD
                   GO  TO  410113-KANTAN-SYUSEI-EXT
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (6)   TO  WRK-KBN
                   PERFORM 5001-NYUGAIKBN-HENSYU-SEC
                   IF      WRK-KBNMEI          =   SPACE
                       MOVE    "0022"              TO  SPA-ERRCD
                       GO  TO  410113-KANTAN-SYUSEI-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    保険適用チェック
           IF      WRK-HENSYU-DATA-O (7)   =   SPACE   OR  "-"
               CONTINUE
           ELSE
               IF      WRK-HENSYU-DATA-O (7) (5:)
                                           NOT =   SPACE
                   MOVE    "0009"              TO  SPA-ERRCD
                   GO  TO  410113-KANTAN-SYUSEI-EXT
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (7)   TO  WRK-KBN
                   PERFORM 5001-HKNCOMBI-HENSYU-SEC
                   IF      WRK-KBNMEI      =   SPACE
                       MOVE    "0009"               TO  SPA-ERRCD
                       GO  TO  410113-KANTAN-SYUSEI-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    転帰・保険適用と開始日のチェック
           IF    ( WRK-HENSYU-DATA-O (1)   NOT =   SPACE )
           OR    ( WRK-HENSYU-DATA-O (6)   NOT =   SPACE )
           OR    ( WRK-HENSYU-DATA-O (7)   NOT =   SPACE )
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       5
                   IF      WRK-STNUM-O (IDXX)  >   ZERO
                       PERFORM VARYING IDX     FROM
                                               WRK-STNUM-O (IDXX)  BY  1
                               UNTIL   IDX     >   WRK-EDNUM-O (IDXX)
                           PERFORM 4101132-KANTAN-DABURI-CHK-SEC
                           IF      SPA-ERRCD   NOT =   SPACE
                               GO  TO  410113-KANTAN-SYUSEI-EXT
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF
      *
      *    慢性と急性の警告チェック
           IF    ( SPA-ERRCD            =   SPACE       )
           AND   ( WRK-HENSYU-DATA-O(3) =   "2" OR  "3" )
               MOVE    SPACE           TO  WRK-ERR-BYOMEI
                                           WRK-ERR-SRYYMD
                                           WRK-ERR-SRYKA
               INITIALIZE                  SPA-C02-KEIKOKU-FLG-AREA
                                           SPA-C02-KEIKOKU-MSG-AREA
               PERFORM 410114-KEIKOKU-CHK-SEC
           END-IF
      *
      *    死亡区分の更新、他診療科の同一病名の警告チェック
           IF      SPA-ERRCD            =   SPACE
               MOVE    SPACE           TO  WRK-ERR-BYOMEI
                                           WRK-ERR-SRYYMD
                                           WRK-ERR-SRYKA
               INITIALIZE                  SPA-C02-KEIKOKU-FLG-AREA
                                           SPA-C02-KEIKOKU-MSG-AREA
      *
      *        警告チェック
               PERFORM 410115-KEIKOKU-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
           OR    ( SPA-CIDCD       NOT =   SPACE )
               IF      SPA-CIDCD       NOT =   SPACE
      *            修正項目をパラメタに設定     
                   INITIALIZE              SPA-C01-HENSYU-NUM-G
                                           SPA-C01-HENSYU-DATA-G
                   PERFORM VARYING IDX FROM    1   BY  1
                            UNTIL   IDX >       5
                       MOVE    WRK-STNUM-O (IDX)   TO
                                               SPA-C01-STNUM-O (IDX)
                       MOVE    WRK-EDNUM-O (IDX)   TO
                                               SPA-C01-EDNUM-O (IDX)
                   END-PERFORM
                   PERFORM VARYING IDX     FROM    1   BY  1
                           UNTIL   IDX     >       7
                       MOVE    WRK-HENSYU-DATA-O (IDX)
                                       TO  SPA-C01-HENSYU-DATA-O (IDX)
                   END-PERFORM
               END-IF
               GO  TO  410113-KANTAN-SYUSEI-EXT
           END-IF
      *
      *    簡単修正時の更新処理
           PERFORM 410113-KANTAN-KOUSHIN-SEC
      *
           INITIALIZE                  SPA-CIDCD-AREA
                                       SPA-CID1-AREA  
           INITIALIZE                  SPA-C02-KEIKOKU-FLG-AREA
                                       SPA-C02-KEIKOKU-MSG-AREA
           INITIALIZE                  SPA-C01-HENSYU-NUM-G
                                       SPA-C01-HENSYU-DATA-G
           .
       410113-KANTAN-SYUSEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    簡単修正時の更新処理
      *****************************************************************
       410113-KANTAN-KOUSHIN-SEC       SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    更新処理
           MOVE    ZERO                TO  FLG-TENKIKBN-CHK
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       5
               IF      WRK-STNUM-O (IDXX)  >   ZERO
                   MOVE    WRK-STNUM-O (IDXX)  TO  WRK-STNUM
                   MOVE    WRK-EDNUM-O (IDXX)  TO  WRK-EDNUM
                   PERFORM VARYING IDX     FROM
                                           WRK-STNUM-O (IDXX)  BY  1
                           UNTIL   IDX     >   WRK-EDNUM-O (IDXX)
                       PERFORM 4101131-KANTAN-SYUSEI-UPD-SEC
                       IF      SPA-ERRCD   NOT =   SPACE
                           GO  TO  410113-KANTAN-KOUSHIN-EXT
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    患者マスタ更新（死亡区分）
           IF      WRK-HENSYU-DATA-O (1)   =   SPACE
               CONTINUE
           ELSE
               IF      SPA-C02-KEIKOKU-FLG4    =   0
                   CONTINUE
               ELSE
                   INITIALIZE                      PTINF-REC
                   MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                   MOVE    SPA-NAI-C02-PTID    TO  PTINF-PTID
                   PERFORM 900-PTINF-READ-SEC
                   IF      FLG-PTINF           =   ZERO
                       IF      SPA-C02-KEIKOKU-FLG4    =   1
                           MOVE    MCPDATA-REC     TO  PTINF-REC
                           MOVE    "1"             TO  PTINF-DEATHKBN 
      *
                           MOVE    SMCNDATE-YMD        TO  PTINF-UPYMD
                           MOVE    SMCNDATE-HMS        TO  PTINF-UPHMS
                           MOVE    PTINF-REC           TO  MCPDATA-REC
                           MOVE    "DBUPDATE"          TO  MCP-FUNC
                           MOVE    "tbl_ptinf"         TO  MCP-TABLE
                           MOVE    "key"               TO  MCP-PATHNAME
                           PERFORM 900-ORCDBMAIN-SEC
                           IF      MCP-RC          NOT =   ZERO 
                               MOVE    "1004"              TO  SPA-ERRCD
                               DISPLAY "UPD PTINF-KEY =>" PTINF-KEY 
                               GO  TO  410113-KANTAN-KOUSHIN-EXT
                           END-IF
      *???
                           DISPLAY "PTINF=" PTINF-DEATHKBN "#"
      *???
                       ELSE 
      *                修正で転帰区分を２以外に変更したときに死亡区分が死亡のとき
                           IF      SPA-C02-KEIKOKU-FLG4    =   9
                               MOVE    SPACE           TO
                                                       PTINF-DEATHKBN 
                               MOVE    SMCNDATE-YMD    TO  PTINF-UPYMD
                               MOVE    SMCNDATE-HMS    TO  PTINF-UPHMS
                               MOVE    PTINF-REC       TO  MCPDATA-REC
                               MOVE    "DBUPDATE"      TO  MCP-FUNC
                               MOVE    "tbl_ptinf"     TO  MCP-TABLE
                               MOVE    "key"           TO  MCP-PATHNAME
                               PERFORM 900-ORCDBMAIN-SEC
                               IF      MCP-RC      NOT =   ZERO 
                                   MOVE    "1004"          TO  SPA-ERRCD
                                   DISPLAY "UPD PTINF-KEY =>" PTINF-KEY 
                                   GO  TO  4502-TOUROKU-SYORI-EXT
                               END-IF
      *???
                               DISPLAY "PTINF=" PTINF-DEATHKBN "#"
      *???
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF     
      *
           MOVE    SPACE               TO  WRK-HAISIYMDH
           MOVE    ZERO                TO  SPA-C02-IKOFLG
           INITIALIZE                      SPA-GMN-C02-KOUMOKU
                                           SPA-NAI-C02-KOUMOKU
                                           SPA-NAI-C02-SELNO
           MOVE    1                   TO  SPA-GMN-C02-CUR
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
      *
           INITIALIZE                  SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-NAI-C02-SELNO
                                       SPA-PTBYOMEI-REC
      *
           IF    ( SPA-GMN-C02-SRYKA   NOT =   ZERO  )
           AND   ( SPA-GMN-C02-SRYKAMEI
                                       NOT =   SPACE )
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
      *
      *    検索処理
           PERFORM 310-SPASET-PTBYO-HENSYU-SEC
           .
       410113-KANTAN-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    簡単修正転帰・保険適用チェック処理
      *****************************************************************
       4101132-KANTAN-DABURI-CHK-SEC         SECTION.
      *
      *    転記日のセット
           MOVE    SPA-NAI-C02-TTENKIYMD (IDX)
                                   TO  WRK-TENKIYMD
           IF      WRK-HENSYU-DATA-O (1)
                                   =   SPACE
               CONTINUE
           ELSE
               IF      WRK-HENSYU-DATA-O (1)
                                       =   "-"
                   MOVE    SPACE                   TO  WRK-TENKIYMD
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (2)   TO  WRK-TENKIYMD
               END-IF
           END-IF
      *
      *    転帰と開始日のチェック
           IF      WRK-HENSYU-DATA-O (1)
                               NOT =   "-" AND SPACE  
               IF      SPA-NAI-C02-TSRYYMD (IDX)
                                       >    WRK-HENSYU-DATA-O (2)
                   MOVE    "0084"      TO  SPA-ERRCD
                   GO  TO  4101132-KANTAN-DABURI-CHK-EXT
               END-IF
           END-IF
      *    保険適用と開始日のチェック
           IF     WRK-HENSYU-DATA-O (7)  
                               NOT =   "-" AND SPACE  
               IF    ( SPA-NAI-C02-TSRYYMD (IDX)
                                   <=   WRK-TEKEDYMD )
                   CONTINUE
               ELSE    
                   MOVE    "0085"     TO  SPA-ERRCD
                   MOVE    SPA-NAI-C02-TSRYYMD (IDX)
                                               TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-ERR-SRYYMD
                   MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                               TO  WRK-ERR-BYOMEI
                   GO  TO  4101132-KANTAN-DABURI-CHK-EXT
               END-IF
           END-IF
      *
      *    保険適用指定時の指定病名の重複チェック
           IF     WRK-HENSYU-DATA-O (7)  
                               NOT =   SPACE
               PERFORM VARYING IDXY    FROM    IDXX    BY  1
                       UNTIL   IDXY    >       5
                   IF      WRK-STNUM-O (IDXY)  >   ZERO
                       PERFORM VARYING IDY FROM    WRK-STNUM-O (IDXY)
                                           BY 1
                               UNTIL   IDY >       WRK-EDNUM-O (IDXY)
                           PERFORM
                               41011321-KANTAN-HKNCOMB-CHK-SEC
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  4101132-KANTAN-DABURI-CHK-EXT
           END-IF
      *
      *    マスタとのチェック
           INITIALIZE                      PTBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-NAI-C02-TSRYKA  (IDX)
                                       TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-TBYOMEI (IDX)
                                       TO  PTBYO-BYOMEI
           MOVE    SPA-NAI-C02-THOSOKU-COMMENT (IDX)
                                       TO  PTBYO-HOSOKU-COMMENT
           MOVE    SPA-NAI-C02-TNYUGAIKBN (IDX)
                                       TO  PTBYO-NYUGAIKBN
           IF      WRK-HENSYU-DATA-O (6)   =   SPACE
               CONTINUE
           ELSE 
               IF      WRK-HENSYU-DATA-O (6)   =   "-"
                   MOVE    SPACE               TO  PTBYO-NYUGAIKBN
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (6)
                                               TO  PTBYO-NYUGAIKBN
               END-IF
           END-IF     
           IF      PTBYO-NYUGAIKBN     =   SPACE 
               MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
               MOVE    "key40"             TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
               MOVE    "key41"             TO  WRK-MCP-PATHNAME
           END-IF    
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM                 UNTIL   FLG-PTBYOMEI    =   1
                                   OR      SPA-ERRCD   NOT =   SPACE
               IF      SPA-NAI-C02-TSRYYMD (IDX)   =   PTBYO-SRYYMD
               AND     SPA-NAI-C02-TSRYKA  (IDX)   =   PTBYO-SRYKA
               AND     SPA-NAI-C02-TRENNUM (IDX)   =   PTBYO-RENNUM
                   CONTINUE
               ELSE
                   EVALUATE    TRUE
                       WHEN    SPA-NAI-C02-TSRYYMD (IDX)
                                           =   PTBYO-SRYYMD
                           MOVE    "0002"              TO  SPA-ERRCD
                       WHEN    SPA-NAI-C02-TSRYYMD (IDX)
                                           <   PTBYO-SRYYMD
                           IF      WRK-TENKIYMD    =   SPACE
                               MOVE    "0038"          TO  SPA-ERRCD
                           ELSE
                               IF     WRK-TENKIYMD >   PTBYO-SRYYMD
                                   MOVE    "0038"      TO  SPA-ERRCD
                               END-IF
                           END-IF
                       WHEN    SPA-NAI-C02-TSRYYMD (IDX)
                                           >   PTBYO-SRYYMD
                           IF      PTBYO-TENKIYMD  =   SPACE
                               MOVE    "0038"      TO  SPA-ERRCD
                           ELSE
                               IF      SPA-NAI-C02-TSRYYMD (IDX)
                                               <   PTBYO-TENKIYMD
                                   MOVE    "0038"      TO  SPA-ERRCD
                               END-IF
                           END-IF
                   END-EVALUATE
      *
                   IF      SPA-ERRCD           =   "0038"  OR
                                                   "0002"
                       INITIALIZE                  WRK-CHK-HKNCOMBI-AREA
      *
                       IF      WRK-HENSYU-DATA-O (7)   =   SPACE
                           MOVE    SPA-NAI-C02-THKNCOMBI (IDX)
                                                   TO  WRK-CHK-HKNCOMBI1
                       ELSE 
                           IF      WRK-HENSYU-DATA-O (7)   =   "-"
                               MOVE    ZERO        TO  WRK-CHK-HKNCOMBI1
                           ELSE
                               MOVE    WRK-HENSYU-DATA-O (7)
                                                   TO  WRK-CHK-HKNCOMBI1
                           END-IF
                       END-IF
                       MOVE    PTBYO-HKNCOMBI      TO  WRK-CHK-HKNCOMBI2
                       MOVE    SPA-NAI-C02-TKOHID (IDX)
                                               TO  WRK-CHK-KOHID1
                       MOVE    PTBYO-KOHID     TO  WRK-CHK-KOHID2
                       PERFORM 45021-HKNCOMBI-DABURI-CHK-SEC
                       IF      FLG-HKNCOMBI-DABURI =   1
                           MOVE   SPACE            TO  SPA-ERRCD
                       ELSE 
                           MOVE    PTBYO-SRYYMD    TO  WRK-SYMD
                           PERFORM 5002-HIZUKE-HEN-SEC
                           MOVE    LNK-DAY2-EDTYMD3
                                                   TO  WRK-ERR-SRYYMD
                           MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                                   TO  WRK-ERR-BYOMEI
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       4101132-KANTAN-DABURI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ変更時同一病名重複チェック処理
      *****************************************************************
       41011321-KANTAN-HKNCOMB-CHK-SEC SECTION.
      *
      *    同一診療科・同一病名の場合だけチェック
           IF  ( IDXX              =    IDXY )
           AND ( IDX               =    IDY  )
                CONTINUE
           ELSE
               IF    ( SPA-GMN-C02-TBYOMEI (IDX)   =
                                   SPA-GMN-C02-TBYOMEI (IDY) )
               AND   ( SPA-NAI-C02-TSRYKA  (IDX)   =
                                   SPA-NAI-C02-TSRYKA  (IDY) )
      *
      *            入外区分が違う場合はチェックしない
                   IF      WRK-HENSYU-DATA-O (6)   =   SPACE
                       IF (      ( SPA-NAI-C02-TNYUGAIKBN (IDX)
                                                       =   "1" )
                           AND   ( SPA-NAI-C02-TNYUGAIKBN (IDY)
                                                       =   "2" ) )
                       OR (      ( SPA-NAI-C02-TNYUGAIKBN (IDX)
                                                       =   "2" )
                           AND   ( SPA-NAI-C02-TNYUGAIKBN (IDY)
                                                       =   "1" ) )
                           GO  TO
                               41011321-KANTAN-HKNCOMB-CHK-EXT
                       END-IF
                   END-IF     
      *
                   EVALUATE    TRUE
                       WHEN    SPA-NAI-C02-TSRYYMD (IDX)
                                           =   SPA-NAI-C02-TSRYYMD (IDY)
                           MOVE    "0002"              TO  SPA-ERRCD
                       WHEN    SPA-NAI-C02-TSRYYMD (IDX)
                                           <   SPA-NAI-C02-TSRYYMD (IDY)
                           IF      WRK-TENKIYMD    =   SPACE
                               MOVE    "0038"          TO  SPA-ERRCD
                           ELSE
                               IF     WRK-TENKIYMD >
                                               SPA-NAI-C02-TSRYYMD (IDY)
                                   MOVE    "0038"      TO  SPA-ERRCD
                               END-IF
                           END-IF
                       WHEN    SPA-NAI-C02-TSRYYMD (IDX)
                                           >   SPA-NAI-C02-TSRYYMD (IDY)
                           IF      SPA-NAI-C02-TTENKIYMD (IDY)
                                               =   SPACE
                               MOVE    "0038"      TO  SPA-ERRCD
                           ELSE
                               IF      SPA-NAI-C02-TSRYYMD (IDX)   <
                                               SPA-NAI-C02-TSRYYMD (IDY)
                                   MOVE    "0038"      TO  SPA-ERRCD
                               END-IF
                           END-IF
                   END-EVALUATE
      *
                   IF      SPA-ERRCD           =   "0038"  OR
                                                   "0002"
                       INITIALIZE                  WRK-CHK-HKNCOMBI-AREA
      *
                       IF      WRK-HENSYU-DATA-O (7)   =   SPACE
                           MOVE    SPA-NAI-C02-THKNCOMBI (IDX)
                                                   TO  WRK-CHK-HKNCOMBI1
                           MOVE    SPA-NAI-C02-THKNCOMBI (IDY)
                                                   TO  WRK-CHK-HKNCOMBI2
                       ELSE 
                           IF      WRK-HENSYU-DATA-O (7)   =   "-"
                               MOVE    ZERO        TO  WRK-CHK-HKNCOMBI1
                                                       WRK-CHK-HKNCOMBI2
                           ELSE
                               MOVE    WRK-HENSYU-DATA-O (7)
                                                   TO  WRK-CHK-HKNCOMBI1
                                                       WRK-CHK-HKNCOMBI2
                           END-IF
                       END-IF
                       MOVE    SPA-NAI-C02-TKOHID (IDX)
                                               TO  WRK-CHK-KOHID1
                       MOVE    SPA-NAI-C02-TKOHID (IDY)
                                               TO  WRK-CHK-KOHID2
                       PERFORM 45021-HKNCOMBI-DABURI-CHK-SEC
                       IF      FLG-HKNCOMBI-DABURI =   1
                           MOVE   SPACE            TO  SPA-ERRCD
                       ELSE 
                           MOVE    SPA-NAI-C02-TSRYYMD (IDY)
                                                   TO  WRK-SYMD
                           PERFORM 5002-HIZUKE-HEN-SEC
                           MOVE    LNK-DAY2-EDTYMD3
                                                   TO  WRK-ERR-SRYYMD
                           MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                                   TO  WRK-ERR-BYOMEI
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       41011321-KANTAN-HKNCOMB-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    簡単修正更新処理
      *****************************************************************
       4101131-KANTAN-SYUSEI-UPD-SEC         SECTION.
      *
           INITIALIZE                      PTBYO-KEY
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-NAI-C02-TSRYKA  (IDX)
                                       TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-TSRYYMD (IDX)
                                       TO  PTBYO-SRYYMD
           MOVE    SPA-NAI-C02-TRENNUM (IDX)
                                       TO  PTBYO-RENNUM 
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM 900-PTBYO-INV-SEC
           IF      FLG-PTBYOMEI        =   1
               MOVE    "1003"              TO  SPA-ERRCD
               DISPLAY "INV PTBYOMEI-KEY =>" PTBYO-KEY 
               GO  TO  4101131-KANTAN-SYUSEI-UPD-EXT
           END-IF
           IF      PTBYO-TENKIKBN      =   "2"
               MOVE    1                   TO  FLG-TENKIKBN-CHK
           END-IF
      *
      *    転帰区分・転帰日
           EVALUATE    WRK-HENSYU-DATA-O (1)
               WHEN    SPACE               
                   CONTINUE
               WHEN    "-"               
                   MOVE    SPACE   TO  PTBYO-TENKIKBN
                                       PTBYO-TENKIYMD
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (1)
                                   TO  PTBYO-TENKIKBN
                   MOVE    WRK-HENSYU-DATA-O (2)
                                   TO  PTBYO-TENKIYMD
           END-EVALUATE      
      *
      *    疑いフラグ
           EVALUATE    WRK-HENSYU-DATA-O (3)
               WHEN    SPACE               
                   CONTINUE
               WHEN    "-"               
                   MOVE    SPACE   TO  PTBYO-UTAGAIFLG
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (3)
                                   TO  PTBYO-UTAGAIFLG
           END-EVALUATE      
      *
      *    主病名フラグ
           EVALUATE    WRK-HENSYU-DATA-O (4)
               WHEN    SPACE               
                   CONTINUE
               WHEN    "-"               
                   MOVE    SPACE   TO  PTBYO-SYUBYOFLG
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (4)
                                   TO  PTBYO-SYUBYOFLG
           END-EVALUATE      
      *
      *    疾患区分
           EVALUATE    WRK-HENSYU-DATA-O (5)
               WHEN    SPACE               
                   CONTINUE
               WHEN    "-"               
                   MOVE    ZERO    TO  PTBYO-MANSEIKBN
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (5)
                                   TO  PTBYO-MANSEIKBN
           END-EVALUATE      
      *
      *    入外区分
           EVALUATE    WRK-HENSYU-DATA-O (6)
               WHEN    SPACE               
                   CONTINUE
               WHEN    "-"               
                   MOVE    SPACE   TO  PTBYO-NYUGAIKBN
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (6)
                                   TO  PTBYO-NYUGAIKBN
           END-EVALUATE      
      *
      *    保険適用
           EVALUATE    WRK-HENSYU-DATA-O (7)
               WHEN    SPACE               
                   CONTINUE
               WHEN    "-"               
                   MOVE    ZERO    TO  PTBYO-HKNCOMBI
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-O (7)
                                   TO  PTBYO-HKNCOMBI
           END-EVALUATE      
      *
      *    保険適用（第三者行為）
           EVALUATE    WRK-HENSYU-DATA-9
               WHEN    ZERO
                   CONTINUE
               WHEN    9999999999               
                   MOVE    ZERO    TO  PTBYO-KOHID
               WHEN    OTHER
                   MOVE    WRK-HENSYU-DATA-9
                                   TO  PTBYO-KOHID
           END-EVALUATE
      *
      *    レセ電エラーフラグ
           INITIALIZE                      WRK-PTBYOMEI-CHK-AREA
           IF      PTBYO-BYOMEIHENFLG  =   SPACE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       WRK-CONS-BYOMEICD-MAX
                       OR      PTBYO-BYOMEICD (IDY)    =   SPACE
                   MOVE    PTBYO-BYOMEICD (IDY)    TO   BYO-BYOMEICD
                   PERFORM 900-BYOMEI-READ-SEC
                   IF      FLG-BYOMEI          =   ZERO
                       IF    ( WRK-HAISIYMD-CHK
                                               =   ZERO                )
                       AND   ( BYO-HAISIYMD
                                         NOT   =   ZERO    AND ALL "9" )
                           MOVE    BYO-HAISIYMD    TO  WRK-HAISIYMD-CHK
                       END-IF
                   END-IF    
               END-PERFORM
           END-IF
           MOVE    PTBYOMEI-REC        TO  WRK-PTBYOMEI-CHK-REC
           PERFORM 5001-RECEDENFLG-HENSYU-SEC
           MOVE    WRK-PTBYO-CHK-RECEDENFLG
                                       TO  PTBYO-RECEDENFLG
      *
           MOVE    SPA-OPID            TO  PTBYO-OPID
           MOVE    SMCNDATE-YMD        TO  PTBYO-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTBYO-UPHMS
           MOVE    "DBUPDATE"          TO  MCP-FUNC
      *???
           DISPLAY "PTBYO=" PTBYO-KEY "#" MCP-FUNC "#"
      *???                
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY MCP-FUNC "PTBYOMEI-KEY =>"  PTBYO-KEY 
           ELSE
               PERFORM 450221-RECEUPD-UPDATE-SEC
           END-IF
           .
       4101131-KANTAN-SYUSEI-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    慢性と急性の警告チェック処理
      *****************************************************************
       410114-KEIKOKU-CHK-SEC                SECTION.
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       5
                   OR      SPA-ERRCD           NOT =   SPACE
               IF      WRK-STNUM-O (IDXX)  >   ZERO
                   PERFORM VARYING IDX     FROM
                                       WRK-STNUM-O (IDXX)  BY  1
                           UNTIL   IDX     >   WRK-EDNUM-O (IDXX)
                           OR      SPA-ERRCD       NOT =   SPACE
                       MOVE    ZERO                TO  FLG-MANSEI
                                                       FLG-KYUSEI
                       PERFORM VARYING IDZZ    FROM    1   BY  2
                               UNTIL   IDZZ    >       79
                           IF      SPA-GMN-C02-TBYOMEI (IDX)(IDZZ:4)
                                                           =   "慢性"
                               MOVE    1               TO  FLG-MANSEI
                           END-IF
                           IF      SPA-GMN-C02-TBYOMEI (IDX)(IDZZ:4)
                                                       =   "急性"
                               MOVE    1               TO  FLG-KYUSEI
                           END-IF
                           IF    ( FLG-MANSEI              =   1 )
                           AND   ( FLG-KYUSEI              =   1 )
                               MOVE    80                      TO  IDZZ
                           END-IF
                       END-PERFORM    
      *
                       IF      FLG-MANSEI              =   1
                           IF      FLG-KYUSEI              =   1
                               CONTINUE
                           ELSE
                               MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                                   TO  WRK-ERR-BYOMEI
                               MOVE    "0104"      TO  SPA-ERRCD
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       410114-KEIKOKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    警告チェック処理（死亡区分更新、他診療科の同一病名の警告）
      *****************************************************************
       410115-KEIKOKU-CHK-SEC                SECTION.
      *
      *    死亡区分更新のチェック
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   ZERO
      *        転帰区分が２で死亡区分が生存のとき
               IF      PTINF-DEATHKBN  NOT =   "1"
                   IF      WRK-HENSYU-DATA-O (1)
                                               =   "2"
                       IF      SPA-C02-1042-MSG5-DSPKBN
                                           =   0
                           MOVE    "0107"      TO  SPA-CIDCD
                       END-IF 
                       MOVE    1           TO  SPA-C02-KEIKOKU-FLG4
                   END-IF
               ELSE
      *            転帰区分を２以外に変更したときに死亡区分が死亡のとき
                   IF      WRK-HENSYU-DATA-O (1)
                                           NOT =   "2"
                       MOVE    ZERO        TO  FLG-TENKIKBN
                       PERFORM VARYING IDXX    FROM    1   BY  1
                               UNTIL   IDXX    >       5
                               OR      FLG-TENKIKBN    =   1
                           IF      WRK-STNUM-O (IDXX)  >   ZERO
                               PERFORM VARYING IDX     FROM
                                           WRK-STNUM-O (IDXX)  BY  1
                                       UNTIL   IDX     >
                                                   WRK-EDNUM-O (IDXX)
                                       OR      FLG-TENKIKBN
                                                       =   1
                                   IF      SPA-NAI-C02-TTENKIKBN  (IDX)
                                                       =   "2"
                                       MOVE    1   TO  FLG-TENKIKBN
                                   END-IF
                               END-PERFORM
                          END-IF
                       END-PERFORM
                       IF      FLG-TENKIKBN     =   1
                           PERFORM 4101151-KEIKOKU-TENKI-CHK-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    他診療科の同一病名の警告チェック
           IF      SPA-C02-1042-MSG4-DSPKBN
                                           =   0
               GO  TO  410115-KEIKOKU-CHK-EXT
           END-IF
      * 
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       5
      ************ OR      SPA-CIDCD           NOT =   SPACE
               IF      WRK-STNUM-O (IDXX)  >   ZERO
                   PERFORM VARYING IDX     FROM
                                       WRK-STNUM-O (IDXX)  BY  1
                           UNTIL   IDX     >   WRK-EDNUM-O (IDXX)
      ************         OR      SPA-CIDCD       NOT =   SPACE
      *
                       IF      WRK-HENSYU-DATA-O (2)   =   SPACE
                           IF      WRK-HENSYU-DATA-O (8)   =   SPACE
                               MOVE    SPA-NAI-C02-TTENKIYMD  (IDX)
                                                   TO  WRK-CHK-TENKIYMD
                           ELSE
                               MOVE    SPACE       TO  WRK-CHK-TENKIYMD
                           END-IF
                       ELSE 
                           MOVE    WRK-HENSYU-DATA-O (2)
                                                   TO  WRK-CHK-TENKIYMD
                       END-IF     
      *
                       IF      WRK-HENSYU-DATA-O (8)   =   SPACE
                           MOVE    SPA-NAI-C02-TSRYYMD  (IDX)
                                                   TO  WRK-CHK-SRYYMD
                       ELSE 
                           MOVE    WRK-HENSYU-DATA-O (8)
                                                   TO  WRK-CHK-SRYYMD
                       END-IF     
      *
                       INITIALIZE                      PTBYOMEI-REC
grpsys                 MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
                       MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
                       MOVE    SPA-NAI-C02-TSRYKA  (IDX)
                                                   TO  PTBYO-SRYKA
                       MOVE    SPA-NAI-C02-TBYOMEI (IDX)
                                                   TO  PTBYO-BYOMEI
                       MOVE    SPA-NAI-C02-THOSOKU-COMMENT (IDX)
                                                   TO
                                                   PTBYO-HOSOKU-COMMENT
                       MOVE    SPA-NAI-C02-TNYUGAIKBN (IDX)
                                                   TO  PTBYO-NYUGAIKBN
                       IF      WRK-HENSYU-DATA-O (6)   =   SPACE
                           CONTINUE
                       ELSE 
                           IF      WRK-HENSYU-DATA-O (6)   =   "-"
                               MOVE    SPACE       TO  PTBYO-NYUGAIKBN
                           ELSE
                               MOVE    WRK-HENSYU-DATA-O (6)
                                                   TO  PTBYO-NYUGAIKBN
                           END-IF
                       END-IF     
                       IF      PTBYO-NYUGAIKBN     =   SPACE
                           MOVE    "tbl_ptbyomei"  TO  WRK-MCP-TABLE
                           MOVE    "key42"         TO  WRK-MCP-PATHNAME
                       ELSE
                           MOVE    "tbl_ptbyomei"  TO  WRK-MCP-TABLE
                           MOVE    "key43"         TO  WRK-MCP-PATHNAME
                       END-IF
      *
                       MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
                       MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                       MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                       PERFORM 900-DBSELECT-SEC
                       IF      MCP-RC              =   ZERO
                           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                           PERFORM 900-PTBYO-READ-SEC
                       ELSE
                           MOVE    1                   TO  FLG-PTBYOMEI
                       END-IF
      *
                       PERFORM         UNTIL   FLG-PTBYOMEI    =   1
      *********                        OR      SPA-CIDCD   NOT =   SPACE
                           EVALUATE    TRUE
                               WHEN    WRK-CHK-SRYYMD
                                                   =   PTBYO-SRYYMD
                                   MOVE    1              TO
                                                   SPA-C02-KEIKOKU-FLG1
                               WHEN    WRK-CHK-SRYYMD
                                                   <   PTBYO-SRYYMD
                                   IF      WRK-CHK-TENKIYMD
                                                       =   SPACE
                                       MOVE    1          TO
                                                   SPA-C02-KEIKOKU-FLG1
                                   ELSE
                                       IF     WRK-CHK-TENKIYMD
                                                   >   PTBYO-SRYYMD
                                           MOVE    1          TO
                                                   SPA-C02-KEIKOKU-FLG1
                                       END-IF
                                   END-IF
                               WHEN    WRK-CHK-SRYYMD
                                                   >   PTBYO-SRYYMD
                                   IF      PTBYO-TENKIYMD =   SPACE
                                       MOVE    1          TO
                                                   SPA-C02-KEIKOKU-FLG1
                                   ELSE
                                       IF      WRK-CHK-SRYYMD
                                                   <   PTBYO-TENKIYMD
                                           MOVE    1          TO
                                                   SPA-C02-KEIKOKU-FLG1
                                       END-IF
                                   END-IF
                           END-EVALUATE
                           IF      SPA-C02-KEIKOKU-FLG1    =   1
                               INITIALIZE          WRK-CHK-HKNCOMBI-AREA
      *
                               IF      WRK-HENSYU-DATA-O (7)   =   SPACE
                                   MOVE    SPA-NAI-C02-THKNCOMBI (IDX)
                                                   TO  WRK-CHK-HKNCOMBI1
                               ELSE 
                                   IF      WRK-HENSYU-DATA-O (7)
                                                       =   "-"
                                       MOVE    ZERO        TO
                                                       WRK-CHK-HKNCOMBI1
                                   ELSE
                                       MOVE    WRK-HENSYU-DATA-O (7)
                                                   TO  WRK-CHK-HKNCOMBI1
                                   END-IF
                               END-IF
                               MOVE    PTBYO-HKNCOMBI
                                                   TO  WRK-CHK-HKNCOMBI2
                               IF      WRK-HENSYU-DATA-9       =   ZERO
                                   MOVE    SPA-NAI-C02-TKOHID (IDX)
                                                   TO  WRK-CHK-KOHID1
                               ELSE 
                                   IF      WRK-HENSYU-DATA-9   =
                                                           9999999999
                                       MOVE    ZERO
                                                   TO  WRK-CHK-KOHID1
                                   ELSE
                                       MOVE    WRK-HENSYU-DATA-9
                                                   TO  WRK-CHK-KOHID1
                                   END-IF
                               END-IF
                               MOVE    PTBYO-KOHID TO  WRK-CHK-KOHID2
                               PERFORM 45021-HKNCOMBI-DABURI-CHK-SEC
                               IF      FLG-HKNCOMBI-DABURI =   1
                                   MOVE    ZERO       TO
                                                   SPA-C02-KEIKOKU-FLG1
                               ELSE 
                                   MOVE    1           TO  FLG-PTBYOMEI
                                   IF      SPA-CIDCD   =   SPACE
                                       MOVE    "0106"      TO  SPA-CIDCD
                                   END-IF
                                   MOVE    PTBYO-SRYYMD    TO  WRK-SYMD
                                   PERFORM 5002-HIZUKE-HEN-SEC
                                   MOVE    LNK-DAY2-EDTYMD3
                                                           TO
                                                       WRK-ERR-SRYYMD
                                   MOVE    PTBYO-SRYKA     TO  WRK-KBN
                                   PERFORM 5001-SRYKA-HENSYU-SEC
                                   MOVE    WRK-KBNMEI          TO
                                                           WRK-ERR-SRYKA
                                   MOVE    PTBYO-BYOMEI
                                                  TO  WRK-ERR-BYOMEI
                               END-IF
                           END-IF
      *
                           IF      FLG-PTBYOMEI    =   ZERO
                               MOVE    WRK-MCP-TABLE   TO  MCP-TABLE
                               MOVE    WRK-MCP-PATHNAME
                                                       TO  MCP-PATHNAME
                               PERFORM 900-PTBYO-READ-SEC
                           END-IF
                       END-PERFORM
      *
                       MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                       MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           IF      WRK-ERR-BYOMEI      NOT =   SPACE
               MOVE    WRK-ERR-BYOMEI      TO  SPA-C02-KEIKOKU-BYOMEI
               MOVE    WRK-ERR-SRYKA       TO  SPA-C02-KEIKOKU-SRYKA
               MOVE    WRK-ERR-SRYYMD      TO  SPA-C02-KEIKOKU-SRYYMD
               MOVE    SPACE               TO  WRK-ERR-BYOMEI
                                               WRK-ERR-SRYKA
                                               WRK-ERR-SRYYMD
           END-IF
           .
       410115-KEIKOKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    死亡区分更新確認メッセージ表示のチェック処理
      *****************************************************************
       4101151-KEIKOKU-TENKI-CHK-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-TENKIKBN
      *
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPNUM     TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID
                                   TO  PTBYO-PTID
           MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
           MOVE    "key48"         TO  MCP-PATHNAME
           MOVE    PTBYOMEI-REC    TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_ptbyomei"
                                       TO  MCP-TABLE
               MOVE    "key48"     TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               INITIALIZE              PTBYOMEI-REC
               MOVE    1           TO  FLG-PTBYOMEI
           END-IF
           PERFORM     UNTIL   FLG-PTBYOMEI    =   1
                       OR      FLG-TENKIKBN    =   9
               MOVE    ZERO            TO  FLG-CHK
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       5
                       OR      FLG-CHK =       9
                   IF      WRK-STNUM-O (IDXX)  >   ZERO
                       PERFORM VARYING IDX FROM
                                           WRK-STNUM-O (IDXX)  BY  1
                               UNTIL   IDX >   WRK-EDNUM-O (IDXX)
                               OR      FLG-CHK     =   9
                           IF    ( PTBYO-SRYYMD    =
                                           SPA-NAI-C02-TSRYYMD (IDX) )
                           AND   ( PTBYO-RENNUM    =
                                           SPA-NAI-C02-TRENNUM (IDX) )
                           AND   ( PTBYO-SRYKA     =
                                           SPA-NAI-C02-TSRYKA  (IDX) )
                               MOVE    9       TO  FLG-CHK
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
      *
      *        対象病名以外に転帰区分２が存在するとき
               IF      FLG-CHK         =   0
                   MOVE    9               TO  FLG-TENKIKBN
               END-IF
      *
               MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
               MOVE    "key48"         TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
           MOVE    "key48"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    転帰区分２の病名が存在しないとき
           IF      FLG-TENKIKBN        =   0
               IF      SPA-C02-1042-MSG5-DSPKBN
                                           =   0
                   MOVE    "0109"      TO  SPA-CIDCD
               END-IF
               MOVE    9           TO  SPA-C02-KEIKOKU-FLG4
           END-IF
           .
       4101151-KEIKOKU-TENKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択病名処理
      *****************************************************************
       410111-BYOMEI-SELECT-SEC        SECTION.
      *      
           MOVE    ZERO                TO  SPA-C02-IKOFLG
           MOVE    SPACE               TO  WRK-HAISIYMDH
      *      
           MOVE    SPA-GMN-C02-TSRYYMD (SPA-NAI-C02-SELNO)
                                       TO  SPA-GMN-C02-SRYYMD
           MOVE    SPA-NAI-C02-TSRYYMD (SPA-NAI-C02-SELNO)
                                       TO  SPA-NAI-C02-SRYYMD
                                           SPA-NAI-C02-SRYYMD-OLD
           MOVE    SPA-NAI-C02-TRENNUM (SPA-NAI-C02-SELNO)
                                       TO  SPA-NAI-C02-RENNUM
           MOVE    SPA-NAI-C02-TSEQNUM (SPA-NAI-C02-SELNO)
                                       TO  SPA-NAI-C02-SEQNUM
           MOVE    SPA-NAI-C02-TSRYKA (SPA-NAI-C02-SELNO)
                                       TO  SPA-GMN-C02-BSRYKA
                                           SPA-NAI-C02-SRYKA-OLD
      *
           INITIALIZE                  PTBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
           MOVE    SPA-NAI-C02-RENNUM  TO  PTBYO-RENNUM
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM 900-PTBYO-INV-SEC
      *
           MOVE    PTBYOMEI-REC        TO  SPA-PTBYOMEI-REC
      *
      *    病名コード
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >       21
               MOVE    PTBYO-BYOMEICD(IDZ)
                                   TO  SPA-NAI-C02-BYOMEICD   (IDZ)
                                       SPA-NAI-C02-BYOMEICD-O (IDZ 1)
                                       BYO-BYOMEICD
               PERFORM 410111-BYOMEI-HENSYU-SEC
               MOVE    SPACE       TO  SPA-NAI-C02-BYOMEITOP (IDZ) 
               IF      SPA-NAI-C02-BYOMEICD-O (IDZ 1)
                                   =   WRK-CONS-BYOMEICD
                   MOVE    PTBYO-BYOMEI
                                   TO  SPA-NAI-C02-BYOMEI-O (IDZ 1)
                                       SPA-NAI-C02-BYOMEI   (IDZ)
               END-IF
           END-PERFORM
      *
      *    入力コード
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >       6
               MOVE    PTBYO-BYOMEIINPUTCD (IDZ)
                                   TO  SPA-GMN-C02-BYOMEICD (IDZ)
      *        
               IF      SPA-GMN-C02-BYOMEICD (IDZ)
                                   NOT =   SPACE
                   MOVE    ZERO                TO  WRK-LENGTH                 
                   PERFORM VARYING IDX FROM    10  BY  -1
                           UNTIL   IDX <       1
                       IF      SPA-GMN-C02-BYOMEICD (IDZ)(IDX:1)
                                               NOT =   SPACE
                           MOVE    IDX             TO  WRK-LENGTH
                           MOVE    1               TO  IDX
                       END-IF
                   END-PERFORM
      *
      *            ７桁のコードのとき病名コードか予約病名コードか確認
                   MOVE    ZERO                TO  FLG-BYOMEI
                   IF      WRK-LENGTH          =   7
                       MOVE    PTBYO-BYOMEIINPUTCD (IDZ)
                                               TO  BYO-BYOMEICD
                       IF  (   BYO-BYOMEICD (1:3)    =   "ZZZ"    )
                       AND ( ( BYO-BYOMEICD (4:1)    <   "1" ) OR 
                             ( BYO-BYOMEICD (4:1)    =   "C" )    )
      *                    予約病名コード検索
                           SET     TBL-BYOMEICD-IDX  TO  1
                           SEARCH  TBL-BYOMEICD-OCC
                                               VARYING TBL-BYOMEICD-IDX
                               AT  END
                                   MOVE    1           TO  FLG-BYOMEI
                               WHEN    BYO-BYOMEICD    =
                                                   TBL-YYK-BYOMEICD
                                                      (TBL-BYOMEICD-IDX)
                                   MOVE    ZERO        TO  FLG-BYOMEI
                           END-SEARCH
                       ELSE
      *                    病名マスタ検索
                           PERFORM 900-BYOMEI-READ-SEC
                       END-IF
                   ELSE
                       INITIALIZE              USERBYOMEI-REC
grpsys                 MOVE    SPA-HOSPNUM TO  USBYO-HOSPNUM
                       MOVE    SPA-GMN-C02-BYOMEICD (IDZ)
                                           TO  USBYO-BYOMEIINPUTCD
                       PERFORM 900-USERBYOMEI-INV-SEC
                       IF      FLG-USERBYOMEI    =   ZERO
                           MOVE    USBYO-BYOMEISBT
                                         TO  SPA-NAI-C02-BYOMEISBT (IDZ)
                       END-IF
                   END-IF
      *
                   IF      WRK-LENGTH          =   7
                       IF      FLG-BYOMEI          =   ZERO
                           MOVE    SPACE           TO
                                       SPA-NAI-C02-USERBYOMEIFLG (IDZ)
                       ELSE
                           MOVE    "1"             TO
                                       SPA-NAI-C02-USERBYOMEIFLG (IDZ)
                       END-IF    
                   ELSE
                       MOVE    "1"             TO
                                       SPA-NAI-C02-USERBYOMEIFLG (IDZ)
                   END-IF
               END-IF
           END-PERFORM
           MOVE    "1"             TO  SPA-NAI-C02-BYOMEITOP (1)
      *
           PERFORM 4101111-BYOMEI-SELECT-HEN-SEC
      *
      *    病名コードに存在しないコードがあるとき
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >       21
                   OR      SPA-NAI-C02-BYOMEICD (IDZ)
                                   =   SPACE
                   OR      SPA-ERRCD
                               NOT =   SPACE
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       21
                       OR      SPA-NAI-C02-BYOMEICD-O (IDZ IDXX)
                                       =   SPACE
                       OR      SPA-ERRCD
                                   NOT =   SPACE
                   IF      SPA-NAI-C02-BYOMEI-O (IDZ IDXX)
                                               =   "■■■"
                       MOVE    "0091"        TO  SPA-ERRCD
                   END-IF
               END-PERFORM       
           END-PERFORM       
      *
           IF      SPA-NAI-C02-UPDOWN  =   9
               MOVE    ZERO                TO  SPA-NAI-C02-UPDOWN
           END-IF
           MOVE    SPA-NAI-C02-SELNO   TO  C02-BYO-ROW
      *
      *    病名がひとつのとき移動処理不可
           IF      SPA-GMN-C02-MAX     =   1
               MOVE    9                   TO  SPA-NAI-C02-UPDOWN
               GO  TO  410111-BYOMEI-SELECT-EXT
           END-IF
      *
      *    選択病名と同じ開始日があるとき移動処理可能とする
           IF     SPA-NAI-C02-SELNO   >   1
               COMPUTE IDXX    =   SPA-NAI-C02-SELNO   -   1
      *        上の病名と開始日が同じとき
               IF      SPA-NAI-C02-TSRYYMD  (SPA-NAI-C02-SELNO)
                                       =   SPA-NAI-C02-TSRYYMD (IDXX)
                   GO  TO  410111-BYOMEI-SELECT-EXT
               END-IF
           END-IF    
           IF    ( SPA-NAI-C02-SELNO   <   SPA-GMN-C02-MAX )
               COMPUTE IDXX    =   SPA-NAI-C02-SELNO   +   1
      *        下の病名と開始日が同じとき
               IF      SPA-NAI-C02-TSRYYMD  (SPA-NAI-C02-SELNO)
                                       =   SPA-NAI-C02-TSRYYMD (IDXX)
                   GO  TO  410111-BYOMEI-SELECT-EXT
               END-IF
           END-IF
           MOVE    9                   TO  SPA-NAI-C02-UPDOWN
           .
       410111-BYOMEI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択病名編集処理
      *****************************************************************
       4101111-BYOMEI-SELECT-HEN-SEC      SECTION.
      *      
           MOVE    SPA-PTBYO-SRYKA    TO  SPA-GMN-C02-BSRYKA
                                          WRK-KBN
           PERFORM 5001-BSRYKA-HENSYU-SEC
           MOVE    WRK-KBNMEI         TO  SPA-GMN-C02-BSRYKAMEI
      *      
           MOVE    SPA-PTBYO-BYOMEI    TO  SPA-GMN-C02-BYOMEI                                           
      *                                 
           MOVE    SPA-PTBYO-BYOMEIMOJI
                                       TO  SPA-NAI-C02-BYOMEIMOJI
      *                                 
           MOVE    SPA-PTBYO-BYOMEIHENFLG
                                       TO  SPA-NAI-C02-BYOMEIHENFLG
           IF      SPA-NAI-C02-BYOMEIHENFLG  =   "1" 
               MOVE    "病名編集"      TO  SPA-GMN-C02-BYOMEIHENFLG
           END-IF    
      *      
           MOVE    SPA-PTBYO-CHARTBYOMEI
                                       TO  SPA-GMN-C02-CHARTBYOMEI
      *                                 
           MOVE    SPA-PTBYO-CHARTBYOMEIMOJI
                                       TO  SPA-NAI-C02-CHARTBYOMEIMOJI
      *
           MOVE    SPA-PTBYO-UTAGAIFLG TO  SPA-GMN-C02-UTAGAIFLG
                                           WRK-KBN
           PERFORM 5001-UTAGAIFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-UTAGAIFLGMEI
      *                                                              
           MOVE    SPA-PTBYO-SYUBYOFLG TO  SPA-GMN-C02-SYUBYOFLG
                                           WRK-KBN
           PERFORM 5001-SYUBYOFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-SYUBYOFLGMEI
      *                                 
           IF      SPA-PTBYO-MANSEIKBN NOT =   ZERO
               MOVE    SPA-PTBYO-MANSEIKBN TO  SPA-GMN-C02-MANSEIKBN
                                               WRK-KBN
               PERFORM 5001-MANSEIKBN-HENSYU-SEC                                
               MOVE    WRK-KBNMEI         TO  SPA-GMN-C02-MANSEIKBNMEI
           END-IF
      *                                     
           MOVE    SPA-PTBYO-NYUGAIKBN TO  SPA-GMN-C02-NYUGAIKBN
                                           WRK-KBN
           PERFORM 5001-NYUGAIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-NYUGAIKBNMEI
      *                                 
           IF      SPA-PTBYO-HKNCOMBI  NOT =   ZERO
               MOVE    SPA-PTBYO-HKNCOMBI  TO  SPA-GMN-C02-HKNCOMBI
                                               WRK-KBN
               PERFORM 5001-HKNCOMBI-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-HKNCOMBIMEI
           END-IF
      *                                 
           MOVE    SPA-PTBYO-KOHID     TO  SPA-NAI-C02-KOHID
           IF      SPA-PTBYO-KOHID NOT =   ZERO
               MOVE    SPA-PTBYO-KOHID     TO  WRK-KOHID
               MOVE    SPACE               TO  WRK-KBNMEI
               PERFORM 5001-KOHID-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-KOHID
           END-IF
      *                                     
           MOVE    SPA-PTBYO-REZEFLG   TO  SPA-GMN-C02-REZEFLG
                                           WRK-KBN
           PERFORM 5001-REZEFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-REZEFLGMEI
      *                                 
           MOVE    SPA-PTBYO-REZEMM    TO  SPA-GMN-C02-REZEMM
      *                                 
           MOVE    SPA-PTBYO-TENKIKBN  TO  SPA-GMN-C02-TENKIKBN
                                           SPA-NAI-C02-TENKIKBN-OLD
                                           WRK-KBN
           PERFORM 5001-TENKIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-TENKIKBNMEI
      *
           MOVE    SPA-PTBYO-TENKIYMD  TO  SPA-NAI-C02-TENKIYMD
           IF      SPA-PTBYO-TENKIYMD  NOT =   SPACE
               MOVE    PTBYO-TENKIYMD      TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-C02-TENKIYMD
           END-IF
      *                                 
           MOVE    SPA-PTBYO-HKNBYOMEIFLG
                                       TO  SPA-GMN-C02-HKNBYOMEIFLG 
                                           WRK-KBN
           PERFORM 5001-HKNBYOMEIFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-HKNBYOMEIFLGMEI
      *                                 
           MOVE    SPA-PTBYO-RECEDENFLG
                                       TO  SPA-NAI-C02-RECEDENFLG
      *
           MOVE    SPA-PTBYO-DISCHARGEFLG
                                       TO  SPA-GMN-C02-DISCHARGEFLG 
                                           WRK-KBN
           PERFORM 5001-DISCHARGEFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-DISCHARGEFLGMEI
      *
           IF      SPA-PTBYO-CLASS1    NOT =   ZERO
               MOVE    SPA-PTBYO-CLASS1    TO  SPA-GMN-C02-CLASS1 
                                               WRK-KBN
               PERFORM 5001-CLASS1-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-CLASS1MEI
           END-IF
      *
           IF      SPA-PTBYO-CLASS2    NOT =   ZERO
               MOVE    SPA-PTBYO-CLASS2    TO  SPA-GMN-C02-CLASS2 
                                               WRK-KBN
               PERFORM 5001-CLASS2-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-CLASS2MEI
           END-IF
      *
           MOVE    ZERO                TO  SPA-NAI-C02-NANBYOCD
                                           SPA-NAI-C02-MANSEIKBN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       21
               IF      SPA-NAI-C02-NANBYOCD-O (IDX)  NOT =   ZERO
                   MOVE    SPA-NAI-C02-NANBYOCD-O (IDX)
                                           TO  SPA-NAI-C02-NANBYOCD
                   MOVE    21              TO  IDX
               END-IF   
           END-PERFORM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       21
               IF      SPA-NAI-C02-TOKSKNCD (IDX)   NOT =   ZERO
                   MOVE    SPA-NAI-C02-TOKSKNCD (IDX)
                                       TO  SPA-NAI-C02-MANSEIKBN
                   MOVE    21              TO  IDX
               END-IF   
           END-PERFORM
           MOVE    SPA-PTBYO-HOSOKU-COMMENT
                                       TO  SPA-GMN-C02-HOSOKU-COMMENT
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       3
               MOVE    SPA-PTBYO-HOSOKUCD (IDX)
                                       TO  SPA-GMN-C02-HOSOKUCD (IDX)
      *        
               MOVE    ZERO                TO  WRK-LENGTH                 
               PERFORM VARYING IDZ FROM    10  BY  -1
                       UNTIL   IDZ <       1
                   IF      SPA-GMN-C02-HOSOKUCD (IDX)(IDZ:1)
                                           NOT =   SPACE
                       MOVE    IDZ             TO  WRK-LENGTH
                       MOVE    1               TO  IDZ
                   END-IF
               END-PERFORM
      *
               IF      WRK-LENGTH          =   7
      *            病名マスタ読み込み
                   INITIALIZE                      BYOMEI-REC
                   MOVE    SPA-GMN-C02-HOSOKUCD (IDX)
                                               TO  BYO-BYOMEICD
                   MOVE    IDX                 TO  IDZ
                   PERFORM 410242-HOSOKU-HENSYU-SEC
               END-IF
      *
               IF    ( FLG-BYOMEI          =   1 )
               OR    ( WRK-LENGTH      NOT =   7 )
                   INITIALIZE                  USERBYOMEI-REC
                   MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
                   MOVE    SPA-GMN-C02-HOSOKUCD(IDX)
                                               TO  USBYO-BYOMEIINPUTCD
                   MOVE    IDX                 TO  IDZ
                   PERFORM 410243-HOSOKU-USERBYO-HEN-SEC
               END-IF
           END-PERFORM
      *    
           PERFORM 5001-HYOUJI-HENSYU-SEC
      *
      *   入力病名コード数カウント
           PERFORM 410111-BYOMEICDSU-HENSYU-SEC
      *
      *    開始日へ遷移
           MOVE    9                   TO  SPA-GMN-C02-CUR
      *
           .
       4101111-BYOMEI-SELECT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   入力病名コード数カウント処理
      *****************************************************************
       410111-BYOMEICDSU-HENSYU-SEC      SECTION.
      *
      *   入力病名コード数カウント
           MOVE    ZERO                TO  SPA-NAI-C02-BYOMEICD-MAX
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
               IF      SPA-NAI-C02-BYOMEITOP (IDX)  =   "1"                   
                   ADD     1               TO  SPA-NAI-C02-BYOMEICD-MAX
               END-IF
           END-PERFORM
      *
           .
       410111-BYOMEICDSU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名マスタ編集処理
      *****************************************************************
       410111-BYOMEI-HENSYU-SEC          SECTION.
      *
           INITIALIZE                    SPA-NAI-C02-BYOMEI-G(IDZ)
                                         SPA-NAI-C02-USERBYOMEIFLG(IDZ)
                                         SPA-NAI-C02-BYOMEITOP (IDZ)
                                         SPA-NAI-C02-BYOMEISBT (IDZ)
      *
           MOVE    ZERO                  TO  FLG-BYOMEI
      *
           IF     BYO-BYOMEICD       NOT =   SPACE
               IF  (   BYO-BYOMEICD (1:3)    =   "ZZZ"    )
               AND ( ( BYO-BYOMEICD (4:1)    <   "1" ) OR 
                     ( BYO-BYOMEICD (4:1)    =   "C" )    )
      *            予約病名コード検索
                   SET     TBL-BYOMEICD-IDX  TO  1
                   SEARCH  TBL-BYOMEICD-OCC  VARYING TBL-BYOMEICD-IDX
                       AT  END
                           MOVE    1           TO  FLG-BYOMEI
                       WHEN    BYO-BYOMEICD    =
                                   TBL-YYK-BYOMEICD (TBL-BYOMEICD-IDX)
                           MOVE    TBL-YYK-BYOMEI (TBL-BYOMEICD-IDX)
                                               TO
                                       SPA-NAI-C02-BYOMEI(IDZ)
                           MOVE    "1"         TO
                                       SPA-NAI-C02-US-BYOMEIHENFLG (IDZ)
                           MOVE    BYO-BYOMEICD
                                               TO
                                       SPA-NAI-C02-BYOMEICD-O (IDZ 1)
                           MOVE    SPA-NAI-C02-BYOMEI (IDZ)
                                               TO
                                       SPA-NAI-C02-BYOMEI-O   (IDZ 1)
                   END-SEARCH
               ELSE
      *            病名マスタ検索
                   MOVE    BYO-BYOMEICD
                                         TO
                                         SPA-NAI-C02-BYOMEICD-O (IDZ 1)
                   PERFORM 900-BYOMEI-READ-SEC
                   IF      FLG-BYOMEI        =   ZERO
                       IF      BYO-BYOMEICD  =   WRK-CONS-BYOMEICD
                           MOVE    SPACE TO  SPA-NAI-C02-BYOMEI (IDZ)
                       ELSE                           
                           MOVE    BYO-BYOMEI
                                         TO  SPA-NAI-C02-BYOMEI  (IDZ)
                       END-IF    
                       MOVE    BYO-UTAGAIFLG
                                         TO  SPA-NAI-C02-UTAGAIFLG(IDZ)
                       IF      BYO-BYOMEICD  =   WRK-CONS-UTAGAI
                           MOVE    "1"   TO  SPA-NAI-C02-UTAGAIFLG(IDZ)
                       END-IF    
                       MOVE    BYO-TOKSKNCD
                                         TO  SPA-NAI-C02-TOKSKNCD  (IDZ)
                       MOVE    BYO-NANBYOCD
                                         TO  SPA-NAI-C02-NANBYOCD-O(IDZ)
                       MOVE    BYO-HAISIYMD
                                         TO  SPA-NAI-C02-HAISIYMD  (IDZ)
                                             SPA-NAI-C02-HAISIYMD-O
                                                                (IDZ 1)
                       MOVE    BYO-BYOMEICD
                                         TO
                                         SPA-NAI-C02-BYOMEICD-O (IDZ 1)
                       MOVE    SPA-NAI-C02-BYOMEI (IDZ)
                                         TO
                                         SPA-NAI-C02-BYOMEI-O   (IDZ 1)
                       MOVE    BYO-TANDOKUKBN
                                         TO
                                         SPA-NAI-C02-TANDOKUKBN-O(IDZ 1)
                   ELSE
                       MOVE    "■■■"  TO
                                         SPA-NAI-C02-BYOMEI-O   (IDZ 1)
                   END-IF
               END-IF
      *
               IF      FLG-BYOMEI        =   ZERO
                   MOVE    IDZ               TO  IDX3
                   PERFORM 4101111-BYOMEITOP-HENSYU-SEC
      *************MOVE    SPACE         TO
      *************                      SPA-NAI-C02-USERBYOMEIFLG(IDZ)
               ELSE
                   MOVE    "■■■"      TO  SPA-NAI-C02-BYOMEI   (IDZ)
               END-IF
           END-IF
           .
       410111-BYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数病名の先頭フラグ編集処理
      *****************************************************************
       4101111-BYOMEITOP-HENSYU-SEC      SECTION.
      *
           IF      SPA-NAI-C02-BYOMEICD (IDX3) 
                                   =   SPACE
               MOVE    SPACE           TO  SPA-NAI-C02-BYOMEITOP(IDX3)
               GO  TO  4101111-BYOMEITOP-HENSYU-EXT
           END-IF
      * 
           IF      IDX3            =   1
               MOVE    "1"             TO  SPA-NAI-C02-BYOMEITOP (IDX3)
               GO  TO  4101111-BYOMEITOP-HENSYU-EXT
           END-IF
      *
      *   自院病名の部位が入力されている場合は複数病名にはしない
           PERFORM VARYING IDW FROM    1   BY  1
                   UNTIL   IDW >       6
               IF    ( SPA-NAI-C02-USERBYOMEIFLG (IDW)
                                           =   "1" )
               AND   ( SPA-NAI-C02-BYOMEISBT(IDW)
                                           =   "1" )
                   MOVE    SPACE       TO  SPA-NAI-C02-BYOMEITOP(IDX3) 
                   GO  TO  4101111-BYOMEITOP-HENSYU-EXT
               END-IF
           END-PERFORM
      *
      *    存在しない自院病名のとき
           IF      SPA-NAI-C02-BYOMEI (IDX3)
                                   =   "■■■"
               MOVE    "1"             TO  SPA-NAI-C02-BYOMEITOP(IDX3) 
               GO  TO  4101111-BYOMEITOP-HENSYU-EXT
           END-IF
      *
               COMPUTE IDW     =   IDX3    -   1
               IF    ( SPA-NAI-C02-BYOMEICD (IDW)
                                           =   SPACE )
               AND   ( IDW                 >   1     )
                   COMPUTE IDW     =   IDW    -   1
               END-IF
      *
      *        前が存在しない自院病名のとき
               IF      SPA-NAI-C02-BYOMEI (IDW)
                                   =   "■■■"
                   MOVE    "1"         TO  SPA-NAI-C02-BYOMEITOP(IDX3) 
                   GO  TO  4101111-BYOMEITOP-HENSYU-EXT
               END-IF
      *
               IF      SPA-NAI-C02-USERBYOMEIFLG (IDX3)
                                       =   "1"
                   EVALUATE    SPA-NAI-C02-BYOMEISBT(IDX3)
                       WHEN    "8"
                           MOVE    SPACE   TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                       WHEN    "7"
                       WHEN    "0"
                           IF      SPA-NAI-C02-USERBYOMEIFLG (IDW)
                                                       =   "1"
                             EVALUATE    SPA-NAI-C02-BYOMEISBT (IDW)
                               WHEN    "7"
                               WHEN    "1"
                                   MOVE    SPACE       TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                               WHEN    "8"
                               WHEN    "0"
                                   MOVE    "1"     TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                             END-EVALUATE
                           ELSE
                             EVALUATE    SPA-NAI-C02-BYOMEICD (IDW)(1:1)
                                   ALSO   SPA-NAI-C02-BYOMEICD(IDW)(4:1)
                               WHEN    "Z" ALSO    "1"    THRU    "7"
                                   MOVE    SPACE       TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                               WHEN    "Z" ALSO    "8"
                                   MOVE    "1"         TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                               WHEN    "Z" ALSO    ANY
                                   MOVE    SPACE       TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                               WHEN    OTHER
                                   MOVE    "1"     TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                             END-EVALUATE
                           END-IF
                       WHEN    "1"
                           MOVE    SPACE       TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                   END-EVALUATE    
               ELSE
                   EVALUATE    SPA-NAI-C02-BYOMEICD(IDX3)(1:1)
                               ALSO   SPA-NAI-C02-BYOMEICD(IDX3)(4:1) 
                       WHEN    "Z"                 ALSO    "8"
                           MOVE    SPACE   TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                       WHEN    "Z"                 ALSO
                                                   "1"    THRU    "7"
                       WHEN    "0" THRU    "9"     ALSO    ANY
                           IF      SPA-NAI-C02-USERBYOMEIFLG (IDW)
                                                       =   "1"
                             EVALUATE    SPA-NAI-C02-BYOMEISBT (IDW)
                               WHEN    "7"
                               WHEN    "1"
                                   MOVE    SPACE       TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                               WHEN    "8"
                               WHEN    "0"
                                   MOVE    "1"     TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                             END-EVALUATE
                           ELSE
                             EVALUATE    SPA-NAI-C02-BYOMEICD (IDW)(1:1)
                                   ALSO   SPA-NAI-C02-BYOMEICD(IDW)(4:1)
                               WHEN    "Z" ALSO    "1"    THRU    "7"
                                   MOVE    SPACE       TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                               WHEN    "Z" ALSO    "8"
                                   MOVE    "1"         TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                               WHEN    "Z" ALSO    ANY
                                   MOVE    SPACE       TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                               WHEN    OTHER
                                   MOVE    "1"     TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                             END-EVALUATE
                           END-IF
                       WHEN    "Z"                 ALSO    ANY
                           MOVE    SPACE   TO
                                           SPA-NAI-C02-BYOMEITOP (IDX3)
                   END-EVALUATE    
               END-IF    
           .
       4101111-BYOMEITOP-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名マスタ編集処理
      *****************************************************************
       410112-USERBYOMEI-HENSYU-SEC      SECTION.
      *
           INITIALIZE                    SPA-NAI-C02-BYOMEI-G (IDZ)
                                         SPA-NAI-C02-USERBYOMEIFLG(IDZ)
                                         SPA-NAI-C02-BYOMEITOP(IDZ)
                                         SPA-NAI-C02-BYOMEISBT (IDZ)
      *
           MOVE    ZERO                  TO  FLG-USERBYOMEI
           IF      USBYO-BYOMEIINPUTCD   =   SPACE
               CONTINUE
           ELSE
grpsys         MOVE    SPA-HOSPNUM     TO  USBYO-HOSPNUM
               PERFORM 900-USERBYOMEI-INV-SEC
               IF      FLG-USERBYOMEI    =   ZERO
                   MOVE    "1"           TO
                                         SPA-NAI-C02-USERBYOMEIFLG(IDZ)
                   MOVE    USBYO-BYOMEI  TO  SPA-NAI-C02-BYOMEI   (IDZ)
                   IF      IDZ           <   4
                       MOVE    USBYO-HOSOKU-COMMENT
                                         TO  SPA-NAI-C02-HOSOKU-COMMENT
                                                                  (IDZ)
                   END-IF
                   IF      USBYO-HOSOKU-COMMENT
                                         =   SPACE
                       CONTINUE
                   ELSE
                       INITIALIZE            SPA-GMN-C02-HOSOKUCD-G
                                             SPA-NAI-C02-HOSOKU-TBL
                       MOVE    USBYO-HOSOKU-COMMENT
                                         TO  SPA-GMN-C02-HOSOKU-COMMENT
                   END-IF
                   MOVE    USBYO-HOSOKU-COMMENT
                                         TO
                                     SPA-NAI-C02-US-HOSOKU-COMMENT(IDZ)
                   MOVE    USBYO-CHARTBYOMEI
                                         TO
                                         SPA-NAI-C02-CHARTBYOMEI  (IDZ)
                   MOVE    USBYO-UTAGAIFLG
                                         TO  SPA-NAI-C02-UTAGAIFLG(IDZ)
                                             SPA-NAI-C02-US-UTAGAIFLG
                                                                  (IDZ)
                   MOVE    USBYO-MANSEIKBN
                                         TO  SPA-NAI-C02-US-TOKSKNCD
                                                                  (IDZ)
                   MOVE    USBYO-BYOMEIHENFLG
                                         TO
                                       SPA-NAI-C02-US-BYOMEIHENFLG(IDZ)
                   MOVE    USBYO-HKNBYOMEIFLG
                                         TO
                                       SPA-NAI-C02-US-HKNBYOMEIFLG(IDZ)
                   MOVE    USBYO-BYOMEISBT
                                         TO  SPA-NAI-C02-BYOMEISBT(IDZ)
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >   21
                           OR      USBYO-BYOMEICD (IDXX)   =   SPACE
                           OR      USBYO-BYOMEICD (IDXX)   =   ALL "9"
                       MOVE    USBYO-BYOMEICD (IDXX)
                                       TO
                                       SPA-NAI-C02-BYOMEICD-O (IDZ IDXX)
                       IF      SPA-NAI-C02-BYOMEICD-O (IDZ IDXX)
                                       =   WRK-CONS-BYOMEICD
                           MOVE    SPA-NAI-C02-BYOMEI   (IDZ)
                                           TO
                                       SPA-NAI-C02-BYOMEI-O (IDZ IDXX)
                       ELSE                           
                           IF  (   SPA-NAI-C02-BYOMEICD-O(IDZ IDXX)(1:3)
                                               =   "ZZZ"    )
                           AND ( ( SPA-NAI-C02-BYOMEICD-O(IDZ IDXX)(4:1)
                                               <   "1" ) OR 
                                 ( SPA-NAI-C02-BYOMEICD-O(IDZ IDXX)(4:1)
                                               =   "C" )    )
      *                        予約病名コード検索
                               SET     TBL-BYOMEICD-IDX  TO  1
                               SEARCH  TBL-BYOMEICD-OCC
                                               VARYING TBL-BYOMEICD-IDX
                                   AT  END
                                       MOVE    "■■■"    TO
                                         SPA-NAI-C02-BYOMEI-O (IDZ IDXX)
                                   WHEN    SPA-NAI-C02-BYOMEICD-O
                                                              (IDZ IDXX)
                                               =   TBL-YYK-BYOMEICD
                                                      (TBL-BYOMEICD-IDX)
                                       MOVE    TBL-YYK-BYOMEI 
                                                      (TBL-BYOMEICD-IDX)
                                                   TO
                                         SPA-NAI-C02-BYOMEI-O (IDZ IDXX)
                               END-SEARCH
                           ELSE
                               MOVE    SPA-NAI-C02-BYOMEICD-O (IDZ IDXX)
                                               TO   BYO-BYOMEICD
                               PERFORM 900-BYOMEI-READ-SEC
                               IF      FLG-BYOMEI        =   ZERO
                                   MOVE    BYO-BYOMEI
                                               TO
                                     SPA-NAI-C02-BYOMEI-O    (IDZ IDXX)
                                   MOVE    BYO-TANDOKUKBN
                                               TO
                                     SPA-NAI-C02-TANDOKUKBN-O(IDZ IDXX)
                                   IF      BYO-HAISIYMD
                                           NOT =   ZERO    AND ALL "9"
                                       MOVE    BYO-HAISIYMD    TO
                                               SPA-NAI-C02-HAISIYMD-O
                                                              (IDZ IDXX)
                                       IF      SPA-NAI-C02-HAISIYMD(IDZ)
                                                   =   ZERO
                                           MOVE    BYO-HAISIYMD    TO
                                               SPA-NAI-C02-HAISIYMD(IDZ)
                                       END-IF
                                   END-IF
      *                            自院病名で設定した疾患区分と同じとき
      *                            疾患区分の設定を行う
                                   IF    ( SPA-NAI-C02-US-TOKSKNCD
                                                                  (IDZ)
                                           NOT =   "00"                )
                                   AND   ( SPA-NAI-C02-US-TOKSKNCD
                                                                  (IDZ)
                                               =   BYO-TOKSKNCD    OR
                                                   BYO-NANBYOCD        )
                                       IF      SPA-NAI-C02-TOKSKNCD(IDZ)
                                                   =   ZERO
                                           MOVE    BYO-TOKSKNCD    TO
                                               SPA-NAI-C02-TOKSKNCD(IDZ)
                                       END-IF
                                       IF      SPA-NAI-C02-NANBYOCD-O
                                                               (IDZ)
                                               =   ZERO
                                           MOVE    BYO-NANBYOCD    TO
                                               SPA-NAI-C02-NANBYOCD-O
                                                                   (IDZ)
                                       END-IF
                                   END-IF
                               ELSE
                                   MOVE    "■■■"        TO
                                       SPA-NAI-C02-BYOMEI-O  (IDZ IDXX)
                               END-IF
                           END-IF 
                       END-IF    
                   END-PERFORM
      *************MOVE    IDZ               TO  IDX3
      *************PERFORM 4101111-BYOMEITOP-HENSYU-SEC
               ELSE    
                   MOVE    "■■■"      TO  SPA-NAI-C02-BYOMEI   (IDZ)
               END-IF
           END-IF
           .
       410112-USERBYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    開始日チェック処理
      *****************************************************************
       41012-SRYYMD-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-C02-SRYYMD
                                    NOT =   SPA-GMN-C02-SRYYMD-OLD
               MOVE   9                     TO  SPA-NAI-C02-UPDOWN
               MOVE   0                     TO  SPA-NAI-C02-SRYYMD-CHK
           END-IF  
      *
           IF     SPA-GMN-C02-SRYYMD    =   SPACE
      *********MOVE    SPA-SYSYMD          TO  SPA-NAI-C02-SRYYMD
               MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
               MOVE    SPACE               TO  SPA-GMN-C02-SRYYMD
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-C02-SRYYMD  TO  SGDAY-INDAY
               MOVE    SPA-NAI-C02-SYSYMD  TO  SGDAY-SYSYMD
               MOVE    "1"                 TO  SGDAY-SYSKBN
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN-C02-SRYYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-C02-SRYYMD
               ELSE
                   MOVE    "0015"          TO  SPA-ERRCD
               END-IF
           END-IF    
      *         
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-C02-SRYYMD  <   SPA-NAI-C02-BIRTHDAY
                   MOVE    "0096"      TO  SPA-ERRCD
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF    ( MCP-WIDGET          =   "SRYYMD" )
               OR    ( SPA-NAI-C02-SRYYMD-CHK
                                           =   ZERO     )
      *            未来日入力チェック（警告）登録時以外チェック
                   IF    ( SPA-NAI-C02-SRYYMD  >   SPA-NAI-C02-SYSYMD )
      *************AND   ( FLG-TOUROKU         =   ZERO               )
                       MOVE    "0039"              TO  SPA-ERRCD
                   END-IF
      *            修正時の開始日入力以外はエラーとしない
                   IF    ( SPA-NAI-C02-SELNO   >   ZERO     )
                   AND   ( MCP-WIDGET      NOT =   "SRYYMD" )
                       MOVE    SPACE               TO  SPA-ERRCD
                   END-IF
                   MOVE    1               TO  SPA-NAI-C02-SRYYMD-CHK
               END-IF 
      *        レセ電エラーフラグ
      *****    PERFORM 41041-RECEDENFLG-CHK-SEC
           ELSE
               MOVE    9                   TO  SPA-GMN-C02-CUR
           END-IF
      *
           .
       41012-SRYYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名コードチェック処理
      *****************************************************************
       4102-BYOMEICD-CHK-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-HAISIYMDH
                                       WRK-SEIGYO
           INITIALIZE                  SPA-C02-BYOMEIINPUTSET-AREA
                                       SPA-C02-BYOSET-INDEX-AREA
      *
           MOVE    MCP-WIDGET      TO  WRK-WIDGET
           MOVE    WRK-WIDGET2     TO  IDZ
      *
           COMPUTE SPA-GMN-C02-CUR     =   1   +   IDZ
      *
      *    自院病名検索へ（自院病名のみ入力のとき）
           IF      C02-BYOMEICD (IDZ)(1:2) =   "//"
               MOVE    ZERO                    TO  WRK-BUNRUICD
               IF      C02-BYOMEICD (IDZ) (3:) NOT =   SPACE
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    C02-BYOMEICD (IDZ) (3:)
                                               TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF      SNUM-RC             =   ZERO
                   AND     SNUM-SEISUU         <   4
                       MOVE    SNUM-OUTNUM         TO  WRK-BUNRUICD
                   END-IF
               END-IF
               PERFORM 490-USERBYOMEI-KENSAKU-SEC
               GO  TO  4102-BYOMEICD-CHK-EXT
           END-IF     
      *
           IF    ( C02-BYOMEICD  (IDZ)
                                   =   SPACE )
           AND   ( SPA-GMN-C02-BYOMEICD (IDZ)
                               NOT =   SPACE )
               STRING   SPA-GMN-C02-BYOMEICD (IDZ)
                                       DELIMITED  BY  SPACE
                        "-"            DELIMITED  BY  SIZE
                                       INTO    C02-BYOMEICD  (IDZ)
               END-STRING
           END-IF
      *
           IF    ( C02-BYOMEICD  (IDZ)
                                   =   SPACE )
           AND   ( SPA-GMN-C02-BYOMEICD-G  
                               NOT =   SPACE )
      *      病名コード未入力時他の病名コードに入力があるときの処理
             IF    ( C02-BYOMEICD  (1)
                                   =   SPA-GMN-C02-BYOMEICD (1) )
             AND   ( C02-BYOMEICD  (2)
                                   =   SPA-GMN-C02-BYOMEICD (2) )
             AND   ( C02-BYOMEICD  (3)
                                   =   SPA-GMN-C02-BYOMEICD (3) )
             AND   ( C02-BYOMEICD  (4)
                                   =   SPA-GMN-C02-BYOMEICD (4) )
             AND   ( C02-BYOMEICD  (5)
                                   =   SPA-GMN-C02-BYOMEICD (5) )
             AND   ( C02-BYOMEICD  (6)
                                   =   SPA-GMN-C02-BYOMEICD (6) )
                 MOVE    ZERO            TO  FLG-CHANGE
                 PERFORM VARYING IDX3    FROM    IDZ   BY  1
                         UNTIL   IDX3    >       6
                     IF       SPA-GMN-C02-BYOMEICD (IDX3)
                                         NOT =   SPACE
                         MOVE    1               TO  FLG-CHANGE
                         MOVE    6               TO  IDX3
                     END-IF
                 END-PERFORM
                 IF      FLG-CHANGE    =   ZERO
                     IF      SPA-NAI-C02-BYOMEICD-MAX
                                           >   1
                         MOVE    9             TO  SPA-GMN-C02-CUR
                     ELSE
                         MOVE    34            TO  SPA-GMN-C02-CUR
                     END-IF
                     MOVE    SPACE         TO  WRK-SEIGYO
                     GO  TO  4102-BYOMEICD-CHK-EXT
                 END-IF
             END-IF
           ELSE
      *      病名欄入力後、病名コード置換え後にコード入力した時の処理
             IF    ( SPA-NAI-C02-BYOMEICD (1)
                               NOT =   SPA-GMN-C02-BYOMEICD (1) )
             OR    ( SPA-NAI-C02-BYOMEICD (2)
                               NOT =   SPA-GMN-C02-BYOMEICD (2) )
             OR    ( SPA-NAI-C02-BYOMEICD (3)
                               NOT =   SPA-GMN-C02-BYOMEICD (3) )
             OR    ( SPA-NAI-C02-BYOMEICD (4)
                               NOT =   SPA-GMN-C02-BYOMEICD (4) )
             OR    ( SPA-NAI-C02-BYOMEICD (5)
                               NOT =   SPA-GMN-C02-BYOMEICD (5) )
             OR    ( SPA-NAI-C02-BYOMEICD (6)
                               NOT =   SPA-GMN-C02-BYOMEICD (6) )
             OR    ( SPA-NAI-C02-BYOMEICD (7)
                               NOT =   SPACE                    )
             OR    ( SPA-NAI-C02-BYOMEICD (1)
                               NOT =   SPACE               AND
                     SPA-NAI-C02-BYOMEICD-O (1 1)
                                   =   SPACE                    )
             OR    ( SPA-NAI-C02-BYOMEICD (2)
                               NOT =   SPACE               AND
                     SPA-NAI-C02-BYOMEICD-O (2 1)
                                   =   SPACE                    )
             OR    ( SPA-NAI-C02-BYOMEICD (3)
                               NOT =   SPACE               AND
                    SPA-NAI-C02-BYOMEICD-O (3 1)
                                   =   SPACE                    )
             OR    ( SPA-NAI-C02-BYOMEICD (4)
                               NOT =   SPACE               AND
                     SPA-NAI-C02-BYOMEICD-O (4 1)
                                   =   SPACE                    )
             OR    ( SPA-NAI-C02-BYOMEICD (5)
                               NOT =   SPACE               AND
                     SPA-NAI-C02-BYOMEICD-O (5 1)
                                   =   SPACE                    )
             OR    ( SPA-NAI-C02-BYOMEICD (6)
                               NOT =   SPACE               AND
                     SPA-NAI-C02-BYOMEICD-O (6 1)
                                   =   SPACE                    )
               INITIALIZE                    SPA-NAI-C02-BYOMEI-TBL
               PERFORM VARYING IDZ FROM    1   BY  1
                       UNTIL   IDZ >       6
                   MOVE    SPA-GMN-C02-BYOMEICD (IDZ)
                                       TO  SPA-NAI-C02-BYOMEICD (IDZ)
                   IF      SPA-NAI-C02-BYOMEICD(IDZ)   =   SPACE
                       CONTINUE
                   ELSE 
                       MOVE    ZERO                TO  WRK-LENGTH                 
                       PERFORM VARYING IDX FROM    10  BY  -1
                               UNTIL   IDX <       1
                           IF      SPA-NAI-C02-BYOMEICD(IDZ)(IDX:1)
                                               NOT =   SPACE
                               MOVE    IDX             TO  WRK-LENGTH
                               MOVE    1               TO  IDX
                           END-IF
                       END-PERFORM
                       EVALUATE    WRK-LENGTH
                           WHEN    7
                               MOVE    SPA-NAI-C02-BYOMEICD (IDZ)
                                               TO  BYO-BYOMEICD
                               PERFORM 410111-BYOMEI-HENSYU-SEC
                               IF      FLG-BYOMEI  =   ZERO
                                   CONTINUE
                               ELSE
                                   INITIALIZE          USERBYOMEI-REC
grpsys                             MOVE    SPA-HOSPNUM
                                               TO  USBYO-HOSPNUM
                                   MOVE    SPA-NAI-C02-BYOMEICD(IDZ)
                                               TO  USBYO-BYOMEIINPUTCD
                                   PERFORM 410112-USERBYOMEI-HENSYU-SEC
                               END-IF 
                           WHEN    OTHER
                               INITIALIZE          USERBYOMEI-REC
grpsys                         MOVE    SPA-HOSPNUM
                                               TO  USBYO-HOSPNUM
                               MOVE    SPA-NAI-C02-BYOMEICD(IDZ)
                                               TO  USBYO-BYOMEIINPUTCD
                               PERFORM 410112-USERBYOMEI-HENSYU-SEC
                       END-EVALUATE
                   END-IF 
               END-PERFORM
               MOVE    WRK-WIDGET2     TO  IDZ
             END-IF
           END-IF
      *
      *    コードの挿入・削除の制御文字取得
           IF      C02-BYOMEICD (IDZ)  NOT =   SPACE
               PERFORM VARYING IDX FROM    12  BY  -1
                       UNTIL   IDX <       2
                   IF      C02-BYOMEICD (IDZ)(IDX:1)
                                       NOT =   SPACE
                       EVALUATE    C02-BYOMEICD (IDZ)(IDX:1)
                           WHEN    "-"
                               MOVE    "-"         TO  WRK-SEIGYO
                           WHEN    "+"
                               MOVE    "+"         TO  WRK-SEIGYO
                           WHEN    OTHER
                               IF      IDX             >   2
                                   COMPUTE IDW     =   IDX     -   1
                                   IF      C02-BYOMEICD (IDZ)(IDW:2)
                                                   =   "−"
                                       MOVE    "-"     TO  WRK-SEIGYO
                                   END-IF
                                   IF      C02-BYOMEICD (IDZ)(IDW:2)
                                                   =   "＋"
                                       MOVE    "+"     TO  WRK-SEIGYO
                                   END-IF
                               END-IF
                       END-EVALUATE
      *                追加のとき
                       IF      WRK-SEIGYO      =   "+"
                           IF      SPA-GMN-C02-BYOMEICD (6)
                                                   NOT =   SPACE
                               MOVE    SPACE           TO  WRK-SEIGYO
                           END-IF 
                           IF      IDZ         >   1
                               COMPUTE IDW     =   IDZ     -   1
                               IF      SPA-NAI-C02-BYOMEICD (IDW)
                                                           =   SPACE 
                                   MOVE    SPACE       TO  WRK-SEIGYO
                               END-IF
                           END-IF
                           IF      WRK-SEIGYO      =   SPACE
                               COMPUTE SPA-GMN-C02-CUR
                                                   =   1   +   IDZ
                               GO  TO  4102-BYOMEICD-CHK-EXT
                           END-IF
                       END-IF
                       MOVE    2               TO  IDX
                   END-IF 
               END-PERFORM
      *****ELSE
      *****    MOVE    "-"                     TO  WRK-SEIGYO
           END-IF
      *       
           EVALUATE    WRK-SEIGYO
               WHEN    "-"
      *            クリアした自院病名の補足コメントと同一のとき、補足コメントをクリア
                   IF      SPA-NAI-C02-US-HOSOKU-COMMENT(IDZ)
                                               =
                                           SPA-GMN-C02-HOSOKU-COMMENT
                           INITIALIZE      SPA-GMN-C02-HOSOKUCD-G
                                           SPA-GMN-C02-HOSOKU-COMMENT
                   END-IF
                   PERFORM VARYING IDX FROM    IDZ BY  1
                           UNTIL   IDX >       6
                       IF      IDX             =   6
                           INITIALIZE  SPA-NAI-C02-BYOMEI-OCC    (IDX)
                                       SPA-GMN-C02-BYOMEICD      (IDX)
                       ELSE
                           COMPUTE IDW     =   IDX     +   1
                           MOVE    SPA-NAI-C02-BYOMEI-OCC    (IDW)
                                   TO  SPA-NAI-C02-BYOMEI-OCC (IDX)
                           MOVE    SPA-GMN-C02-BYOMEICD      (IDW)
                                   TO  SPA-GMN-C02-BYOMEICD   (IDX)
                       END-IF                     
                   END-PERFORM                           
                   PERFORM 41021-BYOMEICD-HENSYU-SEC
                   MOVE    SPACE           TO  WRK-SEIGYO     
                   GO  TO  4102-BYOMEICD-CHK-EXT
               WHEN    "+"
                   PERFORM VARYING IDX FROM    6   BY  -1
                           UNTIL   IDX <       IDZ
                       IF      IDX             =   IDZ
                           INITIALIZE  SPA-NAI-C02-BYOMEI-OCC(IDX)
                                       SPA-GMN-C02-BYOMEICD  (IDX)
                       ELSE
                           COMPUTE IDW     =   IDX     -   1
                           MOVE    SPA-NAI-C02-BYOMEI-OCC (IDW)
                                   TO  SPA-NAI-C02-BYOMEI-OCC(IDX)
                           MOVE    SPA-GMN-C02-BYOMEICD   (IDW)
                                   TO  SPA-GMN-C02-BYOMEICD  (IDX)
                       END-IF                     
                   END-PERFORM
                   PERFORM 41021-BYOMEICD-HENSYU-SEC
                   MOVE    SPACE           TO  WRK-SEIGYO     
                   GO  TO  4102-BYOMEICD-CHK-EXT
           END-EVALUATE                               
      *
      *    半角チェック
           IF      C02-BYOMEICD (IDZ)  NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    C02-BYOMEICD (IDZ)  TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
               AND     KANACHK-RC2         =   ZERO
                   MOVE    KANACHK-OUT-INPUT   TO  C02-BYOMEICD (IDZ)
               ELSE
                   MOVE    "0072"              TO  SPA-ERRCD
                   GO  TO  4102-BYOMEICD-CHK-EXT
               END-IF
      *             
      *    未コード化傷病名入力のチェック
               IF      C02-BYOMEICD (IDZ)
                                       =   WRK-CONS-BYOMEICD
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       6
                       IF      SPA-NAI-C02-BYOMEICD (IDX)
                                              NOT =  SPACE
                       AND     IDX            NOT =   IDZ                       
                           MOVE    "0062"      TO  SPA-ERRCD
                           MOVE    6           TO  IDX
                       END-IF
                   END-PERFORM                                    
               ELSE
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       6
                       IF      SPA-NAI-C02-BYOMEICD(IDX)
                                               =   WRK-CONS-BYOMEICD
                       AND     IDX         NOT =   IDZ                       
                           MOVE    "0063"      TO  SPA-ERRCD
                       END-IF
                   END-PERFORM                                    
               END-IF
           END-IF
      *    
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-BYOMEICD-CHK-EXT
           END-IF
      *
           MOVE    SPACE               TO  C02-BYOMEICD (IDZ) (11:2)
           MOVE    C02-BYOMEICD (IDZ)  TO  SPA-GMN-C02-BYOMEICD (IDZ)
                                           SPA-NAI-C02-BYOMEICD (IDZ)
      *
           INITIALIZE                  SPA-NAI-C02-BYOMEI-G(IDZ)
                                       SPA-NAI-C02-USERBYOMEIFLG (IDZ)
                                       SPA-NAI-C02-BYOMEITOP     (IDZ)
                                       SPA-NAI-C02-BYOMEISBT (IDZ)
                                       WRK-HAISIYMDH
      *         
           IF      C02-BYOMEICD (IDZ) (1:1)
                                       =   "."
               MOVE    "ZZZ"       TO  SPA-GMN-C02-BYOMEICD(IDZ)(1:3)
               MOVE    C02-BYOMEICD(IDZ)(2:)
                                   TO  SPA-GMN-C02-BYOMEICD(IDZ)(4:)
               MOVE    SPA-GMN-C02-BYOMEICD (IDZ)
                                   TO  SPA-NAI-C02-BYOMEICD(IDZ)
           END-IF                                                       
      *
           MOVE    ZERO                TO  WRK-LENGTH                    
           PERFORM VARYING IDX FROM    10  BY  -1
                   UNTIL   IDX <       1
               IF      SPA-NAI-C02-BYOMEICD(IDZ)(IDX:1)
                                   NOT =   SPACE
                   MOVE    IDX             TO  WRK-LENGTH
                   MOVE    1               TO  IDX
               END-IF
           END-PERFORM
      *
           IF      WRK-LENGTH          =   ZERO
               PERFORM VARYING IDX FROM    IDZ BY  1
                       UNTIL   IDX >       6
                   IF      IDX             =   6
                       INITIALIZE  SPA-NAI-C02-BYOMEI-OCC(IDX)
                                   SPA-GMN-C02-BYOMEICD  (IDX)
                   ELSE
                       COMPUTE IDW     =   IDX     +   1
                       MOVE    SPA-NAI-C02-BYOMEI-OCC (IDW)
                                       TO  SPA-NAI-C02-BYOMEI-OCC (IDX)
                       MOVE    SPA-GMN-C02-BYOMEICD   (IDW)
                                       TO  SPA-GMN-C02-BYOMEICD   (IDX)
                   END-IF                     
               END-PERFORM
           ELSE
      *        ７桁入力のときは病名マスタ、以外は自院病名マスタを先に読み込む
               IF         WRK-LENGTH       =   7
      *            病名マスタ読み込み
                   MOVE    SPA-NAI-C02-BYOMEICD (IDZ) TO  BYO-BYOMEICD
                   PERFORM 410111-BYOMEI-HENSYU-SEC
                   IF      FLG-BYOMEI      =   ZERO
                       IF    ( BYO-IKOSAKICD   NOT =   SPACE )
                       AND   ( SPA-C02-1042-MSG2-DSPKBN
                                                   =   ZERO  )
                           MOVE    BYO-IKOSAKICD   TO  BYO-BYOMEICD
                           MOVE    ZERO            TO  FLG-BYOMEI
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      FLG-BYOMEI          =   ZERO
                               MOVE    SPA-NAI-C02-BYOMEICD (IDZ)
                                                   TO
                                               SPA-C02-IKOMOTO-BYOMEICD
                               MOVE    SPA-NAI-C02-BYOMEI   (IDZ)
                                                   TO
                                               SPA-C02-IKOMOTO-BYOMEI
                               MOVE    SPA-NAI-C02-HAISIYMD (IDZ)
                                                   TO
                                               SPA-C02-IKOMOTO-HAISIYMD
                               MOVE    BYO-BYOMEICD
                                                   TO
                                               SPA-C02-IKOSAKI-BYOMEICD
                               MOVE    BYO-BYOMEI  TO
                                               SPA-C02-IKOSAKI-BYOMEI
                               MOVE    IDZ         TO
                                               SPA-C02-IKOSAKI-INDEX
                               MOVE    "0103"      TO  SPA-CIDCD2
      *                        別のコードでの組み立てを検索
                               PERFORM 500-BYOMEI-KENSAKU-SEC
                               GO  TO  4102-BYOMEICD-CHK-EXT
                           END-IF
                       ELSE    
                           IF      BYO-HAISIYMD    =   ALL "9" OR  SPACE
                               CONTINUE
                           ELSE
                               MOVE    SPA-NAI-C02-BYOMEICD (IDZ)
                                                   TO
                                               SPA-C02-IKOMOTO-BYOMEICD
                               MOVE    SPA-NAI-C02-BYOMEI   (IDZ)
                                                   TO
                                               SPA-C02-IKOMOTO-BYOMEI
                               MOVE    SPA-NAI-C02-HAISIYMD (IDZ)
                                                   TO
                                               SPA-C02-IKOMOTO-HAISIYMD
                               MOVE    IDZ         TO
                                               SPA-C02-IKOSAKI-INDEX                    
                               INITIALIZE              SYNONYM-REC
                               MOVE    SPA-NAI-C02-BYOMEI   (IDZ)
                                                   TO  SYNONYM-BYOMEI
                               PERFORM 5001-SYNONYM-CHK-SEC
                               IF      FLG-SYNONYM =   ZERO
                                   MOVE    BYO-BYOMEICD
                                                       TO
                                               SPA-C02-IKOSAKI-BYOMEICD
                                   MOVE    BYO-BYOMEI  TO
                                               SPA-C02-IKOSAKI-BYOMEI
                                   MOVE    "0105"      TO  SPA-CIDCD2
      *                            別のコードでの組み立てを検索
                                   PERFORM 500-BYOMEI-KENSAKU-SEC
                                   GO  TO  4102-BYOMEICD-CHK-EXT
                               ELSE 
                                   MOVE    SPA-NAI-C02-HAISIYMD (IDZ)
                                                       TO  WRK-SYMD
                                   PERFORM 5002-HIZUKE-HEN-SEC
                                   MOVE    LNK-DAY2-EDTYMD3
                                                       TO  WRK-HAISIYMDH
                                   MOVE    SPA-NAI-C02-BYOMEI   (IDZ)
                                                       TO WRK-ERR-BYOMEI
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-BYOMEI
               END-IF
      *
               IF      FLG-BYOMEI              =   1
      *            自院病名マスタ読込
                   INITIALIZE                      USERBYOMEI-REC
                   MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
                   MOVE    SPA-NAI-C02-BYOMEICD(IDZ)
                                               TO  USBYO-BYOMEIINPUTCD
                   PERFORM 410112-USERBYOMEI-HENSYU-SEC
                   IF      FLG-USERBYOMEI      =   ZERO
                       PERFORM 41025-USERBYOMEICD-CHK-SEC
                       IF      FLG-USERBYOMEI-CHK  =   ZERO
                           GO  TO  4102-BYOMEICD-CHK-EXT
                       END-IF
                   ELSE
      *                自院病名検索
                       PERFORM 41029-USERBYOMEICD-KENSAKU-SEC
                       IF      SPA-ERRCD           =   SPACE
                           PERFORM 41021-BYOMEICD-HENSYU-SEC
                           MOVE    SPACE           TO  SPA-ERRCD
                           GO  TO  4102-BYOMEICD-CHK-EXT
                       ELSE
                           IF      SPA-ERRCD       =   "0053"
      *                        病名入力セット読込
                               INITIALIZE          BYOMEIINPUTSET-REC
                               MOVE    SPA-HOSPNUM TO  BYOSET-HOSPNUM
                               MOVE    SPA-NAI-C02-BYOMEICD(IDZ)
                                                   TO
                                                   BYOSET-BYOMEIINPUTSET
                               MOVE    BYOMEIINPUTSET-REC
                                                   TO  MCPDATA-REC
                               PERFORM 900-BYOMEIINPUTSET-READ-SEC
                               IF      FLG-BYOMEIINPUTSET  =   ZERO
                                   IF      SPA-GMN-C02-BANGO   =   ZERO
                                       PERFORM
                                          41026-BYOMEIINPUTSET-CHECK-SEC
                                   ELSE
                                       MOVE    "0098"      TO  SPA-ERRCD
                                   END-IF
                               ELSE
      *                            病名入力セット検索
                                   PERFORM
                                   41028-BYOMEICDINPUTSET-KEN-SEC
                                   IF      FLG-BYOMEIINPUTSET   =   ZERO
                                       PERFORM 41021-BYOMEICD-HENSYU-SEC
                                       MOVE    SPACE       TO  SPA-ERRCD
                                       GO  TO  4102-BYOMEICD-CHK-EXT
                                   ELSE
                                       IF      WRK-LENGTH       =   7
                                           MOVE    "0054"  TO  SPA-ERRCD
                                       ELSE
                                           MOVE    "0053"  TO  SPA-ERRCD
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-C02-BYOSET-INDEX-MAX
                                       >   ZERO
               GO  TO  4102-BYOMEICD-CHK-EXT
           END-IF     
      *
      *    廃止病名のとき
           IF    ( WRK-HAISIYMDH   NOT =   SPACE )
           AND   ( SPA-C02-1042-MSG3-DSPKBN
                                       =   "0"   )
      *        別のコードでの組み立てを検索
               PERFORM 500-BYOMEI-KENSAKU-SEC
      *
               IF      SPA-CIDCD6      NOT =   SPACE
                   GO  TO  4102-BYOMEICD-CHK-EXT
               END-IF
           END-IF
      *
           PERFORM 41021-BYOMEICD-HENSYU-SEC
           MOVE    SPACE           TO  WRK-SEIGYO
           .
       4102-BYOMEICD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    別のコードでの組み立てを検索処理
      *****************************************************************
       500-BYOMEI-KENSAKU-SEC      SECTION.
      *
           IF      SPA-NAI-C02-SELNO   =   ZERO           
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL   IDX3    >       6
                   PERFORM 4101111-BYOMEITOP-HENSYU-SEC
               END-PERFORM
           END-IF
      *
           MOVE    SPACE               TO  ORCSBYOMEIAREA
           INITIALIZE                      ORCSBYOMEIAREA
           MOVE    "1"                 TO  LNK-BYO-SYORI
           IF      SPA-NAI-C02-SELNO   =   ZERO
               MOVE    SPACE               TO  WRK-BYOMEI
               MOVE    ZERO                TO  IDYY
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       6
                   IF      SPA-NAI-C02-BYOMEITOP (IDY)
                                               =   "1"
                       IF      IDY                 >   IDZ
                           COMPUTE SPA-C02-CHANGE-INDEX-ED
                                          =    IDY     -   1
                           MOVE    6                   TO  IDY
                       ELSE
                           MOVE    SPA-NAI-C02-BYOMEI (IDY)
                                                       TO  WRK-BYOMEI
                           MOVE    IDY                 TO
                                               SPA-C02-CHANGE-INDEX-ST
                           IF      IDY                 =   6
                               MOVE    IDY                 TO
                                               SPA-C02-CHANGE-INDEX-ED
                           END-IF
                       END-IF
                   ELSE
                       MOVE    IDY                 TO
                                               SPA-C02-CHANGE-INDEX-ED
                       STRING  WRK-BYOMEI          DELIMITED  BY  SPACE
                           SPA-NAI-C02-BYOMEI(IDY) DELIMITED  BY  SPACE
                                  INTO          WRK-BYOMEI
                       END-STRING
                   END-IF    
               END-PERFORM
               MOVE    WRK-BYOMEI          TO  LNK-BYO-BYOMEI
           ELSE
               MOVE    1                   TO  SPA-C02-CHANGE-INDEX-ST
               MOVE    6                   TO  SPA-C02-CHANGE-INDEX-ED
               STRING  SPA-NAI-C02-BYOMEI(1)   DELIMITED   BY  SPACE
                       SPA-NAI-C02-BYOMEI(2)   DELIMITED   BY  SPACE
                       SPA-NAI-C02-BYOMEI(3)   DELIMITED   BY  SPACE
                       SPA-NAI-C02-BYOMEI(4)   DELIMITED   BY  SPACE
                       SPA-NAI-C02-BYOMEI(5)   DELIMITED   BY  SPACE
                       SPA-NAI-C02-BYOMEI(6)   DELIMITED   BY  SPACE
                                               INTO    LNK-BYO-BYOMEI
               END-STRING
           END-IF
           COMPUTE SPA-C02-CHANGE-INDEX-MAX
                   =   SPA-C02-CHANGE-INDEX-ED -
                       SPA-C02-CHANGE-INDEX-ST +   1
      *
           CALL    "ORCSBYOMEI"        USING   ORCSBYOMEIAREA
                                               SPA-AREA
           IF    ( LNK-BYO-BYOMEIHENFLG    =   SPACE )
           AND   ( LNK-BYO-IKOSAKICD       =   SPACE )
           AND   ( LNK-BYO-HAISIYMD-ERR    =   ZERO  )
               MOVE    LNK-BYO-BYOMEI-MAX  TO SPA-C02-CHANGE-MAX
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       LNK-BYO-BYOMEI-MAX
                   MOVE    LNK-BYO-TBYOMEICD (IDY) TO
                                           SPA-C02-CHANGE-BYOMEICD(IDY) 
                   MOVE    LNK-BYO-TBYOMEI   (IDY) TO
                                           SPA-C02-CHANGE-BYOMEI  (IDY) 
               END-PERFORM
      *
               IF    ( WRK-HAISIYMDH   NOT =   SPACE )
      **             MOVE    SPA-NAI-C02-BYOMEICD        (IDZ)
      **                                     TO  SPA-C02-IKOMOTO-BYOMEICD
      **             MOVE    SPA-NAI-C02-BYOMEI          (IDZ)
      **                                     TO  SPA-C02-IKOMOTO-BYOMEI
      **           MOVE    SPA-NAI-C02-HAISIYMD(IDZ)
      **                                     TO  WRK-HAISIYMDH
      *
                   IF      SPA-NAI-C02-USERBYOMEIFLG (IDZ)
                                           =   "1"
                       MOVE    "0109"          TO  SPA-CIDCD6
                   ELSE
                       MOVE    "0107"          TO  SPA-CIDCD6
                   END-IF
               ELSE
      **         IF      SPA-NAI-C02-IKOSAKI-BYOMEICD (IDX)
      **                                         NOT =   SPACE
      **             MOVE    SPA-NAI-C02-BYOMEICD        (IDZ)
      **                                     TO  SPA-C02-IKOMOTO-BYOMEICD
      **             MOVE    SPA-NAI-C02-BYOMEI          (IDZ)
      **                                     TO  SPA-C02-IKOMOTO-BYOMEI
      **             MOVE    SPA-NAI-C02-IKOSAKI-BYOMEICD(IDZ)
      **                                     TO  SPA-C02-IKOSAKI-BYOMEICD
      **             MOVE    SPA-NAI-C02-IKOSAKI-BYOMEI  (IDZ)
      **                                     TO  SPA-C02-IKOSAKI-BYOMEI
      **             MOVE    IDZ    TO  SPA-C02-IKOSAKI-INDEX
      *
                   MOVE    SPA-CIDCD2      TO  SPA-CIDCD6
                   MOVE    SPACE           TO  SPA-CIDCD2
               END-IF 
      ****     INITIALIZE          SPA-NAI-C02-HAISI (IDX)
           ELSE
               IF      SPA-CIDCD2          =   "0108"
                   MOVE    SPACE           TO  SPA-CIDCD2
               END-IF
           END-IF
           .
       500-BYOMEI-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名コード編集処理（病名コード入力時）
      *****************************************************************
       41021-BYOMEICD-HENSYU-SEC      SECTION.
      *
      *    病名編集処理
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >       6
               PERFORM 4101111-BYOMEITOP-HENSYU-SEC
           END-PERFORM
           PERFORM 41021-BYOMEI-HENSYU-SEC
           PERFORM 41022-CHARTBYOMEI-HENSYU-SEC
      *
      *   入力病名コード数カウント
           PERFORM 410111-BYOMEICDSU-HENSYU-SEC
      *                                       
      *    新規（行指定がない場合）以外のときは複数病名の入力不可
           IF      SPA-ERRCD               =   SPACE  
               IF    ( SPA-NAI-C02-BYOMEICD-MAX    >   1    )
                   IF    ( SPA-GMN-C02-BANGO           >   ZERO )
                       MOVE    "0056"             TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF        
      *    
      *    複数病名のとき補足コメントの入力不可
           IF      SPA-ERRCD               =   SPACE  
               IF    ( SPA-NAI-C02-BYOMEICD-MAX    >   1     )
               AND   ( SPA-GMN-C02-HOSOKU-COMMENT
                                       NOT =   SPACE )
                   MOVE    "0094"              TO  SPA-ERRCD
                   INITIALIZE                  SPA-GMN-C02-HOSOKUCD-G
                                               SPA-NAI-C02-HOSOKU-TBL
                   MOVE    SPACE               TO
                                           SPA-GMN-C02-HOSOKU-COMMENT
               END-IF
           END-IF        
      *
      *    括弧で始まる部位が末尾の警告又は複数病名のときの補足コメントの
      *    入力以外のとき
           IF    ( SPA-ERRCD            NOT =   SPACE  )
           AND   ( SPA-ERRCD            NOT =   "0079"  AND
                                                "0094" )
               GO  TO  41021-BYOMEICD-HENSYU-EXT
           END-IF
      *
      *    レセ電エラーフラグ（複数病名以外）
           IF      SPA-NAI-C02-BYOMEICD-MAX    =   1
               PERFORM 41041-RECEDENFLG-CHK-SEC
           ELSE
               MOVE    SPACE               TO  SPA-NAI-C02-RECEDENFLG
           END-IF
      *
      *    全ての病名コード未入力のとき病名へ遷移
           IF    ( IDZ                 =   1      )
           AND   ( SPA-GMN-C02-BYOMEICD-G  
                                       =   SPACE  )
               MOVE    8               TO  SPA-GMN-C02-CUR
               GO  TO  41021-BYOMEICD-HENSYU-EXT
           END-IF
      *    病名コード未入力のとき開始日へ遷移     
           IF      SPA-NAI-C02-BYOMEICD (IDZ)  =   SPACE
               IF      WRK-SEIGYO      =   "-"
                   GO  TO  41021-BYOMEICD-HENSYU-EXT
               END-IF
               PERFORM VARYING IDX3    FROM    IDZ   BY  1
                       UNTIL   IDX3    >       6
                   IF       SPA-NAI-C02-BYOMEICD (IDX3)
                                       NOT =   SPACE
                       GO  TO  41021-BYOMEICD-HENSYU-EXT
                   END-IF
               END-PERFORM 
               IF      SPA-NAI-C02-BYOMEICD-MAX
                                       >   1
                   MOVE    9               TO  SPA-GMN-C02-CUR
               ELSE
                   MOVE    34              TO  SPA-GMN-C02-CUR
               END-IF
               GO  TO  41021-BYOMEICD-HENSYU-EXT
           END-IF    
      *    複数病名コード入力のとき開始日へ遷移
           IF      WRK-SEIGYO      =   "-"
               CONTINUE
           ELSE
               ADD     1               TO  SPA-GMN-C02-CUR
           END-IF
           IF      SPA-GMN-C02-CUR =   8
               IF      SPA-NAI-C02-BYOMEICD-MAX
                                       >   1
                   MOVE    9               TO  SPA-GMN-C02-CUR
               ELSE
                   MOVE    34              TO  SPA-GMN-C02-CUR
               END-IF
           END-IF
      *
      *    廃止病名のとき
           IF    ( WRK-HAISIYMDH       =   SPACE )
           OR    ( SPA-C02-1042-MSG3-DSPKBN
                                       =   "1"   )
               CONTINUE
           ELSE
               MOVE    "0075"              TO  SPA-ERRCD
           END-IF                     
           .
       41021-BYOMEICD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名コード編集チェック処理
      *****************************************************************
       41021-BYOMEI-HENSYU-SEC        SECTION.
      *         
           MOVE    ZERO                TO  SPA-NAI-C02-BYOMEIMOJI
           MOVE    SPACE               TO  SPA-GMN-C02-BYOMEIHENFLG
           MOVE    SPACE               TO  SPA-NAI-C02-BYOMEIHENFLG
      *
      *    病名編集
           MOVE    SPA-NAI-C02-BYOMEI(1)
                                       TO  WRK-BYOMEI
           PERFORM VARYING IDX FROM    2   BY  1
                   UNTIL   IDX >       21
               IF      SPA-NAI-C02-BYOMEITOP   (IDX)   =   "1"
               AND     WRK-BYOMEI                  NOT =   SPACE    
                   STRING  WRK-BYOMEI              DELIMITED  BY  SPACE
                           "／"                    DELIMITED  BY  SIZE
                           SPA-NAI-C02-BYOMEI(IDX) DELIMITED  BY  SPACE
                                  INTO             WRK-BYOMEI
                   END-STRING
               ELSE    
                   STRING  WRK-BYOMEI              DELIMITED  BY  SPACE
                           SPA-NAI-C02-BYOMEI(IDX) DELIMITED  BY  SPACE
                                  INTO          WRK-BYOMEI
                   END-STRING
               END-IF    
           END-PERFORM
      **** MOVE    WRK-BYOMEI          TO  SPA-GMN-C02-BYOMEI
      *
           IF      WRK-BYOMEI          NOT =   SPA-GMN-C02-BYOMEI
      *      疑いフラグ設定区分による初期化
             IF      SPA-C02-1042-UTAGAIFLG-KBN
                                           =   "9"
                 MOVE    SPACE                 TO  SPA-GMN-C02-UTAGAIFLG
             END-IF 
      *      疑いフラグ編集
             PERFORM VARYING IDX FROM    1   BY  1
                     UNTIL   IDX >       21
                     OR      SPA-GMN-C02-UTAGAIFLG   =   "3"
               EVALUATE    SPA-NAI-C02-UTAGAIFLG   (IDX)
                   WHEN    "1"
                       IF      SPA-GMN-C02-UTAGAIFLG   =   "2"
                           MOVE    "3"     TO  SPA-GMN-C02-UTAGAIFLG
                        ELSE
                           MOVE    SPA-NAI-C02-UTAGAIFLG(IDX)
                                           TO  SPA-GMN-C02-UTAGAIFLG
                        END-IF    
                   WHEN    "2"
                       IF      SPA-GMN-C02-UTAGAIFLG   =   "1"
                           MOVE    "3"     TO  SPA-GMN-C02-UTAGAIFLG
                        ELSE
                           MOVE    SPA-NAI-C02-UTAGAIFLG(IDX)
                                           TO  SPA-GMN-C02-UTAGAIFLG
                       END-IF
                   WHEN    "3"
                       MOVE    "3"     TO  SPA-GMN-C02-UTAGAIFLG
                END-EVALUATE
             END-PERFORM
             MOVE    SPA-GMN-C02-UTAGAIFLG
                                           TO  WRK-KBN
             PERFORM 5001-UTAGAIFLG-HENSYU-SEC
             MOVE    WRK-KBNMEI            TO  SPA-GMN-C02-UTAGAIFLGMEI
      *
      *      慢性区分編集
             MOVE    SPACE               TO  SPA-GMN-C02-MANSEIKBN-G
             MOVE    ZERO                TO  SPA-NAI-C02-NANBYOCD
                                             SPA-NAI-C02-MANSEIKBN
             PERFORM VARYING IDX FROM    1   BY  1
                     UNTIL   IDX >       21
                 IF      SPA-NAI-C02-US-TOKSKNCD (IDX) NOT =   ZERO
                     IF      SPA-GMN-C02-MANSEIKBN
                                                 =   SPACE
                         MOVE    SPA-NAI-C02-US-TOKSKNCD (IDX)
                                             TO  SPA-GMN-C02-MANSEIKBN
                     END-IF
                     MOVE    21              TO  IDX
                 END-IF   
             END-PERFORM
             PERFORM VARYING IDX FROM    1   BY  1
                     UNTIL   IDX >       21
                 IF      SPA-NAI-C02-NANBYOCD-O (IDX)  NOT =   ZERO
                     MOVE    SPA-NAI-C02-NANBYOCD-O (IDX)
                                         TO  SPA-NAI-C02-NANBYOCD
                     IF      SPA-GMN-C02-MANSEIKBN
                                                 =   SPACE
                         MOVE    SPA-NAI-C02-NANBYOCD-O (IDX)
                                             TO  SPA-GMN-C02-MANSEIKBN
                     END-IF
                     MOVE    21              TO  IDX
                 END-IF   
             END-PERFORM
             PERFORM VARYING IDX FROM    1   BY  1
                     UNTIL   IDX >       21
                 IF      SPA-NAI-C02-TOKSKNCD (IDX)   NOT =   ZERO
                     MOVE    SPA-NAI-C02-TOKSKNCD (IDX)
                                         TO  SPA-NAI-C02-MANSEIKBN
                     IF      SPA-GMN-C02-MANSEIKBN
                                                 =   SPACE
                         MOVE    SPA-NAI-C02-TOKSKNCD (IDX)
                                             TO  SPA-GMN-C02-MANSEIKBN
                     END-IF
                     MOVE    21              TO  IDX
                 END-IF   
             END-PERFORM
      *     
             MOVE    SPA-GMN-C02-MANSEIKBN
                                       TO  WRK-KBN
             PERFORM 5001-MANSEIKBN-HENSYU-SEC
             MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-MANSEIKBNMEI
           END-IF
      *    
           PERFORM 5001-HYOUJI-HENSYU-SEC
      *
      *    保険病名フラグセット
           MOVE    SPACE               TO  SPA-GMN-C02-HKNBYOMEIFLG-G
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6 
               IF      SPA-NAI-C02-US-HKNBYOMEIFLG (IDX)
                                       =   "1"
                   MOVE    "1"         TO  SPA-GMN-C02-HKNBYOMEIFLG
                   MOVE    "保険病名"  TO  SPA-GMN-C02-HKNBYOMEIFLGMEI
                   MOVE    6           TO  IDX
               END-IF
           END-PERFORM                                
      *
           MOVE    WRK-BYOMEI          TO  SPA-GMN-C02-BYOMEI
      *
      *    病名コードエラーの時以下の処理はしない
           IF      SPA-ERRCD            NOT =   SPACE     
               GO  TO  41021-BYOMEI-HENSYU-EXT
           END-IF
      *
      *    文字チェック     
           MOVE    ZERO                TO  WRK-MOJISU
           PERFORM VARYING     IDX     FROM    1   BY  2
                   UNTIL       IDX     >       100
                   OR  WRK-BYOMEI(IDX:2)       =   SPACE
               IF      WRK-BYOMEI(IDX:2)   =   "■"
                   MOVE    "0035"          TO  SPA-ERRCD
                   GO  TO  41021-BYOMEI-HENSYU-EXT
              END-IF
           END-PERFORM
      * 
      *    接尾語・接頭語チェック
           MOVE    SPACE                   TO  WRK-RECECD-TBL
      * 
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       21
      *
               IF      SPA-NAI-C02-BYOMEITOP(IDX)    =   "1"
      *            接頭語のみのとき     
                   IF      WRK-RECECD (1)    NOT =   SPACE   AND
                           WRK-RECECD (2)        =   SPACE   AND
                           WRK-RECECD (3)        =   SPACE 
                       MOVE    "0006"        TO  SPA-ERRCD
                       MOVE    21             TO  IDX
                   ELSE
                     MOVE    SPACE               TO  WRK-RECECD-TBL
                   END-IF
               END-IF  
      *
               IF      SPA-NAI-C02-BYOMEICD(IDX)     NOT =   SPACE
                   IF      SPA-ERRCD             =   "0079"
                       MOVE    SPACE                 TO  SPA-ERRCD
                   END-IF
                   IF    ( SPA-ERRCD             =   SPACE   OR  "0079")
                   AND   ( SPA-NAI-C02-BYOMEIHENFLG
                                             =   SPACE                 )
                       IF      SPA-NAI-C02-USERBYOMEIFLG(IDX)
                                                     =   SPACE
                         MOVE    SPACE                 TO  SPA-ERRCD                      
                         EVALUATE    SPA-NAI-C02-BYOMEICD(IDX)(1:3)
                               ALSO    SPA-NAI-C02-BYOMEICD(IDX)(4:1)
                           WHEN    "ZZZ"         ALSO    "1"  THRU   "7"
                               IF    ( WRK-RECECD (2)    NOT =   SPACE )
                               OR    ( WRK-RECECD (3)    NOT =   SPACE )
                                   MOVE    "0006"    TO  SPA-ERRCD
                                   MOVE    21         TO  IDX
                               ELSE
                                   MOVE    "1"       TO  WRK-RECECD (1)
                               END-IF            
                           WHEN    "ZZZ"       ALSO    "8"  
                               IF      WRK-RECECD (2)    =   SPACE 
                                   MOVE    "0006"     TO  SPA-ERRCD
                                   MOVE    21         TO  IDX
                               ELSE
                                   MOVE    "1"        TO  WRK-RECECD (3)
                               END-IF
                           WHEN    OTHER                 
                               IF      WRK-RECECD (3)  NOT =   SPACE 
                                   MOVE    "0006"      TO  SPA-ERRCD
                                   MOVE    21          TO  IDX
                               ELSE
                                   IF      WRK-RECECD (2)
                                                   NOT =   SPACE 
                                       MOVE    "1"       TO
                                               SPA-NAI-C02-BYOMEIHENFLG
                                   ELSE
                                       MOVE    "1"   TO  WRK-RECECD (2)
                                   END-IF
                               END-IF    
                         END-EVALUATE
                       ELSE
                         EVALUATE    SPA-NAI-C02-BYOMEISBT (IDX)
                           WHEN    "1"
                               IF    ( WRK-RECECD (2)    NOT =   SPACE )
                               OR    ( WRK-RECECD (3)    NOT =   SPACE )
                                   MOVE    "1"       TO
                                               SPA-NAI-C02-BYOMEIHENFLG
                                   IF      SPA-NAI-C02-BYOMEI (IDX)(1:2)
                                                     =   "（"
                                       CONTINUE
                                   ELSE
                                       MOVE    "0079"    TO  SPA-ERRCD
                                   END-IF
                               ELSE
                                   MOVE    "1"       TO  WRK-RECECD (1)
                               END-IF            
                           WHEN    "7"
                               IF    ( WRK-RECECD (2)    NOT =   SPACE )
                               OR    ( WRK-RECECD (3)    NOT =   SPACE )
                                   MOVE    "0006"    TO  SPA-ERRCD
                                   MOVE    21         TO  IDX
                               ELSE
                                   MOVE    "1"       TO  WRK-RECECD (1)
                               END-IF            
                           WHEN    "8"  
                               IF      WRK-RECECD (2)    =   SPACE 
                                   MOVE    "0006"    TO  SPA-ERRCD
                                   MOVE    21         TO  IDX
                               ELSE
                                   MOVE    "1"       TO  WRK-RECECD (3)
                               END-IF
                           WHEN    ZERO
                               IF      WRK-RECECD (3)    NOT =   SPACE 
                                   MOVE    "0006"    TO  SPA-ERRCD
                                   MOVE    21         TO  IDX
                               ELSE
                                   IF      WRK-RECECD (2)
                                                   NOT =   SPACE 
                                       MOVE    "1"       TO
                                               SPA-NAI-C02-BYOMEIHENFLG
                                   ELSE
                                       MOVE    "1"   TO  WRK-RECECD (2)
                                   END-IF
                               END-IF    
                         END-EVALUATE
                       END-IF
                   END-IF   
               END-IF
           END-PERFORM
      *
      *    最後の部位の警告以外は終り
           IF      SPA-ERRCD             =   "0006"
               GO  TO  41021-BYOMEI-HENSYU-EXT
           END-IF
      *     
      *    文字数チェック
           MOVE    SPA-NAI-C02-BYOMEI(1)
                                       TO  WRK-BYOMEI
           PERFORM VARYING IDX FROM    2   BY  1
                   UNTIL   IDX  >      21  
               IF      SPA-NAI-C02-BYOMEITOP   (IDX)   =   "1"   
               AND     WRK-BYOMEI                  NOT =   SPACE
                   INITIALIZE                      ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI 
                   MOVE    WRK-BYOMEI          TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
                   IF      KANACHK-RC          =   ZERO
                   AND     KANACHK-SYUBETU     =   "2"
                       IF      KANACHK-MAX     >   80
                           MOVE    "0007"          TO  SPA-ERRCD
                           GO  TO  41021-BYOMEI-HENSYU-EXT
                       END-IF
                   END-IF 
                   MOVE    SPACE                   TO  WRK-BYOMEI
               END-IF    
               STRING  WRK-BYOMEI              DELIMITED  BY  SPACE
                       SPA-NAI-C02-BYOMEI(IDX) DELIMITED  BY  SPACE
                              INTO          WRK-BYOMEI
               END-STRING
           END-PERFORM    
           IF     WRK-BYOMEI                  NOT =   SPACE     
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    WRK-BYOMEI          TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
               AND     KANACHK-SYUBETU     =   "2"
                   IF      KANACHK-MAX     >   80
                       MOVE    "0007"          TO  SPA-ERRCD
                       GO  TO  41021-BYOMEI-HENSYU-EXT
                   END-IF
      *
      *            病名文字数編集
                   COMPUTE SPA-NAI-C02-BYOMEIMOJI
                                        =   KANACHK-MAX  /   2
               END-IF 
           END-IF    
      *
      *    病名編集フラグセット
           IF      SPA-NAI-C02-BYOMEICD(1)
                                        =   WRK-CONS-BYOMEICD
                   MOVE    "1"             TO  SPA-NAI-C02-BYOMEIHENFLG
           END-IF
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
               IF      SPA-NAI-C02-US-BYOMEIHENFLG (IDX)
                                       =   "1"
                   MOVE    "1"             TO  SPA-NAI-C02-BYOMEIHENFLG
                   MOVE    6               TO  IDX
               END-IF
           END-PERFORM                                
           IF      SPA-NAI-C02-BYOMEIHENFLG    =   "1"
               MOVE    "病名編集"      TO  SPA-GMN-C02-BYOMEIHENFLG
           END-IF
           .
       41021-BYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    カルテ病名編集チェック処理
      *****************************************************************
       41022-CHARTBYOMEI-HENSYU-SEC        SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6 
               IF      SPA-NAI-C02-BYOMEICD (IDX)
                                       =   WRK-CONS-BYOMEICD
                   GO  TO  41022-CHARTBYOMEI-HENSYU-EXT
               END-IF
           END-PERFORM                                
      *
           MOVE    ZERO                TO  SPA-NAI-C02-CHARTBYOMEIMOJI
      *　　病名編集
           MOVE    SPA-NAI-C02-CHARTBYOMEI (1)
                                       TO  WRK-BYOMEI
           PERFORM VARYING IDX FROM    2   BY  1
                   UNTIL   IDX >       6
               IF      SPA-NAI-C02-BYOMEITOP   (IDX)   =   "1"
               AND     WRK-BYOMEI                  NOT =   SPACE 
                   STRING  WRK-BYOMEI              DELIMITED  BY  SPACE
                           "／"                    DELIMITED  BY  SIZE
                           SPA-NAI-C02-CHARTBYOMEI (IDX)
                                                   DELIMITED  BY  SPACE
                                  INTO             WRK-BYOMEI
                   END-STRING
               ELSE    
                   STRING  WRK-BYOMEI              DELIMITED  BY  SPACE
                           SPA-NAI-C02-CHARTBYOMEI (IDX)
                                                   DELIMITED  BY  SPACE
                                  INTO          WRK-BYOMEI
                   END-STRING
               END-IF    
           END-PERFORM    
           MOVE    WRK-BYOMEI          TO  SPA-GMN-C02-CHARTBYOMEI
      *
      *    病名文字数編集
           COMPUTE WRK-MOJISU   =   WRK-MOJISU  /   2        
           MOVE    WRK-MOJISU          TO  SPA-NAI-C02-CHARTBYOMEIMOJI
           .
       41022-CHARTBYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名チェック処理
      *****************************************************************
       41025-USERBYOMEICD-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-USERBYOMEI-CHK
      *
      *    自院病名の病名コードに存在しないコードがあるとき
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       21
                   OR      SPA-NAI-C02-BYOMEI-O (IDZ IDXX)
                                   =       SPACE
               IF      SPA-NAI-C02-BYOMEI-O (IDZ IDXX)
                                           =   "■■■"
                   MOVE    "0091"              TO  SPA-ERRCD
                   GO  TO  41025-USERBYOMEICD-CHK-EXT
               END-IF
           END-PERFORM       
           IF      USBYO-BYOMEIHENFLG      =   SPACE
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL   IDX3    >       21
                       OR    ( USBYO-BYOMEICD (IDX3)
                                       =       SPACE   OR  ALL "9" )
                   IF      USBYO-BYOMEICD (IDX3) (1:1) =   "Z"
                       CONTINUE
                   ELSE
                       MOVE    USBYO-BYOMEICD (IDX3)
                                                   TO  BYO-BYOMEICD
                       MOVE    ZERO                TO  FLG-BYOMEI
                       PERFORM 900-BYOMEI-READ-SEC
                       IF    ( FLG-BYOMEI          =   ZERO  )
                       AND   ( BYO-IKOSAKICD   NOT =   SPACE )
                       AND   ( SPA-C02-1042-MSG2-DSPKBN
                                                   =   ZERO  )
                           MOVE    USBYO-BYOMEICD (IDX3) 
                                           TO  SPA-C02-IKOMOTO-BYOMEICD
                           MOVE    BYO-BYOMEI      TO
                                               SPA-C02-IKOMOTO-BYOMEI
                           MOVE    BYO-HAISIYMD    TO
                                               SPA-C02-IKOMOTO-HAISIYMD
                           MOVE    BYO-IKOSAKICD   TO  BYO-BYOMEICD
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      FLG-BYOMEI      =   ZERO
                               MOVE    BYO-BYOMEICD    TO
                                               SPA-C02-IKOSAKI-BYOMEICD
                               MOVE    BYO-BYOMEI      TO
                                               SPA-C02-IKOSAKI-BYOMEI
                               MOVE    IDZ             TO
                                               SPA-C02-IKOSAKI-INDEX                    
                               MOVE    IDX3            TO
                                               SPA-C02-IKOSAKI-INDEX1                    
                               MOVE    "0104"      TO  SPA-CIDCD2
      *                        別のコードでの組み立てを検索
                               PERFORM 500-BYOMEI-KENSAKU-SEC
                               GO  TO  41025-USERBYOMEICD-CHK-EXT
                           END-IF
                       END-IF
                       MOVE    21                  TO  IDX3
                   END-IF    
               END-PERFORM
               IF      SPA-NAI-C02-HAISIYMD (IDZ)  NOT =   ZERO
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       21
                           OR      SPA-NAI-C02-BYOMEI-O (IDZ IDXX)
                                           =       SPACE
                       IF      SPA-NAI-C02-HAISIYMD-O (IDZ IDXX)
                                           NOT =   ZERO
                           MOVE    SPA-NAI-C02-BYOMEICD-O (IDZ IDXX)
                                           TO  SPA-C02-IKOMOTO-BYOMEICD
                           MOVE    SPA-NAI-C02-BYOMEI-O   (IDZ IDXX)
                                           TO  SPA-C02-IKOMOTO-BYOMEI
                           MOVE    SPA-NAI-C02-HAISIYMD-O (IDZ IDXX)
                                           TO  SPA-C02-IKOMOTO-HAISIYMD
                           MOVE    IDZ     TO  SPA-C02-IKOSAKI-INDEX                    
                           INITIALIZE              SYNONYM-REC
                           MOVE    SPA-NAI-C02-BYOMEI-O (IDZ IDXX)
                                           TO  SYNONYM-BYOMEI
                           PERFORM 5001-SYNONYM-CHK-SEC
                           IF      FLG-SYNONYM =   ZERO
                               MOVE    BYO-BYOMEICD
                                                   TO
                                               SPA-C02-IKOSAKI-BYOMEICD
                               MOVE    BYO-BYOMEI  TO
                                               SPA-C02-IKOSAKI-BYOMEI
                               MOVE    IDXX        TO
                                               SPA-C02-IKOSAKI-INDEX1                    
                               MOVE    "0106"  TO  SPA-CIDCD2
      *                        別のコードでの組み立てを検索
                               PERFORM 500-BYOMEI-KENSAKU-SEC
                               GO  TO  41025-USERBYOMEICD-CHK-EXT
                           END-IF
                           MOVE    21          TO  IDXX 
                       END-IF    
                   END-PERFORM
                   MOVE    SPA-NAI-C02-HAISIYMD (IDZ)
                                               TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HAISIYMDH
                   MOVE    SPA-NAI-C02-BYOMEI   (IDZ)
                                               TO  WRK-ERR-BYOMEI
               END-IF
           ELSE  
               MOVE    "0108"  TO  SPA-CIDCD2
               IF      SPA-NAI-C02-BYOMEICD-O (IDZ 1)
                                           =   WRK-CONS-BYOMEICD
                   MOVE    SPA-NAI-C02-BYOMEICD-O (IDZ 1)
                                           TO  SPA-C02-IKOMOTO-BYOMEICD
                   MOVE    SPA-NAI-C02-BYOMEI-O   (IDZ 1)
                                           TO  SPA-C02-IKOMOTO-BYOMEI
                   MOVE    SPA-NAI-C02-HAISIYMD-O (IDZ 1)
                                           TO  SPA-C02-IKOMOTO-HAISIYMD
                   MOVE    IDZ             TO  SPA-C02-IKOSAKI-INDEX
                   INITIALIZE              SYNONYM-REC
                   MOVE    SPA-NAI-C02-BYOMEI (IDZ)
                                           TO  SYNONYM-BYOMEI
                   PERFORM 5001-SYNONYM-CHK-SEC
                   IF      FLG-SYNONYM     =   ZERO
                       MOVE    BYO-BYOMEICD
                                           TO  SPA-C02-IKOSAKI-BYOMEICD
                       MOVE    BYO-BYOMEI  TO  SPA-C02-IKOSAKI-BYOMEI
                       MOVE    1           TO  SPA-C02-IKOSAKI-INDEX1                    
                       MOVE    "0106"      TO  SPA-CIDCD2
                   END-IF
               END-IF    
      *        別のコードでの組み立てを検索
               PERFORM 500-BYOMEI-KENSAKU-SEC
               IF      SPA-CIDCD2  NOT =   SPACE
                   GO  TO  41025-USERBYOMEICD-CHK-EXT
               END-IF
           END-IF
      *
           MOVE    1                   TO  FLG-USERBYOMEI-CHK
           .
       41025-USERBYOMEICD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名入力セット編集処理
      *****************************************************************
       41026-BYOMEIINPUTSET-CHECK-SEC SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRCD
      *
           INITIALIZE                  SPA-C02-BYOSET-INDEX-AREA
      *
           MOVE    ZERO                TO  WRK-BYOMEI-MAX1
                                           WRK-BYOMEI-MAX2
                                           WRK-BYOMEI-INDEX
      *
      *    セットのコード数とコード欄のチェック
           PERFORM VARYING IDWW    FROM    6   BY  -1
                   UNTIL   IDWW    <       1
               IF      BYOSET-BYOMEIINPUTCD (IDWW)
                                   NOT =   SPACE
                   MOVE    IDWW                TO  WRK-BYOMEI-MAX1
                   MOVE    1                   TO  IDWW
               END-IF
           END-PERFORM
      *
      *
           PERFORM VARYING IDWW    FROM    IDZ  BY  1
                   UNTIL   IDWW    >       6
               IF      IDWW                =   IDZ
                   ADD     1                   TO  WRK-BYOMEI-MAX2
               ELSE
                   IF      SPA-GMN-C02-BYOMEICD (IDWW)
                                          NOT =   SPACE
                       IF      WRK-BYOMEI-INDEX    =   ZERO
                           MOVE    IDWW            TO  WRK-BYOMEI-INDEX
                       END-IF
                   ELSE
                       ADD     1                   TO  WRK-BYOMEI-MAX2
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-BYOMEI-MAX1 >   WRK-BYOMEI-MAX2
               MOVE    "0097"          TO  SPA-ERRCD
               GO  TO  41026-BYOMEIINPUTSET-CHECK-EXT
           END-IF
      *
      *    セットコード退避
           INITIALIZE                      SPA-C02-BYOMEIINPUTSET-AREA
                                           SPA-C02-BYOSET-INDEX-AREA
      *
           MOVE    IDZ                 TO  SPA-C02-BYOSET-INDEX-ST
           MOVE    BYOSET-BYOMEIINPUTCD-G
                                       TO
                                       SPA-C02-BYOSET-BYOMEIINPUTCD-G
           MOVE    WRK-BYOMEI-MAX1     TO  SPA-C02-BYOSET-INDEX-MAX
      *
      *    セットのコードを全て表示できるように入力済コードを移動
           IF      WRK-BYOMEI-INDEX    =   ZERO
               CONTINUE
           ELSE
               COMPUTE IDWW    =    WRK-BYOMEI-MAX1    +
                                  ( 6   -   WRK-BYOMEI-MAX2 )
      *
               MOVE    SPA-GMN-C02-BYOMEICD-G  TO
                                               WRK-GMN-C02-BYOMEICD-G
               MOVE    SPA-NAI-C02-BYOMEI-TBL  TO
                                               WRK-NAI-C02-BYOMEI-TBL
               INITIALIZE                      SPA-GMN-C02-BYOMEICD-G
                                               SPA-NAI-C02-BYOMEI-TBL
               PERFORM VARYING IDW1    FROM    IDWW    BY  -1
                       UNTIL   IDW1    <       1
                   IF      IDW1                =   IDZ
                       CONTINUE
                   ELSE
                     IF      IDW1                <   IDZ
                           MOVE    WRK-GMN-C02-BYOMEICD   (IDW1)
                                       TO  SPA-GMN-C02-BYOMEICD   (IDW1)
                           MOVE    WRK-NAI-C02-BYOMEI-OCC (IDW1)
                                       TO  SPA-NAI-C02-BYOMEI-OCC (IDW1)
                     ELSE
                       PERFORM VARYING IDW2    FROM    6   BY  -1
                               UNTIL   IDW2    <       IDZ
                           IF      IDW2                =   IDZ
                               CONTINUE
                           ELSE
                               IF      WRK-GMN-C02-BYOMEICD (IDW2)
                                                   NOT =   SPACE
                                   MOVE    WRK-GMN-C02-BYOMEICD  (IDW2)
                                       TO  SPA-GMN-C02-BYOMEICD   (IDW1)
                                   MOVE    WRK-NAI-C02-BYOMEI-OCC(IDW2)
                                       TO  SPA-NAI-C02-BYOMEI-OCC (IDW1)
      *
                                   INITIALIZE  WRK-GMN-C02-BYOMEICD
                                                                  (IDW2)
                                               WRK-NAI-C02-BYOMEI-OCC
                                                                  (IDW2)
                                   MOVE    1               TO  IDW2
                               END-IF
                           END-IF
                       END-PERFORM
                     END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    病名入力セットから自院病名編集処理
           PERFORM 410261-BYOMEIINPUTSET-HEN-SEC
      *
           .
       41026-BYOMEIINPUTSET-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名入力セットから自院病名編集処理
      *****************************************************************
       410261-BYOMEIINPUTSET-HEN-SEC       SECTION.
      *
           PERFORM VARYING IDWW    FROM    1   BY  1
                   UNTIL   IDWW    >       6
               IF      SPA-C02-BYOSET-BYOMEIINPUTCD (IDWW)
                                   NOT =   SPACE
                   COMPUTE IDZ =   SPA-C02-BYOSET-INDEX-ST
                               +   IDWW
                               -    1
      *
                   COMPUTE SPA-GMN-C02-CUR     =   IDZ +   1
      *
                   MOVE    SPACE           TO  WRK-HAISIYMDH
                                               SPA-ERRCD
      *
                   INITIALIZE              SPA-NAI-C02-BYOMEI-OCC (IDZ)
                   MOVE    SPA-C02-BYOSET-BYOMEIINPUTCD (IDWW)
                                       TO  SPA-GMN-C02-BYOMEICD (IDZ)
                                           SPA-NAI-C02-BYOMEICD (IDZ)
      *
                   MOVE    IDZ         TO  SPA-C02-BYOSET-INDEX-ED
      *
      *            自院病名検索
                   INITIALIZE                      USERBYOMEI-REC
                   MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
                   MOVE    SPA-NAI-C02-BYOMEICD (IDZ)
                                               TO  USBYO-BYOMEIINPUTCD
                   PERFORM 410112-USERBYOMEI-HENSYU-SEC
                   IF      FLG-USERBYOMEI  =   ZERO
                       PERFORM 41025-USERBYOMEICD-CHK-SEC
                   END-IF
      *
                   MOVE    SPACE       TO  SPA-C02-BYOSET-BYOMEIINPUTCD
                                                             (IDWW)
      *************IF      FLG-USERBYOMEI-CHK  =   ZERO
      *************    GO  TO  410261-BYOMEIINPUTSET-HEN-EXT
      *************END-IF
      *
      *            廃止病名のとき
                   IF    ( WRK-HAISIYMDH   NOT =   SPACE )
                   AND   ( SPA-C02-1042-MSG3-DSPKBN
                                               =   "0"   )
      *                別のコードでの組み立てを検索
                       PERFORM 500-BYOMEI-KENSAKU-SEC
                   END-IF
                   IF      SPA-CIDCD6      NOT =   SPACE
                       GO  TO  410261-BYOMEIINPUTSET-HEN-EXT
                   END-IF
      *
                   PERFORM 41021-BYOMEICD-HENSYU-SEC
      *
                   COMPUTE SPA-GMN-C02-CUR     =   IDZ +   1
      *
                   IF      SPA-ERRCD           =   "0075"  OR "0094"
                       GO  TO  410261-BYOMEIINPUTSET-HEN-EXT
                   END-IF
               END-IF
           END-PERFORM
      *
           INITIALIZE                  SPA-C02-BYOMEIINPUTSET-AREA
      *
           .
       410261-BYOMEIINPUTSET-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット病名検索（コードより検索）
      *****************************************************************
       41028-BYOMEICDINPUTSET-KEN-SEC      SECTION.
      *
           INITIALIZE                      BYOMEIINPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  BYOSET-HOSPNUM
           STRING  SPA-NAI-C02-BYOMEICD(IDZ)
                                           DELIMITED   BY  SPACE
                   "%"                     DELIMITED   BY  SIZE
                                           INTO    BYOSET-BYOMEIINPUTSET
           END-STRING
           MOVE    BYOMEIINPUTSET-REC  TO  MCPDATA-REC
           MOVE    "tbl_byomeiinputset"
                                       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_byomeiinputset"
                                           TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-BYOMEIINPUTSET-READ-N-SEC
           ELSE
               INITIALIZE                  BYOMEIINPUTSET-REC
               MOVE    1               TO  FLG-BYOMEIINPUTSET
           END-IF
      *
           MOVE    "tbl_byomeiinputset"
                                       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-BYOMEIINPUTSET  =   1
              GO  TO  41028-BYOMEICDINPUTSET-KEN-EXT
           END-IF
      *
      *    検索画面遷移処理
           PERFORM 410291-C04-SYORI-SEC
      *
           .
       41028-BYOMEICDINPUTSET-KEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名検索画面編集処理（コードより検索）
      *****************************************************************
       41029-USERBYOMEICD-KENSAKU-SEC      SECTION.
      *
      *    検索対象の病名があるか
           INITIALIZE                      USERBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
           EVALUATE    MCP-WIDGET
               WHEN    "BYOMEICD_1"
               WHEN    "BYOMEICD_2"
               WHEN    "BYOMEICD_3"
               WHEN    "BYOMEICD_4"
               WHEN    "BYOMEICD_5"
               WHEN    "BYOMEICD_6"
                   STRING  SPA-NAI-C02-BYOMEICD(IDZ)
                                               DELIMITED   BY  SPACE
                           "%"                 DELIMITED   BY  SIZE
                                       INTO    USBYO-BYOMEIINPUTCD
                   END-STRING
               WHEN    "HOSOKUCD_1"
               WHEN    "HOSOKUCD_2"
               WHEN    "HOSOKUCD_3"
                   STRING  SPA-GMN-C02-HOSOKUCD(IDZ)
                                               DELIMITED   BY  SPACE
                           "%"                 DELIMITED   BY  SIZE
                                       INTO    USBYO-BYOMEIINPUTCD
                   END-STRING
           END-EVALUATE
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "key11"             TO  MCP-PATHNAME
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key11"             TO  MCP-PATHNAME
               MOVE    "tbl_userbyomei"    TO  MCP-TABLE
               PERFORM 900-USBYO-READ-SEC
           ELSE
               INITIALIZE                  USERBYOMEI-REC
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
      *
           MOVE    "key11"             TO  MCP-PATHNAME
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-USERBYOMEI      =   1
               MOVE    "0053"              TO  SPA-ERRCD
               GO  TO  41029-USERBYOMEICD-KENSAKU-EXT
           END-IF
      *
      *    簡単修正処理中のとき
           IF    ( SPA-GMN-C02-BANGO-X
                                   NOT =   SPACE )
               MOVE    "0090"              TO  SPA-ERRCD
               GO  TO  41029-USERBYOMEICD-KENSAKU-EXT
           END-IF
      *
           EVALUATE    MCP-WIDGET
               WHEN    "BYOMEICD_1"
               WHEN    "BYOMEICD_2"
               WHEN    "BYOMEICD_3"
               WHEN    "BYOMEICD_4"
               WHEN    "BYOMEICD_5"
               WHEN    "BYOMEICD_6"
      *            変更処理のとき自院コードの入力がない場合
                   IF    ( SPA-GMN-C02-BANGO   
                                           NOT =   ZERO ) 
                   AND   ( SPA-GMN-C02-BYOMEICD-G
                                               =   SPACE )
                   AND   ( SPA-NAI-C02-BYOMEICD (1)
                                           NOT =   SPACE )       
                       MOVE    "0069"              TO  SPA-ERRCD
                       GO  TO  41029-USERBYOMEICD-KENSAKU-EXT 
                   END-IF
      *
      *            病名コードが正常に入力されているとき
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       6
                       IF    ( SPA-GMN-C02-BYOMEICD (IDX)
                                                   NOT =   SPACE )             
                       AND   ( SPA-NAI-C02-USERBYOMEIFLG (IDX)
                                                       =   SPACE )
                       AND   ( SPA-NAI-C02-BYOMEI(IDX)(1:2)
                                                   NOT =   "■"  )
                           MOVE    "0069"              TO  SPA-ERRCD
                           GO  TO  41029-USERBYOMEICD-KENSAKU-EXT
                       END-IF 
                   END-PERFORM
      *
      *            文字チェック     
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       6
                       IF      SPA-GMN-C02-BYOMEICD (IDX)
                                                   NOT =   SPACE             
                           IF    ( SPA-NAI-C02-BYOMEI(IDX)(1:2)
                                                           =   "■" )
                           AND   ( IDX                 NOT =   IDZ  )
                               MOVE    "0035"              TO  SPA-ERRCD
                               GO  TO  41029-USERBYOMEICD-KENSAKU-EXT
                          END-IF
                       END-IF
                   END-PERFORM
               WHEN    "HOSOKUCD_1"
               WHEN    "HOSOKUCD_2"
               WHEN    "HOSOKUCD_3"
      *            病名コードが正常に入力されているとき
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       3
                       IF    ( SPA-GMN-C02-HOSOKUCD (IDX)
                                                   NOT =   SPACE )             
                       AND   ( SPA-NAI-C02-HOSOKU-USERBYOFLG (IDX)
                                                       =   SPACE )
                       AND   ( SPA-NAI-C02-HOSOKU-COMMENT(IDX)(1:2)
                                                   NOT =   "■"  )
                           MOVE    "0069"              TO  SPA-ERRCD
                           GO  TO  41029-USERBYOMEICD-KENSAKU-EXT
                       END-IF 
                   END-PERFORM
      *            文字チェック     
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       3
                       IF      SPA-GMN-C02-HOSOKUCD (IDX)
                                                   NOT =   SPACE             
                           IF    ( SPA-NAI-C02-HOSOKU-COMMENT(IDX)(1:2)
                                                           =   "■" )
                           AND   ( IDX                 NOT =   IDZ  )
                               MOVE    "0035"              TO  SPA-ERRCD
                               GO  TO  41029-USERBYOMEICD-KENSAKU-EXT
                          END-IF
                       END-IF
                   END-PERFORM
           END-EVALUATE
      *
      *    検索画面遷移処理
           PERFORM 410291-C04-SYORI-SEC
      *
           .
       41029-USERBYOMEICD-KENSAKU-EXT.
           EXIT.
      *     
      *****************************************************************
      *    検索画面遷移処理（コード検索時）
      *****************************************************************
       410291-C04-SYORI-SEC            SECTION.
      *        
           INITIALIZE                      SPA-C01KYOUTU
           MOVE    "C02"               TO  SPA-C01-MOTOPG
      *
      *    病名欄セット
           MOVE    ZERO                TO  IDY
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
               IF      SPA-GMN-C02-BYOMEICD (IDX)  NOT =   SPACE
                   ADD     1               TO  IDY
                   IF    (  IDX             =   IDZ       )
                   AND   (  MCP-WIDGET (1:8)
                                           =   "BYOMEICD" )
                       MOVE    SPA-NAI-C02-BYOMEICD(IDZ)
                                               TO  SPA-C01-BYOMEIINPUTCD
                       MOVE    IDY             TO  SPA-C01-INDEX
                       MOVE    "BYOMEICD"      TO  SPA-C01-WIDGET
                   ELSE 
                       MOVE    SPA-GMN-C02-BYOMEICD  (IDX)
                                           TO  SPA-C01-TBYOMEICD  (IDY)
                       MOVE    SPA-NAI-C02-BYOMEITOP (IDX)
                                           TO  SPA-C01-TBYOMEITOP (IDY)
                       MOVE    SPA-NAI-C02-BYOMEISBT (IDX)
                                           TO  SPA-C01-TBYOMEISBT (IDY)
                       MOVE    SPA-NAI-C02-USERBYOMEIFLG (IDX)
                                           TO  SPA-C01-TUSERBYOMEIFLG
                                                                  (IDY)
                       MOVE    SPA-NAI-C02-BYOMEI    (IDX)
                                           TO  SPA-C01-TBYOMEI    (IDY)
                       MOVE    IDX         TO  SPA-C01-TBYOMEICD-INDEX
                                                                  (IDY)
                   END-IF
               END-IF
           END-PERFORM
      *    補足コメント欄セット
           MOVE    ZERO                TO  IDY
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       3
               IF      SPA-GMN-C02-HOSOKUCD (IDX)  NOT =   SPACE
                   ADD     1               TO  IDY              
                   IF    (  IDX            =   IDZ       )
                   AND   (  MCP-WIDGET (1:8)
                                           =   "HOSOKUCD" )
                       MOVE    SPA-GMN-C02-HOSOKUCD(IDZ)
                                               TO  SPA-C01-BYOMEIINPUTCD
                       MOVE    IDZ             TO  SPA-C01-INDEX
                       MOVE    "HOSOKUCD"      TO  SPA-C01-WIDGET
                   ELSE 
                       MOVE    SPA-GMN-C02-HOSOKUCD        (IDX)
                                           TO  SPA-C01-THOSOKUCD  (IDY)
                       MOVE    SPA-NAI-C02-HOSOKU-COMMENT  (IDX)
                                           TO  SPA-C01-THOSOKU-COMMENT
                                                                  (IDY)
                       MOVE    IDX         TO  SPA-C01-THOSOKUCD-INDEX
                                                                  (IDY)
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-C01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-C02FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGC04"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-C01KYOUTU
           MOVE    SPA-FREE        TO  SPA-C02FREE
      *
           MOVE    "C02"                TO  SPA-MOTOPG
           MOVE    "C04"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "C04"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
           .
       410291-C04-SYORI-EXT.
           EXIT.
      *     
      *****************************************************************
      *    疑い入力処理
      *****************************************************************
       4104-UTAGAI-CHK-SEC                SECTION.
      *
           MOVE    SPA-GMN-C02-UTAGAIFLG   TO  WRK-KBN
           PERFORM 5001-UTAGAIFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI              TO  SPA-GMN-C02-UTAGAIFLGMEI
           IF      SPA-GMN-C02-UTAGAIFLG =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C02-UTAGAIFLGMEI   =   SPACE    
                  MOVE    "0018"               TO  SPA-ERRCD
                   MOVE    18                  TO  SPA-GMN-C02-CUR
                  GO  TO  4104-UTAGAI-CHK-EXT
               END-IF
           END-IF    
      *
      *    レセ電エラーフラグ
      *****PERFORM 41041-RECEDENFLG-CHK-SEC
           .
       4104-UTAGAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電エラーフラグチェック処理
      *****************************************************************
       41041-RECEDENFLG-CHK-SEC        SECTION.
      *   
           INITIALIZE                      WRK-PTBYOMEI-CHK-AREA
           IF      SPA-NAI-C02-BYOMEIHENFLG    =   SPACE
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       WRK-CONS-BYOMEICD-MAX
                       OR      SPA-NAI-C02-BYOMEICD (IDXX) =   SPACE
                   IF    ( WRK-HAISIYMD-CHK    =   ZERO                )
                   AND   ( SPA-NAI-C02-HAISIYMD (IDXX)
                                           NOT =   ZERO    AND ALL "9" )
                       MOVE    SPA-NAI-C02-HAISIYMD (IDXX)
                                               TO  WRK-HAISIYMD-CHK
                   END-IF
                   PERFORM VARYING IDXY    FROM    1   BY  1
                           UNTIL   IDXY    >       WRK-CONS-BYOMEICD-MAX
                           OR      SPA-NAI-C02-BYOMEICD-O (IDXX IDXY)
                                               =   SPACE
                       ADD     1           TO  WRK-PTBYO-CHK-BYOMEICDSU
                       IF      WRK-PTBYO-CHK-BYOMEICDSU <   22
                           MOVE    SPA-NAI-C02-BYOMEICD-O (IDXX IDXY)
                                           TO  WRK-PTBYO-CHK-BYOMEICD 
                                              (WRK-PTBYO-CHK-BYOMEICDSU)
                       ELSE
                           MOVE    "1"     TO  SPA-NAI-C02-BYOMEIHENFLG
                           MOVE    "病名編集"
                                           TO  SPA-GMN-C02-BYOMEIHENFLG
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
           MOVE    SPA-NAI-C02-BYOMEIHENFLG
                                       TO  WRK-PTBYO-CHK-BYOMEIHENFLG
           MOVE    SPA-NAI-C02-BYOMEIMOJI
                                       TO  WRK-PTBYO-CHK-BYOMEIMOJI
           MOVE    SPA-GMN-C02-UTAGAIFLG
                                       TO  WRK-PTBYO-CHK-UTAGAIFLG 
           MOVE    SPA-GMN-C02-SYUBYOFLG
                                       TO  WRK-PTBYO-CHK-SYUBYOFLG 
           MOVE    SPA-GMN-C02-BYOMEI  TO  WRK-PTBYO-CHK-BYOMEI
           MOVE    SPA-NAI-C02-SRYYMD  TO  WRK-PTBYO-CHK-SRYYMD 
           PERFORM 5001-RECEDENFLG-HENSYU-SEC
           MOVE    WRK-PTBYO-CHK-RECEDENFLG
                                       TO  SPA-NAI-C02-RECEDENFLG
           .
       41041-RECEDENFLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主病　入力処理
      *****************************************************************
       4105-SYUBYOFLG-CHK-SEC                SECTION.
      *
           MOVE    SPA-GMN-C02-SYUBYOFLG
                                        TO  WRK-KBN
           PERFORM 5001-SYUBYOFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI              TO  SPA-GMN-C02-SYUBYOFLGMEI
           IF      SPA-GMN-C02-SYUBYOFLG  =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C02-SYUBYOFLGMEI     =   SPACE
                   MOVE    "0019"               TO  SPA-ERRCD
                   MOVE    19                   TO  SPA-GMN-C02-CUR
                   GO  TO  4105-SYUBYOFLG-CHK-EXT
               END-IF
           END-IF        
      *
      *    レセ電エラーフラグ
      *****PERFORM 41041-RECEDENFLG-CHK-SEC
           .
       4105-SYUBYOFLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    慢性　入力処理
      *****************************************************************
       4106-MANSEI-CHK-SEC                SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-C02-HYOUJI
      *
           MOVE    SPA-GMN-C02-MANSEIKBN
                                       TO  WRK-KBN
           PERFORM 5001-MANSEIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-MANSEIKBNMEI
      *                                 
           IF      SPA-GMN-C02-MANSEIKBN =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C02-MANSEIKBNMEI   =   SPACE
                  MOVE    "0021"               TO  SPA-ERRCD
                  MOVE    20                   TO  SPA-GMN-C02-CUR
                  GO  TO  4106-MANSEI-CHK-EXT
               END-IF
           END-IF
      *    
           PERFORM 5001-HYOUJI-HENSYU-SEC
           .
       4106-MANSEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分  入力処理
      *****************************************************************
       4107-NYUGAIKBN-CHK-SEC                SECTION.
      *
           MOVE    SPA-GMN-C02-NYUGAIKBN
                                       TO  WRK-KBN
           PERFORM 5001-NYUGAIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-NYUGAIKBNMEI
      *                                 
           IF      SPA-GMN-C02-NYUGAIKBN =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C02-NYUGAIKBNMEI   =   SPACE    
                   MOVE    "0022"               TO  SPA-ERRCD
                   MOVE    21                   TO  SPA-GMN-C02-CUR
                   GO  TO  4107-NYUGAIKBN-CHK-EXT
               END-IF
           END-IF
           .
       4107-NYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ入力処理
      *****************************************************************
       4108-COMB-CHK-SEC                SECTION.
      *
           IF      SPA-GMN-C02-HKNCOMBI
                                       =   SPACE
               MOVE    SPACE               TO  SPA-GMN-C02-HKNCOMBIMEI
           ELSE
               MOVE    SPA-GMN-C02-HKNCOMBI
                                           TO  WRK-KBN
               PERFORM 5001-HKNCOMBI-HENSYU-SEC
               IF      WRK-KBNMEI      =   SPACE
                   MOVE    "0009"               TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KBNMEI  TO  SPA-GMN-C02-HKNCOMBIMEI
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    22                  TO  SPA-GMN-C02-CUR
           END-IF
      *
           .
       4108-COMB-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト表示入力処理
      *****************************************************************
       4109-REZEFLG-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-C02-REZEFLG TO  WRK-KBN
           PERFORM 5001-REZEFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-REZEFLGMEI
      *                                 
           IF      SPA-GMN-C02-REZEFLG =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C02-REZEFLGMEI   =   SPACE    
                   MOVE    "0023"               TO  SPA-ERRCD
                   MOVE    23                 TO  SPA-GMN-C02-CUR
                   GO  TO  4109-REZEFLG-CHK-EXT
               END-IF
           END-IF
           .
       4109-REZEFLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト月  入力処理
      *****************************************************************
       41010-REZEMM-CHK-SEC            SECTION.
      *
            .
       41010-REZEMM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    転帰区分  入力処理
      *****************************************************************
       41011-TENKIKBN-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-C02-TENKIKBN
                                       TO  WRK-KBN
           PERFORM 5001-TENKIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-TENKIKBNMEI
      *                                 
           IF      SPA-GMN-C02-TENKIKBN
                                       =   SPACE
               MOVE    SPACE               TO  SPA-NAI-C02-TENKIYMD
               MOVE    SPACE               TO  SPA-GMN-C02-TENKIYMD
           ELSE
               IF      SPA-GMN-C02-TENKIKBNMEI
                                           =   SPACE
                   MOVE    "0024"              TO  SPA-ERRCD
                   MOVE    17                  TO  SPA-GMN-C02-CUR
                   GO  TO  41011-TENKIKBN-CHK-EXT
               END-IF
           END-IF
      *     
           IF    ( SPA-GMN-C02-TENKIKBN NOT =   SPACE )
           AND   ( SPA-GMN-C02-TENKIYMD     =   SPACE )
               MOVE    SPA-NAI-C02-SYSYMD   TO  SPA-NAI-C02-TENKIYMD
               MOVE    SPA-GMN-C02-SYSYMD   TO  SPA-GMN-C02-TENKIYMD
           END-IF
      *
           IF      WRK-MCP-WIDGET      =   "TENKIKBN"
               IF      SPA-NAI-C02-BYOMEICD-MAX >   1
                   MOVE    2               TO  SPA-GMN-C02-CUR
               ELSE
                   IF      SPA-GMN-C02-TENKIKBN =   SPACE
                       MOVE    18              TO  SPA-GMN-C02-CUR
                   ELSE
                       MOVE    13              TO  SPA-GMN-C02-CUR
                   END-IF
               END-IF
           END-IF
           .
       41011-TENKIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    転帰年月日  入力処理
      *****************************************************************
       41012-TENKIYMD-CHK-SEC            SECTION.
      *
           MOVE    SPACE              TO  SPA-NAI-C02-TENKIYMD
           IF      SPA-GMN-C02-TENKIYMD
                                      =   SPACE
               IF      SPA-GMN-C02-TENKIKBN   NOT =   SPACE
                   MOVE    "0036"          TO  SPA-ERRCD
               END-IF    
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-C02-TENKIYMD
                                           TO  SGDAY-INDAY
               MOVE    SPA-NAI-C02-SYSYMD  TO  SGDAY-SYSYMD
               MOVE    "1"                 TO  SGDAY-SYSKBN
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN-C02-TENKIYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-C02-TENKIYMD
      *
                   IF      SPA-GMN-C02-TENKIKBN    =   SPACE
                       MOVE    "0037"          TO  SPA-ERRCD
                   END-IF    
               ELSE
                   MOVE    "0016"          TO  SPA-ERRCD
               END-IF
           END-IF    
      *         
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    13                 TO  SPA-GMN-C02-CUR
           END-IF     
           .
       41012-TENKIYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名  入力処理
      *****************************************************************
       41013-BYOMEI-CHK-SEC             SECTION.
      *
           MOVE    8                    TO  SPA-GMN-C02-CUR
      * 
      *    複数病名入力のとき
           IF      SPA-NAI-C02-BYOMEICD-MAX    >   1
               MOVE    "0060"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-C02-CUR 
               GO  TO  41013-BYOMEI-CHK-EXT
           END-IF
      *
      *****未コード化傷病名コードのとき病名がおなじとき    
      *****IF      C02-BYOMEI          =   SPA-GMN-C02-BYOMEI
      *****AND     SPA-NAI-C02-BYOMEICD (1)
      *****                            =   WRK-CONS-BYOMEICD
      *****    MOVE    9                   TO  SPA-GMN-C02-CUR 
      *****    GO  TO  41013-BYOMEI-CHK-EXT
      *****END-IF
      *
      *    病名コード入力済でエラーのときコードクリア
           IF    ( SPA-GMN-C02-BYOMEICD-G
                                   NOT =   SPACE )
           AND   ( SPA-NAI-C02-BYOMEIMOJI
                                       =   ZERO  )
                INITIALIZE                   SPA-GMN-C02-BYOMEICD-G
                                             SPA-NAI-C02-BYOMEI-TBL
                MOVE    ZERO             TO  SPA-NAI-C02-BYOMEICD-MAX
           END-IF
      *
      *    未コード化傷病名コード入力のときコードクリア
           IF      SPA-GMN-C02-BYOMEICD (1)
                                       =   WRK-CONS-BYOMEICD
                INITIALIZE                   SPA-GMN-C02-BYOMEICD-G
                                             SPA-NAI-C02-BYOMEI-TBL
                MOVE    ZERO             TO  SPA-NAI-C02-BYOMEICD-MAX
           END-IF
      *
           MOVE    SPA-GMN-C02-BYOMEI   TO  WRK-BYOMEI-OLD
      *
           MOVE    C02-BYOMEI           TO  SPA-GMN-C02-BYOMEI
      *     
           MOVE    ZERO                 TO  SPA-NAI-C02-BYOMEIMOJI
      *
           IF      SPA-GMN-C02-BYOMEI   =   SPACE
                INITIALIZE                   SPA-GMN-C02-BYOMEICD-G
                                             SPA-NAI-C02-BYOMEI-TBL
                MOVE    SPACE            TO  SPA-NAI-C02-BYOMEIHENFLG
                                             SPA-GMN-C02-BYOMEIHENFLG
                MOVE    ZERO             TO  SPA-NAI-C02-BYOMEIMOJI
                MOVE    ZERO             TO  SPA-NAI-C02-BYOMEICD-MAX
                INITIALIZE                   SPA-GMN-C02-UTAGAIFLG-G
                                             SPA-GMN-C02-MANSEIKBN-G
                MOVE    34               TO  SPA-GMN-C02-CUR
                GO  TO  41013-BYOMEI-CHK-EXT
           END-IF
      *
      *    半角・文字数チェック
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI 
           MOVE    SPA-GMN-C02-BYOMEI  TO  KANACHK-MAE-INPUT
           MOVE    80                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
           AND     KANACHK-SYUBETU     =   2
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-C02-BYOMEI
      *        病名文字数
               IF      KANACHK-MAX     >   80
                   MOVE    "0007"              TO  SPA-ERRCD
                   GO  TO  41013-BYOMEI-CHK-EXT
               END-IF
           ELSE
               MOVE    "0071"              TO  SPA-ERRCD
               GO  TO  41013-BYOMEI-CHK-EXT
           END-IF
      *
      *    文字チェック     
           PERFORM VARYING     IDX     FROM    1   BY  2
                   UNTIL       IDX     >       80
                   OR  SPA-GMN-C02-BYOMEI(IDX:2)   =   SPACE
               IF      SPA-GMN-C02-BYOMEI(IDX:2)   =   "■"
                   MOVE    "0035"          TO  SPA-ERRCD
                   GO  TO  41013-BYOMEI-CHK-EXT
              END-IF
           END-PERFORM
      *
      *    病名からコード検索処理
           INITIALIZE           ORCSBYOMEIAREA
           MOVE    "1"             TO  LNK-BYO-SYORI
           MOVE    SPA-GMN-C02-BYOMEI
                                   TO  LNK-BYO-BYOMEI
           IF      SPA-GMN-C02-SYUBYOFLG
                                   =   "1"
               MOVE    SPA-GMN-C02-SYUBYOFLG
                                       TO  LNK-BYO-SYUBYOFLG 
           END-IF
           CALL    "ORCSBYOMEI"     USING    ORCSBYOMEIAREA
grpsys                                       SPA-AREA 
      * 
           INITIALIZE               SPA-NAI-C02-BYOMEI-TBL
      *
           MOVE    ZERO             TO  WRK-LNK-BYO-INDEX
           IF  (     ( SPA-C02-1042-MSG2-DSPKBN
                                       =   "1" )
                 AND ( SPA-C02-1042-MSG3-DSPKBN
                                       =   "1" ) )
           OR  (       LNK-BYOMEI-KENSAKU-MAX
                                       =   1     )
      *        移行・推奨病名、廃止病名の警告表示をしないとき
      *        または、検索結果がひとつのとき
               CONTINUE
           ELSE
      *        編集病名でなく移行等があるとき
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       LNK-BYOMEI-KENSAKU-MAX
                   IF      LNK-BYO-O-BYOMEIHENFLG(IDXX) =   SPACE
                     IF    ( LNK-BYO-O-IKOSAKICD   (IDXX)
                                             NOT =   SPACE           )
                     AND   ( LNK-BYO-O-IKOSAKICD   (IDXX)
                                                 =
                                                 LNK-BYO-KHNBYOMEICD )
                     AND   ( LNK-BYO-BYOMEICDSU  =  1                )
      *                移行先病名が元の病名と同じ時は対象としない
                       CONTINUE
                     ELSE  
                       IF      LNK-BYO-O-IKOSAKICD   (IDXX)
                                               NOT =   SPACE
                           IF      SPA-C02-1042-MSG2-DSPKBN
                                                   =   "0"
                               MOVE    IDXX        TO  WRK-LNK-BYO-INDEX
                               MOVE    LNK-BYOMEI-KENSAKU-MAX
                                                   TO  IDXX
                           END-IF
                       END-IF
                       IF      LNK-BYO-O-HAISIYMD-ERR(IDXX)
                                               NOT =   ZERO
      **************       IF      SPA-C02-1042-MSG3-DSPKBN
      **************                               =   "0"
      **************           MOVE    IDXX        TO  WRK-LNK-BYO-INDEX
      **************           MOVE    LNK-BYOMEI-KENSAKU-MAX
      **************                               TO  IDXX
      **************       END-IF
                           IF      SPA-C02-1042-MSG2-DSPKBN
                                                   =   "0"
                               INITIALIZE          SYNONYM-REC
                               MOVE    LNK-BYO-BYOMEI
                                                   TO  SYNONYM-BYOMEI
      *                        同義語マスタから病名検索処理
                               PERFORM 5001-SYNONYM-CHK-SEC
                               IF      FLG-SYNONYM =   ZERO
                                   MOVE    IDXX    TO  WRK-LNK-BYO-INDEX
                                   MOVE    LNK-BYOMEI-KENSAKU-MAX
                                                   TO  IDXX
                               END-IF
                           END-IF
                       END-IF     
                     END-IF     
                   END-IF
               END-PERFORM
      *
               IF          WRK-LNK-BYO-INDEX   >   ZERO              
      *            編集病名でなく移行等もないとき置換病名に設定
                   IF    ( LNK-BYO-BYOMEIHENFLG    =   SPACE )
                   AND   ( LNK-BYO-IKOSAKICD       =   SPACE )
                   AND   ( LNK-BYO-HAISIYMD-ERR    =   ZERO  )
                       MOVE    1                   TO
                                               SPA-C02-CHANGE-INDEX-ST
                       MOVE    6                   TO
                                               SPA-C02-CHANGE-INDEX-ED
                       COMPUTE SPA-C02-CHANGE-INDEX-MAX
                               =   SPA-C02-CHANGE-INDEX-ED -
                                   SPA-C02-CHANGE-INDEX-ST +   1
                       MOVE    LNK-BYO-BYOMEI-MAX  TO
                                                   SPA-C02-CHANGE-MAX
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >       LNK-BYO-BYOMEI-MAX
                           MOVE    LNK-BYO-TBYOMEICD (IDY) TO
                                           SPA-C02-CHANGE-BYOMEICD(IDY) 
                           MOVE    LNK-BYO-TBYOMEI   (IDY) TO
                                           SPA-C02-CHANGE-BYOMEI  (IDY) 
                       END-PERFORM
                       MOVE    "0107"          TO  SPA-CIDCD6
      *
      *                編集病名でなく移行等があるコードと置き換える
                       INITIALIZE              LNK-BYOMEI-AREA
                       MOVE    LNK-BYOMEI-AREA-OCC (WRK-LNK-BYO-INDEX)
                                               TO  LNK-BYOMEI-AREA
                   END-IF
               END-IF
           END-IF
      *
           MOVE    ZERO         TO  IDZ
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       LNK-BYO-BYOMEI-MAX
               ADD     1           TO  IDZ
               MOVE    LNK-BYO-TBYOMEICD (IDX)
                                   TO  SPA-NAI-C02-BYOMEICD  (IDZ)
                                       SPA-NAI-C02-BYOMEICD-O(IDZ 1)
               IF      IDZ             =   1
                   MOVE    "1"         TO  SPA-NAI-C02-BYOMEITOP(IDZ)
               END-IF
               MOVE    LNK-BYO-TBYOMEI (IDX)
                                       TO  SPA-NAI-C02-BYOMEI   (IDZ)
                                           SPA-NAI-C02-BYOMEI-O (IDZ 1)
               MOVE    LNK-BYO-TUTAGAIFLG (IDX)
                                       TO  SPA-NAI-C02-UTAGAIFLG(IDZ)
               MOVE    LNK-BYO-TTOKSKNCD (IDX)
                                       TO  SPA-NAI-C02-TOKSKNCD (IDZ)
               MOVE    LNK-BYO-TNANBYOCD  (IDX)
                                       TO  SPA-NAI-C02-NANBYOCD-O(IDZ)
               MOVE    LNK-BYO-THAISIYMD (IDX)
                                       TO  SPA-NAI-C02-HAISIYMD (IDZ)
                                           SPA-NAI-C02-HAISIYMD-O
                                                                (IDZ 1)
               MOVE    LNK-BYO-TTANDOKUKBN (IDX)
                                       TO
                                       SPA-NAI-C02-TANDOKUKBN-O (IDZ 1)
               IF      SPA-NAI-C02-TANDOKUKBN-O (IDZ 1)
                                   NOT =   ZERO
                   MOVE    LNK-BYO-TBYOMEI (IDX)
                                               TO  WRK-TANDOKUKBN-BYOMEI
               END-IF  
           END-PERFORM
      *
           MOVE    LNK-BYO-BYOMEIMOJI  TO  SPA-NAI-C02-BYOMEIMOJI
           MOVE    LNK-BYO-UTAGAIFLG   TO  SPA-GMN-C02-UTAGAIFLG
                                           WRK-KBN
           PERFORM 5001-UTAGAIFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI  TO  SPA-GMN-C02-UTAGAIFLGMEI
           IF      LNK-BYO-NANBYOCD
                                   NOT =   ZERO
               MOVE    LNK-BYO-NANBYOCD    TO  SPA-GMN-C02-MANSEIKBN
                                               WRK-KBN
               PERFORM 5001-MANSEIKBN-HENSYU-SEC                                
               MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-MANSEIKBNMEI
           ELSE
               IF      LNK-BYO-MANSEIKBN
                                       NOT =   ZERO
                   MOVE    LNK-BYO-MANSEIKBN   TO  SPA-GMN-C02-MANSEIKBN
                                                   WRK-KBN
                   PERFORM 5001-MANSEIKBN-HENSYU-SEC                                
                   MOVE    WRK-KBNMEI          TO
                                               SPA-GMN-C02-MANSEIKBNMEI
               ELSE
                   MOVE    SPACE               TO
                                               SPA-GMN-C02-MANSEIKBN-G
               END-IF
           END-IF
           MOVE    LNK-BYO-MANSEIKBN   TO  SPA-NAI-C02-MANSEIKBN
           MOVE    LNK-BYO-NANBYOCD    TO  SPA-NAI-C02-NANBYOCD
      *    
           PERFORM 5001-HYOUJI-HENSYU-SEC
      *
           MOVE    LNK-BYO-RECEDENFLG  TO  SPA-NAI-C02-RECEDENFLG
           MOVE    LNK-BYO-BYOMEIHENFLG
                                       TO  SPA-NAI-C02-BYOMEIHENFLG
           IF      SPA-NAI-C02-BYOMEIHENFLG  =   "1" 
               MOVE    "病名編集"  TO  SPA-GMN-C02-BYOMEIHENFLG
           ELSE
               MOVE    SPACE       TO  SPA-GMN-C02-BYOMEIHENFLG
           END-IF    
           MOVE    1           TO  SPA-NAI-C02-BYOMEICD-MAX
      *
           IF    ( LNK-BYO-IKOSAKICD    =   SPACE )
           AND   ( LNK-BYO-HAISIYMD-ERR =   ZERO  )
               IF      LNK-BYO-TBYOMEICD (1)
                                            =   WRK-CONS-BYOMEICD
                   INITIALIZE              SYNONYM-REC
                   MOVE    SPA-NAI-C02-BYOMEI (1)
                                       TO  SYNONYM-BYOMEI
                   PERFORM 5001-SYNONYM-CHK-SEC
                   IF      FLG-SYNONYM =   ZERO
                       MOVE    SPA-NAI-C02-BYOMEICD (1)
                                           TO  SPA-C02-IKOMOTO-BYOMEICD
                       MOVE    SPA-NAI-C02-BYOMEI   (1)
                                           TO  SPA-C02-IKOMOTO-BYOMEI
      *****            MOVE    SPA-NAI-C02-HAISIYMD (1)
      *****                                TO  SPA-C02-IKOMOTO-HAISIYMD
                       MOVE    SYNONYM-BYOMEICD
                                           TO  SPA-C02-IKOSAKI-BYOMEICD
                       MOVE    BYO-BYOMEI  TO  SPA-C02-IKOSAKI-BYOMEI
                       MOVE    1           TO  SPA-C02-IKOSAKI-INDEX                    
                       MOVE    "0102"      TO  SPA-CIDCD2
                   END-IF
               END-IF
           ELSE    
               IF      LNK-BYO-BYOMEIHENFLG    =   SPACE                          
                   IF      LNK-BYO-HAISIYMD-ERR    NOT =   ZERO
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       LNK-BYO-BYOMEI-MAX
                           IF      LNK-BYO-THAISIYMD (IDX)
                                                       =   ALL "9"
                                                       OR  SPACE  
                               CONTINUE
                           ELSE
                               MOVE    SPA-NAI-C02-BYOMEICD (IDX)
                                           TO  SPA-C02-IKOMOTO-BYOMEICD
                               MOVE    SPA-NAI-C02-BYOMEI   (IDX)
                                           TO  SPA-C02-IKOMOTO-BYOMEI
                               MOVE    SPA-NAI-C02-HAISIYMD (IDX)
                                           TO  SPA-C02-IKOMOTO-HAISIYMD
                               MOVE    IDX TO  SPA-C02-IKOSAKI-INDEX                    
                               INITIALIZE              SYNONYM-REC
                               MOVE    SPA-NAI-C02-BYOMEI (IDX)
                                                   TO  SYNONYM-BYOMEI
                               PERFORM 5001-SYNONYM-CHK-SEC
                               IF      FLG-SYNONYM =   ZERO
                                   MOVE    SYNONYM-BYOMEICD
                                           TO  SPA-C02-IKOSAKI-BYOMEICD
                                   MOVE    BYO-BYOMEI
                                           TO  SPA-C02-IKOSAKI-BYOMEI
                                   MOVE    "0102"
                                           TO  SPA-CIDCD2
                               ELSE 
                                   MOVE    LNK-BYO-HAISIYMD-ERR
                                           TO  WRK-SYMD
                                   PERFORM 5002-HIZUKE-HEN-SEC
                                   MOVE    LNK-DAY2-EDTYMD3
                                           TO  WRK-HAISIYMDH
                                   MOVE    LNK-BYO-TBYOMEI (IDX)
                                           TO  WRK-ERR-BYOMEI
                               END-IF
                           END-IF
                       END-PERFORM
                   END-IF             
                   IF    ( LNK-BYO-IKOSAKICD   NOT =   SPACE )
                   AND   ( SPA-C02-1042-MSG2-DSPKBN
                                                   =   ZERO  )
                       MOVE    LNK-BYO-IKOSAKICD-INDEX
                                           TO  IDX
                       MOVE    SPA-NAI-C02-BYOMEICD (IDX) 
                                           TO  SPA-C02-IKOMOTO-BYOMEICD
                       MOVE    SPA-NAI-C02-BYOMEI   (IDX) 
                                           TO  SPA-C02-IKOMOTO-BYOMEI
                       MOVE    SPA-NAI-C02-HAISIYMD (IDX)
                                           TO  SPA-C02-IKOMOTO-HAISIYMD
                       MOVE    LNK-BYO-IKOSAKICD
                                           TO  SPA-C02-IKOSAKI-BYOMEICD
                       MOVE    LNK-BYO-IKOSAKICD-BYOMEI
                                           TO  SPA-C02-IKOSAKI-BYOMEI
                       MOVE    IDX     TO  SPA-C02-IKOSAKI-INDEX                    
                       MOVE    "0101"  TO  SPA-CIDCD2
                       MOVE    SPACE   TO  WRK-HAISIYMDH
                                           WRK-ERR-BYOMEI
                   END-IF
               END-IF
           END-IF
      *
      *    病名コード最大個数オーバー
           IF      LNK-BYO-BYOMEICD-MAX-ERR =   1
               MOVE    "0077"          TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-CIDCD2
                                           SPA-CIDCD6
                                           WRK-HAISIYMDH
                                           WRK-ERR-BYOMEI
               GO  TO  41013-BYOMEI-CHK-EXT
           END-IF
      *
      *****修飾語コードのみのとき
      *****IF      LNK-BYO-BYOMEICD-ERR    =   1
      *****    MOVE    "0006"          TO  SPA-ERRCD
      *****    MOVE    SPACE           TO  SPA-CIDCD2
      *****                                WRK-HAISIYMDH
      *****    MOVE    ZERO            TO  SPA-NAI-C02-BYOMEIMOJI
      *****    MOVE    SPACE           TO  SPA-NAI-C02-BYOMEIHENFLG
      *****                                SPA-GMN-C02-BYOMEIHENFLG
      *****    GO  TO  41013-BYOMEI-CHK-EXT
      *****END-IF
      *
      *    病名コード順エラー
           IF      LNK-BYO-BYOMEICD-ERR    =   1   OR  2   OR  9
               MOVE    "0103"          TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-CIDCD2
                                           SPA-CIDCD6
                                           WRK-HAISIYMDH
               MOVE    34              TO  SPA-GMN-C02-CUR
               GO  TO  41013-BYOMEI-CHK-EXT
           END-IF
      *
      *    単独使用禁止エラー（補足コメントの入力がないとき）
           IF    ( LNK-BYO-TANDOKUKBN-ERR
                                       =   1     )
           AND   ( SPA-GMN-C02-HOSOKU-COMMENT 
                                       =   SPACE )
               MOVE    "0076"          TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-CIDCD2
                                           SPA-CIDCD6
                                           WRK-HAISIYMDH
               GO  TO  41013-BYOMEI-CHK-EXT
           ELSE
               MOVE    SPACE           TO  WRK-TANDOKUKBN-BYOMEI
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    34              TO  SPA-GMN-C02-CUR
           END-IF
      *
           IF      SPA-CIDCD6          =   "0107"
               IF      SPA-CIDCD2          =   "0101"
                   MOVE    "0111"              TO  SPA-CIDCD6
               ELSE
                   IF      SPA-CIDCD2          =   "0102"
                       MOVE    "0112"              TO  SPA-CIDCD6
                   ELSE
                       MOVE    "0117"              TO  SPA-CIDCD6 
                   END-IF
               END-IF
               MOVE    SPACE              TO  SPA-CIDCD2
           END-IF
      *    廃止病名警告
           IF    ( SPA-CIDCD2          =   SPACE )
           AND   ( SPA-CIDCD6          =   SPACE )
           AND   ( WRK-HAISIYMDH   NOT =   SPACE )
           AND   ( SPA-C02-1042-MSG3-DSPKBN
                                       =   ZERO  )
               MOVE    "0075"          TO  SPA-ERRCD
           END-IF
           .
       41013-BYOMEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険病名フラグ入力処理
      *****************************************************************
       41014-HKNBYOMEIFLG-CHK-SEC                SECTION.
      *
           MOVE    SPA-GMN-C02-HKNBYOMEIFLG
                                       TO  WRK-KBN
           PERFORM 5001-HKNBYOMEIFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-HKNBYOMEIFLGMEI
      *
           IF      SPA-GMN-C02-HKNBYOMEIFLG   =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C02-HKNBYOMEIFLGMEI
                                              =   SPACE
                   MOVE    "0028"               TO  SPA-ERRCD
                   MOVE    25                   TO  SPA-GMN-C02-CUR
                   GO  TO  41014-HKNBYOMEIFLG-CHK-EXT
                   
               END-IF
           END-IF
           .
       41014-HKNBYOMEIFLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    カルテ病名  入力処理
      *****************************************************************
       41018-CHARTBYOMEI-CHK-SEC        SECTION.
      *
           MOVE    ZERO                 TO  SPA-NAI-C02-CHARTBYOMEIMOJI
      *
           IF      SPA-GMN-C02-CHARTBYOMEI  =   SPACE
               GO  TO  41018-CHARTBYOMEI-CHK-EXT
           END-IF
      *
      *    半角チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-C02-CHARTBYOMEI
                                       TO  KANACHK-MAE-INPUT
           MOVE    80                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
           AND     KANACHK-SYUBETU     =   "2"
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-C02-CHARTBYOMEI
      *        病名文字数
               IF      KANACHK-MAX     >   80
                   MOVE    "0033"          TO  SPA-ERRCD
               END-IF 
           ELSE
               MOVE    "0071"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        病名文字数編集
               COMPUTE SPA-NAI-C02-CHARTBYOMEIMOJI
                                       =   KANACHK-MAX  /   2
           ELSE
               MOVE    28                   TO  SPA-GMN-C02-CUR
           END-IF
           .
       41018-CHARTBYOMEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科入力処理
      *****************************************************************
       41019-BSRYKA-CHK-SEC             SECTION.
      *
           MOVE    31                  TO  SPA-GMN-C02-CUR
      *
           IF    ( SPA-NAI-C02-UPDOWN  =   1    )
               IF      C02-BSRYKA      NOT =   SPA-GMN-C02-BSRYKA-G
                   MOVE    "0087"              TO  SPA-ERRCD
               END-IF
               MOVE    2                   TO  SPA-GMN-C02-CUR
               GO  TO  41019-BSRYKA-CHK-EXT
           END-IF 
      *
           IF      C02-BSRYKA      NOT =   SPA-GMN-C02-BSRYKA-G
               MOVE   9                     TO  SPA-NAI-C02-UPDOWN
           END-IF  
      *
           MOVE    C02-BSRYKA          TO  SPA-GMN-C02-BSRYKA-G
           MOVE    SPACE               TO  SPA-GMN-C02-BSRYKA-G (3:)
      *
           IF      SPA-GMN-C02-BSRYKA  =   SPACE
               IF    ( SPA-GMN-C02-SRYKA   NOT =   ZERO    AND SPACE )
               AND   ( SPA-GMN-C02-SRYKAMEI
                                           NOT =   SPACE             )
                   MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
               ELSE
                   MOVE    "0030"              TO  SPA-ERRCD
               END-IF
           ELSE    
               MOVE    SPA-GMN-C02-BSRYKA  TO  WRK-KBN
               PERFORM 5001-BSRYKA-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-BSRYKAMEI
               IF      WRK-KBNMEI          =   SPACE  
                   MOVE    "0030"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    2                   TO  SPA-GMN-C02-CUR
           END-IF
           .
       41019-BSRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理区分入力処理
      *****************************************************************
       41022-SYOKBN-CHK-SEC            SECTION.
      *
           MOVE    32                  TO  SPA-GMN-C02-CUR
      *
           INITIALIZE                  SPA-GMN-C02-KOUMOKU
                                       SPA-NAI-C02-KOUMOKU
                                       SPA-NAI-C02-SELNO
                                       SPA-C02-UPDFLG
                                       SPA-C02-IKOFLG
      *                              
                                       SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-GMN-C02-MAX
                                       SPA-PTBYOMEI-REC
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
           IF    ( SPA-GMN-C02-SRYKA   NOT =   ZERO  )
           AND   ( SPA-GMN-C02-SRYKAMEI
                                       NOT =   SPACE )
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
      *
           MOVE    C02-SYOKBN          TO  SPA-GMN-C02-SYOKBN-G
      *
      *****IF      SPA-GMN-C02-SRYKA  =   SPACE
      *****    MOVE    SPA-GMN-C02-SYOKBNLST (1)
      *****                                TO  SPA-GMN-C02-SYOKBN-G
      *****ELSE    
               MOVE    SPA-GMN-C02-SYOKBN  TO  WRK-KBN
               PERFORM 5001-SYOKBN-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-SYOKBNMEI
               IF      WRK-KBNMEI          =   SPACE  
                   EVALUATE    SPA-C02-1042-SYOKBN-DSPKBN
                       WHEN    ZERO
                           MOVE    SPA-GMN-C02-SYOKBNLST (1)
                                               TO  SPA-GMN-C02-SYOKBN-G
                       WHEN    "1"
                           MOVE    SPA-GMN-C02-SYOKBNLST (2)
                                               TO  SPA-GMN-C02-SYOKBN-G
                       WHEN    "2"
                           MOVE    SPA-GMN-C02-SYOKBNLST (3)
                                               TO  SPA-GMN-C02-SYOKBN-G
                   END-EVALUATE
               END-IF
      *****END-IF
      *
      *    病名一覧
           MOVE    1                   TO  SPA-GMN-C02-PAGE
      *
           PERFORM 310-SPASET-PTBYO-HENSYU-SEC 
           MOVE    1                   TO  SPA-GMN-C02-CUR
           .
       41022-SYOKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分表示切替入力処理
      *****************************************************************
       41023-HNYUGAIKBN-CHK-SEC         SECTION.
      *   
           MOVE    33                   TO  SPA-GMN-C02-CUR
      *
           INITIALIZE                  SPA-GMN-C02-KOUMOKU
                                       SPA-NAI-C02-KOUMOKU
                                       SPA-NAI-C02-SELNO
                                       SPA-C02-UPDFLG
                                       SPA-C02-IKOFLG
      *                              
                                       SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-GMN-C02-MAX
                                       SPA-PTBYOMEI-REC
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
           IF    ( SPA-GMN-C02-SRYKA   NOT =   ZERO  )
           AND   ( SPA-GMN-C02-SRYKAMEI
                                       NOT =   SPACE )
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
      *
           MOVE    C02-HNYUGAIKBN      TO  SPA-GMN-C02-HNYUGAIKBN-G
      *
           MOVE    SPA-GMN-C02-HNYUGAIKBN
                                       TO  WRK-KBN
           PERFORM 5001-HNYUGAIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-HNYUGAIKBNMEI
           IF      SPA-GMN-C02-HNYUGAIKBNMEI  =   SPACE    
               MOVE    "0022"                 TO  SPA-ERRCD
               GO  TO  41023-HNYUGAIKBN-CHK-EXT
           END-IF
      *
      *    病名一覧
           MOVE    1                   TO  SPA-GMN-C02-PAGE
      *
           PERFORM 310-SPASET-PTBYO-HENSYU-SEC 
           MOVE    1                   TO  SPA-GMN-C02-CUR
           .
       41023-HNYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    補足コメントコードチェック処理
      *****************************************************************
       41024-HOSOKUCD-CHK-SEC          SECTION.
      *
      *    カーソル位置設定
           IF      MCP-WIDGET (1:8)   =   "HOSOKUCD"
               MOVE    MCP-WIDGET      TO  WRK-WIDGET
           ELSE
               EVALUATE    TRUE 
                   WHEN    C02-HOSOKUCD (1)
                                       NOT =   SPA-GMN-C02-HOSOKUCD (1)
                       MOVE    1               TO  WRK-WIDGET2
                   WHEN    C02-HOSOKUCD (2)
                                       NOT =   SPA-GMN-C02-HOSOKUCD (2)
                       MOVE    2               TO  WRK-WIDGET2
                   WHEN    C02-HOSOKUCD (3)
                                       NOT =   SPA-GMN-C02-HOSOKUCD (3)
                       MOVE    3               TO  WRK-WIDGET2
               END-EVALUATE
           END-IF
      *
           MOVE    WRK-WIDGET2     TO  IDZZ
           COMPUTE SPA-GMN-C02-CUR     =   33  +   IDZZ
      *
           MOVE    SPACE           TO  WRK-SEIGYO
      *
           IF    ( SPA-GMN-C02-HOSOKUCD-G
                                   =   SPACE )
               IF    ( C02-HOSOKUCD(1) =   SPACE )
               AND   ( C02-HOSOKUCD(2) =   SPACE )
               AND   ( C02-HOSOKUCD(3) =   SPACE )
      *            補足コメントコード欄未入力のとき補足コメント欄へ遷移
                   MOVE    40              TO  SPA-GMN-C02-CUR
                   GO  TO  41024-HOSOKUCD-CHK-EXT
               ELSE
      *            病名欄入力後コード入力した時の処理
                   MOVE    ZERO            TO  FLG-BYOMEICD-CHK
                   PERFORM VARYING IDZ     FROM    1   BY  1
                           UNTIL   IDZ     >       3
                       IF      C02-HOSOKUCD (IDZ)  NOT =   SPACE
                           IF      C02-HOSOKUCD (IDZ)(1:2) =   "//"
                               CONTINUE
                           ELSE
                               MOVE    1           TO  FLG-BYOMEICD-CHK
                           END-IF
                       END-IF 
                   END-PERFORM
      *            コード入力があるとき内部情報クリア
                   IF      FLG-BYOMEICD-CHK    =   1
                       INITIALIZE              SPA-NAI-C02-HOSOKU-TBL
                       MOVE    WRK-WIDGET2     TO  IDZZ
                   END-IF 
               END-IF
           END-IF 
      *
      *    病名コード格納前チェック処理
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >       3
                   OR      SPA-ERRCD   NOT =   SPACE
               IF    ( SPA-GMN-C02-HOSOKUCD (IDZ)
                                       NOT =   C02-HOSOKUCD (IDZ) )
               OR    ( IDZ                 =   IDZZ               )
                   PERFORM 41021-HOSOKUCD-CHECK-SEC
               END-IF
           END-PERFORM
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *****AND   ( SPA-CIDCD2          =   SPACE )
      *        エラー表示処理
               PERFORM 41023-HOSOKUCD-ERR-HYOUJI-SEC
      *        病名コード編集処理
               IF      SPA-CIDCD2          =   SPACE   OR  "9999"
                   MOVE    WRK-WIDGET2         TO  IDZZ
                   PERFORM 41021-HOSOKUCD-HENSYU-SEC
                   IF      SPA-ERRCD   NOT =   SPACE
                       MOVE    SPACE               TO  SPA-CIDCD2
                   END-IF
               END-IF
               IF      SPA-CIDCD2          =   "9999"
      *            自院病名検索へ遷移
                   MOVE    SPACE               TO  SPA-CIDCD2
                   PERFORM 490-USERBYOMEI-KENSAKU-SEC
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  WRK-SEIGYO
           .
       41024-HOSOKUCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    補足コメントコード格納前チェック処理
      *****************************************************************
       41021-HOSOKUCD-CHECK-SEC       SECTION.
      *
           INITIALIZE          SPA-NAI-C02-HOSOKUCD-CHK        (IDZ)
                               SPA-NAI-C02-HOSOKUCD-BUNRUICD   (IDZ)
      *
      *    病名検索へ遷移
           IF    ( C02-HOSOKUCD (IDZ) (1:2) =   "//"  )
               MOVE    "//"            TO  SPA-NAI-C02-HOSOKUCD-FLG(IDZ)
               MOVE    ZERO            TO
                                   SPA-NAI-C02-HOSOKUCD-BUNRUICD (IDZ)
               IF      C02-HOSOKUCD (IDZ) (3:) NOT =   SPACE
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    C02-HOSOKUCD (IDZ) (3:)
                                               TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF      SNUM-RC             =   ZERO
                   AND     SNUM-SEISUU         <   4
                       MOVE    SNUM-OUTNUM         TO
                                   SPA-NAI-C02-HOSOKUCD-BUNRUICD (IDZ)
                   END-IF
               END-IF
               MOVE    SPACE           TO  C02-HOSOKUCD (IDZ)
               GO  TO  41021-HOSOKUCD-CHECK-EXT
           END-IF
      *
      *    コードの挿入・削除の制御文字取得
           MOVE    SPACE           TO  WRK-SEIGYO
      *
           IF      C02-HOSOKUCD (IDZ)  NOT =   SPACE
               MOVE    C02-HOSOKUCD (IDZ)  TO  WRK-HOSOKUCD
               PERFORM VARYING IDX FROM    12  BY  -1
                       UNTIL   IDX <       2
                   IF      WRK-HOSOKUCD (IDX:1)
                                       NOT =   SPACE
                       EVALUATE    WRK-HOSOKUCD (IDX:1)
                           WHEN    "-"
                               MOVE    "-"         TO  WRK-SEIGYO
                           WHEN    "+"
                               MOVE    "+"         TO  WRK-SEIGYO
                           WHEN    OTHER
                               IF      IDX             >   2
                                   COMPUTE IDW     =   IDX     -   1
                                   IF      WRK-HOSOKUCD (IDW:2)
                                                   =   "−"
                                       MOVE    "-"     TO  WRK-SEIGYO
                                   END-IF
                                   IF      WRK-HOSOKUCD (IDW:2)
                                                   =   "＋"
                                       MOVE    "+"     TO  WRK-SEIGYO
                                   END-IF
                               END-IF
                       END-EVALUATE
      *                追加のとき
                       IF      WRK-SEIGYO      =   "+"
                           IF      SPA-GMN-C02-HOSOKUCD (3)
                                                   NOT =   SPACE
                               MOVE    SPACE           TO  WRK-SEIGYO
                           END-IF 
                           IF      IDZ         >   1
                               COMPUTE IDW     =   IDZ     -   1
                               IF      SPA-GMN-C02-HOSOKUCD (IDW)
                                                           =   SPACE 
                                   MOVE    SPACE       TO  WRK-SEIGYO
                               END-IF
                           END-IF
                           IF      WRK-SEIGYO  NOT =   SPACE
                               MOVE    WRK-SEIGYO  TO
                                           SPA-NAI-C02-HOSOKUCD-FLG(IDZ)
                               COMPUTE IDW     =   IDX -   1
                               MOVE    WRK-HOSOKUCD (1:IDW)
                                                   TO  C02-HOSOKUCD(IDZ)
                           ELSE
                               GO  TO  41021-HOSOKUCD-CHECK-EXT
                           END-IF
      *
                           MOVE    2               TO  IDX
                       END-IF
      *                削除のとき
                       IF      WRK-SEIGYO      =   "-"
                               MOVE    WRK-SEIGYO  TO
                                           SPA-NAI-C02-HOSOKUCD-FLG(IDZ)
                           MOVE    SPACE           TO  C02-HOSOKUCD(IDZ)
      *
                           MOVE    2               TO  IDX
                       END-IF
                   END-IF
               END-PERFORM
           ELSE
      *        空白の時
               MOVE    "S"             TO  SPA-NAI-C02-HOSOKUCD-FLG(IDZ)
               MOVE    SPACE           TO  C02-HOSOKUCD            (IDZ)
           END-IF
      *
           IF      C02-HOSOKUCD (IDZ)  =   SPACE
               GO  TO  41021-HOSOKUCD-CHECK-EXT
           END-IF
      *
      *    半角チェック
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    C02-HOSOKUCD (IDZ)  TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           IF    ( KANACHK-RC          =   ZERO )
           AND   ( KANACHK-RC2         =   ZERO )
               MOVE    KANACHK-OUT-INPUT   TO  C02-HOSOKUCD    (IDZ)
           ELSE
               MOVE    "0072"              TO
                                   SPA-NAI-C02-HOSOKUCD-ERRCD  (IDZ)
               GO  TO  41021-HOSOKUCD-CHECK-EXT
           END-IF
      *
           MOVE    SPACE               TO  C02-HOSOKUCD (IDZ) (11:)
           MOVE    C02-HOSOKUCD (IDZ)  TO  SPA-GMN-C02-HOSOKUCD (IDZ)
      *
           INITIALIZE                  SPA-NAI-C02-HOSOKU-OCC (IDZ)
      *         
           IF      C02-HOSOKUCD (IDZ) (1:1)
                                       =   "."
               MOVE    "ZZZ"       TO  SPA-GMN-C02-HOSOKUCD(IDZ)(1:3)
               MOVE    C02-HOSOKUCD(IDZ)(2:)
                                   TO  SPA-GMN-C02-HOSOKUCD(IDZ)(4:)
           END-IF
      *
           MOVE    ZERO                TO  WRK-LENGTH
           PERFORM VARYING IDX FROM    10  BY  -1
                   UNTIL   IDX <       1
               IF      SPA-GMN-C02-HOSOKUCD (IDZ) (IDX:1)
                                   NOT =   SPACE
                   MOVE    IDX             TO  WRK-LENGTH
                   MOVE    1               TO  IDX
               END-IF
           END-PERFORM
      *
      *    病名入力セット読込
           INITIALIZE                      BYOMEIINPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  BYOSET-HOSPNUM
           MOVE    SPA-GMN-C02-HOSOKUCD (IDZ)
                                       TO  BYOSET-BYOMEIINPUTSET
           MOVE    BYOMEIINPUTSET-REC  TO  MCPDATA-REC
           PERFORM 900-BYOMEIINPUTSET-READ-SEC
           IF      FLG-BYOMEIINPUTSET  =   ZERO
               MOVE    "■■■"            TO
                                       SPA-NAI-C02-HOSOKU-COMMENT (IDZ)
               MOVE    "0099"              TO
                                       SPA-NAI-C02-HOSOKUCD-ERRCD (IDZ)
           ELSE
               IF      WRK-LENGTH          =   7
      *            病名マスタ読込
                   INITIALIZE                  BYOMEI-REC
                   MOVE    SPA-GMN-C02-HOSOKUCD (IDZ)
                                               TO  BYO-BYOMEICD
                   PERFORM 410242-HOSOKU-HENSYU-SEC
               END-IF
      *
               IF    ( FLG-BYOMEI          =   1 )
               OR    ( WRK-LENGTH      NOT =   7 )
      *            自院病名マスタ読込
                   INITIALIZE                  USERBYOMEI-REC
                   MOVE    SPA-HOSPNUM     TO  USBYO-HOSPNUM
                   MOVE    SPA-GMN-C02-HOSOKUCD(IDZ)
                                           TO  USBYO-BYOMEIINPUTCD
                   PERFORM 410243-HOSOKU-USERBYO-HEN-SEC
                   IF      FLG-USERBYOMEI   NOT =   ZERO
      *                自院病名検索
                       PERFORM 41029-USERBYOMEICD-KENSAKU-SEC
                       IF      SPA-ERRCD   NOT =   SPACE
                           MOVE    SPA-ERRCD              TO
                                       SPA-NAI-C02-HOSOKUCD-ERRCD (IDZ)
                           IF    ( SPA-ERRCD       =   "0053" )
                           AND   ( WRK-LENGTH      =   7      )
                               MOVE    "0054"              TO
                                       SPA-NAI-C02-HOSOKUCD-ERRCD (IDZ)
                           END-IF
                           MOVE    SPACE           TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       41021-HOSOKUCD-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    補足コメントコード編集処理
      *****************************************************************
       41021-HOSOKUCD-HENSYU-SEC      SECTION.
      *
           MOVE    SPACE              TO  WRK-SEIGYO
      *    コードの挿入・削除の制御
           MOVE    1                  TO  IDX
           PERFORM            UNTIL   IDX >   3
               IF      SPA-NAI-C02-HOSOKUCD-FLG (IDX)
                                       NOT =   SPACE
                   IF      IDX             =   IDZZ
                       MOVE    SPA-NAI-C02-HOSOKUCD-FLG (IDX)
                                               TO  WRK-SEIGYO
                   END-IF
      *
                   EVALUATE    SPA-NAI-C02-HOSOKUCD-FLG (IDX)
                       WHEN    "-"
                       WHEN    "S"
                           MOVE    SPACE               TO
                                       SPA-NAI-C02-HOSOKUCD-FLG (IDX)
                           PERFORM VARYING IDY FROM    IDX BY  1
                                   UNTIL   IDY >       3
                               IF      IDY             =   3
                                   INITIALIZE
                                           SPA-NAI-C02-HOSOKU-OCC(IDY)
                                           SPA-GMN-C02-HOSOKUCD  (IDY)
                               ELSE
                                   COMPUTE IDW     =   IDY     +   1
                                   MOVE    SPA-NAI-C02-HOSOKU-OCC (IDW)
                                       TO  SPA-NAI-C02-HOSOKU-OCC (IDY)
                                   MOVE    SPA-GMN-C02-HOSOKUCD   (IDW)
                                       TO  SPA-GMN-C02-HOSOKUCD   (IDY)
                               END-IF
                           END-PERFORM
                       WHEN    "+"
                           MOVE    SPACE               TO
                                       SPA-NAI-C02-HOSOKUCD-FLG (IDX)
                           PERFORM VARYING IDY FROM    3   BY  -1
                                   UNTIL   IDY <       IDX
                               IF      IDY             =   IDX
                                   INITIALIZE
                                           SPA-NAI-C02-HOSOKU-OCC(IDY)
                                           SPA-GMN-C02-HOSOKUCD  (IDY)
                               ELSE
                                   COMPUTE IDW     =   IDY     -   1
                                   MOVE    SPA-NAI-C02-HOSOKU-OCC (IDW)
                                       TO  SPA-NAI-C02-HOSOKU-OCC (IDY)
                                   MOVE    SPA-GMN-C02-HOSOKUCD   (IDW)
                                       TO  SPA-GMN-C02-HOSOKUCD   (IDY)
                               END-IF
                           END-PERFORM
                           ADD     1               TO  IDX
                       WHEN    OTHER
                           ADD     1               TO  IDX
                   END-EVALUATE
               ELSE
                   ADD     1               TO  IDX
               END-IF 
           END-PERFORM
      *
      *    補足コメント編集処理
           PERFORM 410241-HOSOKUCD-HENSYU-SEC
      *****MOVE    SPACE               TO  WRK-SEIGYO
      *
           IF    ( SPA-ERRCD           NOT =   SPACE   AND "0092" )
           OR    ( SPA-CIDCD2          NOT =   SPACE              )
               GO  TO  41021-HOSOKUCD-HENSYU-EXT
           END-IF
      *  
      *    全ての補足コード未入力のとき補足コメントへ遷移
           IF    (  SPA-GMN-C02-CUR    =   34    )
           AND   ( SPA-GMN-C02-HOSOKUCD-G
                                       =   SPACE )
               MOVE    40              TO  SPA-GMN-C02-CUR
               GO  TO  41021-HOSOKUCD-HENSYU-EXT
           END-IF
      *
      *    補足コード未入力のとき開始日へ遷移
           IF      SPA-GMN-C02-HOSOKUCD (IDZZ)  =   SPACE
               IF      WRK-SEIGYO          =   "-"
                   GO  TO  41021-HOSOKUCD-HENSYU-EXT
               ELSE
                   PERFORM VARYING IDX3    FROM    IDZZ    BY  1
                           UNTIL   IDX3    >       3
                       IF       SPA-GMN-C02-HOSOKUCD (IDX3)
                                           NOT =   SPACE
                           GO  TO  41021-HOSOKUCD-HENSYU-EXT
                       END-IF
                   END-PERFORM
                   MOVE    9                   TO  SPA-GMN-C02-CUR
                   GO  TO  41021-HOSOKUCD-HENSYU-EXT
               END-IF
           ELSE
               IF      WRK-SEIGYO          =   "-" OR  "S"
                   GO  TO  41021-HOSOKUCD-HENSYU-EXT
               END-IF
           END-IF
      *
           ADD     1                   TO  SPA-GMN-C02-CUR
           IF      SPA-GMN-C02-CUR     =   37
               MOVE    9                   TO  SPA-GMN-C02-CUR
           END-IF
      *
           .
       41021-HOSOKUCD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    補足コメント編集処理
      *****************************************************************
       410241-HOSOKUCD-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE           TO  SPA-GMN-C02-HOSOKU-COMMENT
                                       WRK-BYOMEI
      *
           STRING  SPA-NAI-C02-HOSOKU-COMMENT (1)
                                           DELIMITED  BY  SPACE
                   SPA-NAI-C02-HOSOKU-COMMENT (2)
                                           DELIMITED  BY  SPACE
                   SPA-NAI-C02-HOSOKU-COMMENT (3)
                                           DELIMITED  BY  SPACE
                                       INTO    WRK-BYOMEI
           END-STRING
           MOVE    WRK-BYOMEI          TO  SPA-GMN-C02-HOSOKU-COMMENT
      *
      *    半角・文字数チェック
           IF      WRK-BYOMEI      NOT =   SPACE
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    WRK-BYOMEI          TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   0 )
               AND   ( KANACHK-SYUBETU     =   2 )
                   MOVE    KANACHK-OUT-INPUT   TO
                                           SPA-GMN-C02-HOSOKU-COMMENT
      *            病名文字数チェック
                   IF      KANACHK-MAX         >   40
                       MOVE    "0095"              TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    "0071"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  410241-HOSOKUCD-HENSYU-EXT
           END-IF
      *
      *    前後の（）チェック 
           IF      SPA-GMN-C02-HOSOKU-COMMENT
                                   NOT =   SPACE
               COMPUTE IDX3    =   KANACHK-MAX     -   1
               IF    ( SPA-GMN-C02-HOSOKU-COMMENT (1:2)
                                           =   "（" )
               OR    ( SPA-GMN-C02-HOSOKU-COMMENT (IDX3:2)
                                           =   "）" )
                   MOVE    "0092"              TO  SPA-ERRCD
               END-IF
           END-IF
           .
       410241-HOSOKUCD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    補足コメントコード編集処理
      *****************************************************************
       410242-HOSOKU-HENSYU-SEC          SECTION.
      *
           INITIALIZE                    SPA-NAI-C02-HOSOKU-OCC (IDZ)
      *
           MOVE    ZERO                  TO  FLG-BYOMEI
      *
           IF     BYO-BYOMEICD       NOT =   SPACE
               IF  (   BYO-BYOMEICD (1:3)    =   "ZZZ"    )
               AND ( ( BYO-BYOMEICD (4:1)    <   "1" ) OR 
                     ( BYO-BYOMEICD (4:1)    =   "C" )    )
      *            予約病名コード検索
                   SET     TBL-BYOMEICD-IDX  TO  1
                   SEARCH  TBL-BYOMEICD-OCC  VARYING TBL-BYOMEICD-IDX
                       AT  END
                           MOVE    1           TO  FLG-BYOMEI
                           MOVE    "■■■"    TO
                                       SPA-NAI-C02-HOSOKU-COMMENT (IDZ)
                       WHEN    BYO-BYOMEICD    =
                                   TBL-YYK-BYOMEICD (TBL-BYOMEICD-IDX)
                           MOVE    TBL-YYK-BYOMEI (TBL-BYOMEICD-IDX)
                                               TO
                                       SPA-NAI-C02-HOSOKU-COMMENT (IDZ)
                   END-SEARCH
               ELSE
      *            病名マスタ検索
                   PERFORM 900-BYOMEI-READ-SEC
                   IF      FLG-BYOMEI        =   ZERO
                       IF      BYO-BYOMEICD  =   WRK-CONS-BYOMEICD
                           MOVE    SPACE         TO
                                       SPA-NAI-C02-HOSOKU-COMMENT (IDZ)
                       ELSE                           
                           MOVE    BYO-BYOMEI    TO
                                       SPA-NAI-C02-HOSOKU-COMMENT (IDZ)
                       END-IF    
                   ELSE
                       MOVE    "■■■"      TO
                                       SPA-NAI-C02-HOSOKU-COMMENT (IDZ)
                   END-IF
               END-IF
           END-IF
           .
       410242-HOSOKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名マスタ編集処理（補足コメント）
      *****************************************************************
       410243-HOSOKU-USERBYO-HEN-SEC       SECTION.
      *
           INITIALIZE                  SPA-NAI-C04-HOSOKU-G (IDZ)
      *
           MOVE    ZERO                TO  FLG-USERBYOMEI
           IF      USBYO-BYOMEIINPUTCD =   SPACE
               CONTINUE
           ELSE
               MOVE    SPA-HOSPNUM     TO  USBYO-HOSPNUM
               PERFORM 900-USERBYOMEI-INV-SEC
               IF      FLG-USERBYOMEI  =   ZERO
                   MOVE    USBYO-BYOMEI    TO
                                       SPA-NAI-C02-HOSOKU-COMMENT (IDZ)
                   MOVE    "1"             TO 
                                       SPA-NAI-C02-HOSOKU-USERBYOFLG
                                                                  (IDZ)
               ELSE    
                   MOVE    "■■■"        TO
                                       SPA-NAI-C02-HOSOKU-COMMENT (IDZ)
               END-IF
           END-IF
           .    
       410243-HOSOKU-USERBYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    補足コメントコードエラー表示処理
      *****************************************************************
       41023-HOSOKUCD-ERR-HYOUJI-SEC  SECTION.
      *
      *    設定なしのとき
           IF    ( SPA-NAI-C02-HOSOKUCD-CHK (1)
                                       =   SPACE )
           AND   ( SPA-NAI-C02-HOSOKUCD-CHK (2)
                                       =   SPACE )
           AND   ( SPA-NAI-C02-HOSOKUCD-CHK (3)
                                       =   SPACE )
               GO  TO  41023-HOSOKUCD-ERR-HYOUJI-EXT
           END-IF
      *
      *    エラーコード表示
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       3
                   OR      SPA-ERRCD   NOT =   SPACE
               IF      SPA-NAI-C02-HOSOKUCD-ERRCD (IDX)
                                       NOT =   SPACE
                   MOVE    SPA-NAI-C02-HOSOKUCD-ERRCD (IDX)
                                           TO  SPA-ERRCD
                   COMPUTE SPA-GMN-C02-CUR     =   33  +   IDX
      ***          MOVE    IDX             TO  SPA-C02-HOSOKU-ERR-INDEX
                   PERFORM VARYING IDY FROM    IDX BY  1
                           UNTIL   IDY >       3
                       IF      SPA-ERRCD       =
                                       SPA-NAI-C02-HOSOKUCD-ERRCD (IDY)
                           MOVE    SPACE           TO
                                       SPA-NAI-C02-HOSOKUCD-ERRCD (IDY)
                       END-IF
                   END-PERFORM
      *************GO  TO  41023-HOSOKUCD-ERR-HYOUJI-EXT
               END-IF
           END-PERFORM
      *           
      *    エラー等があるとき病名検索はしない
           IF    ( SPA-ERRCD       NOT =   SPACE )
      *****OR    ( SPA-CIDCD2      NOT =   SPACE )
      *****OR    ( SPA-CIDCD6      NOT =   SPACE )
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       3
                   IF      SPA-NAI-C02-HOSOKUCD-FLG (IDX)
                                               =   "//"
                       INITIALIZE  SPA-NAI-C02-HOSOKUCD-FLG (IDX)
                   END-IF
               END-PERFORM
           ELSE
      *        病名検索へ
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       3
                   IF      SPA-NAI-C02-HOSOKUCD-FLG (IDX)
                                               =   "//"
                       PERFORM VARYING IDY FROM    IDX BY  1
                               UNTIL   IDY >       3
                           IF      SPA-NAI-C02-HOSOKUCD-FLG (IDY)
                                               =   "//"
                               INITIALIZE  SPA-NAI-C02-HOSOKUCD-FLG(IDY)
                                           SPA-NAI-C02-HOSOKUCD-BUNRUICD
                                                                   (IDY)
                           END-IF
                       END-PERFORM
                       MOVE    "9999"          TO  SPA-CIDCD2
                       MOVE    SPA-NAI-C02-HOSOKUCD-BUNRUICD (IDX)
                                               TO  WRK-BUNRUICD
                       GO  TO  41023-HOSOKUCD-ERR-HYOUJI-EXT
                   END-IF
               END-PERFORM
           END-IF
           .
       41023-HOSOKUCD-ERR-HYOUJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    補足コメント入力処理
      *****************************************************************
       41025-HOSOKU-COMMENT-CHK-SEC             SECTION.
      *
           MOVE    40                  TO  SPA-GMN-C02-CUR
      *
           MOVE    C02-HOSOKU-COMMENT  TO  SPA-GMN-C02-HOSOKU-COMMENT
      *
           IF      SPA-GMN-C02-HOSOKU-COMMENT
                                       =   SPACE
                INITIALIZE                 SPA-GMN-C02-HOSOKUCD-G
                                           SPA-NAI-C02-HOSOKU-TBL
           ELSE
      *        半角・文字数チェック
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    SPA-GMN-C02-HOSOKU-COMMENT
                                           TO  KANACHK-MAE-INPUT
               MOVE    40                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   0 )
               AND   ( KANACHK-SYUBETU     =   2 )
                   MOVE    KANACHK-OUT-INPUT   TO
                                           SPA-GMN-C02-HOSOKU-COMMENT
      *            病名文字数
                   IF      KANACHK-MAX         >   40
                       MOVE    "0095"              TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    "0071"              TO  SPA-ERRCD
               END-IF
      *
      *        前後の（）チェック 
               IF      SPA-ERRCD           =   SPACE
                   COMPUTE IDX3    =   KANACHK-MAX     -   1
                   IF    ( SPA-GMN-C02-HOSOKU-COMMENT (1:2)
                                               =   "（" )
                   OR    ( SPA-GMN-C02-HOSOKU-COMMENT (IDX3:2)
                                               =   "）" )
                       MOVE    "0092"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      **** IF      SPA-ERRCD       NOT =   SPACE
      ****     MOVE    40                  TO  SPA-GMN-C02-CUR
      **** END-IF 
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    28                  TO  SPA-GMN-C02-CUR
           END-IF
           .
       41025-HOSOKU-COMMENT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    第三者行為入力処理
      *****************************************************************
       41026-KOHID-CHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-C02-KOHID
      *
           IF      SPA-GMN-C02-KOHID   =   SPACE
               CONTINUE
           ELSE
               MOVE    SPA-GMN-C02-KOHID   TO  WRK-KBNMEI
               MOVE    ZERO                TO  WRK-KOHID
               PERFORM 5001-KOHID-HENSYU-SEC
               IF      WRK-KOHID           =   ZERO
                   MOVE    "0100"              TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KOHID       TO  SPA-NAI-C02-KOHID
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    41                  TO  SPA-GMN-C02-CUR
           END-IF
      *
           .
       41026-KOHID-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    退院証明書記載フラグ入力処理
      *****************************************************************
       41027-DISCHARGEFLG-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-C02-DISCHARGEFLG
                                       TO  WRK-KBN
           PERFORM 5001-DISCHARGEFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-DISCHARGEFLGMEI
      *
           IF      SPA-GMN-C02-DISCHARGEFLG =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C02-DISCHARGEFLGMEI =   SPACE
                   MOVE    "0040"              TO  SPA-ERRCD
                   MOVE    42                 TO  SPA-GMN-C02-CUR
               END-IF
           END-IF
           .
       41027-DISCHARGEFLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    原疾患区分入力処理
      *****************************************************************
       41028-CLASS1-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-C02-CLASS1 TO  WRK-KBN
           PERFORM 5001-CLASS1-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-CLASS1MEI
      *
           IF      SPA-GMN-C02-CLASS1 =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C02-CLASS1MEI   =   SPACE
                   MOVE    "0041"               TO  SPA-ERRCD
                   MOVE    43                 TO  SPA-GMN-C02-CUR
               END-IF
           END-IF
           .
       41028-CLASS1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    合併症区分入力処理
      *****************************************************************
       41029-CLASS2-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-C02-CLASS2 TO  WRK-KBN
           PERFORM 5001-CLASS2-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-CLASS2MEI
      *
           IF      SPA-GMN-C02-CLASS2 =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C02-CLASS2MEI   =   SPACE
                   MOVE    "0042"               TO  SPA-ERRCD
                   MOVE    44                 TO  SPA-GMN-C02-CUR
               END-IF
           END-IF
           .
       41029-CLASS2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41020-BYOMEISEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  SPA-C02-IKOFLG           
      *        行選択
           MOVE    ZERO                TO  WRK-SELNO
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C02-MAX
               IF      C02-BYO-SELECT (IDX)    =   "T"
                   IF      IDX             =   SPA-NAI-C02-SELNO
                       CONTINUE
                   ELSE        
                       MOVE    IDX                 TO  WRK-SELNO
                       MOVE    WRK-CONS-C02-MAX    TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNO           >   ZERO
               MOVE    WRK-SELNO           TO  SPA-NAI-C02-SELNO
           END-IF
      *
           INITIALIZE                 SPA-GMN-C02-KOUMOKU
                                      SPA-NAI-C02-KOUMOKU
      *
           MOVE    SPA-GMN-C02-TBANGO (SPA-NAI-C02-SELNO)
                                       TO  SPA-GMN-C02-BANGO
           MOVE    SPA-NAI-C02-SELNO   TO  WRK-BANGO
      *                                 
           PERFORM 410111-BYOMEI-SELECT-SEC
           .
       41020-BYOMEISEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41021-HKNCOMBISEL-SEC       SECTION.
      *          
      *
      *****IF      SPA-NAI-C02-BYOMEICD-MAX    >   1
      *****    MOVE    "0060"              TO  SPA-ERRCD
      *****    MOVE    2                   TO  SPA-GMN-C02-CUR 
      *****    GO  TO  41021-HKNCOMBISEL-EXT
      *****END-IF
      *     
      *        行選択
           MOVE    ZERO                TO  WRK-SELNO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C02-HKN-MAX
               IF      C02-HKN-SELECT (IDX)    =   "T"
                   MOVE    IDX                  TO  WRK-SELNO
                   MOVE    SPA-GMN-C02-HKN-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
      ***  MOVE    SPA-GMN-C02-TNUM (WRK-SELNO)
      ***                              TO  SPA-GMN-C02-HKNCOMBI
      *
           MOVE    ZERO               TO  WRK-HENFLG
           PERFORM VARYING IDY FROM   1   BY  1
                   UNTIL   IDY >      SPA-GMN-C02-HKNCOMBI-MAX
               IF      SPA-GMN-C02-TNUM (WRK-SELNO) 
                                      =   SPA-GMN-C02-HKNCOMBIL (IDY)
                   IF      SPA-NAI-C02-SRYYMD
                                  <= SPA-NAI-C02-TEKEDYMD (IDY)
                       MOVE    SPA-GMN-C02-HKNCOMBILST (IDY)
                                          TO  SPA-GMN-C02-HKNCOMBI-G
                   ELSE    
                       MOVE    "0011"     TO  SPA-ERRCD
                   END-IF        
                   MOVE    1              TO  WRK-HENFLG
                   MOVE    SPA-GMN-C02-HKNCOMBI-MAX
                                          TO  IDY
               END-IF
           END-PERFORM
           IF      WRK-HENFLG         =   ZERO
           AND     SPA-ERRCD          =   SPACE
               MOVE    "0009"               TO  SPA-ERRCD
           END-IF
           .
       41021-HKNCOMBISEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *----(01.00.02) LINE ADD START ----------------------------------
           IF      SPA-C02-UPDFLG          =   "1"
               MOVE    "0101"          TO  SPA-CIDCD
               GO      TO      210-BACK-EXT
           END-IF
      *----(01.00.02) LINE ADD END   ----------------------------------
      *
      *    感染症送信
           IF     (SPA-DATAINFECTIONKBN    =      1)  AND
                  (SPA-NAI-C02-PTID    NOT =   ZERO)
               PERFORM 690-INFECTION-SEND-SEC
           END-IF
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    SPA-C02-MOTOPG     TO  SPA-SAKIPG
           IF      SPA-C02-MOTOPG NOT =   "K02"  AND  "U02"   AND
                                          "K02N" AND  "D04"   AND
                                          "KT01" AND  "Q02" 
      ********* 患者番号入力へ
      *********MOVE   "C01"            TO  SPA-SAKIPG
               MOVE   "M01"            TO  SPA-SAKIPG
           END-IF
           MOVE    "C02"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  SPA-WORK 
           IF      SPA-C02-MOTOPG  NOT =   "K02"  AND  "U02"  AND 
                                           "K02N" AND  "D04"  AND
                                           "KT01" AND  "Q02"
               INITIALIZE                  SPA-KYOTUKEY
                                           SPA-KYOTU-SET
           END-IF 
           MOVE    SPACE               TO  SPA-PTNUM
           MOVE    ZERO                TO  SPA-PTID
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
      ***  MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *    
      *****************************************************************
      *    クリア処理
      *****************************************************************
       220-CLEAR-SEC                   SECTION.
      *
           MOVE    SPACE               TO  WRK-HAISIYMDH
           MOVE    ZERO                TO  SPA-C02-IKOFLG
      *      
           IF    ( SPA-GMN-C02-BYOMEI  NOT =   SPACE )
           OR    ( SPA-GMN-C02-BANGO-X NOT =   SPACE )
           OR    ( SPA-NAI-C02-UPDOWN      =   1     )
               INITIALIZE              SPA-GMN-C02-KOUMOKU
                                       SPA-NAI-C02-KOUMOKU
                                       SPA-NAI-C02-SELNO
               IF      SPA-NAI-C02-UPDOWN     =   1
      *            検索処理
                   PERFORM 310-SPASET-PTBYO-HENSYU-SEC
               END-IF
               INITIALIZE              SPA-NAI-C02-UPDOWN
               MOVE    1               TO  SPA-GMN-C02-CUR
           ELSE                             
      *        感染症送信
               IF     (SPA-DATAINFECTIONKBN    =      1)  AND
                      (SPA-NAI-C02-PTID    NOT =   ZERO)
                   PERFORM 690-INFECTION-SEND-SEC
               END-IF
      *
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
      *
               INITIALIZE                  SPA-GMN-C02-KOUMOKU
                                           SPA-NAI-C02-KOUMOKU
                                           SPA-NAI-C02-SELNO
                                           SPA-C02-UPDFLG
                                           SPA-C02-IKOFLG
                                           SPA-GMN-C02-NEXT
      *                              
                                           SPA-GMN-C02-TBL
                                           SPA-NAI-C02-TBL
                                           SPA-GMN-C02-MAX
                                           SPA-GMN-C02-SRYKA-G
                                           SPA-PTBYOMEI-REC
               EVALUATE    SPA-C02-1042-SYOKBN-DSPKBN
                   WHEN    ZERO
                       MOVE    SPA-GMN-C02-SYOKBNLST (1)
                                               TO  SPA-GMN-C02-SYOKBN-G
                   WHEN    "1"
                       MOVE    SPA-GMN-C02-SYOKBNLST (2)
                                               TO  SPA-GMN-C02-SYOKBN-G
                   WHEN    "2"
                       MOVE    SPA-GMN-C02-SYOKBNLST (3)
                                               TO  SPA-GMN-C02-SYOKBN-G
               END-EVALUATE
               MOVE    SPA-GMN-C02-HNYUGAIKBNLST (1)
                                       TO  SPA-GMN-C02-HNYUGAIKBN-G   
      *
      ******** IF      SPA-C02-MOTOPG      =   "K02"   OR  "U02"   OR
      *                                         "K02N"
      *             MOVE    27              TO  SPA-GMN-C02-CUR
      ******** ELSE                                     
                   INITIALIZE              SPA-GMN-C02-HEAD
                                           SPA-NAI-C02-HEAD
                                           SPA-GMN-C02-HKN-TBL
                                           SPA-NAI-C02-HKN-TBL
                                           SPA-GMN-C02-HKNCOMBILST-G
                                           SPA-GMN-C02-HKN-MAX
                                           SPA-GMN-C02-HKNCOMBI-MAX
                   MOVE    26              TO  SPA-GMN-C02-CUR
      ******** END-IF
      *
               IF      SPA-GMN-C02-SRYKA-MAX   =   1 
                   MOVE    SPA-GMN-C02-SRYKALST (1)
                                           TO  SPA-GMN-C02-SRYKA-G
               END-IF                            
           END-IF                                    
      *
           IF    ( SPA-GMN-C02-SRYKA   NOT =   ZERO  )
           AND   ( SPA-GMN-C02-SRYKAMEI
                                       NOT =   SPACE )
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
      *
           .
       220-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       460-MAEGMN-SEC             SECTION.
      *
           IF      SPA-GMN-C02-BANGO  >   ZERO
               MOVE    "0005"              TO  SPA-ERRCD
               GO  TO  460-MAEGMN-EXT
           END-IF
      *
           IF      SPA-GMN-C02-PAGE    =   1
               GO      TO      460-MAEGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-C02-PAGE    =   SPA-GMN-C02-PAGE    -   1
      *
           INITIALIZE                  SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-NAI-C02-SELNO
      *    検索処理
           PERFORM 310-SPASET-PTBYO-HENSYU-SEC
      *
           .
       460-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁処理
      *****************************************************************
       470-ATOGMN-SEC             SECTION.
      *
           IF      SPA-GMN-C02-BANGO  >   ZERO
               MOVE    "0005"              TO  SPA-ERRCD
               GO  TO  470-ATOGMN-EXT
           END-IF
      *
           IF      SPA-GMN-C02-NEXT        =   ZERO
               GO      TO      470-ATOGMN-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-C02-PAGE
      *
           INITIALIZE                  SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-NAI-C02-SELNO
      *    検索処理
           PERFORM 310-SPASET-PTBYO-HENSYU-SEC
      *
           .
       470-ATOGMN-EXT.
           EXIT.
      *    
      *****************************************************************
      *    前回患者番号編集処理
      *****************************************************************
       230-MAEKPTNUM-SEC             SECTION.
      *
           IF      SPA-LAST-PTNUM      =   SPACE
               GO      TO      230-MAEKPTNUM-EXT
           END-IF
      *
           MOVE    SPA-LAST-PTNUM      TO  C02-PTNUM
           PERFORM 41015-PTNUM-CHK-SEC
      *
           .
       230-MAEKPTNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除ガイドメッセージ処理
      *****************************************************************
       440-SAKUJYO-GID-SEC             SECTION.
      *
           IF      SPA-NAI-C02-PTID    =   ZERO
      *****OR      SPA-GMN-C02-BSRYKA  =   SPACE
               MOVE    "0050"              TO  SPA-ERRCD
               GO  TO  440-SAKUJYO-GID-EXT
           END-IF
      *
           IF      SPA-GMN-C02-BANGO  =   ZERO
               MOVE    "0004"              TO  SPA-ERRCD
               GO  TO  440-SAKUJYO-GID-EXT
           END-IF
      *
      *    入力項目チェック
           MOVE    SPA-GMN-C02-BSRYKA-G   TO  C02-BSRYKA
           PERFORM 41019-BSRYKA-CHK-SEC
           IF      SPA-ERRCD           NOT =   SPACE         
               GO  TO  440-SAKUJYO-GID-EXT
           END-IF
      *
           MOVE    "0101"             TO  SPA-CIDCD4
          .
       440-SAKUJYO-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除　処理
      *****************************************************************
       440-SAKUJYO-SEC             SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                      PTBYO-KEY
grpsys     MOVE    SPA-HOSPNUM             TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID        TO  PTBYO-PTID
           MOVE    SPA-NAI-C02-SRYKA-OLD   TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-SRYYMD-OLD  TO  PTBYO-SRYYMD
           MOVE    SPA-NAI-C02-RENNUM      TO  PTBYO-RENNUM 
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM 900-PTBYO-INV-SEC
           IF      FLG-PTBYOMEI        =   1
               MOVE    "1003"              TO  SPA-ERRCD
               DISPLAY "INV PTBYOMEI-KEY =>" PTBYO-KEY 
               GO  TO  440-SAKUJYO-EXT
           END-IF
      *    削除フラグ
           MOVE    "1"                 TO  PTBYO-DLTFLG
           MOVE    SPA-OPID            TO  PTBYO-DLT-OPID
      *
           MOVE    SPA-OPID            TO  PTBYO-OPID
           MOVE    SMCNDATE-YMD        TO  PTBYO-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTBYO-UPHMS
           MOVE    "DBUPDATE"          TO  MCP-FUNC
      *???
           DISPLAY "PTBYO=" PTBYO-KEY "#" MCP-FUNC "#"
      *???                
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1002"                  TO  SPA-ERRCD
               DISPLAY "PTBYOMEI-KEY =>" PTBYO-KEY 
               GO  TO  440-SAKUJYO-EXT
           ELSE
               PERFORM 450221-RECEUPD-UPDATE-SEC
           END-IF
      *
           COMPUTE C02-BYO-ROW =   SPA-NAI-C02-SELNO   -   1    
      *
           INITIALIZE                  SPA-GMN-C02-KOUMOKU
                                       SPA-NAI-C02-KOUMOKU
                                       SPA-NAI-C02-SELNO
           IF    ( SPA-GMN-C02-SRYKA   NOT =   ZERO  )
           AND   ( SPA-GMN-C02-SRYKAMEI
                                       NOT =   SPACE )
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
      *
          .
       440-SAKUJYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録前処理
      *****************************************************************
       450-TOUROKU-SEC                SECTION.
      *
      *
           IF     (SPA-NAI-C02-PTID         =   ZERO  )
      *****OR     (SPA-GMN-C02-BSRYKA       =   SPACE )
               MOVE    "0050"             TO  SPA-ERRCD
               GO  TO  450-TOUROKU-EXT
           END-IF
      *
      *    入力チェック
           MOVE    SPA-GMN-C02-BSRYKA-G   TO  C02-BSRYKA
           PERFORM 41019-BSRYKA-CHK-SEC
           IF      SPA-ERRCD           NOT =   SPACE
               GO  TO  450-TOUROKU-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-TOUROKU
           PERFORM 400-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
           OR    ( SPA-CIDCD       NOT =   SPACE )
               GO  TO  450-TOUROKU-EXT
           END-IF
      *
      *    登録処理
           PERFORM 4502-TOUROKU-SYORI-SEC
           .
       450-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    警告チェック処理
      *****************************************************************
       4501-KEIKOKU-CHK-SEC                SECTION.
      *
           IF     (SPA-GMN-C02-BANGO        =   ZERO  )
           AND    (SPA-GMN-C02-BYOMEI       =   SPACE )
               MOVE    "0051"             TO  SPA-ERRCD
               GO  TO  4501-KEIKOKU-CHK-EXT
           END-IF
      *
      *    病名が未入力のとき
           IF      SPA-NAI-C02-BYOMEIMOJI   =   ZERO
               MOVE    "0010"               TO  SPA-ERRCD
               MOVE    2                    TO  SPA-GMN-C02-CUR
               GO  TO  4501-KEIKOKU-CHK-EXT
           END-IF
      *
      *    病名コードが存在しないコードのとき
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       21
                   OR      SPA-NAI-C02-BYOMEICD  (IDZ)
                                           =   SPACE
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1    >       21
                       OR      SPA-NAI-C02-BYOMEICD-O  (IDZ IDX1)
                                       =   SPACE
                   IF      SPA-NAI-C02-BYOMEI-O   (IDZ IDX1)
                                           =   "■■■"
                       MOVE    "0091"          TO  SPA-ERRCD
                       MOVE    2               TO  SPA-GMN-C02-CUR
                       GO  TO  4501-KEIKOKU-CHK-EXT
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    補足コメントのチェック
           IF      SPA-GMN-C02-HOSOKU-COMMENT
                                   NOT =   SPACE 
               PERFORM 41025-HOSOKU-COMMENT-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO  4501-KEIKOKU-CHK-EXT
               END-IF
      *
               PERFORM VARYING IDZ FROM    1   BY  2
                       UNTIL   IDZ >       39
                       OR      SPA-GMN-C02-HOSOKU-COMMENT  (IDZ:2)
                                               =   SPACE
                   IF      SPA-GMN-C02-HOSOKU-COMMENT  (IDZ:2)
                                               =   "■"
                       MOVE    34                  TO  SPA-GMN-C02-CUR
                       MOVE    "0093"              TO  SPA-ERRCD
                       GO  TO  4501-KEIKOKU-CHK-EXT
                   END-IF
               END-PERFORM
           END-IF
      *    
      *    新規以外のときは複数病名の入力不可
           IF    ( SPA-NAI-C02-BYOMEICD-MAX    >   1    )
               IF    ( SPA-GMN-C02-BANGO           >   ZERO )
                   MOVE    "0056"             TO  SPA-ERRCD
                   MOVE    2                  TO  SPA-GMN-C02-CUR
                   GO      TO      4501-KEIKOKU-CHK-EXT
               END-IF
           END-IF
      *    
      *    複数病名のとき補足コメントの入力不可
           IF    ( SPA-NAI-C02-BYOMEICD-MAX    >   1     )
           AND   ( SPA-GMN-C02-HOSOKU-COMMENT
                                           NOT =   SPACE )
               MOVE    "0094"              TO  SPA-ERRCD
               INITIALIZE                  SPA-GMN-C02-HOSOKUCD-G
                                           SPA-NAI-C02-HOSOKU-TBL
               MOVE    SPACE               TO
                                           SPA-GMN-C02-HOSOKU-COMMENT
      *??
               MOVE    34                  TO  SPA-GMN-C02-CUR
               GO  TO  4501-KEIKOKU-CHK-EXT
           END-IF
      *
      *    単独使用禁止の病名のとき
           INITIALIZE                  WRK-TANDOKU-AREA
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       21
               IF    ( SPA-NAI-C02-BYOMEITOP (IDZ)
                                           =   "1" )
               AND   ( IDZ                 >   1   )
                   IF    ( WRK-TANDOKUKBN      =   ZERO  )
                   AND   ( SPA-GMN-C02-HOSOKU-COMMENT
                                               =   SPACE )
                       MOVE    "0076"              TO  SPA-ERRCD  
                       GO  TO  4501-KEIKOKU-CHK-EXT
                   END-IF
                   INITIALIZE                  WRK-TANDOKU-AREA
               END-IF
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1    >       21
                       OR      SPA-NAI-C02-BYOMEICD-O  (IDZ IDX1)
                                       =   SPACE
                   IF      SPA-NAI-C02-TANDOKUKBN-O  (IDZ IDX1) 
                                           =   ZERO
                       MOVE    1       TO  WRK-TANDOKUKBN
                   ELSE
                       MOVE    SPA-NAI-C02-BYOMEICD-O (IDZ IDX1)
                                       TO  WRK-TANDOKUKBN-BYOMEICD  
                       MOVE    SPA-NAI-C02-BYOMEI-O   (IDZ IDX1)
                                       TO  WRK-TANDOKUKBN-BYOMEI  
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           IF    ( WRK-TANDOKUKBN      =   ZERO  )
           AND   ( SPA-GMN-C02-HOSOKU-COMMENT
                                       =   SPACE )
               MOVE    "0076"              TO  SPA-ERRCD
               MOVE    8                   TO  SPA-GMN-C02-CUR
               GO  TO  4501-KEIKOKU-CHK-EXT
           END-IF
      *
      *    退院証明書記載フラグ重複チェック処理
           IF    ( SPA-NAI-C02-BYOMEICD-MAX
                                       =   1   )
           AND   ( SPA-GMN-C02-DISCHARGEFLG
                                       =   "1" )
               PERFORM 4508-DISCHARGEFLG-DABURI-CK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    42                  TO  SPA-GMN-C02-CUR
                   GO  TO  4501-KEIKOKU-CHK-EXT
               END-IF
           END-IF
      *
      *    新規又は科変更、開始日変更時の行番号追加が可能か
           IF   (  SPA-NAI-C02-RENNUM  =   ZERO                 )
           OR   (    ( SPA-GMN-C02-BSRYKA  NOT =
                                       SPA-NAI-C02-SRYKA-OLD  )
                  OR ( SPA-NAI-C02-SRYYMD  NOT =
                                       SPA-NAI-C02-SRYYMD-OLD ) )
      *        病名数のチェック
               INITIALIZE                      PTBYOMEI-REC
               MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
               MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
               MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
               MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
               MOVE    ZERO                TO  PTBYO-RENNUM
               MOVE    "%"                 TO  PTBYO-DLTFLG
               MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
               MOVE    "key34"             TO  WRK-MCP-PATHNAME
               PERFORM 900-PTBYO-INV-SEC
      *        同一診療科、開始日で９９件のとき（削除分含む）
               COMPUTE IDXX    =   PTBYO-RENNUM
                               +   SPA-NAI-C02-BYOMEICD-MAX
               IF      IDXX                >   99
                   MOVE    "0034"              TO  SPA-ERRCD
                   INITIALIZE                      PTBYOMEI-REC
grpsys             MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
                   MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
                   MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
                   MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
                   MOVE    ZERO                TO  PTBYO-RENNUM
                   MOVE    "1"                 TO  PTBYO-DLTFLG
                   MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
                   MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
                   MOVE    "key34"             TO  WRK-MCP-PATHNAME
                   PERFORM 900-PTBYO-INV-SEC
      *            同一診療科、開始日で削除分以外が９９件のとき
                   COMPUTE IDXX    =   IDXX    -   PTBYO-RENNUM
                   IF      IDXX                >   99
                       MOVE    "0088"          TO  SPA-ERRCD
                   END-IF
                   GO  TO  4501-KEIKOKU-CHK-EXT
               END-IF
           END-IF
      *
      *    病名編集
           IF      SPA-NAI-C02-SRYYMD   =   SPACE
               MOVE    SPA-NAI-C02-SYSYMD   TO  SPA-NAI-C02-SRYYMD
           END-IF
      *
           MOVE    SPACE           TO  WRK-HAISIYMD-OCC
                                       WRK-PTBYOMEI-REC (1)
                                       WRK-PTBYOMEI-REC (2)
                                       WRK-PTBYOMEI-REC (3)
                                       WRK-PTBYOMEI-REC (4)
                                       WRK-PTBYOMEI-REC (5)
                                       WRK-PTBYOMEI-REC (6)
           INITIALIZE                  WRK-HAISIYMD-OCC
                                       WRK-PTBYOMEI-REC (1)
                                       WRK-PTBYOMEI-REC (2)
                                       WRK-PTBYOMEI-REC (3)
                                       WRK-PTBYOMEI-REC (4)
                                       WRK-PTBYOMEI-REC (5)
                                       WRK-PTBYOMEI-REC (6)
           MOVE    ZERO            TO  IDXX
           MOVE    ZERO            TO  IDY
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       21
               IF      SPA-NAI-C02-BYOMEITOP (IDX)
                                           =   "1"
                   IF      IDXX            >   ZERO
                       PERFORM 4503-BYOMEICD-HENSYU-02-SEC
                   END-IF
                   ADD     1               TO  IDXX
                   MOVE    ZERO            TO  IDY
               END-IF
               PERFORM 4501-BYOMEICD-HENSYU-SEC
           END-PERFORM
           IF      IDXX            >   ZERO
               PERFORM 4503-BYOMEICD-HENSYU-02-SEC
           END-IF
      *
      *    病名重複チェック（画面・ＤＢ）
           MOVE    SPACE           TO  WRK-ERR-BYOMEI
                                       WRK-ERR-SRYYMD
                                       WRK-ERR-SRYKA
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       6
                   OR      SPA-ERRCD           NOT =   SPACE
                   OR      WRK-PTBYO-BYOMEI (IDXX) =   SPACE
               PERFORM 4502-DABURI-CHK-SEC
               IF      SPA-ERRCD          NOT =   SPACE
                   MOVE    WRK-PTBYO-BYOMEI (IDXX)
                                              TO  WRK-ERR-BYOMEI
                   MOVE    6                  TO  IDXX                  
               END-IF
           END-PERFORM
      *
      *    警告チェック
           IF      SPA-ERRCD            =   SPACE
               PERFORM 45011-KEIKOKU-CHK-SEC
           END-IF
           .
       4501-KEIKOKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    警告チェック処理
      *****************************************************************
       45011-KEIKOKU-CHK-SEC                SECTION.
      * 
           MOVE    SPACE           TO  WRK-ERR-BYOMEI
                                       WRK-ERR-SRYYMD
                                       WRK-ERR-SRYKA
      * 
           INITIALIZE                  SPA-C02-KEIKOKU-FLG-AREA
                                       SPA-C02-KEIKOKU-MSG-AREA
      *
      *    死亡区分更新確認メッセージ表示のチェック
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   ZERO
               IF      SPA-GMN-C02-TENKIKBN    =   "2"
      *            転帰区分が２で死亡区分が生存のとき
                   IF      PTINF-DEATHKBN  NOT =   "1"
                       MOVE    1           TO  SPA-C02-KEIKOKU-FLG4
                       IF      SPA-C02-1042-MSG5-DSPKBN
                                           =   0
                           MOVE    "0105"      TO  SPA-CIDCD
                       END-IF
                   END-IF
               ELSE
      *            修正で転帰区分を２以外に変更したときに死亡区分が死亡のとき
                   IF    ( SPA-NAI-C02-SELNO   >   ZERO )
                   AND   ( SPA-NAI-C02-TENKIKBN-OLD
                                               =   "2"  )
                       IF      PTINF-DEATHKBN      =   "1"
                           MOVE    ZERO        TO  FLG-TENKIKBN
      *
                           INITIALIZE                  PTBYOMEI-REC
                           MOVE    SPA-HOSPNUM     TO  PTBYO-HOSPNUM
                           MOVE    SPA-NAI-C02-PTID
                                                   TO  PTBYO-PTID
                           MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
                           MOVE    "key48"         TO  MCP-PATHNAME
                           MOVE    PTBYOMEI-REC    TO  MCPDATA-REC
                           PERFORM 900-DBSELECT-SEC
                           IF      MCP-RC          =   ZERO
                               MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
                               MOVE    "key48"         TO  MCP-PATHNAME
                               PERFORM 900-PTBYO-READ-SEC
                           ELSE
                               INITIALIZE              PTBYOMEI-REC
                               MOVE    1           TO  FLG-PTBYOMEI
                           END-IF
                           PERFORM     UNTIL   FLG-PTBYOMEI    =   1
                                       OR      FLG-TENKIKBN    =   9
                               IF    ( PTBYO-SRYYMD    =
                                                   SPA-NAI-C02-SRYYMD )
                               AND   ( PTBYO-RENNUM    =
                                                   SPA-NAI-C02-RENNUM )
                               AND   ( PTBYO-SRYKA     =
                                                   SPA-GMN-C02-BSRYKA )
                                   CONTINUE
                               ELSE
                                   MOVE    9       TO  FLG-TENKIKBN
                               END-IF
                               MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
                               MOVE    "key48"         TO  MCP-PATHNAME
                               PERFORM 900-PTBYO-READ-SEC
                           END-PERFORM
                           MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
                           MOVE    "key48"         TO  MCP-PATHNAME
                           PERFORM 900-CLOSE-SEC
      *
                           IF      FLG-TENKIKBN        =   9
                               CONTINUE
                           ELSE
      *                        転帰区分２の病名が存在しないとき
                               IF      SPA-C02-1042-MSG5-DSPKBN
                                                       =   0
                                   MOVE    "0108"      TO  SPA-CIDCD
                               END-IF
                               MOVE    9           TO
                                                   SPA-C02-KEIKOKU-FLG4
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    他診療科の同一病名の警告チェック
           IF      SPA-C02-1042-MSG4-DSPKBN
                                           =   1
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       6
      **************** OR      SPA-CIDCD           NOT =   SPACE
                       OR      WRK-PTBYO-BYOMEI (IDXX) =   SPACE
                   INITIALIZE                      PTBYOMEI-REC
grpsys             MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
                   MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
                   MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
                   MOVE    WRK-PTBYO-BYOMEI (IDXX)
                                               TO  PTBYO-BYOMEI
                   MOVE    WRK-PTBYO-HOSOKU-COMMENT (IDXX)
                                               TO  PTBYO-HOSOKU-COMMENT
                   MOVE    WRK-PTBYO-NYUGAIKBN (IDXX)
                                               TO  PTBYO-NYUGAIKBN
                   IF      PTBYO-NYUGAIKBN     =   SPACE
                       MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
                       MOVE    "key42"             TO  WRK-MCP-PATHNAME
                   ELSE
                       MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
                       MOVE    "key43"             TO  WRK-MCP-PATHNAME
                   END-IF
      *
                   MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
                   MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys             PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                       MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                       PERFORM 900-PTBYO-READ-SEC
                   ELSE
                       MOVE    1                   TO  FLG-PTBYOMEI
                   END-IF
      *
                   PERFORM             UNTIL   FLG-PTBYOMEI    =   1
      ***********                      OR      SPA-CIDCD   NOT =   SPACE
                     IF    ( SPA-NAI-C02-RENNUM   >   ZERO         )
                     AND   ( SPA-NAI-C02-SRYYMD-OLD
                                                  =   PTBYO-SRYYMD )
                     AND   ( SPA-NAI-C02-SRYKA-OLD
                                                  =   PTBYO-SRYKA  )
                     AND   ( SPA-NAI-C02-RENNUM   =   PTBYO-RENNUM )
                         CONTINUE
                     ELSE
                       EVALUATE    TRUE
                           WHEN    SPA-NAI-C02-SRYYMD  =   PTBYO-SRYYMD
                               MOVE    1       TO  SPA-C02-KEIKOKU-FLG1
                           WHEN    SPA-NAI-C02-SRYYMD  <   PTBYO-SRYYMD
                               IF      SPA-NAI-C02-TENKIYMD    =   SPACE
                                   MOVE    1          TO
                                                  SPA-C02-KEIKOKU-FLG1
                               ELSE
                                   IF     SPA-NAI-C02-TENKIYMD
                                                   >   PTBYO-SRYYMD
                                       MOVE    1          TO
                                                   SPA-C02-KEIKOKU-FLG1
                                   END-IF
                               END-IF
                           WHEN    SPA-NAI-C02-SRYYMD  >   PTBYO-SRYYMD
                               IF      PTBYO-TENKIYMD  =   SPACE
                                   MOVE    1          TO
                                                   SPA-C02-KEIKOKU-FLG1
                               ELSE
                                   IF      SPA-NAI-C02-SRYYMD
                                                   <   PTBYO-TENKIYMD
                                       MOVE    1          TO
                                                   SPA-C02-KEIKOKU-FLG1
                                   END-IF
                               END-IF
                       END-EVALUATE
                       IF      SPA-C02-KEIKOKU-FLG1    =   1
                           INITIALIZE              WRK-CHK-HKNCOMBI-AREA
      *
                           MOVE    SPA-GMN-C02-HKNCOMBI
                                                   TO  WRK-CHK-HKNCOMBI1
                           MOVE    PTBYO-HKNCOMBI  TO  WRK-CHK-HKNCOMBI2
                           MOVE    SPA-NAI-C02-KOHID
                                                   TO  WRK-CHK-KOHID1
                           MOVE    PTBYO-KOHID     TO  WRK-CHK-KOHID2
                           PERFORM 45021-HKNCOMBI-DABURI-CHK-SEC
                           IF      FLG-HKNCOMBI-DABURI =   1
                               MOVE    ZERO   TO  SPA-C02-KEIKOKU-FLG1
                           ELSE
                               MOVE    1               TO  FLG-PTBYOMEI
                               IF      SPA-CIDCD       =   SPACE
                                   MOVE    "0103"          TO  SPA-CIDCD
                               END-IF
                               MOVE    PTBYO-SRYYMD        TO  WRK-SYMD
                               PERFORM 5002-HIZUKE-HEN-SEC
                               MOVE    LNK-DAY2-EDTYMD3    TO
                                                       WRK-ERR-SRYYMD
                               MOVE    PTBYO-SRYKA         TO  WRK-KBN
                               PERFORM 5001-SRYKA-HENSYU-SEC
                               MOVE    WRK-KBNMEI          TO
                                                       WRK-ERR-SRYKA
                               MOVE    WRK-PTBYO-BYOMEI (IDXX)
                                                           TO
                                                       WRK-ERR-BYOMEI
                               MOVE    6                   TO  IDXX
                           END-IF
                       END-IF
                     END-IF
      *
                     IF      FLG-PTBYOMEI    =   ZERO
                         MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                         MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                         PERFORM 900-PTBYO-READ-SEC
                     END-IF
                   END-PERFORM
      *
                   MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               END-PERFORM
           END-IF
      *
      *    慢性と急性の警告チェック
           IF    ( SPA-NAI-C02-BYOMEICD-MAX    =   1           )
           AND   ( SPA-GMN-C02-UTAGAIFLG       =   "2" OR  "3" ) 
               MOVE    ZERO                TO  FLG-MANSEI
                                               FLG-KYUSEI
               PERFORM VARYING IDZZ    FROM    1   BY  2
                       UNTIL   IDZZ    >       79
                   IF      WRK-PTBYO-BYOMEI (1) (IDZZ:4)
                                                   =   "慢性"
                       MOVE    1               TO  FLG-MANSEI
                   END-IF
                   IF      WRK-PTBYO-BYOMEI (IDXX) (IDZZ:4)
                                                   =   "急性"
                       MOVE    1               TO  FLG-KYUSEI
                   END-IF
                   IF    ( FLG-MANSEI              =   1 )
                   AND   ( FLG-KYUSEI              =   1 )
                       MOVE    80                      TO  IDZZ
                   END-IF
               END-PERFORM    
      *
               IF      FLG-MANSEI              =   1
                   IF      FLG-KYUSEI              =   1
                       CONTINUE
                   ELSE
                       MOVE    1               TO  SPA-C02-KEIKOKU-FLG3
                       MOVE    WRK-PTBYO-BYOMEI (1)
                                               TO  WRK-ERR-BYOMEI
                       IF      SPA-CIDCD       =   SPACE
                           MOVE    "0104"          TO  SPA-CIDCD
                       END-IF 
                   END-IF
               END-IF
           END-IF
      *
           IF      WRK-ERR-BYOMEI      NOT =   SPACE
               MOVE    WRK-ERR-BYOMEI      TO  SPA-C02-KEIKOKU-BYOMEI
               MOVE    WRK-ERR-SRYKA       TO  SPA-C02-KEIKOKU-SRYKA
               MOVE    WRK-ERR-SRYYMD      TO  SPA-C02-KEIKOKU-SRYYMD
               MOVE    SPACE               TO  WRK-ERR-BYOMEI
                                               WRK-ERR-SRYKA
                                               WRK-ERR-SRYYMD
           END-IF
      *
      *    主病名の存在チェック（警告）
           MOVE    ZERO                TO  FLG-SYUBYO
      *
           IF    ( SPA-C02-2005-SYUBYOHENFLG
                                       NOT =   ZERO )
           AND   ( SPA-C02-1042-MSG1-DSPKBN
                                           =   ZERO )
               IF    ( SPA-GMN-C02-SYUBYOFLG   =   "1"                )
               AND   ( SPA-NAI-C02-TENKIYMD    >=  SPA-NAI-C02-SYSYMD
                           OR                                    
                       SPA-NAI-C02-TENKIYMD    =   SPACE              )
                   CONTINUE
               ELSE
                   INITIALIZE                  PTBYOMEI-REC
grpsys             MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
                   MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
                   MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
                   MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
                   IF      SPA-C02-1001-HOSPSBT1   =   "1"
                       MOVE    "tbl_ptbyomei"  TO  WRK-MCP-TABLE
                       MOVE    "key14"         TO  WRK-MCP-PATHNAME
                   ELSE
                       MOVE    "tbl_ptbyomei"  TO  WRK-MCP-TABLE
                       MOVE    "key15"         TO  WRK-MCP-PATHNAME
                   END-IF
      *
                   MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
                   MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys             PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                       MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                       PERFORM 900-PTBYO-READ-SEC
                   ELSE
                       MOVE    1                   TO  FLG-PTBYOMEI
                   END-IF
                   PERFORM UNTIL   FLG-PTBYOMEI    =   1
                           OR      FLG-SYUBYO      =   1
      *                選択中の病名は対象外
                       IF    ( SPA-NAI-C02-RENNUM      >  ZERO         )
                       AND   ( SPA-NAI-C02-RENNUM      =  PTBYO-RENNUM )
                       AND   ( SPA-NAI-C02-SRYKA-OLD   =  PTBYO-SRYKA  )
                       AND   ( SPA-NAI-C02-SRYYMD-OLD  =  PTBYO-SRYYMD )
                           CONTINUE
                       ELSE
                           IF      PTBYO-TENKIYMD      =   SPACE
                               MOVE    1               TO  FLG-SYUBYO
                           ELSE    
                               IF      PTBYO-TENKIYMD
                                               <   SPA-NAI-C02-SYSYMD
                                   CONTINUE
                               ELSE
                                   MOVE    1           TO  FLG-SYUBYO
                               END-IF
                           END-IF
                       END-IF
                       MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                       MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                       PERFORM 900-PTBYO-READ-SEC
                   END-PERFORM
                   MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
      *
                   IF      FLG-SYUBYO          =   ZERO
                       MOVE    1               TO  SPA-C02-KEIKOKU-FLG2
                       IF      SPA-CIDCD           =   SPACE
                           MOVE    "0102"            TO  SPA-CIDCD
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       45011-KEIKOKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名編集処理
      *****************************************************************
       4501-BYOMEICD-HENSYU-SEC                SECTION.
      * 
      *    入力コード
           IF      IDX             <   7
               IF      SPA-GMN-C02-BYOMEICD (IDX)
                                   NOT =   SPACE    
                   ADD     1               TO  IDY
                   MOVE    SPA-GMN-C02-BYOMEICD (IDX)
                                   TO
                                   WRK-PTBYO-BYOMEIINPUTCD (IDXX IDY)
               END-IF
           END-IF
      *
           IF      SPA-NAI-C02-BYOMEICD (IDX)
                                       =   SPACE
               GO  TO  4501-BYOMEICD-HENSYU-EXT
           END-IF
      *
      *    病名コード・病名コード数
           IF      WRK-PTBYO-BYOMEICDSU (IDXX)     <=  21
               PERFORM VARYING IDZ     FROM    1   BY  1
                       UNTIL   IDZ     >       21
                       OR      SPA-NAI-C02-BYOMEICD-O  (IDX IDZ)
                                       =   SPACE
                   ADD     1           TO  WRK-PTBYO-BYOMEICDSU (IDXX)
                   IF      WRK-PTBYO-BYOMEICDSU (IDXX)
                                           <=   21
                       IF      SPA-NAI-C02-BYOMEICD-O (IDX IDZ)
                                                =   WRK-CONS-BYOMEICD
                           MOVE    22  TO  WRK-PTBYO-BYOMEICDSU (IDXX)
                       ELSE
                           MOVE    WRK-PTBYO-BYOMEICDSU (IDXX)
                                       TO  IDX1 
                           MOVE    SPA-NAI-C02-BYOMEICD-O (IDX IDZ)
                                        TO  WRK-PTBYO-BYOMEICD 
                                                        (IDXX IDX1)
                       END-IF
                   END-IF
      *
                   IF      WRK-PTBYO-BYOMEICDSU (IDXX)
                                       >   21
                       MOVE    21                  TO  IDZ
                   END-IF  
               END-PERFORM
           END-IF
      *
      *    廃止年月日（レセ電フラグチェック用）
           IF      WRK-PTBYO-BYOMEICDSU (IDXX)
                                       <=  21
               IF      SPA-NAI-C02-HAISIYMD (IDX)
                                           =   ZERO   OR  ALL "9"
                   CONTINUE
               ELSE
                   MOVE    SPA-NAI-C02-HAISIYMD (IDX)
                                           TO  WRK-HAISIYMD-O (IDXX)
               END-IF         
           END-IF  
      *
           IF      SPA-NAI-C02-BYOMEICD-MAX    >   1    
               PERFORM 45011-BYOMEI-HENSYU-SEC
           END-IF
           .
       4501-BYOMEICD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数病名のときマスタより項目編集処理
      *****************************************************************
       45011-BYOMEI-HENSYU-SEC      SECTION.
      *
      *    病名    
           STRING  WRK-PTBYO-BYOMEI  (IDXX)
                                           DELIMITED  BY  SPACE
                   SPA-NAI-C02-BYOMEI(IDX) DELIMITED  BY  SPACE
                                   INTO    WRK-PTBYO-BYOMEI (IDXX)
           END-STRING
      *
      *    カルテ病名
           STRING  WRK-PTBYO-CHARTBYOMEI   (IDXX)
                                           DELIMITED  BY  SPACE
                   SPA-NAI-C02-CHARTBYOMEI (IDX)
                                          DELIMITED  BY  SPACE
                                   INTO   WRK-PTBYO-CHARTBYOMEI (IDXX)
           END-STRING
      *
      *    補足コメント
           MOVE    SPA-NAI-C02-US-HOSOKU-COMMENT (IDX)
                                   TO  WRK-PTBYO-HOSOKU-COMMENT (IDXX)
      *
      *    疑いフラグ
           IF      WRK-PTBYO-UTAGAIFLG (IDXX)  =   "3"
               CONTINUE
           ELSE
               EVALUATE    SPA-NAI-C02-UTAGAIFLG   (IDX)
                   WHEN    "1"
                       IF      WRK-PTBYO-UTAGAIFLG (IDXX)  =   "2"
                           MOVE    "3" TO  WRK-PTBYO-UTAGAIFLG (IDXX)
                       ELSE
                           MOVE    SPA-NAI-C02-UTAGAIFLG(IDX)
                                       TO  WRK-PTBYO-UTAGAIFLG (IDXX) 
                       END-IF    
                   WHEN    "2"
                       IF      WRK-PTBYO-UTAGAIFLG (IDXX)  =   "1"
                           MOVE    "3" TO  WRK-PTBYO-UTAGAIFLG (IDXX)
                        ELSE
                           MOVE    SPA-NAI-C02-UTAGAIFLG(IDX)
                                       TO  WRK-PTBYO-UTAGAIFLG (IDXX)
                       END-IF
               END-EVALUATE
           END-IF
      *
      *    慢性区分
           IF    ( SPA-NAI-C02-NANBYOCD-O(IDX) NOT =   ZERO    OR
                   SPA-NAI-C02-US-TOKSKNCD(IDX)
                                               NOT =   ZERO    OR
                   SPA-NAI-C02-TOKSKNCD  (IDX) NOT =   ZERO       )
           AND   ( WRK-PTBYO-MANSEIKBN  (IDXX)     =   ZERO       )  
               IF      SPA-NAI-C02-US-TOKSKNCD (IDX)
                                                   NOT =   ZERO
                   MOVE    SPA-NAI-C02-US-TOKSKNCD (IDX)
                                       TO  WRK-PTBYO-MANSEIKBN (IDXX)
               ELSE
                   IF      SPA-NAI-C02-NANBYOCD-O (IDX)
                                                       NOT =   ZERO
                       MOVE    SPA-NAI-C02-NANBYOCD-O (IDX)
                                       TO  WRK-PTBYO-MANSEIKBN (IDXX)
                   ELSE
                       MOVE    SPA-NAI-C02-TOKSKNCD (IDX)
                                       TO  WRK-PTBYO-MANSEIKBN (IDXX)
                   END-IF 
               END-IF 
           END-IF
      *    
      *    病名編集フラグ
           IF    ( SPA-NAI-C02-US-BYOMEIHENFLG (IDX) 
                                                   =   "1"   )
           AND   ( WRK-PTBYO-BYOMEIHENFLG (IDXX)   =   SPACE )  
               MOVE    "1"             TO  WRK-PTBYO-BYOMEIHENFLG(IDXX)
           END-IF
      *
      *    保険病名フラグ  
           IF    ( SPA-NAI-C02-US-HKNBYOMEIFLG (IDX) 
                                                   =   "1"   )
           AND   ( WRK-PTBYO-BYOMEIHENFLG (IDXX)   =   SPACE )  
               MOVE    "1"             TO  WRK-PTBYO-HKNBYOMEIFLG(IDXX)
           END-IF
      *
      *    保険組み合わせ
           IF      SPA-GMN-C02-HKNCOMBI    =   SPACE
               MOVE    ZERO            TO  WRK-PTBYO-HKNCOMBI  (IDXX)
           ELSE                          
               MOVE    SPA-GMN-C02-HKNCOMBI
                                       TO  WRK-PTBYO-HKNCOMBI  (IDXX)
           END-IF                     
      *
      *    第三者行為
               MOVE    SPA-NAI-C02-KOHID
                                       TO  WRK-PTBYO-KOHID     (IDXX)
           .                   
       45011-BYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名編集処理（単独病名・共通項目）
      *****************************************************************
       4503-BYOMEICD-HENSYU-02-SEC     SECTION.
      *
      *    単独病名のとき画面より編集
           IF      SPA-NAI-C02-BYOMEICD-MAX    =   1
               MOVE    SPA-GMN-C02-UTAGAIFLG
                                       TO  WRK-PTBYO-UTAGAIFLG (IDXX)
               MOVE    SPA-GMN-C02-SYUBYOFLG
                                       TO  WRK-PTBYO-SYUBYOFLG (IDXX)
               IF      SPA-GMN-C02-MANSEIKBN   =   SPACE
                   MOVE    ZERO        TO  WRK-PTBYO-MANSEIKBN (IDXX)
               ELSE                            
                   MOVE    SPA-GMN-C02-MANSEIKBN
                                       TO  WRK-PTBYO-MANSEIKBN (IDXX)
               END-IF                     
               MOVE    SPA-GMN-C02-NYUGAIKBN
                                       TO  WRK-PTBYO-NYUGAIKBN (IDXX)
               IF      SPA-GMN-C02-HKNCOMBI    =   SPACE
                   MOVE    ZERO        TO  WRK-PTBYO-HKNCOMBI  (IDXX)
               ELSE
                   MOVE    SPA-GMN-C02-HKNCOMBI
                                       TO  WRK-PTBYO-HKNCOMBI  (IDXX)
               END-IF
               MOVE    SPA-NAI-C02-KOHID
                                       TO  WRK-PTBYO-KOHID     (IDXX)
               MOVE    SPA-GMN-C02-BYOMEI
                                       TO  WRK-PTBYO-BYOMEI    (IDXX)
               MOVE    SPA-NAI-C02-BYOMEIMOJI
                                       TO  WRK-PTBYO-BYOMEIMOJI (IDXX)
               MOVE    SPA-GMN-C02-CHARTBYOMEI
                                       TO  WRK-PTBYO-CHARTBYOMEI (IDXX)
               MOVE    SPA-NAI-C02-CHARTBYOMEIMOJI
                                       TO
                                       WRK-PTBYO-CHARTBYOMEIMOJI (IDXX)
               MOVE    SPA-NAI-C02-BYOMEIHENFLG
                                       TO  WRK-PTBYO-BYOMEIHENFLG(IDXX)
               MOVE    SPA-GMN-C02-HKNBYOMEIFLG
                                       TO  WRK-PTBYO-HKNBYOMEIFLG(IDXX)
               MOVE    SPA-GMN-C02-REZEFLG
                                       TO  WRK-PTBYO-REZEFLG   (IDXX)
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       3
                   MOVE    SPA-GMN-C02-HOSOKUCD (IDY)
                                       TO  WRK-PTBYO-HOSOKUCD (IDXX IDY)
               END-PERFORM
               MOVE    SPA-GMN-C02-HOSOKU-COMMENT
                                       TO  WRK-PTBYO-HOSOKU-COMMENT
                                                               (IDXX)
               MOVE    SPA-GMN-C02-REZEFLG
                                       TO  WRK-PTBYO-REZEFLG   (IDXX)
               MOVE    SPA-GMN-C02-DISCHARGEFLG
                                       TO  WRK-PTBYO-DISCHARGEFLG (IDXX)
               IF      SPA-GMN-C02-CLASS1  =   SPACE
                   MOVE    ZERO        TO  WRK-PTBYO-CLASS1 (IDXX)
               ELSE                            
                   MOVE    SPA-GMN-C02-CLASS1
                                       TO  WRK-PTBYO-CLASS1 (IDXX)
               END-IF
               IF      SPA-GMN-C02-CLASS2  =   SPACE
                   MOVE    ZERO        TO  WRK-PTBYO-CLASS2 (IDXX)
               ELSE                            
                   MOVE    SPA-GMN-C02-CLASS2
                                       TO  WRK-PTBYO-CLASS2 (IDXX)
               END-IF
      *********MOVE    SPA-GMN-C02-HKNNUM
      *********                        TO  WRK-PTBYO-HKNNUM (IDXX)
           ELSE
      *        病名文字数
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    WRK-PTBYO-BYOMEI (IDXX)
                                           TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-SYUBETU     =   "2"  )
                   COMPUTE WRK-PTBYO-BYOMEIMOJI (IDXX)
                                       =   KANACHK-MAX  /   2        
               END-IF
      *        カルテ病名文字数
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    WRK-PTBYO-CHARTBYOMEI (IDXX)
                                           TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-SYUBETU     =   "2"  )
                   COMPUTE WRK-PTBYO-CHARTBYOMEIMOJI (IDXX)
                                       =   KANACHK-MAX  /   2        
               END-IF
           END-IF
      * 
      *    病名コードが２２以上のとき未コード化傷病名コードとする
           IF      WRK-PTBYO-BYOMEICDSU (IDXX) >   21
               INITIALIZE              WRK-PTBYO-BYOMEICD-G     (IDXX)
               MOVE    WRK-CONS-BYOMEICD
                                       TO  WRK-PTBYO-BYOMEICD  (IDXX 1)
               MOVE    1               TO  WRK-PTBYO-BYOMEICDSU(IDXX)
               MOVE    "1"             TO  WRK-PTBYO-BYOMEIHENFLG(IDXX)
           END-IF
      *      
      *    基本病名コード・基本部位コード
           MOVE    ZERO                TO  IDZ
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       21
                   OR      WRK-PTBYO-BYOMEICD (IDXX IDY)
                                       =   SPACE
               IF      WRK-PTBYO-BYOMEICD (IDXX IDY)(1:1)
                                               NOT =   "Z"
                   IF      WRK-PTBYO-KHNBYOMEICD (IDXX)    =   SPACE
                       MOVE    WRK-PTBYO-BYOMEICD (IDXX IDY)
                                       TO  WRK-PTBYO-KHNBYOMEICD (IDXX)
                   END-IF
               ELSE
                   IF      WRK-PTBYO-BYOMEICD (IDXX IDY) (1:4)
                                                   =   "ZZZ1"
                       ADD     1       TO  IDZ
                       IF      IDZ         <   4
                           MOVE    WRK-PTBYO-BYOMEICD (IDXX IDY)
                                       TO  WRK-PTBYO-KHNBUICD(IDXX IDZ)
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( WRK-PTBYO-KHNBYOMEICD(IDXX) NOT =   SPACE )
               AND   ( WRK-PTBYO-KHNBUICD (IDXX 3) NOT =   SPACE )
                   MOVE    21                  TO  IDY
               END-IF
           END-PERFORM
      *
      *    転帰区分・転帰日
           MOVE    SPA-GMN-C02-TENKIKBN
                                       TO  WRK-PTBYO-TENKIKBN (IDXX)
           MOVE    SPA-NAI-C02-TENKIYMD
                                       TO  WRK-PTBYO-TENKIYMD (IDXX)
      *
      *    レセ電エラーフラグ
           INITIALIZE                      WRK-PTBYOMEI-CHK-AREA
           MOVE    WRK-HAISIYMD-O   (IDXX)
                                       TO  WRK-HAISIYMD-CHK
           MOVE    WRK-PTBYOMEI-REC (IDXX)
                                       TO  WRK-PTBYOMEI-CHK-REC
           MOVE    SPA-NAI-C02-SRYYMD  TO  WRK-PTBYO-CHK-SRYYMD 
           PERFORM 5001-RECEDENFLG-HENSYU-SEC
           MOVE    WRK-PTBYO-CHK-RECEDENFLG
                                       TO  WRK-PTBYO-RECEDENFLG (IDXX)
      *    レセプト表示期間
           MOVE    SPA-GMN-C02-REZEMM
                                       TO  WRK-PTBYO-REZEMM    (IDXX)
      *
      *****MOVE    SPA-GMN-C02-HKNNUM  TO  WRK-PTBYO-HKNNUM (IDXX)
           .
       4503-BYOMEICD-HENSYU-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力病名重複チェック処理
      *****************************************************************
       4502-DABURI-CHK-SEC        SECTION.
      *
      *    修飾語コードのみの病名のとき
           IF      WRK-PTBYO-BYOMEIHENFLG (IDXX)
                                       =   SPACE
               INITIALIZE                  WRK-BYOMEICD
               PERFORM VARYING IDZ FROM    1   BY  1
                       UNTIL   IDZ >       21
                       OR      WRK-PTBYO-BYOMEICD  (IDXX IDZ)
                                           =   SPACE
                   IF      WRK-PTBYO-BYOMEICD  (IDXX IDZ)(1:1)
                                           NOT =   "Z"
                       MOVE    WRK-PTBYO-BYOMEICD  (IDXX IDZ)
                                               TO  WRK-BYOMEICD
                   END-IF
               END-PERFORM
      *
               IF      WRK-BYOMEICD        =   SPACE
                   MOVE    "0067"              TO  SPA-ERRCD  
                   GO  TO  4502-DABURI-CHK-EXT
               END-IF
           END-IF    
      *
      *    入力病名との重複チェック（複数病名のとき）
           IF      SPA-NAI-C02-BYOMEICD-MAX    >   1
               PERFORM VARYING IDY    FROM    IDXX    BY  1
                       UNTIL   IDY     >       6
                       OR      WRK-PTBYO-BYOMEI (IDY)  =   SPACE
                   IF      IDY             >   IDXX
                       IF      WRK-PTBYO-BYOMEI (IDXX)
                                           =   WRK-PTBYO-BYOMEI (IDY)
                           MOVE   "0055"           TO  SPA-ERRCD
                           MOVE    6               TO  IDY
                       END-IF
                   END-IF
               END-PERFORM
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO  4502-DABURI-CHK-EXT
               END-IF
           END-IF                   
      *
      *    マスタとの重複チェック
           INITIALIZE                      PTBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
           MOVE    WRK-PTBYO-BYOMEI (IDXX)
                                       TO  PTBYO-BYOMEI
           MOVE    WRK-PTBYO-HOSOKU-COMMENT (IDXX)
                                       TO  PTBYO-HOSOKU-COMMENT
           MOVE    WRK-PTBYO-NYUGAIKBN (IDXX)
                                       TO  PTBYO-NYUGAIKBN
           IF      PTBYO-NYUGAIKBN     =   SPACE
               MOVE    "tbl_ptbyomei"          TO  WRK-MCP-TABLE
               MOVE    "key40"                 TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "tbl_ptbyomei"          TO  WRK-MCP-TABLE
               MOVE    "key41"                 TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM                 UNTIL   FLG-PTBYOMEI    =   1
                                   OR      SPA-ERRCD   NOT =   SPACE
               IF      SPA-NAI-C02-SRYYMD-OLD  =   PTBYO-SRYYMD
               AND     SPA-NAI-C02-SRYKA-OLD   =   PTBYO-SRYKA
               AND     SPA-NAI-C02-RENNUM      =   PTBYO-RENNUM
                   CONTINUE
               ELSE
                   EVALUATE    TRUE
                       WHEN    SPA-NAI-C02-SRYYMD  =   PTBYO-SRYYMD
                           MOVE    "0002"              TO  SPA-ERRCD
                       WHEN    SPA-NAI-C02-SRYYMD  <   PTBYO-SRYYMD
                           IF      SPA-NAI-C02-TENKIYMD    =   SPACE
                               MOVE    "0038"          TO  SPA-ERRCD
                           ELSE
                               IF     SPA-NAI-C02-TENKIYMD
                                                   >   PTBYO-SRYYMD
                                   MOVE    "0038"          TO  SPA-ERRCD
                               END-IF
                           END-IF
                       WHEN    SPA-NAI-C02-SRYYMD  >   PTBYO-SRYYMD
                           IF      PTBYO-TENKIYMD  =   SPACE
                               MOVE    "0038"      TO  SPA-ERRCD
                           ELSE
                               IF      SPA-NAI-C02-SRYYMD
                                                   <   PTBYO-TENKIYMD
                                   MOVE    "0038"      TO  SPA-ERRCD
                               END-IF
                           END-IF
                   END-EVALUATE
      *
                   IF      SPA-ERRCD               =   "0038"  OR
                                                       "0002"
      *                同一病名における保険限定の重複チェック処理
                       INITIALIZE                  WRK-CHK-HKNCOMBI-AREA
      *
                       MOVE    SPA-GMN-C02-HKNCOMBI
                                               TO  WRK-CHK-HKNCOMBI1
                       MOVE    PTBYO-HKNCOMBI  TO  WRK-CHK-HKNCOMBI2
                       MOVE    SPA-NAI-C02-KOHID
                                               TO  WRK-CHK-KOHID1
                       MOVE    PTBYO-KOHID     TO  WRK-CHK-KOHID2
                       PERFORM 45021-HKNCOMBI-DABURI-CHK-SEC
                       IF      FLG-HKNCOMBI-DABURI =   1
                           MOVE   SPACE                TO  SPA-ERRCD
                       ELSE 
                           MOVE    PTBYO-SRYYMD        TO  WRK-SYMD
                           PERFORM 5002-HIZUKE-HEN-SEC
                           MOVE    LNK-DAY2-EDTYMD3    TO
                                                       WRK-ERR-SRYYMD
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .                   
       4502-DABURI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    同一病名における保険限定の重複チェック処理
      *****************************************************************
       45021-HKNCOMBI-DABURI-CHK-SEC   SECTION.
      *
      *    医保以外の保険がない場合はチェックしない
           IF    ( SPA-NAI-C02-HKNCOMBI-ROUSAI
                                       =   ZERO )
           AND   ( SPA-NAI-C02-HKNCOMBI-JIHI
                                       =   ZERO )
           AND   ( SPA-NAI-C02-HKNCOMBI-KOUGAI
                                       =   ZERO )
           AND   ( SPA-NAI-C02-KOHID-DAISANSYA
                                       =   ZERO )
               GO  TO   45021-HKNCOMBI-DABURI-CHK-EXT
           END-IF
      *
      *    保険限定の設定がない場合はチェックしない
           IF    ( WRK-CHK-HKNCOMBI1   =   ZERO )
           AND   ( WRK-CHK-HKNCOMBI2   =   ZERO )
           AND   ( WRK-CHK-KOHID1      =   ZERO )
           AND   ( WRK-CHK-KOHID2      =   ZERO )
               GO  TO   45021-HKNCOMBI-DABURI-CHK-EXT
           END-IF
      *
      *    保険組合せの重複チェック
           IF    ( WRK-CHK-HKNCOMBI1   =   ZERO )
           AND   ( WRK-CHK-HKNCOMBI2   =   ZERO )
             CONTINUE
           ELSE
             IF      WRK-CHK-HKNCOMBI1
                                   NOT =   ZERO
grpsys         MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
               MOVE    SPA-NAI-C02-PTID    TO  COMB-PTID
               MOVE    WRK-CHK-HKNCOMBI1   TO  COMB-HKNCOMBINUM
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               PERFORM 900-COMB-INV-SEC
               MOVE    HKNCOMBI-REC        TO  WRK-CHK-HKNCOMBI-REC (1)
             END-IF    
             IF      WRK-CHK-HKNCOMBI2
                                   NOT =   ZERO
grpsys         MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
               MOVE    SPA-NAI-C02-PTID    TO  COMB-PTID
               MOVE    WRK-CHK-HKNCOMBI2   TO  COMB-HKNCOMBINUM
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               PERFORM 900-COMB-INV-SEC
               MOVE    HKNCOMBI-REC        TO  WRK-CHK-HKNCOMBI-REC (2)
             END-IF
      *
      *      片方のみ労災または自賠責または公害の場合は登録可能とする
             EVALUATE    WRK-CHK-COMB-HKNNUM (1)
                                   ALSO    WRK-CHK-COMB-HKNNUM (2)
               WHEN    "971"       ALSO    "971"
               WHEN    "973"       ALSO    "973"
               WHEN    "975"       ALSO    "975"
                   IF      WRK-CHK-COMB-HKNID (1)
                                       NOT =    WRK-CHK-COMB-HKNID (2)
                       MOVE    1                TO  FLG-HKNCOMBI-DABURI
                   END-IF
               WHEN    "971"       ALSO    ANY
               WHEN    "973"       ALSO    ANY
               WHEN    "975"       ALSO    ANY
               WHEN    ANY         ALSO    "971"
               WHEN    ANY         ALSO    "973"
               WHEN    ANY         ALSO    "975"
                   MOVE    1               TO  FLG-HKNCOMBI-DABURI
               WHEN    OTHER
                   MOVE    ZERO            TO  FLG-HKNCOMBI-DABURI
             END-EVALUATE
      *
      *      自費と保険限定なしまたは同じ自費の場合は登録不可とする
             IF    ( WRK-CHK-COMB-HKNNUM (1) (1:2)
                                   =   "98" ) 
             OR    ( WRK-CHK-COMB-HKNNUM (2) (1:2)
                                   =   "98" ) 
               EVALUATE    WRK-CHK-COMB-HKNNUM (1) (1:2)
                                   ALSO    WRK-CHK-COMB-HKNNUM (2) (1:2)
                   WHEN    "98"       ALSO    "98"
                       IF      WRK-CHK-COMB-HKNID (1)
                                       NOT =    WRK-CHK-COMB-HKNID (2)
                           MOVE    1            TO  FLG-HKNCOMBI-DABURI
                       END-IF
                   WHEN    "98"       ALSO    SPACE
                   WHEN    SPACE      ALSO    "98"
                       MOVE    ZERO            TO  FLG-HKNCOMBI-DABURI
                   WHEN    OTHER
                       MOVE    1               TO  FLG-HKNCOMBI-DABURI
               END-EVALUATE
             END-IF
      *
      *      保険組合せで登録不可のとき
             IF      FLG-HKNCOMBI-DABURI =   ZERO
               GO  TO   45021-HKNCOMBI-DABURI-CHK-EXT
             END-IF
           END-IF
      *
      *    第三者行為の設定がない場合はチェックしない
           IF    ( WRK-CHK-KOHID1   =   ZERO )
           AND   ( WRK-CHK-KOHID2   =   ZERO )
               GO  TO   45021-HKNCOMBI-DABURI-CHK-EXT
           END-IF
      *
      *    第三者行為の設定が両方の場合
           IF    ( WRK-CHK-KOHID1
                               NOT =   ZERO )
           AND   ( WRK-CHK-KOHID2
                               NOT =   ZERO )
      *        同じ第三者行為の場合は登録不可とする
               IF      WRK-CHK-KOHID1  =   WRK-CHK-KOHID2
                   MOVE    ZERO            TO  FLG-HKNCOMBI-DABURI
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI-DABURI
               END-IF
               GO  TO  45021-HKNCOMBI-DABURI-CHK-EXT
           ELSE
      *        片方が医保以外のときチェックしない
               IF    ( WRK-CHK-KOHID1
                                   NOT =   ZERO             )
               AND   ( WRK-CHK-COMB-HKNNUM (2) (1:2)
                                       =   "98"    OR  "97" )
                   GO  TO  45021-HKNCOMBI-DABURI-CHK-EXT
               END-IF
               IF    ( WRK-CHK-KOHID2
                                   NOT =   ZERO             )
               AND   ( WRK-CHK-COMB-HKNNUM (1) (1:2)
                                       =   "98"    OR  "97" )
                   GO  TO  45021-HKNCOMBI-DABURI-CHK-EXT
               END-IF
           END-IF
      *
      *    医保と第三者行為のため登録不可とする
           MOVE    ZERO            TO  FLG-HKNCOMBI-DABURI
      *
           .                   
       45021-HKNCOMBI-DABURI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       4502-TOUROKU-SYORI-SEC             SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       6
                   OR      WRK-PTBYO-BYOMEI (IDXX) =   SPACE
                   OR      SPA-ERRCD           NOT =   SPACE
               PERFORM 45022-PTBYOMEI-UPDATE-SEC
           END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
              GO  TO  4502-TOUROKU-SYORI-EXT
           END-IF
      *
      *    患者マスタ更新（死亡区分）
           IF      SPA-C02-KEIKOKU-FLG4    =   0
               CONTINUE
           ELSE
               INITIALIZE                      PTINF-REC
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    SPA-NAI-C02-PTID    TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
      *            転帰区分が２で死亡区分が生存のとき
                   IF      SPA-C02-KEIKOKU-FLG4    =   1
                       MOVE    "1"                 TO  PTINF-DEATHKBN 
                       MOVE    SMCNDATE-YMD        TO  PTINF-UPYMD
                       MOVE    SMCNDATE-HMS        TO  PTINF-UPHMS
                       MOVE    PTINF-REC           TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_ptinf"         TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM 900-ORCDBMAIN-SEC
                       IF      MCP-RC          NOT =   ZERO 
                           MOVE    "1004"              TO  SPA-ERRCD
                           DISPLAY "UPD PTINF-KEY =>" PTINF-KEY 
                           GO  TO  4502-TOUROKU-SYORI-EXT
                       END-IF
      *???
                       DISPLAY "PTINF=" PTINF-DEATHKBN "#"
      *???
                   ELSE
      *                修正で転帰区分を２以外に変更したときに死亡区分が死亡のとき
                       IF      SPA-C02-KEIKOKU-FLG4    =   9
                           MOVE    SPACE               TO
                                                       PTINF-DEATHKBN 
                           MOVE    SMCNDATE-YMD        TO  PTINF-UPYMD
                           MOVE    SMCNDATE-HMS        TO  PTINF-UPHMS
                           MOVE    PTINF-REC           TO  MCPDATA-REC
                           MOVE    "DBUPDATE"          TO  MCP-FUNC
                           MOVE    "tbl_ptinf"         TO  MCP-TABLE
                           MOVE    "key"               TO  MCP-PATHNAME
                           PERFORM 900-ORCDBMAIN-SEC
                           IF      MCP-RC          NOT =   ZERO 
                               MOVE    "1004"              TO  SPA-ERRCD
                               DISPLAY "UPD PTINF-KEY =>" PTINF-KEY 
                               GO  TO  4502-TOUROKU-SYORI-EXT
                           END-IF
      *???
                           DISPLAY "PTINF=" PTINF-DEATHKBN "#"
      *???                
                       END-IF 
                   END-IF
               END-IF
           END-IF     
      *
           IF      SPA-NAI-C02-UPDOWN       =   1
      *        移動処理分登録処理
               PERFORM 4509-UPDOWN-KOUSIN-SEC
           END-IF
      *
           IF      SPA-NAI-C02-SELNO   >   ZERO
               MOVE    SPA-NAI-C02-SELNO   TO  C02-BYO-ROW
           ELSE
               MOVE    ZERO                TO  C02-BYO-ROW
           END-IF
      *                   
           MOVE    SPACE               TO  WRK-HAISIYMDH
           MOVE    ZERO                TO  SPA-C02-IKOFLG
           INITIALIZE                      SPA-GMN-C02-KOUMOKU
                                           SPA-NAI-C02-KOUMOKU
                                           SPA-NAI-C02-SELNO
           MOVE    1                   TO  SPA-GMN-C02-CUR
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
      *
           INITIALIZE                  SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-NAI-C02-SELNO
                                       SPA-PTBYOMEI-REC
      *
           INITIALIZE                  WRK-PTBYOMEI-REC (1)
                                       WRK-PTBYOMEI-REC (2)
                                       WRK-PTBYOMEI-REC (3)
                                       WRK-PTBYOMEI-REC (4)
                                       WRK-PTBYOMEI-REC (5)
                                       WRK-PTBYOMEI-REC (6)
                                       WRK-HAISIYMD-OCC
      *
           IF    ( SPA-GMN-C02-SRYKA   NOT =   ZERO  )
           AND   ( SPA-GMN-C02-SRYKAMEI
                                       NOT =   SPACE )
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
      *    検索処理
           PERFORM 310-SPASET-PTBYO-HENSYU-SEC
      *
           IF      C02-BYO-ROW         =   ZERO
               MOVE    SPA-GMN-C02-MAX     TO  C02-BYO-ROW
           END-IF
           MOVE    ZERO                TO  FLG-ROW
           .
       4502-TOUROKU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名別入力項目編集処理
      *****************************************************************
       45022-PTBYOMEI-UPDATE-SEC                SECTION.
      *
      *    更新処理
           EVALUATE   SPA-NAI-C02-RENNUM
               WHEN    ZERO
      *            患者病名マスター追加
                   PERFORM 450222-PTBYOMEI-INSERT-SEC
                   GO  TO  45022-PTBYOMEI-UPDATE-EXT
               WHEN    OTHER
                   IF    ( SPA-GMN-C02-BSRYKA  NOT =
                                       SPA-NAI-C02-SRYKA-OLD  )
                   OR    ( SPA-NAI-C02-SRYYMD  NOT =
                                       SPA-NAI-C02-SRYYMD-OLD )
      *                患者病名マスター更新（診療科変更のとき）
                       INITIALIZE                      PTBYO-KEY
grpsys                 MOVE    SPA-HOSPNUM             TO  PTBYO-HOSPNUM
                       MOVE    SPA-NAI-C02-PTID        TO  PTBYO-PTID
                       MOVE    SPA-NAI-C02-SRYKA-OLD   TO  PTBYO-SRYKA
                       MOVE    SPA-NAI-C02-SRYYMD-OLD  TO  PTBYO-SRYYMD
                       MOVE    SPA-NAI-C02-RENNUM      TO  PTBYO-RENNUM 
                       MOVE    PTBYOMEI-REC            TO  MCPDATA-REC
                       MOVE    "DBDELETE"              TO  MCP-FUNC
                       MOVE    "tbl_ptbyomei"          TO  MCP-TABLE
                       MOVE    "key"                   TO  MCP-PATHNAME
grpsys                 PERFORM 900-ORCDBMAIN-SEC
                       IF      MCP-RC              NOT =   ZERO 
                           MOVE    "1002"                  TO  SPA-ERRCD
                           DISPLAY "PTBYOMEI-KEY =>" PTBYO-KEY 
                           GO  TO  45022-PTBYOMEI-UPDATE-EXT
                       END-IF
      *
      *                患者病名マスター追加
                       PERFORM 450222-PTBYOMEI-INSERT-SEC
                       GO  TO  45022-PTBYOMEI-UPDATE-EXT
                   ELSE
      *                患者病名マスター更新（診療科変更なし）
                       INITIALIZE                  PTBYO-KEY
grpsys                 MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
                       MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
                       MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
                       MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
                       MOVE    SPA-NAI-C02-RENNUM  TO  PTBYO-RENNUM 
                       MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
                       MOVE    "key"               TO  WRK-MCP-PATHNAME
                       PERFORM 900-PTBYO-INV-SEC
                       IF      FLG-PTBYOMEI        =   1
                           MOVE    "1003"          TO  SPA-ERRCD
                           DISPLAY "INV PTBYOMEI-KEY =>" PTBYO-KEY 
                           GO  TO  45022-PTBYOMEI-UPDATE-EXT
                       ELSE
                           MOVE    PTBYO-KEY       TO
                                                   WRK-PTBYO-KEY (IDXX)
                           MOVE    PTBYO-CREYMD    TO
                                                   WRK-PTBYO-CREYMD
                                                                 (IDXX)
                           MOVE    PTBYO-CREHMS    TO
                                                   WRK-PTBYO-CREHMS
                                                                 (IDXX)
                           MOVE    PTBYO-SEQNUM    TO
                                                   WRK-PTBYO-SEQNUM
                                                                 (IDXX)
                           MOVE    WRK-PTBYOMEI-REC (IDXX)
                                                   TO  PTBYOMEI-REC
      *
                           MOVE    SPA-OPID        TO  PTBYO-OPID
                           MOVE    SMCNDATE-YMD    TO  PTBYO-UPYMD
                           MOVE    SMCNDATE-HMS    TO  PTBYO-UPHMS
      *
                           MOVE    "DBUPDATE"      TO  MCP-FUNC
                       END-IF
                   END-IF
           END-EVALUATE
      *
      *???
           DISPLAY "PTBYO=" PTBYO-KEY "#" MCP-FUNC "#"
      *???                
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY MCP-FUNC "PTBYOMEI-KEY =>" PTBYO-KEY
           ELSE
               PERFORM 450221-RECEUPD-UPDATE-SEC
           END-IF
      *
          .
       45022-PTBYOMEI-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加処理
      *****************************************************************
       450222-PTBYOMEI-INSERT-SEC           SECTION.
      *
           INITIALIZE                      WRK-PTBYOMEI-INSERT-AREA
           MOVE    ZERO                TO  IDY1
           MOVE    ZERO                TO  WRK-RENNUM
      *
      *    患者病名マスター追加
           INITIALIZE                  PTBYO-KEY
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
           MOVE    "key7"              TO  WRK-MCP-PATHNAME
           PERFORM 900-PTBYO-INV-SEC
           IF      FLG-PTBYOMEI        =   ZERO
               MOVE    PTBYO-RENNUM    TO  WRK-RENNUM
           END-IF
           IF      WRK-RENNUM          =   99
      *        連番をつけなおして更新
                MOVE    ZERO               TO  WRK-RENNUM
      *
      *        登録済の病名を退避して削除する
               INITIALIZE                      PTBYOMEI-REC
               MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
               MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
               MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
               MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
               MOVE    ZERO                TO  PTBYO-RENNUM
               MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
               MOVE    "key36"             TO  WRK-MCP-PATHNAME
               MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 900-PTBYO-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-PTBYOMEI
               END-IF
      *
               PERFORM         UNTIL   FLG-PTBYOMEI    =   1
                               OR      IDY1            =   99
                   ADD     1               TO  IDY1
                                               WRK-RENNUM
      *
                   MOVE    PTBYOMEI-REC    TO  WRK-PTBYOMEI-INSERT-REC
                                                               (IDY1)
                   MOVE    WRK-RENNUM      TO  WRK-PTBYO-INS-RENNUM
                                                               (IDY1)
                   MOVE    SMCNDATE-YMD    TO  WRK-PTBYO-INS-UPYMD
                                                               (IDY1)
                   MOVE    SMCNDATE-HMS    TO  WRK-PTBYO-INS-UPHMS
                                                               (IDY1)
      *            既に登録済の病名を削除する
                   MOVE    PTBYOMEI-REC    TO  MCPDATA-REC
                   MOVE    "DBDELETE"      TO  MCP-FUNC
                   MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
      *???
                   DISPLAY "PTBYO=" PTBYO-KEY "#" MCP-FUNC "#"
      *???                
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC          NOT =   ZERO 
                       MOVE    "1002"              TO  SPA-ERRCD
                       DISPLAY "PTBYOMEI-KEY =>" PTBYO-KEY 
                       GO  TO  450222-PTBYOMEI-INSERT-EXT
                   END-IF
      *
                   MOVE    WRK-MCP-TABLE   TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME
                                           TO  MCP-PATHNAME
                   PERFORM 900-PTBYO-READ-SEC
               END-PERFORM
               MOVE    WRK-MCP-TABLE   TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME
                                       TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
           END-IF
      *
      *    追加分を追加
           ADD     1                   TO  IDY1
           ADD     1                   TO  WRK-RENNUM
           MOVE    WRK-PTBYOMEI-REC (IDXX)
                                       TO  PTBYOMEI-REC 
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-SRYYMD  TO  PTBYO-SRYYMD
           MOVE    WRK-RENNUM          TO  PTBYO-RENNUM
           MOVE    9999                TO  PTBYO-SEQNUM
           MOVE    SPA-OPID            TO  PTBYO-OPID
           MOVE    SMCNDATE-YMD        TO  PTBYO-CREYMD
           MOVE    SMCNDATE-HMS        TO  PTBYO-CREHMS
           MOVE    SMCNDATE-YMD        TO  PTBYO-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTBYO-UPHMS
           MOVE    PTBYOMEI-REC        TO  WRK-PTBYOMEI-INSERT-REC
                                                               (IDY1)
      *
           PERFORM VARYING IDY1    FROM    1   BY  1
                   UNTIL   IDY1    >       99
                   OR      WRK-PTBYO-INS-RENNUM (IDY1)
                                           =   ZERO
               MOVE    WRK-PTBYOMEI-INSERT-REC (IDY1) 
                                           TO  PTBYOMEI-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
      *???
               DISPLAY "PTBYO=" PTBYO-KEY "#" MCP-FUNC "#"
      *???                
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY MCP-FUNC "PTBYOMEI-KEY =>" PTBYO-KEY
                   GO  TO  450222-PTBYOMEI-INSERT-EXT
               END-IF
          END-PERFORM
      *
          PERFORM 450221-RECEUPD-UPDATE-SEC
          .
       450222-PTBYOMEI-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    退院証明書記載フラグ重複チェック処理
      *****************************************************************
       4508-DISCHARGEFLG-DABURI-CK-SEC   SECTION.
      *
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C02-BSRYKA  TO  PTBYO-SRYKA
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key49"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key49"             TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM         UNTIL   FLG-PTBYOMEI    =   1
                           OR      SPA-ERRCD   NOT =   SPACE
               IF      SPA-NAI-C02-SRYYMD-OLD  =   PTBYO-SRYYMD
               AND     SPA-NAI-C02-SRYKA-OLD   =   PTBYO-SRYKA
               AND     SPA-NAI-C02-RENNUM      =   PTBYO-RENNUM
                   CONTINUE
               ELSE
                   EVALUATE    TRUE
                       WHEN    SPA-NAI-C02-SRYYMD  =   PTBYO-SRYYMD
                           MOVE    "0043"              TO  SPA-ERRCD
                       WHEN    SPA-NAI-C02-SRYYMD  <   PTBYO-SRYYMD
                           IF      SPA-NAI-C02-TENKIYMD    =   SPACE
                               MOVE    "0043"          TO  SPA-ERRCD
                           ELSE
                               IF     SPA-NAI-C02-TENKIYMD
                                                   >   PTBYO-SRYYMD
                                   MOVE    "0043"          TO  SPA-ERRCD
                               END-IF
                           END-IF
                       WHEN    SPA-NAI-C02-SRYYMD  >   PTBYO-SRYYMD
                           IF      PTBYO-TENKIYMD  =   SPACE
                               MOVE    "0043"      TO  SPA-ERRCD
                           ELSE
                               IF      SPA-NAI-C02-SRYYMD
                                                   <   PTBYO-TENKIYMD
                                   MOVE    "0043"      TO  SPA-ERRCD
                               END-IF
                           END-IF
                   END-EVALUATE
      *
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    PTBYO-BYOMEI    TO  WRK-ERR-BYOMEI
                   END-IF 
               END-IF
      *
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key49"             TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key49"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .                   
       4508-DISCHARGEFLG-DABURI-CK-EXT.
           EXIT.
      *
      *****************************************************************
      *    移動処理分登録処理
      *****************************************************************
       4509-UPDOWN-KOUSIN-SEC             SECTION.
      *
           MOVE    1               TO  WRK-ST
      *
           PERFORM         UNTIL   WRK-ST  >=  SPA-GMN-C02-MAX
               MOVE    ZERO            TO  IDY1
                                           IDY2
               MOVE    SPACE           TO  WRK-SRYYMD
               MOVE    ZERO            TO  WRK-RENNUM
      *
      *        移動対象の開始日取得
               PERFORM VARYING IDXX    FROM    WRK-ST  BY  1
                       UNTIL   IDXX    >       SPA-GMN-C02-MAX
                       OR      IDY1    >       ZERO
                   IF      SPA-NAI-C02-TUPDOWN (IDXX)  =   1
                       MOVE    SPA-NAI-C02-TSRYYMD (IDXX)
                                               TO  WRK-SRYYMD
                       MOVE    IDXX            TO  IDY1 
                   END-IF
               END-PERFORM
      *
               IF      IDY1                    =   ZERO
                   MOVE    SPA-GMN-C02-MAX     TO  WRK-ST
               ELSE
      *            移動対象の開始日の開始位置の取得
                   PERFORM VARYING IDXX    FROM    IDY1    BY  -1
                           UNTIL   IDXX    <       1
                           OR      SPA-NAI-C02-TSRYYMD (IDXX)
                                           <   SPA-NAI-C02-TSRYYMD(IDY1)
                       MOVE    IDXX             TO  IDY2
                   END-PERFORM
      *
      *            移動対象の開始日の病名の入力順を設定する
                   PERFORM VARYING IDXX    FROM    IDY2    BY  1
                           UNTIL   IDXX    >       SPA-GMN-C02-MAX
                           OR      SPA-ERRCD    NOT =   SPACE
                       IF      SPA-NAI-C02-TSRYYMD (IDXX)
                                               >   WRK-SRYYMD
                           MOVE    IDXX            TO  WRK-ST
                           MOVE    SPA-GMN-C02-MAX TO  IDXX
                       ELSE 
                           ADD     1                TO  WRK-RENNUM
      *
                           INITIALIZE                      PTBYO-KEY
                           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
                           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
                           MOVE    SPA-NAI-C02-TSRYKA (IDXX)
                                                       TO  PTBYO-SRYKA
                           MOVE    SPA-NAI-C02-TSRYYMD (IDXX)
                                                       TO  PTBYO-SRYYMD
                           MOVE    SPA-NAI-C02-TRENNUM (IDXX)
                                                       TO  PTBYO-RENNUM 
                           MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
                           MOVE    "key"               TO
                                                       WRK-MCP-PATHNAME
                           PERFORM 900-PTBYO-INV-SEC
                           IF      FLG-PTBYOMEI        =   1
                               MOVE    "1003"          TO  SPA-ERRCD
                               DISPLAY "INV PTBYOMEI-KEY =>" PTBYO-KEY 
                           ELSE
                               MOVE    WRK-RENNUM      TO  PTBYO-SEQNUM
                               MOVE    SPA-OPID        TO  PTBYO-OPID
                               MOVE    SMCNDATE-YMD    TO  PTBYO-UPYMD
                               MOVE    SMCNDATE-HMS    TO  PTBYO-UPHMS
      *
                               MOVE    "DBUPDATE"      TO  MCP-FUNC
                               MOVE    PTBYOMEI-REC    TO  MCPDATA-REC
                               MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
                               MOVE    "key"           TO  MCP-PATHNAME
      *???
                             DISPLAY "PTBYO=" PTBYO-KEY "#" PTBYO-SEQNUM
                                    "#" MCP-FUNC "#"
      *???                
                               PERFORM 900-ORCDBMAIN-SEC
                               IF      MCP-RC          NOT =   ZERO 
                                   MOVE    "1001"          TO  SPA-ERRCD
                                   DISPLAY MCP-FUNC "PTBYOMEI-KEY =>"
                                                               PTBYO-KEY
                               ELSE
                                   MOVE    ZERO            TO
                                               SPA-NAI-C02-TUPDOWN(IDXX) 
                               END-IF
                           END-IF
                       END-IF
                   END-PERFORM
      *
                   IF      SPA-ERRCD       NOT =   SPACE
                      GO  TO  4509-UPDOWN-KOUSIN-EXT
                   END-IF
               END-IF
           END-PERFORM
      *
           PERFORM 450221-RECEUPD-UPDATE-SEC
      *
           MOVE    ZERO                TO  SPA-NAI-C02-UPDOWN
           PERFORM VARYING IDYY    FROM    1   BY  1
                   UNTIL   IDYY    >       200
                   OR      IDYY    >      SPA-GMN-C02-MAX
               INITIALIZE                 SPA-NAI-C02-TUPDOWN (IDYY)
           END-PERFORM 
           .
       4509-UPDOWN-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト作成管理マスタ更新処理
      *****************************************************************
       450221-RECEUPD-UPDATE-SEC           SECTION.
      *
           INITIALIZE                      ORCSRECEUPDAREA
           MOVE    "1"                 TO  LNK-UPD-SYORI
grpsys     MOVE    SPA-HOSPNUM         TO  LNK-UPD-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  LNK-UPD-PTID
           MOVE    SPA-TERMID          TO  LNK-UPD-TERMID
           MOVE    SPA-OPID            TO  LNK-UPD-OPID
           CALL    "ORCSRECEUPD"       USING    ORCSRECEUPDAREA
grpsys                                          SPA-AREA
           IF      LNK-UPD-RC      NOT =   ZERO
               MOVE    "1006"              TO  SPA-ERRCD
               DISPLAY "LNK-UPD-RC =>" LNK-UPD-RC
           END-IF
      *
           .
       450221-RECEUPD-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *       病名↑処理
      *****************************************************************
       490-BYOMEI-UP-SEC              SECTION.
      *
           IF      SPA-NAI-C02-SELNO   =   ZERO
               MOVE    "0103"              TO  SPA-ERRCD
           ELSE
               IF    ( SPA-NAI-C02-SELNO   =   1 )
               OR    ( SPA-GMN-C02-MAX     =   1 )
                   GO  TO  490-BYOMEI-UP-EXT
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               COMPUTE IDX =   SPA-NAI-C02-SELNO   -   1
      *        上の病名と開始日が違うとき
               IF      SPA-NAI-C02-TSRYYMD
                              (SPA-NAI-C02-SELNO)
                                       NOT =   SPA-NAI-C02-TSRYYMD (IDX)
                   GO  TO  490-BYOMEI-UP-EXT
               ELSE
      *            開始日が同じときの処理
                   MOVE    1               TO  SPA-NAI-C02-TUPDOWN
                                                   (IDX)
                                               SPA-NAI-C02-TUPDOWN
                                                   (SPA-NAI-C02-SELNO)
                   MOVE    SPA-GMN-C02-TBLO
                               (SPA-NAI-C02-SELNO)
                                           TO  WRK-GMN-C02-TBLO
                   MOVE    SPA-GMN-C02-TBLO (IDX)
                                           TO  SPA-GMN-C02-TBLO
                                                   (SPA-NAI-C02-SELNO)
                   MOVE    WRK-GMN-C02-TBANGO
                                           TO   SPA-GMN-C02-TBANGO
                                                   (SPA-NAI-C02-SELNO)
                   MOVE    SPA-GMN-C02-TBANGO (IDX)
                                           TO   WRK-GMN-C02-TBANGO
                   MOVE    WRK-GMN-C02-TBLO
                                           TO  SPA-GMN-C02-TBLO (IDX)
                   MOVE    SPA-NAI-C02-TBLO
                               (SPA-NAI-C02-SELNO)
                                           TO  WRK-NAI-C02-TBLO
                   MOVE    SPA-NAI-C02-TBLO (IDX)
                                           TO  SPA-NAI-C02-TBLO
                                                   (SPA-NAI-C02-SELNO)
                   MOVE    WRK-NAI-C02-TBLO
                                           TO  SPA-NAI-C02-TBLO (IDX)
                   MOVE    IDX             TO  SPA-NAI-C02-SELNO
                   MOVE    IDX             TO  C02-BYO-ROW
      *
                   IF      SPA-NAI-C02-UPDOWN
                                           =   0
                       MOVE    1               TO  SPA-NAI-C02-UPDOWN
                   END-IF
      *###             INITIALIZE                  SPA-GMN-C02-KOUMOKU
      *###                                         SPA-NAI-C02-KOUMOKU
               END-IF
           END-IF
           .
       490-BYOMEI-UP-EXT.
           EXIT.
      *
      *****************************************************************
      *       病名↓処理
      *****************************************************************
       490-BYOMEI-DOWN-SEC              SECTION.
      *
           IF      SPA-NAI-C02-SELNO   =   ZERO
               MOVE    "0103"              TO  SPA-ERRCD
           ELSE
               IF    ( SPA-NAI-C02-SELNO   =   SPA-GMN-C02-MAX )
               OR    ( SPA-GMN-C02-MAX     =   1               )
                   GO  TO  490-BYOMEI-DOWN-EXT
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               COMPUTE IDX =   SPA-NAI-C02-SELNO   +   1
      *        下の病名と開始日が違うとき
               IF      SPA-NAI-C02-TSRYYMD
                              (SPA-NAI-C02-SELNO)
                                       NOT =   SPA-NAI-C02-TSRYYMD (IDX)
                   GO  TO  490-BYOMEI-DOWN-EXT
               ELSE
      *            開始日が同じときの処理
                   MOVE    1               TO  SPA-NAI-C02-TUPDOWN
                                                   (IDX)
                                               SPA-NAI-C02-TUPDOWN
                                                   (SPA-NAI-C02-SELNO)
                   MOVE    SPA-GMN-C02-TBLO
                               (SPA-NAI-C02-SELNO)
                                           TO  WRK-GMN-C02-TBLO
                   MOVE    SPA-GMN-C02-TBLO (IDX)
                                           TO  SPA-GMN-C02-TBLO
                                                   (SPA-NAI-C02-SELNO)
                   MOVE    WRK-GMN-C02-TBANGO
                                           TO   SPA-GMN-C02-TBANGO
                                                   (SPA-NAI-C02-SELNO)
                   MOVE    SPA-GMN-C02-TBANGO (IDX)
                                           TO   WRK-GMN-C02-TBANGO
                   MOVE    WRK-GMN-C02-TBLO
                                           TO  SPA-GMN-C02-TBLO (IDX)
                   MOVE    SPA-NAI-C02-TBLO
                               (SPA-NAI-C02-SELNO)
                                           TO  WRK-NAI-C02-TBLO
                   MOVE    SPA-NAI-C02-TBLO (IDX)
                                           TO  SPA-NAI-C02-TBLO
                                                   (SPA-NAI-C02-SELNO)
                   MOVE    WRK-NAI-C02-TBLO
                                           TO  SPA-NAI-C02-TBLO (IDX)
                   MOVE    IDX             TO  SPA-NAI-C02-SELNO
                   MOVE    IDX             TO  C02-BYO-ROW
      *
                   IF      SPA-NAI-C02-UPDOWN
                                           =   0
                       MOVE    1               TO  SPA-NAI-C02-UPDOWN
                   END-IF
      *###             INITIALIZE                  SPA-GMN-C02-KOUMOKU
      *###                                         SPA-NAI-C02-KOUMOKU
               END-IF
           END-IF
      *
           .
       490-BYOMEI-DOWN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    病名検索へ
      *****************************************************************
       600-C05-SYORI-SEC           SECTION.
      *
           IF      SPA-NAI-C02-PTID    =   ZERO
           OR      SPA-GMN-C02-SRYKA   =   SPACE
      *****OR      SPA-GMN-C02-CUR     =   27  OR  26
               MOVE    "0050"              TO  SPA-ERRCD
               GO  TO  600-C05-SYORI-EXT
           END-IF
      *     
           IF      SPA-NAI-C02-BYOMEICD-MAX    >   1
               MOVE    "0058"              TO  SPA-ERRCD
               GO  TO  600-C05-SYORI-EXT
           END-IF
      *     
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
               IF    ( SPA-GMN-C02-BYOMEICD (IDX)  NOT =   SPACE )             
               AND   ( SPA-NAI-C02-USERBYOMEIFLG (IDX) =   "1"   )
               AND   ( SPA-NAI-C02-BYOMEI(IDX)(1:2)
                                                   NOT =   "■"  )
                   MOVE    "0065"              TO  SPA-ERRCD
                   GO  TO  600-C05-SYORI-EXT
               END-IF
           END-PERFORM
      *     
           IF      SPA-NAI-C02-BYOMEIHENFLG        =  "1"
               MOVE    "0078"              TO  SPA-ERRCD
               GO  TO  600-C05-SYORI-EXT
           END-IF
      *
           IF    ( SPA-GMN-C02-BANGO-X
                                   NOT =   SPACE )
               MOVE    "0090"              TO  SPA-ERRCD
               GO  TO  600-C05-SYORI-EXT
           END-IF
      *
      *    画面移行用のパラメタ変更
      *    受付と患者検索以外の元をセット
           MOVE    "C05"               TO  SPA-SAKIPG
      *
           INITIALIZE                  SPA-C01KYOUTU
           MOVE    "C02"               TO  SPA-C01-MOTOPG
                                           SPA-MOTOPG
           MOVE    ZERO                TO  IDY
      * 
           IF      SPA-NAI-C02-BYOMEIMOJI          >   ZERO
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       21
                   IF      SPA-NAI-C02-BYOMEICD (IDX)  NOT =   SPACE
                       ADD     1               TO  IDY              
                       MOVE    SPA-NAI-C02-BYOMEICD (IDX)
                                           TO  SPA-C01-TBYOMEICD (IDY)
                       MOVE    SPA-NAI-C02-BYOMEI   (IDX)
                                           TO  SPA-C01-TBYOMEI   (IDY)
                   END-IF                              
               END-PERFORM
           END-IF 
      *
           MOVE    SPA-C01KYOUTU       TO  SPA-WORK
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "C05    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       600-C05-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名登録へ
      *****************************************************************
       600-C03-SYORI-SEC           SECTION.
      *
           INITIALIZE                  SPA-C01KYOUTU
      *
      *    病名入力時はそのまま登録を可能とする
      *    複数病名入力時以外
           IF    ( SPA-NAI-C02-BYOMEIMOJI   >   ZERO  )
           AND   ( SPA-NAI-C02-BYOMEICD-MAX =   1     )
               MOVE    1                        TO  SPA-C01-HENFLG
      *
      *        自院病名入力時は対象外
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       6
                   IF    ( SPA-GMN-C02-BYOMEICD (IDX)
                                               NOT =   SPACE )
                   AND   ( SPA-NAI-C02-USERBYOMEIFLG (IDX)
                                                   =   "1"   )
                       MOVE    0                   TO  SPA-C01-HENFLG
                       MOVE    6                   TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *
      *    画面移行用のパラメタ変更
           MOVE    "C02"               TO  SPA-MOTOPG
           MOVE    "C03"               TO  SPA-SAKIPG
           MOVE    SPA-C01KYOUTU       TO  SPA-WORK
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "C03    "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       600-C03-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット登録
      *****************************************************************
       600-C06-SYORI-SEC           SECTION.
      *
           INITIALIZE                  SPA-C01KYOUTU
      *
      *    自院コードのみの入力のときコードも遷移する
           MOVE    ZERO                TO  IDY
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
               IF    ( SPA-GMN-C02-BYOMEICD (IDX)
                                       NOT =   SPACE )
               AND   ( SPA-NAI-C02-BYOMEI(IDX)(1:2)
                                       NOT =   "■"  )
                   IF  SPA-NAI-C02-USERBYOMEIFLG (IDX)
                                           =   SPACE
                       INITIALIZE              SPA-C01-BYOMEI-TBL
                       MOVE    6           TO  IDX
                   ELSE
                       ADD     1           TO  IDY              
                       MOVE    SPA-GMN-C02-BYOMEICD (IDX)
                                           TO  SPA-C01-TBYOMEICD  (IDY)
                       MOVE    SPA-NAI-C02-BYOMEITOP (IDX)
                                           TO  SPA-C01-TBYOMEITOP (IDY)
                       MOVE    SPA-NAI-C02-BYOMEISBT (IDX)
                                           TO  SPA-C01-TBYOMEISBT (IDY)
                       MOVE    IDX         TO  SPA-C01-TBYOMEICD-INDEX
                                                                  (IDY)
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-C01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-C02FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGC06"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-C01KYOUTU
           MOVE    SPA-FREE        TO  SPA-C02FREE
      *
           MOVE    "C02"                TO  SPA-MOTOPG
           MOVE    "C06"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "C06"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       600-C06-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント画面へ
      *****************************************************************
       430-FREEMSG-SEC              SECTION.
      *
      *    患者番号のチェック
           IF      SPA-NAI-C02-PTID    =   ZERO
               MOVE    "0086"          TO  SPA-ERRCD
               GO      TO      430-FREEMSG-EXT
           END-IF
      *
      *    修正のチェック
           IF      SPA-GMN-C02-BANGO   NOT =   ZERO
               MOVE    "0073"          TO  SPA-ERRCD
               GO      TO      430-FREEMSG-EXT
           END-IF
      *
           IF    ( SPA-GMN-C02-BANGO-X
                                   NOT =   SPACE )
               MOVE    "0090"              TO  SPA-ERRCD
               GO      TO      430-FREEMSG-EXT
           END-IF
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-C02             TO  LNK-SCRSPA
      *    画面移行用のパラメタ変更
           MOVE    "C02"               TO  SPA-MOTOPG
           MOVE    "C50"               TO  SPA-SAKIPG
      *
      *    遷移後の初期表示
           MOVE    SPA-GMN-C02-SYSYMD  TO  SPA-SRYYMDW
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-SRYYMD
           MOVE    SPACE               TO  SPA-SRYKA
           MOVE    SPACE               TO  SPA-NYUGAIKBN
           MOVE    SPA-NAI-C02-PTID    TO  SPA-PTID
           MOVE    SPA-GMN-C02-PTNUM   TO  SPA-PTNUM
           MOVE    SPA-GMN-C02-NAME    TO  SPA-NAME
           MOVE    SPA-GMN-C02-SEX     TO  SPA-SEX
           MOVE    SPA-GMN-C02-BIRTHDAY    TO  SPA-BIRTHDAYW
           MOVE    SPA-NAI-C02-BIRTHDAY    TO  SPA-BIRTHDAY
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "C50"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       430-FREEMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術歴登録画面へ
      *****************************************************************
       430-OPECOM-SEC              SECTION.
      *
      *    修正のチェック
           IF      SPA-GMN-C02-BANGO   NOT =   ZERO
               MOVE    "0073"          TO  SPA-ERRCD
               GO      TO      430-OPECOM-EXT
           END-IF
      *
           IF    ( SPA-GMN-C02-BANGO-X
                                   NOT =   SPACE )
               MOVE    "0090"              TO  SPA-ERRCD
               GO      TO      430-OPECOM-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           INITIALIZE                  SPA-C01KYOUTU
           MOVE    SPA-C01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-C02FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGC11"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-C01KYOUTU
           MOVE    SPA-FREE        TO  SPA-C02FREE
      *
      *    カーソル移動
           IF      SPA-NAI-C11-PTID    =   ZERO
               MOVE    "PTNUM"             TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM1"           TO  MCP-WIDGET
           END-IF
      *
           MOVE    "C02"               TO  SPA-MOTOPG
           MOVE    "C11"               TO  SPA-SAKIPG
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE.
           MOVE    "C11"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       430-OPECOM-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除病名一覧表示画面
      *****************************************************************
       430-DELETE-LIST-SEC              SECTION.
      *
      *    患者番号のチェック
           IF      SPA-NAI-C02-PTID    =   ZERO
               MOVE    "0086"          TO  SPA-ERRCD
               GO  TO  430-DELETE-LIST-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           INITIALIZE                  SPA-C01KYOUTU
           MOVE    SPA-C01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-C02FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGC12"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-C01KYOUTU
           MOVE    SPA-FREE        TO  SPA-C02FREE
      *
           MOVE    1               TO  FLG-END
      *
           .
       430-DELETE-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    表示切替え処理
      *****************************************************************
       430-SYOKBN-CHANGE-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-C02-KOUMOKU
                                       SPA-NAI-C02-KOUMOKU
                                       SPA-NAI-C02-SELNO
                                       SPA-C02-UPDFLG
                                       SPA-C02-IKOFLG
      *                              
                                       SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-GMN-C02-MAX
                                       SPA-PTBYOMEI-REC
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
           IF    ( SPA-GMN-C02-SRYKA   NOT =   ZERO  )
           AND   ( SPA-GMN-C02-SRYKAMEI
                                       NOT =   SPACE )
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
      *
           EVALUATE    SPA-GMN-C02-SYOKBN
               WHEN    ZERO
                   MOVE    "1"         TO  SPA-GMN-C02-SYOKBN
               WHEN    "1"
                   MOVE    "2"         TO  SPA-GMN-C02-SYOKBN
               WHEN    "2"
                   MOVE    ZERO        TO  SPA-GMN-C02-SYOKBN
           END-EVALUATE
           MOVE    SPA-GMN-C02-SYOKBN  TO  WRK-KBN
           PERFORM 5001-SYOKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-SYOKBNMEI
           IF      WRK-KBNMEI          =   SPACE  
               EVALUATE    SPA-C02-1042-SYOKBN-DSPKBN
                   WHEN    ZERO
                       MOVE    SPA-GMN-C02-SYOKBNLST (1)
                                           TO  SPA-GMN-C02-SYOKBN-G
                   WHEN    "1"
                       MOVE    SPA-GMN-C02-SYOKBNLST (2)
                                           TO  SPA-GMN-C02-SYOKBN-G
                   WHEN    "2"
                       MOVE    SPA-GMN-C02-SYOKBNLST (3)
                                           TO  SPA-GMN-C02-SYOKBN-G
               END-EVALUATE
           END-IF
      *
      *    病名一覧
           MOVE    1                   TO  SPA-GMN-C02-PAGE
      *
           PERFORM 310-SPASET-PTBYO-HENSYU-SEC 
           MOVE    1                   TO  SPA-GMN-C02-CUR
      *
           .
       430-SYOKBN-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分表示切替処理
      *****************************************************************
       430-HNYUGAIKBN-CHANGE-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-C02-KOUMOKU
                                       SPA-NAI-C02-KOUMOKU
                                       SPA-NAI-C02-SELNO
                                       SPA-C02-UPDFLG
                                       SPA-C02-IKOFLG
      *                              
                                       SPA-GMN-C02-TBL
                                       SPA-NAI-C02-TBL
                                       SPA-GMN-C02-MAX
                                       SPA-PTBYOMEI-REC
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C02-SRYYMD
           IF    ( SPA-GMN-C02-SRYKA   NOT =   ZERO  )
           AND   ( SPA-GMN-C02-SRYKAMEI
                                       NOT =   SPACE )
               MOVE    SPA-GMN-C02-SRYKA-G TO  SPA-GMN-C02-BSRYKA-G
           END-IF
      *
           EVALUATE    SPA-GMN-C02-HNYUGAIKBN
               WHEN    ZERO
                   MOVE    "1"         TO  SPA-GMN-C02-HNYUGAIKBN
               WHEN    "1"
                   MOVE    "2"         TO  SPA-GMN-C02-HNYUGAIKBN
               WHEN    "2"
                   MOVE    ZERO        TO  SPA-GMN-C02-HNYUGAIKBN
           END-EVALUATE
           MOVE    SPA-GMN-C02-HNYUGAIKBN  TO  WRK-KBN
           PERFORM 5001-HNYUGAIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C02-HNYUGAIKBNMEI
           IF      WRK-KBNMEI          =   SPACE  
               MOVE    SPA-GMN-C02-HNYUGAIKBNLST (1)
                                           TO  SPA-GMN-C02-HNYUGAIKBN-G
           END-IF
      *
      *    病名一覧
           MOVE    1                   TO  SPA-GMN-C02-PAGE
      *
           PERFORM 310-SPASET-PTBYO-HENSYU-SEC 
           MOVE    1                   TO  SPA-GMN-C02-CUR
      *
           .
       430-HNYUGAIKBN-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名コード表示へ
      *****************************************************************
       490-C09-SYORI-SEC           SECTION.
      *
           IF    ( SPA-GMN-C02-BYOMEI  =   SPACE )
           OR    ( SPA-NAI-C02-BYOMEIMOJI
                                       =   SPACE )
      *****OR    ( SPA-NAI-C02-BYOMEICD-MAX
      *****                            >   1     )
               GO  TO  490-C09-SYORI-EXT
           END-IF
      *  
           MOVE    SPACE               TO  C09
      *
           INITIALIZE                      C09
           MOVE    ZERO                TO  IDZ
                                           IDXX
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       21
               IF      SPA-NAI-C02-BYOMEICD (IDX)  NOT =   SPACE
                   IF    ( SPA-NAI-C02-BYOMEITOP (IDX) =   "1"  )
                   AND   ( IDXX                        >   ZERO )
                       MOVE    ZERO                    TO  IDXX
                       ADD     1                       TO  IDZ
                   END-IF  
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY >       21
                           OR      SPA-NAI-C02-BYOMEICD-O (IDX IDY)
                                               =   SPACE
                        ADD     1                   TO  IDZ
                                                       IDXX
                       IF      IDZ                 <=   100
                           MOVE    IDXX        TO  WRK-GMN-BANGO
                           MOVE    WRK-GMN-BANGO (4:2)
                                               TO  C09-TBANGO    (IDZ)
                           MOVE    SPA-NAI-C02-BYOMEICD-O (IDX IDY)
                                               TO  C09-TBYOMEICD (IDZ)
                           MOVE    SPA-NAI-C02-BYOMEI-O   (IDX IDY)
                                               TO  C09-TBYOMEI   (IDZ)
                           MOVE    IDZ             TO  C09-COUNT
                       ELSE
                           MOVE    21               TO  IDX
                                                        IDY
                       END-IF 
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "C02"                TO  SPA-MOTOPG
           MOVE    "C09"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
           .
       490-C09-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       480-KENSAKU-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-C02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "C02"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *
      *    氏名をクリアする
           MOVE    SPACE               TO  SPA-NAME
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P97"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       480-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約登録へ
      *****************************************************************
       480-YOYAKU-SEC             SECTION.
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-C02             TO  LNK-SCRSPA
      *    画面移行用のパラメタ変更
           MOVE    "C02"               TO  SPA-MOTOPG
           MOVE    "Y01"               TO  SPA-SAKIPG
           MOVE    "C02"               TO  SPA-MOTOPG3
      *     
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "Y01    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       480-YOYAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧へ
      *****************************************************************
       490-UKEICHI-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-C02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "C02"               TO  SPA-MOTOPG
           MOVE    "U01"               TO  SPA-SAKIPG
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "U01    "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-UKEICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名検索画面編集処理
      *****************************************************************
       490-USERBYOMEI-KENSAKU-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-BYOMEICD-ERRCD
                                           WRK-HOSOKUCD-ERRCD
      *
           IF      SPA-NAI-C02-PTID    =   ZERO
           OR      SPA-GMN-C02-SRYKA   =   SPACE
      *****OR      SPA-GMN-C02-CUR     =   27  OR  26
               MOVE    "0050"              TO  SPA-ERRCD
               GO  TO  490-USERBYOMEI-KENSAKU-EXT
           END-IF
      *
           IF    ( SPA-GMN-C02-BANGO-X
                                   NOT =   SPACE )
               MOVE    "0090"              TO  SPA-ERRCD
                GO  TO  490-USERBYOMEI-KENSAKU-EXT
           END-IF
      *
      *****IF    ( SPA-GMN-C02-BANGO           >   ZERO )
      *    AND   ( SPA-NAI-C02-BYOMEICD-MAX    >   1    )
      *        MOVE    "0056"             TO  SPA-ERRCD
      *        GO  TO  490-USERBYOMEI-KENSAKU-EXT 
      *****END-IF
      *
      *    病名のチェック
      *
      *    自院コードの入力がない場合
      **** 変更処理のとき自院コードの入力がない場合
      **** IF    ( SPA-GMN-C02-BANGO
      ****                         NOT =   ZERO  )
           IF    ( SPA-GMN-C02-BYOMEICD-G
                                       =   SPACE )
           AND   (SPA-NAI-C02-BYOMEICD (1)
                                   NOT =   SPACE )
               MOVE    "0069"              TO  SPA-ERRCD
           END-IF
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
               IF    ( SPA-GMN-C02-BYOMEICD (IDX)
                                           NOT =   SPACE )
               AND   ( SPA-NAI-C02-USERBYOMEIFLG (IDX)
                                               =   SPACE )
               AND   ( SPA-NAI-C02-BYOMEI(IDX)(1:2)
                                           NOT =   "■"  )
                   MOVE    "0069"              TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *
      *    病名コード検索時にエラーのときは遷移しない
           IF      SPA-ERRCD       NOT =   SPACE
               EVALUATE    MCP-WIDGET
                   WHEN    "BYOMEICD_1"
                   WHEN    "BYOMEICD_2"
                   WHEN    "BYOMEICD_3"
                   WHEN    "BYOMEICD_4"
                   WHEN    "BYOMEICD_5"
                   WHEN    "BYOMEICD_6"
                       GO  TO  490-USERBYOMEI-KENSAKU-EXT
                   WHEN    OTHER
                       MOVE    SPA-ERRCD       TO  WRK-BYOMEICD-ERRCD
                       MOVE    SPACE           TO  SPA-ERRCD
               END-EVALUATE
           END-IF
      *
      *    病名の文字チェック
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
               IF    ( SPA-GMN-C02-BYOMEICD (IDX)  NOT =   SPACE )
               AND   ( SPA-NAI-C02-BYOMEI(IDX)(1:2)    =   "■"  )
                   MOVE    "0035"          TO  SPA-ERRCD
                   GO  TO  490-USERBYOMEI-KENSAKU-EXT
               END-IF
           END-PERFORM
      *
      *    補足コメントのチェック
      *    複数病名の補足コメント入力不可
           IF      SPA-NAI-C02-BYOMEICD-MAX    >   1
               MOVE    "0060"              TO  SPA-ERRCD
           END-IF
      *
      *    変更処理のとき自院コードの入力がない場合
           IF      SPA-ERRCD           =   SPACE
      ****     IF    ( SPA-GMN-C02-BANGO   
      ****                             NOT =   ZERO  ) 
               IF    ( SPA-GMN-C02-HOSOKUCD-G
                                           =   SPACE )
               AND   ( SPA-GMN-C02-HOSOKU-COMMENT
                                       NOT =   SPACE )       
                   MOVE    "0069"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       3
                   IF    ( SPA-GMN-C02-HOSOKUCD (IDX)
                                               NOT =   SPACE )             
                   AND   ( SPA-NAI-C02-HOSOKU-USERBYOFLG (IDX)
                                                   =   SPACE )
                   AND   ( SPA-NAI-C02-HOSOKU-COMMENT (IDX) (1:2)
                                               NOT =   "■"  )
                       MOVE    "0069"              TO  SPA-ERRCD
                       MOVE    3                  TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      * 
      *    補足コメントコード検索時にエラーのときは遷移しない
           IF      SPA-ERRCD        NOT =    SPACE
               EVALUATE    MCP-WIDGET
                   WHEN    "HOSOKUCD_1"
                   WHEN    "HOSOKUCD_2"
                   WHEN    "HOSOKUCD_3"
                       GO  TO  490-USERBYOMEI-KENSAKU-EXT
                   WHEN    OTHER
                       MOVE    SPA-ERRCD       TO  WRK-HOSOKUCD-ERRCD
                       MOVE    SPACE           TO  SPA-ERRCD
               END-EVALUATE
           END-IF
      *
      *    補足コメントの文字チェック     
           IF      SPA-ERRCD           =   SPACE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       3
                   IF    ( SPA-GMN-C02-HOSOKUCD (IDX)
                                               NOT =   SPACE )             
                   AND   ( SPA-NAI-C02-HOSOKU-COMMENT(IDX)(1:2)
                                                   =   "■"  )
                       MOVE    "0035"              TO  SPA-ERRCD
                       GO  TO  490-USERBYOMEI-KENSAKU-EXT
                   END-IF
               END-PERFORM
           END-IF
      *
           IF    ( WRK-BYOMEICD-ERRCD  NOT =   SPACE )
           AND   ( WRK-HOSOKUCD-ERRCD  NOT =   SPACE )
               MOVE    WRK-BYOMEICD-ERRCD  TO  SPA-ERRCD
               GO  TO  490-USERBYOMEI-KENSAKU-EXT
           END-IF
      *
           INITIALIZE                  SPA-C01KYOUTU
           MOVE    "C02"               TO  SPA-C01-MOTOPG
      *
      *    補足コメントエラー時入力不可
           IF      WRK-HOSOKUCD-ERRCD  NOT =   SPACE
               MOVE    1                   TO  SPA-C01-B10-FLG
           END-IF   
           IF      WRK-BYOMEICD-ERRCD  NOT =   SPACE
               MOVE    2                   TO  SPA-C01-B10-FLG
           END-IF   
      *    
           MOVE    ZERO                TO  IDY
      * 
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
               IF      SPA-GMN-C02-BYOMEICD (IDX)  NOT =   SPACE
                   ADD     1               TO  IDY              
                   MOVE    SPA-GMN-C02-BYOMEICD (IDX)
                                           TO  SPA-C01-TBYOMEICD (IDY)
                   MOVE    SPA-NAI-C02-BYOMEI   (IDX)
                                           TO  SPA-C01-TBYOMEI   (IDY)
                   MOVE    SPA-NAI-C02-USERBYOMEIFLG (IDX)
                                           TO  SPA-C01-TUSERBYOMEIFLG
                                                                 (IDY)
                   MOVE    SPA-NAI-C02-BYOMEITOP (IDX)
                                           TO  SPA-C01-TBYOMEITOP (IDY)
                   MOVE    SPA-NAI-C02-BYOMEISBT (IDX)
                                           TO  SPA-C01-TBYOMEISBT (IDY)
                   MOVE    IDX             TO  SPA-C01-TBYOMEICD-INDEX
                                                                  (IDY)
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       3
               IF      SPA-GMN-C02-HOSOKUCD (IDX)  NOT =   SPACE
                   ADD     1               TO  IDY              
                   MOVE    SPA-GMN-C02-HOSOKUCD        (IDX)
                                           TO  SPA-C01-THOSOKUCD  (IDY)
                   MOVE    SPA-NAI-C02-HOSOKU-COMMENT  (IDX)
                                           TO  SPA-C01-THOSOKU-COMMENT
                                                                  (IDY)
                   MOVE    IDX             TO  SPA-C01-THOSOKUCD-INDEX
                                                                  (IDY)
               END-IF
           END-PERFORM
           IF      SPA-C01-THOSOKUCD (1)
                                       =   SPACE
               MOVE    SPA-GMN-C02-HOSOKU-COMMENT
                                           TO  SPA-C01-HOSOKU-COMMENT
           END-IF
      *
           EVALUATE    MCP-WIDGET
               WHEN    "HOSOKUCD_1"
               WHEN    "HOSOKUCD_2"
               WHEN    "HOSOKUCD_3"
                   MOVE    "HOSOKUCD"          TO  SPA-C01-WIDGET
               WHEN    "BYOMEICD_1"
               WHEN    "BYOMEICD_2"
               WHEN    "BYOMEICD_3"
               WHEN    "BYOMEICD_4"
               WHEN    "BYOMEICD_5"
               WHEN    "BYOMEICD_6"
                   MOVE    "BYOMEICD"          TO  SPA-C01-WIDGET
               WHEN    OTHER
                   MOVE    "BYOMEICD"          TO  SPA-C01-WIDGET
                   IF      WRK-BYOMEICD-ERRCD  NOT =   SPACE
                       MOVE    "HOSOKUCD"          TO  SPA-C01-WIDGET
                   END-IF
           END-EVALUATE
      *
           MOVE    WRK-BUNRUICD        TO  SPA-C01-BUNRUICD 
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-C01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-C02FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGC04"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-C01KYOUTU
           MOVE    SPA-FREE        TO  SPA-C02FREE
      *
      *****MOVE    "BUNRUICD"           TO  MCP-WIDGET
      *
           MOVE    "C02"                TO  SPA-MOTOPG
           MOVE    "C04"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "C04"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
           .
       490-USERBYOMEI-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    同義語マスタチェック処理
      *****************************************************************
       5001-SYNONYM-CHK-SEC            SECTION.
      *
           IF      SPA-C02-1042-MSG2-DSPKBN
                                       =   "1"
               MOVE    1                   TO  FLG-SYNONYM
               GO  TO  5001-SYNONYM-CHK-EXT
           END-IF
      *
           PERFORM 900-SYNONYM-READ-SEC
      *
           IF      FLG-SYNONYM         =   ZERO
               MOVE    SYNONYM-BYOMEICD    TO  BYO-BYOMEICD
               MOVE    ZERO                TO  FLG-BYOMEI
               PERFORM 900-BYOMEI-READ-SEC
               IF      FLG-BYOMEI          =   1
                   MOVE    1                   TO  FLG-SYNONYM
               ELSE
                   IF      BYO-HAISIYMD        =   SPACE   OR  ALL "9"
                       CONTINUE
                   ELSE    
                       MOVE    1                   TO  FLG-SYNONYM
                   END-IF
               END-IF
           END-IF
      *
           .
       5001-SYNONYM-CHK-EXT.
           EXIT.
      *     
      *****************************************************************
      *    レセ電エラーフラグ編集処理
      *****************************************************************
       5001-RECEDENFLG-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-PTBYO-CHK-RECEDENFLG
      *
           COMPUTE WRK-MOJISU      =   WRK-PTBYO-CHK-BYOMEIMOJI
                                       *   2
           MOVE    WRK-PTBYO-CHK-BYOMEIHENFLG
                                   TO  WRK-BYOMEIHENFLG
      *   
      *    疑いフラグが設定されているとき
           IF      WRK-PTBYO-CHK-UTAGAIFLG
                                       NOT =   SPACE
               INITIALIZE                      ORCSMKBYOMEIAREA
               MOVE    WRK-PTBYO-CHK-UTAGAIFLG
                                           TO  SMKBYO-UTAGAIFLG
               MOVE    WRK-PTBYO-CHK-BYOMEI
                                           TO  SMKBYO-BYOMEI
               MOVE    WRK-BYOMEIHENFLG    TO  SMKBYO-BYOMEIHENFLG
               MOVE    WRK-PTBYO-CHK-BYOMEICD-G
                                           TO  SMKBYO-BYOMEICD-G
               CALL    "ORCSMKBYOMEI"      USING   ORCSMKBYOMEIAREA
      *
               MOVE    SMKBYO-LENGTH       TO  WRK-MOJISU
               MOVE    SMKBYO-BYOMEIHENFLG TO  WRK-BYOMEIHENFLG
           END-IF    
      *
           COMPUTE WRK-MOJISU      =   WRK-MOJISU  /   2
      *
           IF  (   WRK-BYOMEIHENFLG    =   "1"                       )
           OR  ( ( WRK-HAISIYMD-CHK    >   ZERO                 ) AND
                 ( WRK-HAISIYMD-CHK    <   WRK-PTBYO-CHK-SRYYMD )    )
               IF      WRK-MOJISU  >   20
                   MOVE    "1"     TO  WRK-PTBYO-CHK-RECEDENFLG
               END-IF
           END-IF
           .
       5001-RECEDENFLG-HENSYU-EXT.
           EXIT
           .
      *
      *****************************************************************
      *    疑いフラグ編集処理
      *****************************************************************
       5001-UTAGAIFLG-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN             =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-UTAGAIFLG-MAX
                   IF      WRK-KBN (1:1) 
                                  =   SPA-GMN-C02-UTAGAIFLGL (IDY)
                       MOVE    SPA-GMN-C02-UTAGAIFLGMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-UTAGAIFLG-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-UTAGAIFLG-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    慢性フラグ編集処理
      *****************************************************************
       5001-MANSEIKBN-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
                                          WRK-KBNMEITAN
      *     
           IF      WRK-KBN (1:2)      =   SPACE   OR  ZERO
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-MANSEIKBN-MAX
                   IF      WRK-KBN (1:2)
                                  =   SPA-GMN-C02-MANSEIKBNL (IDY)
                       MOVE    SPA-GMN-C02-MANSEIKBNMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-MANSEIKBNTANL(IDY)
                                          TO  WRK-KBNMEITAN                   
                       MOVE    SPA-GMN-C02-MANSEIKBN-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-MANSEIKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    転帰区分編集処理
      *****************************************************************
       5001-TENKIKBN-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN  =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-TENKIKBN-MAX
                   IF      WRK-KBN (1:1)
                                      =   SPA-GMN-C02-TENKIKBNL (IDY)
                       MOVE    SPA-GMN-C02-TENKIKBNMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-TENKIKBN-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF    
      *
           .
       5001-TENKIKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主病フラグ編集処理
      *****************************************************************
       5001-SYUBYOFLG-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-SYUBYOFLG-MAX
                   IF      WRK-KBN (1:1)
                                      =   SPA-GMN-C02-SYUBYOFLGL (IDY)
      **************** MOVE    SPA-GMN-C02-SYUBYOFLGMEIL (IDY) (1:6)
                       MOVE    SPA-GMN-C02-SYUBYOFLGMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-SYUBYOFLG-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF    
      *
           .
       5001-TENKIKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       5001-SRYKA-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-SRYKA-MAX
                   IF      WRK-KBN    =   SPA-GMN-C02-SRYKAL (IDY)
                       MOVE    SPA-GMN-C02-SRYKAMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-SRYKA-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-SRYKA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科（明細欄）編集処理
      *****************************************************************
       5001-BSRYKA-HENSYU-SEC      SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-BSRYKA-MAX
                   IF      WRK-KBN    =   SPA-GMN-C02-BSRYKAL (IDY)
                       MOVE    SPA-GMN-C02-BSRYKAMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-BSRYKA-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-BSRYKA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分編集処理
      *****************************************************************
       5001-NYUGAIKBN-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-NYUGAIKBN-MAX
                       OR      SPA-GMN-C02-NYUGAIKBNL (IDY) =   SPACE
                   IF      WRK-KBN    =   SPA-GMN-C02-NYUGAIKBNL (IDY)
                       MOVE    SPA-GMN-C02-NYUGAIKBNMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-NYUGAIKBN-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-NYUGAIKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分表示切替編集処理
      *****************************************************************
       5001-HNYUGAIKBN-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-HNYUGAIKBN-MAX
                       OR      SPA-GMN-C02-HNYUGAIKBNL (IDY) =   SPACE
                   IF      WRK-KBN    =   SPA-GMN-C02-HNYUGAIKBNL (IDY)
                       MOVE    SPA-GMN-C02-HNYUGAIKBNMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-HNYUGAIKBN-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-HNYUGAIKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       5001-HKNCOMBI-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
           MOVE    ZERO               TO  WRK-TEKEDYMD
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-HKNCOMBI-MAX
                   IF      WRK-KBN        =
                                          SPA-GMN-C02-HKNCOMBIL (IDY)
                       MOVE    SPA-GMN-C02-HKNCOMBIMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-NAI-C02-TEKSTYMD     (IDY)
                                          TO  WRK-TEKSTYMD
                       MOVE    SPA-NAI-C02-TEKEDYMD     (IDY)
                                          TO  WRK-TEKEDYMD
                       MOVE    SPA-GMN-C02-HKNCOMBI-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-HKNCOMBI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    第三者行為編集処理
      *****************************************************************
       5001-KOHID-HENSYU-SEC       SECTION.
      * 
           MOVE    ZERO               TO  WRK-RYOEDYMD
      *     
           IF      WRK-KBNMEI      =   SPACE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-KOHID-MAX
                   IF      WRK-KOHID      =   SPA-NAI-C02-KOHIDL (IDY)
                       MOVE    SPA-GMN-C02-KOHIDL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-KOHID-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-KOHID-MAX
                   IF      WRK-KBNMEI     =   SPA-GMN-C02-KOHIDL (IDY)
                       MOVE    SPA-NAI-C02-KOHIDL (IDY)
                                          TO  WRK-KOHID
                       MOVE    SPA-NAI-C02-RYOSTYMDL    (IDY)
                                          TO  WRK-RYOSTYMD
                       MOVE    SPA-NAI-C02-RYOEDYMDL    (IDY)
                                          TO  WRK-RYOEDYMD
                       MOVE    SPA-GMN-C02-KOHID-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-KOHID-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険病名フラグ編集処理
      *****************************************************************
       5001-HKNBYOMEIFLG-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-HKNBYOMEIFLG-MAX
                   IF      WRK-KBN     =
                                       SPA-GMN-C02-HKNBYOMEIFLGL (IDY)
                       MOVE    SPA-GMN-C02-HKNBYOMEIFLGMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-HKNBYOMEIFLG-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-HKNBYOMEIFLG-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト表示フラグ編集処理
      *****************************************************************
       5001-REZEFLG-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-REZEFLG-MAX
                   IF      WRK-KBN     =   SPA-GMN-C02-REZEFLGL (IDY)
                       MOVE    SPA-GMN-C02-REZEFLGMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-REZEFLG-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-REZEFLG-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理区分編集処理
      *****************************************************************
       5001-SYOKBN-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-SYOKBN-MAX
                   IF      WRK-KBN (1:1)  =   SPA-GMN-C02-SYOKBNL (IDY)
                       MOVE    SPA-GMN-C02-SYOKBNMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-SYOKBN-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-SYOKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定疾患対象区分、難病外来対象区分編集処理
      *****************************************************************
       5001-HYOUJI-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE               TO  SPA-GMN-C02-HYOUJI
      * 
           EVALUATE    SPA-GMN-C02-MANSEIKBN
      ***      WHEN    SPACE
      ***          IF    ( SPA-NAI-C02-NANBYOCD
      ***                                  NOT =   ZERO             )
      ***          AND   ( SPA-NAI-C02-MANSEIKBN
      ***                                      =   "05"    OR  "08" )
      ***              MOVE    "特定疾患・難病外来対象"
      ***                                      TO  SPA-GMN-C02-HYOUJI
      ***          ELSE
      ***              IF      SPA-NAI-C02-MANSEIKBN
      ***                                      =   "05"    OR  "08"
      ***                  MOVE    "特定疾患対象"  TO
      ***                                          SPA-GMN-C02-HYOUJI
      ***              END-IF
      ***              IF      SPA-NAI-C02-NANBYOCD
      ***                                      NOT =   ZERO
      ***                  MOVE    "難病外来対象"  TO
      ***                                          SPA-GMN-C02-HYOUJI
      ***              END-IF
      ***          END-IF
               WHEN    "09"
                   IF      SPA-NAI-C02-MANSEIKBN
                                               =   "05"    OR  "08"
                       MOVE    "特定疾患対象"  TO  SPA-GMN-C02-HYOUJI
                   END-IF
               WHEN    OTHER    
                   IF      SPA-NAI-C02-NANBYOCD
                                           NOT =   ZERO
                       MOVE    "難病外来対象"  TO  SPA-GMN-C02-HYOUJI
                   END-IF
           END-EVALUATE
           .
       5001-HYOUJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    退院証明書記載フラグ編集処理
      *****************************************************************
       5001-DISCHARGEFLG-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE              TO  WRK-KBNMEI
      *
           IF      WRK-KBN            =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-DISCHARGEFLG-MAX
                   IF      WRK-KBN     =
                                       SPA-GMN-C02-DISCHARGEFLGL (IDY)
                       MOVE    SPA-GMN-C02-DISCHARGEFLGMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-DISCHARGEFLG-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-DISCHARGEFLG-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    原疾患区分編集処理
      *****************************************************************
       5001-CLASS1-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE              TO  WRK-KBNMEI
      *
           IF      WRK-KBN (1:2)      =   SPACE    OR  ZERO
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-CLASS1-MAX
                   IF      WRK-KBN (1:2)  =  SPA-GMN-C02-CLASS1L (IDY)
                       MOVE    SPA-GMN-C02-CLASS1MEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-CLASS1-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-CLASS1-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    合併症区分編集処理
      *****************************************************************
       5001-CLASS2-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE              TO  WRK-KBNMEI
      *
           IF      WRK-KBN (1:2)      =   SPACE    OR  ZERO
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-CLASS2-MAX
                   IF      WRK-KBN (1:2)  =   SPA-GMN-C02-CLASS2L (IDY)
                       MOVE    SPA-GMN-C02-CLASS2MEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-CLASS2-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-CLASS2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *----(01.00.03) LINE ADD START ----------------------------------
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY          TO  WRK-SYMD
           ELSE
               MOVE    "0017"          TO  SPA-ERRCD
           END-IF
      *----(01.00.03) LINE ADD END   ----------------------------------
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
               MOVE    SPACE               TO  WRK-NENGO
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
               MOVE    LNK-DAY2-EDTYMD2-G  TO  WRK-NENGO
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-CIDCD       NOT =   SPACE
               PERFORM 520-CIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-CIDCD2      NOT =   SPACE
               PERFORM 530-CIDSET2-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-CIDCD4      NOT =   SPACE
               PERFORM 540-CIDSET4-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-CIDCD6      NOT =   SPACE
               PERFORM 560-CIDSET6-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "C02    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           IF      FLG-ROW             =   1
               MOVE    ZERO                TO  WRK-C02-BYO-ROW
           ELSE
               MOVE    C02-BYO-ROW         TO  WRK-C02-BYO-ROW
           END-IF
      *
           INITIALIZE                      C02
      *
           MOVE    SPA-GMN-C02-PTNUM       TO  C02-PTNUM
           MOVE    SPA-GMN-C02-SYSYMD      TO  C02-SYSYMD
           MOVE    SPA-GMN-C02-KANANAME    TO  C02-KANANAME
           MOVE    SPA-GMN-C02-NAME        TO  C02-NAME
           IF      SPA-NAI-C02-LOCK        =   ZERO
               MOVE    "black"                 TO  C02-NAME-STYLE
                                                   C02-KANANAME-STYLE
           ELSE
               MOVE    "red"                   TO  C02-NAME-STYLE
                                                   C02-KANANAME-STYLE
           END-IF
           MOVE    SPA-GMN-C02-SEX         TO  C02-SEX
           MOVE    SPA-GMN-C02-BIRTHDAY    TO  C02-BIRTHDAY
           MOVE    SPA-GMN-C02-AGE         TO  C02-AGE
           MOVE    SPA-GMN-C02-HKNCOMBIMEIH
                                           TO  C02-HKNNAME
           MOVE    SPA-GMN-C02-FTNRATE     TO  C02-PTFTNRATE
           MOVE    SPA-GMN-C02-LASTYMD     TO  C02-LASTYMD
           IF      SPA-NAI-C02-LOCK        =   ZERO
               MOVE    SPA-GMN-C02-SRYKA-G     TO  C02-SRYKA
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL       IDX     >   SPA-GMN-C02-SRYKA-MAX
                   MOVE    SPA-GMN-C02-SRYKALST (IDX)
                                               TO  C02-SRYKALST (IDX)
               END-PERFORM
               MOVE    SPA-GMN-C02-SRYKA-MAX   TO  C02-SRYKA-COUNT
               MOVE    SPA-GMN-C02-BSRYKA-G    TO  C02-BSRYKA
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL       IDX     >   SPA-GMN-C02-BSRYKA-MAX
                   MOVE    SPA-GMN-C02-BSRYKALST (IDX)
                                               TO  C02-BSRYKALST(IDX)
               END-PERFORM
               MOVE    SPA-GMN-C02-BSRYKA-MAX  TO  C02-BSRYKA-COUNT
           END-IF
           MOVE    SPA-GMN-C02-SYOKBN-G    TO  C02-SYOKBN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C02-SYOKBN-MAX
               MOVE    SPA-GMN-C02-SYOKBNLST (IDX)
                                               TO  C02-SYOKBNLST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C02-SYOKBN-MAX  TO  C02-SYOKBN-COUNT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C02-MAX
               MOVE    SPA-GMN-C02-TBANGO    (IDX)
                                           TO  WRK-NUM
               MOVE    WRK-NUM             TO  C02-TBANGO    (IDX)
               IF      SPA-GMN-C02-SRYKA   =   ZERO
                   MOVE    SPA-GMN-C02-TSRYKA     (IDX)
                                           TO  C02-TSRYKA    (IDX)
               END-IF
               MOVE    SPA-GMN-C02-TSRYYMD    (IDX)
                                           TO  C02-TSRYYMD   (IDX)
               MOVE    SPA-GMN-C02-TBYOMEI    (IDX)
                                           TO  C02-TBYOMEI   (IDX)
               MOVE    SPA-GMN-C02-TSYUBYOFLG (IDX)
                                           TO  C02-TSYUBYOFLG(IDX)
               MOVE    SPA-GMN-C02-TMANSEIKBN (IDX)
                                           TO  C02-TMANSEIFLG(IDX)
               MOVE    SPA-GMN-C02-TNYUGAIKBN (IDX)
                                           TO  C02-TNYUGAIKBN(IDX)
               MOVE    SPA-GMN-C02-TUTAGAIFLG (IDX)
                                           TO  C02-TUTAGAIFLG(IDX)
               MOVE    SPA-GMN-C02-THKNCOMBI  (IDX)
                                           TO  C02-THKNCOMBI (IDX)
               MOVE    SPA-GMN-C02-TKOHID     (IDX)
                                           TO  C02-TKOHID    (IDX)
               MOVE    SPA-GMN-C02-TREZEFLG (IDX)
                                           TO  C02-TREZEFLG (IDX)
               IF      SPA-GMN-C02-TREZEMM  (IDX)    NOT =   ZERO
                   MOVE    SPA-GMN-C02-TREZEMM (IDX)
                                           TO  C02-TREZEMM (IDX)(2:)
               END-IF
               MOVE    SPA-GMN-C02-TTENKIKBN  (IDX)
                                           TO  C02-TTENKIKBN  (IDX)
               MOVE    SPA-GMN-C02-TTENKIYMD (IDX)
                                           TO  C02-TTENKIYMD (IDX)
               MOVE    SPA-GMN-C02-THKNBYOMEIFLG (IDX)
                                           TO  C02-THKNBYOMEIFLG (IDX)
               MOVE    SPA-GMN-C02-TDISCHARGEFLG (IDX)
                                           TO  C02-TDISCHARGEFLG (IDX)
               MOVE    SPA-GMN-C02-TCLASS1 (IDX)
                                           TO  C02-TCLASS1 (IDX)
               MOVE    SPA-GMN-C02-TCLASS2 (IDX)
                                           TO  C02-TCLASS2 (IDX)
               MOVE    SPA-GMN-C02-TCREYMD    (IDX)
                                           TO  C02-TCREYMD  (IDX)
               MOVE    SPA-GMN-C02-TCREHMS    (IDX)
                                           TO  C02-TCREHMS  (IDX)
               MOVE    SPA-GMN-C02-TUPYMD     (IDX)
                                           TO  C02-TUPYMD   (IDX)
               MOVE    SPA-GMN-C02-TUPHMS     (IDX)
                                           TO  C02-TUPHMS   (IDX)
      *
      ***      MOVE    IDX                 TO  C02-BYO-COUNT
      *         
               IF      IDX             =   SPA-NAI-C02-SELNO
                   MOVE    "T"             TO  C02-BYO-SELECT (IDX)
               ELSE
                   MOVE    "F"             TO  C02-BYO-SELECT (IDX)
               END-IF
      *
               IF      SPA-NAI-C02-TBYOMEIHENFLG (IDX)
                                           =   "1"
                   MOVE    "#FFCC99"       TO  C02-BYO-BGCOLOR (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-C02-MAX         TO  C02-BYO-COUNT
      *
           IF    ( SPA-GMN-C02-NEXT        =   1                )
           AND   ( SPA-GMN-C02-MAX         <   WRK-CONS-C02-MAX )
               ADD     1                       TO  C02-BYO-COUNT
               MOVE    "＊＊　次頁があります　＊＊"
                                               TO  C02-TBYOMEI
                                                   (C02-BYO-COUNT)
           END-IF
      *
           MOVE    ZERO                    TO  C02-BYO-ROW
           IF      WRK-C02-BYO-ROW         >   ZERO
               MOVE    WRK-C02-BYO-ROW         TO  C02-BYO-ROW
           END-IF
           IF    ( SPA-NAI-C02-SELNO       >   ZERO )
           OR    ( SPA-NAI-C02-UPDOWN      =   1    )
               MOVE    2                       TO  C02-BYO-ROWATTR
           END-IF
      *
           IF      SPA-GMN-C02-BANGO-X NOT =   SPACE
               MOVE    SPA-GMN-C02-BANGO-X     TO  C02-BANGO
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-C02-BANGO       TO  SNUM-INX
               CALL    "ORCSNUM"               USING
                                               ORCSNUMAREA
               IF      SNUM-RC                 =   ZERO
                   PERFORM VARYING IDX FROM    11  BY  1
                           UNTIL   IDX >       15
                       IF      SNUM-OUTEDT (IDX:1) NOT =   ZERO
                           MOVE    SNUM-OUTEDT (IDX:)  TO  C02-BANGO
                           MOVE    15                  TO  IDX
                       END-IF
                   END-PERFORM
               END-IF             
           END-IF
           MOVE    SPA-GMN-C02-SRYYMD      TO  C02-SRYYMD
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >      6
               MOVE    SPA-GMN-C02-BYOMEICD(IDX)
                                              TO  C02-BYOMEICD (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C02-BYOMEI         TO  C02-BYOMEI
           MOVE    SPA-GMN-C02-BYOMEIHENFLG   TO  C02-BYOMEIHENFLG-VALUE
           MOVE    "black"                    TO  C02-BYOMEIHENFLG-STYLE
           IF      SPA-NAI-C02-RECEDENFLG     =   "1"
               MOVE    "red"                  TO  C02-BYOMEIHENFLG-STYLE
               IF       SPA-GMN-C02-BYOMEIHENFLG   =   SPACE
                   MOVE    "文字超過"         TO  C02-BYOMEIHENFLG-VALUE
               END-IF
           END-IF
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >      3
               MOVE    SPA-GMN-C02-HOSOKUCD(IDX)
                                              TO  C02-HOSOKUCD (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C02-HOSOKU-COMMENT TO  C02-HOSOKU-COMMENT
           MOVE    SPA-GMN-C02-CHARTBYOMEI    TO  C02-CHARTBYOMEI
      *
           MOVE    SPA-GMN-C02-REZEMM         TO  C02-REZEMM
           MOVE    SPA-GMN-C02-TENKIYMD       TO  C02-TENKIYMD
      *
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C02-HKN-MAX
               IF      SPA-GMN-C02-THKNCOMBINUM(IDX)    NOT =   ZERO
                   MOVE    SPA-GMN-C02-THKNCOMBINUM(IDX)
                                               TO  C02-HKNCOMBINUM(IDX)
               END-IF
               MOVE    SPA-GMN-C02-THKNCOMBIMEI(IDX)
                                               TO  C02-HKNCOMBIMEI(IDX)
      *
      ****     MOVE    IDX                     TO  C02-HKN-COUNT
           END-PERFORM
           MOVE    SPA-GMN-C02-HKN-MAX    TO  C02-HKN-COUNT
           MOVE    ZERO                   TO  C02-HKN-ROW 
      *
           MOVE    SPA-GMN-C02-TENKIKBN-G TO  C02-TENKIKBN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C02-TENKIKBN-MAX 
                   OR          SPA-GMN-C02-TENKIKBNL  (IDX)   =   SPACE
               MOVE    SPA-GMN-C02-TENKIKBNLST (IDX)
                                           TO  C02-TENKIKBN-ITEM (IDX)
               MOVE    IDX                 TO  C02-TENKIKBN-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-C02-UTAGAIFLG-G TO  C02-UTAGAIFLG
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C02-UTAGAIFLG-MAX
                   OR          SPA-GMN-C02-UTAGAIFLGL  (IDX)  =   SPACE
               MOVE    SPA-GMN-C02-UTAGAIFLGLST (IDX)
                                           TO  C02-UTAGAIFLG-ITEM (IDX)
               MOVE    IDX                 TO  C02-UTAGAIFLG-COUNT    
           END-PERFORM
      *
           MOVE    SPA-GMN-C02-SYUBYOFLG-G TO  C02-SYUBYOFLG
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C02-SYUBYOFLG-MAX
                   OR          SPA-GMN-C02-SYUBYOFLGL  (IDX)  =   SPACE
               MOVE    SPA-GMN-C02-SYUBYOFLGLST (IDX)
                                           TO  C02-SYUBYOFLG-ITEM (IDX)
               MOVE    IDX                 TO  C02-SYUBYOFLG-COUNT  
           END-PERFORM
      *
           MOVE    SPA-GMN-C02-MANSEIKBN-G TO  C02-MANSEIFLG
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C02-MANSEIKBN-MAX
                   OR          SPA-GMN-C02-MANSEIKBNL  (IDX)  =   SPACE
               MOVE    SPA-GMN-C02-MANSEIKBNLST (IDX)
                                           TO  C02-MANSEIFLG-ITEM (IDX)
               MOVE    IDX                 TO  C02-MANSEIFLG-COUNT
           END-PERFORM
           MOVE    "blue"                  TO  C02-HYOUJI-STYLE
           MOVE    SPA-GMN-C02-HYOUJI      TO  C02-HYOUJI
      *
           MOVE    SPA-GMN-C02-NYUGAIKBN-G TO  C02-NYUGAIKBN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C02-NYUGAIKBN-MAX
                   OR          SPA-GMN-C02-NYUGAIKBNL  (IDX)  =   SPACE
               MOVE    SPA-GMN-C02-NYUGAIKBNLST (IDX)
                                           TO  C02-NYUGAIKBN-ITEM (IDX)
               MOVE    IDX                 TO  C02-NYUGAIKBN-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-C02-HNYUGAIKBN-G
                                       TO  C02-HNYUGAIKBN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C02-HNYUGAIKBN-MAX
               MOVE    SPA-GMN-C02-HNYUGAIKBNLST (IDX)
                                           TO  C02-HNYUGAIKBNLST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C02-HNYUGAIKBN-MAX
                                       TO  C02-HNYUGAIKBN-COUNT
      *
           MOVE    SPA-GMN-C02-HKNCOMBI-G TO  C02-HKNCOMBI
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C02-HKNCOMBI-MAX
               MOVE    SPA-GMN-C02-HKNCOMBILST (IDX)
                                           TO  C02-HKNCOMBI-ITEM (IDX)
      *****    MOVE    IDX                 TO  C02-HKNCOMBI-COUNT
           END-PERFORM
           MOVE    SPA-GMN-C02-HKNCOMBI-MAX    TO  C02-HKNCOMBI-COUNT
      *
           MOVE    SPA-GMN-C02-KOHID   TO  C02-KOHID
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C02-KOHID-MAX
               MOVE    SPA-GMN-C02-KOHIDLST (IDX)
                                           TO  C02-KOHID-ITEM (IDX)
               MOVE    IDX                 TO  C02-KOHID-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-C02-HKNBYOMEIFLG-G TO  C02-HKNBYOMEIFLG
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >
                                           SPA-GMN-C02-HKNBYOMEIFLG-MAX
                   OR          SPA-GMN-C02-HKNBYOMEIFLGL(IDX) =   SPACE
               MOVE    SPA-GMN-C02-HKNBYOMEIFLGLST (IDX)
                                       TO  C02-HKNBYOMEIFLG-ITEM (IDX)
               MOVE    IDX             TO  C02-HKNBYOMEIFLG-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-C02-REZEFLG-G   TO  C02-REZEFLG
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C02-REZEFLG-MAX
                   OR          SPA-GMN-C02-REZEFLGL  (IDX)   =   SPACE
               MOVE    SPA-GMN-C02-REZEFLGLST (IDX)
                                           TO  C02-REZEFLG-ITEM (IDX)
               MOVE    IDX                 TO  C02-REZEFLG-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-C02-DISCHARGEFLG-G TO  C02-DISCHARGEFLG
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >
                                           SPA-GMN-C02-DISCHARGEFLG-MAX
                   OR          SPA-GMN-C02-DISCHARGEFLGL(IDX) =   SPACE
               MOVE    SPA-GMN-C02-DISCHARGEFLGLST (IDX)
                                       TO  C02-DISCHARGEFLG-ITEM (IDX)
               MOVE    IDX             TO  C02-DISCHARGEFLG-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-C02-CLASS1-G TO  C02-CLASS1
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >
                                           SPA-GMN-C02-CLASS1-MAX
                   OR          SPA-GMN-C02-CLASS1L(IDX) =   SPACE
               MOVE    SPA-GMN-C02-CLASS1LST (IDX)
                                       TO  C02-CLASS1-ITEM (IDX)
               MOVE    IDX             TO  C02-CLASS1-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-C02-CLASS2-G TO  C02-CLASS2
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >
                                           SPA-GMN-C02-CLASS2-MAX
                   OR          SPA-GMN-C02-CLASS2L(IDX) =   SPACE
               MOVE    SPA-GMN-C02-CLASS2LST (IDX)
                                       TO  C02-CLASS2-ITEM (IDX)
               MOVE    IDX             TO  C02-CLASS2-COUNT
           END-PERFORM
      *    
      **** IF    ( SPA-C02-MOTOPG      =   "K02"   OR  "U02" 
      *                                             OR  "K02N")
      *     AND   ( SPA-PTNUM       NOT =   SPACE             )
      *         MOVE    WIDGET-INSENSITIVE  TO  C02-PTNUM-STATE
      *         MOVE    WIDGET-INSENSITIVE  TO  C02-B03-STATE
      *                                         C02-B09-STATE
      ***     ELSE
              MOVE    WIDGET-NORMAL        TO  C02-PTNUM-STATE
                                               C02-B03-STATE
                                               C02-B09-STATE
      ***     END-IF
      *
           MOVE    WIDGET-INSENSITIVE  TO  C02-BYOMEI-UP-STATE
                                           C02-BYOMEI-DOWN-STATE
                                           C02-KOHIDCOMBO-STATE
           MOVE    WIDGET-NORMAL       TO  C02-B04-STATE
                                           C02-B05-STATE
                                           C02-B06-STATE
                                           C02-B07-STATE
                                           C02-B08-STATE
                                           C02-B12-STATE
           IF    ( SPA-NAI-C02-SELNO   >   ZERO       )
           AND   ( SPA-GMN-C02-SRYKA   =   "00"    OR
                   SPA-GMN-C02-SRYKA-MAX
                                       =   1          )
           AND   ( SPA-GMN-C02-HNYUGAIKBN
                                       =   "0"        ) 
               EVALUATE    SPA-GMN-C02-SYOKBN
                   WHEN    "2"
                       MOVE    WIDGET-NORMAL   TO  C02-BYOMEI-UP-STATE
                                                   C02-BYOMEI-DOWN-STATE
                       IF      SPA-NAI-C02-UPDOWN      =   1 
                           MOVE    WIDGET-INSENSITIVE
                                                   TO  C02-B08-STATE
      *****************                                C02-B04-STATE
      *****************                                C02-B05-STATE
                       END-IF
                   WHEN    "0"
                       IF      SPA-NAI-C02-TSRYYMD
                                           (SPA-NAI-C02-SELNO) (1:6)
                                       =   SPA-NAI-C02-SYSYMD (1:6)
                           MOVE    WIDGET-NORMAL
                                               TO  C02-BYOMEI-UP-STATE
                                                   C02-BYOMEI-DOWN-STATE
                           IF      SPA-NAI-C02-UPDOWN      =   1 
                               MOVE    WIDGET-INSENSITIVE
                                                   TO  C02-B08-STATE
      *****************                                C02-B04-STATE
      *****************                                C02-B05-STATE
                           END-IF
                       END-IF
                   WHEN    "1"
                       IF      SPA-NAI-C02-TSRYYMD
                                           (SPA-NAI-C02-SELNO) (1:6)
                                       =   SPA-NAI-C02-SYSYMD-BACK (1:6)
                           MOVE    WIDGET-NORMAL
                                               TO  C02-BYOMEI-UP-STATE
                                                   C02-BYOMEI-DOWN-STATE
                           IF      SPA-NAI-C02-UPDOWN      =   1 
                               MOVE    WIDGET-INSENSITIVE
                                                   TO  C02-B08-STATE
      *****************                                C02-B04-STATE
      *****************                                C02-B05-STATE
                           END-IF
                       END-IF
               END-EVALUATE
           END-IF
      *
           IF      SPA-NAI-C02-UPDOWN      =   9
               MOVE    WIDGET-INSENSITIVE      TO  C02-BYOMEI-UP-STATE
                                                   C02-BYOMEI-DOWN-STATE
           END-IF
      *
           IF      SPA-NAI-C02-KOHID-DAISANSYA
                                           =   1
               MOVE    WIDGET-NORMAL           TO  C02-KOHIDCOMBO-STATE
           END-IF
      *
           EVALUATE    SPA-GMN-C02-SYOKBN
               WHEN    ZERO
                   MOVE    "当月"          TO  C02-B21-LABEL
               WHEN    "1"
                   MOVE    "前月"          TO  C02-B21-LABEL
               WHEN    "2"
                   MOVE    "全体"          TO  C02-B21-LABEL
           END-EVALUATE
      *
           EVALUATE    SPA-GMN-C02-HNYUGAIKBN
               WHEN    ZERO
                   MOVE    "入・外"        TO  C02-B23-LABEL
               WHEN    "1"
                   MOVE    "入  院"        TO  C02-B23-LABEL
               WHEN    "2"
                   MOVE    "入院外"        TO  C02-B23-LABEL
           END-EVALUATE
      *               
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE   OR  "0039" )
           AND   ( SPA-GMN-C02-CUR     =   ZERO               )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-C02-CUR
               WHEN    1
                   MOVE  "BANGO"       TO  MCP-WIDGET
               WHEN    9
                   MOVE  "SRYYMD"      TO  MCP-WIDGET
               WHEN    2
                   MOVE  "BYOMEICD_1"  TO  MCP-WIDGET
               WHEN    3
                   MOVE  "BYOMEICD_2"  TO  MCP-WIDGET
               WHEN    4
                   MOVE  "BYOMEICD_3"  TO  MCP-WIDGET
               WHEN    5
                   MOVE  "BYOMEICD_4"  TO  MCP-WIDGET
               WHEN    6
                   MOVE  "BYOMEICD_5"  TO  MCP-WIDGET
               WHEN    7
                   MOVE  "BYOMEICD_6"  TO  MCP-WIDGET
               WHEN    18
                   MOVE  "UTAGAIFLG"   TO  MCP-WIDGET
               WHEN    19
                   MOVE  "SYUBYOFLG"   TO  MCP-WIDGET
               WHEN    20
                   MOVE  "MANSEIFLG"   TO  MCP-WIDGET
               WHEN    21
                   MOVE  "NYUGAIKBN"   TO  MCP-WIDGET
               WHEN    22
                   MOVE  "HKNCOMBI"    TO  MCP-WIDGET
               WHEN    23
                   MOVE  "REZEFLG"     TO  MCP-WIDGET
               WHEN    24
                   MOVE  "REZEMM"      TO  MCP-WIDGET
               WHEN    17
                   MOVE  "TENKIKBN"    TO  MCP-WIDGET
               WHEN    13
                   MOVE  "TENKIYMD"    TO  MCP-WIDGET
               WHEN    8
                   MOVE  "BYOMEI"      TO  MCP-WIDGET
               WHEN    28
                   MOVE  "CHARTBYOMEI" TO  MCP-WIDGET
               WHEN    25
                   MOVE  "HKNBYOMEIFLG"
                                       TO  MCP-WIDGET
               WHEN    26
                   MOVE  "PTNUM"       TO  MCP-WIDGET
               WHEN    27
                   MOVE  "SRYKA"       TO  MCP-WIDGET
      *********WHEN    30
      *********    MOVE  "PTNUM"       TO  MCP-WIDGET
               WHEN    31
                   MOVE  "BSRYKA"      TO  MCP-WIDGET
               WHEN    32
                   MOVE  "SYOKBN"      TO  MCP-WIDGET
               WHEN    33
                   MOVE  "HNYUGAIKBN"  TO  MCP-WIDGET
               WHEN   34
                   MOVE  "HOSOKUCD_1"  TO  MCP-WIDGET
               WHEN   35
                   MOVE  "HOSOKUCD_2"  TO  MCP-WIDGET
               WHEN   36
                   MOVE  "HOSOKUCD_3"  TO  MCP-WIDGET
               WHEN   40
                   MOVE  "HOSOKU_COMMENT"
                                       TO  MCP-WIDGET
               WHEN   41
                    MOVE  "KOHID"      TO  MCP-WIDGET
               WHEN   42
                   MOVE  "DISCHARGEFLG"
                                       TO  MCP-WIDGET
               WHEN   43
                   MOVE  "CLASS1"      TO  MCP-WIDGET
               WHEN   44
                   MOVE  "CLASS2"      TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "BANGO"
                   MOVE    9                   TO  SPA-GMN-C02-CUR
               WHEN    "BSRYKA"
                   MOVE    2                   TO  SPA-GMN-C02-CUR
               WHEN    "BYOMEICD_1"
                   MOVE    3                   TO  SPA-GMN-C02-CUR
               WHEN    "BYOMEICD_2"
                   MOVE    4                   TO  SPA-GMN-C02-CUR
               WHEN    "BYOMEICD_3"
                   MOVE    5                   TO  SPA-GMN-C02-CUR
               WHEN    "BYOMEICD_4"
                   MOVE    6                   TO  SPA-GMN-C02-CUR
               WHEN    "BYOMEICD_5"
                   MOVE    7                   TO  SPA-GMN-C02-CUR
               WHEN    "BYOMEICD_6"
                   MOVE    34                  TO  SPA-GMN-C02-CUR
               WHEN    "BYOMEI"
                   MOVE    34                  TO  SPA-GMN-C02-CUR
               WHEN    "HOSOKU_COMMENT"
                   MOVE    28                  TO  SPA-GMN-C02-CUR
               WHEN    "HOSOKUCD_1"
                   MOVE    34                  TO  SPA-GMN-C02-CUR
               WHEN    "HOSOKUCD_2"
                   MOVE    35                  TO  SPA-GMN-C02-CUR
               WHEN    "HOSOKUCD_3"
                   MOVE    28                  TO  SPA-GMN-C02-CUR
               WHEN    "CHARTBYOMEI"
                   MOVE    9                   TO  SPA-GMN-C02-CUR
               WHEN    "SRYYMD"
                   MOVE    17                  TO  SPA-GMN-C02-CUR
               WHEN    "TENKIKBN"
                   MOVE    13                  TO  SPA-GMN-C02-CUR
               WHEN    "TENKIYMD"
                   MOVE    18                  TO  SPA-GMN-C02-CUR
               WHEN    "UTAGAIFLG"
                   MOVE    19                  TO  SPA-GMN-C02-CUR
               WHEN    "SYUBYOFLG"
                   MOVE    20                  TO  SPA-GMN-C02-CUR
               WHEN    "MANSEIFLG"
                   MOVE    21                  TO  SPA-GMN-C02-CUR
               WHEN    "NYUGAIKBN"
                   MOVE    22                  TO  SPA-GMN-C02-CUR
               WHEN    "HKNCOMBI"
                   IF      SPA-NAI-C02-KOHID-DAISANSYA
                                           =   ZERO
                       MOVE    23                  TO  SPA-GMN-C02-CUR
                   ELSE
                       MOVE    41                  TO  SPA-GMN-C02-CUR
                   END-IF
               WHEN    "KOHID"
                   MOVE    23                  TO  SPA-GMN-C02-CUR
               WHEN    "REZEFLG"
                   MOVE    24                  TO  SPA-GMN-C02-CUR
               WHEN    "REZEMM"
                   MOVE    25                  TO  SPA-GMN-C02-CUR
               WHEN    "HKNBYOMEIFLG"
                   MOVE    42                  TO  SPA-GMN-C02-CUR
               WHEN    "DISCHARGEFLG"
                   MOVE    43                  TO  SPA-GMN-C02-CUR
               WHEN    "CLASS1"
                   MOVE    44                  TO  SPA-GMN-C02-CUR
               WHEN    "CLASS2"
                   MOVE    2                   TO  SPA-GMN-C02-CUR
               WHEN    "B12"
                   MOVE    99                  TO  SPA-GMN-C02-CUR
           END-EVALUATE
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE                TO  CERR
      *
           EVALUATE    SPA-ERRCD
      *********WHEN    "0043"
               WHEN    "0075"
               WHEN    "0104"
                   MOVE    WRK-ERR-BYOMEI  TO  CERR-BYOMEI
           END-EVALUATE
      *
           MOVE    SPA-ERRCD            TO  CERR-ERRCODE
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラーです"            TO  CERR-ERRMSG
               WHEN    "0002"
                   STRING
                       WRK-ERR-BYOMEI          DELIMITED   BY  SPACE
                       "は"                    DELIMITED   BY  SIZE 
                       WRK-ERR-SRYYMD          DELIMITED   BY  SPACE
                       "に登録してあります。"  DELIMITED   BY  SIZE 
                                               INTO    CERR-ERRMSG
                   END-STRING
               WHEN    "0003"
                   MOVE    "警告メッセージ"            TO  CERR-ERRMSG
               WHEN    "0004"
                   MOVE    "対象のデータを選択してありません"
                                                       TO  CERR-ERRMSG
               WHEN    "0005"
                   MOVE    "対象のデータが選択してあります"
                                                       TO  CERR-ERRMSG
               WHEN    "0006"
                   MOVE    "病名コード順がエラーです"  TO  CERR-ERRMSG
               WHEN    "0007"
                   MOVE    "病名が長すぎます"          TO  CERR-ERRMSG
               WHEN    "0008"
                   MOVE    "項目に入力がありません"    TO  CERR-ERRMSG
               WHEN    "0009"
                   MOVE    "該当の保険組合せがありません" 
                                                       TO  CERR-ERRMSG
               WHEN    "0010"
                   MOVE    "病名が未入力か入力の途中です"
                                                       TO  CERR-ERRMSG
               WHEN    "0011"
               MOVE "開始日が該当の保険組合せの適用日の範囲外です"
                                                       TO  CERR-ERRMSG
               WHEN    "0012"
                   MOVE    "病名番号入力エラー"        TO  CERR-ERRMSG
               WHEN    "0013"
                   MOVE    "年号入力エラー"            TO  CERR-ERRMSG
               WHEN    "0014"
                   MOVE    "数字で入力してください"    TO  CERR-ERRMSG
               WHEN    "0015"
                   MOVE    "開始日付入力エラー"        TO  CERR-ERRMSG
               WHEN    "0016"
                   MOVE    "転帰日付入力エラー"        TO  CERR-ERRMSG
               WHEN    "0017"
                   MOVE    "日付入力エラー"            TO  CERR-ERRMSG
               WHEN    "0018"
                   MOVE    "疑い入力エラー"            TO  CERR-ERRMSG
               WHEN    "0019"
                   MOVE    "主病名入力エラー"          TO  CERR-ERRMSG
               WHEN    "0020"
                   MOVE    "既に主病名が入力されています"
                                                       TO  CERR-ERRMSG
               WHEN    "0021"
                   MOVE    "慢性区分入力エラー"        TO  CERR-ERRMSG
               WHEN    "0022"
                   MOVE    "入外区分入力エラー"        TO  CERR-ERRMSG
               WHEN    "0023"
                   MOVE    "レセプト表示入力エラー"    TO  CERR-ERRMSG
               WHEN    "0024"
                   MOVE    "転帰区分入力エラー"        TO  CERR-ERRMSG
               WHEN    "0025"
                   MOVE    "転帰年月日が入力してあります"
                                                       TO  CERR-ERRMSG
               WHEN    "0026"
                   MOVE    "転帰年月日が入力してありません"
                                                       TO  CERR-ERRMSG
               WHEN    "0027"
                   MOVE    "開始日付以前の転帰日付が入力してあります"
                                                       TO  CERR-ERRMSG
               WHEN    "0028"
                   MOVE    "保険病名入力エラー"        TO  CERR-ERRMSG
               WHEN    "0029"
                   MOVE    "基本病名が入力してありません"
                                                       TO  CERR-ERRMSG
               WHEN    "0030"
                   MOVE    "診療科入力エラー"
                                               TO  CERR-ERRMSG
               WHEN    "0031"
                   MOVE    "該当の患者が存在しません"
                                               TO  CERR-ERRMSG
               WHEN    "0033"
                   MOVE    "カルテ病名が長すぎます"    TO  CERR-ERRMSG
               WHEN    "0034"
                   STRING  "これ以上追加できません、"
                                               DELIMITED  BY  SIZE
                           "削除一覧を確認し削除処理を行なって下さい"
                                               DELIMITED  BY  SIZE
                                               INTO  CERR-ERRMSG
                   END-STRING
               WHEN    "0035"
                   MOVE    "不明の病名コードが入力されています"
                                               TO  CERR-ERRMSG
               WHEN    "0036"
                   MOVE    "転帰区分が入力してあります"
                                               TO  CERR-ERRMSG
               WHEN    "0037"
                   MOVE    "転帰区分が入力してありません"
                                               TO  CERR-ERRMSG
               WHEN    "0038"
                   STRING
                       WRK-ERR-BYOMEI          DELIMITED   BY  SPACE
                       "は"                    DELIMITED   BY  SIZE 
                       WRK-ERR-SRYYMD          DELIMITED   BY  SPACE
                       "に登録してあります。転帰日等を確認して下さい"
                                               DELIMITED   BY  SIZE 
                                               INTO    CERR-ERRMSG
                   END-STRING
               WHEN    "0039"
                   MOVE
                   "（警告）開始日が未来日で入力されています" 
                                               TO  CERR-ERRMSG
               WHEN    "0040"
                   MOVE    "退院証明書記載フラグ入力エラー"
                                               TO  CERR-ERRMSG
               WHEN    "0041"
                   MOVE    "原疾患区分入力エラー"
                                               TO  CERR-ERRMSG
               WHEN    "0042"
                   MOVE    "合併症区分表示入力エラー"
                                               TO  CERR-ERRMSG
               WHEN    "0043"
                   STRING
                       "退院証明書記載フラグが設定されている他の病名と"
                                               DELIMITED   BY  SIZE 
                       "期間が重複しているので登録できません。"
                                               DELIMITED   BY  SIZE 
                                               INTO    CERR-ERRMSG
                   END-STRING
               WHEN    "0050"
                   MOVE    "患者番号・診療科の入力がありません" 
                                               TO  CERR-ERRMSG
               WHEN    "0051"
                   MOVE    "入力がありません"  TO  CERR-ERRMSG
               WHEN    "0052"
                   MOVE    "自院病名マスタが変更されています"
                                                       TO  CERR-ERRMSG
               WHEN    "0053"
                   MOVE    "自院病名コードがありません" TO  CERR-ERRMSG
               WHEN    "0054"
                   MOVE    "病名コードがありません"    TO  CERR-ERRMSG
               WHEN    "0055"
                   STRING   WRK-ERR-BYOMEI     DELIMITED   BY  SPACE
                            "が複数入力されています"
                                               DELIMITED   BY  SIZE 
                                               INTO    CERR-ERRMSG
                   END-STRING
               WHEN    "0056"
                   MOVE    "変更処理のときは複数病名の入力はできません"
                                                       TO  CERR-ERRMSG
               WHEN    "0057"
                   MOVE    "すべて病名が登録できません"
                                                       TO  CERR-ERRMSG
               WHEN    "0058"
                   MOVE    "複数病名の入力中なので病名検索はできません"
                                                       TO  CERR-ERRMSG
               WHEN    "0059"
                   MOVE    "接頭語しか入力できません"  TO  CERR-ERRMSG
               WHEN    "0060"
                   MOVE
                 "複数の病名を入力中ですので、この項目の編集は無効です"
                                                       TO  CERR-ERRMSG
               WHEN    "0061"
                   MOVE    "自院病名の病名・カルテ病名は変更できません"
                                                       TO  CERR-ERRMSG
               WHEN    "0062"
                   MOVE
               "未コード化傷病名は他のコードといっしょに入力できません"
                                                       TO  CERR-ERRMSG
               WHEN    "0063"
                   MOVE
                 "未コード化傷病名が入力されているので入力できません"
                                                       TO  CERR-ERRMSG
               WHEN    "0064"
                   MOVE    "自院病名が入力されているので入力できません"
                                                       TO  CERR-ERRMSG
               WHEN    "0065"
                 MOVE    "自院病名の入力があるので病名検索はできません"
                                                       TO  CERR-ERRMSG
               WHEN    "0066"
                   MOVE    "複数の病名が入力してあります"
                                                       TO  CERR-ERRMSG
               WHEN    "0067"
                   STRING   WRK-ERR-BYOMEI     DELIMITED   BY  SPACE
                            "は接頭語で病名が終わっています"
                                               DELIMITED   BY  SIZE 
                                               INTO    CERR-ERRMSG
                   END-STRING
               WHEN    "0068"
                   MOVE    "既にこのコードは登録されています"
                                                       TO  CERR-ERRMSG
               WHEN    "0069"
                 MOVE
                 "自院病名以外の入力があるので自院病名検索はできません"
                                                       TO  CERR-ERRMSG
               WHEN    "0070"
                   MOVE    "病名入力コードの長さが違います"
                                                       TO  CERR-ERRMSG
               WHEN    "0071"
                   MOVE    "全角入力して下さい"        TO  CERR-ERRMSG
               WHEN    "0072"
                   MOVE    "全角または半角のみで入力して下さい" 
                                                       TO  CERR-ERRMSG
               WHEN    "0073"
                   MOVE
                   "病名の入力中です。クリアして下さい。"
                                       TO  CERR-ERRMSG
               WHEN    "0074"
                   MOVE
                   "病名を移行すると全ての病名コードを表示できません。"
                                       TO  CERR-ERRMSG
               WHEN    "0075"
                   MOVE    "入力された病名はマスター上で"
                                       TO  CERR-ERRMSG
                   MOVE    WRK-HAISIYMDH
                                       TO  CERR-ERRMSG (29:22)
                   MOVE    "で廃止の扱いとなっています。"
                                       TO  CERR-ERRMSG (51:)
               WHEN    "0076"
                   STRING   WRK-TANDOKUKBN-BYOMEI  DELIMITED  BY  SPACE
                           "は単独使用禁止の病名です"
                                                   DELIMITED  BY  SIZE
                                                   INTO    CERR-ERRMSG
                   END-STRING
               WHEN    "0077"
                   MOVE
              "病名コードが２２個以上になりますので、編集病名とします。"
                                       TO  CERR-ERRMSG
               WHEN    "0078"
                 MOVE    "編集病名なので病名検索はできません"
                                                       TO  CERR-ERRMSG
               WHEN    "0079"
                 MOVE    "最後の部位が（）で始まらない病名です"
                                                       TO  CERR-ERRMSG
               WHEN    "0080"
                 MOVE    "修正項目が未入力です"       TO  CERR-ERRMSG
               WHEN    "0081"
                 MOVE    "病名番号の指定数が多すぎます"  TO  CERR-ERRMSG
               WHEN    "0082"
                 MOVE    "修正項目が多すぎます"        TO  CERR-ERRMSG
               WHEN    "0083"
                 MOVE    "全体表示のときは、全部指定はできません"
                                                       TO  CERR-ERRMSG
               WHEN    "0084"
                 MOVE
                     "入力した転帰日以降の診療開始日の病名が指定"
                                                   TO  CERR-ERRMSG
                 MOVE
                     "してあります"                TO  CERR-ERRMSG (43:)
               WHEN    "0085"
                   STRING
                       WRK-ERR-BYOMEI          DELIMITED   BY  SPACE
                       "は開始日が"            DELIMITED   BY  SIZE 
                       WRK-ERR-SRYYMD          DELIMITED   BY  SPACE
                       "ですので該当の保険適用は範囲外です"
                                               DELIMITED   BY  SIZE 
                                               INTO    CERR-ERRMSG
                   END-STRING
               WHEN    "0086"
                   MOVE    "患者番号の入力がありません" 
                                                   TO  CERR-ERRMSG
               WHEN    "0087"
                   MOVE   "移動処理中の診療科、開始日の変更はできません"
                                               TO  CERR-ERRMSG
               WHEN    "0088"
                   MOVE
                   "指定された診療科の開始日にはこれ以上追加できません"
                                               TO  CERR-ERRMSG
               WHEN    "0089"
                   MOVE
                   "指定された診療科の開始日にはこれ以上追加できません"
                                               TO  CERR-ERRMSG
               WHEN    "0090"
                   MOVE    "簡単修正処理中です"    TO  CERR-ERRMSG   
               WHEN    "0091"
                   STRING
                       "存在しない病名コードが登録されています、"
                                                DELIMITED   BY  SIZE 
                       "ＣＤ表示で確認して下さい"
                                                DELIMITED   BY  SIZE 
                                               INTO    CERR-ERRMSG
                   END-STRING
               WHEN    "0092"
                   MOVE    "補足コメントの前後に（）があります"
                                               TO  CERR-ERRMSG   
               WHEN    "0093"
                   MOVE    "補足コメントが入力の途中です"
                                               TO  CERR-ERRMSG   
               WHEN    "0094"
                   STRING
                     "複数病名の入力時は補足コメントの入力はできません"
                                                DELIMITED   BY  SIZE 
                       "ので、クリアします"     DELIMITED   BY  SIZE 
                                               INTO    CERR-ERRMSG
                   END-STRING
               WHEN    "0095"
                   MOVE    "補足コメントが長すぎます"
                                               TO  CERR-ERRMSG
               WHEN    "0096"
                   MOVE    "誕生日前の開始日は入力できません"
                                               TO  CERR-ERRMSG
               WHEN    "0097"
                   MOVE    "セットコードの病名を全て表示できません"
                                               TO  CERR-ERRMSG
               WHEN    "0098"
                   MOVE
                       "変更処理のときはセットコードの入力はできません"
                                                       TO  CERR-ERRMSG
               WHEN    "0099"
                   MOVE
                       "補足コメントにセットコードは入力できません"
                                                       TO  CERR-ERRMSG
               WHEN    "0100"
                   MOVE    "該当の第三者行為がありません" 
                                               TO  CERR-ERRMSG
               WHEN    "0101"
                   MOVE
                       "開始日が該当の第三者行為の適用日の範囲外です"
                                               TO  CERR-ERRMSG
               WHEN    "0102"
                   MOVE
                       "保険組合せと第三者行為の両方の入力はできません"
                                               TO  CERR-ERRMSG
               WHEN    "0103"
                   STRING  "病名コードでの組み立てができませんでした。"
                                               DELIMITED  BY  SIZE
                           "編集病名とします"  DELIMITED  BY  SIZE
                                               INTO    CERR-ERRMSG
                   END-STRING
               WHEN    "0104"
                   STRING
                       "この病名は「急性」と「慢性」が混在して"
                                                DELIMITED   BY  SIZE 
                       "レセプトに記載されます。確認して下さい。"
                                                DELIMITED   BY  SIZE 
                                               INTO    CERR-ERRMSG
                   END-STRING
               WHEN    "1001"
                   MOVE    "更新処理でエラーになりました"
                                                       TO  CERR-ERRMSG
               WHEN    "1002"
                   MOVE    "削除処理でエラーになりました"
                                                       TO  CERR-ERRMSG
               WHEN    "1003"
                   MOVE    "患者病名マスタが存在しません"
                                                       TO  CERR-ERRMSG
               WHEN    "1004"
                   MOVE    "患者マスタの更新処理でエラーになりました"
                                                       TO  CERR-ERRMSG
               WHEN    "1005"
                   MOVE    "自院病名の更新処理でエラーになりました"
                                                       TO  CERR-ERRMSG
               WHEN    "1006"
                   MOVE
                  "レセプト作成管理マスタの更新処理でエラーになりました"
                                                       TO  CERR-ERRMSG
      *
               WHEN    "9999"
                   MOVE    "他端末で使用中です。"
                                               TO  CERR-ERRMSG
                   MOVE    SLOCK-MSG           TO  CERR-ERRMSG(21:)
           END-EVALUATE
      *
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "C02"                TO  SPA-MOTOPG
           MOVE    "CERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "CERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           MOVE    SPACE                TO  WRK-HAISIYMDH
                                            WRK-ERR-BYOMEI
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面表示処理
      *****************************************************************
       520-CIDSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-CIDMSG
      *
           EVALUATE    SPA-CIDCD
               WHEN    "0101"
                   MOVE    "内容が修正されています。"
                                       TO  WRK-CIDMSG
                   MOVE    "ＯＫで修正分は反映されずに終了します"
                                       TO  WRK-CIDMSG (25:)
               WHEN    "0102"
                   MOVE    "主病名がありません。更新しますか？"
                                       TO  WRK-CIDMSG
               WHEN    "0103"
               WHEN    "0106"
                   STRING
                       "この病名は"            DELIMITED   BY  SIZE 
                       SPA-C02-KEIKOKU-SRYKA   DELIMITED   BY  SPACE
                       "に"                    DELIMITED   BY  SIZE 
                       SPA-C02-KEIKOKU-SRYYMD  DELIMITED   BY  SPACE
                       "に登録してあります。更新しますか？"
                                               DELIMITED   BY  SIZE 
                                               INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0104"
                   STRING
                       "この病名は「急性」と「慢性」が混在して"
                                                DELIMITED   BY  SIZE 
                       "レセプトに記載されます。更新しますか？"
                                                DELIMITED   BY  SIZE 
                                               INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0105"
               WHEN    "0107"
                   STRING  "転帰区分の設定により患者情報の死亡区分を"
                                                DELIMITED   BY  SIZE 
                           "「死亡」として更新します。よろしいですか"
                                                DELIMITED   BY  SIZE 
                                               INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0108"
               WHEN    "0109"
                   STRING  "転帰区分の変更により患者情報の死亡区分を"
                                                DELIMITED   BY  SIZE 
                           "「生存」として更新します。よろしいですか"
                                                DELIMITED   BY  SIZE 
                                               INTO    WRK-CIDMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-ERRCD   TO  WRK-CIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-CID1-FLG
      *
           MOVE    SPACE               TO  CID1
           INITIALIZE                      CID1
           MOVE    SPA-CIDCD           TO  CID1-ID1CODE
           MOVE    WRK-CIDMSG          TO  CID1-ID1MSG
           EVALUATE    SPA-CIDCD
               WHEN    "0103"
               WHEN    "0104"
               WHEN    "0106"
                   MOVE    SPA-C02-KEIKOKU-BYOMEI  TO  CID1-BYOMEI
           END-EVALUATE 
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "C02"               TO  SPA-MOTOPG
           MOVE    "CID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "CID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-CIDSET-EXT.
           EXIT.
      *----(01.00.02) LINE ADD END   ----------------------------------
      *
      *****************************************************************
      *    病名移行確認画面表示処理
      *****************************************************************
       530-CIDSET2-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-CID2-FLG
                                           WRK-CIDMSG
      *
           EVALUATE    SPA-CIDCD2
               WHEN    "0101"
               WHEN    "0103"
                   STRING  "入力された病名はマスター上で移行"
                                       DELIMITED  BY  SIZE
                           "病名の扱いとなっています。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0104"
                   STRING  "入力された自院病名の病名はマスタ"
                                       DELIMITED  BY  SIZE
                           "ー上で移行病名の扱いとなっています。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0102"
               WHEN    "0105"
                   STRING  "入力された病名は「傷病名コードの統一の"
                                       DELIMITED  BY  SIZE
                           "推進」による推奨病名があります。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0106"
                   STRING  "入力された自院病名の病名は「傷病名コードの"
                                       DELIMITED  BY  SIZE
                           "統一の推進」による推奨病名があります。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-CIDMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE               TO  CID2
           INITIALIZE                      CID2
           MOVE    WRK-CIDMSG          TO  CID2-MSG
           MOVE    SPA-C02-IKOMOTO-BYOMEICD
                                       TO  CID2-BYOMEICD1
           MOVE    SPA-C02-IKOMOTO-BYOMEI  
                                       TO  CID2-BYOMEI1
           MOVE    SPA-C02-IKOSAKI-BYOMEICD
                                       TO  CID2-BYOMEICD2
           MOVE    SPA-C02-IKOSAKI-BYOMEI
                                       TO  CID2-BYOMEI2
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "C02"               TO  SPA-MOTOPG
           MOVE    "CID2"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "CID2"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       530-CIDSET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名移行確認画面表示処理
      *****************************************************************
       540-CIDSET4-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-CID4-FLG
      *
           EVALUATE    SPA-CIDCD4
               WHEN    "0101"
                   MOVE    "選択された病名を削除しますか？"
                                       TO  WRK-CIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  CID4
           INITIALIZE                      CID4
           MOVE    SPA-CIDCD4          TO  CID4-ID1CODE
           MOVE    WRK-CIDMSG          TO  CID4-ID1MSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "C02"               TO  SPA-MOTOPG
           MOVE    "CID4"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "CID4"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       540-CIDSET4-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名移行確認画面表示処理
      *****************************************************************
       560-CIDSET6-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-CID6-FLG
                                       WRK-CIDMSG
      *
           INITIALIZE                  CID6
      *
           EVALUATE    SPA-CIDCD6
               WHEN    "0101"
               WHEN    "0103"
               WHEN    "0111"
                   STRING
                       "入力された病名は"      DELIMITED   BY  SIZE 
                       "移行病名の扱いとなっていますが、"
                                               DELIMITED   BY  SIZE 
                      "別のコードで登録が可能です"
                                               DELIMITED   BY  SIZE
                                               INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0104"
                   STRING
                       "入力された自院病名の病名は" 
                                               DELIMITED   BY  SIZE 
                       "移行病名の扱いとなっていますが、"
                                               DELIMITED   BY  SIZE 
                      "別のコードで登録が可能です"
                                               DELIMITED   BY  SIZE
                                               INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0102"
               WHEN    "0105"
               WHEN    "0112"
                   STRING  "入力された病名は「傷病名コードの統一の推進"
                                               DELIMITED  BY  SIZE
                   "」による推奨病名がありますが、別のコードで登録が"
                                               DELIMITED   BY  SIZE
                   "可能です"                  DELIMITED   BY  SIZE
                                               INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0106"
                   STRING  "入力された自院病名の病名は「傷病名コードの"
                                               DELIMITED   BY  SIZE
                   "統一の推進」による推奨病名がありますが、別のコード"
                                               DELIMITED   BY  SIZE
                   "で登録が可能です"          DELIMITED   BY  SIZE
                                               INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0107"
               WHEN    "0117"
                   STRING
                       "入力された病名は"      DELIMITED   BY  SIZE 
                       "廃止の扱いとなっていますが、"
                                               DELIMITED   BY  SIZE 
                       "別のコードで登録が可能です"
                                               DELIMITED   BY  SIZE 
                                               INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0109"
                   STRING
                       "入力された自院病名の病名は"
                                               DELIMITED   BY  SIZE 
                       "廃止の扱いとなっていますが、"
                                               DELIMITED   BY  SIZE 
                       "別のコードで登録が可能です"
                                               DELIMITED   BY  SIZE 
                                               INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0108"
                   STRING
                       "入力された自院病名の病名は"
                                               DELIMITED   BY  SIZE 
                       "別のコードで登録が可能です"
                                               DELIMITED   BY  SIZE 
                                               INTO    WRK-CIDMSG
                   END-STRING
           END-EVALUATE
           MOVE    WRK-CIDMSG          TO  CID6-MSG
      *
           MOVE    SPA-C02-IKOMOTO-BYOMEICD
                                       TO  CID6-BYOMEICD1
           MOVE    SPA-C02-IKOMOTO-BYOMEI  
                                       TO  CID6-BYOMEI1
      *
           EVALUATE    SPA-CIDCD6
               WHEN    "0108"
                   MOVE    WIDGET-INSENSITIVE  TO  CID6-B10-STATE
                   MOVE    "別のコードでの登録を行いますか"
                                               TO  CID6-MSG1 
               WHEN    "0107"
               WHEN    "0109"
               WHEN    "0117"
                   MOVE    WIDGET-INSENSITIVE  TO  CID6-B10-STATE
                   MOVE    "別のコードでの登録を行いますか"
                                               TO  CID6-MSG1 
                   MOVE    "廃止年月日"        TO  CID6-HAISIYMD-H
                   MOVE    WRK-HAISIYMDH       TO  CID6-HAISIYMD
               WHEN    OTHER
                   MOVE    SPA-C02-IKOSAKI-BYOMEICD
                                               TO  CID6-BYOMEICD2
                   MOVE    SPA-C02-IKOSAKI-BYOMEI
                                               TO  CID6-BYOMEI2
                   MOVE    WIDGET-NORMAL       TO  CID6-B10-STATE
                   IF      SPA-C02-IKOMOTO-HAISIYMD
                                               >   ZERO
                       MOVE    "廃止年月日"        TO  CID6-HAISIYMD-H
                       MOVE    SPA-C02-IKOMOTO-HAISIYMD
                                                   TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    LNK-DAY2-EDTYMD3    TO  CID6-HAISIYMD
                   END-IF
                   EVALUATE    SPA-CIDCD6
                       WHEN    "0102"
                       WHEN    "0105"
                       WHEN    "0106"
                       WHEN    "0112"
                           STRING
                               "推奨する病名へ変更しますか、"
                                                   DELIMITED   BY  SIZE  
                               "別のコードでの登録を行いますか"
                                                   DELIMITED   BY  SIZE  
                                                   INTO  CID6-MSG1
                           END-STRING 
                           MOVE    "推奨する病名"  TO  CID6-LABEL1
                           MOVE    "F10:推奨する病名へ変更"
                                                   TO  CID6-B10-LABEL
                       WHEN    OTHER
                           STRING
                               "移行先病名へ変更しますか、"
                                                   DELIMITED   BY  SIZE  
                               "別のコードでの登録を行いますか"
                                                   DELIMITED   BY  SIZE  
                                                   INTO  CID6-MSG1
                           END-STRING 
                           MOVE    "移行先病名"    TO  CID6-LABEL1
                           MOVE    "F10:移行先病名へ変更"
                                                   TO  CID6-B10-LABEL
                   END-EVALUATE
           END-EVALUATE
      *
           MOVE    SPACE               TO  WRK-HAISIYMDH
      *
           MOVE    "B12"               TO  MCP-WIDGET
           MOVE    WIDGET-NORMAL       TO  CID6-B12-STATE
           MOVE    "red"               TO  CID6-MSG2-STYLE
           EVALUATE    SPA-CIDCD6
               WHEN    "0104"
               WHEN    "0106"
               WHEN    "0109"
               WHEN    "0108"
      *            自院病名コード
                   CONTINUE
               WHEN    OTHER
      *            病名コード
                   IF    ( SPA-NAI-C02-RENNUM          =   ZERO )
                   AND   ( SPA-NAI-C02-BYOMEICD-MAX    >   1    )
      *               複数病名のとき
                       MOVE   ZERO             TO  IDX
                       PERFORM VARYING IDXX    FROM    1   BY  1
                               UNTIL   IDXX    >       6
                           IF      SPA-GMN-C02-BYOMEICD   (IDXX)
                                                   NOT =   SPACE
                               ADD     1            TO  IDX 
                           END-IF
                       END-PERFORM
                       COMPUTE IDX     =   IDX
                                       -   SPA-C02-CHANGE-INDEX-MAX
                                       +   SPA-C02-CHANGE-MAX             
                       IF    ( SPA-C02-CHANGE-MAX  >   6  )
                       OR    ( IDX                 >   6  ) 
                           STRING  "別のコードで登録を行いたい場合、"
                                               DELIMITED   BY  SIZE 
                               "置換病名コードが表示できないため"
                                               DELIMITED   BY  SIZE 
                               "該当の病名を単独で入力して登録処理を"
                                               DELIMITED   BY  SIZE 
                               "行って下さい"  DELIMITED   BY  SIZE 
                                       INTO    CID6-MSG2
                           END-STRING
                           MOVE    WIDGET-INSENSITIVE
                                                   TO  CID6-B12-STATE
                           MOVE    "B10"           TO  MCP-WIDGET
                       END-IF
                   ELSE
                       IF      SPA-C02-CHANGE-MAX  >   6
                           STRING  "別のコードで登録を行う場合は"
                                               DELIMITED   BY  SIZE 
                                   "病名コード欄に表示を行いません"
                                               DELIMITED   BY  SIZE 
                                       INTO    CID6-MSG2
                           END-STRING
                       END-IF
                   END-IF
           END-EVALUATE
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX     >   6
               MOVE    SPA-C02-CHANGE-BYOMEICD (IDX)
                                       TO  CID6-BYOMEICD (IDX)
               MOVE    "black"         TO  CID6-BYOMEICD-STYLE (IDX)
               IF      SPA-C02-CHANGE-MAX  <=  6
                   EVALUATE    IDX
                       WHEN    1
                       WHEN    3
                       WHEN    5
                           MOVE    "black" TO  CID6-BYOMEICD-STYLE (IDX)
                       WHEN    OTHER
                           MOVE    "blue"  TO  CID6-BYOMEICD-STYLE (IDX)
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  WRK-BYOMEI1
           MOVE    "black"             TO  CID6-BYOMEI10-STYLE
           IF      SPA-C02-CHANGE-MAX  <=  6
               STRING  SPA-C02-CHANGE-BYOMEI (1)
                                           DELIMITED   BY  SPACE
                       SPAN-COLOR-BLUE     DELIMITED   BY  SIZE
                       SPA-C02-CHANGE-BYOMEI (2)
                                           DELIMITED   BY  SPACE
                       SPAN-COLOR-END      DELIMITED   BY  SIZE
                       SPA-C02-CHANGE-BYOMEI (3)
                                           DELIMITED   BY  SPACE
                       SPAN-COLOR-BLUE     DELIMITED   BY  SIZE
                       SPA-C02-CHANGE-BYOMEI (4)
                                           DELIMITED   BY  SPACE
                       SPAN-COLOR-END      DELIMITED   BY  SIZE
                       SPA-C02-CHANGE-BYOMEI (5)
                                           DELIMITED   BY  SPACE
                       SPAN-COLOR-BLUE     DELIMITED   BY  SIZE
                       SPA-C02-CHANGE-BYOMEI (6)
                                           DELIMITED   BY  SPACE
                       SPAN-COLOR-END      DELIMITED   BY  SIZE
                                           INTO    WRK-BYOMEI1
               END-STRING
           ELSE    
               MOVE    SPA-C02-CHANGE-BYOMEI (1)
                                           TO  WRK-BYOMEI1
               PERFORM VARYING IDX FROM    2   BY  1
                       UNTIL   IDX >       SPA-C02-CHANGE-MAX
                   STRING  WRK-BYOMEI1         DELIMITED   BY  SPACE
                           SPA-C02-CHANGE-BYOMEI (IDX)
                                               DELIMITED   BY  SPACE
                                               INTO    WRK-BYOMEI1
                   END-STRING
               END-PERFORM
           END-IF    
           MOVE    WRK-BYOMEI1         TO  CID6-BYOMEI10
      *
           MOVE    SPACE               TO  WRK-BYOMEI
           PERFORM VARYING IDX FROM    SPA-C02-CHANGE-INDEX-ST
                                       BY  1
                   UNTIL   IDX >       SPA-C02-CHANGE-INDEX-ED
               STRING  WRK-BYOMEI          DELIMITED   BY  SPACE
                       SPA-NAI-C02-BYOMEI(IDX)
                                           DELIMITED   BY  SPACE
                                           INTO    WRK-BYOMEI
                END-STRING
           END-PERFORM
           MOVE    WRK-BYOMEI          TO  CID6-H-BYOMEI1
           MOVE    "blue"              TO  CID6-H-BYOMEI1-STYLE
      *
           EVALUATE    SPA-CIDCD6
               WHEN    "0107"
               WHEN    "0108"
               WHEN    "0109"
               WHEN    "0117"
                   CONTINUE
               WHEN    "0104"
               WHEN    "0106"
                   MOVE    SPACE               TO  WRK-BYOMEI
                   PERFORM VARYING IDX FROM    SPA-C02-CHANGE-INDEX-ST
                                               BY  1
                           UNTIL   IDX >       SPA-C02-CHANGE-INDEX-ED
                       IF      IDX         =   SPA-C02-IKOSAKI-INDEX
                           PERFORM VARYING IDX2    FROM    1   BY  1
                                   UNTIL   IDX2    >       21
                                   OR      SPA-NAI-C02-BYOMEICD-O
                                                           (IDX IDX2)
                                               =   SPACE
                               IF      IDX2        =
                                                SPA-C02-IKOSAKI-INDEX1
                                   STRING  WRK-BYOMEI
                                                   DELIMITED   BY  SPACE
                                           SPA-C02-IKOSAKI-BYOMEI
                                                   DELIMITED   BY  SPACE
                                                   INTO    WRK-BYOMEI
                                   END-STRING
                               ELSE
                                   STRING  WRK-BYOMEI
                                                   DELIMITED   BY  SPACE
                                           SPA-NAI-C02-BYOMEI-O
                                                           (IDX IDX2)
                                                   DELIMITED   BY  SPACE
                                                   INTO    WRK-BYOMEI
                                   END-STRING
                               END-IF 
                           END-PERFORM
                       ELSE
                           STRING  WRK-BYOMEI      DELIMITED   BY  SPACE
                                   SPA-NAI-C02-BYOMEI (IDX)
                                                   DELIMITED   BY  SPACE
                                                   INTO    WRK-BYOMEI
                           END-STRING
                       END-IF  
                   END-PERFORM
                   MOVE    WRK-BYOMEI          TO  CID6-H-BYOMEI2
               WHEN    OTHER
                   MOVE    SPACE               TO  WRK-BYOMEI
                   PERFORM VARYING IDX FROM    SPA-C02-CHANGE-INDEX-ST
                                               BY  1
                           UNTIL   IDX >       SPA-C02-CHANGE-INDEX-ED
                       IF      IDX         =   SPA-C02-IKOSAKI-INDEX
                           STRING  WRK-BYOMEI      DELIMITED   BY  SPACE
                                   SPA-C02-IKOSAKI-BYOMEI
                                                   DELIMITED   BY  SPACE
                                                   INTO    WRK-BYOMEI
                           END-STRING
                       ELSE
                           STRING  WRK-BYOMEI      DELIMITED   BY  SPACE
                                   SPA-NAI-C02-BYOMEI (IDX)
                                                   DELIMITED   BY  SPACE
                                                   INTO    WRK-BYOMEI
                           END-STRING
                       END-IF  
                   END-PERFORM
                   MOVE    WRK-BYOMEI          TO  CID6-H-BYOMEI2
           END-EVALUATE
           MOVE    "blue"              TO  CID6-H-BYOMEI2-STYLE
      *
           MOVE    "C02"               TO  SPA-MOTOPG
           MOVE    "CID6"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "CID6"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       560-CIDSET6-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    感染症送信処理
      *****************************************************************
       690-INFECTION-SEND-SEC          SECTION.
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    INFECTION-FILE      TO  SHELLTBL-NAME
           MOVE    "C02"               TO  SHELLTBL-ARG1
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG2
           MOVE    SPA-HOSPID          TO  SHELLTBL-ARG3
           MOVE    SPACE               TO  SHELLTBL-ARG4
           MOVE    SPA-NYUGAIKBN       TO  SHELLTBL-ARG5
           MOVE    SPA-PTID            TO  WRK-PTID
           MOVE    WRK-PTID            TO  SHELLTBL-ARG6
           MOVE    SPA-SRYYMD          TO  SHELLTBL-ARG9
           MOVE    SPA-SRYKA           TO  SHELLTBL-ARG8
           MOVE    SPA-BEDFLG          TO  SHELLTBL-ARG9
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "daily"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       690-INFECTION-SEND-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスター読込（主キー）
      *****************************************************************
       900-PTBYO-INV-SEC         SECTION.
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
               INITIALIZE                  PTBYOMEI-REC
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTBYO-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスター読込
      *****************************************************************
       900-PTBYO-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
               MOVE    ZERO            TO  FLG-PTBYOMEI
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           .
       900-PTBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       900-COMB-INV-SEC                SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-COMB-READ-SEC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-COMB-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       900-COMB-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       900-COMB-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名マスター読込
      *****************************************************************
       900-BYOMEI-READ-SEC         SECTION.
      *
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_byomei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-BYOMEI-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名マスター読込
      *****************************************************************
       900-BYOMEI-READ-N-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-BYOMEI
               MOVE    MCPDATA-REC     TO  BYOMEI-REC
           ELSE
              MOVE    1               TO  FLG-BYOMEI
              INITIALIZE                  BYOMEI-REC
           END-IF
           .
       900-BYOMEI-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名マスター読込
      *****************************************************************
       900-USERBYOMEI-INV-SEC         SECTION.
      *
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_userbyomei"    TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 900-USBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-USERBYOMEI
               INITIALIZE                  USERBYOMEI-REC
           END-IF
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-USERBYOMEI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名マスター読込
      *****************************************************************
       900-USBYO-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  USERBYOMEI-REC
               MOVE    ZERO            TO  FLG-USERBYOMEI
           ELSE
               INITIALIZE                  USERBYOMEI-REC
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
      *
           .
       900-USBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       900-JYURRK-INV-SEC         SECTION.
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  JYURRK-REC
                   MOVE    ZERO                TO  FLG-JYURRK
               ELSE
                   MOVE    1                   TO  FLG-JYURRK
               END-IF
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC   
      *
           .
       900-JYURRK-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *     
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    同義語マスタ読込
      *****************************************************************
       900-SYNONYM-READ-SEC         SECTION.
      *
           MOVE    SYNONYM-REC         TO  MCPDATA-REC
           MOVE    "tbl_synonym"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_synonym"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYNONYM-REC
                   MOVE    ZERO            TO  FLG-SYNONYM
               ELSE
                   INITIALIZE                  SYNONYM-REC
                   MOVE    1               TO  FLG-SYNONYM
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYNONYM
               INITIALIZE                      SYNONYM-REC
           END-IF
      *
           MOVE    "tbl_synonym"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYNONYM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名入力セット読込
      *****************************************************************
       900-BYOMEIINPUTSET-READ-SEC     SECTION.
      *
           MOVE    "tbl_byomeiinputset"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_byomeiinputset"
                                           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-BYOMEIINPUTSET-READ-N-SEC
           ELSE
               INITIALIZE                      BYOMEIINPUTSET-REC
               MOVE    1                   TO  FLG-BYOMEIINPUTSET
           END-IF
      *
           MOVE    "tbl_byomeiinputset"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BYOMEIINPUTSET-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名入力セット読込
      *****************************************************************
       900-BYOMEIINPUTSET-READ-N-SEC   SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  BYOMEIINPUTSET-REC
               MOVE    ZERO                TO  FLG-BYOMEIINPUTSET
           ELSE
               INITIALIZE                      BYOMEIINPUTSET-REC
               MOVE    1                   TO  FLG-BYOMEIINPUTSET
           END-IF
      *
           .
       900-BYOMEIINPUTSET-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読込
      *****************************************************************
       900-PTRSIINF-READ-N-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           .
       900-PTRSIINF-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-NAI-C02-PTID    TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    排他制御解除処理
           MOVE    2                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
