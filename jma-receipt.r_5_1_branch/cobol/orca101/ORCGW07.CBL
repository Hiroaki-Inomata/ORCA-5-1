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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGW07.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 自動算定情報（Ｗ０７）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/06/05  薬剤情報提供料の追加
      *  01.00.02    MCC-藤原     01/09/11  削除処理の修正 
      *  01.01.01    MCC-藤原     01/09/21  JOINの修正      
      *  01.01.02    MCC-藤原     01/11/26  項目をコンボボックスに変更
      *  01.02.01    MCC-森脇     02/05/23  チェック機能制御情報の追加
      *  01.02.02    NACL-多々納  02/10/10  外来管理加算チェック追加
      *  01.02.03    NACL-多々納  02/10/31  外来管理加算チェック区分追加
      *  01.02.04    NACL-多々納  02/11/13  一括入力 
      *  01.02.05    NACL-多々納  02/11/20  チェック機能制御初期設定変更
      *  01.02.05    NACL-多々納  03/02/19  療養担当手当て追加
      *  01.02.06    NACL-多々納  03/03/14  特定薬剤治療管理料追加
      *  02.01.01    NACL-森脇    03/10/17  画面変更につき一括変更
      *  02.01.02    NACL-多々納  03/11/07  入院初診料追加
      *  02.01.03    NACL-多々納  04/01/09  包括登録追加
      *  02.01.04    NACL-多々納  04/01/15  数量ゼロ入力追加
      *  02.01.05    NACL-太田    04/01/16  診療行為機能情報タグ追加
      *  02.01.06    NACL-多々納  04/01/23  入院調基チェック追加
      *  02.01.07    NACL-太田    04/01/23  収納機能情報タグ追加
      *  02.01.08    NACL-多々納  04/03/08  時間外加算特例乳幼児他追加
      *  02.01.09    NACL-藤原    04/03/08  生保受給者番号の追加
      *  02.01.10    NACL-多々納  04/03/10  時間外加算乳幼児チェック追加
      *  02.01.11    NACL-藤原    04/03/08  生保受給者番号の機能停止
      *  02.01.12    NACL-多々納  04/07/05  最終受診日から初診の期間追加
      *                                     請求書発行方法他追加
      *  02.01.13    NACL-藤原    04/07/08  生保受給者番号を削除
      *  02.01.14    NACL-多々納  04/10/06  同姓同名者、二重登録疑い追加
      *  02.01.15    NACL-藤原    04/11/15  病名機能情報を追加
      *  02.01.16    NACL-多々納  04/12/06  診療行為クリア確認追加
      *  02.01.17    NACL-太田    04/01/07  収納機能情報−請求書発行
      *  02.01.18    NACL-藤原    05/04/20  移行病名警告メッセージ表示
      *                                     有無区分
      *                                     廃止年月日警告メッセージ表示
      *                                     有無区分
      *                                     診療科初期表示区分
      *                                     表示対象期間初期表示区分
      *                                     同一病名警告メッセージ表示
      *                                     有無区分
      *                                     の追加
      *  02.01.19    NACL-多々納  05/06/06  中途終了展開時の自動発生
      *                                     薬剤情報提供料チェック追加
      *  02.01.20    NACL-多々納  05/07/06  訂正時の自動発生追加
      *  02.01.21    NACL-太田    05/07/26  収納明細登録時刻入力欄追加
      *  02.01.22    NACL-多々納  05/08/19  会計照会情報追加
      *  02.01.21    NACL-太田    05/09/09  収納明細登録仮締め時刻入力欄追加
      *  02.01.22    NACL-多々納  05/12/21  病名表示区分
      *                                     剤登録順区分追加
      *                                     MONFUNC 対応
      *  02.01.23    NACL-多々納  06/01/20  会計照会 診療科初期表示追加
      *  02.01.24    NACL-多々納  06/02/08  地方公費単独チェックの追加
      *  02.01.25    NACL-多々納  06/05/19  患者削除機能チェックの追加
      *  02.01.26    NACL-太田    06/08/16  収納機能情報
      *                                     請求書兼領収書発行区分追加
      *  03.02.00    NACL-多々納  06/08/29  レセコメント詳記区分の追加
      *  03.04.00    NACL-多々納  06/11/13  検査正式名称表示区分の追加
      *  03.05.00    NACL-多々納  07/05/02  グループ診療対応
      *  03.05.00    NACL-多々納  07/06/22  本人・家族区分選択追加
      *  03.05.01    NACL-多々納  07/07/XX  同日再診・訂正追加
      *                                     皮下筋肉注射料変換区分追加
      *  03.05.01    NACL-太田    07/08/03  月別請求書前回未収額印字区分追加
      *  03.05.01    NACL-多々納  07/08/XX  訂正の請求書金額区分追加
      *  03.05.02    NACL-多々納  07/09/07  前回保険組合せ相違区分追加
      *  04.01.00    NACL-多々納  07/11/25  腫瘍マーカー一覧区分追加他
      *  04.01.00    NACL-多々納  07/12/XX  訂正時の自動発生（入院）
      *                                     病名疾患区分からの自動発生
      *                                     投薬診療種別自動変換
      *                                     初診料の警告対応
      *  04.03.00    NACL-多々納  08/06/XX  家族年齢、被保険者名区分追加
      *  04.03.00    NACL-多々納  08/06/18  予約機能情報追加
      *  04.03.00    NACL-太田    08/06/19  支払証明書印刷初期設定追加
      *  04.03.00    NACL-多々納  08/07/04  お薬手帳発行追加
      *  04.03.00    NACL-多々納  08/07/24  入院時初回加算自動発生追加
      *  04.03.00    NACL-太田    08/09/09  支払証明書印刷初期設定追加の追加
      *  04.03.00    NACL-多々納  08/10/30  世帯主名自動記載区分追加
      *  04.04.00    NACL-多々納  09/01/27  グループ医療機関氏名リスト追加
      *  04.05.00    NACL-太田    09/06/15  請求一覧画面並び順設定追加
      *                                     一括計算順追加
      *                                     一括計算方法追加
      *  04.05.00    NACL-多々納  09/08/28  公費上限額履歴チェック追加
      *  04.05.00    NACL-太田    09/09/04  月別請求書発行方法、集計区分追加
      *  04.05.00    NACL-多々納  09/10/16  初診再診料自動算定区分追加他
      *  04.05.00    NACL-多々納  09/11/20  郵便番号自動記載区分追加他
      *
      *  04.06.01    NACL-藤原    10/06/16  疑いフラグ設定対応
      *                                     疑いフラグ設定区分の追加
      *  04.06.00    NACL-多々納  10/06/21  予約・受付連動など追加
      *  04.06.00    NACL-多々納  10/07/01  残量廃棄自動区分追加
      *  04.06.00    NACL-多々納  10/08/17  手帳記載加算確認追加
      *  04.06.00    NACL-多々納  10/09/17  精神療法２０未満加算追加
      *  04.06.00    NACL-多々納  11/02/09  京都府通り名編集区分追加
      *  04.07.00    NACL-多々納  11/06/XX  受付保険組合せ区分追加
      *  04.07.00    NACL-多々納  11/07/XX  公費法別一覧表示区分追加
      *                                     リハビリ等表示区分追加
      *  04.07.00    NACL-多々納  12/01/26  入力スクロール等背景色追加
      *  04.07.00    NACL-多々納  12/05/08  確認画面処方名称区分追加
      *  04.07.00    NACL-太田    12/07/04  再計算該当者チェック設定追加
      *  04.07.00    NACL-多々納  13/08/01  時間外緊急院内チェック区分追加
      *  04.07.00    NACL-多々納  14/12/08  点数算定基準追加
      *  04.07.00    NACL-多々納  15/03/XX  低紹介率追加
      *  04.07.00    NACL-多々納  15/06/11  保険初期表示区分追加
      *
      *  04.08.01    NACL-藤原    16/01/20  転帰区分の死亡変更時の患者
      *                                     情報更新対応
      *                                     死亡区分更新確認メッセージ表示
      *                                     有無区分の追加
      *  04.08.01    NACL-多々納  16/06/10  セット展開区分追加
      *  04.08.01    NACL-太田    16/08/05  収納請求一覧初期表示追加
      *  04.08.01    NACL-多々納  16/10/04  要介護情報表示区分追加
      *  04.08.00    NACL-多々納  16/11/XX  投薬投与量チェック区分対応
      *  04.08.00    NACL-多々納  16/12/XX  病名表示区分に追加対応
      *  04.08.00    NACL-多々納  17/03/XX  訂正時の自動発生（処方料）追加
      *  04.08.00    NACL-多々納  17/04/XX  投薬３０日超チェック追加
      *  04.08.00    NACL-多々納  17/07/XX  心身療法２０未満加算区分追加
      *  04.08.00    NACL-多々納  18/03/XX  Ｈ３０年４月改正対応
      *  04.08.00    NACL-多々納  18/09/XX  選択式コメント一覧区分対応
      *  05.00.00    NACL-多々納  19/12/XX  予約のドクター一覧全件対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-W07.
           03  SPA-W07-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(03).
               05  SPA-GMN-CUR         PIC 9(03).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-STYUKYMD    PIC X(09).
                   07  SPA-GMN-EDYUKYMD    PIC X(09).
                   07  SPA-GMN-PAGENO      PIC 9(01).
      *
                   07  SPA-GMN-PAGE1-AREA.
      *                自動算定チェック
                       09  SPA-GMN-DATA-TBL            OCCURS  7.
                           11  SPA-GMN-DATA-G.
                               13  SPA-GMN-DATA        PIC X(01).
                               13  SPA-GMN-F1          PIC X(01).
                               13  SPA-GMN-DATAMEI     PIC X(20).
      *
      *                自動算定科
                       09  SPA-GMN-SRYKACD             PIC X(02).
                       09  SPA-GMN-SRYKAMEI            PIC X(20).
      *                入院初診料
                       09  SPA-GMN-NYUSYOSIN-G.
                           11  SPA-GMN-NYUSYOSIN       PIC X(01).
                           11  SPA-GMN-FN              PIC X(01).
                           11  SPA-GMN-NYUSYOSINMEI    PIC X(10).
      *
      *                時間外加算特例乳幼児
                       09  SPA-GMN-JIKANTOKU-NYU-G.
                           11  SPA-GMN-JIKANTOKU-NYU   PIC X(01).
                           11  SPA-GMN-FN              PIC X(01).
      **                   11  SPA-GMN-JIKANTOKU-NYUMEI    PIC X(10).
                           11  SPA-GMN-JIKANTOKU-NYUMEI    PIC X(28).
      *H30.4
      *                産婦人科特例対応
                       09  SPA-GMN-JIKANTOKU-NYU-COMB.
                           11  SPA-GMN-JIKANTOKU-NYU-LIST  PIC X(30)
                                                       OCCURS   9.
                       09  SPA-GMN-JIKANTOKU-MAX       PIC 9(02).
      *
      *                画像診断管理加算１
                       09  SPA-GMN-GAZOUKSN1-G.
                           11  SPA-GMN-GAZOUKSN1       PIC X(01).
                           11  SPA-GMN-FN              PIC X(01).
                           11  SPA-GMN-GAZOUKSN1MEI    PIC X(10).
      *
      *                相互作用チェック期間
                       09  SPA-GMN-INTERACT-KIKAN      PIC 9(02).
      *
      *                中途終了展開時の自動発生
                       09  SPA-GMN-CHUTOTENKAI-G.
                           11  SPA-GMN-CHUTOTENKAI       PIC X(01).
                           11  SPA-GMN-CHUTO             PIC X(01).
                           11  SPA-GMN-CHUTOTENKAIMEI    PIC X(10).
      *                訂正時の自動発生
                       09  SPA-GMN-TEISEIAUTO-G.
                           11  SPA-GMN-TEISEIAUTO       PIC X(01).
                           11  SPA-GMN-TEISEI           PIC X(01).
                           11  SPA-GMN-TEISEIAUTOMEI    PIC X(10).
      *                訂正時の自動発生（特定疾患）
                       09  SPA-GMN-TEISEITOKU-G.
                           11  SPA-GMN-TEISEITOKU       PIC X(01).
                           11  SPA-GMN-TEISEIH1         PIC X(01).
                           11  SPA-GMN-TEISEITOKUMEI    PIC X(10).
      *H29.3
      *                訂正時の展開（処方料）
                       09  SPA-GMN-TEISHOHOU-G.
                           11  SPA-GMN-TEISHOHOU        PIC X(01).
                           11  SPA-GMN-TEISHOH1         PIC X(01).
                           11  SPA-GMN-TEISHOHOU-MEI    PIC X(10).
                       09  SPA-GMN-TEISHOHOU-COMB.
                           11  SPA-GMN-TEISHOHOU-LIST    PIC X(12)
                                                         OCCURS  2.
                       09  SPA-TEISHOHOU-MAX             PIC 9(02).
      *
      *                外来管理加算チェック
                       09  SPA-GMN-GAIKANRIKBN-G.
                           11  SPA-GMN-GAIKANRIKBN-CHK     PIC X(01).
                           11  SPA-GMN-GAIKANRIKBN-H       PIC X(01).
                           11  SPA-GMN-GAIKANRIKBN-MEI     PIC X(30).
                       09  SPA-GMN-GAIKANRIKBN-COMB.
                           11  SPA-GMN-GAIKANRIKBN-LIST    PIC X(32)
                                                           OCCURS  9.
                       09  SPA-GAIKANRIKBN-MAX             PIC 9(02).
      *                外来カーソル移動
                       09  SPA-GMN-GAIMOVE-G.
                           11  SPA-GMN-GAIMOVE             PIC X(01).
                           11  SPA-GMN-GAIMOVE-F1          PIC X(01).
                           11  SPA-GMN-GAIMOVEMEI          PIC X(20).
                       09  SPA-GMN-GAIMOVE-COMB.
                           11  SPA-GMN-GAIMOVE-LIST        PIC X(22)
                                                           OCCURS  9.
                       09  SPA-GMN-GAIMOVEMAX              PIC 9(02).
      *                時間外加算チェック
                       09  SPA-GMN-JIKANTOKU-CHK-G.
                           11  SPA-GMN-JIKANTOKU-CHK       PIC X(01).
                           11  SPA-GMN-JIKANTOKU-CHK-F1    PIC X(01).
                           11  SPA-GMN-JIKANTOKU-CHKMEI    PIC X(20).
                       09  SPA-GMN-JIKANTOKU-CHK-COMB.
                           11  SPA-GMN-JIKANTOKU-CHK-LIST  PIC X(22)
                                                           OCCURS  2.
      *                チェック除外時間
                       09  SPA-GMN-JIKANCHK-WEEK           OCCURS   2.
                           11  SPA-GMN-JIKANCHK-WST.
                               13  SPA-GMN-JIKANWST-HH      PIC X(02).
                               13  SPA-GMN-JIKANWST-MM      PIC X(02).
                           11  SPA-GMN-JIKANCHK-WED.
                               13  SPA-GMN-JIKANWED-HH      PIC X(02).
                               13  SPA-GMN-JIKANWED-MM      PIC X(02).
                       09  SPA-GMN-JIKANCHK-SAT            OCCURS   2.
                           11  SPA-GMN-JIKANCHK-SST.
                               13  SPA-GMN-JIKANSST-HH      PIC X(02).
                               13  SPA-GMN-JIKANSST-MM      PIC X(02).
                           11  SPA-GMN-JIKANCHK-SED.
                               13  SPA-GMN-JIKANSED-HH      PIC X(02).
                               13  SPA-GMN-JIKANSED-MM      PIC X(02).
      *
      *                包括まとめ
                       09  SPA-GMN-HOUKATU-G.
                           11  SPA-GMN-HOUKATU             PIC X(01).
                           11  SPA-GMN-HOUKATU-F1          PIC X(01).
                           11  SPA-GMN-HOUKATUMEI          PIC X(28).
                       09  SPA-GMN-HOUKATU-COMB.
                           11  SPA-GMN-HOUKATU-LIST        PIC X(30)
                                                           OCCURS  2.
      *                数量ゼロ入力
                       09  SPA-GMN-SURYOZERO-G.
                           11  SPA-GMN-SURYOZERO           PIC X(01).
                           11  SPA-GMN-SURYOZERO-F1        PIC X(01).
                           11  SPA-GMN-SURYOZEROMEI        PIC X(20).
                       09  SPA-GMN-SURYOZERO-COMB.
                           11  SPA-GMN-SURYOZERO-LIST      PIC X(22)
                                                           OCCURS  2.
      *                入院調基チェック
                       09  SPA-GMN-CHOKICHK-G.
                           11  SPA-GMN-CHOKICHK            PIC X(01).
                           11  SPA-GMN-CHOKICHK-F1         PIC X(01).
                           11  SPA-GMN-CHOKICHKMEI         PIC X(20).
                       09  SPA-GMN-CHOKICHK-COMB.
                           11  SPA-GMN-CHOKICHK-LIST       PIC X(20)
                                                           OCCURS  2.
      *                相互作用チェック期間
                       09  SPA-GMN-LAST-KIKAN              PIC 9(02).
      *H28.8
      *        相互作用チェック区分
                       09  SPA-GMN-KINKCHK-G.
                           11  SPA-GMN-KINKCHK             PIC X(01).
                           11  SPA-GMN-KINKCHK-F1          PIC X(01).
                           11  SPA-GMN-KINKCHK-MEI         PIC X(38).
                       09  SPA-GMN-KINKCHK-COMB.
                           11  SPA-GMN-KINKCHK-LIST        PIC X(40)
                                                           OCCURS  3.
                       09  SPA-GMN-KINKCHK-MAX             PIC 9(02).
      *                診療行為クリア確認チェック
                       09  SPA-GMN-CLEARCHK-G.
                           11  SPA-GMN-CLEARCHK            PIC X(01).
                           11  SPA-GMN-CLEARCHK-F1         PIC X(01).
                           11  SPA-GMN-CLEARCHK-MEI        PIC X(20).
                       09  SPA-GMN-CLEARCHK-COMB.
                           11  SPA-GMN-CLEARCHK-LIST       PIC X(20)
                                                           OCCURS  2.
      *                薬剤情報提供料チェック
                       09  SPA-GMN-YAKJYOCHK-G.
                           11  SPA-GMN-YAKJYOCHK           PIC X(01).
                           11  SPA-GMN-YAKJYOCHK-F1        PIC X(01).
                           11  SPA-GMN-YAKJYOCHK-MEI       PIC X(20).
                       09  SPA-GMN-YAKJYOCHK-COMB.
                           11  SPA-GMN-YAKJYOCHK-LIST      PIC X(22)
                                                           OCCURS  2.
      *                前回保険組合せ相違チェック
                       09  SPA-GMN-ZENHKNCHK-G.
                           11  SPA-GMN-ZENHKNCHK           PIC X(01).
                           11  SPA-GMN-ZENHKNCHK-F1        PIC X(01).
                           11  SPA-GMN-ZENHKNCHK-MEI       PIC X(28).
                       09  SPA-GMN-ZENHKNCHK-COMB.
                           11  SPA-GMN-ZENHKNCHK-LIST      PIC X(30)
                                                           OCCURS  3.
                       09  SPA-ZENHKNCHK-MAX               PIC 9(02).
      *                手帳加算チェック
                       09  SPA-GMN-TECHOKBN-G.
                           11  SPA-GMN-TECHOKBN           PIC X(01).
                           11  SPA-GMN-TECHOKBN-F1        PIC X(01).
                           11  SPA-GMN-TECHOKBN-MEI       PIC X(28).
                       09  SPA-GMN-TECHOKBN-COMB.
                           11  SPA-GMN-TECHOKBN-LIST       PIC X(30)
                                                           OCCURS  5.
                       09  SPA-TECHOKBN-MAX               PIC 9(02).
      *                病名表示区分
                       09  SPA-GMN-BYOHYOKBN-G.
                           11  SPA-GMN-BYOHYOKBN           PIC X(01).
                           11  SPA-GMN-BYOHYOKBN-F1        PIC X(01).
                           11  SPA-GMN-BYOHYOKBN-MEI       PIC X(38).
                       09  SPA-GMN-BYOHYOKBN-COMB.
                           11  SPA-GMN-BYOHYOKBN-LIST      PIC X(40)
                                                           OCCURS  5.
                       09  SPA-GMN-BYOHYOKBN-MAX           PIC 9(02).
      *                剤登録順区分
                       09  SPA-GMN-ZAIJUNKBN-G.
                           11  SPA-GMN-ZAIJUNKBN           PIC X(01).
                           11  SPA-GMN-ZAIJUNKBN-F1        PIC X(01).
                           11  SPA-GMN-ZAIJUNKBN-MEI       PIC X(14).
                       09  SPA-GMN-ZAIJUNKBN-COMB.
                           11  SPA-GMN-ZAIJUNKBN-LIST      PIC X(16)
                                                           OCCURS  2.
      *                検体検査加算自動発生
                       09  SPA-GMN-KENSAAUTO-G.
                           11  SPA-GMN-KENSAAUTO           PIC X(01).
                           11  SPA-GMN-KENSAAUTO-F1        PIC X(01).
                           11  SPA-GMN-KENSAAUTO-MEI       PIC X(28).
                       09  SPA-GMN-KENSAAUTO-COMB.
                           11  SPA-GMN-KENSAAUTO-LIST      PIC X(30)
                                                           OCCURS  3.
                       09  SPA-KENSAAUTO-MAX               PIC 9(02).
      *                訂正時の自動発生（入院）
                       09  SPA-GMN-TEINYUAUTO-G.
                           11  SPA-GMN-TEINYUAUTO       PIC X(01).
                           11  SPA-GMN-TEINYUAUTO-F     PIC X(01).
                           11  SPA-GMN-TEINYUAUTO-MEI   PIC X(10).
      *                病名新患区分からの自動発生
                       09  SPA-GMN-BYOMEIAUTO-G.
                           11  SPA-GMN-BYOMEIAUTO       PIC X(01).
                           11  SPA-GMN-BYOMEIAUTO-F     PIC X(01).
                           11  SPA-GMN-BYOMEIAUTO-MEI   PIC X(10).
                       09  SPA-GMN-BYOMEIAUTO-COMB.
                           11  SPA-GMN-BYOMEIAUTO-LIST      PIC X(12)
                                                           OCCURS  2.
      *                入院時初回加算の自動発生
                       09  SPA-GMN-NYUKENSA-G.
                           11  SPA-GMN-NYUKENSA       PIC X(01).
                           11  SPA-GMN-NYUKENSA-F     PIC X(01).
                           11  SPA-GMN-NYUKENSA-MEI   PIC X(10).
                       09  SPA-GMN-NYUKENSA-COMB.
                           11  SPA-GMN-NYUKENSA-LIST      PIC X(12)
                                                           OCCURS  2.
      *
      *                初診再診料の自動発生
                       09  SPA-GMN-INITAUTO-G.
                           11  SPA-GMN-INITAUTO       PIC X(01).
                           11  SPA-GMN-INITAUTO-F     PIC X(01).
                           11  SPA-GMN-INITAUTO-MEI   PIC X(10).
                       09  SPA-GMN-INITAUTO-COMB.
                           11  SPA-GMN-INITAUTO-LIST       PIC X(12)
                                                           OCCURS  2.
                       09  SPA-GMN-INITAUTO-MAX            PIC 9(02).
      *v4.8
      *                時間外緊急院内チェック
                       09  SPA-GMN-JGAIKEIKBN-G.
                           11  SPA-GMN-JGAIKEIKBN          PIC X(01).
                           11  SPA-GMN-JGAIKEIKBN-F1       PIC X(01).
                           11  SPA-GMN-JGAIKEIKBN-MEI      PIC X(28).
                       09  SPA-GMN-JGAIKEIKBN-COMB.
                           11  SPA-GMN-JGAIKEIKBN-LIST     PIC X(30)
                                                           OCCURS  5.
                       09  SPA-JGAIKEIKBN-MAX              PIC 9(02).
      *H28.8
      *                薬剤投与量チェック区分
                       09  SPA-GMN-TOUYOCHK-G.
                           11  SPA-GMN-TOUYOCHK            PIC X(01).
                           11  SPA-GMN-TOUYOCHK-F1         PIC X(01).
                           11  SPA-GMN-TOUYOCHK-MEI        PIC X(38).
                       09  SPA-GMN-TOUYOCHK-COMB.
                           11  SPA-GMN-TOUYOCHK-LIST       PIC X(40)
                                                           OCCURS  5.
                       09  SPA-GMN-TOUYOCHK-MAX            PIC 9(02).
      *H29.4
      *                投薬３０日超チェック区分
                       09  SPA-GMN-SHOHOU30CHK-G.
                           11  SPA-GMN-SHOHOU30CHK         PIC X(01).
                           11  SPA-GMN-SHOHOU30CHK-F1      PIC X(01).
                           11  SPA-GMN-SHOHOU30CHK-MEI     PIC X(38).
                       09  SPA-GMN-SHOHOU30CHK-COMB.
                           11  SPA-GMN-SHOHOU30CHK-LIST    PIC X(40)
                                                           OCCURS  5.
                       09  SPA-GMN-SHOHOU30CHK-MAX         PIC 9(02).
      *
                   07  SPA-GMN-PAGE2-AREA.
      *                カルテの病名対象
                       09  SPA-GMN-BYOMEIKBN-G.
                           11  SPA-GMN-BYOMEIKBN           PIC X(01).
                           11  SPA-GMN-BYOMEIKBN-F1        PIC X(01).
                           11  SPA-GMN-BYOMEIKBNMEI        PIC X(26).
                       09  SPA-GMN-BYOMEIKBN-COMB.
                           11  SPA-GMN-BYOMEIKBN-LIST      PIC X(28)
                                                           OCCURS  3.
      *                人名辞書使用
                       09  SPA-GMN-JISYOKBN-G.
                           11  SPA-GMN-JISYOKBN            PIC X(01).
                           11  SPA-GMN-JISYOKBN-F1         PIC X(01).
                           11  SPA-GMN-JISYOKBNMEI         PIC X(26).
                       09  SPA-GMN-JISYOKBN-COMB.
                           11  SPA-GMN-JISYOKBN-LIST       PIC X(28)
                                                           OCCURS  2.
      *                患者登録のカルテ発行
                       09  SPA-GMN-HAKKOKBN-G.
                           11  SPA-GMN-HAKKOKBN            PIC X(01).
                           11  SPA-GMN-HAKKOKBN-F1         PIC X(01).
                           11  SPA-GMN-HAKKOKBNMEI         PIC X(26).
                       09  SPA-GMN-HAKKOKBN-COMB.
                           11  SPA-GMN-HAKKOKBN-LIST       PIC X(28)
                                                           OCCURS  3.
      *                自費保険の補助区分
                       09  SPA-GMN-HOJYOKBN-G.
                           11  SPA-GMN-HOJYOKBN            PIC X(01).
                           11  SPA-GMN-HOJYOKBN-F1         PIC X(01).
                           11  SPA-GMN-HOJYOKBNMEI         PIC X(26).
                       09  SPA-GMN-HOJYOKBN-COMB.
                           11  SPA-GMN-HOJYOKBN-LIST       PIC X(28)
                                                           OCCURS  2.
      *                同姓同名リスト
                       09  SPA-GMN-DOUMEI-CHKFLG-G.
                           11  SPA-GMN-DOUMEI-CHKFLG       PIC X(01).
                           11  SPA-GMN-DOUMEI-F1           PIC X(01).
                           11  SPA-GMN-DOUMEI-MEI          PIC X(26).
                       09  SPA-GMN-DOUMEI-CHKFLG-COMB.
                           11  SPA-GMN-DOUMEI-CHKFLG-LIST  PIC X(28)
                                                           OCCURS  2.
      *                二重登録疑い区分
                       09  SPA-GMN-JYUFUKU-CHKKBN-G.
                           11  SPA-GMN-JYUFUKU-CHKKBN      PIC X(01).
                           11  SPA-GMN-JYUFUKU-F1          PIC X(01).
                           11  SPA-GMN-JYUFUKU-MEI         PIC X(58).
                       09  SPA-GMN-JYUFUKU-CHKKBN-COMB.
                           11  SPA-GMN-JYUFUKU-CHKKBN-LIST PIC X(60)
                                                           OCCURS  5.
                       09  SPA-GMN-JYUFUKU-MAX             PIC 9(02).
      *                地方公費単独チェック
                       09  SPA-GMN-KOHONLY-CHKFLG-G.
                           11  SPA-GMN-KOHONLY-CHKFLG      PIC X(01).
                           11  SPA-GMN-KOHONLY-F1          PIC X(01).
                           11  SPA-GMN-KOHONLY-MEI         PIC X(26).
                       09  SPA-GMN-KOHONLY-CHKFLG-COMB.
                           11  SPA-GMN-KOHONLY-CHKFLG-LIST PIC X(28)
                                                           OCCURS  2.
                       09  SPA-GMN-KOHONLY-MAX             PIC 9(02).
      *                患者情報削除機能
                       09  SPA-GMN-PTNUMDEL-FLG-G.
                           11  SPA-GMN-PTNUMDEL-FLG         PIC X(01).
                           11  SPA-GMN-PTNUMDEL-F1          PIC X(01).
                           11  SPA-GMN-PTNUMDEL-MEI         PIC X(26).
                       09  SPA-GMN-PTNUMDEL-FLG-COMB.
                           11  SPA-GMN-PTNUMDEL-FLG-LIST PIC X(28)
                                                           OCCURS  2.
                       09  SPA-GMN-PTNUMDEL-MAX             PIC 9(02).
      *                本人・家族区分選択
                       09  SPA-GMN-HONKZKKBN-FLG-G.
                           11  SPA-GMN-HONKZKKBN-FLG         PIC X(01).
                           11  SPA-GMN-HONKZKKBN-F1          PIC X(01).
                           11  SPA-GMN-HONKZKKBN-MEI         PIC X(26).
                       09  SPA-GMN-HONKZKKBN-FLG-COMB.
                           11  SPA-GMN-HONKZKKBN-FLG-LIST PIC X(28)
                                                            OCCURS  2.
                       09  SPA-GMN-HONKZKKBN-MAX             PIC 9(02).
      *                家族年齢
                       09  SPA-GMN-KZKNEN              PIC X(02).
      *                被保険者区分選択
                       09  SPA-GMN-HIHKNJAKBN-FLG-G.
                           11  SPA-GMN-HIHKNJAKBN-FLG         PIC X(01).
                           11  SPA-GMN-HIHKNJAKBN-F1          PIC X(01).
                           11  SPA-GMN-HIHKNJAKBN-MEI         PIC X(26).
                       09  SPA-GMN-HIHKNJAKBN-FLG-COMB.
                           11  SPA-GMN-HIHKNJAKBN-FLG-LIST PIC X(28)
                                                            OCCURS  5.
                       09  SPA-GMN-HIHKNJAKBN-MAX             PIC 9(02).
      *                世帯主名自動記載区分
                       09  SPA-GMN-SETAIKBN-FLG-G.
                           11  SPA-GMN-SETAIKBN-FLG          PIC X(01).
                           11  SPA-GMN-SETAIKBN-F1           PIC X(01).
                           11  SPA-GMN-SETAIKBN-MEI          PIC X(26).
                       09  SPA-GMN-SETAIKBN-FLG-COMB.
                           11  SPA-GMN-SETAIKBN-FLG-LIST     PIC X(28)
                                                             OCCURS  5.
                       09  SPA-GMN-SETAIKBN-MAX              PIC 9(02).
      *                グループ医療機関患者リスト区分
                       09  SPA-GMN-GRPPTLST-FLG-G.
                           11  SPA-GMN-GRPPTLST-FLG          PIC X(01).
                           11  SPA-GMN-GRPPTLST-F1           PIC X(01).
                           11  SPA-GMN-GRPPTLST-MEI          PIC X(26).
                       09  SPA-GMN-GRPPTLST-FLG-COMB.
                           11  SPA-GMN-GRPPTLST-FLG-LIST     PIC X(28)
                                                             OCCURS  5.
                       09  SPA-GMN-GRPPTLST-MAX              PIC 9(02).
      *                公費上限額履歴チェック区分
                       09  SPA-GMN-KOHRRKKBN-FLG-G.
                           11  SPA-GMN-KOHRRKKBN-FLG         PIC X(01).
                           11  SPA-GMN-KOHRRKKBN-F1          PIC X(01).
                           11  SPA-GMN-KOHRRKKBN-MEI         PIC X(26).
                       09  SPA-GMN-KOHRRKKBN-FLG-COMB.
                           11  SPA-GMN-KOHRRKKBN-FLG-LIST    PIC X(28)
                                                             OCCURS  5.
                       09  SPA-GMN-KOHRRKKBN-MAX             PIC 9(02).
      *                郵便番号自動記載チェック区分
                       09  SPA-GMN-POSTKBN-FLG-G.
                           11  SPA-GMN-POSTKBN-FLG           PIC X(01).
                           11  SPA-GMN-POSTKBN-F1            PIC X(01).
                           11  SPA-GMN-POSTKBN-MEI           PIC X(38).
                       09  SPA-GMN-POSTKBN-FLG-COMB.
                           11  SPA-GMN-POSTKBN-FLG-LIST      PIC X(40)
                                                             OCCURS  5.
                       09  SPA-GMN-POSTKBN-MAX               PIC 9(02).
      *                郵便番号自動記載チェック区分
                       09  SPA-GMN-KENMEIKBN-FLG-G.
                           11  SPA-GMN-KENMEIKBN-FLG         PIC X(01).
                           11  SPA-GMN-KENMEIKBN-F1          PIC X(01).
                           11  SPA-GMN-KENMEIKBN-MEI         PIC X(26).
                       09  SPA-GMN-KENMEIKBN-FLG-COMB.
                           11  SPA-GMN-KENMEIKBN-FLG-LIST    PIC X(28)
                                                             OCCURS  5.
                       09  SPA-GMN-KENMEIKBN-MAX             PIC 9(02).
      *                京都府通り名称編集区分
                       09  SPA-GMN-TOWNNAME2KBN-G.
                           11  SPA-GMN-TOWNNAME2KBN          PIC X(01).
                           11  SPA-GMN-TOWNNAME2KBN-F1       PIC X(01).
                           11  SPA-GMN-TOWNNAME2KBN-MEI      PIC X(26).
                       09  SPA-GMN-TOWNNAME2KBN-COMB.
                           11  SPA-GMN-TOWNNAME2KBN-LIST     PIC X(28)
                                                             OCCURS  5.
                       09  SPA-GMN-TOWNNAME2KBN-MAX          PIC 9(02).
      *                公費法別一覧表示区分
                       09  SPA-GMN-KOHHBTKBN-G.
                           11  SPA-GMN-KOHHBTKBN          PIC X(01).
                           11  SPA-GMN-KOHHBTKBN-F1       PIC X(01).
                           11  SPA-GMN-KOHHBTKBN-MEI      PIC X(26).
                       09  SPA-GMN-KOHHBTKBN-COMB.
                           11  SPA-GMN-KOHHBTKBN-LIST     PIC X(28)
                                                             OCCURS  5.
                       09  SPA-GMN-KOHHBTKBN-MAX          PIC 9(02).
      *H27.6
      *                保険初期表示区分
                       09  SPA-GMN-HKNINITKBN-G.
                           11  SPA-GMN-HKNINITKBN         PIC X(01).
                           11  SPA-GMN-HKNINITKBN-F1      PIC X(01).
                           11  SPA-GMN-HKNINITKBN-MEI     PIC X(30).
                       09  SPA-GMN-HKNINITKBN-COMB.
                           11  SPA-GMN-HKNINITKBN-LIST    PIC X(32)
                                                             OCCURS  5.
                       09  SPA-GMN-HKNINITKBN-MAX         PIC 9(02).
      *
                   07  SPA-GMN-PAGE3-AREA.
      *                入金管理情報（診療行為機能情報）
                       09  SPA-GMN-SRYNYKNKNRKBN-G.
                           11  SPA-GMN-SRYNYKNKNRKBN       PIC X(01).
                           11  SPA-GMN-SRYNYKNKNRKBN-F1    PIC X(01).
                           11  SPA-GMN-SRYNYKNKNRKBNMEI    PIC X(38).
                       09  SPA-GMN-SRYNYKNKNRKBNLST-G.
                           11  SPA-GMN-SRYNYKNKNRKBN-MAX   PIC 9(03).
                           11  SPA-GMN-SRYNYKNKNRKBNLST    OCCURS  5.
                               13  SPA-GMN-SRYNYKNKNRKBNL  PIC X(01).
                               13  SPA-GMN-SRYNL-F1        PIC X(01).
                               13  SPA-GMN-SRYNYKNKNRKBNMEIL
                                                           PIC X(38).
      *                複数科まとめ集計
                       09  SPA-GMN-KAGRPFLG-G.
                           11  SPA-GMN-KAGRPFLG       PIC X(01).
                           11  SPA-GMN-KAGRPFLG-F1    PIC X(01).
                           11  SPA-GMN-KAGRPFLG-MEI   PIC X(38).
                       09  SPA-GMN-KAGRPFLGLST-G.
                           11  SPA-GMN-KAGRPFLG-MAX   PIC 9(03).
                           11  SPA-GMN-KAGRPFLG-LST    OCCURS  5.
                               13  SPA-GMN-KAGRPFLGL  PIC X(01).
                               13  SPA-GMN-KGRAL-F1    PIC X(01).
                               13  SPA-GMN-KAGRPFLGMEIL
                                                       PIC X(38).
      *                請求書発行方法
                       09  SPA-GMN-HAKHOUFLG-G.
                           11  SPA-GMN-HAKHOUFLG       PIC X(01).
                           11  SPA-GMN-HAKHOUFLG-F1    PIC X(01).
                           11  SPA-GMN-HAKHOUFLG-MEI   PIC X(38).
                       09  SPA-GMN-HAKHOUFLGLST-G.
                           11  SPA-GMN-HAKHOUFLG-MAX   PIC 9(03).
                           11  SPA-GMN-HAKHOUFLG-LST    OCCURS  5.
                               13  SPA-GMN-HAKHOUFLGL  PIC X(01).
                               13  SPA-GMN-HAKL-F1     PIC X(01).
                               13  SPA-GMN-HAKHOUFLGMEIL
                                                       PIC X(38).
      *                訂正の請求書金額
                       09  SPA-GMN-TEIKINKBN-G.
                           11  SPA-GMN-TEIKINKBN       PIC X(01).
                           11  SPA-GMN-TEIKINKBN-F1    PIC X(01).
                           11  SPA-GMN-TEIKINKBN-MEI   PIC X(38).
                       09  SPA-GMN-TEIKINKBNLST-G.
                           11  SPA-GMN-TEIKINKBN-MAX   PIC 9(02).
                           11  SPA-GMN-TEIKINKBN-LST    OCCURS  5.
                               13  SPA-GMN-TEIKINKBNL  PIC X(01).
                               13  SPA-GMN-TEIL-F1     PIC X(01).
                               13  SPA-GMN-TEIKINKBNMEIL
                                                       PIC X(38).
      *                薬剤情報提供書発行
                       09  SPA-GMN-YKJCHKKBN-G.
                           11  SPA-GMN-YKJCHKKBN       PIC X(01).
                           11  SPA-GMN-YKJCHKKBN-F1    PIC X(01).
                           11  SPA-GMN-YKJCHKKBN-MEI   PIC X(38).
                       09  SPA-GMN-YKJCHKKBNLST-G.
                           11  SPA-GMN-YKJCHKKBN-MAX   PIC 9(02).
                           11  SPA-GMN-YKJCHKKBN-LST    OCCURS  2.
                               13  SPA-GMN-YKJCHKKBNL  PIC X(01).
                               13                      PIC X(01).
                               13  SPA-GMN-YKJCHKKBNMEIL
                                                       PIC X(38).
      *                ＤＯ初期表示
                       09  SPA-GMN-DOSRYKBNFLG-G.
                           11  SPA-GMN-DOSRYKBNFLG       PIC X(02).
                           11  SPA-GMN-DOSRYKBNFLG-F1    PIC X(01).
                           11  SPA-GMN-DOSRYKBNFLG-MEI   PIC X(36).
                       09  SPA-GMN-DOSRYKBNFLGLST-G.
                           11  SPA-GMN-DOSRYKBNFLG-LST   PIC X(40)
                                                       OCCURS  20.
                           11  SPA-GMN-DOSRYKBNFLG-MAX   PIC 9(03).
      *                中途表示一覧
                       09  SPA-GMN-WKSRYHYOFLG-G.
                           11  SPA-GMN-WKSRYHYOFLG       PIC X(01).
                           11  SPA-GMN-WKSRYHYOFLG-F1    PIC X(01).
                           11  SPA-GMN-WKSRYHYOFLG-MEI   PIC X(38).
                       09  SPA-GMN-WKSRYHYOFLGLST-G.
                           11  SPA-GMN-WKSRYHYOFLG-LST   PIC X(40)
                                                       OCCURS  5.
                           11  SPA-GMN-WKSRYHYOFLG-MAX   PIC 9(03).
      *                検査正式名称表示
                       09  SPA-GMN-FORMALFLG-G.
                           11  SPA-GMN-FORMALFLG       PIC X(01).
                           11  SPA-GMN-FORMALFLG-F1    PIC X(01).
                           11  SPA-GMN-FORMALFLG-MEI   PIC X(38).
                       09  SPA-GMN-FORMALFLGLST-G.
                           11  SPA-GMN-FORMALFLG-LST   PIC X(40)
                                                       OCCURS  5.
                           11  SPA-GMN-FORMALFLG-MAX   PIC 9(03).
      *                腫瘍マーカ一覧表示
                       09  SPA-GMN-AKUSEIHYO-G.
                           11  SPA-GMN-AKUSEIHYO       PIC X(01).
                           11  SPA-GMN-AKUSEIHYO-F1    PIC X(01).
                           11  SPA-GMN-AKUSEIHYO-MEI   PIC X(38).
                       09  SPA-GMN-AKUSEIHYOLST-G.
                           11  SPA-GMN-AKUSEIHYO-LST   PIC X(40)
                                                       OCCURS  2.
                           11  SPA-GMN-AKUSEIHYO-MAX   PIC 9(03).
      *                同日再診・訂正選択
                       09  SPA-GMN-TEISEIFLG-G.
                           11  SPA-GMN-TEISEIFLG       PIC X(01).
                           11  SPA-GMN-TEISEIFLG-F1    PIC X(01).
                           11  SPA-GMN-TEISEIFLG-MEI   PIC X(38).
                       09  SPA-GMN-TEISEIFLGLST-G.
                           11  SPA-GMN-TEISEIFLG-LST   PIC X(40)
                                                       OCCURS  3.
                           11  SPA-GMN-TEISEIFLG-MAX   PIC 9(03).
      *                皮下筋肉注射料変換選択
                       09  SPA-GMN-CHG310FLG-G.
                           11  SPA-GMN-CHG310FLG       PIC X(01).
                           11  SPA-GMN-CHG310FLG-F1    PIC X(01).
                           11  SPA-GMN-CHG310FLG-MEI   PIC X(38).
                       09  SPA-GMN-CHG310FLGLST-G.
                           11  SPA-GMN-CHG310FLG-LST   PIC X(40)
                                                       OCCURS  2.
                           11  SPA-GMN-CHG310FLG-MAX   PIC 9(03).
      *                投薬診療種別変換選択
                       09  SPA-GMN-CHGSRYSYU-G.
                           11  SPA-GMN-CHGSRYSYU       PIC X(01).
                           11  SPA-GMN-CHGSRYSYU-F1    PIC X(01).
                           11  SPA-GMN-CHGSRYSYU-MEI   PIC X(38).
                       09  SPA-GMN-CHGSRYSYU-COMB.
                           11  SPA-GMN-CHGSRYSYU-LST   PIC X(40)
                                                       OCCURS  2.
                           11  SPA-GMN-CHGSRYSYU-MAX   PIC 9(02).
      *                お薬手帳発行
                       09  SPA-GMN-OKUSURIKBN-G.
                           11  SPA-GMN-OKUSURIKBN       PIC X(01).
                           11  SPA-GMN-OKUSURIKBN-F1    PIC X(01).
                           11  SPA-GMN-OKUSURIKBN-MEI   PIC X(58).
                       09  SPA-GMN-OKUSURIKBN-COMB.
                           11  SPA-GMN-OKUSURIKBN-LST   PIC X(60)
                                                        OCCURS  5.
                           11  SPA-GMN-OKUSURIKBN-MAX   PIC 9(02).
      *                レセプトブレビュー
                       09  SPA-GMN-RESPRV038-G.
                           11  SPA-GMN-RESPRV038       PIC X(01).
                           11  SPA-GMN-RESPRV038-F1    PIC X(01).
                           11  SPA-GMN-RESPRV038-MEI   PIC X(38).
                       09  SPA-GMN-RESPRV038-COMB.
                           11  SPA-GMN-RESPRV038-LST   PIC X(40)
                                                        OCCURS  5.
                           11  SPA-GMN-RESPRV038-MAX   PIC 9(02).
      *                薬情・お薬手帳
                       09  SPA-GMN-YKJOKUKBN-G.
                           11  SPA-GMN-YKJOKUKBN       PIC X(01).
                           11  SPA-GMN-YKJOKUKBN-F1    PIC X(01).
                           11  SPA-GMN-YKJOKUKBN-MEI   PIC X(58).
                       09  SPA-GMN-YKJOKUKBN-COMB.
                           11  SPA-GMN-YKJOKUKBN-LST   PIC X(60)
                                                        OCCURS  5.
                           11  SPA-GMN-YKJOKUKBN-MAX   PIC 9(02).
      *                残量廃棄区分
                       09  SPA-GMN-HAIKIKBN-G.
                           11  SPA-GMN-HAIKIKBN       PIC X(01).
                           11  SPA-GMN-HAIKIKBN-F1    PIC X(01).
                           11  SPA-GMN-HAIKIKBN-MEI   PIC X(58).
                       09  SPA-GMN-HAIKIKBN-COMB.
                           11  SPA-GMN-HAIKIKBN-LST   PIC X(60)
                                                        OCCURS  5.
                           11  SPA-GMN-HAIKIKBN-MAX   PIC 9(02).
      *                精神療法２０未満加算区分
                       09  SPA-GMN-SEI20KSNKBN-G.
                           11  SPA-GMN-SEI20KSNKBN       PIC X(01).
                           11  SPA-GMN-SEI20KSNKBN-F1    PIC X(01).
                           11  SPA-GMN-SEI20KSNKBN-MEI   PIC X(58).
                       09  SPA-GMN-SEI20KSNKBN-COMB.
                           11  SPA-GMN-SEI20KSNKBN-LST   PIC X(60)
                                                        OCCURS  5.
                           11  SPA-GMN-SEI20KSNKBN-MAX   PIC 9(01).
      *H29.7
      *                心身療法２０未満加算区分
                       09  SPA-GMN-SIN20KSNKBN-G.
                           11  SPA-GMN-SIN20KSNKBN       PIC X(01).
                           11  SPA-GMN-SIN20KSNKBN-F1    PIC X(01).
                           11  SPA-GMN-SIN20KSNKBN-MEI   PIC X(58).
                       09  SPA-GMN-SIN20KSNKBN-COMB.
                           11  SPA-GMN-SIN20KSNKBN-LST   PIC X(60)
                                                        OCCURS  5.
                           11  SPA-GMN-SIN20KSNKBN-MAX   PIC 9(01).
      *
      *                リハビリ・患者情報表示区分
                       09  SPA-GMN-GMNHYOKBN-AREA.
                           11  SPA-GMN-GMNHYOKBN-TBL     OCCURS   6.
                               13  SPA-GMN-GMNHYOKBN-G.
                                   14  SPA-GMN-GMNHYOKBN     PIC X(01).
                                   14  SPA-GMN-GMNHYOKBN-F1  PIC X(01).
                                   14  SPA-GMN-GMNHYOKBN-MEI PIC X(58).
                               13  SPA-NAI-GMNHYOKBN         PIC X(01).
                           11  SPA-GMN-GMNHYOKBN-COMB.
                               13  SPA-GMN-GMNHYOKBN-LSTG  OCCURS  10.
                                   14  SPA-GMN-GMNHYOKBN-LST PIC X(60).
                                   14  SPA-GMN-GMNHYOKBNL    PIC X(01).
                           11  SPA-GMN-GMNHYOKBN-MAX     PIC 9(01).
      *
      *                入金・返金額設定区分
                       09  SPA-GMN-NYKHNKKBN-G.
                           11  SPA-GMN-NYKHNKKBN       PIC X(01).
                           11  SPA-GMN-NYKHNKKBN-F1    PIC X(01).
                           11  SPA-GMN-NYKHNKKBN-MEI   PIC X(58).
                       09  SPA-GMN-NYKHNKKBN-COMB.
                           11  SPA-GMN-NYKHNKKBN-LST   PIC X(60)
                                                        OCCURS  5.
                           11  SPA-GMN-NYKHNKKBN-MAX   PIC 9(02).
      *                発行日変更区分
                       09  SPA-GMN-YMDHENKBN-G.
                           11  SPA-GMN-YMDHENKBN       PIC X(01).
                           11  SPA-GMN-YMDHENKBN-F1    PIC X(01).
                           11  SPA-GMN-YMDHENKBN-MEI   PIC X(58).
                       09  SPA-GMN-YMDHENKBN-COMB.
                           11  SPA-GMN-YMDHENKBN-LST   PIC X(60)
                                                        OCCURS  5.
                           11  SPA-GMN-YMDHENKBN-MAX   PIC 9(01).
      *H24.4
      *                確認画面名称表示区分
                       09  SPA-GMN-GENERICFLG-G.
                           11  SPA-GMN-GENERICFLG       PIC X(01).
                           11  SPA-GMN-GENERICFLG-F1    PIC X(01).
                           11  SPA-GMN-GENERICFLG-MEI   PIC X(38).
                       09  SPA-GMN-GENERICFLG-COMB.
                           11  SPA-GMN-GENERICFLG-LST   PIC X(40)
                                                        OCCURS  5.
                           11  SPA-GMN-GENERICFLG-MAX   PIC 9(01).
      *H30.9
      *                選択式コメント一覧区分
                       09  SPA-GMN-RECEKISAIFLG-G.
                           11  SPA-GMN-RECEKISAIFLG       PIC X(01).
                           11  SPA-GMN-RECEKISAIFLG-F1    PIC X(01).
                           11  SPA-GMN-RECEKISAIFLG-MEI   PIC X(38).
                       09  SPA-GMN-RECEKISAIFLG-COMB.
                           11  SPA-GMN-RECEKISAIFLG-LST   PIC X(40)
                                                        OCCURS  5.
                           11  SPA-GMN-RECEKISAIFLG-MAX   PIC 9(01).
      *ver.4.7
      *                入力スクロール背景色
                       09  SPA-GMN-BGCOLORS            PIC  X(16).
      *                入力スクロール背景行
                       09  SPA-GMN-BGLINEKBN-G.
                           11  SPA-GMN-BGLINEKBN       PIC X(01).
                           11  SPA-GMN-BGLINEKBN-F1    PIC X(01).
                           11  SPA-GMN-BGLINEKBN-MEI   PIC X(38).
                       09  SPA-GMN-BGLINEKBN-COMB.
                           11  SPA-GMN-BGLINEKBN-LST   PIC X(40)
                                                        OCCURS  5.
                           11  SPA-GMN-BGLINEKBN-MAX   PIC 9(01).
      *                環境設定背景色（標準）
                       09  SPA-GMN-BGCOLORSJ0         PIC  X(16).
      *                環境設定背景色（日付変更）
                       09  SPA-GMN-BGCOLORSDAY         PIC  X(16).
      *                環境設定背景色（時間外）
                       09  SPA-GMN-BGCOLORSJ-G.
                           11  SPA-GMN-BGCOLORSJ       PIC  X(16)
                                                       OCCURS   8.
      *                エラー背景色
                       09  SPA-GMN-ERRCOLORS            PIC  X(16).
      *                警告背景色
                       09  SPA-GMN-KEICOLORS            PIC  X(16).
      *H24.12
      *                レセプトブレビューレイアウト
                       09  SPA-GMN-PRVLAYOUT038-G.
                           11  SPA-GMN-PRVLAYOUT038       PIC X(01).
                           11  SPA-GMN-PRVLAYOUT038-F1    PIC X(01).
                           11  SPA-GMN-PRVLAYOUT038-MEI   PIC X(58).
                       09  SPA-GMN-PRVLAYOUT038-COMB.
                           11  SPA-GMN-PRVLAYOUT038-LST   PIC X(60)
                                                        OCCURS  5.
                           11  SPA-GMN-PRVLAYOUT038-MAX   PIC 9(02).
      *
      *H28.6
      *                セット展開表示
                       09  SPA-GMN-SETALLKBN-G.
                           11  SPA-GMN-SETALLKBN       PIC X(01).
                           11  SPA-GMN-SETALLKBN-F1    PIC X(01).
                           11  SPA-GMN-SETALLKBN-MEI   PIC X(38).
                       09  SPA-GMN-SETALLKBN-COMB.
                           11  SPA-GMN-SETALLKBN-LST   PIC X(40)
                                                        OCCURS  5.
                           11  SPA-GMN-SETALLKBN-MAX   PIC 9(02).
      *
                   07  SPA-GMN-PAGE4-AREA.
      *                入金管理情報（収納機能情報）
                       09  SPA-GMN-SYUNYKNKNRKBN-G.
                           11  SPA-GMN-SYUNYKNKNRKBN       PIC X(01).
                           11  SPA-GMN-SYUNYKNKNRKBN-F1    PIC X(01).
                           11  SPA-GMN-SYUNYKNKNRKBNMEI    PIC X(38).
      *                診療年月指定区分
                       09  SPA-GMN-IKTSRYYMKBN-G.
                           11  SPA-GMN-IKTSRYYMKBN         PIC X(01).
                           11  SPA-GMN-IKTSRYYMKBN-F1      PIC X(01).
                           11  SPA-GMN-IKTSRYYMKBNMEI      PIC X(30).
      *                請求書兼領収書発行（外来）
                       09  SPA-GMN-SYUSKYPRTG-G.
                           11  SPA-GMN-SYUSKYPRTG          PIC X(01).
                           11  SPA-GMN-SYUSKYPRTG-F1       PIC X(01).
                           11  SPA-GMN-SYUSKYPRTGMEI       PIC X(22).
      *                請求書兼領収書前回未収額印字（外来）
                       09  SPA-GMN-S03SKYMISYUPRT-G.
                           11  SPA-GMN-S03SKYMISYUPRT     PIC X(01).
                           11  SPA-GMN-S03SKYMISYUPRT-F1  PIC X(01).
                           11  SPA-GMN-S03SKYMISYUPRTMEI  PIC X(30).
      *                診療費明細書発行（外来）
                       09  SPA-GMN-SYUSRYPRTG-G.
                           11  SPA-GMN-SYUSRYPRTG          PIC X(01).
                           11  SPA-GMN-SYUSRYPRTG-F1       PIC X(01).
                           11  SPA-GMN-SYUSRYPRTGMEI       PIC X(22).
      *                請求書兼領収書発行（入院）
                       09  SPA-GMN-SYUSKYPRTN-G.
                           11  SPA-GMN-SYUSKYPRTN          PIC X(01).
                           11  SPA-GMN-SYUSKYPRTN-F1       PIC X(01).
                           11  SPA-GMN-SYUSKYPRTNMEI       PIC X(22).
      *                診療費明細書発行（入院）
                       09  SPA-GMN-SYUSRYPRTN-G.
                           11  SPA-GMN-SYUSRYPRTN          PIC X(01).
                           11  SPA-GMN-SYUSRYPRTN-F1       PIC X(01).
                           11  SPA-GMN-SYUSRYPRTNMEI       PIC X(22).
      *                支払証明書
                       09  SPA-GMN-S04SHIHARAIKBN-G.
                           11  SPA-GMN-S04SHIHARAIKBN      PIC X(01).
                           11  SPA-GMN-S04SHIHARAIKBN-F1   PIC X(01).
                           11  SPA-GMN-S04SHIHARAIKBNMEI   PIC X(10).
      *                入外区分
                       09  SPA-GMN-S04NYUGAIKBN-G.
                           11  SPA-GMN-S04NYUGAIKBN        PIC X(01).
                           11  SPA-GMN-S04NYUGAIKBN-F1     PIC X(01).
                           11  SPA-GMN-S04NYUGAIKBNMEI     PIC X(04).
      *                期間指定
                       09  SPA-GMN-S04SRYKBN-G.
                           11  SPA-GMN-S04SRYKBN           PIC X(01).
                           11  SPA-GMN-S04SRYKBN-F1        PIC X(01).
                           11  SPA-GMN-S04SRYKBNMEI        PIC X(06).
      *                期間区分
                       09  SPA-GMN-S04KIKANKBN-G.
                           11  SPA-GMN-S04KIKANKBN         PIC X(01).
                           11  SPA-GMN-S04KIKANKBN-F1      PIC X(01).
                           11  SPA-GMN-S04KIKANKBNMEI      PIC X(08).
      *                請求内容の印字
                       09  SPA-GMN-S04DSPKBN-G.
                           11  SPA-GMN-S04DSPKBN           PIC X(01).
                           11  SPA-GMN-S04DSPKBN-F1        PIC X(01).
                           11  SPA-GMN-S04DSPKBNMEI        PIC X(18).
      *                期間開始前診療分の支払い
                       09  SPA-GMN-S04SUMKBN-G.
                           11  SPA-GMN-S04SUMKBN       PIC X(01).
                           11  SPA-GMN-S04SUMKBN-F1    PIC X(01).
                           11  SPA-GMN-S04SUMKBNMEI    PIC X(16).
      *                期間終了日後の支払い
                       09  SPA-GMN-S04SYORIKBN-G.
                           11  SPA-GMN-S04SYORIKBN        PIC X(01).
                           11  SPA-GMN-S04SYORIKBN-F1     PIC X(01).
                           11  SPA-GMN-S04SYORIKBNMEI     PIC X(16).
      *                自費内訳明細書
                       09  SPA-GMN-S04JIHIPRTKBN-G.
                           11  SPA-GMN-S04JIHIPRTKBN        PIC X(01).
                           11  SPA-GMN-S04JIHIPRTKBN-F1     PIC X(01).
                           11  SPA-GMN-S04JIHIPRTKBNMEI     PIC X(22).
      *                一括入金画面請求書兼領収書発行
                       09  SPA-GMN-S06SKYPRT-G.
                           11  SPA-GMN-S06SKYPRT           PIC X(01).
                           11  SPA-GMN-S06SKYPRT-F1        PIC X(01).
                           11  SPA-GMN-S06SKYPRTMEI        PIC X(22).
      *                一括入金画面外来分発行方法
                       09  SPA-GMN-S06SKYSUM-G.
                           11  SPA-GMN-S06SKYSUM           PIC X(01).
                           11  SPA-GMN-S06SKYSUM-F1        PIC X(01).
                           11  SPA-GMN-S06SKYSUMMEI        PIC X(30).
      *                請求一覧画面外来請求書兼領収書発行方法
                       09  SPA-GMN-S02SKYSAI-G.
                           11  SPA-GMN-S02SKYSAI           PIC X(01).
                           11  SPA-GMN-S02SKYSAI-F1        PIC X(01).
                           11  SPA-GMN-S02SKYSAIMEI        PIC X(40).
                       09  SPA-GMN-S02SKYMISYUPRT-G.
                           11  SPA-GMN-S02SKYMISYUPRT      PIC X(01).
                           11  SPA-GMN-S02SKYMISYUPRT-F1   PIC X(01).
                           11  SPA-GMN-S02SKYMISYUPRTMEI   PIC X(30).
                       09  SPA-GMN-TUKIGRP-G.
                           11  SPA-GMN-TUKIGRP             PIC X(01).
                           11  SPA-GMN-TUKIGRP-F1          PIC X(01).
                           11  SPA-GMN-TUKIGRPMEI          PIC X(30).
                       09  SPA-GMN-TUKISKYMISYUPRT-G.
                           11  SPA-GMN-TUKISKYMISYUPRT     PIC X(01).
                           11  SPA-GMN-TUKISKYMISYUPRT-F1  PIC X(01).
                           11  SPA-GMN-TUKISKYMISYUPRTMEI  PIC X(30).
                       09  SPA-GMN-TUKISUM-G.
                           11  SPA-GMN-TUKISUM             PIC X(01).
                           11  SPA-GMN-TUKISUM-F1          PIC X(01).
                           11  SPA-GMN-TUKISUMMEI          PIC X(38).
                       09  SPA-GMN-S02MODE-G.
                           11  SPA-GMN-S02MODE          PIC X(01).
                           11  SPA-GMN-S02MODE-F1       PIC X(01).
                           11  SPA-GMN-S02MODEMEI       PIC X(20).
                       09  SPA-GMN-S02LIMIT             PIC 9(3).
                       09  SPA-GMN-S02SORTKBN-G.
                           11  SPA-GMN-S02SORTKBN          PIC X(01).
                           11  SPA-GMN-S02SORTKBN-F1       PIC X(01).
                           11  SPA-GMN-S02SORTKBNMEI       PIC X(20).
                       09  SPA-GMN-SAIKEISANSKYCHK-G.
                           11  SPA-GMN-SAIKEISANSKYCHK     PIC X(01).
                           11  SPA-GMN-SAIKEISANSKYCHK-F1  PIC X(01).
                           11  SPA-GMN-SAIKEISANSKYCHKMEI  PIC X(60).
                       09  SPA-GMN-IKTSORTKBN-G.
                           11  SPA-GMN-IKTSORTKBN          PIC X(01).
                           11  SPA-GMN-IKTSORTKBN-F1       PIC X(01).
                           11  SPA-GMN-IKTSORTKBNMEI       PIC X(20).
                       09  SPA-GMN-IKTKEISANKBN-G.
                           11  SPA-GMN-IKTKEISANKBN          PIC X(01).
                           11  SPA-GMN-IKTKEISANKBN-F1       PIC X(01).
                           11  SPA-GMN-IKTKEISANKBNMEI       PIC X(50).
                       09  SPA-GMN-SYUSTIME.
                           11  SPA-GMN-SYUSTIME-HH         PIC 9(02).
                           11  SPA-GMN-SYUSTIME-MM         PIC 9(02).
                       09  SPA-GMN-MISYUKIKANKBN-G.
                           11  SPA-GMN-MISYUKIKANKBN      PIC X(01).
                           11  SPA-GMN-MISYUKIKANKBN-F1   PIC X(01).
                           11  SPA-GMN-MISYUKIKANKBNMEI   PIC X(48).
                       09  SPA-GMN-SYUKARITIME-G.
                           11  SPA-GMN-SYUKARITIME-OCC1    OCCURS  2.
                               13  SPA-GMN-SYUKARIDAY-G.
                                   15  SPA-GMN-SYUKARIDAY  PIC X(01)
                                                           OCCURS  7.
                               13  SPA-GMN-SYUKARITIME-OCC2
                                                           OCCURS  2.
                                   15  SPA-GMN-SYUKARISTTIME.
                                       17  SPA-GMN-SYUKARIST-HH
                                                           PIC 9(02).
                                       17  SPA-GMN-SYUKARIST-MM
                                                           PIC 9(02).
                                   15  SPA-GMN-SYUKARIEDTIME.
                                       17  SPA-GMN-SYUKARIED-HH
                                                           PIC 9(02).
                                       17  SPA-GMN-SYUKARIED-MM
                                                           PIC 9(02).
      *
                       09  SPA-GMN-SYUNYKNKNRKBNLST-G.
                           11  SPA-GMN-SYUNYKNKNRKBN-MAX   PIC 9(03).
                           11  SPA-GMN-SYUNYKNKNRKBNLST    OCCURS  5.
                               13  SPA-GMN-SYUNYKNKNRKBNL  PIC X(01).
                               13  SPA-GMN-SYUNYKNKNRKBN-F1L PIC X(01).
                               13  SPA-GMN-SYUNYKNKNRKBNMEIL
                                                           PIC X(38).
                       09  SPA-GMN-IKTSRYYMKBNLST-G.
                           11  SPA-GMN-IKTSRYYMKBN-MAX   PIC 9(03).
                           11  SPA-GMN-IKTSRYYMKBNLST    OCCURS  5.
                               13  SPA-GMN-IKTSRYYMKBNL  PIC X(01).
                               13  SPA-GMN-IKTSRYYMKBN-F1L PIC X(01).
                               13  SPA-GMN-IKTSRYYMKBNMEIL
                                                           PIC X(30).
                       09  SPA-GMN-SYUSKYPRTGLST-G.
                           11  SPA-GMN-SYUSKYPRTG-MAX      PIC 9(03).
                           11  SPA-GMN-SYUSKYPRTGLST       OCCURS  2.
                               13  SPA-GMN-SYUSKYPRTGL     PIC X(01).
                               13  SPA-GMN-SYUSKYPRTG-F1L  PIC X(01).
                               13  SPA-GMN-SYUSKYPRTGMEIL
                                                           PIC X(22).
                       09  SPA-GMN-S03SKYMISYUPRTLST-G.
                           11  SPA-GMN-S03SKYMISYUPRT-MAX  PIC 9(03).
                           11  SPA-GMN-S03SKYMISYUPRTLST   OCCURS  2.
                               13  SPA-GMN-S03SKYMISYUPRTL PIC X(01).
                               13  SPA-GMN-S03SKYMISYUPRT-F1L PIC X(01).
                               13  SPA-GMN-S03SKYMISYUPRTMEIL
                                                           PIC X(30).
                       09  SPA-GMN-SYUSRYPRTGLST-G.
                           11  SPA-GMN-SYUSRYPRTG-MAX      PIC 9(03).
                           11  SPA-GMN-SYUSRYPRTGLST       OCCURS  2.
                               13  SPA-GMN-SYUSRYPRTGL     PIC X(01).
                               13  SPA-GMN-SYUSRYPRTG-F1L  PIC X(01).
                               13  SPA-GMN-SYUSRYPRTGMEIL
                                                           PIC X(22).
                       09  SPA-GMN-SYUSKYPRTNLST-G.
                           11  SPA-GMN-SYUSKYPRTN-MAX      PIC 9(03).
                           11  SPA-GMN-SYUSKYPRTNLST       OCCURS  2.
                               13  SPA-GMN-SYUSKYPRTNL     PIC X(01).
                               13  SPA-GMN-SYUSKYPRTN-F1L  PIC X(01).
                               13  SPA-GMN-SYUSKYPRTNMEIL
                                                           PIC X(22).
                       09  SPA-GMN-SYUSRYPRTNLST-G.
                           11  SPA-GMN-SYUSRYPRTN-MAX      PIC 9(03).
                           11  SPA-GMN-SYUSRYPRTNLST       OCCURS  2.
                               13  SPA-GMN-SYUSRYPRTNL     PIC X(01).
                               13  SPA-GMN-SYUSRYPRTN-F1L  PIC X(01).
                               13  SPA-GMN-SYUSRYPRTNMEIL
                                                           PIC X(22).
                       09  SPA-GMN-S06SKYPRTLST-G.
                           11  SPA-GMN-S06SKYPRT-MAX       PIC 9(03).
                           11  SPA-GMN-S06SKYPRTLST        OCCURS  2.
                               13  SPA-GMN-S06SKYPRTL      PIC X(01).
                               13  SPA-GMN-S06SKYPRT-F1L   PIC X(01).
                               13  SPA-GMN-S06SKYPRTMEIL
                                                           PIC X(22).
                       09  SPA-GMN-S06SKYSUMLST-G.
                           11  SPA-GMN-S06SKYSUM-MAX       PIC 9(03).
                           11  SPA-GMN-S06SKYSUMLST        OCCURS  5.
                               13  SPA-GMN-S06SKYSUML      PIC X(01).
                               13  SPA-GMN-S06SKYSUM-F1L   PIC X(01).
                               13  SPA-GMN-S06SKYSUMMEIL   PIC X(40).
                       09  SPA-GMN-S02SKYSAILST-G.
                           11  SPA-GMN-S02SKYSAI-MAX       PIC 9(03).
                           11  SPA-GMN-S02SKYSAILST        OCCURS  5.
                               13  SPA-GMN-S02SKYSAIL      PIC X(01).
                               13  SPA-GMN-S02SKYSAI-F1L   PIC X(01).
                               13  SPA-GMN-S02SKYSAIMEIL   PIC X(30).
                       09  SPA-GMN-S02SKYMISYUPRTLST-G.
                           11  SPA-GMN-S02SKYMISYUPRT-MAX  PIC 9(03).
                           11  SPA-GMN-S02SKYMISYUPRTLST   OCCURS  5.
                               13  SPA-GMN-S02SKYMISYUPRTL PIC X(01).
                               13  SPA-GMN-S02SKYMISYUPRT-F1L PIC X(01).
                               13  SPA-GMN-S02SKYMISYUPRTMEIL
                                                           PIC X(30).
                       09  SPA-GMN-TUKIGRPLST-G.
                           11  SPA-GMN-TUKIGRP-MAX         PIC 9(03).
                           11  SPA-GMN-TUKIGRPLST          OCCURS  5.
                               13  SPA-GMN-TUKIGRPL        PIC X(01).
                               13  SPA-GMN-TUKIGRP-F1L     PIC X(01).
                               13  SPA-GMN-TUKIGRPMEIL     PIC X(30).
                       09  SPA-GMN-TUKISKYMISYUPRTLST-G.
                           11  SPA-GMN-TUKISKYMISYUPRT-MAX  PIC 9(03).
                           11  SPA-GMN-TUKISKYMISYUPRTLST   OCCURS  5.
                               13  SPA-GMN-TUKISKYMISYUPRTL PIC X(01).
                               13  SPA-GMN-TUKISKYMISYUPRT-F1L
                                                           PIC X(01).
                               13  SPA-GMN-TUKISKYMISYUPRTMEIL
                                                           PIC X(30).
                       09  SPA-GMN-TUKISUMLST-G.
                           11  SPA-GMN-TUKISUM-MAX         PIC 9(03).
                           11  SPA-GMN-TUKISUMLST          OCCURS  5.
                               13  SPA-GMN-TUKISUML        PIC X(01).
                               13  SPA-GMN-TUKISUM-F1L     PIC X(01).
                               13  SPA-GMN-TUKISUMMEIL     PIC X(38).
                       09  SPA-GMN-S02MODELST-G.
                           11  SPA-GMN-S02MODE-MAX      PIC 9(03).
                           11  SPA-GMN-S02MODELST       OCCURS  5.
                               13  SPA-GMN-S02MODEL     PIC X(01).
                               13  SPA-GMN-S02MODE-F1L  PIC X(01).
                               13  SPA-GMN-S02MODEMEIL  PIC X(20).
                       09  SPA-GMN-S02SORTKBNLST-G.
                           11  SPA-GMN-S02SORTKBN-MAX      PIC 9(03).
                           11  SPA-GMN-S02SORTKBNLST       OCCURS  5.
                               13  SPA-GMN-S02SORTKBNL     PIC X(01).
                               13  SPA-GMN-S02SORTKBN-F1L  PIC X(01).
                               13  SPA-GMN-S02SORTKBNMEIL  PIC X(20).
                       09  SPA-GMN-SAIKEISANSKYCHKLST-G.
                           11  SPA-GMN-SAIKEISANSKYCHK-MAX    PIC 9(03).
                           11  SPA-GMN-SAIKEISANSKYCHKLST     OCCURS  5.
                               13  SPA-GMN-SAIKEISANSKYCHKL   PIC X(01).
                               13  SPA-GMN-SAIKEISANSKYCHK-F1L
                                                              PIC X(01).
                               13  SPA-GMN-SAIKEISANSKYCHKMEIL
                                                              PIC X(60).
                       09  SPA-GMN-IKTSORTKBNLST-G.
                           11  SPA-GMN-IKTSORTKBN-MAX      PIC 9(03).
                           11  SPA-GMN-IKTSORTKBNLST       OCCURS  5.
                               13  SPA-GMN-IKTSORTKBNL     PIC X(01).
                               13  SPA-GMN-IKTSORTKBN-F1L  PIC X(01).
                               13  SPA-GMN-IKTSORTKBNMEIL  PIC X(20).
                       09  SPA-GMN-IKTKEISANKBNLST-G.
                           11  SPA-GMN-IKTKEISANKBN-MAX      PIC 9(03).
                           11  SPA-GMN-IKTKEISANKBNLST       OCCURS  5.
                               13  SPA-GMN-IKTKEISANKBNL     PIC X(01).
                               13  SPA-GMN-IKTKEISANKBN-F1L  PIC X(01).
                               13  SPA-GMN-IKTKEISANKBNMEIL  PIC X(50).
                       09  SPA-GMN-S04SHIHARAILST-G.
                           11  SPA-GMN-S04SHIHARAI-MAX     PIC 9(03).
                           11  SPA-GMN-S04SHIHARAILST      OCCURS  3.
                               13  SPA-GMN-S04SHIHARAIL    PIC X(01).
                               13  SPA-GMN-S04SHIHARAI-F1L PIC X(01).
                               13  SPA-GMN-S04SHIHARAIMEIL PIC X(10).
                       09  SPA-GMN-S04NYUGAILST-G.
                           11  SPA-GMN-S04NYUGAI-MAX       PIC 9(03).
                           11  SPA-GMN-S04NYUGAILST        OCCURS  3.
                               13  SPA-GMN-S04NYUGAIL      PIC X(01).
                               13  SPA-GMN-S04NYUGAI-F1L   PIC X(01).
                               13  SPA-GMN-S04NYUGAIMEIL   PIC X(04).
                       09  SPA-GMN-S04SRYLST-G.
                           11  SPA-GMN-S04SRY-MAX          PIC 9(03).
                           11  SPA-GMN-S04SRYLST           OCCURS  3.
                               13  SPA-GMN-S04SRYL         PIC X(01).
                               13  SPA-GMN-S04SRY-F1L      PIC X(01).
                               13  SPA-GMN-S04SRYMEIL      PIC X(06).
                       09  SPA-GMN-S04KIKANLST-G.
                           11  SPA-GMN-S04KIKAN-MAX        PIC 9(03).
                           11  SPA-GMN-S04KIKANLST         OCCURS  3.
                               13  SPA-GMN-S04KIKANL       PIC X(01).
                               13  SPA-GMN-S04KIKAN-F1L    PIC X(01).
                               13  SPA-GMN-S04KIKANMEIL    PIC X(08).
                       09  SPA-GMN-S04DSPLST-G.
                           11  SPA-GMN-S04DSP-MAX          PIC 9(03).
                           11  SPA-GMN-S04DSPLST           OCCURS  4.
                               13  SPA-GMN-S04DSPL         PIC X(01).
                               13  SPA-GMN-S04DSP-F1L      PIC X(01).
                               13  SPA-GMN-S04DSPMEIL      PIC X(18).
                       09  SPA-GMN-S04SUMKBNLST-G.
                           11  SPA-GMN-S04SUMKBN-MAX       PIC 9(03).
                           11  SPA-GMN-S04SUMKBNLST        OCCURS  2.
                               13  SPA-GMN-S04SUMKBNL      PIC X(01).
                               13  SPA-GMN-S04SUMKBN-F1L   PIC X(01).
                               13  SPA-GMN-S04SUMKBNMEIL   PIC X(16).
                       09  SPA-GMN-S04SYORILST-G.
                           11  SPA-GMN-S04SYORI-MAX        PIC 9(03).
                           11  SPA-GMN-S04SYORILST         OCCURS  2.
                               13  SPA-GMN-S04SYORIL       PIC X(01).
                               13  SPA-GMN-S04SYORI-F1L    PIC X(01).
                               13  SPA-GMN-S04SYORIMEIL    PIC X(16).
                       09  SPA-GMN-MISYUKIKANKBNLST-G.
                           11  SPA-GMN-MISYUKIKANKBN-MAX  PIC 9(03).
                           11  SPA-GMN-MISYUKIKANKBNLST   OCCURS  5.
                               13  SPA-GMN-MISYUKIKANKBNL PIC X(01).
                               13  SPA-GMN-MISYUKIKANKBN-F1L PIC X(01).
                               13  SPA-GMN-MISYUKIKANKBNMEIL PIC X(48).
                       09  SPA-GMN-S04JIHIPRTKBNLST-G.
                           11  SPA-GMN-S04JIHIPRTKBN-MAX      PIC 9(03).
                           11  SPA-GMN-S04JIHIPRTKBNLST       OCCURS  3.
                               13  SPA-GMN-S04JIHIPRTKBNL     PIC X(01).
                               13  SPA-GMN-S04JIHIPRTKBN-F1L  PIC X(01).
                               13  SPA-GMN-S04JIHIPRTKBNMEIL  PIC X(22).
                   07  SPA-GMN-PAGE5-AREA.
      *                病名機能情報
                       09  SPA-GMN-MSG1-DSPKBN-G.
                           11  SPA-GMN-MSG1-DSPKBN         PIC X(01).
                           11  SPA-GMN-MSG1-DSPKBN-F1      PIC X(01).
                           11  SPA-GMN-MSG1-DSPKBNMEI      PIC X(10).
      *
                       09  SPA-GMN-MSG1-DSPKBNLST-G.
                           11  SPA-GMN-MSG1-DSPKBN-MAX     PIC 9(03).
                           11  SPA-GMN-MSG1-DSPKBNLST      OCCURS  2.
                               13  SPA-GMN-MSG1-DSPKBNL    PIC X(01).
                               13  SPA-GMN-MSG1-DSPKBN-F1L PIC X(01).
                               13  SPA-GMN-MSG1-DSPKBNMEIL PIC X(10).
      *
                       09  SPA-GMN-MSG2-DSPKBN-G.
                           11  SPA-GMN-MSG2-DSPKBN         PIC X(01).
                           11  SPA-GMN-MSG2-DSPKBN-F1      PIC X(01).
                           11  SPA-GMN-MSG2-DSPKBNMEI      PIC X(10).
      *
                       09  SPA-GMN-MSG2-DSPKBNLST-G.
                           11  SPA-GMN-MSG2-DSPKBN-MAX     PIC 9(03).
                           11  SPA-GMN-MSG2-DSPKBNLST      OCCURS  2.
                               13  SPA-GMN-MSG2-DSPKBNL    PIC X(01).
                               13  SPA-GMN-MSG2-DSPKBN-F1L PIC X(01).
                               13  SPA-GMN-MSG2-DSPKBNMEIL PIC X(10).
      *
                       09  SPA-GMN-MSG3-DSPKBN-G.
                           11  SPA-GMN-MSG3-DSPKBN         PIC X(01).
                           11  SPA-GMN-MSG3-DSPKBN-F1      PIC X(01).
                           11  SPA-GMN-MSG3-DSPKBNMEI      PIC X(10).
      *
                       09  SPA-GMN-MSG3-DSPKBNLST-G.
                           11  SPA-GMN-MSG3-DSPKBN-MAX     PIC 9(03).
                           11  SPA-GMN-MSG3-DSPKBNLST      OCCURS  2.
                               13  SPA-GMN-MSG3-DSPKBNL    PIC X(01).
                               13  SPA-GMN-MSG3-DSPKBN-F1L PIC X(01).
                               13  SPA-GMN-MSG3-DSPKBNMEIL PIC X(10).
      *
                       09  SPA-GMN-MSG4-DSPKBN-G.
                           11  SPA-GMN-MSG4-DSPKBN         PIC X(01).
                           11  SPA-GMN-MSG4-DSPKBN-F1      PIC X(01).
                           11  SPA-GMN-MSG4-DSPKBNMEI      PIC X(10).
      *
                       09  SPA-GMN-MSG4-DSPKBNLST-G.
                           11  SPA-GMN-MSG4-DSPKBN-MAX     PIC 9(03).
                           11  SPA-GMN-MSG4-DSPKBNLST      OCCURS  2.
                               13  SPA-GMN-MSG4-DSPKBNL    PIC X(01).
                               13  SPA-GMN-MSG4-DSPKBN-F1L PIC X(01).
                               13  SPA-GMN-MSG4-DSPKBNMEIL PIC X(10).
      *
                       09  SPA-GMN-MSG5-DSPKBN-G.
                           11  SPA-GMN-MSG5-DSPKBN         PIC X(01).
                           11  SPA-GMN-MSG5-DSPKBN-F1      PIC X(01).
                           11  SPA-GMN-MSG5-DSPKBNMEI      PIC X(10).
      *
                       09  SPA-GMN-MSG5-DSPKBNLST-G.
                           11  SPA-GMN-MSG5-DSPKBN-MAX     PIC 9(03).
                           11  SPA-GMN-MSG5-DSPKBNLST      OCCURS  2.
                               13  SPA-GMN-MSG5-DSPKBNL    PIC X(01).
                               13  SPA-GMN-MSG5-DSPKBN-F1L PIC X(01).
                               13  SPA-GMN-MSG5-DSPKBNMEIL PIC X(10).
      *
                       09  SPA-GMN-SRYKA-DSPKBN-G.
                           11  SPA-GMN-SRYKA-DSPKBN        PIC X(01).
                           11  SPA-GMN-SRYKA-DSPKBN-F1     PIC X(01).
                           11  SPA-GMN-SRYKA-DSPKBNMEI     PIC X(38).
      *
                       09  SPA-GMN-SRYKA-DSPKBNLST-G.
                           11  SPA-GMN-SRYKA-DSPKBN-MAX    PIC 9(02).
                           11  SPA-GMN-SRYKA-DSPKBNLST     OCCURS  3.
                               13  SPA-GMN-SRYKA-DSPKBNL   PIC X(01).
                               13  SPA-GMN-SRYKA-DSPKBN-F1L PIC X(01).
                               13  SPA-GMN-SRYKA-DSPKBNMEIL
                                                           PIC X(38).
      *
                       09  SPA-GMN-SYOKBN-DSPKBN-G.
                           11  SPA-GMN-SYOKBN-DSPKBN       PIC X(01).
                           11  SPA-GMN-SYOKBN-DSPKBN-F1    PIC X(01).
                           11  SPA-GMN-SYOKBN-DSPKBNMEI    PIC X(10).
      *
                       09  SPA-GMN-SYOKBN-DSPKBNLST-G.
                           11  SPA-GMN-SYOKBN-DSPKBN-MAX   PIC 9(02).
                           11  SPA-GMN-SYOKBN-DSPKBNLST    OCCURS  3.
                               13  SPA-GMN-SYOKBN-DSPKBNL  PIC X(01).
                               13  SPA-GMN-SYOKBN-DSPKBN-F1L PIC X(01).
                               13  SPA-GMN-SYOKBN-DSPKBNMEIL
                                                           PIC X(10).
      *
                       09  SPA-GMN-UTAGAIFLG-KBN-G.
                           11  SPA-GMN-UTAGAIFLG-KBN       PIC X(01).
                           11  SPA-GMN-UTAGAIFLG-KBN-F1    PIC X(01).
                           11  SPA-GMN-UTAGAIFLG-KBNMEI    PIC X(34).
      *
                       09  SPA-GMN-UTAGAIFLG-KBNLST-G.
                           11  SPA-GMN-UTAGAIFLG-KBN-MAX   PIC 9(02).
                           11  SPA-GMN-UTAGAIFLG-KBNLST    OCCURS  5.
                               13  SPA-GMN-UTAGAIFLG-KBNL  PIC X(01).
                               13  SPA-GMN-UTAGAIFLG-KBN-F1L
                                                           PIC X(01).
                               13  SPA-GMN-UTAGAIFLG-KBNMEIL
                                                           PIC X(34).
      *                詳記区分初期設定
                       09  SPA-GMN-SJKBN-G.
                           11  SPA-GMN-SJKBN               PIC X(02).
                           11  SPA-GMN-SJKBN-F1            PIC X(01).
                           11  SPA-GMN-SJKBN-MEI           PIC X(50).
                       09  SPA-GMN-SJKBN-LST-G.
                           11  SPA-GMN-SJKBN-MAX           PIC 9(02).
                           11  SPA-GMN-SJKBNLST            PIC X(53)
                                                           OCCURS  20.
      *
      *            会計照会情報
                   07  SPA-GMN-PAGE6-AREA.
      *                表示順
                       09  SPA-GMN-SRYKBN-FLG-G.
                           11  SPA-GMN-SRYKBN-FLG          PIC X(01).
                           11  SPA-GMN-SRYKBN-F1           PIC X(01).
                           11  SPA-GMN-SRYKBN-MEI          PIC X(10).
                       09  SPA-GMN-SRYKBN-COMB.
                           11  SPA-GMN-SRYKBN-LIST         PIC X(12)
                                                           OCCURS  2.
                       09  SPA-GMN-SRYKBN-MAX              PIC 9(04).
      *                初期表示
                       09  SPA-GMN-SRYKA-FLG-G.
                           11  SPA-GMN-SRYKA-FLG          PIC X(01).
                           11  SPA-GMN-SRYKA-F1           PIC X(01).
                           11  SPA-GMN-SRYKA-MEI          PIC X(18).
                       09  SPA-GMN-SRYKA-COMB.
                           11  SPA-GMN-SRYKA-LIST         PIC X(20)
                                                           OCCURS  5.
                       09  SPA-GMN-SRYKA-MAX              PIC 9(04).
      *                レセプトブレビュー
                       09  SPA-GMN-RESPRV043-G.
                           11  SPA-GMN-RESPRV043       PIC X(01).
                           11  SPA-GMN-RESPRV043-F1    PIC X(01).
                           11  SPA-GMN-RESPRV043-MEI   PIC X(38).
                       09  SPA-GMN-RESPRV043-COMB.
                           11  SPA-GMN-RESPRV043-LIST   PIC X(40)
                                                        OCCURS  5.
                           11  SPA-GMN-RESPRV043-MAX   PIC 9(02).
      *H24.12
      *                レセプトブレビューレイアウト
                       09  SPA-GMN-PRVLAYOUT043-G.
                           11  SPA-GMN-PRVLAYOUT043       PIC X(01).
                           11  SPA-GMN-PRVLAYOUT043-F1    PIC X(01).
                           11  SPA-GMN-PRVLAYOUT043-MEI   PIC X(58).
                       09  SPA-GMN-PRVLAYOUT043-COMB.
                           11  SPA-GMN-PRVLAYOUT043-LST   PIC X(60)
                                                        OCCURS  5.
                           11  SPA-GMN-PRVLAYOUT043-MAX   PIC 9(02).
      *
      *            予約機能情報
                   07  SPA-GMN-PAGE7-AREA.
      *                初期表示ドクター
                       09  SPA-GMN-INIT-DRCD-G.
                           11  SPA-GMN-INIT-DRCD          PIC X(04).
                           11  SPA-GMN-INIT-DRCD-F1       PIC X(01).
                           11  SPA-GMN-INIT-DRCDMEI       PIC X(24).
                       09  SPA-GMN-INIT-DRCD-COMB.
                           11  SPA-GMN-INIT-DRCD-LIST     PIC X(29)
                                                          OCCURS  99.
                       09  SPA-INIT-DRCD-MAX          PIC 9(04).
      *
      *R01.12
                       09  SPA-NAI-INIT-DRCD-COMB.
                           11 SPA-NAI-INIT-DRCD-OCC       OCCURS  99.
                              13  SPA-NAI-DRSTYUKYMD      PIC X(08).
                              13  SPA-NAI-DREDYUKYMD      PIC X(08).
                       09  SPA-GMN-DRKIKAN            PIC X(30).
      *
      *                予約一覧初期表示
                       09  SPA-GMN-YYKINIT-FLG-G.
                           11  SPA-GMN-YYKINIT-FLG         PIC X(01).
                           11  SPA-GMN-YYKINIT-F1          PIC X(01).
                           11  SPA-GMN-YYKINIT-MEI         PIC X(18).
                       09  SPA-GMN-YYKINIT-COMB.
                           11  SPA-GMN-YYKINIT-LIST        PIC X(20)
                                                           OCCURS  5.
                       09  SPA-GMN-YYKINIT-MAX             PIC 9(04).
                       09  SPA-GMN-YYKMAXCNT               PIC 9(02).
      *                予約票発行区分
                       09  SPA-GMN-YYKHYO-KBN-G.
                           11  SPA-GMN-YYKHYO-KBN          PIC X(01).
                           11  SPA-GMN-YYKHYO-F1           PIC X(01).
                           11  SPA-GMN-YYKHYO-MEI          PIC X(18).
                       09  SPA-GMN-YYKHYO-COMB.
                           11  SPA-GMN-YYKHYO-LIST         PIC X(20)
                                                           OCCURS  5.
                       09  SPA-GMN-YYKHYO-MAX              PIC 9(04).
      *                予約患者番号連動区分
                       09  SPA-GMN-YYKPTNUM-KBN-G.
                           11  SPA-GMN-YYKPTNUM-KBN        PIC X(01).
                           11  SPA-GMN-YYKPTNUM-F1         PIC X(01).
                           11  SPA-GMN-YYKPTNUM-MEI        PIC X(38).
                       09  SPA-GMN-YYKPTNUM-COMB.
                           11  SPA-GMN-YYKPTNUM-LIST       PIC X(40)
                                                           OCCURS  5.
                       09  SPA-GMN-YYKPTNUM-MAX            PIC 9(04).
      *                予約受付連携区分
                       09  SPA-GMN-YYKUKE-KBN-G.
                           11  SPA-GMN-YYKUKE-KBN          PIC X(01).
                           11  SPA-GMN-YYKUKE-F1           PIC X(01).
                           11  SPA-GMN-YYKUKE-MEI          PIC X(38).
                       09  SPA-GMN-YYKUKE-COMB.
                           11  SPA-GMN-YYKUKE-LIST         PIC X(40)
                                                           OCCURS  5.
                       09  SPA-GMN-YYKUKE-MAX              PIC 9(04).
      *                受付保険組合せ区分
                       09  SPA-GMN-UKEHKNCHK-KBN-G.
                           11  SPA-GMN-UKEHKNCHK-KBN       PIC X(01).
                           11  SPA-GMN-UKEHKNCHK-F1        PIC X(01).
                           11  SPA-GMN-UKEHKNCHK-MEI       PIC X(48).
                       09  SPA-GMN-UKEHKNCHK-COMB.
                           11  SPA-GMN-UKEHKNCHK-LIST      PIC X(50)
                                                           OCCURS  5.
                       09  SPA-GMN-UKEHKNCHK-MAX           PIC 9(04).
      *H26.12
      *            点数算定基準情報
                   07  SPA-GMN-PAGE8-AREA.
      *                妥結率区分
                       09  SPA-GMN-DAKETUKBN-G.
                           11  SPA-GMN-DAKETUKBN          PIC X(01).
                           11  SPA-GMN-DAKETU-F1          PIC X(01).
                           11  SPA-GMN-DAKETUKBN-MEI      PIC X(48).
                       09  SPA-GMN-DAKETUKBN-COMB.
                           11  SPA-GMN-DAKETUKBN-LIST     PIC X(50)
                                                          OCCURS  5.
                       09  SPA-GMN-DAKETUKBN-MAX          PIC 9(04).
      *H27.4
      *                低紹介率区分
                       09  SPA-GMN-TEIRITUKBN-G.
                           11  SPA-GMN-TEIRITUKBN         PIC X(01).
                           11  SPA-GMN-TEIRIT-F1          PIC X(01).
                           11  SPA-GMN-TEIRITUKBN-MEI     PIC X(48).
                       09  SPA-GMN-TEIRITUKBN-COMB.
                           11  SPA-GMN-TEIRITUKBN-LIST    PIC X(50)
                                                          OCCURS  5.
                       09  SPA-GMN-TEIRITUKBN-MAX         PIC 9(04).
      *
      *
                   07  SPA-GMN-DATALST        OCCURS  2.
                       09  SPA-GMN-DATAL      PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-DATAMEIL   PIC X(10).
                   07  SPA-GMN-DATA-MAX       PIC 9(02).
      *
                   07  SPA-GMN-DATALST2       OCCURS  3.
                       09  SPA-GMN-DATAL2     PIC X(01).
                       09  SPA-GMN-F1L2       PIC X(01).
                       09  SPA-GMN-DATAMEIL2  PIC X(20).
                   07  SPA-GMN-DATA-MAX2      PIC 9(02).
      *
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-STYUKYMD       PIC 9(08).
                   07  SPA-NAI-EDYUKYMD       PIC 9(08).
      *
      *                チェック除外時間
                   07  SPA-NAI-JIKAN-TIME.
                       09  SPA-NAI-JIKANCHK-WEEK           OCCURS   2.
                           11  SPA-NAI-JIKANCHK-WST.
                               13  SPA-NAI-JIKANWST-HH      PIC 9(02).
                               13  SPA-NAI-JIKANWST-MM      PIC 9(02).
                           11  SPA-NAI-JIKANCHK-WED.
                               13  SPA-NAI-JIKANWED-HH      PIC 9(02).
                               13  SPA-NAI-JIKANWED-MM      PIC 9(02).
                       09  SPA-NAI-JIKANCHK-SAT             OCCURS   2.
                           11  SPA-NAI-JIKANCHK-SST.
                               13  SPA-NAI-JIKANSST-HH      PIC 9(02).
                               13  SPA-NAI-JIKANSST-MM      PIC 9(02).
                           11  SPA-NAI-JIKANCHK-SED.
                               13  SPA-NAI-JIKANSED-HH      PIC 9(02).
                               13  SPA-NAI-JIKANSED-MM      PIC 9(02).
                   07  SPA-NAI-PAGE4-AREA.
                       09  SPA-NAI-S02MODE-MAE              PIC X(01).
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *!!
           03  FLG-OK2             PIC 9(01).
           03  FLG-NO              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
           03  IDT                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-GMN-CUR         PIC 9(03).
           03  WRK-ERR-CUR         PIC 9(03).
           03  WRK-KARIDAY         PIC X(01).
           03  WRK-WIDGET.
               05  WRK-WIDGET1     PIC X(04).
               05  WRK-WIDGET2     PIC X(02).
           03  WRK-WIDMSG          PIC X(70).
           03  WRK-YMD             PIC X(09).
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
           03  WRK-KZKNEN          PIC 9(02).
      *
           03  WRK-MCP-WIDGET      PIC X(64).
      *
      *!!
      *    初期表示ドクター
           03  WRK-GMN-INIT-DRCD-G.
               05  WRK-GMN-INIT-DRCD          PIC X(04).
               05  WRK-GMN-INIT-DRCD-F1       PIC X(01).
               05  WRK-GMN-INIT-DRCDMEI       PIC X(24).
           03  WRK-CHK-YUKOKIKAN.
               05  WRK-DRSTYUKYMD          PIC X(08).
               05  WRK-DREDYUKYMD          PIC X(08).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
           COPY  "CPSK1001.INC".
           COPY  "CPSK5000.INC".
      *
      *
      *    自動算定情報
           COPY  "CPSK1007.INC".
      *    チェック機能制御
           COPY  "CPSK1008.INC".
      *    患者登録機能制御情報
           COPY  "CPSK1017.INC".
      *    診療科情報
           COPY  "CPSK1005.INC".
      *    診療行為機能情報
           COPY  "CPSK1038.INC".
      *    収納機能情報
           COPY  "CPSK1039.INC".
      *    病名機能情報
           COPY  "CPSK1042.INC".
      *    会計照会機能情報
           COPY  "CPSK1043.INC".
      *
      *    予約機能情報
           COPY  "CPSK1045.INC".
      *
      *    職員情報
           COPY  "CPSK1010.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
      *
           COPY  "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    ドクター編集変換サブ
           COPY    "CPORCSDRSET.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
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
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W07
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
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
           MOVE    SPA-W07         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
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
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
                   PERFORM 510-GSRAUTH-SEC
               END-IF
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE                TO  LINKAREA
      *
               MOVE    SPACE                TO  MCP-PUTTYPE
               MOVE    "W07"                TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                    TO  FLG-END
           END-IF
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
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF      SPA-WID1-FLG    =   "OK"
               EVALUATE      SPA-WIDCD
                   WHEN   "1001"
      *                自動算定情報の更新
                       PERFORM  491-KOUSHIN-1007-SEC
                       PERFORM  491-KOUSHIN-1008-SEC
                   WHEN   "1002"
      *                自動算定情報の削除
                       PERFORM  4032-DEL-1007-SEC
                       PERFORM  4032-DEL-1008-SEC
                   WHEN   "1003"
      *                患者登録情報の更新
                       PERFORM  491-KOUSHIN-1017-SEC
                   WHEN   "1004"
      *                患者登録情報の削除
                       PERFORM  4032-DEL-1017-SEC
                   WHEN   "1005"
      *                診療行為情報の更新
                       PERFORM  491-KOUSHIN-1038-SEC
                   WHEN   "1006"
      *                診療行為情報の削除
                       PERFORM  4032-DEL-1038-SEC
                   WHEN   "1007"
      *                収納情報の更新
                       PERFORM  491-KOUSHIN-1039-SEC
                   WHEN   "1008"
      *                収納情報の削除
                       PERFORM  4032-DEL-1039-SEC
                   WHEN   "1009"
      *                病名情報の更新
                       PERFORM  491-KOUSHIN-1042-SEC
                   WHEN   "1010"
      *                病名情報の更新
                       PERFORM  4032-DEL-1042-SEC
                   WHEN   "1011"
      *                会計照会情報の更新
                       PERFORM  493-KOUSHIN-1043-SEC
                   WHEN   "1012"
      *                会計照会情報の削除
                       PERFORM  4033-DEL-1043-SEC
                   WHEN   "1013"
      *                予約情報の更新
                       PERFORM  493-KOUSHIN-1045-SEC
                   WHEN   "1014"
      *                予約情報の削除
                       PERFORM  4033-DEL-1045-SEC
                   WHEN   "1201"
      *                点数算定基準の更新
                       PERFORM  493-KOUSHIN-1201-SEC
                   WHEN   "1202"
      *                点数算定基準の削除
                       PERFORM  4033-DEL-1201-SEC
               END-EVALUATE
           ELSE
               EVALUATE    SPA-W01-KANRICD
                   WHEN    "1007"
                       MOVE    2            TO  SPA-GMN-CUR
                   WHEN    "1017"
                       MOVE    201          TO  SPA-GMN-CUR
                   WHEN    "1038"
                       MOVE    301          TO  SPA-GMN-CUR
                   WHEN    "1043"
                       MOVE    601          TO  SPA-GMN-CUR
                   WHEN    "1045"
                       MOVE    701          TO  SPA-GMN-CUR
                   WHEN    "1201"
                       MOVE    800          TO  SPA-GMN-CUR
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE   SPA-W07
      *
           MOVE    SPA-W01-STYUKYMDH   TO  SPA-GMN-STYUKYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  SPA-GMN-EDYUKYMD
      *
           MOVE    "0"                 TO  SPA-GMN-DATAL(1)
           MOVE    "算定しない"        TO  SPA-GMN-DATAMEIL(1)
           MOVE    "1"                 TO  SPA-GMN-DATAL(2)
           MOVE    "算定する"          TO  SPA-GMN-DATAMEIL(2)
           MOVE    2                   TO  SPA-GMN-DATA-MAX
      *    薬剤情報提供料
           MOVE    "0"                 TO  SPA-GMN-DATAL2(1)
           MOVE    "算定しない"        TO  SPA-GMN-DATAMEIL2(1)
           MOVE    "1"                 TO  SPA-GMN-DATAL2(2)
           MOVE    "算定する（月１回）"
                                       TO  SPA-GMN-DATAMEIL2(2)
           MOVE    "2"                 TO  SPA-GMN-DATAL2(3)
           MOVE    "算定する（毎回）" 
                                       TO  SPA-GMN-DATAMEIL2(3)
           MOVE    3                   TO  SPA-GMN-DATA-MAX2
      *
           MOVE    "0 チェックなし"    TO  SPA-GMN-GAIKANRIKBN-LIST(1)
           MOVE    "1 チェックあり"    TO  SPA-GMN-GAIKANRIKBN-LIST(2)
           MOVE    "2 削除後の自動発生なし" 
                                       TO  SPA-GMN-GAIKANRIKBN-LIST(3)
           MOVE    "3 自動発生なし"    TO  SPA-GMN-GAIKANRIKBN-LIST(4)
           MOVE    "4 削除後の自動発生・確認なし" 
                                       TO  SPA-GMN-GAIKANRIKBN-LIST(5)
           MOVE    5                   TO  SPA-GAIKANRIKBN-MAX
      *
           MOVE    "0 標準"            TO  SPA-GMN-GAIMOVE-LIST(1)
           MOVE    "1 保険組合せ"      TO  SPA-GMN-GAIMOVE-LIST(2)
           MOVE    "2 診療科"          TO  SPA-GMN-GAIMOVE-LIST(3)
           MOVE    "3 診療科→ドクター"
                                       TO  SPA-GMN-GAIMOVE-LIST(4)
           MOVE    4                   TO  SPA-GMN-GAIMOVEMAX
      *
      *
           MOVE    "0 包括分入力をしない"
                                       TO  SPA-GMN-HOUKATU-LIST(1)
           MOVE    "1 包括分入力をする"
                                       TO  SPA-GMN-HOUKATU-LIST(2)
      *
      *    初診再診料自動発生
           MOVE    "0 算定しない"      TO  SPA-GMN-INITAUTO-LIST(1)
           MOVE    "1 算定する"        TO  SPA-GMN-INITAUTO-LIST(2)
           MOVE    2                   TO  SPA-GMN-INITAUTO-MAX
      *
      *
           MOVE    "0 発行しない"      TO  SPA-GMN-S04SHIHARAILST(1)
           MOVE    "1 日別証明書"      TO  SPA-GMN-S04SHIHARAILST(2)
           MOVE    "2 月別証明書"      TO  SPA-GMN-S04SHIHARAILST(3)
           MOVE    3                   TO  SPA-GMN-S04SHIHARAI-MAX
      *
           MOVE    "0 全て"            TO  SPA-GMN-S04NYUGAILST(1)
           MOVE    "1 入院"            TO  SPA-GMN-S04NYUGAILST(2)
           MOVE    "2 外来"            TO  SPA-GMN-S04NYUGAILST(3)
           MOVE    3                   TO  SPA-GMN-S04NYUGAI-MAX
      *
           MOVE    "1 年月日"          TO  SPA-GMN-S04SRYLST(1)
           MOVE    "2 年"              TO  SPA-GMN-S04SRYLST(2)
           MOVE    "3 年度"            TO  SPA-GMN-S04SRYLST(3)
           MOVE    3                   TO  SPA-GMN-S04SRY-MAX
      *
           MOVE    "1 診療期間"        TO  SPA-GMN-S04KIKANLST(1)
           MOVE    "2 支払期間"        TO  SPA-GMN-S04KIKANLST(2)
           MOVE    2                   TO  SPA-GMN-S04KIKAN-MAX
      *
           MOVE    "1 印字する"            TO  SPA-GMN-S04DSPLST(1)
           MOVE    "2 印字しない"          TO  SPA-GMN-S04DSPLST(2)
           MOVE    "3 医保分のみ印字する"  TO  SPA-GMN-S04DSPLST(3)
           MOVE    "4 入金分のみ印字する"  TO  SPA-GMN-S04DSPLST(4)
           MOVE    4                       TO  SPA-GMN-S04DSP-MAX
      *
           MOVE    "1 支払額に含める"      TO  SPA-GMN-S04SUMKBNLST(1)
           MOVE    "2 支払額に含めない"    TO  SPA-GMN-S04SUMKBNLST(2)
           MOVE    2                       TO  SPA-GMN-S04SUMKBN-MAX
      *
           MOVE    "1 支払額に含める"      TO  SPA-GMN-S04SYORILST(1)
           MOVE    "2 支払額に含めない"    TO  SPA-GMN-S04SYORILST(2)
           MOVE    2                       TO  SPA-GMN-S04SYORI-MAX
      *
           MOVE    "0 発行しない"        TO  SPA-GMN-S04JIHIPRTKBNLST(1)
           MOVE    "1 日別明細書"        TO  SPA-GMN-S04JIHIPRTKBNLST(2)
           MOVE    "2 月別明細書"        TO  SPA-GMN-S04JIHIPRTKBNLST(3)
           MOVE    3                     TO  SPA-GMN-S04JIHIPRTKBN-MAX
      *
           MOVE    "0 標準（行削除する）"
                                       TO  SPA-GMN-SURYOZERO-LIST(1)
           MOVE    "1 入力エラーとする"
                                       TO  SPA-GMN-SURYOZERO-LIST(2)
      *    時間外加算チェック
           MOVE    "0 チェックしない"
                                       TO  SPA-GMN-JIKANTOKU-CHK-LIST(1)
           MOVE    "1 チェックする"
                                       TO  SPA-GMN-JIKANTOKU-CHK-LIST(2)
      *
      *    入院調基チェック
           MOVE    "0 チェックしない"
                                       TO  SPA-GMN-CHOKICHK-LIST(1)
           MOVE    "1 チェックする"
                                       TO  SPA-GMN-CHOKICHK-LIST(2)
      *    クリア確認
           MOVE    "0 チェックしない"
                                       TO  SPA-GMN-CLEARCHK-LIST(1)
           MOVE    "1 チェックする"
                                       TO  SPA-GMN-CLEARCHK-LIST(2)
      *    薬剤情報提供料チェック
           MOVE    "0 チェックしない"
                                       TO  SPA-GMN-YAKJYOCHK-LIST(1)
           MOVE    "1 チェックする"
                                       TO  SPA-GMN-YAKJYOCHK-LIST(2)
      *    前回保険組合せ相違チェック
           MOVE    "0 チェックしない"
                                       TO  SPA-GMN-ZENHKNCHK-LIST(1)
           MOVE    "1 チェックする"
                                       TO  SPA-GMN-ZENHKNCHK-LIST(2)
           MOVE    "2 チェックする（すべて）"
                                       TO  SPA-GMN-ZENHKNCHK-LIST(3)
           MOVE    3                   TO  SPA-ZENHKNCHK-MAX
      *    手帳加算確認チェック
           MOVE    "0 チェックしない"
                                       TO  SPA-GMN-TECHOKBN-LIST(1)
           MOVE    "1 チェックする"
                                       TO  SPA-GMN-TECHOKBN-LIST(2)
           MOVE    2                   TO  SPA-TECHOKBN-MAX
      *v4.8
      *    時間外緊急院内チェック
           MOVE    "0 チェックしない"
                                       TO  SPA-GMN-JGAIKEIKBN-LIST(1)
           MOVE    "1 チェックする"
                                       TO  SPA-GMN-JGAIKEIKBN-LIST(2)
           MOVE    2                   TO  SPA-JGAIKEIKBN-MAX
      *H28.8
      *    相互作用チェック区分
           MOVE    "0 相互作用・チェックマスタを対象"
                                       TO  SPA-GMN-KINKCHK-LIST (1)
           MOVE    "1 チェックマスタのみを対象"
                                       TO  SPA-GMN-KINKCHK-LIST (2)
           MOVE    2                   TO  SPA-GMN-KINKCHK-MAX
      *
      *H28.11
      *    薬剤投与量チェック区分
           MOVE    "0 在宅・投薬・注射を対象"
                                       TO  SPA-GMN-TOUYOCHK-LIST (1)
           MOVE    "1 すべてを対象"
                                       TO  SPA-GMN-TOUYOCHK-LIST (2)
           MOVE    2                   TO  SPA-GMN-TOUYOCHK-MAX
      *H29.3
      *    訂正時の展開（処方料）
           MOVE    "0 展開しない"
                                       TO  SPA-GMN-TEISHOHOU-LIST (1)
           MOVE    "1 展開する"
                                       TO  SPA-GMN-TEISHOHOU-LIST (2)
           MOVE    2                   TO  SPA-TEISHOHOU-MAX
      *H29.4
      *    投薬３０日超チェック区分
           MOVE    "0 チェックしない"
                                       TO  SPA-GMN-SHOHOU30CHK-LIST (1)
           MOVE    "1 チェックする"
                                       TO  SPA-GMN-SHOHOU30CHK-LIST (2)
           MOVE    2                   TO  SPA-GMN-SHOHOU30CHK-MAX
      *H30.4
      *    時間外加算特例 産婦人科特例対応
           MOVE    "0 算定しない"      TO  SPA-GMN-JIKANTOKU-NYU-LIST(1)
           MOVE    "1 算定する"        TO  SPA-GMN-JIKANTOKU-NYU-LIST(2)
           MOVE    "2 算定する（小児科のみ）"
                                       TO  SPA-GMN-JIKANTOKU-NYU-LIST(3)
           MOVE    "3 算定する（産・産婦人科のみ）"
                                       TO  SPA-GMN-JIKANTOKU-NYU-LIST(4)
           MOVE    4                   TO  SPA-GMN-JIKANTOKU-MAX
      *
      *
      *    病名表示区分
           MOVE    "0 表示しない"      TO  SPA-GMN-BYOHYOKBN-LIST(1)
           MOVE    "1 表示する"        TO  SPA-GMN-BYOHYOKBN-LIST(2)
      *H28.12
           MOVE    "2 表示する（レセプト表示のみ）"
                                       TO  SPA-GMN-BYOHYOKBN-LIST(3)
           MOVE    3                   TO  SPA-GMN-BYOHYOKBN-MAX
      *    剤登録順区分
           MOVE    "0 診療区分順"      TO  SPA-GMN-ZAIJUNKBN-LIST(1)
           MOVE    "1 入力順"          TO  SPA-GMN-ZAIJUNKBN-LIST(2)
      *    検体検査加算自動発生
           MOVE    "0 算定しない"      TO  SPA-GMN-KENSAAUTO-LIST(1)
           MOVE    "1 算定する"        TO  SPA-GMN-KENSAAUTO-LIST(2)
           MOVE    "2 算定する（設定）"
                                       TO  SPA-GMN-KENSAAUTO-LIST(3)
           MOVE    3                   TO  SPA-KENSAAUTO-MAX
      *
           MOVE    "0 すべての病名"    TO  SPA-GMN-BYOMEIKBN-LIST(1)
           MOVE    "1 継続病名のみ"    TO  SPA-GMN-BYOMEIKBN-LIST(2)
           MOVE    "2 病名記載なし"    TO  SPA-GMN-BYOMEIKBN-LIST(3)
      *
           MOVE    "0 使用する"        TO  SPA-GMN-JISYOKBN-LIST(1)
           MOVE    "1 使用しない"      TO  SPA-GMN-JISYOKBN-LIST(2)
      *
           MOVE    "0 発行しない"      TO  SPA-GMN-HAKKOKBN-LIST(1)
           MOVE    "1 発行する（保険組合せ指示）"
                                       TO  SPA-GMN-HAKKOKBN-LIST(2)
           MOVE    "2 発行する（保険組合せ自動）"
                                       TO  SPA-GMN-HAKKOKBN-LIST(3)
      *
           MOVE    "0 課税"            TO  SPA-GMN-HOJYOKBN-LIST(1)
           MOVE    "1 非課税"          TO  SPA-GMN-HOJYOKBN-LIST(2)
      *
           MOVE    "0 表示する"        TO  SPA-GMN-DOUMEI-CHKFLG-LIST(1)
           MOVE    "1 表示しない"      TO  SPA-GMN-DOUMEI-CHKFLG-LIST(2)
      *
           MOVE    "0 カナ氏名＋性別＋生年月日" 
                                   TO  SPA-GMN-JYUFUKU-CHKKBN-LIST (1)
           MOVE    "1 カナ氏名＋漢字氏名＋性別＋生年月日" 
                                   TO  SPA-GMN-JYUFUKU-CHKKBN-LIST (2)
           MOVE    "2 性別＋生年月日" 
                                   TO  SPA-GMN-JYUFUKU-CHKKBN-LIST (3)
           MOVE    3                   TO  SPA-GMN-JYUFUKU-MAX 
      *
           MOVE    "0 チェックしない" 
                                   TO  SPA-GMN-KOHONLY-CHKFLG-LIST (1)
           MOVE    "1 チェックする" 
                                   TO  SPA-GMN-KOHONLY-CHKFLG-LIST (2)
           MOVE    2               TO  SPA-GMN-KOHONLY-MAX 
      *
           MOVE    "0 削除機能あり" 
                                   TO  SPA-GMN-PTNUMDEL-FLG-LIST (1)
           MOVE    "1 削除機能なし" 
                                   TO  SPA-GMN-PTNUMDEL-FLG-LIST (2)
           MOVE    2               TO  SPA-GMN-PTNUMDEL-MAX 
      *
           MOVE    "0 初期設定あり" 
                                   TO  SPA-GMN-HONKZKKBN-FLG-LIST (1)
           MOVE    "1 初期設定なし" 
                                   TO  SPA-GMN-HONKZKKBN-FLG-LIST (2)
           MOVE    2               TO  SPA-GMN-HONKZKKBN-MAX 
      *    被保険者区分選択
           MOVE    "0 標準設定（国保は姓名）" 
                                   TO  SPA-GMN-HIHKNJAKBN-FLG-LIST (1)
           MOVE    "1 家族は姓のみ"
                                   TO  SPA-GMN-HIHKNJAKBN-FLG-LIST (2)
           MOVE    2               TO  SPA-GMN-HIHKNJAKBN-MAX 
      *    世帯主名自動記載区分
           MOVE    "0 自動記載しない" 
                                   TO  SPA-GMN-SETAIKBN-FLG-LIST (1)
           MOVE    "1 自動記載する"
                                   TO  SPA-GMN-SETAIKBN-FLG-LIST (2)
           MOVE    2               TO  SPA-GMN-SETAIKBN-MAX 
      *    グループ医療機関患者リスト
           MOVE    "0 表示しない" 
                                   TO  SPA-GMN-GRPPTLST-FLG-LIST (1)
           MOVE    "1 表示する"
                                   TO  SPA-GMN-GRPPTLST-FLG-LIST (2)
           MOVE    2               TO  SPA-GMN-GRPPTLST-MAX 
      *    公費上限額履歴チェック
           MOVE    "0 チェックしない"
                                   TO  SPA-GMN-KOHRRKKBN-FLG-LIST (1)
           MOVE    "1 チェックする"
                                   TO  SPA-GMN-KOHRRKKBN-FLG-LIST (2)
           MOVE    2               TO  SPA-GMN-KOHRRKKBN-MAX 
      *    郵便番号自動記載区分
           MOVE    "0 自動記載しない"
                                   TO  SPA-GMN-POSTKBN-FLG-LIST (1)
           MOVE    "1 自動記載する"
                                   TO  SPA-GMN-POSTKBN-FLG-LIST (2)
           MOVE    "2 自動記載する（郵便番号未設定）"
                                   TO  SPA-GMN-POSTKBN-FLG-LIST (3)
           MOVE    3               TO  SPA-GMN-POSTKBN-MAX 
      *    都道府県名記載区分
           MOVE    "0 記載しない"
                                   TO  SPA-GMN-KENMEIKBN-FLG-LIST (1)
           MOVE    "1 記載する"
                                   TO  SPA-GMN-KENMEIKBN-FLG-LIST (2)
           MOVE    2               TO  SPA-GMN-KENMEIKBN-MAX 
      *    京都府通り名称編集区分
           MOVE    "0 編集しない"
                                   TO  SPA-GMN-TOWNNAME2KBN-LIST (1)
           MOVE    "1 編集する"
                                   TO  SPA-GMN-TOWNNAME2KBN-LIST (2)
           MOVE    2               TO  SPA-GMN-TOWNNAME2KBN-MAX 
      *    公費法別一覧表示区分
           MOVE    "0 自動表示しない"
                                   TO  SPA-GMN-KOHHBTKBN-LIST (1)
           MOVE    "1 自動表示する"
                                   TO  SPA-GMN-KOHHBTKBN-LIST (2)
           MOVE    2               TO  SPA-GMN-KOHHBTKBN-MAX 
      *    保険初期表示区分
           MOVE    "0 標準"
                                   TO  SPA-GMN-HKNINITKBN-LIST (1)
           MOVE    "1 自費以外"
                                   TO  SPA-GMN-HKNINITKBN-LIST (2)
           MOVE    "2 自費・労災・自賠責・治験以外"
                                   TO  SPA-GMN-HKNINITKBN-LIST (3)
           MOVE    3               TO  SPA-GMN-HKNINITKBN-MAX 
      *
           MOVE    "1 今回請求分のみ入金"
                                       TO  SPA-GMN-SRYNYKNKNRKBNLST(1)
           MOVE    "2 今回分・伝票の古い未収順に入金"
                                       TO  SPA-GMN-SRYNYKNKNRKBNLST(2)
           MOVE    "3 今回分・伝票の新しい未収順に入金"
                                       TO  SPA-GMN-SRYNYKNKNRKBNLST(3)
           MOVE    "4 伝票の古い未収順に入金"
                                       TO  SPA-GMN-SRYNYKNKNRKBNLST(4)
           MOVE    "5 伝票の新しい未収順に入金"
                                       TO  SPA-GMN-SRYNYKNKNRKBNLST(5)
           MOVE    5                   TO  SPA-GMN-SRYNYKNKNRKBN-MAX
      *
           MOVE    "0 複数科まとめ集計をする"
                                       TO  SPA-GMN-KAGRPFLG-LST(1)
           MOVE    "1 複数科まとめ集計をしない"
                                       TO  SPA-GMN-KAGRPFLG-LST(2)
           MOVE    2                   TO  SPA-GMN-KAGRPFLG-MAX
      *
      *
           MOVE    "1 診療科・保険組合せ別に発行"
                                       TO  SPA-GMN-HAKHOUFLG-LST(1)
           MOVE    "2 保険組合せ別に発行"
                                       TO  SPA-GMN-HAKHOUFLG-LST(2)
           MOVE    "3 診療科別に発行"
                                       TO  SPA-GMN-HAKHOUFLG-LST(3)
           MOVE    "4 全体をまとめて発行"
                                       TO  SPA-GMN-HAKHOUFLG-LST(4)
           MOVE    4                   TO  SPA-GMN-HAKHOUFLG-MAX
      *    訂正の請求書金額
           MOVE    "0 訂正分（差額）"
                                       TO  SPA-GMN-TEIKINKBN-LST(1)
           MOVE    "1 合計"
                                       TO  SPA-GMN-TEIKINKBN-LST(2)
           MOVE    2                   TO  SPA-GMN-TEIKINKBN-MAX
      *    薬剤情報提供発行
           MOVE    "0 薬剤情報提供料と連動しない"
                                       TO  SPA-GMN-YKJCHKKBN-LST(1)
           MOVE    "1 薬剤情報提供料と連動する"
                                       TO  SPA-GMN-YKJCHKKBN-LST(2)
           MOVE    2                   TO  SPA-GMN-YKJCHKKBN-MAX
      *    お薬手帳供発行
           MOVE    "0 薬剤情報提供料（手帳に記載する）と連動しない"
                                       TO  SPA-GMN-OKUSURIKBN-LST(1)
           MOVE    "1 薬剤情報提供料（手帳に記載する）と連動する"
                                       TO  SPA-GMN-OKUSURIKBN-LST(2)
           MOVE    2                   TO  SPA-GMN-OKUSURIKBN-MAX
      *    薬情・お薬手帳対象
           MOVE    "0 包括分入力の保険を対象としない"
                                       TO  SPA-GMN-YKJOKUKBN-LST(1)
           MOVE    "1 包括分入力の保険を対象とする"
                                       TO  SPA-GMN-YKJOKUKBN-LST(2)
           MOVE    2                   TO  SPA-GMN-YKJOKUKBN-MAX
      *
           MOVE    "1 伝票の古い未収順に入金"
                                       TO  SPA-GMN-SYUNYKNKNRKBNLST(1)
           MOVE    "2 伝票の新しい未収順に入金"
                                       TO  SPA-GMN-SYUNYKNKNRKBNLST(2)
           MOVE    "3 診療日の古い未収順に入金"
                                       TO  SPA-GMN-SYUNYKNKNRKBNLST(3)
           MOVE    "4 診療日の新しい未収順に入金"
                                       TO  SPA-GMN-SYUNYKNKNRKBNLST(4)
           MOVE    4                   TO  SPA-GMN-SYUNYKNKNRKBN-MAX
      *
           MOVE    "1 診療年月を指定しない"
                                       TO  SPA-GMN-IKTSRYYMKBNLST(1)
           MOVE    "2 診療年月を指定する"
                                       TO  SPA-GMN-IKTSRYYMKBNLST(2)
           MOVE    2                   TO  SPA-GMN-IKTSRYYMKBN-MAX
      *    ＤＯ初期表示
           MOVE    "00 全体"           TO  SPA-GMN-DOSRYKBNFLG-LST (01)
           MOVE    "10 医学管理・在宅" TO  SPA-GMN-DOSRYKBNFLG-LST (02)
           MOVE    "20 投薬"           TO  SPA-GMN-DOSRYKBNFLG-LST (03)
           MOVE    "30 注射"           TO  SPA-GMN-DOSRYKBNFLG-LST (04)
           MOVE    "40 処置"           TO  SPA-GMN-DOSRYKBNFLG-LST (05)
           MOVE    "50 手術・麻酔"     TO  SPA-GMN-DOSRYKBNFLG-LST (06)
           MOVE    "60 検査"           TO  SPA-GMN-DOSRYKBNFLG-LST (07)
           MOVE    "70 画像診断"       TO  SPA-GMN-DOSRYKBNFLG-LST (08)
           MOVE    "80 リハビリ・その他"
                                       TO  SPA-GMN-DOSRYKBNFLG-LST (09)
           MOVE    "90 自費・その他"   TO  SPA-GMN-DOSRYKBNFLG-LST (10)
           MOVE    "29 投薬・注射"     TO  SPA-GMN-DOSRYKBNFLG-LST (11)
           MOVE    11                  TO  SPA-GMN-DOSRYKBNFLG-MAX
      *    中途一覧表示順
           MOVE    "0 患者番号順"      TO  SPA-GMN-WKSRYHYOFLG-LST(1)
           MOVE    "1 登録順"          TO  SPA-GMN-WKSRYHYOFLG-LST(2)
           MOVE    2                   TO  SPA-GMN-WKSRYHYOFLG-MAX
      *    検査正式名称表示
           MOVE    "0 使用しない"      TO  SPA-GMN-FORMALFLG-LST(1)
           MOVE    "1 使用する"        TO  SPA-GMN-FORMALFLG-LST(2)
           MOVE    2                   TO  SPA-GMN-FORMALFLG-MAX
      *    腫瘍マーカ一覧
           MOVE    "0 自動表示しない"  TO  SPA-GMN-AKUSEIHYO-LST(1)
           MOVE    "1 自動表示する"    TO  SPA-GMN-AKUSEIHYO-LST(2)
           MOVE    2                   TO  SPA-GMN-AKUSEIHYO-MAX
      *    同日再診・訂正選択
           MOVE    "0 選択しない"      TO  SPA-GMN-TEISEIFLG-LST(1)
           MOVE    "1 選択する（OK：同日再診）" 
                                       TO  SPA-GMN-TEISEIFLG-LST(2)
           MOVE    "2 選択する（OK：訂正）"
                                       TO  SPA-GMN-TEISEIFLG-LST(3)
           MOVE    3                   TO  SPA-GMN-TEISEIFLG-MAX
      *    皮下筋肉注射料変換選択
           MOVE    "0 変換する"        TO  SPA-GMN-CHG310FLG-LST(1)
           MOVE    "1 変換しない"      TO  SPA-GMN-CHG310FLG-LST(2)
           MOVE    2                   TO  SPA-GMN-CHG310FLG-MAX
      *    投薬診療種別変換選択
           MOVE    "0 変換する"        TO  SPA-GMN-CHGSRYSYU-LST(1)
           MOVE    "1 変換しない"      TO  SPA-GMN-CHGSRYSYU-LST(2)
           MOVE    2                   TO  SPA-GMN-CHGSRYSYU-MAX
      *    レセプトブレビュー
           MOVE    "1 院外処方含まない"    TO  SPA-GMN-RESPRV038-LST(1)
           MOVE    "2 院外処方含む"        TO  SPA-GMN-RESPRV038-LST(2)
           MOVE    2                   TO  SPA-GMN-RESPRV038-MAX
      *    残量廃棄
           MOVE    "1 注射のみ"        TO  SPA-GMN-HAIKIKBN-LST(1)
           MOVE    "2 すべての診療区分"
                                       TO  SPA-GMN-HAIKIKBN-LST(2)
           MOVE    2                   TO  SPA-GMN-HAIKIKBN-MAX
      *    精神療法２０未満加算区分
           MOVE    "0 自動算定する"    TO  SPA-GMN-SEI20KSNKBN-LST(1)
           MOVE    "1 自動算定しない"
                                       TO  SPA-GMN-SEI20KSNKBN-LST(2)
           MOVE    2                   TO  SPA-GMN-SEI20KSNKBN-MAX
      *H29.7
      *    心身療法２０未満加算区分
           MOVE    "0 自動算定する"    TO  SPA-GMN-SIN20KSNKBN-LST(1)
           MOVE    "1 自動算定しない"
                                       TO  SPA-GMN-SIN20KSNKBN-LST(2)
           MOVE    2                   TO  SPA-GMN-SIN20KSNKBN-MAX
      *ver.4.7
      *    リハビリ・患者情報表示区分
           MOVE    "1"                 TO  SPA-GMN-GMNHYOKBNL (1)
           MOVE    "リハビリ開始日"    TO  SPA-GMN-GMNHYOKBN-LST (1)
           MOVE    "2"                 TO  SPA-GMN-GMNHYOKBNL (2)
           MOVE    "禁忌"              TO  SPA-GMN-GMNHYOKBN-LST (2)
           MOVE    "3"                 TO  SPA-GMN-GMNHYOKBNL (3)
           MOVE    "アレルギー"        TO  SPA-GMN-GMNHYOKBN-LST (3)
           MOVE    "4"                 TO  SPA-GMN-GMNHYOKBNL (4)
           MOVE    "感染症"            TO  SPA-GMN-GMNHYOKBN-LST (4)
           MOVE    "5"                 TO  SPA-GMN-GMNHYOKBNL (5)
           MOVE    "コメント"          TO  SPA-GMN-GMNHYOKBN-LST (5)
      *H28.10
           MOVE    "6"                 TO  SPA-GMN-GMNHYOKBNL (6)
           MOVE    "介護認定情報"      TO  SPA-GMN-GMNHYOKBN-LST (6)
           MOVE    6                   TO  SPA-GMN-GMNHYOKBN-MAX
      *    入力スクロール背景行区分
           MOVE    "0 行毎"            TO  SPA-GMN-BGLINEKBN-LST (1)
           MOVE    "1 剤毎"            TO  SPA-GMN-BGLINEKBN-LST (2)
           MOVE    2                   TO  SPA-GMN-BGLINEKBN-MAX
      *    レセプトブレビューレイアウト
           MOVE    "0 通常表示"        TO  SPA-GMN-PRVLAYOUT038-LST(1)
           MOVE    "1 一画面表示"      TO  SPA-GMN-PRVLAYOUT038-LST(2)
           MOVE    2                   TO  SPA-GMN-PRVLAYOUT038-MAX
      *H28.6
      *    セット内容展開表示
           MOVE    "0 通常展開"        TO  SPA-GMN-SETALLKBN-LST(1)
           MOVE    "1 全展開"          TO  SPA-GMN-SETALLKBN-LST(2)
           MOVE    2                   TO  SPA-GMN-SETALLKBN-MAX
      *
      *    入金・返金額初期設定区分
           MOVE    "0 今回請求額のみ"  TO  SPA-GMN-NYKHNKKBN-LST(1)
           MOVE    "1 一括入金反映"    TO  SPA-GMN-NYKHNKKBN-LST(2)
           MOVE    "2 一括返金反映"
                                       TO  SPA-GMN-NYKHNKKBN-LST(3)
           MOVE    "3 一括入金・返金反映"
                                       TO  SPA-GMN-NYKHNKKBN-LST(4)
           MOVE    4                   TO  SPA-GMN-NYKHNKKBN-MAX
      *    発行日変更区分
           MOVE    "0 変更不可"        TO  SPA-GMN-YMDHENKBN-LST(1)
           MOVE    "1 変更可能とする"
                                       TO  SPA-GMN-YMDHENKBN-LST(2)
           MOVE    2                   TO  SPA-GMN-YMDHENKBN-MAX
      *H24.4
      *    確認画面名称表示区分
           MOVE    "0 一般名表示"      TO  SPA-GMN-GENERICFLG-LST(1)
           MOVE    "1 銘柄名表示"      TO  SPA-GMN-GENERICFLG-LST(2)
           MOVE    "2 一般名表示（全画面）"
                                       TO  SPA-GMN-GENERICFLG-LST(3)
           MOVE    3                   TO  SPA-GMN-GENERICFLG-MAX
      *
      *H30.9
      *選択式コメント一覧区分
           MOVE    "0 自動表示しない"  TO  SPA-GMN-RECEKISAIFLG-LST(1)
           MOVE    "1 自動表示する"    TO  SPA-GMN-RECEKISAIFLG-LST(2)
           MOVE    2                   TO  SPA-GMN-RECEKISAIFLG-MAX
      *
           MOVE    "0 発行しない"      TO  SPA-GMN-SYUSKYPRTGLST(1)
           MOVE    "1 発行する"        TO  SPA-GMN-SYUSKYPRTGLST(2)
           MOVE    2                   TO  SPA-GMN-SYUSKYPRTG-MAX
      *
           MOVE    "1 診療科・保険組合せ別に発行"
                                       TO  SPA-GMN-TUKIGRPLST(1)
           MOVE    "2 保険組合せ別に発行"
                                       TO  SPA-GMN-TUKIGRPLST(2)
           MOVE    "3 診療科別に発行"
                                       TO  SPA-GMN-TUKIGRPLST(3)
           MOVE    "4 まとめて発行"
                                       TO  SPA-GMN-TUKIGRPLST(4)
           MOVE    4                   TO  SPA-GMN-TUKIGRP-MAX
      *
           MOVE    "0 前回請求額を印字しない"
                                       TO  SPA-GMN-S03SKYMISYUPRTLST(1)
           MOVE    "1 前回請求額を印字する"
                                       TO  SPA-GMN-S03SKYMISYUPRTLST(2)
           MOVE    2                   TO  SPA-GMN-S03SKYMISYUPRT-MAX
      *
           MOVE    "0 入金済の請求データを集計に含める"
                                       TO  SPA-GMN-TUKISUMLST(1)
           MOVE    "1 入金済の請求データを集計に含めない"
                                       TO  SPA-GMN-TUKISUMLST(2)
           MOVE    2                   TO  SPA-GMN-TUKISUM-MAX
      *
           MOVE    "0 発行しない"      TO  SPA-GMN-SYUSRYPRTGLST(1)
           MOVE    "1 発行する"        TO  SPA-GMN-SYUSRYPRTGLST(2)
           MOVE    2                   TO  SPA-GMN-SYUSRYPRTG-MAX
      *
           MOVE    "0 発行しない"      TO  SPA-GMN-SYUSKYPRTNLST(1)
           MOVE    "1 発行する"        TO  SPA-GMN-SYUSKYPRTNLST(2)
           MOVE    2                   TO  SPA-GMN-SYUSKYPRTN-MAX
      *
           MOVE    "0 発行しない"      TO  SPA-GMN-SYUSRYPRTNLST(1)
           MOVE    "1 発行する"        TO  SPA-GMN-SYUSRYPRTNLST(2)
           MOVE    2                   TO  SPA-GMN-SYUSRYPRTN-MAX
      *
           MOVE    "0 発行しない"      TO  SPA-GMN-S06SKYPRTLST(1)
           MOVE    "1 発行する"        TO  SPA-GMN-S06SKYPRTLST(2)
           MOVE    2                   TO  SPA-GMN-S06SKYPRT-MAX
      *
           MOVE    "0 個別に発行"      TO  SPA-GMN-S06SKYSUMLST(1)
           MOVE    "1 月毎に診療科・保険組合せ別に発行"
                                       TO  SPA-GMN-S06SKYSUMLST(2)
           MOVE    "2 月毎に保険組合せ別に発行"
                                       TO  SPA-GMN-S06SKYSUMLST(3)
           MOVE    "3 月毎に診療科別に発行"
                                       TO  SPA-GMN-S06SKYSUMLST(4)
           MOVE    "4 月毎にまとめて発行"
                                       TO  SPA-GMN-S06SKYSUMLST(5)
           MOVE    5                   TO  SPA-GMN-S06SKYSUM-MAX
      *
           MOVE    "0 個別に発行"      TO  SPA-GMN-S02SKYSAILST(1)
           MOVE    "1 診療行為で指定した方法で発行"
                                       TO  SPA-GMN-S02SKYSAILST(2)
           MOVE    2                   TO  SPA-GMN-S02SKYSAI-MAX
      *
           MOVE    "0 前回請求額を印字しない"
                                       TO  SPA-GMN-S02SKYMISYUPRTLST(1)
           MOVE    "1 前回請求額を印字する"
                                       TO  SPA-GMN-S02SKYMISYUPRTLST(2)
           MOVE    2                   TO  SPA-GMN-S02SKYMISYUPRT-MAX
      *
           MOVE    "0 前回請求額を印字しない"
                                       TO  SPA-GMN-TUKISKYMISYUPRTLST(1)
           MOVE    "1 前回請求額を印字する"
                                       TO  SPA-GMN-TUKISKYMISYUPRTLST(2)
           MOVE    2                   TO  SPA-GMN-TUKISKYMISYUPRT-MAX
      *
           MOVE    "0 収納一覧（月）"  TO  SPA-GMN-S02MODELST  (1)
           MOVE    "1 収納一覧（年）"  TO  SPA-GMN-S02MODELST (2)
           MOVE    "2 収納一覧（全部）" TO SPA-GMN-S02MODELST (3)
           MOVE    "3 未収一覧"        TO  SPA-GMN-S02MODELST (4)
           MOVE    "4 入返金日"        TO  SPA-GMN-S02MODELST (5)
           MOVE    5                   TO  SPA-GMN-S02MODE-MAX
      *
           MOVE "0 しない"
                                       TO  SPA-GMN-SAIKEISANSKYCHKLST(1)
           MOVE "1 する（存在する場合ボタンの文字色を変更する）"
                                       TO  SPA-GMN-SAIKEISANSKYCHKLST(2)
           MOVE    2                   TO  SPA-GMN-SAIKEISANSKYCHK-MAX
      *
           MOVE    "0 伝票番号順"
                                       TO  SPA-GMN-IKTSORTKBNLST(1)
           MOVE    "1 診療日順"
                                       TO  SPA-GMN-IKTSORTKBNLST(2)
           MOVE    2                   TO  SPA-GMN-IKTSORTKBN-MAX
      *
           MOVE    "0 一部負担金を保持し先頭より再計算を行う"
                                       TO  SPA-GMN-IKTKEISANKBNLST(1)
           MOVE    "1 一部負担金をクリアし先頭より再計算を行う"
                                       TO  SPA-GMN-IKTKEISANKBNLST(2)
           MOVE    2                   TO  SPA-GMN-IKTKEISANKBN-MAX
      *
           MOVE    "0 診療日（請求期間）以降の未収金を含める"
                                       TO  SPA-GMN-MISYUKIKANKBNLST(1)
           MOVE    "1 診療日（請求期間）以降の未収金を含めない"
                                       TO  SPA-GMN-MISYUKIKANKBNLST(2)
           MOVE    2                   TO  SPA-GMN-MISYUKIKANKBN-MAX
      *
           MOVE    "0 表示する"        TO  SPA-GMN-MSG1-DSPKBNLST(1)
           MOVE    "1 表示しない"      TO  SPA-GMN-MSG1-DSPKBNLST(2)
           MOVE    2                   TO  SPA-GMN-MSG1-DSPKBN-MAX
      *
           MOVE    "0 表示する"        TO  SPA-GMN-MSG2-DSPKBNLST(1)
           MOVE    "1 表示しない"      TO  SPA-GMN-MSG2-DSPKBNLST(2)
           MOVE    2                   TO  SPA-GMN-MSG2-DSPKBN-MAX
      *
           MOVE    "0 表示する"        TO  SPA-GMN-MSG3-DSPKBNLST(1)
           MOVE    "1 表示しない"      TO  SPA-GMN-MSG3-DSPKBNLST(2)
           MOVE    2                   TO  SPA-GMN-MSG3-DSPKBN-MAX
      *
           MOVE    "0 表示しない"      TO  SPA-GMN-MSG4-DSPKBNLST(1)
           MOVE    "1 表示する"        TO  SPA-GMN-MSG4-DSPKBNLST(2)
           MOVE    2                   TO  SPA-GMN-MSG4-DSPKBN-MAX
      *
           MOVE    "0 表示する"        TO  SPA-GMN-MSG5-DSPKBNLST(1)
           MOVE    "1 表示しない"      TO  SPA-GMN-MSG5-DSPKBNLST(2)
           MOVE    2                   TO  SPA-GMN-MSG5-DSPKBN-MAX
      *
           MOVE    "0 標準（最終受診科、遷移時は選択診療科）"
                                       TO  SPA-GMN-SRYKA-DSPKBNLST(1)
           MOVE    "1 全科"            TO  SPA-GMN-SRYKA-DSPKBNLST(2)
           MOVE    "2 全科（遷移時は選択診療科）"
                                       TO  SPA-GMN-SRYKA-DSPKBNLST(3)
           MOVE    3                   TO  SPA-GMN-SRYKA-DSPKBN-MAX
      *
           MOVE    "0 当月"            TO  SPA-GMN-SYOKBN-DSPKBNLST(1)
           MOVE    "1 前月"            TO  SPA-GMN-SYOKBN-DSPKBNLST(2)
           MOVE    "2 全体"            TO  SPA-GMN-SYOKBN-DSPKBNLST(3)
           MOVE    3                   TO  SPA-GMN-SYOKBN-DSPKBN-MAX
      *
           MOVE    "0 病名欄と連動して消去"
                                       TO  SPA-GMN-UTAGAIFLG-KBNLST(1)
           MOVE    "9 病名欄、病名コード欄と連動して消去"
                                       TO  SPA-GMN-UTAGAIFLG-KBNLST(2)
           MOVE    2                   TO  SPA-GMN-UTAGAIFLG-KBN-MAX
      *    コメント症状詳記区分初期
           MOVE    "01 主たる疾患の臨床症状"
                                       TO  SPA-GMN-SJKBNLST (01)
           MOVE    "02 主たる疾患の診療・検査所見"
                                       TO  SPA-GMN-SJKBNLST (02)
           MOVE    "03 主な治療行為の必要性"
                                       TO  SPA-GMN-SJKBNLST (03)
           MOVE    "04 主な治療行為の経過"
                                       TO  SPA-GMN-SJKBNLST (04)
           MOVE    "05 １００万点以上の薬剤に係る症状等"
                                       TO  SPA-GMN-SJKBNLST (05)
           MOVE    "06 １００万点以上の処置に係る症状等"
                                       TO  SPA-GMN-SJKBNLST (06)
           MOVE    "07 その他（１）"
                                       TO  SPA-GMN-SJKBNLST (07)
           MOVE    "08 その他（２）"
                                       TO  SPA-GMN-SJKBNLST (08)
           MOVE    "09 その他（３）"
                                       TO  SPA-GMN-SJKBNLST (09)
      *    Ｈ１９．４からの追加
           MOVE    "50 治験概要"
                                       TO  SPA-GMN-SJKBNLST (10)
           MOVE    "51 疾患別リハビリテーションに係る治療継続理由"
                                       TO  SPA-GMN-SJKBNLST (11)
      *    Ｈ２０．４ から追加
           MOVE    "52 廃用症候群に係る評価表"
                                       TO  SPA-GMN-SJKBNLST (12)
      *H26.4　追加
           MOVE    "90 上記以外の診療報酬明細書"
                                       TO  SPA-GMN-SJKBNLST (13)
           MOVE    "99 レセプト摘要欄コメント"
                                       TO  SPA-GMN-SJKBNLST (14)
      *H25.7 から追加
           MOVE    "AA 労災レセプト「傷病の経過」"
                                       TO  SPA-GMN-SJKBNLST (15)
      *H25.10
           MOVE    "BB 自賠責（第三者行為）レセプト摘要欄コメント"
                                       TO  SPA-GMN-SJKBNLST (16)
           MOVE    16                  TO  SPA-GMN-SJKBN-MAX
      *
      *    会計照会情報
           MOVE    "0 診療科順"        TO  SPA-GMN-SRYKBN-LIST(1)
           MOVE    "1 診療区分順"      TO  SPA-GMN-SRYKBN-LIST(2)
           MOVE    2                   TO  SPA-GMN-SRYKBN-MAX
      *    診療科初期表示
           MOVE    "0 標準"            TO  SPA-GMN-SRYKA-LIST(1)
           MOVE    "1 前回表示科"      TO  SPA-GMN-SRYKA-LIST(2)
           MOVE    "2 全科"            TO  SPA-GMN-SRYKA-LIST(3)
           MOVE    3                   TO  SPA-GMN-SRYKA-MAX
      *    レセプトブレビュー
           MOVE    "1 院外処方含まない"    TO  SPA-GMN-RESPRV043-LIST(1)
           MOVE    "2 院外処方含む"        TO  SPA-GMN-RESPRV043-LIST(2)
           MOVE    2                   TO  SPA-GMN-RESPRV043-MAX
      *    レセプトブレビューレイアウト
           MOVE    "0 通常表示"        TO  SPA-GMN-PRVLAYOUT043-LST(1)
           MOVE    "1 一画面表示"      TO  SPA-GMN-PRVLAYOUT043-LST(2)
           MOVE    2                   TO  SPA-GMN-PRVLAYOUT043-MAX
      *
      *予約情報
      *    予約一覧初期
           MOVE    "0 予約登録内容"    TO  SPA-GMN-YYKINIT-LIST(1)
           MOVE    "1 全体"            TO  SPA-GMN-YYKINIT-LIST(2)
           MOVE    2                   TO  SPA-GMN-YYKINIT-MAX
      *    予約票発行
           MOVE    "0 発行しない"      TO  SPA-GMN-YYKHYO-LIST(1)
           MOVE    "1 発行する"        TO  SPA-GMN-YYKHYO-LIST(2)
           MOVE    2                   TO  SPA-GMN-YYKHYO-MAX
      *    予約患者番号連動
           MOVE    "0 連動しない"      TO  SPA-GMN-YYKPTNUM-LIST(1)
           MOVE    "1 連動する"        TO  SPA-GMN-YYKPTNUM-LIST(2)
           MOVE    2                   TO  SPA-GMN-YYKPTNUM-MAX
      *    予約受付連動
           MOVE    "0 連動しない"      TO  SPA-GMN-YYKUKE-LIST(1)
           MOVE    "1 連動する"        TO  SPA-GMN-YYKUKE-LIST(2)
           MOVE    "2 連動する（予約のみ）"
                                       TO  SPA-GMN-YYKUKE-LIST(3)
           MOVE    3                   TO  SPA-GMN-YYKUKE-MAX
      *    受付保険組合せ区分
           MOVE    "0 空白チェックなし"
                                       TO  SPA-GMN-UKEHKNCHK-LIST(1)
           MOVE    "1 空白チェックあり"
                                       TO  SPA-GMN-UKEHKNCHK-LIST(2)
           MOVE    "2 空白チェックあり（予約設定有）"
                                       TO  SPA-GMN-UKEHKNCHK-LIST(3)
           MOVE    3                   TO  SPA-GMN-UKEHKNCHK-MAX
      *H26.12
      *    妥結率区分
           MOVE    "0 低妥結率でない"
                                       TO  SPA-GMN-DAKETUKBN-LIST(1)
           MOVE    "1 低妥結率である"
                                       TO  SPA-GMN-DAKETUKBN-LIST(2)
           MOVE    2                   TO  SPA-GMN-DAKETUKBN-MAX
      *H27.4
      *    低紹介率区分
           MOVE    "0 低紹介率でない"
                                       TO  SPA-GMN-TEIRITUKBN-LIST(1)
           MOVE    "1 低紹介率である"
                                       TO  SPA-GMN-TEIRITUKBN-LIST(2)
           MOVE    2                   TO  SPA-GMN-TEIRITUKBN-MAX
      *
      *
      *ver.4.7
      *    背景色初期設定
           PERFORM 3109-COLORS-INIT-SEC
      *
           EVALUATE    SPA-W01-KANRICD
      *        診療行為情報
               WHEN    "1007"
                   PERFORM 310-SPASET-1007-SEC
      *        患者登録機能制御情報
               WHEN    "1017"
                   PERFORM 310-SPASET-1017-SEC
      *        診療行為機能情報
               WHEN    "1038"
                   PERFORM 310-SPASET-1038-SEC
      *        収納機能情報
               WHEN    "1039"
                   PERFORM 310-SPASET-1039-SEC
      *        病名機能情報
               WHEN    "1042"
                   PERFORM 310-SPASET-1042-SEC
      *        会計照会機能情報
               WHEN    "1043"
                   PERFORM 310-SPASET-1043-SEC
      *        予約機能情報
               WHEN    "1045"
                   PERFORM 310-SPASET-1045-SEC
      *        点数算定基準
               WHEN    "1201"
                   PERFORM 310-SPASET-1201-SEC
           END-EVALUATE
      *
           . 
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-1007-SEC         SECTION.
      *
      *    自動算定情報
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1007"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1007-REC
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       6
                   MOVE    SYS-1007-SANTEI(IDX:1)
                                               TO  SPA-GMN-DATA(IDX)
                   IF      IDX             =   2   OR  3  OR  4
      *            薬剤情報提供料
                       PERFORM VARYING IDY     FROM    1   BY  1
                               UNTIL   IDY     >   SPA-GMN-DATA-MAX2
                           IF      SPA-GMN-DATA(IDX)
                                               =   SPA-GMN-DATAL2(IDY)
                               MOVE    SPA-GMN-DATALST2(IDY)
                                               TO SPA-GMN-DATA-G(IDX)
                               MOVE    SPA-GMN-DATA-MAX2   TO  IDY
                           END-IF
                       END-PERFORM
                   ELSE
      *            薬剤情報提供料以外
                       PERFORM VARYING IDY     FROM    1   BY  1
                               UNTIL   IDY     >   SPA-GMN-DATA-MAX
                           IF      SPA-GMN-DATA(IDX)
                                               =   SPA-GMN-DATAL(IDY)
                               MOVE    SPA-GMN-DATALST(IDY)
                                               TO SPA-GMN-DATA-G(IDX)
                               MOVE    SPA-GMN-DATA-MAX   TO  IDY
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF
      *
      *    チェック機能制御割り当て情報
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1008"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYSKANRI-REC    TO  SYS-1008-REC
      *
      *        育児自動算定
               MOVE    SYS-1008-IKUJIEIYO-FLG
                                       TO  SPA-GMN-DATA(7)
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL   IDY     >   SPA-GMN-DATA-MAX
                   IF      SPA-GMN-DATA(7)
                                       =   SPA-GMN-DATAL(IDY)
                       MOVE    SPA-GMN-DATALST(IDY)
                                           TO SPA-GMN-DATA-G(7)
                       MOVE    SPA-GMN-DATA-MAX   TO  IDY
                   END-IF
               END-PERFORM
      *
               IF      SPA-GMN-DATA(7)     =   "1"
      *            育児自動算定診療科
                   MOVE    SYS-1008-IKUJIEIYO-SRYKA
                                           TO  SPA-GMN-SRYKACD
               END-IF
      *
               MOVE    SYS-1008-INTERACT-KIKAN
                                           TO  SPA-GMN-INTERACT-KIKAN
      *        外来管理加算チェック
               MOVE    SYS-1008-GAIKANRIKBN-CHK
                                           TO  SPA-GMN-GAIKANRIKBN-CHK
      *        外来カーソル移動
               MOVE    SYS-1008-GAI-CURSOR-SET-1
                                           TO  SPA-GMN-GAIMOVE
      *        数量ゼロ入力
               MOVE    SYS-1008-SURYOZERO-FLG
                                           TO  SPA-GMN-SURYOZERO
      *        入院調基チェック
               MOVE    SYS-1008-CHOKICHK-FLG
                                           TO  SPA-GMN-CHOKICHK
      *
      *        入院初診料自動発生
               MOVE    SYS-1008-NYU-SYOSIN
                                       TO  SPA-GMN-NYUSYOSIN
      *
      *        包括まとめ
               MOVE    SYS-1008-HOUKATU-FLG
                                       TO  SPA-GMN-HOUKATU
      *        クリア確認
               MOVE    SYS-1008-CLEARCHK-FLG
                                       TO  SPA-GMN-CLEARCHK
      *        薬剤情報提供料チェック
               MOVE    SYS-1008-YAKJYOCHK-FLG
                                       TO  SPA-GMN-YAKJYOCHK
      *        前回保険組合せ相違チェック
               MOVE    SYS-1008-ZENHKNCHK-FLG
                                       TO  SPA-GMN-ZENHKNCHK
      *        手帳加算確認チェック
               MOVE    SYS-1008-TECHOKBN-FLG
                                       TO  SPA-GMN-TECHOKBN
      *        最終受診日からの期間
               IF      SYS-1008-LAST-KIKAN-X =   SPACE
                   MOVE    ZERO            TO  SYS-1008-LAST-KIKAN
               END-IF
               MOVE    SYS-1008-LAST-KIKAN
                                       TO  SPA-GMN-LAST-KIKAN
      *        病名表示区分
               IF     SYS-1008-BYOHYOKBN-FLG   =   SPACE
                   MOVE    "1"             TO  SPA-GMN-BYOHYOKBN
               ELSE
                   MOVE    SYS-1008-BYOHYOKBN-FLG
                                           TO  SPA-GMN-BYOHYOKBN
               END-IF
      *        剤登録順区分
               IF     SYS-1008-ZAIJUNKBN-FLG   =   SPACE
                   MOVE    "0"             TO  SPA-GMN-ZAIJUNKBN
               ELSE
                   MOVE    SYS-1008-ZAIJUNKBN-FLG
                                           TO  SPA-GMN-ZAIJUNKBN
               END-IF
      *        検体検査加算自動発生
               IF     SYS-1008-KENSAAUTO-FLG   =   SPACE
                   MOVE    "0"             TO  SPA-GMN-KENSAAUTO
               ELSE
                   MOVE    SYS-1008-KENSAAUTO-FLG
                                           TO  SPA-GMN-KENSAAUTO
               END-IF
      *
      *        時間外加算特例乳幼児
               MOVE    SYS-1008-JIKANTOKU-NYU-FLG
                                       TO  SPA-GMN-JIKANTOKU-NYU
      *        画像診断管理加算１
               MOVE    SYS-1008-GAZOUKSN1-FLG
                                       TO  SPA-GMN-GAZOUKSN1
      *        中途終了展開時の自動発生
               MOVE    SYS-1008-CHUTOTENKAI-FLG
                                       TO  SPA-GMN-CHUTOTENKAI
      *        訂正時の自動発生（外来）
               MOVE    SYS-1008-TEISEIAUTO-FLG
                                       TO  SPA-GMN-TEISEIAUTO
      *        訂正時の自動発生（特定疾患処方管理）
               MOVE    SYS-1008-TEISEITOKU-FLG
                                       TO  SPA-GMN-TEISEITOKU
      *        訂正時の自動発生（入院）
               MOVE    SYS-1008-TEINYUAUTO-FLG
                                       TO  SPA-GMN-TEINYUAUTO
      *H29.3
      *        訂正時の自動発生（処方料）
               MOVE    SYS-1008-TEISHOHOU-FLG
                                       TO  SPA-GMN-TEISHOHOU
      *        病名からの自動発生
               MOVE    SYS-1008-BYOMEIAUTO-FLG
                                       TO  SPA-GMN-BYOMEIAUTO
      *        入院時初回加算の自動発生
               MOVE    SYS-1008-NYUKENSA-FLG
                                       TO  SPA-GMN-NYUKENSA
      *        初診再診料の自動発生
               MOVE    SYS-1008-INITAUTO-FLG
                                       TO  SPA-GMN-INITAUTO
      *v4.8
      *        時間外緊急院内チェック
               MOVE    SYS-1008-JGAIKEIKBN-FLG
                                       TO  SPA-GMN-JGAIKEIKBN
      *
      *        時間外加算チェック
               MOVE    SYS-1008-JIKANTOKU-CHK
                                       TO  SPA-GMN-JIKANTOKU-CHK
      *        チェック除外時間
               MOVE    SYS-1008-JIKANTOKU-CHK
                                       TO  SPA-GMN-JIKANTOKU-CHK
      *        平日
               IF      SYS-1008-JIKANCHK-WEEK-G    NOT NUMERIC
                   MOVE    ZERO            TO  SYS-1008-JIKANCHK-WEEK-G
               END-IF
               MOVE      SYS-1008-JIKANCHK-WST(1)
                                           TO  SPA-NAI-JIKANCHK-WST(1)
               MOVE      SYS-1008-JIKANCHK-WST(2)
                                           TO  SPA-NAI-JIKANCHK-WST(2)
               MOVE      SYS-1008-JIKANCHK-WED(1)
                                           TO  SPA-NAI-JIKANCHK-WED(1)
               MOVE      SYS-1008-JIKANCHK-WED(2)
                                           TO  SPA-NAI-JIKANCHK-WED(2)
      *        土曜
               IF      SYS-1008-JIKANCHK-SAT-G    NOT NUMERIC
                   MOVE    ZERO            TO  SYS-1008-JIKANCHK-SAT-G
               END-IF
               MOVE      SYS-1008-JIKANCHK-SST(1)
                                           TO  SPA-NAI-JIKANCHK-SST(1)
               MOVE      SYS-1008-JIKANCHK-SST(2)
                                           TO  SPA-NAI-JIKANCHK-SST(2)
               MOVE      SYS-1008-JIKANCHK-SED(1)
                                           TO  SPA-NAI-JIKANCHK-SED(1)
               MOVE      SYS-1008-JIKANCHK-SED(2)
                                           TO  SPA-NAI-JIKANCHK-SED(2)
      *H28.8
      *        相互作用チェック区分
               MOVE    SYS-1008-INTERACT-CHKKBN
                                           TO  SPA-GMN-KINKCHK
      *H28.11  薬剤投与量チェック区分
               MOVE    SYS-1008-TOUYOCHK-FLG
                                           TO  SPA-GMN-TOUYOCHK
      *H29.4   投薬３０日超チェック区分
               MOVE    SYS-1008-SHOHOU30CHK
                                           TO  SPA-GMN-SHOHOU30CHK
           END-IF
      *    チェック除外時間
           PERFORM VARYING     IDT     FROM    1   BY  1
                   UNTIL       IDT     >   2
               MOVE    SPA-NAI-JIKANWST-HH(IDT)
                                      TO  SPA-GMN-JIKANWST-HH(IDT)
               MOVE    SPA-NAI-JIKANWST-MM(IDT)
                                      TO  SPA-GMN-JIKANWST-MM(IDT)
               MOVE    SPA-NAI-JIKANWED-HH(IDT)
                                      TO  SPA-GMN-JIKANWED-HH(IDT)
               MOVE    SPA-NAI-JIKANWED-MM(IDT)
                                      TO  SPA-GMN-JIKANWED-MM(IDT)
               MOVE    SPA-NAI-JIKANSST-HH(IDT)
                                      TO  SPA-GMN-JIKANSST-HH(IDT)
               MOVE    SPA-NAI-JIKANSST-MM(IDT)
                                      TO  SPA-GMN-JIKANSST-MM(IDT)
               MOVE    SPA-NAI-JIKANSED-HH(IDT)
                                      TO  SPA-GMN-JIKANSED-HH(IDT)
               MOVE    SPA-NAI-JIKANSED-MM(IDT)
                                      TO  SPA-GMN-JIKANSED-MM(IDT)
           END-PERFORM
      *
      *    入院初診料
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-NYUSYOSIN
                                       =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST(IDY)
                                               TO SPA-GMN-NYUSYOSIN-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-GMN-DATA-MAX    TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-DATALST(01) TO SPA-GMN-NYUSYOSIN-G
           END-IF
      *
      *    初期設定
           PERFORM VARYING IDX    FROM     1   BY  1
                   UNTIL   IDX    >   7
               IF      IDX             =   2   OR  3  OR  4
                   IF     SPA-GMN-DATA (IDX)   =   "0"  OR  "1"
                                                        OR  "2"
                       CONTINUE
                   ELSE
                       MOVE    SPA-GMN-DATALST2(01)
                                       TO  SPA-GMN-DATA-G(IDX)
                   END-IF
               ELSE
                   IF     SPA-GMN-DATA (IDX)   =   "0"  OR  "1"
                       CONTINUE
                   ELSE
                       MOVE    SPA-GMN-DATALST(01)
                                       TO  SPA-GMN-DATA-G(IDX)
                   END-IF
               END-IF
           END-PERFORM
      *
           EVALUATE    SPA-GMN-GAIMOVE
                WHEN    "1"
                    MOVE    SPA-GMN-GAIMOVE-LIST(2)
                                               TO  SPA-GMN-GAIMOVE-G
                WHEN    "2"
                    MOVE    SPA-GMN-GAIMOVE-LIST(3)
                                               TO  SPA-GMN-GAIMOVE-G
                WHEN    "3"
                    MOVE    SPA-GMN-GAIMOVE-LIST(4)
                                               TO  SPA-GMN-GAIMOVE-G
                WHEN    OTHER
                    MOVE    SPA-GMN-GAIMOVE-LIST(1)
                                               TO  SPA-GMN-GAIMOVE-G
            END-EVALUATE
      *
           IF      SPA-GMN-GAIKANRIKBN-CHK   =   SPACE
                MOVE    "1"            TO  SPA-GMN-GAIKANRIKBN-CHK
           END-IF
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GAIKANRIKBN-MAX
               IF      SPA-GMN-GAIKANRIKBN-CHK
                                       =   SPA-GMN-GAIKANRIKBN-LIST(IDY)
                                                               (1:1)
                   MOVE    SPA-GMN-GAIKANRIKBN-LIST(IDY)
                                               TO SPA-GMN-GAIKANRIKBN-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-GAIKANRIKBN-MAX TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
                MOVE    SPA-GMN-GAIKANRIKBN-LIST(1)
                                               TO  SPA-GMN-GAIKANRIKBN-G
           END-IF
      *
      *    時間外加算特例乳幼児
      *H30.4
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-JIKANTOKU-MAX
               IF      SPA-GMN-JIKANTOKU-NYU
                                       =   SPA-GMN-JIKANTOKU-NYU-LIST
                                                (IDY)(1:1)
                   MOVE    SPA-GMN-JIKANTOKU-NYU-LIST(IDY)
                                           TO SPA-GMN-JIKANTOKU-NYU-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-GMN-JIKANTOKU-MAX   TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-JIKANTOKU-NYU-LIST(01)
                                       TO SPA-GMN-JIKANTOKU-NYU-G
           END-IF
      *
      *    画像診断管理加算
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-GAZOUKSN1
                                       =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST(IDY)
                                           TO SPA-GMN-GAZOUKSN1-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-GMN-DATA-MAX    TO  IDY
               END-IF
           END-PERFORM
      *    初期値は’１’とする
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-DATALST(02) TO SPA-GMN-GAZOUKSN1-G
           END-IF
      *
      *    中途終了展開時の自動発生
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-CHUTOTENKAI
                                       =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST(IDY)
                                           TO SPA-GMN-CHUTOTENKAI-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-GMN-DATA-MAX    TO  IDY
               END-IF
           END-PERFORM
      *    初期値は’１’とする
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-DATALST(02) TO SPA-GMN-CHUTOTENKAI-G
           END-IF
      *    訂正時の自動発生
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-TEISEIAUTO
                                       =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST(IDY)
                                           TO SPA-GMN-TEISEIAUTO-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-GMN-DATA-MAX    TO  IDY
               END-IF
           END-PERFORM
      *    初期値は’１’とする
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-DATALST(02) TO SPA-GMN-TEISEIAUTO-G
           END-IF
      *    訂正時の自動発生（特定疾患処方管理）
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-TEISEITOKU
                                       =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST(IDY)
                                           TO SPA-GMN-TEISEITOKU-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-GMN-DATA-MAX    TO  IDY
               END-IF
           END-PERFORM
      *    初期値は’０’とする
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-DATALST(01) TO SPA-GMN-TEISEITOKU-G
           END-IF
      *H29.3
      *    訂正時の展開（処方料）
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-TEISHOHOU-MAX
               IF      SPA-GMN-TEISHOHOU
                                       =   SPA-GMN-TEISHOHOU-LIST(IDY)
                                                        (1:1)
                   MOVE    SPA-GMN-TEISHOHOU-LIST(IDY)
                                           TO SPA-GMN-TEISHOHOU-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-TEISHOHOU-MAX   TO  IDY
               END-IF
           END-PERFORM
      *    初期値は’０’とする
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-TEISHOHOU-LIST(01)
                                           TO SPA-GMN-TEISHOHOU-G
           END-IF
      *
      *    包括まとめ
           IF      SPA-GMN-HOUKATU    =   SPACE
               MOVE    "0"            TO  SPA-GMN-HOUKATU
           END-IF
           EVALUATE    SPA-GMN-HOUKATU
                WHEN    "0"
                    MOVE    SPA-GMN-HOUKATU-LIST(1)
                                               TO  SPA-GMN-HOUKATU-G
                WHEN    "1"
                    MOVE    SPA-GMN-HOUKATU-LIST(2)
                                               TO  SPA-GMN-HOUKATU-G
                WHEN    OTHER
                    MOVE    SPA-GMN-HOUKATU-LIST(1)
                                               TO  SPA-GMN-HOUKATU-G
            END-EVALUATE
      *
      *    数量ゼロ入力
           IF      SPA-GMN-SURYOZERO  =   SPACE
               MOVE    "0"            TO  SPA-GMN-SURYOZERO
           END-IF
           EVALUATE    SPA-GMN-SURYOZERO
                WHEN    "0"
                    MOVE    SPA-GMN-SURYOZERO-LIST(1)
                                               TO  SPA-GMN-SURYOZERO-G
                WHEN    "1"
                    MOVE    SPA-GMN-SURYOZERO-LIST(2)
                                               TO  SPA-GMN-SURYOZERO-G
                WHEN    OTHER
                    MOVE    SPA-GMN-SURYOZERO-LIST(1)
                                               TO  SPA-GMN-SURYOZERO-G
            END-EVALUATE
      *
      *    入院調基チェック
           IF      SPA-GMN-CHOKICHK   =   SPACE
               MOVE    "0"            TO  SPA-GMN-CHOKICHK
           END-IF
           EVALUATE    SPA-GMN-CHOKICHK
                WHEN    "0"
                    MOVE    SPA-GMN-CHOKICHK-LIST(1)
                                               TO  SPA-GMN-CHOKICHK-G
                WHEN    "1"
                    MOVE    SPA-GMN-CHOKICHK-LIST(2)
                                               TO  SPA-GMN-CHOKICHK-G
                WHEN    OTHER
                    MOVE    SPA-GMN-CHOKICHK-LIST(1)
                                               TO  SPA-GMN-CHOKICHK-G
            END-EVALUATE
      *    チェック除外時間
           IF      SPA-GMN-JIKANTOKU-CHK   =   SPACE
               MOVE    "0"            TO  SPA-GMN-JIKANTOKU-CHK
           END-IF
           EVALUATE    SPA-GMN-JIKANTOKU-CHK
                WHEN    "0"
                    MOVE    SPA-GMN-JIKANTOKU-CHK-LIST(1)
                                           TO  SPA-GMN-JIKANTOKU-CHK-G
                WHEN    "1"
                    MOVE    SPA-GMN-JIKANTOKU-CHK-LIST(2)
                                           TO  SPA-GMN-JIKANTOKU-CHK-G
                WHEN    OTHER
                    MOVE    SPA-GMN-JIKANTOKU-CHK-LIST(1)
                                           TO  SPA-GMN-JIKANTOKU-CHK-G
           END-EVALUATE
      *    クリア確認
           IF      SPA-GMN-CLEARCHK   =   SPACE
               MOVE    "0"            TO  SPA-GMN-CLEARCHK
           END-IF
           EVALUATE    SPA-GMN-CLEARCHK
                WHEN    "0"
                    MOVE    SPA-GMN-CLEARCHK-LIST(1)
                                               TO  SPA-GMN-CLEARCHK-G
                WHEN    "1"
                    MOVE    SPA-GMN-CLEARCHK-LIST(2)
                                               TO  SPA-GMN-CLEARCHK-G
                WHEN    OTHER
                    MOVE    SPA-GMN-CLEARCHK-LIST(1)
                                               TO  SPA-GMN-CLEARCHK-G
            END-EVALUATE
      *    薬剤情報提供料チェック
           IF      SPA-GMN-YAKJYOCHK  =   SPACE
               MOVE    "1"            TO  SPA-GMN-YAKJYOCHK
           END-IF
           EVALUATE    SPA-GMN-YAKJYOCHK
                WHEN    "0"
                    MOVE    SPA-GMN-YAKJYOCHK-LIST(1)
                                               TO  SPA-GMN-YAKJYOCHK-G
                WHEN    "1"
                    MOVE    SPA-GMN-YAKJYOCHK-LIST(2)
                                               TO  SPA-GMN-YAKJYOCHK-G
                WHEN    OTHER
                    MOVE    SPA-GMN-YAKJYOCHK-LIST(2)
                                               TO  SPA-GMN-YAKJYOCHK-G
           END-EVALUATE
      *    前回保険組合せ相違チェック
           IF      SPA-GMN-ZENHKNCHK  =   SPACE
               MOVE    "0"            TO  SPA-GMN-ZENHKNCHK
           END-IF
           EVALUATE    SPA-GMN-ZENHKNCHK
                WHEN    "0"
                    MOVE    SPA-GMN-ZENHKNCHK-LIST(1)
                                               TO  SPA-GMN-ZENHKNCHK-G
                WHEN    "1"
                    MOVE    SPA-GMN-ZENHKNCHK-LIST(2)
                                               TO  SPA-GMN-ZENHKNCHK-G
                WHEN    "2"
                    MOVE    SPA-GMN-ZENHKNCHK-LIST(3)
                                               TO  SPA-GMN-ZENHKNCHK-G
                WHEN    OTHER
                    MOVE    SPA-GMN-ZENHKNCHK-LIST(1)
                                               TO  SPA-GMN-ZENHKNCHK-G
           END-EVALUATE
      *    手帳加算確認チェック
           IF      SPA-GMN-TECHOKBN    =   SPACE
               MOVE    "0"             TO  SPA-GMN-TECHOKBN
           END-IF
           EVALUATE    SPA-GMN-TECHOKBN
                WHEN    "0"
                    MOVE    SPA-GMN-TECHOKBN-LIST(1)
                                               TO  SPA-GMN-TECHOKBN-G
                WHEN    "1"
                    MOVE    SPA-GMN-TECHOKBN-LIST(2)
                                               TO  SPA-GMN-TECHOKBN-G
                WHEN    OTHER
                    MOVE    SPA-GMN-TECHOKBN-LIST(1)
                                               TO  SPA-GMN-TECHOKBN-G
           END-EVALUATE
      *v4.8
      *    時間外緊急院内チェック
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-JGAIKEIKBN-MAX
               IF      SPA-GMN-JGAIKEIKBN
                                       =   SPA-GMN-JGAIKEIKBN-LIST
                                                   (IDY)(1:1)
                   MOVE    SPA-GMN-JGAIKEIKBN-LIST(IDY)
                                           TO  SPA-GMN-JGAIKEIKBN-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-JGAIKEIKBN-MAX  TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
      *        初期は「チェックする」
               MOVE    SPA-GMN-JGAIKEIKBN-LIST(2)
                                           TO  SPA-GMN-JGAIKEIKBN-G
           END-IF
      *
      *    診療科コードセット
           PERFORM 4102-SRYKACD-CHK-SEC
      *    病名表示区分
           IF     SPA-GMN-BYOHYOKBN   =   SPACE
               MOVE    "1"             TO  SPA-GMN-BYOHYOKBN
           END-IF
      *H28.12
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-BYOHYOKBN-MAX
               IF      SPA-GMN-BYOHYOKBN
                                       =    SPA-GMN-BYOHYOKBN-LIST
                                                    (IDX)(1:1)
                   MOVE    SPA-GMN-BYOHYOKBN-LIST(IDX)
                                       TO  SPA-GMN-BYOHYOKBN-G
                   MOVE    SPA-GMN-BYOHYOKBN-MAX   TO  IDX
               END-IF
           END-PERFORM
      ***  EVALUATE    SPA-GMN-BYOHYOKBN
      *         WHEN    "0"
      *             MOVE    SPA-GMN-BYOHYOKBN-LIST(1)
      *                                        TO  SPA-GMN-BYOHYOKBN-G
      *         WHEN    "1"
      *             MOVE    SPA-GMN-BYOHYOKBN-LIST(2)
      *                                        TO  SPA-GMN-BYOHYOKBN-G
      *         WHEN    OTHER
      *             MOVE    SPA-GMN-BYOHYOKBN-LIST(2)
      *                                        TO  SPA-GMN-BYOHYOKBN-G
      **** END-EVALUATE
      *    剤登録順区分
           IF     SPA-GMN-ZAIJUNKBN   =   SPACE
               MOVE    "0"             TO  SPA-GMN-ZAIJUNKBN
           END-IF
           EVALUATE    SPA-GMN-ZAIJUNKBN
                WHEN    "0"
                    MOVE    SPA-GMN-ZAIJUNKBN-LIST(1)
                                               TO  SPA-GMN-ZAIJUNKBN-G
                WHEN    "1"
                    MOVE    SPA-GMN-ZAIJUNKBN-LIST(2)
                                               TO  SPA-GMN-ZAIJUNKBN-G
                WHEN    OTHER
                    MOVE    SPA-GMN-ZAIJUNKBN-LIST(1)
                                               TO  SPA-GMN-ZAIJUNKBN-G
           END-EVALUATE
      *    検体検査加算自動発生
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-KENSAAUTO-MAX
               IF      SPA-GMN-KENSAAUTO  =    SPA-GMN-KENSAAUTO-LIST
                                                         (IDY)(1:1)
                   MOVE    SPA-GMN-KENSAAUTO-LIST(IDY)
                                           TO  SPA-GMN-KENSAAUTO-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
                MOVE    SPA-GMN-KENSAAUTO-LIST(1)
                                               TO  SPA-GMN-KENSAAUTO-G
            END-IF
      *    訂正時の自動発生（入院）
           IF     SPA-GMN-TEINYUAUTO  =   SPACE
      *        未設定は外来と同様
               MOVE    SPA-GMN-TEISEIAUTO  TO  SPA-GMN-TEINYUAUTO
           END-IF
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-TEINYUAUTO
                                       =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST(IDY)
                                               TO SPA-GMN-TEINYUAUTO-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-GMN-DATA-MAX    TO  IDY
               END-IF
           END-PERFORM
      *
      *    病名からの自動発生
           IF      SPA-GMN-BYOMEIAUTO  =   SPACE
               MOVE    "1"                 TO  SPA-GMN-BYOMEIAUTO
           END-IF
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-BYOMEIAUTO
                                           =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST(IDY)
                                               TO SPA-GMN-BYOMEIAUTO-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-GMN-DATA-MAX    TO  IDY
               END-IF
           END-PERFORM
      *    入院時初回加算の自動発生
           IF      SPA-GMN-NYUKENSA     =   SPACE
               MOVE    "0"              TO  SPA-GMN-NYUKENSA
           END-IF
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-NYUKENSA
                                           =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST(IDY)
                                               TO SPA-GMN-NYUKENSA-G
                   MOVE    SPA-GMN-DATA-MAX    TO  IDY
               END-IF
           END-PERFORM
      *
      *    初診再診料の自動発生（初期表示は算定する）
           IF      SPA-GMN-INITAUTO     =   SPACE
               MOVE    "1"              TO  SPA-GMN-INITAUTO
           END-IF
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-INITAUTO
                                       =   SPA-GMN-INITAUTO-LIST(IDY)
                                                              (1:1)
                   MOVE    SPA-GMN-INITAUTO-LIST(IDY)
                                               TO SPA-GMN-INITAUTO-G
                   MOVE    SPA-GMN-DATA-MAX    TO  IDY
               END-IF
           END-PERFORM
      *H28.8
      *    相互作用チェック区分
           EVALUATE    SPA-GMN-KINKCHK
               WHEN    "1"
                   MOVE    SPA-GMN-KINKCHK-LIST (2)
                                           TO  SPA-GMN-KINKCHK-G
               WHEN    OTHER
                   MOVE    SPA-GMN-KINKCHK-LIST (1)
                                           TO  SPA-GMN-KINKCHK-G
           END-EVALUATE
      *H28.11
      *    薬剤投与量チェック区分
           IF      SPA-GMN-TOUYOCHK     =   SPACE
               MOVE    "0"              TO  SPA-GMN-TOUYOCHK
           END-IF
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-TOUYOCHK-MAX
               IF      SPA-GMN-TOUYOCHK
                                       =   SPA-GMN-TOUYOCHK-LIST(IDY)
                                                              (1:1)
                   MOVE    SPA-GMN-TOUYOCHK-LIST(IDY)
                                               TO SPA-GMN-TOUYOCHK-G
                   MOVE    SPA-GMN-TOUYOCHK-MAX    TO  IDY
               END-IF
           END-PERFORM
      *H29.4
      *    投薬３０日超チェック区分
           IF      SPA-GMN-SHOHOU30CHK  =   SPACE
               MOVE    "0"              TO  SPA-GMN-SHOHOU30CHK
           END-IF
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SHOHOU30CHK-MAX
               IF      SPA-GMN-SHOHOU30CHK
                                       =   SPA-GMN-SHOHOU30CHK-LIST
                                                        (IDY) (1:1)
                   MOVE    SPA-GMN-SHOHOU30CHK-LIST(IDY)
                                               TO SPA-GMN-SHOHOU30CHK-G
                   MOVE    SPA-GMN-SHOHOU30CHK-MAX    TO  IDY
               END-IF
           END-PERFORM
      *
            MOVE    1                  TO  SPA-GMN-CUR
      *
           . 
       310-SPASET-1007-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-1017-SEC         SECTION.
      *
      *    患者登録機能制御情報
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1017"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1017-REC
      *        カルテの病名対象
               MOVE    SYS-1017-KARUTE-BYOMEI
                                       TO  SPA-GMN-BYOMEIKBN
      *        人名辞書使用
               MOVE    SYS-1017-JINMEI-USE
                                       TO  SPA-GMN-JISYOKBN
      *        患者登録のカルテ発行
               MOVE    SYS-1017-KARUTE-HAKKO
                                       TO  SPA-GMN-HAKKOKBN
      *        自費保険の補助区分
               MOVE    SYS-1017-JIHIHKN-TAX
                                       TO  SPA-GMN-HOJYOKBN
      *        同姓同名リスト
               MOVE    SYS-1017-DOUMEI-CHKFLG
                                       TO  SPA-GMN-DOUMEI-CHKFLG
      *        二重登録疑い区分
               MOVE    SYS-1017-JYUFUKU-CHKKBN
                                       TO  SPA-GMN-JYUFUKU-CHKKBN
      *        地方公費単独チェック
               MOVE    SYS-1017-KOHONLY-CHKFLG
                                       TO  SPA-GMN-KOHONLY-CHKFLG
      *        患者情報削除機能
               MOVE    SYS-1017-PTNUMDEL-FLG
                                       TO  SPA-GMN-PTNUMDEL-FLG
      *        本人・家族区分
               MOVE    SYS-1017-HONKZKKBN-FLG
                                       TO  SPA-GMN-HONKZKKBN-FLG
      *        家族年齢
               MOVE    SYS-1017-KZKNEN
                                       TO  SPA-GMN-KZKNEN
      *        被保険者区分
               MOVE    SYS-1017-HIHKNJAKBN-FLG
                                       TO  SPA-GMN-HIHKNJAKBN-FLG
      *        世帯主名自動記載区分
               MOVE    SYS-1017-SETAIKBN-FLG
                                       TO  SPA-GMN-SETAIKBN-FLG
      *        グループ医療機関患者リスト
               MOVE    SYS-1017-GRPPTLST-FLG
                                       TO  SPA-GMN-GRPPTLST-FLG
      *        公費上限額履歴チェック
               MOVE    SYS-1017-KOHRRKKBN-FLG
                                       TO  SPA-GMN-KOHRRKKBN-FLG
      *        郵便番号自動記載区分
               MOVE    SYS-1017-POSTKBN-FLG
                                       TO  SPA-GMN-POSTKBN-FLG
      *        都道府県名記載区分
               MOVE    SYS-1017-KENMEIKBN-FLG
                                       TO  SPA-GMN-KENMEIKBN-FLG
      *        京都府通り名称編集区分
               MOVE    SYS-1017-TOWNNAME2KBN-FLG
                                       TO  SPA-GMN-TOWNNAME2KBN
      *        公費法別一覧表示区分
               MOVE    SYS-1017-KOHHBTKBN-FLG
                                       TO  SPA-GMN-KOHHBTKBN
      *        保険初期表示区分
               MOVE    SYS-1017-HKNINITKBN-FLG
                                       TO  SPA-GMN-HKNINITKBN
           END-IF
      *
      *    初期設定
           EVALUATE    SPA-GMN-BYOMEIKBN
                WHEN    "1"
                    MOVE    SPA-GMN-BYOMEIKBN-LIST(2)
                                               TO  SPA-GMN-BYOMEIKBN-G
                WHEN    "2"
                    MOVE    SPA-GMN-BYOMEIKBN-LIST(3)
                                               TO  SPA-GMN-BYOMEIKBN-G
                WHEN    OTHER
                    MOVE    SPA-GMN-BYOMEIKBN-LIST(1)
                                               TO  SPA-GMN-BYOMEIKBN-G
           END-EVALUATE
      *
           EVALUATE    SPA-GMN-JISYOKBN
                WHEN    "1"
                    MOVE    SPA-GMN-JISYOKBN-LIST(2)
                                               TO  SPA-GMN-JISYOKBN-G
                WHEN    OTHER
                    MOVE    SPA-GMN-JISYOKBN-LIST(1)
                                               TO  SPA-GMN-JISYOKBN-G
           END-EVALUATE
      *
           EVALUATE    SPA-GMN-HAKKOKBN
                WHEN    "1"
                    MOVE    SPA-GMN-HAKKOKBN-LIST(2)
                                               TO  SPA-GMN-HAKKOKBN-G
                WHEN    "2"
                    MOVE    SPA-GMN-HAKKOKBN-LIST(3)
                                               TO  SPA-GMN-HAKKOKBN-G
                WHEN    OTHER
                    MOVE    SPA-GMN-HAKKOKBN-LIST(1)
                                               TO  SPA-GMN-HAKKOKBN-G
           END-EVALUATE
      *
           EVALUATE    SPA-GMN-HOJYOKBN
                WHEN    "1"
                    MOVE    SPA-GMN-HOJYOKBN-LIST(2)
                                               TO  SPA-GMN-HOJYOKBN-G
                WHEN    OTHER
                    MOVE    SPA-GMN-HOJYOKBN-LIST(1)
                                               TO  SPA-GMN-HOJYOKBN-G
           END-EVALUATE
      *
           EVALUATE    SPA-GMN-DOUMEI-CHKFLG
               WHEN    "1"
                   MOVE    SPA-GMN-DOUMEI-CHKFLG-LIST(2)
                                           TO  SPA-GMN-DOUMEI-CHKFLG-G
               WHEN    OTHER
                   MOVE    SPA-GMN-DOUMEI-CHKFLG-LIST(1)
                                           TO  SPA-GMN-DOUMEI-CHKFLG-G
           END-EVALUATE
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-JYUFUKU-MAX ) OR
                              (FLG-OK  =   1    )
               IF      SPA-GMN-JYUFUKU-CHKKBN
                                       =   SPA-GMN-JYUFUKU-CHKKBN-LIST
                                                        (IDX)(1:1)
                   MOVE    SPA-GMN-JYUFUKU-CHKKBN-LIST(IDX)
                                          TO  SPA-GMN-JYUFUKU-CHKKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-JYUFUKU-CHKKBN-LIST(1)
                                          TO  SPA-GMN-JYUFUKU-CHKKBN-G
           END-IF
      *    地方公費単独チェック
           EVALUATE    SPA-GMN-KOHONLY-CHKFLG
               WHEN    "0"
                   MOVE    SPA-GMN-KOHONLY-CHKFLG-LIST(1)
                                           TO  SPA-GMN-KOHONLY-CHKFLG-G
               WHEN    "1"
                   MOVE    SPA-GMN-KOHONLY-CHKFLG-LIST(2)
                                           TO  SPA-GMN-KOHONLY-CHKFLG-G
               WHEN    OTHER
                   MOVE    SPA-GMN-KOHONLY-CHKFLG-LIST(2)
                                           TO  SPA-GMN-KOHONLY-CHKFLG-G
           END-EVALUATE
      *    患者情報削除機能
           EVALUATE    SPA-GMN-PTNUMDEL-FLG
               WHEN    "0"
                   MOVE    SPA-GMN-PTNUMDEL-FLG-LIST(1)
                                           TO  SPA-GMN-PTNUMDEL-FLG-G
               WHEN    "1"
                   MOVE    SPA-GMN-PTNUMDEL-FLG-LIST(2)
                                           TO  SPA-GMN-PTNUMDEL-FLG-G
               WHEN    OTHER
                   MOVE    SPA-GMN-PTNUMDEL-FLG-LIST(1)
                                           TO  SPA-GMN-PTNUMDEL-FLG-G
           END-EVALUATE
      *    本人・家族区分設定
           EVALUATE    SPA-GMN-HONKZKKBN-FLG
               WHEN    "0"
                   MOVE    SPA-GMN-HONKZKKBN-FLG-LIST(1)
                                           TO  SPA-GMN-HONKZKKBN-FLG-G
               WHEN    "1"
                   MOVE    SPA-GMN-HONKZKKBN-FLG-LIST(2)
                                           TO  SPA-GMN-HONKZKKBN-FLG-G
               WHEN    OTHER
                   MOVE    SPA-GMN-HONKZKKBN-FLG-LIST(1)
                                           TO  SPA-GMN-HONKZKKBN-FLG-G
           END-EVALUATE
      *    被保険者区分設定
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-HIHKNJAKBN-MAX
               IF      SPA-GMN-HIHKNJAKBN-FLG
                                       =   SPA-GMN-HIHKNJAKBN-FLG-LIST
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-HIHKNJAKBN-FLG-LIST(IDX)
                                           TO  SPA-GMN-HIHKNJAKBN-FLG-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-GMN-HIHKNJAKBN-MAX    TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-HIHKNJAKBN-FLG-LIST(1)
                                           TO  SPA-GMN-HIHKNJAKBN-FLG-G
           END-IF
      *    世帯主自動記載設定
           IF      SPA-GMN-SETAIKBN-FLG    =   SPACE
               MOVE    "1"                 TO  SPA-GMN-SETAIKBN-FLG
           END-IF
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SETAIKBN-MAX
               IF      SPA-GMN-SETAIKBN-FLG
                                       =   SPA-GMN-SETAIKBN-FLG-LIST
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-SETAIKBN-FLG-LIST(IDX)
                                           TO  SPA-GMN-SETAIKBN-FLG-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-GMN-SETAIKBN-MAX    TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SETAIKBN-FLG-LIST(2)
                                           TO  SPA-GMN-SETAIKBN-FLG-G
           END-IF
      *    グループ医療機関患者リスト
           IF      SPA-GMN-GRPPTLST-FLG    =   SPACE
               MOVE    "0"                 TO  SPA-GMN-GRPPTLST-FLG
           END-IF
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-GRPPTLST-MAX
               IF      SPA-GMN-GRPPTLST-FLG
                                       =   SPA-GMN-GRPPTLST-FLG-LIST
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-GRPPTLST-FLG-LIST(IDX)
                                           TO  SPA-GMN-GRPPTLST-FLG-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-GMN-GRPPTLST-MAX    TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-GRPPTLST-FLG-LIST(1)
                                           TO  SPA-GMN-GRPPTLST-FLG-G
           END-IF
      *    公費上限額履歴チェック
           IF      SPA-GMN-KOHRRKKBN-FLG    =   SPACE
               MOVE    "0"                 TO  SPA-GMN-KOHRRKKBN-FLG
           END-IF
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-KOHRRKKBN-MAX
               IF      SPA-GMN-KOHRRKKBN-FLG
                                       =   SPA-GMN-KOHRRKKBN-FLG-LIST
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-KOHRRKKBN-FLG-LIST(IDX)
                                           TO  SPA-GMN-KOHRRKKBN-FLG-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-GMN-KOHRRKKBN-MAX    TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-KOHRRKKBN-FLG-LIST(1)
                                           TO  SPA-GMN-KOHRRKKBN-FLG-G
           END-IF
      *    郵便番号自動記載区分
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-POSTKBN-MAX
               IF      SPA-GMN-POSTKBN-FLG
                                       =   SPA-GMN-POSTKBN-FLG-LIST
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-POSTKBN-FLG-LIST(IDX)
                                           TO  SPA-GMN-POSTKBN-FLG-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-GMN-POSTKBN-MAX    TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-POSTKBN-FLG-LIST(2)
                                           TO  SPA-GMN-POSTKBN-FLG-G
           END-IF
      *    都道府県名記載区分
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-KENMEIKBN-MAX
               IF      SPA-GMN-KENMEIKBN-FLG
                                       =   SPA-GMN-KENMEIKBN-FLG-LIST
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-KENMEIKBN-FLG-LIST(IDX)
                                           TO  SPA-GMN-KENMEIKBN-FLG-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-GMN-KENMEIKBN-MAX    TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-KENMEIKBN-FLG-LIST(2)
                                           TO  SPA-GMN-KENMEIKBN-FLG-G
           END-IF
      *    京都府通り名称編集区分
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TOWNNAME2KBN-MAX
               IF      SPA-GMN-TOWNNAME2KBN
                                       =   SPA-GMN-TOWNNAME2KBN-LIST
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-TOWNNAME2KBN-LIST(IDX)
                                           TO  SPA-GMN-TOWNNAME2KBN-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-GMN-TOWNNAME2KBN-MAX    TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-TOWNNAME2KBN-LIST(2)
                                           TO  SPA-GMN-TOWNNAME2KBN-G
           END-IF
      *    公費法別一覧表示区分
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-KOHHBTKBN-MAX
               IF      SPA-GMN-KOHHBTKBN
                                       =   SPA-GMN-KOHHBTKBN-LIST
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-KOHHBTKBN-LIST(IDX)
                                           TO  SPA-GMN-KOHHBTKBN-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-GMN-KOHHBTKBN-MAX    TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-KOHHBTKBN-LIST(1)
                                           TO  SPA-GMN-KOHHBTKBN-G
           END-IF
      *    保険初期表示区分
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-HKNINITKBN-MAX
               IF      SPA-GMN-HKNINITKBN
                                       =   SPA-GMN-HKNINITKBN-LIST
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-HKNINITKBN-LIST(IDX)
                                           TO  SPA-GMN-HKNINITKBN-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-GMN-HKNINITKBN-MAX    TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-HKNINITKBN-LIST(1)
                                           TO  SPA-GMN-HKNINITKBN-G
           END-IF
      *
           MOVE    201                 TO  SPA-GMN-CUR
      *
           . 
       310-SPASET-1017-EXT.
           EXIT.
      *ver.4.7
      *
      *****************************************************************
      *    背景色初期設定処理
      *****************************************************************
       3109-COLORS-INIT-SEC         SECTION.
      *
           MOVE    "#000000"           TO  SPA-GMN-BGCOLORS
           MOVE    "#000000"           TO  SPA-GMN-BGCOLORSJ0
           MOVE    "#000000"           TO  SPA-GMN-BGCOLORSDAY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   8   )
               MOVE    "#000000"       TO  SPA-GMN-BGCOLORSJ (IDX)
           END-PERFORM
           MOVE    "#000000"           TO  SPA-GMN-ERRCOLORS
           MOVE    "#000000"           TO  SPA-GMN-KEICOLORS
      *
           .
       3109-COLORS-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-1038-SEC         SECTION.
      *
      *    診療行為機能情報
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1038"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1038-REC
           ELSE
               INITIALIZE  SYS-1038-REC
           END-IF
      *
           MOVE    SYS-1038-NYKNKNRKBN TO  SPA-GMN-SRYNYKNKNRKBN
           MOVE    SYS-1038-KAGRPFLG   TO  SPA-GMN-KAGRPFLG
           MOVE    SYS-1038-HAKHOUFLG  TO  SPA-GMN-HAKHOUFLG
      *    訂正請求書金額選択
           MOVE    SYS-1038-TEIKINKBN  TO  SPA-GMN-TEIKINKBN
      *    薬剤情報提供発行
           MOVE    SYS-1038-YKJCHKKBN  TO  SPA-GMN-YKJCHKKBN
      *    お薬手帳発行
           MOVE    SYS-1038-OKUSURIKBN     TO  SPA-GMN-OKUSURIKBN
      *    薬情・お薬手帳対象
           MOVE    SYS-1038-YKJOKUKBN      TO  SPA-GMN-YKJOKUKBN
      *
           MOVE    SYS-1038-DOSRYKBNFLG    TO  SPA-GMN-DOSRYKBNFLG
           MOVE    SYS-1038-WKSRYHYOFLG    TO  SPA-GMN-WKSRYHYOFLG
      *    検査正式名称表示
           MOVE    SYS-1038-FORMALFLG      TO  SPA-GMN-FORMALFLG
      *    腫瘍マーカ一覧表示
           MOVE    SYS-1038-AKUSEIHYOFLG   TO  SPA-GMN-AKUSEIHYO
      *    同日再診・訂正選択
           MOVE    SYS-1038-TEISEIFLG      TO  SPA-GMN-TEISEIFLG
      *    皮下筋肉注射料変換選択
           MOVE    SYS-1038-CHG310FLG      TO  SPA-GMN-CHG310FLG
      *    投薬診療種別変換選択
           MOVE    SYS-1038-CHGSRYSYU-FLG  TO  SPA-GMN-CHGSRYSYU
      *    レセプトブレビュー
           MOVE    SYS-1038-RESPRVFLG      TO  SPA-GMN-RESPRV038
      *    残量廃棄区分
           MOVE    SYS-1038-HAIKIKBN       TO  SPA-GMN-HAIKIKBN
      *    精神療法２０未満加算区分
           MOVE    SYS-1038-SEI20KSNKBN    TO  SPA-GMN-SEI20KSNKBN
      *H29.7
      *    心身療法２０未満加算区分
           MOVE    SYS-1038-SIN20KSNKBN    TO  SPA-GMN-SIN20KSNKBN
      *    入金・返金額設定区分
           MOVE    SYS-1038-NYKHNKKBN      TO  SPA-GMN-NYKHNKKBN
      *    発行日変更区分
           MOVE    SYS-1038-YMDHENKBN      TO  SPA-GMN-YMDHENKBN
      *H24.4
      *    確認画面表示区分
           MOVE    SYS-1038-GENERICFLG     TO  SPA-GMN-GENERICFLG
      *H24.12
      *    レセプトブレビューレイアウト
           MOVE    SYS-1038-PRVLAYOUT      TO  SPA-GMN-PRVLAYOUT038
      *H28.6
      *    セット展開表示
           MOVE    SYS-1038-SETALLKBN      TO  SPA-GMN-SETALLKBN
      *H30.9
      *選択式コメント一覧区分
           MOVE    SYS-1038-RECEKISAIFLG   TO  SPA-GMN-RECEKISAIFLG
      *
      *ver.4.7
      *    リハビリ・患者情報表示区分
           PERFORM VARYING     IDX     FROM   1   BY  1
     ****          UNTIL       IDX     >   5
                   UNTIL       IDX     >   6
      *H28.10
               IF      IDX             <=  5
      *            リハビリ・患者情報・介護表示区分１〜５
                   MOVE    SYS-1038-GMNHYOKBN (IDX)
                                           TO  SPA-NAI-GMNHYOKBN (IDX)
               ELSE
      *            リハビリ・患者情報・介護表示区分６
                   MOVE    SYS-1038-GMNHYOKBN-6
                                           TO  SPA-NAI-GMNHYOKBN (IDX)
               END-IF
      *******  IF      SYS-1038-GMNHYOKBN (IDX)
      *        表示区分がすべて空白の時、初期設定
               IF     (SYS-1038-GMNHYOKBN-G    =   SPACE )
                  AND (SYS-1038-GMNHYOKBN-6    =   SPACE )
      *            初期設定
                   EVALUATE    IDX
                   WHEN    1
                       MOVE    "1"         TO  SPA-NAI-GMNHYOKBN (IDX)
                   WHEN    2
                       MOVE    "2"         TO  SPA-NAI-GMNHYOKBN (IDX)
                   WHEN    3
                       MOVE    "3"         TO  SPA-NAI-GMNHYOKBN (IDX)
                   WHEN    4
                       MOVE    "4"         TO  SPA-NAI-GMNHYOKBN (IDX)
                   WHEN    5
                       MOVE    "5"         TO  SPA-NAI-GMNHYOKBN (IDX)
                   WHEN    6
                       MOVE    "6"         TO  SPA-NAI-GMNHYOKBN (IDX)
                   END-EVALUATE
               END-IF
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING IDY         FROM    1   BY  1
                       UNTIL   IDY         >   SPA-GMN-GMNHYOKBN-MAX
                   IF      SPA-NAI-GMNHYOKBN (IDX)
                                           =   SPA-GMN-GMNHYOKBNL
                                                             (IDY)
                       MOVE    SPA-GMN-GMNHYOKBN-LST (IDY)
                                           TO  SPA-GMN-GMNHYOKBN-G (IDX)
                       MOVE    1               TO  FLG-OK
                       MOVE    SPA-GMN-GMNHYOKBN-MAX
                                               TO  IDY
                   END-IF
               END-PERFORM
               IF      SPA-NAI-GMNHYOKBN (IDX) =   "0"
                   MOVE    SPACE           TO  SPA-GMN-GMNHYOKBN-G (IDX)
               END-IF
           END-PERFORM
      *
      *    入力スクロール背景色
           MOVE    SYS-1038-BGCOLORS       TO  SPA-GMN-BGCOLORS
           IF      SPA-GMN-BGCOLORS        =   SPACE
      *        空白は初期表示
               MOVE    "#FAEBD7"           TO  SPA-GMN-BGCOLORS
           END-IF
      *    入力スクロール背景行区分
           MOVE    SYS-1038-BGLINEKBN      TO  SPA-GMN-BGLINEKBN
      *    環境設定背景色
           MOVE    SYS-1038-BGCOLORSJ0     TO  SPA-GMN-BGCOLORSJ0
           IF      SPA-GMN-BGCOLORSJ0      =   SPACE
               MOVE    "#000000"           TO  SPA-GMN-BGCOLORSJ0
           END-IF
           MOVE    SYS-1038-BGCOLORSDAY    TO  SPA-GMN-BGCOLORSDAY
           IF      SPA-GMN-BGCOLORSDAY     =   SPACE
               MOVE    "#000000"           TO  SPA-GMN-BGCOLORSDAY
           END-IF
           MOVE    SYS-1038-BGCOLORSJ-G    TO  SPA-GMN-BGCOLORSJ-G
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   8   )
               IF      SPA-GMN-BGCOLORSJ (IDX) =   SPACE
      *             クロは背景色なし
                   MOVE    "#000000"       TO  SPA-GMN-BGCOLORSJ (IDX)
               END-IF
           END-PERFORM
      *    エラー背景色
           MOVE    SYS-1038-ERRCOLORS      TO  SPA-GMN-ERRCOLORS
           IF      SPA-GMN-ERRCOLORS       =   SPACE
      *        空白は初期表示
               MOVE    "#FFB5C5"           TO  SPA-GMN-ERRCOLORS
           END-IF
      *    警告背景色
           MOVE    SYS-1038-KEICOLORS      TO  SPA-GMN-KEICOLORS
           IF      SPA-GMN-KEICOLORS       =   SPACE
      *        空白は初期表示
               MOVE    "#FFB5C5"           TO  SPA-GMN-KEICOLORS
           END-IF
      *
      *    入金管理区分
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SRYNYKNKNRKBN-MAX
               IF      SPA-GMN-SRYNYKNKNRKBN
                                       =   SPA-GMN-SRYNYKNKNRKBNL (IDY)
                   MOVE    SPA-GMN-SRYNYKNKNRKBNLST(IDY)
                                       TO  SPA-GMN-SRYNYKNKNRKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-SRYNYKNKNRKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SRYNYKNKNRKBNLST (1)
                                       TO  SPA-GMN-SRYNYKNKNRKBN-G
           END-IF
      *    複数科まとめ集計
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-KAGRPFLG-MAX
               IF      SPA-GMN-KAGRPFLG
                                       =   SPA-GMN-KAGRPFLGL (IDY)
                   MOVE    SPA-GMN-KAGRPFLG-LST (IDY)
                                       TO  SPA-GMN-KAGRPFLG-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-KAGRPFLG-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-KAGRPFLG-LST (1)
                                       TO  SPA-GMN-KAGRPFLG-G
           END-IF
      *    請求書発行方法
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-HAKHOUFLG-MAX
               IF      SPA-GMN-HAKHOUFLG
                                       =   SPA-GMN-HAKHOUFLGL (IDY)
                   MOVE    SPA-GMN-HAKHOUFLG-LST (IDY)
                                       TO  SPA-GMN-HAKHOUFLG-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-HAKHOUFLG-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-HAKHOUFLG-LST (1)
                                       TO  SPA-GMN-HAKHOUFLG-G
           END-IF
      *    訂正請求書金額選択
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-TEIKINKBN-MAX
               IF      SPA-GMN-TEIKINKBN
                                       =   SPA-GMN-TEIKINKBNL (IDY)
                   MOVE    SPA-GMN-TEIKINKBN-LST (IDY)
                                       TO  SPA-GMN-TEIKINKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-TEIKINKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-TEIKINKBN-LST (1)
                                       TO  SPA-GMN-TEIKINKBN-G
           END-IF
      *    薬剤情報提供発行
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-YKJCHKKBN-MAX
               IF      SPA-GMN-YKJCHKKBN
                                       =   SPA-GMN-YKJCHKKBNL (IDY)
                   MOVE    SPA-GMN-YKJCHKKBN-LST (IDY)
                                       TO  SPA-GMN-YKJCHKKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-YKJCHKKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-YKJCHKKBN-LST (1)
                                       TO  SPA-GMN-YKJCHKKBN-G
           END-IF
      *    薬剤情報提供発行
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-OKUSURIKBN-MAX
               IF      SPA-GMN-OKUSURIKBN
                                       =   SPA-GMN-OKUSURIKBN-LST(IDY)
                                                                 (1:1)
                   MOVE    SPA-GMN-OKUSURIKBN-LST (IDY)
                                           TO  SPA-GMN-OKUSURIKBN-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-GMN-OKUSURIKBN-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-OKUSURIKBN-LST (1)
                                       TO  SPA-GMN-OKUSURIKBN-G
           END-IF
      *    薬情・お薬手帳対象
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-YKJOKUKBN-MAX
               IF      SPA-GMN-YKJOKUKBN
                                       =   SPA-GMN-YKJOKUKBN-LST(IDY)
                                                                 (1:1)
                   MOVE    SPA-GMN-YKJOKUKBN-LST (IDY)
                                           TO  SPA-GMN-YKJOKUKBN-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-GMN-YKJOKUKBN-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-YKJOKUKBN-LST (1)
                                       TO  SPA-GMN-YKJOKUKBN-G
           END-IF
      *    ＤＯ初期表示
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-DOSRYKBNFLG-MAX
               IF      SPA-GMN-DOSRYKBNFLG
                                       =   SPA-GMN-DOSRYKBNFLG-LST (IDY)
                                                             (1:2)
                   MOVE    SPA-GMN-DOSRYKBNFLG-LST (IDY)
                                       TO  SPA-GMN-DOSRYKBNFLG-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-DOSRYKBNFLG-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-DOSRYKBNFLG-LST (1)
                                       TO  SPA-GMN-DOSRYKBNFLG-G
           END-IF
      *    中途表示一覧
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-WKSRYHYOFLG-MAX
               IF      SPA-GMN-WKSRYHYOFLG
                                       =   SPA-GMN-WKSRYHYOFLG-LST (IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-WKSRYHYOFLG-LST (IDY)
                                       TO  SPA-GMN-WKSRYHYOFLG-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-WKSRYHYOFLG-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-WKSRYHYOFLG-LST (1)
                                       TO  SPA-GMN-WKSRYHYOFLG-G
           END-IF
      *    検査正式名称表示
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-FORMALFLG-MAX
               IF      SPA-GMN-FORMALFLG
                                       =   SPA-GMN-FORMALFLG-LST (IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-FORMALFLG-LST (IDY)
                                       TO  SPA-GMN-FORMALFLG-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-FORMALFLG-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-FORMALFLG-LST (1)
                                       TO  SPA-GMN-FORMALFLG-G
           END-IF
      *    腫瘍マーカ−一覧表示
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-AKUSEIHYO-MAX
               IF      SPA-GMN-AKUSEIHYO
                                       =   SPA-GMN-AKUSEIHYO-LST (IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-AKUSEIHYO-LST (IDY)
                                       TO  SPA-GMN-AKUSEIHYO-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-AKUSEIHYO-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-AKUSEIHYO-LST (1)
                                       TO  SPA-GMN-AKUSEIHYO-G
           END-IF
      *    同日再診・訂正選択
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL  (IDY         >   SPA-GMN-TEISEIFLG-MAX )
                       OR (FLG-OK      =   1       )
               IF      SPA-GMN-TEISEIFLG
                                       =   SPA-GMN-TEISEIFLG-LST (IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-TEISEIFLG-LST (IDY)
                                       TO  SPA-GMN-TEISEIFLG-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-TEISEIFLG-LST (1)
                                       TO  SPA-GMN-TEISEIFLG-G
           END-IF
      *    皮下筋肉注射料変換選択
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL  (IDY         >   SPA-GMN-CHG310FLG-MAX )
                       OR (FLG-OK      =   1       )
               IF      SPA-GMN-CHG310FLG
                                       =   SPA-GMN-CHG310FLG-LST (IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-CHG310FLG-LST (IDY)
                                       TO  SPA-GMN-CHG310FLG-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-CHG310FLG-LST (1)
                                       TO  SPA-GMN-CHG310FLG-G
           END-IF
      *    投薬診療種別変換選択
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL  (IDY         >   SPA-GMN-CHGSRYSYU-MAX )
                       OR (FLG-OK      =   1       )
               IF      SPA-GMN-CHGSRYSYU
                                       =   SPA-GMN-CHGSRYSYU-LST (IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-CHGSRYSYU-LST (IDY)
                                       TO  SPA-GMN-CHGSRYSYU-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-CHGSRYSYU-LST (1)
                                       TO  SPA-GMN-CHGSRYSYU-G
           END-IF
      *
      *    レセプトブレビュー
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL  (IDY         >   SPA-GMN-RESPRV038-MAX )
                       OR (FLG-OK      =   1       )
               IF      SPA-GMN-RESPRV038
                                       =   SPA-GMN-RESPRV038-LST (IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-RESPRV038-LST (IDY)
                                       TO  SPA-GMN-RESPRV038-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-RESPRV038-LST (1)
                                       TO  SPA-GMN-RESPRV038-G
           END-IF
      *    残量廃棄区分
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL  (IDY         >   SPA-GMN-HAIKIKBN-MAX )
                       OR (FLG-OK      =   1       )
               IF      SPA-GMN-HAIKIKBN
                                       =   SPA-GMN-HAIKIKBN-LST (IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-HAIKIKBN-LST (IDY)
                                       TO  SPA-GMN-HAIKIKBN-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-HAIKIKBN-LST (1)
                                       TO  SPA-GMN-HAIKIKBN-G
           END-IF
      *    精神療法２０未満加算区分
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL  (IDY         >   SPA-GMN-SEI20KSNKBN-MAX )
                       OR (FLG-OK      =   1       )
               IF      SPA-GMN-SEI20KSNKBN
                                       =   SPA-GMN-SEI20KSNKBN-LST (IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-SEI20KSNKBN-LST (IDY)
                                       TO  SPA-GMN-SEI20KSNKBN-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SEI20KSNKBN-LST (1)
                                       TO  SPA-GMN-SEI20KSNKBN-G
           END-IF
      *    心身療法２０未満加算区分
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL  (IDY         >   SPA-GMN-SIN20KSNKBN-MAX )
                       OR (FLG-OK      =   1       )
               IF      SPA-GMN-SIN20KSNKBN
                                       =   SPA-GMN-SIN20KSNKBN-LST (IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-SIN20KSNKBN-LST (IDY)
                                       TO  SPA-GMN-SIN20KSNKBN-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SIN20KSNKBN-LST (1)
                                       TO  SPA-GMN-SIN20KSNKBN-G
           END-IF
      *    入金・返金額設定区分
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL  (IDY         >   SPA-GMN-NYKHNKKBN-MAX )
                       OR (FLG-OK      =   1       )
               IF      SPA-GMN-NYKHNKKBN
                                       =   SPA-GMN-NYKHNKKBN-LST (IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-NYKHNKKBN-LST (IDY)
                                       TO  SPA-GMN-NYKHNKKBN-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-NYKHNKKBN-LST (1)
                                       TO  SPA-GMN-NYKHNKKBN-G
           END-IF
      *    発行日変更区分
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL  (IDY         >   SPA-GMN-YMDHENKBN-MAX )
                       OR (FLG-OK      =   1       )
               IF      SPA-GMN-YMDHENKBN
                                       =   SPA-GMN-YMDHENKBN-LST (IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-YMDHENKBN-LST (IDY)
                                       TO  SPA-GMN-YMDHENKBN-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-YMDHENKBN-LST (1)
                                       TO  SPA-GMN-YMDHENKBN-G
           END-IF
      *    入力スクロール背景行
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL  (IDY         >   SPA-GMN-BGLINEKBN-MAX )
                       OR (FLG-OK      =   1       )
               IF      SPA-GMN-BGLINEKBN
                                       =   SPA-GMN-BGLINEKBN-LST (IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-BGLINEKBN-LST (IDY)
                                       TO  SPA-GMN-BGLINEKBN-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-BGLINEKBN-LST (1)
                                       TO  SPA-GMN-BGLINEKBN-G
           END-IF
      *H24.4
      *    確認画面表示区分
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL  (IDY         >   SPA-GMN-GENERICFLG-MAX )
                       OR (FLG-OK      =   1       )
               IF      SPA-GMN-GENERICFLG
                                       =   SPA-GMN-GENERICFLG-LST (IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-GENERICFLG-LST (IDY)
                                       TO  SPA-GMN-GENERICFLG-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-GENERICFLG-LST (1)
                                       TO  SPA-GMN-GENERICFLG-G
           END-IF
      *H24.12
      *    レセプトブレビューレイアウト
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL  (IDY         >   SPA-GMN-PRVLAYOUT038-MAX )
                       OR (FLG-OK      =   1       )
               IF      SPA-GMN-PRVLAYOUT038
                                       =   SPA-GMN-PRVLAYOUT038-LST
                                                       (IDY) (1:1)
                   MOVE    SPA-GMN-PRVLAYOUT038-LST (IDY)
                                       TO  SPA-GMN-PRVLAYOUT038-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-PRVLAYOUT038-LST (1)
                                       TO  SPA-GMN-PRVLAYOUT038-G
           END-IF
      *H28.6
      *    セット展開表示
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL  (IDY         >   SPA-GMN-SETALLKBN-MAX )
                       OR (FLG-OK      =   1       )
               IF      SPA-GMN-SETALLKBN
                                       =   SPA-GMN-SETALLKBN-LST
                                                       (IDY) (1:1)
                   MOVE    SPA-GMN-SETALLKBN-LST (IDY)
                                       TO  SPA-GMN-SETALLKBN-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SETALLKBN-LST (1)
                                       TO  SPA-GMN-SETALLKBN-G
           END-IF
      *
      *H30.9
      *    選択式コメント一覧区分
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-RECEKISAIFLG-MAX
               IF      SPA-GMN-RECEKISAIFLG
                                       =   SPA-GMN-RECEKISAIFLG-LST
                                                        (IDY)(1:1)
                   MOVE    SPA-GMN-RECEKISAIFLG-LST (IDY)
                                       TO  SPA-GMN-RECEKISAIFLG-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-RECEKISAIFLG-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
      *        自動表示するを初期表示
               MOVE    SPA-GMN-RECEKISAIFLG-LST (2)
                                       TO  SPA-GMN-RECEKISAIFLG-G
           END-IF
      *
           MOVE    301                  TO  SPA-GMN-CUR
      *
           . 
       310-SPASET-1038-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-1039-SEC         SECTION.
      *
      *    収納機能情報
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1039"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1039-REC
           ELSE
               INITIALIZE  SYS-1039-REC
           END-IF
      *
      *    入金管理区分
           MOVE    SYS-1039-NYKNKNRKBN TO  SPA-GMN-SYUNYKNKNRKBN
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SYUNYKNKNRKBN-MAX
               IF      SPA-GMN-SYUNYKNKNRKBN
                                       =   SPA-GMN-SYUNYKNKNRKBNL (IDY)
                   MOVE    SPA-GMN-SYUNYKNKNRKBNLST(IDY)
                                       TO  SPA-GMN-SYUNYKNKNRKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-SYUNYKNKNRKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SYUNYKNKNRKBNLST (1)
                                       TO  SPA-GMN-SYUNYKNKNRKBN-G
           END-IF
      *
      *    診療年月指定区分
           MOVE    SYS-1039-IKTSRYYMKBN    TO  SPA-GMN-IKTSRYYMKBN
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-IKTSRYYMKBN-MAX
               IF      SPA-GMN-IKTSRYYMKBN
                                       =   SPA-GMN-IKTSRYYMKBNL (IDY)
                   MOVE    SPA-GMN-IKTSRYYMKBNLST(IDY)
                                       TO  SPA-GMN-IKTSRYYMKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-IKTSRYYMKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-IKTSRYYMKBNLST (1)
                                       TO  SPA-GMN-IKTSRYYMKBN-G
           END-IF
      *
      *    請求書兼領収書発行区分（外来）
           MOVE    SYS-1039-SKYPRTGKBN TO  SPA-GMN-SYUSKYPRTG
           PERFORM 3101-SYUSKYPRTG-SYOKI-SEC
      *
      *    請求書兼領収書前回請求額印字区分（外来）
           MOVE    SYS-1039-S03SKYMISYUPRTKBN
                                       TO  SPA-GMN-S03SKYMISYUPRT
           PERFORM 3101-S03SKYMISYUPRT-SYOKI-SEC
      *
      *    診療費明細書発行区分（外来）
           MOVE    SYS-1039-SRYPRTGKBN TO  SPA-GMN-SYUSRYPRTG
           PERFORM 3101-SYUSRYPRTG-SYOKI-SEC
      *
      *    請求書兼領収書発行区分（入院）
           MOVE    SYS-1039-SKYPRTNKBN TO  SPA-GMN-SYUSKYPRTN
           PERFORM 3101-SYUSKYPRTN-SYOKI-SEC
      *
      *    診療費明細書発行区分（入院）
           MOVE    SYS-1039-SRYPRTNKBN TO  SPA-GMN-SYUSRYPRTN
           PERFORM 3101-SYUSRYPRTN-SYOKI-SEC
      *
      *    収納一括入金画面請求書兼領収書発行区分
           MOVE    SYS-1039-S06SKYPRTGKBN
                                       TO  SPA-GMN-S06SKYPRT
           PERFORM 3101-S06SKYPRT-SYOKI-SEC
      *
      *    収納一括入金画面外来分発行方法
           MOVE    SYS-1039-S06SKYSUMKBN
                                       TO  SPA-GMN-S06SKYSUM
           PERFORM 3101-S06SKYSUM-SYOKI-SEC
      *
      *    収納請求一覧画面外来請求書兼領収書発行方法
           MOVE    SYS-1039-S02SKYSAIKBN
                                       TO  SPA-GMN-S02SKYSAI
           PERFORM 3101-S02SKYSAI-SYOKI-SEC
      *
      *    請求書兼領収書前回請求額印字区分（外来）
           MOVE    SYS-1039-S02SKYMISYUPRTKBN
                                       TO  SPA-GMN-S02SKYMISYUPRT
           PERFORM 3101-S02SKYMISYUPRT-SYOKI-SEC
      *
      *    月別請求書兼領収書発行方法
           MOVE    SYS-1039-TUKIGRPKBN
                                       TO  SPA-GMN-TUKIGRP
           PERFORM 3101-TUKIGRP-SYOKI-SEC
      *
      *    月別請求書兼領収書前回請求額印字区分
           MOVE    SYS-1039-TUKISKYMISYUPRTKBN
                                       TO  SPA-GMN-TUKISKYMISYUPRT
           PERFORM 3101-TUKISKYMISYUPRT-SYOKI-SEC
      *
      *    月別請求書兼領収書集計区分
           MOVE    SYS-1039-TUKISUMKBN
                                       TO  SPA-GMN-TUKISUM
           PERFORM 3101-TUKISUM-SYOKI-SEC
      *
      *    請求一覧画面コラムリスト
           MOVE    SYS-1039-S02MODE    TO  SPA-GMN-S02MODE
           PERFORM 3101-S02MODE-SYOKI-SEC
      *
      *    IF    ( SYS-1039-S02LIMIT   IS  NOT NUMERIC )
               MOVE    ZERO            TO  SPA-GMN-S02LIMIT
      *    ELSE
      *        MOVE    SYS-1039-S02LIMIT   TO  SPA-GMN-S02LIMIT
      *    END-IF
      *
      *    請求一覧画面並び順
           PERFORM 3101-S02SORTKBNLST-SEC
           EVALUATE    SYS-1039-S02SORTKBN
           WHEN    "0"
               MOVE    "0"             TO  SPA-GMN-S02SORTKBN
           WHEN    "1"
               MOVE    "2"             TO  SPA-GMN-S02SORTKBN
           WHEN    "2"
               MOVE    "1"             TO  SPA-GMN-S02SORTKBN
           WHEN    "3"
               MOVE    "3"             TO  SPA-GMN-S02SORTKBN
           END-EVALUATE
           PERFORM 3101-S02SORTKBN-SYOKI-SEC
      *
      *    再計算該当者チェック
           MOVE    SYS-1039-SAIKEISANSKYCHK
                                       TO  SPA-GMN-SAIKEISANSKYCHK
           PERFORM 3101-SAIKEISANSKYCHK-SYOKI-SEC
      *
      *    一括計算順
           MOVE    SYS-1039-IKTSORTKBN
                                       TO  SPA-GMN-IKTSORTKBN
           PERFORM 3101-IKTSORTKBN-SYOKI-SEC
      *
      *    一括計算方法
           MOVE    SYS-1039-IKTKEISANKBN
                                       TO  SPA-GMN-IKTKEISANKBN
           PERFORM 3101-IKTKEISANKBN-SYOKI-SEC
      *
      *    請求書未収金集計期間区分
           MOVE    SYS-1039-MISYUKIKANKBN
                                       TO  SPA-GMN-MISYUKIKANKBN
           PERFORM 3101-MISYUKIKANKBN-SYOKI-SEC
      *
           IF    ( SYS-1039-STIME          IS  NUMERIC )
               MOVE    SYS-1039-STIME      TO  SPA-GMN-SYUSTIME
           ELSE
               MOVE    ZERO                TO  SPA-GMN-SYUSTIME
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   2 )
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   7 )
                   IF    ( SYS-1039-KARIDAY (IDX1 IDX2)    =   "1" )
                       MOVE    "T"         TO  SPA-GMN-SYUKARIDAY
                                                        (IDX1 IDX2)
                   ELSE
                       MOVE    "F"         TO  SPA-GMN-SYUKARIDAY
                                                        (IDX1 IDX2)
                   END-IF
               END-PERFORM
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   2 )
      *
                   IF    ( SYS-1039-KARISTTIME (IDX1 IDX2) IS  NUMERIC )
                       MOVE    SYS-1039-KARISTTIME (IDX1 IDX2)
                                           TO  SPA-GMN-SYUKARISTTIME
                                                             (IDX1 IDX2)
                   ELSE
                       MOVE    ZERO        TO  SPA-GMN-SYUKARISTTIME
                                                             (IDX1 IDX2)
                   END-IF
      *
                   IF    ( SYS-1039-KARIEDTIME (IDX1 IDX2) IS  NUMERIC )
                       MOVE    SYS-1039-KARIEDTIME (IDX1 IDX2)
                                           TO  SPA-GMN-SYUKARIEDTIME
                                                             (IDX1 IDX2)
                   ELSE
                       MOVE    ZERO        TO  SPA-GMN-SYUKARIEDTIME
                                                             (IDX1 IDX2)
                   END-IF
      *
               END-PERFORM
           END-PERFORM
      *
      *    支払証明書印刷画面支払証明書発行区分
           MOVE    SYS-1039-S04SHIHARAIKBN
                                       TO  SPA-GMN-S04SHIHARAIKBN
           PERFORM 4109-S04SHIHARAIKBN-CHK-SEC
      *    支払証明書印刷画面入外区分
           MOVE    SYS-1039-S04NYUGAIKBN
                                       TO  SPA-GMN-S04NYUGAIKBN
           PERFORM 4109-S04NYUGAIKBN-CHK-SEC
      *    支払証明書印刷画面期間指定
           MOVE    SYS-1039-S04SRYKBN  TO  SPA-GMN-S04SRYKBN
           PERFORM 4109-S04SRYKBN-CHK-SEC
      *    支払証明書印刷画面期間区分
           MOVE    SYS-1039-S04KIKANKBN TO SPA-GMN-S04KIKANKBN
           PERFORM 4109-S04KIKANKBN-CHK-SEC
      *    支払証明書印刷画面請求内容の印字
           MOVE    SYS-1039-S04DSPKBN  TO  SPA-GMN-S04DSPKBN
           PERFORM 4109-S04DSPKBN-CHK-SEC
      *    支払証明書印刷画面期間開始前診療分の支払い
           MOVE    SYS-1039-S04SUMKBN TO SPA-GMN-S04SUMKBN
           PERFORM 4109-S04SUMKBN-CHK-SEC
      *    支払証明書印刷画面期間終了後の支払い
           MOVE    SYS-1039-S04SYORIKBN TO SPA-GMN-S04SYORIKBN
           PERFORM 4109-S04SYORIKBN-CHK-SEC
      *    支払証明書印刷自費内訳明細書
           MOVE    SYS-1039-S04JIHIPRTKBN TO SPA-GMN-S04JIHIPRTKBN
           PERFORM 4109-S04JIHIPRTKBN-CHK-SEC
      *
           MOVE    49                 TO  SPA-GMN-CUR
      *
           . 
       310-SPASET-1039-EXT.
           EXIT.
      *****************************************************************
      *    並び順ＳＰＡ画面編集処理
      *****************************************************************
       3101-S02SORTKBNLST-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-S02SORTKBNLST-G
      *
           IF    ( SPA-GMN-S02MODE     =   "3" OR "4" )
               MOVE    "0"             TO  SPA-GMN-S02SORTKBNL       (1)
               MOVE    "発行日の古い順"
                                       TO  SPA-GMN-S02SORTKBNMEIL    (1)
               MOVE    "1"             TO  SPA-GMN-S02SORTKBNL       (2)
               MOVE    "発行日の新しい順"
                                       TO  SPA-GMN-S02SORTKBNMEIL    (2)
           ELSE
               MOVE    "0"             TO  SPA-GMN-S02SORTKBNL       (1)
               MOVE    "伝票番号の小さい順"
                                       TO  SPA-GMN-S02SORTKBNMEIL    (1)
               MOVE    "1"             TO  SPA-GMN-S02SORTKBNL       (2)
               MOVE    "伝票番号の大きい順"
                                       TO  SPA-GMN-S02SORTKBNMEIL    (2)
           END-IF
           MOVE    "2"                 TO  SPA-GMN-S02SORTKBNL       (3)
           MOVE    "診療日の古い順"    TO  SPA-GMN-S02SORTKBNMEIL    (3)
           MOVE    "3"                 TO  SPA-GMN-S02SORTKBNL       (4)
           MOVE    "診療日の新しい順"  TO  SPA-GMN-S02SORTKBNMEIL    (4)
           MOVE    4                   TO  SPA-GMN-S02SORTKBN-MAX
      *
           .
       3101-S02SORTKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    収納請求書兼領収書外来初期値設定処理
      *****************************************************************
       3101-SYUSKYPRTG-SYOKI-SEC       SECTION.
      *
           IF    ( SPA-GMN-SYUSKYPRTG  =   SPACE )
               INITIALIZE                      SYS-1001-REC
               MOVE    "1001"              TO  SYS-1001-KANRICD
               MOVE    "*"                 TO  SYS-1001-KBNCD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-1001-STYUKYMD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-1001-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
               MOVE    SYS-1001-REC        TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-INV-SEC
               IF    ( FLG-SYSKANRI        =   ZERO )
                   MOVE    SYSKANRI-REC       TO  SYS-1001-REC
                   IF    ( SYS-1001-SKYPRTFLG  =   "1" OR  "2" )
                       MOVE    "1"         TO  SPA-GMN-SYUSKYPRTG
                   ELSE
                       MOVE    "0"         TO  SPA-GMN-SYUSKYPRTG
                   END-IF
               ELSE
                       MOVE    "0"         TO  SPA-GMN-SYUSKYPRTG
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SYUSKYPRTG-MAX
               IF      SPA-GMN-SYUSKYPRTG
                                       =   SPA-GMN-SYUSKYPRTGL (IDY)
                   MOVE    SPA-GMN-SYUSKYPRTGLST(IDY)
                                       TO  SPA-GMN-SYUSKYPRTG-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-SYUSKYPRTG-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SYUSKYPRTGLST (1)
                                       TO  SPA-GMN-SYUSKYPRTG-G
           END-IF
      *
           . 
       3101-SYUSKYPRTG-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    収納請求書兼領収書前回請求額印字区分初期値設定処理
      *****************************************************************
       3101-S03SKYMISYUPRT-SYOKI-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-S03SKYMISYUPRT-MAX
               IF      SPA-GMN-S03SKYMISYUPRT
                                       =   SPA-GMN-S03SKYMISYUPRTL (IDY)
                   MOVE    SPA-GMN-S03SKYMISYUPRTLST(IDY)
                                       TO  SPA-GMN-S03SKYMISYUPRT-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-S03SKYMISYUPRT-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-S03SKYMISYUPRTLST (2)
                                       TO  SPA-GMN-S03SKYMISYUPRT-G
           END-IF
      *
           . 
       3101-S03SKYMISYUPRT-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    収納診療費明細書外来初期値設定処理
      *****************************************************************
       3101-SYUSRYPRTG-SYOKI-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SYUSRYPRTG-MAX
               IF      SPA-GMN-SYUSRYPRTG
                                       =   SPA-GMN-SYUSRYPRTGL (IDY)
                   MOVE    SPA-GMN-SYUSRYPRTGLST(IDY)
                                       TO  SPA-GMN-SYUSRYPRTG-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-SYUSRYPRTG-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SYUSRYPRTGLST (1)
                                       TO  SPA-GMN-SYUSRYPRTG-G
           END-IF
      *
           . 
       3101-SYUSRYPRTG-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    収納請求書兼領収書入院初期値設定処理
      *****************************************************************
       3101-SYUSKYPRTN-SYOKI-SEC       SECTION.
      *
           IF    ( SPA-GMN-SYUSKYPRTN  =   SPACE )
               INITIALIZE                      SYS-5000-REC
               MOVE    "5000"              TO  SYS-5000-KANRICD
               MOVE    "*"                 TO  SYS-5000-KBNCD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-5000-STYUKYMD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-5000-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-5000-HOSPNUM
               MOVE    SYS-5000-REC        TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-INV-SEC
               IF    ( FLG-SYSKANRI        =   ZERO )
                   MOVE    SYSKANRI-REC        TO  SYS-5000-REC
                   IF    ( SYS-5000-SKYPRTKBN  =   "1" OR  "2" )
                       MOVE    "1"         TO  SPA-GMN-SYUSKYPRTN
                   ELSE
                       MOVE    "0"         TO  SPA-GMN-SYUSKYPRTN
                   END-IF
               ELSE
                       MOVE    "0"         TO  SPA-GMN-SYUSKYPRTN
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SYUSKYPRTN-MAX
               IF      SPA-GMN-SYUSKYPRTN
                                       =   SPA-GMN-SYUSKYPRTNL (IDY)
                   MOVE    SPA-GMN-SYUSKYPRTNLST(IDY)
                                       TO  SPA-GMN-SYUSKYPRTN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-SYUSKYPRTN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SYUSKYPRTNLST (1)
                                       TO  SPA-GMN-SYUSKYPRTN-G
           END-IF
      *
           . 
       3101-SYUSKYPRTN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    収納診療費明細書入院初期値設定処理
      *****************************************************************
       3101-SYUSRYPRTN-SYOKI-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SYUSRYPRTN-MAX
               IF      SPA-GMN-SYUSRYPRTN
                                       =   SPA-GMN-SYUSRYPRTNL (IDY)
                   MOVE    SPA-GMN-SYUSRYPRTNLST(IDY)
                                       TO  SPA-GMN-SYUSRYPRTN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-SYUSRYPRTN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SYUSRYPRTNLST (1)
                                       TO  SPA-GMN-SYUSRYPRTN-G
           END-IF
      *
           . 
       3101-SYUSRYPRTN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    収納一括入金画面請求書兼領収書発行区分初期値設定処理
      *****************************************************************
       3101-S06SKYPRT-SYOKI-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-S06SKYPRT-MAX
               IF      SPA-GMN-S06SKYPRT
                                       =   SPA-GMN-S06SKYPRTL (IDY)
                   MOVE    SPA-GMN-S06SKYPRTLST(IDY)
                                       TO  SPA-GMN-S06SKYPRT-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-S06SKYPRT-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-S06SKYPRTLST (1)
                                       TO  SPA-GMN-S06SKYPRT-G
           END-IF
      *
           . 
       3101-S06SKYPRT-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    収納一括入金画面外来分発行方法初期値設定処理
      *****************************************************************
       3101-S06SKYSUM-SYOKI-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-S06SKYSUM-MAX
               IF      SPA-GMN-S06SKYSUM
                                       =   SPA-GMN-S06SKYSUML (IDY)
                   MOVE    SPA-GMN-S06SKYSUMLST(IDY)
                                       TO  SPA-GMN-S06SKYSUM-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-S06SKYSUM-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-S06SKYSUMLST (1)
                                       TO  SPA-GMN-S06SKYSUM-G
           END-IF
      *
           . 
       3101-S06SKYSUM-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    収納一括入金画面外来分発行方法初期値設定処理
      *****************************************************************
       3101-S02SKYSAI-SYOKI-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-S02SKYSAI-MAX
               IF      SPA-GMN-S02SKYSAI
                                       =   SPA-GMN-S02SKYSAIL (IDY)
                   MOVE    SPA-GMN-S02SKYSAILST(IDY)
                                       TO  SPA-GMN-S02SKYSAI-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-S02SKYSAI-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-S02SKYSAILST (1)
                                       TO  SPA-GMN-S02SKYSAI-G
           END-IF
      *
           . 
       3101-S02SKYSAI-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書前回請求額印字区分（外来）初期値設定処理
      *****************************************************************
       3101-S02SKYMISYUPRT-SYOKI-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-S02SKYMISYUPRT-MAX
               IF      SPA-GMN-S02SKYMISYUPRT
                                       =   SPA-GMN-S02SKYMISYUPRTL (IDY)
                   MOVE    SPA-GMN-S02SKYMISYUPRTLST(IDY)
                                       TO  SPA-GMN-S02SKYMISYUPRT-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-S02SKYMISYUPRT-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-S02SKYMISYUPRTLST (2)
                                       TO  SPA-GMN-S02SKYMISYUPRT-G
           END-IF
      *
           . 
       3101-S02SKYMISYUPRT-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    月別請求書兼領収書前回請求額印字区分初期値設定処理
      *****************************************************************
       3101-TUKISKYMISYUPRT-SYOKI-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-TUKISKYMISYUPRT-MAX
               IF      SPA-GMN-TUKISKYMISYUPRT
                                       =   SPA-GMN-TUKISKYMISYUPRTL(IDY)
                   MOVE    SPA-GMN-TUKISKYMISYUPRTLST(IDY)
                                       TO  SPA-GMN-TUKISKYMISYUPRT-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-TUKISKYMISYUPRT-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-TUKISKYMISYUPRTLST (2)
                                       TO  SPA-GMN-TUKISKYMISYUPRT-G
           END-IF
      *
           . 
       3101-TUKISKYMISYUPRT-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    月別請求書兼領収書発行方法設定処理
      *****************************************************************
       3101-TUKIGRP-SYOKI-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-TUKIGRP-MAX
               IF      SPA-GMN-TUKIGRP
                                       =   SPA-GMN-TUKIGRPL(IDY)
                   MOVE    SPA-GMN-TUKIGRPLST(IDY)
                                       TO  SPA-GMN-TUKIGRP-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-TUKIGRP-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-TUKIGRPLST (4)
                                       TO  SPA-GMN-TUKIGRP-G
           END-IF
      *
           . 
       3101-TUKIGRP-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    月別請求書兼領収書集計区分設定処理
      *****************************************************************
       3101-TUKISUM-SYOKI-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-TUKISUM-MAX
               IF      SPA-GMN-TUKISUM
                                       =   SPA-GMN-TUKISUML(IDY)
                   MOVE    SPA-GMN-TUKISUMLST(IDY)
                                       TO  SPA-GMN-TUKISUM-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-TUKISUM-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-TUKISUMLST (1)
                                       TO  SPA-GMN-TUKISUM-G
           END-IF
      *
           . 
       3101-TUKISUM-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    請求一覧コラムリスト初期値設定処理
      *****************************************************************
       3101-S02MODE-SYOKI-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-S02MODE-MAX
               IF      SPA-GMN-S02MODE
                                       =   SPA-GMN-S02MODEL(IDY)
                   MOVE    SPA-GMN-S02MODELST(IDY)
                                       TO  SPA-GMN-S02MODE-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-S02MODE-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-S02MODELST (1)
                                       TO  SPA-GMN-S02MODE-G
           END-IF
      *
           . 
       3101-S02MODE-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    請求一覧画面並び順初期値設定処理
      *****************************************************************
       3101-S02SORTKBN-SYOKI-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-S02SORTKBN-MAX
               IF      SPA-GMN-S02SORTKBN
                                       =   SPA-GMN-S02SORTKBNL(IDY)
                   MOVE    SPA-GMN-S02SORTKBNLST(IDY)
                                       TO  SPA-GMN-S02SORTKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-S02SORTKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-S02SORTKBNLST (1)
                                       TO  SPA-GMN-S02SORTKBN-G
           END-IF
      *
           . 
       3101-S02SORTKBN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    再計算対象者チェック初期値設定処理
      *****************************************************************
       3101-SAIKEISANSKYCHK-SYOKI-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SAIKEISANSKYCHK-MAX
               IF      SPA-GMN-SAIKEISANSKYCHK
                                       =   SPA-GMN-SAIKEISANSKYCHKL(IDY)
                   MOVE    SPA-GMN-SAIKEISANSKYCHKLST(IDY)
                                       TO  SPA-GMN-SAIKEISANSKYCHK-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-SAIKEISANSKYCHK-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SAIKEISANSKYCHKLST (2)
                                       TO  SPA-GMN-SAIKEISANSKYCHK-G
           END-IF
      *
           . 
       3101-SAIKEISANSKYCHK-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    一括計算順初期値設定処理
      *****************************************************************
       3101-IKTSORTKBN-SYOKI-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-IKTSORTKBN-MAX
               IF      SPA-GMN-IKTSORTKBN
                                       =   SPA-GMN-IKTSORTKBNL(IDY)
                   MOVE    SPA-GMN-IKTSORTKBNLST(IDY)
                                       TO  SPA-GMN-IKTSORTKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-IKTSORTKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-IKTSORTKBNLST (1)
                                       TO  SPA-GMN-IKTSORTKBN-G
           END-IF
      *
           . 
       3101-IKTSORTKBN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    一括計算方法区分初期値設定処理
      *****************************************************************
       3101-IKTKEISANKBN-SYOKI-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-IKTKEISANKBN-MAX
               IF      SPA-GMN-IKTKEISANKBN
                                       =   SPA-GMN-IKTKEISANKBNL(IDY)
                   MOVE    SPA-GMN-IKTKEISANKBNLST(IDY)
                                       TO  SPA-GMN-IKTKEISANKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-IKTKEISANKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-IKTKEISANKBNLST (1)
                                       TO  SPA-GMN-IKTKEISANKBN-G
           END-IF
      *
           . 
       3101-IKTKEISANKBN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    請求書未収金集計期間区分
      *****************************************************************
       3101-MISYUKIKANKBN-SYOKI-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-MISYUKIKANKBN-MAX
               IF      SPA-GMN-MISYUKIKANKBN
                                       =   SPA-GMN-MISYUKIKANKBNL(IDY)
                   MOVE    SPA-GMN-MISYUKIKANKBNLST(IDY)
                                       TO  SPA-GMN-MISYUKIKANKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-MISYUKIKANKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-MISYUKIKANKBNLST (1)
                                       TO  SPA-GMN-MISYUKIKANKBN-G
           END-IF
      *
           . 
       3101-MISYUKIKANKBN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-1042-SEC         SECTION.
      *
      *    病名機能情報
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1042"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1042-REC
           ELSE
               INITIALIZE  SYS-1042-REC
           END-IF
      *
      *    主病名確認メッセージ表示有無
           MOVE    SYS-1042-MSG1-DSPKBN
                                       TO  SPA-GMN-MSG1-DSPKBN
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-MSG1-DSPKBN-MAX
               IF      SPA-GMN-MSG1-DSPKBN
                                       =   SPA-GMN-MSG1-DSPKBNL (IDY)
                   MOVE    SPA-GMN-MSG1-DSPKBNLST(IDY)
                                       TO  SPA-GMN-MSG1-DSPKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-MSG1-DSPKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-MSG1-DSPKBNLST (1)
                                       TO  SPA-GMN-MSG1-DSPKBN-G
           END-IF
      *
      *    移行病名警告メッセージ表示有無
           MOVE    SYS-1042-MSG2-DSPKBN
                                       TO  SPA-GMN-MSG2-DSPKBN
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-MSG2-DSPKBN-MAX
               IF      SPA-GMN-MSG2-DSPKBN
                                       =   SPA-GMN-MSG2-DSPKBNL (IDY)
                   MOVE    SPA-GMN-MSG2-DSPKBNLST(IDY)
                                       TO  SPA-GMN-MSG2-DSPKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-MSG2-DSPKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-MSG2-DSPKBNLST (1)
                                       TO  SPA-GMN-MSG2-DSPKBN-G
           END-IF
      *
      *    廃止年月日警告メッセージ表示有無
           MOVE    SYS-1042-MSG3-DSPKBN
                                       TO  SPA-GMN-MSG3-DSPKBN
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-MSG3-DSPKBN-MAX
               IF      SPA-GMN-MSG3-DSPKBN
                                       =   SPA-GMN-MSG3-DSPKBNL (IDY)
                   MOVE    SPA-GMN-MSG3-DSPKBNLST(IDY)
                                       TO  SPA-GMN-MSG3-DSPKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-MSG3-DSPKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-MSG3-DSPKBNLST (1)
                                       TO  SPA-GMN-MSG3-DSPKBN-G
           END-IF
      *
      *    同一病名警告メッセージ表示有無
           MOVE    SYS-1042-MSG4-DSPKBN
                                       TO  SPA-GMN-MSG4-DSPKBN
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-MSG4-DSPKBN-MAX
               IF      SPA-GMN-MSG4-DSPKBN
                                       =   SPA-GMN-MSG4-DSPKBNL (IDY)
                   MOVE    SPA-GMN-MSG4-DSPKBNLST(IDY)
                                       TO  SPA-GMN-MSG4-DSPKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-MSG4-DSPKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-MSG4-DSPKBNLST (1)
                                       TO  SPA-GMN-MSG4-DSPKBN-G
           END-IF
      *
      *    死亡区分更新確認メッセージ表示有無
           MOVE    SYS-1042-MSG5-DSPKBN
                                       TO  SPA-GMN-MSG5-DSPKBN
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-MSG5-DSPKBN-MAX
               IF      SPA-GMN-MSG5-DSPKBN
                                       =   SPA-GMN-MSG5-DSPKBNL (IDY)
                   MOVE    SPA-GMN-MSG5-DSPKBNLST(IDY)
                                       TO  SPA-GMN-MSG5-DSPKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-MSG5-DSPKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-MSG5-DSPKBNLST (1)
                                       TO  SPA-GMN-MSG5-DSPKBN-G
           END-IF
      *
      *    診療科初期表示区分
           MOVE    SYS-1042-SRYKA-DSPKBN
                                       TO  SPA-GMN-SRYKA-DSPKBN
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SRYKA-DSPKBN-MAX
               IF      SPA-GMN-SRYKA-DSPKBN
                                       =   SPA-GMN-SRYKA-DSPKBNL (IDY)
                   MOVE    SPA-GMN-SRYKA-DSPKBNLST(IDY)
                                       TO  SPA-GMN-SRYKA-DSPKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-SRYKA-DSPKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SRYKA-DSPKBNLST (1)
                                           TO  SPA-GMN-SRYKA-DSPKBN-G
           END-IF
      *
      *    表示対象期間初期表示区分
           MOVE    SYS-1042-SYOKBN-DSPKBN
                                       TO  SPA-GMN-SYOKBN-DSPKBN
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SYOKBN-DSPKBN-MAX
               IF      SPA-GMN-SYOKBN-DSPKBN
                                       =   SPA-GMN-SYOKBN-DSPKBNL (IDY)
                   MOVE    SPA-GMN-SYOKBN-DSPKBNLST(IDY)
                                       TO  SPA-GMN-SYOKBN-DSPKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-SYOKBN-DSPKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SYOKBN-DSPKBNLST (1)
                                       TO  SPA-GMN-SYOKBN-DSPKBN-G
           END-IF
      *
      *    疑いフラグ設定区分
           IF      SYS-1042-UTAGAIFLG-KBN
                                       =   SPACE
               MOVE    "0"                 TO  SYS-1042-UTAGAIFLG-KBN
           END-IF
           MOVE    SYS-1042-UTAGAIFLG-KBN
                                       TO  SPA-GMN-UTAGAIFLG-KBN
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-UTAGAIFLG-KBN-MAX
               IF      SPA-GMN-UTAGAIFLG-KBN
                                       =   SPA-GMN-UTAGAIFLG-KBNL (IDY)
                   MOVE    SPA-GMN-UTAGAIFLG-KBNLST(IDY)
                                       TO  SPA-GMN-UTAGAIFLG-KBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-UTAGAIFLG-KBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-UTAGAIFLG-KBNLST (1)
                                       TO  SPA-GMN-UTAGAIFLG-KBN-G
           END-IF
      *
      *    コメント症状詳記区分
           MOVE    SYS-1042-SJKBN-SET  TO  SPA-GMN-SJKBN
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SJKBN-MAX
               IF      SPA-GMN-SJKBN   =   SPA-GMN-SJKBNLST (IDY)(1:2)
                   MOVE    SPA-GMN-SJKBNLST(IDY)
                                       TO  SPA-GMN-SJKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-SJKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SJKBNLST (1)
                                       TO  SPA-GMN-SJKBN-G
           END-IF
      *
           MOVE    500                 TO  SPA-GMN-CUR
      *
           . 
       310-SPASET-1042-EXT.
           EXIT.
      *****************************************************************
      *    会計照会スパ初期編集処理
      *****************************************************************
       310-SPASET-1043-SEC         SECTION.
      *
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1043"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1043-REC
           ELSE
               INITIALIZE  SYS-1043-REC
           END-IF
      *
      *    全科の表示順区分
           MOVE    SYS-1043-SRYKBN-FLG
                                       TO  SPA-GMN-SRYKBN-FLG
      *    診療科の初期表示
           MOVE    SYS-1043-SRYKA-FLG
                                       TO  SPA-GMN-SRYKA-FLG
      *    レセプトブレビュー
           MOVE    SYS-1043-RESPRVFLG
                                       TO  SPA-GMN-RESPRV043
      *H24.12
      *    レセプトブレビューレイアウト
           MOVE    SYS-1043-PRVLAYOUT  TO  SPA-GMN-PRVLAYOUT043
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SRYKBN-MAX
               IF      SPA-GMN-SRYKBN-FLG
                                       =   SPA-GMN-SRYKBN-LIST(IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-SRYKBN-LIST(IDY)
                                           TO  SPA-GMN-SRYKBN-FLG-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-GMN-SRYKBN-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SRYKBN-LIST (1)
                                       TO  SPA-GMN-SRYKBN-FLG-G
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SRYKA-MAX
               IF      SPA-GMN-SRYKA-FLG
                                       =   SPA-GMN-SRYKA-LIST(IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-SRYKA-LIST(IDY)
                                           TO  SPA-GMN-SRYKA-FLG-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-GMN-SRYKA-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SRYKA-LIST (1)
                                       TO  SPA-GMN-SRYKA-FLG-G
           END-IF
      *    レセプトブレビュー
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL  (IDY         >   SPA-GMN-RESPRV043-MAX )
                       OR (FLG-OK      =   1       )
               IF      SPA-GMN-RESPRV043
                                       =   SPA-GMN-RESPRV043-LIST (IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-RESPRV043-LIST (IDY)
                                       TO  SPA-GMN-RESPRV043-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-RESPRV043-LIST (1)
                                       TO  SPA-GMN-RESPRV043-G
           END-IF
      *H24.12
      *    レセプトブレビューレイアウト
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL  (IDY         >   SPA-GMN-PRVLAYOUT043-MAX )
                       OR (FLG-OK      =   1       )
               IF      SPA-GMN-PRVLAYOUT043
                                       =   SPA-GMN-PRVLAYOUT043-LST
                                                       (IDY) (1:1)
                   MOVE    SPA-GMN-PRVLAYOUT043-LST (IDY)
                                       TO  SPA-GMN-PRVLAYOUT043-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-PRVLAYOUT043-LST (1)
                                       TO  SPA-GMN-PRVLAYOUT043-G
           END-IF
      *
      *
           MOVE    600                 TO  SPA-GMN-CUR
      *
           . 
       310-SPASET-1043-EXT.
           EXIT.
      *****************************************************************
      *    予約機能スパ初期編集処理
      *****************************************************************
       310-SPASET-1045-SEC         SECTION.
      *
      *R01.12
      *    ドクター編集
      *    INITIALIZE                  ORCSDRSETAREA
      *    MOVE    SPA-W01-EDYUKYMD    TO  ORCSDR-SYSYMD
      *    MOVE    "1"                 TO  ORCSDR-KBN
      *    MOVE    SPACE               TO  ORCSDR-SRYKA
      *    MOVE    SPACE               TO  ORCSDR-IN-DRCD
      *    CALL   "ORCSDRSET"          USING
      *                                SPA-AREA
      *                                ORCSDRSETAREA
      *    MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-INIT-DRCD-COMB
      *    MOVE    ORCSDR-DRCD-MAX     TO  SPA-INIT-DRCD-MAX
      *R01.12
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    "1"                 TO  SYS-1010-KBNCD (1:1)
           MOVE    "%"                 TO  SYS-1010-KBNCD (2:1)
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           INITIALIZE                  SPA-GMN-INIT-DRCD-COMB
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  SPA-INIT-DRCD-MAX
           PERFORM UNTIL       (IDX             >=  99 )  OR
                               (FLG-SYSKANRI    =   1  )
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               MOVE    ZERO                TO  FLG-NO
      *        表示分のみ
               IF     (SYS-1010-HYOJIFLG       =   "1"  )
                   MOVE    1                   TO  FLG-NO
               END-IF
      *        有効期間外
               IF    ((SYS-1010-STYUKYMD       >   SPA-W01-EDYUKYMD)
                  OR  (SYS-1010-EDYUKYMD       <   SPA-W01-STYUKYMD))
                   MOVE    1                   TO  FLG-NO
               END-IF
               IF      FLG-NO              =   ZERO
                   IF     (IDX                 >   ZERO )
                     AND  (SYS-1010-KBNCD(2:4)
                                           =   SPA-GMN-INIT-DRCD-LIST
                                                            (IDX)(1:4))
      *                同一ドクターは最終履歴の名称
                       CONTINUE
                   ELSE
                       ADD     1                   TO  IDX
                   END-IF
                   MOVE    SPACE           TO  WRK-GMN-INIT-DRCD-G
                   MOVE    SYS-1010-KBNCD(2:4)
                                               TO  WRK-GMN-INIT-DRCD
                   MOVE    SYS-1010-NAME   TO  WRK-GMN-INIT-DRCDMEI
                   MOVE    WRK-GMN-INIT-DRCD-G
                                           TO  SPA-GMN-INIT-DRCD-LIST
                                                               (IDX)
      *
                   IF     (SPA-NAI-DRSTYUKYMD (IDX)  =   SPACE)
                      OR  (SPA-NAI-DRSTYUKYMD (IDX)
                                               >   SYS-1010-STYUKYMD)
                       MOVE    SYS-1010-STYUKYMD
                                           TO  SPA-NAI-DRSTYUKYMD
                                                               (IDX)
                   END-IF
                   IF     (SPA-NAI-DREDYUKYMD (IDX)  =   SPACE)
                      OR  (SPA-NAI-DREDYUKYMD (IDX)
                                               <   SYS-1010-EDYUKYMD)
                       MOVE    SYS-1010-EDYUKYMD
                                           TO  SPA-NAI-DREDYUKYMD
                                                               (IDX)
                   END-IF
                   MOVE    IDX             TO  SPA-INIT-DRCD-MAX
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1045"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1045-REC
           ELSE
               INITIALIZE  SYS-1045-REC
           END-IF
      *
      *    初期表示ドクター
           MOVE    SYS-1045-INIT-DRCD(2:4)
                                       TO  SPA-GMN-INIT-DRCD
      *    予約一覧の初期表示
           MOVE    SYS-1045-YYKINIT-FLG
                                       TO  SPA-GMN-YYKINIT-FLG
      *    予約枠最大件数
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYS-1045-YYKMAXCNT
                                       TO  SPA-GMN-YYKMAXCNT
           ELSE
               MOVE    12              TO  SPA-GMN-YYKMAXCNT
           END-IF
      *    予約票発行区分
           MOVE    SYS-1045-YYKHYO-KBN
                                       TO  SPA-GMN-YYKHYO-KBN
      *    予約患者番号連動区分
           MOVE    SYS-1045-YYKPTNUM-KBN
                                       TO  SPA-GMN-YYKPTNUM-KBN
      *    予約受付連携区分
           MOVE    SYS-1045-YYKUKE-KBN
                                       TO  SPA-GMN-YYKUKE-KBN
      *    受付保険組合せ区分
           MOVE    SYS-1045-UKEHKNCHK-KBN
                                       TO  SPA-GMN-UKEHKNCHK-KBN
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-YYKINIT-MAX
               IF      SPA-GMN-YYKINIT-FLG
                                       =   SPA-GMN-YYKINIT-LIST(IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-YYKINIT-LIST(IDY)
                                           TO  SPA-GMN-YYKINIT-FLG-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-GMN-YYKINIT-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-YYKINIT-LIST (1)
                                       TO  SPA-GMN-YYKINIT-FLG-G
           END-IF
      *    ドクター
           IF      SPA-GMN-INIT-DRCD   =   SPACE
               MOVE    SPA-GMN-INIT-DRCD-LIST (1)
                                       TO  SPA-GMN-INIT-DRCD-G
           END-IF
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-INIT-DRCD-MAX
               IF      SPA-GMN-INIT-DRCD
                                       =   SPA-GMN-INIT-DRCD-LIST(IDY)
                                                             (1:4)
                   MOVE    SPA-GMN-INIT-DRCD-LIST(IDY)
                                           TO  SPA-GMN-INIT-DRCD-G
      *R01.12
                   MOVE    SPA-NAI-DRSTYUKYMD (IDY)
                                           TO  WRK-DRSTYUKYMD
                   MOVE    SPA-NAI-DREDYUKYMD (IDY)
                                           TO  WRK-DREDYUKYMD
      *
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-INIT-DRCD-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               PERFORM 4511-INIT-DRCD-CHK-SEC
               MOVE    SPACE           TO  SPA-ERRCD
      *R01.12
           ELSE
               PERFORM 45111-DRCD-KIKANCHK-SEC
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-YYKHYO-MAX
               IF      SPA-GMN-YYKHYO-KBN
                                       =   SPA-GMN-YYKHYO-LIST(IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-YYKHYO-LIST(IDY)
                                           TO  SPA-GMN-YYKHYO-KBN-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-GMN-YYKHYO-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-YYKHYO-LIST (1)
                                       TO  SPA-GMN-YYKHYO-KBN-G
           END-IF
      *    受付連携
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-YYKUKE-MAX
               IF      SPA-GMN-YYKUKE-KBN
                                       =   SPA-GMN-YYKUKE-LIST(IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-YYKUKE-LIST(IDY)
                                           TO  SPA-GMN-YYKUKE-KBN-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-GMN-YYKUKE-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-YYKUKE-LIST (1)
                                       TO  SPA-GMN-YYKUKE-KBN-G
           END-IF
      *    患者番号連携
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-YYKPTNUM-MAX
               IF      SPA-GMN-YYKPTNUM-KBN
                                       =   SPA-GMN-YYKPTNUM-LIST(IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-YYKPTNUM-LIST(IDY)
                                           TO  SPA-GMN-YYKPTNUM-KBN-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-GMN-YYKPTNUM-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-YYKPTNUM-LIST (1)
                                       TO  SPA-GMN-YYKPTNUM-KBN-G
           END-IF
      *
      *    受付保険組合せ区分
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-UKEHKNCHK-MAX
               IF      SPA-GMN-UKEHKNCHK-KBN
                                       =   SPA-GMN-UKEHKNCHK-LIST(IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-UKEHKNCHK-LIST(IDY)
                                           TO  SPA-GMN-UKEHKNCHK-KBN-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-GMN-UKEHKNCHK-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-UKEHKNCHK-LIST (1)
                                       TO  SPA-GMN-UKEHKNCHK-KBN-G
           END-IF
      *
           MOVE    700                 TO  SPA-GMN-CUR
      *
           . 
       310-SPASET-1045-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数算定基準スパ初期編集処理
      *****************************************************************
       310-SPASET-1201-SEC         SECTION.
      *
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1201"              TO  SYS-KANRICD
           MOVE    "A0000001"          TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    NOT =   ZERO
               MOVE    SPACE               TO  SYSKANRI-REC
               INITIALIZE                      SYSKANRI-REC
           END-IF
      *
      *    妥結率区分
           MOVE    SYS-KANRITBL(1:1)   TO  SPA-GMN-DAKETUKBN
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-DAKETUKBN-MAX
               IF      SPA-GMN-DAKETUKBN
                                       =   SPA-GMN-DAKETUKBN-LIST(IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-DAKETUKBN-LIST(IDY)
                                           TO  SPA-GMN-DAKETUKBN-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-GMN-DAKETUKBN-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-DAKETUKBN-LIST (1)
                                       TO  SPA-GMN-DAKETUKBN-G
           END-IF
      *
      *    低紹介率区分
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1202"              TO  SYS-KANRICD
           MOVE    "A0000001"          TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    NOT =   ZERO
               MOVE    SPACE               TO  SYSKANRI-REC
               INITIALIZE                      SYSKANRI-REC
           END-IF
           MOVE    SYS-KANRITBL(1:1)   TO  SPA-GMN-TEIRITUKBN
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-TEIRITUKBN-MAX
               IF      SPA-GMN-TEIRITUKBN
                                       =   SPA-GMN-TEIRITUKBN-LIST(IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-TEIRITUKBN-LIST(IDY)
                                           TO  SPA-GMN-TEIRITUKBN-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-GMN-TEIRITUKBN-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-TEIRITUKBN-LIST (1)
                                       TO  SPA-GMN-TEIRITUKBN-G
           END-IF
      *
           MOVE    800                 TO  SPA-GMN-CUR
      *
           . 
       310-SPASET-1201-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "SYUKARI1DAY1"
               WHEN    "CLICKED"       ALSO    "SYUKARI1DAY2"
               WHEN    "CLICKED"       ALSO    "SYUKARI1DAY3"
               WHEN    "CLICKED"       ALSO    "SYUKARI1DAY4"
               WHEN    "CLICKED"       ALSO    "SYUKARI1DAY5"
               WHEN    "CLICKED"       ALSO    "SYUKARI1DAY6"
               WHEN    "CLICKED"       ALSO    "SYUKARI1DAY7"
               WHEN    "CLICKED"       ALSO    "SYUKARI2DAY1"
               WHEN    "CLICKED"       ALSO    "SYUKARI2DAY2"
               WHEN    "CLICKED"       ALSO    "SYUKARI2DAY3"
               WHEN    "CLICKED"       ALSO    "SYUKARI2DAY4"
               WHEN    "CLICKED"       ALSO    "SYUKARI2DAY5"
               WHEN    "CLICKED"       ALSO    "SYUKARI2DAY6"
               WHEN    "CLICKED"       ALSO    "SYUKARI2DAY7"
                   MOVE    MCP-WIDGET (8:1)
                                       TO  WRK-KARIDAY
                   PERFORM 4110-SYUKARIDAY-CHK-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC        SECTION.
      *
      *    自動算定情報
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   7
      *        MOVE    W07-DATA (IDX)      TO  SPA-GMN-DATA-G (IDX)
      *    END-PERFORM
           MOVE    W07-DATA01          TO  SPA-GMN-DATA-G (01)
           MOVE    W07-DATA02          TO  SPA-GMN-DATA-G (02)
           MOVE    W07-DATA03          TO  SPA-GMN-DATA-G (03)
           MOVE    W07-DATA04          TO  SPA-GMN-DATA-G (04)
           MOVE    W07-DATA05          TO  SPA-GMN-DATA-G (05)
           MOVE    W07-DATA06          TO  SPA-GMN-DATA-G (06)
           MOVE    W07-DATA07          TO  SPA-GMN-DATA-G (07)
      *    入院初診料自動算定
           MOVE    W07-NYUSYOSIN       TO  SPA-GMN-NYUSYOSIN-G
      *    時間外加算特例乳幼児
           MOVE    W07-JIKANTOKUNYU    TO  SPA-GMN-JIKANTOKU-NYU-G
           MOVE    W07-GAZOUKSN1       TO  SPA-GMN-GAZOUKSN1-G
      *    中途終了展開時の自動発生
           MOVE    W07-CHUTOTENKAI     TO  SPA-GMN-CHUTOTENKAI-G
      *    訂正時の自動発生
           MOVE    W07-TEISEI          TO  SPA-GMN-TEISEIAUTO-G
      *    訂正時の自動発生
           MOVE    W07-TEISEITOKU      TO  SPA-GMN-TEISEITOKU-G
      *    訂正時の自動発生（入院）
           MOVE    W07-TEINYUAUTO      TO  SPA-GMN-TEINYUAUTO-G
      *    訂正時の展開（処方料）
           MOVE    W07-TEISHOHOU       TO  SPA-GMN-TEISHOHOU-G
      *    病名からの自動発生
           MOVE    W07-BYOMEIAUTO      TO  SPA-GMN-BYOMEIAUTO-G
      *    入院時初回加算の自動発生
           MOVE    W07-NYUKENSA        TO  SPA-GMN-NYUKENSA-G
      *    初診再診料の自動発生
           MOVE    W07-INITAUTO        TO  SPA-GMN-INITAUTO-G
      *
           MOVE    W07-LASTKIKAN       TO  SPA-GMN-LAST-KIKAN
      *
           MOVE    W07-SRYKACD         TO  SPA-GMN-SRYKACD
           MOVE    W07-SRYKAMEI        TO  SPA-GMN-SRYKAMEI
      *    時間外加算乳幼児チェック
           MOVE    W07-JIKANTOKU-CHK   TO  SPA-GMN-JIKANTOKU-CHK-G
      *    チェック除外時間
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    W07-JIKANW1-HH(IDX) TO  SPA-GMN-JIKANWST-HH (IDX)
               MOVE    W07-JIKANW1-MM(IDX) TO  SPA-GMN-JIKANWST-MM (IDX)
               MOVE    W07-JIKANW2-HH(IDX) TO  SPA-GMN-JIKANWED-HH (IDX)
               MOVE    W07-JIKANW2-MM(IDX) TO  SPA-GMN-JIKANWED-MM (IDX)
      *
               MOVE    W07-JIKANS1-HH(IDX) TO  SPA-GMN-JIKANSST-HH (IDX)
               MOVE    W07-JIKANS1-MM(IDX) TO  SPA-GMN-JIKANSST-MM (IDX)
               MOVE    W07-JIKANS2-HH(IDX) TO  SPA-GMN-JIKANSED-HH (IDX)
               MOVE    W07-JIKANS2-MM(IDX) TO  SPA-GMN-JIKANSED-MM (IDX)
           END-PERFORM
      *
      *    チェック制御
           MOVE    W07-KIKAN           TO  SPA-GMN-INTERACT-KIKAN
      *H28.8
      *    相互作用チェック区分
           MOVE    W07-KINKCHK         TO  SPA-GMN-KINKCHK-G
      *H28.11
           MOVE    W07-TOUYOCHK        TO  SPA-GMN-TOUYOCHK-G
      *H29.4
           MOVE    W07-SHOHOU30CHK     TO  SPA-GMN-SHOHOU30CHK-G
      *
           MOVE    W07-GAIKANKBN       TO  SPA-GMN-GAIKANRIKBN-G
           MOVE    W07-GAIMOVE         TO  SPA-GMN-GAIMOVE-G
           MOVE    W07-SURYOZERO       TO  SPA-GMN-SURYOZERO-G
           MOVE    W07-CHOKICHK        TO  SPA-GMN-CHOKICHK-G
           MOVE    W07-CLEARCHK        TO  SPA-GMN-CLEARCHK-G
           MOVE    W07-YAKJYOCHK       TO  SPA-GMN-YAKJYOCHK-G
           MOVE    W07-ZENHKNCHK       TO  SPA-GMN-ZENHKNCHK-G
           MOVE    W07-TECHOKBN        TO  SPA-GMN-TECHOKBN-G
           MOVE    W07-JGAIKEIKBN      TO  SPA-GMN-JGAIKEIKBN-G
      *
           MOVE    W07-HOUKATU         TO  SPA-GMN-HOUKATU-G
           MOVE    W07-BYOHYOKBN       TO  SPA-GMN-BYOHYOKBN-G
           MOVE    W07-ZAIJUNKBN       TO  SPA-GMN-ZAIJUNKBN-G
           MOVE    W07-KENSAAUTO       TO  SPA-GMN-KENSAAUTO-G
      *
      *    患者登録情報
           MOVE    W07-BYOMEIKBN       TO  SPA-GMN-BYOMEIKBN-G
           MOVE    W07-JISYOKBN        TO  SPA-GMN-JISYOKBN-G
           MOVE    W07-HAKKOKBN        TO  SPA-GMN-HAKKOKBN-G
           MOVE    W07-HOJYOKBN        TO  SPA-GMN-HOJYOKBN-G
           MOVE    W07-DOUMEI-CHKFLG   TO  SPA-GMN-DOUMEI-CHKFLG-G
           MOVE    W07-JYUFUKU-CHKKBN  TO  SPA-GMN-JYUFUKU-CHKKBN-G
           MOVE    W07-KOHONLY-CHKFLG  TO  SPA-GMN-KOHONLY-CHKFLG-G
           MOVE    W07-PTNUMDEL-FLG    TO  SPA-GMN-PTNUMDEL-FLG-G
           MOVE    W07-HONKZKKBN       TO  SPA-GMN-HONKZKKBN-FLG-G
           MOVE    W07-KZKNEN          TO  SPA-GMN-KZKNEN
           MOVE    W07-HIHKNJAKBN      TO  SPA-GMN-HIHKNJAKBN-FLG-G
           MOVE    W07-SETAIKBN        TO  SPA-GMN-SETAIKBN-FLG-G
           MOVE    W07-GRPPTLST        TO  SPA-GMN-GRPPTLST-FLG-G
           MOVE    W07-KOHRRK          TO  SPA-GMN-KOHRRKKBN-FLG-G
           MOVE    W07-POSTKBN         TO  SPA-GMN-POSTKBN-FLG-G
           MOVE    W07-KENMEIKBN       TO  SPA-GMN-KENMEIKBN-FLG-G
           MOVE    W07-TOWNNAME2KBN    TO  SPA-GMN-TOWNNAME2KBN-G
           MOVE    W07-KOHHBTKBN       TO  SPA-GMN-KOHHBTKBN-G
           MOVE    W07-HKNINITKBN      TO  SPA-GMN-HKNINITKBN-G
      *
      *    診療行為機能情報
           MOVE    W07-SRYNYKNKNRKBN   TO  SPA-GMN-SRYNYKNKNRKBN-G
           MOVE    W07-KAGRPFLG        TO  SPA-GMN-KAGRPFLG-G
           MOVE    W07-HAKHOUFLG       TO  SPA-GMN-HAKHOUFLG-G
           MOVE    W07-TEIKINKBN       TO  SPA-GMN-TEIKINKBN-G
           MOVE    W07-YKJCHKKBN       TO  SPA-GMN-YKJCHKKBN-G
           MOVE    W07-OKUSURIKBN      TO  SPA-GMN-OKUSURIKBN-G
           MOVE    W07-DOSRYKBN-FLG    TO  SPA-GMN-DOSRYKBNFLG-G
           MOVE    W07-WKSRYHYO-FLG    TO  SPA-GMN-WKSRYHYOFLG-G
           MOVE    W07-FORMAL-FLG      TO  SPA-GMN-FORMALFLG-G
           MOVE    W07-AKUSEIHYO       TO  SPA-GMN-AKUSEIHYO-G
           MOVE    W07-TEISEIFLG       TO  SPA-GMN-TEISEIFLG-G
           MOVE    W07-CHG310FLG       TO  SPA-GMN-CHG310FLG-G
           MOVE    W07-CHGSRYSYU       TO  SPA-GMN-CHGSRYSYU-G
           MOVE    W07-RESPRV038       TO  SPA-GMN-RESPRV038-G
           MOVE    W07-HAIKIKBN        TO  SPA-GMN-HAIKIKBN-G
           MOVE    W07-YKJOKUKBN       TO  SPA-GMN-YKJOKUKBN-G
           MOVE    W07-NYKHNKKBN       TO  SPA-GMN-NYKHNKKBN-G
           MOVE    W07-SEI20KSNKBN     TO  SPA-GMN-SEI20KSNKBN-G
           MOVE    W07-SIN20KSNKBN     TO  SPA-GMN-SIN20KSNKBN-G
           MOVE    W07-YMDHENKBN       TO  SPA-GMN-YMDHENKBN-G
      *H24.4
      *    確認画面名称表示区分
           MOVE    W07-GENERIC-FLG     TO  SPA-GMN-GENERICFLG-G
      *H30.9
      *    選択式コメント一覧区分
           MOVE    W07-RECEKISAI-FLG   TO  SPA-GMN-RECEKISAIFLG-G
      *
           MOVE    W07-GMNHYOKBN1      TO  SPA-GMN-GMNHYOKBN-G (1)
           MOVE    W07-GMNHYOKBN2      TO  SPA-GMN-GMNHYOKBN-G (2)
           MOVE    W07-GMNHYOKBN3      TO  SPA-GMN-GMNHYOKBN-G (3)
           MOVE    W07-GMNHYOKBN4      TO  SPA-GMN-GMNHYOKBN-G (4)
           MOVE    W07-GMNHYOKBN5      TO  SPA-GMN-GMNHYOKBN-G (5)
           MOVE    W07-GMNHYOKBN6      TO  SPA-GMN-GMNHYOKBN-G (6)
           MOVE    W07-BGCOLORS        TO  SPA-GMN-BGCOLORS
           MOVE    W07-BGLINEKBN       TO  SPA-GMN-BGLINEKBN-G
           MOVE    W07-BGCOLORSDAY     TO  SPA-GMN-BGCOLORSDAY
           MOVE    W07-BGCOLORSJ0      TO  SPA-GMN-BGCOLORSJ0
           MOVE    W07-BGCOLORSJ1      TO  SPA-GMN-BGCOLORSJ(1)
           MOVE    W07-BGCOLORSJ2      TO  SPA-GMN-BGCOLORSJ(2)
           MOVE    W07-BGCOLORSJ3      TO  SPA-GMN-BGCOLORSJ(3)
           MOVE    W07-BGCOLORSJ4      TO  SPA-GMN-BGCOLORSJ(4)
           MOVE    W07-BGCOLORSJ5      TO  SPA-GMN-BGCOLORSJ(5)
           MOVE    W07-BGCOLORSJ6      TO  SPA-GMN-BGCOLORSJ(6)
           MOVE    W07-BGCOLORSJ7      TO  SPA-GMN-BGCOLORSJ(7)
           MOVE    W07-BGCOLORSJ8      TO  SPA-GMN-BGCOLORSJ(8)
           MOVE    W07-ERRCOLORS       TO  SPA-GMN-ERRCOLORS
           MOVE    W07-KEICOLORS       TO  SPA-GMN-KEICOLORS
           MOVE    W07-PRVLAYOUT038    TO  SPA-GMN-PRVLAYOUT038-G
           MOVE    W07-SETALLKBN       TO  SPA-GMN-SETALLKBN-G
      *
      *    収納機能情報
           MOVE    W07-SYUNYKNKNRKBN   TO  SPA-GMN-SYUNYKNKNRKBN-G
           MOVE    W07-IKTSRYYMKBN     TO  SPA-GMN-IKTSRYYMKBN-G
           MOVE    W07-SYUSKYPRTG      TO  SPA-GMN-SYUSKYPRTG-G
           MOVE    W07-S03SKYMISYUPRT  TO  SPA-GMN-S03SKYMISYUPRT-G
           MOVE    W07-SYUSRYPRTG      TO  SPA-GMN-SYUSRYPRTG-G
           MOVE    W07-SYUSKYPRTN      TO  SPA-GMN-SYUSKYPRTN-G
           MOVE    W07-SYUSRYPRTN      TO  SPA-GMN-SYUSRYPRTN-G
           MOVE    W07-S04SHIHARAIKBN  TO  SPA-GMN-S04SHIHARAIKBN-G
           MOVE    W07-S04NYUGAIKBN    TO  SPA-GMN-S04NYUGAIKBN-G
           MOVE    W07-S04SRYKBN       TO  SPA-GMN-S04SRYKBN-G
           MOVE    W07-S04KIKANKBN     TO  SPA-GMN-S04KIKANKBN-G
           MOVE    W07-S04DSPKBN       TO  SPA-GMN-S04DSPKBN-G
           MOVE    W07-S04SUMKBN       TO  SPA-GMN-S04SUMKBN-G
           MOVE    W07-S04SYORIKBN     TO  SPA-GMN-S04SYORIKBN-G
           MOVE    W07-S04JIHIPRTKBN   TO  SPA-GMN-S04JIHIPRTKBN-G
           MOVE    W07-S06SKYPRT       TO  SPA-GMN-S06SKYPRT-G
           MOVE    W07-S06SKYSUM       TO  SPA-GMN-S06SKYSUM-G
           MOVE    W07-S02SKYSAI       TO  SPA-GMN-S02SKYSAI-G
           MOVE    W07-S02SKYMISYUPRT  TO  SPA-GMN-S02SKYMISYUPRT-G
           MOVE    W07-TUKIGRP         TO  SPA-GMN-TUKIGRP-G
           MOVE    W07-TUKISKYMISYUPRT TO  SPA-GMN-TUKISKYMISYUPRT-G
           MOVE    W07-TUKISUM         TO  SPA-GMN-TUKISUM-G
           MOVE    W07-S02MODE         TO  SPA-GMN-S02MODE-G
           MOVE    W07-S02LIMIT        TO  SPA-GMN-S02LIMIT
           MOVE    W07-S02SORTKBN      TO  SPA-GMN-S02SORTKBN-G
           MOVE    W07-SAIKEISANSKYCHK TO  SPA-GMN-SAIKEISANSKYCHK-G
           MOVE    W07-IKTSORTKBN      TO  SPA-GMN-IKTSORTKBN-G
           MOVE    W07-IKTKEISANKBN      TO  SPA-GMN-IKTKEISANKBN-G
           MOVE    W07-SYUSTIME-HH     TO  SPA-GMN-SYUSTIME-HH
           MOVE    W07-SYUSTIME-MM     TO  SPA-GMN-SYUSTIME-MM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   2 )
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   2 )
                   MOVE    W07-SYUKARISTTIME (IDX1 IDX2)
                                       TO  SPA-GMN-SYUKARISTTIME
                                                       (IDX1 IDX2)
                   MOVE    W07-SYUKARIEDTIME (IDX1 IDX2)
                                       TO  SPA-GMN-SYUKARIEDTIME
                                                       (IDX1 IDX2)
      *
               END-PERFORM
           END-PERFORM
           MOVE    W07-MISYUKIKANKBN  TO  SPA-GMN-MISYUKIKANKBN-G
      *
      *    病名機能情報
           MOVE    W07-MSG1-DSPKBN     TO  SPA-GMN-MSG1-DSPKBN-G
           MOVE    W07-MSG2-DSPKBN     TO  SPA-GMN-MSG2-DSPKBN-G
           MOVE    W07-MSG3-DSPKBN     TO  SPA-GMN-MSG3-DSPKBN-G
           MOVE    W07-MSG4-DSPKBN     TO  SPA-GMN-MSG4-DSPKBN-G
           MOVE    W07-MSG5-DSPKBN     TO  SPA-GMN-MSG5-DSPKBN-G
           MOVE    W07-SRYKA-DSPKBN    TO  SPA-GMN-SRYKA-DSPKBN-G
           MOVE    W07-SYOKBN-DSPKBN   TO  SPA-GMN-SYOKBN-DSPKBN-G
           MOVE    W07-UTAGAIFLG-KBN   TO  SPA-GMN-UTAGAIFLG-KBN-G
           MOVE    W07-SJKBN           TO  SPA-GMN-SJKBN-G
      *
      *    会計照会機能情報 
           MOVE    W07-SRYKBN-FLG      TO  SPA-GMN-SRYKBN-FLG-G
           MOVE    W07-SRYKA-FLG       TO  SPA-GMN-SRYKA-FLG-G
           MOVE    W07-RESPRV043       TO  SPA-GMN-RESPRV043-G
           MOVE    W07-PRVLAYOUT043    TO  SPA-GMN-PRVLAYOUT043-G
      *
      *    予約機能情報 
           MOVE    W07-INIT-DRCD       TO  SPA-GMN-INIT-DRCD-G
           MOVE    W07-YYKINIT-FLG     TO  SPA-GMN-YYKINIT-FLG-G
           MOVE    W07-YYKMAXCNT       TO  SPA-GMN-YYKMAXCNT
           MOVE    W07-YYKHYO-KBN      TO  SPA-GMN-YYKHYO-KBN-G
           MOVE    W07-YYKUKE-KBN      TO  SPA-GMN-YYKUKE-KBN-G
           MOVE    W07-YYKPTNUM-KBN    TO  SPA-GMN-YYKPTNUM-KBN-G
           MOVE    W07-UKEHKNCHK       TO  SPA-GMN-UKEHKNCHK-KBN-G
      *
      *    点数算定基準情報
           MOVE    W07-DAKETUKBN       TO  SPA-GMN-DAKETUKBN-G
           MOVE    W07-TEIRITUKBN      TO  SPA-GMN-TEIRITUKBN-G
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
           EVALUATE    SPA-W01-KANRICD
               WHEN    "1007"
      *            外来診察料チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4100-INITAUTO-CHK-SEC
                   END-IF
      *            育児栄養加算チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4100-DATA07-CHK-SEC
                   END-IF
      *            診療科チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4102-SRYKACD-CHK-SEC
                   END-IF
      *            療養担当手当チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4100-DATA05-CHK-SEC
                   END-IF
      *            病名からの自動発生
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-BYOMEIAUTO-CHK-SEC
                   END-IF
      *            中途終了展開時の自動発生
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-CHUTOTENKAI-CHK-SEC
                   END-IF
      *            入院初診料自動算定チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-NYUSYOSIN-CHK-SEC
                   END-IF
      *            時間外加算特例乳幼児
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-JIKANTOKU-CHK-SEC
                   END-IF
      *            特定薬剤治療管理チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4100-DATA06-CHK-SEC
                   END-IF
      *            調剤技術基本料から薬剤情報提供料チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4100-DATA-CHK-SEC
                   END-IF
      *            画像診断管理加算１
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-GAZOUKSN1-CHK-SEC
                   END-IF
      *            検体検査加算自動発生）
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-KENSAAUTO-CHK-SEC
                   END-IF
      *            入院時初回加算の自動発生
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-NYUKENSA-CHK-SEC
                   END-IF
      *            訂正時の自動発生
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-TEISEIAUTO-CHK-SEC
                   END-IF
      *            訂正時の自動発生（特定疾患処方管理）
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-TEISEITOKU-CHK-SEC
                   END-IF
      *            訂正時の自動発生（入院）
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-TEINYUAUTO-CHK-SEC
                   END-IF
      *H29.3
      *            訂正時の展開（処方料）
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-TEISHOHOU-CHK-SEC
                   END-IF
      *            相互作用チェック期間
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4100-KIKAN-CHK2-SEC
                   END-IF
      *            外来管理加算チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4101-GAIKANRIKBN-CHK2-SEC
                   END-IF
      *            外来カーソル移動チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-GAIMOVE-CHK-SEC
                   END-IF
      *            数量ゼロチェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-SURYOZERO-CHK-SEC
                   END-IF
      *            入院調基チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-CHOKICHK-CHK-SEC
                   END-IF
      *            時間外加算乳幼児チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-JIKANTOKU-CHK-CHK-SEC
                   END-IF
      *            チェック除外時間
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-JIKANCHK-TIME-SEC
                   END-IF
      *            クリア確認チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-CLERACHK-CHK-SEC
                   END-IF
      *            薬剤情報提供料チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-YAKJYOCHK-CHK-SEC
                   END-IF
      *            前回保険組合せ相違チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-ZENHKNCHK-CHK-SEC
                   END-IF
      *            手帳加算確認チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-TECHOKBN-CHK-SEC
                   END-IF
      *ver.4.8
      *            時間外緊急院内チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-JGAIKEIKBN-CHK-SEC
                   END-IF
      *H28.11
      *            薬剤投与量チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-TOUYOCHK-CHK-SEC
                   END-IF
      *H29.4
      *            投薬３０日超チェック区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-SHOHOU30CHK-CHK-SEC
                   END-IF
      *            包括入力チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-HOUKATU-CHK-SEC
                   END-IF
      *            病名表示区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-BYOHYOKBN-CHK-SEC
                   END-IF
      *            剤登録順区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-ZAIJUNKBN-CHK-SEC
                   END-IF
               WHEN    "1017"
      *            カルテの病名対象チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4104-BYOMEI-CHK-SEC
                   END-IF
      *            人名辞書使用チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4105-JISYO-CHK-SEC
                   END-IF
      *            患者登録のカルテ発行チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4106-HAKKO-CHK-SEC
                   END-IF
      *            自費保険の補助区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4107-HOJYO-CHK-SEC
                   END-IF
      *            同姓同名リスト区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4107-DOUMEI-CHK-SEC
                   END-IF
      *            二重登録疑い区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4107-JYUFUKU-CHK-SEC
                   END-IF
      *            地方公費単独チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4107-KOHONLY-CHKFLG-SEC
                   END-IF
      *            患者情報削除機能
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4107-PTNUMDEL-FLG-SEC
                   END-IF
      *            本人・家族区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4107-HONKZKKBN-FLG-SEC
                   END-IF
      *            家族年齢
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4107-KZKNEN-CHK-SEC
                   END-IF
      *            被保険者名設定区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4107-HIHKNJAKBN-FLG-SEC
                   END-IF
      *            世帯主名自動記載区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4107-SETAIKBN-FLG-SEC
                   END-IF
      *            グループ医療機関患者リスト
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4107-GRPPTLST-FLG-SEC
                   END-IF
      *            公費上限額履歴チェック区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4107-KOHRRKKBN-FLG-SEC
                   END-IF
      *            郵便番号自動記載区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4107-POSTKBN-FLG-SEC
                   END-IF
      *            都道府県名記載区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4107-KENMEIKBN-FLG-SEC
                   END-IF
      *            京都府通り名称編集区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4107-TOWNNAME2KBN-FLG-SEC
                   END-IF
      *            公費法別一覧表示区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4107-KOHHBTKBN-FLG-SEC
                   END-IF
      *            保険初期表示区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4107-HKNINITKBN-FLG-SEC
                   END-IF
               WHEN    "1038"
      *            ＤＯ初期表示
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-DOSRYKBNFLG-CHK-SEC
                   END-IF
      *            中途表示一覧
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-WKSRYHYOFLG-CHK-SEC
                   END-IF
      *            検査正式名称表示
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-FORMALFLG-CHK-SEC
                   END-IF
      *            腫瘍マーカー検査表示
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-AKUSEIHYO-CHK-SEC
                   END-IF
      *H30.9
      *            選択式コメント一覧区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-RECEKISAIFLG-CHK-SEC
                   END-IF
      *H24.4
      *            確認画面名称表示区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-GENERICFLG-CHK-SEC
                   END-IF
      *            同日再診・訂正選択
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-TEISEIFLG-CHK-SEC
                   END-IF
      *            皮下筋肉注射料変換選択
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-CHG310FLG-CHK-SEC
                   END-IF
      *            投薬診療種別変換選択
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-CHGSRYSYU-CHK-SEC
                   END-IF
      *            レセプトブレビュー
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-RESPRV038-CHK-SEC
                   END-IF
      *            残量廃棄区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-HAIKIKBN-CHK-SEC
                   END-IF
      *            精神療法２０未満加算区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-SEI20KSNKBN-CHK-SEC
                   END-IF
      *            心身療法２０未満加算区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-SIN20KSNKBN-CHK-SEC
                   END-IF
      *            レセプトブレビューレイアウト
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-PRVLAYOUT038-CHK-SEC
                   END-IF
      *            セット展開表示
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-SETALLKBN-CHK-SEC
                   END-IF
      *            リハビリ・患者情報表示区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-GMNHYOKBN-CHK-SEC
                   END-IF
      *            入金管理区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-SRYNYKNKNRKBN-CHK-SEC
                   END-IF
      *            複数科まとめチェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-KAGRPFLG-CHK-SEC
                   END-IF
      *            請求書発行方法チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-HAKHOUFLG-CHK-SEC
                   END-IF
      *            訂正請求書金額区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-TEIKINKBN-CHK-SEC
                   END-IF
      *            薬剤情報提供発行チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-YKJCHKKBN-CHK-SEC
                   END-IF
      *            お薬手帳発行チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-OKUSURIKBN-CHK-SEC
                   END-IF
      *            薬情・お薬手帳対象チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-YKJOKUKBN-CHK-SEC
                   END-IF
      *            入金・返金額設定区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-NYKHNKKBN-CHK-SEC
                   END-IF
      *            発行日変更区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-YMDHENKBN-CHK-SEC
                   END-IF
      *            環境設定背景色標準
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-BGCOLORSJ0-CHK-SEC
                   END-IF
      *            環境設定背景色（日付変更）
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-BGCOLORSDAY-CHK-SEC
                   END-IF
      *            環境設定背景色（時間外）
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-BGCOLORJG-CHK-SEC
                   END-IF
      *            入力スクロール背景色
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-BGCOLORS-CHK-SEC
                   END-IF
      *            入力スクロール背景行区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-BGLINEKBN-CHK-SEC
                   END-IF
      *            エラー背景色
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-ERRCOLORS-CHK-SEC
                   END-IF
      *            警告背景色
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-KEICOLORS-CHK-SEC
                   END-IF
               WHEN    "1039"
      *            請求書兼領収書発行区分（外来）チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-SYUSKYPRTG-CHK-SEC
                   END-IF
      *            請求書兼領収書前回請求額印字区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-S03SKYMISYUPRT-CHK-SEC
                   END-IF
      *            診療費明細書発行区分（外来）チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-SYUSRYPRTG-CHK-SEC
                   END-IF
      *            請求書兼領収書発行区分（入院）チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-SYUSKYPRTN-CHK-SEC
                   END-IF
      *            診療費明細書発行区分（入院）チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-SYUSRYPRTN-CHK-SEC
                   END-IF
      *            支払証明書印刷画面支払証明書発行区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4109-S04SHIHARAIKBN-CHK-SEC
                   END-IF
      *            支払証明書印刷画面入外区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4109-S04NYUGAIKBN-CHK-SEC
                   END-IF
      *            支払証明書印刷画面期間指定チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4109-S04SRYKBN-CHK-SEC
                   END-IF
      *            支払証明書印刷画面期間区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4109-S04KIKANKBN-CHK-SEC
                   END-IF
      *            支払証明書印刷画面請求内容の印字チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4109-S04DSPKBN-CHK-SEC
                   END-IF
      *            支払証明書印刷画面期間開始前診療分の支払いチェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4109-S04SUMKBN-CHK-SEC
                   END-IF
      *            支払証明書印刷画面期間終了後の支払いチェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4109-S04SYORIKBN-CHK-SEC
                   END-IF
      *            支払証明書印刷画面自費内訳明細書チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4109-S04JIHIPRTKBN-CHK-SEC
                   END-IF
      *            入金管理区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4109-SYUNYKNKNRKBN-CHK-SEC
                   END-IF
      *            診療年月指定区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4109-IKTSRYYMKBN-CHK-SEC
                   END-IF
      *            一括入金画面請求書兼領収書発行区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4109-S06SKYPRT-CHK-SEC
                   END-IF
      *            一括入金画面外来分発行方法チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4109-S06SKYSUM-CHK-SEC
                   END-IF
      *            請求一覧画面外来分再発行方法チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4109-S02SKYSAI-CHK-SEC
                   END-IF
      *            請求書兼領収書前回請求額印字区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-S02SKYMISYUPRT-CHK-SEC
                   END-IF
      *            月別請求書兼領収書発行方法チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-TUKIGRP-CHK-SEC
                   END-IF
      *            月別請求書兼領収書前回請求額印字区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-TUKISKYMISYUPRT-CHK-SEC
                   END-IF
      *            月別請求書兼領収書集計区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-TUKISUM-CHK-SEC
                   END-IF
      *            請求一覧画コラムリストチェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-S02MODE-CHK-SEC
                   END-IF
      *            請求一覧画コラムリスト件数チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-S02LIMIT-CHK-SEC
                   END-IF
      *            請求一覧画面並び順区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-S02SORTKBN-CHK-SEC
                   END-IF
      *            再計算該当者チェックチェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-SAIKEISANSKYCHK-CHK-SEC
                   END-IF
      *            一括計算順区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-IKTSORTKBN-CHK-SEC
                   END-IF
      *            一括計算方法区分区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-IKTKEISANKBN-CHK-SEC
                   END-IF
      *            登録時刻チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-SYUSTIME-CHK-SEC
                   END-IF
      *            仮締め時間帯チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-SYUKARITIME-CHK-SEC
                   END-IF
      *            請求書未収金集計期間区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-MISYUKIKANKBN-CHK-SEC
                   END-IF
               WHEN    "1042"
      *            主病名確認メッセージ表示有無チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4111-MSG1-DSPKBN-CHK-SEC
                   END-IF
      *            移行病名警告メッセージ表示有無チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4111-MSG2-DSPKBN-CHK-SEC
                   END-IF
      *            廃止年月日警告メッセージ表示有無チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4111-MSG3-DSPKBN-CHK-SEC
                   END-IF
      *            同一病名警告メッセージ表示有無チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4111-MSG4-DSPKBN-CHK-SEC
                   END-IF
      *            死亡区分更新確認メッセージ表示有無チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4111-MSG5-DSPKBN-CHK-SEC
                   END-IF
      *            診療科初期表示区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4111-SRYKA-DSPKBN-CHK-SEC
                   END-IF
      *            表示対象期間初期表示区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4111-SYOKBN-DSPKBN-CHK-SEC
                   END-IF
      *            疑いフラグ設定区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4111-UTAGAIFLG-KBN-CHK-SEC
                   END-IF
      *            症状詳記区分初期表示チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4111-SJKBN-CHK-SEC
                   END-IF
               WHEN    "1043"
      *            会計照会全科表示順
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4311-SRYKBN-FLG-CHK-SEC
                   END-IF
      *            診療科の初期表示
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4311-SRYKA-FLG-CHK-SEC
                   END-IF
      *            レセプトブレビュー
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4311-RESPRV043-CHK-SEC
                   END-IF
      *            レセプトブレビューレイアウト
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4311-PRVLAYOUT043-CHK-SEC
                   END-IF
               WHEN    "1045"
      *            初期ドクター
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4511-INIT-DRCD-CHK-SEC
                   END-IF
      *            予約一覧の初期表示
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4511-YYKINIT-FLG-CHK-SEC
                   END-IF
      *            予約枠最大件数
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4511-YYKMAXCNT-CHK-SEC
                   END-IF
      *            予約票発行区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4511-YYKHYO-KBN-CHK-SEC
                   END-IF
      *            予約受付連動区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4511-YYKUKE-KBN-CHK-SEC
                   END-IF
      *            患者番号連動区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4511-YYKPTNUM-KBN-CHK-SEC
                   END-IF
      *            受付保険組合せ区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4511-UKEHKNCHK-KBN-CHK-SEC
                   END-IF
      *
               WHEN    "1201"
      *            妥結率区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4611-DAKETUKBN-CHK-SEC
                   END-IF
      *            低紹介率区分
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4611-TEIRITUKBN-CHK-SEC
                   END-IF
           END-EVALUATE
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初診再診料自動算定チェック処理
      *****************************************************************
       4100-INITAUTO-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-INITAUTO-MAX
               IF      SPA-GMN-INITAUTO
                                       =   SPA-GMN-INITAUTO-LIST(IDY)
                                                               (1:1)
                   MOVE    SPA-GMN-INITAUTO-LIST (IDY)
                                           TO  SPA-GMN-INITAUTO-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0020"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               IF      SPA-GMN-INITAUTO    =   "0"
      *            算定しないのクリア
                   PERFORM 41001-INITAUTO-HEN-SEC
               END-IF
           END-IF
      *
          .
       4100-INITAUTO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診再診料算定なしの初期表示処理
      *****************************************************************
       41001-INITAUTO-HEN-SEC       SECTION.
      *
      *    中途終了展開時の自動発生
           MOVE    "0"                 TO  SPA-GMN-CHUTOTENKAI
      *    病名からの自動発生
           MOVE    "0"                 TO  SPA-GMN-BYOMEIAUTO
      *    期間
           MOVE    ZERO                TO  SPA-GMN-LAST-KIKAN
      *    育児自動診療科
           MOVE    SPACE           TO  SPA-GMN-SRYKACD
           MOVE    SPACE           TO  SPA-GMN-SRYKAMEI
      *    育児自動算定
           MOVE    ZERO                TO  SPA-GMN-DATA(7)
      *    療養担当手当
           MOVE    ZERO                TO  SPA-GMN-DATA(5)
           .
       41001-INITAUTO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    育児栄養加算チェック
      *****************************************************************
       4100-DATA07-CHK-SEC       SECTION.
      *
           MOVE    7                   TO  IDX
      *    初診再診料算定なし
           IF      SPA-GMN-INITAUTO    =   "0"
               MOVE    "0"             TO  SPA-GMN-DATA (IDX)
           END-IF
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-DATA (IDX)
                                       =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST (IDY)
                                           TO  SPA-GMN-DATA-G(IDX)
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK             =   ZERO
               MOVE    "0010"             TO SPA-ERRCD
               MOVE    2                  TO  SPA-GMN-CUR
           END-IF
           .
       4100-DATA07-CHK-EXT.
           EXIT.
      *****************************************************************
      *    療養担当手当チェック
      *****************************************************************
       4100-DATA05-CHK-SEC       SECTION.
      *
           MOVE    5                   TO  IDX
      *    初診再診料算定なし
           IF      SPA-GMN-INITAUTO    =   "0"
               MOVE    "0"             TO  SPA-GMN-DATA (IDX)
           END-IF
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-DATA (IDX)
                                       =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST (IDY)
                                           TO  SPA-GMN-DATA-G(IDX)
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK             =   ZERO
               MOVE    "0008"             TO SPA-ERRCD
               MOVE    4                  TO  SPA-GMN-CUR
           END-IF
           .
       4100-DATA05-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定薬剤治療管理料チェック
      *****************************************************************
       4100-DATA06-CHK-SEC       SECTION.
      *
           MOVE    6                   TO  IDX
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-DATA (IDX)
                                       =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST (IDY)
                                           TO  SPA-GMN-DATA-G(IDX)
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK             =   ZERO
               MOVE    "0009"             TO SPA-ERRCD
               MOVE    10                 TO  SPA-GMN-CUR
           END-IF
           .
       4100-DATA06-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    調剤技術基本料チェック
      *****************************************************************
       4100-DATA-CHK-SEC       SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   4   )
                   OR         (SPA-ERRCD   NOT =   SPACE )
               MOVE    ZERO                TO  FLG-OK
      *
               IF      IDX                 =   2
                                           OR  3
                                           OR  4
      *            薬剤情報提供料
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   SPA-GMN-DATA-MAX2
                       IF      SPA-GMN-DATA (IDX)
                                               =   SPA-GMN-DATAL2(IDY)
                           MOVE    SPA-GMN-DATALST2 (IDY)
                                               TO  SPA-GMN-DATA-G(IDX)
                           MOVE    1               TO  FLG-OK
                       END-IF
                   END-PERFORM
               ELSE
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   SPA-GMN-DATA-MAX
                       IF      SPA-GMN-DATA (IDX)
                                               =   SPA-GMN-DATAL(IDY)
                           MOVE    SPA-GMN-DATALST (IDY)
                                               TO  SPA-GMN-DATA-G(IDX)
                           MOVE    1               TO  FLG-OK
                       END-IF
                   END-PERFORM
               END-IF
      *
               IF      FLG-OK             =   ZERO
                   EVALUATE    IDX
                       WHEN    1
                           MOVE    "0001"             TO SPA-ERRCD
                           MOVE    11                 TO  SPA-GMN-CUR
                       WHEN    2
                           MOVE    "0002"             TO SPA-ERRCD
                           MOVE    12                 TO  SPA-GMN-CUR
                       WHEN    3
                           MOVE    "0003"             TO SPA-ERRCD
                           MOVE    13                 TO  SPA-GMN-CUR
                       WHEN    4
                           MOVE    "0004"             TO SPA-ERRCD
                           MOVE    14                 TO  SPA-GMN-CUR
                   END-EVALUATE
               END-IF
           END-PERFORM
           .
       4100-DATA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    相互作用チェック期間チェック処理
      *****************************************************************
       4100-KIKAN-CHK2-SEC       SECTION.
      *
      *H28.8
      *    相互作用チェック区分
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-KINKCHK-MAX
               IF      SPA-GMN-KINKCHK     =   SPA-GMN-KINKCHK-LIST
                                                         (IDY)(1:1)
                   MOVE    SPA-GMN-KINKCHK-LIST (IDY)
                                           TO  SPA-GMN-KINKCHK-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0044"              TO  SPA-ERRCD
               MOVE    35                  TO  SPA-GMN-CUR
           END-IF
          .
       4100-KIKAN-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院初診料自動算定チェック処理
      *****************************************************************
       4103-NYUSYOSIN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-NYUSYOSIN   =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST (IDY)
                                           TO  SPA-GMN-NYUSYOSIN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0013"              TO  SPA-ERRCD
               MOVE    8                   TO  SPA-GMN-CUR
           END-IF
      *
          .
       4103-NYUSYOSIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画像診断管理加算１チェック処理
      *****************************************************************
       4103-GAZOUKSN1-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-GAZOUKSN1   =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST (IDY)
                                           TO  SPA-GMN-GAZOUKSN1-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0018"              TO  SPA-ERRCD
               MOVE    15                  TO  SPA-GMN-CUR
           END-IF
      *
          .
       4103-GAZOUKSN1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    中途終了展開時の自動発生チェック処理
      *****************************************************************
       4103-CHUTOTENKAI-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-CHUTOTENKAI  =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST (IDY)
                                           TO  SPA-GMN-CHUTOTENKAI-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0019"              TO  SPA-ERRCD
               MOVE    7                   TO  SPA-GMN-CUR
           END-IF
      *
          .
       4103-CHUTOTENKAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    訂正時の自動発生チェック処理
      *****************************************************************
       4103-TEISEIAUTO-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-TEISEIAUTO  =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST (IDY)
                                           TO  SPA-GMN-TEISEIAUTO-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0080"              TO  SPA-ERRCD
               MOVE    18                  TO  SPA-GMN-CUR
           END-IF
      *
          .
       4103-TEISEIAUTO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    訂正時の自動発生（入院）チェック処理
      *****************************************************************
       4103-TEINYUAUTO-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-TEINYUAUTO  =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST (IDY)
                                           TO  SPA-GMN-TEINYUAUTO-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0083"              TO  SPA-ERRCD
               MOVE    20                  TO  SPA-GMN-CUR
           END-IF
      *
          .
       4103-TEINYUAUTO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名疾患区分からの自動発生チェック処理
      *****************************************************************
       4103-BYOMEIAUTO-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-BYOMEIAUTO  =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST (IDY)
                                           TO  SPA-GMN-BYOMEIAUTO-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0084"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF
      *
          .
       4103-BYOMEIAUTO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院時初回加算の自動発生チェック処理
      *****************************************************************
       4103-NYUKENSA-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-NYUKENSA    =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST (IDY)
                                           TO  SPA-GMN-NYUKENSA-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0085"              TO  SPA-ERRCD
               MOVE    17                  TO  SPA-GMN-CUR
           END-IF
      *
          .
       4103-NYUKENSA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    訂正時の自動発生（特定疾患処方管理）チェック処理
      *****************************************************************
       4103-TEISEITOKU-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-TEISEITOKU  =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST (IDY)
                                           TO  SPA-GMN-TEISEITOKU-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0081"              TO  SPA-ERRCD
               MOVE    19                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       4103-TEISEITOKU-CHK-EXT.
           EXIT.
      *H29.3
      *****************************************************************
      *    訂正時の展開（処方料）チェック処理
      *****************************************************************
       4103-TEISHOHOU-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   SPA-TEISHOHOU-MAX)
                           OR (FLG-OK      =   1   )
               IF      SPA-GMN-TEISHOHOU  =   SPA-GMN-TEISHOHOU-LIST
                                                         (IDY)(1:1)
                   MOVE    SPA-GMN-TEISHOHOU-LIST (IDY)
                                           TO  SPA-GMN-TEISHOHOU-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0046"              TO  SPA-ERRCD
               MOVE    36                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       4103-TEISHOHOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検体検査加算自動発生チェック処理
      *****************************************************************
       4103-KENSAAUTO-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-KENSAAUTO-MAX
               IF      SPA-GMN-KENSAAUTO  =    SPA-GMN-KENSAAUTO-LIST
                                                         (IDY)(1:1)
                   MOVE    SPA-GMN-KENSAAUTO-LIST(IDY)
                                           TO  SPA-GMN-KENSAAUTO-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0082"              TO  SPA-ERRCD
               MOVE    16                  TO  SPA-GMN-CUR
           END-IF
      *
          .
       4103-KENSAAUTO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間外加算特例乳幼児算定チェック処理
      *****************************************************************
       4103-JIKANTOKU-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
      *    PERFORM VARYING     IDY     FROM    1   BY  1
      *            UNTIL       IDY     >   SPA-GMN-DATA-MAX
      *        IF      SPA-GMN-JIKANTOKU-NYU   =   SPA-GMN-DATAL(IDY)
      *            MOVE    SPA-GMN-DATALST (IDY)
      *                                    TO  SPA-GMN-JIKANTOKU-NYU-G
      *            MOVE    1               TO  FLG-OK
      *        END-IF
      *    END-PERFORM
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   SPA-GMN-JIKANTOKU-MAX)
                           OR (FLG-OK  =   1         )
               IF      SPA-GMN-JIKANTOKU-NYU
                                       =   SPA-GMN-JIKANTOKU-NYU-LIST
                                                        (IDY)(1:1)
                   MOVE    SPA-GMN-JIKANTOKU-NYU-LIST (IDY)
                                           TO  SPA-GMN-JIKANTOKU-NYU-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0017"              TO  SPA-ERRCD
               MOVE    9                   TO  SPA-GMN-CUR
           END-IF
      *
          .
       4103-JIKANTOKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来管理加算チェック処理
      *****************************************************************
       4101-GAIKANRIKBN-CHK2-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GAIKANRIKBN-MAX
               IF      SPA-GMN-GAIKANRIKBN-CHK
                                      =   SPA-GMN-GAIKANRIKBN-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-GAIKANRIKBN-LIST(IDX)
                                          TO  SPA-GMN-GAIKANRIKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    31              TO  SPA-GMN-CUR
               MOVE    "0006"          TO  SPA-ERRCD
           END-IF
      *
          .
       4101-GAIKANRIKBN-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動算定科チェック処理
      *****************************************************************
       4102-SRYKACD-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-DATA (7)  =   "1"
               IF      ( SPA-GMN-SRYKACD   =   "0" )
                   OR  ( SPA-GMN-SRYKACD   =   "00" )
                   OR  ( SPA-GMN-SRYKACD   =   SPACE )
                   MOVE    "00"            TO  SPA-GMN-SRYKACD
                   MOVE    "全科"          TO  SPA-GMN-SRYKAMEI
               ELSE
      *            診療科名
                   MOVE    SPACE           TO  SYSKANRI-REC 
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    "1005"          TO  SYS-KANRICD
                   MOVE    SPA-GMN-SRYKACD TO  SYS-KBNCD
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
                   MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                   MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                   PERFORM 900-SYSKANRI-INV-SEC
                   IF      FLG-SYSKANRI            =   ZERO
                       MOVE    SYSKANRI-REC        TO  SYS-1005-REC
                       MOVE    SYS-1005-SRYKANAME1
                                               TO  SPA-GMN-SRYKAMEI
                   ELSE
                       MOVE    3               TO  SPA-GMN-CUR
                       MOVE    SPACE           TO  SPA-GMN-SRYKAMEI
                       MOVE    "0011"          TO  SPA-ERRCD
                   END-IF
               END-IF
           ELSE
               MOVE    SPACE           TO  SPA-GMN-SRYKACD
               MOVE    SPACE           TO  SPA-GMN-SRYKAMEI
           END-IF
           .
       4102-SRYKACD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来カーソル移動チェック処理
      *****************************************************************
       4103-GAIMOVE-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-GAIMOVEMAX
               IF      SPA-GMN-GAIMOVE
                                      =   SPA-GMN-GAIMOVE-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-GAIMOVE-LIST(IDX)
                                          TO  SPA-GMN-GAIMOVE-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    32              TO  SPA-GMN-CUR
               MOVE    "0012"          TO  SPA-ERRCD
           END-IF
      *
          .
       4103-GAIMOVE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量ゼロ入力チェック処理
      *****************************************************************
       4103-SURYOZERO-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-SURYOZERO
                                      =   SPA-GMN-SURYOZERO-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-SURYOZERO-LIST(IDX)
                                          TO  SPA-GMN-SURYOZERO-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    33              TO  SPA-GMN-CUR
               MOVE    "0015"          TO  SPA-ERRCD
           END-IF
      *
          .
       4103-SURYOZERO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院調基チェック入力チェック処理
      *****************************************************************
       4103-CHOKICHK-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-CHOKICHK
                                      =   SPA-GMN-CHOKICHK-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-CHOKICHK-LIST(IDX)
                                          TO  SPA-GMN-CHOKICHK-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    34              TO  SPA-GMN-CUR
               MOVE    "0016"          TO  SPA-ERRCD
           END-IF
      *
          .
       4103-CHOKICHK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア確認チェック処理
      *****************************************************************
       4103-CLERACHK-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-CLEARCHK
                                      =   SPA-GMN-CLEARCHK-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-CLEARCHK-LIST(IDX)
                                          TO  SPA-GMN-CLEARCHK-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    81              TO  SPA-GMN-CUR
               MOVE    "0034"          TO  SPA-ERRCD
           END-IF
      *
          .
       4103-CLERACHK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報提供料チェック処理
      *****************************************************************
       4103-YAKJYOCHK-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-YAKJYOCHK
                                      =   SPA-GMN-YAKJYOCHK-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-YAKJYOCHK-LIST(IDX)
                                          TO  SPA-GMN-YAKJYOCHK-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    82              TO  SPA-GMN-CUR
               MOVE    "0037"          TO  SPA-ERRCD
           END-IF
      *
          .
       4103-YAKJYOCHK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    前回保険組合せ相違チェック処理
      *****************************************************************
       4103-ZENHKNCHK-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-ZENHKNCHK-MAX
               IF      SPA-GMN-ZENHKNCHK
                                      =   SPA-GMN-ZENHKNCHK-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-ZENHKNCHK-LIST(IDX)
                                          TO  SPA-GMN-ZENHKNCHK-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    83              TO  SPA-GMN-CUR
               MOVE    "0040"          TO  SPA-ERRCD
           END-IF
      *
          .
       4103-ZENHKNCHK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    手帳加算確認チェック処理
      *****************************************************************
       4103-TECHOKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TECHOKBN-MAX
               IF      SPA-GMN-TECHOKBN
                                      =   SPA-GMN-TECHOKBN-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-TECHOKBN-LIST(IDX)
                                          TO  SPA-GMN-TECHOKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    87              TO  SPA-GMN-CUR
               MOVE    "0041"          TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      (SPA-GMN-TECHOKBN   =   "1"  )
                 AND   (SPA-GMN-DATA (2)   =   "0"  )
                 AND   (SPA-GMN-DATA (3)   =   "0"  )
                 AND   (SPA-GMN-DATA (4)   =   "0"  )
                   MOVE    87              TO  SPA-GMN-CUR
                   MOVE    "0042"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
          .
       4103-TECHOKBN-CHK-EXT.
           EXIT.
      *ver.4.8
      *****************************************************************
      *    時間外緊急院内チェック処理
      *****************************************************************
       4103-JGAIKEIKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-JGAIKEIKBN-MAX )
                          OR  (FLG-OK  =   1                  )
               IF      SPA-GMN-JGAIKEIKBN
                                      =   SPA-GMN-JGAIKEIKBN-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-JGAIKEIKBN-LIST(IDX)
                                          TO  SPA-GMN-JGAIKEIKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    88              TO  SPA-GMN-CUR
               MOVE    "0043"          TO  SPA-ERRCD
           END-IF
      *
          .
       4103-JGAIKEIKBN-CHK-EXT.
           EXIT.
      *H28.11
      *****************************************************************
      *    薬剤投与量チェック処理
      *****************************************************************
       4103-TOUYOCHK-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-TOUYOCHK-MAX)
                          OR  (FLG-OK  =   1                  )
               IF      SPA-GMN-TOUYOCHK
                                      =   SPA-GMN-TOUYOCHK-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-TOUYOCHK-LIST(IDX)
                                          TO  SPA-GMN-TOUYOCHK-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    89              TO  SPA-GMN-CUR
               MOVE    "0045"          TO  SPA-ERRCD
           END-IF
      *
          .
       4103-TOUYOCHK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    投薬３０日超チェック区分処理
      *****************************************************************
       4103-SHOHOU30CHK-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-SHOHOU30CHK-MAX)
                          OR  (FLG-OK  =   1                  )
               IF      SPA-GMN-SHOHOU30CHK
                                       =   SPA-GMN-SHOHOU30CHK-LIST
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-SHOHOU30CHK-LIST(IDX)
                                          TO  SPA-GMN-SHOHOU30CHK-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    90              TO  SPA-GMN-CUR
               MOVE    "0047"          TO  SPA-ERRCD
           END-IF
      *
           .
       4103-SHOHOU30CHK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間外加算乳幼児入力チェック処理
      *****************************************************************
       4103-JIKANTOKU-CHK-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-JIKANTOKU-CHK
                                      =   SPA-GMN-JIKANTOKU-CHK-LIST
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-JIKANTOKU-CHK-LIST(IDX)
                                          TO  SPA-GMN-JIKANTOKU-CHK-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    50              TO  SPA-GMN-CUR
               MOVE    "0030"          TO  SPA-ERRCD
           END-IF
      *
          .
       4103-JIKANTOKU-CHK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック除外時間チェック処理
      *****************************************************************
       4103-JIKANCHK-TIME-SEC          SECTION.
      *
      *    時間外チェックなしの時、ゼロとする
           IF      SPA-GMN-JIKANTOKU-CHK   NOT =   "1"
               MOVE    SPACE           TO  SPA-GMN-JIKANCHK-WEEK(1)
               MOVE    SPACE           TO  SPA-GMN-JIKANCHK-WEEK(2)
               MOVE    SPACE           TO  SPA-GMN-JIKANCHK-SAT (1)
               MOVE    SPACE           TO  SPA-GMN-JIKANCHK-SAT (2)
               MOVE    ZERO            TO  SPA-NAI-JIKANCHK-WEEK(1)
               MOVE    ZERO            TO  SPA-NAI-JIKANCHK-WEEK(2)
               MOVE    ZERO            TO  SPA-NAI-JIKANCHK-SAT (1)
               MOVE    ZERO            TO  SPA-NAI-JIKANCHK-SAT (2)
           ELSE
      *        時間判定
               PERFORM 41031-JIKANCHK-TIMECHK-SEC
           END-IF
      *
          .
       4103-JIKANCHK-TIME-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間判定処理
      *****************************************************************
       41031-JIKANCHK-TIMECHK-SEC       SECTION.
      *
      *    時間判定
           PERFORM VARYING     IDT     FROM    1   BY  1
                   UNTIL      (IDT     >   2   )   OR
                              (SPA-ERRCD   NOT =   SPACE )
      *
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-JIKANWST-HH(IDT)    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "0031"              TO  SPA-ERRCD
                   MOVE    51                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               ELSE
                   MOVE    SNUM-OUTNUM         TO  SPA-NAI-JIKANWST-HH
                                                                (IDT)
               END-IF
      *
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-JIKANWST-MM(IDT)    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    "0031"          TO  SPA-ERRCD
                       MOVE    52              TO  SPA-GMN-CUR
                       COMPUTE SPA-GMN-CUR     =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
                   END-IF
               ELSE
                   MOVE    SNUM-OUTNUM         TO  SPA-NAI-JIKANWST-MM
                                                                (IDT)
               END-IF
      *
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-JIKANWED-HH(IDT)    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    "0031"          TO  SPA-ERRCD
                       MOVE    53              TO  SPA-GMN-CUR
                       COMPUTE SPA-GMN-CUR     =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
                   END-IF
               ELSE
                   MOVE    SNUM-OUTNUM         TO  SPA-NAI-JIKANWED-HH
                                                                (IDT)
               END-IF
      *
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-JIKANWED-MM(IDT)    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    "0031"          TO  SPA-ERRCD
                       MOVE    54              TO  SPA-GMN-CUR
                       COMPUTE SPA-GMN-CUR     =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
                   END-IF
               ELSE
                   MOVE    SNUM-OUTNUM         TO  SPA-NAI-JIKANWED-MM
                                                                (IDT)
               END-IF
      *
      *
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-JIKANSST-HH(IDT)    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    "0031"          TO  SPA-ERRCD
                       MOVE    59              TO  SPA-GMN-CUR
                       COMPUTE SPA-GMN-CUR     =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
                   END-IF
               ELSE
                   MOVE    SNUM-OUTNUM         TO  SPA-NAI-JIKANSST-HH
                                                                (IDT)
               END-IF
      *
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-JIKANSST-MM(IDT)    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    "0031"          TO  SPA-ERRCD
                       MOVE    60              TO  SPA-GMN-CUR
                       COMPUTE SPA-GMN-CUR     =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
                   END-IF
               ELSE
                   MOVE    SNUM-OUTNUM         TO  SPA-NAI-JIKANSST-MM
                                                                (IDT)
               END-IF
      *
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-JIKANSED-HH(IDT)    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    "0031"          TO  SPA-ERRCD
                       MOVE    61              TO  SPA-GMN-CUR
                       COMPUTE SPA-GMN-CUR     =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
                   END-IF
               ELSE
                   MOVE    SNUM-OUTNUM         TO  SPA-NAI-JIKANSED-HH
                                                                (IDT)
               END-IF
      *
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-JIKANSED-MM(IDT)    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    "0031"          TO  SPA-ERRCD
                       MOVE    62              TO  SPA-GMN-CUR
                       COMPUTE SPA-GMN-CUR     =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
                   END-IF
               ELSE
                   MOVE    SNUM-OUTNUM         TO  SPA-NAI-JIKANSED-MM
                                                                (IDT)
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING     IDT     FROM    1   BY  1
                   UNTIL      (IDT     >   2   )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               MOVE    SPA-NAI-JIKANWST-HH(IDT)
                                      TO  SPA-GMN-JIKANWST-HH(IDT)
               MOVE    SPA-NAI-JIKANWST-MM(IDT)
                                      TO  SPA-GMN-JIKANWST-MM(IDT)
               MOVE    SPA-NAI-JIKANWED-HH(IDT)
                                      TO  SPA-GMN-JIKANWED-HH(IDT)
               MOVE    SPA-NAI-JIKANWED-MM(IDT)
                                      TO  SPA-GMN-JIKANWED-MM(IDT)
               MOVE    SPA-NAI-JIKANSST-HH(IDT)
                                      TO  SPA-GMN-JIKANSST-HH(IDT)
               MOVE    SPA-NAI-JIKANSST-MM(IDT)
                                      TO  SPA-GMN-JIKANSST-MM(IDT)
               MOVE    SPA-NAI-JIKANSED-HH(IDT)
                                      TO  SPA-GMN-JIKANSED-HH(IDT)
               MOVE    SPA-NAI-JIKANSED-MM(IDT)
                                      TO  SPA-GMN-JIKANSED-MM(IDT)
           END-PERFORM
      *
      *    時間判定
           PERFORM VARYING     IDT     FROM    1   BY  1
                   UNTIL      (IDT     >   2   )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-NAI-JIKANWST-HH(IDT)    >   23
                   MOVE    "0031"              TO  SPA-ERRCD
                   MOVE    51                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
               IF      SPA-NAI-JIKANWST-MM(IDT)    >   59
                 AND  (SPA-ERRCD           =   SPACE )
                   MOVE    "0031"              TO  SPA-ERRCD
                   MOVE    52                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
               IF      SPA-NAI-JIKANWED-HH(IDT)    >   23
                 AND  (SPA-ERRCD           =   SPACE )
                   MOVE    "0031"              TO  SPA-ERRCD
                   MOVE    53                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
               IF      SPA-NAI-JIKANWED-MM(IDT)    >   59
                 AND  (SPA-ERRCD           =   SPACE )
                   MOVE    "0031"              TO  SPA-ERRCD
                   MOVE    54                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
               IF      SPA-NAI-JIKANSST-HH(IDT)    >   23
                 AND  (SPA-ERRCD           =   SPACE )
                   MOVE    "0031"              TO  SPA-ERRCD
                   MOVE    59                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
               IF      SPA-NAI-JIKANSST-MM(IDT)    >   59
                 AND  (SPA-ERRCD           =   SPACE )
                   MOVE    "0031"              TO  SPA-ERRCD
                   MOVE    60                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
               IF      SPA-NAI-JIKANSED-HH(IDT)    >   23
                 AND  (SPA-ERRCD           =   SPACE )
                   MOVE    "0031"              TO  SPA-ERRCD
                   MOVE    61                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
               IF      SPA-NAI-JIKANSED-MM(IDT)    >   59
                 AND  (SPA-ERRCD           =   SPACE )
                   MOVE    "0031"              TO  SPA-ERRCD
                   MOVE    62                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDT     FROM    1   BY  1
                   UNTIL      (IDT     >   2   )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF     (SPA-NAI-JIKANCHK-WED(IDT)   >   ZERO  ) AND
                      (SPA-NAI-JIKANCHK-WST(IDT)
                                           >  SPA-NAI-JIKANCHK-WED(IDT))
                   MOVE    "0032"              TO  SPA-ERRCD
                   MOVE    51                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
               IF     (SPA-NAI-JIKANCHK-SED(IDT)   >   ZERO  )  AND
                      (SPA-NAI-JIKANCHK-SST(IDT)
                                           >  SPA-NAI-JIKANCHK-SED(IDT))
                   MOVE    "0032"              TO  SPA-ERRCD
                   MOVE    59                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
           END-PERFORM
      *
          .
       41031-JIKANCHK-TIMECHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括まとめ入力チェック処理
      *****************************************************************
       4103-HOUKATU-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-HOUKATU
                                      =   SPA-GMN-HOUKATU-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-HOUKATU-LIST(IDX)
                                          TO  SPA-GMN-HOUKATU-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    84              TO  SPA-GMN-CUR
               MOVE    "0014"          TO  SPA-ERRCD
           END-IF
      *
          .
       4103-HOUKATU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名表示区分入力チェック処理
      *****************************************************************
       4103-BYOHYOKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-BYOHYOKBN-MAX
               IF      SPA-GMN-BYOHYOKBN
                                      =   SPA-GMN-BYOHYOKBN-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-BYOHYOKBN-LIST(IDX)
                                          TO  SPA-GMN-BYOHYOKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    85              TO  SPA-GMN-CUR
               MOVE    "0038"          TO  SPA-ERRCD
           END-IF
      *
          .
       4103-BYOHYOKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤登録順区分チェック処理
      *****************************************************************
       4103-ZAIJUNKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-ZAIJUNKBN
                                      =   SPA-GMN-ZAIJUNKBN-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-ZAIJUNKBN-LIST(IDX)
                                          TO  SPA-GMN-ZAIJUNKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    86              TO  SPA-GMN-CUR
               MOVE    "0039"          TO  SPA-ERRCD
           END-IF
      *
          .
       4103-ZAIJUNKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    カルテの病名対象チェック処理
      *****************************************************************
       4104-BYOMEI-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               IF      SPA-GMN-BYOMEIKBN
                                      =   SPA-GMN-BYOMEIKBN-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-BYOMEIKBN-LIST(IDX)
                                          TO  SPA-GMN-BYOMEIKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    201             TO  SPA-GMN-CUR
               MOVE    "1001"          TO  SPA-ERRCD
           END-IF
      *
          .
       4104-BYOMEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    人名辞書使用チェック処理
      *****************************************************************
       4105-JISYO-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-JISYOKBN
                                      =   SPA-GMN-JISYOKBN-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-JISYOKBN-LIST(IDX)
                                          TO  SPA-GMN-JISYOKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    202              TO  SPA-GMN-CUR
               MOVE    "1002"          TO  SPA-ERRCD
           END-IF
      *
          .
       4105-JISYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者登録のカルテ発行チェック処理
      *****************************************************************
       4106-HAKKO-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               IF      SPA-GMN-HAKKOKBN
                                      =   SPA-GMN-HAKKOKBN-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-HAKKOKBN-LIST(IDX)
                                          TO  SPA-GMN-HAKKOKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    203              TO  SPA-GMN-CUR
               MOVE    "1003"          TO  SPA-ERRCD
           END-IF
      *
          .
       4106-HAKKO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費保険の補助区分チェック処理
      *****************************************************************
       4107-HOJYO-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-HOJYOKBN
                                       =   SPA-GMN-HOJYOKBN-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-HOJYOKBN-LIST(IDX)
                                          TO  SPA-GMN-HOJYOKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    204             TO  SPA-GMN-CUR
               MOVE    "1004"          TO  SPA-ERRCD
           END-IF
      *
          .
       4107-HOJYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    同姓同名リスト区分チェック処理
      *****************************************************************
       4107-DOUMEI-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-DOUMEI-CHKFLG
                                       =   SPA-GMN-DOUMEI-CHKFLG-LIST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-DOUMEI-CHKFLG-LIST(IDX)
                                          TO  SPA-GMN-DOUMEI-CHKFLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    205             TO  SPA-GMN-CUR
               MOVE    "1005"          TO  SPA-ERRCD
           END-IF
      *
          .
       4107-DOUMEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    同姓同名リスト区分チェック処理
      *****************************************************************
       4107-JYUFUKU-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-JYUFUKU-MAX) OR
                              (FLG-OK  =   1    )
               IF      SPA-GMN-JYUFUKU-CHKKBN
                                       =   SPA-GMN-JYUFUKU-CHKKBN-LIST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-JYUFUKU-CHKKBN-LIST(IDX)
                                          TO  SPA-GMN-JYUFUKU-CHKKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    206             TO  SPA-GMN-CUR
               MOVE    "1006"          TO  SPA-ERRCD
           END-IF
      *
          .
       4107-JYUFUKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    地方公費単独チェック処理
      *****************************************************************
       4107-KOHONLY-CHKFLG-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-KOHONLY-MAX) OR
                              (FLG-OK  =   1    )
               IF      SPA-GMN-KOHONLY-CHKFLG
                                       =   SPA-GMN-KOHONLY-CHKFLG-LIST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-KOHONLY-CHKFLG-LIST(IDX)
                                          TO  SPA-GMN-KOHONLY-CHKFLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    207             TO  SPA-GMN-CUR
               MOVE    "1007"          TO  SPA-ERRCD
           END-IF
      *
          .
       4107-KOHONLY-CHKFLG-EXT.
           EXIT.
      *****************************************************************
      *    患者情報削除機能チェック処理
      *****************************************************************
       4107-PTNUMDEL-FLG-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-PTNUMDEL-MAX) OR
                              (FLG-OK  =   1    )
               IF      SPA-GMN-PTNUMDEL-FLG
                                       =   SPA-GMN-PTNUMDEL-FLG-LIST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-PTNUMDEL-FLG-LIST(IDX)
                                          TO  SPA-GMN-PTNUMDEL-FLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    208             TO  SPA-GMN-CUR
               MOVE    "1008"          TO  SPA-ERRCD
           END-IF
      *
          .
       4107-PTNUMDEL-FLG-EXT.
           EXIT.
      *****************************************************************
      *    本人・家族区分チェック処理
      *****************************************************************
       4107-HONKZKKBN-FLG-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-HONKZKKBN-MAX) OR
                              (FLG-OK  =   1    )
               IF      SPA-GMN-HONKZKKBN-FLG
                                       =   SPA-GMN-HONKZKKBN-FLG-LIST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-HONKZKKBN-FLG-LIST(IDX)
                                          TO  SPA-GMN-HONKZKKBN-FLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    209             TO  SPA-GMN-CUR
               MOVE    "1009"          TO  SPA-ERRCD
           END-IF
      *
          .
       4107-HONKZKKBN-FLG-EXT.
           EXIT.
      *****************************************************************
      *    家族年齢設定チェック処理
      *****************************************************************
       4107-KZKNEN-CHK-SEC          SECTION.
      *
           IF      SPA-GMN-KZKNEN      =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-KZKNEN      TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    210                 TO  SPA-GMN-CUR
                   MOVE    "1010"              TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-KZKNEN
                   IF      WRK-KZKNEN          =   ZERO
                       MOVE    SPACE               TO  SPA-GMN-KZKNEN
                   ELSE
                       MOVE    WRK-KZKNEN          TO  SPA-GMN-KZKNEN
                   END-IF
               END-IF
           END-IF
      *
          .
       4107-KZKNEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    被保険者名設定区分チェック処理
      *****************************************************************
       4107-HIHKNJAKBN-FLG-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-HIHKNJAKBN-MAX) OR
                              (FLG-OK  =   1    )
               IF      SPA-GMN-HIHKNJAKBN-FLG
                                       =   SPA-GMN-HIHKNJAKBN-FLG-LIST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-HIHKNJAKBN-FLG-LIST(IDX)
                                          TO  SPA-GMN-HIHKNJAKBN-FLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    211             TO  SPA-GMN-CUR
               MOVE    "1011"          TO  SPA-ERRCD
           END-IF
      *
          .
       4107-HIHKNJAKBN-FLG-EXT.
           EXIT.
      *****************************************************************
      *    世帯主名自動記載区分チェック処理
      *****************************************************************
       4107-SETAIKBN-FLG-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-SETAIKBN-MAX) OR
                              (FLG-OK  =   1    )
               IF      SPA-GMN-SETAIKBN-FLG
                                       =   SPA-GMN-SETAIKBN-FLG-LIST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-SETAIKBN-FLG-LIST(IDX)
                                          TO  SPA-GMN-SETAIKBN-FLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    212             TO  SPA-GMN-CUR
               MOVE    "1012"          TO  SPA-ERRCD
           END-IF
      *
          .
       4107-SETAIKBN-FLG-EXT.
           EXIT.
      *****************************************************************
      *    グループ医療機関患者リストチェック処理
      *****************************************************************
       4107-GRPPTLST-FLG-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-GRPPTLST-MAX) OR
                              (FLG-OK  =   1    )
               IF      SPA-GMN-GRPPTLST-FLG
                                       =   SPA-GMN-GRPPTLST-FLG-LIST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-GRPPTLST-FLG-LIST(IDX)
                                          TO  SPA-GMN-GRPPTLST-FLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    213             TO  SPA-GMN-CUR
               MOVE    "1013"          TO  SPA-ERRCD
           END-IF
      *
          .
       4107-GRPPTLST-FLG-EXT.
           EXIT.
      *****************************************************************
      *    公費上限額履歴チェック区分チェック処理
      *****************************************************************
       4107-KOHRRKKBN-FLG-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-KOHRRKKBN-MAX) OR
                              (FLG-OK  =   1    )
               IF      SPA-GMN-KOHRRKKBN-FLG
                                       =   SPA-GMN-KOHRRKKBN-FLG-LIST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-KOHRRKKBN-FLG-LIST(IDX)
                                          TO  SPA-GMN-KOHRRKKBN-FLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    214             TO  SPA-GMN-CUR
               MOVE    "1014"          TO  SPA-ERRCD
           END-IF
      *
          .
       4107-KOHRRKKBN-FLG-EXT.
           EXIT.
      *****************************************************************
      *    郵便番号自動記載区分チェック処理
      *****************************************************************
       4107-POSTKBN-FLG-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-POSTKBN-MAX) OR
                              (FLG-OK  =   1    )
               IF      SPA-GMN-POSTKBN-FLG
                                       =   SPA-GMN-POSTKBN-FLG-LIST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-POSTKBN-FLG-LIST(IDX)
                                          TO  SPA-GMN-POSTKBN-FLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    215             TO  SPA-GMN-CUR
               MOVE    "1015"          TO  SPA-ERRCD
           END-IF
      *
          .
       4107-POSTKBN-FLG-EXT.
           EXIT.
      *****************************************************************
      *    都道府県名記載区分チェック処理
      *****************************************************************
       4107-KENMEIKBN-FLG-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-KENMEIKBN-MAX) OR
                              (FLG-OK  =   1    )
               IF      SPA-GMN-KENMEIKBN-FLG
                                       =   SPA-GMN-KENMEIKBN-FLG-LIST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-KENMEIKBN-FLG-LIST(IDX)
                                          TO  SPA-GMN-KENMEIKBN-FLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    216             TO  SPA-GMN-CUR
               MOVE    "1016"          TO  SPA-ERRCD
           END-IF
      *
          .
       4107-KENMEIKBN-FLG-EXT.
           EXIT.
      *****************************************************************
      *    京都府通り名称編集区分チェック処理
      *****************************************************************
       4107-TOWNNAME2KBN-FLG-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-TOWNNAME2KBN-MAX) OR
                              (FLG-OK  =   1    )
               IF      SPA-GMN-TOWNNAME2KBN
                                       =   SPA-GMN-TOWNNAME2KBN-LIST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-TOWNNAME2KBN-LIST(IDX)
                                          TO  SPA-GMN-TOWNNAME2KBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    217             TO  SPA-GMN-CUR
               MOVE    "1017"          TO  SPA-ERRCD
           END-IF
      *
          .
       4107-TOWNNAME2KBN-FLG-EXT.
           EXIT.
      *****************************************************************
      *    公費法別一覧表示区分チェック処理
      *****************************************************************
       4107-KOHHBTKBN-FLG-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-KOHHBTKBN-MAX) OR
                              (FLG-OK  =   1    )
               IF      SPA-GMN-KOHHBTKBN
                                       =   SPA-GMN-KOHHBTKBN-LIST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-KOHHBTKBN-LIST(IDX)
                                          TO  SPA-GMN-KOHHBTKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    218             TO  SPA-GMN-CUR
               MOVE    "1018"          TO  SPA-ERRCD
           END-IF
      *
          .
       4107-KOHHBTKBN-FLG-EXT.
           EXIT.
      *****************************************************************
      *    公費法別一覧表示区分チェック処理
      *****************************************************************
       4107-HKNINITKBN-FLG-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-HKNINITKBN-MAX) OR
                              (FLG-OK  =   1    )
               IF      SPA-GMN-HKNINITKBN
                                       =   SPA-GMN-HKNINITKBN-LIST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-HKNINITKBN-LIST(IDX)
                                          TO  SPA-GMN-HKNINITKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    219             TO  SPA-GMN-CUR
               MOVE    "1019"          TO  SPA-ERRCD
           END-IF
      *
          .
       4107-HKNINITKBN-FLG-EXT.
           EXIT.
      *****************************************************************
      *    入金管理区分チェック処理（診療行為機能情報）
      *****************************************************************
       4108-SRYNYKNKNRKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYNYKNKNRKBN-MAX
               IF      SPA-GMN-SRYNYKNKNRKBN
                                       =   SPA-GMN-SRYNYKNKNRKBNL (IDX)
                   MOVE    SPA-GMN-SRYNYKNKNRKBNLST(IDX)
                                          TO  SPA-GMN-SRYNYKNKNRKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    320             TO  SPA-GMN-CUR
               MOVE    "2001"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-SRYNYKNKNRKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求書発行方法チェック処理（診療行為機能情報）
      *****************************************************************
       4108-KAGRPFLG-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-KAGRPFLG-MAX
               IF      SPA-GMN-KAGRPFLG
                                       =   SPA-GMN-KAGRPFLGL (IDX)
                   MOVE    SPA-GMN-KAGRPFLG-LST(IDX)
                                          TO  SPA-GMN-KAGRPFLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    321             TO  SPA-GMN-CUR
               MOVE    "2002"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-KAGRPFLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求書発行方法チェック処理（診療行為機能情報）
      *****************************************************************
       4108-HAKHOUFLG-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-HAKHOUFLG-MAX
               IF      SPA-GMN-HAKHOUFLG
                                       =   SPA-GMN-HAKHOUFLGL (IDX)
                   MOVE    SPA-GMN-HAKHOUFLG-LST(IDX)
                                          TO  SPA-GMN-HAKHOUFLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    322             TO  SPA-GMN-CUR
               MOVE    "2003"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-HAKHOUFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    訂正請求書金額選択チェック処理（診療行為機能情報）
      *****************************************************************
       4108-TEIKINKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TEIKINKBN-MAX
               IF      SPA-GMN-TEIKINKBN
                                       =   SPA-GMN-TEIKINKBNL (IDX)
                   MOVE    SPA-GMN-TEIKINKBN-LST(IDX)
                                          TO  SPA-GMN-TEIKINKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    323             TO  SPA-GMN-CUR
               MOVE    "2009"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-TEIKINKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報提供発行チェック処理（診療行為機能情報）
      *****************************************************************
       4108-YKJCHKKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-YKJCHKKBN-MAX
               IF      SPA-GMN-YKJCHKKBN
                                       =   SPA-GMN-YKJCHKKBNL(IDX)
                   MOVE    SPA-GMN-YKJCHKKBN-LST(IDX)
                                          TO  SPA-GMN-YKJCHKKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    324             TO  SPA-GMN-CUR
               MOVE    "2010"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-YKJCHKKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ＤＯ初期表示チェック処理（診療行為機能情報）
      *****************************************************************
       4108-DOSRYKBNFLG-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-DOSRYKBNFLG-MAX
               IF      SPA-GMN-DOSRYKBNFLG
                                       =   SPA-GMN-DOSRYKBNFLG-LST (IDX)
                                                          (1:2)
                   MOVE    SPA-GMN-DOSRYKBNFLG-LST(IDX)
                                          TO  SPA-GMN-DOSRYKBNFLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    301             TO  SPA-GMN-CUR
               MOVE    "2004"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-DOSRYKBNFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    中途表示一覧チェック処理（診療行為機能情報）
      *****************************************************************
       4108-WKSRYHYOFLG-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-WKSRYHYOFLG-MAX
               IF      SPA-GMN-WKSRYHYOFLG
                                       =   SPA-GMN-WKSRYHYOFLG-LST (IDX)
                                                          (1:1)
                   MOVE    SPA-GMN-WKSRYHYOFLG-LST(IDX)
                                          TO  SPA-GMN-WKSRYHYOFLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    302             TO  SPA-GMN-CUR
               MOVE    "2005"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-WKSRYHYOFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検査正式名称表示チェック処理（診療行為機能情報）
      *****************************************************************
       4108-FORMALFLG-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-FORMALFLG-MAX
               IF      SPA-GMN-FORMALFLG
                                       =   SPA-GMN-FORMALFLG-LST (IDX)
                                                          (1:1)
                   MOVE    SPA-GMN-FORMALFLG-LST(IDX)
                                          TO  SPA-GMN-FORMALFLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    303             TO  SPA-GMN-CUR
               MOVE    "2006"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-FORMALFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    腫瘍マーカ一覧表示チェック処理（診療行為機能情報）
      *****************************************************************
       4108-AKUSEIHYO-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-AKUSEIHYO-MAX
               IF      SPA-GMN-AKUSEIHYO
                                       =   SPA-GMN-AKUSEIHYO-LST (IDX)
                                                          (1:1)
                   MOVE    SPA-GMN-AKUSEIHYO-LST(IDX)
                                          TO  SPA-GMN-AKUSEIHYO-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    304             TO  SPA-GMN-CUR
               MOVE    "2011"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-AKUSEIHYO-CHK-EXT.
           EXIT.
      *H30.9
      *****************************************************************
      *    選択式コメント一覧区分チェック処理（診療行為機能情報）
      *****************************************************************
       4108-RECEKISAIFLG-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-RECEKISAIFLG-MAX
               IF      SPA-GMN-RECEKISAIFLG
                                       =   SPA-GMN-RECEKISAIFLG-LST
                                                     (IDX) (1:1)
                   MOVE    SPA-GMN-RECEKISAIFLG-LST(IDX)
                                          TO  SPA-GMN-RECEKISAIFLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    315             TO  SPA-GMN-CUR
               MOVE    "2030"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-RECEKISAIFLG-CHK-EXT.
           EXIT.
      *H24.4
      *****************************************************************
      *    確認画面名称表示区分チェック処理（診療行為機能情報）
      *****************************************************************
       4108-GENERICFLG-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-GENERICFLG-MAX
               IF      SPA-GMN-GENERICFLG
                                       =   SPA-GMN-GENERICFLG-LST (IDX)
                                                          (1:1)
                   MOVE    SPA-GMN-GENERICFLG-LST(IDX)
                                          TO  SPA-GMN-GENERICFLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    311             TO  SPA-GMN-CUR
               MOVE    "2027"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-GENERICFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    同日再診・訂正選択チェック処理（診療行為機能情報）
      *****************************************************************
       4108-TEISEIFLG-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-TEISEIFLG-MAX)
                       OR     (FLG-OK  =   1    ) 
               IF      SPA-GMN-TEISEIFLG
                                       =   SPA-GMN-TEISEIFLG-LST (IDX)
                                                          (1:1)
                   MOVE    SPA-GMN-TEISEIFLG-LST(IDX)
                                          TO  SPA-GMN-TEISEIFLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    305             TO  SPA-GMN-CUR
               MOVE    "2007"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-TEISEIFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    皮下筋肉注射料変換チェック処理（診療行為機能情報）
      *****************************************************************
       4108-CHG310FLG-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-CHG310FLG-MAX)
                       OR     (FLG-OK  =   1    ) 
               IF      SPA-GMN-CHG310FLG
                                       =   SPA-GMN-CHG310FLG-LST (IDX)
                                                          (1:1)
                   MOVE    SPA-GMN-CHG310FLG-LST(IDX)
                                          TO  SPA-GMN-CHG310FLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    306             TO  SPA-GMN-CUR
               MOVE    "2008"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-CHG310FLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    投薬診療種別変換選択チェック処理（診療行為機能情報）
      *****************************************************************
       4108-CHGSRYSYU-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-CHGSRYSYU-MAX)
                       OR     (FLG-OK  =   1    ) 
               IF      SPA-GMN-CHGSRYSYU
                                       =   SPA-GMN-CHGSRYSYU-LST (IDX)
                                                          (1:1)
                   MOVE    SPA-GMN-CHGSRYSYU-LST(IDX)
                                          TO  SPA-GMN-CHGSRYSYU-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    307             TO  SPA-GMN-CUR
               MOVE    "2012"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-CHGSRYSYU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    レセプトブレビューチェック処理（診療行為機能情報）
      *****************************************************************
       4108-RESPRV038-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-RESPRV038-MAX)
                       OR     (FLG-OK  =   1    ) 
               IF      SPA-GMN-RESPRV038
                                       =   SPA-GMN-RESPRV038-LST (IDX)
                                                          (1:1)
                   MOVE    SPA-GMN-RESPRV038-LST(IDX)
                                          TO  SPA-GMN-RESPRV038-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    308             TO  SPA-GMN-CUR
               MOVE    "2015"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-RESPRV038-CHK-EXT.
           EXIT.
      *****************************************************************
      *    残量廃棄区分チェック処理（診療行為機能情報）
      *****************************************************************
       4108-HAIKIKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-HAIKIKBN-MAX)
                       OR     (FLG-OK  =   1    ) 
               IF      SPA-GMN-HAIKIKBN
                                       =   SPA-GMN-HAIKIKBN-LST (IDX)
                                                          (1:1)
                   MOVE    SPA-GMN-HAIKIKBN-LST(IDX)
                                          TO  SPA-GMN-HAIKIKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    309             TO  SPA-GMN-CUR
               MOVE    "2017"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-HAIKIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    精神療法２０未満加算区分チェック処理（診療行為機能情報）
      *****************************************************************
       4108-SEI20KSNKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-SEI20KSNKBN-MAX)
                       OR     (FLG-OK  =   1    ) 
               IF      SPA-GMN-SEI20KSNKBN
                                       =   SPA-GMN-SEI20KSNKBN-LST (IDX)
                                                          (1:1)
                   MOVE    SPA-GMN-SEI20KSNKBN-LST(IDX)
                                          TO  SPA-GMN-SEI20KSNKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    310             TO  SPA-GMN-CUR
               MOVE    "2020"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-SEI20KSNKBN-CHK-EXT.
           EXIT.
      *H29.7
      *****************************************************************
      *    心身療法２０未満加算区分チェック処理（診療行為機能情報）
      *****************************************************************
       4108-SIN20KSNKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-SIN20KSNKBN-MAX)
                       OR     (FLG-OK  =   1    ) 
               IF      SPA-GMN-SIN20KSNKBN
                                       =   SPA-GMN-SIN20KSNKBN-LST (IDX)
                                                          (1:1)
                   MOVE    SPA-GMN-SIN20KSNKBN-LST(IDX)
                                          TO  SPA-GMN-SIN20KSNKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    314             TO  SPA-GMN-CUR
               MOVE    "2030"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-SIN20KSNKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    レセプトブレビューレイアウトチェック処理（診療行為機能情報）
      *****************************************************************
       4108-PRVLAYOUT038-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-PRVLAYOUT038-MAX)
                       OR     (FLG-OK  =   1    ) 
               IF      SPA-GMN-PRVLAYOUT038
                                       =   SPA-GMN-PRVLAYOUT038-LST
                                                      (IDX)(1:1)
                   MOVE    SPA-GMN-PRVLAYOUT038-LST(IDX)
                                          TO  SPA-GMN-PRVLAYOUT038-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    312             TO  SPA-GMN-CUR
               MOVE    "2028"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-PRVLAYOUT038-CHK-EXT.
           EXIT.
      *H28.6
      *****************************************************************
      *    セット展開表示チェック処理（診療行為機能情報）
      *****************************************************************
       4108-SETALLKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-SETALLKBN-MAX)
                       OR     (FLG-OK  =   1    ) 
               IF      SPA-GMN-SETALLKBN
                                       =   SPA-GMN-SETALLKBN-LST
                                                      (IDX)(1:1)
                   MOVE    SPA-GMN-SETALLKBN-LST(IDX)
                                          TO  SPA-GMN-SETALLKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    313             TO  SPA-GMN-CUR
               MOVE    "2029"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-SETALLKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    リハビリ・患者情報表示区分チェック処理（診療行為機能情報）
      *****************************************************************
       4108-GMNHYOKBN-CHK-SEC      SECTION.
      *
      *    空白を詰める
      *H28.10  ５→６へ
           PERFORM VARYING     IDX     FROM    1   BY  1
      ****         UNTIL      (IDX             >=  5    )
                   UNTIL      (IDX             >=  6    )
               IF     (SPA-GMN-GMNHYOKBN-G (IDX)   =   SPACE )
                   PERFORM VARYING     IDY     FROM    IDX BY  1
                           UNTIL      (IDY             >   6    )
                       IF     (IDY             NOT =   IDX )
                       AND    (SPA-GMN-GMNHYOKBN-G (IDY)
                                               NOT =   SPACE )
                           MOVE    SPA-GMN-GMNHYOKBN-G (IDY)
                                               TO  SPA-GMN-GMNHYOKBN-G
                                                              (IDX)
                           MOVE    SPACE       TO  SPA-GMN-GMNHYOKBN-G
                                                              (IDY)
                           MOVE    6           TO  IDY
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   6    )
                       OR     (SPA-ERRCD   NOT =   SPACE )
               MOVE    ZERO                TO  FLG-OK
               IF      SPA-GMN-GMNHYOKBN-G (IDX)   =   SPACE
                   MOVE    "0"             TO  SPA-NAI-GMNHYOKBN (IDX)
                   MOVE    1               TO  FLG-OK
               END-IF
               PERFORM VARYING IDY         FROM    1   BY  1
                       UNTIL  (IDY         >   SPA-GMN-GMNHYOKBN-MAX)
                           OR (FLG-OK      =   1    )
                   IF      SPA-GMN-GMNHYOKBN-G (IDX)
                                           =   SPA-GMN-GMNHYOKBN-LST
                                                             (IDY)
                       MOVE    SPA-GMN-GMNHYOKBNL (IDY)
                                           TO  SPA-NAI-GMNHYOKBN (IDX)
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   EVALUATE    IDX
                   WHEN    1
                       MOVE    330             TO  SPA-GMN-CUR
                   WHEN    2
                       MOVE    331             TO  SPA-GMN-CUR
                   WHEN    3
                       MOVE    332             TO  SPA-GMN-CUR
                   WHEN    4
                       MOVE    333             TO  SPA-GMN-CUR
                   WHEN    5
                       MOVE    334             TO  SPA-GMN-CUR
      *H28.10
                   WHEN    6
                       MOVE    349             TO  SPA-GMN-CUR
                   END-EVALUATE
                   MOVE    "2023"          TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *
           .
       4108-GMNHYOKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力スクロール背景色チェック処理（診療行為機能情報）
      *****************************************************************
       4108-BGCOLORS-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-BGCOLORS        =   SPACE
                                            OR  "#000000"
      *        空白は初期表示
               MOVE    "#FAEBD7"           TO  SPA-GMN-BGCOLORS
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    16                  TO  KANACHK-MAX-CNT
               MOVE    SPA-GMN-BGCOLORS    TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC          =   ZERO  )  AND
                      (KANACHK-SYUBETU     =   1     )
      *            半角以外はエラー
                   CONTINUE
               ELSE
                   MOVE    345             TO  SPA-GMN-CUR
                   MOVE    "2024"          TO  SPA-ERRCD
               END-IF
           END-IF
           .
       4108-BGCOLORS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力スクロール背景行区分チェック処理（診療行為機能情報）
      *****************************************************************
       4108-BGLINEKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-BGLINEKBN-MAX
               IF      SPA-GMN-BGLINEKBN
                                       =   SPA-GMN-BGLINEKBN-LST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-BGLINEKBN-LST(IDX)
                                          TO  SPA-GMN-BGLINEKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    346             TO  SPA-GMN-CUR
               MOVE    "2025"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-BGLINEKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    エラー背景色チェック処理（診療行為機能情報）
      *****************************************************************
       4108-ERRCOLORS-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-ERRCOLORS        =   SPACE
                                            OR  "#000000"
      *        空白は初期表示
               MOVE    "#FFB5C5"            TO  SPA-GMN-ERRCOLORS
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    16                  TO  KANACHK-MAX-CNT
               MOVE    SPA-GMN-ERRCOLORS   TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC          =   ZERO  )  AND
                      (KANACHK-SYUBETU     =   1     )
      *            半角以外はエラー
                   CONTINUE
               ELSE
                   MOVE    347             TO  SPA-GMN-CUR
                   MOVE    "2026"          TO  SPA-ERRCD
               END-IF
           END-IF
           .
       4108-ERRCOLORS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    エラー背景色チェック処理（診療行為機能情報）
      *****************************************************************
       4108-KEICOLORS-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-KEICOLORS        =   SPACE
                                            OR  "#000000"
      *        空白は初期表示
               MOVE    "#FFB5C5"            TO  SPA-GMN-KEICOLORS
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    16                  TO  KANACHK-MAX-CNT
               MOVE    SPA-GMN-KEICOLORS   TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC          =   ZERO  )  AND
                      (KANACHK-SYUBETU     =   1     )
      *            半角以外はエラー
                   CONTINUE
               ELSE
                   MOVE    348             TO  SPA-GMN-CUR
                   MOVE    "2026"          TO  SPA-ERRCD
               END-IF
           END-IF
           .
       4108-KEICOLORS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬情・お薬手帳対象チェック処理（診療行為機能情報）
      *****************************************************************
       4108-OKUSURIKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-OKUSURIKBN-MAX
               IF      SPA-GMN-OKUSURIKBN
                                       =   SPA-GMN-OKUSURIKBN-LST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-OKUSURIKBN-LST(IDX)
                                          TO  SPA-GMN-OKUSURIKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    325             TO  SPA-GMN-CUR
               MOVE    "2014"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-OKUSURIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    お薬手帳発行チェック処理（診療行為機能情報）
      *****************************************************************
       4108-YKJOKUKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-YKJOKUKBN-MAX
               IF      SPA-GMN-YKJOKUKBN
                                       =   SPA-GMN-YKJOKUKBN-LST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-YKJOKUKBN-LST(IDX)
                                          TO  SPA-GMN-YKJOKUKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    326             TO  SPA-GMN-CUR
               MOVE    "2016"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-YKJOKUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金・返金額設定区分チェック処理（診療行為機能情報）
      *****************************************************************
       4108-NYKHNKKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-NYKHNKKBN-MAX
               IF      SPA-GMN-NYKHNKKBN
                                       =   SPA-GMN-NYKHNKKBN-LST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-NYKHNKKBN-LST(IDX)
                                           TO  SPA-GMN-NYKHNKKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    327             TO  SPA-GMN-CUR
               MOVE    "2018"          TO  SPA-ERRCD
           END-IF
      *
           IF     SPA-ERRCD            =   SPACE
      *        今回請求分のみ入金の時、設定なし
               IF     (SPA-GMN-SRYNYKNKNRKBN   =   "1"  )
                AND   (SPA-GMN-NYKHNKKBN   NOT =   "0"  )
                   MOVE    327             TO  SPA-GMN-CUR
                   MOVE    "2019"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4108-NYKHNKKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    発行日変更区分チェック処理（診療行為機能情報）
      *****************************************************************
       4108-YMDHENKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-YMDHENKBN-MAX
               IF      SPA-GMN-YMDHENKBN
                                       =   SPA-GMN-YMDHENKBN-LST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-YMDHENKBN-LST(IDX)
                                           TO  SPA-GMN-YMDHENKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    328             TO  SPA-GMN-CUR
               MOVE    "2021"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-YMDHENKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    環境設定背景色標準チェック処理（診療行為機能情報）
      *****************************************************************
       4108-BGCOLORSJ0-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-BGCOLORSJ0      =   "#000000"
                                           OR  SPACE
      *        クロは背景色なし
               MOVE    "#000000"           TO  SPA-GMN-BGCOLORSJ0
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    16                  TO  KANACHK-MAX-CNT
               MOVE    SPA-GMN-BGCOLORSJ0  TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC          =   ZERO  )  AND
                      (KANACHK-SYUBETU     =   1     )
      *            半角以外はエラー
                   CONTINUE
               ELSE
                   MOVE    335             TO  SPA-GMN-CUR
                   MOVE    "2026"          TO  SPA-ERRCD
               END-IF
           END-IF
           .
       4108-BGCOLORSJ0-CHK-EXT.
           EXIT.
      *****************************************************************
      *    環境設定背景色（日付変更）チェック処理（診療行為機能情報）
      *****************************************************************
       4108-BGCOLORSDAY-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-BGCOLORSDAY =   "#000000"
                                           OR  SPACE
      *        クロは背景色なし
               MOVE    "#000000"           TO  SPA-GMN-BGCOLORSDAY
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    16                  TO  KANACHK-MAX-CNT
               MOVE    SPA-GMN-BGCOLORSDAY TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC          =   ZERO  )  AND
                      (KANACHK-SYUBETU     =   1     )
      *            半角以外はエラー
                   CONTINUE
               ELSE
                   MOVE    336             TO  SPA-GMN-CUR
                   MOVE    "2026"          TO  SPA-ERRCD
               END-IF
           END-IF
           .
       4108-BGCOLORSDAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    環境設定背景色（時間外）チェック処理（診療行為機能情報）
      *****************************************************************
       4108-BGCOLORJG-CHK-SEC      SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   8   )
                          OR  (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-BGCOLORSJ (IDX) =   "#000000"
                                               OR  SPACE
      *             クロは背景色なし
                   MOVE    "#000000"           TO  SPA-GMN-BGCOLORSJ
                                                            (IDX)
               ELSE
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    16                  TO  KANACHK-MAX-CNT
                   MOVE    SPA-GMN-BGCOLORSJ (IDX)
                                               TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   IF     (KANACHK-RC          =   ZERO  )  AND
                          (KANACHK-SYUBETU     =   1     )
      *                半角以外はエラー
                       CONTINUE
                   ELSE
                       EVALUATE    IDX
                       WHEN    1
                           MOVE    337             TO  SPA-GMN-CUR
                       WHEN    2
                           MOVE    338             TO  SPA-GMN-CUR
                       WHEN    3
                           MOVE    339             TO  SPA-GMN-CUR
                       WHEN    4
                           MOVE    340             TO  SPA-GMN-CUR
                       WHEN    5
                           MOVE    341             TO  SPA-GMN-CUR
                       WHEN    6
                           MOVE    342             TO  SPA-GMN-CUR
                       WHEN    7
                           MOVE    343             TO  SPA-GMN-CUR
                       WHEN    8
                           MOVE    344             TO  SPA-GMN-CUR
                       END-EVALUATE
                       MOVE    "2026"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
           .
       4108-BGCOLORJG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金管理区分チェック処理（収納行為機能情報）
      *****************************************************************
       4109-SYUNYKNKNRKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYUNYKNKNRKBN-MAX
               IF      SPA-GMN-SYUNYKNKNRKBN
                                       =   SPA-GMN-SYUNYKNKNRKBNL (IDX)
                   MOVE    SPA-GMN-SYUNYKNKNRKBNLST(IDX)
                                          TO  SPA-GMN-SYUNYKNKNRKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SYUNYKNKNRKBNLST (1)
                                       TO  SPA-GMN-SYUNYKNKNRKBN-G
           END-IF
      *
           .
       4109-SYUNYKNKNRKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療年月指定区分チェック処理（収納行為機能情報）
      *****************************************************************
       4109-IKTSRYYMKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-IKTSRYYMKBN-MAX
               IF      SPA-GMN-IKTSRYYMKBN
                                       =   SPA-GMN-IKTSRYYMKBNL (IDX)
                   MOVE    SPA-GMN-IKTSRYYMKBNLST(IDX)
                                          TO  SPA-GMN-IKTSRYYMKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-IKTSRYYMKBNLST (1)
                                       TO  SPA-GMN-IKTSRYYMKBN-G
           END-IF
      *
           .
       4109-IKTSRYYMKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一括入金画面請求書兼領収書発行区分チェック処理（収納行為機能情報）
      *****************************************************************
       4109-S06SKYPRT-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S06SKYPRT-MAX
               IF      SPA-GMN-S06SKYPRT
                                       =   SPA-GMN-S06SKYPRTL (IDX)
                   MOVE    SPA-GMN-S06SKYPRTLST(IDX)
                                          TO  SPA-GMN-S06SKYPRT-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-S06SKYPRTLST (1)
                                       TO  SPA-GMN-S06SKYPRT-G
           END-IF
      *
           .
       4109-S06SKYPRT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一括入金画面請求書兼領収書発行区分チェック処理（収納行為機能情報）
      *****************************************************************
       4109-S06SKYSUM-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S06SKYSUM-MAX
               IF      SPA-GMN-S06SKYSUM
                                       =   SPA-GMN-S06SKYSUML (IDX)
                   MOVE    SPA-GMN-S06SKYSUMLST(IDX)
                                          TO  SPA-GMN-S06SKYSUM-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-S06SKYSUMLST (1)
                                       TO  SPA-GMN-S06SKYSUM-G
           END-IF
      *
           .
       4109-S06SKYSUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求一覧画面請求書兼領収書再発行方法チェック処理（収納行為機能情報）
      *****************************************************************
       4109-S02SKYSAI-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S02SKYSAI-MAX
               IF      SPA-GMN-S02SKYSAI
                                       =   SPA-GMN-S02SKYSAIL (IDX)
                   MOVE    SPA-GMN-S02SKYSAILST(IDX)
                                          TO  SPA-GMN-S02SKYSAI-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-S02SKYSAILST (1)
                                       TO  SPA-GMN-S02SKYSAI-G
           END-IF
      *
           .
       4109-S02SKYSAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書前回請求額印字区分チェック
      *****************************************************************
       4110-S02SKYMISYUPRT-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S02SKYMISYUPRT-MAX
               IF      SPA-GMN-S02SKYMISYUPRT
                                       =   SPA-GMN-S02SKYMISYUPRTL (IDX)
                   MOVE    SPA-GMN-S02SKYMISYUPRTLST(IDX)
                                          TO  SPA-GMN-S02SKYMISYUPRT-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-S02SKYMISYUPRTLST(2)
                                          TO  SPA-GMN-S02SKYMISYUPRT-G
           END-IF
      *
           .
       4110-S02SKYMISYUPRT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    月別請求書兼領収書発行方法チェック
      *****************************************************************
       4110-TUKIGRP-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TUKIGRP-MAX
               IF      SPA-GMN-TUKIGRP
                                       =   SPA-GMN-TUKIGRPL(IDX)
                   MOVE    SPA-GMN-TUKIGRPLST(IDX)
                                          TO  SPA-GMN-TUKIGRP-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-TUKIGRPLST(4)
                                          TO  SPA-GMN-TUKIGRP-G
           END-IF
      *
           .
       4110-TUKIGRP-CHK-EXT.
           EXIT.
      *****************************************************************
      *    月別請求書兼領収書前回請求額印字区分チェック
      *****************************************************************
       4110-TUKISKYMISYUPRT-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TUKISKYMISYUPRT-MAX
               IF      SPA-GMN-TUKISKYMISYUPRT
                                       =   SPA-GMN-TUKISKYMISYUPRTL(IDX)
                   MOVE    SPA-GMN-TUKISKYMISYUPRTLST(IDX)
                                          TO  SPA-GMN-TUKISKYMISYUPRT-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-TUKISKYMISYUPRTLST(2)
                                          TO  SPA-GMN-TUKISKYMISYUPRT-G
           END-IF
      *
           .
       4110-TUKISKYMISYUPRT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    月別請求書兼領収書集計区分チェック
      *****************************************************************
       4110-TUKISUM-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TUKISUM-MAX
               IF      SPA-GMN-TUKISUM
                                       =   SPA-GMN-TUKISUML(IDX)
                   MOVE    SPA-GMN-TUKISUMLST(IDX)
                                          TO  SPA-GMN-TUKISUM-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-TUKISUMLST(4)
                                          TO  SPA-GMN-TUKISUM-G
           END-IF
      *
           .
       4110-TUKISUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求一覧画コラムリストチェック
      *****************************************************************
       4110-S02MODE-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S02MODE-MAX
               IF      SPA-GMN-S02MODE
                                       =   SPA-GMN-S02MODEL(IDX)
                   MOVE    SPA-GMN-S02MODELST(IDX)
                                          TO  SPA-GMN-S02MODE-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-S02MODELST(1)
                                          TO  SPA-GMN-S02MODE-G
           END-IF
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-S02MODE-MAE =   SPACE )
           WHEN  ( SPA-GMN-S02MODE NOT =   SPA-NAI-S02MODE-MAE )
               PERFORM 3101-S02SORTKBNLST-SEC
               MOVE    SPA-GMN-S02MODE TO  SPA-NAI-S02MODE-MAE
           END-EVALUATE
      *
           .
       4110-S02MODE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求一覧画コラムリスト件数チェック
      *****************************************************************
       4110-S02LIMIT-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-S02LIMIT    >=   ZERO)
            AND  ( SPA-GMN-S02LIMIT    <=   500 )
               CONTINUE
           ELSE
               MOVE    ZERO            TO   SPA-GMN-S02LIMIT
           END-IF
      *
           .
       4110-S02LIMIT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求一覧画面並び順区分チェック
      *****************************************************************
       4110-S02SORTKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S02SORTKBN-MAX
               IF      SPA-GMN-S02SORTKBN
                                       =   SPA-GMN-S02SORTKBNL(IDX)
                   MOVE    SPA-GMN-S02SORTKBNLST(IDX)
                                          TO  SPA-GMN-S02SORTKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-S02SORTKBNLST(1)
                                          TO  SPA-GMN-S02SORTKBN-G
           END-IF
      *
           .
       4110-S02SORTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    再計算該当者チェックチェック
      *****************************************************************
       4110-SAIKEISANSKYCHK-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SAIKEISANSKYCHK-MAX
               IF      SPA-GMN-SAIKEISANSKYCHK
                                       =   SPA-GMN-SAIKEISANSKYCHKL(IDX)
                   MOVE    SPA-GMN-SAIKEISANSKYCHKLST(IDX)
                                          TO  SPA-GMN-SAIKEISANSKYCHK-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SAIKEISANSKYCHKLST(2)
                                          TO  SPA-GMN-SAIKEISANSKYCHK-G
           END-IF
      *
           .
       4110-SAIKEISANSKYCHK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一括計算順区分チェック
      *****************************************************************
       4110-IKTSORTKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-IKTSORTKBN-MAX
               IF      SPA-GMN-IKTSORTKBN
                                       =   SPA-GMN-IKTSORTKBNL(IDX)
                   MOVE    SPA-GMN-IKTSORTKBNLST(IDX)
                                          TO  SPA-GMN-IKTSORTKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-IKTSORTKBNLST(1)
                                          TO  SPA-GMN-IKTSORTKBN-G
           END-IF
      *
           .
       4110-IKTSORTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一括計算方法区分区分チェック
      *****************************************************************
       4110-IKTKEISANKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-IKTKEISANKBN-MAX
               IF      SPA-GMN-IKTKEISANKBN
                                       =   SPA-GMN-IKTKEISANKBNL(IDX)
                   MOVE    SPA-GMN-IKTKEISANKBNLST(IDX)
                                          TO  SPA-GMN-IKTKEISANKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-IKTKEISANKBNLST(1)
                                          TO  SPA-GMN-IKTKEISANKBN-G
           END-IF
      *
           .
       4110-IKTKEISANKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書未収金集計期間区分チェック
      *****************************************************************
       4110-MISYUKIKANKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MISYUKIKANKBN-MAX
               IF      SPA-GMN-MISYUKIKANKBN
                                       =   SPA-GMN-MISYUKIKANKBNL(IDX)
                   MOVE    SPA-GMN-MISYUKIKANKBNLST(IDX)
                                          TO  SPA-GMN-MISYUKIKANKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-MISYUKIKANKBNLST(1)
                                          TO  SPA-GMN-MISYUKIKANKBN-G
           END-IF
      *
           .
       4110-MISYUKIKANKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書発行区分（外来）チェック処理（収納行為機能情報）
      *****************************************************************
       4110-SYUSKYPRTG-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYUSKYPRTG-MAX
               IF      SPA-GMN-SYUSKYPRTG
                                       =   SPA-GMN-SYUSKYPRTGL (IDX)
                   MOVE    SPA-GMN-SYUSKYPRTGLST(IDX)
                                          TO  SPA-GMN-SYUSKYPRTG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SYUSKYPRTGLST (1)
                                       TO  SPA-GMN-SYUSKYPRTG-G
           END-IF
      *
           .
       4110-SYUSKYPRTG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書前回請求額印字区分チェック
      *****************************************************************
       4110-S03SKYMISYUPRT-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S03SKYMISYUPRT-MAX
               IF      SPA-GMN-S03SKYMISYUPRT
                                       =   SPA-GMN-S03SKYMISYUPRTL (IDX)
                   MOVE    SPA-GMN-S03SKYMISYUPRTLST(IDX)
                                          TO  SPA-GMN-S03SKYMISYUPRT-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-S03SKYMISYUPRTLST(2)
                                          TO  SPA-GMN-S03SKYMISYUPRT-G
           END-IF
      *
           .
       4110-S03SKYMISYUPRT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書発行区分（外来）チェック処理（収納行為機能情報）
      *****************************************************************
       4110-SYUSRYPRTG-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYUSRYPRTG-MAX
               IF      SPA-GMN-SYUSRYPRTG
                                       =   SPA-GMN-SYUSRYPRTGL (IDX)
                   MOVE    SPA-GMN-SYUSRYPRTGLST(IDX)
                                          TO  SPA-GMN-SYUSRYPRTG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SYUSRYPRTGLST (1)
                                       TO  SPA-GMN-SYUSRYPRTG-G
           END-IF
      *
           .
       4110-SYUSRYPRTG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書発行区分（入院）チェック処理（収納行為機能情報）
      *****************************************************************
       4110-SYUSKYPRTN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYUSKYPRTN-MAX
               IF      SPA-GMN-SYUSKYPRTN
                                       =   SPA-GMN-SYUSKYPRTNL (IDX)
                   MOVE    SPA-GMN-SYUSKYPRTNLST(IDX)
                                          TO  SPA-GMN-SYUSKYPRTN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SYUSKYPRTNLST (1)
                                       TO  SPA-GMN-SYUSKYPRTN-G
           END-IF
      *
           .
       4110-SYUSKYPRTN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書発行区分（入院）チェック処理（収納行為機能情報）
      *****************************************************************
       4110-SYUSRYPRTN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYUSRYPRTN-MAX
               IF      SPA-GMN-SYUSRYPRTN
                                       =   SPA-GMN-SYUSRYPRTNL (IDX)
                   MOVE    SPA-GMN-SYUSRYPRTNLST(IDX)
                                          TO  SPA-GMN-SYUSRYPRTN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SYUSRYPRTNLST (1)
                                       TO  SPA-GMN-SYUSRYPRTN-G
           END-IF
      *
           .
       4110-SYUSRYPRTN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    支払証明書印刷画面支払証明書発行区分チェック
      *****************************************************************
       4109-S04SHIHARAIKBN-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-S04SHIHARAI-MAX )
                    OR       ( FLG-OK  =   1 )
               IF    ( SPA-GMN-S04SHIHARAIKBN
                                       =   SPA-GMN-S04SHIHARAIL (IDX))
                   MOVE    SPA-GMN-S04SHIHARAILST (IDX)
                                       TO  SPA-GMN-S04SHIHARAIKBN-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF    ( FLG-OK              =   ZERO )
                   MOVE    SPA-GMN-S04SHIHARAILST (2)
                                       TO  SPA-GMN-S04SHIHARAIKBN-G
           END-IF
      *
           .
       4109-S04SHIHARAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    支払証明書印刷画面入外区分チェック
      *****************************************************************
       4109-S04NYUGAIKBN-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-S04NYUGAI-MAX )
                    OR       ( FLG-OK  =   1 )
               IF    ( SPA-GMN-S04NYUGAIKBN
                                       =   SPA-GMN-S04NYUGAIL (IDX))
                   MOVE    SPA-GMN-S04NYUGAILST (IDX)
                                       TO  SPA-GMN-S04NYUGAIKBN-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF    ( FLG-OK              =   ZERO )
                   MOVE    SPA-GMN-S04NYUGAILST (1)
                                       TO  SPA-GMN-S04NYUGAIKBN-G
           END-IF
      *
           .
       4109-S04NYUGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    支払証明書印刷画面期間指定チェック
      *****************************************************************
       4109-S04SRYKBN-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-S04SRY-MAX )
                    OR       ( FLG-OK  =   1 )
               IF    ( SPA-GMN-S04SRYKBN
                                       =   SPA-GMN-S04SRYL (IDX))
                   MOVE    SPA-GMN-S04SRYLST (IDX)
                                       TO  SPA-GMN-S04SRYKBN-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF    ( FLG-OK              =   ZERO )
                   MOVE    SPA-GMN-S04SRYLST (1)
                                       TO  SPA-GMN-S04SRYKBN-G
           END-IF
      *
           .
       4109-S04SRYKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    支払証明書印刷画面期間区分チェック
      *****************************************************************
       4109-S04KIKANKBN-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-S04KIKAN-MAX )
                    OR       ( FLG-OK  =   1 )
               IF    ( SPA-GMN-S04KIKANKBN
                                       =   SPA-GMN-S04KIKANL (IDX))
                   MOVE    SPA-GMN-S04KIKANLST (IDX)
                                       TO  SPA-GMN-S04KIKANKBN-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF    ( FLG-OK              =   ZERO )
                   MOVE    SPA-GMN-S04KIKANLST (1)
                                       TO  SPA-GMN-S04KIKANKBN-G
           END-IF
      *
           IF    ( SPA-GMN-S04KIKANKBN =   "1" )
               IF    ( SPA-GMN-S04SYORIKBN-G  =   SPACE )
                   MOVE    SPA-GMN-S04DSPLST(1)
                                       TO  SPA-GMN-S04DSPKBN-G
                   MOVE    SPA-GMN-S04SUMKBNLST (2)
                                       TO  SPA-GMN-S04SUMKBN-G
                   MOVE    SPA-GMN-S04SYORILST (1)
                                       TO  SPA-GMN-S04SYORIKBN-G
                   MOVE    SPA-GMN-S04JIHIPRTKBNLST(1)
                                       TO  SPA-GMN-S04JIHIPRTKBN-G
               END-IF
           ELSE
               MOVE    SPACE           TO  SPA-GMN-S04SYORIKBN-G
               MOVE    SPACE           TO  SPA-GMN-S04SUMKBN-G
               MOVE    SPA-GMN-S04DSPLST(2)
                                       TO  SPA-GMN-S04DSPKBN-G
               MOVE    SPA-GMN-S04JIHIPRTKBNLST(1)
                                       TO  SPA-GMN-S04JIHIPRTKBN-G
           END-IF
      *
           .
       4109-S04KIKANKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    支払証明書印刷画面請求内容の印字チェック
      *****************************************************************
       4109-S04DSPKBN-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-S04DSP-MAX )
                    OR       ( FLG-OK  =   1 )
               IF    ( SPA-GMN-S04DSPKBN
                                       =   SPA-GMN-S04DSPL (IDX))
                   MOVE    SPA-GMN-S04DSPLST (IDX)
                                       TO  SPA-GMN-S04DSPKBN-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF    ( FLG-OK              =   ZERO )
                   MOVE    SPA-GMN-S04DSPLST (1)
                                       TO  SPA-GMN-S04DSPKBN-G
           END-IF
      *
           .
       4109-S04DSPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    支払証明書印刷画面期間開始診療分の支払いチェック
      *****************************************************************
       4109-S04SUMKBN-CHK-SEC     SECTION.
      *
           IF    ( SPA-GMN-S04KIKANKBN =   "1" )
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL     ( IDX     >   SPA-GMN-S04SUMKBN-MAX )
                        OR       ( FLG-OK  =   1 )
                   IF    ( SPA-GMN-S04SUMKBN
                                       =   SPA-GMN-S04SUMKBNL (IDX))
                       MOVE    SPA-GMN-S04SUMKBNLST (IDX)
                                       TO  SPA-GMN-S04SUMKBN-G
                       MOVE    1       TO  FLG-OK
                   END-IF
               END-PERFORM
               IF    ( FLG-OK              =   ZERO )
                   MOVE    SPA-GMN-S04SUMKBNLST (2)
                                       TO  SPA-GMN-S04SUMKBN-G
               END-IF
           END-IF
      *
           .
       4109-S04SUMKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    支払証明書印刷画面期間終了後の支払いチェック
      *****************************************************************
       4109-S04SYORIKBN-CHK-SEC     SECTION.
      *
           IF    ( SPA-GMN-S04KIKANKBN =   "1" )
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL     ( IDX     >   SPA-GMN-S04SYORI-MAX )
                        OR       ( FLG-OK  =   1 )
                   IF    ( SPA-GMN-S04SYORIKBN
                                       =   SPA-GMN-S04SYORIL (IDX))
                       MOVE    SPA-GMN-S04SYORILST (IDX)
                                       TO  SPA-GMN-S04SYORIKBN-G
                       MOVE    1       TO  FLG-OK
                   END-IF
               END-PERFORM
               IF    ( FLG-OK              =   ZERO )
                   MOVE    SPA-GMN-S04SYORILST (1)
                                       TO  SPA-GMN-S04SYORIKBN-G
               END-IF
           END-IF
      *
           .
       4109-S04SYORIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    支払証明書印刷画面自費内訳明細書チェック
      *****************************************************************
       4109-S04JIHIPRTKBN-CHK-SEC     SECTION.
      *
           IF    ( SPA-GMN-S04KIKANKBN =   "1" )
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL     ( IDX   > SPA-GMN-S04JIHIPRTKBN-MAX )
                        OR       ( FLG-OK  =   1 )
                   IF    ( SPA-GMN-S04JIHIPRTKBN
                                       =   SPA-GMN-S04JIHIPRTKBNL (IDX))
                       MOVE    SPA-GMN-S04JIHIPRTKBNLST (IDX)
                                       TO  SPA-GMN-S04JIHIPRTKBN-G
                       MOVE    1       TO  FLG-OK
                   END-IF
               END-PERFORM
               IF    ( FLG-OK              =   ZERO )
                   MOVE    SPA-GMN-S04JIHIPRTKBNLST (1)
                                       TO  SPA-GMN-S04JIHIPRTKBN-G
               END-IF
           END-IF
      *
           .
       4109-S04JIHIPRTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    登録時刻チェック
      *****************************************************************
       4110-SYUSTIME-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-SYUSTIME-HH     >=  24 )
               MOVE    "3003"          TO  SPA-ERRCD
               MOVE    43              TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-SYUSTIME-MM     >=  60 )
                   MOVE    "3003"      TO  SPA-ERRCD
                   MOVE    44          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4110-SYUSTIME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    仮締め時間帯チェック
      *****************************************************************
       4110-SYUKARITIME-CHK-SEC        SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   2 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   2 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                   COMPUTE WRK-ERR-CUR
                       =   400
                       + ( IDX1    -   1 ) *  8
                       + ( IDX2    -   1 ) *  4
      *
                   IF    ( SPA-GMN-SYUKARIST-HH (IDX1 IDX2)    >=  24 )
                       MOVE    "3004"          TO  SPA-ERRCD
                       MOVE    WRK-ERR-CUR     TO  SPA-GMN-CUR
                   END-IF
      *
                   IF    ( SPA-ERRCD           =   SPACE )
                       IF    ( SPA-GMN-SYUKARIST-MM(IDX1 IDX2) >=  60 )
                           MOVE    "3004"      TO  SPA-ERRCD
                           COMPUTE SPA-GMN-CUR
                               =   WRK-ERR-CUR +   1
                       END-IF
                   END-IF
      *
                   IF    ( SPA-ERRCD           =   SPACE )
                       IF    ( SPA-GMN-SYUKARIED-HH(IDX1 IDX2) >=  24 )
                           MOVE    "3004"      TO  SPA-ERRCD
                           COMPUTE SPA-GMN-CUR
                               =   WRK-ERR-CUR +   2
                       END-IF
                   END-IF
      *
                   IF    ( SPA-ERRCD           =   SPACE )
                       IF    ( SPA-GMN-SYUKARIED-MM(IDX1 IDX2) >=  60 )
                           MOVE    "3004"      TO  SPA-ERRCD
                           COMPUTE SPA-GMN-CUR
                               =   WRK-ERR-CUR +   3
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           .
       4110-SYUKARITIME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    仮締め時間帯曜日チェック
      *****************************************************************
       4110-SYUKARIDAY-CHK-SEC       SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   2 )
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   7 )
                   MOVE    W07-SYUKARIDAY (IDX1 IDX2)
                                       TO  SPA-GMN-SYUKARIDAY
                                                       (IDX1 IDX2)
               END-PERFORM
           END-PERFORM
      *
           IF    ( WRK-KARIDAY     =   "1" )
               MOVE    1           TO  IDX1
               MOVE    2           TO  IDX2
           ELSE
               MOVE    2           TO  IDX1
               MOVE    1           TO  IDX2
           END-IF
      *
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL ( IDX3    >   7 )
      *
               IF    ( SPA-GMN-SYUKARIDAY (IDX1 IDX3)  =   "T" )
                   MOVE    "F"     TO  SPA-GMN-SYUKARIDAY (IDX2 IDX3)
               END-IF
      *
           END-PERFORM
      *
           .
       4110-SYUKARIDAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    主病名確認メッセージ表示有無チェック処理（病名機能情報）
      *****************************************************************
       4111-MSG1-DSPKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MSG1-DSPKBN-MAX
               IF      SPA-GMN-MSG1-DSPKBN
                                       =   SPA-GMN-MSG1-DSPKBNL (IDX)
                   MOVE    SPA-GMN-MSG1-DSPKBNLST(IDX)
                                          TO  SPA-GMN-MSG1-DSPKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    500             TO  SPA-GMN-CUR
               MOVE    "4001"          TO  SPA-ERRCD
           END-IF
      *
           .
       4111-MSG1-DSPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    移行病名警告メッセージ表示有無チェック処理（病名機能情報）
      *****************************************************************
       4111-MSG2-DSPKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MSG2-DSPKBN-MAX
               IF      SPA-GMN-MSG2-DSPKBN
                                       =   SPA-GMN-MSG2-DSPKBNL (IDX)
                   MOVE    SPA-GMN-MSG2-DSPKBNLST(IDX)
                                          TO  SPA-GMN-MSG2-DSPKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    501             TO  SPA-GMN-CUR
               MOVE    "4002"          TO  SPA-ERRCD
           END-IF
      *
           .
       4111-MSG2-DSPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    廃止年月日警告メッセージ表示有無チェック処理（病名機能情報）
      *****************************************************************
       4111-MSG3-DSPKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MSG3-DSPKBN-MAX
               IF      SPA-GMN-MSG3-DSPKBN
                                       =   SPA-GMN-MSG3-DSPKBNL (IDX)
                   MOVE    SPA-GMN-MSG3-DSPKBNLST(IDX)
                                          TO  SPA-GMN-MSG3-DSPKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    502             TO  SPA-GMN-CUR
               MOVE    "4003"          TO  SPA-ERRCD
           END-IF
      *
           .
       4111-MSG3-DSPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    同一病名警告メッセージ表示有無チェック処理（病名機能情報）
      *****************************************************************
       4111-MSG4-DSPKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MSG4-DSPKBN-MAX
               IF      SPA-GMN-MSG4-DSPKBN
                                       =   SPA-GMN-MSG4-DSPKBNL (IDX)
                   MOVE    SPA-GMN-MSG4-DSPKBNLST(IDX)
                                          TO  SPA-GMN-MSG4-DSPKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    503             TO  SPA-GMN-CUR
               MOVE    "4006"          TO  SPA-ERRCD
           END-IF
      *
           .
       4111-MSG4-DSPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    死亡区分更新確認メッセージ表示有無チェック処理（病名機能情報）
      *****************************************************************
       4111-MSG5-DSPKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MSG5-DSPKBN-MAX
               IF      SPA-GMN-MSG5-DSPKBN
                                       =   SPA-GMN-MSG5-DSPKBNL (IDX)
                   MOVE    SPA-GMN-MSG5-DSPKBNLST(IDX)
                                          TO  SPA-GMN-MSG5-DSPKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    508             TO  SPA-GMN-CUR
               MOVE    "4009"          TO  SPA-ERRCD
           END-IF
      *
           .
       4111-MSG5-DSPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療科初期表示区分チェック処理（病名機能情報）
      *****************************************************************
       4111-SRYKA-DSPKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYKA-DSPKBN-MAX
               IF      SPA-GMN-SRYKA-DSPKBN
                                       =   SPA-GMN-SRYKA-DSPKBNL (IDX)
                   MOVE    SPA-GMN-SRYKA-DSPKBNLST(IDX)
                                          TO  SPA-GMN-SRYKA-DSPKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    504             TO  SPA-GMN-CUR
               MOVE    "4004"          TO  SPA-ERRCD
           END-IF
      *
           .
       4111-SRYKA-DSPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    表示対象期間初期表示区分チェック処理（病名機能情報）
      *****************************************************************
       4111-SYOKBN-DSPKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYOKBN-DSPKBN-MAX
               IF      SPA-GMN-SYOKBN-DSPKBN
                                       =   SPA-GMN-SYOKBN-DSPKBNL (IDX)
                   MOVE    SPA-GMN-SYOKBN-DSPKBNLST(IDX)
                                          TO  SPA-GMN-SYOKBN-DSPKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    505             TO  SPA-GMN-CUR
               MOVE    "4005"          TO  SPA-ERRCD
           END-IF
      *
           .
       4111-SYOKBN-DSPKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    疑いフラグ設定区分チェック処理（病名機能情報）
      *****************************************************************
       4111-UTAGAIFLG-KBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-UTAGAIFLG-KBN-MAX
               IF      SPA-GMN-UTAGAIFLG-KBN
                                       =   SPA-GMN-UTAGAIFLG-KBNL (IDX)
                   MOVE    SPA-GMN-UTAGAIFLG-KBNLST(IDX)
                                          TO  SPA-GMN-UTAGAIFLG-KBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    507             TO  SPA-GMN-CUR
               MOVE    "4008"          TO  SPA-ERRCD
           END-IF
      *
           .
       4111-UTAGAIFLG-KBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記区分初期表示チェック処理（病名機能情報）
      *****************************************************************
       4111-SJKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SJKBN-MAX
               IF      SPA-GMN-SJKBN   =   SPA-GMN-SJKBNLST (IDX)(1:2)
                   MOVE    SPA-GMN-SJKBNLST(IDX)
                                          TO  SPA-GMN-SJKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    506             TO  SPA-GMN-CUR
               MOVE    "4007"          TO  SPA-ERRCD
           END-IF
           .
       4111-SJKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全科表示順チェック処理（会計照会機能情報）
      *****************************************************************
       4311-SRYKBN-FLG-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYKBN-MAX
               IF      SPA-GMN-SRYKBN-FLG
                                       =   SPA-GMN-SRYKBN-LIST(IDX)
                                                             (1:1)
                   MOVE    SPA-GMN-SRYKBN-LIST(IDX)
                                           TO  SPA-GMN-SRYKBN-FLG-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    600             TO  SPA-GMN-CUR
               MOVE    "5001"          TO  SPA-ERRCD
           END-IF
      *
           .
       4311-SRYKBN-FLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療科の初期表示チェック処理（会計照会機能情報）
      *****************************************************************
       4311-SRYKA-FLG-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYKA-MAX
               IF      SPA-GMN-SRYKA-FLG
                                       =   SPA-GMN-SRYKA-LIST(IDX)
                                                             (1:1)
                   MOVE    SPA-GMN-SRYKA-LIST(IDX)
                                           TO  SPA-GMN-SRYKA-FLG-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    601             TO  SPA-GMN-CUR
               MOVE    "5002"          TO  SPA-ERRCD
           END-IF
      *
           .
       4311-SRYKA-FLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    レセプトブレビューチェック処理（会計照会機能情報）
      *****************************************************************
       4311-RESPRV043-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-RESPRV043-MAX
               IF      SPA-GMN-RESPRV043
                                       =   SPA-GMN-RESPRV043-LIST(IDX)
                                                             (1:1)
                   MOVE    SPA-GMN-RESPRV043-LIST(IDX)
                                           TO  SPA-GMN-RESPRV043-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    602             TO  SPA-GMN-CUR
               MOVE    "5003"          TO  SPA-ERRCD
           END-IF
      *
           .
       4311-RESPRV043-CHK-EXT.
           EXIT.
      *****************************************************************
      *    レセプトブレビューレイアウトチェック処理（会計照会機能情報）
      *****************************************************************
       4311-PRVLAYOUT043-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-PRVLAYOUT043-MAX)
                       OR     (FLG-OK  =   1    ) 
               IF      SPA-GMN-PRVLAYOUT043
                                       =   SPA-GMN-PRVLAYOUT043-LST
                                                      (IDX)(1:1)
                   MOVE    SPA-GMN-PRVLAYOUT043-LST(IDX)
                                          TO  SPA-GMN-PRVLAYOUT043-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    603             TO  SPA-GMN-CUR
               MOVE    "5004"          TO  SPA-ERRCD
           END-IF
      *
           .
       4311-PRVLAYOUT043-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期表示ドクターチェック処理（予約機能情報）
      *****************************************************************
       4511-INIT-DRCD-CHK-SEC       SECTION.
      *
      *R01.12
           MOVE    SPACE               TO  SPA-GMN-DRKIKAN
           MOVE    ZERO                TO  FLG-OK
           MOVE    SPACE               TO  WRK-CHK-YUKOKIKAN
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-INIT-DRCD-MAX
               IF      SPA-GMN-INIT-DRCD
                                       =   SPA-GMN-INIT-DRCD-LIST(IDY)
                                                             (1:4)
                   MOVE    SPA-GMN-INIT-DRCD-LIST(IDY)
                                           TO  SPA-GMN-INIT-DRCD-G
                   MOVE    SPA-NAI-DRSTYUKYMD (IDY)
                                           TO  WRK-DRSTYUKYMD
                   MOVE    SPA-NAI-DREDYUKYMD (IDY)
                                           TO  WRK-DREDYUKYMD
      *
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-INIT-DRCD-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   1
               PERFORM 45111-DRCD-KIKANCHK-SEC
               GO   TO  4511-INIT-DRCD-CHK-EXT
           END-IF
      *
      *    職員情報（ドクター）検索
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    "1"                 TO  SYS-1010-KBNCD(1:1)
           MOVE    SPA-GMN-INIT-DRCD   TO  SYS-1010-KBNCD(2:4)
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-OK2
           PERFORM UNTIL       (FLG-OK     NOT  =   ZERO) OR
                               (FLG-SYSKANRI    =   1   )
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
      *        表示分の判定なし
               IF    ((SYS-1010-STYUKYMD   >   SPA-W01-EDYUKYMD)
                  OR  (SYS-1010-EDYUKYMD   <   SPA-W01-STYUKYMD))
                   MOVE    1                    TO  FLG-OK2
               ELSE
      *            有効期間内
                   MOVE    1                    TO  FLG-OK
               END-IF
               IF      FLG-OK              =   1
                   IF      SPA-INIT-DRCD-MAX   <   99
                       ADD     1           TO  SPA-INIT-DRCD-MAX
                   END-IF
                   MOVE    SPA-INIT-DRCD-MAX   TO  IDX
                   MOVE    SPACE           TO  WRK-GMN-INIT-DRCD-G
                   MOVE    SYS-1010-KBNCD(2:4)
                                           TO  WRK-GMN-INIT-DRCD
                   MOVE    SYS-1010-NAME   TO  WRK-GMN-INIT-DRCDMEI
                   MOVE    WRK-GMN-INIT-DRCD-G
                                           TO  SPA-GMN-INIT-DRCD-LIST
                                                               (IDX)
                   MOVE    SPA-GMN-INIT-DRCD-LIST(IDX)
                                           TO  SPA-GMN-INIT-DRCD-G
      *
                   MOVE    SYS-1010-STYUKYMD
                                           TO  SPA-NAI-DRSTYUKYMD
                                                               (IDX)
                   MOVE    SYS-1010-EDYUKYMD
                                           TO  SPA-NAI-DREDYUKYMD
                                                               (IDX)
                   MOVE    SPA-NAI-DRSTYUKYMD (IDX)
                                           TO  WRK-DRSTYUKYMD
                   MOVE    SPA-NAI-DREDYUKYMD (IDX)
                                           TO  WRK-DREDYUKYMD
                   IF     (SYS-1010-HYOJIFLG       =   "1"  )
                       MOVE    2                   TO  FLG-OK
                   END-IF
               END-IF
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-OK          NOT =   ZERO
               PERFORM 45111-DRCD-KIKANCHK-SEC
               GO   TO  4511-INIT-DRCD-CHK-EXT
           END-IF
      *    対象なし
           IF      FLG-OK2             =   ZERO
               MOVE    700             TO  SPA-GMN-CUR
               MOVE    "6001"          TO  SPA-ERRCD
           ELSE
               IF     (FLG-OK              =   ZERO  )
      *            期間外あり
                   MOVE    700             TO  SPA-GMN-CUR
                   MOVE    "6008"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4511-INIT-DRCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初期表示ドクターチェック処理（ドクター期間チェック情報）
      *****************************************************************
       45111-DRCD-KIKANCHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-DRKIKAN
           IF     (WRK-DREDYUKYMD      <   SPA-W01-EDYUKYMD )
      *        システム管理期間外
               MOVE    WRK-DREDYUKYMD        TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               STRING  "有効終了日："
                       WRK-HENYMDG     DELIMITED   BY  SIZE
                                           INTO    SPA-GMN-DRKIKAN
               END-STRING
           END-IF
      *
           IF     (WRK-DRSTYUKYMD      >   SPA-W01-STYUKYMD )
             AND  (SPA-GMN-DRKIKAN     =   SPACE       )
      *        システム管理期間外
               MOVE    WRK-DRSTYUKYMD        TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               STRING  "有効開始日："
                       WRK-HENYMDG     DELIMITED   BY  SIZE
                                           INTO    SPA-GMN-DRKIKAN
               END-STRING
           END-IF
           .
       45111-DRCD-KIKANCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約一覧の初期表示チェック処理（予約機能情報）
      *****************************************************************
       4511-YYKINIT-FLG-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-YYKINIT-MAX )
                           OR (FLG-OK  =   1    )
               IF      SPA-GMN-YYKINIT-FLG
                                       =   SPA-GMN-YYKINIT-LIST(IDX)
                                                             (1:1)
                   MOVE    SPA-GMN-YYKINIT-LIST(IDX)
                                           TO  SPA-GMN-YYKINIT-FLG-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    701             TO  SPA-GMN-CUR
               MOVE    "6002"          TO  SPA-ERRCD
           END-IF
      *
           .
       4511-YYKINIT-FLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    予約枠最大件数チェック処理（予約機能情報）
      *****************************************************************
       4511-YYKMAXCNT-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-YYKMAXCNT   =   ZERO
               MOVE    702             TO  SPA-GMN-CUR
               MOVE    "6003"          TO  SPA-ERRCD
           END-IF
           .
       4511-YYKMAXCNT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    予約票発行区分チェック処理（予約機能情報）
      *****************************************************************
       4511-YYKHYO-KBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-YYKHYO-MAX )
                          OR  (FLG-OK  =   1    )
               IF      SPA-GMN-YYKHYO-KBN
                                       =   SPA-GMN-YYKHYO-LIST(IDX)
                                                             (1:1)
                   MOVE    SPA-GMN-YYKHYO-LIST(IDX)
                                           TO  SPA-GMN-YYKHYO-KBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    703             TO  SPA-GMN-CUR
               MOVE    "6004"          TO  SPA-ERRCD
           END-IF
      *
           .
       4511-YYKHYO-KBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    予約受付連動区分チェック処理（予約機能情報）
      *****************************************************************
       4511-YYKUKE-KBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-YYKUKE-MAX )
                          OR  (FLG-OK  =   1    )
               IF      SPA-GMN-YYKUKE-KBN
                                       =   SPA-GMN-YYKUKE-LIST(IDX)
                                                             (1:1)
                   MOVE    SPA-GMN-YYKUKE-LIST(IDX)
                                           TO  SPA-GMN-YYKUKE-KBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    705             TO  SPA-GMN-CUR
               MOVE    "6005"          TO  SPA-ERRCD
           END-IF
      *
           .
       4511-YYKUKE-KBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    予約患者番号連動区分チェック処理（予約機能情報）
      *****************************************************************
       4511-YYKPTNUM-KBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-YYKPTNUM-MAX )
                          OR  (FLG-OK  =   1    )
               IF      SPA-GMN-YYKPTNUM-KBN
                                       =   SPA-GMN-YYKPTNUM-LIST(IDX)
                                                             (1:1)
                   MOVE    SPA-GMN-YYKPTNUM-LIST(IDX)
                                           TO  SPA-GMN-YYKPTNUM-KBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    704             TO  SPA-GMN-CUR
               MOVE    "6006"          TO  SPA-ERRCD
           END-IF
      *
           .
       4511-YYKPTNUM-KBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    受付保険組合せ区分チェック処理（予約機能情報）
      *****************************************************************
       4511-UKEHKNCHK-KBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-UKEHKNCHK-MAX )
                          OR  (FLG-OK  =   1    )
               IF      SPA-GMN-UKEHKNCHK-KBN
                                       =   SPA-GMN-UKEHKNCHK-LIST(IDX)
                                                             (1:1)
                   MOVE    SPA-GMN-UKEHKNCHK-LIST(IDX)
                                           TO  SPA-GMN-UKEHKNCHK-KBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    706             TO  SPA-GMN-CUR
               MOVE    "6007"          TO  SPA-ERRCD
           END-IF
      *
           .
       4511-UKEHKNCHK-KBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    妥結率区分チェック処理（点数算定基準情報）
      *****************************************************************
       4611-DAKETUKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-DAKETUKBN-MAX )
                          OR  (FLG-OK  =   1    )
               IF      SPA-GMN-DAKETUKBN
                                       =   SPA-GMN-DAKETUKBN-LIST(IDX)
                                                             (1:1)
                   MOVE    SPA-GMN-DAKETUKBN-LIST(IDX)
                                           TO  SPA-GMN-DAKETUKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    800             TO  SPA-GMN-CUR
               MOVE    "8001"          TO  SPA-ERRCD
           END-IF
      *
           .
       44611-DAKETUKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    低紹介率区分チェック処理（点数算定基準情報）
      *****************************************************************
       4611-TEIRITUKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-TEIRITUKBN-MAX )
                          OR  (FLG-OK  =   1    )
               IF      SPA-GMN-TEIRITUKBN
                                       =   SPA-GMN-TEIRITUKBN-LIST(IDX)
                                                             (1:1)
                   MOVE    SPA-GMN-TEIRITUKBN-LIST(IDX)
                                           TO  SPA-GMN-TEIRITUKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    801             TO  SPA-GMN-CUR
               MOVE    "8002"          TO  SPA-ERRCD
           END-IF
      *
           .
       44611-TEIRITUKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"           TO  SPA-SAKIPG
           MOVE    "W07"           TO  SPA-MOTOPG
      *
      **** INITIALIZE                  SPA-W01KYOUTU
      *
      ***  MOVE   "CHANGE"         TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                 SECTION.
      *
           EVALUATE    SPA-W01-KANRICD
               WHEN    "1007"
                   MOVE    "1002"              TO  SPA-WIDCD
               WHEN    "1017"
                   MOVE    "1004"              TO  SPA-WIDCD
               WHEN    "1038"
                   MOVE    "1006"              TO  SPA-WIDCD
               WHEN    "1039"
                   MOVE    "1008"              TO  SPA-WIDCD
               WHEN    "1042"
                   MOVE    "1010"              TO  SPA-WIDCD
               WHEN    "1043"
                   MOVE    "1012"              TO  SPA-WIDCD
               WHEN    "1045"
                   MOVE    "1014"              TO  SPA-WIDCD
               WHEN    "1201"
                   MOVE    "1202"              TO  SPA-WIDCD
           END-EVALUATE
      *
           .    
       403-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理１００７
      *****************************************************************
       4032-DEL-1007-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1007-REC
           INITIALIZE                      SYS-1007-REC
           MOVE    "1007"              TO  SYS-1007-KANRICD
           MOVE    "*"                 TO  SYS-1007-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1007-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1007-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1007-HOSPNUM
           MOVE    SYS-1007-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "9001"              TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-1007-KEY 
                       ",MCP-RC:" MCP-RC 
               GO  TO  4032-DEL-1007-EXT
           END-IF
      *
      *     MOVE   "JOIN"               TO  MCP-PUTTYPE
      *     PERFORM 210-BACK
           .
       4032-DEL-1007-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理１００８
      *****************************************************************
       4032-DEL-1008-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1008-REC
           INITIALIZE                      SYS-1008-REC
           MOVE    "1008"              TO  SYS-1008-KANRICD
           MOVE    "*"                 TO  SYS-1008-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1008-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1008-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1008-HOSPNUM
           MOVE    SYS-1008-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "9001"              TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-1008-KEY 
                       ",MCP-RC:" MCP-RC 
               GO  TO  4032-DEL-1008-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4032-DEL-1008-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理１０１７
      *****************************************************************
       4032-DEL-1017-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1017-REC
           INITIALIZE                      SYS-1017-REC
           MOVE    "1017"              TO  SYS-1017-KANRICD
           MOVE    "*"                 TO  SYS-1017-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1017-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1017-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1017-HOSPNUM
           MOVE    SYS-1017-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "9001"                  TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-1017-KEY
                       ",MCP-RC:" MCP-RC 
               GO  TO  4032-DEL-1017-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4032-DEL-1017-EXT.
           EXIT.
      *****************************************************************
      *    削除処理１０３８
      *****************************************************************
       4032-DEL-1038-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1038-REC
           INITIALIZE                      SYS-1038-REC
           MOVE    "1038"              TO  SYS-1038-KANRICD
           MOVE    "*"                 TO  SYS-1038-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1038-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1038-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1038-HOSPNUM
           MOVE    SYS-1038-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "9001"              TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-1038-KEY 
                       ",MCP-RC:" MCP-RC 
               GO  TO  4032-DEL-1038-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4032-DEL-1038-EXT.
           EXIT.
      *****************************************************************
      *    削除処理１０３９
      *****************************************************************
       4032-DEL-1039-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1039-REC
           INITIALIZE                      SYS-1039-REC
           MOVE    "1039"              TO  SYS-1039-KANRICD
           MOVE    "*"                 TO  SYS-1039-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1039-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1039-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1039-HOSPNUM
           MOVE    SYS-1039-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "9001"              TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-1039-KEY 
                       ",MCP-RC:" MCP-RC 
               GO  TO  4032-DEL-1039-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4032-DEL-1039-EXT.
           EXIT.
      *****************************************************************
      *    削除処理１０４２
      *****************************************************************
       4032-DEL-1042-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1042-REC
           INITIALIZE                      SYS-1042-REC
           MOVE    "1042"              TO  SYS-1042-KANRICD
           MOVE    "*"                 TO  SYS-1042-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1042-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1042-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1042-HOSPNUM
           MOVE    SYS-1042-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "9001"              TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-1042-KEY 
                       ",MCP-RC:" MCP-RC 
               GO  TO  4032-DEL-1042-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4032-DEL-1042-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理１０４３
      *****************************************************************
       4033-DEL-1043-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1043-REC
           INITIALIZE                      SYS-1043-REC
           MOVE    "1043"              TO  SYS-1043-KANRICD
           MOVE    "*"                 TO  SYS-1043-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1043-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1043-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1043-HOSPNUM
           MOVE    SYS-1043-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "9001"              TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-1043-KEY 
                       ",MCP-RC:" MCP-RC 
               GO  TO  4033-DEL-1043-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4033-DEL-1043-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理１０４５
      *****************************************************************
       4033-DEL-1045-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1045-REC
           INITIALIZE                      SYS-1045-REC
           MOVE    "1045"              TO  SYS-1045-KANRICD
           MOVE    "*"                 TO  SYS-1045-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1045-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1045-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1045-HOSPNUM
           MOVE    SYS-1045-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "9001"              TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-1045-KEY 
                       ",MCP-RC:" MCP-RC 
               GO  TO  4033-DEL-1045-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4033-DEL-1045-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    削除処理１２０１（点数算定基準の削除）
      *****************************************************************
       4033-DEL-1201-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1201"              TO  SYS-KANRICD
           MOVE    "A0000001"          TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "9001"              TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-KEY 
                       ",MCP-RC:" MCP-RC 
               GO  TO  4033-DEL-1201-EXT
           END-IF
      *
      *    低紹介率区分
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1202"              TO  SYS-KANRICD
           MOVE    "A0000001"          TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "9001"              TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-KEY 
                       ",MCP-RC:" MCP-RC 
               GO  TO  4033-DEL-1201-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4033-DEL-1201-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC             SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    SPA-W01-KANRICD
               WHEN    "1007"
                   PERFORM 4900-KANRENCHK-SEC
                   IF      SPA-ERRCD        =   SPACE
                       MOVE    "1001"           TO  SPA-WIDCD
                   END-IF
               WHEN    "1017"
                   IF      SPA-ERRCD        =   SPACE
                       MOVE    "1003"           TO  SPA-WIDCD
                   END-IF
               WHEN    "1038"
                   IF      SPA-ERRCD        =   SPACE
                       MOVE    "1005"           TO  SPA-WIDCD
                   END-IF
               WHEN    "1039"
                   PERFORM 4904-KANRENCHK-SEC
                   IF      SPA-ERRCD        =   SPACE
                       MOVE    "1007"           TO  SPA-WIDCD
                   END-IF
               WHEN    "1042"
                   IF      SPA-ERRCD        =   SPACE
                       MOVE    "1009"           TO  SPA-WIDCD
                   END-IF
               WHEN    "1043"
                   IF      SPA-ERRCD        =   SPACE
                       MOVE    "1011"           TO  SPA-WIDCD
                   END-IF
               WHEN    "1045"
                   IF      SPA-ERRCD        =   SPACE
                       MOVE    "1013"           TO  SPA-WIDCD
                   END-IF
               WHEN    "1201"
                   IF      SPA-ERRCD        =   SPACE
                       MOVE    "1201"           TO  SPA-WIDCD
                   END-IF
              END-EVALUATE
          END-IF
      *
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      ******************************************************************
      *       登録処理
      *****************************************************************
       491-KOUSHIN-1007-SEC        SECTION.
      *
           MOVE    SPACE               TO  SYS-1007-REC
           INITIALIZE                      SYS-1007-REC
           MOVE    "1007"              TO  SYS-1007-KANRICD
           MOVE    "*"                 TO  SYS-1007-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1007-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1007-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1007-HOSPNUM
           MOVE    SYS-1007-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1007-REC
           ELSE
               MOVE    SPACE               TO  SYS-1007-REC
               INITIALIZE                      SYS-1007-REC
               MOVE    "1007"              TO  SYS-1007-KANRICD
               MOVE    "*"                 TO  SYS-1007-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1007-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-1007-EDYUKYMD
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
               MOVE    SPA-GMN-DATA (IDX)  TO  SYS-1007-SANTEI (IDX:1)
           END-PERFORM 
           MOVE    SYS-1007-REC            TO  SYSKANRI-REC
      *    ＤＢ更新処理
           PERFORM 4910-DBKOUSIN-SEC
      *
      *     MOVE   "JOIN"               TO  MCP-PUTTYPE
      *     PERFORM 210-BACK 
           .
       491-KOUSHIN-1007-EXT.
           EXIT.
      *
      ******************************************************************
      *       登録処理
      *****************************************************************
       4910-DBKOUSIN-SEC        SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           IF      FLG-SYSKANRI            =   ZERO
      *
               MOVE    SPA-OPID                TO  SYS-OPID
      *
               MOVE    SMCNDATE-YMD            TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
      *
               MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    SPA-HOSPNUM             TO  SYSUP-HOSPNUM
               MOVE    SPA-HOSPNUM             TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
      *
               MOVE    "DBUPDATE"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "upd1"                  TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC            NOT =   ZERO 
                   MOVE    "9002"                  TO  SPA-ERRCD
                   DISPLAY "W07 UPD ERR SYSKANRI-KEY =>" SYS-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF 
           ELSE
               MOVE    SPA-OPID                TO  SYS-OPID
      *
               MOVE    SMCNDATE-YMD            TO  SYS-CREYMD
                                                   SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
               MOVE    SPA-HOSPNUM             TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
      *
               MOVE    "DBINSERT"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC            NOT =   ZERO 
                   MOVE    "9002"                  TO  SPA-ERRCD
                   DISPLAY "W07 INS ERR SYSKANRI-KEY =>" SYS-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF 
           END-IF
      *
           .
       4910-DBKOUSIN-EXT.
           EXIT.
      *
      ******************************************************************
      *       登録処理
      *****************************************************************
       491-KOUSHIN-1008-SEC        SECTION.
      *
           MOVE    SPACE               TO  SYS-1008-REC
           INITIALIZE                      SYS-1008-REC
           MOVE    "1008"              TO  SYS-1008-KANRICD
           MOVE    "*"                 TO  SYS-1008-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1008-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1008-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1008-HOSPNUM
           MOVE    SYS-1008-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1008-REC
           ELSE
               MOVE    SPACE               TO  SYS-1008-REC
               INITIALIZE                      SYS-1008-REC
               MOVE    "1008"              TO  SYS-1008-KANRICD
               MOVE    "*"                 TO  SYS-1008-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1008-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-1008-EDYUKYMD
           MOVE    SPA-GMN-INTERACT-KIKAN  TO  SYS-1008-INTERACT-KIKAN
           MOVE    SPA-GMN-KINKCHK         TO  SYS-1008-INTERACT-CHKKBN
           MOVE    SPA-GMN-GAIKANRIKBN-CHK TO  SYS-1008-GAIKANRIKBN-CHK
           MOVE    SPA-GMN-DATA(7)         TO  SYS-1008-IKUJIEIYO-FLG
           MOVE    SPA-GMN-SRYKACD         TO  SYS-1008-IKUJIEIYO-SRYKA
           MOVE    SPA-GMN-GAIMOVE         TO  SYS-1008-GAI-CURSOR-SET-1
      *    入院初診料自動発生
           MOVE    SPA-GMN-NYUSYOSIN       TO  SYS-1008-NYU-SYOSIN
      *    包括入力
           MOVE    SPA-GMN-HOUKATU         TO  SYS-1008-HOUKATU-FLG
      *    数量ゼロ入力
           MOVE    SPA-GMN-SURYOZERO       TO  SYS-1008-SURYOZERO-FLG
      *    入院調基チェック
           MOVE    SPA-GMN-CHOKICHK        TO  SYS-1008-CHOKICHK-FLG
      *    クリア確認チェック
           MOVE    SPA-GMN-CLEARCHK        TO  SYS-1008-CLEARCHK-FLG
      *    薬剤情報提供料チェック
           MOVE    SPA-GMN-YAKJYOCHK       TO  SYS-1008-YAKJYOCHK-FLG
      *    前回保険組合せ相違チェック
           MOVE    SPA-GMN-ZENHKNCHK       TO  SYS-1008-ZENHKNCHK-FLG
      *    手帳加算確認チェック
           MOVE    SPA-GMN-TECHOKBN        TO  SYS-1008-TECHOKBN-FLG
      *v4.8
      *    時間外緊急院内チェック
           MOVE    SPA-GMN-JGAIKEIKBN      TO  SYS-1008-JGAIKEIKBN-FLG
      *H28.11
      *    薬剤投与量チェック区分
           MOVE    SPA-GMN-TOUYOCHK        TO  SYS-1008-TOUYOCHK-FLG
      *H29.4
      *    投薬３０日超チェック区分
           MOVE    SPA-GMN-SHOHOU30CHK     TO  SYS-1008-SHOHOU30CHK
      *
      *    時間外加算特例乳幼児
           MOVE    SPA-GMN-JIKANTOKU-NYU   TO
                                           SYS-1008-JIKANTOKU-NYU-FLG
      *    画像診断管理加算１（写真診断）
           MOVE    SPA-GMN-GAZOUKSN1       TO  SYS-1008-GAZOUKSN1-FLG
      *    最終来院日からの期間
           MOVE    SPA-GMN-LAST-KIKAN      TO  SYS-1008-LAST-KIKAN
      *    中途終了展開時の自動発生
           MOVE    SPA-GMN-CHUTOTENKAI     TO  SYS-1008-CHUTOTENKAI-FLG
      *    訂正時の自動発生
           MOVE    SPA-GMN-TEISEIAUTO      TO  SYS-1008-TEISEIAUTO-FLG
      *    訂正時の自動発生（特定疾患処方管理）
           MOVE    SPA-GMN-TEISEITOKU      TO  SYS-1008-TEISEITOKU-FLG
      *    訂正時の展開（処方料）
           MOVE    SPA-GMN-TEISHOHOU       TO  SYS-1008-TEISHOHOU-FLG
      *    病名表示区分
           MOVE    SPA-GMN-BYOHYOKBN       TO  SYS-1008-BYOHYOKBN-FLG
      *    剤登録順区分
           MOVE    SPA-GMN-ZAIJUNKBN       TO  SYS-1008-ZAIJUNKBN-FLG
      *    検体検査加算自動発生
           MOVE    SPA-GMN-KENSAAUTO       TO  SYS-1008-KENSAAUTO-FLG
      *    訂正時の自動発生（入院）
           MOVE    SPA-GMN-TEINYUAUTO      TO  SYS-1008-TEINYUAUTO-FLG
      *    病名疾患区分からの自動発生
           MOVE    SPA-GMN-BYOMEIAUTO      TO  SYS-1008-BYOMEIAUTO-FLG
      *    入院時初回加算の自動発生
           MOVE    SPA-GMN-NYUKENSA        TO  SYS-1008-NYUKENSA-FLG
      *    初診再診料の自動発生
           MOVE    SPA-GMN-INITAUTO        TO  SYS-1008-INITAUTO-FLG
      *
      *    時間外加算乳幼児チェック
           MOVE    SPA-GMN-JIKANTOKU-CHK   TO
                                           SYS-1008-JIKANTOKU-CHK
      *    チェック除外時間
           MOVE    SPA-NAI-JIKANCHK-WST(1) TO
                                           SYS-1008-JIKANCHK-WST(1)
           MOVE    SPA-NAI-JIKANCHK-WST(2) TO
                                           SYS-1008-JIKANCHK-WST(2)
           MOVE    SPA-NAI-JIKANCHK-WED(1) TO
                                           SYS-1008-JIKANCHK-WED(1)
           MOVE    SPA-NAI-JIKANCHK-WED(2) TO
                                           SYS-1008-JIKANCHK-WED(2)
           MOVE    SPA-NAI-JIKANCHK-SST(1) TO
                                           SYS-1008-JIKANCHK-SST(1)
           MOVE    SPA-NAI-JIKANCHK-SST(2) TO
                                           SYS-1008-JIKANCHK-SST(2)
           MOVE    SPA-NAI-JIKANCHK-SED(1) TO
                                           SYS-1008-JIKANCHK-SED(1)
           MOVE    SPA-NAI-JIKANCHK-SED(2) TO
                                           SYS-1008-JIKANCHK-SED(2)
      *
           MOVE    SYS-1008-REC            TO  SYSKANRI-REC
      *
      *    ＤＢ更新処理
           PERFORM 4910-DBKOUSIN-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK 
           END-IF
           .
       490-KOUSHIN-1008-EXT.
           EXIT.
      *
      ******************************************************************
      *       登録処理
      *****************************************************************
       491-KOUSHIN-1017-SEC        SECTION.
      *
           MOVE    SPACE               TO  SYS-1017-REC
           INITIALIZE                      SYS-1017-REC
           MOVE    "1017"              TO  SYS-1017-KANRICD
           MOVE    "*"                 TO  SYS-1017-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1017-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1017-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1017-HOSPNUM
           MOVE    SYS-1017-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1017-REC
           ELSE
               MOVE    SPACE               TO  SYS-1017-REC
               INITIALIZE                      SYS-1017-REC
               MOVE    "1017"              TO  SYS-1017-KANRICD
               MOVE    "*"                 TO  SYS-1017-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1017-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-1017-EDYUKYMD
           MOVE    SPA-GMN-BYOMEIKBN       TO  SYS-1017-KARUTE-BYOMEI
           MOVE    SPA-GMN-JISYOKBN        TO  SYS-1017-JINMEI-USE
           MOVE    SPA-GMN-HAKKOKBN        TO  SYS-1017-KARUTE-HAKKO
           MOVE    SPA-GMN-HOJYOKBN        TO  SYS-1017-JIHIHKN-TAX
           MOVE    SPA-GMN-DOUMEI-CHKFLG   TO  SYS-1017-DOUMEI-CHKFLG
           MOVE    SPA-GMN-JYUFUKU-CHKKBN  TO  SYS-1017-JYUFUKU-CHKKBN
           MOVE    SPA-GMN-KOHONLY-CHKFLG  TO  SYS-1017-KOHONLY-CHKFLG
           MOVE    SPA-GMN-PTNUMDEL-FLG    TO  SYS-1017-PTNUMDEL-FLG
           MOVE    SPA-GMN-HONKZKKBN-FLG   TO  SYS-1017-HONKZKKBN-FLG
           MOVE    SPA-GMN-KZKNEN          TO  SYS-1017-KZKNEN
           MOVE    SPA-GMN-HIHKNJAKBN-FLG  TO  SYS-1017-HIHKNJAKBN-FLG
           MOVE    SPA-GMN-SETAIKBN-FLG    TO  SYS-1017-SETAIKBN-FLG
           MOVE    SPA-GMN-GRPPTLST-FLG    TO  SYS-1017-GRPPTLST-FLG
           MOVE    SPA-GMN-KOHRRKKBN-FLG   TO  SYS-1017-KOHRRKKBN-FLG
           MOVE    SPA-GMN-POSTKBN-FLG     TO  SYS-1017-POSTKBN-FLG
           MOVE    SPA-GMN-KENMEIKBN-FLG   TO  SYS-1017-KENMEIKBN-FLG
           MOVE    SPA-GMN-TOWNNAME2KBN    TO  SYS-1017-TOWNNAME2KBN-FLG
           MOVE    SPA-GMN-KOHHBTKBN       TO  SYS-1017-KOHHBTKBN-FLG
           MOVE    SPA-GMN-HKNINITKBN      TO  SYS-1017-HKNINITKBN-FLG
      *
           MOVE    SYS-1017-REC            TO  SYSKANRI-REC
      *
      *    ＤＢ更新処理
           PERFORM 4910-DBKOUSIN-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
           .
       490-KOUSHIN-1017-EXT.
           EXIT.
      *
      ******************************************************************
      *       登録処理
      *****************************************************************
       491-KOUSHIN-1038-SEC        SECTION.
      *
           MOVE    SPACE               TO  SYS-1038-REC
           INITIALIZE                      SYS-1038-REC
           MOVE    "1038"              TO  SYS-1038-KANRICD
           MOVE    "*"                 TO  SYS-1038-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1038-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1038-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1038-HOSPNUM
           MOVE    SYS-1038-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1038-REC
           ELSE
               MOVE    SPACE               TO  SYS-1038-REC
               INITIALIZE                      SYS-1038-REC
               MOVE    "1038"              TO  SYS-1038-KANRICD
               MOVE    "*"                 TO  SYS-1038-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1038-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-1038-EDYUKYMD
           MOVE    SPA-GMN-SRYNYKNKNRKBN   TO  SYS-1038-NYKNKNRKBN
           MOVE    SPA-GMN-KAGRPFLG        TO  SYS-1038-KAGRPFLG
           MOVE    SPA-GMN-HAKHOUFLG       TO  SYS-1038-HAKHOUFLG
           MOVE    SPA-GMN-TEIKINKBN       TO  SYS-1038-TEIKINKBN
           MOVE    SPA-GMN-YKJCHKKBN       TO  SYS-1038-YKJCHKKBN
           MOVE    SPA-GMN-OKUSURIKBN      TO  SYS-1038-OKUSURIKBN
           MOVE    SPA-GMN-DOSRYKBNFLG     TO  SYS-1038-DOSRYKBNFLG
           MOVE    SPA-GMN-WKSRYHYOFLG     TO  SYS-1038-WKSRYHYOFLG
           MOVE    SPA-GMN-FORMALFLG       TO  SYS-1038-FORMALFLG
           MOVE    SPA-GMN-AKUSEIHYO       TO  SYS-1038-AKUSEIHYOFLG
      *H24.4
           MOVE    SPA-GMN-GENERICFLG      TO  SYS-1038-GENERICFLG
      *H30.9
           MOVE    SPA-GMN-RECEKISAIFLG    TO  SYS-1038-RECEKISAIFLG
           MOVE    SPA-GMN-TEISEIFLG       TO  SYS-1038-TEISEIFLG
           MOVE    SPA-GMN-CHG310FLG       TO  SYS-1038-CHG310FLG
           MOVE    SPA-GMN-YKJOKUKBN       TO  SYS-1038-YKJOKUKBN
      *
           MOVE    SPA-GMN-CHGSRYSYU       TO  SYS-1038-CHGSRYSYU-FLG
           MOVE    SPA-GMN-RESPRV038       TO  SYS-1038-RESPRVFLG
           MOVE    SPA-GMN-HAIKIKBN        TO  SYS-1038-HAIKIKBN
           MOVE    SPA-GMN-NYKHNKKBN       TO  SYS-1038-NYKHNKKBN
           MOVE    SPA-GMN-SEI20KSNKBN     TO  SYS-1038-SEI20KSNKBN
      *H29.7
      *    心身療法２０未満加算区分
           MOVE    SPA-GMN-SIN20KSNKBN     TO  SYS-1038-SIN20KSNKBN
           MOVE    SPA-GMN-YMDHENKBN       TO  SYS-1038-YMDHENKBN
           MOVE    SPA-GMN-PRVLAYOUT038    TO  SYS-1038-PRVLAYOUT
           MOVE    SPA-GMN-SETALLKBN       TO  SYS-1038-SETALLKBN
      *
           MOVE    SPA-NAI-GMNHYOKBN (1)   TO  SYS-1038-GMNHYOKBN (1)
           MOVE    SPA-NAI-GMNHYOKBN (2)   TO  SYS-1038-GMNHYOKBN (2)
           MOVE    SPA-NAI-GMNHYOKBN (3)   TO  SYS-1038-GMNHYOKBN (3)
           MOVE    SPA-NAI-GMNHYOKBN (4)   TO  SYS-1038-GMNHYOKBN (4)
           MOVE    SPA-NAI-GMNHYOKBN (5)   TO  SYS-1038-GMNHYOKBN (5)
      *H28.10
           MOVE    SPA-NAI-GMNHYOKBN (6)   TO  SYS-1038-GMNHYOKBN-6
      *
           IF      SPA-GMN-BGCOLORS        =   "#000000"
               MOVE    SPACE               TO  SYS-1038-BGCOLORS
           ELSE
               MOVE    SPA-GMN-BGCOLORS    TO  SYS-1038-BGCOLORS
           END-IF
           MOVE    SPA-GMN-BGLINEKBN       TO  SYS-1038-BGLINEKBN
      *    環境設定背景色
           IF      SPA-GMN-BGCOLORSJ0      =   "#000000"
               MOVE    SPACE                   TO  SYS-1038-BGCOLORSJ0
           ELSE
               MOVE    SPA-GMN-BGCOLORSJ0      TO  SYS-1038-BGCOLORSJ0
           END-IF
           IF      SPA-GMN-BGCOLORSDAY     =   "#000000"
               MOVE    SPACE                   TO  SYS-1038-BGCOLORSDAY
           ELSE
               MOVE    SPA-GMN-BGCOLORSDAY     TO  SYS-1038-BGCOLORSDAY
           END-IF
      *    時間外区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   8   )
               IF      SPA-GMN-BGCOLORSJ (IDX) =   "#000000"
      *             クロは背景色なし
                   MOVE    SPACE               TO  SYS-1038-BGCOLORSJ
                                                            (IDX)
               ELSE
                   MOVE    SPA-GMN-BGCOLORSJ (IDX)
                                               TO  SYS-1038-BGCOLORSJ
                                                           (IDX)
               END-IF
           END-PERFORM
           IF      SPA-GMN-ERRCOLORS       =   "#000000"
               MOVE    SPACE                   TO  SYS-1038-ERRCOLORS
           ELSE
               MOVE    SPA-GMN-ERRCOLORS       TO  SYS-1038-ERRCOLORS
           END-IF
           IF      SPA-GMN-KEICOLORS       =   "#000000"
               MOVE    SPACE                   TO  SYS-1038-KEICOLORS
           ELSE
               MOVE    SPA-GMN-KEICOLORS       TO  SYS-1038-KEICOLORS
           END-IF
      *
           MOVE    SYS-1038-REC            TO  SYSKANRI-REC
      *
      *    ＤＢ更新処理
           PERFORM 4910-DBKOUSIN-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
           .
       490-KOUSHIN-1038-EXT.
           EXIT.
      ******************************************************************
      *       登録処理
      *****************************************************************
       491-KOUSHIN-1039-SEC        SECTION.
      *
           MOVE    SPACE               TO  SYS-1039-REC
           INITIALIZE                      SYS-1039-REC
           MOVE    "1039"              TO  SYS-1039-KANRICD
           MOVE    "*"                 TO  SYS-1039-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1039-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1039-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1039-HOSPNUM
           MOVE    SYS-1039-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1039-REC
           ELSE
               MOVE    SPACE               TO  SYS-1039-REC
               INITIALIZE                      SYS-1039-REC
               MOVE    "1039"              TO  SYS-1039-KANRICD
               MOVE    "*"                 TO  SYS-1039-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1039-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-1039-EDYUKYMD
           MOVE    SPA-GMN-SYUNYKNKNRKBN   TO  SYS-1039-NYKNKNRKBN
           MOVE    SPA-GMN-IKTSRYYMKBN     TO  SYS-1039-IKTSRYYMKBN
           MOVE    SPA-GMN-S06SKYPRT       TO  SYS-1039-S06SKYPRTGKBN
                                               SYS-1039-S06SKYPRTNKBN
           MOVE    SPA-GMN-S06SKYSUM       TO  SYS-1039-S06SKYSUMKBN
           MOVE    SPA-GMN-S02SKYSAI       TO  SYS-1039-S02SKYSAIKBN
           MOVE    SPA-GMN-SYUSKYPRTG      TO  SYS-1039-SKYPRTGKBN
           MOVE    SPA-GMN-SYUSKYPRTN      TO  SYS-1039-SKYPRTNKBN
           MOVE    SPA-GMN-SYUSRYPRTG      TO  SYS-1039-SRYPRTGKBN
           MOVE    SPA-GMN-SYUSRYPRTN      TO  SYS-1039-SRYPRTNKBN
           MOVE    SPA-GMN-SYUSTIME        TO  SYS-1039-STIME
      *
           MOVE    SPA-GMN-S02SKYMISYUPRT  TO SYS-1039-S02SKYMISYUPRTKBN
           MOVE    SPA-GMN-S03SKYMISYUPRT  TO SYS-1039-S03SKYMISYUPRTKBN
           MOVE    SPA-GMN-TUKIGRP         TO SYS-1039-TUKIGRPKBN
           MOVE    SPA-GMN-TUKISKYMISYUPRT
                                       TO SYS-1039-TUKISKYMISYUPRTKBN
           MOVE    SPA-GMN-TUKISUM         TO SYS-1039-TUKISUMKBN
           MOVE    SPA-GMN-S02MODE
                                       TO SYS-1039-S02MODE
           MOVE    SPA-GMN-S02LIMIT
                                       TO SYS-1039-S02LIMIT
           EVALUATE    SPA-GMN-S02SORTKBN
           WHEN    "0"
               MOVE    "0"             TO  SYS-1039-S02SORTKBN
           WHEN    "2"
               MOVE    "1"             TO  SYS-1039-S02SORTKBN
           WHEN    "1"
               MOVE    "2"             TO  SYS-1039-S02SORTKBN
           WHEN    "3"
               MOVE    "3"             TO  SYS-1039-S02SORTKBN
           WHEN    OTHER
               MOVE    "0"             TO  SYS-1039-S02SORTKBN
           END-EVALUATE
           MOVE    SPA-GMN-SAIKEISANSKYCHK
                                       TO SYS-1039-SAIKEISANSKYCHK
           MOVE    SPA-GMN-IKTSORTKBN
                                       TO SYS-1039-IKTSORTKBN
           MOVE    SPA-GMN-IKTKEISANKBN
                                       TO SYS-1039-IKTKEISANKBN
           MOVE    SPA-GMN-MISYUKIKANKBN
                                       TO SYS-1039-MISYUKIKANKBN
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   2 )
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   7 )
                   IF    ( SPA-GMN-SYUKARIDAY (IDX1 IDX2)  =   "T" )
                       MOVE    "1"         TO  SYS-1039-KARIDAY
                                                        (IDX1 IDX2)
                   ELSE
                       MOVE    SPACE       TO  SYS-1039-KARIDAY
                                                        (IDX1 IDX2)
                   END-IF
               END-PERFORM
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   2 )
                       MOVE    SPA-GMN-SYUKARISTTIME (IDX1 IDX2)
                                           TO  SYS-1039-KARISTTIME
                                                             (IDX1 IDX2)
                       MOVE    SPA-GMN-SYUKARIEDTIME (IDX1 IDX2)
                                           TO  SYS-1039-KARIEDTIME
                                                             (IDX1 IDX2)
               END-PERFORM
           END-PERFORM
      *
           MOVE    SPA-GMN-S04SHIHARAIKBN  TO  SYS-1039-S04SHIHARAIKBN
           MOVE    SPA-GMN-S04NYUGAIKBN    TO  SYS-1039-S04NYUGAIKBN
           MOVE    SPA-GMN-S04SRYKBN       TO  SYS-1039-S04SRYKBN
           MOVE    SPA-GMN-S04KIKANKBN     TO  SYS-1039-S04KIKANKBN
           MOVE    SPA-GMN-S04DSPKBN       TO  SYS-1039-S04DSPKBN
           MOVE    SPA-GMN-S04SYORIKBN     TO  SYS-1039-S04SYORIKBN
           MOVE    SPA-GMN-S04JIHIPRTKBN   TO  SYS-1039-S04JIHIPRTKBN
           MOVE    SPA-GMN-S04SUMKBN    TO  SYS-1039-S04SUMKBN
      *
           MOVE    SYS-1039-REC            TO  SYSKANRI-REC
      *
      *    ＤＢ更新処理
           PERFORM 4910-DBKOUSIN-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
           .
       490-KOUSHIN-1039-EXT.
           EXIT.
      ******************************************************************
      *       登録処理
      *****************************************************************
       491-KOUSHIN-1042-SEC        SECTION.
      *
           MOVE    SPACE               TO  SYS-1042-REC
           INITIALIZE                      SYS-1042-REC
           MOVE    "1042"              TO  SYS-1042-KANRICD
           MOVE    "*"                 TO  SYS-1042-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1042-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1042-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1042-HOSPNUM
           MOVE    SYS-1042-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1042-REC
           ELSE
               MOVE    SPACE               TO  SYS-1042-REC
               INITIALIZE                      SYS-1042-REC
               MOVE    "1042"              TO  SYS-1042-KANRICD
               MOVE    "*"                 TO  SYS-1042-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1042-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-1042-EDYUKYMD
           MOVE    SPA-GMN-MSG1-DSPKBN     TO  SYS-1042-MSG1-DSPKBN
           MOVE    SPA-GMN-MSG2-DSPKBN     TO  SYS-1042-MSG2-DSPKBN
           MOVE    SPA-GMN-MSG3-DSPKBN     TO  SYS-1042-MSG3-DSPKBN
           MOVE    SPA-GMN-MSG4-DSPKBN     TO  SYS-1042-MSG4-DSPKBN
           MOVE    SPA-GMN-MSG5-DSPKBN     TO  SYS-1042-MSG5-DSPKBN
           MOVE    SPA-GMN-SRYKA-DSPKBN    TO  SYS-1042-SRYKA-DSPKBN
           MOVE    SPA-GMN-SYOKBN-DSPKBN   TO  SYS-1042-SYOKBN-DSPKBN
           MOVE    SPA-GMN-UTAGAIFLG-KBN   TO  SYS-1042-UTAGAIFLG-KBN
           MOVE    SPA-GMN-SJKBN           TO  SYS-1042-SJKBN-SET
      *
           MOVE    SYS-1042-REC            TO  SYSKANRI-REC
      *
      *    ＤＢ更新処理
           PERFORM 4910-DBKOUSIN-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
           .
       491-KOUSHIN-1042-EXT.
           EXIT.
      ******************************************************************
      *       登録処理
      *****************************************************************
       493-KOUSHIN-1043-SEC        SECTION.
      *
           MOVE    SPACE               TO  SYS-1043-REC
           INITIALIZE                      SYS-1043-REC
           MOVE    "1043"              TO  SYS-1043-KANRICD
           MOVE    "*"                 TO  SYS-1043-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1043-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1043-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1043-HOSPNUM
           MOVE    SYS-1043-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1043-REC
           ELSE
               MOVE    SPACE               TO  SYS-1043-REC
               INITIALIZE                      SYS-1043-REC
               MOVE    "1043"              TO  SYS-1043-KANRICD
               MOVE    "*"                 TO  SYS-1043-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1043-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-1043-EDYUKYMD
           MOVE    SPA-GMN-SRYKBN-FLG      TO  SYS-1043-SRYKBN-FLG
           MOVE    SPA-GMN-SRYKA-FLG       TO  SYS-1043-SRYKA-FLG
           MOVE    SPA-GMN-RESPRV043       TO  SYS-1043-RESPRVFLG
           MOVE    SPA-GMN-PRVLAYOUT043    TO  SYS-1043-PRVLAYOUT
      *
           MOVE    SYS-1043-REC            TO  SYSKANRI-REC
      *
      *    ＤＢ更新処理
           PERFORM 4910-DBKOUSIN-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
           .
       493-KOUSHIN-1043-EXT.
           EXIT.
      ******************************************************************
      *       登録処理
      *****************************************************************
       493-KOUSHIN-1045-SEC        SECTION.
      *
           MOVE    SPACE               TO  SYS-1045-REC
           INITIALIZE                      SYS-1045-REC
           MOVE    "1045"              TO  SYS-1045-KANRICD
           MOVE    "*"                 TO  SYS-1045-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1045-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1045-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1045-HOSPNUM
           MOVE    SYS-1045-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1045-REC
           ELSE
               MOVE    SPACE               TO  SYS-1045-REC
               INITIALIZE                      SYS-1045-REC
               MOVE    "1045"              TO  SYS-1045-KANRICD
               MOVE    "*"                 TO  SYS-1045-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1045-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-1045-EDYUKYMD
           MOVE    "1"                     TO  SYS-1045-INIT-DRCD(1:1)
           MOVE    SPA-GMN-INIT-DRCD       TO  SYS-1045-INIT-DRCD(2:4)
           MOVE    SPA-GMN-YYKINIT-FLG     TO  SYS-1045-YYKINIT-FLG
           MOVE    SPA-GMN-YYKHYO-KBN      TO  SYS-1045-YYKHYO-KBN
           MOVE    SPA-GMN-YYKMAXCNT       TO  SYS-1045-YYKMAXCNT
           MOVE    SPA-GMN-YYKUKE-KBN      TO  SYS-1045-YYKUKE-KBN
           MOVE    SPA-GMN-YYKPTNUM-KBN    TO  SYS-1045-YYKPTNUM-KBN
           MOVE    SPA-GMN-UKEHKNCHK-KBN   TO  SYS-1045-UKEHKNCHK-KBN
      *
           MOVE    SYS-1045-REC            TO  SYSKANRI-REC
      *
      *    ＤＢ更新処理
           PERFORM 4910-DBKOUSIN-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
           .
       493-KOUSHIN-1045-EXT.
           EXIT.
      *H26.12
      ******************************************************************
      *       登録処理（点数算定基準）
      *****************************************************************
       493-KOUSHIN-1201-SEC        SECTION.
      *
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1201"              TO  SYS-KANRICD
           MOVE    "A0000001"          TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        NOT =   ZERO
               MOVE    SPACE               TO  SYSKANRI-REC
               INITIALIZE                      SYSKANRI-REC
               MOVE    "1201"              TO  SYS-KANRICD
               MOVE    "A0000001"          TO  SYS-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD        TO  SYS-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-EDYUKYMD
      *    妥結率区分
           MOVE    SPA-GMN-DAKETUKBN       TO  SYS-KANRITBL(1:1)
      *
      *    ＤＢ更新処理
           PERFORM 4910-DBKOUSIN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      493-KOUSHIN-1201-EXT
           END-IF
      *
      *    低紹介率区分
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1202"              TO  SYS-KANRICD
           MOVE    "A0000001"          TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        NOT =   ZERO
               MOVE    SPACE               TO  SYSKANRI-REC
               INITIALIZE                      SYSKANRI-REC
               MOVE    "1202"              TO  SYS-KANRICD
               MOVE    "A0000001"          TO  SYS-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD        TO  SYS-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-EDYUKYMD
      *    低紹介率区分
           MOVE    SPA-GMN-TEIRITUKBN      TO  SYS-KANRITBL(1:1)
      *
      *    ＤＢ更新処理
           PERFORM 4910-DBKOUSIN-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
           .
       493-KOUSHIN-1201-EXT.
           EXIT.
      *
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC           SECTION.
      *
           MOVE    SPACE                TO  SPA-ERRCD
           MOVE    SPA-GMN-CUR          TO  WRK-GMN-CUR
      *
           PERFORM 4100-DATA-CHK-SEC
      *
           IF      SPA-ERRCD            =   SPACE
               IF     (SPA-GMN-DATA (3)     =   "1"  OR  "2" )
               AND    (SPA-GMN-DATA (4)     =   "1"  OR  "2" )
                   MOVE    "0005"               TO  SPA-ERRCD
                   MOVE    4                    TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *    時間外チェックありの時、ゼロはエラー
           IF     (SPA-ERRCD               =   SPACE  )  AND
                  (SPA-GMN-JIKANTOKU-CHK   =   "1"    )
               IF     (SPA-NAI-JIKANCHK-WEEK(1)    =   ZERO )  AND
                      (SPA-NAI-JIKANCHK-WEEK(2)    =   ZERO )  AND
                      (SPA-NAI-JIKANCHK-SAT (1)    =   ZERO )  AND
                      (SPA-NAI-JIKANCHK-SAT (2)    =   ZERO )
                   MOVE    "0033"              TO  SPA-ERRCD
                   MOVE    51                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           PERFORM VARYING     IDT     FROM    1   BY  1
                   UNTIL      (IDT     >   2   )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF    ((SPA-NAI-JIKANCHK-WST(IDT)   >   ZERO  ) OR
                      (SPA-NAI-JIKANCHK-WED(IDT)   >   ZERO  )) AND
                      (SPA-NAI-JIKANCHK-WST(IDT)
                                           >  SPA-NAI-JIKANCHK-WED(IDT))
                   MOVE    "0032"              TO  SPA-ERRCD
                   MOVE    51                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
               IF    ((SPA-NAI-JIKANCHK-SST(IDT)   >   ZERO  ) OR
                      (SPA-NAI-JIKANCHK-SED(IDT)   >   ZERO  )) AND
                      (SPA-NAI-JIKANCHK-SST(IDT)
                                           >  SPA-NAI-JIKANCHK-SED(IDT))
                   MOVE    "0032"              TO  SPA-ERRCD
                   MOVE    59                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
           END-PERFORM
      *
      *
           IF      SPA-ERRCD            =   SPACE
               MOVE    WRK-GMN-CUR          TO  SPA-GMN-CUR
           END-IF
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       入力項目の関連チェック処理（収納機能情報）
      *****************************************************************
       4904-KANRENCHK-SEC          SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   2 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   2 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                   COMPUTE WRK-ERR-CUR
                       =   400
                       + ( IDX1    -   1 ) *  8
                       + ( IDX2    -   1 ) *  4
      *
                   IF    ( SPA-GMN-SYUKARISTTIME (IDX1 IDX2)
                           >       SPA-GMN-SYUKARIEDTIME (IDX1 IDX2) )
      *
                       MOVE    WRK-ERR-CUR TO  SPA-GMN-CUR
                       MOVE    "3101"      TO  SPA-ERRCD
      *
                   END-IF
      *
               END-PERFORM
           END-PERFORM
      *
           .
       4904-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
           PERFORM 510-GSRAUTH-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W07    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE           TO  W07
           INITIALIZE                  W07
      *
           MOVE    SPA-GMN-STYUKYMD    TO  W07-YUKOSTYMD
           MOVE    SPA-GMN-EDYUKYMD    TO  W07-YUKOEDYMD
      *
      **** PERFORM VARYING IDX     FROM    1   BY  1
      *            UNTIL   IDX     >       7 
      *        MOVE    SPA-GMN-DATA-G(IDX)     TO  W07-DATA(IDX)
      *        PERFORM VARYING IDY     FROM    1   BY  1
      *                UNTIL   IDY     >       2
      *            MOVE    SPA-GMN-DATALST(IDY)
      *                                TO  W07-DATALST(IDX IDY)
      *        END-PERFORM
      *        MOVE    SPA-GMN-DATA-MAX    TO  W07-COUNT(IDX)
      **** END-PERFORM
           MOVE    SPA-GMN-DATA-G(01)      TO  W07-DATA01
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX
               MOVE    SPA-GMN-DATALST(IDY)
                                       TO  W07-DATA01-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX    TO  W07-DATA01-COUNT
      *
           MOVE    SPA-GMN-DATA-G(02)      TO  W07-DATA02
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX2
               MOVE    SPA-GMN-DATALST2(IDY)
                                       TO  W07-DATA02-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX2   TO  W07-DATA02-COUNT
      *
           MOVE    SPA-GMN-DATA-G(03)      TO  W07-DATA03
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX2
               MOVE    SPA-GMN-DATALST2(IDY)
                                       TO  W07-DATA03-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX2   TO  W07-DATA03-COUNT
      *
           MOVE    SPA-GMN-DATA-G(04)      TO  W07-DATA04
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX2
               MOVE    SPA-GMN-DATALST2(IDY)
                                       TO  W07-DATA04-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX2   TO  W07-DATA04-COUNT
      *
           MOVE    SPA-GMN-DATA-G(05)      TO  W07-DATA05
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX
               MOVE    SPA-GMN-DATALST(IDY)
                                       TO  W07-DATA05-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX    TO  W07-DATA05-COUNT
      *
           MOVE    SPA-GMN-DATA-G(06)      TO  W07-DATA06
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX
               MOVE    SPA-GMN-DATALST(IDY)
                                       TO  W07-DATA06-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX    TO  W07-DATA06-COUNT
      *
           MOVE    SPA-GMN-DATA-G(07)      TO  W07-DATA07
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX
               MOVE    SPA-GMN-DATALST(IDY)
                                       TO  W07-DATA07-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX    TO  W07-DATA07-COUNT
      *
           MOVE    SPA-GMN-SRYKACD     TO  W07-SRYKACD
           MOVE    SPA-GMN-SRYKAMEI    TO  W07-SRYKAMEI
      *    入院初診料
           MOVE    SPA-GMN-NYUSYOSIN-G TO  W07-NYUSYOSIN
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       2
               MOVE    SPA-GMN-DATALST(IDY)
                                       TO  W07-NYUSYOSIN-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX    TO  W07-NYUSYOSIN-COUNT
      *
      *    時間外加算特例乳幼児
           MOVE    SPA-GMN-JIKANTOKU-NYU-G TO  W07-JIKANTOKUNYU
      *    PERFORM VARYING IDY     FROM    1   BY  1
      *            UNTIL   IDY     >       SPA-GMN-DATA-MAX
      *        MOVE    SPA-GMN-DATALST(IDY)
      *                                TO  W07-JIKANTOKUNYU-ITEM(IDY)
      *    END-PERFORM
      ***  MOVE    SPA-GMN-DATA-MAX    TO  W07-JIKANTOKUNYU-COUNT
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-JIKANTOKU-MAX
               MOVE    SPA-GMN-JIKANTOKU-NYU-LIST(IDY)
                                       TO  W07-JIKANTOKUNYU-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-JIKANTOKU-MAX    TO  W07-JIKANTOKUNYU-COUNT
      *
      *    画像診断管理加算１（写真診断）
           MOVE    SPA-GMN-GAZOUKSN1-G TO  W07-GAZOUKSN1
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX
               MOVE    SPA-GMN-DATALST(IDY)
                                       TO  W07-GAZOUKSN1-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX    TO  W07-GAZOUKSN1-COUNT
      *
      *    中途終了展開時の自動発生
           MOVE    SPA-GMN-CHUTOTENKAI-G   TO  W07-CHUTOTENKAI
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX
               MOVE    SPA-GMN-DATALST(IDY)
                                       TO  W07-CHUTOTENKAI-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX    TO  W07-CHUTOTENKAI-COUNT
      *
      *    訂正時の自動発生
           MOVE    SPA-GMN-TEISEIAUTO-G    TO  W07-TEISEI
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX
               MOVE    SPA-GMN-DATALST(IDY)
                                       TO  W07-TEISEI-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX    TO  W07-TEISEI-COUNT
      *
      *    訂正時の自動発生（特定疾患処方管理）
           MOVE    SPA-GMN-TEISEITOKU-G    TO  W07-TEISEITOKU
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX
               MOVE    SPA-GMN-DATALST(IDY)
                                       TO  W07-TEISEITOKU-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX    TO  W07-TEISEITOKU-COUNT
      *
      *H29.3
      *    訂正時の自動発生（特定疾患処方管理）
           MOVE    SPA-GMN-TEISHOHOU-G    TO  W07-TEISHOHOU
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-TEISHOHOU-MAX
               MOVE    SPA-GMN-TEISHOHOU-LIST(IDY)
                                       TO  W07-TEISHOHOU-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-TEISHOHOU-MAX   TO  W07-TEISHOHOU-COUNT
      *
           MOVE  SPA-GMN-LAST-KIKAN    TO  W07-LASTKIKAN
      *
           MOVE  SPA-GMN-INTERACT-KIKAN    TO  W07-KIKAN
      *H28.8
      *    相互作用チェック区分
           MOVE    SPA-GMN-KINKCHK-G   TO  W07-KINKCHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-KINKCHK-MAX
               MOVE    SPA-GMN-KINKCHK-LIST (IDX)
                                       TO  W07-KINKCHK-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-KINKCHK-MAX TO  W07-KINKCHK-COUNT
      *
      *    外来管理加算チェック
           MOVE    SPA-GMN-GAIKANRIKBN-G   TO  W07-GAIKANKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GAIKANRIKBN-MAX
               MOVE    SPA-GMN-GAIKANRIKBN-LIST (IDX)
                                       TO  W07-GAIKANKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GAIKANRIKBN-MAX TO  W07-GAIKANKBN-COUNT
      *
      *    外来カーソル移動
           MOVE    SPA-GMN-GAIMOVE-G       TO  W07-GAIMOVE
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-GAIMOVEMAX
               MOVE    SPA-GMN-GAIMOVE-LIST (IDX)
                                       TO  W07-GAIMOVE-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-GAIMOVEMAX  TO  W07-GAIMOVE-COUNT
      *
      *    数量ゼロ入力
           MOVE    SPA-GMN-SURYOZERO-G TO  W07-SURYOZERO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-SURYOZERO-LIST (IDX)
                                       TO  W07-SURYOZERO-ITEM(IDX)
           END-PERFORM
           MOVE    2                   TO  W07-SURYOZERO-COUNT
      *
      *    入院調基チェック
           MOVE    SPA-GMN-CHOKICHK-G  TO  W07-CHOKICHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-CHOKICHK-LIST (IDX)
                                       TO  W07-CHOKICHK-ITEM(IDX)
           END-PERFORM
           MOVE    2                   TO  W07-CHOKICHK-COUNT
      *
      *    クリア確認チェック
           MOVE    SPA-GMN-CLEARCHK-G  TO  W07-CLEARCHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-CLEARCHK-LIST (IDX)
                                       TO  W07-CLEARCHK-ITEM(IDX)
           END-PERFORM
           MOVE    2                   TO  W07-CLEARCHK-COUNT
      *    薬剤情報提供料チェック
           MOVE    SPA-GMN-YAKJYOCHK-G  TO  W07-YAKJYOCHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-YAKJYOCHK-LIST (IDX)
                                       TO  W07-YAKJYOCHK-ITEM(IDX)
           END-PERFORM
           MOVE    2                   TO  W07-YAKJYOCHK-COUNT
      *    前回保険組合せ相違チェック
           MOVE    SPA-GMN-ZENHKNCHK-G  TO  W07-ZENHKNCHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-ZENHKNCHK-MAX
               MOVE    SPA-GMN-ZENHKNCHK-LIST (IDX)
                                       TO  W07-ZENHKNCHK-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-ZENHKNCHK-MAX   TO  W07-ZENHKNCHK-COUNT
      *    手帳加算確認チェック
           MOVE    SPA-GMN-TECHOKBN-G  TO  W07-TECHOKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TECHOKBN-MAX
               MOVE    SPA-GMN-TECHOKBN-LIST (IDX)
                                       TO  W07-TECHOKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-TECHOKBN-MAX    TO  W07-TECHOKBN-COUNT
      *
      *    時間外緊急院内チェック
           MOVE    SPA-GMN-JGAIKEIKBN-G  TO  W07-JGAIKEIKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-JGAIKEIKBN-MAX
               MOVE    SPA-GMN-JGAIKEIKBN-LIST (IDX)
                                       TO  W07-JGAIKEIKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-JGAIKEIKBN-MAX  TO  W07-JGAIKEIKBN-COUNT
      *
      *    薬剤投与量チェック区分
           MOVE    SPA-GMN-TOUYOCHK-G  TO  W07-TOUYOCHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TOUYOCHK-MAX
               MOVE    SPA-GMN-TOUYOCHK-LIST (IDX)
                                       TO  W07-TOUYOCHK-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-TOUYOCHK-MAX  TO  W07-TOUYOCHK-COUNT
      *
      *    投薬３０日超チェック
           MOVE    SPA-GMN-SHOHOU30CHK-G     TO  W07-SHOHOU30CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SHOHOU30CHK-MAX
               MOVE    SPA-GMN-SHOHOU30CHK-LIST (IDX)
                                       TO  W07-SHOHOU30CHK-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SHOHOU30CHK-MAX  TO  W07-SHOHOU30CHK-COUNT
      *
      *    包括入力
           MOVE    SPA-GMN-HOUKATU-G       TO  W07-HOUKATU
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-HOUKATU-LIST (IDX)
                                       TO  W07-HOUKATU-ITEM(IDX)
           END-PERFORM
           MOVE    2                   TO  W07-HOUKATU-COUNT
      *    病名表示区分
           MOVE    SPA-GMN-BYOHYOKBN-G TO  W07-BYOHYOKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-BYOHYOKBN-MAX
               MOVE    SPA-GMN-BYOHYOKBN-LIST (IDX)
                                       TO  W07-BYOHYOKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-BYOHYOKBN-MAX   TO  W07-BYOHYOKBN-COUNT
      *    剤登録順区分
           MOVE    SPA-GMN-ZAIJUNKBN-G TO  W07-ZAIJUNKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-ZAIJUNKBN-LIST (IDX)
                                       TO  W07-ZAIJUNKBN-ITEM(IDX)
           END-PERFORM
           MOVE    2                   TO  W07-ZAIJUNKBN-COUNT
      *
      *    検体検査加算自動発生
           MOVE    SPA-GMN-KENSAAUTO-G TO  W07-KENSAAUTO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KENSAAUTO-MAX
               MOVE    SPA-GMN-KENSAAUTO-LIST (IDX)
                                       TO  W07-KENSAAUTO-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-KENSAAUTO-MAX   TO  W07-KENSAAUTO-COUNT
      *
      *    訂正時の自動発生（入院）
           MOVE    SPA-GMN-TEINYUAUTO-G    TO  W07-TEINYUAUTO
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX
               MOVE    SPA-GMN-DATALST(IDY)
                                       TO  W07-TEINYUAUTO-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX    TO  W07-TEINYUAUTO-COUNT
      *    病名疾患区分からの自動発生
           MOVE    SPA-GMN-BYOMEIAUTO-G    TO  W07-BYOMEIAUTO
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX
               MOVE    SPA-GMN-DATALST(IDY)
                                       TO  W07-BYOMEIAUTO-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX    TO  W07-BYOMEIAUTO-COUNT
      *    入院時初回加算の自動発生
           MOVE    SPA-GMN-NYUKENSA-G  TO  W07-NYUKENSA
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX
               MOVE    SPA-GMN-DATALST(IDY)
                                       TO  W07-NYUKENSA-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX    TO  W07-NYUKENSA-COUNT
      *    初診再診料の自動発生
           MOVE    SPA-GMN-INITAUTO-G    TO  W07-INITAUTO
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-INITAUTO-MAX
               MOVE    SPA-GMN-INITAUTO-LIST(IDY)
                                       TO  W07-INITAUTO-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-INITAUTO-MAX    TO  W07-INITAUTO-COUNT
      *
      *    時間外加算チェック
           MOVE    SPA-GMN-JIKANTOKU-CHK-G TO  W07-JIKANTOKU-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-JIKANTOKU-CHK-LIST (IDX)
                                       TO  W07-JIKANTOKU-CHK-ITEM(IDX)
           END-PERFORM
           MOVE    2                   TO  W07-JIKANTOKU-CHK-COUNT
      *    チェック除外時間
           IF      SPA-GMN-JIKANTOKU-CHK   =   "0"
               CONTINUE
           ELSE
               PERFORM VARYING     IDX     FROM    1   BY  1
                        UNTIL       IDX     >   2
                   MOVE    SPA-GMN-JIKANWST-HH(IDX)
                                           TO  W07-JIKANW1-HH (IDX)
                   MOVE    SPA-GMN-JIKANWST-MM(IDX)
                                           TO  W07-JIKANW1-MM (IDX)
                   MOVE    SPA-GMN-JIKANWED-HH(IDX)
                                           TO  W07-JIKANW2-HH (IDX)
                   MOVE    SPA-GMN-JIKANWED-MM(IDX)
                                           TO  W07-JIKANW2-MM (IDX)
      *
                   MOVE    SPA-GMN-JIKANSST-HH(IDX)
                                           TO  W07-JIKANS1-HH (IDX)
                   MOVE    SPA-GMN-JIKANSST-MM(IDX)
                                           TO  W07-JIKANS1-MM (IDX)
                   MOVE    SPA-GMN-JIKANSED-HH(IDX)
                                           TO  W07-JIKANS2-HH (IDX)
                   MOVE    SPA-GMN-JIKANSED-MM(IDX)
                                           TO  W07-JIKANS2-MM (IDX)
               END-PERFORM
           END-IF
      *
      *    カルテの病名対象
           MOVE    SPA-GMN-BYOMEIKBN-G     TO  W07-BYOMEIKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               MOVE    SPA-GMN-BYOMEIKBN-LIST (IDX)
                                       TO  W07-BYOMEIKBN-ITEM(IDX)
           END-PERFORM
           MOVE    3               TO  W07-BYOMEIKBN-COUNT
      *
      *    人名辞書使用
           MOVE    SPA-GMN-JISYOKBN-G      TO  W07-JISYOKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-JISYOKBN-LIST (IDX)
                                       TO  W07-JISYOKBN-ITEM(IDX)
           END-PERFORM
           MOVE    2               TO  W07-JISYOKBN-COUNT
      *
      *    患者登録のカルテ発行
           MOVE    SPA-GMN-HAKKOKBN-G      TO  W07-HAKKOKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               MOVE    SPA-GMN-HAKKOKBN-LIST (IDX)
                                       TO  W07-HAKKOKBN-ITEM(IDX)
           END-PERFORM
           MOVE    3               TO  W07-HAKKOKBN-COUNT
      *
      *    自費保険の補助区分
           MOVE    SPA-GMN-HOJYOKBN-G      TO  W07-HOJYOKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-HOJYOKBN-LIST (IDX)
                                       TO  W07-HOJYOKBN-ITEM(IDX)
           END-PERFORM
           MOVE    2               TO  W07-HOJYOKBN-COUNT
      *
      *    同姓同名チェック
           MOVE    SPA-GMN-DOUMEI-CHKFLG-G     TO  W07-DOUMEI-CHKFLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-DOUMEI-CHKFLG-LIST (IDX)
                                       TO  W07-DOUMEI-CHKFLG-ITEM(IDX)
           END-PERFORM
           MOVE    2               TO  W07-DOUMEI-CHKFLG-COUNT
      *
      *    二重登録疑いチェック
           MOVE    SPA-GMN-JYUFUKU-CHKKBN-G    TO  W07-JYUFUKU-CHKKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-JYUFUKU-MAX
               MOVE    SPA-GMN-JYUFUKU-CHKKBN-LIST (IDX)
                                       TO  W07-JYUFUKU-CHKKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-JYUFUKU-MAX TO  W07-JYUFUKU-CHKKBN-COUNT
      *    地方公費単独チェック
           MOVE    SPA-GMN-KOHONLY-CHKFLG-G    TO  W07-KOHONLY-CHKFLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-KOHONLY-MAX
               MOVE    SPA-GMN-KOHONLY-CHKFLG-LIST (IDX)
                                       TO  W07-KOHONLY-CHKFLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-KOHONLY-MAX TO  W07-KOHONLY-CHKFLG-COUNT
      *    患者情報削除機能
           MOVE    SPA-GMN-PTNUMDEL-FLG-G  TO  W07-PTNUMDEL-FLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-PTNUMDEL-MAX
               MOVE    SPA-GMN-PTNUMDEL-FLG-LIST (IDX)
                                       TO  W07-PTNUMDEL-FLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-PTNUMDEL-MAX TO  W07-PTNUMDEL-FLG-COUNT
      *    本人家族区分
           MOVE    SPA-GMN-HONKZKKBN-FLG-G  TO  W07-HONKZKKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-HONKZKKBN-MAX
               MOVE    SPA-GMN-HONKZKKBN-FLG-LIST (IDX)
                                       TO  W07-HONKZKKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-HONKZKKBN-MAX   TO  W07-HONKZKKBN-COUNT
      *    家族年齢
           MOVE    SPA-GMN-KZKNEN      TO  W07-KZKNEN
      *    被保険者名設定区分
           MOVE    SPA-GMN-HIHKNJAKBN-FLG-G    TO  W07-HIHKNJAKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-HIHKNJAKBN-MAX
               MOVE    SPA-GMN-HIHKNJAKBN-FLG-LIST (IDX)
                                       TO  W07-HIHKNJAKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-HIHKNJAKBN-MAX  TO  W07-HIHKNJAKBN-COUNT
      *    世帯主名自動記載区分
           MOVE    SPA-GMN-SETAIKBN-FLG-G  TO  W07-SETAIKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SETAIKBN-MAX
               MOVE    SPA-GMN-SETAIKBN-FLG-LIST (IDX)
                                       TO  W07-SETAIKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SETAIKBN-MAX    TO  W07-SETAIKBN-COUNT
      *    グループ医療機関患者リスト
           MOVE    SPA-GMN-GRPPTLST-FLG-G  TO  W07-GRPPTLST
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-GRPPTLST-MAX
               MOVE    SPA-GMN-GRPPTLST-FLG-LIST (IDX)
                                       TO  W07-GRPPTLST-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-GRPPTLST-MAX    TO  W07-GRPPTLST-COUNT
      *    公費上限額履歴チェック区分
           MOVE    SPA-GMN-KOHRRKKBN-FLG-G  TO  W07-KOHRRK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-KOHRRKKBN-MAX
               MOVE    SPA-GMN-KOHRRKKBN-FLG-LIST (IDX)
                                       TO  W07-KOHRRK-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-KOHRRKKBN-MAX   TO  W07-KOHRRK-COUNT
      *    郵便番号自動記載区分
           MOVE    SPA-GMN-POSTKBN-FLG-G   TO  W07-POSTKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-POSTKBN-MAX
               MOVE    SPA-GMN-POSTKBN-FLG-LIST (IDX)
                                           TO  W07-POSTKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-POSTKBN-MAX     TO  W07-POSTKBN-COUNT
      *    都道府県名記載区分
           MOVE    SPA-GMN-KENMEIKBN-FLG-G TO  W07-KENMEIKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-KENMEIKBN-MAX
               MOVE    SPA-GMN-KENMEIKBN-FLG-LIST (IDX)
                                           TO  W07-KENMEIKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-KENMEIKBN-MAX   TO  W07-KENMEIKBN-COUNT
      *    京都府通り名称編集区分
           MOVE    SPA-GMN-TOWNNAME2KBN-G  TO  W07-TOWNNAME2KBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TOWNNAME2KBN-MAX
               MOVE    SPA-GMN-TOWNNAME2KBN-LIST (IDX)
                                       TO  W07-TOWNNAME2KBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-TOWNNAME2KBN-MAX   TO  W07-TOWNNAME2KBN-COUNT
      *    公費法別一覧表示区分
           MOVE    SPA-GMN-KOHHBTKBN-G     TO  W07-KOHHBTKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-KOHHBTKBN-MAX
               MOVE    SPA-GMN-KOHHBTKBN-LIST (IDX)
                                       TO  W07-KOHHBTKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-KOHHBTKBN-MAX   TO  W07-KOHHBTKBN-COUNT
      *    保険初期表示区分
           MOVE    SPA-GMN-HKNINITKBN-G    TO  W07-HKNINITKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-HKNINITKBN-MAX
               MOVE    SPA-GMN-HKNINITKBN-LIST (IDX)
                                       TO  W07-HKNINITKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-HKNINITKBN-MAX  TO  W07-HKNINITKBN-COUNT
      *
      *    入金管理区分（診療行為機能情報）
           MOVE    SPA-GMN-SRYNYKNKNRKBN-G TO  W07-SRYNYKNKNRKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYNYKNKNRKBN-MAX
               MOVE    SPA-GMN-SRYNYKNKNRKBNLST (IDX)
                                       TO  W07-SRYNYKNKNRKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SRYNYKNKNRKBN-MAX
                                       TO  W07-SRYNYKNKNRKBN-COUNT
      *
      *    請求書発行方法（診療行為機能情報）
           MOVE    SPA-GMN-HAKHOUFLG-G TO  W07-HAKHOUFLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-HAKHOUFLG-MAX
               MOVE    SPA-GMN-HAKHOUFLG-LST (IDX)
                                       TO  W07-HAKHOUFLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-HAKHOUFLG-MAX
                                       TO  W07-HAKHOUFLG-COUNT
      *    訂正請求書金額選択（診療行為機能情報）
           MOVE    SPA-GMN-TEIKINKBN-G TO  W07-TEIKINKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TEIKINKBN-MAX
               MOVE    SPA-GMN-TEIKINKBN-LST (IDX)
                                       TO  W07-TEIKINKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-TEIKINKBN-MAX
                                       TO  W07-TEIKINKBN-COUNT
      *    薬剤情報提供発行（診療行為機能情報）
           MOVE    SPA-GMN-YKJCHKKBN-G TO  W07-YKJCHKKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-YKJCHKKBN-MAX
               MOVE    SPA-GMN-YKJCHKKBN-LST (IDX)
                                       TO  W07-YKJCHKKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-YKJCHKKBN-MAX
                                       TO  W07-YKJCHKKBN-COUNT
      *    お薬手帳発行（診療行為機能情報）
           MOVE    SPA-GMN-OKUSURIKBN-G    TO  W07-OKUSURIKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-OKUSURIKBN-MAX
               MOVE    SPA-GMN-OKUSURIKBN-LST (IDX)
                                       TO  W07-OKUSURIKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-OKUSURIKBN-MAX
                                       TO  W07-OKUSURIKBN-COUNT
      *    ＤＯ初期表示（診療行為機能情報）
           MOVE    SPA-GMN-DOSRYKBNFLG-G   TO  W07-DOSRYKBN-FLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-DOSRYKBNFLG-MAX
               MOVE    SPA-GMN-DOSRYKBNFLG-LST (IDX)
                                       TO  W07-DOSRYKBN-FLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-DOSRYKBNFLG-MAX
                                       TO  W07-DOSRYKBN-FLG-COUNT
      *    中途表示一覧（診療行為機能情報）
           MOVE    SPA-GMN-WKSRYHYOFLG-G   TO  W07-WKSRYHYO-FLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-WKSRYHYOFLG-MAX
               MOVE    SPA-GMN-WKSRYHYOFLG-LST (IDX)
                                       TO  W07-WKSRYHYO-FLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-WKSRYHYOFLG-MAX
                                       TO  W07-WKSRYHYO-FLG-COUNT
      *    検査正式名称表示（診療行為機能情報）
           MOVE    SPA-GMN-FORMALFLG-G   TO  W07-FORMAL-FLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-FORMALFLG-MAX
               MOVE    SPA-GMN-FORMALFLG-LST (IDX)
                                       TO  W07-FORMAL-FLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-FORMALFLG-MAX
                                       TO  W07-FORMAL-FLG-COUNT
      *    腫瘍マーカ一覧表示（診療行為機能情報）
           MOVE    SPA-GMN-AKUSEIHYO-G   TO  W07-AKUSEIHYO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-AKUSEIHYO-MAX
               MOVE    SPA-GMN-AKUSEIHYO-LST (IDX)
                                       TO  W07-AKUSEIHYO-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-AKUSEIHYO-MAX
                                       TO  W07-AKUSEIHYO-COUNT
      *H30.9
      *    選択式コメント一覧表示（診療行為機能情報）
           MOVE    SPA-GMN-RECEKISAIFLG-G   TO  W07-RECEKISAI-FLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-RECEKISAIFLG-MAX
               MOVE    SPA-GMN-RECEKISAIFLG-LST (IDX)
                                       TO  W07-RECEKISAI-FLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-RECEKISAIFLG-MAX
                                       TO  W07-RECEKISAI-FLG-COUNT
      *H24.4
      *    確認画面名称表示区分（診療行為機能情報）
           MOVE    SPA-GMN-GENERICFLG-G   TO  W07-GENERIC-FLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-GENERICFLG-MAX
               MOVE    SPA-GMN-GENERICFLG-LST (IDX)
                                       TO  W07-GENERIC-FLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-GENERICFLG-MAX
                                       TO  W07-GENERIC-FLG-COUNT
      *
      *    同日再診・訂正選択（診療行為機能情報）
           MOVE    SPA-GMN-TEISEIFLG-G   TO  W07-TEISEIFLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TEISEIFLG-MAX
               MOVE    SPA-GMN-TEISEIFLG-LST (IDX)
                                       TO  W07-TEISEIFLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-TEISEIFLG-MAX
                                       TO  W07-TEISEIFLG-COUNT
      *    皮下筋肉注射料変換選択（診療行為機能情報）
           MOVE    SPA-GMN-CHG310FLG-G   TO  W07-CHG310FLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-CHG310FLG-MAX
               MOVE    SPA-GMN-CHG310FLG-LST (IDX)
                                       TO  W07-CHG310FLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-CHG310FLG-MAX
                                       TO  W07-CHG310FLG-COUNT
      *    投薬診療種別変換選択（診療行為機能情報）
           MOVE    SPA-GMN-CHGSRYSYU-G   TO  W07-CHGSRYSYU
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-CHGSRYSYU-MAX
               MOVE    SPA-GMN-CHGSRYSYU-LST (IDX)
                                       TO  W07-CHGSRYSYU-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-CHGSRYSYU-MAX
                                       TO  W07-CHGSRYSYU-COUNT
      *    レセプトブレビュー（診療行為機能情報）
           MOVE    SPA-GMN-RESPRV038-G   TO  W07-RESPRV038
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-RESPRV038-MAX
               MOVE    SPA-GMN-RESPRV038-LST (IDX)
                                       TO  W07-RESPRV038-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-RESPRV038-MAX
                                       TO  W07-RESPRV038-COUNT
      *    残量廃棄区分（診療行為機能情報）
           MOVE    SPA-GMN-HAIKIKBN-G  TO  W07-HAIKIKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-HAIKIKBN-MAX
               MOVE    SPA-GMN-HAIKIKBN-LST (IDX)
                                       TO  W07-HAIKIKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-HAIKIKBN-MAX
                                       TO  W07-HAIKIKBN-COUNT
      *    精神療法２０未満加算区分（診療行為機能情報）
           MOVE    SPA-GMN-SEI20KSNKBN-G   TO  W07-SEI20KSNKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SEI20KSNKBN-MAX
               MOVE    SPA-GMN-SEI20KSNKBN-LST (IDX)
                                       TO  W07-SEI20KSNKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SEI20KSNKBN-MAX
                                       TO  W07-SEI20KSNKBN-COUNT
      *
      *    心身療法２０未満加算区分（診療行為機能情報）
           MOVE    SPA-GMN-SIN20KSNKBN-G   TO  W07-SIN20KSNKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SIN20KSNKBN-MAX
               MOVE    SPA-GMN-SIN20KSNKBN-LST (IDX)
                                       TO  W07-SIN20KSNKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SIN20KSNKBN-MAX
                                       TO  W07-SIN20KSNKBN-COUNT
      *    レセプトブレビューレイアウト（診療行為機能情報）
           MOVE    SPA-GMN-PRVLAYOUT038-G  TO  W07-PRVLAYOUT038
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-PRVLAYOUT038-MAX
               MOVE    SPA-GMN-PRVLAYOUT038-LST (IDX)
                                       TO  W07-PRVLAYOUT038-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-PRVLAYOUT038-MAX
                                       TO  W07-PRVLAYOUT038-COUNT
      *H28.6
      *    セット展開表示（診療行為機能情報）
           MOVE    SPA-GMN-SETALLKBN-G TO  W07-SETALLKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SETALLKBN-MAX
               MOVE    SPA-GMN-SETALLKBN-LST (IDX)
                                       TO  W07-SETALLKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SETALLKBN-MAX
                                       TO  W07-SETALLKBN-COUNT
      *
      *    リハビリ・患者情報表示区分（診療行為機能情報）
           MOVE    SPA-GMN-GMNHYOKBN-G(1)  TO  W07-GMNHYOKBN1
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-GMNHYOKBN-MAX
               MOVE    SPA-GMN-GMNHYOKBN-LST (IDX)
                                       TO  W07-GMNHYOKBN1-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-GMNHYOKBN-MAX
                                       TO  W07-GMNHYOKBN1-COUNT
      *
           MOVE    SPA-GMN-GMNHYOKBN-G(2)  TO  W07-GMNHYOKBN2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-GMNHYOKBN-MAX
               MOVE    SPA-GMN-GMNHYOKBN-LST (IDX)
                                       TO  W07-GMNHYOKBN2-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-GMNHYOKBN-MAX
                                       TO  W07-GMNHYOKBN2-COUNT
      *
           MOVE    SPA-GMN-GMNHYOKBN-G(3)  TO  W07-GMNHYOKBN3
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-GMNHYOKBN-MAX
               MOVE    SPA-GMN-GMNHYOKBN-LST (IDX)
                                       TO  W07-GMNHYOKBN3-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-GMNHYOKBN-MAX
                                       TO  W07-GMNHYOKBN3-COUNT
      *
           MOVE    SPA-GMN-GMNHYOKBN-G(4)  TO  W07-GMNHYOKBN4
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-GMNHYOKBN-MAX
               MOVE    SPA-GMN-GMNHYOKBN-LST (IDX)
                                       TO  W07-GMNHYOKBN4-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-GMNHYOKBN-MAX
                                       TO  W07-GMNHYOKBN4-COUNT
      *
           MOVE    SPA-GMN-GMNHYOKBN-G(5)  TO  W07-GMNHYOKBN5
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-GMNHYOKBN-MAX
               MOVE    SPA-GMN-GMNHYOKBN-LST (IDX)
                                       TO  W07-GMNHYOKBN5-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-GMNHYOKBN-MAX
                                       TO  W07-GMNHYOKBN5-COUNT
      *
           MOVE    SPA-GMN-GMNHYOKBN-G(6)  TO  W07-GMNHYOKBN6
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-GMNHYOKBN-MAX
               MOVE    SPA-GMN-GMNHYOKBN-LST (IDX)
                                       TO  W07-GMNHYOKBN6-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-GMNHYOKBN-MAX
                                       TO  W07-GMNHYOKBN6-COUNT
      *    入力スクロール背景色
           MOVE    SPA-GMN-BGCOLORS    TO  W07-BGCOLORS
      *    入力スクロール背景行区分
           MOVE    SPA-GMN-BGLINEKBN-G TO  W07-BGLINEKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-BGLINEKBN-MAX
               MOVE    SPA-GMN-BGLINEKBN-LST (IDX)
                                       TO  W07-BGLINEKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-BGLINEKBN-MAX
                                       TO  W07-BGLINEKBN-COUNT
      *    エラー背景色
           MOVE    SPA-GMN-ERRCOLORS   TO  W07-ERRCOLORS
      *    警告背景色
           MOVE    SPA-GMN-KEICOLORS   TO  W07-KEICOLORS
      *
      *    薬情・お薬手帳対象（診療行為機能情報）
           MOVE    SPA-GMN-YKJOKUKBN-G TO  W07-YKJOKUKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-YKJOKUKBN-MAX
               MOVE    SPA-GMN-YKJOKUKBN-LST (IDX)
                                       TO  W07-YKJOKUKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-YKJOKUKBN-MAX
                                       TO  W07-YKJOKUKBN-COUNT
      *
      *    複数科まとめ（診療行為機能情報）
           MOVE    SPA-GMN-KAGRPFLG-G  TO  W07-KAGRPFLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-KAGRPFLG-MAX
               MOVE    SPA-GMN-KAGRPFLG-LST (IDX)
                                       TO  W07-KAGRPFLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-KAGRPFLG-MAX
                                       TO  W07-KAGRPFLG-COUNT
      *
      *    入金・返金額設定区分（診療行為機能情報）
           MOVE    SPA-GMN-NYKHNKKBN-G TO  W07-NYKHNKKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-NYKHNKKBN-MAX
               MOVE    SPA-GMN-NYKHNKKBN-LST (IDX)
                                       TO  W07-NYKHNKKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-NYKHNKKBN-MAX
                                       TO  W07-NYKHNKKBN-COUNT
      *    発行日変更区分（診療行為機能情報）
           MOVE    SPA-GMN-YMDHENKBN-G TO  W07-YMDHENKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-YMDHENKBN-MAX
               MOVE    SPA-GMN-YMDHENKBN-LST (IDX)
                                       TO  W07-YMDHENKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-YMDHENKBN-MAX
                                       TO  W07-YMDHENKBN-COUNT
      *    環境設定背景色
           MOVE    SPA-GMN-BGCOLORSJ0  TO  W07-BGCOLORSJ0
           MOVE    SPA-GMN-BGCOLORSDAY TO  W07-BGCOLORSDAY
      *    環境設定背景色（時間外）
           MOVE    SPA-GMN-BGCOLORSJ (1)    TO  W07-BGCOLORSJ1
           MOVE    SPA-GMN-BGCOLORSJ (2)    TO  W07-BGCOLORSJ2
           MOVE    SPA-GMN-BGCOLORSJ (3)    TO  W07-BGCOLORSJ3
           MOVE    SPA-GMN-BGCOLORSJ (4)    TO  W07-BGCOLORSJ4
           MOVE    SPA-GMN-BGCOLORSJ (5)    TO  W07-BGCOLORSJ5
           MOVE    SPA-GMN-BGCOLORSJ (6)    TO  W07-BGCOLORSJ6
           MOVE    SPA-GMN-BGCOLORSJ (7)    TO  W07-BGCOLORSJ7
           MOVE    SPA-GMN-BGCOLORSJ (8)    TO  W07-BGCOLORSJ8
      *
      *    入金管理区分（収納機能情報）
           MOVE    SPA-GMN-SYUNYKNKNRKBN-G TO  W07-SYUNYKNKNRKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYUNYKNKNRKBN-MAX
               MOVE    SPA-GMN-SYUNYKNKNRKBNLST (IDX)
                                       TO  W07-SYUNYKNKNRKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SYUNYKNKNRKBN-MAX
                                       TO  W07-SYUNYKNKNRKBN-COUNT
      *
      *
      *    診療年月指定区分（収納機能情報）
           MOVE    SPA-GMN-IKTSRYYMKBN-G TO  W07-IKTSRYYMKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-IKTSRYYMKBN-MAX
               MOVE    SPA-GMN-IKTSRYYMKBNLST (IDX)
                                       TO  W07-IKTSRYYMKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-IKTSRYYMKBN-MAX
                                       TO  W07-IKTSRYYMKBN-COUNT
      *
      *    一括入金画面請求書兼領収書発行区分（収納機能情報）
           MOVE    SPA-GMN-S06SKYPRT-G TO  W07-S06SKYPRT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S06SKYPRT-MAX
               MOVE    SPA-GMN-S06SKYPRTLST (IDX)
                                       TO  W07-S06SKYPRT-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-S06SKYPRT-MAX
                                       TO  W07-S06SKYPRT-COUNT
      *
      *    一括入金画面外来分発行方法（収納機能情報）
           MOVE    SPA-GMN-S06SKYSUM-G TO  W07-S06SKYSUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S06SKYSUM-MAX
               MOVE    SPA-GMN-S06SKYSUMLST (IDX)
                                       TO  W07-S06SKYSUM-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-S06SKYSUM-MAX
                                       TO  W07-S06SKYSUM-COUNT
      *
      *    請求一覧画面外来分再発行方法（収納機能情報）
           MOVE    SPA-GMN-S02SKYSAI-G TO  W07-S02SKYSAI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S02SKYSAI-MAX
               MOVE    SPA-GMN-S02SKYSAILST (IDX)
                                       TO  W07-S02SKYSAI-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-S02SKYSAI-MAX
                                       TO  W07-S02SKYSAI-COUNT
      *
      *    請求書兼領収書前回請求額印字区分
           MOVE    SPA-GMN-S02SKYMISYUPRT-G TO  W07-S02SKYMISYUPRT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S02SKYMISYUPRT-MAX
               MOVE    SPA-GMN-S02SKYMISYUPRTLST (IDX)
                                       TO  W07-S02SKYMISYUPRT-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-S02SKYMISYUPRT-MAX
                                       TO  W07-S02SKYMISYUPRT-COUNT
      *
      *    月別請求書兼領収書発行方法
           MOVE    SPA-GMN-TUKIGRP-G   TO  W07-TUKIGRP
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TUKIGRP-MAX
               MOVE    SPA-GMN-TUKIGRPLST (IDX)
                                       TO  W07-TUKIGRP-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-TUKIGRP-MAX TO  W07-TUKIGRP-COUNT
      *
      *    月別請求書兼領収書前回請求額印字区分
           MOVE    SPA-GMN-TUKISKYMISYUPRT-G TO  W07-TUKISKYMISYUPRT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TUKISKYMISYUPRT-MAX
               MOVE    SPA-GMN-TUKISKYMISYUPRTLST (IDX)
                                       TO  W07-TUKISKYMISYUPRT-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-TUKISKYMISYUPRT-MAX
                                       TO  W07-TUKISKYMISYUPRT-COUNT
      *
      *    月別請求書兼領収書集計区分
           MOVE    SPA-GMN-TUKISUM-G   TO  W07-TUKISUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TUKISUM-MAX
               MOVE    SPA-GMN-TUKISUMLST (IDX)
                                       TO  W07-TUKISUM-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-TUKISUM-MAX TO  W07-TUKISUM-COUNT
      *
      *    請求一覧画面コラムリスト
           MOVE    SPA-GMN-S02MODE-G TO  W07-S02MODE
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S02MODE-MAX
               MOVE    SPA-GMN-S02MODELST (IDX)
                                       TO  W07-S02MODE-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-S02MODE-MAX
                                       TO  W07-S02MODE-COUNT
      *
      *    請求一覧画面コラムリスト件数
           MOVE    "F"                 TO  W07-S02LIMIT-VISIBLE
           MOVE    SPA-GMN-S02LIMIT    TO  W07-S02LIMIT
      *
      *    請求一覧画面並び順区分
           MOVE    SPA-GMN-S02SORTKBN-G TO  W07-S02SORTKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S02SORTKBN-MAX
               MOVE    SPA-GMN-S02SORTKBNLST (IDX)
                                       TO  W07-S02SORTKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-S02SORTKBN-MAX
                                       TO  W07-S02SORTKBN-COUNT
      *
      *    再計算該当者チェック
           MOVE    SPA-GMN-SAIKEISANSKYCHK-G TO  W07-SAIKEISANSKYCHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SAIKEISANSKYCHK-MAX
               MOVE    SPA-GMN-SAIKEISANSKYCHKLST (IDX)
                                       TO  W07-SAIKEISANSKYCHK-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SAIKEISANSKYCHK-MAX
                                       TO  W07-SAIKEISANSKYCHK-COUNT
      *
      *    一括計算順区分
           MOVE    SPA-GMN-IKTSORTKBN-G TO  W07-IKTSORTKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-IKTSORTKBN-MAX
               MOVE    SPA-GMN-IKTSORTKBNLST (IDX)
                                       TO  W07-IKTSORTKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-IKTSORTKBN-MAX
                                       TO  W07-IKTSORTKBN-COUNT
      *
      *    一括計算方法区分
           MOVE    SPA-GMN-IKTKEISANKBN-G TO  W07-IKTKEISANKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-IKTKEISANKBN-MAX
               MOVE    SPA-GMN-IKTKEISANKBNLST (IDX)
                                       TO  W07-IKTKEISANKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-IKTKEISANKBN-MAX
                                       TO  W07-IKTKEISANKBN-COUNT
      *
      *    請求書未収金集計期間区分チェック
           MOVE    SPA-GMN-MISYUKIKANKBN-G TO  W07-MISYUKIKANKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MISYUKIKANKBN-MAX
               MOVE    SPA-GMN-MISYUKIKANKBNLST (IDX)
                                       TO  W07-MISYUKIKANKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-MISYUKIKANKBN-MAX
                                       TO  W07-MISYUKIKANKBN-COUNT
      *
      *    請求書兼領収書発行区分（外来）
           MOVE    SPA-GMN-SYUSKYPRTG-G TO  W07-SYUSKYPRTG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYUSKYPRTG-MAX
               MOVE    SPA-GMN-SYUSKYPRTGLST (IDX)
                                       TO  W07-SYUSKYPRTG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SYUSKYPRTG-MAX
                                       TO  W07-SYUSKYPRTG-COUNT
      *
      *    請求書兼領収書前回請求額印字区分
           MOVE    SPA-GMN-S03SKYMISYUPRT-G TO  W07-S03SKYMISYUPRT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S03SKYMISYUPRT-MAX
               MOVE    SPA-GMN-S03SKYMISYUPRTLST (IDX)
                                       TO  W07-S03SKYMISYUPRT-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-S03SKYMISYUPRT-MAX
                                       TO  W07-S03SKYMISYUPRT-COUNT
      *
      *    診療費明細書発行区分（外来）
           MOVE    SPA-GMN-SYUSRYPRTG-G TO  W07-SYUSRYPRTG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYUSRYPRTG-MAX
               MOVE    SPA-GMN-SYUSRYPRTGLST (IDX)
                                       TO  W07-SYUSRYPRTG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SYUSRYPRTG-MAX
                                       TO  W07-SYUSRYPRTG-COUNT
      *
      *    請求書兼領収書発行区分（入院）
           MOVE    SPA-GMN-SYUSKYPRTN-G TO  W07-SYUSKYPRTN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYUSKYPRTN-MAX
               MOVE    SPA-GMN-SYUSKYPRTNLST (IDX)
                                       TO  W07-SYUSKYPRTN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SYUSKYPRTN-MAX
                                       TO  W07-SYUSKYPRTN-COUNT
      *
      *    診療費明細書発行区分（入院）
           MOVE    SPA-GMN-SYUSRYPRTN-G TO  W07-SYUSRYPRTN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYUSRYPRTN-MAX
               MOVE    SPA-GMN-SYUSRYPRTNLST (IDX)
                                       TO  W07-SYUSRYPRTN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SYUSRYPRTN-MAX
                                       TO  W07-SYUSRYPRTN-COUNT
      *
      *    支払証明書印刷画面支払証明書発行区分
           MOVE    SPA-GMN-S04SHIHARAIKBN-G TO  W07-S04SHIHARAIKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S04SHIHARAI-MAX
               MOVE    SPA-GMN-S04SHIHARAILST (IDX)
                                       TO  W07-S04SHIHARAIKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-S04SHIHARAI-MAX
                                       TO  W07-S04SHIHARAIKBN-COUNT
      *
      *    支払証明書印刷画面入外区分
           MOVE    SPA-GMN-S04NYUGAIKBN-G TO  W07-S04NYUGAIKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S04NYUGAI-MAX
               MOVE    SPA-GMN-S04NYUGAILST (IDX)
                                       TO  W07-S04NYUGAIKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-S04NYUGAI-MAX
                                       TO  W07-S04NYUGAIKBN-COUNT
      *
      *    支払証明書印刷画面期間指定
           MOVE    SPA-GMN-S04SRYKBN-G TO  W07-S04SRYKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S04SRY-MAX
               MOVE    SPA-GMN-S04SRYLST (IDX)
                                       TO  W07-S04SRYKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-S04SRY-MAX
                                       TO  W07-S04SRYKBN-COUNT
      *
      *    支払証明書印刷画面期間区分
           MOVE    SPA-GMN-S04KIKANKBN-G TO  W07-S04KIKANKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S04KIKAN-MAX
               MOVE    SPA-GMN-S04KIKANLST (IDX)
                                       TO  W07-S04KIKANKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-S04KIKAN-MAX
                                       TO  W07-S04KIKANKBN-COUNT
      *
      *    支払証明書印刷画面請求内容の印字
           MOVE    SPA-GMN-S04DSPKBN-G TO  W07-S04DSPKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S04DSP-MAX
               MOVE    SPA-GMN-S04DSPLST (IDX)
                                       TO  W07-S04DSPKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-S04DSP-MAX
                                       TO  W07-S04DSPKBN-COUNT
      *
      *    支払証明書印刷画面期間終了後の支払い
           MOVE    SPA-GMN-S04SYORIKBN-G TO  W07-S04SYORIKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S04SYORI-MAX
               MOVE    SPA-GMN-S04SYORILST (IDX)
                                       TO  W07-S04SYORIKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-S04SYORI-MAX
                                       TO  W07-S04SYORIKBN-COUNT
      *
      *    支払証明書印刷画面自費内訳明細書
           MOVE    SPA-GMN-S04JIHIPRTKBN-G TO  W07-S04JIHIPRTKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S04JIHIPRTKBN-MAX
               MOVE    SPA-GMN-S04JIHIPRTKBNLST (IDX)
                                       TO  W07-S04JIHIPRTKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-S04JIHIPRTKBN-MAX
                                       TO  W07-S04JIHIPRTKBN-COUNT
      *
      *    支払証明書印刷画面期間終了後の支払い
           MOVE    SPA-GMN-S04SUMKBN-G TO  W07-S04SUMKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S04SUMKBN-MAX
               MOVE    SPA-GMN-S04SUMKBNLST (IDX)
                                       TO  W07-S04SUMKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-S04SUMKBN-MAX
                                       TO  W07-S04SUMKBN-COUNT
      *
      *    過去日分の登録時刻
           MOVE    SPA-GMN-SYUSTIME-HH TO  W07-SYUSTIME-HH
           MOVE    SPA-GMN-SYUSTIME-MM TO  W07-SYUSTIME-MM
      *
      *    仮締め時間帯
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   2 )
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   7 )
      *
                   MOVE    SPA-GMN-SYUKARIDAY (IDX1 IDX2)
                                       TO  W07-SYUKARIDAY
                                                       (IDX1 IDX2)
               END-PERFORM
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   2 )
      *
                   MOVE    SPA-GMN-SYUKARISTTIME (IDX1 IDX2)
                                       TO  W07-SYUKARISTTIME
                                                       (IDX1 IDX2)
                   MOVE    SPA-GMN-SYUKARIEDTIME (IDX1 IDX2)
                                       TO  W07-SYUKARIEDTIME
                                                       (IDX1 IDX2)
      *
               END-PERFORM
           END-PERFORM
      *
      *    主病名確認メッセージ表示有無区分（病名機能情報）
           MOVE    SPA-GMN-MSG1-DSPKBN-G   TO  W07-MSG1-DSPKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MSG1-DSPKBN-MAX
               MOVE    SPA-GMN-MSG1-DSPKBNLST (IDX)
                                       TO  W07-MSG1-DSPKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-MSG1-DSPKBN-MAX
                                       TO  W07-MSG1-DSPKBN-COUNT
      *
      *    移行病名警告メッセージ表示有無区分（病名機能情報）
           MOVE    SPA-GMN-MSG2-DSPKBN-G   TO  W07-MSG2-DSPKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MSG2-DSPKBN-MAX
               MOVE    SPA-GMN-MSG2-DSPKBNLST (IDX)
                                       TO  W07-MSG2-DSPKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-MSG2-DSPKBN-MAX
                                       TO  W07-MSG2-DSPKBN-COUNT
      *
      *    廃止年月日警告メッセージ表示有無区分（病名機能情報）
           MOVE    SPA-GMN-MSG3-DSPKBN-G   TO  W07-MSG3-DSPKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MSG3-DSPKBN-MAX
               MOVE    SPA-GMN-MSG3-DSPKBNLST (IDX)
                                       TO  W07-MSG3-DSPKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-MSG3-DSPKBN-MAX
                                       TO  W07-MSG3-DSPKBN-COUNT
      *
      *    同一病名警告メッセージ表示有無区分（病名機能情報）
           MOVE    SPA-GMN-MSG4-DSPKBN-G   TO  W07-MSG4-DSPKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MSG4-DSPKBN-MAX
               MOVE    SPA-GMN-MSG4-DSPKBNLST (IDX)
                                       TO  W07-MSG4-DSPKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-MSG4-DSPKBN-MAX
                                       TO  W07-MSG4-DSPKBN-COUNT
      *
      *    死亡区分更新確認メッセージ表示有無区分（病名機能情報）
           MOVE    SPA-GMN-MSG5-DSPKBN-G   TO  W07-MSG5-DSPKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MSG5-DSPKBN-MAX
               MOVE    SPA-GMN-MSG5-DSPKBNLST (IDX)
                                       TO  W07-MSG5-DSPKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-MSG5-DSPKBN-MAX
                                       TO  W07-MSG5-DSPKBN-COUNT
      *
      *    診療科初期表示区分（病名機能情報）
           MOVE    SPA-GMN-SRYKA-DSPKBN-G   TO  W07-SRYKA-DSPKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYKA-DSPKBN-MAX
               MOVE    SPA-GMN-SRYKA-DSPKBNLST (IDX)
                                       TO  W07-SRYKA-DSPKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SRYKA-DSPKBN-MAX
                                       TO  W07-SRYKA-DSPKBN-COUNT
      *
      *    表示対象期間初期表示区分（病名機能情報）
           MOVE    SPA-GMN-SYOKBN-DSPKBN-G   TO  W07-SYOKBN-DSPKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYOKBN-DSPKBN-MAX
               MOVE    SPA-GMN-SYOKBN-DSPKBNLST (IDX)
                                       TO  W07-SYOKBN-DSPKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SYOKBN-DSPKBN-MAX
                                       TO  W07-SYOKBN-DSPKBN-COUNT
      *
      *    疑いフラグ設定区分（病名機能情報）
           MOVE    SPA-GMN-UTAGAIFLG-KBN-G   TO  W07-UTAGAIFLG-KBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-UTAGAIFLG-KBN-MAX
               MOVE    SPA-GMN-UTAGAIFLG-KBNLST (IDX)
                                       TO  W07-UTAGAIFLG-KBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-UTAGAIFLG-KBN-MAX
                                       TO  W07-UTAGAIFLG-KBN-COUNT
      *
      *    コメント症状詳記区分初期設定（病名機能情報）
           MOVE    SPA-GMN-SJKBN-G    TO  W07-SJKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SJKBN-MAX
               MOVE    SPA-GMN-SJKBNLST (IDX)
                                       TO  W07-SJKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SJKBN-MAX
                                       TO  W07-SJKBN-COUNT
      *
      *    全科の表示順区分（会計照会機能情報）
           MOVE    SPA-GMN-SRYKBN-FLG-G    TO  W07-SRYKBN-FLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYKBN-MAX
               MOVE    SPA-GMN-SRYKBN-LIST (IDX)
                                       TO  W07-SRYKBN-FLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SRYKBN-MAX  TO  W07-SRYKBN-FLG-COUNT
      *    診療科の初期表示（会計照会機能情報）
           MOVE    SPA-GMN-SRYKA-FLG-G TO  W07-SRYKA-FLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYKA-MAX
               MOVE    SPA-GMN-SRYKA-LIST (IDX)
                                       TO  W07-SRYKA-FLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SRYKA-MAX   TO  W07-SRYKA-FLG-COUNT
      *    レセプトブレビュー（会計照会機能情報）
           MOVE    SPA-GMN-RESPRV043-G   TO  W07-RESPRV043
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-RESPRV043-MAX
               MOVE    SPA-GMN-RESPRV043-LIST (IDX)
                                       TO  W07-RESPRV043-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-RESPRV043-MAX
                                       TO  W07-RESPRV043-COUNT
      *    レセプトブレビューレイアウト（診療行為機能情報）
           MOVE    SPA-GMN-PRVLAYOUT043-G  TO  W07-PRVLAYOUT043
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-PRVLAYOUT043-MAX
               MOVE    SPA-GMN-PRVLAYOUT043-LST (IDX)
                                       TO  W07-PRVLAYOUT043-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-PRVLAYOUT043-MAX
                                       TO  W07-PRVLAYOUT043-COUNT
      *
      *予約情報
      *    初期表示ドクター
           MOVE    SPA-GMN-INIT-DRCD-G TO  W07-INIT-DRCD
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-INIT-DRCD-MAX
               MOVE    SPA-GMN-INIT-DRCD-LIST (IDX)
                                       TO  W07-INIT-DRCD-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-INIT-DRCD-MAX   TO  W07-INIT-DRCD-COUNT
      *
           MOVE    SPA-GMN-DRKIKAN     TO  W07-YYK-DRKIKAN
      *
      *    予約一覧初期表示
           MOVE    SPA-GMN-YYKINIT-FLG-G   TO  W07-YYKINIT-FLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-YYKINIT-MAX
               MOVE    SPA-GMN-YYKINIT-LIST (IDX)
                                       TO  W07-YYKINIT-FLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-YYKINIT-MAX TO  W07-YYKINIT-FLG-COUNT
      *    予約枠最大件数
           MOVE    SPA-GMN-YYKMAXCNT   TO  W07-YYKMAXCNT
      *    予約票発行区分
           MOVE    SPA-GMN-YYKHYO-KBN-G   TO  W07-YYKHYO-KBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-YYKHYO-MAX
               MOVE    SPA-GMN-YYKHYO-LIST (IDX)
                                       TO  W07-YYKHYO-KBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-YYKHYO-MAX  TO  W07-YYKHYO-KBN-COUNT
      *    予約受付連携区分
           MOVE    SPA-GMN-YYKUKE-KBN-G   TO  W07-YYKUKE-KBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-YYKUKE-MAX
               MOVE    SPA-GMN-YYKUKE-LIST (IDX)
                                       TO  W07-YYKUKE-KBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-YYKUKE-MAX  TO  W07-YYKUKE-KBN-COUNT
      *    予約患者番号連動区分
           MOVE    SPA-GMN-YYKPTNUM-KBN-G  TO  W07-YYKPTNUM-KBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-YYKPTNUM-MAX
               MOVE    SPA-GMN-YYKPTNUM-LIST (IDX)
                                      TO  W07-YYKPTNUM-KBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-YYKPTNUM-MAX    TO  W07-YYKPTNUM-KBN-COUNT
      *    受付保険組合せ区分
           MOVE    SPA-GMN-UKEHKNCHK-KBN-G  TO  W07-UKEHKNCHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-UKEHKNCHK-MAX
               MOVE    SPA-GMN-UKEHKNCHK-LIST (IDX)
                                      TO  W07-UKEHKNCHK-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-UKEHKNCHK-MAX    TO  W07-UKEHKNCHK-COUNT
      *
      *点数算定基準
      *    妥結率区分
           MOVE    SPA-GMN-DAKETUKBN-G     TO  W07-DAKETUKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-DAKETUKBN-MAX
               MOVE    SPA-GMN-DAKETUKBN-LIST (IDX)
                                      TO  W07-DAKETUKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-DAKETUKBN-MAX   TO  W07-DAKETUKBN-COUNT
      *    低紹介率区分
           MOVE    SPA-GMN-TEIRITUKBN-G    TO  W07-TEIRITUKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TEIRITUKBN-MAX
               MOVE    SPA-GMN-TEIRITUKBN-LIST (IDX)
                                      TO  W07-TEIRITUKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-TEIRITUKBN-MAX TO  W07-TEIRITUKBN-COUNT
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務権限非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           MOVE    WIDGET-INSENSITIVE  TO  W07-B03-STATE
                                           W07-TAGNAME1-STATE
                                           W07-TAGNAME2-STATE
                                           W07-TAGNAME3-STATE
                                           W07-TAGNAME4-STATE
                                           W07-TAGNAME5-STATE
                                           W07-TAGNAME6-STATE
                                           W07-TAGNAME7-STATE
                                           W07-SRYKACD-STATE
                                           W07-TAGNAME8-STATE
      *
           IF      SPA-W01-SELNUM          >   ZERO
               MOVE    WIDGET-NORMAL   TO  W07-B03-STATE
           END-IF 
      *
           EVALUATE    SPA-W01-KANRICD
               WHEN    "1007" 
                   MOVE    WIDGET-NORMAL   TO  W07-TAGNAME1-STATE
               WHEN    "1017" 
                   MOVE    WIDGET-NORMAL   TO  W07-TAGNAME2-STATE
               WHEN    "1038" 
                   MOVE    WIDGET-NORMAL   TO  W07-TAGNAME3-STATE
               WHEN    "1039" 
                   MOVE    WIDGET-NORMAL   TO  W07-TAGNAME4-STATE
               WHEN    "1042" 
                   MOVE    WIDGET-NORMAL   TO  W07-TAGNAME5-STATE
               WHEN    "1043" 
                   MOVE    WIDGET-NORMAL   TO  W07-TAGNAME6-STATE
               WHEN    "1045" 
                   MOVE    WIDGET-NORMAL   TO  W07-TAGNAME7-STATE
               WHEN    "1201" 
                   MOVE    WIDGET-NORMAL   TO  W07-TAGNAME8-STATE
           END-EVALUATE
      *
           IF      SPA-GMN-DATA(7)         =   "1"
               MOVE    WIDGET-NORMAL   TO  W07-SRYKACD-STATE
           END-IF
      *    チェック除外時間
           IF      SPA-GMN-JIKANTOKU-CHK   =   "1"
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   2
                   MOVE    WIDGET-NORMAL   TO  W07-JIKANW1-HH-STATE(IDX)
                   MOVE    WIDGET-NORMAL   TO  W07-JIKANW1-MM-STATE(IDX)
                   MOVE    WIDGET-NORMAL   TO  W07-JIKANW2-HH-STATE(IDX)
                   MOVE    WIDGET-NORMAL   TO  W07-JIKANW2-MM-STATE(IDX)
                   MOVE    WIDGET-NORMAL   TO  W07-JIKANS1-HH-STATE(IDX)
                   MOVE    WIDGET-NORMAL   TO  W07-JIKANS1-MM-STATE(IDX)
                   MOVE    WIDGET-NORMAL   TO  W07-JIKANS2-HH-STATE(IDX)
                   MOVE    WIDGET-NORMAL   TO  W07-JIKANS2-MM-STATE(IDX)
               END-PERFORM
           ELSE
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   2
                   MOVE    WIDGET-INSENSITIVE  TO  W07-JIKANW1-HH-STATE
                                                                  (IDX)
                   MOVE    WIDGET-INSENSITIVE  TO  W07-JIKANW1-MM-STATE
                                                                  (IDX)
                   MOVE    WIDGET-INSENSITIVE  TO  W07-JIKANW2-HH-STATE
                                                                  (IDX)
                   MOVE    WIDGET-INSENSITIVE  TO  W07-JIKANW2-MM-STATE
                                                                  (IDX)
                   MOVE    WIDGET-INSENSITIVE  TO  W07-JIKANS1-HH-STATE
                                                                  (IDX)
                   MOVE    WIDGET-INSENSITIVE  TO  W07-JIKANS1-MM-STATE
                                                                  (IDX)
                   MOVE    WIDGET-INSENSITIVE  TO  W07-JIKANS2-HH-STATE
                                                                  (IDX)
                   MOVE    WIDGET-INSENSITIVE  TO  W07-JIKANS2-MM-STATE
                                                                  (IDX)
               END-PERFORM
           END-IF
      *
           MOVE    WIDGET-NORMAL       TO  W07-S04SYORIKBN-STATE
                                           W07-S04SYORIKBN-ITEM-STATE
                                           W07-S04SUMKBN-STATE
                                           W07-S04SUMKBN-ITEM-STATE
                                           W07-S04DSPKBN-STATE
                                           W07-S04DSPKBN-ITEM-STATE
                                           W07-S04JIHIPRTKBN-STATE
                                           W07-S04JIHIPRTKBN-ITEM-STATE
           IF    ( SPA-GMN-S04KIKANKBN =   "2" )
               MOVE    WIDGET-INSENSITIVE
                                       TO  W07-S04SYORIKBN-STATE
                                           W07-S04SYORIKBN-ITEM-STATE
                                           W07-S04SUMKBN-STATE
                                           W07-S04SUMKBN-ITEM-STATE
                                           W07-S04DSPKBN-STATE
                                           W07-S04DSPKBN-ITEM-STATE
                                           W07-S04JIHIPRTKBN-STATE
                                           W07-S04JIHIPRTKBN-ITEM-STATE
           END-IF
      *
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "調剤技術基本料入力エラー"  TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "薬剤情報提供料（一般）入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE
                      "薬剤情報提供料（老人手帳記載あり）入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE
                       "薬剤情報提供料（老人手帳記載なし）入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE
                 "薬剤情報提供料（老人手帳記載）は両方の選択できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "外来管理加算チェック区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE
                       "療養担当手当（北海道）入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE
                       "特定薬剤治療管理料入力エラー"  TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE
                       "育児栄養加算自動算定入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "自動算定科がありません。"  TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "外来カーソル移動区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "入院初診料入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "包括登録入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "数量ゼロ入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "入院調剤技術基本料チェックエラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "時間外加算特例乳幼児チェックエラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "画像診断管理加算チェックエラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "中途終了展開時の自動発生チェックエラー"
                                                    TO  SPA-ERRMSG
               WHEN    "0080"
                   MOVE    "訂正時の自動発生チェックエラー"
                                                    TO  SPA-ERRMSG
               WHEN    "0081"
                   MOVE    "訂正時の自動発生（特定疾患処方管理）"
                                                    TO  SPA-ERRMSG
                   MOVE    "チェックエラー"
                                                    TO  SPA-ERRMSG(37:)
               WHEN    "0082"
                   MOVE    "検体検査加算自動発生チェックエラー"
                                                    TO  SPA-ERRMSG
               WHEN    "0083"
                   MOVE    "訂正時の自動発生（入院）チェックエラー"
                                                    TO  SPA-ERRMSG
               WHEN    "0084"
                   MOVE    "病名疾患区分からの自動発生チェックエラー"
                                                    TO  SPA-ERRMSG
               WHEN    "0085"
                   MOVE    "入院時初回加算自動発生チェックエラー"
                                                    TO  SPA-ERRMSG
               WHEN    "0030"
                   MOVE    "時間外加算（乳幼児）チェックエラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0031"
                   MOVE    "チェック除外時間が違います。"
                                                       TO  SPA-ERRMSG
               WHEN    "0032"
                   MOVE    "チェック除外開始時間＜終了時間で"
                                                       TO  SPA-ERRMSG
                   MOVE    "入力して下さい"
                                               TO  SPA-ERRMSG(35:)
               WHEN    "0033"
                   MOVE    "チェック除外時間が設定されていません。"
                                                       TO  SPA-ERRMSG
                   MOVE    "入力して下さい"
                                               TO  SPA-ERRMSG(39:)
               WHEN    "0034"
                   MOVE    "診療行為クリア確認チェックエラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0037"
                   MOVE    "薬剤情報提供料チェックエラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0038"
                   MOVE    "病名表示エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0039"
                   MOVE    "剤登録順エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0040"
                   MOVE    "前回保険組合せ相違チェックエラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0041"
                   MOVE    "手帳記載加算確認エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0042"
                   STRING  "薬剤情報提供料が算定しないとなっています。"
                           "手帳記載加算確認のチェックはできません。"
                           DELIMITED  BY  SIZE
                                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0043"
                   STRING  "時間外緊急院内検査・画像診断加算"
                           "算定チェックエラー"
                           DELIMITED  BY  SIZE
                                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0020"
                   MOVE    "外来初診・再診料区分エラー"
                                                       TO  SPA-ERRMSG
      *H28.8
               WHEN    "0044"
                   MOVE    "相互作用チェック区分エラー"
                                                       TO  SPA-ERRMSG
      *H28.8
               WHEN    "0045"
                   MOVE    "薬剤投与量チェック区分エラー"
                                                       TO  SPA-ERRMSG
      *H29.3
               WHEN    "0046"
                   MOVE    "訂正時の展開（処方料）区分エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0047"
                   MOVE    "薬投３０日超チェック区分エラー"
                                                       TO  SPA-ERRMSG
      *        
               WHEN    "1001"
                   MOVE    "カルテの病名対象区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "人名辞書使用区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "患者登録のカルテ発行区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "自費保険の補助区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "1005"
                   MOVE    "同姓同名者リスト区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "1006"
                   MOVE    "二重登録疑いリスト区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "1007"
                   MOVE    "地方公費単独チェック区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "1008"
                   MOVE    "患者削除機能区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "1009"
                   MOVE    "本人・家族区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "1010"
                   MOVE    "家族年齢は数値で入力して下さい。"
                                                       TO  SPA-ERRMSG
               WHEN    "1011"
                   MOVE    "被保険者名自動記載区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "1012"
                   MOVE    "世帯主名自動記載区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "1013"
                   STRING  "グループ医療機関内同一患者リスト区分が"
                           "ありません"
                                                   DELIMITED BY  SIZE
                                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1014"
                   STRING  "公費上限額履歴チェック区分が"
                           "ありません"
                                                   DELIMITED BY  SIZE
                                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1015"
                   STRING  "郵便番号自動記載区分が"
                           "ありません"
                                                   DELIMITED BY  SIZE
                                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1016"
                   STRING  "住所都道府県名記載区分が"
                           "ありません"
                                                   DELIMITED BY  SIZE
                                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1017"
                   STRING  "京都府通り名編集区分が"
                           "ありません"
                                                   DELIMITED BY  SIZE
                                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1018"
                   STRING  "公費法別番号毎一覧表示区分が"
                           "ありません"
                                                   DELIMITED BY  SIZE
                                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1019"
                   STRING  "保険初期表示区分がありません"
                                                   DELIMITED BY  SIZE
                                                       INTO  SPA-ERRMSG
                   END-STRING
      *    診療行為機能情報
               WHEN    "2001"
                   MOVE    "入金管理区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2002"
                   MOVE    "複数科まとめ集計区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2003"
                   MOVE    "請求書発行方法区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2004"
                   MOVE    "ＤＯ初期表示区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2005"
                   MOVE    "中途終了一覧区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2006"
                   MOVE    "検査正式名称表示区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2007"
                   MOVE    "同日再診・訂正選択区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2008"
                   MOVE    "皮下筋肉注射料自動変換区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2009"
                   MOVE    "訂正の請求書金額区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2010"
                   MOVE    "薬剤情報提供発行区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2011"
                   MOVE    "悪性腫瘍検査一覧区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2012"
                   MOVE    "投薬診療種別変換選択区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2013"
                   STRING  "特定疾患療養指導料と初診料のチェック区分が"
                           "ありません。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2014"
                   MOVE    "お薬手帳発行区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2015"
                   MOVE    "レセプトプレビュー区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2016"
                   MOVE    "薬情・お薬手帳対象区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2017"
                   MOVE    "残量廃棄区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2018"
                   MOVE    "入金・返金額設定区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2019"
                   STRING  "入金の取り扱いが今回請求分のみ入金です。"
                           "入金・返金額設定はできません。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2020"
                   MOVE    "精神療法２０未満加算区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2030"
                   MOVE    "心身療法２０未満加算区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2021"
                   MOVE    "発行日変更区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2023"
                   MOVE    "リハビリ・患者情報表示区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2024"
                   STRING  "入力スクロール背景色は半角で入力して"
                           "下さい。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2025"
                   STRING  "入力スクロール背景行区分がありません。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2026"
                   STRING  "背景色は半角で入力して"
                           "下さい。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2027"
                   STRING  "確認画面処方名称表示区分がありません。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2028"
                   STRING  "レセプトレイアウト区分がありません。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2029"
                   STRING  "セット展開区分がありません。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
      *H30.9
               WHEN    "2030"
                   STRING  "選択式コメント一覧区分がありません。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
      *    収納情報
               WHEN    "3003"
                   MOVE    "登録時刻入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "3004"
                   MOVE    "仮締め時刻入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "3005"
                   MOVE    "仮締め時刻入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "3101"
                   MOVE    "期間の指定に誤りがあります"
                                                       TO  SPA-ERRMSG
      *    病名機能情報
               WHEN    "4001"
                   MOVE
                       "主病名確認メッセージ表示有無区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "4002"
                   MOVE
                   "移行病名警告メッセージ表示有無区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "4003"
                   MOVE
                   "廃止年月日警告メッセージ表示有無区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "4004"
                   MOVE    "診療科初期表示区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "4005"
                   MOVE    "表示対象期間初期表示区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "4006"
                   MOVE
                   "同一病名警告メッセージ表示有無区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "4007"
                   MOVE    "詳記区分初期表示がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "4008"
                   MOVE    "疑いフラグ設定区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "4009"
                   MOVE
                  "死亡区分更新確認メッセージ表示有無区分がありません。"
                                                       TO  SPA-ERRMSG
      *        会計照会機能情報
               WHEN    "5001"
                   MOVE    "全科の表示順区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "5002"
                   MOVE    "診療科の初期表示区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "5003"
                   MOVE    "レセプトプレビュー区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "5004"
                   MOVE    "レセプトレイアウト区分がありません。"
                                                       TO  SPA-ERRMSG
      *        予約機能情報
               WHEN    "6001"
                   MOVE    "初期表示ドクターがありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "6002"
                   MOVE    "予約一覧初期表示区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "6003"
                   MOVE    "予約枠最大件数はゼロは入力できません。"
                                                       TO  SPA-ERRMSG
               WHEN    "6004"
                   MOVE    "予約票発行区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "6005"
                   MOVE    "予約・受付番号連動区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "6006"
                   MOVE    "患者番号連動区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "6007"
                   MOVE    "受付保険組合せ区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "6008"
                   MOVE    "初期表示ドクターは有効期間外です。"
                                                       TO  SPA-ERRMSG
      *
               WHEN    "8001"
                   MOVE    "妥結率区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "8002"
                   MOVE    "低紹介率区分がありません。"
                                                       TO  SPA-ERRMSG
      *
               WHEN    "0099"
                   MOVE    "未入力の項目があります"    TO  SPA-ERRMSG
               WHEN    "9001"
                   MOVE    "削除ができませんでした"    TO  SPA-ERRMSG
               WHEN    "9002"
                   MOVE    "更新ができませんでした"    TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  WERR
           MOVE    SPA-ERRCD       TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  WERR-ERRMSG
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "W07"           TO  SPA-MOTOPG
           MOVE    "WERR"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "WERR"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "自動算定・チェック機能制御情報を登録します"
                                                       TO  WRK-WIDMSG
               WHEN    "1002"
                   MOVE    "自動算定・チェック機能制御情報を削除します"
                                                       TO  WRK-WIDMSG
               WHEN    "1003"
                   MOVE    "患者登録機能情報を登録します"
                                                       TO  WRK-WIDMSG
               WHEN    "1004"
                   MOVE    "患者登録機能情報を削除します"
                                                       TO  WRK-WIDMSG
               WHEN    "1005"
                   MOVE    "診療行為機能情報を登録します"
                                                       TO  WRK-WIDMSG
               WHEN    "1006"
                   MOVE    "診療行為機能情報を削除します"
                                                       TO  WRK-WIDMSG
               WHEN    "1007"
                   MOVE    "収納機能情報を登録します"
                                                       TO  WRK-WIDMSG
               WHEN    "1008"
                   MOVE    "収納機能情報を削除します"
                                                       TO  WRK-WIDMSG
      *
               WHEN    "1009"
                   MOVE    "病名機能情報を登録します"  TO  WRK-WIDMSG
               WHEN    "1010"
                   MOVE    "病名機能情報を削除します"  TO  WRK-WIDMSG
      *
               WHEN    "1011"
                   MOVE    "会計照会機能情報を登録します"
                                                   TO  WRK-WIDMSG
               WHEN    "1012"
                   MOVE    "会計照会機能情報を削除します"
                                                   TO  WRK-WIDMSG
      *
               WHEN    "1013"
                   MOVE    "予約・受付機能情報を登録します"
                                                   TO  WRK-WIDMSG
               WHEN    "1014"
                   MOVE    "予約・受付機能情報を削除します"
                                                   TO  WRK-WIDMSG
      *
               WHEN    "1201"
                   MOVE    "点数算定基準情報を登録します"
                                                   TO  WRK-WIDMSG
               WHEN    "1202"
                   MOVE    "点数算定基準情報を削除します"
                                                   TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WID1
           INITIALIZE                      WID1
           MOVE    SPA-WIDCD           TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG          TO  WID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "W07"               TO  SPA-MOTOPG
           MOVE    "WID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理    
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-GMN-CUR     =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE  "INITAUTO"        TO   MCP-WIDGET
               WHEN    2
                   MOVE  "DATA07"          TO   MCP-WIDGET
               WHEN    3
                   MOVE  "SRYKACD"         TO   MCP-WIDGET
               WHEN    4
                   MOVE  "DATA05"          TO   MCP-WIDGET
               WHEN    5 
                   MOVE  "BYOMEIAUTO"      TO   MCP-WIDGET
               WHEN    6 
                   MOVE  "LASTKIKAN"       TO   MCP-WIDGET
               WHEN    7 
                   MOVE  "CHUTOTENKAI"     TO   MCP-WIDGET
               WHEN    8
                   MOVE  "NYUSYOSIN"       TO   MCP-WIDGET
               WHEN    9 
                   MOVE  "JIKANTOKUNYU"    TO   MCP-WIDGET
               WHEN    10
                   MOVE  "DATA06"          TO   MCP-WIDGET
      *
               WHEN    11
                   MOVE  "DATA01"          TO   MCP-WIDGET
               WHEN    12
                   MOVE  "DATA02"          TO   MCP-WIDGET
               WHEN    13
                   MOVE  "DATA03"          TO   MCP-WIDGET
               WHEN    14
                   MOVE  "DATA04"          TO   MCP-WIDGET
               WHEN    15
                   MOVE  "GAZOUKSN1"       TO   MCP-WIDGET
               WHEN    16
                   MOVE  "KENSAAUTO"       TO   MCP-WIDGET
               WHEN    17
                   MOVE  "NYUKENSA"        TO   MCP-WIDGET
               WHEN    18
                   MOVE  "TEISEI"          TO   MCP-WIDGET
               WHEN    19
                   MOVE  "TEISEITOKU"      TO   MCP-WIDGET
               WHEN    20
                   MOVE  "TEINYUAUTO"      TO   MCP-WIDGET
      *
               WHEN    30
                   MOVE  "KIKAN"           TO   MCP-WIDGET
      *H28.8
               WHEN    35
                   MOVE  "KINKCHK"         TO   MCP-WIDGET
               WHEN    31
                   MOVE  "GAIKANKBN"       TO   MCP-WIDGET
               WHEN    32
                   MOVE  "GAIMOVE"         TO   MCP-WIDGET
               WHEN    33
                   MOVE  "SURYOZERO"       TO   MCP-WIDGET
               WHEN    34
                   MOVE  "CHOKICHK"        TO   MCP-WIDGET
      *H29.3
               WHEN    36
                   MOVE  "TEISHOHOU"       TO   MCP-WIDGET
      *
               WHEN    81
                   MOVE  "CLEARCHK"        TO   MCP-WIDGET
               WHEN    82
                   MOVE  "YAKJYOCHK"       TO   MCP-WIDGET
               WHEN    83
                   MOVE  "ZENHKNCHK"       TO   MCP-WIDGET
               WHEN    87
                   MOVE  "TECHOKBN"        TO   MCP-WIDGET
               WHEN    88
                   MOVE  "JGAIKEIKBN"      TO   MCP-WIDGET
               WHEN    89
                   MOVE  "TOUYOCHK"        TO   MCP-WIDGET
               WHEN    90
                   MOVE  "SHOHOU30CHK"     TO   MCP-WIDGET
               WHEN    84
                   MOVE  "HOUKATU"         TO   MCP-WIDGET
               WHEN    85
                   MOVE  "BYOHYOKBN"       TO   MCP-WIDGET
               WHEN    86
                   MOVE  "ZAIJUNKBN"       TO   MCP-WIDGET
      *        時間外加算チェック
               WHEN    50
                   MOVE  "JIKANTOKU_CHK"   TO   MCP-WIDGET
               WHEN    51
                   MOVE  "JIKANW1_HH1"     TO   MCP-WIDGET
               WHEN    52
                   MOVE  "JIKANW1_MM1"     TO   MCP-WIDGET
               WHEN    53
                   MOVE  "JIKANW1_HH2"     TO   MCP-WIDGET
               WHEN    54
                   MOVE  "JIKANW1_MM2"     TO   MCP-WIDGET
               WHEN    55
                   MOVE  "JIKANW2_HH1"     TO   MCP-WIDGET
               WHEN    56
                   MOVE  "JIKANW2_MM1"     TO   MCP-WIDGET
               WHEN    57
                   MOVE  "JIKANW2_HH2"     TO   MCP-WIDGET
               WHEN    58
                   MOVE  "JIKANW2_MM2"     TO   MCP-WIDGET
               WHEN    59
                   MOVE  "JIKANS1_HH1"     TO   MCP-WIDGET
               WHEN    60
                   MOVE  "JIKANS1_MM1"     TO   MCP-WIDGET
               WHEN    61
                   MOVE  "JIKANS1_HH2"     TO   MCP-WIDGET
               WHEN    62
                   MOVE  "JIKANS1_MM2"     TO   MCP-WIDGET
               WHEN    63
                   MOVE  "JIKANS2_HH1"     TO   MCP-WIDGET
               WHEN    64
                   MOVE  "JIKANS2_MM1"     TO   MCP-WIDGET
               WHEN    65
                   MOVE  "JIKANS2_HH2"     TO   MCP-WIDGET
               WHEN    66
                   MOVE  "JIKANS2_MM2"     TO   MCP-WIDGET
      *患者登録情報
               WHEN    201
                   MOVE  "BYOMEIKBN"       TO   MCP-WIDGET
               WHEN    202
                   MOVE  "JISYOKBN"        TO   MCP-WIDGET
               WHEN    203
                   MOVE  "HAKKOKBN"        TO   MCP-WIDGET
               WHEN    204
                   MOVE  "HOJYOKBN"        TO   MCP-WIDGET
               WHEN    205
                   MOVE  "DOUMEI_CHKFLG"   TO   MCP-WIDGET
               WHEN    206
                   MOVE  "JYUFUKU_CHKKBN"  TO   MCP-WIDGET
               WHEN    207
                   MOVE  "KOHONLY_CHKFLG"  TO   MCP-WIDGET
               WHEN    208
                   MOVE  "PTNUMDEL_FLG"    TO   MCP-WIDGET
               WHEN    209
                   MOVE  "HONKZKKBN"       TO   MCP-WIDGET
               WHEN    210
                   MOVE  "KZKNEN"          TO   MCP-WIDGET
               WHEN    211
                   MOVE  "HIHKNJAKBN"      TO   MCP-WIDGET
               WHEN    212
                   MOVE  "SETAIKBN"        TO   MCP-WIDGET
               WHEN    213
                   MOVE  "GRPPTLST"        TO   MCP-WIDGET
               WHEN    214
                   MOVE  "KOHRRK"          TO   MCP-WIDGET
               WHEN    215
                   MOVE  "POSTKBN"         TO   MCP-WIDGET
               WHEN    216
                   MOVE  "KENMEIKBN"       TO   MCP-WIDGET
               WHEN    217
                   MOVE  "TOWNNAME2KBN"    TO   MCP-WIDGET
               WHEN    218
                   MOVE  "KOHHBTKBN"       TO   MCP-WIDGET
               WHEN    219
                   MOVE  "HKNINITKBN"      TO   MCP-WIDGET
      *診療行為情報
               WHEN    301
                   MOVE  "DOSRYKBN_FLG"    TO   MCP-WIDGET
               WHEN    302
                   MOVE  "WKSRYHYO_FLG"    TO   MCP-WIDGET
               WHEN    303
                   MOVE  "FORMAL_FLG"      TO   MCP-WIDGET
               WHEN    304
                   MOVE  "AKUSEIHYO"       TO   MCP-WIDGET
      *H24.4
               WHEN    311
                   MOVE  "GENERIC_FLG"     TO   MCP-WIDGET
      *H24.4
               WHEN    305
                   MOVE  "TEISEIFLG"       TO   MCP-WIDGET
               WHEN    306
                   MOVE  "CHG310FLG"       TO   MCP-WIDGET
               WHEN    307
                   MOVE  "CHGSRYSYU"       TO   MCP-WIDGET
               WHEN    308
                   MOVE  "RESPRV038"       TO   MCP-WIDGET
               WHEN    309
                   MOVE  "HAIKIKBN"        TO   MCP-WIDGET
               WHEN    310
                   MOVE  "SEI20KSNKBN"     TO   MCP-WIDGET
               WHEN    314
                   MOVE  "SIN20KSNKBN"     TO   MCP-WIDGET
               WHEN    312
                   MOVE  "PRVLAYOUT038"    TO   MCP-WIDGET
               WHEN    313
                   MOVE  "SETALLKBN"       TO   MCP-WIDGET
      *H30.9
               WHEN    315
                   MOVE  "RECEKISAI_FLG"   TO   MCP-WIDGET
      *        請求選択
               WHEN    320
                   MOVE  "SRYNYKNKNRKBN"   TO   MCP-WIDGET
               WHEN    321
                   MOVE  "KAGRPFLG"        TO   MCP-WIDGET
               WHEN    322
                   MOVE  "HAKHOUFLG"       TO   MCP-WIDGET
               WHEN    323
                   MOVE  "TEIKINKBN"       TO   MCP-WIDGET
               WHEN    324
                   MOVE  "YKJCHKKBN"       TO   MCP-WIDGET
               WHEN    325
                   MOVE  "OKUSURIKBN"       TO   MCP-WIDGET
               WHEN    326
                   MOVE  "YKJOKUKBN"       TO   MCP-WIDGET
               WHEN    327
                   MOVE  "NYKHNKKBN"       TO   MCP-WIDGET
               WHEN    328
                   MOVE  "YMDHENKBN"       TO   MCP-WIDGET
               WHEN    330
                   MOVE  "GMNHYOKBN1"      TO   MCP-WIDGET
               WHEN    331
                   MOVE  "GMNHYOKBN2"      TO   MCP-WIDGET
               WHEN    332
                   MOVE  "GMNHYOKBN3"      TO   MCP-WIDGET
               WHEN    333
                   MOVE  "GMNHYOKBN4"      TO   MCP-WIDGET
               WHEN    334
                   MOVE  "GMNHYOKBN5"      TO   MCP-WIDGET
      *H28.10
               WHEN    349
                   MOVE  "GMNHYOKBN6"      TO   MCP-WIDGET
      *ver.4.7
               WHEN    335
                   MOVE  "BGCOLORSJ0"      TO   MCP-WIDGET
               WHEN    336
                   MOVE  "BGCOLORSDAY"     TO   MCP-WIDGET
               WHEN    337
                   MOVE  "BGCOLORSJ1"      TO   MCP-WIDGET
               WHEN    338
                   MOVE  "BGCOLORSJ2"      TO   MCP-WIDGET
               WHEN    339
                   MOVE  "BGCOLORSJ3"      TO   MCP-WIDGET
               WHEN    340
                   MOVE  "BGCOLORSJ4"      TO   MCP-WIDGET
               WHEN    341
                   MOVE  "BGCOLORSJ5"      TO   MCP-WIDGET
               WHEN    342
                   MOVE  "BGCOLORSJ6"      TO   MCP-WIDGET
               WHEN    343
                   MOVE  "BGCOLORSJ7"      TO   MCP-WIDGET
               WHEN    344
                   MOVE  "BGCOLORSJ8"      TO   MCP-WIDGET
               WHEN    345
                   MOVE  "BGCOLORS"        TO   MCP-WIDGET
               WHEN    346
                   MOVE  "BGLINEKBN"       TO   MCP-WIDGET
               WHEN    347
                   MOVE  "ERRCOLORS"       TO   MCP-WIDGET
               WHEN    348
                   MOVE  "KEICOLORS"       TO   MCP-WIDGET
      *収納情報
               WHEN    40
                   MOVE  "SYUSKYPRTG"      TO   MCP-WIDGET
      *
               WHEN    416
                   MOVE  "S03SKYMISYUPRT"  TO   MCP-WIDGET
               WHEN    45
                   MOVE  "SYUSRYPRTG"      TO   MCP-WIDGET
               WHEN    41
                   MOVE  "SYUSKYPRTN"      TO   MCP-WIDGET
               WHEN    46
                   MOVE  "SYUSRYPRTN"      TO   MCP-WIDGET
               WHEN    42
                   MOVE  "SYUNYKNKNRKBN"   TO   MCP-WIDGET
               WHEN    47
                   MOVE  "S06SKYPRT"       TO   MCP-WIDGET
               WHEN    48
                   MOVE  "S06SKYSUM"       TO   MCP-WIDGET
               WHEN    49
                   MOVE  "S02SKYSAI"       TO   MCP-WIDGET
               WHEN    417
                   MOVE  "S02SKYMISYUPRT"  TO   MCP-WIDGET
               WHEN    418
                   MOVE  "TUKISKYMISYUPRT" TO   MCP-WIDGET
               WHEN    43
                   MOVE  "SYUSTIME_HH"     TO   MCP-WIDGET
               WHEN    44
                   MOVE  "SYUSTIME_MM"     TO   MCP-WIDGET
               WHEN    400
                   MOVE  "SYUKARIST11_HH"  TO   MCP-WIDGET
               WHEN    401
                   MOVE  "SYUKARIST11_MM"  TO   MCP-WIDGET
               WHEN    402
                   MOVE  "SYUKARIED11_HH"  TO   MCP-WIDGET
               WHEN    403
                   MOVE  "SYUKARIED11_MM"  TO   MCP-WIDGET
               WHEN    404
                   MOVE  "SYUKARIST12_HH"  TO   MCP-WIDGET
               WHEN    405
                   MOVE  "SYUKARIST12_MM"  TO   MCP-WIDGET
               WHEN    406
                   MOVE  "SYUKARIED12_HH"  TO   MCP-WIDGET
               WHEN    407
                   MOVE  "SYUKARIED12_MM"  TO   MCP-WIDGET
               WHEN    408
                   MOVE  "SYUKARIST21_HH"  TO   MCP-WIDGET
               WHEN    409
                   MOVE  "SYUKARIST21_MM"  TO   MCP-WIDGET
               WHEN    410
                   MOVE  "SYUKARIED21_HH"  TO   MCP-WIDGET
               WHEN    411
                   MOVE  "SYUKARIED21_MM"  TO   MCP-WIDGET
               WHEN    412
                   MOVE  "SYUKARIST22_HH"  TO   MCP-WIDGET
               WHEN    413
                   MOVE  "SYUKARIST22_MM"  TO   MCP-WIDGET
               WHEN    414
                   MOVE  "SYUKARIED22_HH"  TO   MCP-WIDGET
               WHEN    415
                   MOVE  "SYUKARIED22_MM"  TO   MCP-WIDGET
               WHEN    419
                   MOVE  "S04SHIHARAIKBN"  TO   MCP-WIDGET
               WHEN    420
                   MOVE  "S04NYUGAIKBN"    TO   MCP-WIDGET
               WHEN    421
                   MOVE  "S04SRYKBN"       TO   MCP-WIDGET
               WHEN    422
                   MOVE  "S04KIKANKBN"     TO   MCP-WIDGET
               WHEN    423
                   MOVE  "S04DSPKBN"       TO   MCP-WIDGET
               WHEN    424
                   MOVE  "S04SUMKBN"    TO   MCP-WIDGET
               WHEN    425
                   MOVE  "S04SYORIKBN"     TO   MCP-WIDGET
               WHEN    426
                   MOVE  "S02SORTKBN"      TO   MCP-WIDGET
               WHEN    427
                   MOVE  "IKTSORTKBN"      TO   MCP-WIDGET
               WHEN    428
                   MOVE  "IKTKEISANKBN"    TO   MCP-WIDGET
               WHEN    429
                   MOVE  "IKTSRYYMKBN"     TO   MCP-WIDGET
               WHEN    430
                   MOVE  "TUKIGRP"         TO   MCP-WIDGET
               WHEN    431
                   MOVE  "TUKISUM"         TO   MCP-WIDGET
               WHEN    432
                   MOVE  "MISYUKIKANKBN"  TO   MCP-WIDGET
               WHEN    433
                   MOVE  "SAIKEISANSKYCHK" TO   MCP-WIDGET
               WHEN    434
                   MOVE  "SAIKEISANSKYCHK" TO   MCP-WIDGET
               WHEN    435
                   MOVE  "S02MODE"         TO   MCP-WIDGET
               WHEN    436
                   MOVE  "S02LIMIT"        TO   MCP-WIDGET
      *病名・コメント情報
               WHEN    500
                   MOVE  "MSG1_DSPKBN"     TO   MCP-WIDGET
               WHEN    501
                   MOVE  "MSG2_DSPKBN"     TO   MCP-WIDGET
               WHEN    502
                   MOVE  "MSG3_DSPKBN"     TO   MCP-WIDGET
               WHEN    503
                   MOVE  "MSG4_DSPKBN"     TO   MCP-WIDGET
               WHEN    504
                   MOVE  "SRYKA_DSPKBN"    TO   MCP-WIDGET
               WHEN    505
                   MOVE  "SYOKBN_DSPKBN"   TO   MCP-WIDGET
               WHEN    506
                   MOVE  "SJKBN"           TO   MCP-WIDGET
               WHEN    507
                   MOVE  "UTAGAIFLG_KBN"   TO   MCP-WIDGET
               WHEN    508
                   MOVE  "MSG5_DSPKBN"     TO   MCP-WIDGET
      *会計照会情報
               WHEN    600
                   MOVE  "SRYKBN_FLG"      TO   MCP-WIDGET
               WHEN    601
                   MOVE  "SRYKA_FLG"       TO   MCP-WIDGET
               WHEN    602
                   MOVE  "RESPRV043"       TO   MCP-WIDGET
               WHEN    603
                   MOVE  "PRVLAYOUT043"    TO   MCP-WIDGET
      *予約情報
               WHEN    700
                   MOVE  "INIT_DRCD"       TO   MCP-WIDGET
               WHEN    701
                   MOVE  "YYKINIT_FLG"     TO   MCP-WIDGET
               WHEN    702
                   MOVE  "YYKMAXCNT"       TO   MCP-WIDGET
               WHEN    703
                   MOVE  "YYKHYO_KBN"      TO   MCP-WIDGET
               WHEN    704
                   MOVE  "YYKPTNUM_KBN"    TO   MCP-WIDGET
               WHEN    705
                   MOVE  "YYKUKE_KBN"      TO   MCP-WIDGET
               WHEN    706
                   MOVE  "UKEHKNCHK"       TO   MCP-WIDGET
      *点数算定基準
               WHEN    800
                   MOVE  "DAKETUKBN"       TO   MCP-WIDGET
               WHEN    801
                   MOVE  "TEIRITUKBN"      TO   MCP-WIDGET
      *
               WHEN    999
                   MOVE  "B12"             TO   MCP-WIDGET
           END-EVALUATE
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    2   THRU    39
               WHEN    50  THRU    199
                   MOVE    0               TO  W07-PAGENO
                   MOVE    0               TO  SPA-GMN-PAGENO
               WHEN    200  THRU   299
                   MOVE    1               TO  W07-PAGENO
                   MOVE    1               TO  SPA-GMN-PAGENO
               WHEN    300 THRU    399
                   MOVE    2               TO  W07-PAGENO
                   MOVE    2               TO  SPA-GMN-PAGENO
               WHEN    40  THRU    49
               WHEN    400 THRU    499
                   MOVE    3               TO  W07-PAGENO
                   MOVE    3               TO  SPA-GMN-PAGENO
               WHEN    500 THRU    599
                   MOVE    4               TO  W07-PAGENO
                   MOVE    4               TO  SPA-GMN-PAGENO
               WHEN    600 THRU    699
                   MOVE    5               TO  W07-PAGENO
                   MOVE    5               TO  SPA-GMN-PAGENO
               WHEN    700 THRU    799
                   MOVE    6               TO  W07-PAGENO
                   MOVE    6               TO  SPA-GMN-PAGENO
               WHEN    800 THRU    899
                   MOVE    7               TO  W07-PAGENO
                   MOVE    7               TO  SPA-GMN-PAGENO
               WHEN    OTHER
                   MOVE    SPA-GMN-PAGENO  TO  W07-PAGENO
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC           SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "INITAUTO"
                   IF      SPA-GMN-INITAUTO    =   "0"
                       MOVE    8               TO  SPA-GMN-CUR
                   ELSE
                       MOVE    2               TO  SPA-GMN-CUR
                   END-IF
               WHEN    "DATA07"
                   IF      SPA-GMN-DATA(7)     =   "1"
                       MOVE    3           TO  SPA-GMN-CUR
                   ELSE
                       MOVE    4           TO  SPA-GMN-CUR
                   END-IF
               WHEN    "SRYKACD"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "DATA05"
                   MOVE    5               TO  SPA-GMN-CUR
               WHEN    "BYOMEIAUTO"
                   MOVE    6               TO  SPA-GMN-CUR
               WHEN    "LASTKIKAN"
                   MOVE    7               TO  SPA-GMN-CUR
               WHEN    "CHUTOTENKAI"
                   MOVE    8               TO  SPA-GMN-CUR
               WHEN    "NYUSYOSIN"
                   MOVE    9               TO  SPA-GMN-CUR
               WHEN    "JIKANTOKUNYU"
                   MOVE    10              TO  SPA-GMN-CUR
               WHEN    "DATA06"
                   MOVE    11              TO  SPA-GMN-CUR
               WHEN    "DATA01"
                   MOVE    12              TO  SPA-GMN-CUR
               WHEN    "DATA02"
                   MOVE    13              TO  SPA-GMN-CUR
               WHEN    "DATA03"
                   MOVE    14              TO  SPA-GMN-CUR
               WHEN    "DATA04"
                   MOVE    15              TO  SPA-GMN-CUR
               WHEN    "GAZOUKSN1"
                   MOVE    16              TO  SPA-GMN-CUR
               WHEN    "KENSAAUTO"
                   MOVE    17              TO  SPA-GMN-CUR
               WHEN    "NYUKENSA"
                   MOVE    18              TO  SPA-GMN-CUR
               WHEN    "TEISEI"
                   MOVE    19              TO  SPA-GMN-CUR
               WHEN    "TEISEITOKU"
                   MOVE    20              TO  SPA-GMN-CUR
               WHEN    "TEINYUAUTO"
      *********    MOVE    30              TO  SPA-GMN-CUR
                   MOVE    36              TO  SPA-GMN-CUR
      *H29.3
               WHEN    "TEISHOHOU"
                   MOVE    30              TO  SPA-GMN-CUR
               WHEN    "KIKAN"
     ******        MOVE    31              TO  SPA-GMN-CUR
                   MOVE    35              TO  SPA-GMN-CUR
      *H28.8
               WHEN    "KINKCHK"
                   MOVE    31              TO  SPA-GMN-CUR
               WHEN    "GAIKANKBN"
                   MOVE    32              TO  SPA-GMN-CUR
               WHEN    "GAIMOVE"
                   MOVE    33              TO  SPA-GMN-CUR
               WHEN    "SURYOZERO"
                   MOVE    34              TO  SPA-GMN-CUR
               WHEN    "CHOKICHK"
                   MOVE    50              TO  SPA-GMN-CUR
      *        時間外加算チェック
               WHEN    "JIKANTOKU_CHK"
                   IF      SPA-GMN-JIKANTOKU-CHK   =   "1"
                       MOVE    51          TO  SPA-GMN-CUR
                   ELSE
                       MOVE    81          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "JIKANW1_HH1"
                   MOVE    52              TO  SPA-GMN-CUR
               WHEN    "JIKANW1_MM1"
                   MOVE    53              TO  SPA-GMN-CUR
               WHEN    "JIKANW1_HH2"
                   MOVE    54              TO  SPA-GMN-CUR
               WHEN    "JIKANW1_MM2"
                   MOVE    55              TO  SPA-GMN-CUR
               WHEN    "JIKANW2_HH1"
                   MOVE    56              TO  SPA-GMN-CUR
               WHEN    "JIKANW2_MM1"
                   MOVE    57              TO  SPA-GMN-CUR
               WHEN    "JIKANW2_HH2"
                   MOVE    58              TO  SPA-GMN-CUR
               WHEN    "JIKANW2_MM2"
                   MOVE    59              TO  SPA-GMN-CUR
               WHEN    "JIKANS1_HH1"
                   MOVE    60              TO  SPA-GMN-CUR
               WHEN    "JIKANS1_MM1"
                   MOVE    61              TO  SPA-GMN-CUR
               WHEN    "JIKANS1_HH2"
                   MOVE    62              TO  SPA-GMN-CUR
               WHEN    "JIKANS1_MM2"
                   MOVE    63              TO  SPA-GMN-CUR
               WHEN    "JIKANS2_HH1"
                   MOVE    64              TO  SPA-GMN-CUR
               WHEN    "JIKANS2_MM1"
                   MOVE    65              TO  SPA-GMN-CUR
               WHEN    "JIKANS2_HH2"
                   MOVE    66              TO  SPA-GMN-CUR
               WHEN    "JIKANS2_MM2"
                   MOVE    81              TO  SPA-GMN-CUR
               WHEN    "CLEARCHK"
                   MOVE    82              TO  SPA-GMN-CUR
               WHEN    "YAKJYOCHK"
                   MOVE    83              TO  SPA-GMN-CUR
               WHEN    "ZENHKNCHK"
      ********     MOVE    84              TO  SPA-GMN-CUR
                   MOVE    87              TO  SPA-GMN-CUR
               WHEN    "TECHOKBN"
      ************ MOVE    84              TO  SPA-GMN-CUR
                   MOVE    88              TO  SPA-GMN-CUR
      *
               WHEN    "JGAIKEIKBN"
      *********    MOVE    84              TO  SPA-GMN-CUR
                   MOVE    89              TO  SPA-GMN-CUR
               WHEN    "TOUYOCHK"
      *******      MOVE    84              TO  SPA-GMN-CUR
                   MOVE    90              TO  SPA-GMN-CUR
      *
               WHEN    "SHOHOU30CHK"
                   MOVE    84              TO  SPA-GMN-CUR
      *
               WHEN    "HOUKATU"
                   MOVE    85              TO  SPA-GMN-CUR
               WHEN    "BYOHYOKBN"
                   MOVE    86              TO  SPA-GMN-CUR
               WHEN    "ZAIJUNKBN"
                   MOVE    999              TO  SPA-GMN-CUR
      *        患者登録情報
               WHEN    "BYOMEIKBN"
                   MOVE    202              TO  SPA-GMN-CUR
               WHEN    "JISYOKBN"
                   MOVE    203              TO  SPA-GMN-CUR
               WHEN    "HAKKOKBN"
                   MOVE    204              TO  SPA-GMN-CUR
               WHEN    "HOJYOKBN"
                   MOVE    205              TO  SPA-GMN-CUR
               WHEN    "DOUMEI_CHKFLG"
                   MOVE    206              TO  SPA-GMN-CUR
               WHEN    "JYUFUKU_CHKKBN"
                   MOVE    207              TO  SPA-GMN-CUR
               WHEN    "KOHONLY_CHKFLG"
                   MOVE    208              TO  SPA-GMN-CUR
               WHEN    "PTNUMDEL_FLG"
                   MOVE    209              TO  SPA-GMN-CUR
               WHEN    "HONKZKKBN"
                   MOVE    210              TO  SPA-GMN-CUR
               WHEN    "KZKNEN"
                   MOVE    211              TO  SPA-GMN-CUR
               WHEN    "HIHKNJAKBN"
                   MOVE    212              TO  SPA-GMN-CUR
               WHEN    "SETAIKBN"
                   MOVE    213              TO  SPA-GMN-CUR
               WHEN    "GRPPTLST"
                   MOVE    214              TO  SPA-GMN-CUR
               WHEN    "KOHRRK"
                   MOVE    215              TO  SPA-GMN-CUR
               WHEN    "POSTKBN"
                   MOVE    216              TO  SPA-GMN-CUR
               WHEN    "KENMEIKBN"
                   MOVE    217              TO  SPA-GMN-CUR
               WHEN    "TOWNNAME2KBN"
                   MOVE    218              TO  SPA-GMN-CUR
               WHEN    "KOHHBTKBN"
                   MOVE    219              TO  SPA-GMN-CUR
               WHEN    "HKNINITKBN"
                   MOVE    999              TO  SPA-GMN-CUR
      *    診療行為情報
               WHEN    "DOSRYKBN_FLG"
                   MOVE    302             TO  SPA-GMN-CUR
               WHEN    "WKSRYHYO_FLG"
                   MOVE    303             TO  SPA-GMN-CUR
               WHEN    "FORMAL_FLG"
                   MOVE    304             TO  SPA-GMN-CUR
               WHEN    "AKUSEIHYO" 
      **********   MOVE    305             TO  SPA-GMN-CUR
                   MOVE    311             TO  SPA-GMN-CUR
      *H24.4
               WHEN    "GENERIC_FLG" 
      *H30.9
     *****         MOVE    305             TO  SPA-GMN-CUR
                   MOVE    315             TO  SPA-GMN-CUR
      *H30.9
               WHEN    "RECEKISAI_FLG" 
                   MOVE    305             TO  SPA-GMN-CUR
      *
               WHEN    "TEISEIFLG"
                   MOVE    306             TO  SPA-GMN-CUR
               WHEN    "CHG310FLG"
      ********     MOVE    320             TO  SPA-GMN-CUR
                   MOVE    307             TO  SPA-GMN-CUR
               WHEN    "CHGSRYSYU"
      **********   MOVE    320             TO  SPA-GMN-CUR
                   MOVE    308             TO  SPA-GMN-CUR
               WHEN    "RESPRV038"
                   MOVE    309             TO  SPA-GMN-CUR
               WHEN    "HAIKIKBN"
      *********    MOVE    320             TO  SPA-GMN-CUR
                   MOVE    310             TO  SPA-GMN-CUR
               WHEN    "SEI20KSNKBN"
      *ver.4.7     MOVE    320             TO  SPA-GMN-CUR
      *H24.12      MOVE    330             TO  SPA-GMN-CUR
      ****         MOVE    312             TO  SPA-GMN-CUR
                   MOVE    314             TO  SPA-GMN-CUR
      *H29.7
      *        心身療法
               WHEN    "SIN20KSNKBN"
                   MOVE    312             TO  SPA-GMN-CUR
      *
               WHEN    "PRVLAYOUT038"
      *H28.6 ***   MOVE    330             TO  SPA-GMN-CUR
                   MOVE    313             TO  SPA-GMN-CUR
               WHEN    "SETALLKBN"
                   MOVE    330             TO  SPA-GMN-CUR
               WHEN    "GMNHYOKBN1"
                   MOVE    331             TO  SPA-GMN-CUR
               WHEN    "GMNHYOKBN2"
                   MOVE    332             TO  SPA-GMN-CUR
               WHEN    "GMNHYOKBN3"
                   MOVE    333             TO  SPA-GMN-CUR
               WHEN    "GMNHYOKBN4"
                   MOVE    334             TO  SPA-GMN-CUR
               WHEN    "GMNHYOKBN5"
      *H29.10      MOVE    320             TO  SPA-GMN-CUR
                   MOVE    349             TO  SPA-GMN-CUR
               WHEN    "GMNHYOKBN6"
                   MOVE    320             TO  SPA-GMN-CUR
      *ver.4.7
               WHEN    "SRYNYKNKNRKBN"
                   MOVE    321             TO  SPA-GMN-CUR
               WHEN    "KAGRPFLG"
                   MOVE    322             TO  SPA-GMN-CUR
               WHEN    "HAKHOUFLG"
                   MOVE    323             TO  SPA-GMN-CUR
               WHEN    "TEIKINKBN"
                   MOVE    324             TO  SPA-GMN-CUR
               WHEN    "YKJCHKKBN"
                   MOVE    325             TO  SPA-GMN-CUR
               WHEN    "OKUSURIKBN"
                   MOVE    326             TO  SPA-GMN-CUR
               WHEN    "YKJOKUKBN"
                   MOVE    327             TO  SPA-GMN-CUR
               WHEN    "NYKHNKKBN"
                   MOVE    328             TO  SPA-GMN-CUR
               WHEN    "YMDHENKBN"
      ****         MOVE    999             TO  SPA-GMN-CUR
                   MOVE    335             TO  SPA-GMN-CUR
      *ver.4.7
               WHEN    "BGCOLORSJ0"
                   MOVE    336             TO  SPA-GMN-CUR
               WHEN    "BGCOLORSDAY"
                   MOVE    337             TO  SPA-GMN-CUR
               WHEN    "BGCOLORSJ1"
                   MOVE    338             TO  SPA-GMN-CUR
               WHEN    "BGCOLORSJ2"
                   MOVE    339             TO  SPA-GMN-CUR
               WHEN    "BGCOLORSJ3"
                   MOVE    340             TO  SPA-GMN-CUR
               WHEN    "BGCOLORSJ4"
                   MOVE    341             TO  SPA-GMN-CUR
               WHEN    "BGCOLORSJ5"
                   MOVE    342             TO  SPA-GMN-CUR
               WHEN    "BGCOLORSJ6"
                   MOVE    343             TO  SPA-GMN-CUR
               WHEN    "BGCOLORSJ7"
                   MOVE    344             TO  SPA-GMN-CUR
               WHEN    "BGCOLORSJ8"
                   MOVE    345             TO  SPA-GMN-CUR
               WHEN    "BGCOLORS"
                   MOVE    346             TO  SPA-GMN-CUR
               WHEN    "BGLINEKBN"
                   MOVE    347             TO  SPA-GMN-CUR
               WHEN    "ERRCOLORS"
                   MOVE    348             TO  SPA-GMN-CUR
               WHEN    "KEICOLORS"
                   MOVE    999             TO  SPA-GMN-CUR
      *
      *収納情報
               WHEN    "S02SKYSAI"
                   MOVE    417             TO  SPA-GMN-CUR
               WHEN    "S02SKYMISYUPRT"
                   MOVE    430             TO  SPA-GMN-CUR
               WHEN    "TUKIGRP"
                   MOVE    418             TO  SPA-GMN-CUR
               WHEN    "TUKISKYMISYUPRT"
                   MOVE    431             TO  SPA-GMN-CUR
               WHEN    "TUKISUM"
                   MOVE    435             TO  SPA-GMN-CUR
               WHEN    "S02MODE"
      *            MOVE    436             TO  SPA-GMN-CUR
      *        WHEN    "S02LIMIT"
                   MOVE    426             TO  SPA-GMN-CUR
               WHEN    "S02SORTKBN"
                   MOVE    433             TO  SPA-GMN-CUR
               WHEN    "SAIKEISANSKYCHK"
                   MOVE    427             TO  SPA-GMN-CUR
               WHEN    "IKTSORTKBN"
                   MOVE    428             TO  SPA-GMN-CUR
               WHEN    "IKTKEISANKBN"
                   MOVE    40              TO  SPA-GMN-CUR
               WHEN    "SYUSKYPRTG"
                   MOVE    416             TO  SPA-GMN-CUR
               WHEN    "S03SKYMISYUPRT"
                   MOVE    45              TO  SPA-GMN-CUR
               WHEN    "SYUSRYPRTG"
                   MOVE    41              TO  SPA-GMN-CUR
               WHEN    "SYUSKYPRTN"
                   MOVE    46              TO  SPA-GMN-CUR
               WHEN    "SYUSRYPRTN"
                   MOVE    419             TO  SPA-GMN-CUR
               WHEN    "S04SHIHARAIKBN"
                   MOVE    420             TO  SPA-GMN-CUR
               WHEN    "S04NYUGAIKBN"
                   MOVE    421             TO  SPA-GMN-CUR
               WHEN    "S04SRYKBN"
                   MOVE    422             TO  SPA-GMN-CUR
               WHEN    "S04KIKANKBN"
                   IF    ( SPA-GMN-S04KIKANKBN =   "1" )
                       MOVE    423         TO  SPA-GMN-CUR
                   ELSE
                       MOVE    42          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "S04DSPKBN"
                   IF    ( SPA-GMN-S04KIKANKBN =   "1" )
                       MOVE    424         TO  SPA-GMN-CUR
                   ELSE
                       MOVE    42          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "S04SUMKBN"
                   IF    ( SPA-GMN-S04KIKANKBN =   "1" )
                       MOVE    425         TO  SPA-GMN-CUR
                   ELSE
                       MOVE    42          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "S04SYORIKBN"
                   IF    ( SPA-GMN-S04KIKANKBN =   "1" )
                       MOVE    434         TO  SPA-GMN-CUR
                   ELSE
                       MOVE    42          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "S04JIHIPRTKBN"
                   MOVE    42              TO  SPA-GMN-CUR
               WHEN    "SYUNYKNKNRKBN"
                   MOVE    429             TO  SPA-GMN-CUR
               WHEN    "IKTSRYYMKBN"
                   MOVE    47              TO  SPA-GMN-CUR
               WHEN    "S06SKYPRT"
                   MOVE    48              TO  SPA-GMN-CUR
               WHEN    "S06SKYSUM"
                   MOVE    43              TO  SPA-GMN-CUR
               WHEN    "SYUSTIME_HH"
                   MOVE    44              TO  SPA-GMN-CUR
               WHEN    "SYUSTIME_MM"
                   MOVE    400             TO  SPA-GMN-CUR
               WHEN    "SYUKARIST11_HH"
                   MOVE    401             TO  SPA-GMN-CUR
               WHEN    "SYUKARIST11_MM"
                   MOVE    402             TO  SPA-GMN-CUR
               WHEN    "SYUKARIED11_HH"
                   MOVE    403             TO  SPA-GMN-CUR
               WHEN    "SYUKARIED11_MM"
                   MOVE    404             TO  SPA-GMN-CUR
               WHEN    "SYUKARIST12_HH"
                   MOVE    405             TO  SPA-GMN-CUR
               WHEN    "SYUKARIST12_MM"
                   MOVE    406             TO  SPA-GMN-CUR
               WHEN    "SYUKARIED12_HH"
                   MOVE    407             TO  SPA-GMN-CUR
               WHEN    "SYUKARIED12_MM"
                   MOVE    408             TO  SPA-GMN-CUR
               WHEN    "SYUKARIST21_HH"
                   MOVE    409             TO  SPA-GMN-CUR
               WHEN    "SYUKARIST21_MM"
                   MOVE    410             TO  SPA-GMN-CUR
               WHEN    "SYUKARIED21_HH"
                   MOVE    411             TO  SPA-GMN-CUR
               WHEN    "SYUKARIED21_MM"
                   MOVE    412             TO  SPA-GMN-CUR
               WHEN    "SYUKARIST22_HH"
                   MOVE    413              TO  SPA-GMN-CUR
               WHEN    "SYUKARIST22_MM"
                   MOVE    414              TO  SPA-GMN-CUR
               WHEN    "SYUKARIED22_HH"
                   MOVE    415             TO  SPA-GMN-CUR
               WHEN    "SYUKARIED22_MM"
                   MOVE    432             TO  SPA-GMN-CUR
               WHEN    "MISYUKIKANKBN"
                   MOVE    999             TO  SPA-GMN-CUR
      *病名コメント情報
               WHEN    "MSG1_DSPKBN"
                   MOVE    501              TO  SPA-GMN-CUR
               WHEN    "MSG2_DSPKBN"
                   MOVE    502             TO  SPA-GMN-CUR
               WHEN    "MSG3_DSPKBN"
                   MOVE    503             TO  SPA-GMN-CUR
               WHEN    "MSG4_DSPKBN"
                   MOVE    508             TO  SPA-GMN-CUR
               WHEN    "MSG5_DSPKBN"
                   MOVE    504             TO  SPA-GMN-CUR
               WHEN    "SRYKA_DSPKBN"
                   MOVE    505             TO  SPA-GMN-CUR
               WHEN    "SYOKBN_DSPKBN"
                   MOVE    507             TO  SPA-GMN-CUR
               WHEN    "UTAGAIFLG_KBN"
                   MOVE    506             TO  SPA-GMN-CUR
               WHEN    "SJKBN"
                   MOVE    999             TO  SPA-GMN-CUR
      *会計照会情報
               WHEN    "SRYKBN_FLG"
                   MOVE    601             TO  SPA-GMN-CUR
               WHEN    "SRYKA_FLG"
      ***********  MOVE    999             TO  SPA-GMN-CUR
                   MOVE    602             TO  SPA-GMN-CUR
               WHEN    "RESPRV043"
                   MOVE    603             TO  SPA-GMN-CUR
               WHEN    "PRVLAYOUT043"
                   MOVE    999             TO  SPA-GMN-CUR
      *予約情報
               WHEN    "INIT_DRCD"
                   MOVE    701             TO  SPA-GMN-CUR
               WHEN    "YYKINIT_FLG"
                   MOVE    702             TO  SPA-GMN-CUR
               WHEN    "YYKMAXCNT"
                   MOVE    703             TO  SPA-GMN-CUR
               WHEN    "YYKHYO_KBN"
                   MOVE    704             TO  SPA-GMN-CUR
               WHEN    "YYKPTNUM_KBN"
                   MOVE    705             TO  SPA-GMN-CUR
               WHEN    "YYKUKE_KBN"
                   MOVE    706             TO  SPA-GMN-CUR
               WHEN    "UKEHKNCHK"
                   MOVE    999             TO  SPA-GMN-CUR
      *点数算定基準
               WHEN    "DAKETUKBN"
                   MOVE    801             TO  SPA-GMN-CUR
               WHEN    "TEIRITUKBN"
                   MOVE    999             TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           IF      WRK-YMD  (1:7)  NOT NUMERIC
               INSPECT     WRK-YMD      REPLACING   ALL " " BY  "0"
               MOVE    WRK-YMD(1:1)     TO  WRK-WGO
               MOVE    WRK-YMD(2:2)     TO  WRK-WYY
               MOVE    WRK-YMD(5:2)     TO  WRK-WMM
               MOVE    WRK-YMD(8:2)     TO  WRK-WDD
           ELSE
               MOVE    WRK-YMD(1:7)     TO  WRK-WYMD
           END-IF
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "12"                TO  LNK-DAY1-IRAI
           MOVE    ZERO                TO  LNK-DAY1-YMD
           MOVE    WRK-WYMD            TO  LNK-DAY1-YMD(2:7)
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
               MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
           END-IF
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
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC        SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       990-SYSKANRI-READ-SEC                SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       990-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
