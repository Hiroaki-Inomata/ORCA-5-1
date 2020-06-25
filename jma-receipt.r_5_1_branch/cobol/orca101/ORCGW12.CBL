      ********************************************************************
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
       PROGRAM-ID.             ORCGW12.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : レセプト印刷情報（Ｗ１２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     02/02/26  レセ電区分、レセ電出力先の追加
      *  01.00.02    MCC-藤原     02/03/28  国保ＯＣＲ区分、国保診療報酬請求書
      *                                     区分の追加
      *  01.00.03    MCC-藤原     02/04/03  県単用項目編集プログラム名の追加
      *  01.00.04    MCC-藤原     02/05/13  病名編集区分１（診療科別）の追加
      *  01.00.05    MCC-藤原     02/06/04  転帰日編集区分の追加
      *  01.00.06    MCC-藤原     02/07/04  出力先プリンタ情報の追加
      *  02.00.01    NACL-多々納  02/11/13  一括入力 
      *  02.00.02    NACL-藤原    03/01/08  通し番号編集区分の追加 
      *  02.00.03    NACL-藤原    03/02/27  入院用県単用項目編集プログラム名の追加
      *  02.00.04    NACL-藤原    03/03/07  未入力時のチェック
      *  02.00.05    NACL-藤原    03/04/09  続紙区分（入院）の追加
      *  02.00.06    NACL-藤原    03/07/02  主病名編集フラグの追加
      *  02.00.07    NACL-藤原    03/08/06  レセ電の設定の修正
      *  02.00.08    NACL-藤原    03/08/21  続紙区分（入院外）
      *                                     ・続紙出力先の追加
      *  02.00.09    NACL-太田    03/11/18  老人と原爆区分の追加
      *  02.00.10    NACL-太田    04/01/15  自費レセプト摘要欄記載区分追加
      *  02.00.11    NACL-藤原    04/02/18  社保ＯＣＲ区分の追加
      *  02.00.12    NACL-藤原    04/03/05  主病名編集フラグに区分を追加
      *  02.00.13    NACL-藤原    04/03/29  続紙区分に縦形式（長）追加
      *  02.00.14    NACL-藤原    04/04/09  レセプトコメント（症状詳記）記録区分
      *                                     ・診療科編集区分の追加
      *  02.00.15    NACL-太田    04/04/15  プリンタ名の指定を入／外分ける
      *  02.00.16    NACL-藤原    04/07/02  出力順・自動遷移区分を追加
      *  02.00.17    NACL-藤原    04/09/29  レセ電・ＯＣＲ区分の設定に追加
      *
      *  02.07.01    NACL-藤原    05/08/20  特別療養費対応
      *                                     「特別療養費」文字記載位置
      *                                     給付割合欄記載区分
      *                                     レセプト種別欄記載区分
      *                                     を追加 
      *
      *  02.07.02    NACL-藤原    05/12/12  国保レセプト編綴順対応
      *                                     MONFUNC対応
      *  02.07.03    NACL-藤原    06/02/01  主科対応
      *  
      *  02.09.01    NACL-藤原    06/05/12  改定通信医１６０４９号
      *                                     レセ電日計表レコード記録条件
      *                                     の追加
      *  3.0.0
      *  03.00.01    NACL-藤原    06/05/15  入退院履歴の記載区分の追加
      *                                     （入院外レセプトのみ）
      *
      *  03.02.01    NACL-藤原    06/07/11  労災・自賠責のプリンタ指定対応
      *                                     （入院外・入院別）
      *  03.02.02    NACL-藤原    06/07/24  マル長の別総括対応
      *  03.02.03    NACL-藤原    06/07/26  自賠責の枠印刷選択機能対応
      *  03.02.04    NACL-藤原    06/08/01  国保主科対応
      *
      *  03.04.01    NACL-藤原    07/01/16  レセプト一括作成出力順区分
      *                                     （全体）の追加
      *  03.04.05    NACL-藤原    07/01/24  レセプト作成出力順区分
      *                                     種別・カナ氏名順、種別・患者
      *                                     番号順の追加
      *  03.04.06    NACL-藤原    07/02/07  編綴詳細設定画面の追加
      *
      *  03.05.01    NACL-多々納  07/05/02  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/19  自賠責の様式選択
      *                                     （平成１９年４月改正様式）
      *                                     印刷後のチェック解除選択
      *                                     病棟番号記載区分
      *                                     の追加
      *
      *  04.02.01    NACL-藤原    08/03/04  平成２０年４月改正対応
      *  04.02.02    NACL-藤原    08/04/21  広域連合診療報酬請求書区分
      *                                     の追加
      *
      *  04.03.00    NACL-太田    08/06/17  タブ切替ボタン追加
      *  04.03.01    NACL-藤原    08/06/19  医師別出力対応
      *  04.03.02    NACL-藤原    08/08/07  労災コメント記載区分
      *                                     入院料未算定時コメント記載区分
      *                                     労災枠記載区分の追加
      *
      *  04.04.01    NACL-藤原    08/11/19  レセ電ＣＤ−Ｒ出力対応
      *  04.04.02    NACL-藤原    08/11/20  レセ電出力先チェック追加
      *  04.04.03    NACL-藤原    08/11/25  光ディスク等送付書作成区分、
      *                                     症状詳記作成区分追加
      *  04.04.04    NACL-藤原    09/04/02  レセ電出力先チェックを登録時
      *                                     の確認ダイアログに変更
      *
      *  04.05.01    NACL-藤原    09/06/11  病棟別出力対応
      *  04.05.02    NACL-藤原    09/08/17  生保等まとめ入力画面表示順
      *                                     設定対応
      *  04.05.03    NACL-藤原    09/08/19  全件印刷時の自費レセプトの
      *                                     印刷指定対応
      *  04.05.04    NACL-藤原    09/08/21  レセ電データ格納フォルダ設定
      *                                     対応
      *                                     ファイル出力先のデフォルト値
      *                                     の設定も可能にする
      *  04.05.05    NACL-藤原    11/02/22  レセ電データの記号番号区切り
      *                                     文字設定対応
      *  04.05.06    NACL-藤原    11/03/25  災害対応
      *                                     災害該当レセプトのレセ電作成
      *                                     区分の追加
      *  04.05.07    NACL-藤原    11/04/18  災害対応
      *                                     保険者が特定できない災害該当
      *                                     レセプトのレセ電記録のデフォ
      *                                     ルトを「1 記録する」とする
      *  04.05.08    NACL-藤原    11/08/02  労災新様式対応
      *                                     点検レセ印字位置変更対応
      *  04.05.09    NACL-藤原    12/05/10  労災様式の表示変更
      *
      *  04.06.01    NACL-藤原    10/06/23  レセ電請求時の受付エラー分の
      *                                     種別不明レセ対応
      *  04.06.02    NACL-藤原    10/07/10  病名記載順設定対応
      *  04.06.03    NACL-藤原    11/12/12  点検レセ０点公費印字対応
      *  04.06.04    NACL-藤原    12/07/04  表示期間によるレセプト記載対応
      *                                     転帰済傷病名記載区分の追加
      *  04.06.05    NACL-藤原    12/09/22  点検用・提出用初期表示設定対応
      *  04.06.06    NACL-藤原    12/12/21  入院料の算定がない入退院履歴
      *                                     の記載対応
      *                                     入退院履歴の記載区分（入院）
      *                                     の追加
      *
      *  04.07.01    NACL-藤原    11/06/02  入退院履歴の記載区分に
      *                                     自費入院分の記載設定を追加
      *  04.07.02    NACL-藤原    11/06/08  レセ電作成設定時の広域連合作
      *                                     成区分との関連チェックを追加
      *                                     広域連合作成区分のデフォルト
      *                                     表示を国保分とまとめるに変更
      *  04.07.03    NACL-藤原    12/02/13  健保組合への直接請求と審査
      *                                     支払機関請求対応
      *  04.07.04    NACL-藤原    12/03/25  公費記載順設定対応
      *  04.07.05    NACL-藤原    12/05/28  タブラベル対応
      *  04.07.06    NACL-藤原    12/08/01  レセ電データチェック機能対応
      *  04.07.07    NACL-藤原    12/09/24  点検用レセプトの医師名記載対応
      *  04.07.08    NACL-藤原    12/10/31  ファイル出力先設定のデフォルト
      *                                     変更
      *  04.07.09    NACL-藤原    12/12/03  ターミナルデジットによる出力対応
      *  04.07.10    NACL-藤原    13/02/07  労災レセ電作成対応
      *  04.07.11    NACL-藤原    13/06/25  レセ電請求時の受付エラー分の
      *                                     自費レセプト対象の有無設定
      *  04.07.12    NACL-藤原    13/06/26  入院料他保険算定時コメント記載
      *                                     対応
      *  04.07.13    NACL-藤原    13/12/04  自賠責（労災準拠）のレセプト
      *                                     様式追加対応
      *  04.07.14    NACL-藤原    14/03/20  上限０円時の食事欄記載対応
      *                                    （法別２４・７９）
      *  04.07.15    NACL-藤原    15/05/14  ０点全国公費レセプト記載区分
      *                                    （提出用レセプト）対応
      *  04.07.16    NACL-藤原    15/06/15  「自賠責・様式選択」の初期表
      *                                     示変更対応
      *  04.07.17    NACL-藤原    15/06/18  光ディスク等送付書の媒体種類
      *                                     設定対応
      *  04.07.18    NACL-藤原    15/07/17  労災診療費請求書及びアフター
      *                                     ケア委託費請求書のＯＣＲ様式
      *                                     対応
      *
      *  04.08.01    NACL-藤原    12/01/08  包括診療分の点検用レセプト記
      *                                     載対応
      *  04.08.02    NACL-藤原    14/08/26  fdd 廃止に伴う対応
      *  04.08.03    NACL-藤原    14/08/28  ４．８ミドル対応
      *  04.08.04    NACL-藤原    15/08/17  自賠責レセプトの請求金額欄端
      *                                     数処理対応
      *  04.08.05    NACL-藤原    15/09/01  保険履歴の記載対応
      *  04.08.06    NACL-藤原    15/11/30  現物給付一部負担金記載対応
      *                                    （法別２４・７９）
      *  04.08.07    NACL-藤原    16/01/18  点検用レセプト一般名処方記載
      *                                     対応
      *  04.08.06    NACL-藤原    16/04/20  湿布薬用法レセプト記載対応
      *  04.08.07    NACL-藤原    16/06/13  社保診療報酬請求書の災害該当
      *                                     分のまとめ対応
      *  04.08.08    NACL-藤原    16/08/08  災害該当分（非減免対象と減免対象）
      *                                     レセプトの分割対応
      *  04.08.09    NACL-藤原    16/08/24  持参薬対応
      *  04.08.10    NACL-藤原    16/09/21  １５円以下薬剤のレセプト記載対応
      *  04.08.11    NACL-藤原    16/10/07  自賠責ＰＤＦファイル作成対応
      *  04.08.12    NACL-藤原    17/05/26  直接請求する保険者に地方職員
      *                                     共済組合を追加
      *  04.08.13    NACL-藤原    18/01/26  全国公費一部負担金記載対応
      *                                    （公費のみのレセプト）
      *
      *  05.00.01    NACL-藤原    19/02/13  上限０円時の食事欄記載
      *                                    （法別２４・７９）に追加
      *  05.00.02    NACL-藤原    19/03/25  元号改正対応
      *                                     Warning 対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *    
      *    レセ電データ出力先チェック用ワークファイル
           SELECT  WRK-FILE        ASSIGN  WRKPARA
                                   FILE    STATUS  IS  STS-WRK.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    レセ電データ出力先チェック用ワークファイル
       FD  WRK-FILE.
       01  WRK-R                       PIC X(10).
      *
       WORKING-STORAGE         SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-W12.
           03  SPA-W12-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(03).
               05  SPA-GMN-CUR         PIC 9(03).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-HSTYUKYMD       PIC X(09).
                   07  SPA-GMN-HEDYUKYMD       PIC X(09).              
      *
                   07  SPA-GMN-KOUMOKU.
                       09  SPA-GMN-BYOMEIHENKBN-G.
                           11  SPA-GMN-BYOMEIHENKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-BYOMEIHENKBNMEI
                                              PIC X(18).
                       09  SPA-GMN-BYOMEIHENKBN1-G.
                           11  SPA-GMN-BYOMEIHENKBN1
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-BYOMEIHENKBN1MEI
                                              PIC X(20).
                       09  SPA-GMN-SYUBYOHENFLG-G.
                           11  SPA-GMN-SYUBYOHENFLG
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-SYUBYOHENFLGMEI
                                              PIC X(44).
                       09  SPA-GMN-TENKIHENKBN-G.
                           11  SPA-GMN-TENKIHENKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-TENKIHENKBNMEI
                                              PIC X(18).
                       09  SPA-GMN-TENKIBYOMEI-KBN-G.
                           11  SPA-GMN-TENKIBYOMEI-KBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-TENKIBYOMEI-KBNMEI
                                              PIC X(50).
                       09  SPA-GMN-KKHRECEKBN-G.
                           11  SPA-GMN-KKHRECEKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-KKHRECEKBNMEI
                                              PIC X(22).
                       09  SPA-GMN-SYARECEKBN-G.
                           11  SPA-GMN-SYARECEKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-SYARECEKBNMEI
                                              PIC X(22).
                       09  SPA-GMN-KOUIKIRECEKBN-G.
                           11  SPA-GMN-KOUIKIRECEKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-KOUIKIRECEKBNMEI
                                              PIC X(22).
                       09  SPA-GMN-ROURECEKBN-G.
                           11  SPA-GMN-ROURECEKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-ROURECEKBNMEI
                                              PIC X(22).
                       09  SPA-GMN-RECEDENOUT PIC X(30).              
                       09  SPA-GMN-SYAOCRKBN-G.
                           11  SPA-GMN-SYAOCRKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-SYAOCRKBNMEI
                                              PIC X(22).
                       09  SPA-GMN-KKHOCRKBN-G.
                           11  SPA-GMN-KKHOCRKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-KKHOCRKBNMEI
                                              PIC X(22).
                       09  SPA-GMN-KOUIKIOCRKBN-G.
                           11  SPA-GMN-KOUIKIOCRKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-KOUIKIOCRKBNMEI
                                              PIC X(22).
                       09  SPA-GMN-NYUTAIIN-KSIKBN-G.
                           11  SPA-GMN-NYUTAIIN-KSIKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-NYUTAIIN-KSIKBNMEI
                                              PIC X(40).
                       09  SPA-GMN-NYUTAIIN-NYU-KSIKBN-G.
                           11  SPA-GMN-NYUTAIIN-NYU-KSIKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-NYUTAIIN-NYU-KSIKBNMEI
                                              PIC X(40).
                       09  SPA-GMN-PREFPRGID  PIC X(15).              
                       09  SPA-GMN-PREFPRGID1 PIC X(15).              
                       09  SPA-GMN-SYAPRTNM   PIC X(16).              
                       09  SPA-GMN-KKHPRTNM   PIC X(16).              
                       09  SPA-GMN-SYAPRTNM-NEXT
                                              PIC X(16).              
                       09  SPA-GMN-KKHPRTNM-NEXT
                                              PIC X(16).              
                       09  SPA-GMN-SYAPRTNM-N PIC X(16).              
                       09  SPA-GMN-KKHPRTNM-N PIC X(16).              
                       09  SPA-GMN-SYAPRTNM-NEXT-N
                                              PIC X(16).              
                       09  SPA-GMN-KKHPRTNM-NEXT-N
                                              PIC X(16).              
                       09  SPA-GMN-KOUIKIPRTNM
                                              PIC X(16).              
                       09  SPA-GMN-KOUIKIPRTNM-NEXT
                                              PIC X(16).              
                       09  SPA-GMN-KOUIKIPRTNM-N
                                              PIC X(16).              
                       09  SPA-GMN-KOUIKIPRTNM-NEXT-N
                                              PIC X(16).              
                       09  SPA-GMN-RENNUMHENKBN-G.
                           11  SPA-GMN-RENNUMHENKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-RENNUMHENKBNMEI
                                              PIC X(20).
                       09  SPA-GMN-NEXTPRTKBN-G.
                           11  SPA-GMN-NEXTPRTKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-NEXTPRTKBNMEI
                                              PIC X(14).
                       09  SPA-GMN-NEXTPRTKBN1-G.
                           11  SPA-GMN-NEXTPRTKBN1
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-NEXTPRTKBN1MEI
                                              PIC X(14).
                       09  SPA-GMN-RJNGNBKKBN-G.
                           11  SPA-GMN-RJNGNBKKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-RJNGNBKKBNMEI
                                              PIC X(22).
                       09  SPA-GMN-JIHITEKKSIKBN-G.
                           11  SPA-GMN-JIHITEKKSIKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-JIHITEKKSIKBNMEI
                                              PIC X(08).
                       09  SPA-GMN-KOUIKIKBN-G.
                           11  SPA-GMN-KOUIKIKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-KOUIKIKBNMEI
                                              PIC X(32).
                       09  SPA-GMN-COMMENT-NYUINRYO-G.
                           11  SPA-GMN-COMMENT-NYUINRYO
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-COMMENT-NYUINRYOMEI
                                              PIC X(32).
                       09  SPA-GMN-COMMENT-NYUINRYO1-G.
                           11  SPA-GMN-COMMENT-NYUINRYO1
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-COMMENT-NYUINRYO1MEI
                                              PIC X(32).
                       09  SPA-GMN-CHKKBN-G.
                           11  SPA-GMN-CHKKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-CHKKBNMEI
                                              PIC X(34).
                       09  SPA-GMN-BYOMEI-PRTJYUN-G.
                           11  SPA-GMN-BYOMEI-PRTJYUN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-BYOMEI-PRTJYUNMEI
                                              PIC X(34).
                       09  SPA-GMN-SHOKUJI-PRTKBN-G.
                           11  SPA-GMN-SHOKUJI-PRTKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-SHOKUJI-PRTKBNMEI
                                              PIC X(34).
                       09  SPA-GMN-INSPECT-CHARA-KBN-G.
                           11  SPA-GMN-INSPECT-CHARA-KBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-INSPECT-CHARA-KBNMEI
                                              PIC X(34).
                       09  SPA-GMN-INSPECT-ZERO-KBN-G.
                           11  SPA-GMN-INSPECT-ZERO-KBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-INSPECT-ZERO-KBNMEI
                                              PIC X(22).
                       09  SPA-GMN-INSPECT-INCLUDE-KBN-G.
                           11  SPA-GMN-INSPECT-INCLUDE-KBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-INSPECT-INCLUDE-KBNMEI
                                              PIC X(30).
                       09  SPA-GMN-INSPECT-GENERAL-KBN-G.
                           11  SPA-GMN-INSPECT-GENERAL-KBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-INSPECT-GENERAL-KBNMEI
                                              PIC X(28).
                       09  SPA-GMN-SEIHO-DSPKBN-G.
                           11  SPA-GMN-SEIHO-DSPKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-SEIHO-DSPKBNMEI
                                              PIC X(32).
                       09  SPA-GMN-RECEIPT-MAKEKBN-G.
                           11  SPA-GMN-RECEIPT-MAKEKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-RECEIPT-MAKEKBNMEI
                                              PIC X(18).
                       09  SPA-GMN-ZERO-KOH-PRTKBN-G.
                           11  SPA-GMN-ZERO-KOH-PRTKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-ZERO-KOH-PRTKBNMEI
                                              PIC X(18).
                       09  SPA-GMN-RECECOMKBN-G.
                           11  SPA-GMN-RECECOMKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-RECECOMKBNMEI
                                              PIC X(24).
                       09  SPA-GMN-RECEDEN-CDRKBN-G.
                           11  SPA-GMN-RECEDEN-CDRKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-RECEDEN-CDRKBNMEI
                                              PIC X(18).
                       09  SPA-GMN-RECEDEN-LST1KBN-G.
                           11  SPA-GMN-RECEDEN-LST1KBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-RECEDEN-LST1KBNMEI
                                              PIC X(18).
                       09  SPA-GMN-RECEDEN-LST2KBN-G.
                           11  SPA-GMN-RECEDEN-LST2KBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-RECEDEN-LST2KBNMEI
                                              PIC X(18).
                       09  SPA-GMN-RECEDEN-LST2-MEDIA-G.
                           11  SPA-GMN-RECEDEN-LST2-MEDIA
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-RECEDEN-LST2-MEDIAMEI
                                              PIC X(18).
                       09  SPA-GMN-RECEDEN-RIREKIKBN-G.
                           11  SPA-GMN-RECEDEN-RIREKIKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-RECEDEN-RIREKIKBNMEI
                                              PIC X(18).
                       09  SPA-GMN-RECEDEN-DATAKBN-G.
                           11  SPA-GMN-RECEDEN-DATAKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-RECEDEN-DATAKBNMEI
                                              PIC X(38).
                       09  SPA-GMN-RECEDEN-DISASTER-G.
                           11  SPA-GMN-RECEDEN-DISASTER
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-RECEDEN-DISASTERMEI
                                              PIC X(22).
                       09  SPA-GMN-RECEDEN-CHK-G.
                           11  SPA-GMN-RECEDEN-CHK
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-RECEDEN-CHKMEI
                                              PIC X(22).
                       09  SPA-GMN-KIGO-DELI-FLG-G.
                           11  SPA-GMN-KIGO-DELI-FLG
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-KIGO-DELI-FLGMEI
                                              PIC X(28).
                       09  SPA-GMN-KIGO-DELI-HKNJA-FLG-G.
                           11  SPA-GMN-KIGO-DELI-HKNJA-FLG
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-KIGO-DELI-HKNJA-FLGMEI
                                              PIC X(28).
                       09  SPA-GMN-NUM-DELIMITER-FLG-G.
                           11  SPA-GMN-NUM-DELIMITER-FLG
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-NUM-DELIMITER-FLGMEI
                                              PIC X(28).
                       09  SPA-GMN-NUM-DELI-HKNJA-FLG-G.
                           11  SPA-GMN-NUM-DELI-HKNJA-FLG
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-NUM-DELI-HKNJA-FLGMEI
                                              PIC X(28).
                       09  SPA-GMN-SRYKAHENKBN-G.
                           11  SPA-GMN-SRYKAHENKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-SRYKAHENKBNMEI
                                              PIC X(18).
                       09  SPA-GMN-BTUNUM-KSIKBN-G.
                           11  SPA-GMN-BTUNUM-KSIKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-BTUNUM-KSIKBNMEI
                                              PIC X(24).
                       09  SPA-GMN-DRNAME-KSIKBN-G.
                           11  SPA-GMN-DRNAME-KSIKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-DRNAME-KSIKBNMEI
                                              PIC X(18).
                       09  SPA-GMN-PRTJYUN1-G.
                           11  SPA-GMN-PRTJYUN1
                                              PIC X(02).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-PRTJYUN1MEI
                                              PIC X(28).
                       09  SPA-GMN-PRTJYUN2-G.
                           11  SPA-GMN-PRTJYUN2
                                              PIC X(02).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-PRTJYUN2MEI
                                              PIC X(28).
                       09  SPA-GMN-PRTJYUN3-G.
                           11  SPA-GMN-PRTJYUN3
                                              PIC X(02).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-PRTJYUN3MEI
                                              PIC X(28).
                       09  SPA-GMN-PRTJYUN4-G.
                           11  SPA-GMN-PRTJYUN4
                                              PIC X(02).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-PRTJYUN4MEI
                                              PIC X(28).
                       09  SPA-GMN-PRTJYUN5-G.
                           11  SPA-GMN-PRTJYUN5
                                              PIC X(02).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-PRTJYUN5MEI
                                              PIC X(28).
                       09  SPA-GMN-PRTJYUN6-G.
                           11  SPA-GMN-PRTJYUN6
                                              PIC X(02).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-PRTJYUN6MEI
                                              PIC X(28).
                       09  SPA-GMN-AUTODSPKBN-G.
                           11  SPA-GMN-AUTODSPKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-AUTODSPKBNMEI
                                              PIC X(18).
                       09  SPA-GMN-PRT-CHECKKBN-G.
                           11  SPA-GMN-PRT-CHECKKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-PRT-CHECKKBNMEI
                                              PIC X(20).
                       09  SPA-GMN-JIHI-PRTKBN-G.
                           11  SPA-GMN-JIHI-PRTKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-JIHI-PRTKBNMEI
                                              PIC X(24).
                       09  SPA-GMN-SELNUM     PIC 9(02).
                       09  SPA-GMN-HKNJANUM1  PIC X(08).
                       09  SPA-GMN-HKNJANUM1-63
                                              PIC X(08).
                       09  SPA-GMN-HKNJANAME1 PIC X(50).
      *
                       09  SPA-GMN-HKN-RIREKI-G.
                           11  SPA-GMN-HKN-RIREKI
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-HKN-RIREKIMEI
                                              PIC X(20).
                       09  SPA-GMN-GENBUTU-ICHIBU-G.
                           11  SPA-GMN-GENBUTU-ICHIBU
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-GENBUTU-ICHIBUMEI
                                              PIC X(36).
                       09  SPA-GMN-COMPRESS-G.
                           11  SPA-GMN-COMPRESS
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-COMPRESSMEI
                                              PIC X(28).
                       09  SPA-GMN-DISASTER-RECEIPT-G.
                           11  SPA-GMN-DISASTER-RECEIPT
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-DISASTER-RECEIPTMEI
                                              PIC X(18).
                       09  SPA-GMN-15-YKZ-G.
                           11  SPA-GMN-15-YKZ
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-15-YKZMEI
                                              PIC X(18).
                       09  SPA-GMN-KOUHI-ICHIBU-KSI-G.
                           11  SPA-GMN-KOUHI-ICHIBU-KSI
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-KOUHI-ICHIBU-KSIMEI
                                              PIC X(30).
      *
                       09  SPA-GMN-FRAME-ROUPRTKBN-G.
                           11  SPA-GMN-FRAME-ROUPRTKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-FRAME-ROUPRTKBNMEI
                                              PIC X(20).
                       09  SPA-GMN-FRAME-ROUPRTKBN-N-G.
                           11  SPA-GMN-FRAME-ROUPRTKBN-N
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-FRAME-ROUPRTKBN-NMEI
                                              PIC X(20).
                       09  SPA-GMN-COMMENT-ROU-G.
                           11  SPA-GMN-COMMENT-ROU
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-COMMENT-ROUMEI
                                              PIC X(20).
                       09  SPA-GMN-RSI-RECEIPTKBN-G.
                           11  SPA-GMN-RSI-RECEIPTKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-RSI-RECEIPTKBNMEI
                                              PIC X(22).
                      09  SPA-GMN-ROUPRTKBN-G.
                           11  SPA-GMN-ROUPRTKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-ROUPRTKBNMEI
                                              PIC X(22).
                       09  SPA-GMN-ROUPRTKBN-N-G.
                           11  SPA-GMN-ROUPRTKBN-N
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-ROUPRTKBN-NMEI
                                              PIC X(22).
                       09  SPA-GMN-FRAME-JIBAIPRTKBN-G.
                           11  SPA-GMN-FRAME-JIBAIPRTKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-FRAME-JIBAIPRTKBNMEI
                                              PIC X(20).
                       09  SPA-GMN-FRAME-JIBAIPRTKBN-N-G.
                           11  SPA-GMN-FRAME-JIBAIPRTKBN-N
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-FRAME-JIBAIPRTKBN-NMEI
                                              PIC X(20).
                       09  SPA-GMN-JIBAIPRTKBN-G.
                           11  SPA-GMN-JIBAIPRTKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-JIBAIPRTKBNMEI
                                              PIC X(22).
                       09  SPA-GMN-JIBAIPRTKBN-N-G.
                           11  SPA-GMN-JIBAIPRTKBN-N
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-JIBAIPRTKBN-NMEI
                                              PIC X(22).
                       09  SPA-GMN-JIBAIPRTKBN1-G.
                           11  SPA-GMN-JIBAIPRTKBN1
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-JIBAIPRTKBN1MEI
                                              PIC X(22).
                       09  SPA-GMN-JIBAIPRTKBN1-N-G.
                           11  SPA-GMN-JIBAIPRTKBN1-N
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-JIBAIPRTKBN1-NMEI
                                              PIC X(22).
                       09  SPA-GMN-JIBAI-FRACTION-KBN1-G.
                           11  SPA-GMN-JIBAI-FRACTION-KBN1
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-JIBAI-FRACTION-KBN1MEI
                                              PIC X(36).
                       09  SPA-GMN-JIBAI-FRACTION-KBN2-G.
                           11  SPA-GMN-JIBAI-FRACTION-KBN2
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-JIBAI-FRACTION-KBN2MEI
                                              PIC X(36).
                       09  SPA-GMN-JIBAI-PDF-G.
                           11  SPA-GMN-JIBAI-PDF
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-JIBAI-PDFMEI
                                              PIC X(20).
                       09  SPA-GMN-ROUPRTNM   PIC X(16).              
                       09  SPA-GMN-JBAIPRTNM  PIC X(16).              
                       09  SPA-GMN-ROUPRTNM-NEXT
                                              PIC X(16).              
                       09  SPA-GMN-JBAIPRTNM-NEXT
                                              PIC X(16).              
                       09  SPA-GMN-ROUPRTNM-N PIC X(16).              
                       09  SPA-GMN-JBAIPRTNM-N
                                              PIC X(16).              
                       09  SPA-GMN-ROUPRTNM-NEXT-N
                                              PIC X(16).              
                       09  SPA-GMN-JBAIPRTNM-NEXT-N
                                              PIC X(16).              
      *                                        
                       09  SPA-GMN-MOJI-KISAIKBN-G.
                           11  SPA-GMN-MOJI-KISAIKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-MOJI-KISAIKBNMEI
                                              PIC X(18).
                       09  SPA-GMN-KYURATE-KISAIKBN-G.
                           11  SPA-GMN-KYURATE-KISAIKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-KYURATE-KISAIKBNMEI
                                              PIC X(24).
                       09  SPA-GMN-RECESYUBET-KISAIKBN-G.
                           11  SPA-GMN-RECESYUBET-KISAIKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-RECESYUBET-KISAIKBNMEI
                                              PIC X(24).
      *                                        
                       09  SPA-GMN-KKHPRTKBN-G.
                           11  SPA-GMN-KKHPRTKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-KKHPRTKBNMEI
                                              PIC X(18).
                       09  SPA-GMN-SYAPRTKBN-G.
                           11  SPA-GMN-SYAPRTKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-SYAPRTKBNMEI
                                              PIC X(22).
                       09  SPA-GMN-DISASTER-SYAPRTKBN-G.
                           11  SPA-GMN-DISASTER-SYAPRTKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-DISASTER-SYAPRTKBNMEI
                                              PIC X(30).
                       09  SPA-GMN-KOUIKIPRTKBN-G.
                           11  SPA-GMN-KOUIKIPRTKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-KOUIKIPRTKBNMEI
                                              PIC X(34).
                       09  SPA-GMN-RSI-INVOICEKBN-G.
                           11  SPA-GMN-RSI-INVOICEKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-RSI-INVOICEKBNMEI
                                              PIC X(22).
                       09  SPA-GMN-ROUSAIPRTKBN-G.
                           11  SPA-GMN-ROUSAIPRTKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-ROUSAIPRTKBNMEI
                                              PIC X(18).
                       09  SPA-GMN-FRAME-ROUSAIPRTKBN-G.
                           11  SPA-GMN-FRAME-ROUSAIPRTKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-FRAME-ROUSAIPRTKBNMEI
                                              PIC X(18).
      *
                       09  SPA-GMN-PREFKBN-PRTJYUN-G.
                           11  SPA-GMN-PREFKBN-PRTJYUN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-PREFKBN-PRTJYUNMEI
                                              PIC X(10).
                       09  SPA-GMN-HKNJANUM-PRTJYUN-G.
                           11  SPA-GMN-HKNJANUM-PRTJYUN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-HKNJANUM-PRTJYUNMEI
                                              PIC X(44).
                       09  SPA-GMN-RECESYUBETU-PRTJYUN-G.
                           11  SPA-GMN-RECESYUBETU-PRTJYUN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-RECESYUBETU-PRTJYUNMEI
                                              PIC X(28).
                       09  SPA-GMN-ZAITAKU-G.
                           11  SPA-GMN-ZAITAKU
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-ZAITAKUMEI
                                              PIC X(24).
                       09  SPA-GMN-CHOUKI-G.
                           11  SPA-GMN-CHOUKI
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-CHOUKIMEI
                                              PIC X(24).
                       09  SPA-GMN-HKNJANUM-PRTKBN-G.
                           11  SPA-GMN-HKNJANUM-PRTKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-HKNJANUM-PRTKBNMEI
                                              PIC X(22).
                       09  SPA-GMN-HKNJANUM   PIC X(08).
                       09  SPA-GMN-HKNJANAME  PIC X(50).
                       09  SPA-GMN-DRCD-PRTKBN-G.
                           11  SPA-GMN-DRCD-PRTKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-DRCD-PRTKBNMEI
                                              PIC X(24).
                       09  SPA-GMN-DRCD-PRTJYUNKBN-G.
                           11  SPA-GMN-DRCD-PRTJYUNKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-DRCD-PRTJYUNKBNMEI
                                              PIC X(24).
                       09  SPA-GMN-BTUNUM-PRTKBN-G.
                           11  SPA-GMN-BTUNUM-PRTKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-BTUNUM-PRTKBNMEI
                                              PIC X(24).
                       09  SPA-GMN-BTUNUM-PRTJYUNKBN-G.
                           11  SPA-GMN-BTUNUM-PRTJYUNKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-BTUNUM-PRTJYUNKBNMEI
                                              PIC X(24).
                       09  SPA-GMN-TERMINAL-DIG-PRTKBN-G.
                           11  SPA-GMN-TERMINAL-DIG-PRTKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-TERMINAL-DIG-PRTKBNMEI
                                              PIC X(36).
                       09  SPA-GMN-STYUKYMD   PIC X(10).
                       09  SPA-GMN-EDYUKYMD   PIC X(10).
                       09  SPA-GMN-SYUKA-KBN-G.
                           11  SPA-GMN-SYUKA-KBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-SYUKA-KBNMEI
                                              PIC X(20).
                       09  SPA-GMN-SYUKA-NYUIN-G.
                           11  SPA-GMN-SYUKA-NYUIN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-SYUKA-NYUINMEI
                                              PIC X(20).
                       09  SPA-GMN-SYUKA-GAIRAI-G.
                           11  SPA-GMN-SYUKA-GAIRAI
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-SYUKA-GAIRAIMEI
                                              PIC X(20).
                       09  SPA-GMN-SYUKA-MODE-G.
                           11  SPA-GMN-SYUKA-MODE
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-SYUKA-MODEMEI
                                              PIC X(20).
                       09  SPA-GMN-SYUKA-SELECT-G.
                           11  SPA-GMN-SYUKA-SELECT
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-SYUKA-SELECTMEI
                                              PIC X(28).
                       09  SPA-GMN-SYUKA-SYAHO-G.
                           11  SPA-GMN-SYUKA-SYAHO
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-SYUKA-SYAHOMEI
                                              PIC X(20).
                       09  SPA-GMN-SYUKA-KOKUHO-G.
                           11  SPA-GMN-SYUKA-KOKUHO
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-SYUKA-KOKUHOMEI
                                              PIC X(20).
                       09  SPA-GMN-SYUKA-KOUIKI-G.
                           11  SPA-GMN-SYUKA-KOUIKI
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-SYUKA-KOUIKIMEI
                                              PIC X(20).
                       09  SPA-GMN-NI-JYOKEN1 PIC 9(08).
                       09  SPA-GMN-NI-JYOKEN2 PIC 9(08).
                       09  SPA-GMN-NI-JYOKEN3 PIC 9(08).
                       09  SPA-GMN-NI-JYOKEN4 PIC 9(08).
                       09  SPA-GMN-NI-JYOKEN5 PIC 9(08).
                       09  SPA-GMN-NI-JYOKEN6 PIC 9(08).
      *
                       09  SPA-GMN-HKNJALIST.
                           11  SPA-GMN-HKNJALIST-TBL OCCURS  10.
                               13  SPA-GMN-HKNJALIST-TNO
                                               PIC 9(02).
                               13  SPA-GMN-HKNJALIST-THKNJANUM
                                               PIC X(08).
                               13  SPA-GMN-HKNJALIST-THKNJANAME
                                               PIC X(50).
                               13  SPA-GMN-HKNJALIST-THKNJANUM-63
                                               PIC X(08).
                           11  SPA-GMN-HKNJALIST-MAX
                                               PIC 9(02).
      *                                        
                   07  SPA-GMN-BYOMEIHENKBNLST        OCCURS  2.
                       09  SPA-GMN-BYOMEIHENKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-BYOMEIHENKBNMEIL
                                              PIC X(18).
                   07  SPA-GMN-BYOMEIHENKBN-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-BYOMEIHENKBN1LST       OCCURS  2.
                       09  SPA-GMN-BYOMEIHENKBN1L
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-BYOMEIHENKBN1MEIL
                                              PIC X(20).
                   07  SPA-GMN-BYOMEIHENKBN1-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-SYUBYOHENFLGLST       OCCURS  4.
                       09  SPA-GMN-SYUBYOHENFLGL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-SYUBYOHENFLGMEIL
                                              PIC X(44).
                   07  SPA-GMN-SYUBYOHENFLG-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-TENKIHENKBNLST         OCCURS  2.
                       09  SPA-GMN-TENKIHENKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-TENKIHENKBNMEIL
                                              PIC X(18).
                   07  SPA-GMN-TENKIHENKBN-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-TENKIBYOMEI-KBNLST     OCCURS  3.
                       09  SPA-GMN-TENKIBYOMEI-KBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-TENKIBYOMEI-KBNMEIL
                                              PIC X(50).
                   07  SPA-GMN-TENKIBYOMEI-KBN-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-KKHRECEKBNLST          OCCURS  4.
                       09  SPA-GMN-KKHRECEKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-KKHRECEKBNMEIL
                                              PIC X(22).
                   07  SPA-GMN-KKHRECEKBN-MAX PIC 9(02).    
      *                                        
                   07  SPA-GMN-SYARECEKBNLST          OCCURS  4.
                       09  SPA-GMN-SYARECEKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-SYARECEKBNMEIL
                                              PIC X(22).
                   07  SPA-GMN-SYARECEKBN-MAX PIC 9(02).    
      *                                        
                   07  SPA-GMN-KOUIKIRECEKBNLST          OCCURS  4.
                       09  SPA-GMN-KOUIKIRECEKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-KOUIKIRECEKBNMEIL
                                              PIC X(22).
                   07  SPA-GMN-KOUIKIRECEKBN-MAX PIC 9(02).    
      *                                        
                   07  SPA-GMN-ROURECEKBNLST          OCCURS  4.
                       09  SPA-GMN-ROURECEKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-ROURECEKBNMEIL
                                              PIC X(22).
                   07  SPA-GMN-ROURECEKBN-MAX PIC 9(02).    
      *                                        
                   07  SPA-GMN-KKHOCRKBNLST           OCCURS  4.
                       09  SPA-GMN-KKHOCRKBNL PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-KKHOCRKBNMEIL
                                              PIC X(22).
                   07  SPA-GMN-KKHOCRKBN-MAX  PIC 9(02).    
      *                                        
                   07  SPA-GMN-KOUIKIOCRKBNLST        OCCURS  4.
                       09  SPA-GMN-KOUIKIOCRKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-KOUIKIOCRKBNMEIL
                                              PIC X(22).
                   07  SPA-GMN-KOUIKIOCRKBN-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-NYUTAIIN-KSIKBNLST  OCCURS  3.
                       09  SPA-GMN-NYUTAIIN-KSIKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-NYUTAIIN-KSIKBNMEIL
                                              PIC X(40).
                   07  SPA-GMN-NYUTAIIN-KSIKBN-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-NYUTAIIN-NYU-KSIKBNLST  OCCURS  2.
                       09  SPA-GMN-NYUTAIIN-NYU-KSIKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-NYUTAIIN-NYU-KSIKBNMEIL
                                              PIC X(40).
                   07  SPA-GMN-NYUTAIIN-NYU-KSIKBN-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-SYAOCRKBNLST           OCCURS  4.
                       09  SPA-GMN-SYAOCRKBNL PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-SYAOCRKBNMEIL
                                              PIC X(22).
                   07  SPA-GMN-SYAOCRKBN-MAX  PIC 9(02).    
      *                                        
                   07  SPA-GMN-KKHPRTKBNLST           OCCURS  2.
                       09  SPA-GMN-KKHPRTKBNL PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-KKHPRTKBNMEIL
                                              PIC X(18).
                   07  SPA-GMN-KKHPRTKBN-MAX  PIC 9(02).    
      *                                        
                   07  SPA-GMN-SYAPRTKBNLST           OCCURS  2.
                       09  SPA-GMN-SYAPRTKBNL PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-SYAPRTKBNMEIL
                                              PIC X(22).
                   07  SPA-GMN-SYAPRTKBN-MAX  PIC 9(02).    
      *
                   07  SPA-GMN-DISASTER-SYAPRTKBNLST   OCCURS  2.
                       09  SPA-GMN-DISASTER-SYAPRTKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-DISASTER-SYAPRTKBNMEIL
                                              PIC X(30).
                   07  SPA-GMN-DISASTER-SYAPRTKBN-MAX
                                              PIC 9(02).
      *                                        
                   07  SPA-GMN-KOUIKIPRTKBNLST       OCCURS  5.
                       09  SPA-GMN-KOUIKIPRTKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-KOUIKIPRTKBNMEIL
                                              PIC X(32).
                   07  SPA-GMN-KOUIKIPRTKBN-MAX
                                              PIC 9(02).    
      *
                   07  SPA-GMN-RSI-INVOICEKBNLST         OCCURS  2.
                       09  SPA-GMN-RSI-INVOICEKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-RSI-INVOICEKBNMEIL
                                              PIC X(22).
                   07  SPA-GMN-RSI-INVOICEKBN-MAX
                                              PIC 9(02).
      *
                   07  SPA-GMN-ROUSAIPRTKBNLST         OCCURS  2.
                       09  SPA-GMN-ROUSAIPRTKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-ROUSAIPRTKBNMEIL
                                              PIC X(18).
                   07  SPA-GMN-ROUSAIPRTKBN-MAX
                                              PIC 9(02).
      *
                   07  SPA-GMN-FRAME-ROUSAIPRTKBNLST   OCCURS  2.
                       09  SPA-GMN-FRAME-ROUSAIPRTKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-FRAME-ROUSAIPRTKBNMEIL
                                              PIC X(18).
                   07  SPA-GMN-FRAME-ROUSAIPRTKBN-MAX
                                              PIC 9(02).
      *
                   07  SPA-GMN-RENNUMHENKBNLST        OCCURS  3.
                       09  SPA-GMN-RENNUMHENKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-RENNUMHENKBNMEIL
                                              PIC X(20).
                   07  SPA-GMN-RENNUMHENKBN-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-NEXTPRTKBNLST        OCCURS  3.
                       09  SPA-GMN-NEXTPRTKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-NEXTPRTKBNMEIL
                                              PIC X(14).
                   07  SPA-GMN-NEXTPRTKBN-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-NEXTPRTKBN1LST       OCCURS  4.
                       09  SPA-GMN-NEXTPRTKBN1L
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-NEXTPRTKBN1MEIL
                                              PIC X(14).
                   07  SPA-GMN-NEXTPRTKBN1-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-RJNGNBKKBNLST       OCCURS  4.
                       09  SPA-GMN-RJNGNBKKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-RJNGNBKKBNMEIL
                                              PIC X(22).
                   07  SPA-GMN-RJNGNBKKBN-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-JIHITEKKSIKBNLST
                                              OCCURS  2.
                       09  SPA-GMN-JIHITEKKSIKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-JIHITEKKSIKBNMEIL
                                              PIC X(08).
                   07  SPA-GMN-JIHITEKKSIKBN-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-KOUIKIKBNLST       OCCURS  4.
                       09  SPA-GMN-KOUIKIKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-KOUIKIKBNMEIL
                                              PIC X(32).
                   07  SPA-GMN-KOUIKIKBN-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-COMMENT-NYUINRYOLST       OCCURS  5.
                       09  SPA-GMN-COMMENT-NYUINRYOL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-COMMENT-NYUINRYOMEIL
                                              PIC X(32).
                   07  SPA-GMN-COMMENT-NYUINRYO-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-COMMENT-NYUINRYO1LST       OCCURS  5.
                       09  SPA-GMN-COMMENT-NYUINRYO1L
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-COMMENT-NYUINRYO1MEIL
                                              PIC X(32).
                   07  SPA-GMN-COMMENT-NYUINRYO1-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-CHKKBNLST      OCCURS  5.
                       09  SPA-GMN-CHKKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-CHKKBNMEIL PIC X(34).
                   07  SPA-GMN-CHKKBN-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-BYOMEI-PRTJYUNLST       OCCURS  5.
                       09  SPA-GMN-BYOMEI-PRTJYUNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-BYOMEI-PRTJYUNMEIL
                                              PIC X(34).
                   07  SPA-GMN-BYOMEI-PRTJYUN-MAX
                                              PIC 9(02).    
      *
                   07  SPA-GMN-SHOKUJI-PRTKBNLST       OCCURS  5.
                       09  SPA-GMN-SHOKUJI-PRTKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-SHOKUJI-PRTKBNMEIL
                                              PIC X(34).
                   07  SPA-GMN-SHOKUJI-PRTKBN-MAX
                                              PIC 9(02).
      *                                        
                   07  SPA-GMN-INSPECT-CHARA-KBNLST    OCCURS  5.
                       09  SPA-GMN-INSPECT-CHARA-KBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-INSPECT-CHARA-KBNMEIL
                                              PIC X(34).
                   07  SPA-GMN-INSPECT-CHARA-KBN-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-INSPECT-ZERO-KBNLST     OCCURS  5.
                       09  SPA-GMN-INSPECT-ZERO-KBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-INSPECT-ZERO-KBNMEIL
                                              PIC X(22).
                   07  SPA-GMN-INSPECT-ZERO-KBN-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-INSPECT-INCLUDE-KBNLST  OCCURS  5.
                       09  SPA-GMN-INSPECT-INCLUDE-KBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-INSPECT-INCLUDE-KBNMEIL
                                              PIC X(30).
                   07  SPA-GMN-INSPECT-INCLUDE-KBN-MAX
                                              PIC 9(02).    
      *
                   07  SPA-GMN-INSPECT-GENERAL-KBNLST  OCCURS  5.
                       09  SPA-GMN-INSPECT-GENERAL-KBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-INSPECT-GENERAL-KBNMEIL
                                              PIC X(28).
                   07  SPA-GMN-INSPECT-GENERAL-KBN-MAX
                                              PIC 9(02).
      *                                        
                   07  SPA-GMN-SEIHO-DSPKBNLST         OCCURS  5.
                       09  SPA-GMN-SEIHO-DSPKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-SEIHO-DSPKBNMEIL
                                              PIC X(32).
                   07  SPA-GMN-SEIHO-DSPKBN-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-RECEIPT-MAKEKBNLST       OCCURS  2.
                       09  SPA-GMN-RECEIPT-MAKEKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-RECEIPT-MAKEKBNMEIL
                                              PIC X(18).
                   07  SPA-GMN-RECEIPT-MAKEKBN-MAX
                                              PIC 9(02).    
      *
                   07  SPA-GMN-ZERO-KOH-PRTKBNLST   OCCURS  2.
                       09  SPA-GMN-ZERO-KOH-PRTKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-ZERO-KOH-PRTKBNMEIL
                                              PIC X(18).
                   07  SPA-GMN-ZERO-KOH-PRTKBN-MAX
                                              PIC 9(02).
      *                                        
                   07  SPA-GMN-RECECOMKBNLST  OCCURS  2.
                       09  SPA-GMN-RECECOMKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-RECECOMKBNMEIL
                                              PIC X(24).
                   07  SPA-GMN-RECECOMKBN-MAX PIC 9(02).    
      *                                        
                   07  SPA-GMN-RECEDEN-CDRKBNLST  OCCURS  2.
                       09  SPA-GMN-RECEDEN-CDRKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-RECEDEN-CDRKBNMEIL
                                              PIC X(18).
                   07  SPA-GMN-RECEDEN-CDRKBN-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-RECEDEN-LST1KBNLST  OCCURS  2.
                       09  SPA-GMN-RECEDEN-LST1KBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-RECEDEN-LST1KBNMEIL
                                              PIC X(18).
                   07  SPA-GMN-RECEDEN-LST1KBN-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-RECEDEN-LST2KBNLST  OCCURS  2.
                       09  SPA-GMN-RECEDEN-LST2KBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-RECEDEN-LST2KBNMEIL
                                              PIC X(18).
                   07  SPA-GMN-RECEDEN-LST2KBN-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-RECEDEN-LST2-MEDIALST  OCCURS  5.
                       09  SPA-GMN-RECEDEN-LST2-MEDIAL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-RECEDEN-LST2-MEDIAMEIL
                                              PIC X(18).
                   07  SPA-GMN-RECEDEN-LST2-MEDIA-MAX
                                              PIC 9(02).
      *                                        
                   07  SPA-GMN-RECEDEN-RIREKIKBNLST  OCCURS  2.
                       09  SPA-GMN-RECEDEN-RIREKIKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-RECEDEN-RIREKIKBNMEIL
                                              PIC X(22).
                   07  SPA-GMN-RECEDEN-RIREKIKBN-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-RECEDEN-DATAKBNLST  OCCURS  10.
                       09  SPA-GMN-RECEDEN-DATAKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-RECEDEN-DATAKBNMEIL
                                              PIC X(38).
                   07  SPA-GMN-RECEDEN-DATAKBN-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-RECEDEN-DISASTERLST  OCCURS  5.
                       09  SPA-GMN-RECEDEN-DISASTERL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-RECEDEN-DISASTERMEIL
                                              PIC X(22).
                   07  SPA-GMN-RECEDEN-DISASTER-MAX
                                              PIC 9(01).    
      *                                        
                   07  SPA-GMN-RECEDEN-CHKLST  OCCURS  5.
                       09  SPA-GMN-RECEDEN-CHKL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-RECEDEN-CHKMEIL
                                              PIC X(22).
                   07  SPA-GMN-RECEDEN-CHK-MAX
                                              PIC 9(01).    
      *                                        
                   07  SPA-GMN-KIGO-DELI-FLGLST    OCCURS  5.
                       09  SPA-GMN-KIGO-DELI-FLGL
                                               PIC X(01).
                       09  SPA-GMN-F1L         PIC X(01).
                       09  SPA-GMN-KIGO-DELI-FLGMEIL
                                               PIC X(28).
                   07  SPA-GMN-KIGO-DELI-FLG-MAX
                                               PIC 9(02).    
      *                                        
                   07  SPA-GMN-KIGO-DELI-HKNJA-FLGLST OCCURS  5.
                       09  SPA-GMN-KIGO-DELI-HKNJA-FLGL
                                               PIC X(01).
                       09  SPA-GMN-F1L         PIC X(01).
                       09  SPA-GMN-KIGO-DELI-HKNJA-FLGMEIL
                                               PIC X(28).
                   07  SPA-GMN-KIGO-DELI-HKNJA-FLG-MAX
                                               PIC 9(02).    
      *                                        
                   07  SPA-GMN-NUM-DELIMITER-FLGLST    OCCURS  5.
                       09  SPA-GMN-NUM-DELIMITER-FLGL
                                               PIC X(01).
                       09  SPA-GMN-F1L         PIC X(01).
                       09  SPA-GMN-NUM-DELIMITER-FLGMEIL
                                               PIC X(28).
                   07  SPA-GMN-NUM-DELIMITER-FLG-MAX
                                               PIC 9(02).    
      *                                        
                   07  SPA-GMN-NUM-DELI-HKNJA-FLGLST OCCURS  5.
                       09  SPA-GMN-NUM-DELI-HKNJA-FLGL
                                               PIC X(01).
                       09  SPA-GMN-F1L         PIC X(01).
                       09  SPA-GMN-NUM-DELI-HKNJA-FLGMEIL
                                               PIC X(28).
                   07  SPA-GMN-NUM-DELI-HKNJA-FLG-MAX
                                               PIC 9(02).    
      *                                        
                   07  SPA-GMN-SRYKAHENKBNLST OCCURS  2.
                       09  SPA-GMN-SRYKAHENKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-SRYKAHENKBNMEIL
                                              PIC X(18).
                   07  SPA-GMN-SRYKAHENKBN-MAX
                                              PIC 9(02).   
      *                                        
                   07  SPA-GMN-BTUNUM-KSIKBNLST OCCURS  5.
                       09  SPA-GMN-BTUNUM-KSIKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-BTUNUM-KSIKBNMEIL
                                              PIC X(24).
                   07  SPA-GMN-BTUNUM-KSIKBN-MAX
                                              PIC 9(02).   
      *                                        
                   07  SPA-GMN-DRNAME-KSIKBNLST OCCURS  2.
                       09  SPA-GMN-DRNAME-KSIKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-DRNAME-KSIKBNMEIL
                                              PIC X(18).
                   07  SPA-GMN-DRNAME-KSIKBN-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-PRTJYUN1LST    OCCURS  10.
                       09  SPA-GMN-PRTJYUN1L  PIC X(02).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-PRTJYUN1MEIL
                                              PIC X(28).
                   07  SPA-GMN-PRTJYUN1-MAX   PIC 9(02).   
      * 
                   07  SPA-GMN-PRTJYUN2LST    OCCURS  10.
                       09  SPA-GMN-PRTJYUN2L  PIC X(02).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-PRTJYUN2MEIL
                                              PIC X(28).
                   07  SPA-GMN-PRTJYUN2-MAX   PIC 9(02).   
      * 
                   07  SPA-GMN-PRTJYUN3LST    OCCURS  10.
                       09  SPA-GMN-PRTJYUN3L  PIC X(02).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-PRTJYUN3MEIL
                                              PIC X(28).
                   07  SPA-GMN-PRTJYUN3-MAX   PIC 9(02).   
      * 
                   07  SPA-GMN-PRTJYUN4LST    OCCURS  10.
                       09  SPA-GMN-PRTJYUN4L  PIC X(02).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-PRTJYUN4MEIL
                                              PIC X(28).
                   07  SPA-GMN-PRTJYUN4-MAX   PIC 9(02).   
      * 
                   07  SPA-GMN-PRTJYUN5LST    OCCURS  10.
                       09  SPA-GMN-PRTJYUN5L  PIC X(02).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-PRTJYUN5MEIL
                                              PIC X(28).
                   07  SPA-GMN-PRTJYUN5-MAX   PIC 9(02).   
      * 
                   07  SPA-GMN-PRTJYUN6LST    OCCURS  10.
                       09  SPA-GMN-PRTJYUN6L  PIC X(02).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-PRTJYUN6MEIL
                                              PIC X(28).
                   07  SPA-GMN-PRTJYUN6-MAX   PIC 9(02).   
      * 
                   07  SPA-GMN-AUTODSPKBNLST  OCCURS  2.
                       09  SPA-GMN-AUTODSPKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-AUTODSPKBNMEIL
                                              PIC X(18).
                   07  SPA-GMN-AUTODSPKBN-MAX PIC 9(02).   
      * 
                   07  SPA-GMN-PRT-CHECKKBNLST
                                              OCCURS  2.
                       09  SPA-GMN-PRT-CHECKKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-PRT-CHECKKBNMEIL
                                              PIC X(20).
                   07  SPA-GMN-PRT-CHECKKBN-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-JIHI-PRTKBNLST
                                              OCCURS  2.
                       09  SPA-GMN-JIHI-PRTKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-JIHI-PRTKBNMEIL
                                              PIC X(24).
                   07  SPA-GMN-JIHI-PRTKBN-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-MOJI-KISAIKBNLST  OCCURS  5.
                       09  SPA-GMN-MOJI-KISAIKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-MOJI-KISAIKBNMEIL
                                              PIC X(18).
                   07  SPA-GMN-MOJI-KISAIKBN-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-KYURATE-KISAIKBNLST     OCCURS  5.
                       09  SPA-GMN-KYURATE-KISAIKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-KYURATE-KISAIKBNMEIL
                                              PIC X(24).
                   07  SPA-GMN-KYURATE-KISAIKBN-MAX PIC 9(02).   
      * 
                   07  SPA-GMN-RECESYUBET-KISAIKBNLST OCCURS  5.
                       09  SPA-GMN-RECESYUBET-KISAIKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-RECESYUBET-KISAIKBNMEIL
                                              PIC X(24).
                   07  SPA-GMN-RECESYUBET-KISAIKBN-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-PREFKBN-PRTJYUNLST  OCCURS  5.
                       09  SPA-GMN-PREFKBN-PRTJYUNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-PREFKBN-PRTJYUNMEIL
                                              PIC X(10).
                   07  SPA-GMN-PREFKBN-PRTJYUN-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-HKNJANUM-PRTJYUNLST     OCCURS  6.
                       09  SPA-GMN-HKNJANUM-PRTJYUNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-HKNJANUM-PRTJYUNMEIL
                                              PIC X(44).
                   07  SPA-GMN-HKNJANUM-PRTJYUN-MAX PIC 9(02).   
      * 
                   07  SPA-GMN-RECESYUBETU-PRTJYUNLST OCCURS  6.
                       09  SPA-GMN-RECESYUBETU-PRTJYUNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-RECESYUBETU-PRTJYUNMEIL
                                              PIC X(28).
                   07  SPA-GMN-RECESYUBETU-PRTJYUN-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-ZAITAKULST  OCCURS  5.
                       09  SPA-GMN-ZAITAKUL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-ZAITAKUMEIL
                                              PIC X(24).
                   07  SPA-GMN-ZAITAKU-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-CHOUKILST     OCCURS  5.
                       09  SPA-GMN-CHOUKIL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-CHOUKIMEIL
                                              PIC X(24).
                   07  SPA-GMN-CHOUKI-MAX PIC 9(02).   
      * 
                   07  SPA-GMN-HKNJANUM-PRTKBNLST OCCURS  5.
                       09  SPA-GMN-HKNJANUM-PRTKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-HKNJANUM-PRTKBNMEIL
                                              PIC X(22).
                   07  SPA-GMN-HKNJANUM-PRTKBN-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-DRCD-PRTKBNLST     OCCURS  2.
                       09  SPA-GMN-DRCD-PRTKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-DRCD-PRTKBNMEIL
                                              PIC X(24).
                   07  SPA-GMN-DRCD-PRTKBN-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-DRCD-PRTJYUNKBNLST
                                                  OCCURS  10.
                       09  SPA-GMN-DRCD-PRTJYUNKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-DRCD-PRTJYUNKBNMEIL
                                              PIC X(24).
                   07  SPA-GMN-DRCD-PRTJYUNKBN-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-BTUNUM-PRTKBNLST     OCCURS  2.
                       09  SPA-GMN-BTUNUM-PRTKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-BTUNUM-PRTKBNMEIL
                                              PIC X(24).
                   07  SPA-GMN-BTUNUM-PRTKBN-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-BTUNUM-PRTJYUNKBNLST
                                                  OCCURS  10.
                       09  SPA-GMN-BTUNUM-PRTJYUNKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-BTUNUM-PRTJYUNKBNMEIL
                                              PIC X(24).
                   07  SPA-GMN-BTUNUM-PRTJYUNKBN-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-TERMINAL-DIG-PRTKBNLST
                                                  OCCURS  2.
                       09  SPA-GMN-TERMINAL-DIG-PRTKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-TERMINAL-DIG-PRTKBNMEIL
                                              PIC X(36).
                   07  SPA-GMN-TERMINAL-DIG-PRTKBN-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-SYUKA-KBNLST   OCCURS  2.
                       09  SPA-GMN-SYUKA-KBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-SYUKA-KBNMEIL
                                              PIC X(20).
                   07  SPA-GMN-SYUKA-KBN-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-SYUKA-NYUINLST  OCCURS  5.
                       09  SPA-GMN-SYUKA-NYUINL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-SYUKA-NYUINMEIL
                                              PIC X(20).
                   07  SPA-GMN-SYUKA-NYUIN-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-SYUKA-GAIRAILST    OCCURS  5.
                       09  SPA-GMN-SYUKA-GAIRAIL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-SYUKA-GAIRAIMEIL
                                              PIC X(20).
                   07  SPA-GMN-SYUKA-GAIRAI-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-SYUKA-SELECTLST    OCCURS  5.
                       09  SPA-GMN-SYUKA-SELECTL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-SYUKA-SELECTMEIL
                                              PIC X(28).
                   07  SPA-GMN-SYUKA-SELECT-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-SYUKA-MODELST  OCCURS  2.
                       09  SPA-GMN-SYUKA-MODEL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-SYUKA-MODEMEIL
                                              PIC X(20).
                   07  SPA-GMN-SYUKA-MODE-MAX PIC 9(02).   
      * 
                   07  SPA-GMN-SYUKA-SYAHOLST  OCCURS  3.
                       09  SPA-GMN-SYUKA-SYAHOL
                                               PIC X(01).
                       09  SPA-GMN-F1L         PIC X(01).
                       09  SPA-GMN-SYUKA-SYAHOMEIL
                                               PIC X(20).
                   07  SPA-GMN-SYUKA-SYAHO-MAX PIC 9(02).   
      * 
                   07  SPA-GMN-SYUKA-KOKUHOLST OCCURS  3.
                       09  SPA-GMN-SYUKA-KOKUHOL
                                               PIC X(01).
                       09  SPA-GMN-F1L         PIC X(01).
                       09  SPA-GMN-SYUKA-KOKUHOMEIL
                                               PIC X(20).
                   07  SPA-GMN-SYUKA-KOKUHO-MAX
                                               PIC 9(02).   
      * 
                   07  SPA-GMN-SYUKA-KOUIKILST OCCURS  3.
                       09  SPA-GMN-SYUKA-KOUIKIL
                                               PIC X(01).
                       09  SPA-GMN-F1L         PIC X(01).
                       09  SPA-GMN-SYUKA-KOUIKIMEIL
                                               PIC X(20).
                   07  SPA-GMN-SYUKA-KOUIKI-MAX
                                               PIC 9(02).   
      * 
                   07  SPA-GMN-FRAME-ROUPRTKBNLST      OCCURS  2.
                       09  SPA-GMN-FRAME-ROUPRTKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-FRAME-ROUPRTKBNMEIL
                                              PIC X(20).
                   07  SPA-GMN-FRAME-ROUPRTKBN-MAX PIC 9(02).   
      * 
                   07  SPA-GMN-FRAME-ROUPRTKBN-NLST    OCCURS  2.
                       09  SPA-GMN-FRAME-ROUPRTKBN-NL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-FRAME-ROUPRTKBN-NMEIL
                                              PIC X(20).
                   07  SPA-GMN-FRAME-ROUPRTKBN-N-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-COMMENT-ROULST          OCCURS  2.
                       09  SPA-GMN-COMMENT-ROUL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-COMMENT-ROUMEIL
                                              PIC X(20).
                   07  SPA-GMN-COMMENT-ROU-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-RSI-RECEIPTKBNLST  OCCURS  2.
                       09  SPA-GMN-RSI-RECEIPTKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-RSI-RECEIPTKBNMEIL
                                              PIC X(22).
                   07  SPA-GMN-RSI-RECEIPTKBN-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-ROUPRTKBNLST  OCCURS  2.
                       09  SPA-GMN-ROUPRTKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-ROUPRTKBNMEIL
                                              PIC X(22).
                   07  SPA-GMN-ROUPRTKBN-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-ROUPRTKBN-NLST  OCCURS  2.
                       09  SPA-GMN-ROUPRTKBN-NL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-ROUPRTKBN-NMEIL
                                              PIC X(22).
                   07  SPA-GMN-ROUPRTKBN-N-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-FRAME-JIBAIPRTKBNLST    OCCURS  2.
                       09  SPA-GMN-FRAME-JIBAIPRTKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-FRAME-JIBAIPRTKBNMEIL
                                              PIC X(20).
                   07  SPA-GMN-FRAME-JIBAIPRTKBN-MAX PIC 9(02).   
      * 
                   07  SPA-GMN-FRAME-JIBAIPRTKBN-NLST  OCCURS  2.
                       09  SPA-GMN-FRAME-JIBAIPRTKBN-NL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-FRAME-JIBAIPRTKBN-NMEIL
                                              PIC X(20).
                   07  SPA-GMN-FRAME-JIBAIPRTKBN-N-MAX PIC 9(02).   
      * 
                   07  SPA-GMN-JIBAIPRTKBNLST  OCCURS  5.
                       09  SPA-GMN-JIBAIPRTKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-JIBAIPRTKBNMEIL
                                              PIC X(22).
                   07  SPA-GMN-JIBAIPRTKBN-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-JIBAIPRTKBN-NLST  OCCURS  5.
                       09  SPA-GMN-JIBAIPRTKBN-NL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-JIBAIPRTKBN-NMEIL
                                              PIC X(22).
                   07  SPA-GMN-JIBAIPRTKBN-N-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-JIBAIPRTKBN1LST    OCCURS  5.
                       09  SPA-GMN-JIBAIPRTKBN1L
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-JIBAIPRTKBN1MEIL
                                              PIC X(22).
                   07  SPA-GMN-JIBAIPRTKBN1-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-JIBAIPRTKBN1-NLST   OCCURS  5.
                       09  SPA-GMN-JIBAIPRTKBN1-NL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-JIBAIPRTKBN1-NMEIL
                                              PIC X(22).
                   07  SPA-GMN-JIBAIPRTKBN1-N-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-JIBAI-FRACTION-KBN1LST   OCCURS  5.
                       09  SPA-GMN-JIBAI-FRACTION-KBN1L
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-JIBAI-FRACTION-KBN1MEIL
                                              PIC X(36).
                   07  SPA-GMN-JIBAI-FRACTION-KBN1-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-JIBAI-FRACTION-KBN2LST   OCCURS  5.
                       09  SPA-GMN-JIBAI-FRACTION-KBN2L
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-JIBAI-FRACTION-KBN2MEIL
                                              PIC X(36).
                   07  SPA-GMN-JIBAI-FRACTION-KBN2-MAX
                                              PIC 9(02).   
      *
                   07  SPA-GMN-JIBAI-PDFLST   OCCURS  2.
                       09  SPA-GMN-JIBAI-PDFL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-JIBAI-PDFMEIL
                                              PIC X(20).
                   07  SPA-GMN-JIBAI-PDF-MAX
                                              PIC 9(02).
      *
                   07  SPA-GMN-HKN-RIREKILST       OCCURS  2.
                       09  SPA-GMN-HKN-RIREKIL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-HKN-RIREKIMEIL
                                              PIC X(20).
                   07  SPA-GMN-HKN-RIREKI-MAX
                                              PIC 9(02).
      *
                   07  SPA-GMN-GENBUTU-ICHIBULST   OCCURS  2.
                       09  SPA-GMN-GENBUTU-ICHIBUL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-GENBUTU-ICHIBUMEIL
                                              PIC X(36).
                   07  SPA-GMN-GENBUTU-ICHIBU-MAX
                                              PIC 9(02).
      *
                   07  SPA-GMN-COMPRESSLST    OCCURS  2.
                       09  SPA-GMN-COMPRESSL  PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-COMPRESSMEIL
                                              PIC X(28).
                   07  SPA-GMN-COMPRESS-MAX   PIC 9(02).
      *
                   07  SPA-GMN-DISASTER-RECEIPTLST     OCCURS  2.
                       09  SPA-GMN-DISASTER-RECEIPTL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-DISASTER-RECEIPTMEIL
                                              PIC X(18).
                   07  SPA-GMN-DISASTER-RECEIPT-MAX
                                              PIC 9(02).
      *
                   07  SPA-GMN-15-YKZLST      OCCURS  2.
                       09  SPA-GMN-15-YKZL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-15-YKZMEIL
                                              PIC X(18).
                   07  SPA-GMN-15-YKZ-MAX
                                              PIC 9(02).
      *
                   07  SPA-GMN-KOUHI-ICHIBU-KSILST  OCCURS  2.
                       09  SPA-GMN-KOUHI-ICHIBU-KSIL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-KOUHI-ICHIBU-KSIMEIL
                                              PIC X(30).
                   07  SPA-GMN-KOUHI-ICHIBU-KSI-MAX
                                              PIC 9(02).
      *
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-STYUKYMD       PIC 9(08).
                   07  SPA-NAI-EDYUKYMD       PIC 9(08).
      *
                   07  SPA-NAI-2008-KBNCD.
                       09  SPA-NAI-2008-KBNCD-9
                                              PIC 9(08).
      *
               05  SPA-1001-PREFNUM           PIC 9(02). 
               05  SPA-1001-HOSPSBT1          PIC 9(01).
               05  SPA-4001-RSIIRYOKBN-FLG    PIC 9(01).
               05  SPA-200501-RECEDEN-DATAKBN PIC X(01).
               05  SPA-W12-PAGENO             PIC S9(9)   BINARY.
      *
               05  SPA-WIDCD-1004-FLG         PIC 9(01).
      *
               05  SPA-GMN-KKHRECEKBN-O       PIC X(01).
               05  SPA-GMN-SYARECEKBN-O       PIC X(01).
               05  SPA-GMN-KOUIKIRECEKBN-O    PIC X(01).
               05  SPA-GMN-KOUIKIKBN-O        PIC X(01).
      *
               05  SPA-GMN-SELNUM-O           PIC 9(02). 
      *
           COPY    "ENUM-VALUE".
      *
       01  STS-AREA.
           03  STS-WRK             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
      *
           03  FLG-ROW             PIC 9(01).
           03  FLG-UPDFLG          PIC 9(01).
           03  FLG-IN-SELNUM       PIC 9(01).
           03  FLG-HKNJANUMLIST-ROW
                                   PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
      *    添字領域
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
           03  IDXX                PIC 9(04).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-GMN-CUR         PIC 9(02).
           03  WRK-WIDMSG          PIC X(100). 
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
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-KBNCD.
               05  WRK-KBNCD-9     PIC 9(08).
      *
           03  WRK-SELNUM          PIC 9(02).
           03  WRK-Z9              PIC Z9.
      *
           03  WRK-HKNJANUMLIST-ROW
                                   PIC S9(09).
      *
       01  WRKPARA                 PIC X(40).
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
       01  WRK-CONST-AREA.
           03  WRK-CONS-JYOKEN-TENSU
                                   PIC 9(08)   VALUE   350000.
      *    直接請求する保険者番号の最大数
           03  WRK-CONS-200505-HKNJANUM-MAX
                                   PIC 9(02)   VALUE   5.
      *         
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    レセプト印刷情報
           COPY  "CPSK2005.INC".
           COPY  "CPSK200501.INC"  REPLACING   //SYS-200501//
                                   BY          //SYS-25//.
           COPY  "CPSK200505.INC".
      *
      *    レセプト印刷情報（編綴順）
           COPY  "CPSK2008.INC".
      *
      *    レセプト印刷情報（主科設定）
           COPY  "CPSK2009.INC".
      *
      *    レセ電記号番号区切り文字設定情報
           COPY    "CPSK2011.INC"  REPLACING   //SYS-2011//
                                   BY          //S21//.
      *
           COPY    "CPSK4001.INC".
      *
      *    保険者番号
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
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
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
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
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W12
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
           MOVE    SPA-W12         TO  SPA-FREE
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
      *    エラー画面より
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               PERFORM 510-GSRAUTH-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
               IF      SPA-WIDCD       NOT =   SPACE
                   PERFORM 520-WIDSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
               IF      SPA-WIDCD2      NOT =   SPACE
                   PERFORM 530-WID2SET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      * 
           MOVE    SPACE                TO  MCP-PUTTYPE
           MOVE    "W12"                TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                    TO  FLG-END
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
                   GO      TO      300-SCREEN-EXT
               WHEN    "WID2"
                   PERFORM 330-WID2-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "W121"
                   PERFORM 310-SPASET-SEC
                   MOVE    70           TO  SPA-GMN-CUR
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＷＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF      SPA-WID1-FLG    =   "OK"
               EVALUATE    SPA-WIDCD
                   WHEN    "1001"
                   WHEN    "1002"
                       PERFORM 490-KOUSHIN-SEC
                   WHEN    "1003"
                       MOVE    SPA-GMN-SYUBYOHENFLGLST (1)
                                           TO  SPA-GMN-SYUBYOHENFLG-G
               END-EVALUATE
           ELSE
               EVALUATE    SPA-WIDCD
                   WHEN    "1003"
                       MOVE    65              TO  SPA-GMN-CUR
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
      *    確認画面（ＷＩＤ２）ＯＫ処理
      *****************************************************************
       330-WID2-SET-SEC            SECTION.
      *
           IF      SPA-WID2-FLG    =   "OK"
               EVALUATE    SPA-WIDCD2
                   WHEN    "1004"
                       MOVE    SPA-GMN-KOUIKIKBNLST (2)
                                           TO  SPA-GMN-KOUIKIKBN-G
               END-EVALUATE
           ELSE
               EVALUATE    SPA-WIDCD2
                   WHEN    "1005"
      *                特定健康保険組合は対象としない
                       MOVE    SPACE       TO   SPA-GMN-HKNJANUM1-63
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-WIDCD2
               WHEN    "1005"
      *            直接請求する保険者の更新処理
                   PERFORM 4901-HKNJANUM-UPDATE-01-SEC
           END-EVALUATE
      *
      **** MOVE    0               TO  SPA-WIDCD2-1004-FLG 
           MOVE    SPACE           TO  SPA-WID2-FLG
           MOVE    SPACE           TO  SPA-WIDCD2      
           .
       330-WID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE   SPA-W12-AREA
      *
           MOVE    2                   TO  SPA-WIDCD-1004-FLG 
      *
           MOVE    SPA-W01-STYUKYMDH   TO  SPA-GMN-HSTYUKYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  SPA-GMN-HEDYUKYMD
      *     
           MOVE    "0"                 TO  SPA-GMN-BYOMEIHENKBNL    (1)
           MOVE    "連結記載"          TO  SPA-GMN-BYOMEIHENKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-BYOMEIHENKBNL    (2)
           MOVE    "単独記載"          TO  SPA-GMN-BYOMEIHENKBNMEIL (2)
           MOVE    2                   TO  SPA-GMN-BYOMEIHENKBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-BYOMEIHENKBN1L   (1)
           MOVE    "受診科のみ記載"    TO  SPA-GMN-BYOMEIHENKBN1MEIL(1)
           MOVE    "1"                 TO  SPA-GMN-BYOMEIHENKBN1L   (2)
           MOVE    "全ての診療科分を記載"
                                       TO  SPA-GMN-BYOMEIHENKBN1MEIL(2)
           MOVE    2                   TO  SPA-GMN-BYOMEIHENKBN1-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-SYUBYOHENFLGL   (1)
           MOVE    "主病名の編集を行わない"
                                       TO  SPA-GMN-SYUBYOHENFLGMEIL(1)
           MOVE    "1"                 TO  SPA-GMN-SYUBYOHENFLGL   (2)
           MOVE    "主病名の編集を行う（区切り有）"
                                       TO  SPA-GMN-SYUBYOHENFLGMEIL(2)
           MOVE    "2"                 TO  SPA-GMN-SYUBYOHENFLGL   (3)
           MOVE    "主病名の編集を行う（区切り無）"
                                       TO  SPA-GMN-SYUBYOHENFLGMEIL(3)
           MOVE    "3"                 TO  SPA-GMN-SYUBYOHENFLGL   (4)
           MOVE    "主病名の編集を行う（区切り有・主単）"
                                       TO  SPA-GMN-SYUBYOHENFLGMEIL(4)
           MOVE    4                   TO  SPA-GMN-SYUBYOHENFLG-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-TENKIHENKBNL     (1)
           MOVE    "転帰日を記載しない"
                                       TO  SPA-GMN-TENKIHENKBNMEIL  (1)
           MOVE    "1"                 TO  SPA-GMN-TENKIHENKBNL     (2)
           MOVE    "転帰日を記載する"  TO  SPA-GMN-TENKIHENKBNMEIL  (2)
           MOVE    2                   TO  SPA-GMN-TENKIHENKBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-TENKIBYOMEI-KBNL  (1)
           MOVE    "転帰日を優先して記載（転帰日以前は表示期間を優先）"
                                       TO  SPA-GMN-TENKIBYOMEI-KBNMEIL
                                                                     (1)
           MOVE    "1"                 TO  SPA-GMN-TENKIBYOMEI-KBNL  (2)
           MOVE    "表示期間を優先して記載"
                                       TO  SPA-GMN-TENKIBYOMEI-KBNMEIL
                                                                     (2)
           MOVE    2                   TO  SPA-GMN-TENKIBYOMEI-KBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-KKHRECEKBNL      (1)
           MOVE    "出力なし"          TO  SPA-GMN-KKHRECEKBNMEIL   (1)
           MOVE    "1"                 TO  SPA-GMN-KKHRECEKBNL      (2)
           MOVE    "入院外のみ"        TO  SPA-GMN-KKHRECEKBNMEIL   (2)
           MOVE    "2"                 TO  SPA-GMN-KKHRECEKBNL      (3)
           MOVE    "入院のみ"          TO  SPA-GMN-KKHRECEKBNMEIL   (3)
           MOVE    "3"                 TO  SPA-GMN-KKHRECEKBNL      (4)
           MOVE    "入院・入院外"
                                       TO  SPA-GMN-KKHRECEKBNMEIL   (4)
           MOVE    4                   TO  SPA-GMN-KKHRECEKBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-SYARECEKBNL      (1)
           MOVE    "出力なし"          TO  SPA-GMN-SYARECEKBNMEIL   (1)
           MOVE    "1"                 TO  SPA-GMN-SYARECEKBNL      (2)
           MOVE    "入院外のみ"        TO  SPA-GMN-SYARECEKBNMEIL   (2)
           MOVE    "2"                 TO  SPA-GMN-SYARECEKBNL      (3)
           MOVE    "入院のみ"          TO  SPA-GMN-SYARECEKBNMEIL   (3)
           MOVE    "3"                 TO  SPA-GMN-SYARECEKBNL      (4)
           MOVE    "入院・入院外"      TO  SPA-GMN-SYARECEKBNMEIL   (4)
           MOVE    4                   TO  SPA-GMN-SYARECEKBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-KOUIKIRECEKBNL    (1)
           MOVE    "出力なし"          TO  SPA-GMN-KOUIKIRECEKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-KOUIKIRECEKBNL    (2)
           MOVE    "入院外のみ"        TO  SPA-GMN-KOUIKIRECEKBNMEIL (2)
           MOVE    "2"                 TO  SPA-GMN-KOUIKIRECEKBNL    (3)
           MOVE    "入院のみ"          TO  SPA-GMN-KOUIKIRECEKBNMEIL (3)
           MOVE    "3"                 TO  SPA-GMN-KOUIKIRECEKBNL    (4)
           MOVE    "入院・入院外"
                                       TO  SPA-GMN-KOUIKIRECEKBNMEIL (4)
           MOVE    4                   TO  SPA-GMN-KOUIKIRECEKBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-ROURECEKBNL    (1)
           MOVE    "出力なし"          TO  SPA-GMN-ROURECEKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-ROURECEKBNL    (2)
           MOVE    "入院外のみ"        TO  SPA-GMN-ROURECEKBNMEIL (2)
           MOVE    "2"                 TO  SPA-GMN-ROURECEKBNL    (3)
           MOVE    "入院のみ"          TO  SPA-GMN-ROURECEKBNMEIL (3)
           MOVE    "3"                 TO  SPA-GMN-ROURECEKBNL    (4)
           MOVE    "入院・入院外"
                                       TO  SPA-GMN-ROURECEKBNMEIL (4)
           MOVE    4                   TO  SPA-GMN-ROURECEKBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-KKHOCRKBNL    (1)
           MOVE    "ＯＣＲなし"        TO  SPA-GMN-KKHOCRKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-KKHOCRKBNL    (2)
           MOVE    "ＯＣＲ（入院外のみ）"
                                       TO  SPA-GMN-KKHOCRKBNMEIL (2)
           MOVE    "2"                 TO  SPA-GMN-KKHOCRKBNL    (3)
           MOVE    "ＯＣＲ（入院のみ）"
                                       TO  SPA-GMN-KKHOCRKBNMEIL (3)
           MOVE    "3"                 TO  SPA-GMN-KKHOCRKBNL    (4)
           MOVE    "ＯＣＲ（入院・入院外）"
                                       TO  SPA-GMN-KKHOCRKBNMEIL (4)
           MOVE    4                   TO  SPA-GMN-KKHOCRKBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-KOUIKIOCRKBNL    (1)
           MOVE    "ＯＣＲなし"        TO  SPA-GMN-KOUIKIOCRKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-KOUIKIOCRKBNL    (2)
           MOVE    "ＯＣＲ（入院外のみ）"
                                       TO  SPA-GMN-KOUIKIOCRKBNMEIL (2)
           MOVE    "2"                 TO  SPA-GMN-KOUIKIOCRKBNL    (3)
           MOVE    "ＯＣＲ（入院のみ）"
                                       TO  SPA-GMN-KOUIKIOCRKBNMEIL (3)
           MOVE    "3"                 TO  SPA-GMN-KOUIKIOCRKBNL    (4)
           MOVE    "ＯＣＲ（入院・入院外）"
                                       TO  SPA-GMN-KOUIKIOCRKBNMEIL (4)
           MOVE    4                   TO  SPA-GMN-KOUIKIOCRKBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-SYAOCRKBNL    (1)
           MOVE    "ＯＣＲ（入院外のみ）"
                                       TO  SPA-GMN-SYAOCRKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-SYAOCRKBNL    (2)
           MOVE    "ＯＣＲなし"        TO  SPA-GMN-SYAOCRKBNMEIL (2)
           MOVE    "2"                 TO  SPA-GMN-SYAOCRKBNL    (3)
           MOVE    "ＯＣＲ（入院・入院外）"
                                       TO  SPA-GMN-SYAOCRKBNMEIL (3)
           MOVE    3                   TO  SPA-GMN-SYAOCRKBN-MAX 
      *     
           MOVE    "0"                 TO
                                   SPA-GMN-NYUTAIIN-KSIKBNL    (1)
           MOVE    "入退院履歴を記載しない"
                                       TO
                                   SPA-GMN-NYUTAIIN-KSIKBNMEIL (1)
           MOVE    "1"                 TO
                                   SPA-GMN-NYUTAIIN-KSIKBNL    (2)
           MOVE    "入退院履歴を記載する"
                                       TO
                                   SPA-GMN-NYUTAIIN-KSIKBNMEIL (2)
           MOVE    "2"                 TO
                                   SPA-GMN-NYUTAIIN-KSIKBNL    (3)
           MOVE    "入退院履歴を記載する（自費入院分は除く）"
                                       TO
                                   SPA-GMN-NYUTAIIN-KSIKBNMEIL (3)
           MOVE    3                   TO
                                   SPA-GMN-NYUTAIIN-KSIKBN-MAX 
      *     
           MOVE    "0"                 TO
                                   SPA-GMN-NYUTAIIN-NYU-KSIKBNL (1)
           MOVE    "入院料の算定がない入退院履歴を記載しない"
                                       TO
                                   SPA-GMN-NYUTAIIN-NYU-KSIKBNMEIL (1)
           MOVE    "1"                 TO
                                   SPA-GMN-NYUTAIIN-NYU-KSIKBNL  (2)
           MOVE    "入院料の算定がない入退院履歴を記載する"
                                       TO
                                   SPA-GMN-NYUTAIIN-NYU-KSIKBNMEIL (2)
           MOVE    2                   TO
                                   SPA-GMN-NYUTAIIN-NYU-KSIKBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-KKHPRTKBNL    (1)
           MOVE    "給付割合なしの様式"
                                       TO  SPA-GMN-KKHPRTKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-KKHPRTKBNL    (2)
           MOVE    "給付割合ありの様式"
                                       TO  SPA-GMN-KKHPRTKBNMEIL (2)
           MOVE    2                   TO  SPA-GMN-KKHPRTKBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-SYAPRTKBNL    (1)
           MOVE    "マル長分を別作成しない"
                                       TO  SPA-GMN-SYAPRTKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-SYAPRTKBNL    (2)
           MOVE    "マル長分を別作成する"
                                       TO  SPA-GMN-SYAPRTKBNMEIL (2)
           MOVE    2                   TO  SPA-GMN-SYAPRTKBN-MAX 
      *
           MOVE    "0"                 TO  SPA-GMN-DISASTER-SYAPRTKBNL
                                                                   (1)
           MOVE    "災害該当未確定分を別作成する"
                                       TO SPA-GMN-DISASTER-SYAPRTKBNMEIL
                                                                   (1)
           MOVE    "1"                 TO  SPA-GMN-DISASTER-SYAPRTKBNL
                                                                   (2)
           MOVE    "災害該当未確定分を別作成しない"
                                       TO SPA-GMN-DISASTER-SYAPRTKBNMEIL
                                                                   (2)
           MOVE    2                   TO SPA-GMN-DISASTER-SYAPRTKBN-MAX
      *     
           MOVE    "0"                 TO  SPA-GMN-KOUIKIPRTKBNL    (1)
           MOVE    "全て広域連合単位で作成"
                                       TO  SPA-GMN-KOUIKIPRTKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-KOUIKIPRTKBNL    (2)
           MOVE    "全て保険者番号単位で作成"
                                       TO  SPA-GMN-KOUIKIPRTKBNMEIL (2)
           MOVE    "2"                 TO  SPA-GMN-KOUIKIPRTKBNL    (3)
           MOVE    "県内分は保険者番号単位で作成"
                                       TO  SPA-GMN-KOUIKIPRTKBNMEIL (3)
           MOVE    "3"                 TO  SPA-GMN-KOUIKIPRTKBNL    (4)
           MOVE    "県外分は保険者番号単位で作成"
                                       TO  SPA-GMN-KOUIKIPRTKBNMEIL (4)
           MOVE    4                   TO  SPA-GMN-KOUIKIPRTKBN-MAX 
      *
           MOVE    "0"                 TO  SPA-GMN-RSI-INVOICEKBNL   (1)
           MOVE    "旧様式"            TO  SPA-GMN-RSI-INVOICEKBNMEIL(1)
           MOVE    "1"                 TO  SPA-GMN-RSI-INVOICEKBNL   (2)
           MOVE    "新様式"            TO  SPA-GMN-RSI-INVOICEKBNMEIL(2)
           MOVE    2                   TO  SPA-GMN-RSI-INVOICEKBN-MAX
      *
           MOVE    "0"                 TO  SPA-GMN-ROUSAIPRTKBNL    (1)
           MOVE    "ＯＣＲ専用様式"    TO  SPA-GMN-ROUSAIPRTKBNMEIL (1)
      *****MOVE    "1"                 TO  SPA-GMN-ROUSAIPRTKBNL    (2)
      *****MOVE    "確認様式"          TO  SPA-GMN-ROUSAIPRTKBNMEIL (2)
           MOVE    1                   TO  SPA-GMN-ROUSAIPRTKBN-MAX 
      *
           MOVE    "0"                 TO
                                       SPA-GMN-FRAME-ROUSAIPRTKBNL   (1)
           MOVE    "枠を記載しない"    TO
                                       SPA-GMN-FRAME-ROUSAIPRTKBNMEIL(1)
           MOVE    "1"                 TO
                                       SPA-GMN-FRAME-ROUSAIPRTKBNL   (2)
           MOVE    "枠を記載する"      TO
                                       SPA-GMN-FRAME-ROUSAIPRTKBNMEIL(2)
           MOVE    2                   TO
                                       SPA-GMN-FRAME-ROUSAIPRTKBN-MAX 
      *
           MOVE    "0"                 TO  SPA-GMN-RENNUMHENKBNL    (1)
           MOVE    "通し番号を記載しない"
                                       TO  SPA-GMN-RENNUMHENKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-RENNUMHENKBNL    (2)
           MOVE    "通し番号を記載する"
                                       TO  SPA-GMN-RENNUMHENKBNMEIL (2)
           MOVE    2                   TO  SPA-GMN-RENNUMHENKBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-NEXTPRTKBNL    (1)
           MOVE    "横タイプ"          TO  SPA-GMN-NEXTPRTKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-NEXTPRTKBNL    (2)
           MOVE    "縦タイプ"          TO  SPA-GMN-NEXTPRTKBNMEIL (2)
           MOVE    "2"                 TO  SPA-GMN-NEXTPRTKBNL    (3)
           MOVE    "縦タイプ（長）"    TO  SPA-GMN-NEXTPRTKBNMEIL (3)
           MOVE    3                   TO  SPA-GMN-NEXTPRTKBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-NEXTPRTKBN1L    (1)
           MOVE    "指定なし"          TO  SPA-GMN-NEXTPRTKBN1MEIL (1)
           MOVE    "1"                 TO  SPA-GMN-NEXTPRTKBN1L    (2)
           MOVE    "横タイプ"          TO  SPA-GMN-NEXTPRTKBN1MEIL (2)
           MOVE    "2"                 TO  SPA-GMN-NEXTPRTKBN1L    (3)
           MOVE    "縦タイプ"          TO  SPA-GMN-NEXTPRTKBN1MEIL (3)
           MOVE    "3"                 TO  SPA-GMN-NEXTPRTKBN1L    (4)
           MOVE    "縦タイプ（長）"    TO  SPA-GMN-NEXTPRTKBN1MEIL (4)
           MOVE    4                   TO  SPA-GMN-NEXTPRTKBN1-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-RJNGNBKKBNL    (1)
           MOVE    "社保、国保とも印刷不可"
                                       TO  SPA-GMN-RJNGNBKKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-RJNGNBKKBNL    (2)
           MOVE    "社保の併用に限り印刷" 
                                       TO  SPA-GMN-RJNGNBKKBNMEIL (2)
           MOVE    "2"                 TO  SPA-GMN-RJNGNBKKBNL    (3)
           MOVE    "国保の併用に限り印刷"
                                       TO  SPA-GMN-RJNGNBKKBNMEIL (3)
           MOVE    "3"                 TO  SPA-GMN-RJNGNBKKBNL    (4)
           MOVE    "社保、国保とも印刷"
                                       TO  SPA-GMN-RJNGNBKKBNMEIL (4)
           MOVE    4                   TO  SPA-GMN-RJNGNBKKBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-RECECOMKBNL    (1)
           MOVE    "レセ電データに記録する"
                                       TO  SPA-GMN-RECECOMKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-RECECOMKBNL    (2)
           MOVE    "レセ電データに記録しない"
                                       TO  SPA-GMN-RECECOMKBNMEIL (2)
           MOVE    2                   TO  SPA-GMN-RECECOMKBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-RECEDEN-CDRKBNL   (1)
           MOVE    "しない"            TO  SPA-GMN-RECEDEN-CDRKBNMEIL(1)
           MOVE    "1"                 TO  SPA-GMN-RECEDEN-CDRKBNL   (2)
           MOVE    "する"              TO  SPA-GMN-RECEDEN-CDRKBNMEIL(2)
           MOVE    2                   TO  SPA-GMN-RECEDEN-CDRKBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-RECEDEN-LST1KBNL (1)
           MOVE    "作成しない"        TO  SPA-GMN-RECEDEN-LST1KBNMEIL
                                                                    (1)
           MOVE    "1"                 TO  SPA-GMN-RECEDEN-LST1KBNL (2)
           MOVE    "作成する"          TO  SPA-GMN-RECEDEN-LST1KBNMEIL
                                                                    (2)
           MOVE    2                   TO  SPA-GMN-RECEDEN-LST1KBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-RECEDEN-LST2KBNL (1)
           MOVE    "作成しない"        TO  SPA-GMN-RECEDEN-LST2KBNMEIL
                                                                    (1)
           MOVE    "1"                 TO  SPA-GMN-RECEDEN-LST2KBNL (2)
           MOVE    "作成する"          TO  SPA-GMN-RECEDEN-LST2KBNMEIL
                                                                    (2)
           MOVE    2                   TO  SPA-GMN-RECEDEN-LST2KBN-MAX 
      *
           MOVE    "0"                 TO
                                       SPA-GMN-RECEDEN-LST2-MEDIAL  (1)
           MOVE    "設定なし"          TO
                                       SPA-GMN-RECEDEN-LST2-MEDIAMEIL
                                                                    (1)
           MOVE    "1"                 TO
                                       SPA-GMN-RECEDEN-LST2-MEDIAL  (2)
           MOVE    "フロッピィ"        TO
                                       SPA-GMN-RECEDEN-LST2-MEDIAMEIL
                                                                    (2)
           MOVE    "2"                 TO
                                       SPA-GMN-RECEDEN-LST2-MEDIAL  (3)
           MOVE    "ＭＯ"              TO
                                       SPA-GMN-RECEDEN-LST2-MEDIAMEIL
                                                                    (3)
           MOVE    "3"                 TO
                                       SPA-GMN-RECEDEN-LST2-MEDIAL  (4)
           MOVE    "ＣＤ−Ｒ"          TO
                                       SPA-GMN-RECEDEN-LST2-MEDIAMEIL
                                                                    (4)
           MOVE    4                   TO
                                       SPA-GMN-RECEDEN-LST2-MEDIA-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-RECEDEN-RIREKIKBNL(1)
           MOVE    "行なわない"        TO  SPA-GMN-RECEDEN-RIREKIKBNMEIL
                                                                     (1)
           MOVE    "1"                 TO  SPA-GMN-RECEDEN-RIREKIKBNL(2)
           MOVE    "行なう"            TO  SPA-GMN-RECEDEN-RIREKIKBNMEIL
                                                                     (2)
           MOVE    2                   TO  SPA-GMN-RECEDEN-RIREKIKBN-MAX 
      *     
           MOVE    ZERO                TO  IDXX
           IF      SPA-MW              =   SPA-PANDA
               ADD     1                   TO  IDXX
               MOVE    "3"                 TO  SPA-GMN-RECEDEN-DATAKBNL
                                                                 (IDXX)
               MOVE    "システム管理設定場所（フロッピィ用）"
                                       TO  SPA-GMN-RECEDEN-DATAKBNMEIL
                                                                 (IDXX)
               ADD     1                   TO  IDXX
               MOVE    "4"                 TO  SPA-GMN-RECEDEN-DATAKBNL
                                                                 (IDXX)
               MOVE    "システム管理設定場所（ＭＯ用）"
                                       TO  SPA-GMN-RECEDEN-DATAKBNMEIL
                                                                 (IDXX)
           END-IF
           ADD     1                   TO  IDXX
           MOVE    "5"                 TO  SPA-GMN-RECEDEN-DATAKBNL
                                                                 (IDXX)
           MOVE    "クライアント保存"  TO  SPA-GMN-RECEDEN-DATAKBNMEIL
                                                                 (IDXX)
           ADD     1                   TO  IDXX
           MOVE    "6"                 TO  SPA-GMN-RECEDEN-DATAKBNL
                                                                 (IDXX)
           MOVE    "クライアント保存（ＣＤ−Ｒ用）"
                                       TO  SPA-GMN-RECEDEN-DATAKBNMEIL
                                                                 (IDXX)
           MOVE    IDXX                TO  SPA-GMN-RECEDEN-DATAKBN-MAX
      *     
           MOVE    "0"                 TO  SPA-GMN-RECEDEN-DISASTERL (1)
           MOVE    "記録しない"        TO  SPA-GMN-RECEDEN-DISASTERMEIL
                                                                     (1)
           MOVE    "1"                 TO  SPA-GMN-RECEDEN-DISASTERL (2)
           MOVE    "記録する"          TO  SPA-GMN-RECEDEN-DISASTERMEIL
                                                                     (2)
           MOVE    2                   TO  SPA-GMN-RECEDEN-DISASTER-MAX
      *     
           MOVE    "0"                 TO  SPA-GMN-RECEDEN-CHKL (1)
           MOVE    "チェックしない"    TO  SPA-GMN-RECEDEN-CHKMEIL
                                                                (1)
           MOVE    "1"                 TO  SPA-GMN-RECEDEN-CHKL (2)
           MOVE    "チェックする"      TO  SPA-GMN-RECEDEN-CHKMEIL
                                                                (2)
           MOVE    2                   TO  SPA-GMN-RECEDEN-CHK-MAX
      *     
           MOVE    "0"                 TO  SPA-GMN-KIGO-DELI-FLGL
                                                                   (1)
           MOVE    "編集なし"          TO
                                       SPA-GMN-KIGO-DELI-FLGMEIL
                                                                   (1)
           MOVE    "1"                 TO  SPA-GMN-KIGO-DELI-FLGL
                                                                   (2)
           MOVE    "「−」を全角ハイフンに変換" 
                                       TO
                                       SPA-GMN-KIGO-DELI-FLGMEIL
                                                                   (2)
           MOVE    "2"                 TO  SPA-GMN-KIGO-DELI-FLGL
                                                                   (3)
           MOVE    "「−」を全角マイナスに変換"
                                       TO
                                       SPA-GMN-KIGO-DELI-FLGMEIL
                                                                   (3)
           MOVE    "9"                 TO  SPA-GMN-KIGO-DELI-FLGL
                                                                   (4)
           MOVE    "「−」を削除"      TO
                                       SPA-GMN-KIGO-DELI-FLGMEIL
                                                                   (4)
           MOVE    4                   TO
                                       SPA-GMN-KIGO-DELI-FLG-MAX
      *     
           MOVE    "0"                 TO
                               SPA-GMN-KIGO-DELI-HKNJA-FLGL
                                                                   (1)
           MOVE    "設定なし"          TO
                               SPA-GMN-KIGO-DELI-HKNJA-FLGMEIL
                                                                   (1)
           MOVE    "1"                 TO
                               SPA-GMN-KIGO-DELI-HKNJA-FLGL
                                                                   (2)
           MOVE    "県内保険者"        TO
                               SPA-GMN-KIGO-DELI-HKNJA-FLGMEIL
                                                                   (2)
           MOVE    "2"                 TO
                               SPA-GMN-KIGO-DELI-HKNJA-FLGL
                                                                   (3)
           MOVE    "県内保険者と県内扱い保険者"
                                       TO
                               SPA-GMN-KIGO-DELI-HKNJA-FLGMEIL
                                                                   (3)
           MOVE    3                   TO
                               SPA-GMN-KIGO-DELI-HKNJA-FLG-MAX
      *     
           MOVE    "0"                 TO  SPA-GMN-NUM-DELIMITER-FLGL
                                                                   (1)
           MOVE    "編集なし"          TO
                                       SPA-GMN-NUM-DELIMITER-FLGMEIL
                                                                   (1)
           MOVE    "1"                 TO  SPA-GMN-NUM-DELIMITER-FLGL
                                                                   (2)
           MOVE    "「−」を全角ハイフンに変換" 
                                       TO
                                       SPA-GMN-NUM-DELIMITER-FLGMEIL
                                                                   (2)
           MOVE    "2"                 TO  SPA-GMN-NUM-DELIMITER-FLGL
                                                                   (3)
           MOVE    "「−」を全角マイナスに変換"
                                       TO
                                       SPA-GMN-NUM-DELIMITER-FLGMEIL
                                                                   (3)
           MOVE    "9"                 TO  SPA-GMN-NUM-DELIMITER-FLGL
                                                                   (4)
           MOVE    "「−」を削除"      TO
                                       SPA-GMN-NUM-DELIMITER-FLGMEIL
                                                                   (4)
           MOVE    4                   TO
                                       SPA-GMN-NUM-DELIMITER-FLG-MAX
      *     
           MOVE    "0"                 TO
                               SPA-GMN-NUM-DELI-HKNJA-FLGL (1)
           MOVE    "設定なし"          TO
                               SPA-GMN-NUM-DELI-HKNJA-FLGMEIL
                                                                   (1)
           MOVE    "1"                 TO
                               SPA-GMN-NUM-DELI-HKNJA-FLGL (2)
           MOVE    "県内保険者"        TO
                               SPA-GMN-NUM-DELI-HKNJA-FLGMEIL
                                                                   (2)
           MOVE    "2"                 TO
                               SPA-GMN-NUM-DELI-HKNJA-FLGL (3)
           MOVE    "県内保険者と県内扱い保険者"
                                       TO
                               SPA-GMN-NUM-DELI-HKNJA-FLGMEIL
                                                                   (3)
           MOVE    3                   TO
                               SPA-GMN-NUM-DELI-HKNJA-FLG-MAX
      *       
           MOVE    "0"                 TO  SPA-GMN-SRYKAHENKBNL    (1)
           MOVE    "診療科を記載する"  TO  SPA-GMN-SRYKAHENKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-SRYKAHENKBNL    (2)
           MOVE    "診療科を記載しない"
                                       TO  SPA-GMN-SRYKAHENKBNMEIL (2)
           MOVE    2                   TO  SPA-GMN-SRYKAHENKBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-BTUNUM-KSIKBNL    (1)
           MOVE    "病棟番号を記載しない"
                                       TO  SPA-GMN-BTUNUM-KSIKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-BTUNUM-KSIKBNL    (2)
           MOVE    "病棟番号を記載する"
                                       TO  SPA-GMN-BTUNUM-KSIKBNMEIL (2)
           MOVE    "2"                 TO  SPA-GMN-BTUNUM-KSIKBNL    (3)
           MOVE    "病棟・病室番号を記載する"
                                       TO  SPA-GMN-BTUNUM-KSIKBNMEIL (3)
           MOVE    3                   TO  SPA-GMN-BTUNUM-KSIKBN-MAX 
      *
           MOVE    "0"                 TO  SPA-GMN-DRNAME-KSIKBNL    (1)
           MOVE    "医師名を記載しない"
                                       TO  SPA-GMN-DRNAME-KSIKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-DRNAME-KSIKBNL    (2)
           MOVE    "医師名を記載する"  TO  SPA-GMN-DRNAME-KSIKBNMEIL (2)
           MOVE    2                   TO  SPA-GMN-DRNAME-KSIKBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-JIHITEKKSIKBNL    (1)
           MOVE    "なし"              TO  SPA-GMN-JIHITEKKSIKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-JIHITEKKSIKBNL    (2)
           MOVE    "あり"              TO  SPA-GMN-JIHITEKKSIKBNMEIL (2)
           MOVE    2                   TO  SPA-GMN-JIHITEKKSIKBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-KOUIKIKBNL    (1)
           MOVE    "広域連合分のみで作成"
                                       TO  SPA-GMN-KOUIKIKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-KOUIKIKBNL    (2)
           MOVE    "国保分と広域連合分をまとめて作成"
                                       TO  SPA-GMN-KOUIKIKBNMEIL (2)
           MOVE    2                   TO  SPA-GMN-KOUIKIKBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-COMMENT-NYUINRYOL(1)
           MOVE    "自動記載しない"
                                       TO  SPA-GMN-COMMENT-NYUINRYOMEIL
                                                                    (1)
           MOVE    "1"                 TO  SPA-GMN-COMMENT-NYUINRYOL(2)
           MOVE    "「自費にて入院料算定済」と記載"
                                       TO  SPA-GMN-COMMENT-NYUINRYOMEIL
                                                                    (2)
           MOVE    "2"                 TO  SPA-GMN-COMMENT-NYUINRYOL(3)
           MOVE    "「他保険にて入院料算定済」と記載"
                                       TO  SPA-GMN-COMMENT-NYUINRYOMEIL
                                                                    (3)
           MOVE    "3"                 TO  SPA-GMN-COMMENT-NYUINRYOL(4)
           MOVE    "保険組合せに準ずる自動記載"
                                       TO  SPA-GMN-COMMENT-NYUINRYOMEIL
                                                                    (4)
           MOVE    4                   TO  SPA-GMN-COMMENT-NYUINRYO-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-COMMENT-NYUINRYO1L(1)
           MOVE    "自動記載しない"
                                       TO  SPA-GMN-COMMENT-NYUINRYO1MEIL
                                                                     (1)
           MOVE    "1"                 TO  SPA-GMN-COMMENT-NYUINRYO1L(2)
           MOVE    "「他保険にて入院料算定済」と記載"
                                       TO  SPA-GMN-COMMENT-NYUINRYO1MEIL
                                                                     (2)
           MOVE    "2"                 TO  SPA-GMN-COMMENT-NYUINRYO1L(3)
           MOVE    "保険組合せに準ずる自動記載"
                                       TO  SPA-GMN-COMMENT-NYUINRYO1MEIL
                                                                     (3)
           MOVE    3                   TO  SPA-GMN-COMMENT-NYUINRYO1-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-CHKKBNL    (1)
           MOVE    "チェックしない"    TO  SPA-GMN-CHKKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-CHKKBNL    (2)
           MOVE    "チェックする"      TO  SPA-GMN-CHKKBNMEIL (2)
           MOVE    "2"                 TO  SPA-GMN-CHKKBNL    (3)
           MOVE    "チェックする(自費レセプトは除く)" 
                                       TO  SPA-GMN-CHKKBNMEIL (3)
           MOVE    3                   TO  SPA-GMN-CHKKBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-BYOMEI-PRTJYUNL   (1)
           MOVE    "転帰分を同一開始日の末尾に記載する"
                                       TO  SPA-GMN-BYOMEI-PRTJYUNMEIL(1)
           MOVE    "1"                 TO  SPA-GMN-BYOMEI-PRTJYUNL   (2)
           MOVE    "転帰分を同一開始日の先頭に記載する"
                                       TO  SPA-GMN-BYOMEI-PRTJYUNMEIL(2)
           MOVE    "2"                 TO  SPA-GMN-BYOMEI-PRTJYUNL   (3)
           MOVE    "入力順に記載する"
                                       TO  SPA-GMN-BYOMEI-PRTJYUNMEIL(3)
           MOVE    3                   TO  SPA-GMN-BYOMEI-PRTJYUN-MAX 
      *
           MOVE    "0"                 TO  SPA-GMN-SHOKUJI-PRTKBNL   (1)
           MOVE    "食事上限０円時は「０」を記載する"
                                       TO  SPA-GMN-SHOKUJI-PRTKBNMEIL(1)
           MOVE    "1"                 TO  SPA-GMN-SHOKUJI-PRTKBNL   (2)
           MOVE    "食事上限０円時は「０」を記載しない"
                                       TO  SPA-GMN-SHOKUJI-PRTKBNMEIL(2)
           MOVE    "2"                 TO  SPA-GMN-SHOKUJI-PRTKBNL   (3)
           MOVE    "１と同様（単独分の標準負担は空欄）"
                                       TO  SPA-GMN-SHOKUJI-PRTKBNMEIL(3)
           MOVE    3                   TO  SPA-GMN-SHOKUJI-PRTKBN-MAX
      *     
           MOVE    "0"                 TO
                                   SPA-GMN-INSPECT-CHARA-KBNL   (1)
           MOVE    "「点検用レセプト」の文字を記載する"
                                       TO
                                   SPA-GMN-INSPECT-CHARA-KBNMEIL(1)
           MOVE    "1"                 TO
                                   SPA-GMN-INSPECT-CHARA-KBNL   (2)
           MOVE    "斜線を記載する"    TO
                                   SPA-GMN-INSPECT-CHARA-KBNMEIL(2)
           MOVE    2                   TO
                                   SPA-GMN-INSPECT-CHARA-KBN-MAX 
      *     
           MOVE    "0"                 TO
                                   SPA-GMN-INSPECT-ZERO-KBNL   (1)
           MOVE    "０点公費を記載しない"
                                       TO
                                   SPA-GMN-INSPECT-ZERO-KBNMEIL(1)
           MOVE    "1"                 TO
                                   SPA-GMN-INSPECT-ZERO-KBNL   (2)
           MOVE    "０点公費を記載する"
                                       TO
                                   SPA-GMN-INSPECT-ZERO-KBNMEIL(2)
           MOVE    2                   TO
                                   SPA-GMN-INSPECT-ZERO-KBN-MAX 
      *     
           MOVE    "0"                 TO
                                   SPA-GMN-INSPECT-INCLUDE-KBNL   (1)
           MOVE    "包括診療分・持参薬を記載しない"
                                       TO
                                   SPA-GMN-INSPECT-INCLUDE-KBNMEIL(1)
           MOVE    "1"                 TO
                                   SPA-GMN-INSPECT-INCLUDE-KBNL   (2)
           MOVE    "包括診療分・持参薬を記載する"
                                       TO
                                   SPA-GMN-INSPECT-INCLUDE-KBNMEIL(2)
           MOVE    2                   TO
                                   SPA-GMN-INSPECT-INCLUDE-KBN-MAX 
      *
           MOVE    "0"                 TO
                                   SPA-GMN-INSPECT-GENERAL-KBNL   (1)
           MOVE    "一般名処方を銘柄名で記載する"
                                       TO
                                   SPA-GMN-INSPECT-GENERAL-KBNMEIL(1)
           MOVE    "1"                 TO
                                   SPA-GMN-INSPECT-GENERAL-KBNL   (2)
           MOVE    "一般名処方を一般名で記載する"
                                       TO
                                   SPA-GMN-INSPECT-GENERAL-KBNMEIL(2)
           MOVE    2                   TO
                                   SPA-GMN-INSPECT-GENERAL-KBN-MAX
      *
           MOVE    "0"                 TO  SPA-GMN-SEIHO-DSPKBNL(1)
           MOVE    "患者番号順"        TO  SPA-GMN-SEIHO-DSPKBNMEIL
                                                                (1)
           MOVE    "1"                 TO  SPA-GMN-SEIHO-DSPKBNL(2)
           MOVE    "負担者番号順"      TO  SPA-GMN-SEIHO-DSPKBNMEIL
                                                                (2)
           MOVE    "2"                 TO  SPA-GMN-SEIHO-DSPKBNL(3)
           MOVE    "カナ氏名順"        TO  SPA-GMN-SEIHO-DSPKBNMEIL
                                                                (3)
           MOVE    3                   TO  SPA-GMN-SEIHO-DSPKBN-MAX 
      *     
           MOVE    "1"                 TO  SPA-GMN-RECEIPT-MAKEKBNL(1)
           MOVE    "提出用レセプト"    TO  SPA-GMN-RECEIPT-MAKEKBNMEIL
                                                                   (1)
           MOVE    "2"                 TO  SPA-GMN-RECEIPT-MAKEKBNL(2)
           MOVE    "点検用レセプト"    TO  SPA-GMN-RECEIPT-MAKEKBNMEIL
                                                                   (2)
           MOVE    2                   TO  SPA-GMN-RECEIPT-MAKEKBN-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-ZERO-KOH-PRTKBNL
                                                                   (1)
           MOVE    "記載しない"        TO
                                       SPA-GMN-ZERO-KOH-PRTKBNMEIL
                                                                   (1)
           MOVE    "1"                 TO  SPA-GMN-ZERO-KOH-PRTKBNL
                                                                   (2)
           MOVE    "記載する"          TO
                                       SPA-GMN-ZERO-KOH-PRTKBNMEIL
                                                                   (2)
           MOVE    2                   TO
                                       SPA-GMN-ZERO-KOH-PRTKBN-MAX 
      *
           MOVE    "01"                TO  SPA-GMN-PRTJYUN1L    (1)
           MOVE    "種別・保険者番号・カナ氏名順"
                                       TO  SPA-GMN-PRTJYUN1MEIL (1)
           MOVE    "02"                TO  SPA-GMN-PRTJYUN1L    (2)
           MOVE    "種別・保険者番号・患者番号順"
                                       TO  SPA-GMN-PRTJYUN1MEIL (2)
           MOVE    "03"                TO  SPA-GMN-PRTJYUN1L    (3)
           MOVE    "カナ氏名順"        TO  SPA-GMN-PRTJYUN1MEIL (3)
           MOVE    "04"                TO  SPA-GMN-PRTJYUN1L    (4)
           MOVE    "患者番号順"        TO  SPA-GMN-PRTJYUN1MEIL (4)
           MOVE    "06"                TO  SPA-GMN-PRTJYUN1L    (5)
           MOVE    "編綴順"            TO  SPA-GMN-PRTJYUN1MEIL (5)
           MOVE    "09"                TO  SPA-GMN-PRTJYUN1L    (6)
           MOVE    "種別・カナ氏名順"  TO  SPA-GMN-PRTJYUN1MEIL (6)
           MOVE    "10"                TO  SPA-GMN-PRTJYUN1L    (7)
           MOVE    "種別・患者番号順"  TO  SPA-GMN-PRTJYUN1MEIL (7)
           MOVE    7                   TO  SPA-GMN-PRTJYUN1-MAX 
      *
           MOVE    "01"                TO  SPA-GMN-PRTJYUN3L    (1)
           MOVE    "種別・保険者番号・カナ氏名順"
                                       TO  SPA-GMN-PRTJYUN3MEIL (1)
           MOVE    "02"                TO  SPA-GMN-PRTJYUN3L    (2)
           MOVE    "種別・保険者番号・患者番号順"
                                       TO  SPA-GMN-PRTJYUN3MEIL (2)
           MOVE    "03"                TO  SPA-GMN-PRTJYUN3L    (3)
           MOVE    "カナ氏名順"        TO  SPA-GMN-PRTJYUN3MEIL (3)
           MOVE    "04"                TO  SPA-GMN-PRTJYUN3L    (4)
           MOVE    "患者番号順"        TO  SPA-GMN-PRTJYUN3MEIL (4)
           MOVE    "06"                TO  SPA-GMN-PRTJYUN3L    (5)
           MOVE    "編綴順"            TO  SPA-GMN-PRTJYUN3MEIL (5)
           MOVE    "09"                TO  SPA-GMN-PRTJYUN3L    (6)
           MOVE    "種別・カナ氏名順"  TO  SPA-GMN-PRTJYUN3MEIL (6)
           MOVE    "10"                TO  SPA-GMN-PRTJYUN3L    (7)
           MOVE    "種別・患者番号順"  TO  SPA-GMN-PRTJYUN3MEIL (7)
           MOVE    7                   TO  SPA-GMN-PRTJYUN3-MAX 
      *
           MOVE    "01"                TO  SPA-GMN-PRTJYUN2L    (1)
           MOVE    "種別・保険者番号・カナ氏名順"
                                       TO  SPA-GMN-PRTJYUN2MEIL (1)
           MOVE    "02"                TO  SPA-GMN-PRTJYUN2L    (2)
           MOVE    "種別・保険者番号・患者番号順"
                                       TO  SPA-GMN-PRTJYUN2MEIL (2)
           MOVE    "03"                TO  SPA-GMN-PRTJYUN2L    (3)
           MOVE    "カナ氏名順"        TO  SPA-GMN-PRTJYUN2MEIL (3)
           MOVE    "04"                TO  SPA-GMN-PRTJYUN2L    (4)
           MOVE    "患者番号順"        TO  SPA-GMN-PRTJYUN2MEIL (4)
           MOVE    "05"                TO  SPA-GMN-PRTJYUN2L    (5)
           MOVE    "入力順"            TO  SPA-GMN-PRTJYUN2MEIL (5)
           MOVE    "07"                TO  SPA-GMN-PRTJYUN2L    (6)
           MOVE    "カナ氏名順（全体）"
                                       TO  SPA-GMN-PRTJYUN2MEIL (6)
           MOVE    "08"                TO  SPA-GMN-PRTJYUN2L    (7)
           MOVE    "患者番号順（全体）"
                                       TO  SPA-GMN-PRTJYUN2MEIL (7)
           MOVE    "09"                TO  SPA-GMN-PRTJYUN2L    (8)
           MOVE    "種別・カナ氏名順"  TO  SPA-GMN-PRTJYUN2MEIL (8)
           MOVE    "10"                TO  SPA-GMN-PRTJYUN2L    (9)
           MOVE    "種別・患者番号順"  TO  SPA-GMN-PRTJYUN2MEIL (9)
           MOVE    9                   TO  SPA-GMN-PRTJYUN2-MAX 
      *
           MOVE    "07"                TO  SPA-GMN-PRTJYUN4L    (1)
           MOVE    "カナ氏名順（全体）"
                                       TO  SPA-GMN-PRTJYUN4MEIL (1)
           MOVE    "08"                TO  SPA-GMN-PRTJYUN4L    (2)
           MOVE    "患者番号順（全体）"
                                       TO  SPA-GMN-PRTJYUN4MEIL (2)
           MOVE    "11"                TO  SPA-GMN-PRTJYUN4L    (3)
           MOVE    "医師別順"          TO  SPA-GMN-PRTJYUN4MEIL (3)
           MOVE    "13"                TO  SPA-GMN-PRTJYUN4L    (4)
           MOVE    "ターミナルデジット"
                                       TO  SPA-GMN-PRTJYUN4MEIL (4)
           MOVE    4                   TO  SPA-GMN-PRTJYUN4-MAX 
      *
           MOVE    "01"                TO  SPA-GMN-PRTJYUN5L    (1)
           MOVE    "種別・保険者番号・カナ氏名順"
                                       TO  SPA-GMN-PRTJYUN5MEIL (1)
           MOVE    "02"                TO  SPA-GMN-PRTJYUN5L    (2)
           MOVE    "種別・保険者番号・患者番号順"
                                       TO  SPA-GMN-PRTJYUN5MEIL (2)
           MOVE    "03"                TO  SPA-GMN-PRTJYUN5L    (3)
           MOVE    "カナ氏名順"        TO  SPA-GMN-PRTJYUN5MEIL (3)
           MOVE    "04"                TO  SPA-GMN-PRTJYUN5L    (4)
           MOVE    "患者番号順"        TO  SPA-GMN-PRTJYUN5MEIL (4)
           MOVE    "09"                TO  SPA-GMN-PRTJYUN5L    (5)
           MOVE    "種別・カナ氏名順"  TO  SPA-GMN-PRTJYUN5MEIL (5)
           MOVE    "10"                TO  SPA-GMN-PRTJYUN5L    (6)
           MOVE    "種別・患者番号順"  TO  SPA-GMN-PRTJYUN5MEIL (6)
           MOVE    6                   TO  SPA-GMN-PRTJYUN5-MAX 
      *
           MOVE    "07"                TO  SPA-GMN-PRTJYUN6L    (1)
           MOVE    "カナ氏名順（全体）"
                                       TO  SPA-GMN-PRTJYUN6MEIL (1)
           MOVE    "08"                TO  SPA-GMN-PRTJYUN6L    (2)
           MOVE    "患者番号順（全体）"
                                       TO  SPA-GMN-PRTJYUN6MEIL (2)
           MOVE    "11"                TO  SPA-GMN-PRTJYUN6L    (3)
           MOVE    "医師別順"          TO  SPA-GMN-PRTJYUN6MEIL (3)
           MOVE    "12"                TO  SPA-GMN-PRTJYUN6L    (4)
           MOVE    "病棟別順"          TO  SPA-GMN-PRTJYUN6MEIL (4)
           MOVE    "13"                TO  SPA-GMN-PRTJYUN6L    (5)
           MOVE    "ターミナルデジット"
                                       TO  SPA-GMN-PRTJYUN6MEIL (5)
           MOVE    5                   TO  SPA-GMN-PRTJYUN6-MAX 
      *
           MOVE    "0"                 TO  SPA-GMN-AUTODSPKBNL    (1)
           MOVE    "自動遷移しない"    TO  SPA-GMN-AUTODSPKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-AUTODSPKBNL    (2)
           MOVE    "自動遷移する"      TO  SPA-GMN-AUTODSPKBNMEIL (2)
           MOVE    2                   TO  SPA-GMN-AUTODSPKBN-MAX 
      *
           MOVE    "0"                 TO  SPA-GMN-PRT-CHECKKBNL    (1)
           MOVE    "チェックを解除しない"
                                       TO  SPA-GMN-PRT-CHECKKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-PRT-CHECKKBNL    (2)
           MOVE    "チェックを解除する"
                                       TO  SPA-GMN-PRT-CHECKKBNMEIL (2)
           MOVE    2                   TO  SPA-GMN-PRT-CHECKKBN-MAX 
      *
           MOVE    "0"                 TO  SPA-GMN-JIHI-PRTKBNL    (1)
           MOVE    "自費レセプトを印刷する"
                                       TO  SPA-GMN-JIHI-PRTKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-JIHI-PRTKBNL    (2)
           MOVE    "自費レセプトを印刷しない"
                                       TO  SPA-GMN-JIHI-PRTKBNMEIL (2)
           MOVE    2                   TO  SPA-GMN-JIHI-PRTKBN-MAX 
      *
           MOVE    "0"                 TO  SPA-GMN-MOJI-KISAIKBNL    (1)
           MOVE    "レセプト上部中央"  TO  SPA-GMN-MOJI-KISAIKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-MOJI-KISAIKBNL    (2)
           MOVE    "レセプト上部右"    TO  SPA-GMN-MOJI-KISAIKBNMEIL (2)
           MOVE    2                   TO  SPA-GMN-MOJI-KISAIKBN-MAX 
      *
           MOVE    "0"             TO  SPA-GMN-KYURATE-KISAIKBNL    (1)
           MOVE    "１０割"        TO  SPA-GMN-KYURATE-KISAIKBNMEIL (1)
           MOVE    "1"             TO  SPA-GMN-KYURATE-KISAIKBNL    (2)
           MOVE    "０割"          TO  SPA-GMN-KYURATE-KISAIKBNMEIL (2)
           MOVE    "2"             TO  SPA-GMN-KYURATE-KISAIKBNL    (3)
           MOVE    "記載なし"      TO  SPA-GMN-KYURATE-KISAIKBNMEIL (3)
           MOVE    "3"             TO  SPA-GMN-KYURATE-KISAIKBNL    (4)
           MOVE    "通常の給付割合を記載"
                                   TO  SPA-GMN-KYURATE-KISAIKBNMEIL (4)
           MOVE    4               TO  SPA-GMN-KYURATE-KISAIKBN-MAX 
      *
           MOVE    "0"             TO
                                   SPA-GMN-RECESYUBET-KISAIKBNL    (1)
           MOVE    "通常のレセプト種別を記載"
                                   TO
                                   SPA-GMN-RECESYUBET-KISAIKBNMEIL (1)
           MOVE    "1"             TO
                                   SPA-GMN-RECESYUBET-KISAIKBNL    (2)
           MOVE    "記載なし"      TO
                                   SPA-GMN-RECESYUBET-KISAIKBNMEIL (2)
           MOVE    2               TO  SPA-GMN-RECESYUBET-KISAIKBN-MAX 
      *
           MOVE    "1"             TO  SPA-GMN-PREFKBN-PRTJYUNL    (1)
           MOVE    "県内・県外"    TO  SPA-GMN-PREFKBN-PRTJYUNMEIL (1)
           MOVE    "2"             TO  SPA-GMN-PREFKBN-PRTJYUNL    (2)
           MOVE    "県外・県内"    TO  SPA-GMN-PREFKBN-PRTJYUNMEIL (2)
           MOVE    2               TO  SPA-GMN-PREFKBN-PRTJYUN-MAX 
      *
           MOVE    "1"             TO  SPA-GMN-HKNJANUM-PRTJYUNL    (1)
           MOVE    "市町村国保・国保組合・県内扱いの県外国保組合"
                                   TO  SPA-GMN-HKNJANUM-PRTJYUNMEIL (1)
           MOVE    "2"             TO  SPA-GMN-HKNJANUM-PRTJYUNL    (2)
           MOVE    "市町村国保・県内扱いの県外国保組合・国保組合"
                                   TO  SPA-GMN-HKNJANUM-PRTJYUNMEIL (2)
           MOVE    "3"             TO  SPA-GMN-HKNJANUM-PRTJYUNL    (3)
           MOVE    "国保組合・市町村国保・県内扱いの県外国保組合"
                                   TO  SPA-GMN-HKNJANUM-PRTJYUNMEIL (3)
           MOVE    "4"             TO  SPA-GMN-HKNJANUM-PRTJYUNL    (4)
           MOVE    "国保組合・県内扱いの県外国保組合・市町村国保"
                                   TO  SPA-GMN-HKNJANUM-PRTJYUNMEIL (4)
           MOVE    "5"             TO  SPA-GMN-HKNJANUM-PRTJYUNL    (5)
           MOVE    "県内扱いの県外国保組合・市町村国保・国保組合"
                                   TO  SPA-GMN-HKNJANUM-PRTJYUNMEIL (5)
           MOVE    "6"             TO  SPA-GMN-HKNJANUM-PRTJYUNL    (6)
           MOVE
               "県内扱いの県外国保組合・国保組合・市町村国保"
                                   TO  SPA-GMN-HKNJANUM-PRTJYUNMEIL (6)
           MOVE    6               TO  SPA-GMN-HKNJANUM-PRTJYUN-MAX 
      *
           MOVE    "1"             TO
                                   SPA-GMN-RECESYUBETU-PRTJYUNL    (1)
           MOVE    "国保一般・老人保健・退職国保"
                                   TO
                                   SPA-GMN-RECESYUBETU-PRTJYUNMEIL (1)
           MOVE    "2"             TO
                                   SPA-GMN-RECESYUBETU-PRTJYUNL    (2)
           MOVE    "国保一般・退職国保・老人保健"      TO
                                   SPA-GMN-RECESYUBETU-PRTJYUNMEIL (2)
           MOVE    "3"             TO
                                   SPA-GMN-RECESYUBETU-PRTJYUNL    (3)
           MOVE    "老人保健・国保一般・退職国保"
                                   TO
                                   SPA-GMN-RECESYUBETU-PRTJYUNMEIL (3)
           MOVE    "4"             TO
                                   SPA-GMN-RECESYUBETU-PRTJYUNL    (4)
           MOVE    "老人保健・退職国保・国保一般"      TO
                                   SPA-GMN-RECESYUBETU-PRTJYUNMEIL (4)
           MOVE    "5"             TO
                                   SPA-GMN-RECESYUBETU-PRTJYUNL    (5)
           MOVE    "退職国保・国保一般・老人保健"
                                   TO
                                   SPA-GMN-RECESYUBETU-PRTJYUNMEIL (5)
           MOVE    "6"             TO
                                   SPA-GMN-RECESYUBETU-PRTJYUNL    (6)
           MOVE    "退職国保・老人保健・国保一般"      TO
                                   SPA-GMN-RECESYUBETU-PRTJYUNMEIL (6)
           MOVE    6               TO  SPA-GMN-RECESYUBETU-PRTJYUN-MAX 
      *
           MOVE    "1"             TO  SPA-GMN-ZAITAKUL    (1)
           MOVE    "県内・県外の先頭に印刷" 
                                   TO  SPA-GMN-ZAITAKUMEIL (1)
           MOVE    "2"             TO  SPA-GMN-ZAITAKUL    (2)
           MOVE    "保険者の先頭に印刷"
                                   TO  SPA-GMN-ZAITAKUMEIL (2)
           MOVE    2               TO  SPA-GMN-ZAITAKU-MAX 
      *
           MOVE    "1"             TO  SPA-GMN-CHOUKIL    (1)
           MOVE    "県内・県外の先頭に印刷" 
                                   TO  SPA-GMN-CHOUKIMEIL (1)
           MOVE    "2"             TO  SPA-GMN-CHOUKIL    (2)
           MOVE    "保険者の先頭に印刷"
                                   TO  SPA-GMN-CHOUKIMEIL (2)
           MOVE    2               TO  SPA-GMN-CHOUKI-MAX 
      *
           MOVE    "1"             TO  SPA-GMN-HKNJANUM-PRTKBNL    (1)
           MOVE    "市町村国保の先頭に印刷" 
                                   TO  SPA-GMN-HKNJANUM-PRTKBNMEIL (1)
           MOVE    "2"             TO  SPA-GMN-HKNJANUM-PRTKBNL    (2)
           MOVE    "市町村国保の最後に印刷"
                                   TO  SPA-GMN-HKNJANUM-PRTKBNMEIL (2)
           MOVE    2               TO  SPA-GMN-HKNJANUM-PRTKBN-MAX
      *
           MOVE    "0"             TO  SPA-GMN-DRCD-PRTKBNL    (1)
           MOVE    "医師別出力をしない" 
                                   TO  SPA-GMN-DRCD-PRTKBNMEIL (1)
           MOVE    "1"             TO  SPA-GMN-DRCD-PRTKBNL    (2)
           MOVE    "医師別出力をする"
                                   TO  SPA-GMN-DRCD-PRTKBNMEIL (2)
           MOVE    2               TO  SPA-GMN-DRCD-PRTKBN-MAX 
      *
           MOVE    "1"             TO  SPA-GMN-DRCD-PRTJYUNKBNL   (1)
           MOVE    "カナ氏名順（全体）"
                                   TO  SPA-GMN-DRCD-PRTJYUNKBNMEIL(1)
           MOVE    "2"             TO  SPA-GMN-DRCD-PRTJYUNKBNL   (2)
           MOVE    "患者番号順（全体）"
                                   TO  SPA-GMN-DRCD-PRTJYUNKBNMEIL(2)
           MOVE    "3"             TO  SPA-GMN-DRCD-PRTJYUNKBNL   (3)
           MOVE    "提出先・カナ氏名順"
                                   TO  SPA-GMN-DRCD-PRTJYUNKBNMEIL(3)
           MOVE    "4"             TO  SPA-GMN-DRCD-PRTJYUNKBNL   (4)
           MOVE    "提出先・患者番号順"
                                   TO  SPA-GMN-DRCD-PRTJYUNKBNMEIL(4)
           MOVE    4               TO  SPA-GMN-DRCD-PRTJYUNKBN-MAX
      *
           MOVE    "0"             TO  SPA-GMN-BTUNUM-PRTKBNL    (1)
           MOVE    "病棟別出力をしない" 
                                   TO  SPA-GMN-BTUNUM-PRTKBNMEIL (1)
           MOVE    "1"             TO  SPA-GMN-BTUNUM-PRTKBNL    (2)
           MOVE    "病棟別出力をする"
                                   TO  SPA-GMN-BTUNUM-PRTKBNMEIL (2)
           MOVE    2               TO  SPA-GMN-BTUNUM-PRTKBN-MAX 
      *
           MOVE    "1"             TO  SPA-GMN-BTUNUM-PRTJYUNKBNL   (1)
           MOVE    "カナ氏名順（全体）"
                                   TO  SPA-GMN-BTUNUM-PRTJYUNKBNMEIL(1)
           MOVE    "2"             TO  SPA-GMN-BTUNUM-PRTJYUNKBNL   (2)
           MOVE    "患者番号順（全体）"
                                   TO  SPA-GMN-BTUNUM-PRTJYUNKBNMEIL(2)
           MOVE    2               TO  SPA-GMN-BTUNUM-PRTJYUNKBN-MAX
      *
           MOVE    "0"             TO  SPA-GMN-TERMINAL-DIG-PRTKBNL
                                                                   (1)
           MOVE    "ターミナルデジットによる出力をしない"
                                   TO  SPA-GMN-TERMINAL-DIG-PRTKBNMEIL
                                                                   (1)
           MOVE    "1"             TO  SPA-GMN-TERMINAL-DIG-PRTKBNL
                                                                   (2)
           MOVE    "ターミナルデジットによる出力をする"
                                   TO  SPA-GMN-TERMINAL-DIG-PRTKBNMEIL
                                                                   (2)
           MOVE    2               TO  SPA-GMN-TERMINAL-DIG-PRTKBN-MAX
      *
           MOVE    "0"             TO  SPA-GMN-SYUKA-KBNL      (1)
           MOVE    "しない"        TO  SPA-GMN-SYUKA-KBNMEIL   (1)
           MOVE    "1"             TO  SPA-GMN-SYUKA-KBNL      (2)
           MOVE    "する"          TO  SPA-GMN-SYUKA-KBNMEIL   (2)
           MOVE    2               TO  SPA-GMN-SYUKA-KBN-MAX 
      *
           MOVE    "1"             TO  SPA-GMN-SYUKA-NYUINL    (1)
           MOVE    "当月の入院日数" 
                                   TO  SPA-GMN-SYUKA-NYUINMEIL (1)
           MOVE    "2"             TO  SPA-GMN-SYUKA-NYUINL    (2)
           MOVE    "当月の請求点数"
                                   TO  SPA-GMN-SYUKA-NYUINMEIL (2)
           MOVE    "3"             TO  SPA-GMN-SYUKA-NYUINL    (3)
           MOVE    "当月の最終入院科"
                                   TO  SPA-GMN-SYUKA-NYUINMEIL (3)
           MOVE    3               TO  SPA-GMN-SYUKA-NYUIN-MAX 
      *
           MOVE    "1"             TO  SPA-GMN-SYUKA-GAIRAIL   (1)
           MOVE    "当月の受診回数" 
                                   TO  SPA-GMN-SYUKA-GAIRAIMEIL(1)
           MOVE    "2"             TO  SPA-GMN-SYUKA-GAIRAIL   (2)
           MOVE    "当月の請求点数"
                                   TO  SPA-GMN-SYUKA-GAIRAIMEIL(2)
           MOVE    2               TO  SPA-GMN-SYUKA-GAIRAI-MAX 
      *
           MOVE    "0"             TO  SPA-GMN-SYUKA-SELECTL     (1)
           MOVE    "主科を設定しない"
                                   TO  SPA-GMN-SYUKA-SELECTMEIL  (1)
           MOVE    "1"             TO  SPA-GMN-SYUKA-SELECTL     (2)
           MOVE    "診療科コードの小さい科で設定"
                                   TO  SPA-GMN-SYUKA-SELECTMEIL  (2)
           MOVE    "2"             TO  SPA-GMN-SYUKA-SELECTL     (3)
           MOVE    "診療科コードの大きい科で設定"
                                   TO  SPA-GMN-SYUKA-SELECTMEIL  (3)
           MOVE    3               TO  SPA-GMN-SYUKA-SELECT-MAX 
      *
           MOVE    "1"             TO  SPA-GMN-SYUKA-MODEL     (1)
           MOVE    "オンライン"    TO  SPA-GMN-SYUKA-MODEMEIL  (1)
           MOVE    "2"             TO  SPA-GMN-SYUKA-MODEL     (2)
           MOVE    "一括バッチ"    TO  SPA-GMN-SYUKA-MODEMEIL  (2)
           MOVE    2               TO  SPA-GMN-SYUKA-MODE-MAX 
      *
           MOVE    "0"             TO  SPA-GMN-SYUKA-SYAHOL     (1)
           MOVE    "しない"        TO  SPA-GMN-SYUKA-SYAHOMEIL  (1)
           MOVE    "1"             TO  SPA-GMN-SYUKA-SYAHOL     (2)
           MOVE    "主科"          TO  SPA-GMN-SYUKA-SYAHOMEIL  (2)
           MOVE    "2"             TO  SPA-GMN-SYUKA-SYAHOL     (3)
           MOVE    "科別印刷のみ"  TO  SPA-GMN-SYUKA-SYAHOMEIL  (3)
           MOVE    3               TO  SPA-GMN-SYUKA-SYAHO-MAX 
      *
           MOVE    "0"             TO  SPA-GMN-SYUKA-KOKUHOL    (1)
           MOVE    "しない"        TO  SPA-GMN-SYUKA-KOKUHOMEIL (1)
           MOVE    "1"             TO  SPA-GMN-SYUKA-KOKUHOL    (2)
           MOVE    "主科"          TO  SPA-GMN-SYUKA-KOKUHOMEIL (2)
           MOVE    "2"             TO  SPA-GMN-SYUKA-KOKUHOL    (3)
           MOVE    "科別印刷のみ"  TO  SPA-GMN-SYUKA-KOKUHOMEIL (3)
           MOVE    3               TO  SPA-GMN-SYUKA-KOKUHO-MAX 
      *
           MOVE    "0"             TO  SPA-GMN-SYUKA-KOUIKIL    (1)
           MOVE    "しない"        TO  SPA-GMN-SYUKA-KOUIKIMEIL (1)
           MOVE    "1"             TO  SPA-GMN-SYUKA-KOUIKIL    (2)
           MOVE    "主科"          TO  SPA-GMN-SYUKA-KOUIKIMEIL (2)
           MOVE    "2"             TO  SPA-GMN-SYUKA-KOUIKIL    (3)
           MOVE    "科別印刷のみ"  TO  SPA-GMN-SYUKA-KOUIKIMEIL (3)
           MOVE    3               TO  SPA-GMN-SYUKA-KOUIKI-MAX 
      *
           MOVE    "0"             TO  SPA-GMN-FRAME-ROUPRTKBNL    (1)
           MOVE    "枠を記載しない"
                                   TO  SPA-GMN-FRAME-ROUPRTKBNMEIL (1)
           MOVE    "1"             TO  SPA-GMN-FRAME-ROUPRTKBNL    (2)
           MOVE    "枠を記載する"  TO  SPA-GMN-FRAME-ROUPRTKBNMEIL (2)
           MOVE    2               TO  SPA-GMN-FRAME-ROUPRTKBN-MAX 
      *
           MOVE    "0"             TO  SPA-GMN-FRAME-ROUPRTKBN-NL  (1)
           MOVE    "枠を記載しない"
                                   TO
                                   SPA-GMN-FRAME-ROUPRTKBN-NMEIL   (1)
           MOVE    "1"             TO  SPA-GMN-FRAME-ROUPRTKBN-NL  (2)
           MOVE    "枠を記載する"  TO
                                   SPA-GMN-FRAME-ROUPRTKBN-NMEIL   (2)
           MOVE    2               TO  SPA-GMN-FRAME-ROUPRTKBN-N-MAX 
      *
           MOVE    "0"             TO  SPA-GMN-COMMENT-ROUL    (1)
           MOVE    "傷病の経過欄に記載"
                                   TO  SPA-GMN-COMMENT-ROUMEIL (1)
           MOVE    "1"             TO  SPA-GMN-COMMENT-ROUL    (2)
           MOVE    "摘要欄に記載"  TO  SPA-GMN-COMMENT-ROUMEIL (2)
           MOVE    2               TO  SPA-GMN-COMMENT-ROU-MAX 
      *
           MOVE    "0"             TO  SPA-GMN-RSI-RECEIPTKBNL    (1)
           MOVE    "旧様式"        TO  SPA-GMN-RSI-RECEIPTKBNMEIL (1)
           MOVE    "1"             TO  SPA-GMN-RSI-RECEIPTKBNL    (2)
           MOVE    "新様式"        TO  SPA-GMN-RSI-RECEIPTKBNMEIL (2)
           MOVE    2               TO  SPA-GMN-RSI-RECEIPTKBN-MAX 
      *
           MOVE    "0"             TO  SPA-GMN-ROUPRTKBNL    (1)
           MOVE    "ＯＣＲ専用様式"
                                   TO  SPA-GMN-ROUPRTKBNMEIL (1)
      *****MOVE    "1"             TO  SPA-GMN-ROUPRTKBNL    (2)
      *****MOVE    "確認様式"      TO  SPA-GMN-ROUPRTKBNMEIL (2)
           MOVE    1               TO  SPA-GMN-ROUPRTKBN-MAX 
      *
           MOVE    "0"             TO  SPA-GMN-ROUPRTKBN-NL      (1)
           MOVE    "ＯＣＲ専用様式"
                                   TO  SPA-GMN-ROUPRTKBN-NMEIL   (1)
      *****MOVE    "1"             TO  SPA-GMN-ROUPRTKBN-NL      (2)
      *****MOVE    "確認様式"      TO  SPA-GMN-ROUPRTKBN-NMEIL   (2)
           MOVE    1               TO  SPA-GMN-ROUPRTKBN-N-MAX 
      *
           MOVE    "0"             TO  SPA-GMN-FRAME-JIBAIPRTKBNL    (1)
           MOVE    "枠を記載しない"
                                   TO  SPA-GMN-FRAME-JIBAIPRTKBNMEIL (1)
           MOVE    "1"             TO  SPA-GMN-FRAME-JIBAIPRTKBNL    (2)
           MOVE    "枠を記載する"  TO  SPA-GMN-FRAME-JIBAIPRTKBNMEIL (2)
           MOVE    2               TO  SPA-GMN-FRAME-JIBAIPRTKBN-MAX 
      *
           MOVE    "0"             TO  SPA-GMN-FRAME-JIBAIPRTKBN-NL  (1)
           MOVE    "枠を記載しない"
                                   TO
                                   SPA-GMN-FRAME-JIBAIPRTKBN-NMEIL   (1)
           MOVE    "1"             TO  SPA-GMN-FRAME-JIBAIPRTKBN-NL  (2)
           MOVE    "枠を記載する"  TO
                                   SPA-GMN-FRAME-JIBAIPRTKBN-NMEIL   (2)
           MOVE    2               TO  SPA-GMN-FRAME-JIBAIPRTKBN-N-MAX 
      *
      *****MOVE    "0"             TO  SPA-GMN-JIBAIPRTKBNL    (1)
      *****MOVE    "旧様式"        TO  SPA-GMN-JIBAIPRTKBNMEIL (1)
           MOVE    "1"             TO  SPA-GMN-JIBAIPRTKBNL    (1)
           MOVE    "平成１９年４月改正様式"
                                   TO  SPA-GMN-JIBAIPRTKBNMEIL (1)
           MOVE    1               TO  SPA-GMN-JIBAIPRTKBN-MAX 
      *
      *****MOVE    "0"             TO  SPA-GMN-JIBAIPRTKBN-NL      (1)
      *****MOVE    "旧様式"        TO  SPA-GMN-JIBAIPRTKBN-NMEIL   (1)
           MOVE    "1"             TO  SPA-GMN-JIBAIPRTKBN-NL      (1)
           MOVE    "平成１９年４月改正様式"
                                   TO  SPA-GMN-JIBAIPRTKBN-NMEIL   (1)
           MOVE    1               TO  SPA-GMN-JIBAIPRTKBN-N-MAX 
      *
      *****MOVE    "0"             TO  SPA-GMN-JIBAIPRTKBN1L    (1)
      *****MOVE    "旧様式"        TO  SPA-GMN-JIBAIPRTKBN1MEIL (1)
           MOVE    "1"             TO  SPA-GMN-JIBAIPRTKBN1L    (1)
           MOVE    "平成１９年４月改正様式"
                                   TO  SPA-GMN-JIBAIPRTKBN1MEIL (1)
           MOVE    "2"             TO  SPA-GMN-JIBAIPRTKBN1L    (2)
           MOVE    "ＯＣＲ様式"
                                   TO  SPA-GMN-JIBAIPRTKBN1MEIL (2)
           MOVE    2               TO  SPA-GMN-JIBAIPRTKBN1-MAX 
      *
      *****MOVE    "0"             TO  SPA-GMN-JIBAIPRTKBN1-NL      (1)
      *****MOVE    "旧様式"        TO  SPA-GMN-JIBAIPRTKBN1-NMEIL   (1)
           MOVE    "1"             TO  SPA-GMN-JIBAIPRTKBN1-NL      (1)
           MOVE    "平成１９年４月改正様式"
                                   TO  SPA-GMN-JIBAIPRTKBN1-NMEIL   (1)
           MOVE    "2"             TO  SPA-GMN-JIBAIPRTKBN1-NL      (2)
           MOVE    "ＯＣＲ様式"
                                   TO  SPA-GMN-JIBAIPRTKBN1-NMEIL   (2)
           MOVE    2               TO  SPA-GMN-JIBAIPRTKBN1-N-MAX 
      *
           MOVE    "0"             TO  SPA-GMN-JIBAI-FRACTION-KBN1L  (1)
           MOVE    "１０円未満端数処理なし"
                                   TO  SPA-GMN-JIBAI-FRACTION-KBN1MEIL
                                                                     (1)
           MOVE    "1"             TO  SPA-GMN-JIBAI-FRACTION-KBN1L  (2)
           MOVE    "医療機関情報に準拠"
                                   TO  SPA-GMN-JIBAI-FRACTION-KBN1MEIL
                                                                     (2)
           MOVE    "2"             TO  SPA-GMN-JIBAI-FRACTION-KBN1L  (3)
           MOVE    "医療機関情報に準拠（患者請求分のみ）"
                                   TO  SPA-GMN-JIBAI-FRACTION-KBN1MEIL
                                                                     (3)
           MOVE    3               TO  SPA-GMN-JIBAI-FRACTION-KBN1-MAX 
      *
           MOVE    "0"             TO  SPA-GMN-JIBAI-FRACTION-KBN2L  (1)
           MOVE    "１０円未満端数処理なし"
                                   TO  SPA-GMN-JIBAI-FRACTION-KBN2MEIL
                                                                     (1)
           MOVE    "1"             TO  SPA-GMN-JIBAI-FRACTION-KBN2L  (2)
           MOVE    "医療機関情報に準拠"
                                   TO  SPA-GMN-JIBAI-FRACTION-KBN2MEIL
                                                                     (2)
           MOVE    "2"             TO  SPA-GMN-JIBAI-FRACTION-KBN2L  (3)
           MOVE    "医療機関情報に準拠（患者請求分のみ）"
                                   TO  SPA-GMN-JIBAI-FRACTION-KBN2MEIL
                                                                     (3)
           MOVE    3               TO  SPA-GMN-JIBAI-FRACTION-KBN2-MAX 
      *
           MOVE    "0"             TO  SPA-GMN-JIBAI-PDFL  (1)
           MOVE    "作成しない"    TO  SPA-GMN-JIBAI-PDFMEIL
                                                           (1)
           MOVE    "1"             TO  SPA-GMN-JIBAI-PDFL  (2)
           MOVE    "作成する"      TO  SPA-GMN-JIBAI-PDFMEIL
                                                           (2)
           MOVE    2               TO  SPA-GMN-JIBAI-PDF-MAX
      *     
           MOVE    "0"                 TO  SPA-GMN-HKN-RIREKIL     (1)
           MOVE    "個人情報を記載する"
                                       TO  SPA-GMN-HKN-RIREKIMEIL  (1)
           MOVE    "1"                 TO  SPA-GMN-HKN-RIREKIL     (2)
           MOVE    "個人情報を記載しない"
                                       TO  SPA-GMN-HKN-RIREKIMEIL  (2)
           MOVE    2                   TO  SPA-GMN-HKN-RIREKI-MAX 
      *     
           MOVE    "0"                 TO  SPA-GMN-GENBUTU-ICHIBUL   (1)
           MOVE    "上限１円単位の１０円未満四捨五入なし"
                                       TO  SPA-GMN-GENBUTU-ICHIBUMEIL(1)
           MOVE    "1"                 TO  SPA-GMN-GENBUTU-ICHIBUL   (2)
           MOVE    "上限１円単位の１０円未満四捨五入あり"
                                       TO  SPA-GMN-GENBUTU-ICHIBUMEIL(2)
           MOVE    2                   TO  SPA-GMN-GENBUTU-ICHIBU-MAX 
      *
           MOVE    "0"                 TO  SPA-GMN-COMPRESSL   (1)
           MOVE    "用法コードの内容を記載しない"
                                       TO  SPA-GMN-COMPRESSMEIL(1)
           MOVE    "1"                 TO  SPA-GMN-COMPRESSL   (2)
           MOVE    "用法コードの内容を記載する"
                                       TO  SPA-GMN-COMPRESSMEIL(2)
           MOVE    2                   TO  SPA-GMN-COMPRESS-MAX 
      *
           MOVE    "0"                 TO  SPA-GMN-DISASTER-RECEIPTL (1)
           MOVE    "分割しない"        TO
                                       SPA-GMN-DISASTER-RECEIPTMEIL  (1)
           MOVE    "1"                 TO  SPA-GMN-DISASTER-RECEIPTL (2)
           MOVE    "分割する"          TO
                                       SPA-GMN-DISASTER-RECEIPTMEIL  (2)
           MOVE    2                   TO  SPA-GMN-DISASTER-RECEIPT-MAX 
      *
           MOVE    "0"                 TO  SPA-GMN-15-YKZL     (1)
           MOVE    "記載しない"        TO  SPA-GMN-15-YKZMEIL  (1)
           MOVE    "1"                 TO  SPA-GMN-15-YKZL     (2)
           MOVE    "記載する"          TO  SPA-GMN-15-YKZMEIL  (2)
           MOVE    2                   TO  SPA-GMN-15-YKZ-MAX 
      *
           MOVE    "0"                 TO  SPA-GMN-KOUHI-ICHIBU-KSIL (1)
           MOVE    "上限０円時は「０」を記載しない"
                                       TO
                                       SPA-GMN-KOUHI-ICHIBU-KSIMEIL  (1)
           MOVE    "1"                 TO  SPA-GMN-KOUHI-ICHIBU-KSIL (2)
           MOVE    "上限０円時は「０」を記載する"
                                       TO
                                       SPA-GMN-KOUHI-ICHIBU-KSIMEIL  (2)
           MOVE    2                   TO  SPA-GMN-KOUHI-ICHIBU-KSI-MAX 
      *
      *       医療機関情報
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-W01-SEL-EDYUKYMD
                                       TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-1001-REC
               MOVE    SYS-1001-PREFNUM        TO  SPA-1001-PREFNUM
               MOVE    SYS-1001-HOSPSBT1       TO  SPA-1001-HOSPSBT1 
           END-IF 
      *
      *    レセプト・総括編集情報
           MOVE    SPACE               TO  SYS-2005-REC
           INITIALIZE                      SYS-2005-REC
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    SPA-W01-SEL-STYUKYMD
                                       TO  SYS-2005-STYUKYMD
           MOVE    SPA-W01-SEL-EDYUKYMD
                                       TO  SYS-2005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"     TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-2005-REC
               IF      SYS-2005-NI-JYOKEN (1)  NOT NUMERIC
                   MOVE    WRK-CONS-JYOKEN-TENSU
                                               TO  SYS-2005-NI-JYOKEN(1)
               END-IF
               IF      SYS-2005-NI-JYOKEN (2)  NOT NUMERIC
                   MOVE    WRK-CONS-JYOKEN-TENSU
                                               TO  SYS-2005-NI-JYOKEN(2)
               END-IF
               IF      SYS-2005-NI-JYOKEN (3)   NOT NUMERIC
                   MOVE    WRK-CONS-JYOKEN-TENSU
                                               TO  SYS-2005-NI-JYOKEN(3)
               END-IF
               IF      SYS-2005-NI-JYOKEN (4)   NOT NUMERIC
                   MOVE    WRK-CONS-JYOKEN-TENSU
                                               TO  SYS-2005-NI-JYOKEN(4)
               END-IF
      *
               IF      SYS-2005-PRTJYUNKBN3    =   SPACE
                   MOVE    SYS-2005-PRTJYUNKBN1    TO
                                                   SYS-2005-PRTJYUNKBN3
               END-IF
      *         
               MOVE    SYS-2005-BYOMEIHENKBN   TO  SPA-GMN-BYOMEIHENKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-BYOMEIHENKBN-MAX
                   IF      SPA-GMN-BYOMEIHENKBN 
                                   =   SPA-GMN-BYOMEIHENKBNL (IDY)
                       MOVE    SPA-GMN-BYOMEIHENKBNLST (IDY)
                                           TO  SPA-GMN-BYOMEIHENKBN-G
                       MOVE    SPA-GMN-BYOMEIHENKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *         
               MOVE    SYS-2005-BYOMEIHENKBN1  TO  SPA-GMN-BYOMEIHENKBN1
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-BYOMEIHENKBN1-MAX
                   IF      SPA-GMN-BYOMEIHENKBN1 
                                   =   SPA-GMN-BYOMEIHENKBN1L (IDY)
                       MOVE    SPA-GMN-BYOMEIHENKBN1LST (IDY)
                                           TO  SPA-GMN-BYOMEIHENKBN1-G
                       MOVE    SPA-GMN-BYOMEIHENKBN1-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *         
               MOVE    SYS-2005-SYUBYOHENFLG  TO  SPA-GMN-SYUBYOHENFLG
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-SYUBYOHENFLG-MAX
                   IF      SPA-GMN-SYUBYOHENFLG 
                                   =   SPA-GMN-SYUBYOHENFLGL (IDY)
                       MOVE    SPA-GMN-SYUBYOHENFLGLST (IDY)
                                           TO  SPA-GMN-SYUBYOHENFLG-G
                       MOVE    SPA-GMN-SYUBYOHENFLG-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *         
               MOVE    SYS-2005-TENKIHENKBN    TO  SPA-GMN-TENKIHENKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-TENKIHENKBN-MAX
                   IF      SPA-GMN-TENKIHENKBN 
                                   =   SPA-GMN-TENKIHENKBNL (IDY)
                       MOVE    SPA-GMN-TENKIHENKBNLST (IDY)
                                           TO  SPA-GMN-TENKIHENKBN-G
                       MOVE    SPA-GMN-TENKIHENKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *
               MOVE    SYS-2005-KKHRECEKBN TO  SPA-GMN-KKHRECEKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-KKHRECEKBN-MAX
                   IF      SPA-GMN-KKHRECEKBN 
                                   =   SPA-GMN-KKHRECEKBNL (IDY)
                       MOVE    SPA-GMN-KKHRECEKBNLST (IDY)
                                           TO  SPA-GMN-KKHRECEKBN-G
                       MOVE    SPA-GMN-KKHRECEKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *         
               MOVE    SYS-2005-SYARECEKBN TO  SPA-GMN-SYARECEKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-SYARECEKBN-MAX
                   IF      SPA-GMN-SYARECEKBN 
                                   =   SPA-GMN-SYARECEKBNL (IDY)
                       MOVE    SPA-GMN-SYARECEKBNLST (IDY)
                                           TO  SPA-GMN-SYARECEKBN-G
                       MOVE    SPA-GMN-SYARECEKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *
               MOVE    SYS-2005-RECEDENOUT TO  SPA-GMN-RECEDENOUT
               IF      SPA-MW              =   SPA-GINBEE
                   MOVE    SPACE               TO  SPA-GMN-RECEDENOUT
               END-IF
      *         
               MOVE    SYS-2005-KKHOCRKBN  TO  SPA-GMN-KKHOCRKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-KKHOCRKBN-MAX
                   IF      SPA-GMN-KKHOCRKBN 
                                   =   SPA-GMN-KKHOCRKBNL (IDY)
                       MOVE    SPA-GMN-KKHOCRKBNLST (IDY)
                                           TO  SPA-GMN-KKHOCRKBN-G
                       MOVE    SPA-GMN-KKHOCRKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *         
               MOVE    SYS-2005-SYAOCRKBN  TO  SPA-GMN-SYAOCRKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-SYAOCRKBN-MAX
                   IF      SPA-GMN-SYAOCRKBN 
                                   =   SPA-GMN-SYAOCRKBNL (IDY)
                       MOVE    SPA-GMN-SYAOCRKBNLST (IDY)
                                           TO  SPA-GMN-SYAOCRKBN-G
                       MOVE    SPA-GMN-SYAOCRKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *         
               MOVE    SYS-2005-NYUTAIINREKI-KSIKBN
                                       TO  SPA-GMN-NYUTAIIN-KSIKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-NYUTAIIN-KSIKBN-MAX
                   IF      SPA-GMN-NYUTAIIN-KSIKBN 
                               =   SPA-GMN-NYUTAIIN-KSIKBNL (IDY)
                       MOVE    SPA-GMN-NYUTAIIN-KSIKBNLST (IDY)
                                       TO  SPA-GMN-NYUTAIIN-KSIKBN-G
                       MOVE    SPA-GMN-NYUTAIIN-KSIKBN-MAX
                                       TO  IDY
                   END-IF                                     
               END-PERFORM
      *
               MOVE    SYS-2005-PREFPRGID (1)
                                           TO  SPA-GMN-PREFPRGID
               MOVE    SYS-2005-PREFPRGID (2)
                                           TO  SPA-GMN-PREFPRGID1
      *         
               MOVE    SYS-2005-KKHPRTKBN  TO  SPA-GMN-KKHPRTKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-KKHPRTKBN-MAX
                   IF      SPA-GMN-KKHPRTKBN 
                                   =   SPA-GMN-KKHPRTKBNL (IDY)
                       MOVE    SPA-GMN-KKHPRTKBNLST (IDY)
                                           TO  SPA-GMN-KKHPRTKBN-G
                       MOVE    SPA-GMN-KKHPRTKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *
               MOVE    SYS-2005-MARUCHO-SYAPRTKBN  TO  SPA-GMN-SYAPRTKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-SYAPRTKBN-MAX
                   IF      SPA-GMN-SYAPRTKBN 
                                   =   SPA-GMN-SYAPRTKBNL (IDY)
                       MOVE    SPA-GMN-SYAPRTKBNLST (IDY)
                                           TO  SPA-GMN-SYAPRTKBN-G
                       MOVE    SPA-GMN-SYAPRTKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *
               MOVE    SYS-2005-SYAPRTNM   TO  SPA-GMN-SYAPRTNM         
               MOVE    SYS-2005-SYAPRTNM-NEXT
                                           TO  SPA-GMN-SYAPRTNM-NEXT
      *
               MOVE    SYS-2005-KKHPRTNM   TO  SPA-GMN-KKHPRTNM         
               MOVE    SYS-2005-KKHPRTNM-NEXT
                                           TO  SPA-GMN-KKHPRTNM-NEXT
               MOVE    SYS-2005-SYAPRTNM-N TO  SPA-GMN-SYAPRTNM-N
               MOVE    SYS-2005-SYAPRTNM-NEXT-N
                                           TO  SPA-GMN-SYAPRTNM-NEXT-N
      *
               MOVE    SYS-2005-KKHPRTNM-N TO  SPA-GMN-KKHPRTNM-N
               MOVE    SYS-2005-KKHPRTNM-NEXT-N
                                           TO  SPA-GMN-KKHPRTNM-NEXT-N
      *
               MOVE    SYS-2005-ROUPRTNM   TO  SPA-GMN-ROUPRTNM         
               MOVE    SYS-2005-ROUPRTNM-NEXT
                                           TO  SPA-GMN-ROUPRTNM-NEXT
      *
               MOVE    SYS-2005-JBAIPRTNM  TO  SPA-GMN-JBAIPRTNM         
               MOVE    SYS-2005-JBAIPRTNM-NEXT
                                           TO  SPA-GMN-JBAIPRTNM-NEXT
               MOVE    SYS-2005-ROUPRTNM-N TO  SPA-GMN-ROUPRTNM-N
               MOVE    SYS-2005-ROUPRTNM-NEXT-N
                                           TO  SPA-GMN-ROUPRTNM-NEXT-N
      *
               MOVE    SYS-2005-JBAIPRTNM-N
                                           TO  SPA-GMN-JBAIPRTNM-N
               MOVE    SYS-2005-JBAIPRTNM-NEXT-N
                                           TO  SPA-GMN-JBAIPRTNM-NEXT-N
      *         
               MOVE    SYS-2005-RENNUMHENKBN 
                                           TO  SPA-GMN-RENNUMHENKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-RENNUMHENKBN-MAX
                   IF      SPA-GMN-RENNUMHENKBN 
                                   =   SPA-GMN-RENNUMHENKBNL (IDY)
                       MOVE    SPA-GMN-RENNUMHENKBNLST (IDY)
                                           TO  SPA-GMN-RENNUMHENKBN-G
                       MOVE    SPA-GMN-RENNUMHENKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *         
               MOVE    SYS-2005-NEXTPRTKBN TO  SPA-GMN-NEXTPRTKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-NEXTPRTKBN-MAX
                   IF      SPA-GMN-NEXTPRTKBN 
                                   =   SPA-GMN-NEXTPRTKBNL (IDY)
                       MOVE    SPA-GMN-NEXTPRTKBNLST (IDY)
                                           TO  SPA-GMN-NEXTPRTKBN-G
                       MOVE    SPA-GMN-NEXTPRTKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *         
               MOVE    SYS-2005-NEXTPRTKBN-G 
                                           TO  SPA-GMN-NEXTPRTKBN1
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-NEXTPRTKBN1-MAX
                   IF      SPA-GMN-NEXTPRTKBN1 
                                   =   SPA-GMN-NEXTPRTKBN1L (IDY)
                       MOVE    SPA-GMN-NEXTPRTKBN1LST (IDY)
                                           TO  SPA-GMN-NEXTPRTKBN1-G
                       MOVE    SPA-GMN-NEXTPRTKBN1-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *
               MOVE    SYS-2005-RJNGNBKKBN TO  SPA-GMN-RJNGNBKKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-RJNGNBKKBN-MAX
                   IF      SPA-GMN-RJNGNBKKBN 
                                   =   SPA-GMN-RJNGNBKKBNL (IDY)
                       MOVE    SPA-GMN-RJNGNBKKBNLST (IDY)
                                           TO  SPA-GMN-RJNGNBKKBN-G
                       MOVE    SPA-GMN-RJNGNBKKBN-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2005-JIHITEKKSIKBN TO  SPA-GMN-JIHITEKKSIKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-JIHITEKKSIKBN-MAX
                   IF      SPA-GMN-JIHITEKKSIKBN 
                                   =   SPA-GMN-JIHITEKKSIKBNL (IDY)
                       MOVE    SPA-GMN-JIHITEKKSIKBNLST (IDY)
                                           TO  SPA-GMN-JIHITEKKSIKBN-G
                       MOVE    SPA-GMN-JIHITEKKSIKBN-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2005-RECECOMKBN TO  SPA-GMN-RECECOMKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-RECECOMKBN-MAX
                   IF      SPA-GMN-RECECOMKBN 
                                   =   SPA-GMN-RECECOMKBNL (IDY)
                       MOVE    SPA-GMN-RECECOMKBNLST (IDY)
                                           TO  SPA-GMN-RECECOMKBN-G
                       MOVE    SPA-GMN-RECECOMKBN-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2005-SRYKAHENKBN TO  SPA-GMN-SRYKAHENKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-SRYKAHENKBN-MAX
                   IF      SPA-GMN-SRYKAHENKBN 
                                   =   SPA-GMN-SRYKAHENKBNL (IDY)
                       MOVE    SPA-GMN-SRYKAHENKBNLST (IDY)
                                           TO  SPA-GMN-SRYKAHENKBN-G
                       MOVE    SPA-GMN-SRYKAHENKBN-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2005-AUTODSPKBN TO  SPA-GMN-AUTODSPKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-AUTODSPKBN-MAX
                   IF      SPA-GMN-AUTODSPKBN
                                   =   SPA-GMN-AUTODSPKBNL (IDY)
                       MOVE    SPA-GMN-AUTODSPKBNLST (IDY)
                                           TO  SPA-GMN-AUTODSPKBN-G
                       MOVE    SPA-GMN-AUTODSPKBN-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2005-PRTJYUNKBN1   TO  SPA-GMN-PRTJYUN1
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PRTJYUN1-MAX
                   IF      SPA-GMN-PRTJYUN1 
                                   =   SPA-GMN-PRTJYUN1L (IDY)
                       MOVE    SPA-GMN-PRTJYUN1LST (IDY)
                                           TO  SPA-GMN-PRTJYUN1-G
                       MOVE    SPA-GMN-PRTJYUN1-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2005-PRTJYUNKBN2   TO  SPA-GMN-PRTJYUN2
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PRTJYUN2-MAX
                   IF      SPA-GMN-PRTJYUN2
                                   =   SPA-GMN-PRTJYUN2L (IDY)
                       MOVE    SPA-GMN-PRTJYUN2LST (IDY)
                                           TO  SPA-GMN-PRTJYUN2-G
                       MOVE    SPA-GMN-PRTJYUN2-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2005-PRTJYUNKBN3    TO  SPA-GMN-PRTJYUN3
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PRTJYUN3-MAX
                   IF      SPA-GMN-PRTJYUN3 
                                   =   SPA-GMN-PRTJYUN3L (IDY)
                       MOVE    SPA-GMN-PRTJYUN3LST (IDY)
                                           TO  SPA-GMN-PRTJYUN3-G
                       MOVE    SPA-GMN-PRTJYUN3-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2005-PRTJYUNKBN4    TO  SPA-GMN-PRTJYUN4
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PRTJYUN4-MAX
                   IF      SPA-GMN-PRTJYUN4
                                   =   SPA-GMN-PRTJYUN4L (IDY)
                       MOVE    SPA-GMN-PRTJYUN4LST (IDY)
                                           TO  SPA-GMN-PRTJYUN4-G
                       MOVE    SPA-GMN-PRTJYUN4-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2005-MOJI-KISAIKBN  TO  SPA-GMN-MOJI-KISAIKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-MOJI-KISAIKBN-MAX
                   IF      SPA-GMN-MOJI-KISAIKBN
                                   =   SPA-GMN-MOJI-KISAIKBNL (IDY)
                       MOVE    SPA-GMN-MOJI-KISAIKBNLST (IDY)
                                           TO  SPA-GMN-MOJI-KISAIKBN-G
                       MOVE    SPA-GMN-MOJI-KISAIKBN-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2005-KYURATE-KISAIKBN
                                           TO  SPA-GMN-KYURATE-KISAIKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-KYURATE-KISAIKBN-MAX
                   IF      SPA-GMN-KYURATE-KISAIKBN
                                   =   SPA-GMN-KYURATE-KISAIKBNL (IDY)
                       MOVE    SPA-GMN-KYURATE-KISAIKBNLST (IDY)
                                           TO
                                           SPA-GMN-KYURATE-KISAIKBN-G
                       MOVE    SPA-GMN-KYURATE-KISAIKBN-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2005-RECESYUBETU-KISAIKBN
                                           TO
                                           SPA-GMN-RECESYUBET-KISAIKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-RECESYUBET-KISAIKBN-MAX
                   IF      SPA-GMN-RECESYUBET-KISAIKBN
                               =   SPA-GMN-RECESYUBET-KISAIKBNL (IDY)
                       MOVE    SPA-GMN-RECESYUBET-KISAIKBNLST (IDY)
                                           TO
                                       SPA-GMN-RECESYUBET-KISAIKBN-G
                       MOVE    SPA-GMN-RECESYUBET-KISAIKBN-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2005-NI-JYOKEN (1) TO
                                           SPA-GMN-NI-JYOKEN1
               MOVE    SYS-2005-NI-JYOKEN (2) TO
                                           SPA-GMN-NI-JYOKEN2
               MOVE    SYS-2005-NI-JYOKEN (3) TO
                                           SPA-GMN-NI-JYOKEN3
               MOVE    SYS-2005-NI-JYOKEN (4) TO
                                           SPA-GMN-NI-JYOKEN4
      *
               MOVE    SYS-2005-FRAME-JIBAIPRTKBN
                                           TO  SPA-GMN-FRAME-JIBAIPRTKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-FRAME-JIBAIPRTKBN-MAX
                   IF      SPA-GMN-FRAME-JIBAIPRTKBN
                               =   SPA-GMN-FRAME-JIBAIPRTKBNL (IDY)
                       MOVE    SPA-GMN-FRAME-JIBAIPRTKBNLST (IDY)
                                           TO
                                           SPA-GMN-FRAME-JIBAIPRTKBN-G
                       MOVE    SPA-GMN-FRAME-JIBAIPRTKBN-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2005-FRAME-JIBAIPRTKBN-N
                                           TO
                                           SPA-GMN-FRAME-JIBAIPRTKBN-N
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-FRAME-JIBAIPRTKBN-N-MAX
                   IF      SPA-GMN-FRAME-JIBAIPRTKBN-N
                               =   SPA-GMN-FRAME-JIBAIPRTKBN-NL (IDY)
                       MOVE    SPA-GMN-FRAME-JIBAIPRTKBN-NLST (IDY)
                                           TO
                                           SPA-GMN-FRAME-JIBAIPRTKBN-N-G
                       MOVE    SPA-GMN-FRAME-JIBAIPRTKBN-N-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
      *    レセプト・総括編集情報（区分コード０１のデータ読み込み）
           MOVE    SPACE               TO  SYS-25-REC
           INITIALIZE                      SYS-25-REC
           MOVE    "2005"              TO  SYS-25-KANRICD
           MOVE    "01"                TO  SYS-25-KBNCD
           MOVE    SPA-W01-SEL-STYUKYMD
                                       TO  SYS-25-STYUKYMD
           MOVE    SPA-W01-SEL-EDYUKYMD
                                       TO  SYS-25-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-25-HOSPNUM
           MOVE    SYS-25-REC          TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-25-REC
           ELSE
               INITIALIZE                      SYS-25-REC
               MOVE    SYS-2005-TERMID (1:4)
                                           TO  SYS-25-TBL     (1:4)
               MOVE    "1"                 TO  SYS-25-KOUIKIKBN
           END-IF
      *
           MOVE    SYS-25-RECEDEN-DATAKBN
                                       TO  SPA-200501-RECEDEN-DATAKBN
      *
           IF      SYS-25-PRTJYUNKBN5       =   SPACE
               MOVE    SYS-2005-PRTJYUNKBN1 TO  SYS-25-PRTJYUNKBN5
           END-IF
      *
           IF     SYS-25-ROUSAIPRTKBN  =   SPACE   OR  "1"
               MOVE    "0"             TO  SYS-25-ROUSAIPRTKBN
           END-IF
           IF     SYS-25-FRAME-ROUSAIPRTKBN
                                       =   SPACE
               MOVE    "0"             TO  SYS-25-FRAME-ROUSAIPRTKBN
           END-IF
           IF     SYS-25-RSI-RECEIPT-KBN   =   SPACE
               MOVE    "0"             TO  SYS-25-RSI-RECEIPT-KBN
           END-IF
           IF     SYS-25-RSI-INVOICE-KBN    =   SPACE
               MOVE    "0"             TO  SYS-25-RSI-INVOICE-KBN
           END-IF
      *
           MOVE    SYS-25-JIBAIPRTKBN  TO  SPA-GMN-JIBAIPRTKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >   SPA-GMN-JIBAIPRTKBN-MAX
               IF      SPA-GMN-JIBAIPRTKBN
                               =   SPA-GMN-JIBAIPRTKBNL (IDY)
                   MOVE    SPA-GMN-JIBAIPRTKBNLST (IDY)
                                       TO  SPA-GMN-JIBAIPRTKBN-G
                   MOVE    SPA-GMN-JIBAIPRTKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-JIBAIPRTKBN-N
                                       TO  SPA-GMN-JIBAIPRTKBN-N
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >   SPA-GMN-JIBAIPRTKBN-N-MAX
               IF      SPA-GMN-JIBAIPRTKBN-N
                               =   SPA-GMN-JIBAIPRTKBN-NL (IDY)
                   MOVE    SPA-GMN-JIBAIPRTKBN-NLST (IDY)
                                       TO  SPA-GMN-JIBAIPRTKBN-N-G
                   MOVE    SPA-GMN-JIBAIPRTKBN-N-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-JIBAIPRTKBN1
                                       TO  SPA-GMN-JIBAIPRTKBN1
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >   SPA-GMN-JIBAIPRTKBN1-MAX
               IF      SPA-GMN-JIBAIPRTKBN1
                               =   SPA-GMN-JIBAIPRTKBN1L (IDY)
                   MOVE    SPA-GMN-JIBAIPRTKBN1LST (IDY)
                                       TO  SPA-GMN-JIBAIPRTKBN1-G
                   MOVE    SPA-GMN-JIBAIPRTKBN1-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-JIBAIPRTKBN1-N
                                       TO  SPA-GMN-JIBAIPRTKBN1-N
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >   SPA-GMN-JIBAIPRTKBN1-N-MAX
               IF      SPA-GMN-JIBAIPRTKBN1-N
                               =   SPA-GMN-JIBAIPRTKBN1-NL (IDY)
                   MOVE    SPA-GMN-JIBAIPRTKBN1-NLST (IDY)
                                       TO  SPA-GMN-JIBAIPRTKBN1-N-G
                   MOVE    SPA-GMN-JIBAIPRTKBN1-N-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-JIBAI-FRACTION-KBN     (1)
                                       TO  SPA-GMN-JIBAI-FRACTION-KBN1
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >   SPA-GMN-JIBAI-FRACTION-KBN1-MAX
               IF      SPA-GMN-JIBAI-FRACTION-KBN1
                               =   SPA-GMN-JIBAI-FRACTION-KBN1L (IDY)
                   MOVE    SPA-GMN-JIBAI-FRACTION-KBN1LST (IDY)
                                       TO  SPA-GMN-JIBAI-FRACTION-KBN1-G
                   MOVE    SPA-GMN-JIBAI-FRACTION-KBN1-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-JIBAI-FRACTION-KBN     (2)
                                       TO  SPA-GMN-JIBAI-FRACTION-KBN2
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >   SPA-GMN-JIBAI-FRACTION-KBN2-MAX
               IF      SPA-GMN-JIBAI-FRACTION-KBN2
                               =   SPA-GMN-JIBAI-FRACTION-KBN2L (IDY)
                   MOVE    SPA-GMN-JIBAI-FRACTION-KBN2LST (IDY)
                                       TO  SPA-GMN-JIBAI-FRACTION-KBN2-G
                   MOVE    SPA-GMN-JIBAI-FRACTION-KBN2-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-JIBAI-PDF
                                       TO  SPA-GMN-JIBAI-PDF
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >   SPA-GMN-JIBAI-PDF-MAX
               IF      SPA-GMN-JIBAI-PDF
                               =   SPA-GMN-JIBAI-PDFL (IDY)
                   MOVE    SPA-GMN-JIBAI-PDFLST (IDY)
                                       TO  SPA-GMN-JIBAI-PDF-G
                   MOVE    SPA-GMN-JIBAI-PDF-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-BTUNUM-KSIKBN
                                       TO  SPA-GMN-BTUNUM-KSIKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-BTUNUM-KSIKBN-MAX
               IF      SPA-GMN-BTUNUM-KSIKBN 
                               =   SPA-GMN-BTUNUM-KSIKBNL (IDY)
                   MOVE    SPA-GMN-BTUNUM-KSIKBNLST (IDY)
                                       TO  SPA-GMN-BTUNUM-KSIKBN-G
                   MOVE    SPA-GMN-BTUNUM-KSIKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-DRNAME-KSIKBN
                                       TO  SPA-GMN-DRNAME-KSIKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-DRNAME-KSIKBN-MAX
               IF      SPA-GMN-DRNAME-KSIKBN 
                               =   SPA-GMN-DRNAME-KSIKBNL (IDY)
                   MOVE    SPA-GMN-DRNAME-KSIKBNLST (IDY)
                                       TO  SPA-GMN-DRNAME-KSIKBN-G
                   MOVE    SPA-GMN-DRNAME-KSIKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-PRT-CHECKKBN TO  SPA-GMN-PRT-CHECKKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-PRT-CHECKKBN-MAX
               IF      SPA-GMN-PRT-CHECKKBN
                               =   SPA-GMN-PRT-CHECKKBNL (IDY)
                   MOVE    SPA-GMN-PRT-CHECKKBNLST (IDY)
                                       TO  SPA-GMN-PRT-CHECKKBN-G
                   MOVE    SPA-GMN-PRT-CHECKKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-JIHI-PRTKBN  TO  SPA-GMN-JIHI-PRTKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-JIHI-PRTKBN-MAX
               IF      SPA-GMN-JIHI-PRTKBN
                               =   SPA-GMN-JIHI-PRTKBNL (IDY)
                   MOVE    SPA-GMN-JIHI-PRTKBNLST (IDY)
                                       TO  SPA-GMN-JIHI-PRTKBN-G
                   MOVE    SPA-GMN-JIHI-PRTKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-PRTJYUNKBN5      TO  SPA-GMN-PRTJYUN5
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-PRTJYUN5-MAX
               IF      SPA-GMN-PRTJYUN5 
                               =   SPA-GMN-PRTJYUN5L (IDY)
                   MOVE    SPA-GMN-PRTJYUN5LST (IDY)
                                       TO  SPA-GMN-PRTJYUN5-G
                   MOVE    SPA-GMN-PRTJYUN5-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-PRTJYUNKBN6      TO  SPA-GMN-PRTJYUN6
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-PRTJYUN6-MAX
               IF      SPA-GMN-PRTJYUN6 
                               =   SPA-GMN-PRTJYUN6L (IDY)
                   MOVE    SPA-GMN-PRTJYUN6LST (IDY)
                                       TO  SPA-GMN-PRTJYUN6-G
                   MOVE    SPA-GMN-PRTJYUN6-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *         
           MOVE    SYS-25-KOUIKIOCRKBN TO  SPA-GMN-KOUIKIOCRKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-KOUIKIOCRKBN-MAX
               IF      SPA-GMN-KOUIKIOCRKBN 
                               =   SPA-GMN-KOUIKIOCRKBNL (IDY)
                   MOVE    SPA-GMN-KOUIKIOCRKBNLST (IDY)
                                       TO  SPA-GMN-KOUIKIOCRKBN-G
                   MOVE    SPA-GMN-KOUIKIOCRKBN-MAX
                                       TO  IDY
               END-IF                                     
           END-PERFORM
      *
           MOVE    SYS-25-KOUIKIKBN      TO  SPA-GMN-KOUIKIKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-KOUIKIKBN-MAX
               IF      SPA-GMN-KOUIKIKBN 
                               =   SPA-GMN-KOUIKIKBNL (IDY)
                   MOVE    SPA-GMN-KOUIKIKBNLST (IDY)
                                       TO  SPA-GMN-KOUIKIKBN-G
                   MOVE    SPA-GMN-KOUIKIKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-KOUIKIRECEKBN
                                       TO  SPA-GMN-KOUIKIRECEKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-KOUIKIRECEKBN-MAX
               IF      SPA-GMN-KOUIKIRECEKBN 
                               =   SPA-GMN-KOUIKIRECEKBNL (IDY)
                   MOVE    SPA-GMN-KOUIKIRECEKBNLST (IDY)
                                       TO  SPA-GMN-KOUIKIRECEKBN-G
                   MOVE    SPA-GMN-KOUIKIRECEKBN-MAX
                                       TO  IDY
               END-IF                                     
           END-PERFORM
      *
           MOVE    SYS-25-ROURECEKBN   TO  SPA-GMN-ROURECEKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-ROURECEKBN-MAX
               IF      SPA-GMN-ROURECEKBN 
                               =   SPA-GMN-ROURECEKBNL (IDY)
                   MOVE    SPA-GMN-ROURECEKBNLST (IDY)
                                       TO  SPA-GMN-ROURECEKBN-G
                   MOVE    SPA-GMN-ROURECEKBN-MAX
                                       TO  IDY
               END-IF                                     
           END-PERFORM
      *
           MOVE    SYS-25-NI-JYOKEN     (1)
                                       TO  SPA-GMN-NI-JYOKEN5
           MOVE    SYS-25-NI-JYOKEN     (2)
                                       TO  SPA-GMN-NI-JYOKEN6
      *
           MOVE    SYS-25-RECEDEN-CDRKBN
                                       TO  SPA-GMN-RECEDEN-CDRKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-RECEDEN-CDRKBN-MAX
               IF      SPA-GMN-RECEDEN-CDRKBN 
                                   =   SPA-GMN-RECEDEN-CDRKBNL (IDY)
                   MOVE    SPA-GMN-RECEDEN-CDRKBNLST (IDY)
                                           TO  SPA-GMN-RECEDEN-CDRKBN-G
                   MOVE    SPA-GMN-RECEDEN-CDRKBN-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-RECEDEN-LST1KBN
                                       TO  SPA-GMN-RECEDEN-LST1KBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-RECEDEN-LST1KBN-MAX
               IF      SPA-GMN-RECEDEN-LST1KBN 
                                   =   SPA-GMN-RECEDEN-LST1KBNL (IDY)
                   MOVE    SPA-GMN-RECEDEN-LST1KBNLST (IDY)
                                           TO  SPA-GMN-RECEDEN-LST1KBN-G
                   MOVE    SPA-GMN-RECEDEN-LST1KBN-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-RECEDEN-LST2KBN
                                       TO  SPA-GMN-RECEDEN-LST2KBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-RECEDEN-LST2KBN-MAX
               IF      SPA-GMN-RECEDEN-LST2KBN 
                                   =   SPA-GMN-RECEDEN-LST2KBNL (IDY)
                   MOVE    SPA-GMN-RECEDEN-LST2KBNLST (IDY)
                                           TO  SPA-GMN-RECEDEN-LST2KBN-G
                   MOVE    SPA-GMN-RECEDEN-LST2KBN-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-RECEDEN-LST2-MEDIA
                                   TO  SPA-GMN-RECEDEN-LST2-MEDIA
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-RECEDEN-LST2-MEDIA-MAX
               IF      SPA-GMN-RECEDEN-LST2-MEDIA 
                               =   SPA-GMN-RECEDEN-LST2-MEDIAL (IDY)
                   MOVE    SPA-GMN-RECEDEN-LST2-MEDIALST (IDY)
                                       TO
                                       SPA-GMN-RECEDEN-LST2-MEDIA-G
                   MOVE    SPA-GMN-RECEDEN-LST2-MEDIA-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-RECEDEN-RIREKIKBN
                                       TO  SPA-GMN-RECEDEN-RIREKIKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-RECEDEN-RIREKIKBN-MAX
               IF      SPA-GMN-RECEDEN-RIREKIKBN 
                                   =   SPA-GMN-RECEDEN-RIREKIKBNL (IDY)
                   MOVE    SPA-GMN-RECEDEN-RIREKIKBNLST (IDY)
                                       TO  SPA-GMN-RECEDEN-RIREKIKBN-G
                   MOVE    SPA-GMN-RECEDEN-RIREKIKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-RECEDEN-DATAKBN
                                       TO  SPA-GMN-RECEDEN-DATAKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-RECEDEN-DATAKBN-MAX
               IF      SPA-GMN-RECEDEN-DATAKBN 
                                   =   SPA-GMN-RECEDEN-DATAKBNL (IDY)
                   MOVE    SPA-GMN-RECEDEN-DATAKBNLST (IDY)
                                           TO  SPA-GMN-RECEDEN-DATAKBN-G
                   MOVE    SPA-GMN-RECEDEN-DATAKBN-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-RECEDEN-DISASTER
                                       TO  SPA-GMN-RECEDEN-DISASTER
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-RECEDEN-DISASTER-MAX
               IF      SPA-GMN-RECEDEN-DISASTER 
                                   =   SPA-GMN-RECEDEN-DISASTERL (IDY)
                   MOVE    SPA-GMN-RECEDEN-DISASTERLST (IDY)
                                       TO   SPA-GMN-RECEDEN-DISASTER-G
                   MOVE    SPA-GMN-RECEDEN-DISASTER-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-RECEDEN-CHK-KBN
                                       TO  SPA-GMN-RECEDEN-CHK
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-RECEDEN-CHK-MAX
               IF      SPA-GMN-RECEDEN-CHK 
                                   =   SPA-GMN-RECEDEN-CHKL (IDY)
                   MOVE    SPA-GMN-RECEDEN-CHKLST (IDY)
                                       TO   SPA-GMN-RECEDEN-CHK-G
                   MOVE    SPA-GMN-RECEDEN-CHK-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-KOUIKIPRTNM
                                       TO  SPA-GMN-KOUIKIPRTNM         
           MOVE    SYS-25-KOUIKIPRTNM-NEXT
                                       TO  SPA-GMN-KOUIKIPRTNM-NEXT
           MOVE    SYS-25-KOUIKIPRTNM-N
                                       TO  SPA-GMN-KOUIKIPRTNM-N
           MOVE    SYS-25-KOUIKIPRTNM-NEXT-N
                                       TO SPA-GMN-KOUIKIPRTNM-NEXT-N
      *         
           MOVE    SYS-25-KOUIKIPRTKBN     TO  SPA-GMN-KOUIKIPRTKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-KOUIKIPRTKBN-MAX
               IF      SPA-GMN-KOUIKIPRTKBN 
                               =   SPA-GMN-KOUIKIPRTKBNL (IDY)
                   MOVE    SPA-GMN-KOUIKIPRTKBNLST (IDY)
                                       TO  SPA-GMN-KOUIKIPRTKBN-G
                   MOVE    SPA-GMN-KOUIKIPRTKBN-MAX
                                       TO  IDY
               END-IF                                     
           END-PERFORM
      *
           MOVE    SYS-25-RSI-INVOICE-KBN
                                   TO  SPA-GMN-RSI-INVOICEKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-RSI-INVOICEKBN-MAX
               IF      SPA-GMN-RSI-INVOICEKBN 
                                   =   SPA-GMN-RSI-INVOICEKBNL (IDY)
                   MOVE    SPA-GMN-RSI-INVOICEKBNLST (IDY)
                                       TO  SPA-GMN-RSI-INVOICEKBN-G
                   MOVE    SPA-GMN-RSI-INVOICEKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-ROUSAIPRTKBN
                                   TO  SPA-GMN-ROUSAIPRTKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-ROUSAIPRTKBN-MAX
               IF      SPA-GMN-ROUSAIPRTKBN 
                                   =   SPA-GMN-ROUSAIPRTKBNL (IDY)
                   MOVE    SPA-GMN-ROUSAIPRTKBNLST (IDY)
                                       TO  SPA-GMN-ROUSAIPRTKBN-G
                   MOVE    SPA-GMN-ROUSAIPRTKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-FRAME-ROUSAIPRTKBN
                                   TO  SPA-GMN-FRAME-ROUSAIPRTKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >      SPA-GMN-FRAME-ROUSAIPRTKBN-MAX
               IF      SPA-GMN-FRAME-ROUSAIPRTKBN 
                               =   SPA-GMN-FRAME-ROUSAIPRTKBNL (IDY)
                   MOVE    SPA-GMN-FRAME-ROUSAIPRTKBNLST (IDY)
                                   TO  SPA-GMN-FRAME-ROUSAIPRTKBN-G
                   MOVE    SPA-GMN-FRAME-ROUSAIPRTKBN-MAX
                                   TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-DRCD-PRTKBN
                                       TO  SPA-GMN-DRCD-PRTKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-DRCD-PRTKBN-MAX
               IF      SPA-GMN-DRCD-PRTKBN
                               =   SPA-GMN-DRCD-PRTKBNL (IDY)
                   MOVE    SPA-GMN-DRCD-PRTKBNLST (IDY)
                                       TO  SPA-GMN-DRCD-PRTKBN-G
                   MOVE    SPA-GMN-DRCD-PRTKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-DRCD-PRTJYUNKBN
                                       TO  SPA-GMN-DRCD-PRTJYUNKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-DRCD-PRTJYUNKBN-MAX
               IF      SPA-GMN-DRCD-PRTJYUNKBN
                               =   SPA-GMN-DRCD-PRTJYUNKBNL (IDY)
                   MOVE    SPA-GMN-DRCD-PRTJYUNKBNLST (IDY)
                                       TO  SPA-GMN-DRCD-PRTJYUNKBN-G
                   MOVE    SPA-GMN-DRCD-PRTJYUNKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-BTUNUM-PRTKBN
                                       TO  SPA-GMN-BTUNUM-PRTKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-BTUNUM-PRTKBN-MAX
               IF      SPA-GMN-BTUNUM-PRTKBN
                               =   SPA-GMN-BTUNUM-PRTKBNL (IDY)
                   MOVE    SPA-GMN-BTUNUM-PRTKBNLST (IDY)
                                       TO  SPA-GMN-BTUNUM-PRTKBN-G
                   MOVE    SPA-GMN-BTUNUM-PRTKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-BTUNUM-PRTJYUNKBN
                                       TO  SPA-GMN-BTUNUM-PRTJYUNKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-BTUNUM-PRTJYUNKBN-MAX
               IF      SPA-GMN-BTUNUM-PRTJYUNKBN
                               =   SPA-GMN-BTUNUM-PRTJYUNKBNL (IDY)
                   MOVE    SPA-GMN-BTUNUM-PRTJYUNKBNLST (IDY)
                                       TO  SPA-GMN-BTUNUM-PRTJYUNKBN-G
                   MOVE    SPA-GMN-BTUNUM-PRTJYUNKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-TERMINAL-DIGIT-PRTKBN
                                       TO  SPA-GMN-TERMINAL-DIG-PRTKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-TERMINAL-DIG-PRTKBN-MAX
               IF      SPA-GMN-TERMINAL-DIG-PRTKBN
                               =   SPA-GMN-TERMINAL-DIG-PRTKBNL (IDY)
                   MOVE    SPA-GMN-TERMINAL-DIG-PRTKBNLST (IDY)
                                       TO
                                       SPA-GMN-TERMINAL-DIG-PRTKBN-G
                   MOVE    SPA-GMN-TERMINAL-DIG-PRTKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-COMMENT-NYUINRYO-KSIKBN
                                       TO  SPA-GMN-COMMENT-NYUINRYO
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-COMMENT-NYUINRYO-MAX
               IF      SPA-GMN-COMMENT-NYUINRYO 
                               =   SPA-GMN-COMMENT-NYUINRYOL (IDY)
                   MOVE    SPA-GMN-COMMENT-NYUINRYOLST (IDY)
                                       TO  SPA-GMN-COMMENT-NYUINRYO-G
                   MOVE    SPA-GMN-COMMENT-NYUINRYO-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-COMMENT-NYUINRYO1-KSIKBN
                                       TO  SPA-GMN-COMMENT-NYUINRYO1
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-COMMENT-NYUINRYO1-MAX
               IF      SPA-GMN-COMMENT-NYUINRYO1 
                               =   SPA-GMN-COMMENT-NYUINRYO1L (IDY)
                   MOVE    SPA-GMN-COMMENT-NYUINRYO1LST (IDY)
                                       TO  SPA-GMN-COMMENT-NYUINRYO1-G
                   MOVE    SPA-GMN-COMMENT-NYUINRYO1-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-CHKKBN       TO  SPA-GMN-CHKKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-CHKKBN-MAX
               IF      SPA-GMN-CHKKBN 
                               =   SPA-GMN-CHKKBNL (IDY)
                   MOVE    SPA-GMN-CHKKBNLST (IDY)
                                       TO  SPA-GMN-CHKKBN-G
                   MOVE    SPA-GMN-CHKKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-BYOMEI-PRTJYUN
                                       TO  SPA-GMN-BYOMEI-PRTJYUN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-BYOMEI-PRTJYUN-MAX
               IF      SPA-GMN-BYOMEI-PRTJYUN 
                               =   SPA-GMN-BYOMEI-PRTJYUNL (IDY)
                   MOVE    SPA-GMN-BYOMEI-PRTJYUNLST (IDY)
                                       TO  SPA-GMN-BYOMEI-PRTJYUN-G
                   MOVE    SPA-GMN-BYOMEI-PRTJYUN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-SHOKUJI-PRTKBN
                                       TO  SPA-GMN-SHOKUJI-PRTKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-SHOKUJI-PRTKBN-MAX
               IF      SPA-GMN-SHOKUJI-PRTKBN 
                               =   SPA-GMN-SHOKUJI-PRTKBNL (IDY)
                   MOVE    SPA-GMN-SHOKUJI-PRTKBNLST (IDY)
                                       TO  SPA-GMN-SHOKUJI-PRTKBN-G
                   MOVE    SPA-GMN-SHOKUJI-PRTKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-INSPECT-CHARACTER-KBN
                                       TO  SPA-GMN-INSPECT-CHARA-KBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-INSPECT-CHARA-KBN-MAX
               IF      SPA-GMN-INSPECT-CHARA-KBN 
                               =   SPA-GMN-INSPECT-CHARA-KBNL (IDY)
                   MOVE    SPA-GMN-INSPECT-CHARA-KBNLST (IDY)
                                       TO
                                       SPA-GMN-INSPECT-CHARA-KBN-G
                   MOVE    SPA-GMN-INSPECT-CHARA-KBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-INSPECT-ZERO-KBN
                                       TO  SPA-GMN-INSPECT-ZERO-KBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-INSPECT-ZERO-KBN-MAX
               IF      SPA-GMN-INSPECT-ZERO-KBN 
                               =   SPA-GMN-INSPECT-ZERO-KBNL (IDY)
                   MOVE    SPA-GMN-INSPECT-ZERO-KBNLST (IDY)
                                       TO
                                       SPA-GMN-INSPECT-ZERO-KBN-G
                   MOVE    SPA-GMN-INSPECT-ZERO-KBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-INSPECT-INCLUDE-KBN
                                       TO  SPA-GMN-INSPECT-INCLUDE-KBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-INSPECT-INCLUDE-KBN-MAX
               IF      SPA-GMN-INSPECT-INCLUDE-KBN 
                               =   SPA-GMN-INSPECT-INCLUDE-KBNL (IDY)
                   MOVE    SPA-GMN-INSPECT-INCLUDE-KBNLST (IDY)
                                       TO
                                       SPA-GMN-INSPECT-INCLUDE-KBN-G
                   MOVE    SPA-GMN-INSPECT-INCLUDE-KBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-INSPECT-GENERAL-KBN
                                       TO  SPA-GMN-INSPECT-GENERAL-KBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-INSPECT-GENERAL-KBN-MAX
               IF      SPA-GMN-INSPECT-GENERAL-KBN 
                               =   SPA-GMN-INSPECT-GENERAL-KBNL (IDY)
                   MOVE    SPA-GMN-INSPECT-GENERAL-KBNLST (IDY)
                                       TO
                                       SPA-GMN-INSPECT-GENERAL-KBN-G
                   MOVE    SPA-GMN-INSPECT-GENERAL-KBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-SEIHO-DSPKBN
                                       TO  SPA-GMN-SEIHO-DSPKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-SEIHO-DSPKBN-MAX
               IF      SPA-GMN-SEIHO-DSPKBN 
                               =   SPA-GMN-SEIHO-DSPKBNL (IDY)
                   MOVE    SPA-GMN-SEIHO-DSPKBNLST (IDY)
                                       TO  SPA-GMN-SEIHO-DSPKBN-G
                   MOVE    SPA-GMN-SEIHO-DSPKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-RECEIPT-MAKE-KBN
                                       TO  SPA-GMN-RECEIPT-MAKEKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-RECEIPT-MAKEKBN-MAX
               IF      SPA-GMN-RECEIPT-MAKEKBN 
                               =   SPA-GMN-RECEIPT-MAKEKBNL (IDY)
                   MOVE    SPA-GMN-RECEIPT-MAKEKBNLST (IDY)
                                       TO  SPA-GMN-RECEIPT-MAKEKBN-G
                   MOVE    SPA-GMN-RECEIPT-MAKEKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-ZERO-KOH-PRTKBN
                                       TO  SPA-GMN-ZERO-KOH-PRTKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-ZERO-KOH-PRTKBN-MAX
               IF      SPA-GMN-ZERO-KOH-PRTKBN 
                               =   SPA-GMN-ZERO-KOH-PRTKBNL (IDY)
                   MOVE    SPA-GMN-ZERO-KOH-PRTKBNLST (IDY)
                                       TO  SPA-GMN-ZERO-KOH-PRTKBN-G
                   MOVE    SPA-GMN-ZERO-KOH-PRTKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-FRAME-ROUPRTKBN
                                       TO  SPA-GMN-FRAME-ROUPRTKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >   SPA-GMN-FRAME-ROUPRTKBN-MAX
               IF      SPA-GMN-FRAME-ROUPRTKBN
                           =   SPA-GMN-FRAME-ROUPRTKBNL (IDY)
                   MOVE    SPA-GMN-FRAME-ROUPRTKBNLST (IDY)
                                       TO  SPA-GMN-FRAME-ROUPRTKBN-G
                   MOVE    SPA-GMN-FRAME-ROUPRTKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-FRAME-ROUPRTKBN-N
                                       TO  SPA-GMN-FRAME-ROUPRTKBN-N
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >   SPA-GMN-FRAME-ROUPRTKBN-N-MAX
               IF      SPA-GMN-FRAME-ROUPRTKBN-N
                           =   SPA-GMN-FRAME-ROUPRTKBN-NL (IDY)
                   MOVE    SPA-GMN-FRAME-ROUPRTKBN-NLST (IDY)
                                       TO  SPA-GMN-FRAME-ROUPRTKBN-N-G
                   MOVE    SPA-GMN-FRAME-ROUPRTKBN-N-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-COMMENT-ROU-KSIKBN
                                       TO  SPA-GMN-COMMENT-ROU
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >   SPA-GMN-COMMENT-ROU-MAX
               IF      SPA-GMN-COMMENT-ROU
                           =   SPA-GMN-COMMENT-ROUL (IDY)
                   MOVE    SPA-GMN-COMMENT-ROULST (IDY)
                                       TO  SPA-GMN-COMMENT-ROU-G
                   MOVE    SPA-GMN-COMMENT-ROU-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-RSI-RECEIPT-KBN
                                       TO  SPA-GMN-RSI-RECEIPTKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >   SPA-GMN-RSI-RECEIPTKBN-MAX
               IF      SPA-GMN-RSI-RECEIPTKBN
                               =   SPA-GMN-RSI-RECEIPTKBNL (IDY)
                   MOVE    SPA-GMN-RSI-RECEIPTKBNLST (IDY)
                                       TO  SPA-GMN-RSI-RECEIPTKBN-G
                   MOVE    SPA-GMN-RSI-RECEIPTKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-ROUPRTKBN    TO  SPA-GMN-ROUPRTKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >   SPA-GMN-ROUPRTKBN-MAX
               IF      SPA-GMN-ROUPRTKBN
                               =   SPA-GMN-ROUPRTKBNL (IDY)
                   MOVE    SPA-GMN-ROUPRTKBNLST (IDY)
                                       TO  SPA-GMN-ROUPRTKBN-G
                   MOVE    SPA-GMN-ROUPRTKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-ROUPRTKBN-N  TO  SPA-GMN-ROUPRTKBN-N
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >   SPA-GMN-ROUPRTKBN-N-MAX
               IF      SPA-GMN-ROUPRTKBN-N
                               =   SPA-GMN-ROUPRTKBN-NL (IDY)
                   MOVE    SPA-GMN-ROUPRTKBN-NLST (IDY)
                                       TO  SPA-GMN-ROUPRTKBN-N-G
                   MOVE    SPA-GMN-ROUPRTKBN-N-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *         
           MOVE    SYS-25-TENKIBYOMEI-KBN
                                       TO  SPA-GMN-TENKIBYOMEI-KBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-TENKIBYOMEI-KBN-MAX
               IF      SPA-GMN-TENKIBYOMEI-KBN 
                               =   SPA-GMN-TENKIBYOMEI-KBNL (IDY)
                   MOVE    SPA-GMN-TENKIBYOMEI-KBNLST (IDY)
                                       TO  SPA-GMN-TENKIBYOMEI-KBN-G
                   MOVE    SPA-GMN-TENKIBYOMEI-KBN-MAX
                                       TO  IDY
               END-IF                                     
           END-PERFORM
      *         
           MOVE    SYS-25-NYUTAIINREKI-NYU-KSIKBN
                                       TO
                                       SPA-GMN-NYUTAIIN-NYU-KSIKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >   SPA-GMN-NYUTAIIN-NYU-KSIKBN-MAX
               IF      SPA-GMN-NYUTAIIN-NYU-KSIKBN 
                               =   SPA-GMN-NYUTAIIN-NYU-KSIKBNL(IDY)
                   MOVE    SPA-GMN-NYUTAIIN-NYU-KSIKBNLST  (IDY)
                                       TO
                                       SPA-GMN-NYUTAIIN-NYU-KSIKBN-G
                   MOVE    SPA-GMN-NYUTAIIN-NYU-KSIKBN-MAX
                                       TO  IDY
               END-IF                                     
           END-PERFORM
      *
           MOVE    SYS-25-HKN-RIREKI-KBN
                                       TO  SPA-GMN-HKN-RIREKI
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-HKN-RIREKI-MAX
               IF      SPA-GMN-HKN-RIREKI 
                               =   SPA-GMN-HKN-RIREKIL (IDY)
                   MOVE    SPA-GMN-HKN-RIREKILST (IDY)
                                       TO  SPA-GMN-HKN-RIREKI-G
                   MOVE    SPA-GMN-HKN-RIREKI-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-GENBUTU-ICHIBU-KSIKBN
                                       TO  SPA-GMN-GENBUTU-ICHIBU
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-GENBUTU-ICHIBU-MAX
               IF      SPA-GMN-GENBUTU-ICHIBU
                               =   SPA-GMN-GENBUTU-ICHIBUL (IDY)
                   MOVE    SPA-GMN-GENBUTU-ICHIBULST (IDY)
                                       TO  SPA-GMN-GENBUTU-ICHIBU-G
                   MOVE    SPA-GMN-GENBUTU-ICHIBU-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-COMPRESS-KBN
                                       TO  SPA-GMN-COMPRESS
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-COMPRESS-MAX
               IF      SPA-GMN-COMPRESS
                               =   SPA-GMN-COMPRESSL (IDY)
                   MOVE    SPA-GMN-COMPRESSLST (IDY)
                                       TO  SPA-GMN-COMPRESS-G
                   MOVE    SPA-GMN-COMPRESS-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-DISASTER-RECEIPT
                                       TO  SPA-GMN-DISASTER-RECEIPT
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-DISASTER-RECEIPT-MAX
               IF      SPA-GMN-DISASTER-RECEIPT
                               =   SPA-GMN-DISASTER-RECEIPTL (IDY)
                   MOVE    SPA-GMN-DISASTER-RECEIPTLST (IDY)
                                       TO  SPA-GMN-DISASTER-RECEIPT-G
                   MOVE    SPA-GMN-DISASTER-RECEIPT-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-DISASTER-SYAPRTKBN
                                       TO  SPA-GMN-DISASTER-SYAPRTKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >   SPA-GMN-DISASTER-SYAPRTKBN-MAX
               IF      SPA-GMN-DISASTER-SYAPRTKBN 
                               =   SPA-GMN-DISASTER-SYAPRTKBNL (IDY)
                   MOVE    SPA-GMN-DISASTER-SYAPRTKBNLST (IDY)
                                       TO  SPA-GMN-DISASTER-SYAPRTKBN-G
                   MOVE    SPA-GMN-DISASTER-SYAPRTKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-15-YKZ-PRTKBN
                                       TO  SPA-GMN-15-YKZ
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-15-YKZ-MAX
               IF      SPA-GMN-15-YKZ
                               =   SPA-GMN-15-YKZL (IDY)
                   MOVE    SPA-GMN-15-YKZLST (IDY)
                                       TO  SPA-GMN-15-YKZ-G
                   MOVE    SPA-GMN-15-YKZ-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-25-KOUHI-ICHIBU-KSIKBN
                                       TO  SPA-GMN-KOUHI-ICHIBU-KSI
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-KOUHI-ICHIBU-KSI-MAX
               IF      SPA-GMN-KOUHI-ICHIBU-KSI
                               =   SPA-GMN-KOUHI-ICHIBU-KSIL (IDY)
                   MOVE    SPA-GMN-KOUHI-ICHIBU-KSILST (IDY)
                                       TO  SPA-GMN-KOUHI-ICHIBU-KSI-G
                   MOVE    SPA-GMN-KOUHI-ICHIBU-KSI-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
      *    レセプト・総括編集情報（区分コード１０のデータ読込）
           MOVE    SPACE               TO  SYS-200505-REC
           INITIALIZE                      SYS-200505-REC
           MOVE    "2005"              TO  SYS-200505-KANRICD
           MOVE    "05"                TO  SYS-200505-KBNCD
           MOVE    SPA-W01-SEL-STYUKYMD
                                       TO  SYS-200505-STYUKYMD
           MOVE    SPA-W01-SEL-EDYUKYMD
                                       TO  SYS-200505-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-200505-HOSPNUM
           MOVE    SYS-200505-REC      TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-200505-REC
           ELSE
               INITIALIZE                      SYS-200505-REC
           END-IF
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-CONS-200505-HKNJANUM-MAX
                   OR      SYS-200505-HKNJANUM (IDX)
                               =   SPACE
               MOVE    IDX                 TO
                                   SPA-GMN-HKNJALIST-TNO       (IDX)
               MOVE    SYS-200505-HKNJANUM (IDX)
                                           TO
                                   SPA-GMN-HKNJALIST-THKNJANUM (IDX)
               MOVE    SYS-200505-HKNJANUM-63 (IDX)
                                           TO
                                   SPA-GMN-HKNJALIST-THKNJANUM-63 (IDX)
      *
               MOVE    SPACE               TO  HKNJAINF-REC
               INITIALIZE                      HKNJAINF-REC
               MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
               MOVE    SPA-GMN-HKNJALIST-THKNJANUM (IDX)
                                           TO  HKNJA-HKNJANUM
               MOVE    HKNJAINF-REC        TO  MCPDATA-REC
               PERFORM 900-HKNJAINF-READ-SEC
               MOVE    HKNJA-HKNJANAME     TO
                                   SPA-GMN-HKNJALIST-THKNJANAME(IDX)
      *
               MOVE    IDX                 TO  SPA-GMN-HKNJALIST-MAX
           END-PERFORM
      *
      *    レセプト（編綴順）情報
           MOVE    SPACE               TO  SYS-2008-REC
           INITIALIZE                      SYS-2008-REC
           MOVE    "2008"              TO  SYS-2008-KANRICD
           MOVE    SPA-W01-SEL-STYUKYMD
                                       TO  SYS-2008-STYUKYMD
           MOVE    SPA-W01-SEL-EDYUKYMD
                                       TO  SYS-2008-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2008-HOSPNUM
           MOVE    SYS-2008-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key13"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-2008-REC
      *
               MOVE    SYS-2008-KBNCD      TO  SPA-NAI-2008-KBNCD
      *
               MOVE    SYS-2008-PREFKBN-PRTJYUN
                                           TO  SPA-GMN-PREFKBN-PRTJYUN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PREFKBN-PRTJYUN-MAX
                   IF      SPA-GMN-PREFKBN-PRTJYUN
                                   =   SPA-GMN-PREFKBN-PRTJYUNL (IDY)
                       MOVE    SPA-GMN-PREFKBN-PRTJYUNLST (IDY)
                                           TO  SPA-GMN-PREFKBN-PRTJYUN-G
                       MOVE    SPA-GMN-PREFKBN-PRTJYUN-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2008-HKNJANUM-PRTJYUN
                                           TO  SPA-GMN-HKNJANUM-PRTJYUN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-HKNJANUM-PRTJYUN-MAX
                   IF      SPA-GMN-HKNJANUM-PRTJYUN
                                   =   SPA-GMN-HKNJANUM-PRTJYUNL (IDY)
                       MOVE    SPA-GMN-HKNJANUM-PRTJYUNLST (IDY)
                                           TO
                                           SPA-GMN-HKNJANUM-PRTJYUN-G
                       MOVE    SPA-GMN-HKNJANUM-PRTJYUN-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2008-RECESYUBETU-PRTJYUN
                                           TO
                                           SPA-GMN-RECESYUBETU-PRTJYUN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-RECESYUBETU-PRTJYUN-MAX
                   IF      SPA-GMN-RECESYUBETU-PRTJYUN
                               =   SPA-GMN-RECESYUBETU-PRTJYUNL (IDY)
                       MOVE    SPA-GMN-RECESYUBETU-PRTJYUNLST (IDY)
                                           TO
                                       SPA-GMN-RECESYUBETU-PRTJYUN-G
                       MOVE    SPA-GMN-RECESYUBETU-PRTJYUN-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *         
               MOVE    SYS-2008-ZAITAKU    TO  SPA-GMN-ZAITAKU
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-ZAITAKU-MAX
                   IF      SPA-GMN-ZAITAKU
                                   =   SPA-GMN-ZAITAKUL (IDY)
                       MOVE    SPA-GMN-ZAITAKULST (IDY)
                                           TO  SPA-GMN-ZAITAKU-G
                       MOVE    SPA-GMN-ZAITAKU-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2008-CHOUKI     TO  SPA-GMN-CHOUKI
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-CHOUKI-MAX
                   IF      SPA-GMN-CHOUKI
                                   =   SPA-GMN-CHOUKIL (IDY)
                       MOVE    SPA-GMN-CHOUKILST (IDY)
                                           TO
                                           SPA-GMN-CHOUKI-G
                       MOVE    SPA-GMN-CHOUKI-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2008-HKNJANUM-PRTKBN
                                           TO
                                           SPA-GMN-HKNJANUM-PRTKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-HKNJANUM-PRTKBN-MAX
                   IF      SPA-GMN-HKNJANUM-PRTKBN
                               =   SPA-GMN-HKNJANUM-PRTKBNL (IDY)
                       MOVE    SPA-GMN-HKNJANUM-PRTKBNLST (IDY)
                                           TO
                                       SPA-GMN-HKNJANUM-PRTKBN-G
                       MOVE    SPA-GMN-HKNJANUM-PRTKBN-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2008-HKNJANUM   TO  SPA-GMN-HKNJANUM
               MOVE    SPACE               TO  HKNJAINF-REC
               INITIALIZE                      HKNJAINF-REC
               MOVE    SPA-HOSPNUM          TO  HKNJA-HOSPNUM
               MOVE    SPA-GMN-HKNJANUM    TO  HKNJA-HKNJANUM
grpsys*        MOVE    SPA-HOSPNUM         TO  -HOSPNUM
               MOVE    HKNJAINF-REC        TO  MCPDATA-REC
               PERFORM 900-HKNJAINF-READ-SEC
               MOVE    HKNJA-HKNJANAME     TO  SPA-GMN-HKNJANAME
           ELSE
               MOVE    ZERO                TO  SPA-NAI-2008-KBNCD-9  
           END-IF
      *
      *    レセプト情報（主科設定）
      *
      *    レセプト・総括編集情報
           MOVE    SPACE               TO  SYS-2009-REC
           INITIALIZE                      SYS-2009-REC
           MOVE    "2009"              TO  SYS-2009-KANRICD
           MOVE    "*"                 TO  SYS-2009-KBNCD
           MOVE    SPA-W01-SEL-STYUKYMD
                                       TO  SYS-2009-STYUKYMD
           MOVE    SPA-W01-SEL-EDYUKYMD
                                       TO  SYS-2009-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2009-HOSPNUM
           MOVE    SYS-2009-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-2009-REC
      *
      *        主科設定フラグが「する」で社保設定フラグが未設定のとき
      *        社保設定フラグを「主科」にする
               IF    ( SYS-2009-SYUKA-KBN      =   "1"   )
               AND   ( SYS-2009-SYUKA-SYAHO    =   SPACE )
                   MOVE    "1"             TO  SYS-2009-SYUKA-SYAHO
               END-IF      
               IF    ( SYS-2009-SYUKA-KBN      =   "1"   )
               AND   ( SYS-2009-SYUKA-KOKUHO   =   SPACE )
                   MOVE    "0"             TO  SYS-2009-SYUKA-KOKUHO
               END-IF      
               IF    ( SYS-2009-SYUKA-KBN      =   "1"   )
               AND   ( SYS-2009-SYUKA-KOUIKI   =   SPACE )
                   MOVE    "0"             TO  SYS-2009-SYUKA-KOUIKI
               END-IF      
      *
               MOVE    SYS-2009-SYUKA-KBN      TO  SPA-GMN-SYUKA-KBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-SYUKA-KBN-MAX
                   IF      SPA-GMN-SYUKA-KBN
                                   =   SPA-GMN-SYUKA-KBNL (IDY)
                       MOVE    SPA-GMN-SYUKA-KBNLST (IDY)
                                               TO  SPA-GMN-SYUKA-KBN-G
                       MOVE    SPA-GMN-SYUKA-KBN-MAX
                                               TO  IDY
                   END-IF
               END-PERFORM
      *         
               MOVE    SYS-2009-SYUKA-NYUIN    TO  SPA-GMN-SYUKA-NYUIN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-SYUKA-NYUIN-MAX
                   IF      SPA-GMN-SYUKA-NYUIN
                                   =   SPA-GMN-SYUKA-NYUINL (IDY)
                       MOVE    SPA-GMN-SYUKA-NYUINLST (IDY)
                                               TO  SPA-GMN-SYUKA-NYUIN-G
                       MOVE    SPA-GMN-SYUKA-NYUIN-MAX
                                               TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2009-SYUKA-GAIRAI   TO  SPA-GMN-SYUKA-GAIRAI
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-SYUKA-GAIRAI-MAX
                   IF      SPA-GMN-SYUKA-GAIRAI
                                   =   SPA-GMN-SYUKA-GAIRAIL (IDY)
                       MOVE    SPA-GMN-SYUKA-GAIRAILST (IDY)
                                               TO SPA-GMN-SYUKA-GAIRAI-G
                       MOVE    SPA-GMN-SYUKA-GAIRAI-MAX
                                               TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2009-SYUKA-SELECT   TO  SPA-GMN-SYUKA-SELECT
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-SYUKA-SELECT-MAX
                   IF      SPA-GMN-SYUKA-SELECT
                                   =   SPA-GMN-SYUKA-SELECTL (IDY)
                       MOVE    SPA-GMN-SYUKA-SELECTLST (IDY)
                                               TO
                                               SPA-GMN-SYUKA-SELECT-G
                       MOVE    SPA-GMN-SYUKA-SELECT-MAX
                                               TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2009-SYUKA-MODE     TO  SPA-GMN-SYUKA-MODE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-SYUKA-MODE-MAX
                   IF      SPA-GMN-SYUKA-MODE
                                   =   SPA-GMN-SYUKA-MODEL (IDY)
                       MOVE    SPA-GMN-SYUKA-MODELST (IDY)
                                               TO  SPA-GMN-SYUKA-MODE-G
                       MOVE    SPA-GMN-SYUKA-MODE-MAX
                                               TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2009-SYUKA-SYAHO    TO  SPA-GMN-SYUKA-SYAHO
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-SYUKA-SYAHO-MAX
                   IF      SPA-GMN-SYUKA-SYAHO
                                   =   SPA-GMN-SYUKA-SYAHOL (IDY)
                       MOVE    SPA-GMN-SYUKA-SYAHOLST (IDY)
                                               TO  SPA-GMN-SYUKA-SYAHO-G
                       MOVE    SPA-GMN-SYUKA-SYAHO-MAX
                                               TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2009-SYUKA-KOKUHO   TO  SPA-GMN-SYUKA-KOKUHO
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-SYUKA-KOKUHO-MAX
                   IF      SPA-GMN-SYUKA-KOKUHO
                                   =   SPA-GMN-SYUKA-KOKUHOL (IDY)
                       MOVE    SPA-GMN-SYUKA-KOKUHOLST (IDY)
                                               TO
                                               SPA-GMN-SYUKA-KOKUHO-G
                       MOVE    SPA-GMN-SYUKA-KOKUHO-MAX
                                               TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    SYS-2009-SYUKA-KOUIKI   TO  SPA-GMN-SYUKA-KOUIKI
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-SYUKA-KOUIKI-MAX
                   IF      SPA-GMN-SYUKA-KOUIKI
                                   =   SPA-GMN-SYUKA-KOUIKIL (IDY)
                       MOVE    SPA-GMN-SYUKA-KOUIKILST (IDY)
                                               TO
                                               SPA-GMN-SYUKA-KOUIKI-G
                       MOVE    SPA-GMN-SYUKA-KOUIKI-MAX
                                               TO  IDY
                   END-IF
               END-PERFORM
           END-IF    
      *
      *    記号番号区切り文字設定情報
      *
           MOVE    SPACE               TO  S21-REC
           INITIALIZE                      S21-REC
           MOVE    "2011"              TO  S21-KANRICD
           MOVE    "*"                 TO  S21-KBNCD
           MOVE    SPA-W01-SEL-STYUKYMD
                                       TO  S21-STYUKYMD
           MOVE    SPA-W01-SEL-EDYUKYMD
                                       TO  S21-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  S21-HOSPNUM
           MOVE    S21-REC             TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  S21-REC
           ELSE
               INITIALIZE                      S21-REC
           END-IF
           IF      S21-KIGO-DELIMITER-FLG
                                       =   SPACE
               MOVE    "0"             TO  S21-KIGO-DELIMITER-FLG
           END-IF 
           IF      S21-KIGO-DELIMITER-HKNJANUM-FLG
                                       =   SPACE
               MOVE    "0"             TO
                                   S21-KIGO-DELIMITER-HKNJANUM-FLG
           END-IF 
           IF      S21-NUM-DELIMITER-FLG
                                       =   SPACE
               MOVE    "0"             TO  S21-NUM-DELIMITER-FLG
           END-IF 
           IF      S21-NUM-DELIMITER-HKNJANUM-FLG
                                       =   SPACE
               MOVE    "0"             TO
                                   S21-NUM-DELIMITER-HKNJANUM-FLG
           END-IF 
      *
           MOVE    S21-KIGO-DELIMITER-FLG
                                       TO  SPA-GMN-KIGO-DELI-FLG
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-KIGO-DELI-FLG-MAX
               IF      SPA-GMN-KIGO-DELI-FLG 
                                   =   SPA-GMN-KIGO-DELI-FLGL (IDY)
                   MOVE    SPA-GMN-KIGO-DELI-FLGLST (IDY)
                                           TO
                                           SPA-GMN-KIGO-DELI-FLG-G
                   MOVE    SPA-GMN-KIGO-DELI-FLG-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    S21-KIGO-DELIMITER-HKNJANUM-FLG
                                       TO
                                   SPA-GMN-KIGO-DELI-HKNJA-FLG
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >
                           SPA-GMN-KIGO-DELI-HKNJA-FLG-MAX
               IF      SPA-GMN-KIGO-DELI-HKNJA-FLG 
                                   =
                           SPA-GMN-KIGO-DELI-HKNJA-FLGL (IDY)
                   MOVE    SPA-GMN-KIGO-DELI-HKNJA-FLGLST (IDY)
                                           TO
                                   SPA-GMN-KIGO-DELI-HKNJA-FLG-G
                   MOVE    SPA-GMN-KIGO-DELI-HKNJA-FLG-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    S21-NUM-DELIMITER-FLG
                                       TO  SPA-GMN-NUM-DELIMITER-FLG
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-NUM-DELIMITER-FLG-MAX
               IF      SPA-GMN-NUM-DELIMITER-FLG 
                                   =   SPA-GMN-NUM-DELIMITER-FLGL (IDY)
                   MOVE    SPA-GMN-NUM-DELIMITER-FLGLST (IDY)
                                           TO
                                           SPA-GMN-NUM-DELIMITER-FLG-G
                   MOVE    SPA-GMN-NUM-DELIMITER-FLG-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    S21-NUM-DELIMITER-HKNJANUM-FLG
                                       TO
                                   SPA-GMN-NUM-DELI-HKNJA-FLG
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >
                           SPA-GMN-NUM-DELI-HKNJA-FLG-MAX
               IF      SPA-GMN-NUM-DELI-HKNJA-FLG 
                                   =
                           SPA-GMN-NUM-DELI-HKNJA-FLGL (IDY)
                   MOVE    SPA-GMN-NUM-DELI-HKNJA-FLGLST (IDY)
                                           TO
                                   SPA-GMN-NUM-DELI-HKNJA-FLG-G
                   MOVE    SPA-GMN-NUM-DELI-HKNJA-FLG-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
      *
           IF     SPA-GMN-BYOMEIHENKBN-G   =   SPACE
               MOVE    SPA-GMN-BYOMEIHENKBNLST (1)
                                           TO  SPA-GMN-BYOMEIHENKBN-G
           END-IF                                              
           IF     SPA-GMN-BYOMEIHENKBN1-G  =   SPACE
               MOVE    SPA-GMN-BYOMEIHENKBN1LST (1)
                                           TO  SPA-GMN-BYOMEIHENKBN1-G
           END-IF                                              
           IF     SPA-GMN-SYUBYOHENFLG-G  =   SPACE
               MOVE    SPA-GMN-SYUBYOHENFLGLST (1)
                                           TO  SPA-GMN-SYUBYOHENFLG-G
           END-IF                                              
           IF     SPA-GMN-TENKIHENKBN-G    =   SPACE
               MOVE    SPA-GMN-TENKIHENKBNLST (1)
                                           TO  SPA-GMN-TENKIHENKBN-G
           END-IF                                              
           IF     SPA-GMN-TENKIBYOMEI-KBN-G
                                           =   SPACE
               MOVE    SPA-GMN-TENKIBYOMEI-KBNLST (1)
                                           TO  SPA-GMN-TENKIBYOMEI-KBN-G
           END-IF                                              
           IF     SPA-GMN-KKHRECEKBN-G     =   SPACE
               MOVE    SPA-GMN-KKHRECEKBNLST (1)
                                           TO  SPA-GMN-KKHRECEKBN-G
           END-IF                                              
           IF     SPA-GMN-SYARECEKBN-G     =   SPACE
               MOVE    SPA-GMN-SYARECEKBNLST (1)
                                           TO  SPA-GMN-SYARECEKBN-G
           END-IF                                              
           IF     SPA-GMN-KOUIKIRECEKBN-G  =   SPACE
               MOVE    SPA-GMN-KOUIKIRECEKBNLST (1)
                                           TO  SPA-GMN-KOUIKIRECEKBN-G
           END-IF                                              
           IF     SPA-GMN-ROURECEKBN-G  =   SPACE
               MOVE    SPA-GMN-ROURECEKBNLST (1)
                                           TO  SPA-GMN-ROURECEKBN-G
           END-IF                                              
           IF     SPA-GMN-RENNUMHENKBN-G   =   SPACE
               MOVE    SPA-GMN-RENNUMHENKBNLST (1)
                                           TO  SPA-GMN-RENNUMHENKBN-G
           END-IF                                              
           IF     SPA-GMN-NEXTPRTKBN-G   =   SPACE
               MOVE    SPA-GMN-NEXTPRTKBNLST (1)
                                           TO  SPA-GMN-NEXTPRTKBN-G
           END-IF                                              
           IF     SPA-GMN-NEXTPRTKBN1-G  =   SPACE
               MOVE    SPA-GMN-NEXTPRTKBN1LST (1)
                                           TO  SPA-GMN-NEXTPRTKBN1-G
           END-IF                                              
           IF     SPA-GMN-RJNGNBKKBN-G  =   SPACE
               MOVE    SPA-GMN-RJNGNBKKBNLST (1)
                                           TO  SPA-GMN-RJNGNBKKBN-G
           END-IF                                              
           IF     SPA-GMN-JIHITEKKSIKBN-G  =   SPACE
               MOVE    SPA-GMN-JIHITEKKSIKBNLST (1)
                                           TO  SPA-GMN-JIHITEKKSIKBN-G
           END-IF                                              
           IF     SPA-GMN-KOUIKIKBN-G  =   SPACE
               MOVE    SPA-GMN-KOUIKIKBNLST (1)
                                           TO  SPA-GMN-KOUIKIKBN-G
           END-IF                                              
           IF     SPA-GMN-SYAOCRKBN-G  =   SPACE
               MOVE    SPA-GMN-SYAOCRKBNLST (1)
                                           TO  SPA-GMN-SYAOCRKBN-G
           END-IF                                              
           IF     SPA-GMN-KKHOCRKBN-G  =   SPACE
               MOVE    SPA-GMN-KKHOCRKBNLST (1)
                                           TO  SPA-GMN-KKHOCRKBN-G
           END-IF                                              
           IF     SPA-GMN-KOUIKIOCRKBN-G  =   SPACE
               MOVE    SPA-GMN-KOUIKIOCRKBNLST (1)
                                           TO  SPA-GMN-KOUIKIOCRKBN-G
           END-IF                                              
           IF     SPA-GMN-NYUTAIIN-KSIKBN-G  =   SPACE
               MOVE    SPA-GMN-NYUTAIIN-KSIKBNLST (1)
                                       TO  SPA-GMN-NYUTAIIN-KSIKBN-G
           END-IF                                              
           IF     SPA-GMN-NYUTAIIN-NYU-KSIKBN-G  =   SPACE
               MOVE    SPA-GMN-NYUTAIIN-NYU-KSIKBNLST (1)
                                       TO
                                       SPA-GMN-NYUTAIIN-NYU-KSIKBN-G
           END-IF                                              
           IF     SPA-GMN-RECECOMKBN   =   SPACE
               MOVE    SPA-GMN-RECECOMKBNLST (1)
                                           TO  SPA-GMN-RECECOMKBN-G
           END-IF                                              
           IF     SPA-GMN-RECEDEN-CDRKBN
                                       =   SPACE
               MOVE    SPA-GMN-RECEDEN-CDRKBNLST (1)
                                           TO  SPA-GMN-RECEDEN-CDRKBN-G
           END-IF                                              
           IF     SPA-GMN-RECEDEN-LST1KBN
                                       =   SPACE
               MOVE    SPA-GMN-RECEDEN-LST1KBNLST (2)
                                           TO  SPA-GMN-RECEDEN-LST1KBN-G
           END-IF                                              
           IF     SPA-GMN-RECEDEN-LST2KBN
                                       =   SPACE
               MOVE    SPA-GMN-RECEDEN-LST2KBNLST (1)
                                           TO  SPA-GMN-RECEDEN-LST2KBN-G
           END-IF                                              
           IF     SPA-GMN-RECEDEN-LST2-MEDIA
                                       =   SPACE
               MOVE    SPA-GMN-RECEDEN-LST2-MEDIALST (1)
                                       TO
                                       SPA-GMN-RECEDEN-LST2-MEDIA-G
           END-IF                                              
           IF      SPA-MW              =   SPA-GINBEE
               MOVE    SPA-GMN-RECEDEN-RIREKIKBNLST (1)
                                           TO
                                           SPA-GMN-RECEDEN-RIREKIKBN-G
           ELSE
               IF     SPA-GMN-RECEDEN-RIREKIKBN
                                           =   SPACE
                   MOVE    SPA-GMN-RECEDEN-RIREKIKBNLST (1)
                                           TO
                                           SPA-GMN-RECEDEN-RIREKIKBN-G
               END-IF
           END-IF                                              
           IF      SPA-MW              =   SPA-GINBEE
               IF     SPA-GMN-RECEDEN-DATAKBN
                                           =   "5" OR  "6"
                   CONTINUE
               ELSE
                   MOVE    SPACE           TO  SPA-GMN-RECEDEN-DATAKBN-G
               END-IF
           ELSE
               IF     SPA-GMN-RECEDEN-DATAKBN
                                           =   SPACE
                   MOVE    SPA-GMN-RECEDEN-DATAKBNLST (2)
                                           TO  SPA-GMN-RECEDEN-DATAKBN-G
               END-IF
               IF     SPA-200501-RECEDEN-DATAKBN
                                       =   "1" OR "2"
                   MOVE    SPACE           TO  SPA-GMN-RECEDEN-DATAKBN-G
               END-IF                                              
           END-IF                                              
           IF     SPA-GMN-RECEDEN-DISASTER
                                       =   SPACE
               MOVE    SPA-GMN-RECEDEN-DISASTERLST (2)
                                           TO
                                           SPA-GMN-RECEDEN-DISASTER-G
           END-IF                                              
           IF     SPA-GMN-RECEDEN-CHK  =   SPACE
               MOVE    SPA-GMN-RECEDEN-CHKLST (1)
                                           TO
                                           SPA-GMN-RECEDEN-CHK-G
           END-IF                                              
           IF     SPA-GMN-SRYKAHENKBN  =   SPACE
               MOVE    SPA-GMN-SRYKAHENKBNLST (1)
                                           TO  SPA-GMN-SRYKAHENKBN-G
           END-IF                                              
           IF     SPA-GMN-BTUNUM-KSIKBN  =   SPACE
               MOVE    SPA-GMN-BTUNUM-KSIKBNLST (1)
                                           TO  SPA-GMN-BTUNUM-KSIKBN-G
           END-IF                                              
           IF     SPA-GMN-DRNAME-KSIKBN  =   SPACE
               MOVE    SPA-GMN-DRNAME-KSIKBNLST (1)
                                           TO  SPA-GMN-DRNAME-KSIKBN-G
           END-IF                                              
           IF     SPA-GMN-AUTODSPKBN   =   SPACE
               MOVE    SPA-GMN-AUTODSPKBNLST (1)
                                           TO  SPA-GMN-AUTODSPKBN-G
           END-IF                                              
           IF     SPA-GMN-PRT-CHECKKBN   =   SPACE
               MOVE    SPA-GMN-PRT-CHECKKBNLST (1)
                                           TO  SPA-GMN-PRT-CHECKKBN-G
           END-IF                                              
           IF     SPA-GMN-JIHI-PRTKBN   =   SPACE
               MOVE    SPA-GMN-JIHI-PRTKBNLST (1)
                                           TO  SPA-GMN-JIHI-PRTKBN-G
           END-IF                                              
           IF     SPA-GMN-PRTJYUN1     =   SPACE
               MOVE    SPA-GMN-PRTJYUN1LST (1)
                                           TO  SPA-GMN-PRTJYUN1-G
           END-IF                                              
           IF     SPA-GMN-PRTJYUN2     =   SPACE
               MOVE    SPA-GMN-PRTJYUN2LST (1)
                                           TO  SPA-GMN-PRTJYUN2-G
           END-IF                                              
           IF     SPA-GMN-PRTJYUN3     =   SPACE
               MOVE    SPA-GMN-PRTJYUN3LST (1)
                                           TO  SPA-GMN-PRTJYUN3-G
           END-IF                                              
      *****IF     SPA-GMN-PRTJYUN4     =   SPACE
      *****    MOVE    SPA-GMN-PRTJYUN4LST (1)
      *****                                TO  SPA-GMN-PRTJYUN4-G
      *****END-IF                                              
           IF     SPA-GMN-PRTJYUN5     =   SPACE
               MOVE    SPA-GMN-PRTJYUN5LST (1)
                                           TO  SPA-GMN-PRTJYUN5-G
           END-IF                                              
      *****IF     SPA-GMN-PRTJYUN6     =   SPACE
      *****    MOVE    SPA-GMN-PRTJYUN6LST (1)
      *****                                TO  SPA-GMN-PRTJYUN6-G
      *****END-IF                                              
           IF     SPA-GMN-MOJI-KISAIKBN    =   SPACE
               MOVE    SPA-GMN-MOJI-KISAIKBNLST (1)
                                           TO  SPA-GMN-MOJI-KISAIKBN-G
           END-IF                                              
           IF     SPA-GMN-KYURATE-KISAIKBN =   SPACE
               MOVE    SPA-GMN-KYURATE-KISAIKBNLST (1)
                                           TO
                                           SPA-GMN-KYURATE-KISAIKBN-G
           END-IF                                              
           IF     SPA-GMN-RECESYUBET-KISAIKBN =   SPACE
               MOVE    SPA-GMN-RECESYUBET-KISAIKBNLST (1)
                                           TO
                                       SPA-GMN-RECESYUBET-KISAIKBN-G
           END-IF                                              
           IF     SPA-GMN-SYUKA-KBN    =   SPACE
               MOVE    SPA-GMN-SYUKA-KBNLST (1)
                                           TO  SPA-GMN-SYUKA-KBN-G
           END-IF
           IF      SPA-GMN-NI-JYOKEN1  =   ZERO
               MOVE    WRK-CONS-JYOKEN-TENSU
                                           TO  SPA-GMN-NI-JYOKEN1
           END-IF                                              
           IF      SPA-GMN-NI-JYOKEN2  =   ZERO
               MOVE    WRK-CONS-JYOKEN-TENSU
                                           TO  SPA-GMN-NI-JYOKEN2
           END-IF                                              
           IF      SPA-GMN-NI-JYOKEN3  =   ZERO
               MOVE    WRK-CONS-JYOKEN-TENSU
                                           TO  SPA-GMN-NI-JYOKEN3
           END-IF                                              
           IF      SPA-GMN-NI-JYOKEN4  =   ZERO
               MOVE    WRK-CONS-JYOKEN-TENSU
                                           TO  SPA-GMN-NI-JYOKEN4
           END-IF                                              
           IF      SPA-GMN-NI-JYOKEN5  =   ZERO
               MOVE    WRK-CONS-JYOKEN-TENSU
                                           TO  SPA-GMN-NI-JYOKEN5
           END-IF                                              
           IF      SPA-GMN-NI-JYOKEN6  =   ZERO
               MOVE    WRK-CONS-JYOKEN-TENSU
                                           TO  SPA-GMN-NI-JYOKEN6
           END-IF                                              
           IF     SPA-GMN-FRAME-ROUPRTKBN    =   SPACE
               MOVE    SPA-GMN-FRAME-ROUPRTKBNLST (1)
                                           TO
                                       SPA-GMN-FRAME-ROUPRTKBN-G
           END-IF                                              
           IF     SPA-GMN-FRAME-ROUPRTKBN-N  =   SPACE
               MOVE    SPA-GMN-FRAME-ROUPRTKBN-NLST (1)
                                           TO
                                       SPA-GMN-FRAME-ROUPRTKBN-N-G
           END-IF                                              
           IF     SPA-GMN-COMMENT-ROU    =   SPACE
               MOVE    SPA-GMN-COMMENT-ROULST (1)
                                           TO  SPA-GMN-COMMENT-ROU-G
           END-IF                                              
           IF     SPA-GMN-RSI-RECEIPTKBN   =   SPACE
               MOVE    SPA-GMN-RSI-RECEIPTKBNLST (1)
                                           TO  SPA-GMN-RSI-RECEIPTKBN-G
           END-IF
           IF     SPA-GMN-ROUPRTKBN      =   SPACE
               MOVE    SPA-GMN-ROUPRTKBNLST (1)
                                           TO  SPA-GMN-ROUPRTKBN-G
           END-IF                                              
           IF     SPA-GMN-ROUPRTKBN-N  =   SPACE
               MOVE    SPA-GMN-ROUPRTKBN-NLST (1)
                                           TO  SPA-GMN-ROUPRTKBN-N-G
           END-IF                                              
           IF     SPA-GMN-FRAME-JIBAIPRTKBN    =   SPACE
               MOVE    SPA-GMN-FRAME-JIBAIPRTKBNLST (1)
                                           TO
                                       SPA-GMN-FRAME-JIBAIPRTKBN-G
           END-IF                                              
           IF     SPA-GMN-FRAME-JIBAIPRTKBN-N  =   SPACE
               MOVE    SPA-GMN-FRAME-JIBAIPRTKBN-NLST (1)
                                           TO
                                       SPA-GMN-FRAME-JIBAIPRTKBN-N-G
           END-IF                                              
           IF     SPA-GMN-JIBAIPRTKBN      =   SPACE
               MOVE    SPA-GMN-JIBAIPRTKBNLST (2)
                                           TO  SPA-GMN-JIBAIPRTKBN-G
           END-IF                                              
           IF     SPA-GMN-JIBAIPRTKBN-N  =   SPACE
               MOVE    SPA-GMN-JIBAIPRTKBN-NLST (2)
                                           TO  SPA-GMN-JIBAIPRTKBN-N-G
           END-IF                                              
           IF     SPA-GMN-JIBAIPRTKBN1      =   SPACE
               MOVE    SPA-GMN-JIBAIPRTKBN1LST (3)
                                           TO  SPA-GMN-JIBAIPRTKBN1-G
           END-IF                                              
           IF     SPA-GMN-JIBAIPRTKBN1-N  =   SPACE
               MOVE    SPA-GMN-JIBAIPRTKBN1-NLST (3)
                                           TO  SPA-GMN-JIBAIPRTKBN1-N-G
           END-IF                                              
           IF     SPA-GMN-JIBAI-FRACTION-KBN1  =   SPACE
               MOVE    SPA-GMN-JIBAI-FRACTION-KBN1LST (1)
                                           TO
                                       SPA-GMN-JIBAI-FRACTION-KBN1-G
           END-IF                                              
           IF     SPA-GMN-JIBAI-FRACTION-KBN2  =   SPACE
               MOVE    SPA-GMN-JIBAI-FRACTION-KBN2LST (1)
                                           TO
                                       SPA-GMN-JIBAI-FRACTION-KBN2-G
           END-IF                                              
           IF     SPA-GMN-JIBAI-PDF    =   SPACE
               MOVE    SPA-GMN-JIBAI-PDFLST (1)
                                           TO  SPA-GMN-JIBAI-PDF-G
           END-IF
           IF     SPA-GMN-COMMENT-NYUINRYO-G   =   SPACE
               MOVE    SPA-GMN-COMMENT-NYUINRYOLST (2)
                                           TO
                                           SPA-GMN-COMMENT-NYUINRYO-G
           END-IF                                              
           IF     SPA-GMN-COMMENT-NYUINRYO1-G  =   SPACE
               MOVE    SPA-GMN-COMMENT-NYUINRYO1LST (3)
                                           TO
                                           SPA-GMN-COMMENT-NYUINRYO1-G
           END-IF                                              
           IF     SPA-GMN-CHKKBN-G     =   SPACE
               MOVE    SPA-GMN-CHKKBNLST (1)
                                           TO  SPA-GMN-CHKKBN-G
           END-IF                                              
           IF     SPA-GMN-BYOMEI-PRTJYUN-G
                                       =   SPACE
               MOVE    SPA-GMN-BYOMEI-PRTJYUNLST (1)
                                           TO  SPA-GMN-BYOMEI-PRTJYUN-G
           END-IF                                              
           IF     SPA-GMN-SHOKUJI-PRTKBN-G
                                       =   SPACE
               MOVE    SPA-GMN-SHOKUJI-PRTKBNLST (1)
                                           TO  SPA-GMN-SHOKUJI-PRTKBN-G
           END-IF                                              
           IF     SPA-GMN-INSPECT-CHARA-KBN-G
                                       =   SPACE
               MOVE    SPA-GMN-INSPECT-CHARA-KBNLST (1)
                                           TO
                                       SPA-GMN-INSPECT-CHARA-KBN-G
           END-IF                                              
           IF     SPA-GMN-INSPECT-ZERO-KBN-G
                                       =   SPACE
               MOVE    SPA-GMN-INSPECT-ZERO-KBNLST (1)
                                           TO SPA-GMN-INSPECT-ZERO-KBN-G
           END-IF                                              
           IF     SPA-GMN-INSPECT-INCLUDE-KBN-G
                                       =   SPACE
               MOVE    SPA-GMN-INSPECT-INCLUDE-KBNLST (1)
                                           TO
                                           SPA-GMN-INSPECT-INCLUDE-KBN-G
           END-IF                                              
           IF     SPA-GMN-INSPECT-GENERAL-KBN-G
                                       =   SPACE
               MOVE    SPA-GMN-INSPECT-GENERAL-KBNLST (1)
                                           TO
                                           SPA-GMN-INSPECT-GENERAL-KBN-G
           END-IF                                              
           IF     SPA-GMN-SEIHO-DSPKBN-G   =   SPACE
               MOVE    SPA-GMN-SEIHO-DSPKBNLST (1)
                                           TO  SPA-GMN-SEIHO-DSPKBN-G
           END-IF                                              
           IF     SPA-GMN-RECEIPT-MAKEKBN-G
                                           =   SPACE
               MOVE    SPA-GMN-RECEIPT-MAKEKBNLST  (1)
                                           TO
                                           SPA-GMN-RECEIPT-MAKEKBN-G
           END-IF                                              
           IF     SPA-GMN-ZERO-KOH-PRTKBN-G
                                           =   SPACE
               MOVE    SPA-GMN-ZERO-KOH-PRTKBNLST  (2)
                                           TO  SPA-GMN-ZERO-KOH-PRTKBN-G
           END-IF                                              
           IF     SPA-GMN-HKN-RIREKI-G     =   SPACE
               MOVE    SPA-GMN-HKN-RIREKILST  (1)
                                           TO  SPA-GMN-HKN-RIREKI-G
           END-IF                                              
           IF     SPA-GMN-GENBUTU-ICHIBU-G =   SPACE
               MOVE    SPA-GMN-GENBUTU-ICHIBULST  (1)
                                           TO  SPA-GMN-GENBUTU-ICHIBU-G
           END-IF                                              
           IF     SPA-GMN-COMPRESS-G =   SPACE
               MOVE    SPA-GMN-COMPRESSLST  (1)
                                           TO  SPA-GMN-COMPRESS-G
           END-IF
           IF     SPA-GMN-DISASTER-RECEIPT-G
                                           =   SPACE
               MOVE    SPA-GMN-DISASTER-RECEIPTLST  (1)
                                           TO
                                           SPA-GMN-DISASTER-RECEIPT-G
           END-IF
           IF     SPA-GMN-DISASTER-SYAPRTKBN-G
                                           =   SPACE
               MOVE    SPA-GMN-DISASTER-SYAPRTKBNLST  (1)
                                           TO
                                           SPA-GMN-DISASTER-SYAPRTKBN-G
           END-IF
           IF     SPA-GMN-15-YKZ-G         =   SPACE
               MOVE    SPA-GMN-15-YKZLST (1)
                                           TO  SPA-GMN-15-YKZ-G
           END-IF
           IF     SPA-GMN-KOUHI-ICHIBU-KSI-G
                                           =   SPACE
               MOVE    SPA-GMN-KOUHI-ICHIBU-KSILST (1)
                                           TO
                                           SPA-GMN-KOUHI-ICHIBU-KSI-G
           END-IF
      *
      *    労災医療機関情報
           INITIALIZE                      SYS-4001-REC
           MOVE    SPA-HOSPNUM         TO  SYS-4001-HOSPNUM
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               PERFORM UNTIL ( FLG-SYSKANRI        =   1   )
                   MOVE    MCPDATA-REC         TO  SYS-4001-REC
                   IF      SYS-4001-EDYUKYMD   <   "20130701"
                       CONTINUE
                   ELSE
                       IF      SYS-4001-RSIIRYOKBN =   "1"
                           MOVE    1           TO
                                               SPA-4001-RSIIRYOKBN-FLG
                       END-IF
                   END-IF
                   MOVE    "tbl_syskanri"    TO  MCP-TABLE
                   MOVE    "key8"            TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    入力前退避
           MOVE    SPA-GMN-KKHRECEKBN  TO  SPA-GMN-KKHRECEKBN-O 
           MOVE    SPA-GMN-SYARECEKBN  TO  SPA-GMN-SYARECEKBN-O 
           MOVE    SPA-GMN-KOUIKIRECEKBN
                                       TO  SPA-GMN-KOUIKIRECEKBN-O
           MOVE    SPA-GMN-KOUIKIKBN   TO  SPA-GMN-KOUIKIKBN-O
      *
           MOVE    1                   TO  FLG-HKNJANUMLIST-ROW
      *
           MOVE    1                   TO  SPA-GMN-CUR
           . 
       310-SPASET-EXT.
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
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    公費記載順設定
               WHEN    "CLICKED"       ALSO    "B34"
                   PERFORM 490-KOUHI-PRTJYUN-SETTEI-SEC
      *    詳細設定
               WHEN    "CLICKED"       ALSO    "B36"
                   PERFORM 490-SYOUSAI-SETTEI-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    削除（直接請求する保険者）
               WHEN    "CLICKED"       ALSO    "HKNJANUM_DEL"
                   PERFORM 490-HKNJANUM-DELETE-SEC
      *    更新（直接請求する保険者）
               WHEN    "CLICKED"       ALSO    "HKNJANUM_UPD"
                   PERFORM 490-HKNJANUM-UPDATE-SEC
      *    タグ
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 440-TAG-CHANGE-SEC
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        選択番号選択（直接請求する保険者）
               WHEN    ANY             ALSO    "HKNJANUMLIST"
                   PERFORM 420-HKNJANUMLIST-SEC
      *        タグ
               WHEN    "SWITCH"        ALSO    "note3"
                   PERFORM 440-TAG-CHANGE-SEC
                   GO  TO  200-ENTRY-EXT
           END-EVALUATE
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
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
      *    選択番号
           IF      W12-SELNUM          =   SPA-GMN-SELNUM-O
               MOVE    ZERO                TO  FLG-IN-SELNUM
           ELSE
               MOVE    1                   TO  FLG-IN-SELNUM
           END-IF
       *
           MOVE    W12-BYOMEIHENKBN    TO  SPA-GMN-BYOMEIHENKBN-G 
           MOVE    W12-KKHRECEKBN      TO  SPA-GMN-KKHRECEKBN-G 
           MOVE    W12-SYARECEKBN      TO  SPA-GMN-SYARECEKBN-G 
           MOVE    W12-KOUIKIRECEKBN   TO  SPA-GMN-KOUIKIRECEKBN-G 
           MOVE    W12-ROURECEKBN      TO  SPA-GMN-ROURECEKBN-G 
           MOVE    W12-RECEDENOUT      TO  SPA-GMN-RECEDENOUT 
           MOVE    W12-SYAOCRKBN       TO  SPA-GMN-SYAOCRKBN-G 
           MOVE    W12-KKHOCRKBN       TO  SPA-GMN-KKHOCRKBN-G 
           MOVE    W12-KOUIKIOCRKBN    TO  SPA-GMN-KOUIKIOCRKBN-G 
           MOVE    W12-NYUTAIINREKI    TO  SPA-GMN-NYUTAIIN-KSIKBN-G
           MOVE    W12-NYUTAIINREKI-NYU
                                       TO
                                       SPA-GMN-NYUTAIIN-NYU-KSIKBN-G
           MOVE    W12-PREFPRGID       TO  SPA-GMN-PREFPRGID
           MOVE    W12-PREFPRGID1      TO  SPA-GMN-PREFPRGID1
           MOVE    W12-KKHPRTKBN       TO  SPA-GMN-KKHPRTKBN-G 
           MOVE    W12-SYAPRTKBN       TO  SPA-GMN-SYAPRTKBN-G 
           MOVE    W12-DISASTER-SYAPRTKBN
                                       TO  SPA-GMN-DISASTER-SYAPRTKBN-G
           MOVE    W12-KOUIKIPRTKBN    TO  SPA-GMN-KOUIKIPRTKBN-G 
           MOVE    W12-RSI-INVOICEKBN  TO  SPA-GMN-RSI-INVOICEKBN-G 
           MOVE    W12-ROUSAIPRTKBN    TO  SPA-GMN-ROUSAIPRTKBN-G 
           MOVE    W12-FRAME-ROUSAIPRTKBN
                                       TO  SPA-GMN-FRAME-ROUSAIPRTKBN-G 
           MOVE    W12-BYOMEIHENKBN1   TO  SPA-GMN-BYOMEIHENKBN1-G 
           MOVE    W12-SYUBYOHENFLG    TO  SPA-GMN-SYUBYOHENFLG-G 
           MOVE    W12-TENKIHENKBN     TO  SPA-GMN-TENKIHENKBN-G 
           MOVE    W12-TENKIBYOMEI-KBN TO  SPA-GMN-TENKIBYOMEI-KBN-G 
           MOVE    W12-SYAPRTNM        TO  SPA-GMN-SYAPRTNM
           MOVE    W12-KKHPRTNM        TO  SPA-GMN-KKHPRTNM
           MOVE    W12-RENNUMHENKBN    TO  SPA-GMN-RENNUMHENKBN-G 
           MOVE    W12-NEXTPRTKBN      TO  SPA-GMN-NEXTPRTKBN-G 
           MOVE    W12-NEXTPRTKBN1     TO  SPA-GMN-NEXTPRTKBN1-G 
           MOVE    W12-RJNGNBKKBN      TO  SPA-GMN-RJNGNBKKBN-G 
           MOVE    W12-JIHITEKKSIKBN   TO  SPA-GMN-JIHITEKKSIKBN-G 
           MOVE    W12-KOUIKIKBN       TO  SPA-GMN-KOUIKIKBN-G 
           MOVE    W12-COMMENT-NYUINRYO
                                       TO  SPA-GMN-COMMENT-NYUINRYO-G 
           MOVE    W12-COMMENT-NYUINRYO1
                                       TO  SPA-GMN-COMMENT-NYUINRYO1-G 
           MOVE    W12-CHKKBN          TO  SPA-GMN-CHKKBN-G 
           MOVE    W12-BYOMEI-PRTJYUN  TO  SPA-GMN-BYOMEI-PRTJYUN-G 
           MOVE    W12-SHOKUJI-PRTKBN  TO  SPA-GMN-SHOKUJI-PRTKBN-G 
           MOVE    W12-INSPECT-CHARACTER-KBN  TO
                                       SPA-GMN-INSPECT-CHARA-KBN-G 
           MOVE    W12-INSPECT-ZERO-KBN
                                       TO  SPA-GMN-INSPECT-ZERO-KBN-G 
           MOVE    W12-INSPECT-INCLUDE-KBN
                                       TO  SPA-GMN-INSPECT-INCLUDE-KBN-G 
           MOVE    W12-INSPECT-GENERAL-KBN
                                       TO  SPA-GMN-INSPECT-GENERAL-KBN-G
           MOVE    W12-SEIHO-DSPKBN    TO  SPA-GMN-SEIHO-DSPKBN-G 
           MOVE    W12-RECEIPT-MAKE    TO  SPA-GMN-RECEIPT-MAKEKBN-G 
           MOVE    W12-ZERO-KOH-PRTKBN TO  SPA-GMN-ZERO-KOH-PRTKBN-G
           MOVE    W12-HKN-RIREKI      TO  SPA-GMN-HKN-RIREKI-G
           MOVE    W12-GENBUTU-ICHIBU  TO  SPA-GMN-GENBUTU-ICHIBU-G
           MOVE    W12-COMPRESS        TO  SPA-GMN-COMPRESS-G
           MOVE    W12-DISASTER-RECEIPT
                                       TO  SPA-GMN-DISASTER-RECEIPT-G
           MOVE    W12-15-YKZ          TO  SPA-GMN-15-YKZ-G
           MOVE    W12-KOUHI-ICHIBU-KSI
                                       TO  SPA-GMN-KOUHI-ICHIBU-KSI-G
           MOVE    W12-SYAPRTNM-NEXT   TO  SPA-GMN-SYAPRTNM-NEXT
           MOVE    W12-KKHPRTNM-NEXT   TO  SPA-GMN-KKHPRTNM-NEXT
           MOVE    W12-SRYKAHENKBN     TO  SPA-GMN-SRYKAHENKBN-G
           MOVE    W12-BTUNUM-KSIKBN   TO  SPA-GMN-BTUNUM-KSIKBN-G
           MOVE    W12-DRNAME-KSIKBN   TO  SPA-GMN-DRNAME-KSIKBN-G
           MOVE    W12-RECECOMKBN      TO  SPA-GMN-RECECOMKBN-G
           MOVE    W12-RECEDEN-CDRKBN  TO  SPA-GMN-RECEDEN-CDRKBN-G
           MOVE    W12-RECEDEN-LST1KBN TO  SPA-GMN-RECEDEN-LST1KBN-G
           MOVE    W12-RECEDEN-LST2KBN TO  SPA-GMN-RECEDEN-LST2KBN-G
           MOVE    W12-RECEDEN-LST2KBN-MEDIA
                                       TO
                                       SPA-GMN-RECEDEN-LST2-MEDIA-G
           MOVE    W12-RECEDEN-RIREKIKBN
                                       TO  SPA-GMN-RECEDEN-RIREKIKBN-G
           MOVE    W12-RECEDEN-DATAKBN TO  SPA-GMN-RECEDEN-DATAKBN-G
           MOVE    W12-RECEDEN-DISASTER TO  SPA-GMN-RECEDEN-DISASTER-G
           MOVE    W12-RECEDEN-CHK     TO  SPA-GMN-RECEDEN-CHK-G
           MOVE    W12-KIGO-DELI-FLG
                                       TO  SPA-GMN-KIGO-DELI-FLG-G
           MOVE    W12-KIGO-DELI-HKNJA-FLG
                                       TO
                                SPA-GMN-KIGO-DELI-HKNJA-FLG-G
           MOVE    W12-NUM-DELI-FLG
                                       TO  SPA-GMN-NUM-DELIMITER-FLG-G
           MOVE    W12-NUM-DELI-HKNJA-FLG
                                       TO
                                SPA-GMN-NUM-DELI-HKNJA-FLG-G
           MOVE    W12-SYAPRTNM-N      TO  SPA-GMN-SYAPRTNM-N
           MOVE    W12-KKHPRTNM-N      TO  SPA-GMN-KKHPRTNM-N
           MOVE    W12-SYAPRTNM-NEXT-N TO  SPA-GMN-SYAPRTNM-NEXT-N
           MOVE    W12-KKHPRTNM-NEXT-N TO  SPA-GMN-KKHPRTNM-NEXT-N
           MOVE    W12-AUTODSPKBN      TO  SPA-GMN-AUTODSPKBN-G
           MOVE    W12-PRT-CHECKKBN    TO  SPA-GMN-PRT-CHECKKBN-G
           MOVE    W12-JIHI-PRTKBN     TO  SPA-GMN-JIHI-PRTKBN-G
           MOVE    W12-PRTJYUNKBN1     TO  SPA-GMN-PRTJYUN1-G
           MOVE    W12-PRTJYUNKBN2     TO  SPA-GMN-PRTJYUN2-G
           MOVE    W12-PRTJYUNKBN3     TO  SPA-GMN-PRTJYUN3-G
           MOVE    W12-PRTJYUNKBN4     TO  SPA-GMN-PRTJYUN4-G
           MOVE    W12-PRTJYUNKBN5     TO  SPA-GMN-PRTJYUN5-G
           MOVE    W12-PRTJYUNKBN6     TO  SPA-GMN-PRTJYUN6-G
           MOVE    W12-KOUIKIPRTNM     TO  SPA-GMN-KOUIKIPRTNM
           MOVE    W12-KOUIKIPRTNM-NEXT
                                       TO  SPA-GMN-KOUIKIPRTNM-NEXT
           MOVE    W12-KOUIKIPRTNM-N   TO  SPA-GMN-KOUIKIPRTNM-N
           MOVE    W12-KOUIKIPRTNM-NEXT-N
                                       TO  SPA-GMN-KOUIKIPRTNM-NEXT-N
           MOVE    W12-SELNUM          TO  SPA-GMN-SELNUM
           MOVE    W12-HKNJANUM1       TO  SPA-GMN-HKNJANUM1
      *
           MOVE    W12-FRAME-ROU       TO  SPA-GMN-FRAME-ROUPRTKBN-G
           MOVE    W12-FRAME-ROU-N     TO  SPA-GMN-FRAME-ROUPRTKBN-N-G
           MOVE    W12-COMMENT-ROU     TO  SPA-GMN-COMMENT-ROU-G
           MOVE    W12-RSI-RECEIPTKBN  TO  SPA-GMN-RSI-RECEIPTKBN-G
           MOVE    W12-ROUPRTKBN       TO  SPA-GMN-ROUPRTKBN-G
           MOVE    W12-ROUPRTKBN-N     TO  SPA-GMN-ROUPRTKBN-N-G
           MOVE    W12-FRAME-JIBAI     TO  SPA-GMN-FRAME-JIBAIPRTKBN-G
           MOVE    W12-FRAME-JIBAI-N   TO  SPA-GMN-FRAME-JIBAIPRTKBN-N-G
           MOVE    W12-JIBAIPRTKBN     TO  SPA-GMN-JIBAIPRTKBN-G
           MOVE    W12-JIBAIPRTKBN-N   TO  SPA-GMN-JIBAIPRTKBN-N-G
           MOVE    W12-JIBAIPRTKBN1    TO  SPA-GMN-JIBAIPRTKBN1-G
           MOVE    W12-JIBAIPRTKBN1-N  TO  SPA-GMN-JIBAIPRTKBN1-N-G
           MOVE    W12-JIBAI-FRACTION-KBN1
                                       TO  SPA-GMN-JIBAI-FRACTION-KBN1-G
           MOVE    W12-JIBAI-FRACTION-KBN2
                                       TO  SPA-GMN-JIBAI-FRACTION-KBN2-G
           MOVE    W12-JIBAI-PDF       TO  SPA-GMN-JIBAI-PDF-G
           MOVE    W12-ROUPRTNM        TO  SPA-GMN-ROUPRTNM
           MOVE    W12-JBAIPRTNM       TO  SPA-GMN-JBAIPRTNM
           MOVE    W12-ROUPRTNM-NEXT   TO  SPA-GMN-ROUPRTNM-NEXT
           MOVE    W12-JBAIPRTNM-NEXT  TO  SPA-GMN-JBAIPRTNM-NEXT
           MOVE    W12-ROUPRTNM-N      TO  SPA-GMN-ROUPRTNM-N
           MOVE    W12-JBAIPRTNM-N     TO  SPA-GMN-JBAIPRTNM-N
           MOVE    W12-ROUPRTNM-NEXT-N TO  SPA-GMN-ROUPRTNM-NEXT-N
           MOVE    W12-JBAIPRTNM-NEXT-N
                                       TO  SPA-GMN-JBAIPRTNM-NEXT-N
      *
           MOVE    W12-MOJI-KISAIKBN   TO  SPA-GMN-MOJI-KISAIKBN-G
           MOVE    W12-KYURATE-KISAIKBN
                                       TO  SPA-GMN-KYURATE-KISAIKBN-G
           MOVE    W12-RECESYUBETU-KISAIKBN
                                       TO
                                       SPA-GMN-RECESYUBET-KISAIKBN-G
      *
           MOVE    W12-PREFKBN-PRTJYUN TO  SPA-GMN-PREFKBN-PRTJYUN-G
           MOVE    W12-HKNJANUM-PRTJYUN
                                       TO  SPA-GMN-HKNJANUM-PRTJYUN-G
           MOVE    W12-RECESYUBETU-PRTJYUN
                                       TO
                                       SPA-GMN-RECESYUBETU-PRTJYUN-G
           MOVE    W12-ZAITAKU         TO  SPA-GMN-ZAITAKU-G
           MOVE    W12-CHOUKI          TO  SPA-GMN-CHOUKI-G
           MOVE    W12-HKNJANUM-PRTKBN TO  SPA-GMN-HKNJANUM-PRTKBN-G
           MOVE    W12-HKNJANUM        TO  SPA-GMN-HKNJANUM
           MOVE    W12-DRCD-PRTKBN     TO  SPA-GMN-DRCD-PRTKBN-G
           MOVE    W12-DRCD-PRTJYUNKBN TO  SPA-GMN-DRCD-PRTJYUNKBN-G
           MOVE    W12-BTUNUM-PRTKBN   TO  SPA-GMN-BTUNUM-PRTKBN-G
           MOVE    W12-BTUNUM-PRTJYUNKBN TO  SPA-GMN-BTUNUM-PRTJYUNKBN-G
           MOVE    W12-TERMINAL-DIGIT-PRTKBN TO
                                         SPA-GMN-TERMINAL-DIG-PRTKBN-G
      *
           MOVE    W12-SYUKA-KBN       TO  SPA-GMN-SYUKA-KBN-G
           MOVE    W12-SYUKA-NYUIN     TO  SPA-GMN-SYUKA-NYUIN-G
           MOVE    W12-SYUKA-GAIRAI    TO  SPA-GMN-SYUKA-GAIRAI-G
           MOVE    W12-SYUKA-SELECT    TO  SPA-GMN-SYUKA-SELECT-G
           MOVE    W12-SYUKA-MODE      TO  SPA-GMN-SYUKA-MODE-G
           MOVE    W12-SYUKA-SYAHO     TO  SPA-GMN-SYUKA-SYAHO-G
           MOVE    W12-SYUKA-KOKUHO    TO  SPA-GMN-SYUKA-KOKUHO-G
           MOVE    W12-SYUKA-KOUIKI    TO  SPA-GMN-SYUKA-KOUIKI-G
           MOVE    W12-NI-JYOKEN1      TO  SPA-GMN-NI-JYOKEN1
           MOVE    W12-NI-JYOKEN2      TO  SPA-GMN-NI-JYOKEN2
           MOVE    W12-NI-JYOKEN3      TO  SPA-GMN-NI-JYOKEN3
           MOVE    W12-NI-JYOKEN4      TO  SPA-GMN-NI-JYOKEN4
           MOVE    W12-NI-JYOKEN5      TO  SPA-GMN-NI-JYOKEN5
           MOVE    W12-NI-JYOKEN6      TO  SPA-GMN-NI-JYOKEN6
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
           PERFORM 4100-BYOMEIHENKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4107-BYOMEIHENKBN1-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4114-SYUBYOHENFLG-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4108-TENKIHENKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4187-TENKIBYOMEI-KBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4119-SYAOCRKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4104-KKHOCRKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4155-KOUIKIOCRKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4111-RENNUMHEKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4144-NYUTAIIN-KSI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4144-NYUTAIIN-NYU-KSI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4121-SRYKAHENKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4151-BTUNUM-KSIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4190-DRNAME-KSIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4117-RJNGNBKKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4118-JIHITEKKSIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4163-COMMENT-NYUINRYO-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4194-COMMENT-NYUINRYO1-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4156-KOUIKIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4174-CHKKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4175-BYOMEI-PRTJYUN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4197-SHOKUJI-PRTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4181-INSPECT-CHARA-KBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4184-INSPECT-ZERO-KBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4192-INSPECT-INCLUDE-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4206-INSPECT-GENERAL-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4106-PREFPRGID-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4112-PREFPRGID1-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4122-AUTODSPKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4173-SEIHO-DSPKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4189-RECEIPT-MAKEKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4198-ZERO-KOH-PRTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4185-SELNUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4186-HKNJANUM1-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4152-PRT-CHECKKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4152-JIHI-PRTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4204-HKN-RIREKI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4205-GENBUTU-ICHIBU-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4207-COMPRESS-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4209-DISASTER-RECEIPT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4210-15-YKZ-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4212-KOUHI-ICHIBU-KSI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4150-PRTJYUN4-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4123-PRTJYUN1-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4135-PRTJYUN3-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4124-PRTJYUN2-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4157-PRTJYUN5-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4172-PRTJYUN6-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4109-SYAPRTNM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4115-SYAPRTNM-NEXT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4110-KKHPRTNM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4116-KKHPRTNM-NEXT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4114-NEXTPRTKBN1-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4113-NEXTPRTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4164-FRAME-ROUPRTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4165-FRAME-ROUPRTKBN-N-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4213-RSI-RECEIPTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4182-ROUPRTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4183-ROUPRTKBN-N-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4166-COMMENT-ROU-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4146-FRAME-JIBAIPRTKB-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4147-FRAME-JIBAIPRTKB-N-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4153-JIBAIPRTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4154-JIBAIPRTKBN-N-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4195-JIBAIPRTKBN1-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4196-JIBAIPRTKBN1-N-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4202-JIBAI-FRACTIONKBN1-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4203-JIBAI-FRACTIONKBN2-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4211-JIBAI-PDF-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4102-SYARECEKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-KKHRECEKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4158-KOUIKIRECEKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4193-ROURECEKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *****PERFORM 4103-RECEDENOUT-CHK-SEC
      *****IF      SPA-ERRCD       NOT =   SPACE
      *****    GO      TO      4102-KIHON-CHK-EXT
      *****END-IF
      *
           PERFORM 4120-RECECOMKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4140-NI-JYOKEN1-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4141-NI-JYOKEN2-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4142-NI-JYOKEN3-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4143-NI-JYOKEN4-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4159-NI-JYOKEN5-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4160-NI-JYOKEN6-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4167-RECEDEN-CDRKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4168-RECEDEN-LST1KBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4169-RECEDEN-LST2KBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4199-RECEDEN-LST2-MEDIA-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4168-RECEDEN-RIREKIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4169-RECEDEN-DATAKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4180-RECEDEN-DISASTER-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4188-RECEDEN-CHK-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4176-KIGO-DELIMITER-FLG-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4177-KIGO-DELI-HKNJA-F-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4178-NUM-DELIMITER-FLG-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4179-NUM-DELI-HKNJA-F-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4125-MOJI-KISAIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4126-KYURATE-KISAIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4127-RECESYU-KISAIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4128-PREFKBN-PRTJYUN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4129-HKNJANUM-PRTJYUN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4130-RECESYU-PRTJYUN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4131-ZAITAKU-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4132-CHOUKI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4133-HKNJANUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4134-HKNJANUM-PRTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4161-DRCD-PRTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4162-DRCD-PRTJYUNKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4170-BTUNUM-PRTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4171-BTUNUM-PRTJYUNKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4191-TERMINAL-DI-PRTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4145-SYAPRTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4208-DISASTER-SYAPRTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4105-KKHPRTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4146-KOUIKIPRTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4214-RSI-INVOICEKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4200-ROUSAIPRTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4201-FRAME-ROUSAIPRTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4135-SYUKA-KBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4136-SYUKA-NYUIN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4137-SYUKA-GAIRAI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4139-SYUKA-SELECT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4138-SYUKA-MODE-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4148-SYUKA-SYAHO-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4149-SYUKA-KOKUHO-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4150-SYUKA-KOUIKI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名編集区分ェック
      *****************************************************************
       4100-BYOMEIHENKBN-CHK-SEC       SECTION.
      *
      *
      **** MOVE    W12-BYOMEIHENKBN       TO  SPA-GMN-BYOMEIHENKBN-G 
      *
           IF      SPA-GMN-BYOMEIHENKBN   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-BYOMEIHENKBN-MAX
                   IF      SPA-GMN-BYOMEIHENKBN 
                                   =   SPA-GMN-BYOMEIHENKBNL (IDY)
                       MOVE    SPA-GMN-BYOMEIHENKBNLST (IDY)
                                           TO  SPA-GMN-BYOMEIHENKBN-G
                       MOVE    SPA-GMN-BYOMEIHENKBN-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *     
               IF      SPA-GMN-BYOMEIHENKBNMEI =   SPACE
                   MOVE    1                   TO  SPA-GMN-CUR
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF
           END-IF 
      *
           .
       4100-BYOMEIHENKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保レセ電出力区分チェック
      *****************************************************************
       4101-KKHRECEKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-KKHRECEKBN  =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-KKHRECEKBN-MAX
                   IF      SPA-GMN-KKHRECEKBN 
                                   =   SPA-GMN-KKHRECEKBNL (IDY)
                       MOVE    SPA-GMN-KKHRECEKBNLST (IDY)
                                           TO  SPA-GMN-KKHRECEKBN-G
                       MOVE    SPA-GMN-KKHRECEKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-KKHRECEKBNMEI   =   SPACE
                   MOVE    101                     TO  SPA-GMN-CUR
                   MOVE    "0002"                  TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4101-KKHRECEKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    社保レセ電出力区分チェック
      *****************************************************************
       4102-SYARECEKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-SYARECEKBN  =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-SYARECEKBN-MAX
                   IF      SPA-GMN-SYARECEKBN 
                                   =   SPA-GMN-SYARECEKBNL (IDY)
                       MOVE    SPA-GMN-SYARECEKBNLST (IDY)
                                           TO  SPA-GMN-SYARECEKBN-G
                       MOVE    SPA-GMN-SYARECEKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-SYARECEKBNMEI   =   SPACE
                   MOVE    100                     TO  SPA-GMN-CUR
                   MOVE    "0003"                  TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4102-SYARECEKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    広域連合レセ電出力区分チェック
      *****************************************************************
       4158-KOUIKIRECEKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-KOUIKIRECEKBN  =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-KOUIKIRECEKBN-MAX
                   IF      SPA-GMN-KOUIKIRECEKBN 
                                   =   SPA-GMN-KOUIKIRECEKBNL (IDY)
                       MOVE    SPA-GMN-KOUIKIRECEKBNLST (IDY)
                                           TO  SPA-GMN-KOUIKIRECEKBN-G
                       MOVE    SPA-GMN-KOUIKIRECEKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-KOUIKIRECEKBNMEI   =   SPACE
                   MOVE    108                     TO  SPA-GMN-CUR
                   MOVE    "0046"                  TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4158-KOUIKIRECEKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災レセ電出力区分チェック
      *****************************************************************
       4193-ROURECEKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-ROURECEKBN  =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-ROURECEKBN-MAX
                   IF      SPA-GMN-ROURECEKBN 
                                   =   SPA-GMN-ROURECEKBNL (IDY)
                       MOVE    SPA-GMN-ROURECEKBNLST (IDY)
                                           TO  SPA-GMN-ROURECEKBN-G
                       MOVE    SPA-GMN-ROURECEKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-ROURECEKBNMEI   =   SPACE
                   MOVE    306                     TO  SPA-GMN-CUR
                   MOVE    "0121"                  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF    ( SPA-4001-RSIIRYOKBN-FLG =   0             )
               AND   ( SPA-GMN-ROURECEKBN  NOT =   SPACE AND "0" )       
                   MOVE    306                     TO  SPA-GMN-CUR
                   MOVE    "0122"                  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4193-ROURECEKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電データ出力先チェック
      *****************************************************************
       4103-RECEDENOUT-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-RECEDENOUT  =   SPACE
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRKPARA
               STRING  SPA-GMN-RECEDENOUT  DELIMITED   BY  SPACE
                       "/W12.txt"          DELIMITED   BY  SIZE
                                           INTO    WRKPARA
               END-STRING
      *
               OPEN    INPUT               WRK-FILE
               IF      STS-WRK             =   "00"
                   CLOSE   WRK-FILE
               ELSE
                   OPEN    OUTPUT              WRK-FILE
                   IF      STS-WRK             =   "00"
                       CLOSE   WRK-FILE
                       MOVE    ZERO                TO  IF-RESULT
                       MOVE    WRKPARA             TO  IF-FILENAME
                       CALL    "orcfiledel"    USING   ORCSFDELAREA
                   ELSE
                       MOVE    102                 TO  SPA-GMN-CUR
                       MOVE    "1002"              TO  SPA-WIDCD
                   END-IF
               END-IF     
           END-IF     
      *
           .
       4103-RECEDENOUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保ＯＣＲ印刷区分ェック
      *****************************************************************
       4104-KKHOCRKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-KKHOCRKBN   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-KKHOCRKBN-MAX
                   IF      SPA-GMN-KKHOCRKBN
                                   =   SPA-GMN-KKHOCRKBNL (IDY)
                       MOVE    SPA-GMN-KKHOCRKBNLST (IDY)
                                           TO  SPA-GMN-KKHOCRKBN-G
                       MOVE    SPA-GMN-KKHOCRKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-KKHOCRKBNMEI    =   SPACE
                   MOVE    7                  TO  SPA-GMN-CUR
                   MOVE    "0004"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4104-KKHOCRKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    広域連合ＯＣＲ印刷区分ェック
      *****************************************************************
       4155-KOUIKIOCRKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-KOUIKIOCRKBN   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-KOUIKIOCRKBN-MAX
                   IF      SPA-GMN-KOUIKIOCRKBN
                                   =   SPA-GMN-KOUIKIOCRKBNL (IDY)
                       MOVE    SPA-GMN-KOUIKIOCRKBNLST (IDY)
                                           TO  SPA-GMN-KOUIKIOCRKBN-G
                       MOVE    SPA-GMN-KOUIKIOCRKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-KOUIKIOCRKBNMEI =   SPACE
                   MOVE    43                      TO  SPA-GMN-CUR
                   MOVE    "0047"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4155-KOUIKIOCRKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    県単用項目編集プログラム名チェック
      *****************************************************************
       4106-PREFPRGID-CHK-SEC       SECTION.
      *
      *
           IF      SPA-GMN-PREFPRGID      =   SPACE
               CONTINUE
           ELSE     
               IF      SPA-GMN-PREFPRGID(1:1) =   "S"
                   CONTINUE
               ELSE
                  MOVE    8                      TO  SPA-GMN-CUR
                  MOVE    "0006"                 TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4106-PREFPRGID-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    県単用項目編集プログラム名（入院用）チェック
      *****************************************************************
       4112-PREFPRGID1-CHK-SEC       SECTION.
      *
      *
           IF      SPA-GMN-PREFPRGID1     =   SPACE
               CONTINUE
           ELSE     
               IF      SPA-GMN-PREFPRGID1(1:1) =   "S"
                   CONTINUE
               ELSE
                  MOVE    13                     TO  SPA-GMN-CUR
                  MOVE    "0006"                 TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4112-PREFPRGID1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保診療報酬請求書印刷分チェック
      *****************************************************************
       4105-KKHPRTKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-KKHPRTKBN   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-KKHPRTKBN-MAX
                   IF      SPA-GMN-KKHPRTKBN
                                   =   SPA-GMN-KKHPRTKBNL (IDY)
                       MOVE    SPA-GMN-KKHPRTKBNLST (IDY)
                                           TO  SPA-GMN-KKHPRTKBN-G
                       MOVE    SPA-GMN-KKHPRTKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-KKHPRTKBNMEI    =   SPACE
                   MOVE    11                  TO  SPA-GMN-CUR
                   MOVE    "0005"                  TO  SPA-ERRCD
               END-IF
           END-IF     
      *
           .
       4105-KKHPRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名編集区分１チェック
      *****************************************************************
       4107-BYOMEIHENKBN1-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-BYOMEIHENKBN1   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-BYOMEIHENKBN1-MAX
                   IF      SPA-GMN-BYOMEIHENKBN1 
                                   =   SPA-GMN-BYOMEIHENKBN1L (IDY)
                       MOVE    SPA-GMN-BYOMEIHENKBN1LST (IDY)
                                           TO  SPA-GMN-BYOMEIHENKBN1-G
                       MOVE    SPA-GMN-BYOMEIHENKBN1-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-BYOMEIHENKBN1MEI =   SPACE
                   MOVE    2                       TO  SPA-GMN-CUR
                   MOVE    "0001"                  TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4107-BYOMEIHENKBN1-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    主病名編集フラグチェック
      *****************************************************************
       4114-SYUBYOHENFLG-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-SYUBYOHENFLG   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-SYUBYOHENFLG-MAX
                   IF      SPA-GMN-SYUBYOHENFLG 
                                   =   SPA-GMN-SYUBYOHENFLGL (IDY)
                       MOVE    SPA-GMN-SYUBYOHENFLGLST (IDY)
                                           TO  SPA-GMN-SYUBYOHENFLG-G
                       MOVE    SPA-GMN-SYUBYOHENFLG-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-SYUBYOHENFLGMEI =   SPACE
                   MOVE    15                      TO  SPA-GMN-CUR
                   MOVE    "0010"                  TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4114-SYUBYOHENFLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    転帰日編集区分チェック
      *****************************************************************
       4108-TENKIHENKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-TENKIHENKBN     =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-TENKIHENKBN-MAX
                   IF      SPA-GMN-TENKIHENKBN 
                                   =   SPA-GMN-TENKIHENKBNL (IDY)
                       MOVE    SPA-GMN-TENKIHENKBNLST (IDY)
                                           TO  SPA-GMN-TENKIHENKBN-G
                       MOVE    SPA-GMN-TENKIHENKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *      
               IF      SPA-GMN-TENKIHENKBNMEI =   SPACE
                   MOVE    3                   TO  SPA-GMN-CUR
                   MOVE    "0007"                  TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4108-TENKIHENKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    社保出力先プリンタ名チェック
      *****************************************************************
       4109-SYAPRTNM-CHK-SEC       SECTION.
      *
           .
       4109-SYAPRTNM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保出力先プリンタ名チェック
      *****************************************************************
       4110-KKHPRTNM-CHK-SEC       SECTION.
      *
           .
       4110-KKHPRTNM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    通し番号編集区分チェック
      *****************************************************************
       4111-RENNUMHEKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-RENNUMHENKBN    =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-RENNUMHENKBN-MAX
                   IF      SPA-GMN-RENNUMHENKBN
                                   =   SPA-GMN-RENNUMHENKBNL (IDY)
                       MOVE    SPA-GMN-RENNUMHENKBNLST (IDY)
                                           TO  SPA-GMN-RENNUMHENKBN-G
                       MOVE    SPA-GMN-RENNUMHENKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-RENNUMHENKBNMEI    =   SPACE
                   MOVE    12                  TO  SPA-GMN-CUR
                   MOVE    "0008"                  TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4111-RENNUMHEKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    続紙区分（入院）チェック
      *****************************************************************
       4113-NEXTPRTKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-NEXTPRTKBN    =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-NEXTPRTKBN-MAX
                   IF      SPA-GMN-NEXTPRTKBN
                                   =   SPA-GMN-NEXTPRTKBNL (IDY)
                       MOVE    SPA-GMN-NEXTPRTKBNLST (IDY)
                                           TO  SPA-GMN-NEXTPRTKBN-G
                       MOVE    SPA-GMN-NEXTPRTKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-NEXTPRTKBNMEI    =   SPACE
                   MOVE    14                 TO  SPA-GMN-CUR
                   MOVE    "0009"                  TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4113-NEXTPRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    続紙区分（入院外）チェック
      *****************************************************************
       4114-NEXTPRTKBN1-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-NEXTPRTKBN1    =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-NEXTPRTKBN1-MAX
                   IF      SPA-GMN-NEXTPRTKBN1
                                   =   SPA-GMN-NEXTPRTKBN1L (IDY)
                       MOVE    SPA-GMN-NEXTPRTKBN1LST (IDY)
                                           TO  SPA-GMN-NEXTPRTKBN1-G
                       MOVE    SPA-GMN-NEXTPRTKBN1-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-NEXTPRTKBN1MEI    =   SPACE
                   MOVE    16                 TO  SPA-GMN-CUR
                   MOVE    "0009"                  TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4114-NEXTPRTKBN1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    社保出力先プリンタ名（続紙）チェック
      *****************************************************************
       4115-SYAPRTNM-NEXT-CHK-SEC       SECTION.
      *
           .
       4115-SYAPRTNM-NEXT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保出力先プリンタ名（続紙）チェック
      *****************************************************************
       4116-KKHPRTNM-NEXT-CHK-SEC       SECTION.
      *
           .
       4116-KKHPRTNM-NEXT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人と原爆区分チェック
      *****************************************************************
       4117-RJNGNBKKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-RJNGNBKKBN    =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-RJNGNBKKBN-MAX
                        OR     SPA-GMN-RJNGNBKKBN
                                   =       SPA-GMN-RJNGNBKKBNL (IDY)
                   CONTINUE
               END-PERFORM
      * 
               IF      IDY <=      SPA-GMN-RJNGNBKKBN-MAX
                   MOVE    SPA-GMN-RJNGNBKKBNLST (IDY)
                                              TO  SPA-GMN-RJNGNBKKBN-G
               ELSE
                   MOVE    19                 TO  SPA-GMN-CUR
                   MOVE    "0011"             TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4117-RJNGNBKKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費レセ摘要欄記載区分チェック
      *****************************************************************
       4118-JIHITEKKSIKBN-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-JIHITEKKSIKBN    =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-JIHITEKKSIKBN-MAX
                        OR     SPA-GMN-JIHITEKKSIKBN
                                   =       SPA-GMN-JIHITEKKSIKBNL (IDY)
                   CONTINUE
               END-PERFORM
      * 
               IF      IDY <=      SPA-GMN-JIHITEKKSIKBN-MAX
                   MOVE    SPA-GMN-JIHITEKKSIKBNLST (IDY)
                                          TO  SPA-GMN-JIHITEKKSIKBN-G
               ELSE
                   MOVE    20             TO  SPA-GMN-CUR
                   MOVE    "0012"         TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4118-JIHITEKKSIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    広域連合作成区分チェック
      *****************************************************************
       4156-KOUIKIKBN-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-KOUIKIKBN    =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-KOUIKIKBN-MAX
                        OR     SPA-GMN-KOUIKIKBN
                                   =       SPA-GMN-KOUIKIKBNL (IDY)
                   CONTINUE
               END-PERFORM
      * 
               IF      IDY <=      SPA-GMN-KOUIKIKBN-MAX
                   MOVE    SPA-GMN-KOUIKIKBNLST (IDY)
                                          TO  SPA-GMN-KOUIKIKBN-G
               ELSE
                   MOVE    44             TO  SPA-GMN-CUR
                   MOVE    "0049"         TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4156-KOUIKIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料未算定時コメント記載区分チェック
      *****************************************************************
       4163-COMMENT-NYUINRYO-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-COMMENT-NYUINRYO    =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-COMMENT-NYUINRYO-MAX
                        OR     SPA-GMN-COMMENT-NYUINRYO
                                   =   SPA-GMN-COMMENT-NYUINRYOL (IDY)
                   CONTINUE
               END-PERFORM
      * 
               IF      IDY <=      SPA-GMN-COMMENT-NYUINRYO-MAX
                   MOVE    SPA-GMN-COMMENT-NYUINRYOLST (IDY)
                                          TO  SPA-GMN-COMMENT-NYUINRYO-G
               ELSE
                   MOVE    60             TO  SPA-GMN-CUR
                   MOVE    "0054"         TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4163-COMMENT-NYUINRYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料他保険算定時コメント記載区分チェック
      *****************************************************************
       4194-COMMENT-NYUINRYO1-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-COMMENT-NYUINRYO1    =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-COMMENT-NYUINRYO1-MAX
                        OR     SPA-GMN-COMMENT-NYUINRYO1
                                   =   SPA-GMN-COMMENT-NYUINRYO1L (IDY)
                   CONTINUE
               END-PERFORM
      * 
               IF      IDY <=      SPA-GMN-COMMENT-NYUINRYO1-MAX
                   MOVE    SPA-GMN-COMMENT-NYUINRYO1LST (IDY)
                                          TO
                                          SPA-GMN-COMMENT-NYUINRYO1-G
               ELSE
                   MOVE    307            TO  SPA-GMN-CUR
                   MOVE    "0077"         TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4194-COMMENT-NYUINRYO1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック区分チェック
      *****************************************************************
       4174-CHKKBN-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-CHKKBN    =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-CHKKBN-MAX
                        OR     SPA-GMN-CHKKBN
                                   =   SPA-GMN-CHKKBNL (IDY)
                   CONTINUE
               END-PERFORM
      * 
               IF      IDY <=      SPA-GMN-CHKKBN-MAX
                   MOVE    SPA-GMN-CHKKBNLST (IDY)
                                          TO  SPA-GMN-CHKKBN-G
               ELSE
                   MOVE    64             TO  SPA-GMN-CUR
                   MOVE    "0065"         TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4174-CHKKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名記載順区分チェック
      *****************************************************************
       4175-BYOMEI-PRTJYUN-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-BYOMEI-PRTJYUN    =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-BYOMEI-PRTJYUN-MAX
                        OR     SPA-GMN-BYOMEI-PRTJYUN
                                   =   SPA-GMN-BYOMEI-PRTJYUNL (IDY)
                   CONTINUE
               END-PERFORM
      * 
               IF      IDY <=      SPA-GMN-BYOMEI-PRTJYUN-MAX
                   MOVE    SPA-GMN-BYOMEI-PRTJYUNLST (IDY)
                                          TO  SPA-GMN-BYOMEI-PRTJYUN-G
               ELSE
                   MOVE    65             TO  SPA-GMN-CUR
                   MOVE    "0066"         TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD           =   SPACE
               IF    ( SPA-GMN-BYOMEI-PRTJYUN  =   "2" )
               AND   ( SPA-GMN-SYUBYOHENFLG
                                           NOT =   "0" )
                   MOVE    "1003"          TO  SPA-WIDCD
               END-IF 
           END-IF
           .
       4175-BYOMEI-PRTJYUN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    食事欄記載区分チェック
      *****************************************************************
       4197-SHOKUJI-PRTKBN-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-SHOKUJI-PRTKBN    =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-SHOKUJI-PRTKBN-MAX
                        OR     SPA-GMN-SHOKUJI-PRTKBN
                                   =   SPA-GMN-SHOKUJI-PRTKBNL (IDY)
                   CONTINUE
               END-PERFORM
      * 
               IF      IDY <=      SPA-GMN-SHOKUJI-PRTKBN-MAX
                   MOVE    SPA-GMN-SHOKUJI-PRTKBNLST (IDY)
                                          TO  SPA-GMN-SHOKUJI-PRTKBN-G
               ELSE
                   MOVE    308            TO  SPA-GMN-CUR
                   MOVE    "0080"         TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4197-SHOKUJI-PRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点検用レセプト印字文字設定区分チェック
      *****************************************************************
       4181-INSPECT-CHARA-KBN-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-INSPECT-CHARA-KBN
                                   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-INSPECT-CHARA-KBN-MAX
                        OR     SPA-GMN-INSPECT-CHARA-KBN
                                   =
                                   SPA-GMN-INSPECT-CHARA-KBNL (IDY)
                   CONTINUE
               END-PERFORM
      * 
               IF      IDY <=      SPA-GMN-INSPECT-CHARA-KBN-MAX
                   MOVE    SPA-GMN-INSPECT-CHARA-KBNLST (IDY)
                                          TO
                                      SPA-GMN-INSPECT-CHARA-KBN-G
               ELSE
                   MOVE    66             TO  SPA-GMN-CUR
                   MOVE    "0067"         TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4181-INSPECT-CHARA-KBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点検用レセプト０点公費記載区分チェック
      *****************************************************************
       4184-INSPECT-ZERO-KBN-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-INSPECT-ZERO-KBN
                                   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-INSPECT-ZERO-KBN-MAX
                        OR     SPA-GMN-INSPECT-ZERO-KBN
                                   =
                                   SPA-GMN-INSPECT-ZERO-KBNL (IDY)
                   CONTINUE
               END-PERFORM
      * 
               IF      IDY <=      SPA-GMN-INSPECT-ZERO-KBN-MAX
                   MOVE    SPA-GMN-INSPECT-ZERO-KBNLST (IDY)
                                          TO
                                      SPA-GMN-INSPECT-ZERO-KBN-G
               ELSE
                   MOVE    67             TO  SPA-GMN-CUR
                   MOVE    "0070"         TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4184-INSPECT-ZERO-KBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点検用レセプト０点公費記載区分チェック
      *****************************************************************
       4192-INSPECT-INCLUDE-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-INSPECT-INCLUDE-KBN
                                   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-INSPECT-INCLUDE-KBN-MAX
                        OR     SPA-GMN-INSPECT-INCLUDE-KBN
                                   =
                                   SPA-GMN-INSPECT-INCLUDE-KBNL (IDY)
                   CONTINUE
               END-PERFORM
      * 
               IF      IDY <=      SPA-GMN-INSPECT-INCLUDE-KBN-MAX
                   MOVE    SPA-GMN-INSPECT-INCLUDE-KBNLST (IDY)
                                          TO
                                      SPA-GMN-INSPECT-INCLUDE-KBN-G
               ELSE
                   MOVE    305            TO  SPA-GMN-CUR
                   MOVE    "0120"         TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4192-INSPECT-INCLUDE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点検用レセプト一般名処方記載区分チェック
      *****************************************************************
       4206-INSPECT-GENERAL-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-INSPECT-GENERAL-KBN
                                   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-INSPECT-GENERAL-KBN-MAX
                        OR     SPA-GMN-INSPECT-GENERAL-KBN
                                   =
                                   SPA-GMN-INSPECT-GENERAL-KBNL (IDY)
                   CONTINUE
               END-PERFORM
      * 
               IF      IDY         <=  SPA-GMN-INSPECT-GENERAL-KBN-MAX
                   MOVE    SPA-GMN-INSPECT-GENERAL-KBNLST (IDY)
                                       TO  SPA-GMN-INSPECT-GENERAL-KBN-G
               ELSE
                   MOVE    402         TO  SPA-GMN-CUR
                   MOVE    "0132"      TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4206-INSPECT-GENERAL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    生保等まとめ入力画面表示順チェック
      *****************************************************************
       4173-SEIHO-DSPKBN-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-SEIHO-DSPKBN    =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-SEIHO-DSPKBN-MAX
                        OR     SPA-GMN-SEIHO-DSPKBN
                                   =   SPA-GMN-SEIHO-DSPKBNL (IDY)
                   CONTINUE
               END-PERFORM
      * 
               IF      IDY <=      SPA-GMN-SEIHO-DSPKBN-MAX
                   MOVE    SPA-GMN-SEIHO-DSPKBNLST (IDY)
                                          TO  SPA-GMN-SEIHO-DSPKBN-G
               ELSE
                   MOVE    62             TO  SPA-GMN-CUR
                   MOVE    "0062"         TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4173-SEIHO-DSPKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト作成区分チェック
      *****************************************************************
       4189-RECEIPT-MAKEKBN-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-RECEIPT-MAKEKBN
                                           =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-RECEIPT-MAKEKBN-MAX
                        OR     SPA-GMN-RECEIPT-MAKEKBN
                                   =   SPA-GMN-RECEIPT-MAKEKBNL (IDY)
                   CONTINUE
               END-PERFORM
      * 
               IF      IDY <=      SPA-GMN-RECEIPT-MAKEKBN-MAX
                   MOVE    SPA-GMN-RECEIPT-MAKEKBNLST (IDY)
                                          TO  SPA-GMN-RECEIPT-MAKEKBN-G
               ELSE
                   MOVE    302            TO  SPA-GMN-CUR
                   MOVE    "0116"          TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4189-RECEIPT-MAKEKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ０点全国公費レセプト記載区分（提出用レセプト）チェック
      *****************************************************************
       4198-ZERO-KOH-PRTKBN-CHK-SEC        SECTION.
      *
           IF      SPA-GMN-ZERO-KOH-PRTKBN =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-ZERO-KOH-PRTKBN-MAX
                        OR     SPA-GMN-ZERO-KOH-PRTKBN
                                   =   SPA-GMN-ZERO-KOH-PRTKBNL (IDY)
                   CONTINUE
               END-PERFORM
      * 
               IF      IDY <=      SPA-GMN-ZERO-KOH-PRTKBN-MAX
                   MOVE    SPA-GMN-ZERO-KOH-PRTKBNLST (IDY)
                                           TO
                                           SPA-GMN-ZERO-KOH-PRTKBN-G
               ELSE
                   MOVE    309             TO  SPA-GMN-CUR
                   MOVE    "0124"          TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4198-ZERO-KOH-PRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       4185-SELNUM-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-SELNUM  =   ZERO
               MOVE    SPA-GMN-SELNUM  TO  SPA-GMN-SELNUM-O
               GO  TO  4185-SELNUM-CHK-EXT
           END-IF
      *
           IF      FLG-IN-SELNUM   =   ZERO
               GO  TO  4185-SELNUM-CHK-EXT
            END-IF
      *
      *    選択番号チェック
           IF    ( SPA-GMN-SELNUM  >=  1                        )
           AND   ( SPA-GMN-SELNUM  <=  SPA-GMN-HKNJALIST-MAX )
               PERFORM 41851-SELNUM-HENSYU-SEC
           ELSE
               MOVE    SPACE           TO  SPA-GMN-HKNJANUM1
                                           SPA-GMN-HKNJANUM1-63
                                           SPA-GMN-HKNJANAME1
               MOVE    ZERO            TO  SPA-GMN-SELNUM 
               MOVE    "0071"          TO  SPA-ERRCD
               MOVE    68              TO  SPA-GMN-CUR
           END-IF
           .
       4185-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号編集処理
      *****************************************************************
       41851-SELNUM-HENSYU-SEC         SECTION.
      *
           MOVE    SPA-GMN-SELNUM      TO  SPA-GMN-SELNUM-O
           MOVE    SPA-GMN-HKNJALIST-THKNJANUM (SPA-GMN-SELNUM)
                                       TO  SPA-GMN-HKNJANUM1
           MOVE    SPA-GMN-HKNJALIST-THKNJANUM-63 (SPA-GMN-SELNUM)
                                       TO  SPA-GMN-HKNJANUM1-63
           MOVE    SPA-GMN-HKNJALIST-THKNJANAME(SPA-GMN-SELNUM)
                                       TO  SPA-GMN-HKNJANAME1
      *
           MOVE    SPA-GMN-SELNUM  TO  W12-HKNJANUMLIST-ROW
           .
       41851-SELNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    直接請求する保険者番号チェック
      *****************************************************************
       4186-HKNJANUM1-CHK-SEC   SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-HKNJANAME1
                                           SPA-GMN-HKNJANUM1-63
      *
           IF      SPA-GMN-HKNJANUM1   =   SPACE
               IF      WRK-MCP-WIDGET      =   "HKNJANUM1"
                   MOVE    69                  TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    SPACE               TO  HKNJAINF-REC
               INITIALIZE                      HKNJAINF-REC
               MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
               MOVE    SPA-GMN-HKNJANUM1   TO  HKNJA-HKNJANUM
               MOVE    HKNJAINF-REC        TO  MCPDATA-REC
               PERFORM 900-HKNJAINF-READ-SEC
               IF      FLG-HKNJAINF        =   ZERO
                   MOVE    HKNJA-HKNJANAME     TO  SPA-GMN-HKNJANAME1
      *            健康保険組合か地方職員共済組合かチェック
                   IF    ( HKNJA-HKNJANUM(1:2) =   "06"    OR  "32"  )
                   AND   ( HKNJA-HKNNUM        =   "006"   OR  "032" )
                       CONTINUE
                   ELSE
                       MOVE    69                  TO  SPA-GMN-CUR
                       MOVE    "0074"              TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    69                  TO  SPA-GMN-CUR
                   MOVE    "0072"              TO  SPA-ERRCD
               END-IF
      *
               IF      SPA-ERRCD           =   SPACE
                   IF      SPA-GMN-SELNUM      =   ZERO
      *                既に登録済かチェック
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >   SPA-GMN-HKNJALIST-MAX
                           IF      SPA-GMN-HKNJANUM1   =
                                       SPA-GMN-HKNJALIST-THKNJANUM
                                                                  (IDX)
                               MOVE    IDX         TO  SPA-GMN-SELNUM
                               PERFORM 41851-SELNUM-HENSYU-SEC
                           END-IF
                       END-PERFORM
                   END-IF
      *            特定健康保険組合の存在チェック（健康保険組合のみ）
                   IF      SPA-GMN-HKNJANUM1-63    =   SPACE
                       MOVE    SPACE               TO  HKNJAINF-REC
                       INITIALIZE                      HKNJAINF-REC
                       MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
                       MOVE    SPA-GMN-HKNJANUM1   TO  HKNJA-HKNJANUM
                       IF      HKNJA-HKNJANUM(1:2) =   "06"
                           MOVE    "63"                TO
                                                   HKNJA-HKNJANUM (1:2)
                           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
                           PERFORM 900-HKNJAINF-READ-SEC
                           IF      FLG-HKNJAINF        =   ZERO
                               MOVE    HKNJA-HKNJANUM      TO
                                                   SPA-GMN-HKNJANUM1-63
                           END-IF 
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       4186-HKNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    転帰済傷病名記載区分チェック
      *****************************************************************
       4187-TENKIBYOMEI-KBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-TENKIBYOMEI-KBN =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-TENKIBYOMEI-KBN-MAX
                   IF      SPA-GMN-TENKIBYOMEI-KBN 
                                   =   SPA-GMN-TENKIBYOMEI-KBNL (IDY)
                       MOVE    SPA-GMN-TENKIBYOMEI-KBNLST (IDY)
                                           TO  SPA-GMN-TENKIBYOMEI-KBN-G
                       MOVE    SPA-GMN-TENKIBYOMEI-KBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *      
               IF      SPA-GMN-TENKIBYOMEI-KBNMEI  =   SPACE
                   MOVE    301                 TO  SPA-GMN-CUR
                   MOVE    "0114"              TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4187-TENKIBYOMEI-KBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    社保ＯＣＲ印刷区分ェック
      *****************************************************************
       4119-SYAOCRKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-SYAOCRKBN   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-SYAOCRKBN-MAX
                   IF      SPA-GMN-SYAOCRKBN
                                   =   SPA-GMN-SYAOCRKBNL (IDY)
                       MOVE    SPA-GMN-SYAOCRKBNLST (IDY)
                                           TO  SPA-GMN-SYAOCRKBN-G
                       MOVE    SPA-GMN-SYAOCRKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-SYAOCRKBNMEI    =   SPACE
                   MOVE    21                      TO  SPA-GMN-CUR
                   MOVE    "0063"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4119-SYAOCRKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトコメント（症状詳記）記録区分チェック
      *****************************************************************
       4120-RECECOMKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-RECECOMKBN   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-RECECOMKBN-MAX
                   IF      SPA-GMN-RECECOMKBN
                                   =   SPA-GMN-RECECOMKBNL (IDY)
                       MOVE    SPA-GMN-RECECOMKBNLST (IDY)
                                           TO  SPA-GMN-RECECOMKBN-G
                       MOVE    SPA-GMN-RECECOMKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-RECECOMKBNMEI   =   SPACE
                   MOVE    103                     TO  SPA-GMN-CUR
                   MOVE    "0013"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4120-RECECOMKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科編集区分チェック
      *****************************************************************
       4121-SRYKAHENKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-SRYKAHENKBN   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-SRYKAHENKBN-MAX
                   IF      SPA-GMN-SRYKAHENKBN
                                   =   SPA-GMN-SRYKAHENKBNL (IDY)
                       MOVE    SPA-GMN-SRYKAHENKBNLST (IDY)
                                           TO  SPA-GMN-SRYKAHENKBN-G
                       MOVE    SPA-GMN-SRYKAHENKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-SRYKAHENKBNMEI  =   SPACE
                   MOVE    23                      TO  SPA-GMN-CUR
                   MOVE    "0014"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4121-SRYKAHENKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟番号記載区分（入院）チェック
      *****************************************************************
       4151-BTUNUM-KSIKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-BTUNUM-KSIKBN   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-BTUNUM-KSIKBN-MAX
                   IF      SPA-GMN-BTUNUM-KSIKBN
                                   =   SPA-GMN-BTUNUM-KSIKBNL (IDY)
                       MOVE    SPA-GMN-BTUNUM-KSIKBNLST (IDY)
                                           TO  SPA-GMN-BTUNUM-KSIKBN-G
                       MOVE    SPA-GMN-BTUNUM-KSIKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-BTUNUM-KSIKBNMEI  =   SPACE
                   MOVE    41                      TO  SPA-GMN-CUR
                   MOVE    "0042"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4151-BTUNUM-KSIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    医師名記載区分チェック
      *****************************************************************
       4190-DRNAME-KSIKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-DRNAME-KSIKBN   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-DRNAME-KSIKBN-MAX
                   IF      SPA-GMN-DRNAME-KSIKBN
                                   =   SPA-GMN-DRNAME-KSIKBNL (IDY)
                       MOVE    SPA-GMN-DRNAME-KSIKBNLST (IDY)
                                           TO  SPA-GMN-DRNAME-KSIKBN-G
                       MOVE    SPA-GMN-DRNAME-KSIKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-DRNAME-KSIKBNMEI  =   SPACE
                   MOVE    303                     TO  SPA-GMN-CUR
                   MOVE    "0117"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4190-DRNAME-KSIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動遷移区分チェック
      *****************************************************************
       4122-AUTODSPKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-AUTODSPKBN   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-AUTODSPKBN-MAX
                   IF      SPA-GMN-AUTODSPKBN
                                   =   SPA-GMN-AUTODSPKBNL (IDY)
                       MOVE    SPA-GMN-AUTODSPKBNLST (IDY)
                                           TO  SPA-GMN-AUTODSPKBN-G
                       MOVE    SPA-GMN-AUTODSPKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-AUTODSPKBNMEI  =   SPACE
                   MOVE    28                      TO  SPA-GMN-CUR
                   MOVE    "0015"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4122-AUTODSPKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷後チェック解除区分チェック
      *****************************************************************
       4152-PRT-CHECKKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-PRT-CHECKKBN   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PRT-CHECKKBN-MAX
                   IF      SPA-GMN-PRT-CHECKKBN
                                   =   SPA-GMN-PRT-CHECKKBNL (IDY)
                       MOVE    SPA-GMN-PRT-CHECKKBNLST (IDY)
                                           TO  SPA-GMN-PRT-CHECKKBN-G
                       MOVE    SPA-GMN-PRT-CHECKKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-PRT-CHECKKBNMEI  =   SPACE
                   MOVE    42                      TO  SPA-GMN-CUR
                   MOVE    "0043"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4152-PRT-CHECKKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費レセプト印刷区分チェック
      *****************************************************************
       4152-JIHI-PRTKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-JIHI-PRTKBN   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-JIHI-PRTKBN-MAX
                   IF      SPA-GMN-JIHI-PRTKBN
                                   =   SPA-GMN-JIHI-PRTKBNL (IDY)
                       MOVE    SPA-GMN-JIHI-PRTKBNLST (IDY)
                                           TO  SPA-GMN-JIHI-PRTKBN-G
                       MOVE    SPA-GMN-JIHI-PRTKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-JIHI-PRTKBNMEI  =   SPACE
                   MOVE    63                      TO  SPA-GMN-CUR
                   MOVE    "0064"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4152-JIHI-PRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括作成出力順（全体）チェック
      *****************************************************************
       4150-PRTJYUN4-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-PRTJYUN4   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PRTJYUN4-MAX
                   IF      SPA-GMN-PRTJYUN4
                                   =   SPA-GMN-PRTJYUN4L (IDY)
                       MOVE    SPA-GMN-PRTJYUN4LST (IDY)
                                           TO  SPA-GMN-PRTJYUN4-G
                       MOVE    SPA-GMN-PRTJYUN4-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-PRTJYUN4MEI =   SPACE
                   MOVE    40                  TO  SPA-GMN-CUR
                   MOVE    "0041"              TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4150-PRTJYUN4-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括作成出力順（社保）チェック
      *****************************************************************
       4123-PRTJYUN1-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-PRTJYUN1   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PRTJYUN1-MAX
                   IF      SPA-GMN-PRTJYUN1
                                   =   SPA-GMN-PRTJYUN1L (IDY)
                       MOVE    SPA-GMN-PRTJYUN1LST (IDY)
                                           TO  SPA-GMN-PRTJYUN1-G
                       MOVE    SPA-GMN-PRTJYUN1-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-PRTJYUN1MEI  =   SPACE
                   MOVE    29                      TO  SPA-GMN-CUR
                   MOVE    "0016"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4123-PRTJYUN1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括作成出力順（国保）チェック
      *****************************************************************
       4135-PRTJYUN3-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-PRTJYUN3   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PRTJYUN3-MAX
                   IF      SPA-GMN-PRTJYUN3
                                   =   SPA-GMN-PRTJYUN3L (IDY)
                       MOVE    SPA-GMN-PRTJYUN3LST (IDY)
                                           TO  SPA-GMN-PRTJYUN3-G
                       MOVE    SPA-GMN-PRTJYUN3-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-PRTJYUN3MEI  =   SPACE
                   MOVE    31                      TO  SPA-GMN-CUR
                   MOVE    "0028"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4135-PRTJYUN3-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括作成出力順（広域連合）チェック
      *****************************************************************
       4157-PRTJYUN5-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-PRTJYUN5   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PRTJYUN5-MAX
                   IF      SPA-GMN-PRTJYUN5
                                   =   SPA-GMN-PRTJYUN5L (IDY)
                       MOVE    SPA-GMN-PRTJYUN5LST (IDY)
                                           TO  SPA-GMN-PRTJYUN5-G
                       MOVE    SPA-GMN-PRTJYUN5-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-PRTJYUN5MEI  =   SPACE
                   MOVE    49                      TO  SPA-GMN-CUR
                   MOVE    "0048"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4157-PRTJYUN5-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別作成出力順チェック
      *****************************************************************
       4124-PRTJYUN2-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-PRTJYUN2   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PRTJYUN2-MAX
                   IF      SPA-GMN-PRTJYUN2
                                   =   SPA-GMN-PRTJYUN2L (IDY)
                       MOVE    SPA-GMN-PRTJYUN2LST (IDY)
                                           TO  SPA-GMN-PRTJYUN2-G
                       MOVE    SPA-GMN-PRTJYUN2-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-PRTJYUN2MEI  =   SPACE
                   MOVE    30                      TO  SPA-GMN-CUR
                   MOVE    "0017"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4124-PRTJYUN2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括作成出力順（全体・入院）チェック
      *****************************************************************
       4172-PRTJYUN6-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-PRTJYUN6   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PRTJYUN6-MAX
                   IF      SPA-GMN-PRTJYUN6
                                   =   SPA-GMN-PRTJYUN6L (IDY)
                       MOVE    SPA-GMN-PRTJYUN6LST (IDY)
                                           TO  SPA-GMN-PRTJYUN6-G
                       MOVE    SPA-GMN-PRTJYUN6-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-PRTJYUN6MEI =   SPACE
                   MOVE    61                  TO  SPA-GMN-CUR
                   MOVE    "0061"              TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4172-PRTJYUN6-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    「特別療養費」文字記載位置チェック
      *****************************************************************
       4125-MOJI-KISAIKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-MOJI-KISAIKBN   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-MOJI-KISAIKBN-MAX
                   IF      SPA-GMN-MOJI-KISAIKBN
                                   =   SPA-GMN-MOJI-KISAIKBNL (IDY)
                       MOVE    SPA-GMN-MOJI-KISAIKBNLST (IDY)
                                           TO  SPA-GMN-MOJI-KISAIKBN-G
                       MOVE    SPA-GMN-MOJI-KISAIKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-MOJI-KISAIKBNMEI  =   SPACE
                   MOVE    50                      TO  SPA-GMN-CUR
                   MOVE    "0018"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4125-MOJI-KISAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    給付割合欄記載区分チェック
      *****************************************************************
       4126-KYURATE-KISAIKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-KYURATE-KISAIKBN
                                           =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-KYURATE-KISAIKBN-MAX
                   IF      SPA-GMN-KYURATE-KISAIKBN
                                   =   SPA-GMN-KYURATE-KISAIKBNL (IDY)
                       MOVE    SPA-GMN-KYURATE-KISAIKBNLST (IDY)
                                           TO
                                           SPA-GMN-KYURATE-KISAIKBN-G
                       MOVE    SPA-GMN-KYURATE-KISAIKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-KYURATE-KISAIKBNMEI =   SPACE
                   MOVE    51                      TO  SPA-GMN-CUR
                   MOVE    "0019"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4126-KYURATE-KISAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト種別欄記載区分チェック
      *****************************************************************
       4127-RECESYU-KISAIKBN-CHK-SEC   SECTION.
      *
           IF      SPA-GMN-RECESYUBET-KISAIKBN
                                           =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-RECESYUBET-KISAIKBN-MAX
                   IF      SPA-GMN-RECESYUBET-KISAIKBN
                                   =
                                   SPA-GMN-RECESYUBET-KISAIKBNL (IDY)
                       MOVE    SPA-GMN-RECESYUBET-KISAIKBNLST (IDY)
                                           TO
                                       SPA-GMN-RECESYUBET-KISAIKBN-G
                       MOVE    SPA-GMN-RECESYUBET-KISAIKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-RECESYUBET-KISAIKBNMEI =   SPACE
                   MOVE    52                      TO  SPA-GMN-CUR
                   MOVE    "0020"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4127-RECESYU-KISAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷順指定・県内県外チェック
      *****************************************************************
       4128-PREFKBN-PRTJYUN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-PREFKBN-PRTJYUN   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PREFKBN-PRTJYUN-MAX
                   IF      SPA-GMN-PREFKBN-PRTJYUN
                                   =   SPA-GMN-PREFKBN-PRTJYUNL (IDY)
                       MOVE    SPA-GMN-PREFKBN-PRTJYUNLST (IDY)
                                           TO  SPA-GMN-PREFKBN-PRTJYUN-G
                       MOVE    SPA-GMN-PREFKBN-PRTJYUN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-PREFKBN-PRTJYUNMEI  =   SPACE
                   MOVE    70                      TO  SPA-GMN-CUR
                   MOVE    "0021"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4128-PREFKBN-PRTJYUN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷順指定・保険者チェック
      *****************************************************************
       4129-HKNJANUM-PRTJYUN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-HKNJANUM-PRTJYUN
                                           =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-HKNJANUM-PRTJYUN-MAX
                   IF      SPA-GMN-HKNJANUM-PRTJYUN
                                   =   SPA-GMN-HKNJANUM-PRTJYUNL (IDY)
                       MOVE    SPA-GMN-HKNJANUM-PRTJYUNLST (IDY)
                                           TO
                                           SPA-GMN-HKNJANUM-PRTJYUN-G
                       MOVE    SPA-GMN-HKNJANUM-PRTJYUN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-HKNJANUM-PRTJYUNMEI =   SPACE
                   MOVE    71                      TO  SPA-GMN-CUR
                   MOVE    "0022"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4129-HKNJANUM-PRTJYUN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷順指定・レセプト種別チェック
      *****************************************************************
       4130-RECESYU-PRTJYUN-CHK-SEC   SECTION.
      *
           IF      SPA-GMN-RECESYUBETU-PRTJYUN
                                           =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-RECESYUBETU-PRTJYUN-MAX
                   IF      SPA-GMN-RECESYUBETU-PRTJYUN
                                   =
                                   SPA-GMN-RECESYUBETU-PRTJYUNL (IDY)
                       MOVE    SPA-GMN-RECESYUBETU-PRTJYUNLST (IDY)
                                           TO
                                       SPA-GMN-RECESYUBETU-PRTJYUN-G
                       MOVE    SPA-GMN-RECESYUBETU-PRTJYUN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-RECESYUBETU-PRTJYUNMEI =   SPACE
                   MOVE    72                      TO  SPA-GMN-CUR
                   MOVE    "0023"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4130-RECESYU-PRTJYUN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    在総診・在医総チェック
      *****************************************************************
       4131-ZAITAKU-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-ZAITAKU   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-ZAITAKU-MAX
                   IF      SPA-GMN-ZAITAKU
                                   =   SPA-GMN-ZAITAKUL (IDY)
                       MOVE    SPA-GMN-ZAITAKULST (IDY)
                                           TO  SPA-GMN-ZAITAKU-G
                       MOVE    SPA-GMN-ZAITAKU-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-ZAITAKUMEI  =   SPACE
                   MOVE    73                      TO  SPA-GMN-CUR
                   MOVE    "0024"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4131-ZAITAKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    長期（長期高額療養費）チェック
      *****************************************************************
       4132-CHOUKI-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-CHOUKI
                                           =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-CHOUKI-MAX
                   IF      SPA-GMN-CHOUKI
                                   =   SPA-GMN-CHOUKIL (IDY)
                       MOVE    SPA-GMN-CHOUKILST (IDY)
                                           TO
                                           SPA-GMN-CHOUKI-G
                       MOVE    SPA-GMN-CHOUKI-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-CHOUKIMEI =   SPACE
                   MOVE    74                      TO  SPA-GMN-CUR
                   MOVE    "0025"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4132-CHOUKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関の所在地保険者チェック
      *****************************************************************
       4133-HKNJANUM-CHK-SEC   SECTION.
      *
           MOVE    SPACE                TO SPA-GMN-HKNJANAME
      *
           IF      SPA-GMN-HKNJANUM        =   SPACE
               MOVE    SPACE               TO  SPA-GMN-HKNJANUM-PRTKBN-G
           ELSE
               MOVE    SPACE               TO  HKNJAINF-REC
               INITIALIZE                      HKNJAINF-REC
               MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
               MOVE    SPA-GMN-HKNJANUM    TO  HKNJA-HKNJANUM
               MOVE    HKNJAINF-REC        TO  MCPDATA-REC
               PERFORM 900-HKNJAINF-READ-SEC
               IF      FLG-HKNJAINF        =   ZERO
                   MOVE    HKNJA-HKNJANAME     TO  SPA-GMN-HKNJANAME
                   IF      HKNJA-HKNNUM    NOT =   "060"      
                       MOVE    75                      TO  SPA-GMN-CUR
                       MOVE    "0026"                  TO  SPA-ERRCD
                   ELSE
                       IF      HKNJA-HKNJANUM (1:2)
                                               NOT =   SPA-1001-PREFNUM
                           MOVE    75                  TO  SPA-GMN-CUR
                           MOVE    "0029"              TO  SPA-ERRCD
                       END-IF
                   END-IF
               ELSE
                   MOVE    75                      TO  SPA-GMN-CUR
                   MOVE    "0026"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4133-HKNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関の所在地保険者印刷位置チェック
      *****************************************************************
       4134-HKNJANUM-PRTKBN-CHK-SEC   SECTION.
      *
           IF      SPA-GMN-HKNJANUM-PRTKBN
                                           =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-HKNJANUM-PRTKBN-MAX
                   IF      SPA-GMN-HKNJANUM-PRTKBN
                                   =
                                   SPA-GMN-HKNJANUM-PRTKBNL (IDY)
                       MOVE    SPA-GMN-HKNJANUM-PRTKBNLST (IDY)
                                           TO
                                       SPA-GMN-HKNJANUM-PRTKBN-G
                       MOVE    SPA-GMN-HKNJANUM-PRTKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-HKNJANUM-PRTKBNMEI =   SPACE
                   MOVE    76                      TO  SPA-GMN-CUR
                   MOVE    "0027"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4134-HKNJANUM-PRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    医師別出力区分チェック
      *****************************************************************
       4161-DRCD-PRTKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-DRCD-PRTKBN     =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-DRCD-PRTKBN-MAX
                   IF      SPA-GMN-DRCD-PRTKBN
                                   =   SPA-GMN-DRCD-PRTKBNL (IDY)
                       MOVE    SPA-GMN-DRCD-PRTKBNLST (IDY)
                                           TO
                                           SPA-GMN-DRCD-PRTKBN-G
                       MOVE    SPA-GMN-DRCD-PRTKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-DRCD-PRTKBNMEI =   SPACE
                   MOVE    77                      TO  SPA-GMN-CUR
                   MOVE    "0052"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4161-DRCD-PRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    医師別出力順区分チェック
      *****************************************************************
       4162-DRCD-PRTJYUNKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-DRCD-PRTKBN     =   SPACE   OR  "0"
               MOVE    SPACE                   TO  
                                           SPA-GMN-DRCD-PRTJYUNKBN-G
           END-IF
      *
           IF      SPA-GMN-DRCD-PRTJYUNKBN     =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-DRCD-PRTJYUNKBN-MAX
                   IF      SPA-GMN-DRCD-PRTJYUNKBN
                                   =   SPA-GMN-DRCD-PRTJYUNKBNL (IDY)
                       MOVE    SPA-GMN-DRCD-PRTJYUNKBNLST (IDY)
                                           TO
                                           SPA-GMN-DRCD-PRTJYUNKBN-G
                       MOVE    SPA-GMN-DRCD-PRTJYUNKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-DRCD-PRTJYUNKBNMEI =   SPACE
                   MOVE    78                      TO  SPA-GMN-CUR
                   MOVE    "0053"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4162-DRCD-PRTJYUNKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟別出力区分チェック
      *****************************************************************
       4170-BTUNUM-PRTKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-BTUNUM-PRTKBN     =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-BTUNUM-PRTKBN-MAX
                   IF      SPA-GMN-BTUNUM-PRTKBN
                                   =   SPA-GMN-BTUNUM-PRTKBNL (IDY)
                       MOVE    SPA-GMN-BTUNUM-PRTKBNLST (IDY)
                                           TO
                                           SPA-GMN-BTUNUM-PRTKBN-G
                       MOVE    SPA-GMN-BTUNUM-PRTKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-BTUNUM-PRTKBNMEI =   SPACE
                   MOVE    79                      TO  SPA-GMN-CUR
                   MOVE    "0059"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4170-BTUNUM-PRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟出力順区分チェック
      *****************************************************************
       4171-BTUNUM-PRTJYUNKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-BTUNUM-PRTKBN     =   SPACE   OR  "0"
               MOVE    SPACE                   TO  
                                           SPA-GMN-BTUNUM-PRTJYUNKBN-G
           END-IF
      *
           IF      SPA-GMN-BTUNUM-PRTJYUNKBN     =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-BTUNUM-PRTJYUNKBN-MAX
                   IF      SPA-GMN-BTUNUM-PRTJYUNKBN
                                   =   SPA-GMN-BTUNUM-PRTJYUNKBNL (IDY)
                       MOVE    SPA-GMN-BTUNUM-PRTJYUNKBNLST (IDY)
                                           TO
                                           SPA-GMN-BTUNUM-PRTJYUNKBN-G
                       MOVE    SPA-GMN-BTUNUM-PRTJYUNKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-BTUNUM-PRTJYUNKBNMEI =   SPACE
                   MOVE    80                      TO  SPA-GMN-CUR
                   MOVE    "0060"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4171-BTUNUM-PRTJYUNKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ターミナルデジットによる出力区分チェック
      *****************************************************************
       4191-TERMINAL-DI-PRTKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-TERMINAL-DIG-PRTKBN     =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-TERMINAL-DIG-PRTKBN-MAX
                   IF      SPA-GMN-TERMINAL-DIG-PRTKBN
                               =   SPA-GMN-TERMINAL-DIG-PRTKBNL (IDY)
                       MOVE    SPA-GMN-TERMINAL-DIG-PRTKBNLST (IDY)
                                           TO
                                       SPA-GMN-TERMINAL-DIG-PRTKBN-G
                       MOVE    SPA-GMN-TERMINAL-DIG-PRTKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-TERMINAL-DIG-PRTKBNMEI
                                       =   SPACE
                   MOVE    81              TO  SPA-GMN-CUR
                   MOVE    "0119"          TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4191-TERMINAL-DI-PRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科設定フラグチェック
      *****************************************************************
       4135-SYUKA-KBN-CHK-SEC   SECTION.
      *
           IF      SPA-GMN-SYUKA-KBN   =   SPACE
               MOVE    SPA-GMN-SYUKA-KBNLST (1)
                                               TO  SPA-GMN-SYUKA-KBN-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-SYUKA-KBN-MAX
                   IF      SPA-GMN-SYUKA-KBN
                                   =
                                   SPA-GMN-SYUKA-KBNL (IDY)
                       MOVE    SPA-GMN-SYUKA-KBNLST (IDY)
                                           TO
                                       SPA-GMN-SYUKA-KBN-G
                       MOVE    SPA-GMN-SYUKA-KBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-SYUKA-KBNMEI =   SPACE
                   MOVE    90                      TO  SPA-GMN-CUR
                   MOVE    "0030"                  TO  SPA-ERRCD
               END-IF
           END-IF
           .
       4135-SYUKA-KBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科設定情報（入院）チェック
      *****************************************************************
       4136-SYUKA-NYUIN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-SYUKA-NYUIN   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-SYUKA-NYUIN-MAX
                   IF      SPA-GMN-SYUKA-NYUIN
                                   =   SPA-GMN-SYUKA-NYUINL (IDY)
                       MOVE    SPA-GMN-SYUKA-NYUINLST (IDY)
                                           TO  SPA-GMN-SYUKA-NYUIN-G
                       MOVE    SPA-GMN-SYUKA-NYUIN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-SYUKA-NYUINMEI  =   SPACE
                   MOVE    91                      TO  SPA-GMN-CUR
                   MOVE    "0031"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4136-SYUKA-NYUIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科設定情報（入院外）チェック
      *****************************************************************
       4137-SYUKA-GAIRAI-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-SYUKA-GAIRAI    =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-SYUKA-GAIRAI-MAX
                   IF      SPA-GMN-SYUKA-GAIRAI
                                   =   SPA-GMN-SYUKA-GAIRAIL (IDY)
                       MOVE    SPA-GMN-SYUKA-GAIRAILST (IDY)
                                           TO
                                           SPA-GMN-SYUKA-GAIRAI-G
                       MOVE    SPA-GMN-SYUKA-GAIRAI-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-SYUKA-GAIRAIMEI =   SPACE
                   MOVE    92                      TO  SPA-GMN-CUR
                   MOVE    "0032"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4137-SYUKA-GAIRAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科情報取得フラグチェック
      *****************************************************************
       4138-SYUKA-MODE-CHK-SEC   SECTION.
      *
           IF      SPA-GMN-SYUKA-MODE      =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-SYUKA-MODE-MAX
                   IF      SPA-GMN-SYUKA-MODE
                                   =
                                   SPA-GMN-SYUKA-MODEL (IDY)
                       MOVE    SPA-GMN-SYUKA-MODELST (IDY)
                                           TO
                                       SPA-GMN-SYUKA-MODE-G
                       MOVE    SPA-GMN-SYUKA-MODE-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-SYUKA-MODEMEI =   SPACE
                   MOVE    94                      TO  SPA-GMN-CUR
                   MOVE    "0033"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4138-SYUKA-MODE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数科同一選択チェック
      *****************************************************************
       4139-SYUKA-SELECT-CHK-SEC   SECTION.
      *
           IF      SPA-GMN-SYUKA-SELECT    =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-SYUKA-SELECT-MAX
                   IF      SPA-GMN-SYUKA-SELECT
                                   =
                                   SPA-GMN-SYUKA-SELECTL (IDY)
                       MOVE    SPA-GMN-SYUKA-SELECTLST (IDY)
                                           TO
                                       SPA-GMN-SYUKA-SELECT-G
                       MOVE    SPA-GMN-SYUKA-SELECT-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-SYUKA-SELECTMEI =   SPACE
                   MOVE    93                      TO  SPA-GMN-CUR
                   MOVE    "0034"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4139-SYUKA-SELECT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    日計表レコード記録条件（入院外・社保）チェック
      *****************************************************************
       4140-NI-JYOKEN1-CHK-SEC   SECTION.
      *
           IF      SPA-GMN-NI-JYOKEN1  =   ZERO
               MOVE    WRK-CONS-JYOKEN-TENSU
                                           TO  SPA-GMN-NI-JYOKEN1
           END-IF
           .
       4140-NI-JYOKEN1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    日計表レコード記録条件（入院外・国保）チェック
      *****************************************************************
       4141-NI-JYOKEN2-CHK-SEC   SECTION.
      *
           IF      SPA-GMN-NI-JYOKEN2  =   ZERO
               MOVE    WRK-CONS-JYOKEN-TENSU
                                           TO  SPA-GMN-NI-JYOKEN2
           END-IF
           .
       4141-NI-JYOKEN2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    日計表レコード記録条件（入院・社保）チェック
      *****************************************************************
       4142-NI-JYOKEN3-CHK-SEC   SECTION.
      *
           IF      SPA-GMN-NI-JYOKEN3  =   ZERO
               MOVE    WRK-CONS-JYOKEN-TENSU
                                           TO  SPA-GMN-NI-JYOKEN3
           END-IF
           .
       4142-NI-JYOKEN3-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    日計表レコード記録条件（入院・国保）チェック
      *****************************************************************
       4143-NI-JYOKEN4-CHK-SEC   SECTION.
      *
           IF      SPA-GMN-NI-JYOKEN4  =   ZERO
               MOVE    WRK-CONS-JYOKEN-TENSU
                                           TO  SPA-GMN-NI-JYOKEN4
           END-IF
           .
       4143-NI-JYOKEN4-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    日計表レコード記録条件（入院外・広域連合）チェック
      *****************************************************************
       4159-NI-JYOKEN5-CHK-SEC   SECTION.
      *
           IF      SPA-GMN-NI-JYOKEN5  =   ZERO
               MOVE    WRK-CONS-JYOKEN-TENSU
                                           TO  SPA-GMN-NI-JYOKEN5
           END-IF
           .
       4159-NI-JYOKEN5-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    日計表レコード記録条件（入院・広域連合）チェック
      *****************************************************************
       4160-NI-JYOKEN6-CHK-SEC   SECTION.
      *
           IF      SPA-GMN-NI-JYOKEN6  =   ZERO
               MOVE    WRK-CONS-JYOKEN-TENSU
                                           TO  SPA-GMN-NI-JYOKEN6
           END-IF
           .
       4160-NI-JYOKEN6-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電ＣＤ−Ｒ出力区分チェック
      *****************************************************************
       4167-RECEDEN-CDRKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-RECEDEN-CDRKBN  =   SPACE
               MOVE    SPA-GMN-RECEDEN-CDRKBNLST (1)
                                           TO  SPA-GMN-RECEDEN-CDRKBN-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-RECEDEN-CDRKBN-MAX
                   IF      SPA-GMN-RECEDEN-CDRKBN
                                   =   SPA-GMN-RECEDEN-CDRKBNL (IDY)
                       MOVE    SPA-GMN-RECEDEN-CDRKBNLST (IDY)
                                           TO  SPA-GMN-RECEDEN-CDRKBN-G
                       MOVE    SPA-GMN-RECEDEN-CDRKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-RECEDEN-CDRKBNMEI   =   SPACE
                   MOVE    111                     TO  SPA-GMN-CUR
                   MOVE    "0103"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4167-RECEDEN-CDRKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記作成区分チェック
      *****************************************************************
       4168-RECEDEN-LST1KBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-RECEDEN-LST1KBN  =   SPACE
               MOVE    SPA-GMN-RECEDEN-LST1KBNLST (2)
                                       TO  SPA-GMN-RECEDEN-LST1KBN-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-RECEDEN-LST1KBN-MAX
                   IF      SPA-GMN-RECEDEN-LST1KBN
                                   =   SPA-GMN-RECEDEN-LST1KBNL (IDY)
                       MOVE    SPA-GMN-RECEDEN-LST1KBNLST (IDY)
                                           TO  SPA-GMN-RECEDEN-LST1KBN-G
                       MOVE    SPA-GMN-RECEDEN-LST1KBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-RECEDEN-LST1KBNMEI   =   SPACE
                   MOVE    112                     TO  SPA-GMN-CUR
                   MOVE    "0104"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4168-RECEDEN-LST1KBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    光ディスク等送付書作成区分チェック
      *****************************************************************
       4169-RECEDEN-LST2KBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-RECEDEN-LST2KBN  =   SPACE
               MOVE    SPA-GMN-RECEDEN-LST2KBNLST (1)
                                       TO  SPA-GMN-RECEDEN-LST2KBN-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-RECEDEN-LST2KBN-MAX
                   IF      SPA-GMN-RECEDEN-LST2KBN
                                   =   SPA-GMN-RECEDEN-LST2KBNL (IDY)
                       MOVE    SPA-GMN-RECEDEN-LST2KBNLST (IDY)
                                           TO  SPA-GMN-RECEDEN-LST2KBN-G
                       MOVE    SPA-GMN-RECEDEN-LST2KBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-RECEDEN-LST2KBNMEI   =   SPACE
                   MOVE    113                     TO  SPA-GMN-CUR
                   MOVE    "0105"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4169-RECEDEN-LST2KBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    光ディスク等送付書の媒体設定チェック
      *****************************************************************
       4199-RECEDEN-LST2-MEDIA-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-RECEDEN-LST2-MEDIA  =   SPACE
               MOVE    SPA-GMN-RECEDEN-LST2-MEDIALST (1)
                                       TO
                                       SPA-GMN-RECEDEN-LST2-MEDIA-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-RECEDEN-LST2-MEDIA-MAX
                   IF      SPA-GMN-RECEDEN-LST2-MEDIA
                               =   SPA-GMN-RECEDEN-LST2-MEDIAL (IDY)
                       MOVE    SPA-GMN-RECEDEN-LST2-MEDIALST (IDY)
                                           TO
                                       SPA-GMN-RECEDEN-LST2-MEDIA-G
                       MOVE    SPA-GMN-RECEDEN-LST2-MEDIA-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-RECEDEN-LST2-MEDIAMEI   =   SPACE
                   MOVE    310                     TO  SPA-GMN-CUR
                   MOVE    "0125"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4199-RECEDEN-LST2-MEDIA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電データ履歴保存設定区分チェック
      *****************************************************************
       4168-RECEDEN-RIREKIKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-RECEDEN-RIREKIKBN  =   SPACE
               MOVE    SPA-GMN-RECEDEN-RIREKIKBNLST (1)
                                       TO  SPA-GMN-RECEDEN-RIREKIKBN-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-RECEDEN-RIREKIKBN-MAX
                   IF      SPA-GMN-RECEDEN-RIREKIKBN
                                   =   SPA-GMN-RECEDEN-RIREKIKBNL (IDY)
                       MOVE    SPA-GMN-RECEDEN-RIREKIKBNLST (IDY)
                                           TO
                                           SPA-GMN-RECEDEN-RIREKIKBN-G
                       MOVE    SPA-GMN-RECEDEN-RIREKIKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-RECEDEN-RIREKIKBNMEI   =   SPACE
                   MOVE    114                     TO  SPA-GMN-CUR
                   MOVE    "0106"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4168-RECEDEN-RIREKIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル出力先設定区分チェック
      *****************************************************************
       4169-RECEDEN-DATAKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-RECEDEN-DATAKBN =   SPACE
               IF      SPA-MW                  =   SPA-GINBEE
                   IF      SPA-200501-RECEDEN-DATAKBN
                                                   =   "5" OR "6" OR
                                                       SPACE
                       CONTINUE
                   ELSE
                       MOVE    SPACE       TO
                                           SPA-200501-RECEDEN-DATAKBN
                       MOVE   "0123"       TO  SPA-ERRCD
                       MOVE    115         TO  SPA-GMN-CUR
                       GO  TO  4169-RECEDEN-DATAKBN-CHK-EXT
                   END-IF
               ELSE
                   IF      SPA-200501-RECEDEN-DATAKBN
                                                   =   "1" OR "2"
                       MOVE    SPACE       TO
                                           SPA-200501-RECEDEN-DATAKBN
                       MOVE   "0123"       TO  SPA-ERRCD
                       MOVE    115         TO  SPA-GMN-CUR
                       GO  TO  4169-RECEDEN-DATAKBN-CHK-EXT
                   END-IF
               END-IF
           END-IF    
      *
           IF      SPA-GMN-RECEDEN-DATAKBN  =   SPACE
               IF      SPA-MW                  =   SPA-GINBEE
                   MOVE    SPA-GMN-RECEDEN-DATAKBNLST (1)
                                       TO  SPA-GMN-RECEDEN-DATAKBN-G
      ************ MOVE    SPACE       TO  SPA-GMN-RECEDEN-DATAKBN-G
               ELSE
                   MOVE    SPA-GMN-RECEDEN-DATAKBNLST (2)
                                       TO  SPA-GMN-RECEDEN-DATAKBN-G
               END-IF
           ELSE
               MOVE    SPACE           TO  SPA-GMN-RECEDEN-DATAKBNMEI
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-RECEDEN-DATAKBN-MAX
                   IF      SPA-GMN-RECEDEN-DATAKBN
                                   =   SPA-GMN-RECEDEN-DATAKBNL (IDY)
                       MOVE    SPA-GMN-RECEDEN-DATAKBNLST (IDY)
                                           TO  SPA-GMN-RECEDEN-DATAKBN-G
                       MOVE    SPA-GMN-RECEDEN-DATAKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
           END-IF    
      *     
           IF      SPA-GMN-RECEDEN-DATAKBNMEI   =   SPACE
               MOVE    115                     TO  SPA-GMN-CUR
               MOVE    "0107"                  TO  SPA-ERRCD
           END-IF    
           .
       4169-RECEDEN-DATAKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者が特定できない災害該当レセプトのレセ電記録区分チェック
      *****************************************************************
       4180-RECEDEN-DISASTER-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-RECEDEN-DISASTER  =   SPACE
               MOVE    SPA-GMN-RECEDEN-DISASTERLST (2)
                                       TO  SPA-GMN-RECEDEN-DISASTER-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-RECEDEN-DISASTER-MAX
                   IF      SPA-GMN-RECEDEN-DISASTER
                                   =   SPA-GMN-RECEDEN-DISASTERL (IDY)
                       MOVE    SPA-GMN-RECEDEN-DISASTERLST (IDY)
                                           TO
                                           SPA-GMN-RECEDEN-DISASTER-G
                       MOVE    SPA-GMN-RECEDEN-DISASTER-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-RECEDEN-DISASTERMEI   =   SPACE
                   MOVE    120                     TO  SPA-GMN-CUR
                   MOVE    "0113"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4180-RECEDEN-DISASTER-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電データチェックの有無区分チェック
      *****************************************************************
       4188-RECEDEN-CHK-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-RECEDEN-CHK  =   SPACE
               MOVE    SPA-GMN-RECEDEN-CHKLST (1)
                                       TO  SPA-GMN-RECEDEN-CHK-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-RECEDEN-CHK-MAX
                   IF      SPA-GMN-RECEDEN-CHK
                                   =   SPA-GMN-RECEDEN-CHKL (IDY)
                       MOVE    SPA-GMN-RECEDEN-CHKLST (IDY)
                                           TO
                                           SPA-GMN-RECEDEN-CHK-G
                       MOVE    SPA-GMN-RECEDEN-CHK-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-RECEDEN-CHKMEI   =   SPACE
                   MOVE    121                     TO  SPA-GMN-CUR
                   MOVE    "0115"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4188-RECEDEN-CHK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    記号番号区切り文字設定フラグ（記号）チェック
      *****************************************************************
       4176-KIGO-DELIMITER-FLG-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-KIGO-DELI-FLG
                                   =   SPACE
               MOVE    SPA-GMN-KIGO-DELI-FLGLST (1)
                                       TO  SPA-GMN-KIGO-DELI-FLG-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-KIGO-DELI-FLG-MAX
                   IF      SPA-GMN-KIGO-DELI-FLG
                                   =   SPA-GMN-KIGO-DELI-FLGL (IDY)
                       MOVE    SPA-GMN-KIGO-DELI-FLGLST (IDY)
                                           TO
                                           SPA-GMN-KIGO-DELI-FLG-G
                       MOVE    SPA-GMN-KIGO-DELI-FLG-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *
               IF      SPA-GMN-KIGO-DELI-FLGMEI   =   SPACE
                   MOVE    116                     TO  SPA-GMN-CUR
                   MOVE    "0108"                  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-GMN-KIGO-DELI-FLG
                                   =   "0"
               MOVE    "0"             TO
                                   SPA-GMN-KIGO-DELI-HKNJA-FLG
           END-IF
           .
       4176-KIGO-DELIMITER-FLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    記号番号区切り文字対象保険者フラグ（記号）チェック
      *****************************************************************
       4177-KIGO-DELI-HKNJA-F-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-KIGO-DELI-HKNJA-FLG
                                   =   SPACE
               MOVE    SPA-GMN-KIGO-DELI-HKNJA-FLGLST (1)
                                       TO
                                   SPA-GMN-KIGO-DELI-HKNJA-FLG-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >
                               SPA-GMN-KIGO-DELI-HKNJA-FLG-MAX
                   IF      SPA-GMN-KIGO-DELI-HKNJA-FLG
                                   =
                           SPA-GMN-KIGO-DELI-HKNJA-FLGL (IDY)
                       MOVE    SPA-GMN-KIGO-DELI-HKNJA-FLGLST
                                                                (IDY)
                                           TO
                                   SPA-GMN-KIGO-DELI-HKNJA-FLG-G
                       MOVE    SPA-GMN-KIGO-DELI-HKNJA-FLG-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-KIGO-DELI-HKNJA-FLGMEI
                                   =   SPACE
                   MOVE    117                     TO  SPA-GMN-CUR
                   MOVE    "0109"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4177-KIGO-DELI-HKNJA-F-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    記号番号区切り文字設定フラグ（番号）チェック
      *****************************************************************
       4178-NUM-DELIMITER-FLG-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-NUM-DELIMITER-FLG
                                   =   SPACE
               MOVE    SPA-GMN-NUM-DELIMITER-FLGLST (1)
                                       TO  SPA-GMN-NUM-DELIMITER-FLG-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-NUM-DELIMITER-FLG-MAX
                   IF      SPA-GMN-NUM-DELIMITER-FLG
                                   =   SPA-GMN-NUM-DELIMITER-FLGL (IDY)
                       MOVE    SPA-GMN-NUM-DELIMITER-FLGLST (IDY)
                                           TO
                                           SPA-GMN-NUM-DELIMITER-FLG-G
                       MOVE    SPA-GMN-NUM-DELIMITER-FLG-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-NUM-DELIMITER-FLGMEI   =   SPACE
                   MOVE    118                     TO  SPA-GMN-CUR
                   MOVE    "0110"                  TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           IF      SPA-GMN-NUM-DELIMITER-FLG
                                   =   "0"
               MOVE    "0"             TO
                                   SPA-GMN-NUM-DELI-HKNJA-FLG
           END-IF
           .
       4178-NUM-DELIMITER-FLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    記号番号区切り文字対象保険者フラグ（番号）チェック
      *****************************************************************
       4179-NUM-DELI-HKNJA-F-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-NUM-DELI-HKNJA-FLG
                                   =   SPACE
               MOVE    SPA-GMN-NUM-DELI-HKNJA-FLGLST (1)
                                       TO
                                   SPA-GMN-NUM-DELI-HKNJA-FLG-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >
                               SPA-GMN-NUM-DELI-HKNJA-FLG-MAX
                   IF      SPA-GMN-NUM-DELI-HKNJA-FLG
                                   =
                           SPA-GMN-NUM-DELI-HKNJA-FLGL (IDY)
                       MOVE    SPA-GMN-NUM-DELI-HKNJA-FLGLST
                                                                (IDY)
                                           TO
                                   SPA-GMN-NUM-DELI-HKNJA-FLG-G
                       MOVE    SPA-GMN-NUM-DELI-HKNJA-FLG-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-NUM-DELI-HKNJA-FLGMEI
                                   =   SPACE
                   MOVE    119                     TO  SPA-GMN-CUR
                   MOVE    "0111"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4179-NUM-DELI-HKNJA-F-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入退院履歴記載区分（入院外レセプトのみ）チェック
      *****************************************************************
       4144-NYUTAIIN-KSI-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-NYUTAIIN-KSIKBN
                                       =   SPACE
               MOVE    SPA-GMN-NYUTAIIN-KSIKBNLST (1)
                                   TO  SPA-GMN-NYUTAIIN-KSIKBN-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-NYUTAIIN-KSIKBN-MAX
                   IF      SPA-GMN-NYUTAIIN-KSIKBN
                                   =   SPA-GMN-NYUTAIIN-KSIKBNL(IDY)
                       MOVE    SPA-GMN-NYUTAIIN-KSIKBNLST (IDY)
                                       TO  SPA-GMN-NYUTAIIN-KSIKBN-G
                       MOVE    SPA-GMN-NYUTAIIN-KSIKBN-MAX
                                       TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-NYUTAIIN-KSIKBNMEI    =   SPACE
                   MOVE    4                   TO  SPA-GMN-CUR
                   MOVE    "0035"              TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4144-NYUTAIIN-KSI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入退院履歴記載区分（入院）チェック
      *****************************************************************
       4144-NYUTAIIN-NYU-KSI-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-NYUTAIIN-NYU-KSIKBN
                                       =   SPACE
               MOVE    SPA-GMN-NYUTAIIN-NYU-KSIKBNLST (1)
                                   TO
                                   SPA-GMN-NYUTAIIN-NYU-KSIKBN-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >
                               SPA-GMN-NYUTAIIN-NYU-KSIKBN-MAX
                   IF      SPA-GMN-NYUTAIIN-NYU-KSIKBN
                                   =
                                   SPA-GMN-NYUTAIIN-NYU-KSIKBNL(IDY)
                       MOVE    SPA-GMN-NYUTAIIN-NYU-KSIKBNLST (IDY)
                                       TO
                                       SPA-GMN-NYUTAIIN-NYU-KSIKBN-G
                       MOVE    SPA-GMN-NYUTAIIN-NYU-KSIKBN-MAX
                                       TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-NYUTAIIN-NYU-KSIKBNMEI    =   SPACE
                   MOVE    304                 TO  SPA-GMN-CUR
                   MOVE    "0076"              TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4144-NYUTAIIN-NYU-KSI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療報酬請求書マル長分作成区分（社保）チェック
      *****************************************************************
       4145-SYAPRTKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-SYAPRTKBN   =   SPACE
               CONTINUE         
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-SYAPRTKBN-MAX
                   IF      SPA-GMN-SYAPRTKBN
                                   =   SPA-GMN-SYAPRTKBNL (IDY)
                       MOVE    SPA-GMN-SYAPRTKBNLST (IDY)
                                           TO  SPA-GMN-SYAPRTKBN-G
                       MOVE    SPA-GMN-SYAPRTKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-SYAPRTKBNMEI    =   SPACE
                   MOVE    150                 TO  SPA-GMN-CUR
                   MOVE    "0036"              TO  SPA-ERRCD
               END-IF
           END-IF     
      *
           .
       4145-SYAPRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療報酬請求書災害該当分（非減免対象と減免対象）まとめチェック
      *****************************************************************
       4208-DISASTER-SYAPRTKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-DISASTER-SYAPRTKBN   =   SPACE
               CONTINUE         
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-DISASTER-SYAPRTKBN-MAX
                   IF      SPA-GMN-DISASTER-SYAPRTKBN
                                   =   SPA-GMN-DISASTER-SYAPRTKBNL (IDY)
                       MOVE    SPA-GMN-DISASTER-SYAPRTKBNLST (IDY)
                                           TO
                                           SPA-GMN-DISASTER-SYAPRTKBN-G
                       MOVE    SPA-GMN-DISASTER-SYAPRTKBN-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *
               IF      SPA-GMN-DISASTER-SYAPRTKBNMEI    =   SPACE
                   MOVE    404                 TO  SPA-GMN-CUR
                   MOVE    "0134"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4208-DISASTER-SYAPRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    枠記載区分（労災・入院外）チェック
      *****************************************************************
       4164-FRAME-ROUPRTKBN-CHK-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-FRAME-ROUPRTKBN-G   =   SPACE
               CONTINUE         
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-FRAME-ROUPRTKBN-MAX
                   IF      SPA-GMN-FRAME-ROUPRTKBN
                                   =   SPA-GMN-FRAME-ROUPRTKBNL (IDY)
                       MOVE    SPA-GMN-FRAME-ROUPRTKBNLST (IDY)
                                       TO  SPA-GMN-FRAME-ROUPRTKBN-G
                       MOVE    SPA-GMN-FRAME-ROUPRTKBN-MAX
                                       TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF                                     
               END-PERFORM
      *     
               IF      FLG-CHK             =   ZERO
                   MOVE    212                 TO  SPA-GMN-CUR
                   MOVE    "0055"              TO  SPA-ERRCD
               END-IF
           END-IF     
      *
           .
       4164-FRAME-ROUPRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    枠記載区分（労災・入院）チェック
      *****************************************************************
       4165-FRAME-ROUPRTKBN-N-CHK-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-FRAME-ROUPRTKBN-N-G =   SPACE
               CONTINUE         
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-FRAME-ROUPRTKBN-N-MAX
                   IF      SPA-GMN-FRAME-ROUPRTKBN-N
                                   =   SPA-GMN-FRAME-ROUPRTKBN-NL(IDY)
                       MOVE    SPA-GMN-FRAME-ROUPRTKBN-NLST (IDY)
                                       TO  SPA-GMN-FRAME-ROUPRTKBN-N-G
                       MOVE    SPA-GMN-FRAME-ROUPRTKBN-N-MAX
                                       TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF                                     
               END-PERFORM
      *     
               IF      FLG-CHK             =   ZERO
                   MOVE    213                 TO  SPA-GMN-CUR
                   MOVE    "0056"              TO  SPA-ERRCD
               END-IF
           END-IF     
      *
           .
       4165-FRAME-ROUPRTKBN-N-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災コメント記載区分チェック
      *****************************************************************
       4166-COMMENT-ROU-CHK-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-COMMENT-ROU-G   =   SPACE
               CONTINUE         
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-COMMENT-ROU-MAX
                   IF      SPA-GMN-COMMENT-ROU
                                   =   SPA-GMN-COMMENT-ROUL (IDY)
                       MOVE    SPA-GMN-COMMENT-ROULST (IDY)
                                       TO  SPA-GMN-COMMENT-ROU-G
                       MOVE    SPA-GMN-COMMENT-ROU-MAX
                                       TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF                                     
               END-PERFORM
      *     
               IF      FLG-CHK             =   ZERO
                   MOVE    214                 TO  SPA-GMN-CUR
                   MOVE    "0057"              TO  SPA-ERRCD
               END-IF
           END-IF     
      *
           .
       4166-COMMENT-ROU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    様式選択機能（労災・入院外）チェック
      *****************************************************************
       4182-ROUPRTKBN-CHK-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-ROUPRTKBN-G =   SPACE   OR  "1"
               MOVE    SPA-GMN-ROUPRTKBNLST (1)
                                       TO  SPA-GMN-ROUPRTKBN-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-ROUPRTKBN-MAX
                   IF      SPA-GMN-ROUPRTKBN
                                   =   SPA-GMN-ROUPRTKBNL (IDY)
                       MOVE    SPA-GMN-ROUPRTKBNLST (IDY)
                                       TO  SPA-GMN-ROUPRTKBN-G
                       MOVE    SPA-GMN-ROUPRTKBN-MAX
                                       TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF                                     
               END-PERFORM
      *     
               IF      FLG-CHK             =   ZERO
                   MOVE    215                 TO  SPA-GMN-CUR
                   MOVE    "0068"              TO  SPA-ERRCD
               END-IF
           END-IF     
      *
           .
       4182-ROUPRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    様式選択機能（労災・入院）チェック
      *****************************************************************
       4183-ROUPRTKBN-N-CHK-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-ROUPRTKBN-N-G =   SPACE   OR  "1"
               MOVE    SPA-GMN-ROUPRTKBN-NLST (1)
                                       TO  SPA-GMN-ROUPRTKBN-N-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-ROUPRTKBN-N-MAX
                   IF      SPA-GMN-ROUPRTKBN-N
                                   =   SPA-GMN-ROUPRTKBN-NL(IDY)
                       MOVE    SPA-GMN-ROUPRTKBN-NLST (IDY)
                                       TO  SPA-GMN-ROUPRTKBN-N-G
                       MOVE    SPA-GMN-ROUPRTKBN-N-MAX
                                       TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF                                     
               END-PERFORM
      *     
               IF      FLG-CHK             =   ZERO
                   MOVE    216                   TO  SPA-GMN-CUR
                   MOVE    "0069"                TO  SPA-ERRCD
               END-IF
           END-IF     
      *
           .
       4183-ROUPRTKBN-N-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    枠記載区分（自賠責・入院外）チェック
      *****************************************************************
       4146-FRAME-JIBAIPRTKB-CHK-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-FRAME-JIBAIPRTKBN-G =   SPACE
               CONTINUE         
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-FRAME-JIBAIPRTKBN-MAX
                   IF      SPA-GMN-FRAME-JIBAIPRTKBN
                                   =   SPA-GMN-FRAME-JIBAIPRTKBNL (IDY)
                       MOVE    SPA-GMN-FRAME-JIBAIPRTKBNLST (IDY)
                                       TO  SPA-GMN-FRAME-JIBAIPRTKBN-G
                       MOVE    SPA-GMN-FRAME-JIBAIPRTKBN-MAX
                                       TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF                                     
               END-PERFORM
      *     
               IF      FLG-CHK             =   ZERO
                   MOVE    200                 TO  SPA-GMN-CUR
                   MOVE    "0037"              TO  SPA-ERRCD
               END-IF
           END-IF     
      *
           .
       4146-FRAME-JIBAIPRTKB-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    枠記載区分（自賠責・入院）チェック
      *****************************************************************
       4147-FRAME-JIBAIPRTKB-N-CHK-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-FRAME-JIBAIPRTKBN-N-G =   SPACE
               CONTINUE         
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-FRAME-JIBAIPRTKBN-N-MAX
                   IF      SPA-GMN-FRAME-JIBAIPRTKBN-N
                                   =   SPA-GMN-FRAME-JIBAIPRTKBN-NL(IDY)
                       MOVE    SPA-GMN-FRAME-JIBAIPRTKBN-NLST (IDY)
                                       TO  SPA-GMN-FRAME-JIBAIPRTKBN-N-G
                       MOVE    SPA-GMN-FRAME-JIBAIPRTKBN-N-MAX
                                       TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF                                     
               END-PERFORM
      *     
               IF      FLG-CHK             =   ZERO
                   MOVE    201                 TO  SPA-GMN-CUR
                   MOVE    "0038"              TO  SPA-ERRCD
               END-IF
           END-IF     
      *
           .
       4147-FRAME-JIBAIPRTKB-N-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責・様式選択機能（従来様式・入院外）チェック
      *****************************************************************
       4153-JIBAIPRTKBN-CHK-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-JIBAIPRTKBN-G   =   SPACE   OR  "0"
               MOVE    SPA-GMN-JIBAIPRTKBNLST (1)
                                       TO  SPA-GMN-JIBAIPRTKBN-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-JIBAIPRTKBN-MAX
                   IF      SPA-GMN-JIBAIPRTKBN
                                   =   SPA-GMN-JIBAIPRTKBNL (IDY)
                       MOVE    SPA-GMN-JIBAIPRTKBNLST (IDY)
                                       TO  SPA-GMN-JIBAIPRTKBN-G
                       MOVE    SPA-GMN-JIBAIPRTKBN-MAX
                                       TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF                                     
               END-PERFORM
      *     
               IF      FLG-CHK             =   ZERO
                   MOVE    210                 TO  SPA-GMN-CUR
                   MOVE    "0044"              TO  SPA-ERRCD
               END-IF
           END-IF     
      *
           .
       4153-JIBAIPRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責・様式選択機能（従来様式・入院）チェック
      *****************************************************************
       4154-JIBAIPRTKBN-N-CHK-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-JIBAIPRTKBN-N-G =   SPACE   OR  "0"
                MOVE    SPA-GMN-JIBAIPRTKBN-NLST (1)
                                       TO  SPA-GMN-JIBAIPRTKBN-N-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-JIBAIPRTKBN-N-MAX
                   IF      SPA-GMN-JIBAIPRTKBN-N
                                   =   SPA-GMN-JIBAIPRTKBN-NL(IDY)
                       MOVE    SPA-GMN-JIBAIPRTKBN-NLST (IDY)
                                       TO  SPA-GMN-JIBAIPRTKBN-N-G
                       MOVE    SPA-GMN-JIBAIPRTKBN-N-MAX
                                       TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF                                     
               END-PERFORM
      *     
               IF      FLG-CHK             =   ZERO
                   MOVE    211                 TO  SPA-GMN-CUR
                   MOVE    "0045"              TO  SPA-ERRCD
               END-IF
           END-IF     
      *
           .
       4154-JIBAIPRTKBN-N-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    自賠責・様式選択機能（新様式・入院外）チェック
      *****************************************************************
       4195-JIBAIPRTKBN1-CHK-SEC   SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-JIBAIPRTKBN1-G
                                   =   SPACE   OR  "0"
               MOVE    SPA-GMN-JIBAIPRTKBN1LST (2)
                                       TO  SPA-GMN-JIBAIPRTKBN1-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-JIBAIPRTKBN1-MAX
                   IF      SPA-GMN-JIBAIPRTKBN1
                                   =   SPA-GMN-JIBAIPRTKBN1L (IDY)
                       MOVE    SPA-GMN-JIBAIPRTKBN1LST (IDY)
                                       TO  SPA-GMN-JIBAIPRTKBN1-G
                       MOVE    SPA-GMN-JIBAIPRTKBN1-MAX
                                       TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF                                     
               END-PERFORM
      *     
               IF      FLG-CHK         =   ZERO
                   MOVE    217             TO  SPA-GMN-CUR
                   MOVE    "0078"          TO  SPA-ERRCD
               END-IF
           END-IF     
      *
           .
       4195-JIBAIPRTKBN1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責・様式選択機能（新様式・入院）チェック
      *****************************************************************
       4196-JIBAIPRTKBN1-N-CHK-SEC SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-JIBAIPRTKBN1-N-G
                                   =   SPACE   OR  "0"
               MOVE    SPA-GMN-JIBAIPRTKBN1-NLST (2)
                                       TO  SPA-GMN-JIBAIPRTKBN1-N-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-JIBAIPRTKBN1-N-MAX
                   IF      SPA-GMN-JIBAIPRTKBN1-N
                                   =   SPA-GMN-JIBAIPRTKBN1-NL(IDY)
                       MOVE    SPA-GMN-JIBAIPRTKBN1-NLST (IDY)
                                       TO  SPA-GMN-JIBAIPRTKBN1-N-G
                       MOVE    SPA-GMN-JIBAIPRTKBN1-N-MAX
                                       TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF                                     
               END-PERFORM
      *     
               IF      FLG-CHK         =   ZERO
                   MOVE    218             TO  SPA-GMN-CUR
                   MOVE    "0079"          TO  SPA-ERRCD
               END-IF
           END-IF     
      *
           .
       4196-JIBAIPRTKBN1-N-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責請求金額欄端数処理区分（自賠責）チェック
      *****************************************************************
       4202-JIBAI-FRACTIONKBN1-CHK-SEC SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-JIBAI-FRACTION-KBN1-G
                                   =   SPACE
               MOVE    SPA-GMN-JIBAI-FRACTION-KBN1LST (1)
                                       TO  SPA-GMN-JIBAI-FRACTION-KBN1-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-JIBAI-FRACTION-KBN1-MAX
                   IF      SPA-GMN-JIBAI-FRACTION-KBN1
                                   =   SPA-GMN-JIBAI-FRACTION-KBN1L(IDY)
                       MOVE    SPA-GMN-JIBAI-FRACTION-KBN1LST (IDY)
                                       TO  SPA-GMN-JIBAI-FRACTION-KBN1-G
                       MOVE    SPA-GMN-JIBAI-FRACTION-KBN1-MAX
                                       TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF                                     
               END-PERFORM
      *     
               IF      FLG-CHK         =   ZERO
                   MOVE    313             TO  SPA-GMN-CUR
                   MOVE    "0128"          TO  SPA-ERRCD
               END-IF
           END-IF     
      *
           .
       4202-JIBAI-FRACTIONKBN1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責請求金額欄端数処理区分（第三者行為）チェック
      *****************************************************************
       4203-JIBAI-FRACTIONKBN2-CHK-SEC SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-JIBAI-FRACTION-KBN2-G
                                   =   SPACE
               MOVE    SPA-GMN-JIBAI-FRACTION-KBN2LST (1)
                                       TO  SPA-GMN-JIBAI-FRACTION-KBN2-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-JIBAI-FRACTION-KBN2-MAX
                   IF      SPA-GMN-JIBAI-FRACTION-KBN2
                                   =   SPA-GMN-JIBAI-FRACTION-KBN2L(IDY)
                       MOVE    SPA-GMN-JIBAI-FRACTION-KBN2LST (IDY)
                                       TO  SPA-GMN-JIBAI-FRACTION-KBN2-G
                       MOVE    SPA-GMN-JIBAI-FRACTION-KBN2-MAX
                                       TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF                                     
               END-PERFORM
      *     
               IF      FLG-CHK         =   ZERO
                   MOVE    314             TO  SPA-GMN-CUR
                   MOVE    "0129"          TO  SPA-ERRCD
               END-IF
           END-IF     
      *
           .
       4203-JIBAI-FRACTIONKBN2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険履歴記載区分チェック
      *****************************************************************
       4204-HKN-RIREKI-CHK-SEC        SECTION.
      *
           IF      SPA-GMN-HKN-RIREKI =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-HKN-RIREKI-MAX
                        OR     SPA-GMN-HKN-RIREKI
                                   =   SPA-GMN-HKN-RIREKIL (IDY)
                   CONTINUE
               END-PERFORM
      * 
               IF      IDY <=      SPA-GMN-HKN-RIREKI-MAX
                   MOVE    SPA-GMN-HKN-RIREKILST (IDY)
                                           TO
                                           SPA-GMN-HKN-RIREKI-G
               ELSE
                   MOVE    400             TO  SPA-GMN-CUR
                   MOVE    "0130"          TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4204-HKN-RIREKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    現物給付一部負担金記載チェック
      *****************************************************************
       4205-GENBUTU-ICHIBU-CHK-SEC        SECTION.
      *
           IF      SPA-GMN-GENBUTU-ICHIBU =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-GENBUTU-ICHIBU-MAX
                        OR     SPA-GMN-GENBUTU-ICHIBU
                                   =   SPA-GMN-GENBUTU-ICHIBUL (IDY)
                   CONTINUE
               END-PERFORM
      * 
               IF      IDY <=      SPA-GMN-GENBUTU-ICHIBU-MAX
                   MOVE    SPA-GMN-GENBUTU-ICHIBULST (IDY)
                                           TO
                                           SPA-GMN-GENBUTU-ICHIBU-G
               ELSE
                   MOVE    401             TO  SPA-GMN-CUR
                   MOVE    "0131"          TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4205-GENBUTU-ICHIBU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    湿布薬用法レセプト記載区分チェック
      *****************************************************************
       4207-COMPRESS-CHK-SEC        SECTION.
      *
           IF      SPA-GMN-COMPRESS =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-COMPRESS-MAX
                        OR     SPA-GMN-COMPRESS
                                   =   SPA-GMN-COMPRESSL (IDY)
                   CONTINUE
               END-PERFORM
      * 
               IF      IDY <=      SPA-GMN-COMPRESS-MAX
                   MOVE    SPA-GMN-COMPRESSLST (IDY)
                                           TO  SPA-GMN-COMPRESS-G
               ELSE
                   MOVE    403             TO  SPA-GMN-CUR
                   MOVE    "0133"          TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4207-COMPRESS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    災害該当分（非減免対象と減免対象）レセプトの分割チェック
      *****************************************************************
       4209-DISASTER-RECEIPT-CHK-SEC        SECTION.
      *
           IF      SPA-GMN-DISASTER-RECEIPT
                                       =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-DISASTER-RECEIPT-MAX
                        OR     SPA-GMN-DISASTER-RECEIPT
                                   =   SPA-GMN-DISASTER-RECEIPTL (IDY)
                   CONTINUE
               END-PERFORM
      * 
               IF      IDY <=      SPA-GMN-DISASTER-RECEIPT-MAX
                   MOVE    SPA-GMN-DISASTER-RECEIPTLST (IDY)
                                           TO
                                           SPA-GMN-DISASTER-RECEIPT-G
               ELSE
                   MOVE    405             TO  SPA-GMN-CUR
                   MOVE    "0135"          TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           .
       4209-DISASTER-RECEIPT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    １５円以下薬剤のレセプト記載チェック
      *****************************************************************
       4210-15-YKZ-CHK-SEC        SECTION.
      *
           IF      SPA-GMN-15-YKZ      =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-15-YKZ-MAX
                        OR     SPA-GMN-15-YKZ
                                   =   SPA-GMN-15-YKZL (IDY)
                   CONTINUE
               END-PERFORM
      *
               IF      IDY <=      SPA-GMN-15-YKZ-MAX
                   MOVE    SPA-GMN-15-YKZLST (IDY)
                                           TO  SPA-GMN-15-YKZ-G
               ELSE
                   MOVE    406             TO  SPA-GMN-CUR
                   MOVE    "0136"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4210-15-YKZ-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責ＰＤＦファイル作成区分チェック
      *****************************************************************
       4211-JIBAI-PDF-CHK-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-JIBAI-PDF-G
                                   =   SPACE
               MOVE    SPA-GMN-JIBAI-PDFLST (1)
                                       TO  SPA-GMN-JIBAI-PDF-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-JIBAI-PDF-MAX
                   IF      SPA-GMN-JIBAI-PDF
                                   =   SPA-GMN-JIBAI-PDFL(IDY)
                       MOVE    SPA-GMN-JIBAI-PDFLST (IDY)
                                       TO  SPA-GMN-JIBAI-PDF-G
                       MOVE    SPA-GMN-JIBAI-PDF-MAX
                                       TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF
               END-PERFORM
      *     
               IF      FLG-CHK         =   ZERO
                   MOVE    315             TO  SPA-GMN-CUR
                   MOVE    "0137"          TO  SPA-ERRCD
               END-IF
           END-IF     
      *
           .
       4211-JIBAI-PDF-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    全国公費一部負担金記載区分（公費のみのレセプト）チェック
      *****************************************************************
       4212-KOUHI-ICHIBU-KSI-CHK-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-KOUHI-ICHIBU-KSI-G
                                   =   SPACE
               MOVE    SPA-GMN-KOUHI-ICHIBU-KSILST (1)
                                       TO  SPA-GMN-KOUHI-ICHIBU-KSI-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-KOUHI-ICHIBU-KSI-MAX
                   IF      SPA-GMN-KOUHI-ICHIBU-KSI
                                   =   SPA-GMN-KOUHI-ICHIBU-KSIL (IDY)
                       MOVE    SPA-GMN-KOUHI-ICHIBU-KSILST (IDY)
                                       TO  SPA-GMN-KOUHI-ICHIBU-KSI-G
                       MOVE    SPA-GMN-KOUHI-ICHIBU-KSI-MAX
                                       TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF
               END-PERFORM
      *     
               IF      FLG-CHK         =   ZERO
                   MOVE    407             TO  SPA-GMN-CUR
                   MOVE    "0138"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4212-KOUHI-ICHIBU-KSI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災レセプト様式区分チェック
      *****************************************************************
       4213-RSI-RECEIPTKBN-CHK-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-RSI-RECEIPTKBN-G
                                   =   SPACE
               MOVE    SPA-GMN-RSI-RECEIPTKBNLST (1)
                                       TO  SPA-GMN-RSI-RECEIPTKBN-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-RSI-RECEIPTKBN-MAX
                   IF      SPA-GMN-RSI-RECEIPTKBN
                                   =   SPA-GMN-RSI-RECEIPTKBNL (IDY)
                       MOVE    SPA-GMN-RSI-RECEIPTKBNLST (IDY)
                                       TO  SPA-GMN-RSI-RECEIPTKBN-G
                       MOVE    SPA-GMN-RSI-RECEIPTKBN-MAX
                                       TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF
               END-PERFORM
      *     
               IF      FLG-CHK         =   ZERO
                   MOVE    316             TO  SPA-GMN-CUR
                   MOVE    "0139"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4213-RSI-RECEIPTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災診療費請求書様式区分チェック
      *****************************************************************
       4214-RSI-INVOICEKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-RSI-INVOICEKBN   =   SPACE
               MOVE    SPACE           TO  SPA-GMN-RSI-INVOICEKBN-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-RSI-INVOICEKBN-MAX
                   IF      SPA-GMN-RSI-INVOICEKBN
                                   =   SPA-GMN-RSI-INVOICEKBNL (IDY)
                       MOVE    SPA-GMN-RSI-INVOICEKBNLST (IDY)
                                           TO  SPA-GMN-RSI-INVOICEKBN-G
                       MOVE    SPA-GMN-RSI-INVOICEKBN-MAX
                                           TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF
               END-PERFORM
      *
               IF      FLG-CHK             =   ZERO
                   MOVE    408                 TO  SPA-GMN-CUR
                   MOVE    "0140"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4214-RSI-INVOICEKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科社保設定フラグチェック
      *****************************************************************
       4148-SYUKA-SYAHO-CHK-SEC   SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-SYUKA-SYAHO-G   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-SYUKA-SYAHO-MAX
                   IF      SPA-GMN-SYUKA-SYAHO
                                       =   SPA-GMN-SYUKA-SYAHOL (IDY)
                       MOVE    SPA-GMN-SYUKA-SYAHOLST (IDY)
                                           TO  SPA-GMN-SYUKA-SYAHO-G
                       MOVE    SPA-GMN-SYUKA-SYAHO-MAX
                                           TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF                                     
               END-PERFORM
      *     
               IF      FLG-CHK             =   ZERO
                   MOVE    95                      TO  SPA-GMN-CUR
                   MOVE    "0039"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4148-SYUKA-SYAHO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科国保設定フラグチェック
      *****************************************************************
       4149-SYUKA-KOKUHO-CHK-SEC   SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-SYUKA-KOKUHO-G  =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-SYUKA-KOKUHO-MAX
                   IF      SPA-GMN-SYUKA-KOKUHO
                                       =   SPA-GMN-SYUKA-KOKUHOL (IDY)
                       MOVE    SPA-GMN-SYUKA-KOKUHOLST (IDY)
                                           TO  SPA-GMN-SYUKA-KOKUHO-G
                       MOVE    SPA-GMN-SYUKA-KOKUHO-MAX
                                           TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF                                     
               END-PERFORM
      *     
               IF      FLG-CHK             =   ZERO
                   MOVE    96                      TO  SPA-GMN-CUR
                   MOVE    "0040"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4149-SYUKA-KOKUHO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科広域連合設定フラグチェック
      *****************************************************************
       4150-SYUKA-KOUIKI-CHK-SEC   SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-SYUKA-KOUIKI-G  =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-SYUKA-KOUIKI-MAX
                   IF      SPA-GMN-SYUKA-KOUIKI
                                       =   SPA-GMN-SYUKA-KOUIKIL (IDY)
                       MOVE    SPA-GMN-SYUKA-KOUIKILST (IDY)
                                           TO  SPA-GMN-SYUKA-KOUIKI-G
                       MOVE    SPA-GMN-SYUKA-KOUIKI-MAX
                                           TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF                                     
               END-PERFORM
      *     
               IF      FLG-CHK             =   ZERO
                   MOVE    97                      TO  SPA-GMN-CUR
                   MOVE    "0050"                  TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4150-SYUKA-KOUIKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    広域連合診療報酬請求書印刷分チェック
      *****************************************************************
       4146-KOUIKIPRTKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-KOUIKIPRTKBN   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-KOUIKIPRTKBN-MAX
                   IF      SPA-GMN-KOUIKIPRTKBN
                                   =   SPA-GMN-KOUIKIPRTKBNL (IDY)
                       MOVE    SPA-GMN-KOUIKIPRTKBNLST (IDY)
                                           TO  SPA-GMN-KOUIKIPRTKBN-G
                       MOVE    SPA-GMN-KOUIKIPRTKBN-MAX
                                           TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF                                     
               END-PERFORM
      *     
               IF      FLG-CHK             =   ZERO
                   MOVE    151                     TO  SPA-GMN-CUR
                   MOVE    "0051"                  TO  SPA-ERRCD
               END-IF
           END-IF     
      *
           .
       4146-KOUIKIPRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災診療費請求書、アフターケア委託費請求書の様式選択チェック
      *****************************************************************
       4200-ROUSAIPRTKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-ROUSAIPRTKBN   =   SPACE
               MOVE    SPACE           TO  SPA-GMN-ROUSAIPRTKBN-G
                                           SPA-GMN-FRAME-ROUSAIPRTKBN-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-ROUSAIPRTKBN-MAX
                   IF      SPA-GMN-ROUSAIPRTKBN
                                   =   SPA-GMN-ROUSAIPRTKBNL (IDY)
                       MOVE    SPA-GMN-ROUSAIPRTKBNLST (IDY)
                                           TO  SPA-GMN-ROUSAIPRTKBN-G
                       MOVE    SPA-GMN-ROUSAIPRTKBN-MAX
                                           TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF
               END-PERFORM
      *
               IF      FLG-CHK             =   ZERO
                   MOVE    311                 TO  SPA-GMN-CUR
                   MOVE    "0126"              TO  SPA-ERRCD
               ELSE
                   IF      SPA-GMN-ROUSAIPRTKBN    =   "0"
                       IF      SPA-GMN-FRAME-ROUSAIPRTKBN-G
                                                       =   SPACE
                           MOVE    SPA-GMN-FRAME-ROUSAIPRTKBNLST (1)
                                                       TO
                                           SPA-GMN-FRAME-ROUSAIPRTKBN-G
                       END-IF
                   END-IF 
                   IF      SPA-GMN-ROUSAIPRTKBN    =   "1"
                       MOVE    SPA-GMN-FRAME-ROUSAIPRTKBNLST (2)
                                                   TO
                                           SPA-GMN-FRAME-ROUSAIPRTKBN-G
                       IF      WRK-MCP-WIDGET      =   "ROUSAIPRTKBN" 
                           MOVE    99                  TO  SPA-GMN-CUR
                       END-IF
                   END-IF 
               END-IF
           END-IF
      *
           .
       4200-ROUSAIPRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災診療費請求書、アフターケア委託費請求書の枠記載区分チェック
      *****************************************************************
       4201-FRAME-ROUSAIPRTKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           IF      SPA-GMN-FRAME-ROUSAIPRTKBN   =   SPACE
               MOVE    SPACE           TO  SPA-GMN-FRAME-ROUSAIPRTKBN-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >      SPA-GMN-FRAME-ROUSAIPRTKBN-MAX
                   IF      SPA-GMN-FRAME-ROUSAIPRTKBN
                                   =   SPA-GMN-FRAME-ROUSAIPRTKBNL (IDY)
                       MOVE    SPA-GMN-FRAME-ROUSAIPRTKBNLST (IDY)
                                       TO  SPA-GMN-FRAME-ROUSAIPRTKBN-G
                       MOVE    SPA-GMN-FRAME-ROUSAIPRTKBN-MAX
                                       TO  IDY
                       MOVE    1       TO  FLG-CHK
                   END-IF
               END-PERFORM
      *     
               IF      FLG-CHK             =   ZERO
                   MOVE    312                 TO  SPA-GMN-CUR
                   MOVE    "0127"              TO  SPA-ERRCD
               ELSE
                   IF      SPA-GMN-ROUSAIPRTKBN    =   "1"
                       MOVE    SPA-GMN-FRAME-ROUSAIPRTKBNLST (2)
                                                   TO
                                           SPA-GMN-FRAME-ROUSAIPRTKBN-G
                   END-IF 
                   IF      SPA-GMN-ROUSAIPRTKBN   =   SPACE
                       MOVE    SPACE               TO
                                           SPA-GMN-FRAME-ROUSAIPRTKBN-G
                   END-IF
               END-IF
           END-IF
      *
           .
       4201-FRAME-ROUSAIPRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       420-HKNJANUMLIST-SEC        SECTION.
      *
           MOVE    ZERO            TO  WRK-SELNUM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-HKNJALIST-MAX
               IF      W12-HKNJANUMLIST-SELECT (IDX)
                                           =   "T"
                   IF      IDX                 =   SPA-GMN-SELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX                 TO  WRK-SELNUM
                       MOVE    SPA-GMN-HKNJALIST-MAX
                                                   TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM         >   ZERO
               MOVE    WRK-SELNUM         TO  SPA-GMN-SELNUM
           END-IF
      *
           IF      SPA-GMN-SELNUM     >   ZERO
               MOVE    SPA-GMN-SELNUM     TO  W12-SELNUM
               MOVE    69                 TO  SPA-GMN-CUR
           END-IF
           .
       420-HKNJANUMLIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    タグ編集処理
      *****************************************************************
       440-TAG-CHANGE-SEC            SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "B08"
                   IF    ( W12-PAGENO  >=  9 )
                       MOVE    0       TO  IDX
                   ELSE
                       COMPUTE IDX =   W12-PAGENO  +   1
                   END-IF
               WHEN    OTHER
                   MOVE   W12-PAGENO   TO  IDX
           END-EVALUATE
      *
      ***  タグが変わったとき項目チェック
      ***  IF      IDX              NOT =   W12-PAGENO
               MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
               MOVE    ZERO                TO  SPA-GMN-CUR
      *
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
               IF    ( SPA-ERRCD            =   SPACE )
      *            関連チェック
                   PERFORM 4900-KANRENCHK-SEC
                   IF    ( SPA-ERRCD        =   SPACE             )
                   AND   ( SPA-WIDCD        =   SPACE             )
                   AND   ( SPA-WIDCD2       =   SPACE  OR  "1004" )
                       EVALUATE    IDX
                           WHEN    0
                               MOVE    1            TO  SPA-GMN-CUR
                           WHEN    1
                               MOVE    400          TO  SPA-GMN-CUR
                           WHEN    2
                               MOVE    42           TO  SPA-GMN-CUR
                           WHEN    3
                               MOVE    212          TO  SPA-GMN-CUR
                           WHEN    4
                               MOVE    100          TO  SPA-GMN-CUR
                           WHEN    5
                               MOVE    70           TO  SPA-GMN-CUR
                           WHEN    6
                               MOVE    50           TO  SPA-GMN-CUR
                           WHEN    7
                               MOVE    150          TO  SPA-GMN-CUR
                           WHEN    8
                               MOVE    90           TO  SPA-GMN-CUR
                           WHEN    9
                               MOVE    66           TO  SPA-GMN-CUR
                       END-EVALUATE
                   END-IF
               END-IF
      ***  END-IF
           .
       440-TAG-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W12"               TO  SPA-MOTOPG
      *
      *     INITIALIZE                  SPA-W01KYOUTU
      *
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
      *       公費記載順設定処理
      *****************************************************************
       490-KOUHI-PRTJYUN-SETTEI-SEC        SECTION.
      *
           MOVE    "W12"               TO  SPA-MOTOPG
           MOVE    "W122"              TO  SPA-SAKIPG
      *
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       490-KOUHI-PRTJYUN-SETTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *       詳細設定処理
      *****************************************************************
       490-SYOUSAI-SETTEI-SEC              SECTION.
      *
           MOVE    "W12"               TO  SPA-MOTOPG
           MOVE    "W121"              TO  SPA-SAKIPG
      *
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       490-SYOUSAI-SETTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC              SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
              GO      TO      490-TOUROKU-EXT
           END-IF
      *
      *    入力項目の関連チェック処理
           PERFORM 4900-KANRENCHK-SEC
      *
      *    レセ電データ出力先チェック
           IF    ( SPA-ERRCD        =   SPACE )
           AND   ( SPA-WIDCD        =   SPACE )
           AND   ( SPA-WIDCD2       =   SPACE )
               PERFORM 4103-RECEDENOUT-CHK-SEC
           END-IF
      *
      *    医師別出力区分と医師名記載区分のチェック
           IF    ( SPA-ERRCD        =   SPACE )
           AND   ( SPA-WIDCD        =   SPACE )
           AND   ( SPA-WIDCD2       =   SPACE )
               IF    ( SPA-GMN-DRCD-PRTKBN
                                       NOT =   "1" )
               AND   ( SPA-GMN-DRNAME-KSIKBN
                                           =   "1" )
                   MOVE    303                 TO  SPA-GMN-CUR
                   MOVE    "0118"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD        =   SPACE )
           AND   ( SPA-WIDCD        =   SPACE )
           AND   ( SPA-WIDCD2       =   SPACE )
              MOVE    "1001"           TO  SPA-WIDCD
           END-IF
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC           SECTION.
      *
      *    レセプト編綴順情報
           IF    ( SPA-GMN-PREFKBN-PRTJYUN =   SPACE )
           AND   ( SPA-GMN-HKNJANUM-PRTJYUN
                                           =   SPACE )
           AND   ( SPA-GMN-RECESYUBETU-PRTJYUN
                                           =   SPACE )
           AND   ( SPA-GMN-ZAITAKU         =   SPACE )
           AND   ( SPA-GMN-CHOUKI          =   SPACE )
           AND   ( SPA-GMN-HKNJANUM-PRTKBN =   SPACE )
           AND   ( SPA-GMN-HKNJANUM        =   SPACE )
               CONTINUE
           ELSE
      *        印刷順指定が全て入力されているか  
               IF    ( SPA-GMN-PREFKBN-PRTJYUN =   SPACE )
               OR    ( SPA-GMN-HKNJANUM-PRTJYUN
                                               =   SPACE )
               OR   ( SPA-GMN-RECESYUBETU-PRTJYUN
                                               =   SPACE )
                   MOVE    70                  TO  SPA-GMN-CUR
                   MOVE    "0097"              TO  SPA-ERRCD
               END-IF
      *        医療機関の所在地保険者と所在地保険者印刷位置
               IF      SPA-ERRCD               =   SPACE  
                   IF    ( SPA-GMN-HKNJANUM-PRTKBN
                                               NOT =   SPACE )
                   AND   ( SPA-GMN-HKNJANUM        =   SPACE )
                       MOVE    75                  TO  SPA-GMN-CUR
                       MOVE    "0098"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *    印刷順（国保）と編綴順設定の関連チェック
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-PRTJYUN3    =   "06"
                   IF    ( SPA-GMN-PREFKBN-PRTJYUN =   SPACE )
                   OR    ( SPA-GMN-HKNJANUM-PRTJYUN
                                                   =   SPACE )
                   OR   ( SPA-GMN-RECESYUBETU-PRTJYUN
                                                   =   SPACE )
                       MOVE    70                  TO  SPA-GMN-CUR
                       MOVE    "0096"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF                         
      *
      *    レセプト・総括編集情報（主科設定）
           IF      SPA-ERRCD           =   SPACE
               IF    ( SPA-GMN-SYUKA-KBN       =   ZERO  )
               AND   ( SPA-GMN-SYUKA-NYUIN     =   SPACE )
               AND   ( SPA-GMN-SYUKA-GAIRAI    =   SPACE )
               AND   ( SPA-GMN-SYUKA-MODE      =   SPACE )
               AND   ( SPA-GMN-SYUKA-SELECT    =   SPACE )
               AND   ( SPA-GMN-SYUKA-SYAHO     =   SPACE )
               AND   ( SPA-GMN-SYUKA-KOKUHO    =   SPACE )
               AND   ( SPA-GMN-SYUKA-KOUIKI    =   SPACE )
                   CONTINUE
               ELSE
                   IF    ( SPA-GMN-SYUKA-KBN       =   ZERO )
      *****************IF    ( SPA-GMN-SYUKA-NYUIN     =   SPACE )
      **               AND   ( SPA-GMN-SYUKA-GAIRAI    =   SPACE )
      **               AND   ( SPA-GMN-SYUKA-MODE      =   SPACE )
      **               AND   ( SPA-GMN-SYUKA-SELECT    =   SPACE )
      **               AND   ( SPA-GMN-SYUKA-SYAHO     =   SPACE )
      **               AND   ( SPA-GMN-SYUKA-KOKUHO    =   SPACE )
      **               AND   ( SPA-GMN-SYUKA-KOUIKI    =   SPACE )
      **                   CONTINUE
      **               ELSE
      **                   MOVE    91                  TO  SPA-GMN-CUR
      **                   MOVE    "0100"              TO  SPA-ERRCD
      *****************END-IF
                       CONTINUE
                   ELSE
                       IF      SPA-1001-HOSPSBT1   =   1
                           MOVE    90                  TO  SPA-GMN-CUR
                           MOVE    "0101"              TO  SPA-ERRCD
                       ELSE
                           IF    ( SPA-GMN-SYUKA-GAIRAI    =   SPACE )
                           OR    ( SPA-GMN-SYUKA-MODE      =   SPACE )
                           OR    ( SPA-GMN-SYUKA-SELECT    =   SPACE )
                               MOVE    91              TO  SPA-GMN-CUR
                               MOVE    "0099"          TO  SPA-ERRCD
                           END-IF
                           IF    ( SPA-ERRCD               =   SPACE )
                           AND   ( SPA-GMN-SYUKA-SYAHO     =   SPACE
                                                           OR  "0"   )
                           AND   ( SPA-GMN-SYUKA-KOKUHO    =   SPACE
                                                           OR  "0"   )
                           AND   ( SPA-GMN-SYUKA-KOUIKI    =   SPACE
                                                           OR  "0"   )
                               MOVE    95              TO  SPA-GMN-CUR
                               MOVE    "0102"          TO  SPA-ERRCD
                           END-IF
                           IF    ( SPA-ERRCD               =   SPACE )
                           AND   ( SPA-BEDSU               >   ZERO  )
                           AND   ( SPA-GMN-SYUKA-NYUIN     =   SPACE )
                               MOVE    91              TO  SPA-GMN-CUR
                               MOVE    "0099"          TO  SPA-ERRCD
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    記号番号区切り文字設定の関連チェック
           IF      SPA-ERRCD           =   SPACE
               IF    ( SPA-GMN-KIGO-DELI-FLG
                                       NOT =   "0" )
               AND   ( SPA-GMN-KIGO-DELI-HKNJA-FLG
                                           =   "0" )
                   MOVE    117                 TO  SPA-GMN-CUR
                   MOVE    "0112"              TO  SPA-ERRCD                    
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF    ( SPA-GMN-NUM-DELIMITER-FLG
                                       NOT =   "0" )
               AND   ( SPA-GMN-NUM-DELI-HKNJA-FLG
                                           =   "0" )
                   MOVE    119                 TO  SPA-GMN-CUR
                   MOVE    "0112"              TO  SPA-ERRCD                    
               END-IF
           END-IF
      *
      *    レセ電出力区分と広域連合作成区分チェック
           IF    ( SPA-ERRCD        =   SPACE )
           AND   ( SPA-WIDCD        =   SPACE )
      *        入力チェック
               IF      SPA-WIDCD-1004-FLG  =   2
                   CONTINUE
               ELSE
                   IF    ( SPA-GMN-KOUIKIKBN   =   SPA-GMN-KOUIKIKBN-O )
                   AND   ( SPA-GMN-KKHRECEKBN  =   SPA-GMN-KKHRECEKBN-O)
      *************AND   ( SPA-GMN-SYARECEKBN  =   SPA-GMN-SYARECEKBN-O)
                   AND   ( SPA-GMN-KOUIKIRECEKBN
                                               =
                                               SPA-GMN-KOUIKIRECEKBN-O )
                       MOVE    ZERO            TO  SPA-WIDCD-1004-FLG
                   ELSE
                       MOVE    1               TO  SPA-WIDCD-1004-FLG
                   END-IF
                   MOVE    SPA-GMN-KKHRECEKBN  TO  SPA-GMN-KKHRECEKBN-O 
                   MOVE    SPA-GMN-SYARECEKBN  TO  SPA-GMN-SYARECEKBN-O 
                   MOVE    SPA-GMN-KOUIKIRECEKBN
                                               TO
                                               SPA-GMN-KOUIKIRECEKBN-O
                   MOVE    SPA-GMN-KOUIKIKBN   TO  SPA-GMN-KOUIKIKBN-O
               END-IF
      *
               IF      SPA-WIDCD-1004-FLG  >   ZERO
                   IF    ( SPA-GMN-KOUIKIKBN   =   "0" OR  SPACE )
      *****************IF    ( SPA-GMN-SYARECEKBN
      *****************                        NOT =   "0" AND SPACE )
                       IF    ( SPA-GMN-KKHRECEKBN
                                               NOT =   "0" AND SPACE )
                       OR    ( SPA-GMN-KOUIKIRECEKBN
                                               NOT =   "0" AND SPACE )
                           MOVE    "1004"          TO  SPA-WIDCD2
                       END-IF
                   END-IF 
               END-IF
      *
               MOVE    ZERO        TO  SPA-WIDCD-1004-FLG 
           END-IF
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
      *    レセプト（基本・特別療養費）総括編集情報
           MOVE    SPACE               TO  SYS-2005-REC
           INITIALIZE                      SYS-2005-REC
           MOVE    "2005"                  TO  SYS-2005-KANRICD
           MOVE    "*"                     TO  SYS-2005-KBNCD
           MOVE    SPA-W01-SEL-STYUKYMD    TO  SYS-2005-STYUKYMD
           MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYS-2005-EDYUKYMD
           MOVE    SPA-HOSPNUM             TO  SYS-2005-HOSPNUM
           MOVE    SYS-2005-REC            TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-2005-REC
               MOVE    SPACE                   TO  SYS-2005-TERMID 
           ELSE
               MOVE    SPACE                   TO  SYS-2005-REC
               INITIALIZE                          SYS-2005-REC
               MOVE    "2005"                  TO  SYS-2005-KANRICD
               MOVE    "*"                     TO  SYS-2005-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD        TO  SYS-2005-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-2005-EDYUKYMD
           MOVE    SPA-GMN-BYOMEIHENKBN    TO  SYS-2005-BYOMEIHENKBN
           MOVE    SPA-GMN-BYOMEIHENKBN1   TO  SYS-2005-BYOMEIHENKBN1
           MOVE    SPA-GMN-SYUBYOHENFLG    TO  SYS-2005-SYUBYOHENFLG
           MOVE    SPA-GMN-TENKIHENKBN     TO  SYS-2005-TENKIHENKBN
           MOVE    SPA-GMN-KKHRECEKBN      TO  SYS-2005-KKHRECEKBN
           MOVE    SPA-GMN-SYARECEKBN      TO  SYS-2005-SYARECEKBN
           MOVE    SPA-GMN-RECEDENOUT      TO  SYS-2005-RECEDENOUT
           MOVE    SPA-GMN-SYAOCRKBN       TO  SYS-2005-SYAOCRKBN
           MOVE    SPA-GMN-KKHOCRKBN       TO  SYS-2005-KKHOCRKBN
           MOVE    SPA-GMN-NYUTAIIN-KSIKBN
                                           TO
                                           SYS-2005-NYUTAIINREKI-KSIKBN
           MOVE    SPA-GMN-SYAPRTKBN       TO
                                           SYS-2005-MARUCHO-SYAPRTKBN
           MOVE    SPA-GMN-KKHPRTKBN       TO  SYS-2005-KKHPRTKBN
           MOVE    SPA-GMN-PREFPRGID       TO  SYS-2005-PREFPRGID (1)
           MOVE    SPA-GMN-PREFPRGID1      TO  SYS-2005-PREFPRGID (2)
           MOVE    SPA-GMN-SYAPRTNM        TO  SYS-2005-SYAPRTNM
           MOVE    SPA-GMN-KKHPRTNM        TO  SYS-2005-KKHPRTNM
           MOVE    SPA-GMN-RENNUMHENKBN    TO  SYS-2005-RENNUMHENKBN
           MOVE    SPA-GMN-NEXTPRTKBN      TO  SYS-2005-NEXTPRTKBN
           MOVE    SPA-GMN-NEXTPRTKBN1     TO  SYS-2005-NEXTPRTKBN-G
           MOVE    SPA-GMN-RJNGNBKKBN      TO  SYS-2005-RJNGNBKKBN
           MOVE    SPA-GMN-JIHITEKKSIKBN   TO  SYS-2005-JIHITEKKSIKBN
           MOVE    SPA-GMN-SYAPRTNM-NEXT   TO  SYS-2005-SYAPRTNM-NEXT
           MOVE    SPA-GMN-KKHPRTNM-NEXT   TO  SYS-2005-KKHPRTNM-NEXT
           MOVE    SPA-GMN-SRYKAHENKBN     TO  SYS-2005-SRYKAHENKBN
           MOVE    SPA-GMN-RECECOMKBN      TO  SYS-2005-RECECOMKBN
           MOVE    SPA-GMN-SYAPRTNM-N      TO  SYS-2005-SYAPRTNM-N
           MOVE    SPA-GMN-KKHPRTNM-N      TO  SYS-2005-KKHPRTNM-N
           MOVE    SPA-GMN-SYAPRTNM-NEXT-N TO  SYS-2005-SYAPRTNM-NEXT-N
           MOVE    SPA-GMN-KKHPRTNM-NEXT-N TO  SYS-2005-KKHPRTNM-NEXT-N
           MOVE    SPA-GMN-ROUPRTNM        TO  SYS-2005-ROUPRTNM
           MOVE    SPA-GMN-JBAIPRTNM       TO  SYS-2005-JBAIPRTNM
           MOVE    SPA-GMN-ROUPRTNM-NEXT   TO  SYS-2005-ROUPRTNM-NEXT
           MOVE    SPA-GMN-JBAIPRTNM-NEXT  TO  SYS-2005-JBAIPRTNM-NEXT
           MOVE    SPA-GMN-ROUPRTNM-N      TO  SYS-2005-ROUPRTNM-N
           MOVE    SPA-GMN-JBAIPRTNM-N     TO  SYS-2005-JBAIPRTNM-N
           MOVE    SPA-GMN-ROUPRTNM-NEXT-N TO  SYS-2005-ROUPRTNM-NEXT-N
           MOVE    SPA-GMN-JBAIPRTNM-NEXT-N
                                           TO  SYS-2005-JBAIPRTNM-NEXT-N
           MOVE    SPA-GMN-FRAME-JIBAIPRTKBN
                                           TO
                                           SYS-2005-FRAME-JIBAIPRTKBN
           MOVE    SPA-GMN-FRAME-JIBAIPRTKBN-N
                                           TO
                                           SYS-2005-FRAME-JIBAIPRTKBN-N
           MOVE    SPA-GMN-AUTODSPKBN      TO  SYS-2005-AUTODSPKBN
           MOVE    SPA-GMN-PRTJYUN1        TO  SYS-2005-PRTJYUNKBN1
           MOVE    SPA-GMN-PRTJYUN2        TO  SYS-2005-PRTJYUNKBN2
           MOVE    SPA-GMN-PRTJYUN3        TO  SYS-2005-PRTJYUNKBN3
           MOVE    SPA-GMN-PRTJYUN4        TO  SYS-2005-PRTJYUNKBN4
           MOVE    SPA-GMN-MOJI-KISAIKBN   TO  SYS-2005-MOJI-KISAIKBN
           MOVE    SPA-GMN-KYURATE-KISAIKBN
                                           TO  SYS-2005-KYURATE-KISAIKBN
           MOVE    SPA-GMN-RECESYUBET-KISAIKBN
                                           TO
                                           SYS-2005-RECESYUBETU-KISAIKBN
           MOVE    SPA-GMN-NI-JYOKEN1      TO  SYS-2005-NI-JYOKEN(1)
           MOVE    SPA-GMN-NI-JYOKEN2      TO  SYS-2005-NI-JYOKEN(2)
           MOVE    SPA-GMN-NI-JYOKEN3      TO  SYS-2005-NI-JYOKEN(3)
           MOVE    SPA-GMN-NI-JYOKEN4      TO  SYS-2005-NI-JYOKEN(4)
           MOVE    SYS-2005-REC            TO  SYSKANRI-REC
      *     
           PERFORM 800-MCNDATE-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SMCNDATE-YMD            TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
               MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    SPA-HOSPNUM             TO  SYSUP-HOSPNUM
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "upd1"                  TO  MCP-PATHNAME
               MOVE    "DBUPDATE"              TO  MCP-FUNC
           ELSE
               MOVE    SMCNDATE-YMD            TO  SYS-CREYMD
                                                   SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               MOVE    "DBINSERT"              TO  MCP-FUNC
           END-IF
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1003"                  TO  SPA-ERRCD
               GO  TO  490-KOUSHIN-EXT
           END-IF      
      *
      *    レセプト（基本・特別療養費）総括編集情報
      *    区分コード０１
           MOVE    SPACE               TO  SYS-25-REC
           INITIALIZE                      SYS-25-REC
           MOVE    SPA-HOSPNUM         TO  SYS-25-HOSPNUM
           MOVE    "2005"              TO  SYS-25-KANRICD
           MOVE    "01"                TO  SYS-25-KBNCD
           MOVE    SPA-W01-SEL-STYUKYMD
                                       TO  SYS-25-STYUKYMD
           MOVE    SPA-W01-SEL-EDYUKYMD
                                       TO  SYS-25-EDYUKYMD
           MOVE    SYS-25-REC          TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-25-REC
           ELSE
               MOVE    SPACE                   TO  SYS-25-REC
               INITIALIZE                          SYS-25-REC
               MOVE    "2005"                  TO  SYS-25-KANRICD
               MOVE    "01"                    TO  SYS-25-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD        TO  SYS-25-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-25-EDYUKYMD
           MOVE    SPA-GMN-JIBAIPRTKBN     TO  SYS-25-JIBAIPRTKBN
           MOVE    SPA-GMN-JIBAIPRTKBN-N   TO  SYS-25-JIBAIPRTKBN-N
           MOVE    SPA-GMN-JIBAIPRTKBN1    TO  SYS-25-JIBAIPRTKBN1
           MOVE    SPA-GMN-JIBAIPRTKBN1-N  TO  SYS-25-JIBAIPRTKBN1-N
           MOVE    SPA-GMN-JIBAI-FRACTION-KBN1
                                           TO
                                       SYS-25-JIBAI-FRACTION-KBN     (1)
           MOVE    SPA-GMN-JIBAI-FRACTION-KBN2
                                           TO
                                       SYS-25-JIBAI-FRACTION-KBN     (2)
           MOVE    SPA-GMN-JIBAI-PDF       TO  SYS-25-JIBAI-PDF
           MOVE    SPA-GMN-HKN-RIREKI      TO  SYS-25-HKN-RIREKI-KBN
           MOVE    SPA-GMN-GENBUTU-ICHIBU  TO
                                       SYS-25-GENBUTU-ICHIBU-KSIKBN
           MOVE    SPA-GMN-COMPRESS        TO  SYS-25-COMPRESS-KBN
           MOVE    SPA-GMN-DISASTER-RECEIPT
                                           TO
                                           SYS-25-DISASTER-RECEIPT
           MOVE    SPA-GMN-15-YKZ          TO  SYS-25-15-YKZ-PRTKBN
           MOVE    SPA-GMN-KOUHI-ICHIBU-KSI
                                           TO
                                       SYS-25-KOUHI-ICHIBU-KSIKBN
           MOVE    SPA-GMN-BTUNUM-KSIKBN   TO  SYS-25-BTUNUM-KSIKBN
           MOVE    SPA-GMN-DRNAME-KSIKBN   TO  SYS-25-DRNAME-KSIKBN
           MOVE    SPA-GMN-PRT-CHECKKBN    TO  SYS-25-PRT-CHECKKBN
           MOVE    SPA-GMN-JIHI-PRTKBN     TO  SYS-25-JIHI-PRTKBN
           MOVE    SPA-GMN-PRTJYUN5        TO  SYS-25-PRTJYUNKBN5
           MOVE    SPA-GMN-PRTJYUN6        TO  SYS-25-PRTJYUNKBN6
           MOVE    SPA-GMN-KOUIKIOCRKBN    TO  SYS-25-KOUIKIOCRKBN
           MOVE    SPA-GMN-KOUIKIKBN       TO  SYS-25-KOUIKIKBN
           MOVE    SPA-GMN-KOUIKIRECEKBN   TO  SYS-25-KOUIKIRECEKBN
           MOVE    SPA-GMN-NI-JYOKEN5      TO  SYS-25-NI-JYOKEN(1)
           MOVE    SPA-GMN-NI-JYOKEN6      TO  SYS-25-NI-JYOKEN(2)
           MOVE    SPA-GMN-KOUIKIPRTNM     TO  SYS-25-KOUIKIPRTNM
           MOVE    SPA-GMN-KOUIKIPRTNM-NEXT
                                           TO
                                           SYS-25-KOUIKIPRTNM-NEXT
           MOVE    SPA-GMN-KOUIKIPRTNM-N   TO  SYS-25-KOUIKIPRTNM-N
           MOVE    SPA-GMN-KOUIKIPRTNM-NEXT-N
                                           TO  SYS-25-KOUIKIPRTNM-NEXT-N
           MOVE    SPA-GMN-KOUIKIPRTKBN    TO  SYS-25-KOUIKIPRTKBN
           MOVE    SPA-GMN-RSI-INVOICEKBN  TO  SYS-25-RSI-INVOICE-KBN
           MOVE    SPA-GMN-ROUSAIPRTKBN    TO  SYS-25-ROUSAIPRTKBN
           MOVE    SPA-GMN-FRAME-ROUSAIPRTKBN
                                           TO
                                           SYS-25-FRAME-ROUSAIPRTKBN
           MOVE    SPA-GMN-DRCD-PRTKBN     TO  SYS-25-DRCD-PRTKBN
           MOVE    SPA-GMN-DRCD-PRTJYUNKBN TO
                                           SYS-25-DRCD-PRTJYUNKBN
           MOVE    SPA-GMN-BTUNUM-PRTKBN   TO  SYS-25-BTUNUM-PRTKBN
           MOVE    SPA-GMN-BTUNUM-PRTJYUNKBN
                                           TO
                                           SYS-25-BTUNUM-PRTJYUNKBN
           MOVE    SPA-GMN-TERMINAL-DIG-PRTKBN
                                           TO
                                       SYS-25-TERMINAL-DIGIT-PRTKBN
           MOVE    SPA-GMN-COMMENT-NYUINRYO
                                           TO
                                   SYS-25-COMMENT-NYUINRYO-KSIKBN
           MOVE    SPA-GMN-COMMENT-NYUINRYO1
                                           TO
                                   SYS-25-COMMENT-NYUINRYO1-KSIKBN
           MOVE    SPA-GMN-CHKKBN          TO  SYS-25-CHKKBN
           MOVE    SPA-GMN-BYOMEI-PRTJYUN  TO  SYS-25-BYOMEI-PRTJYUN
           MOVE    SPA-GMN-SHOKUJI-PRTKBN  TO  SYS-25-SHOKUJI-PRTKBN
           MOVE    SPA-GMN-SEIHO-DSPKBN    TO  SYS-25-SEIHO-DSPKBN
           MOVE    SPA-GMN-RECEIPT-MAKEKBN TO
                                           SYS-25-RECEIPT-MAKE-KBN
           MOVE    SPA-GMN-ZERO-KOH-PRTKBN TO
                                           SYS-25-ZERO-KOH-PRTKBN
           MOVE    SPA-GMN-FRAME-ROUPRTKBN
                                           TO
                                           SYS-25-FRAME-ROUPRTKBN
           MOVE    SPA-GMN-FRAME-ROUPRTKBN-N
                                           TO
                                           SYS-25-FRAME-ROUPRTKBN-N
           MOVE    SPA-GMN-COMMENT-ROU     TO
                                           SYS-25-COMMENT-ROU-KSIKBN
           MOVE    SPA-GMN-RECEDEN-CDRKBN  TO  SYS-25-RECEDEN-CDRKBN
           MOVE    SPA-GMN-RECEDEN-LST1KBN TO
                                           SYS-25-RECEDEN-LST1KBN
           MOVE    SPA-GMN-RECEDEN-LST2KBN TO
                                           SYS-25-RECEDEN-LST2KBN
           MOVE    SPA-GMN-RECEDEN-LST2-MEDIA
                                           TO
                                       SYS-25-RECEDEN-LST2-MEDIA
           MOVE    SPA-GMN-RECEDEN-RIREKIKBN   TO
                                           SYS-25-RECEDEN-RIREKIKBN
           MOVE    SPA-GMN-RECEDEN-DATAKBN TO
                                           SYS-25-RECEDEN-DATAKBN
           MOVE    SPA-GMN-RECEDEN-DISASTER
                                           TO
                                           SYS-25-RECEDEN-DISASTER
           MOVE    SPA-GMN-RECEDEN-CHK     TO
                                           SYS-25-RECEDEN-CHK-KBN
           MOVE    SPA-GMN-INSPECT-CHARA-KBN
                                           TO
                                       SYS-25-INSPECT-CHARACTER-KBN
           MOVE    SPA-GMN-INSPECT-ZERO-KBN
                                           TO
                                           SYS-25-INSPECT-ZERO-KBN
           MOVE    SPA-GMN-INSPECT-INCLUDE-KBN
                                           TO
                                       SYS-25-INSPECT-INCLUDE-KBN
           MOVE    SPA-GMN-INSPECT-GENERAL-KBN
                                           TO
                                       SYS-25-INSPECT-GENERAL-KBN
           MOVE    SPA-GMN-RSI-RECEIPTKBN  TO  SYS-25-RSI-RECEIPT-KBN
           MOVE    SPA-GMN-ROUPRTKBN       TO  SYS-25-ROUPRTKBN
           MOVE    SPA-GMN-ROUPRTKBN-N     TO  SYS-25-ROUPRTKBN-N
           MOVE    SPA-GMN-TENKIBYOMEI-KBN TO
                                           SYS-25-TENKIBYOMEI-KBN
           MOVE    SPA-GMN-NYUTAIIN-NYU-KSIKBN
                                           TO
                                   SYS-25-NYUTAIINREKI-NYU-KSIKBN
           MOVE    SPA-GMN-ROURECEKBN      TO  SYS-25-ROURECEKBN
           MOVE    SPA-GMN-DISASTER-SYAPRTKBN
                                           TO
                                           SYS-25-DISASTER-SYAPRTKBN
           MOVE    SYS-25-REC              TO  SYSKANRI-REC
      *     
           PERFORM 800-MCNDATE-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SMCNDATE-YMD            TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
               MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    SPA-HOSPNUM             TO  SYSUP-HOSPNUM
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "upd1"                  TO  MCP-PATHNAME
               MOVE    "DBUPDATE"              TO  MCP-FUNC
           ELSE
               MOVE    SMCNDATE-YMD            TO  SYS-CREYMD
                                                   SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               MOVE    "DBINSERT"              TO  MCP-FUNC
           END-IF
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1003"                  TO  SPA-ERRCD
               GO  TO  490-KOUSHIN-EXT
           END-IF      
      *
      *    レセプト（基本・特別療養費）総括編集情報
      *    区分コード０５
           MOVE    SPACE               TO  SYS-200505-REC
           INITIALIZE                      SYS-200505-REC
           MOVE    SPA-HOSPNUM         TO  SYS-200505-HOSPNUM
           MOVE    "2005"              TO  SYS-200505-KANRICD
           MOVE    "05"                TO  SYS-200505-KBNCD
           MOVE    SPA-W01-SEL-STYUKYMD
                                       TO  SYS-200505-STYUKYMD
           MOVE    SPA-W01-SEL-EDYUKYMD
                                       TO  SYS-200505-EDYUKYMD
           MOVE    SYS-200505-REC      TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-200505-REC
           ELSE
               MOVE    SPACE               TO  SYS-200505-REC
               INITIALIZE                      SYS-200505-REC
               MOVE    "2005"              TO  SYS-200505-KANRICD
               MOVE    "05"                TO  SYS-200505-KBNCD
           END-IF
      *
           IF    ( FLG-SYSKANRI        =   1    )
           AND   ( SPA-GMN-HKNJALIST-MAX
                                       =   ZERO )
               CONTINUE
           ELSE
               MOVE    SPA-W01-STYUKYMD        TO  SYS-200505-STYUKYMD
               MOVE    SPA-W01-EDYUKYMD        TO  SYS-200505-EDYUKYMD
               INITIALIZE                      SYS-200505-HKNJANUM-T
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-HKNJALIST-MAX
                   MOVE    SPA-GMN-HKNJALIST-THKNJANUM      (IDX)
                                           TO  SYS-200505-HKNJANUM (IDX)
                   MOVE    SPA-GMN-HKNJALIST-THKNJANUM-63   (IDX)
                                           TO  SYS-200505-HKNJANUM-63
                                                                   (IDX)
               END-PERFORM
               MOVE    SYS-200505-REC          TO  SYSKANRI-REC
      *     
               PERFORM 800-MCNDATE-SEC
               IF      FLG-SYSKANRI            =   ZERO
                   MOVE    SMCNDATE-YMD            TO  SYS-UPYMD
                   MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
                   MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
                   MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
                   MOVE    SPA-W01-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
                   MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
                   MOVE    SPA-HOSPNUM             TO  SYSUP-HOSPNUM
                   MOVE    "tbl_syskanri"          TO  MCP-TABLE
                   MOVE    "upd1"                  TO  MCP-PATHNAME
                   MOVE    "DBUPDATE"              TO  MCP-FUNC
               ELSE
                   MOVE    SMCNDATE-YMD            TO  SYS-CREYMD
                   MOVE    "tbl_syskanri"          TO  MCP-TABLE
                   MOVE    "key"                   TO  MCP-PATHNAME
                   MOVE    "DBINSERT"              TO  MCP-FUNC
               END-IF
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "1003"                  TO  SPA-ERRCD
                   GO  TO  490-KOUSHIN-EXT
               END-IF
           END-IF
      *
      *    レセプト・総括編集情報（主科設定）
           MOVE    SPACE               TO  SYS-2009-REC
           INITIALIZE                      SYS-2009-REC
           MOVE    "2009"              TO  SYS-2009-KANRICD
           MOVE    "*"                 TO  SYS-2009-KBNCD
           MOVE    SPA-W01-SEL-STYUKYMD
                                       TO  SYS-2009-STYUKYMD
           MOVE    SPA-W01-SEL-EDYUKYMD
                                       TO  SYS-2009-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2009-HOSPNUM
           MOVE    SYS-2009-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2009-REC
           ELSE
               MOVE    SPACE               TO  SYS-2009-REC
               INITIALIZE                      SYS-2009-REC
               MOVE    "2009"              TO  SYS-2009-KANRICD
               MOVE    "*"                 TO  SYS-2009-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD    TO  SYS-2009-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-2009-EDYUKYMD
           MOVE    SPA-GMN-SYUKA-KBN   TO  SYS-2009-SYUKA-KBN
           MOVE    SPA-GMN-SYUKA-NYUIN TO  SYS-2009-SYUKA-NYUIN
           MOVE    SPA-GMN-SYUKA-GAIRAI
                                       TO  SYS-2009-SYUKA-GAIRAI 
           MOVE    SPA-GMN-SYUKA-MODE  TO  SYS-2009-SYUKA-MODE
           MOVE    SPA-GMN-SYUKA-SELECT
                                       TO  SYS-2009-SYUKA-SELECT
           MOVE    SPA-GMN-SYUKA-SYAHO TO  SYS-2009-SYUKA-SYAHO
           MOVE    SPA-GMN-SYUKA-KOKUHO
                                       TO  SYS-2009-SYUKA-KOKUHO
           MOVE    SPA-GMN-SYUKA-KOUIKI
                                       TO  SYS-2009-SYUKA-KOUIKI
           MOVE    SYS-2009-REC        TO  SYSKANRI-REC
      *     
           PERFORM 800-MCNDATE-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SMCNDATE-YMD        TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
               MOVE    SYS-KANRICD         TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD           TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD
                                           TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD
                                           TO  SYSUP-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           ELSE
               MOVE    SMCNDATE-YMD        TO  SYS-CREYMD
                                               SYS-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBINSERT"          TO  MCP-FUNC
           END-IF
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1003"                  TO  SPA-ERRCD
               GO  TO  490-KOUSHIN-EXT
           END-IF      
      *
      *    記号番号区切り文字設定情報
           MOVE    SPACE               TO  S21-REC
           INITIALIZE                      S21-REC
           MOVE    "2011"              TO  S21-KANRICD
           MOVE    "*"                 TO  S21-KBNCD
           MOVE    SPA-W01-SEL-STYUKYMD
                                       TO  S21-STYUKYMD
           MOVE    SPA-W01-SEL-EDYUKYMD
                                       TO  S21-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  S21-HOSPNUM
           MOVE    S21-REC             TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  S21-REC
           ELSE
               MOVE    SPACE               TO  S21-REC
               INITIALIZE                      S21-REC
               MOVE    "2011"              TO  S21-KANRICD
               MOVE    "*"                 TO  S21-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD    TO  S21-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  S21-EDYUKYMD
           MOVE    SPA-GMN-KIGO-DELI-FLG
                                       TO  S21-KIGO-DELIMITER-FLG
           MOVE    SPA-GMN-KIGO-DELI-HKNJA-FLG
                                       TO
                                   S21-KIGO-DELIMITER-HKNJANUM-FLG
           MOVE    SPA-GMN-NUM-DELIMITER-FLG
                                       TO  S21-NUM-DELIMITER-FLG
           MOVE    SPA-GMN-NUM-DELI-HKNJA-FLG
                                       TO
                                   S21-NUM-DELIMITER-HKNJANUM-FLG
           MOVE    S21-REC             TO  SYSKANRI-REC
      *     
           PERFORM 800-MCNDATE-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SMCNDATE-YMD        TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
               MOVE    SYS-KANRICD         TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD           TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD
                                           TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD
                                           TO  SYSUP-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           ELSE
               MOVE    SMCNDATE-YMD        TO  SYS-CREYMD
                                               SYS-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBINSERT"          TO  MCP-FUNC
           END-IF
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1003"                  TO  SPA-ERRCD
               GO  TO  490-KOUSHIN-EXT
           END-IF      
      *
      *    レセプト（編綴順）情報
      *
           MOVE    ZERO                TO  FLG-UPDFLG
      *
      *    項目が未入力で未登録の場合は更新しない 
           IF    ( SPA-GMN-PREFKBN-PRTJYUN =   SPACE )
           AND   ( SPA-GMN-HKNJANUM-PRTJYUN
                                           =   SPACE )
           AND   ( SPA-GMN-RECESYUBETU-PRTJYUN
                                           =   SPACE )
           AND   ( SPA-GMN-ZAITAKU         =   SPACE )
           AND   ( SPA-GMN-CHOUKI          =   SPACE )
           AND   ( SPA-GMN-HKNJANUM-PRTKBN =   SPACE )
           AND   ( SPA-GMN-HKNJANUM        =   SPACE )
           AND   ( SPA-NAI-2008-KBNCD-9    =   ZERO  )
               CONTINUE
           ELSE
      *        項目に変更があった場合
               IF      SPA-NAI-2008-KBNCD-9    >   ZERO
                   MOVE    SPACE               TO  SYS-2008-REC
                   INITIALIZE                      SYS-2008-REC
                   MOVE    "2008"              TO  SYS-2008-KANRICD
                   MOVE    SPA-NAI-2008-KBNCD  TO  SYS-2008-KBNCD
                   MOVE    SPA-W01-SEL-STYUKYMD
                                               TO  SYS-2008-STYUKYMD
                   MOVE    SPA-W01-SEL-EDYUKYMD
                                               TO  SYS-2008-EDYUKYMD
                   MOVE    SPA-HOSPNUM         TO  SYS-2008-HOSPNUM
                   MOVE    SYS-2008-REC        TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
                   MOVE    "key10"             TO  WRK-MCP-PATHNAME
                   PERFORM 900-SYSKANRI-INV-SEC
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC         TO  SYS-2008-REC
      *
                       IF    ( SPA-GMN-PREFKBN-PRTJYUN     =
                                           SYS-2008-PREFKBN-PRTJYUN    )
                       AND   ( SPA-GMN-HKNJANUM-PRTJYUN    =
                                           SYS-2008-HKNJANUM-PRTJYUN   )
                       AND   ( SPA-GMN-RECESYUBETU-PRTJYUN =
                                           SYS-2008-RECESYUBETU-PRTJYUN)
                       AND   ( SPA-GMN-ZAITAKU             =
                                           SYS-2008-ZAITAKU            )
                       AND   ( SPA-GMN-CHOUKI              =
                                           SYS-2008-CHOUKI             )
                       AND   ( SPA-GMN-HKNJANUM-PRTKBN     =
                                           SYS-2008-HKNJANUM-PRTKBN    )
                       AND   ( SPA-GMN-HKNJANUM            =
                                           SYS-2008-HKNJANUM           )
                           CONTINUE
                       ELSE
                           MOVE    1                   TO  FLG-UPDFLG
                       END-IF
                   ELSE
                       MOVE    1                   TO  FLG-UPDFLG
                   END-IF
               ELSE 
                   MOVE    1                   TO  FLG-UPDFLG
               END-IF
      *
               IF      FLG-UPDFLG          =   1    
                   MOVE    SPACE               TO  SYS-2008-REC
                   INITIALIZE                      SYS-2008-REC
                   MOVE    "2008"              TO  SYS-2008-KANRICD
                   MOVE    SPA-NAI-2008-KBNCD-9
                                               TO  WRK-KBNCD-9
                   IF      WRK-KBNCD-9         =   99999999
                       MOVE    1                   TO  WRK-KBNCD-9
                   ELSE
                       ADD     1                   TO  WRK-KBNCD-9
                   END-IF
                   MOVE    WRK-KBNCD           TO  SYS-2008-KBNCD
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-2008-STYUKYMD
                   MOVE    SPA-W01-EDYUKYMD    TO  SYS-2008-EDYUKYMD
                   MOVE    SPA-GMN-PREFKBN-PRTJYUN TO
                                           SYS-2008-PREFKBN-PRTJYUN
                   MOVE    SPA-GMN-HKNJANUM-PRTJYUN
                                               TO
                                           SYS-2008-HKNJANUM-PRTJYUN
                   MOVE    SPA-GMN-RECESYUBETU-PRTJYUN
                                               TO
                                           SYS-2008-RECESYUBETU-PRTJYUN
                   MOVE    SPA-GMN-ZAITAKU     TO  SYS-2008-ZAITAKU
                   MOVE    SPA-GMN-CHOUKI      TO  SYS-2008-CHOUKI 
                   MOVE    SPA-GMN-HKNJANUM-PRTKBN TO
                                           SYS-2008-HKNJANUM-PRTKBN
                   MOVE    SPA-GMN-HKNJANUM    TO  SYS-2008-HKNJANUM 
                   MOVE    SYS-2008-REC        TO  SYSKANRI-REC
      *     
                   PERFORM 800-MCNDATE-SEC
                   MOVE    SMCNDATE-YMD        TO  SYS-CREYMD
                                                   SYS-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                   MOVE    SYSKANRI-REC        TO  MCPDATA-REC
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO 
                       MOVE    "1003"              TO  SPA-ERRCD
                       GO  TO  490-KOUSHIN-EXT
                   END-IF
               END-IF
           END-IF          
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK 
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    直接請求する保険者の更新
      *****************************************************************
       490-HKNJANUM-UPDATE-SEC           SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF  SPA-ERRCD           NOT =   SPACE
               GO  TO  490-HKNJANUM-UPDATE-EXT
           END-IF
      *
      *    既に登録済かチェック
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-HKNJALIST-MAX
               IF    ( SPA-GMN-SELNUM      >   ZERO )
               AND   ( SPA-GMN-SELNUM      =   IDX  )
                   CONTINUE
               ELSE
                   IF      SPA-GMN-HKNJANUM1   =
                                   SPA-GMN-HKNJALIST-THKNJANUM (IDX)
                       MOVE    "0075"              TO  SPA-ERRCD
                       MOVE    69                  TO  SPA-GMN-CUR
                       GO  TO  490-HKNJANUM-UPDATE-EXT
                   END-IF
               END-IF
           END-PERFORM
      *
      *    追加可能かチェック
           IF      SPA-GMN-SELNUM      =   ZERO
               IF      SPA-GMN-HKNJALIST-MAX
                                       >=  WRK-CONS-200505-HKNJANUM-MAX
                   MOVE    "0073"              TO  SPA-ERRCD
                   MOVE    69                  TO  SPA-GMN-CUR
                   GO  TO  490-HKNJANUM-UPDATE-EXT
               END-IF
           END-IF
      *
      *    特定健康保険組合の更新チェック  
           IF      SPA-GMN-HKNJANUM1-63
                                   NOT =   SPACE     
      *****    IF (    SPA-GMN-SELNUM      =   ZERO                  )
      *****    OR (   (SPA-GMN-SELNUM      >   ZERO                 )
      *****       AND (SPA-GMN-HKNJANUM1
      *****                            NOT =
      *****                        SPA-GMN-HKNJALIST-THKNJANUM-63
      *****                                        (SPA-GMN-SELNUM) ))
                   MOVE    "1005"              TO  SPA-WIDCD2
                   GO  TO  490-HKNJANUM-UPDATE-EXT
      *****    END-IF
           END-IF
      *
      *    直接請求する保険者の更新処理
           PERFORM 4901-HKNJANUM-UPDATE-01-SEC
           .
       490-HKNJANUM-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    直接請求する保険者の更新処理
      *****************************************************************
       4901-HKNJANUM-UPDATE-01-SEC          SECTION.
      *
           IF      SPA-GMN-SELNUM      =   ZERO
               ADD     1                   TO  SPA-GMN-HKNJALIST-MAX
               MOVE    SPA-GMN-HKNJALIST-MAX
                                           TO  SPA-GMN-SELNUM
           END-IF
      *
           MOVE    SPA-GMN-SELNUM      TO
                       SPA-GMN-HKNJALIST-TNO        (SPA-GMN-SELNUM) 
           MOVE    SPA-GMN-HKNJANUM1   TO
                       SPA-GMN-HKNJALIST-THKNJANUM  (SPA-GMN-SELNUM)
           MOVE    SPA-GMN-HKNJANUM1-63
                                       TO
                       SPA-GMN-HKNJALIST-THKNJANUM-63
                                                       (SPA-GMN-SELNUM)
           MOVE    SPA-GMN-HKNJANAME1  TO
                       SPA-GMN-HKNJALIST-THKNJANAME (SPA-GMN-SELNUM)
      *
           MOVE    SPA-GMN-SELNUM  TO  W12-HKNJANUMLIST-ROW
      *
           INITIALIZE                  SPA-GMN-SELNUM
                                       SPA-GMN-HKNJANUM1
                                       SPA-GMN-HKNJANUM1-63
                                       SPA-GMN-HKNJANAME1
                                       SPA-GMN-SELNUM-O
      *
           MOVE    68                  TO  SPA-GMN-CUR
      *
           .
       4901-HKNJANUM-UPDATE-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    直接請求する保険者の削除
      *****************************************************************
       490-HKNJANUM-DELETE-SEC          SECTION.
      *
           IF      SPA-GMN-SELNUM      =   ZERO
               MOVE    69                  TO  SPA-GMN-CUR
               GO  TO  490-HKNJANUM-DELETE-EXT
           END-IF
      *
           PERFORM VARYING IDX     FROM    SPA-GMN-SELNUM  BY  1
                   UNTIL   IDX     >       SPA-GMN-HKNJALIST-MAX
               INITIALIZE                  SPA-GMN-HKNJALIST-TBL(IDX)
               IF      IDX                 <   SPA-GMN-HKNJALIST-MAX
                   COMPUTE IDY     =   IDX     +   1
                   MOVE    SPA-GMN-HKNJALIST-TBL (IDY)
                                               TO
                                       SPA-GMN-HKNJALIST-TBL (IDX)
                   MOVE    IDX                 TO
                                       SPA-GMN-HKNJALIST-TNO (IDX)
               END-IF
           END-PERFORM
           COMPUTE SPA-GMN-HKNJALIST-MAX
                                       =   SPA-GMN-HKNJALIST-MAX  -
                                           1
      *
           INITIALIZE                  SPA-GMN-SELNUM
                                       SPA-GMN-HKNJANUM1
                                       SPA-GMN-HKNJANAME1
                                       SPA-GMN-SELNUM-O
      *
           MOVE    68                  TO  SPA-GMN-CUR
      *
           .
       490-HKNJANUM-DELETE-EXT.
           EXIT.
      *
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
           IF      SPA-WIDCD2      NOT =   SPACE
               PERFORM 530-WID2SET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "W12    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           IF      FLG-HKNJANUMLIST-ROW    =   1
               MOVE    ZERO                    TO  WRK-HKNJANUMLIST-ROW
           ELSE
               MOVE    W12-HKNJANUMLIST-ROW    TO  WRK-HKNJANUMLIST-ROW
           END-IF
      *
           INITIALIZE                      W12
      *
           MOVE    SPA-GMN-HSTYUKYMD        TO  W12-YUKOSTYMD
           MOVE    SPA-GMN-HEDYUKYMD        TO  W12-YUKOEDYMD
      *
           MOVE    SPA-GMN-BYOMEIHENKBN-G   TO  W12-BYOMEIHENKBN
           MOVE    SPA-GMN-BYOMEIHENKBN1-G  TO  W12-BYOMEIHENKBN1
           MOVE    SPA-GMN-SYUBYOHENFLG-G   TO  W12-SYUBYOHENFLG
           MOVE    SPA-GMN-TENKIHENKBN-G    TO  W12-TENKIHENKBN
           MOVE    SPA-GMN-TENKIBYOMEI-KBN-G
                                            TO  W12-TENKIBYOMEI-KBN
           MOVE    SPA-GMN-KKHRECEKBN-G     TO  W12-KKHRECEKBN
           MOVE    SPA-GMN-SYARECEKBN-G     TO  W12-SYARECEKBN
           MOVE    SPA-GMN-KOUIKIRECEKBN-G  TO  W12-KOUIKIRECEKBN
           MOVE    SPA-GMN-ROURECEKBN-G     TO  W12-ROURECEKBN
           MOVE    SPA-GMN-SYAOCRKBN-G      TO  W12-SYAOCRKBN
           MOVE    SPA-GMN-KKHOCRKBN-G      TO  W12-KKHOCRKBN
           MOVE    SPA-GMN-KOUIKIOCRKBN-G   TO  W12-KOUIKIOCRKBN
           MOVE    SPA-GMN-NYUTAIIN-KSIKBN-G
                                            TO  W12-NYUTAIINREKI
           MOVE    SPA-GMN-NYUTAIIN-NYU-KSIKBN-G
                                            TO  W12-NYUTAIINREKI-NYU
           MOVE    SPA-GMN-SYAPRTKBN-G      TO  W12-SYAPRTKBN
           MOVE    SPA-GMN-DISASTER-SYAPRTKBN-G
                                            TO  W12-DISASTER-SYAPRTKBN
           MOVE    SPA-GMN-KKHPRTKBN-G      TO  W12-KKHPRTKBN
           MOVE    SPA-GMN-KOUIKIPRTKBN-G   TO  W12-KOUIKIPRTKBN
           MOVE    SPA-GMN-RSI-INVOICEKBN-G TO  W12-RSI-INVOICEKBN
           MOVE    SPA-GMN-ROUSAIPRTKBN-G   TO  W12-ROUSAIPRTKBN
           MOVE    SPA-GMN-FRAME-ROUSAIPRTKBN-G
                                            TO  W12-FRAME-ROUSAIPRTKBN
           MOVE    SPA-GMN-RENNUMHENKBN-G   TO  W12-RENNUMHENKBN
           MOVE    SPA-GMN-NEXTPRTKBN-G     TO  W12-NEXTPRTKBN
           MOVE    SPA-GMN-NEXTPRTKBN1-G    TO  W12-NEXTPRTKBN1
           MOVE    SPA-GMN-RJNGNBKKBN-G     TO  W12-RJNGNBKKBN
           MOVE    SPA-GMN-JIHITEKKSIKBN-G  TO  W12-JIHITEKKSIKBN
           MOVE    SPA-GMN-KOUIKIKBN-G      TO  W12-KOUIKIKBN
           MOVE    SPA-GMN-COMMENT-NYUINRYO-G
                                            TO  W12-COMMENT-NYUINRYO
           MOVE    SPA-GMN-COMMENT-NYUINRYO1-G
                                            TO  W12-COMMENT-NYUINRYO1
           MOVE    SPA-GMN-CHKKBN-G         TO  W12-CHKKBN
           MOVE    SPA-GMN-BYOMEI-PRTJYUN-G TO  W12-BYOMEI-PRTJYUN
           MOVE    SPA-GMN-SHOKUJI-PRTKBN-G TO  W12-SHOKUJI-PRTKBN
           MOVE    SPA-GMN-INSPECT-CHARA-KBN-G
                                            TO
                                            W12-INSPECT-CHARACTER-KBN
           MOVE    SPA-GMN-INSPECT-ZERO-KBN-G
                                            TO  W12-INSPECT-ZERO-KBN
           MOVE    SPA-GMN-INSPECT-INCLUDE-KBN-G
                                            TO  W12-INSPECT-INCLUDE-KBN
           MOVE    SPA-GMN-INSPECT-GENERAL-KBN-G
                                            TO  W12-INSPECT-GENERAL-KBN
           MOVE    SPA-GMN-SEIHO-DSPKBN-G   TO  W12-SEIHO-DSPKBN
           MOVE    SPA-GMN-RECEIPT-MAKEKBN-G
                                            TO  W12-RECEIPT-MAKE
           MOVE    SPA-GMN-ZERO-KOH-PRTKBN-G
                                            TO  W12-ZERO-KOH-PRTKBN
           MOVE    SPA-GMN-RECECOMKBN-G     TO  W12-RECECOMKBN
           MOVE    SPA-GMN-SRYKAHENKBN-G    TO  W12-SRYKAHENKBN
           MOVE    SPA-GMN-BTUNUM-KSIKBN-G  TO  W12-BTUNUM-KSIKBN
           MOVE    SPA-GMN-DRNAME-KSIKBN-G  TO  W12-DRNAME-KSIKBN
           MOVE    SPA-GMN-AUTODSPKBN-G     TO  W12-AUTODSPKBN
           MOVE    SPA-GMN-PRT-CHECKKBN-G   TO  W12-PRT-CHECKKBN
           MOVE    SPA-GMN-JIHI-PRTKBN-G    TO  W12-JIHI-PRTKBN
           MOVE    SPA-GMN-PRTJYUN1-G       TO  W12-PRTJYUNKBN1
           MOVE    SPA-GMN-PRTJYUN2-G       TO  W12-PRTJYUNKBN2
           MOVE    SPA-GMN-PRTJYUN3-G       TO  W12-PRTJYUNKBN3
           MOVE    SPA-GMN-PRTJYUN4-G       TO  W12-PRTJYUNKBN4
           MOVE    SPA-GMN-PRTJYUN5-G       TO  W12-PRTJYUNKBN5
           MOVE    SPA-GMN-PRTJYUN6-G       TO  W12-PRTJYUNKBN6
           MOVE    SPA-GMN-MOJI-KISAIKBN-G  TO  W12-MOJI-KISAIKBN
           MOVE    SPA-GMN-KYURATE-KISAIKBN-G
                                            TO  W12-KYURATE-KISAIKBN
           MOVE    SPA-GMN-RECESYUBET-KISAIKBN-G
                                            TO  W12-RECESYUBETU-KISAIKBN
           MOVE    SPA-GMN-PREFKBN-PRTJYUN-G
                                            TO  W12-PREFKBN-PRTJYUN
           MOVE    SPA-GMN-HKNJANUM-PRTJYUN-G
                                            TO  W12-HKNJANUM-PRTJYUN
           MOVE    SPA-GMN-RECESYUBETU-PRTJYUN-G
                                            TO  W12-RECESYUBETU-PRTJYUN
           MOVE    SPA-GMN-ZAITAKU-G        TO  W12-ZAITAKU
           MOVE    SPA-GMN-CHOUKI-G         TO  W12-CHOUKI
           MOVE    SPA-GMN-HKNJANUM         TO  W12-HKNJANUM
           MOVE    SPA-GMN-HKNJANAME        TO  W12-HKNJANAME
           MOVE    SPA-GMN-HKNJANUM-PRTKBN-G
                                            TO  W12-HKNJANUM-PRTKBN
           MOVE    SPA-GMN-DRCD-PRTKBN-G    TO  W12-DRCD-PRTKBN
           MOVE    SPA-GMN-DRCD-PRTJYUNKBN-G
                                            TO  W12-DRCD-PRTJYUNKBN
           MOVE    SPA-GMN-BTUNUM-PRTKBN-G  TO  W12-BTUNUM-PRTKBN
           MOVE    SPA-GMN-BTUNUM-PRTJYUNKBN-G
                                            TO  W12-BTUNUM-PRTJYUNKBN
           MOVE    SPA-GMN-TERMINAL-DIG-PRTKBN-G
                                            TO
                                            W12-TERMINAL-DIGIT-PRTKBN
           MOVE    SPA-GMN-SYUKA-KBN-G      TO  W12-SYUKA-KBN
           MOVE    SPA-GMN-SYUKA-NYUIN-G    TO  W12-SYUKA-NYUIN
           MOVE    SPA-GMN-SYUKA-GAIRAI-G   TO  W12-SYUKA-GAIRAI
           MOVE    SPA-GMN-SYUKA-MODE-G     TO  W12-SYUKA-MODE
           MOVE    SPA-GMN-SYUKA-SELECT-G   TO  W12-SYUKA-SELECT
           MOVE    SPA-GMN-SYUKA-SYAHO-G    TO  W12-SYUKA-SYAHO
           MOVE    SPA-GMN-SYUKA-KOKUHO-G   TO  W12-SYUKA-KOKUHO
           MOVE    SPA-GMN-SYUKA-KOUIKI-G   TO  W12-SYUKA-KOUIKI
           MOVE    SPA-GMN-NI-JYOKEN1       TO  W12-NI-JYOKEN1
           MOVE    SPA-GMN-NI-JYOKEN2       TO  W12-NI-JYOKEN2
           MOVE    SPA-GMN-NI-JYOKEN3       TO  W12-NI-JYOKEN3
           MOVE    SPA-GMN-NI-JYOKEN4       TO  W12-NI-JYOKEN4
           MOVE    SPA-GMN-NI-JYOKEN5       TO  W12-NI-JYOKEN5
           MOVE    SPA-GMN-NI-JYOKEN6       TO  W12-NI-JYOKEN6
           MOVE    SPA-GMN-RECEDEN-CDRKBN-G TO  W12-RECEDEN-CDRKBN
           MOVE    SPA-GMN-RECEDEN-LST1KBN-G
                                            TO  W12-RECEDEN-LST1KBN
           MOVE    SPA-GMN-RECEDEN-LST2KBN-G
                                            TO  W12-RECEDEN-LST2KBN
           MOVE    SPA-GMN-RECEDEN-LST2-MEDIA-G
                                            TO
                                            W12-RECEDEN-LST2KBN-MEDIA
           MOVE    SPA-GMN-RECEDEN-RIREKIKBN-G
                                            TO  W12-RECEDEN-RIREKIKBN
           MOVE    SPA-GMN-RECEDEN-DATAKBN-G
                                            TO  W12-RECEDEN-DATAKBN
           MOVE    SPA-GMN-RECEDEN-DISASTER-G
                                            TO  W12-RECEDEN-DISASTER
           MOVE    SPA-GMN-RECEDEN-CHK-G    TO  W12-RECEDEN-CHK
           MOVE    SPA-GMN-KIGO-DELI-FLG-G
                                            TO  W12-KIGO-DELI-FLG
           MOVE    SPA-GMN-KIGO-DELI-HKNJA-FLG-G
                                            TO
                                        W12-KIGO-DELI-HKNJA-FLG
           MOVE    SPA-GMN-NUM-DELIMITER-FLG-G
                                            TO  W12-NUM-DELI-FLG
           MOVE    SPA-GMN-NUM-DELI-HKNJA-FLG-G
                                            TO
                                        W12-NUM-DELI-HKNJA-FLG
           MOVE    SPA-GMN-SELNUM           TO  W12-SELNUM
           MOVE    SPA-GMN-HKNJANUM1        TO  W12-HKNJANUM1
           MOVE    SPA-GMN-HKNJANAME1       TO  W12-HKNJANAME1
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-BYOMEIHENKBN-MAX
               MOVE    SPA-GMN-BYOMEIHENKBNLST (IDY)
                                       TO  W12-BYOMEIHENKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-BYOMEIHENKBN-MAX
                                       TO  W12-BYOMEIHENKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-BYOMEIHENKBN1-MAX
               MOVE    SPA-GMN-BYOMEIHENKBN1LST (IDY)
                                       TO  W12-BYOMEIHENKBN1LST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-BYOMEIHENKBN1-MAX
                                       TO  W12-BYOMEIHENKBN1-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-SYUBYOHENFLG-MAX
               MOVE    SPA-GMN-SYUBYOHENFLGLST (IDY)
                                       TO  W12-SYUBYOHENFLGLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-SYUBYOHENFLG-MAX
                                       TO  W12-SYUBYOHENFLG-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-TENKIHENKBN-MAX
               MOVE    SPA-GMN-TENKIHENKBNLST (IDY)
                                       TO  W12-TENKIHENKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-TENKIHENKBN-MAX
                                       TO  W12-TENKIHENKBN-COUNT 
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-TENKIBYOMEI-KBN-MAX
               MOVE    SPA-GMN-TENKIBYOMEI-KBNLST (IDY)
                                       TO  W12-TENKIBYOMEI-KBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-TENKIBYOMEI-KBN-MAX
                                       TO  W12-TENKIBYOMEI-KBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-KKHRECEKBN-MAX
               MOVE    SPA-GMN-KKHRECEKBNLST (IDY)
                                       TO  W12-KKHRECEKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-KKHRECEKBN-MAX
                                       TO  W12-KKHRECEKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-SYARECEKBN-MAX
               MOVE    SPA-GMN-SYARECEKBNLST (IDY)
                                       TO  W12-SYARECEKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-SYARECEKBN-MAX
                                       TO  W12-SYARECEKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-KOUIKIRECEKBN-MAX
               MOVE    SPA-GMN-KOUIKIRECEKBNLST (IDY)
                                       TO  W12-KOUIKIRECEKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-KOUIKIRECEKBN-MAX
                                       TO  W12-KOUIKIRECEKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-ROURECEKBN-MAX
               MOVE    SPA-GMN-ROURECEKBNLST (IDY)
                                       TO  W12-ROURECEKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-ROURECEKBN-MAX
                                       TO  W12-ROURECEKBN-COUNT
      *
           MOVE    SPA-GMN-RECEDENOUT  TO  W12-RECEDENOUT
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-SYAOCRKBN-MAX
               MOVE    SPA-GMN-SYAOCRKBNLST (IDY)
                                       TO  W12-SYAOCRKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-SYAOCRKBN-MAX
                                       TO  W12-SYAOCRKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-KKHOCRKBN-MAX
               MOVE    SPA-GMN-KKHOCRKBNLST (IDY)
                                       TO  W12-KKHOCRKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-KKHOCRKBN-MAX
                                       TO  W12-KKHOCRKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-KOUIKIOCRKBN-MAX
               MOVE    SPA-GMN-KOUIKIOCRKBNLST (IDY)
                                       TO  W12-KOUIKIOCRKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-KOUIKIOCRKBN-MAX
                                       TO  W12-KOUIKIOCRKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-NYUTAIIN-KSIKBN-MAX
               MOVE    SPA-GMN-NYUTAIIN-KSIKBNLST (IDY)
                                       TO  W12-NYUTAIINREKILST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-NYUTAIIN-KSIKBN-MAX
                                       TO  W12-NYUTAIINREKI-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >
                                   SPA-GMN-NYUTAIIN-NYU-KSIKBN-MAX
               MOVE    SPA-GMN-NYUTAIIN-NYU-KSIKBNLST (IDY)
                                       TO  W12-NYUTAIINREKI-NYULST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-NYUTAIIN-NYU-KSIKBN-MAX
                                       TO  W12-NYUTAIINREKI-NYU-COUNT 
      *
           MOVE    SPA-GMN-PREFPRGID   TO  W12-PREFPRGID
           MOVE    SPA-GMN-PREFPRGID1  TO  W12-PREFPRGID1
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-SYAPRTKBN-MAX
               MOVE    SPA-GMN-SYAPRTKBNLST (IDY)
                                       TO  W12-SYAPRTKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-SYAPRTKBN-MAX
                                       TO  W12-SYAPRTKBN-COUNT 
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-DISASTER-SYAPRTKBN-MAX
               MOVE    SPA-GMN-DISASTER-SYAPRTKBNLST (IDY)
                                       TO  W12-DISASTER-SYAPRTKBNLST
                                                               (IDY)
           END-PERFORM
           MOVE    SPA-GMN-DISASTER-SYAPRTKBN-MAX
                                       TO  W12-DISASTER-SYAPRTKBN-COUNT
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-KKHPRTKBN-MAX
               MOVE    SPA-GMN-KKHPRTKBNLST (IDY)
                                       TO  W12-KKHPRTKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-KKHPRTKBN-MAX
                                       TO  W12-KKHPRTKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-KOUIKIPRTKBN-MAX
               MOVE    SPA-GMN-KOUIKIPRTKBNLST (IDY)
                                       TO  W12-KOUIKIPRTKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-KOUIKIPRTKBN-MAX
                                       TO  W12-KOUIKIPRTKBN-COUNT 
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-RSI-INVOICEKBN-MAX
               MOVE    SPA-GMN-RSI-INVOICEKBNLST (IDY)
                                       TO  W12-RSI-INVOICEKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-RSI-INVOICEKBN-MAX
                                       TO  W12-RSI-INVOICEKBN-COUNT
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-ROUSAIPRTKBN-MAX
               MOVE    SPA-GMN-ROUSAIPRTKBNLST (IDY)
                                       TO  W12-ROUSAIPRTKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-ROUSAIPRTKBN-MAX
                                       TO  W12-ROUSAIPRTKBN-COUNT 
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >      SPA-GMN-FRAME-ROUSAIPRTKBN-MAX
               MOVE    SPA-GMN-FRAME-ROUSAIPRTKBNLST (IDY)
                                   TO  W12-FRAME-ROUSAIPRTKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-FRAME-ROUSAIPRTKBN-MAX
                                   TO  W12-FRAME-ROUSAIPRTKBN-COUNT 
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-RENNUMHENKBN-MAX
               MOVE    SPA-GMN-RENNUMHENKBNLST (IDY)
                                       TO  W12-RENNUMHENKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-RENNUMHENKBN-MAX
                                       TO  W12-RENNUMHENKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-NEXTPRTKBN-MAX
               MOVE    SPA-GMN-NEXTPRTKBNLST (IDY)
                                       TO  W12-NEXTPRTKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-NEXTPRTKBN-MAX
                                       TO  W12-NEXTPRTKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-NEXTPRTKBN1-MAX
               MOVE    SPA-GMN-NEXTPRTKBN1LST (IDY)
                                       TO  W12-NEXTPRTKBN1LST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-NEXTPRTKBN1-MAX
                                       TO  W12-NEXTPRTKBN1-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-RJNGNBKKBN-MAX
               MOVE    SPA-GMN-RJNGNBKKBNLST (IDY)
                                       TO  W12-RJNGNBKKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-RJNGNBKKBN-MAX
                                       TO  W12-RJNGNBKKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-JIHITEKKSIKBN-MAX
               MOVE    SPA-GMN-JIHITEKKSIKBNLST (IDY)
                                       TO  W12-JIHITEKKSIKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-JIHITEKKSIKBN-MAX
                                       TO  W12-JIHITEKKSIKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-KOUIKIKBN-MAX
               MOVE    SPA-GMN-KOUIKIKBNLST (IDY)
                                       TO  W12-KOUIKIKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-KOUIKIKBN-MAX
                                       TO  W12-KOUIKIKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-COMMENT-NYUINRYO-MAX
               MOVE    SPA-GMN-COMMENT-NYUINRYOLST (IDY)
                                       TO  W12-COMMENT-NYUINRYOLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-COMMENT-NYUINRYO-MAX
                                       TO  W12-COMMENT-NYUINRYO-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-COMMENT-NYUINRYO1-MAX
               MOVE    SPA-GMN-COMMENT-NYUINRYO1LST (IDY)
                                       TO  W12-COMMENT-NYUINRYO1LST(IDY)
           END-PERFORM
           MOVE    SPA-GMN-COMMENT-NYUINRYO1-MAX
                                       TO  W12-COMMENT-NYUINRYO1-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-CHKKBN-MAX
               MOVE    SPA-GMN-CHKKBNLST (IDY)
                                       TO  W12-CHKKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-CHKKBN-MAX
                                       TO  W12-CHKKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-BYOMEI-PRTJYUN-MAX
               MOVE    SPA-GMN-BYOMEI-PRTJYUNLST (IDY)
                                       TO  W12-BYOMEI-PRTJYUNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-BYOMEI-PRTJYUN-MAX
                                       TO  W12-BYOMEI-PRTJYUN-COUNT 
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-SHOKUJI-PRTKBN-MAX
               MOVE    SPA-GMN-SHOKUJI-PRTKBNLST (IDY)
                                       TO  W12-SHOKUJI-PRTKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-SHOKUJI-PRTKBN-MAX
                                       TO  W12-SHOKUJI-PRTKBN-COUNT
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-INSPECT-CHARA-KBN-MAX
               MOVE    SPA-GMN-INSPECT-CHARA-KBNLST (IDY)
                                       TO
                                   W12-INSPECT-CHARACTER-KBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-INSPECT-CHARA-KBN-MAX
                                       TO
                                       W12-INSPECT-CHARACTER-KBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-INSPECT-ZERO-KBN-MAX
               MOVE    SPA-GMN-INSPECT-ZERO-KBNLST (IDY)
                                       TO  W12-INSPECT-ZERO-KBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-INSPECT-ZERO-KBN-MAX
                                       TO  W12-INSPECT-ZERO-KBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-INSPECT-INCLUDE-KBN-MAX
               MOVE    SPA-GMN-INSPECT-INCLUDE-KBNLST (IDY)
                                       TO  W12-INSPECT-INCLUDE-KBNLST
                                                                   (IDY)
           END-PERFORM
           MOVE    SPA-GMN-INSPECT-INCLUDE-KBN-MAX
                                       TO  W12-INSPECT-INCLUDE-KBN-COUNT 
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-INSPECT-GENERAL-KBN-MAX
               MOVE    SPA-GMN-INSPECT-GENERAL-KBNLST (IDY)
                                       TO  W12-INSPECT-GENERAL-KBNLST
                                                                   (IDY)
           END-PERFORM
           MOVE    SPA-GMN-INSPECT-GENERAL-KBN-MAX
                                       TO  W12-INSPECT-GENERAL-KBN-COUNT
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-SEIHO-DSPKBN-MAX
               MOVE    SPA-GMN-SEIHO-DSPKBNLST (IDY)
                                       TO  W12-SEIHO-DSPKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-SEIHO-DSPKBN-MAX
                                       TO  W12-SEIHO-DSPKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-RECEIPT-MAKEKBN-MAX
               MOVE    SPA-GMN-RECEIPT-MAKEKBNLST (IDY)
                                       TO  W12-RECEIPT-MAKELST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-RECEIPT-MAKEKBN-MAX
                                       TO  W12-RECEIPT-MAKE-COUNT 
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-ZERO-KOH-PRTKBN-MAX
               MOVE    SPA-GMN-ZERO-KOH-PRTKBNLST (IDY)
                                       TO  W12-ZERO-KOH-PRTKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-ZERO-KOH-PRTKBN-MAX
                                       TO  W12-ZERO-KOH-PRTKBN-COUNT 
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-HKN-RIREKI-MAX
               MOVE    SPA-GMN-HKN-RIREKILST (IDY)
                                       TO  W12-HKN-RIREKILST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-HKN-RIREKI-MAX
                                       TO  W12-HKN-RIREKI-COUNT 
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-GENBUTU-ICHIBU-MAX
               MOVE    SPA-GMN-GENBUTU-ICHIBULST (IDY)
                                       TO  W12-GENBUTU-ICHIBULST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-GENBUTU-ICHIBU-MAX
                                       TO  W12-GENBUTU-ICHIBU-COUNT
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-COMPRESS-MAX
               MOVE    SPA-GMN-COMPRESSLST (IDY)
                                       TO  W12-COMPRESSLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-COMPRESS-MAX
                                       TO  W12-COMPRESS-COUNT
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DISASTER-RECEIPT-MAX
               MOVE    SPA-GMN-DISASTER-RECEIPTLST (IDY)
                                       TO  W12-DISASTER-RECEIPTLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-DISASTER-RECEIPT-MAX
                                       TO  W12-DISASTER-RECEIPT-COUNT
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-15-YKZ-MAX
               MOVE    SPA-GMN-15-YKZLST (IDY)
                                       TO  W12-15-YKZLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-15-YKZ-MAX  TO  W12-15-YKZ-COUNT
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-KOUHI-ICHIBU-KSI-MAX
               MOVE    SPA-GMN-KOUHI-ICHIBU-KSILST (IDY)
                                       TO  W12-KOUHI-ICHIBU-KSILST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-KOUHI-ICHIBU-KSI-MAX
                                       TO  W12-KOUHI-ICHIBU-KSI-COUNT
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-RECECOMKBN-MAX
               MOVE    SPA-GMN-RECECOMKBNLST (IDY)
                                       TO  W12-RECECOMKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-RECECOMKBN-MAX
                                       TO  W12-RECECOMKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-SRYKAHENKBN-MAX
               MOVE    SPA-GMN-SRYKAHENKBNLST (IDY)
                                       TO  W12-SRYKAHENKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-SRYKAHENKBN-MAX
                                       TO  W12-SRYKAHENKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-BTUNUM-KSIKBN-MAX
               MOVE    SPA-GMN-BTUNUM-KSIKBNLST (IDY)
                                       TO  W12-BTUNUM-KSIKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-BTUNUM-KSIKBN-MAX
                                       TO  W12-BTUNUM-KSIKBN-COUNT 
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DRNAME-KSIKBN-MAX
               MOVE    SPA-GMN-DRNAME-KSIKBNLST (IDY)
                                       TO  W12-DRNAME-KSIKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-DRNAME-KSIKBN-MAX
                                       TO  W12-DRNAME-KSIKBN-COUNT 
      *
           MOVE    SPA-GMN-SYAPRTNM    TO  W12-SYAPRTNM
      *
           MOVE    SPA-GMN-SYAPRTNM-NEXT
                                       TO  W12-SYAPRTNM-NEXT
      *
           MOVE    SPA-GMN-KKHPRTNM    TO  W12-KKHPRTNM 
      *
           MOVE    SPA-GMN-KKHPRTNM-NEXT
                                       TO  W12-KKHPRTNM-NEXT 
      *
           MOVE    SPA-GMN-KOUIKIPRTNM    TO  W12-KOUIKIPRTNM 
      *
           MOVE    SPA-GMN-KOUIKIPRTNM-NEXT
                                       TO  W12-KOUIKIPRTNM-NEXT 
      *
           MOVE    SPA-GMN-SYAPRTNM-N  TO  W12-SYAPRTNM-N
      *
           MOVE    SPA-GMN-SYAPRTNM-NEXT-N
                                       TO  W12-SYAPRTNM-NEXT-N
      *
           MOVE    SPA-GMN-KKHPRTNM-N  TO  W12-KKHPRTNM-N
      *
           MOVE    SPA-GMN-KKHPRTNM-NEXT-N
                                       TO  W12-KKHPRTNM-NEXT-N
      *
           MOVE    SPA-GMN-KOUIKIPRTNM-N  TO  W12-KOUIKIPRTNM-N
      *
           MOVE    SPA-GMN-KOUIKIPRTNM-NEXT-N
                                       TO  W12-KOUIKIPRTNM-NEXT-N
      *
           MOVE    SPA-GMN-ROUPRTNM    TO  W12-ROUPRTNM
      *
           MOVE    SPA-GMN-ROUPRTNM-NEXT
                                       TO  W12-ROUPRTNM-NEXT
      *
           MOVE    SPA-GMN-JBAIPRTNM   TO  W12-JBAIPRTNM 
      *
           MOVE    SPA-GMN-JBAIPRTNM-NEXT
                                       TO  W12-JBAIPRTNM-NEXT 
      *
           MOVE    SPA-GMN-ROUPRTNM-N  TO  W12-ROUPRTNM-N
      *
           MOVE    SPA-GMN-ROUPRTNM-NEXT-N
                                       TO  W12-ROUPRTNM-NEXT-N
      *
           MOVE    SPA-GMN-JBAIPRTNM-N TO  W12-JBAIPRTNM-N
      *
           MOVE    SPA-GMN-JBAIPRTNM-NEXT-N
                                       TO  W12-JBAIPRTNM-NEXT-N
      *
           MOVE    SPA-GMN-FRAME-ROUPRTKBN-G
                                       TO  W12-FRAME-ROU
      *
           MOVE    SPA-GMN-FRAME-ROUPRTKBN-N-G
                                       TO  W12-FRAME-ROU-N
      *
           MOVE    SPA-GMN-COMMENT-ROU-G
                                       TO  W12-COMMENT-ROU
      *
           MOVE    SPA-GMN-RSI-RECEIPTKBN-G
                                       TO  W12-RSI-RECEIPTKBN
      *
           MOVE    SPA-GMN-ROUPRTKBN-G
                                       TO  W12-ROUPRTKBN
      *
           MOVE    SPA-GMN-ROUPRTKBN-N-G
                                       TO  W12-ROUPRTKBN-N
      *
           MOVE    SPA-GMN-FRAME-JIBAIPRTKBN-G
                                       TO  W12-FRAME-JIBAI
      *
           MOVE    SPA-GMN-FRAME-JIBAIPRTKBN-N-G
                                       TO  W12-FRAME-JIBAI-N
      *
           MOVE    SPA-GMN-JIBAIPRTKBN-G
                                       TO  W12-JIBAIPRTKBN
      *
           MOVE    SPA-GMN-JIBAIPRTKBN-N-G
                                       TO  W12-JIBAIPRTKBN-N
      *
           MOVE    SPA-GMN-JIBAIPRTKBN1-G
                                       TO  W12-JIBAIPRTKBN1
      *
           MOVE    SPA-GMN-JIBAIPRTKBN1-N-G
                                       TO  W12-JIBAIPRTKBN1-N
      *
           MOVE    SPA-GMN-JIBAI-FRACTION-KBN1-G
                                       TO  W12-JIBAI-FRACTION-KBN1
      *
           MOVE    SPA-GMN-JIBAI-FRACTION-KBN2-G
                                       TO  W12-JIBAI-FRACTION-KBN2
      *
           MOVE    SPA-GMN-JIBAI-PDF-G TO  W12-JIBAI-PDF
      *
           MOVE    SPA-GMN-HKN-RIREKI-G
                                       TO  W12-HKN-RIREKI
      *
           MOVE    SPA-GMN-GENBUTU-ICHIBU-G
                                       TO  W12-GENBUTU-ICHIBU
      *
           MOVE    SPA-GMN-COMPRESS-G  TO  W12-COMPRESS
      *
           MOVE    SPA-GMN-DISASTER-RECEIPT-G
                                       TO  W12-DISASTER-RECEIPT
      *
           MOVE    SPA-GMN-15-YKZ-G    TO  W12-15-YKZ
      *
           MOVE    SPA-GMN-KOUHI-ICHIBU-KSI-G
                                       TO  W12-KOUHI-ICHIBU-KSI
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-AUTODSPKBN-MAX
               MOVE    SPA-GMN-AUTODSPKBNLST (IDY)
                                       TO  W12-AUTODSPKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-AUTODSPKBN-MAX
                                       TO  W12-AUTODSPKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-PRT-CHECKKBN-MAX
               MOVE    SPA-GMN-PRT-CHECKKBNLST (IDY)
                                       TO  W12-PRT-CHECKKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-PRT-CHECKKBN-MAX
                                       TO  W12-PRT-CHECKKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-JIHI-PRTKBN-MAX
               MOVE    SPA-GMN-JIHI-PRTKBNLST (IDY)
                                       TO  W12-JIHI-PRTKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-JIHI-PRTKBN-MAX
                                       TO  W12-JIHI-PRTKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-PRTJYUN1-MAX
               MOVE    SPA-GMN-PRTJYUN1LST (IDY)
                                       TO  W12-PRTJYUNKBN1LST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-PRTJYUN1-MAX
                                       TO  W12-PRTJYUNKBN1-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-PRTJYUN3-MAX
               MOVE    SPA-GMN-PRTJYUN3LST (IDY)
                                       TO  W12-PRTJYUNKBN3LST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-PRTJYUN3-MAX
                                       TO  W12-PRTJYUNKBN3-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-PRTJYUN2-MAX
               MOVE    SPA-GMN-PRTJYUN2LST (IDY)
                                       TO  W12-PRTJYUNKBN2LST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-PRTJYUN2-MAX
                                       TO  W12-PRTJYUNKBN2-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-PRTJYUN4-MAX
               MOVE    SPA-GMN-PRTJYUN4LST (IDY)
                                       TO  W12-PRTJYUNKBN4LST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-PRTJYUN4-MAX
                                       TO  W12-PRTJYUNKBN4-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-PRTJYUN5-MAX
               MOVE    SPA-GMN-PRTJYUN5LST (IDY)
                                       TO  W12-PRTJYUNKBN5LST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-PRTJYUN5-MAX
                                       TO  W12-PRTJYUNKBN5-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-PRTJYUN6-MAX
               MOVE    SPA-GMN-PRTJYUN6LST (IDY)
                                       TO  W12-PRTJYUNKBN6LST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-PRTJYUN6-MAX
                                       TO  W12-PRTJYUNKBN6-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-MOJI-KISAIKBN-MAX
               MOVE    SPA-GMN-MOJI-KISAIKBNLST (IDY)
                                       TO  W12-MOJI-KISAIKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-MOJI-KISAIKBN-MAX
                                       TO  W12-MOJI-KISAIKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-KYURATE-KISAIKBN-MAX
               MOVE    SPA-GMN-KYURATE-KISAIKBNLST (IDY)
                                       TO  W12-KYURATE-KISAIKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-KYURATE-KISAIKBN-MAX
                                       TO  W12-KYURATE-KISAIKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-RECESYUBET-KISAIKBN-MAX
               MOVE    SPA-GMN-RECESYUBET-KISAIKBNLST (IDY)
                                       TO
                                       W12-RECESYUBETU-KISAIKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-RECESYUBET-KISAIKBN-MAX
                                       TO
                                       W12-RECESYUBETU-KISAIKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-PREFKBN-PRTJYUN-MAX
               MOVE    SPA-GMN-PREFKBN-PRTJYUNLST (IDY)
                                       TO  W12-PREFKBN-PRTJYUNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-PREFKBN-PRTJYUN-MAX
                                       TO  W12-PREFKBN-PRTJYUN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-HKNJANUM-PRTJYUN-MAX
               MOVE    SPA-GMN-HKNJANUM-PRTJYUNLST (IDY)
                                       TO  W12-HKNJANUM-PRTJYUNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-HKNJANUM-PRTJYUN-MAX
                                       TO  W12-HKNJANUM-PRTJYUN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-RECESYUBETU-PRTJYUN-MAX
               MOVE    SPA-GMN-RECESYUBETU-PRTJYUNLST (IDY)
                                       TO
                                       W12-RECESYUBETU-PRTJYUNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-RECESYUBETU-PRTJYUN-MAX
                                       TO
                                       W12-RECESYUBETU-PRTJYUN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-ZAITAKU-MAX
               MOVE    SPA-GMN-ZAITAKULST (IDY)
                                       TO  W12-ZAITAKULST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-ZAITAKU-MAX TO  W12-ZAITAKU-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-CHOUKI-MAX
               MOVE    SPA-GMN-CHOUKILST (IDY)
                                       TO  W12-CHOUKILST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-CHOUKI-MAX  TO  W12-CHOUKI-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-HKNJANUM-PRTKBN-MAX
               MOVE    SPA-GMN-HKNJANUM-PRTKBNLST (IDY)
                                       TO
                                       W12-HKNJANUM-PRTKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-HKNJANUM-PRTKBN-MAX
                                       TO
                                       W12-HKNJANUM-PRTKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DRCD-PRTKBN-MAX
               MOVE    SPA-GMN-DRCD-PRTKBNLST (IDY)
                                       TO  W12-DRCD-PRTKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-DRCD-PRTKBN-MAX
                                       TO  W12-DRCD-PRTKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DRCD-PRTJYUNKBN-MAX
               MOVE    SPA-GMN-DRCD-PRTJYUNKBNLST (IDY)
                                       TO  W12-DRCD-PRTJYUNKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-DRCD-PRTJYUNKBN-MAX
                                       TO  W12-DRCD-PRTJYUNKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-BTUNUM-PRTKBN-MAX
               MOVE    SPA-GMN-BTUNUM-PRTKBNLST (IDY)
                                       TO  W12-BTUNUM-PRTKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-BTUNUM-PRTKBN-MAX
                                       TO  W12-BTUNUM-PRTKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-BTUNUM-PRTJYUNKBN-MAX
               MOVE    SPA-GMN-BTUNUM-PRTJYUNKBNLST (IDY)
                                       TO  W12-BTUNUM-PRTJYUNKBNLST(IDY)
           END-PERFORM
           MOVE    SPA-GMN-BTUNUM-PRTJYUNKBN-MAX
                                       TO  W12-BTUNUM-PRTJYUNKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-TERMINAL-DIG-PRTKBN-MAX
               MOVE    SPA-GMN-TERMINAL-DIG-PRTKBNLST (IDY)
                                       TO
                                       W12-TERMINAL-DIGIT-PRTKBNLST(IDY)
           END-PERFORM
           MOVE    SPA-GMN-TERMINAL-DIG-PRTKBN-MAX
                                       TO
                                       W12-TERMINAL-DIGIT-PRTKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-SYUKA-KBN-MAX
               MOVE    SPA-GMN-SYUKA-KBNLST (IDY)
                                           TO  W12-SYUKA-KBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-SYUKA-KBN-MAX   TO  W12-SYUKA-KBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-SYUKA-NYUIN-MAX
               MOVE    SPA-GMN-SYUKA-NYUINLST (IDY)
                                           TO  W12-SYUKA-NYUINLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-SYUKA-NYUIN-MAX TO  W12-SYUKA-NYUIN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-SYUKA-GAIRAI-MAX
               MOVE    SPA-GMN-SYUKA-GAIRAILST (IDY)
                                           TO  W12-SYUKA-GAIRAILST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-SYUKA-GAIRAI-MAX
                                           TO  W12-SYUKA-GAIRAI-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-SYUKA-MODE-MAX
               MOVE    SPA-GMN-SYUKA-MODELST (IDY)
                                           TO  W12-SYUKA-MODELST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-SYUKA-MODE-MAX  TO  W12-SYUKA-MODE-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-SYUKA-SELECT-MAX
               MOVE    SPA-GMN-SYUKA-SELECTLST (IDY)
                                           TO  W12-SYUKA-SELECTLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-SYUKA-SELECT-MAX
                                           TO  W12-SYUKA-SELECT-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-SYUKA-SYAHO-MAX
               MOVE    SPA-GMN-SYUKA-SYAHOLST (IDY)
                                           TO  W12-SYUKA-SYAHOLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-SYUKA-SYAHO-MAX TO  W12-SYUKA-SYAHO-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-SYUKA-KOKUHO-MAX
               MOVE    SPA-GMN-SYUKA-KOKUHOLST (IDY)
                                           TO  W12-SYUKA-KOKUHOLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-SYUKA-KOKUHO-MAX
                                           TO  W12-SYUKA-KOKUHO-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-SYUKA-KOUIKI-MAX
               MOVE    SPA-GMN-SYUKA-KOUIKILST (IDY)
                                           TO  W12-SYUKA-KOUIKILST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-SYUKA-KOUIKI-MAX
                                           TO  W12-SYUKA-KOUIKI-COUNT 
     *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-FRAME-ROUPRTKBN-MAX
               MOVE    SPA-GMN-FRAME-ROUPRTKBNLST (IDY)
                                           TO
                                           W12-FRAME-ROULST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-FRAME-ROUPRTKBN-MAX
                                           TO  W12-FRAME-ROU-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-FRAME-ROUPRTKBN-N-MAX
               MOVE    SPA-GMN-FRAME-ROUPRTKBN-NLST (IDY)
                                           TO  W12-FRAME-ROU-NLST(IDY)
           END-PERFORM
           MOVE    SPA-GMN-FRAME-ROUPRTKBN-N-MAX
                                           TO  W12-FRAME-ROU-N-COUNT 
     *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-COMMENT-ROU-MAX
               MOVE    SPA-GMN-COMMENT-ROULST (IDY)
                                           TO
                                           W12-COMMENT-ROULST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-COMMENT-ROU-MAX
                                           TO  W12-COMMENT-ROU-COUNT 
     *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-RSI-RECEIPTKBN-MAX
               MOVE    SPA-GMN-RSI-RECEIPTKBNLST (IDY)
                                       TO  W12-RSI-RECEIPTKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-RSI-RECEIPTKBN-MAX
                                   TO  W12-RSI-RECEIPTKBN-COUNT
     *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-ROUPRTKBN-MAX
               MOVE    SPA-GMN-ROUPRTKBNLST (IDY)
                                           TO  W12-ROUPRTKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-ROUPRTKBN-MAX TO  W12-ROUPRTKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-ROUPRTKBN-N-MAX
               MOVE    SPA-GMN-ROUPRTKBN-NLST (IDY)
                                           TO  W12-ROUPRTKBN-NLST(IDY)
           END-PERFORM
           MOVE    SPA-GMN-ROUPRTKBN-N-MAX
                                           TO  W12-ROUPRTKBN-N-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-FRAME-JIBAIPRTKBN-MAX
               MOVE    SPA-GMN-FRAME-JIBAIPRTKBNLST (IDY)
                                           TO
                                           W12-FRAME-JIBAILST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-FRAME-JIBAIPRTKBN-MAX
                                           TO  W12-FRAME-JIBAI-COUNT 
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-FRAME-JIBAIPRTKBN-N-MAX
               MOVE    SPA-GMN-FRAME-JIBAIPRTKBN-NLST (IDY)
                                           TO  W12-FRAME-JIBAI-NLST(IDY)
           END-PERFORM
           MOVE    SPA-GMN-FRAME-JIBAIPRTKBN-N-MAX
                                           TO  W12-FRAME-JIBAI-N-COUNT 
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-JIBAIPRTKBN-MAX
               MOVE    SPA-GMN-JIBAIPRTKBNLST (IDY)
                                           TO  W12-JIBAIPRTKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-JIBAIPRTKBN-MAX TO  W12-JIBAIPRTKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-JIBAIPRTKBN-N-MAX
               MOVE    SPA-GMN-JIBAIPRTKBN-NLST (IDY)
                                           TO  W12-JIBAIPRTKBN-NLST(IDY)
           END-PERFORM
           MOVE    SPA-GMN-JIBAIPRTKBN-N-MAX
                                           TO  W12-JIBAIPRTKBN-N-COUNT 
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-JIBAIPRTKBN1-MAX
               MOVE    SPA-GMN-JIBAIPRTKBN1LST (IDY)
                                           TO  W12-JIBAIPRTKBN1LST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-JIBAIPRTKBN1-MAX
                                           TO  W12-JIBAIPRTKBN1-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-JIBAIPRTKBN1-N-MAX
               MOVE    SPA-GMN-JIBAIPRTKBN1-NLST (IDY)
                                           TO  W12-JIBAIPRTKBN1-NLST
                                                                   (IDY)
           END-PERFORM
           MOVE    SPA-GMN-JIBAIPRTKBN1-N-MAX
                                           TO  W12-JIBAIPRTKBN1-N-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-JIBAI-FRACTION-KBN1-MAX
               MOVE    SPA-GMN-JIBAI-FRACTION-KBN1LST (IDY)
                                       TO  W12-JIBAI-FRACTION-KBN1LST
                                                                   (IDY)
           END-PERFORM
           MOVE    SPA-GMN-JIBAI-FRACTION-KBN1-MAX
                                       TO  W12-JIBAI-FRACTION-KBN1-COUNT
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-JIBAI-FRACTION-KBN2-MAX
               MOVE    SPA-GMN-JIBAI-FRACTION-KBN2LST (IDY)
                                       TO  W12-JIBAI-FRACTION-KBN2LST
                                                                   (IDY)
           END-PERFORM
           MOVE    SPA-GMN-JIBAI-FRACTION-KBN2-MAX
                                       TO  W12-JIBAI-FRACTION-KBN2-COUNT
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-JIBAI-PDF-MAX
               MOVE    SPA-GMN-JIBAI-PDFLST (IDY)
                                       TO  W12-JIBAI-PDFLST    (IDY)
           END-PERFORM
           MOVE    SPA-GMN-JIBAI-PDF-MAX
                                       TO  W12-JIBAI-PDF-COUNT
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-RECEDEN-CDRKBN-MAX
               MOVE    SPA-GMN-RECEDEN-CDRKBNLST (IDY)
                                       TO  W12-RECEDEN-CDRKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-RECEDEN-CDRKBN-MAX
                                       TO  W12-RECEDEN-CDRKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-RECEDEN-LST1KBN-MAX
               MOVE    SPA-GMN-RECEDEN-LST1KBNLST (IDY)
                                       TO  W12-RECEDEN-LST1KBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-RECEDEN-LST1KBN-MAX
                                       TO  W12-RECEDEN-LST1KBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-RECEDEN-LST2KBN-MAX
               MOVE    SPA-GMN-RECEDEN-LST2KBNLST (IDY)
                                       TO  W12-RECEDEN-LST2KBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-RECEDEN-LST2KBN-MAX
                                       TO  W12-RECEDEN-LST2KBN-COUNT 
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-RECEDEN-LST2-MEDIA-MAX
               MOVE    SPA-GMN-RECEDEN-LST2-MEDIALST (IDY)
                                   TO  W12-RECEDEN-LST2KBN-MEDIALST(IDY)
           END-PERFORM
           MOVE    SPA-GMN-RECEDEN-LST2-MEDIA-MAX
                                   TO  W12-RECEDEN-LST2KBN-MEDIA-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-RECEDEN-RIREKIKBN-MAX
               MOVE    SPA-GMN-RECEDEN-RIREKIKBNLST (IDY)
                                       TO  W12-RECEDEN-RIREKIKBNLST(IDY)
           END-PERFORM
           MOVE    SPA-GMN-RECEDEN-RIREKIKBN-MAX
                                       TO  W12-RECEDEN-RIREKIKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-RECEDEN-DATAKBN-MAX
               MOVE    SPA-GMN-RECEDEN-DATAKBNLST (IDY)
                                       TO  W12-RECEDEN-DATAKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-RECEDEN-DATAKBN-MAX
                                       TO  W12-RECEDEN-DATAKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-RECEDEN-DISASTER-MAX
               MOVE    SPA-GMN-RECEDEN-DISASTERLST (IDY)
                                       TO  W12-RECEDEN-DISASTERLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-RECEDEN-DISASTER-MAX
                                       TO  W12-RECEDEN-DISASTER-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-RECEDEN-CHK-MAX
               MOVE    SPA-GMN-RECEDEN-CHKLST (IDY)
                                       TO  W12-RECEDEN-CHKLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-RECEDEN-CHK-MAX
                                       TO  W12-RECEDEN-CHK-COUNT 
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-KIGO-DELI-FLG-MAX
               MOVE    SPA-GMN-KIGO-DELI-FLGLST (IDY)
                                       TO  W12-KIGO-DELI-FLGLST
                                                                   (IDY)
           END-PERFORM
           MOVE    SPA-GMN-KIGO-DELI-FLG-MAX
                                       TO  W12-KIGO-DELI-FLG-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >
                               SPA-GMN-KIGO-DELI-HKNJA-FLG-MAX
               MOVE    SPA-GMN-KIGO-DELI-HKNJA-FLGLST (IDY)
                                       TO
                                   W12-KIGO-DELI-HKNJA-FLGLST
                                                                   (IDY)
           END-PERFORM
           MOVE    SPA-GMN-KIGO-DELI-HKNJA-FLG-MAX
                                       TO
                                   W12-KIGO-DELI-HKNJA-FLG-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-NUM-DELIMITER-FLG-MAX
               MOVE    SPA-GMN-NUM-DELIMITER-FLGLST (IDY)
                                       TO  W12-NUM-DELI-FLGLST
                                                                   (IDY)
           END-PERFORM
           MOVE    SPA-GMN-NUM-DELIMITER-FLG-MAX
                                       TO  W12-NUM-DELI-FLG-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >
                               SPA-GMN-NUM-DELI-HKNJA-FLG-MAX
               MOVE    SPA-GMN-NUM-DELI-HKNJA-FLGLST (IDY)
                                       TO
                                   W12-NUM-DELI-HKNJA-FLGLST
                                                                   (IDY)
           END-PERFORM
           MOVE    SPA-GMN-NUM-DELI-HKNJA-FLG-MAX
                                       TO
                                   W12-NUM-DELI-HKNJA-FLG-COUNT 
      *
           IF      SPA-BEDSU       >   ZERO
               MOVE    WIDGET-NORMAL       TO  W12-PREFPRGID1-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  W12-PREFPRGID1-STATE
           END-IF
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-HKNJALIST-MAX
               MOVE    SPA-GMN-HKNJALIST-TNO  (IDX) 
                                       TO  WRK-Z9
               MOVE    WRK-Z9          TO  W12-HKNJANUMLIST-TNO  (IDX)
               MOVE    SPA-GMN-HKNJALIST-THKNJANUM (IDX)
                                       TO
                                       W12-HKNJANUMLIST-THKNJANUM (IDX)
               IF      SPA-GMN-HKNJALIST-THKNJANUM-63 (IDX)
                                   NOT =   SPACE
                   MOVE    "◎"        TO
                                       W12-HKNJANUMLIST-THKNJANUM-63
                                                                  (IDX)
               END-IF
               MOVE    SPA-GMN-HKNJALIST-THKNJANAME(IDX)
                                       TO
                                       W12-HKNJANUMLIST-THKNJANAME(IDX)
      *
               IF      IDX             =   SPA-GMN-SELNUM
                   MOVE    "T"         TO  W12-HKNJANUMLIST-SELECT (IDX)
               ELSE
                   MOVE    "F"         TO  W12-HKNJANUMLIST-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-HKNJALIST-MAX
                                       TO  W12-HKNJANUMLIST-COUNT
           MOVE    ZERO                TO  W12-HKNJANUMLIST-ROW
           IF      WRK-HKNJANUMLIST-ROW    >   ZERO
               MOVE    WRK-HKNJANUMLIST-ROW
                                           TO  W12-HKNJANUMLIST-ROW
           END-IF
      *
           MOVE    WIDGET-NORMAL   TO  W12-RECEDENOUT-STATE
                                       W12-RECEDEN-RIREKI-STATE
           IF      SPA-MW          =   SPA-GINBEE
               MOVE    WIDGET-INSENSITIVE
                                       TO  W12-RECEDENOUT-STATE
                                           W12-RECEDEN-RIREKI-STATE
           END-IF
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
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "傷病名記載区分入力エラー"    TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "国保レセ電出力区分入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "社保レセ電出力区分入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "国保ＯＣＲ記載区分入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "国保診療報酬請求書区分入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE
                       "県単用プログラム名の一桁目はＳで入力して下さい"
                                                         TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "転帰日記載区分入力エラー"    TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "通し番号記載区分入力エラー"  TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "続紙区分入力エラー"          TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "主病名編集フラグ入力エラー"  TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "老人と原爆区分入力エラー"    TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "自費レセ摘要欄記載区分入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE
                       "レセプトコメント（症状詳記）記録区分入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "診療科記載区分入力エラー"    TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "自動遷移区分入力エラー"      TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "一括作成出力順（社保）入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "個別作成出力順入力エラー"    TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "「特別療養費」文字記載位置入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "給付割合欄記載区分入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE    "レセプト種別欄記載区分入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "印刷順指定・県内県外入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0022"
                   MOVE    "印刷順指定・保険者入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0023"
                   MOVE    "印刷順指定・レセプト種別入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0024"
                   MOVE    "在総診入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0025"
                   MOVE    "長期（長期高額療養費）入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0026"
                   MOVE    "所在地保険者番号入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0027"
                   MOVE    "所在地保険者印刷位置入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0028"
                   MOVE    "一括作成出力順（国保）入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0029"
                   MOVE    "所在地保険者は県内の保険者を入力して下さい"
                                                         TO  SPA-ERRMSG
               WHEN    "0030"
                   MOVE    "主科実施の有無入力エラー"    TO  SPA-ERRMSG
               WHEN    "0031"
                   MOVE    "主科設定情報（入院）入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0032"
                   MOVE    "主科設定情報（入院外）入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0033"
                   MOVE    "主科情報取得入力エラー"      TO  SPA-ERRMSG
               WHEN    "0034"
                   MOVE    "複数科同一選択入力エラー"    TO  SPA-ERRMSG
               WHEN    "0035"
                   MOVE    "入退院履歴記載区分（入院外）入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0036"
                   STRING  "社保診療報酬請求書（マル長分）"
                                               DELIMITED  BY  SIZE
                           "入力エラー"        DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0037"
                   MOVE    "自賠責・枠記載区分（入院外）入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0038"
                   MOVE    "自賠責・枠記載区分（入院）入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0039"
                   MOVE    "主科社保設定入力エラー"      TO  SPA-ERRMSG
               WHEN    "0040"
                   MOVE    "主科国保設定入力エラー"      TO  SPA-ERRMSG
               WHEN    "0041"
                   MOVE    "一括作成出力順（全体）入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0042"
                   MOVE    "病棟番号記載区分入力エラー"  TO  SPA-ERRMSG
               WHEN    "0043"
                   MOVE    "印刷チェック解除設定入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0044"
                   MOVE
                       "自倍責様式選択（従来様式・入院外）入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0045"
                   MOVE
                       "自倍責様式選択（従来様式・入院）入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0046"
                   MOVE    "広域連合レセ電出力区分入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0047"
                   MOVE    "広域連合ＯＣＲ記載区分入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0048"
                   MOVE    "一括作成出力順（広域連合）入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0049"
                   MOVE    "広域連合作成区分入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0050"
                   MOVE    "主科広域連合設定入力エラー"  TO  SPA-ERRMSG
               WHEN    "0051"
                   MOVE    "広域診療報酬請求書区分入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0052"
                   MOVE    "医師別出力設定入力エラー"    TO  SPA-ERRMSG
               WHEN    "0053"
                   MOVE    "医師別出力順入力エラー"      TO  SPA-ERRMSG
               WHEN    "0054"
                   MOVE    "入院料未算定時コメント記載区分入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0055"
                   MOVE    "労災・枠記載区分（入院外）入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0056"
                   MOVE    "労災・枠記載区分（入院）入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0057"
                   MOVE    "労災コメント記載区分入力エラー"
                                                         TO  SPA-ERRMSG
      *********WHEN    "0058"
      *********    MOVE    "レセ電出力先入力エラー"      TO  SPA-ERRMSG
               WHEN    "0059"
                   MOVE    "病棟別出力設定入力エラー"    TO  SPA-ERRMSG
               WHEN    "0060"
                   MOVE    "病棟別出力順入力エラー"      TO  SPA-ERRMSG
               WHEN    "0061"
                   MOVE    "一括作成出力順（全体）入院入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0062"
                   MOVE    "生保等まとめ入力画面表示順入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0063"
                   MOVE    "社保ＯＣＲ記載区分入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0064"
                   MOVE    "自費レセプト印刷区分入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0065"
                   MOVE    "チェック区分入力エラー"      TO  SPA-ERRMSG
               WHEN    "0066"
                   MOVE    "病名記載順区分入力エラー"    TO  SPA-ERRMSG
               WHEN    "0067"
                   MOVE    "点検用レセプト記載入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0068"
                   MOVE    "労災様式選択（入院外）入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0069"
                   MOVE    "労災様式選択（入院）入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0070"
                   MOVE    "点検用レセプト０点公費記載入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0071"
                   MOVE    "選択番号入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0072"
                   MOVE    "保険者番号入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0073"
                   MOVE    "これ以上保険者を追加できません"
                                               TO  SPA-ERRMSG
               WHEN    "0074"
                   MOVE    "健康保険組合、地方共済組合ではありません"
                                               TO  SPA-ERRMSG
               WHEN    "0075"
                   MOVE    "既に入力済の保険者番号です"
                                               TO  SPA-ERRMSG
               WHEN    "0076"
                   MOVE    "入退院履歴記載区分（入院）入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0077"
                   STRING  "入院料他保険算定時コメント記載区分"
                                               DELIMITED  BY  SIZE
                           "入力エラー"        DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0078"
                   MOVE
                       "自倍責様式選択（新様式・入院外）入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0079"
                   MOVE
                       "自倍責様式選択（新様式・入院）入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0080"
                   MOVE
                       "食事欄記載入力エラー"  TO  SPA-ERRMSG
      *
               WHEN    "0096"
                   STRING  "一括作成出力順（国保）の指定が編綴順です。"
                                               DELIMITED  BY  SIZE
                           "編綴順の設定を行って下さい"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0097"
                   MOVE    "印刷順に未入力の項目があります"
                                               TO  SPA-ERRMSG
               WHEN    "0098"
                   STRING
                       "医療機関の所在地保険者印刷位置を選択して"
                                               DELIMITED  BY  SIZE
                       "あるのに所在地保険者が未入力です"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0099"
                   MOVE    "未入力の項目があります"
                                               TO  SPA-ERRMSG
               WHEN    "0100"
                   MOVE
                   "主科対応をしないときに設定されている項目があります"
                                               TO  SPA-ERRMSG
               WHEN    "0101"
                   MOVE    "旧総合病院であるとき主科設定はできません"
                                               TO  SPA-ERRMSG
               WHEN    "0102"
                   MOVE    "社保または国保、広域の設定がありません"
                                               TO  SPA-ERRMSG
               WHEN    "0103"
                   MOVE    "ＣＤ−Ｒ出力入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0104"
                   MOVE    "症状詳記作成区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0105"
                   MOVE    "光ディスク等送付書作成区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0106"
                   MOVE    "履歴保存入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0107"
                   MOVE    "ファイル出力先設定入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0108"
                   MOVE    "記号編集設定入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0109"
                   MOVE    "記号対象保険者入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0110"
                   MOVE    "番号編集設定入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0111"
                   MOVE    "番号対象保険者入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0112"
                   MOVE    "対象保険者を設定して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0113"
                   MOVE    "災害該当レセプトのレセ電記録区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0114"
                   MOVE    "転帰済傷病名記載区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0115"
                   MOVE    "レセ電データチェック入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0116"
                   MOVE    "点検用・提出用初期表示入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0117"
                   MOVE    "医師名記載区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0118"
                   STRING   "医師別出力設定が「医師別出力をしない」"
                                               DELIMITED  BY  SIZE
                            "のときは医師名の記載を行うことはできません"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0119"
                   MOVE    "ターミナルデジットによる出力設定入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0120"
                   MOVE    "点検用レセプト包括診療分記載入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0121"
                   MOVE    "労災レセ電出力区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0122"
                   STRING  "労災指定医療機関でないため"
                                               DELIMITED  BY  SIZE
                           "レセ電出力区分の設定はできません"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0123"
                   MOVE    "ファイル出力先設定を行って下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0124"
                   MOVE    "０点全国公費レセプト記載区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0125"
                   MOVE    "光ディスク等送付書の媒体設定入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0126"
                   STRING  "労災診療費請求書、アフターケア委託費請求書"
                                               DELIMITED  BY  SIZE
                           "の様式選択入力エラー"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0127"
                   STRING  "労災診療費請求書、アフターケア委託費請求書"
                                               DELIMITED  BY  SIZE
                           "の枠記載区分入力エラー"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0128"
                   STRING  "自賠責請求金額欄端数処理区分（自賠責）"
                                               DELIMITED  BY  SIZE
                           "入力エラー"        DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0129"
                   STRING  "自賠責請求金額欄端数処理区分（第三者行為）"
                                               DELIMITED  BY  SIZE
                           "入力エラー"        DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0130"
                   MOVE    "保険履歴記載区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0131"
                   MOVE    "現物給付一部負担金記載入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0132"
                   MOVE    "点検用レセプト一般名処方記載入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0133"
                   MOVE    "湿布薬用法レセプト記載入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0134"
                   STRING  "社保診療報酬請求書（災害該当分）"
                                               DELIMITED  BY  SIZE
                           "入力エラー"        DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0135"
                   STRING  "災害該当分（非減免対象と減免対象）分割"
                                               DELIMITED  BY  SIZE
                           "入力エラー"        DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0136"
                   MOVE    "１５円以下薬剤レセプト記載入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0137"
                   MOVE    "自賠責ＰＤＦファイル作成入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0138"
                   MOVE    "全国公費一部負担金記載入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0139"
                   MOVE    "労災・様式選択入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0140"
                   MOVE    "労災診療費請求書・様式選択入力エラー"
                                               TO  SPA-ERRMSG
      *
               WHEN    "1003"
                   MOVE    "更新ができませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除ができませんでした"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W12"                TO  SPA-MOTOPG
           MOVE    "WERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
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
           MOVE    SPACE               TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "レセプト印刷情報を登録します"
                                                TO  WRK-WIDMSG
               WHEN    "1002"
                   STRING  "レセ電出力先がマウントできませんが、"
                                           DELIMITED  BY  SIZE
                           "レセプト印刷情報を登録しますか"
                                           DELIMITED  BY  SIZE
                                           INTO  WRK-WIDMSG
                   END-STRING
               WHEN    "1003"
                   STRING  "病名記載順が入力順の設定ですので、"
                                           DELIMITED  BY  SIZE
                           "主病名の編集を行わないに変更します"
                                           DELIMITED  BY  SIZE
                                           INTO  WRK-WIDMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W12"                TO  SPA-MOTOPG
           MOVE    "WID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ガイドメッセージ表示２
      *****************************************************************
       530-WID2SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD2
               WHEN    "1004"
                   STRING  "広域連合作成区分の設定を"
                                           DELIMITED  BY  SIZE
                          "国保分とまとめて作成に変更しますか"
                                           DELIMITED  BY  SIZE
                                           INTO  WRK-WIDMSG
                   END-STRING
               WHEN    "1005"
                   STRING  "特定健康保険組合も一緒に直接請求しますか"
                                           DELIMITED  BY  SIZE
                           "　保険者番号："
                                           DELIMITED  BY  SIZE
                           SPA-GMN-HKNJANUM1-63
                                           DELIMITED  BY  SIZE
                                           INTO  WRK-WIDMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE               TO  WID2
           INITIALIZE                      WID2
           MOVE    SPA-WIDCD2          TO  WID2-ID1CODE
           MOVE    WRK-WIDMSG          TO  WID2-ID1MSG
           MOVE    "いいえ"            TO  WID2-B01
           MOVE    "は  い"            TO  WID2-B12
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "W12"               TO  SPA-MOTOPG
           MOVE    "WID2"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WID2"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END              
      *
           .
       530-WID2SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理    
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
          IF      SPA-GMN-CUR          =   ZERO
               PERFORM 50011-CUR-SET-SEC
          END-IF
      *
          EVALUATE    SPA-GMN-CUR
                WHEN    1
                    MOVE  "BYOMEIHENKBN"     TO   MCP-WIDGET
                WHEN    2
                    MOVE  "BYOMEIHENKBN1"    TO   MCP-WIDGET
                WHEN    3
                    MOVE  "TENKIHENKBN"      TO   MCP-WIDGET
                WHEN    301
                    MOVE  "TENKIBYOMEI_KBN"  TO   MCP-WIDGET
                WHEN    4
                    MOVE  "NYUTAIINREKI"     TO   MCP-WIDGET
                WHEN    304
                    MOVE  "NYUTAIINREKI_NYU" TO   MCP-WIDGET
                WHEN    7
                    MOVE  "KKHOCRKBN"        TO   MCP-WIDGET
                WHEN    8
                    MOVE  "PREFPRGID"        TO   MCP-WIDGET
                WHEN    12
                    MOVE  "RENNUMHENKBN"     TO   MCP-WIDGET
                WHEN    13
                    MOVE  "PREFPRGID1"       TO   MCP-WIDGET
                WHEN    15
                    MOVE  "SYUBYOHENFLG"     TO   MCP-WIDGET
                WHEN    19
                    MOVE  "RJNGNBKKBN"       TO   MCP-WIDGET
                WHEN    20
                    MOVE  "JIHITEKKSIKBN"    TO   MCP-WIDGET
                WHEN    21
                    MOVE  "SYAOCRKBN"        TO   MCP-WIDGET
                WHEN    23
                    MOVE  "SRYKAHENKBN"      TO   MCP-WIDGET
                WHEN    41
                    MOVE  "BTUNUM_KSIKBN"    TO   MCP-WIDGET
                WHEN    28
                    MOVE  "AUTODSPKBN"       TO   MCP-WIDGET
                WHEN    43
                    MOVE  "KOUIKIOCRKBN"     TO   MCP-WIDGET
                WHEN    60
                    MOVE  "COMMENT_NYUINRYO" TO   MCP-WIDGET
                WHEN    307
                    MOVE  "COMMENT_NYUINRYO1"
                                             TO   MCP-WIDGET
                WHEN    64
                    MOVE  "CHKKBN"           TO   MCP-WIDGET
                WHEN    65
                    MOVE  "BYOMEI_PRTJYUN"   TO   MCP-WIDGET
                WHEN    308
                    MOVE  "SHOKUJI_PRTKBN"   TO   MCP-WIDGET
                WHEN    62
                    MOVE  "SEIHO_DSPKBN"     TO   MCP-WIDGET
                WHEN    302
                    MOVE  "RECEIPT_MAKE"     TO   MCP-WIDGET
                WHEN    68
                    MOVE  "SELNUM"           TO   MCP-WIDGET
                WHEN    69
                    MOVE  "HKNJANUM1"        TO   MCP-WIDGET
                WHEN    300
                    MOVE  "HKNJANUM_UPD"     TO   MCP-WIDGET
                WHEN    303
                    MOVE  "DRNAME_KSIKBN"    TO   MCP-WIDGET
                WHEN    309
                    MOVE  "ZERO_KOH_PRTKBN"  TO   MCP-WIDGET
      *
                WHEN    400
                    MOVE  "HKN_RIREKI"       TO   MCP-WIDGET
                WHEN    401
                    MOVE  "GENBUTU_ICHIBU"   TO   MCP-WIDGET
                WHEN    403
                    MOVE  "COMPRESS"         TO   MCP-WIDGET
                WHEN    405
                   MOVE  "DISASTER_RECEIPT"  TO   MCP-WIDGET
                WHEN    406
                   MOVE  "YKZ_15"            TO   MCP-WIDGET
                WHEN    407
                   MOVE  "KOUHI_ICHIBU"      TO   MCP-WIDGET
      *
                WHEN    9
                    MOVE  "SYAPRTNM"         TO   MCP-WIDGET
                WHEN    10
                    MOVE  "KKHPRTNM"         TO   MCP-WIDGET
                WHEN    16
                    MOVE  "NEXTPRTKBN_1"     TO   MCP-WIDGET
                WHEN    14
                    MOVE  "NEXTPRTKBN"       TO   MCP-WIDGET
                WHEN    17
                    MOVE  "SYAPRTNM_NEXT"    TO   MCP-WIDGET
                WHEN    18
                    MOVE  "KKHPRTNM_NEXT"    TO   MCP-WIDGET
                WHEN    24
                    MOVE  "SYAPRTNM_N"       TO   MCP-WIDGET
                WHEN    25
                    MOVE  "SYAPRTNM_NEXT_N"  TO   MCP-WIDGET
                WHEN    26
                    MOVE  "KKHPRTNM_N"       TO   MCP-WIDGET
                WHEN    27
                    MOVE  "KKHPRTNM_NEXT_N"  TO   MCP-WIDGET
                WHEN    45
                    MOVE  "KOUIKIPRTNM"      TO   MCP-WIDGET
                WHEN    46
                    MOVE  "KOUIKIPRTNM_NEXT" TO   MCP-WIDGET
                WHEN    47
                    MOVE  "KOUIKIPRTNM_N"    TO   MCP-WIDGET
                WHEN    48
                    MOVE  "KOUIKIPRTNM_NEXT_N"
                                             TO   MCP-WIDGET
                WHEN    42
                    MOVE  "PRT_CHECKKBN"     TO   MCP-WIDGET
                WHEN    63
                    MOVE  "JIHI_PRTKBN"      TO   MCP-WIDGET
                WHEN    40
                    MOVE  "PRTJYUNKBN4"      TO   MCP-WIDGET
                WHEN    29
                    MOVE  "PRTJYUNKBN1"      TO   MCP-WIDGET
                WHEN    31
                    MOVE  "PRTJYUNKBN3"      TO   MCP-WIDGET
                WHEN    30
                    MOVE  "PRTJYUNKBN2"      TO   MCP-WIDGET
                WHEN    49
                    MOVE  "PRTJYUNKBN5"      TO   MCP-WIDGET
                WHEN    61
                    MOVE  "PRTJYUNKBN6"      TO   MCP-WIDGET
      *
                WHEN    200
                    MOVE  "FRAME_JIBAI"      TO   MCP-WIDGET
                WHEN    201
                    MOVE  "FRAME_JIBAI_N"    TO   MCP-WIDGET
                WHEN    210
                    MOVE  "JIBAIPRTKBN"      TO   MCP-WIDGET
                WHEN    211
                    MOVE  "JIBAIPRTKBN_N"    TO   MCP-WIDGET
                WHEN    217
                    MOVE  "JIBAIPRTKBN1"     TO   MCP-WIDGET
                WHEN    218
                    MOVE  "JIBAIPRTKBN1_N"   TO   MCP-WIDGET
                WHEN    313
                    MOVE  "JIBAI_FRACTION_KBN1"
                                             TO   MCP-WIDGET
                WHEN    314
                    MOVE  "JIBAI_FRACTION_KBN2"
                                             TO   MCP-WIDGET
                WHEN    315
                    MOVE  "JIBAI_PDF"        TO   MCP-WIDGET
                WHEN    202
                    MOVE  "ROUPRTNM"         TO   MCP-WIDGET
                WHEN    203
                    MOVE  "ROUPRTNM_NEXT"    TO   MCP-WIDGET
                WHEN    204
                    MOVE  "JBAIPRTNM"        TO   MCP-WIDGET
                WHEN    205
                    MOVE  "JBAIPRTNM_NEXT"   TO   MCP-WIDGET
                WHEN    206
                    MOVE  "ROUPRTNM_N"       TO   MCP-WIDGET
                WHEN    207
                    MOVE  "ROUPRTNM_NEXT_N"  TO   MCP-WIDGET
                WHEN    208
                    MOVE  "JBAIPRTNM_N"      TO   MCP-WIDGET
                WHEN    209
                    MOVE  "JBAIPRTNM_NEXT_N" TO   MCP-WIDGET
                WHEN    212
                    MOVE  "FRAME_ROU"        TO   MCP-WIDGET
                WHEN    213
                    MOVE  "FRAME_ROU_N"      TO   MCP-WIDGET
                WHEN    214 
                    MOVE  "COMMENT_ROU"      TO   MCP-WIDGET
                WHEN    316
                    MOVE  "RSI_RECEIPTKBN"   TO   MCP-WIDGET
                WHEN    215
                    MOVE  "ROUPRTKBN"        TO   MCP-WIDGET
                WHEN    216
                    MOVE  "ROUPRTKBN_N"      TO   MCP-WIDGET
      *
                WHEN    50
                    MOVE  "MOJI_KISAIKBN"    TO   MCP-WIDGET
                WHEN    51
                    MOVE  "KYURATE_KISAIKBN" TO   MCP-WIDGET
                WHEN    52
                    MOVE  "RECESYUBETU_KISAIKBN"
                                             TO   MCP-WIDGET
      *
                WHEN    70
                    MOVE  "PR_PRTJYUN"       TO   MCP-WIDGET
                WHEN    71
                    MOVE  "HKN_PRTJYUN"      TO   MCP-WIDGET
                WHEN    72
                    MOVE  "REC_PRTJYUN"      TO   MCP-WIDGET
                WHEN    73
                    MOVE  "ZAITAKU"          TO   MCP-WIDGET
                WHEN    74
                    MOVE  "CHOUKI"           TO   MCP-WIDGET
                WHEN    75
                    MOVE  "HKNJANUM"         TO   MCP-WIDGET
                WHEN    76
                    MOVE  "HKNJ_PRTKBN"      TO   MCP-WIDGET
                WHEN    77
                    MOVE  "DRCD_PRTKBN"      TO   MCP-WIDGET
                WHEN    78
                    MOVE  "DRCD_PRTJYUNKBN"  TO   MCP-WIDGET
                WHEN    79
                    MOVE  "BTUNUM_PRTKBN"    TO   MCP-WIDGET
                WHEN    80
                    MOVE  "BTUNUM_PRTJYUNKBN"
                                             TO   MCP-WIDGET
                WHEN    81
                    MOVE  "TERMINAL_DIGIT_PRTKBN"
                                             TO   MCP-WIDGET
      *
                WHEN    90
                    MOVE  "SYUKA_KBN"        TO   MCP-WIDGET
                WHEN    91
                    MOVE  "SYUKA_NYUIN"      TO   MCP-WIDGET
                WHEN    92
                    MOVE  "SYUKA_GAIRAI"     TO   MCP-WIDGET
                WHEN    93
                    MOVE  "SYUKA_SELECT"     TO   MCP-WIDGET
                WHEN    94
                    MOVE  "SYUKA_MODE"       TO   MCP-WIDGET
                WHEN    95
                    MOVE  "SYUKA_SYAHO"      TO   MCP-WIDGET
                WHEN    96
                    MOVE  "SYUKA_KOKUHO"     TO   MCP-WIDGET
                WHEN    97
                    MOVE  "SYUKA_KOUIKI"     TO   MCP-WIDGET
      *
                WHEN    100
                    MOVE  "SYARECEKBN"       TO   MCP-WIDGET
                WHEN    101
                    MOVE  "KKHRECEKBN"       TO   MCP-WIDGET
                WHEN    102
                    MOVE  "RECEDENOUT"       TO   MCP-WIDGET
                WHEN    103
                    MOVE  "RECECOMKBN"       TO   MCP-WIDGET
                WHEN    104
                    MOVE  "NI_JYOKEN1"        TO   MCP-WIDGET
                WHEN    105
                    MOVE  "NI_JYOKEN2"        TO   MCP-WIDGET
                WHEN    106
                    MOVE  "NI_JYOKEN3"        TO   MCP-WIDGET
                WHEN    107
                    MOVE  "NI_JYOKEN4"        TO   MCP-WIDGET
                WHEN    108
                    MOVE  "KOUIKIRECEKBN"     TO   MCP-WIDGET
                WHEN    109
                    MOVE  "NI_JYOKEN5"        TO   MCP-WIDGET
                WHEN    110
                    MOVE  "NI_JYOKEN6"        TO   MCP-WIDGET
                WHEN    111
                    MOVE  "RECEDEN_CDRKBN"    TO   MCP-WIDGET
                WHEN    112
                    MOVE  "RECEDEN_LST1KBN"   TO   MCP-WIDGET
                WHEN    113
                    MOVE  "RECEDEN_LST2KBN"   TO   MCP-WIDGET
                WHEN    114
                    MOVE  "RECEDEN_RIREKIKBN"  TO  MCP-WIDGET
                WHEN    115
                    MOVE  "RECEDEN_DATAKBN"    TO  MCP-WIDGET
                WHEN    116
                    MOVE  "KIGO_DELI_FLG"      TO  MCP-WIDGET
                WHEN    117
                    MOVE  "KIGO_DELI_HKNJA"
                                               TO  MCP-WIDGET
                WHEN    118
                    MOVE  "NUM_DELI_FLG"       TO  MCP-WIDGET
                WHEN    119
                    MOVE  "NUM_DELI_HKNJA"
                                               TO  MCP-WIDGET
                WHEN    120
                    MOVE  "RECEDEN_DISASTER"   TO  MCP-WIDGET
                WHEN    121
                    MOVE  "RECEDEN_CHK"        TO  MCP-WIDGET
                WHEN    44
                    MOVE  "KOUIKIKBN"          TO  MCP-WIDGET
                WHEN    306
                    MOVE  "ROURECEKBN"         TO   MCP-WIDGET
                WHEN    310
                    MOVE  "RECEDEN_LST2KBN_MEDIA"
                                               TO   MCP-WIDGET
      * 
                WHEN    11
                    MOVE  "KKHPRTKBN"        TO   MCP-WIDGET
                WHEN    150
                    MOVE  "SYAPRTKBN"        TO   MCP-WIDGET
                WHEN    404
                    MOVE  "DISASTER_SYAPRTKBN"
                                             TO   MCP-WIDGET
                WHEN    151
                    MOVE  "KOUIKIPRTKBN"     TO   MCP-WIDGET
                WHEN    408
                    MOVE  "RSI_INVOICEKBN"   TO   MCP-WIDGET
                WHEN    311
                    MOVE  "ROUSAIPRTKBN"     TO   MCP-WIDGET
                WHEN    312
                    MOVE  "FRAME_ROUSAIPRTKBN"
                                             TO   MCP-WIDGET
      *
                WHEN    66
                    MOVE  "INSPECT_CHAR"     TO   MCP-WIDGET
                WHEN    67
                    MOVE  "INSPECT_ZERO"     TO   MCP-WIDGET
                WHEN    305
                    MOVE  "INSPECT_INCLUDE"  TO   MCP-WIDGET
                WHEN    402
                    MOVE  "INSPECT_GENERAL"  TO   MCP-WIDGET
      *
                WHEN    99
                    MOVE  "B12"              TO   MCP-WIDGET
            END-EVALUATE
      *
            EVALUATE    SPA-GMN-CUR
                WHEN    1   THRU    8
                WHEN    12  THRU    13
                WHEN    15
                WHEN    19  THRU    23
                WHEN    28
                WHEN    32  THRU    39
                WHEN    41
                WHEN    43
                WHEN    60
                WHEN    62
                WHEN    64  THRU    65
                WHEN    68  THRU    69
                WHEN    300 THRU    302
                WHEN    304
                WHEN    307 THRU    309
                    MOVE    0                TO  W12-PAGENO
                WHEN    400 THRU    401
                WHEN    403
                WHEN    405 THRU    407
                    MOVE    1                TO  W12-PAGENO
                WHEN    9
                WHEN    10
                WHEN    14
                WHEN    16  THRU    18
                WHEN    24  THRU    27
                WHEN    29  THRU    31
                WHEN    40
                WHEN    42
                WHEN    45  THRU    49
                WHEN    61
                WHEN    63
                    MOVE    2                TO  W12-PAGENO
                WHEN    200 THRU    218
                WHEN    313 THRU    316
                    MOVE    3                TO  W12-PAGENO
                WHEN    100 THRU    121
                WHEN    44
                WHEN    306
                WHEN    310
                    MOVE    4                TO  W12-PAGENO
                WHEN    70  THRU    81
                    MOVE    5                TO  W12-PAGENO
                WHEN    50  THRU    52
                    MOVE    6                TO  W12-PAGENO
                WHEN    11
                WHEN    150 THRU    151
                WHEN    311 THRU    312
                WHEN    404
                WHEN    408
                    MOVE    7                TO  W12-PAGENO
                WHEN    90  THRU    98
                    MOVE    8                TO  W12-PAGENO
                WHEN    66  THRU    67
                WHEN    303
                WHEN    305
                WHEN    402
                    MOVE    9                TO  W12-PAGENO
                WHEN    99
                    MOVE    SPA-W12-PAGENO   TO  W12-PAGENO
            END-EVALUATE
      *
           MOVE    W12-PAGENO            TO  SPA-W12-PAGENO
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "BYOMEIHENKBN"
                   MOVE    2           TO  SPA-GMN-CUR
               WHEN    "BYOMEIHENKBN1"
                   MOVE    15          TO  SPA-GMN-CUR
               WHEN    "SYUBYOHENFLG"
                   MOVE    3           TO  SPA-GMN-CUR
               WHEN    "TENKIHENKBN"
                   MOVE    301         TO  SPA-GMN-CUR
               WHEN    "TENKIBYOMEI_KBN"
                   MOVE    21          TO  SPA-GMN-CUR
               WHEN    "SYAOCRKBN"
                   MOVE    7           TO  SPA-GMN-CUR
               WHEN    "KKHOCRKBN"
                   MOVE    43          TO  SPA-GMN-CUR
               WHEN    "KOUIKIOCRKBN"
                   MOVE    12          TO  SPA-GMN-CUR
               WHEN    "RENNUMHENKBN"
                   MOVE    4           TO  SPA-GMN-CUR
               WHEN    "NYUTAIINREKI"
                   IF      SPA-BEDSU       >   ZERO
                       MOVE    304         TO  SPA-GMN-CUR
                   ELSE
                       MOVE    19          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "NYUTAIINREKI_NYU"
                   IF      SPA-BEDSU       >   ZERO
                       MOVE    23          TO  SPA-GMN-CUR
                   ELSE
                       MOVE    19          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "SRYKAHENKBN"
                   IF      SPA-BEDSU       >   ZERO
                       MOVE    41          TO  SPA-GMN-CUR
                   ELSE
                       MOVE    19          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "BTUNUM_KSIKBN"
                   MOVE    19          TO  SPA-GMN-CUR
               WHEN    "RJNGNBKKBN"
                   MOVE    20          TO  SPA-GMN-CUR
               WHEN    "JIHITEKKSIKBN"
                   MOVE    60          TO  SPA-GMN-CUR
               WHEN    "COMMENT_NYUINRYO"
                   MOVE    307         TO  SPA-GMN-CUR
               WHEN    "COMMENT_NYUINRYO1"
                   MOVE    64          TO  SPA-GMN-CUR
               WHEN    "CHKKBN"
                   MOVE    65          TO  SPA-GMN-CUR
               WHEN    "BYOMEI_PRTJYUN"
                   MOVE    308         TO  SPA-GMN-CUR
               WHEN    "SHOKUJI_PRTKBN"
                   MOVE    8          TO  SPA-GMN-CUR
               WHEN    "PREFPRGID"
                   IF      SPA-BEDSU       >   ZERO
                       MOVE    13          TO  SPA-GMN-CUR
                   ELSE
                       MOVE    28          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "PREFPRGID1"
                   MOVE    28          TO  SPA-GMN-CUR
               WHEN    "AUTODSPKBN"
                   MOVE    62          TO  SPA-GMN-CUR
               WHEN    "SEIHO_DSPKBN"
                   MOVE    302         TO  SPA-GMN-CUR
               WHEN    "RECEIPT_MAKE"
                   MOVE    309          TO  SPA-GMN-CUR
               WHEN    "ZERO_KOH_PRTKBN"
                   MOVE    99          TO  SPA-GMN-CUR
               WHEN    "SELNUM"
                   MOVE    69          TO  SPA-GMN-CUR
               WHEN    "HKNJANUM1"
                   MOVE    300         TO  SPA-GMN-CUR
               WHEN    "HKNJANUM_UPD"
                   MOVE    68          TO  SPA-GMN-CUR
               WHEN    "PRT_CHECKKBN"
                   MOVE    63          TO  SPA-GMN-CUR
               WHEN    "JIHI_PRTKBN"
                   MOVE    40          TO  SPA-GMN-CUR
      *
               WHEN    "HKN_RIREKI"
                   MOVE    401         TO  SPA-GMN-CUR
               WHEN    "GENBUTU_ICHIBU"
                   MOVE    403         TO  SPA-GMN-CUR
               WHEN    "COMPRESS"
                   MOVE    405         TO  SPA-GMN-CUR
               WHEN    "DISASTER_RECEIPT"
                   MOVE    406         TO  SPA-GMN-CUR
               WHEN    "YKZ_15"
                   MOVE    407         TO  SPA-GMN-CUR
               WHEN    "KOUHI_ICHIBU"
                   MOVE    99          TO  SPA-GMN-CUR
      *
               WHEN    "PRTJYUNKBN4"
                   IF      SPA-BEDSU       =   ZERO
                       IF      SPA-GMN-PRTJYUN4 =   SPACE
                           MOVE    29          TO  SPA-GMN-CUR
                       ELSE
                           MOVE    30          TO  SPA-GMN-CUR
                       END-IF
                   ELSE
                       MOVE    61          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "PRTJYUNKBN6"
                   IF      SPA-GMN-PRTJYUN4 =   SPACE
                       MOVE    29          TO  SPA-GMN-CUR
                   ELSE
                       MOVE    30          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "PRTJYUNKBN1"
                   MOVE    31          TO  SPA-GMN-CUR
               WHEN    "PRTJYUNKBN3"
                   MOVE    49          TO  SPA-GMN-CUR
               WHEN    "PRTJYUNKBN5"
                   MOVE    30          TO  SPA-GMN-CUR
               WHEN    "PRTJYUNKBN2"
                   MOVE    9           TO  SPA-GMN-CUR
               WHEN    "SYAPRTNM"
                   MOVE    17          TO  SPA-GMN-CUR
               WHEN    "SYAPRTNM_NEXT"
                   MOVE    10          TO  SPA-GMN-CUR
               WHEN    "KKHPRTNM"
                   MOVE    18          TO  SPA-GMN-CUR
               WHEN    "KKHPRTNM_NEXT"
                   MOVE    45          TO  SPA-GMN-CUR
               WHEN    "KOUIKIPRTNM"
                   MOVE    46          TO  SPA-GMN-CUR
               WHEN    "KOUIKIPRTNM_NEXT"
                   MOVE    16          TO  SPA-GMN-CUR
               WHEN    "NEXTPRTKBN_1"
                   IF      SPA-BEDSU       >   ZERO
                       MOVE    24              TO  SPA-GMN-CUR
                   ELSE
                       MOVE    99              TO  SPA-GMN-CUR
                   END-IF
               WHEN    "SYAPRTNM_N"
                   MOVE    25          TO  SPA-GMN-CUR
               WHEN    "SYAPRTNM_NEXT_N"
                   MOVE    26          TO  SPA-GMN-CUR
               WHEN    "KKHPRTNM_N"
                   MOVE    27          TO  SPA-GMN-CUR
               WHEN    "KKHPRTNM_NEXT_N"
                   MOVE    47          TO  SPA-GMN-CUR
               WHEN    "KOUIKIPRTNM_N"
                   MOVE    48          TO  SPA-GMN-CUR
               WHEN    "KOUIKIPRTNM_NEXT_N"
                   MOVE    14          TO  SPA-GMN-CUR
               WHEN    "NEXTPRTKBN"
                   MOVE    99          TO  SPA-GMN-CUR
      *
               WHEN    "FRAME_ROU"
                   MOVE    213         TO  SPA-GMN-CUR
               WHEN    "FRAME_ROU_N"
                   MOVE    316         TO  SPA-GMN-CUR
               WHEN    "RSI_RECEIPTKBN"
                   MOVE    215         TO  SPA-GMN-CUR
               WHEN    "ROUPRTKBN"
                   MOVE    216         TO  SPA-GMN-CUR
               WHEN    "ROUPRTKBN_N"
                   MOVE    214         TO  SPA-GMN-CUR
               WHEN    "COMMENT_ROU"
                   MOVE    200         TO  SPA-GMN-CUR
               WHEN    "FRAME_JIBAI"
                   MOVE    201         TO  SPA-GMN-CUR
               WHEN    "FRAME_JIBAI_N"
                   MOVE    210         TO  SPA-GMN-CUR
               WHEN    "JIBAIPRTKBN"
                   MOVE    211         TO  SPA-GMN-CUR
               WHEN    "JIBAIPRTKBN_N"
                   MOVE    217         TO  SPA-GMN-CUR
               WHEN    "JIBAIPRTKBN1"
                   MOVE    218         TO  SPA-GMN-CUR
               WHEN    "JIBAIPRTKBN1_N"
                   MOVE    313         TO  SPA-GMN-CUR
               WHEN    "JIBAI_FRACTION_KBN1"
                   MOVE    314         TO  SPA-GMN-CUR
               WHEN    "JIBAI_FRACTION_KBN2"
                   MOVE    315         TO  SPA-GMN-CUR
               WHEN    "JIBAI_PDF"
                   MOVE    202         TO  SPA-GMN-CUR
               WHEN    "ROUPRTNM"
                   MOVE    203         TO  SPA-GMN-CUR
               WHEN    "ROUPRTNM_NEXT"
                   MOVE    204         TO  SPA-GMN-CUR
               WHEN    "JBAIPRTNM"
                   MOVE    205         TO  SPA-GMN-CUR
               WHEN    "JBAIPRTNM_NEXT"
                   IF      SPA-BEDSU       >   ZERO
                       MOVE    206             TO  SPA-GMN-CUR
                   ELSE
                       MOVE    99              TO  SPA-GMN-CUR
                   END-IF
               WHEN    "ROUPRTNM_N"
                   MOVE    207         TO  SPA-GMN-CUR
               WHEN    "ROUPRTNM_NEXT_N"
                   MOVE    208         TO  SPA-GMN-CUR
               WHEN    "JBAIPRTNM_N"
                   MOVE    209         TO  SPA-GMN-CUR
               WHEN    "JBAIPRTNM_NEXT_N"
                   MOVE    99          TO  SPA-GMN-CUR
      *
               WHEN    "MOJI_KISAIKBN"
                   MOVE    51          TO  SPA-GMN-CUR
               WHEN    "KYURATE_KISAIKBN"
                   MOVE    52          TO  SPA-GMN-CUR
               WHEN    "RECESYUBETU_KISAIKBN"
                   MOVE    99          TO  SPA-GMN-CUR
      *
               WHEN    "PR_PRTJYUN"
                   MOVE    71          TO  SPA-GMN-CUR
               WHEN    "HKN_PRTJYUN"
                   MOVE    72          TO  SPA-GMN-CUR
               WHEN    "REC_PRTJYUN"
                   MOVE    73          TO  SPA-GMN-CUR
               WHEN    "ZAITAKU"
                   MOVE    74          TO  SPA-GMN-CUR
               WHEN    "CHOUKI"
                   MOVE    75          TO  SPA-GMN-CUR
               WHEN    "HKNJANUM"
                   IF      SPA-GMN-HKNJANUM    =   SPACE
                       MOVE    77          TO  SPA-GMN-CUR
                   ELSE
                       MOVE    76          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "HKNJ_PRTKBN"
                   MOVE    77          TO  SPA-GMN-CUR
               WHEN    "DRCD_PRTKBN"
                   MOVE    78          TO  SPA-GMN-CUR
               WHEN    "DRCD_PRTJYUNKBN"
                   MOVE    79          TO  SPA-GMN-CUR
               WHEN    "BTUNUM_PRTKBN"
                   MOVE    80          TO  SPA-GMN-CUR
               WHEN    "BTUNUM_PRTJYUNKBN"
                   MOVE    81          TO  SPA-GMN-CUR
               WHEN    "TERMINAL_DIGIT_PRTKBN"
                   MOVE    99          TO  SPA-GMN-CUR
      *
               WHEN    "SYUKA_KBN"
                   MOVE    91          TO  SPA-GMN-CUR
               WHEN    "SYUKA_NYUIN"
                   MOVE    92          TO  SPA-GMN-CUR
               WHEN    "SYUKA_GAIRAI"
                   MOVE    93          TO  SPA-GMN-CUR
               WHEN    "SYUKA_SELECT"
                   MOVE    94          TO  SPA-GMN-CUR
               WHEN    "SYUKA_MODE"
                   MOVE    95          TO  SPA-GMN-CUR
               WHEN    "SYUKA_SYAHO"
                   MOVE    96          TO  SPA-GMN-CUR
               WHEN    "SYUKA_KOKUHO"
                   MOVE    97          TO  SPA-GMN-CUR
               WHEN    "SYUKA_KOUIKI"
                   MOVE    99          TO  SPA-GMN-CUR
      *
               WHEN    "SYAPRTKBN"
                   MOVE    404         TO  SPA-GMN-CUR
               WHEN    "DISASTER_SYAPRTKBN"
                   MOVE    11          TO  SPA-GMN-CUR
               WHEN    "KKHPRTKBN"
                   MOVE    151         TO  SPA-GMN-CUR
               WHEN    "KOUIKIPRTKBN"
                   MOVE    408         TO  SPA-GMN-CUR
               WHEN    "RSI_INVOICEKBN"
                   MOVE    311         TO  SPA-GMN-CUR
               WHEN    "ROUSAIPRTKBN"
                   MOVE    312         TO  SPA-GMN-CUR
               WHEN    "FRAME_ROUSAIPRTKBN"
                   MOVE    99          TO  SPA-GMN-CUR
      *
               WHEN    "SYARECEKBN"
                   MOVE    101         TO  SPA-GMN-CUR
               WHEN    "KKHRECEKBN"
                   MOVE    108         TO  SPA-GMN-CUR
               WHEN    "KOUIKIRECEKBN"
                   MOVE    306         TO  SPA-GMN-CUR
               WHEN    "ROURECEKBN"
                   IF    ( SPA-MW         NOT =   SPA-PANDA )
                       MOVE    103         TO  SPA-GMN-CUR
                   ELSE
                       MOVE    102         TO  SPA-GMN-CUR
                   END-IF
               WHEN    "RECEDENOUT"
                   MOVE    103         TO  SPA-GMN-CUR
               WHEN    "RECECOMKBN"
                   MOVE    104         TO  SPA-GMN-CUR
               WHEN    "NI_JYOKEN1"
                   MOVE    105         TO  SPA-GMN-CUR
               WHEN    "NI_JYOKEN2"
                   MOVE    109         TO  SPA-GMN-CUR
               WHEN    "NI_JYOKEN5"
                   MOVE    106         TO  SPA-GMN-CUR
               WHEN    "NI_JYOKEN3"
                   MOVE    107         TO  SPA-GMN-CUR
               WHEN    "NI_JYOKEN4"
                   MOVE    110         TO  SPA-GMN-CUR
               WHEN    "NI_JYOKEN6"
                   MOVE    111         TO  SPA-GMN-CUR
               WHEN    "RECEDEN_CDRKBN"
                   MOVE    112         TO  SPA-GMN-CUR
               WHEN    "RECEDEN_LST1KBN"
                   MOVE    113         TO  SPA-GMN-CUR
               WHEN    "RECEDEN_LST2KBN"
                   MOVE    310         TO  SPA-GMN-CUR
               WHEN    "RECEDEN_LST2KBN_MEDIA"
                   IF    ( SPA-MW         NOT =   SPA-PANDA )
                       MOVE    115         TO  SPA-GMN-CUR
                   ELSE
                       MOVE    114         TO  SPA-GMN-CUR
                   END-IF
               WHEN    "RECEDEN_RIREKIKBN"
                   MOVE    115         TO  SPA-GMN-CUR
               WHEN    "RECEDEN_DATAKBN"
                   MOVE    120         TO  SPA-GMN-CUR
               WHEN    "RECEDEN_DISASTER"
                   MOVE    121         TO  SPA-GMN-CUR
               WHEN    "RECEDEN_CHK"
                   MOVE    44          TO  SPA-GMN-CUR
               WHEN    "KOUIKIKBN"
                   MOVE    116         TO  SPA-GMN-CUR
               WHEN    "KIGO_DELI_FLG"
                   MOVE    117         TO  SPA-GMN-CUR
               WHEN    "KIGO_DELI_HKNJA"
                   MOVE    118         TO  SPA-GMN-CUR
               WHEN    "NUM_DELI_FLG"
                   MOVE    119         TO  SPA-GMN-CUR
               WHEN    "NUM_DELI_HKNJA"
                   MOVE    99          TO  SPA-GMN-CUR
      *
               WHEN    "INSPECT_CHAR"
                   MOVE    67          TO  SPA-GMN-CUR
               WHEN    "INSPECT_ZERO"
                   MOVE    305         TO  SPA-GMN-CUR
               WHEN    "INSPECT_INCLUDE"
                   MOVE    402         TO  SPA-GMN-CUR
               WHEN    "INSPECT_GENERAL"
                   MOVE    303         TO  SPA-GMN-CUR
               WHEN    "DRNAME_KSIKBN"
                   MOVE    99          TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
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
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
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
      *    保険者マスター読込（キー）
      *****************************************************************
       900-HKNJAINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNJAINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-HKNJAINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
               CALL    "coblog" USING "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
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
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
