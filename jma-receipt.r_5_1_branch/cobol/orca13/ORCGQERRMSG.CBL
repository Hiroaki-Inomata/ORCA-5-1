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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCGQERRMSG.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者照会
      *  コンポーネント名  : 患者照会サブ（エラーメッセージ返却）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/10/04    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "COMMON-SPA".
           COPY    "Q01SCR-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           SPA-Q01FREE.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           IF    ( SPA-ERRCD       =   "0030" OR "0070" )
               CONTINUE
           ELSE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           EXIT PROGRAM
           .
       000-PROC-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    SPACE               TO  SPA-Q01-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0002"
               MOVE    "患者番号の桁数が違います。" 
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0003"
               MOVE    "対象件数が２００件をオーバーしました。" 
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0004"
               MOVE    "このコードの病名は登録されていません。" 
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0005"
               MOVE    "このコードの行為名は登録されていません。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0006"
               MOVE    "未来日は入力できません。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0007"
               MOVE    "セットコードは入力できません。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0008"
               MOVE    "項目を設定してください。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0009"
               MOVE    "全角文字でのみ入力してください。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0101"
               MOVE    "診療年月日の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0102"
               MOVE    "生年月日の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0103"
               MOVE    "年齢の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0104"
               MOVE    "基準日の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0105"
               MOVE    "最終受診療日の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0106"
               MOVE    "患者番号の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0107"
               MOVE    "病名コードの入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0108"
               MOVE    "診療行為コードの入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0109"
               MOVE    "性別の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0110"
               MOVE    "保険の種類の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0111"
               MOVE    "公費の種類の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0112"
               MOVE  "項目名の入力に誤りがあります（検索結果　基本）。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0113"
               MOVE    "本人家族の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0114"
               MOVE    "状態１の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0115"
               MOVE    "状態２の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0116"
               MOVE    "状態３の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0117"
               MOVE    "診療科の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0118"
               MOVE    "入外区分の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0119"
               MOVE    "疑い病名区分の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0120"
               MOVE    "転帰区分の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0121"
               MOVE    "開始転帰日の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0122"
               MOVE    "終了転帰日の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0123"
               MOVE    "減免事由の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0124"
               MOVE    "低所得１・２の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0125"
               MOVE    "低所得期間開始の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0126"
               MOVE    "低所得開始月識別の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0127"
               MOVE    "低所得期間終了の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0128"
               MOVE    "死亡区分の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0129"
               MOVE    "診療区分の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0130"
               MOVE    "診療種別の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0131"
               MOVE    "コメント選択の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0132"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING  "レセプトコメントのキーワード"  DELIMITED BY SIZE
                       "は３語までにしてください。"    DELIMITED BY SIZE
               INTO    SPA-Q01-ERRMSG
               END-STRING
           WHEN    "0133"
               MOVE    "誕生月の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0134"
               MOVE    "急性病名区分の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0135"
               MOVE    "主病名区分の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0136"
               MOVE    "特記事項情報の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0137"
               MOVE    "特記事項コードの入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0138"
               MOVE    "特記事項名称の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0139"
               MOVE  "項目名の入力に誤りがあります（検索結果　点数）。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0140"
               IF    ( SPA-MOTOPG      =   "API" )
                   MOVE    "数量・回数指定区分の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
               ELSE
                 MOVE "診療行為数値項目指定区分の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
               END-IF
           WHEN    "0141"
               IF    ( SPA-MOTOPG      =   "API" )
                   MOVE   "数量（回数又は合計）の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
               ELSE
                   MOVE    "診療行為数値の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
               END-IF
           WHEN    "0142"
               IF    ( SPA-MOTOPG      =   "API" )
                   MOVE "診療行為の比較条件区分の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
               ELSE
                 MOVE "診療行為数値項目比較条件の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
               END-IF
           WHEN    "0143"
               MOVE    "災害該当の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0144"
               MOVE    "病名比較条件の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0145"
               MOVE    "点検用レセプトの入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0146"
               MOVE    "開始診療開始日の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0147"
               MOVE    "終了診療開始日の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0148"
               MOVE    "包括分の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0149"
               MOVE    "テスト患者の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0150"
               MOVE    "保険有効期間終了月の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0151"
               MOVE    "公費適用期間終了月の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0152"
               MOVE    "負担割合の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0153"
               MOVE    "地域連携情報提供の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0154"
               MOVE    "介護保険開始月識別の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0155"
               MOVE    "介護保険期間開始の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0156"
               MOVE    "介護保険期間終了の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0157"
               MOVE    "介護保険保険者番号の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0158"
               MOVE    "介護認定開始月識別の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0159"
               MOVE    "介護認定期間開始の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0160"
               MOVE    "介護認定期間終了の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0161"
               MOVE    "介護認定要介護状態の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0162"
               MOVE    "ドクター入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0163"
               MOVE    "低所得者有効期間識別の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0164"
               MOVE    "電話番号識別の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0165"
               MOVE    "電話番号の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0166"
               MOVE    "コメント識別の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0167"
               MOVE    "低所得者開始月の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0168"
               MOVE    "低所得者終了月の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0169"
               MOVE    "低所得者有効開始月の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0170"
               MOVE    "低所得者有効終了月の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0171"
               MOVE    "低所得者有効開始日の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0172"
               MOVE    "低所得者有効終了日の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0173"
               MOVE    "下限点数の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0174"
               MOVE    "上限点数の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0175"
               MOVE    "保険者番号検索区分の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0176"
               MOVE    "保険者番号の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0177"
               MOVE    "保険の種類検索区分の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0178"
               MOVE    "公費の種類検索区分の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0179"
               MOVE    "負担者番号の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0180"
               MOVE    "保険公費検索区分の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0181"
               MOVE    "介護保険有効期間識別の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0182"
               MOVE    "介護保険開始月の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0183"
               MOVE    "介護保険終了月の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0184"
               MOVE    "介護保険有効開始月の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0185"
               MOVE    "介護保険有効終了月の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0186"
               MOVE    "介護保険有効開始日の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0187"
               MOVE    "介護保険有効終了日の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0188"
               MOVE    "介護保険保険者番号の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0189"
               MOVE    "介護認定有効期間識別の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0190"
               MOVE    "介護認定開始月の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0191"
               MOVE    "介護認定終了月の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0192"
               MOVE    "介護認定有効開始月の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0193"
               MOVE    "介護認定有効終了月の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0194"
               MOVE    "介護認定有効開始日の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0195"
               MOVE    "介護認定有効終了日の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0196"
               MOVE    "並べ替え項目１の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0197"
               MOVE    "並び順１の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0198"
               MOVE    "並べ替え項目２の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0199"
               MOVE    "並び順２の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0200"
               MOVE    "並べ替え項目３の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0201"
               MOVE    "並び順３の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0202"
               MOVE    "診療行為検索区分の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0203"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING  "電子カルテＵＩＤが未入力です。"
                                                       DELIMITED BY SIZE
                       "初回接続からやり直してください。"
                                                       DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "0205"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING  "オルカＵＩＤが未入力です。"    DELIMITED BY SIZE
                       "初回接続からやり直してください。"
                                                       DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "0206"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING "接続状態の取得に失敗しました。" DELIMITED BY SIZE
                      "初回接続から行ってください。"   DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "0207"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING "接続状態の取得に失敗しました。" DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "0208"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING  "電子カルテＵＩＤが未入力です。"
                                                       DELIMITED BY SIZE
                       "初回接続からやり直してください。"
                                                       DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "0209"
               MOVE    "ＮＯＴ検索区分の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0210"
               MOVE    "基準日の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0211"
               MOVE    "患者番号の桁数が違います。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0212"
               MOVE    "検索結果データ作成区分の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0213"
               MOVE    "検索結果リスト印刷区分の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0214"
               MOVE    "表題は全角文字のみで入力してください。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0215"
               MOVE    "対象名は全角文字のみで入力してください。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1001"
               MOVE    "開始診療年月日＞終了診療年月日となっています。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1002"
               MOVE    "開始生年月日＞終了生年月日となっています。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1003"
               MOVE    "開始年齢＞終了年齢となっています。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1004"
               MOVE    "生年月日と年齢は同時に入力できません。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1005"
               MOVE    "年齢を入力する場合、基準日は必須項目です。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1006"
               MOVE    "開始患者番号＞終了患者番号となっています。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1007"
               MOVE    "検索条件が入力されていません。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1008"
               MOVE    "開始最終受診日＞終了最終受診日となっています。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1009"
               MOVE    "開始転帰日＞終了転帰日となっています。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1010"
               MOVE    "低所得の有効期間の指定に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1011"
               MOVE    "低所得の開始月を入力してください。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1012"
               MOVE    "低所得の終了月を入力してください。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1013"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               IF    ( SPA-MOTOPG      =   "API" )
                   MOVE    SPACE           TO  SPA-Q01-ERRMSG
                   STRING  "点数情報を照会する場合、"
                                                       DELIMITED BY SIZE
                           "開始診療年月日と終了診療年月日を"
                                                       DELIMITED BY SIZE
                           "同じ月の日付で入力してください。"
                                                       DELIMITED BY SIZE
                   INTO  SPA-Q01-ERRMSG
                   END-STRING
               ELSE
                   MOVE    SPACE           TO  SPA-Q01-ERRMSG
                   STRING  "点数検索を行う場合、"      DELIMITED BY SIZE
                           "診療月ボタンより診療月を指定してください。"
                                                       DELIMITED BY SIZE
                   INTO  SPA-Q01-ERRMSG
                   END-STRING
               END-IF
           WHEN    "1014"
               MOVE    "比較条件を選択してください。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1015"
               IF    ( SPA-MOTOPG      =   "API" )
                   MOVE    "数量・回数指定区分を入力してください。"
                                       TO  SPA-Q01-ERRMSG
               ELSE
                   MOVE    "数値指定を行う項目を選択してください。"
                                       TO  SPA-Q01-ERRMSG
               END-IF
           WHEN    "1016"
               IF    ( SPA-MOTOPG      =   "API" )
                   MOVE    SPACE           TO  SPA-Q01-ERRMSG
                   STRING  "診療行為の数量・回数指定で同じ診療行為が"
                                               DELIMITED BY SIZE
                           "複数入力されています。"
                                               DELIMITED BY SIZE
                   INTO    SPA-Q01-ERRMSG
                   END-STRING
               ELSE
                   MOVE    SPACE           TO  SPA-Q01-ERRMSG
                   STRING  "診療行為の数値指定で同じ診療行為が"
                                               DELIMITED BY SIZE
                           "複数入力されています。"
                                               DELIMITED BY SIZE
                   INTO    SPA-Q01-ERRMSG
                   END-STRING
               END-IF
           WHEN    "1017"
               IF    ( SPA-MOTOPG      =   "API" )
                   MOVE    SPACE           TO  SPA-Q01-ERRMSG
                   STRING  "診療行為の数量・回数指定を行う場合、"
                                               DELIMITED BY SIZE
                           "対象となる診療行為を入力してください。"
                                               DELIMITED BY SIZE
                   INTO    SPA-Q01-ERRMSG
                   END-STRING
               ELSE
                   MOVE    SPACE           TO  SPA-Q01-ERRMSG
                   STRING  "診療行為の数値指定を行う場合、"
                                               DELIMITED BY SIZE
                           "対象となる診療行為を入力してください。"
                                               DELIMITED BY SIZE
                   INTO    SPA-Q01-ERRMSG
                   END-STRING
               END-IF
           WHEN    "1018"
               IF    ( SPA-MOTOPG      =   "API" )
                   MOVE    SPACE           TO  SPA-Q01-ERRMSG
                   STRING  "診療行為の数量・回数指定を行う場合、"
                                           DELIMITED BY SIZE
                           "診療年月日の期間指定をしてください"
                                           DELIMITED BY SIZE
                           "（最大１２ケ月）。"
                                           DELIMITED BY SIZE
                   INTO    SPA-Q01-ERRMSG
                   END-STRING
               ELSE
                   MOVE    SPACE           TO  SPA-Q01-ERRMSG
                   STRING  "診療行為の数値指定を行う場合、"
                                           DELIMITED BY SIZE
                           "診療年月日の期間指定をしてください"
                                           DELIMITED BY SIZE
                           "（最大１２ケ月）。"
                                           DELIMITED BY SIZE
                   INTO    SPA-Q01-ERRMSG
                   END-STRING
               END-IF
           WHEN    "1019"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING  "点数と診療行為の数値指定は"
                                           DELIMITED BY SIZE
                       "いずれかのみ設定してください。"
                                           DELIMITED BY SIZE
               INTO    SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1020"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING  "ＮＯＴ検索を行う場合、"
                                           DELIMITED BY SIZE
                       "診療年月日の期間指定をしてください"
                                           DELIMITED BY SIZE
                       "（最大１２ケ月）。"
                                           DELIMITED BY SIZE
               INTO    SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1021"
               MOVE    "開始診療開始日＞終了診療開始日となっています。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1022"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING  "包括分を含まない検索を行う場合、対象となる"
                                           DELIMITED BY SIZE
                       "診療行為を入力してください。"
                                           DELIMITED BY SIZE
               INTO    SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1023"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING     "保険有効期間終了月の検索を行う場合、"
                                           DELIMITED BY SIZE
                          "保険者番号または保険の種類を"
                                           DELIMITED BY SIZE
                          "入力してください。"
                                           DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1024"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING     "保険有効期間終了月の検索を行う場合、"
                                           DELIMITED BY SIZE
                          "ＮＯＴ検索はできません。"
                                           DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1025"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING     "公費適用期間終了月の検索を行う場合、"
                                           DELIMITED BY SIZE
                          "公費の種類または負担者番号を"
                                           DELIMITED BY SIZE
                          "入力してください。"
                                           DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1026"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING     "公費適用期間終了月の検索を行う場合、"
                                           DELIMITED BY SIZE
                          "ＮＯＴ検索はできません。"
                                           DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1027"
               IF    ( SPA-MOTOPG      =   "API" )
                   MOVE    SPACE           TO  SPA-Q01-ERRMSG
                   STRING  "負担割合検索を行う場合、"  DELIMITED BY SIZE
                           "開始診療年月日と終了診療年月日を"
                                                       DELIMITED BY SIZE
                           "同じ月の日付で入力してください。"
                                                       DELIMITED BY SIZE
                   INTO  SPA-Q01-ERRMSG
                   END-STRING
               ELSE
                   MOVE    SPACE           TO  SPA-Q01-ERRMSG
                   STRING  "負担割合検索を行う場合、"  DELIMITED BY SIZE
                           "診療月ボタンより診療月を指定してください。"
                                                       DELIMITED BY SIZE
                   INTO  SPA-Q01-ERRMSG
                   END-STRING
               END-IF
           WHEN    "1028"
               MOVE    "介護保険の有効期間の指定に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1029"
               MOVE    "介護保険の開始月を入力してください。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1030"
               MOVE    "介護保険の終了月を入力してください。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1031"
               MOVE    "介護認定の有効期間の指定に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1032"
               MOVE    "介護認定の開始月を入力してください。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1033"
               MOVE    "介護認定の終了月を入力してください。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1034"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING  "ドクターの指定を行う場合、"
                                           DELIMITED BY SIZE
                       "診療年月日の期間指定をしてください"
                                           DELIMITED BY SIZE
                       "（最大１２ケ月）。"
                                           DELIMITED BY SIZE
               INTO    SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1036"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING  "診療行為情報を照会する場合、"
                                                       DELIMITED BY SIZE
                       "数量・回数指定区分を入力してください。"
                                                       DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1037"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING  "点数情報を照会する場合、"
                                                       DELIMITED BY SIZE
                       "点数を入力してください。"      DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1038"
               MOVE  "他の端末より同じカルテＵＩＤでの接続があります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1039"
               MOVE  "検索条件が多すぎます。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1040"
               MOVE    "検索中です。しばらくお待ち下さい。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1041"
               MOVE    "検索条件に該当する患者は存在しませんでした。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1042"
               MOVE    "開始件数の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1043"
               MOVE    "終了件数の入力に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1044"
               MOVE    "開始件数が検索結果の総件数を超えています。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1045"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING  "数量・回数指定区分が入力されている場合は"
                                                       DELIMITED BY SIZE
                       "診療行為情報の照会のみ可能です。"
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1046"
               MOVE    "開始件数＞終了件数となっています。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1047"
               MOVE    "転帰区分を入力してください。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1048"
               STRING  "ＮＯＴ検索区分が入力されている場合、"
                                                       DELIMITED BY SIZE
                  "数量（回数又は合計）および比較条件は入力できません。"
                                                       DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1049"
               MOVE    "検索処理に失敗しました。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1050"
               MOVE    "下限点数＞上限点数となっています。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1051"
               STRING  "検索結果リスト印刷区分または"  DELIMITED BY SIZE
                       "検索結果データ作成区分"        DELIMITED BY SIZE
                       "を入力してください。"          DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1052"
               MOVE    "処理実行中です。しばらくお待ち下さい。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1053"
               MOVE    "対象の処理はありません。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1054"
               MOVE    "ファイルの作成に失敗しました。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "4001"      THRU    "4016"
               MOVE    "更新処理に失敗しました。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "8900"
               MOVE    "システム項目が設定できません。"
                                       TO  SPA-Q01-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8901"
               MOVE    "職員情報が取得できません。"
                                       TO  SPA-Q01-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8902"
               MOVE    "医療機関情報が取得できません。"
                                       TO  SPA-Q01-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8903"
               MOVE    "システム日付が取得できません。"
                                       TO  SPA-Q01-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8905"
               MOVE    "患者番号構成情報が取得できません。"
                                       TO  SPA-Q01-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8915"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING "グループ医療機関が不整合です。" DELIMITED BY SIZE
                      "処理を終了して下さい。"         DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "0091"
               MOVE    "リクエスト番号に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0097"
               MOVE    "送信内容に誤りがあります。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0098"
               MOVE    "送信内容の読込ができませんでした。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0099"
               MOVE    "ユーザＩＤが未登録です。"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0000"
               MOVE    "正常終了"
                                       TO  SPA-Q01-ERRMSG
           END-EVALUATE
      *
           .
      *
       200-MAIN-EXT.
           EXIT.
      *
