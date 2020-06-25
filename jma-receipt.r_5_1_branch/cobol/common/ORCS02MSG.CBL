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
       PROGRAM-ID.                     ORCS02MSG.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 負担金計算サブエラーメッセージ編集
      *  管理者            :
      *  作成日付    作業者        記述
      *  13/01/28    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       DATA                            DIVISION.
       WORKING-STORAGE             SECTION.
      *
       01  WRK-AREA.
           03  WRK-MSG                 PIC X(100)  OCCURS 3.
      *
      ****************************************************************
       LINKAGE                         SECTION.
           COPY    "CPORCS02MSG.INC".
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       PROCEDURE                       DIVISION
           USING
           S02MSG-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  S02MSG-MSG-AREA
      *
           EVALUATE    S02MSG-S02-RC
           WHEN    ZERO
               CONTINUE
           WHEN    13
               STRING  "退院計算に必要な会計情報が作成されていません"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    14
               STRING  "入院料と異なる保険の入院料加算があります"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    15
               STRING  "外泊日に入院料加算が算定されています"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    16
               STRING  "入院料の算定が無い日に入院料加算が"
                                           DELIMITED   BY  SIZE
                       "算定されています"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    17
               STRING  "入院料の有効期間が切れています"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    18
               STRING  "保険組合せの有効期間が切れています"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    19
               STRING  "包括点数の治験減点分の点数を"
                                           DELIMITED   BY  SIZE
                       "確認してください"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    20
               STRING  "入院点数に誤りがあります。"
                                           DELIMITED   BY  SIZE
                       "異動処理をしてください"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    21
               STRING  "救急搬送診療料の算定日に"
                                           DELIMITED   BY  SIZE
                       "入院料（加算）の算定があります"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    22
               STRING  "他医受診（外泊）の入力に誤りがあります"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    23
               STRING  "同日再入院日に実日数算定コード"
                                           DELIMITED   BY  SIZE
                       "を入力してください"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    24
               STRING  "同日再入院日の再入院分に入院料加算"
                                           DELIMITED   BY  SIZE
                       "が算定されています"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    25
               STRING  "薬剤料減点数を確認してください"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    26
               STRING  "特定時間退院減算（外泊）の入力に誤りがあります"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    27
               STRING  "特定曜日入退院減算（外泊）の入力に"
                                           DELIMITED   BY  SIZE
                       "誤りがあります"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    29
               STRING  "入院時食事療養（２）算定時に特別食"
                                           DELIMITED   BY  SIZE
                       "加算の算定があります"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    30
               STRING  "保険組合せの数が多すぎます"
                                           DELIMITED   BY  SPACE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    31
               STRING  "入院時生活療養（２）算定時に流動食"
                                           DELIMITED   BY  SIZE
                       "の入力があります"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    33
               STRING  "入院基本設定「入院時食事療養」が未設定です"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    34
               STRING  "適用できない低所得１の入力があります"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    35
               STRING  "同一日に食事保険指定コードの入力が"
                       "２件以上あります"  DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    36
               STRING  "食事保険指定コードの入力日に食事の算定"
                       "がありません"      DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    37
               STRING  "保険組合せが存在しません"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    38
               STRING  "請求点数の合計が上限を超えます"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    39
               STRING  "保険外金額の合計が上限を超えます"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    40
               STRING  "負担割合計算額が上限を超えます"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    41
               STRING  "今回請求金額が上限を超えます"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    42
               STRING  "【精神病床】食事負担据え置き対象コードの"
                                           DELIMITED   BY  SIZE
                       "診療入力を確認"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    43
               STRING  "当月請求済分計算時と所得区分が異なります"
                                           DELIMITED   BY  SIZE
                       "（要確認）"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    101
               STRING  "包括入院期間に算定できない入院料加算"
                                           DELIMITED   BY  SIZE
                       "の算定があります"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           END-EVALUATE
      *
           MOVE    WRK-MSG (1)         TO  S02MSG-MSG (1)
                                           S02MSG-MSG (2)
                                           S02MSG-MSG (3)
      *
      *
           .
           EXIT PROGRAM
           .
