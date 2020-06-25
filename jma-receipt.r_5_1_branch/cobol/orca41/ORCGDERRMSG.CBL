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
       PROGRAM-ID.                     ORCGDERRMSG.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : データチェックサブ（エラーメッセージ返却）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  18/12/27    NACL−太田    新規作成
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
           COPY    "D01COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           SPA-D01KYOUTU.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
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
           MOVE    SPACE           TO  SPA-D01-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "8900"
               MOVE    "システム項目が設定できません。"
                                       TO  SPA-D01-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8901"
               MOVE    "職員情報が取得できません。"
                                       TO  SPA-D01-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8902"
               MOVE    "医療機関情報が取得できません。"
                                       TO  SPA-D01-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8903"
               MOVE    "システム日付が取得できません。"
                                       TO  SPA-D01-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8905"
               MOVE    "患者番号構成情報が取得できません。"
                                       TO  SPA-D01-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8915"
               STRING "グループ医療機関が不整合です。" DELIMITED BY SIZE
                      "処理を終了して下さい。"         DELIMITED BY SIZE
               INTO  SPA-D01-ERRMSG
               END-STRING
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "0091"
               MOVE    "リクエスト番号に誤りがあります。"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0097"
               MOVE    "送信内容に誤りがあります。"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0098"
               MOVE    "送信内容の読込ができませんでした。"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0099"
               MOVE    "ユーザＩＤが未登録です。"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0203"
               STRING  "電子カルテＵＩＤが未入力です。"
                                                       DELIMITED BY SIZE
               INTO  SPA-D01-ERRMSG
               END-STRING
           WHEN    "0204"
               STRING  "電子カルテＵＩＤに半角の英数字、"
                                                       DELIMITED BY SIZE
                       "ハイフン、アンダーバー以外の文字が入力されて"
                                                       DELIMITED BY SIZE
                       "います。"                      DELIMITED BY SIZE
               INTO  SPA-D01-ERRMSG
               END-STRING
           WHEN    "0205"
               MOVE    SPACE           TO  SPA-D01-ERRMSG
               STRING  "オルカＵＩＤが未入力です。"    DELIMITED BY SIZE
               INTO  SPA-D01-ERRMSG
               END-STRING
           WHEN    "0001"
               MOVE    "診療年月の入力に誤りがあります。"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0002"
               MOVE    "診療年月を入力してください。"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0003"
               MOVE    "期間指定開始日の入力に誤りがあります。"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0004"
               MOVE    "期間指定終了日の入力に誤りがあります。"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0005"
               MOVE "期間指定開始日＞期間指定終了日となっています。" 
                                       TO  SPA-D01-ERRMSG
           WHEN    "0006"
               MOVE "入外区分の入力に誤りがあります。" 
                                       TO  SPA-D01-ERRMSG
           WHEN    "0007"
               MOVE    "入外区分を入力してください。"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0008"
               MOVE    "チェック対象の保険の入力に誤りがあります。"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0009"
               MOVE    "チェック対象の保険を入力してください。"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0010"
               MOVE    "確認項目の入力に誤りがあります。"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0011"
               MOVE    "確認項目を入力してください。"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0012"
               MOVE    "院外処方の処理の入力に誤りがあります。"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0013"
               MOVE    "印刷順の入力に誤りがあります。"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0014"
               MOVE    "患者番号の入力に誤りがあります。"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0015"
               MOVE    "該当の患者が存在しません。"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0016"
               MOVE    "診療年月の診療情報が存在しません。"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0017"
               MOVE    "該当患者の入力が複数存在しています。"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0040"
               MOVE    "対象のジョブ管理がありません。"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0041"
               MOVE    "対象の処理はありません。"
                                       TO  SPA-D01-ERRMSG
           WHEN    "4001" THRU "4003"
           WHEN    "4101" THRU "4104"
               MOVE    "更新処理に失敗しました。"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0000"
               MOVE    "正常終了"
                                       TO  SPA-D01-ERRMSG
           END-EVALUATE
      *
           .
      *
       200-MAIN-EXT.
           EXIT.
      *
