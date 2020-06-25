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
       PROGRAM-ID.                     ORCGI2ERRMSG.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院患者照会
      *  コンポーネント名  : 入院患者照会サブ（エラーメッセージ返却）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/09/19    NACL−太田    新規作成
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
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "該当の患者が存在しません。"
                                                   TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "患者氏名は全角で入力して下さい。"
                                                   TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "開始年齢の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "住所は全角で入力して下さい。"
                                                   TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "開始入院日の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "終了入院日の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "診療科の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "病棟番号の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE    "状態の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE    "範囲外の番号が入力されています"
                                                   TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE    "開始退院日の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE    "終了退院日の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0013"
               MOVE    "退院事由の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0014"
               MOVE    "死亡区分の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0015"
               MOVE    "終了年齢の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0016"
               MOVE    "開始生年月日の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0017"
               MOVE    "終了生年月日の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0018"
               MOVE    "テスト患者区分の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0019"
               MOVE    "並べ替え項目１の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0020"
               MOVE    "並び順１の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0021"
               MOVE    "並べ替え項目２の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0022"
               MOVE    "並び順２の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0023"
               MOVE    "並べ替え項目３の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0024"
               MOVE    "並び順３の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0025"
               MOVE    "基準日の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0026"
               MOVE    "請求状態の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0027"
               MOVE    "定期請求年月の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0028"
               MOVE    "定期請求期別の入力に誤りがあります。"
                                                   TO  SPA-ERRMSG
           WHEN    "0100"
               MOVE    "検索条件が入力されていません。"
                                                   TO  SPA-ERRMSG
           WHEN    "0101"
               MOVE    "開始入院日＞終了入院日となっています。" 
                                                   TO  SPA-ERRMSG
           WHEN    "0102"
               MOVE    "開始退院日＞終了退院日となっています。" 
                                                   TO  SPA-ERRMSG
           WHEN    "0103"
               MOVE    "開始生年月日＞終了生年月日となっています。" 
                                                   TO  SPA-ERRMSG
           WHEN    "0104"
               MOVE    "対象の患者が選択されていません。" 
                                                   TO  SPA-ERRMSG
           WHEN    "0105"
               MOVE    "検索処理に失敗しました。"
                                       TO  SPA-ERRMSG
           WHEN    "0106"
               MOVE    "検索条件に該当する患者は存在しませんでした。"
                                       TO  SPA-ERRMSG
           WHEN    "0107"
               MOVE    "開始件数の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0108"
               MOVE    "終了件数の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0109"
               MOVE    "開始件数が検索結果の総件数を超えています。"
                                       TO  SPA-ERRMSG
           WHEN    "0110"
               MOVE    "開始件数＞終了件数となっています。"
                                       TO  SPA-ERRMSG
           WHEN    "0201"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "電子カルテＵＩＤが未入力です。"
                                                       DELIMITED BY SIZE
                       "初回接続からやり直してください。"
                                                       DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0202"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "オルカＵＩＤが未入力です。"    DELIMITED BY SIZE
                       "初回接続からやり直してください。"
                                                       DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0203"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING "接続状態の取得に失敗しました。" DELIMITED BY SIZE
                      "初回接続から行ってください。"   DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0204"
               MOVE  "他の端末より同じカルテＵＩＤでの接続があります。"
                                       TO  SPA-ERRMSG
           WHEN    "0205"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING "接続状態の取得に失敗しました。" DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "4001"      THRU    "4002"
               MOVE    "更新処理に失敗しました。"
                                       TO  SPA-ERRMSG
           WHEN    "8900"
               MOVE    "システム項目が設定できません。"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8901"
               MOVE    "職員情報が取得できません。"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8902"
               MOVE    "医療機関情報が取得できません。"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8903"
               MOVE    "システム日付が取得できません。"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8905"
               MOVE    "患者番号構成情報が取得できません。"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8915"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING "グループ医療機関が不整合です。" DELIMITED BY SIZE
                      "処理を終了して下さい。"         DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "0091"
               MOVE    "リクエスト番号に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0097"
               MOVE    "送信内容に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0098"
               MOVE    "送信内容の読込ができませんでした。"
                                       TO  SPA-ERRMSG
           WHEN    "0099"
               MOVE    "ユーザＩＤが未登録です。"
                                       TO  SPA-ERRMSG
           WHEN    "0000"
               MOVE    "正常終了"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           EXIT PROGRAM
           .
      *
