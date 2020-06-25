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
       PROGRAM-ID.                     ORCGSERRMSG.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 収納
      *  コンポーネント名  : 収納サブ（エラーメッセージ返却）
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
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSLOCKAREA.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "入金取消が可能な請求ではありません。"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "請求取消が可能な請求ではありません。"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "返金が可能な請求ではありません。"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "入金が可能な請求ではありません。"
                                       TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "履歴修正はできません。"
                                       TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "Ｊデビットでの入金はできません。"
                                       TO  SPA-ERRMSG
           WHEN    "0009" 
               MOVE    "診療月以前の請求情報はありません。"
                                       TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE    "請求取消済みの為、再発行はできません。"
                                       TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE    "前月の請求情報はありません。"
                                       TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE    "次月の請求情報はありません。"
                                       TO  SPA-ERRMSG
           WHEN    "0020"
               MOVE    "対象の診療年月に請求情報はありません。"
                                       TO  SPA-ERRMSG
           WHEN    "0030"
               MOVE    "診療科の入力に誤りがあります。"
                                               TO  SPA-ERRMSG
           WHEN    "0031"
               MOVE    "該当の患者が存在しません。"
                                               TO  SPA-ERRMSG
           WHEN    "0033"
               MOVE    "診療年月の入力に誤りがあります。"
                                               TO  SPA-ERRMSG
           WHEN    "0034"
               MOVE    "入外区分の入力に誤りがあります。"
                                               TO  SPA-ERRMSG
           WHEN    "0050"
               MOVE "患者番号・診療年月が未入力です。" 
                                               TO  SPA-ERRMSG
      *
           WHEN    "0060"
               MOVE "該当する外来請求が見つかりません。" 
                                               TO  SPA-ERRMSG
           WHEN    "0061"
               MOVE "請求取消済みの為、再計算はできません。"
                                               TO  SPA-ERRMSG
           WHEN    "0062"
               MOVE    SPACE                   TO  SPA-ERRMSG
               STRING  "再計算できません。"    DELIMITED BY SIZE
                       "診療行為業務で訂正を行ってください。"
                                               DELIMITED BY SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0063"
               MOVE "受診履歴が見つかりません。"
                                               TO  SPA-ERRMSG
           WHEN    "0064"
               STRING  "一括再計算できません。"
                                               DELIMITED BY SIZE
                       "診療行為業務で訂正を行ってください。"
                                               DELIMITED BY SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0065"
               MOVE    "選択番号を入力してください。"
                                       TO  SPA-ERRMSG
           WHEN    "0071"
               MOVE "患者番号が未入力です。" 
                                               TO  SPA-ERRMSG
           WHEN    "0072"
               MOVE "患者番号の入力に誤りがあります。" 
                                               TO  SPA-ERRMSG
           WHEN    "0073"
               MOVE "基準日の入力に誤りがあります。" 
                                               TO  SPA-ERRMSG
           WHEN    "0074"
               MOVE "診療日の期間指定開始日の入力に誤りがあります。" 
                                               TO  SPA-ERRMSG
           WHEN    "0075"
               MOVE "診療日の期間指定終了日の入力に誤りがあります。" 
                                               TO  SPA-ERRMSG
           WHEN    "0076"
               MOVE "返却内容指示の入力に誤りがあります。" 
                                               TO  SPA-ERRMSG
           WHEN    "0077"
               MOVE "並べ替え項目の入力に誤りがあります。" 
                                               TO  SPA-ERRMSG
           WHEN    "0078"
               MOVE "並び順の入力に誤りがあります。" 
                                               TO  SPA-ERRMSG
           WHEN    "0079"
               MOVE    "開始件数の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0080"
               MOVE    "終了件数の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0081"
               MOVE    "開始件数が検索結果の総件数を超えています。"
                                       TO  SPA-ERRMSG
           WHEN    "0082"
               MOVE    "開始件数＞終了件数となっています。"
                                       TO  SPA-ERRMSG
           WHEN    "0083"
               MOVE "診療月の期間指定開始月の入力に誤りがあります。" 
                                               TO  SPA-ERRMSG
           WHEN    "0084"
               MOVE "診療月の期間指定終了月の入力に誤りがあります。" 
                                               TO  SPA-ERRMSG
           WHEN    "0085"
               MOVE "請求情報が存在しません。" 
                                               TO  SPA-ERRMSG
           WHEN    "0086"
               MOVE    "入外区分を入力してください。"
                                               TO  SPA-ERRMSG
           WHEN    "0100"
               MOVE "伝票番号を入力してください。" 
                                               TO  SPA-ERRMSG
           WHEN    "0101"
               MOVE "伝票番号の入力に誤りがあります。" 
                                               TO  SPA-ERRMSG
           WHEN    "0102"
               MOVE "入返金区分を入力してください。" 
                                               TO  SPA-ERRMSG
           WHEN    "0103"
               MOVE "入返金区分の入力に誤りがあります。" 
                                               TO  SPA-ERRMSG
           WHEN    "0104"
               MOVE "過入金許可フラグの入力に誤りがあります。" 
                                               TO  SPA-ERRMSG
           WHEN    "0105"
               MOVE "処理時刻の入力に誤りがあります。" 
                                               TO  SPA-ERRMSG
           WHEN    "0106"
               MOVE    "返金額を入力してください。"
                                               TO  SPA-ERRMSG
           WHEN    "0107"
               MOVE "過入金になります。入金できません。" 
                                               TO  SPA-ERRMSG
           WHEN    "0108"
               MOVE "過入金のない請求に対して返金はできません。"
                                               TO  SPA-ERRMSG
           WHEN    "0109"
               MOVE "返金額の入力に誤りがあります。"
                                               TO  SPA-ERRMSG
           WHEN    "0110"
               MOVE "過入金を超える返金はできません。"
                                               TO  SPA-ERRMSG
           WHEN    "0111"
               MOVE "同じ請求情報の複数入力はできません。"
                                               TO  SPA-ERRMSG
           WHEN    "0112"
               MOVE "入返金情報の入力に誤りがあります。" 
                                               TO  SPA-ERRMSG
           WHEN    "0113"
               MOVE "診療日の期間指定開始日を入力してください。" 
                                               TO  SPA-ERRMSG
           WHEN    "0114"
               MOVE "診療月の期間指定開始月を入力してください。" 
                                               TO  SPA-ERRMSG
           WHEN    "0115"
               MOVE    "明細番号を入力してください。"
                                       TO  SPA-ERRMSG
           WHEN    "0116"
               MOVE    "明細情報が存在しません。"
                                       TO  SPA-ERRMSG
           WHEN    "0117"
               MOVE    "明細数が上限に達しているため処理できません。"
                                       TO  SPA-ERRMSG
           WHEN    "0118"
               MOVE    "外来分の請求取消はできません。"
                                       TO  SPA-ERRMSG
           WHEN    "0119"
               MOVE    "既に請求取消済みです。"
                                       TO  SPA-ERRMSG
           WHEN    "0120"
               MOVE    "診療年月を入力してください。"
                                       TO  SPA-ERRMSG
           WHEN    "0121"
               MOVE    "請求書兼領収書印刷区分の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0122"
               MOVE    "診療費明細書印刷区分の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0123"
               MOVE    "帳票の印刷指示がありません。"
                                       TO  SPA-ERRMSG
           WHEN    "0125"
               MOVE "期間指定開始日の入力に誤りがあります。" 
                                               TO  SPA-ERRMSG
           WHEN    "0126"
               MOVE "期間指定終了日の入力に誤りがあります。" 
                                               TO  SPA-ERRMSG
           WHEN    "0127"
               MOVE "期間指定開始日＞期間指定終了日となっています。" 
                                               TO  SPA-ERRMSG
           WHEN    "0128"
               MOVE "印刷対象の請求情報が存在しません。"
                                               TO  SPA-ERRMSG
           WHEN    "0129"
               MOVE "外来請求書兼領収書発行方法の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0130"
               MOVE    "診療年月を入力してください。"
                                               TO  SPA-ERRMSG
           WHEN    "0131"
               MOVE    "印刷モードの入力に誤りがあります。"
                                               TO  SPA-ERRMSG
           WHEN    "0132"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "印刷モードを有効にする場合、"
                                                       DELIMITED BY SIZE
                       "ミドルウェアを更新してください。"
                                                       DELIMITED BY SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0203"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "電子カルテＵＩＤが未入力です。"
                                                       DELIMITED BY SIZE
                       "初回接続からやり直してください。"
                                                       DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0205"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "オルカＵＩＤが未入力です。"    DELIMITED BY SIZE
                       "初回接続からやり直してください。"
                                                       DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0206"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING "接続状態の取得に失敗しました。" DELIMITED BY SIZE
                      "初回接続から行ってください。"   DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0207"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING "接続状態の取得に失敗しました。" DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0208"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "電子カルテＵＩＤが未入力です。"
                                                       DELIMITED BY SIZE
                       "初回接続からやり直してください。"
                                                       DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0209"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "初回接続時と患者番号が異なります。"
                                                       DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0210"
               MOVE    "初回接続が完了していません。"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "該当する請求情報は存在しませんでした。"
                                       TO  SPA-ERRMSG
           WHEN    "1038"
               MOVE  "他の端末より同じカルテＵＩＤでの接続があります。"
                                       TO  SPA-ERRMSG
           WHEN    "3004"
               MOVE    "入金方法を選択して下さい。"
                                       TO  SPA-ERRMSG
           WHEN    "3005"
               MOVE    "明細番号の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "3006"
               MOVE    "明細を選択してください。"
                                       TO  SPA-ERRMSG
           WHEN    "3007"
               MOVE    "入金額を入力してください。"
                                       TO  SPA-ERRMSG
           WHEN    "3008"
               MOVE    "入金合計額が０円を下回ります。"
                                       TO  SPA-ERRMSG
           WHEN    "3009"
               MOVE    "入金のある明細を選択してください。"
                                       TO  SPA-ERRMSG
           WHEN    "3010"
               MOVE    "処理日の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "3011"
               MOVE    "入金方法の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "3012"
               MOVE    "処理日を入力してください。"
                                       TO  SPA-ERRMSG
           WHEN    "3013"
               MOVE    "マイナスの入金額を入力してください。"
                                       TO  SPA-ERRMSG
           WHEN    "3014"
               MOVE    "入金額が０円を下回ります。"
                                       TO  SPA-ERRMSG
           WHEN    "3016"
               MOVE    "請求金額が０円を下回ります。"
                                       TO  SPA-ERRMSG
           WHEN    "3017"
               MOVE    "調整金１の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "3018"
               MOVE    "調整金２の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "3019"
               MOVE    "入金額の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "3020"
               MOVE    "調整金の入力が可能な請求ではありません。"
                                       TO  SPA-ERRMSG
           WHEN    "3021"
               MOVE    "調整金の入力が可能な請求ではありません。"
                                       TO  SPA-ERRMSG
           WHEN    "3022"
               MOVE    "マイナスの入金額を入力してください。"
                                       TO  SPA-ERRMSG
           WHEN    "3023"
               MOVE    "今回請求金額が上限を超えます。"
                                       TO  SPA-ERRMSG
           WHEN    "3024"
               MOVE    "請求金額が上限を超えます。"
                                       TO  SPA-ERRMSG
           WHEN    "3025"
               MOVE    "入金額が上限を超えます。"
                                       TO  SPA-ERRMSG
           WHEN    "1001"  THRU    "1007"
           WHEN    "4000"  THRU    "4011"
           WHEN    "4900"  THRU    "4911"
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
           WHEN    "0092"
               MOVE    "リクエスト種別に誤りがあります。"
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
           WHEN    "9999"
               MOVE    "他端末で使用中です。"
                                               TO  SPA-ERRMSG
               MOVE    SLOCK-MSG               TO  SPA-ERRMSG(21:)
           WHEN    "0000"
               MOVE    "正常終了"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           EXIT PROGRAM
           .
      *
