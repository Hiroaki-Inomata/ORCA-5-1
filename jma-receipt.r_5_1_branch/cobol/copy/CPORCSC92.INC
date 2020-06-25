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
      *----------------------------------------------------------*
      *    診療コードチェック　   COPY (CPORCSC92.INC)
      *                              (ORCSC92 で使用）
      *
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCSC92AREA.
      *
      *    1:追加分（追加ＯＫ時、'２'へ変更）３：チェックのみ
           03  LNK-SC92-KBN            PIC X(01).
      *    １：入力値あり
           03  LNK-SC92-INFLG          PIC X(01).
      *    訂正フラグ（１：訂正）
           03  LNK-SC92-SYORIFLG       PIC 9(01).
      *    1:注加算チェックなし（画像診断の乳幼児加算等）
           03  LNK-SC92-CHUKSN         PIC X(01).
      *    空白:警告エラーなし（自動発生時に対象とする）
      *    1:警告エラーあり（入力時）
           03  LNK-SC92-KEIFLG         PIC X(01).
      *
      *    診療コード
           03  LNK-SC92-SRYCD          PIC X(09).
      *    対象・追加ＩＤＸ
           03  LNK-SC92-GMN-IDX        PIC 9(04).
      *    処理ＰＧ名
           03  LNK-SC92-PG             PIC X(04).
      *
           03  LNK-SC92-AREA.
      *        年齢 
               05  LNK-SC92-NENREI.
                   07  LNK-SC92-NENREI-YY     PIC 9(03).
                   07  LNK-SC92-NENREI-MM     PIC 9(02).
                   07  LNK-SC92-NENREI-DD     PIC 9(02).
      *        １５歳未満再診科算定有り（初診・再診のみ使用）
               05  LNK-SC92-15MIMAN-ARI       PIC  9(01).
      *        老人区分
               05  LNK-SC92-ROUJIN            PIC 9(01).
      *        最終受診科
               05  LNK-SC92-LSTSRYKA          PIC  X(02).
      *        最終受診科
               05  LNK-SC92-LSTSRYKAMEI       PIC  X(20).
      *        当月入院有無
               05  LNK-SC92-NYUINFLG          PIC  9(01).
      *    訂正時の診療コード
           03  LNK-SC92-TEISEI-AREA.
               05  LNK-SC92-TEISEI-OCC      OCCURS   400.
                   07  LNK-SC92-TEI-SRYCD   PIC X(09).
                   07  LNK-SC92-TEI-KAISU   PIC 9(03).
      *
           03  LNK-SC92-ERRAREA.
      *        エラーコード（入外適用区分）
               05  LNK-SC92-ERRCD1         PIC X(04).
      *        エラーコード（老人適用区分）
               05  LNK-SC92-ERRCD2         PIC X(04).
      *        エラーコード（算定回数）
               05  LNK-SC92-ERRCD3         PIC X(04).
      *        エラーコード（施設基準適合分）
               05  LNK-SC92-ERRCD4         PIC X(04).
      *        エラーコード（対象年齢）
               05  LNK-SC92-ERRCD5         PIC X(04).
      *        エラーコード（病床数）
               05  LNK-SC92-ERRCD6         PIC X(04).
      *        エラーコード（併用算定）
               05  LNK-SC92-ERRCD7         PIC X(04).
      *        エラーコード
               05  LNK-SC92-ERRCD8         PIC X(04).
      *        エラーコード
               05  LNK-SC92-ERRCD9         PIC X(04).
      *        エラーコード
               05  LNK-SC92-ERRCD10        PIC X(04).
           03  LNK-SC92-ERRAREA-R      REDEFINES   LNK-SC92-ERRAREA.
               05  LNK-SC92-ERRCD          PIC X(04)
                                           OCCURS  10.
      *    エラーメッセージ（併用算定）
           03  LNK-SC92-ERRMSG         PIC X(100).
      *    併用算定警告
           03  LNK-SC92-KEIARI         PIC X(01).
      *    初診料併用算定エラー
           03  LNK-SC92-SYOKBN         PIC X(01).
      *
      *    入力値（ORCSC97）（診療行為画面入力時のみ）
           03  LNK-SC92-INPUT-G        PIC X(100).
           03  LNK-SC92-TIMEFLG        PIC X(01).
      *
      *外来まとめ算定履歴領域
           03  LNK-SC92-KT01-AREA.
               05  LNK-SC92-KT01FLG         PIC X(01).
               05  LNK-SC92-KT01-OCC        OCCURS   100.
                   07  LNK-SC92-KT01-SRYCD  PIC X(09).
                   07  LNK-SC92-KT01-DAY-G.
                       09  LNK-SC92-KT01-DAY     PIC 9(03)
                                                  OCCURS   31.
      *
