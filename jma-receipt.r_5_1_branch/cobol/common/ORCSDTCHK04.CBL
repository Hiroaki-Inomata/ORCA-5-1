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
       PROGRAM-ID.             ORCSDTCHK04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : 記号番号チェック
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/02/02    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/06/04  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MOJI                    PIC X(02).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-LEN-MAX               PIC 9(05)   VALUE   100.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSDTCHK04.INC".
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
       SDTCHK04AREA
       SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC.
      *
           PERFORM 200-MAIN-SEC.
      *
           PERFORM 300-END-SEC.
      *
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
                                       SDTCHK04-OT
           MOVE    ZERO            TO  SDTCHK04-KETA9
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  2
                   UNTIL ( IDX0   >=   CONST-LEN-MAX )
                    OR   ( SDTCHK04-KIGONUM (IDX0:)   =   SPACE )
      *
               EVALUATE    SDTCHK04-KIGONUM (IDX0:2)
               WHEN    "０"    THRU    "９"
                   MOVE    1       TO  SDTCHK04-SUJI
               WHEN    "ぁ"    THRU    "ヶ"
               WHEN    "亜"    THRU    "熙"
                   MOVE    1       TO  SDTCHK04-KANJI
               WHEN    "Ａ"    THRU     "Ｚ"
               WHEN    "ａ"    THRU     "ｚ"
                   MOVE    1       TO  SDTCHK04-ALPHABET
               WHEN    "−"
               WHEN    "―"
               WHEN    "‐"
                   MOVE    1       TO  SDTCHK04-HYPHEN
               WHEN    "ー"
                   MOVE    1       TO  SDTCHK04-TYOON
               WHEN    "（"    THRU     "】"
                   MOVE    1       TO  SDTCHK04-KAKKO
               WHEN    OTHER
                   MOVE    1       TO  SDTCHK04-ETC
                   IF    ( SDTCHK04-KIGONUM (IDX0:2)   =   "　" )
                       MOVE    1   TO  SDTCHK04-SPACE
                   END-IF
               END-EVALUATE
      *
               COMPUTE SDTCHK04-KETA9  =   SDTCHK04-KETA9  +   1
      *
           END-PERFORM
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
