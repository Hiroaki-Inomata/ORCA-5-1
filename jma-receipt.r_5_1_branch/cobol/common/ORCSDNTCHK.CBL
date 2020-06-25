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
       PROGRAM-ID.                     ORCSDNTCHK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 同日入退院チェック
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/02/16    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTNYUINRRK          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-YMD                 PIC X(10).
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-KEISANYMD           PIC X(08).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *    COPY    "ORCA-DBPATH".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                         SECTION.
      *
      *    入院チェックサブ
           COPY    "CPORCSDNTCHK.INC".
      *
       01  LNKNRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-// 
                                       BY         //LNKNRRK-//.
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING   SDNTCHK-AREA
                                               LNKNRRK-REC.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *
           INITIALIZE                  SDNTCHK-OT
      *
           IF    ( SDNTCHK-KJNYMD      =   SPACE )
               MOVE    LNKNRRK-TAIINYMD    TO  SDNTCHK-KJNYMD
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    SPACE               TO  SDNTCHK-DJKBN
           MOVE    ZERO                TO  SDNTCHK-STYMD
      *
           EVALUATE    LNKNRRK-DOUJITSUKBN
           WHEN    "1"
      *
               MOVE    LNKNRRK-DOUJITSUKBN TO  SDNTCHK-DJKBN
      *
               IF    ( LNKNRRK-NYUINYMD    <  LNKNRRK-TAIINYMD )
                AND  ( LNKNRRK-NYUINYMD    <  SDNTCHK-KJNYMD )
                   MOVE   LNKNRRK-NYUINYMD TO  WRK-YMD
                   MOVE    "1"             TO  WRK-ZOGENPTN
                   MOVE    1               TO  WRK-ZOGEN
                   PERFORM 800-CALENDAR-SEC
                   MOVE    WRK-KEISANYMD   TO  SDNTCHK-STYMD
               END-IF
           WHEN    "2"
               MOVE    LNKNRRK-DOUJITSUKBN TO  SDNTCHK-DJKBN
               MOVE    LNKNRRK-NYUINYMD    TO  SDNTCHK-STYMD
           WHEN    OTHER
               MOVE    LNKNRRK-NYUINYMD    TO  SDNTCHK-STYMD
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
      *    増減パターン 1:日／2:月／3:年
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
