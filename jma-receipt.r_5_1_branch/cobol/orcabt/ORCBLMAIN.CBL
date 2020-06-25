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
       PROGRAM-ID.                     ORCBLMAIN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次統計
      *  コンポーネント名  : 処理プログラム呼出
      *  管理者            :
      *  作成日付    作業者        記述
      *  03/04/08    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                         DIVISION.
       CONFIGURATION                       SECTION.
       INPUT-OUTPUT                        SECTION.
       FILE-CONTROL.
      *
       DATA                                DIVISION.
       FILE                                SECTION.
      *
       WORKING-STORAGE                     SECTION.
grpsys*
grpsys COPY    "COMMON-SPA".
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-DUMMY                       PIC X(02).
      *
      *    フラグ
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-OUT1                        PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX1                            PIC 9(05). 
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME.
               05  SYS-THH                     PIC 9(02).
               05  SYS-TMM                     PIC 9(02).
               05  SYS-TSS                     PIC 9(02).
               05  SYS-TDD                     PIC 9(02).
           03  SYS-YMD.
               05  SYS-YY                      PIC 9(04).
               05  SYS-MM                      PIC 9(02).
               05  SYS-DD                      PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PROGRAM-ID                  PIC X(50).
           03  WRK-PARAMETER                   PIC X(2000).
           03  WRK-POINT                       PIC 9(05).
      *
           COPY    "MCPAREA".
      *
      *    COPY    "COMMON-SPA".
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
       01  COMMAND-PARAM       PIC X(400).
      *
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-TERM-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       INDEX-AREA
                                       CNT-AREA
                                       WRK-AREA
                                       SYS-AREA
      *
      *
           MOVE    1               TO  WRK-POINT
           UNSTRING        COMMAND-PARAM   DELIMITED   BY  ","
           INTO            WRK-PROGRAM-ID
           WITH POINTER    WRK-POINT
           END-UNSTRING
      *
           MOVE    COMMAND-PARAM (WRK-POINT : )
                                   TO  WRK-PARAMETER
      *
      *
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    処理プログラム呼出
           CALL    WRK-PROGRAM-ID  USING   WRK-PARAMETER
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
      *
           .
       300-TERM-EXT.
           EXIT.
      *
