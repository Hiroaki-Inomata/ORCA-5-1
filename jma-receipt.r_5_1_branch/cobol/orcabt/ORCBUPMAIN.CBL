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
       PROGRAM-ID.                     ORCBUPMAIN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ユーザプログラム
      *  コンポーネント名  : 処理プログラム呼出
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/10/16    NACL−太田    新規作成
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
      *
       01  WRK-AREA.
           03  WRK-POINTER                 PIC 9(5).
           03  WRK-PROGRAM-ID              PIC X(20).
           03  WRK-PARAMETER               PIC X(400).
      *
       COPY    "CPCOMMAND-PARAM.INC"       REPLACING
                                           //COMMAND-// BY //LNK-//.
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
       COPY    "CPCOMMAND-PARAM.INC".
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
           PERFORM 300-END-SEC
      *
           EXIT PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
                                       LNK-PARAM
      *
           MOVE        1           TO  WRK-POINTER
           UNSTRING    COMMAND-PARAM   DELIMITED   BY  ","
           INTO    WRK-PROGRAM-ID
           WITH    POINTER WRK-POINTER
           END-UNSTRING
      *
           IF    ( WRK-POINTER     <=  FUNCTION LENGTH(COMMAND-PARAM))
               MOVE    COMMAND-PARAM(WRK-POINTER:)
                                   TO  LNK-PARAM
           END-IF
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
           CALL    WRK-PROGRAM-ID  USING   LNK-PARAM
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *
