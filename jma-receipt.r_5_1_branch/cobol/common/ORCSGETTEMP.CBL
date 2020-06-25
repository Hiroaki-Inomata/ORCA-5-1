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
       IDENTIFICATION                      DIVISION.
       PROGRAM-ID.                         ORCSGETTEMP.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 一時ファイル名編集
      *  返却エラーコード  : 00:正常　99:エラー
      *  管理者            : 
      *  作成日付    作業者        記述
      *  14/06/20    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                         DIVISION.
       CONFIGURATION                       SECTION.
       INPUT-OUTPUT                        SECTION.
       DATA                                DIVISION.
       FILE                                SECTION.
       WORKING-STORAGE                     SECTION.
      *
           COPY    "ENUM-VALUE".
      *
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-ORCA-TEMPDIR            PIC X(1024).
           03  WRK-BASENAME                PIC X(1024).
           03  WRK-TEMPNAME                PIC X(1024).
           03  WRK-FULLNAME                PIC X(1024).
           03  WRK-POINTER                 PIC 9(05).
           03  WRK-POINTER-PATH            PIC 9(05).
           03  WRK-SEQ-X.
               05  WRK-SEQ                 PIC 9(03).
      *
       01  CONST-AREA.
           03  CONST-PATH-SIZE             PIC 9(05)    VALUE 1024.
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
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                             SECTION.
      *
           COPY    "CPORCSGETTEMP.INC".
      *
       PROCEDURE                           DIVISION    USING
           SGETTEMP-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                        SECTION.
      *
           INITIALIZE                      IDX-AREA
                                           WRK-AREA
      *
           MOVE      SPACE             TO  MCP-TEMPDIR
                                           MCP-MIDDLEWARE-NAME
                                           WRK-ORCA-TEMPDIR
                                           SGETTEMP-FULLNAME-AREA
      *
           ACCEPT   MCP-TEMPDIR  FROM ENVIRONMENT  "MCP_TEMPDIR"
      *
           ACCEPT   MCP-MIDDLEWARE-NAME
                                 FROM ENVIRONMENT  "MCP_MIDDLEWARE_NAME"
      *
      *    ミドルウェア
           EVALUATE    MCP-MIDDLEWARE-NAME
           WHEN    "panda"
               MOVE    SPA-PANDA       TO  SGETTEMP-MW
           WHEN    "ginbee"
               MOVE    SPA-GINBEE      TO  SGETTEMP-MW
      *    取得できない場合はpandaとみなす
           WHEN    OTHER
               MOVE    SPA-PANDA       TO  SGETTEMP-MW
           END-EVALUATE
      *
           MOVE   1                    TO  WRK-POINTER-PATH
      *
           STRING FUNCTION TRIM(MCP-TEMPDIR)   DELIMITED BY SIZE
           INTO   WRK-FULLNAME
           WITH   POINTER  WRK-POINTER-PATH
           END-STRING
      *
           MOVE   WRK-POINTER-PATH TO  WRK-POINTER
           MOVE   WRK-FULLNAME     TO  SGETTEMP-TEMPDIR
      *
           STRING "/"                  DELIMITED BY SIZE
           INTO   SGETTEMP-TEMPDIR
           WITH   POINTER  WRK-POINTER
           END-STRING
      *
           MOVE   WRK-POINTER      TO  SGETTEMP-ST
      *
           IF  ( SGETTEMP-BASENAME     NOT =   SPACE )
      *
               MOVE    SGETTEMP-BASENAME
                                   TO  WRK-BASENAME
               PERFORM 100-GET-BASENAME-SEC
      *
               MOVE   WRK-POINTER-PATH TO  WRK-POINTER
               MOVE   WRK-FULLNAME     TO  SGETTEMP-FULLNAME
               STRING "/"
                      FUNCTION TRIM(WRK-BASENAME)   DELIMITED BY SIZE
               INTO   SGETTEMP-FULLNAME
               WITH   POINTER  WRK-POINTER
               END-STRING
           END-IF
      *
           PERFORM VARYING WRK-SEQ FROM    1   BY  1
                   UNTIL ( WRK-SEQ > 10 )
      *
               IF  ( SGETTEMP-BASENAMES (WRK-SEQ)  NOT =   SPACE )
      *
                   MOVE    SGETTEMP-BASENAMES (WRK-SEQ)
                                   TO  WRK-BASENAME
                   PERFORM 100-GET-BASENAME-SEC
      *
                   MOVE   WRK-POINTER-PATH TO  WRK-POINTER
                   MOVE   WRK-FULLNAME TO  SGETTEMP-FULLNAMES (WRK-SEQ)
                   STRING "/"
                          FUNCTION TRIM(WRK-BASENAME)
                                                       DELIMITED BY SIZE
                   INTO   SGETTEMP-FULLNAMES (WRK-SEQ)
                   WITH   POINTER  WRK-POINTER
                   END-STRING
               END-IF
      *
           END-PERFORM
      *
           EXIT PROGRAM
      *
           .
       000-PROC-EXT.
           EXIT.
           .
      *****************************************************************
      *    ファイル名取得処理
      *****************************************************************
       100-GET-BASENAME-SEC            SECTION.
      *
           MOVE    WRK-BASENAME    TO  WRK-TEMPNAME
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >=  CONST-PATH-SIZE )
                    OR   ( WRK-BASENAME (IDX0 : )  =   SPACE )
      *
              IF    ( WRK-BASENAME (IDX0 : 1 ) =   "/" )
                   MOVE    WRK-BASENAME (IDX0 + 1 : )
                                   TO      WRK-TEMPNAME
              END-IF
      *
           END-PERFORM
      *
           MOVE    WRK-TEMPNAME    TO  WRK-BASENAME
      *
           .
       100-GET-BASENAME-EXT.
           EXIT.
