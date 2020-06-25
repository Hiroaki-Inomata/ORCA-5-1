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
       PROGRAM-ID.             ORCSTEMPDIR.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 一時ディレクトリ作成・削除
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/05/17    NACL 太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
      *
       01  WRK-AREA.
           03  WRK-TEMPDIR             PIC X(512).
           03  WRK-TEMPDIR-ROOT        PIC X(512).
           03  WRK-TERM                PIC X(64).
           03  WRK-RC                  PIC S9(8).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *ＵＩＤ取得用エリア
       01 UIDIO-AREA.
           03  C-RET   PIC X(2).
           03  C-UID   PIC X(36).
       01  ST          PIC 9(2).
      *
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
       LINKAGE                     SECTION.
      *
           COPY "CPORCSTEMPDIR.INC".
      *
      **************************************************************************
       PROCEDURE           DIVISION
               USING
               STEMPDIR-AREA.
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
           EXIT PROGRAM.
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE              WRK-AREA
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
           ACCEPT  STEMPDIR-TEMPDIR
                                   FROM    ENVIRONMENT  "MCP_TEMPDIR"
           ON EXCEPTION
              MOVE    SPACE        TO  STEMPDIR-TEMPDIR
           END-ACCEPT
      *
           EVALUATE    STEMPDIR-MODE
           WHEN    "DEL"
               MOVE    SPACE           TO  SHELLTBL
               INITIALIZE                  SHELLTBL
               MOVE    "delete_tempdir.sh"
                                       TO  SHELLTBL-NAME
               MOVE    SHELLTBL        TO  MCPDATA-REC
               MOVE    "SHELL"         TO  MCP-FUNC
               MOVE    "shell"         TO  MCP-TABLE
               MOVE    "allways"       TO  MCP-PATHNAME
               CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
           WHEN    "UNSET"
               CALL    "CBL_DELETE_DIR"   USING
                                          STEMPDIR-TEMPDIR
               SET ENVIRONMENT "MCP_TEMPDIR" TO LOW-VALUE
           WHEN    OTHER
      *
               IF    ( STEMPDIR-TEMPDIR    =   SPACE )
      *
                   ACCEPT  WRK-TEMPDIR-ROOT
                               FROM    ENVIRONMENT  "MCP_TEMPDIR_ROOT"
                   ON EXCEPTION
                       MOVE    SPACE   TO  WRK-TEMPDIR-ROOT
                   END-ACCEPT
                   IF    ( WRK-TEMPDIR-ROOT    =   SPACE )
                       MOVE    "/tmp/jma-tempdir"
                                       TO  WRK-TEMPDIR-ROOT
                   END-IF
      *
                   ACCEPT  WRK-TERM
                                   FROM    ENVIRONMENT  "MCP_TERM"
                   ON EXCEPTION
                       MOVE    SPACE   TO  WRK-TERM
                   END-ACCEPT
                   IF    ( WRK-TERM    =   SPACE )
                       INITIALIZE              UIDIO-AREA
                       CALL    "orcuidset" USING ST UIDIO-AREA
                       MOVE    C-UID   TO  WRK-TERM
                   END-IF
      *
                   STRING  WRK-TEMPDIR-ROOT    DELIMITED BY SPACE
                           "/"                 DELIMITED BY SIZE
                           WRK-TERM            DELIMITED BY SPACE
                   INTO    STEMPDIR-TEMPDIR
                   END-STRING
      *
      *
                   CALL "CBL_CREATE_DIR"   USING
                                           STEMPDIR-TEMPDIR
                   RETURNING  WRK-RC
                   END-CALL
      *            ディレクトリの作成に失敗した場合は例外処理を行う。
                   IF    ( WRK-RC      NOT =   ZERO )
                       CALL    "cobabort"  USING
                                           FUNCTION CONCATENATE (
                       "ORCSTEMPDIR couldn't create directory " ;
                       FUNCTION TRIM(STEMPDIR-TEMPDIR);
                       LOW-VALUE
                       )
                   END-IF
      *
                   SET ENVIRONMENT "MCP_TEMPDIR" TO STEMPDIR-TEMPDIR
               END-IF
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
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
