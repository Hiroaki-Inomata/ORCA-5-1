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
       PROGRAM-ID.                     ORCSBLOBIMPORT.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : ＢＬＯＢインポート
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/04/28    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
           SELECT  ZIP-FILE            ASSIGN  ASGN-ZIP
                                       ORGANIZATION    LINE SEQUENTIAL
                                       FILE    STATUS  IS  STS-ZIP.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       FD  ZIP-FILE.
       01  ZIP-REC                     PIC X(36).
      *
      *
       WORKING-STORAGE                 SECTION.
      *
       01  ASGN-ZIP                    PIC X(1024).
      *
       01  STS-AREA.
           03  STS-ZIP                 PIC X(02).
      *
       01  WRK-AREA.
           03  WRK-BASE-ZIP            PIC X(1024).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSHELLTBL.INC".
      *
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
       01 UIDIO-AREA.
           03  UIDIO-RET   PIC X(2).
           03  UIDIO-UID   PIC X(36).
       01  UIDIO-ST        PIC 9(2).
      *
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                         SECTION.
           COPY    "CPORCSBLOBIMPORT.INC".
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       PROCEDURE                       DIVISION
           USING
           SBI-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT PROGRAM
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  WRK-AREA
                                       STS-AREA
      *
           MOVE    SPACE           TO  SBI-BLOB-ID
      *
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       UIDIO-ST
                                       UIDIO-AREA
           MOVE    SPACE           TO  WRK-BASE-ZIP
      *
           STRING  UIDIO-UID       DELIMITED BY SPACE
                   ".txt"          DELIMITED BY SIZE
           INTO    WRK-BASE-ZIP
           END-STRING
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    WRK-BASE-ZIP    TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  ASGN-ZIP
       *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    SPACE               TO  SHELLTBL
           INITIALIZE                      SHELLTBL
           MOVE    "import_blob.sh"    TO  SHELLTBL-NAME
           MOVE    SBI-FILENAME        TO  SHELLTBL-ARG1
           MOVE    WRK-BASE-ZIP        TO  SHELLTBL-ARG2
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "EXCOMMAND"         TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "daily"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           OPEN INPUT ZIP-FILE
           IF    ( STS-ZIP             =   ZERO )
               READ ZIP-FILE
               END-READ
               IF    ( STS-ZIP         =   ZERO )
                   MOVE    "1#"        TO  SBI-BLOB-ID
                   MOVE    ZIP-REC     TO  SBI-BLOB-ID(3:)
               END-IF
           END-IF
           CLOSE   ZIP-FILE
      *
           CALL    "CBL_DELETE_FILE"   USING
                                       ASGN-ZIP
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    終了　処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
