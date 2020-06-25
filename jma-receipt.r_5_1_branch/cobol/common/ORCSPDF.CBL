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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSPDF.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 印刷
      *  コンポーネント名  : PDFファイル作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/11/29    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-太田    14/08/06  4.8クライアント印刷対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  ASGN-PRT
                                   ORGANIZATION    LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PRT.
      *
           SELECT  PDF-FILE        ASSIGN  ASGN-PDF
                                   ORGANIZATION    LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PDF.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
       FD  PRT-FILE.
       01  PRT-REC.
           03  PRT-PRTID           PIC X(20).
           03  PRT-OFFSET-X        PIC X(05).
           03  PRT-OFFSET-Y        PIC X(05).
           03  PRT-SITEKBN         PIC X(01).
           03  PRT-PDF             PIC X(1024).
           03  PRT-PRTDATA         PIC X(20000).
      *
       FD  PDF-FILE.
       01  PDF-REC                 PIC X(36).
      *
       WORKING-STORAGE             SECTION.
      *
       01  ASGN-PRT                PIC X(1024).
       01  ASGN-PDF                PIC X(1024).
      *
       01  STS-AREA.
           03  STS-PRT             PIC X(02).
           03  STS-PDF             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
      *
       01  WRK-AREA.
           03  WRK-BASE-PRT        PIC X(1024).
           03  WRK-BASE-PDF        PIC X(1024).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1044.INC".
      *
      *    シェル
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
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
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "CPORCSPRT.INC".
           COPY    "COMMON-SPA".
      *
      **************************************************************************
       PROCEDURE           DIVISION USING
               ORCSPRTAREA
               SPA-AREA.
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
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  WRK-AREA
      *
           INITIALIZE              UIDIO-AREA
           CALL    "orcuidset"     USING
                                   UIDIO-ST
                                   UIDIO-AREA
           MOVE    SPACE           TO  ASGN-PRT
                                       ASGN-PDF
      *
           STRING  SPRT-TBL-KEY    DELIMITED BY SPACE
                   "-"             DELIMITED BY SIZE
                   FUNCTION SUBSTITUTE(SPRT-PRTID, ".red", " ")
                                   DELIMITED BY SPACE
                   "-"             DELIMITED BY SIZE
                   UIDIO-UID       DELIMITED BY SPACE
           INTO    ASGN-PRT
           END-STRING
      *
           STRING  "pdf_uuid-"     DELIMITED BY SPACE
                   UIDIO-UID       DELIMITED BY SPACE
           INTO    ASGN-PDF
           END-STRING
      *
           MOVE    ASGN-PRT        TO  WRK-BASE-PRT
           MOVE    ASGN-PDF        TO  WRK-BASE-PDF
      *
           INITIALIZE              SGETTEMP-AREA
           MOVE    ASGN-PRT        TO  SGETTEMP-BASENAMES (1)
           MOVE    ASGN-PDF        TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  ASGN-PRT
           MOVE    SGETTEMP-FULLNAMES (2)
                                   TO  ASGN-PDF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 2001-OFFSET-SEC
      *
           OPEN OUTPUT PRT-FILE
      *
           INITIALIZE                      PRT-REC
           MOVE    SPRT-PRTID          TO  PRT-PRTID
           MOVE    SYS-1044-OFFSET-X   TO  PRT-OFFSET-X
           MOVE    SYS-1044-OFFSET-Y   TO  PRT-OFFSET-Y
           MOVE    SPRT-SITEKBN        TO  PRT-SITEKBN
           MOVE    WRK-BASE-PDF        TO  PRT-PDF
           MOVE    SPRT-PRTDATA        TO  PRT-PRTDATA
           WRITE   PRT-REC
           CLOSE   PRT-FILE
      *
           MOVE    SPACE               TO  SHELLTBL
           INITIALIZE                      SHELLTBL
           MOVE    "red2pdf.sh"        TO  SHELLTBL-NAME
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG1
           MOVE    WRK-BASE-PRT        TO  SHELLTBL-ARG2
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "EXCOMMAND"         TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "daily"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           DISPLAY "EXCOMMAND!!!"
      *
           OPEN INPUT PDF-FILE
           READ PDF-FILE
           END-READ
           IF    ( STS-PDF             =   ZERO )
               MOVE    PDF-REC         TO  SPRT-PDF
           END-IF
           CLOSE   PDF-FILE
      *
           CALL    "CBL_DELETE_FILE"   USING
                                       ASGN-PDF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    オフセット取得処理
      *****************************************************************
       2001-OFFSET-SEC                  SECTION.
      *
           INITIALIZE                  SYS-1044-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1044-HOSPNUM
           MOVE    "1044"          TO  SYS-1044-KANRICD
           MOVE    "99999999"      TO  SYS-1044-STYUKYMD
           MOVE    "00000000"      TO  SYS-1044-EDYUKYMD
           MOVE    SPRT-PRTID      TO  SYS-1044-FORMID
           MOVE    SPRT-PRTNM      TO  SYS-1044-PRTNM
           MOVE    "%"             TO  SYS-1044-OFFSET-X
           MOVE    SYS-1044-REC    TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY3-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1044-REC
      *
           .
       2001-OFFSET-EXT.
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
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY3-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FETCHはしない）
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
