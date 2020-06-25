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
       PROGRAM-ID.                 ORCBCLPRT.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : クライアント印刷
      *  管理者            :
      *  作成日付    作業者        記述
      *  14/08/08    NACL-太田     新規作成
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
      *    PUSH用データ
           SELECT  PUSH-FILE       ASSIGN  PUSHPARA
                                   ORGANIZATION    IS  SEQUENTIAL 
                                   FILE    STATUS  IS  STS-PUSH.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    PUSH用データ
       FD  PUSH-FILE.
           COPY    "CPPUSHPRINT002.INC".
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPASGNPARA.INC"    REPLACING //ASGNPARA//
                                       BY        //PUSHPARA//.
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PUSH            PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-PRTKANRI        PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-TABLE           PIC X(64).
      *
           COPY    "COMMON-SPA".
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "CPRPKEY.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    オンライン帳票ＩＤ連絡
           COPY    "CPORCSRPLNK.INC".
      *
       01  SPRT-AREA.
           COPY    "CPORCSPRTLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPCOMMAND-PARAM.INC".
      *
      ****************************************************************
       PROCEDURE               DIVISION
                                       USING
                                       COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
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
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      FLG-AREA
                                           IDX-AREA
                                           WRK-AREA
                                           SPA-AREA
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
           INTO                        LNK-PRTKANRI-RENNUM
                                       LNK-PRTKANRI-TBL-KEY
                                       LNK-PRTKANRI-TBL-GROUP
                                       LNK-PRTKANRI-SHORI-RENNUM
                                       LNK-PRTKANRI-SRYYM
                                       LNK-PRTKANRI-SKYYMD
                                       LNK-PRTKANRI-SHELLID
                                       LNK-PRTKANRI-PRIORITY
                                       LNK-PRTKANRI-TERMID
                                       LNK-PRTKANRI-OPID
                                       LNK-PRTKANRI-PRTNM
                                       LNK-PRTKANRI-TBL-UUID
                                       SPA-HOSPNUM
                                       SPA-CLIENT-SHOWDIALOG
                                       SPA-PRTCONF
                                       SPA-PTID
                                       PUSHPARA
           END-UNSTRING
      *
           MOVE    LNK-PRTKANRI-TBL-UUID
                                       TO  SPA-PRT-UUID
           MOVE    MCP-USER            TO  SPA-OPID
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 900-PRTKANRI-KEY14-SEL-SEC
      *
      *    PERFORM UNTIL ( FLG-PRTKANRI    NOT =   ZERO )
      *
             MOVE    SPACE               TO  SHELLTBL
             MOVE    "print_prv.sh"      TO  SHELLTBL-NAME
             MOVE    "print_prv.sh"      TO  LNK-PRTKANRI-SHELLID
             MOVE    PRTKANRI-RENNUM     TO  SHELLTBL-ARG1
             MOVE    PRTKANRI-TBL-KEY    TO  SHELLTBL-ARG2
             MOVE    PRTKANRI-TBL-GROUP  TO  SHELLTBL-ARG3
             MOVE    ZERO                TO  PRTKANRI-SHORI-RENNUM
             MOVE    PRTKANRI-SHORI-RENNUM
                                         TO  SHELLTBL-ARG4
             MOVE    PRTKANRI-SRYYM      TO  SHELLTBL-ARG5
             MOVE    PRTKANRI-SKYYMD     TO  SHELLTBL-ARG6
             MOVE    PRTKANRI-SHELLID    TO  SHELLTBL-ARG7
             MOVE    PRTKANRI-PRIORITY   TO  SHELLTBL-ARG8
             MOVE    PRTKANRI-TERMID     TO  SHELLTBL-ARG9
             MOVE    PRTKANRI-OPID       TO  SHELLTBL-ARG10
             MOVE    PRTKANRI-PRTNM      TO  SHELLTBL-ARG11
             MOVE    "K03DUMMY"          TO  SHELLTBL-ARG12
             MOVE    "0000000"           TO  SHELLTBL-ARG13
             MOVE    "REPRINT"           TO  SHELLTBL-ARG14
             MOVE    "00001"             TO  SHELLTBL-ARG15
             MOVE    "99999"             TO  SHELLTBL-ARG16
             EVALUATE    SPA-PRTCONF
             WHEN    "1"
                 MOVE    "6"             TO  SHELLTBL-ARG17
             WHEN    "2"
             WHEN    "3"
                 MOVE    "9"             TO  SHELLTBL-ARG17
             WHEN    OTHER
                 MOVE    "5"             TO  SHELLTBL-ARG17
             END-EVALUATE
             MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG18
             MOVE    PRTKANRI-TBL-UUID   TO  SHELLTBL-ARG19
             MOVE    PRTKANRI-GYOUMU-CD  TO  SHELLTBL-ARG20
             MOVE    SPA-CLIENT-SHOWDIALOG
                                         TO  SHELLTBL-ARG22
             MOVE    SHELLTBL            TO  MCPDATA-REC
             IF   (  SPA-PRTCONF         =   "2" OR "3" )
                 MOVE    "EXCOMMAND"     TO  MCP-FUNC
             ELSE
                 MOVE    "SHELL"         TO  MCP-FUNC
             END-IF
             MOVE    "shell"             TO  MCP-TABLE
             MOVE    "allways"           TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *      PERFORM 900-PRTKANRI-KEY14-FET-SEC
      *
      *    END-PERFORM
      *
           MOVE    "tbl_prtkanri"          TO  MCP-TABLE
           MOVE    "key14"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( SPA-PRTCONF             =   "2" OR "3" )
      *
               INITIALIZE                  SGETTEMP-AREA
               MOVE    PUSHPARA    TO  SGETTEMP-BASENAMES (1)
               CALL    "ORCSGETTEMP"   USING
                                       SGETTEMP-AREA
               MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  PUSHPARA
      *
               OPEN    OUTPUT          PUSH-FILE
      *
               MOVE    "00"            TO  SPA-PRT-FLG
               INITIALIZE                  SRPLNK-AREA
               MOVE    "SEL"           TO  SRPLNK-MODE
               MOVE    RPKEY-COMMON    TO  SRPLNK-VAL
               CALL    "ORCSRPLNK"         USING
                                           SRPLNK-AREA
                                           SPA-AREA
      *
      D        DISPLAY "SRPLNK-AREA = " SRPLNK-AREA
      *
               INITIALIZE              PUSHP002-REC
               MOVE    "print002"  TO  PUSHP002-EVENT
               MOVE    SRPLNK-ORCA-UID
                                   TO  PUSHP002-ORCA-UID
               MOVE    SPA-PRT-UUID
                                   TO  PUSHP002-DATA-ID
               WRITE   PUSHP002-REC
               CLOSE   PUSH-FILE
      *
               INITIALIZE                  SRPLNK-AREA
               MOVE    "DEL_GROUP"     TO  SRPLNK-MODE
               CALL    "ORCSRPLNK"         USING
                                           SRPLNK-AREA
                                           SPA-AREA
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    印刷管理検索処理
      *****************************************************************
       900-PRTKANRI-KEY14-SEL-SEC          SECTION.
      *
           MOVE    ZERO                    TO  FLG-PRTKANRI
      *
           INITIALIZE                          PRTKANRI-REC
           MOVE    SPA-HOSPNUM             TO  PRTKANRI-HOSPNUM
           MOVE    LNK-PRTKANRI-TBL-KEY    TO  PRTKANRI-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM     TO  PRTKANRI-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP  TO  PRTKANRI-TBL-GROUP
           MOVE    LNK-PRTKANRI-TBL-UUID   TO  PRTKANRI-TBL-UUID
           MOVE    PRTKANRI-REC            TO  MCPDATA-REC
           MOVE    "tbl_prtkanri"          TO  MCP-TABLE
           MOVE    "key14"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC         TO  PRTKANRI-REC
           ELSE
               MOVE    1                   TO  FLG-PRTKANRI
               INITIALIZE                      PRTKANRI-REC
           END-IF
      *
           .
      *
       900-PRTKANRI-KEY14-SEL-EXT.
           EXIT.
      *****************************************************************
      *    印刷管理FETCH処理
      *****************************************************************
       900-PRTKANRI-KEY14-FET-SEC          SECTION.
      *
           MOVE    ZERO                    TO  FLG-PRTKANRI
      *
           MOVE    "tbl_prtkanri"          TO  MCP-TABLE
           MOVE    "key14"                 TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC         TO  PRTKANRI-REC
           ELSE
               MOVE    1                   TO  FLG-PRTKANRI
               INITIALIZE                      PRTKANRI-REC
           END-IF
      *
           .
      *
       900-PRTKANRI-KEY14-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
      *    ＤＢ検索処理（FHETCHを行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
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
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
