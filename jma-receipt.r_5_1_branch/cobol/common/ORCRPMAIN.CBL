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
       PROGRAM-ID.             ORCRPMAIN.
      *****************************************************************
      *  システム名        : ORCA
      *  サブシステム名    : 
      *  コンポーネント名  : 外部連携用帳票データ作成サブ
      *  返却エラーコード  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/05/26    NACL-太田     新規作成
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT2-FILE       ASSIGN              PRTOUTPARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PRT2.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT2-FILE.
       01  PRT2-REC.
           03  PRT2-PRTDATA        PIC X(20000).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPCOMMONPRT.INC".
           COPY    "CPASGNPARA.INC"    REPLACING   //ASGNPARA//
                                       BY          //PRTOUTPARA//.
      *
      *    シェル用領域
           COPY    "CPSHELLTBL.INC".
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-PRT2            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PRT2            PIC 9(01).
           03  FLG-PRTKANRI        PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-OUT             PIC 9(04).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-NUM2-G.
               05  WRK-NUM2            PIC 9(02).
               05  WRK-SHORI-RENNUM    PIC 9(02).
           03  WRK-DATAID              PIC X(80).
           03  WRK-RC                  PIC S9(8).
           03  WRK-TABLE               PIC X(64).
      *
       01  SGYOUMU-CODE                PIC 9(03).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    印刷管理ＤＢ
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷ＤＢ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      *
      *    ＡＰＩパラメタ
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
      *
      *    帳票データ取得API用パラメタ
           COPY    "CPPARA001.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "ORCA-BLOB".
           COPY    "MCPDATA.INC".
           COPY    "XMLDATA.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
       01  SPRTLNK-AREA.
           COPY    "CPORCSPRTLNK.INC".
      *
      *   ＵＩＤ取得用エリア
       01 UIDIO-ST         PIC 9(2).
       01 UIDIO-AREA.
           03  UIDIO-RET   PIC X(2).
           03  UIDIO-UID   PIC X(36).
      *
      *   印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *    共通領域
           COPY    "MCPAREA".
      *
           COPY    "CPORCRPMAIN.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
           MCPAREA
           RPMAIN-AREA
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                        SECTION.
      *
           INITIALIZE                      STS-AREA
           INITIALIZE                      FLG-AREA
           INITIALIZE                      CNT-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      *
           MOVE    MCP-TABLE           TO  WRK-TABLE
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
      *
           .
      *
           EXIT PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    ZERO                TO  SGYOUMU-CODE
           IF    (  RPMAIN-TBL-KEY     =   "ORCBS01" )
               MOVE    23              TO  SGYOUMU-CODE
           ELSE
               CALL     "ORCSGYOUMUGET"    USING
                                           SPA-AREA
                                           SGYOUMU-CODE
           END-IF
      *
           CALL    "orcuidset"         USING
                                       UIDIO-ST
                                       UIDIO-AREA
      *
           INITIALIZE                      HC01PARA
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    "RP00"              TO  HC01PARA-FORM-ID
           MOVE    UIDIO-UID           TO  HC01PARA-TERMID
           MOVE    SMCNDATE-TIME       TO  HC01PARA-SYOHMS
           IF      SPA-PRT-FLG         IS  NUMERIC
               MOVE SPA-PRT-FLG        TO  WRK-NUM2-G
           ELSE
               MOVE ZERO               TO  WRK-NUM2-G
           END-IF
           MOVE    WRK-NUM2            TO  HC01PARA-CNT
           INSPECT HC01PARA-BASENAME   REPLACING  ALL " " BY "_"
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA-BASENAME   TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"           USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  PRTOUTPARA
      *
           .
      *
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    "PRINTED"           TO  SPA-PUTTYPE
      *
           PERFORM 2001-JSON-EXPORT-SEC
      *
           IF    ( SPA-API-FLG     NOT =   ZERO )
            OR   ( SPA-CLIENT-PRT-FLG  =   ZERO )
      *
               PERFORM 2001-PRTDATA-EDIT-SEC
      *
               PERFORM 2001-APIPARA-INS-SEC
      *
               IF    ( SPA-CLIENT-PRT-FLG  =   ZERO )
                   PERFORM 2001-PUSH-SEC
               END-IF
      *
           ELSE
               PERFORM 2001-REPRINT-SEC
           END-IF
      *
           .
      *
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    データ出力処理（ＪＳＯＮ形式）
      *****************************************************************
       2001-JSON-EXPORT-SEC            SECTION.
      *
           INITIALIZE                      XMLDATA-REC
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    3                   TO  XMLDATA-MODE
           MOVE    RPMAIN-DATA         TO  XMLDATA-DATA
           MOVE    "data"              TO  XMLDATA-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
      *
           INITIALIZE                      ORCA-BLOB
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "blob"              TO  MCP-TABLE
           MOVE     XMLDATA-OBJECT     TO  ORCA-BLOB-OBJECT
           MOVE     PRTOUTPARA         TO  ORCA-BLOB-FILE
           PERFORM 910-BLOBEXPORT-SEC
      *
           .
      *
       2001-JSON-EXPORT-EXT.
           EXIT.
      *****************************************************************
      *    PUSH処理
      *    （シェルの実行はオンラインのトランザクション終了後）
      *****************************************************************
       2001-PUSH-SEC                   SECTION.
      *
           MOVE    SPACE               TO  SHELLTBL
           INITIALIZE                      SHELLTBL
      *
           MOVE    "rpmain1.sh"        TO  SHELLTBL-NAME
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG1
           MOVE    SPA-TERMID          TO  SHELLTBL-ARG2
           MOVE    SPA-OPID            TO  SHELLTBL-ARG3
           MOVE    SPA-SYSYMD          TO  SHELLTBL-ARG4
      *
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "daily"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
      *
       2001-PUSH-EXT.
           EXIT.
      *****************************************************************
      *    再印刷データ作成処理
      *****************************************************************
       2001-REPRINT-SEC                SECTION.
      *
           MOVE    SPACE               TO  SHELLTBL
           INITIALIZE                      SHELLTBL
      *
           PERFORM 20011-SHELL-ARG-EDIT-SEC
      *
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "daily"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
      *
       2001-REPRINT-EXT.
           EXIT.
      *****************************************************************
      *    シェル引数編集処理
      *****************************************************************
       20011-SHELL-ARG-EDIT-SEC            SECTION.
      *
      *
           MOVE    "online-reprint.sh" TO  SHELLTBL-NAME
           MOVE    1                   TO  LNK-PRTKANRI-RENNUM
           IF    ( RPMAIN-TBL-KEY      NOT =   SPACE )
               MOVE    RPMAIN-TBL-KEY  TO  LNK-PRTKANRI-TBL-KEY
           ELSE
               MOVE    "ONLINE"        TO  LNK-PRTKANRI-TBL-KEY
           END-IF
           MOVE    SPA-PRT-GROUP       TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-PRT-UUID        TO  LNK-PRTKANRI-TBL-UUID
           MOVE    SPA-SYSYMD          TO  LNK-PRTKANRI-SKYYMD
                                           LNK-PRTKANRI-SRYYM
           MOVE    1                   TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID          TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID            TO  LNK-PRTKANRI-OPID
      *
           MOVE    LNK-PRTKANRI-RENNUM TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-TBL-KEY TO SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SHELLTBL-ARG3
           IF      SPA-PRT-FLG     IS  NUMERIC
               MOVE SPA-PRT-FLG        TO  WRK-SHORI-RENNUM
           ELSE
               MOVE ZERO               TO  WRK-SHORI-RENNUM
           END-IF
           COMPUTE LNK-PRTKANRI-SHORI-RENNUM
               =   WRK-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SRYYM  TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SKYYMD TO  SHELLTBL-ARG6
           IF    ( SPA-API-FLG         =   2 OR 3 )
            OR   ( SPA-CLIENT-PRT-FLG  =   ZERO )
               MOVE    "API_01"        TO  SHELLTBL-ARG7
           ELSE
               MOVE    "API"           TO  SHELLTBL-ARG7
           END-IF
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-TERMID
                                       TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-OPID
                                       TO  SHELLTBL-ARG10
           MOVE    SPACE               TO  SHELLTBL-ARG11
           MOVE    SPA-SRYYMD          TO  SHELLTBL-ARG12
           MOVE    LNK-PRTKANRI-TBL-UUID
                                       TO  SHELLTBL-ARG13
           MOVE    SPA-PTID            TO  SHELLTBL-ARG14
      *    SITE_KBN
           MOVE    "1"                 TO  SHELLTBL-ARG15
      *
           MOVE    SGYOUMU-CODE        TO  SHELLTBL-ARG16
           MOVE    RPMAIN-TITLE        TO  SHELLTBL-ARG17
      *    DIA
      *    MOVE    MKPRT-ID            TO  SHELLTBL-ARG18
      *    FORM
           IF    ( RPMAIN-CUSTOM-ID    NOT =   SPACE )
               MOVE    RPMAIN-CUSTOM-ID
                                       TO  SHELLTBL-ARG19
           END-IF
      *    DEF
           MOVE    SPACE               TO  SHELLTBL-ARG20
      *    データ内容
           MOVE    HC01PARA-BASENAME   TO  SHELLTBL-ARG21
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG22
      *
           EVALUATE    TRUE
           WHEN  ( SPA-CLIENT-TEMP-FLG =   2 )
           WHEN  ( SPA-API-FLG         =   2 OR 3)
               MOVE    "D"             TO  SHELLTBL-ARG23
           WHEN    OTHER
               MOVE    "1"             TO  SHELLTBL-ARG23
           END-EVALUATE
           MOVE    HC01PARA-BASENAME   TO  SHELLTBL-ARG24
           MOVE    RPMAIN-ID           TO  SHELLTBL-ARG25
      *
           .
      *
       20011-SHELL-ARG-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    印刷データ編集処理
      *****************************************************************
       2001-PRTDATA-EDIT-SEC           SECTION.
      *
           MOVE    SPACE               TO  SHELLTBL
           INITIALIZE                      SHELLTBL
      *
           PERFORM 20011-SHELL-ARG-EDIT-SEC
      *
           IF    ( SPA-API-FLG         =   1 )
               PERFORM 20011-HAORI-EDIT-SEC
           ELSE
               PERFORM 20011-NOT-HAORI-EDIT-SEC
           END-IF
      *
           MOVE    SPACE               TO  WRK-DATAID
           STRING  LNK-PRTKANRI-TBL-KEY    DELIMITED BY SPACE
                   "#"                     DELIMITED BY SIZE
                   LNK-PRTKANRI-TBL-UUID   DELIMITED BY SPACE
                   "#"                     DELIMITED BY SIZE
                   LNK-PRTKANRI-TBL-GROUP  DELIMITED BY SIZE
                   "#"                     DELIMITED BY SIZE
                   LNK-PRTKANRI-RENNUM     DELIMITED BY SIZE
                   "#"                     DELIMITED BY SIZE
                   LNK-PRTKANRI-SHORI-RENNUM
                                           DELIMITED BY SIZE
           INTO    WRK-DATAID
           END-STRING
      *
           .
       2001-PRTDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    印刷データ編集処理（HAORIからの印刷）
      *****************************************************************
       20011-HAORI-EDIT-SEC            SECTION.
      *
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "daily"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       20011-HAORI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    印刷データ編集処理（HAORIからの印刷以外）
      *****************************************************************
       20011-NOT-HAORI-EDIT-SEC          SECTION.
      *
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "EXCOMMAND"         TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "daily"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           OPEN    INPUT               PRT2-FILE
           IF    ( STS-PRT2    NOT =   ZERO )
               MOVE    1           TO  FLG-END
           END-IF
      *
           PERFORM 900-PRTKANRI-KEY-SEL-SEC
      *
           PERFORM 900-PRT2-READ-SEC
      *
           MOVE    ZERO                TO  CNT-OUT
      *
           PERFORM UNTIL ( FLG-END     NOT =   ZERO )
                    OR   ( FLG-PRT2    NOT =   ZERO )
      *
               COMPUTE CNT-OUT =   CNT-OUT +   1
      *
               IF    ( FLG-PRTKANRI    =   ZERO )
                AND  ( CNT-OUT         =   1 )
                   MOVE    ","             TO  PRTDATA-PRTDATA(1:1)
                   MOVE    PRT2-PRTDATA    TO  PRTDATA-PRTDATA(2:)
               ELSE
                   MOVE    PRT2-PRTDATA    TO  PRTDATA-PRTDATA
               END-IF
               PERFORM 390-PRINT-OUT-SEC
      *
               PERFORM 900-PRT2-READ-SEC
      *
           END-PERFORM
      *
           CLOSE                       PRT2-FILE
      *
           CALL    "CBL_DELETE_FILE"   USING
                                       PRTOUTPARA
           RETURNING   WRK-RC
           END-CALL
      *
           .
       20011-NOT-HAORI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC                SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-TBL-UUID
                                       TO  SPRT-TBL-UUID
           MOVE    LNK-PRTKANRI-GYOUMU-CD
                                       TO  SPRT-GYOUMU-CD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    SPA-SRYYMD          TO  SPRT-SKYYMD
           IF    ( SPA-API-FLG         =   2 OR 3 )
               MOVE    "API_01"        TO  SPRT-SHELLID
           ELSE
               MOVE    "API"           TO  SPRT-SHELLID
           END-IF
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    RPMAIN-CUSTOM-ID    TO  SPRT-PRTID
           MOVE    PRTDATA-PRTDATA     TO  SPRT-PRTDATA
           MOVE    RPMAIN-TITLE        TO  SPRT-TITLE
      *
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           EVALUATE    TRUE
           WHEN  ( SPA-CLIENT-TEMP-FLG =   2 )
           WHEN  ( SPA-API-FLG         =   2 OR 3)
               MOVE    "D"             TO  SPRT-PRTFLG
           WHEN    OTHER
               MOVE    "1"             TO  SPRT-PRTFLG
           END-EVALUATE
      *
           MOVE    SPA-PTID            TO  SPRT-PTID
           MOVE    SPACE               TO  SPRT-DATA-TYPE
           MOVE    RPMAIN-ID           TO  SPRT-API-ID
      *
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
      *    IF      SPRT-RETURN         =   ZERO
      *        CONTINUE
      *    ELSE
      *        MOVE    "印刷ＤＢに更新できませんでした"
      *                                   TO  WRK-RECEERR
      *        PERFORM 500-ERR-HENSYU-SEC                           
      *    END-IF                                                              
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    APIパラメタ作成処理
      *****************************************************************
       2001-APIPARA-INS-SEC            SECTION.
      *
           INITIALIZE                  APIPARA-REC
                                       PARA001-REC
      *
           MOVE    SPA-HOSPNUM     TO  APIPARA-HOSPNUM
           MOVE    "api001"        TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID      TO  APIPARA-TERMID
           MOVE    SPA-OPID        TO  APIPARA-FILEMEI
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  APIPARA-EDANUM
           MOVE    SPA-PRT-UUID    TO  APIPARA-KARTE-UID
           MOVE    SMCNDATE-YMD    TO  APIPARA-UPYMD
           MOVE    SMCNDATE-HMS    TO  APIPARA-UPHMS
      *
           MOVE    RPMAIN-ID       TO  PARA001-FORMID
           MOVE    RPMAIN-CUSTOM-ID TO PARA001-CUSTOMID
           MOVE    RPMAIN-TITLE    TO  PARA001-FORMNAME
           MOVE    WRK-DATAID      TO  PARA001-DATAID
           MOVE    PARA001-REC     TO  APIPARA-DATA-REC
      *
           MOVE    APIPARA-REC     TO  MCPDATA-REC
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           MOVE    APIPARA-REC     TO  MCPDATA-REC
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
      *
       2001-APIPARA-INS-EXT.
           EXIT.
      *****************************************************************
      *    印刷管理検索処理
      *****************************************************************
       900-PRTKANRI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PRTKANRI
      *
           INITIALIZE                      PRTKANRI-REC
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  PRTKANRI-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM TO  PRTKANRI-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  PRTKANRI-TBL-GROUP
           MOVE    LNK-PRTKANRI-TBL-UUID
                                       TO  PRTKANRI-TBL-UUID
           MOVE    LNK-PRTKANRI-GYOUMU-CD
                                       TO  PRTKANRI-GYOUMU-CD
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  PRTKANRI-SHORI-RENNUM
           MOVE    SPA-HOSPNUM         TO  PRTKANRI-HOSPNUM
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PRTKANRI-REC
           ELSE
               MOVE    1               TO  FLG-PRTKANRI
               INITIALIZE                  PRTKANRI-REC
           END-IF
      *
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PRTKANRI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    帳票データ読込処理
      *****************************************************************
       900-PRT2-READ-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-PRT2
      *
           READ    PRT2-FILE
           AT  END
               MOVE    1               TO  FLG-PRT2
           END-READ
      *
           .
       900-PRT2-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＸＭＬＷＲＩＴＥ処理
      *****************************************************************
       910-XMLWRITE-SEC                SECTION.
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           CALL    "MONFUNC"               USING
                                           MCPAREA
                                           XMLDATA-REC
      *
           .
       900-XMLWRITE-EXT.
           EXIT.
      *****************************************************************
      *    ＢＬＯＢＥＸＰＯＲＴ処理
      *****************************************************************
       910-BLOBEXPORT-SEC              SECTION.
      *
           MOVE    "BLOBEXPORT"        TO  MCP-FUNC
           CALL    "MONFUNC"               USING
                                           MCPAREA
                                           ORCA-BLOB
      *
           .
       900-BLOBEXPORT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                 SECTION.
      *
           MOVE    "DBINSERT"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
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
