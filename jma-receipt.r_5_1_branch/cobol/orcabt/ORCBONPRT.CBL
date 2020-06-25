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
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBONPRT.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : オンライン印刷管理
      *  コンポーネント名  : オンライン帳票ＤＢの格納
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/05/25    NACL-竹田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  PRTOUTPARA
                                   FILE    STATUS  IS  STS-PRT.
      *
      *    印刷用データ
           SELECT  PRT2-FILE       ASSIGN  PRTOUTPARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PRT2.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
       01  PRT-REC.
           03  PRT-PRTDATA         PIC X(20000).
      *
       FD  PRT2-FILE.
       01  PRT2-REC.
           03  PRT2-PRTDATA        PIC X(20000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).    
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PRT             PIC X(02).
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
           03  CNT-IN              PIC 9(04).
           03  CNT-IN-CANCEL       PIC 9(06).
           03  CNT-OUT             PIC 9(04).
      *
       01  INDEX-AREA.
           03  IDX                     PIC 9(03). 
      *
       COPY    "CPASGNPARA.INC"        REPLACING   //ASGNPARA//
                                       BY          //PRTOUTPARA//.
      *
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-PATH                PIC X(64). 
      *
           03  WRK-PARA.
               05  WRK-PARA1.  
                   07  WRK-PARA-SYSYMD           PIC X(08).
                   07  WRK-PARA-SRYYMD           PIC X(08).
                   07  WRK-PARA-PRT-UUID         PIC X(31).
                   07  WRK-PARA-PTID             PIC 9(10).
                   07  WRK-PARA-SITE-KBN         PIC X(01).
                   07  WRK-PARA-GYOMU-NAME       PIC X(40).
                   07  WRK-PARA-TITLE0           PIC X(40).
                   07  WRK-PARA-FORMID           PIC X(20).
                   07  WRK-PARA-SYOHMS           PIC X(06).
                   07  WRK-PARA-TYPE             PIC X(16).
                   07  WRK-PARA-DATASETNAME      PIC X(100).
                   07  WRK-PARA-TITLE            PIC X(40).
                   07  WRK-PARA-SRYYM            PIC X(06).
                   07  WRK-PARA-TERMID           PIC X(16).
                   07  WRK-PARA-HOSPNUM          PIC 9(02).
                   07  WRK-PARA-TEMP-KBN         PIC X(01).
                   07  WRK-PARA-API-ID           PIC X(20).
      *
           COPY    "CPORCSPRTLNK.INC".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *     
      *    COPY    "ORCA-DBPATH".         
      *
           COPY    "COMMON-SPA".
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
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".          
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
           COPY    "CPCOMMAND-PARAM.INC".
      ****************************************************************
       PROCEDURE           DIVISION
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
           IF    ( FLG-END         =   ZERO )
               IF    ( LNK-PRTKANRI-SHELLID    =   "API" )
                   PERFORM 201-MAIN-SEC
               ELSE
                   PERFORM 200-MAIN-SEC
               END-IF
           END-IF
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      * 
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
      *
           ACCEPT  SYS-TIME            FROM    TIME     
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO  LNK-PRTKANRI-RENNUM
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
                                             WRK-PARA-SRYYMD
                                             LNK-PRTKANRI-TBL-UUID
                                             WRK-PARA-PTID
                                             WRK-PARA-SITE-KBN
                                             LNK-PRTKANRI-GYOUMU-CD
                                             WRK-PARA-TITLE0
                                             WRK-PARA-SYOHMS
                                             WRK-PARA-FORMID
                                             WRK-PARA-TYPE
                                             WRK-PARA-DATASETNAME
                                             WRK-PARA-HOSPNUM
                                             WRK-PARA-TEMP-KBN
                                             WRK-PARA-API-ID
           END-UNSTRING                                  
      *
           MOVE    WRK-PARA-HOSPNUM    TO    SPA-HOSPNUM 
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    帳票印刷処理
           INITIALIZE                  SGETTEMP-AREA
           MOVE    WRK-PARA-DATASETNAME
                                       TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  PRTOUTPARA
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN    INPUT               PRT-FILE
           MOVE    SPACE               TO  PRT-PRTDATA
           IF        ( STS-PRT         =   ZERO )
               DISPLAY "INPUT FILE OPEN OK"
               READ PRT-FILE
               AT  END
                   DISPLAY "INPUT NO DATA"
               END-READ
           ELSE
               DISPLAY "INPUT FILE OPEN ERR"
           END-IF
      *
           MOVE    PRT-PRTDATA         TO  PRTDATA-PRTDATA
           PERFORM 390-PRINT-OUT-SEC
      *
           CLOSE                       PRT-FILE
           MOVE    1                   TO  FLG-END     
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理（ＡＰＩ用）
      *****************************************************************
       201-MAIN-SEC                    SECTION.
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
           MOVE    1                   TO  FLG-END
           .
       201-MAIN-EXT.
           EXIT.
      *
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
           MOVE    WRK-PARA-SRYYMD     TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    WRK-PARA-FORMID     TO  SPRT-PRTID
           MOVE    PRTDATA-PRTDATA     TO  SPRT-PRTDATA
           MOVE    WRK-PARA-TITLE0     TO  SPRT-TITLE
      *
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    WRK-PARA-SITE-KBN   TO  SPRT-SITEKBN
           IF    ( WRK-PARA-TEMP-KBN   =   "D" )
               MOVE    "D"             TO  SPRT-PRTFLG
           ELSE
               MOVE    "1"             TO  SPRT-PRTFLG
           END-IF
      *    
           MOVE    WRK-PARA-PTID       TO  SPRT-PTID
           MOVE    WRK-PARA-TYPE       TO  SPRT-DATA-TYPE
           MOVE    WRK-PARA-API-ID     TO  SPRT-API-ID
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
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *     
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORCBONPRT END ***"
           .
       300-END-EXT.
           EXIT.
      *
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
      *    印刷管理検索処理
      *****************************************************************
       900-PRTKANRI-KEY-SEL-SEC        SECTION.
      *
           INITIALIZE                      FLG-PRTKANRI
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
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC
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
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
