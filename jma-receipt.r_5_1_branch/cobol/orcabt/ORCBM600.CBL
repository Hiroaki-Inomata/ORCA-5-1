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
       PROGRAM-ID.             ORCBM600.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次
      *  コンポーネント名  : 地方公費ＣＳＶデータ作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/08/22    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    ＣＳＶデータ
           SELECT  CSV-FILE   ASSIGN  CSVPARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-CSV.
      *    エラーファイル
           SELECT  CSVERR-FILE  ASSIGN  CSVERR
                                   FILE    STATUS  IS  STS-CSVERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ＣＳＶデータ
       FD  CSV-FILE.
       01  CSV-REC                 PIC X(2000). 
      *
      *    エラーファイル
       FD  CSVERR-FILE.
       01  CSVERR-REC              PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    ＣＳＶデータ
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //CSVPARA//.
      *
      *01  CSVPARA.
      ***  03  FILLER              PIC X(05) VALUE "/tmp/".
       01  CSVPARA-BASENAME.
           03  CSVPARA-HOSPNUM     PIC 9(02).
           03  FILLER              PIC X(10)   VALUE 
                                               "TIHOUKOUHI".
           03  CSVPARA-RENNUM      PIC 9(02)   VALUE   1.
           03  FILLER              PIC X(04)   VALUE   ".txt".
      *
           COPY    "CPRECEERR.INC" REPLACING  //RECEERR//
                                   BY         //CSVERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-CSV             PIC X(02).
           03  STS-CSVERR          PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-CSV-INFO        PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-CSV             PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                 PIC 9(03). 
           03  IDY                 PIC 9(03). 
           03  IDZ                 PIC 9(03).
      *
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-HOSPNUM        PIC 9(02).
               05  WRK-PARA-SRYYM          PIC X(06).
               05  WRK-PARA-CSV-SHELLID    PIC X(20).
               05  WRK-PARA-SHORI-RENNUM   PIC 9(03).
               05  WRK-PARA-RENNUM         PIC 9(03).
               05  WRK-PARA-TO-DATA        PIC X(200).
               05  WRK-PARA-JOBID          PIC 9(07).
               05  WRK-PARA-SHELLID        PIC X(08).
      *
       01  WRK-ERR-AREA.
           03  WRK-CSVERR                          PIC X(200). 
           03  WRK-ERRMSG                          PIC X(300).
           03  WRK-MCP-RC                          PIC 9(9).
           03  WRK-ERR-FILE-STS                    PIC X(02).
           03  WRK-ERR-FILE-MSG                    PIC X(100).
           03  WRK-ERR-FILE-MSG1                   PIC X(17).
           03  WRK-ERR-FLG                         PIC 9(01).
      *    エラーファイル名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  CSV-INFO-REC.
           COPY    "CPCSV-INFO.INC".          
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(400).
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
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1 
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
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       FLG-AREA
                                       SPA-AREA
                                       CSVERR
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA-HOSPNUM
                                               WRK-PARA-SRYYM
                                               WRK-PARA-CSV-SHELLID
                                               WRK-PARA-SHORI-RENNUM
                                               WRK-PARA-RENNUM
                                               WRK-PARA-TO-DATA 
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               CSVERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM
                                   TO  SPA-HOSPNUM
      *
           MOVE    SPA-HOSPNUM     TO  CSVPARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    CSVERR              TO  SGETTEMP-BASENAMES  (1)
           MOVE    CSVPARA-BASENAME    TO  SGETTEMP-BASENAMES  (2)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  CSVERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  CSVERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  CSVPARA
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBM600"      TO  JOB-PGID
           MOVE    "ＣＳＶデータ作成"
                                   TO  JOB-SHELLMSG
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           OPEN    OUTPUT              CSV-FILE
      *
           PERFORM 900-CSV-INFO-START-SEC   
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *    
           MOVE    CSV-INFO-CSVDATA    TO  CSV-REC
           WRITE   CSV-REC
      *
           IF      STS-CSV             =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-CSVERR
               STRING "ＣＳＶデータ書き込みエラー STS="
                                                   DELIMITED  BY  SIZE
                       STS-CSV                     DELIMITED  BY  SIZE
                                           INTO    WRK-CSVERR
               END-STRING                                
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF 
           ADD     1                   TO  CNT-CSV
      *
           PERFORM 900-CSV-INFO-READ-SEC   
           .
       200-MAIN-EXT.
           EXIT. 
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   CSVERR-FILE
           IF      STS-CSVERR         =   ZERO
               CLOSE   CSVERR-FILE
           ELSE
               OPEN    OUTPUT              CSVERR-FILE
      *
               MOVE    WRK-CSVERR         TO  CSVERR-REC
               WRITE   CSVERR-REC
               CLOSE   CSVERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-CSVERR      TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCBM600 "         DELIMITED  BY  SIZE
                   WRK-CSVERR          DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING   
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           MOVE    "tbl_csv_info"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           CLOSE   CSV-FILE
      *
           IF      CNT-CSV    =   ZERO  
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-CSVERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *     
           DISPLAY "CSV  CNT-OUT:" CNT-CSV
           DISPLAY "*** ORCBM600 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ＿ＩＮＦＯＤＢ読込
      *****************************************************************
       900-CSV-INFO-START-SEC              SECTION.
      *
           MOVE    SPACE               TO  CSV-INFO-REC
           INITIALIZE                      CSV-INFO-REC
           MOVE    SPA-HOSPNUM         TO  CSV-INFO-HOSPNUM
           MOVE    WRK-PARA-SHELLID    TO  CSV-INFO-TBL-KEY
           MOVE    WRK-PARA-SRYYM      TO  CSV-INFO-SRYYM
           MOVE    WRK-PARA-CSV-SHELLID
                                       TO  CSV-INFO-SHELLID
           MOVE    WRK-PARA-SHORI-RENNUM
                                       TO  CSV-INFO-SHORI-RENNUM
           MOVE    WRK-PARA-RENNUM     TO  CSV-INFO-RENNUM
      *
           MOVE    CSV-INFO-REC        TO  MCPDATA-REC
           MOVE    "tbl_csv_info"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-CSV-INFO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-CSV-INFO
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-CSV-INFO-START-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ＿ＩＮＦＯＤＢ読込
      *****************************************************************
       900-CSV-INFO-READ-SEC             SECTION.
      *
           MOVE    "tbl_csv_info"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  CSV-INFO-REC
               MOVE    ZERO                TO  FLG-CSV-INFO
           ELSE
               MOVE    1                   TO  FLG-CSV-INFO
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-CSV-INFO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "ＤＢ読み込みエラー　TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-CSVERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCBM600 select err TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " PATHNAME="        DELIMITED   BY  SIZE
                       MCP-PATHNAME        DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               CALL    "cobabort"          USING   WRK-ERRMSG
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
