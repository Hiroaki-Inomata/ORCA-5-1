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
       PROGRAM-ID.             ORCBQ02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者照会
      *  コンポーネント名  : 明細書連携データ作成（ＯＲＣＢＱ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/10/31    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付       内容
      *  03.05.00    NACL-太田    07.05.23 グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメータファイル
           SELECT  BQ01-FILE       ASSIGN  PRFNAME
                                   ORGANIZATION    IS  SEQUENTIAL
                                   FILE    STATUS  IS  STS-BQ01.
      *
      *    一時ファイル（患者重複チェック）
           SELECT  BQ03T01-FILE    ASSIGN  BQ03T01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BQ03T01-KEY
                                   FILE    STATUS  IS  STS-BQ03T01.
           
      
      *
      *    エラーファイル
           SELECT  BQERR-FILE      ASSIGN  ERRFNAME
                                   FILE    STATUS  IS  STS-BQERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    パラメータファイル
       FD  BQ01-FILE.
           COPY    "CPBQ01.INC".
      *
      *    一時ファイル（患者重複チェック）
       FD  BQ03T01-FILE.
       01  BQ03T01-REC.
           03  BQ03T01-KEY.
               05  BQ03T01-HOSPNUM     PIC 9(02).
               05  BQ03T01-PTID        PIC 9(10).
      *
      *    パラメータファイル
       FD  BQERR-FILE.
       01  BQERR-REC               PIC X(100).
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    シェル用領域
      *    COPY    "CPCOMMONSHELL.INC".
           COPY    "CPSHELLTBL.INC".
      *
      *    ワークパラメータファイル
           COPY    "CPBQ01.INC"    REPLACING  //BQ01//
                                   BY         //WKBQ01//.
      *
      *
           COPY    "CPASGNPARA.INC"
                                   REPLACING  //ASGNPARA//
                                   BY         //PRFNAME//.
      *
           COPY    "CPASGNPARA.INC"
                                   REPLACING  //ASGNPARA//
                                   BY         //ERRFNAME//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                           REPLACING   //RECE01//
                                           BY          //BQ03T01//.
           03  FILLER                      PIC X(04)   VALUE   ".dat".
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-BQ01                PIC X(02).
           03  STS-BQERR               PIC X(02).
           03  STS-BQ03T01             PIC X(02).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDX1                    PIC 9(09).
           03  IDX2                    PIC 9(09).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-PTSRH               PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-BQ03T01             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PTINF               PIC 9(05).
           03  CNT-GIHO                PIC 9(05).
           03  CNT-GRSI                PIC 9(05).
           03  CNT-GJBI                PIC 9(05).
           03  CNT-GJBIR               PIC 9(05).
           03  CNT-GJBIK               PIC 9(05).
           03  CNT-GKGI                PIC 9(05).
           03  CNT-GDI3                PIC 9(05).
           03  CNT-NIHO                PIC 9(05).
           03  CNT-NRSI                PIC 9(05).
           03  CNT-NJBI                PIC 9(05).
           03  CNT-NJBIR               PIC 9(05).
           03  CNT-NJBIK               PIC 9(05).
           03  CNT-NKGI                PIC 9(05).
           03  CNT-NDI3                PIC 9(05).
      *    一時領域
       01  WRK-AREA. 
           03  WRK-BQERRCD             PIC X(04).
           03  WRK-PARA.
               05  WRK-PARA-PAFNAME    PIC X(100).
               05  WRK-PARA-TERMID     PIC X(64).
               05  WRK-PARA-SYSYMD     PIC X(08).
               05  WRK-PARA-SYSTIME    PIC X(08).
               05  WRK-PARA-HOSPNUM    PIC 9(02).
               05  WRK-PARA-OPID       PIC X(16).
               05  WRK-PARA-ERRFNAME   PIC X(200).
               05  WRK-PARA-JOBID      PIC 9(07).
               05  WRK-PARA-SHELLID    PIC X(08).
               05  WRK-PARA-BTPARA-SHELLID
                                       PIC X(08).
      *
       01  CONST-AREA.
           03  CONST-SCRIPTID          PIC X(20)   VALUE   "ORCBQ02.sh".
           03  CONST-PTINF-MAX         PIC 9(05)   VALUE   2000.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    患者照会マスタ
       01  PTSRH-REC.
           COPY    "CPPTSRH.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
           COPY    "CPQTORMAIN.INC".
           COPY    "CPQTORSUB.INC".
           COPY    "CPQTOANY.INC".
      *
      *    入院診療会計マスター
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    保険分類サブ
           COPY    "CPORCSS001.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      *
      ******************************************************************
       PROCEDURE                       DIVISION
                                       USING     COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA
                                       FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
                                       CNT-AREA
                                       SPA-AREA
      *
           PERFORM 800-MCNDATE-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    WRK-PARA-PAFNAME
                                               WRK-PARA-TERMID
                                               WRK-PARA-SYSYMD
                                               WRK-PARA-SYSTIME
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-OPID
                                               WRK-PARA-ERRFNAME
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-BTPARA-SHELLID
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    WRK-PARA-TERMID     TO  SPA-TERMID
      *
      *    ＤＢオープン
           PERFORM 120-DBOPEN-SEC
      *
           PERFORM 900-JOB-STEP-START-SEC
      *
           MOVE    SPA-HOSPNUM         TO  BQ03T01PARA-HOSPNUM
           MOVE    "BQ03T01"           TO  BQ03T01PARA-FILE-ID
           MOVE    SPA-TERMID          TO  BQ03T01PARA-TERMID
      *
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    BQ03T01PARA-BASENAME TO SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"   USING
                                   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  BQ03T01PARA-FULLNAME
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    WRK-PARA-PAFNAME    TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"   USING
                                   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  PRFNAME
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    WRK-PARA-ERRFNAME   TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"   USING
                                   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  ERRFNAME
      *
      *    パラメータファイルオープン
           OPEN    INPUT               BQ01-FILE
      *
           OPEN    OUTPUT              BQ03T01-FILE
           DISPLAY "OUTPUT[" STS-BQ03T01 "]"
           CLOSE   BQ03T01-FILE
           DISPLAY "CLOSE[" STS-BQ03T01 "]"
           OPEN    I-O                 BQ03T01-FILE
           DISPLAY "I-O[" STS-BQ03T01 "]"
      *
      *    パラメータファイル読み込み
           READ    BQ01-FILE
           END-READ
      *
           MOVE    BQ01                TO  WKBQ01
      *
           CLOSE   BQ01-FILE
      *
           INITIALIZE                  BTPARA-REC
           MOVE    WRK-PARA-HOSPNUM    TO  BTPARA-HOSPNUM
           MOVE    WRK-PARA-BTPARA-SHELLID
                                       TO  BTPARA-SHELLID
           MOVE    WRK-PARA-JOBID      TO  BTPARA-JOBID
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "0001"          TO  WRK-BQERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
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
           PERFORM 2001-PTINF-EDIT-SEC
      *
           IF    ( FLG-END         =   ZERO )
               IF        ( WKBQ01-LNKKBN   =   "1" )
                   PERFORM 2001-QLINK-EDIT-SEC
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       2001-PTINF-EDIT-SEC           SECTION.
      *
      *    患者対象データ検索処理
           PERFORM 900-PTSRH-ODR-SEL-SEC
      *
           DISPLAY "FLG-PTSRH   =" FLG-PTSRH   
      *
           PERFORM UNTIL ( FLG-PTSRH   NOT =   ZERO )
                    OR   ( FLG-END     NOT =   ZERO )
      *
               PERFORM 900-BQ03T01-READ-SEC
               DISPLAY "[" FLG-BQ03T01 "][" PTSRH-PTID "]"
               IF    ( FLG-BQ03T01     =   ZERO )
                   CONTINUE
               ELSE
                   INITIALIZE  BQ03T01-REC
                   MOVE    PTSRH-HOSPNUM   TO  BQ03T01-HOSPNUM
                   MOVE    PTSRH-PTID      TO  BQ03T01-PTID
                   WRITE   BQ03T01-REC
      *
                   PERFORM 900-PTINF-KEY-SEL-SEC
      *
                   IF        ( WKBQ01-LNKKBN   =   "1" )
                       IF    ( CNT-PTINF      >=   CONST-PTINF-MAX )
                           MOVE    1           TO  FLG-PTSRH
                       ELSE
                           PERFORM 20011-QTORSUB-EDIT-SEC
                       END-IF
                   ELSE
                       PERFORM 20011-QTOANY-EDIT-SEC
                   END-IF
      *
               END-IF
      *
               IF    ( FLG-PTSRH       =   ZERO )
                   PERFORM 900-PTSRH-ODR-FET-SEC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "odr"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-PTINF-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       20011-QTORSUB-EDIT-SEC          SECTION.
      *
           COMPUTE CNT-PTINF   =   CNT-PTINF   +   1
      *
           INITIALIZE                  QTORSUB-AREA
      *
           MOVE    PTINF-PTID        TO  QTORS-PTID
           MOVE    PTINF-TSTPTNUMKBN TO  QTORS-TSTPTNUMKBN
      *
           INITIALIZE                  SS001-AREA
           MOVE    WRK-PARA-HOSPNUM    TO  SS001-HOSPNUM
           MOVE    PTINF-PTID    TO  SS001-PTID
           MOVE    WKBQ01-SRYYM        TO  SS001-SRYYM
           CALL    "ORCSS001"          USING   SS001-AREA
                                               SPA-AREA
      *
           MOVE    SS001-GIHO-FLG  TO  QTORS-GIHO-FLG
           MOVE    SS001-GRSI-FLG  TO  QTORS-GRSI-FLG
           MOVE    SS001-GJBI-FLG  TO  QTORS-GJBI-FLG
           MOVE    SS001-GJBIR-FLG TO  QTORS-GJBIR-FLG
           MOVE    SS001-GJBIK-FLG TO  QTORS-GJBIK-FLG
           MOVE    SS001-GKGI-FLG  TO  QTORS-GKGI-FLG
           MOVE    SS001-GDI3-FLG  TO  QTORS-GDI3-FLG
           MOVE    SS001-NIHO-FLG  TO  QTORS-NIHO-FLG
           MOVE    SS001-NRSI-FLG  TO  QTORS-NRSI-FLG
           MOVE    SS001-NJBI-FLG  TO  QTORS-NJBI-FLG
           MOVE    SS001-NJBIR-FLG TO  QTORS-NJBIR-FLG
           MOVE    SS001-NJBIK-FLG TO  QTORS-NJBIK-FLG
           MOVE    SS001-NKGI-FLG  TO  QTORS-NKGI-FLG
           MOVE    SS001-NDI3-FLG  TO  QTORS-NDI3-FLG
      *
           IF    ( QTORS-GIHO-FLG      =   1 )
               COMPUTE CNT-GIHO    =   CNT-GIHO    +   1
           END-IF
      *
           IF    ( QTORS-GRSI-FLG      =   1 )
               COMPUTE CNT-GRSI    =   CNT-GRSI    +   1
           END-IF
      *
           IF    ( QTORS-GJBI-FLG      =   1 )
               COMPUTE CNT-GJBI    =   CNT-GJBI    +   1
           END-IF
      *
           IF    ( QTORS-GJBIR-FLG     =   1 )
               COMPUTE CNT-GJBIR   =   CNT-GJBIR    +   1
           END-IF
      *
           IF    ( QTORS-GJBIK-FLG     =   1 )
               COMPUTE CNT-GJBIK   =   CNT-GJBIK    +   1
           END-IF
      *
           IF    ( QTORS-GKGI-FLG      =   1 )
               COMPUTE CNT-GKGI    =   CNT-GKGI    +   1
           END-IF
      *
           IF    ( QTORS-GDI3-FLG      =   1 )
               COMPUTE CNT-GDI3    =   CNT-GDI3    +   1
           END-IF
      *
           IF    ( QTORS-NIHO-FLG      =   1 )
               COMPUTE CNT-NIHO    =   CNT-NIHO    +   1
           END-IF
      *
           IF    ( QTORS-NRSI-FLG      =   1 )
               COMPUTE CNT-NRSI    =   CNT-NRSI    +   1
           END-IF
      *
           IF    ( QTORS-NJBI-FLG      =   1 )
               COMPUTE CNT-NJBI    =   CNT-NJBI    +   1
           END-IF
      *
           IF    ( QTORS-NJBIR-FLG     =   1 )
               COMPUTE CNT-NJBIR   =   CNT-NJBIR    +   1
           END-IF
      *
           IF    ( QTORS-NJBIK-FLG     =   1 )
               COMPUTE CNT-NJBIK   =   CNT-NJBIK    +   1
           END-IF
      *
           IF    ( QTORS-NKGI-FLG      =   1 )
               COMPUTE CNT-NKGI    =   CNT-NKGI    +   1
           END-IF
      *
           IF    ( QTORS-NDI3-FLG      =   1 )
               COMPUTE CNT-NDI3    =   CNT-NDI3    +   1
           END-IF
      *
           PERFORM 200111-BTPARA-COMMON-SEC
      *
           MOVE    2                   TO  BTPARA-DSP-RENNUM
           MOVE    CNT-PTINF           TO  BTPARA-RENNUM
           MOVE    QTORSUB-AREA        TO  BTPARA-INFO-PARA
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "0002"          TO  WRK-BQERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       20011-QTORSUB-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    照会連携データ作成処理
      *****************************************************************
       20011-QTOANY-EDIT-SEC           SECTION.
      *
           COMPUTE CNT-PTINF   =   CNT-PTINF   +   1
      *
           INITIALIZE                  QTOANY-AREA
           MOVE    PTINF-PTID    TO  QTOANY-PTID
           MOVE    PTSRH-PTNUM   TO  QTOANY-PTNUM
      *
           PERFORM 200111-BTPARA-COMMON-SEC
      *
           MOVE    2                   TO  BTPARA-DSP-RENNUM
           MOVE    CNT-PTINF           TO  BTPARA-RENNUM
           MOVE    QTOANY-AREA         TO  BTPARA-INFO-PARA
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "0002"          TO  WRK-BQERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       20011-QTOANY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    バッチパラメタ共通項目編集処理
      *****************************************************************
       200111-BTPARA-COMMON-SEC        SECTION.
      *
           INITIALIZE                  BTPARA-REC
           MOVE    WRK-PARA-HOSPNUM    TO  BTPARA-HOSPNUM
           MOVE    WRK-PARA-BTPARA-SHELLID
                                       TO  BTPARA-SHELLID
           MOVE    WRK-PARA-OPID       TO  BTPARA-RUN-OPID
           MOVE    WRK-PARA-JOBID      TO  BTPARA-JOBID
           MOVE    CONST-SCRIPTID      TO  BTPARA-SCRIPTID
           MOVE    WRK-PARA-TERMID     TO  BTPARA-TERMID
           MOVE    SMCNDATE-YMD        TO  BTPARA-CREYMD
                                           BTPARA-UPYMD
           MOVE    SMCNDATE-HMS        TO  BTPARA-UPHMS
      *
           .
       200111-BTPARA-COMMON-EXT.
           EXIT.
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       2001-QLINK-EDIT-SEC          SECTION.
      *
               INITIALIZE                  QTORMAIN-AREA
      *
               MOVE    WKBQ01-SRYYM        TO  QTORM-SRYYM
               MOVE    WKBQ01-NYUGAIKBN    TO  QTORM-NYUGAIKBN
               MOVE    SMCNDATE-YMD        TO  QTORM-CREYMD
               MOVE    SMCNDATE-HMS        TO  QTORM-CREHMS
               MOVE    CNT-PTINF           TO  QTORM-PTINF-CNT
               MOVE    CNT-GIHO            TO  QTORM-GIHO-CNT
               MOVE    CNT-GRSI            TO  QTORM-GRSI-CNT
               MOVE    CNT-GJBI            TO  QTORM-GJBI-CNT
               MOVE    CNT-GJBIR           TO  QTORM-GJBIR-CNT
               MOVE    CNT-GJBIK           TO  QTORM-GJBIK-CNT
               MOVE    CNT-GKGI            TO  QTORM-GKGI-CNT
               MOVE    CNT-GDI3            TO  QTORM-GDI3-CNT
               MOVE    CNT-NIHO            TO  QTORM-NIHO-CNT
               MOVE    CNT-NRSI            TO  QTORM-NRSI-CNT
               MOVE    CNT-NJBI            TO  QTORM-NJBI-CNT
               MOVE    CNT-NJBIR           TO  QTORM-NJBIR-CNT
               MOVE    CNT-NJBIK           TO  QTORM-NJBIK-CNT
               MOVE    CNT-NKGI            TO  QTORM-NKGI-CNT
               MOVE    CNT-NDI3            TO  QTORM-NDI3-CNT
      *
               PERFORM 200111-BTPARA-COMMON-SEC
      *
               MOVE    1                   TO  BTPARA-DSP-RENNUM
               MOVE    1                   TO  BTPARA-RENNUM
               MOVE    QTORMAIN-AREA       TO  BTPARA-INFO-PARA
               MOVE    BTPARA-REC          TO  MCPDATA-REC
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "0003"          TO  WRK-BQERRCD
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
      *
           IF    ( FLG-END             =   ZERO )
      *
               PERFORM 200111-BTPARA-COMMON-SEC
      *
               MOVE    1                   TO  BTPARA-DSP-RENNUM
               MOVE    2                   TO  BTPARA-RENNUM
               MOVE    BQ01-HYODAI         TO  BTPARA-INFO-PARA
               MOVE    BTPARA-REC          TO  MCPDATA-REC
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "0004"          TO  WRK-BQERRCD
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
           IF    ( FLG-END             =   ZERO )
      *
               PERFORM 200111-BTPARA-COMMON-SEC
      *
               MOVE    1                   TO  BTPARA-DSP-RENNUM
               MOVE    3                   TO  BTPARA-RENNUM
               MOVE    BQ01-JYOKEN1        TO  BTPARA-INFO-PARA
               MOVE    BTPARA-REC          TO  MCPDATA-REC
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "0005"          TO  WRK-BQERRCD
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
           IF    ( FLG-END             =   ZERO )
      *
               PERFORM 200111-BTPARA-COMMON-SEC
      *
               MOVE    1                   TO  BTPARA-DSP-RENNUM
               MOVE    4                   TO  BTPARA-RENNUM
               MOVE    BQ01-JYOKEN2        TO  BTPARA-INFO-PARA
               MOVE    BTPARA-REC          TO  MCPDATA-REC
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "0006"          TO  WRK-BQERRCD
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
           .
       2001-QLINK-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   BQERR-FILE
           IF      STS-BQERR           =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT              BQERR-FILE
      *
               MOVE    "更新処理に失敗しました"
                                       TO  BQERR-REC
               WRITE   BQERR-REC
               CLOSE   BQERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-HOSPNUM TO JOB-HOSPNUM
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    "更新処理に失敗しました"
                                       TO  JOB-YOBI
               MOVE    WRK-BQERRCD     TO  JOB-ERRCD
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           END-IF
      *
           MOVE    1               TO  FLG-END
                                       FLG-ERR
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           CLOSE   BQ03T01-FILE
      *
           MOVE    ZERO        TO  IF-RESULT
           MOVE    BQ03T01PARA TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           PERFORM 900-PTSRH-DEL1-SEC
      *
           PERFORM 900-JOB-STEP-END-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索処理
      *****************************************************************
       900-PTSRH-ODR-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-PTSRH
      *
           INITIALIZE                      PTSRH-REC
           MOVE    SPA-HOSPNUM         TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID          TO  PTSRH-TERMID
           MOVE    "2"                 TO  PTSRH-DATAKBN
           MOVE    WKBQ01-SQL          TO  PTSRH-SQL
           MOVE    "%"                 TO  PTSRH-UPYMD
                                           PTSRH-UPHMS
           MOVE    PTSRH-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "odr"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTSRH
               MOVE    MCPDATA-REC     TO  PTSRH-REC
           ELSE
               MOVE    1               TO  FLG-PTSRH
               INITIALIZE                  PTSRH-REC
           END-IF
      *
           .
       900-PTSRH-ODR-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会FETCH処理
      *****************************************************************
       900-PTSRH-ODR-FET-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-PTSRH
      *
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "odr"               TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTSRH
               MOVE    MCPDATA-REC     TO  PTSRH-REC
           ELSE
               MOVE    1               TO  FLG-PTSRH
               INITIALIZE                  PTSRH-REC
           END-IF
      *
           .
       900-PTSRH-ODR-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    PTSRH-PTID      TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               INITIALIZE              PTINF-REC
               MOVE    1           TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会削除処理
      *****************************************************************
       900-PTSRH-DEL1-SEC              SECTION.
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    "2"             TO  PTSRH-DATAKBN
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-PTSRH-DEL1-EXT.
           EXIT.
      *****************************************************************
      *    一時ファイル（患者重複チェック）読込処理
      *****************************************************************
       900-BQ03T01-READ-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-BQ03T01
      *
           INITIALIZE                  BQ03T01-REC
           MOVE    PTSRH-HOSPNUM   TO  BQ03T01-HOSPNUM
           MOVE    PTSRH-PTID      TO  BQ03T01-PTID
           READ    BQ03T01-FILE
           INVALID
               MOVE    1           TO  FLG-BQ03T01
           END-READ
           DISPLAY "STS-BQ03T01[" STS-BQ03T01 "]"
      *
           .
       900-BQ03T01-READ-EXT.
           EXIT.
      *****************************************************************
      *    ステップ管理開始処理
      *****************************************************************
       900-JOB-STEP-START-SEC      SECTION.
      *
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-HOSPNUM TO JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID   TO  JOB-SHELLID
           MOVE    "連携データ作成"
                                   TO  JOB-SHELLMSG
           MOVE    "ORCBQ02"       TO  JOB-PGID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           .
       900-JOB-STEP-START-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       900-JOB-STEP-END-SEC            SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-HOSPNUM TO JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID   TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID TO  JOB-SHELLID
           MOVE    CNT-PTINF       TO  JOB-UPDCNT
      *
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           .
       900-JOB-STEP-END-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　接続処理
      *****************************************************************
       120-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       120-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　切断処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
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
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
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
       911-DBSELECT-EXT.
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
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
