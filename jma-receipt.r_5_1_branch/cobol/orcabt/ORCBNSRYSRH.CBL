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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBNSRYSRH.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次帳票
      *  コンポーネント名  : 入院診療行為照会テーブル作成処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/01/23    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    入院会計ファイル
           SELECT  TMP01-FILE      ASSIGN  TMP01PARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-TMP01.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
      *
       FILE                        SECTION.
      *
      *    入院会計ファイル
       FD  TMP01-FILE.
       01  TMP01-REC.
           COPY    "CPNYUINACCT.INC"
                                   REPLACING   //NACCT-//
                                   BY          //TMP01-//.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "COMMON-SPA".
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING   //RECE01//
                                   BY          //TMP01//.
      *
           COPY    "CPRECEERR.INC".
      *
       01  STS-AREA.
           03  STS-TMP01                    PIC X(02).
           03  STS-RECEERR                 PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-PTNUM                   PIC 9(01).
           03  FLG-TMP01                   PIC 9(01).
      *
       01  CNT-AREA.
           03  CNT-UPD                     PIC 9(05).
      *
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-RECEERR                 PIC X(200).
           03  WRK-STSRYYM                 PIC X(06).
           03  WRK-EDSRYYM                 PIC X(06).
           03  WRK-YMD                     PIC X(10). 
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-TIME.
               05  WRK-TIME-HH             PIC 9(02).
               05  WRK-TIME-MM             PIC 9(02).
               05  WRK-TIME-SS             PIC 9(02).
           03  WRK-EDTYMD1                 PIC X(10).
           03  WRK-EDTYMD3                 PIC X(22).
      *
       01  KEY-AREA.
           03  KEY-SRYYM                   PIC X(06).
      *
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC X(02).
           03  WRK-PARA-SRYYM          PIC X(06).
           03  WRK-PARA-PTNUM          PIC X(20).
      *
      *
      *01  CONST-AREA.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    入院診療行為照会テーブル
           COPY  "CPORCSS010.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    マシン日付
           COPY    "CPORCSMCNDATE.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(256).
      *
      ****************************************************************
       PROCEDURE                   DIVISION
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
           IF    ( FLG-END     =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA
                                       IDX-AREA
                                       CNT-AREA
                                       FLG-AREA
                                       WRK-AREA
                                       SPA-AREA
      *
           INITIALIZE                  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
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
                                       WRK-PARA-JOBID
                                       WRK-PARA-SHELLID
                                       WRK-PARA-HOSPNUM
                                       RECEERR
                                       WRK-PARA-SRYYM
                                       WRK-PARA-PTNUM
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    "NSRYSRH"       TO  TMP01PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  TMP01PARA-TERMID
           MOVE    SPA-HOSPNUM     TO  TMP01PARA-HOSPNUM
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    TMP01PARA-BASENAME
                                   TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  TMP01PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                   TO  RECEERR
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    "ORCBNSRYSRH"   TO  JOB-PGID
           MOVE    "入院診療行為照会情報作成"
                                   TO  JOB-SHELLMSG
           PERFORM 800-ORCSJOB-SEC
      *
           MOVE    "STP"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    3               TO  JOB-STOPFLG
           PERFORM 800-ORCSJOB-SEC
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
           PERFORM 800-MCNDATE-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
           IF    ( WRK-PARA-SRYYM  =   SPACE )
               MOVE    "000000"    TO  WRK-STSRYYM
           ELSE
               MOVE    WRK-PARA-SRYYM
                                   TO  WRK-STSRYYM
           END-IF
      *
           IF    ( WRK-PARA-PTNUM  =   SPACE )
               CONTINUE
           ELSE
               PERFORM 900-PTNUM-KEY4-SEL-SEC
           END-IF
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF    ( WRK-PARA-PTNUM  =   SPACE )
               PERFORM 2001-IKKATU-SEC
           ELSE
               PERFORM 2001-KOBETU-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括処理
      *****************************************************************
       2001-IKKATU-SEC                 SECTION.
      *
           OPEN    OUTPUT  TMP01-FILE
      *
           PERFORM 900-NYUINACCT-KEY41-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               MOVE    NYUINACCT-REC   TO  TMP01-REC
               WRITE   TMP01-REC
      *
               PERFORM 900-NYUINACCT-KEY41-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   TMP01-FILE
      *
           OPEN    INPUT   TMP01-FILE
      *
           PERFORM 900-TMP01-READ-SEC
      *
           PERFORM UNTIL ( FLG-TMP01   NOT =   ZERO )
      *
               PERFORM 20011-SYORI-KEIKA-SEC
      *
               MOVE    NACCT-SRYYM     TO  KEY-SRYYM
               PERFORM UNTIL ( FLG-TMP01       NOT =   ZERO )
                        OR   ( NACCT-SRYYM     NOT =   KEY-SRYYM )
      *
                   PERFORM 20011-ORCSS010-SEC
      *
                   PERFORM 900-TMP01-READ-SEC
               END-PERFORM
           END-PERFORM
      *
           CLOSE   TMP01-FILE
      *
           .
       2001-IKKATU-EXT.
           EXIT.
      *****************************************************************
      *    個別処理
      *****************************************************************
       2001-KOBETU-SEC                 SECTION.
      *
           PERFORM 900-NYUINACCT-KEY40-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               PERFORM 20011-ORCSS010-SEC
      *
               PERFORM 900-NYUINACCT-KEY40-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key40"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-KOBETU-EXT.
           EXIT.
      *****************************************************************
      *    処理経過編集処理
      *****************************************************************
       20011-SYORI-KEIKA-SEC           SECTION.
      *
           ACCEPT  WRK-TIME        FROM    TIME
      *
           MOVE    NACCT-SRYYM         TO  WRK-YMD
           MOVE    "01"                TO  WRK-YMD(7:2)
           PERFORM 800-SEIWA-HEN-SEC
      *
      *    ステップ途中処理
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           STRING  WRK-EDTYMD3 (1:16)      DELIMITED BY SIZE
                   "分　"                  DELIMITED BY SPACE
                   WRK-TIME-HH             DELIMITED BY SIZE
                   "."                     DELIMITED BY SIZE
                   WRK-TIME-MM             DELIMITED BY SIZE
                   "."                     DELIMITED BY SIZE
                   WRK-TIME-SS             DELIMITED BY SIZE
           INTO    JOB-YOBI
           END-STRING
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBNSRYSRH"       TO  JOB-PGID
           MOVE    "入院診療行為照会情報作成"
                                       TO  JOB-SHELLMSG
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           PERFORM 800-ORCSJOB-SEC
      *
           .
       20011-SYORI-KEIKA-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為照会テーブル編集サブ呼出
      *****************************************************************
       20011-ORCSS010-SEC              SECTION.
      *
           INITIALIZE                      SS010-AREA
           MOVE    NACCT-PTID          TO  SS010-PTID
           MOVE    NACCT-SRYYM         TO  SS010-SRYYM
           CALL    "ORCSS010"      USING   SS010-AREA
                                           SPA-AREA
           IF    ( SS010-RC            =   ZERO )
               COMPUTE CNT-UPD =   CNT-UPD +   1
           ELSE
               DISPLAY "ORCSS010 ERR [" SS010-RC "] "
                       NACCT-PTID " " NACCT-SRYYM
           END-IF
      *
           .
       20011-ORCSS010-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    TMP01PARA       TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *    MOVE    CNT-UPD         TO  JOB-UPDCNT
           PERFORM 800-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-EDTYMD1
                                           WRK-EDTYMD3
      *
           IF    ( WRK-YMD            =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-YMD             TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
               INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "　"
           END-IF
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC                 SECTION.
      *
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           .
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理サブ
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
      *
       800-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索
      *****************************************************************
       900-NYUINACCT-KEY40-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    PTNUM-PTID          TO  NACCT-PTID
           MOVE    WRK-STSRYYM         TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key40"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-KEY40-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計FETCH
      *****************************************************************
       900-NYUINACCT-KEY40-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key40"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-KEY40-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索
      *****************************************************************
       900-NYUINACCT-KEY41-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    WRK-STSRYYM         TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-KEY41-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計FETCH
      *****************************************************************
       900-NYUINACCT-KEY41-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-KEY41-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者番号検索処理(KEY4)
      *****************************************************************
       900-PTNUM-KEY4-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    WRK-PARA-PTNUM      TO  PTNUM-PTNUM
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTNUM
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計ファイル
      *****************************************************************
       900-TMP01-READ-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-TMP01
      *
           READ    TMP01-FILE
           AT  END
               MOVE    1           TO  FLG-TMP01
           NOT AT  END
               MOVE    TMP01-REC   TO  NYUINACCT-REC
           END-READ
      *
           .
       900-TMP01-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
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
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC            SECTION.
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
       910-DBFETCH-SEC             SECTION.
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
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       910-DBINSERT-EXT.
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
           .
       900-DBDISCONNECT-EXT.
           EXIT.
