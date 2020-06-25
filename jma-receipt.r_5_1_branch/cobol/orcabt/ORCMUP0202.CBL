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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCMUP0202.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＤＢ作成処理
      *  コンポーネント名  : 入院レセプト記載略称更新処理
      *                    : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  11.02.15    NACL-森脇     新規作成
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
      *    入力：入院レセプト記載略称更新データ
           SELECT  IN-FILE         ASSIGN
                                   ASS-IN
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-IN.
      *    出力：エラーファイル
           SELECT  ERR-FILE        ASSIGN
                                   ASS-ERR
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-ERR.
      *    出力：プルーフファイル
           SELECT  PLF-FILE        ASSIGN
                                   ASS-PLF
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PLF.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    入力：入院レセプト記載略称更新データ
       FD  IN-FILE.
       01  IN-REC                      PIC X(50200).
      *
      *    出力：エラーファイル
       FD  ERR-FILE.
       01  ERR-RYAKUSHOWK-REC.
           03  ERR-RYAKUSHOWK-SYORIKBN PIC  X(01).
           03  ERR-RYAKUSHOWK-YUKOEDYMD2
                                       PIC X(08).
           COPY    "CPNYUINRYAKUSHO.INC"
                                       REPLACING  //NYURYAKUSHO//
                                              BY  //ERR-RYAKUSHOWK//.
      *
      *    出力：プルーフファイル
       FD  PLF-FILE.
       01  PLF-REC                     PIC X(1000).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "COMMON-SPA".
      *
      *    ファイル指定領域
       01  ASS-AREA.
           03  ASS-IN                  PIC X(256).
           03  ASS-ERR                 PIC X(256).
           03  ASS-PLF                 PIC X(256).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-IN                  PIC X(02).
           03  STS-ERR                 PIC X(02).
           03  STS-PLF                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-IN                  PIC 9(01).
           03  FLG-RYAKUSHO            PIC 9(01).
           03  FLG-PLF                 PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDX-PNT1                PIC 9(05).
           03  IDX-PNT2                PIC 9(05).
           03  IDX-PNT3                PIC 9(05).
           03  IDX                     PIC 9(03).
           03  IDY                     PIC 9(03).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-INS                 PIC 9(06).
           03  CNT-UPD                 PIC 9(06).
           03  CNT-DEL                 PIC 9(06).
           03  CNT-ERR                 PIC 9(06).
           03  CNT-PLF                 PIC 9(06).
           03  CNT-MAE                 PIC 9(03).
           03  CNT-ATO                 PIC 9(03).
      *
       01  SYS-DATE.
           03  SYS-YY                  PIC 9(02).
           03  SYS-MM                  PIC 9(02).
           03  SYS-DD                  PIC 9(02).
      *
       01  SYS-TIME                    PIC 9(08).
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYMD2.
               05  WRK-SYY2            PIC 9(02).
               05  WRK-SMM2            PIC 9(02).
               05  WRK-SDD2            PIC 9(02).
           03  WRK-ERRCD               PIC 9(01).
      *
           03  WRK-IN                  PIC X(01).
           03  WRK-HOMEDIR             PIC X(128).
      *
      *    出力ファイル領域
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(26) VALUE
                                       "【PGID:ORCMUP0202.CBL】".
               05  MES-TITLE1-YY       PIC X(04) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-MM       PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-DD       PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(02) VALUE SPACE.
               05  MES-TITLE1-HOSPNUM  PIC 9(02).
      *
       01  WRK-MSG-AREA.
           03  WRK-MSG-01              PIC X(30).
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  WRK-SYORIKBN            PIC X(01).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-SRYCD               PIC X(09).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-RYAKUSHO            PIC X(20).
      *
       01  CONST-AREA.
           03  CONST-HOMEDIR           PIC X(28)  VALUE
               "/var/lib/jma-receipt/master/".
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
           COPY    "ENUM-VALUE".
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
       01  RYAKUSHOWK-REC.
           03  RYAKUSHOWK-SYORIKBN     PIC X(01).
           03  RYAKUSHOWK-YUKOEDYMD2   PIC X(08).
           COPY    "CPNYUINRYAKUSHO.INC"
                                       REPLACING   //NYURYAKUSHO//
                                              BY   //RYAKUSHOWK//.
      *    入院レセプト記載略称
       01  RYAKUSHO-REC.
           COPY    "CPNYUINRYAKUSHO.INC"
                                       REPLACING   //NYURYAKUSHO//
                                              BY   //RYAKUSHO//.
      *
      *    ジョブ管理情報
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPMUPLINK.INC".
      *
       PROCEDURE                   DIVISION
           USING
           LNK-AREA.
      *
      ****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL  (FLG-IN          =   1)
                     OR   (LNK-RC      NOT =   ZERO)
      *
           PERFORM 300-END-SEC
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    ZERO            TO  FLG-AREA
           MOVE    ZERO            TO  CNT-AREA
           INITIALIZE                  WRK-AREA
                                       SPA-AREA
                                       IDX-AREA
      *
           MOVE    LNK-HOSPNUM     TO  SPA-HOSPNUM
      *
           ACCEPT  SYS-DATE        FROM    DATE
           COMPUTE WRK-SYY         =   SYS-YY    +   2000
           MOVE    SYS-MM          TO  WRK-SMM
           MOVE    SYS-DD          TO  WRK-SDD
           ACCEPT  SYS-TIME        FROM    TIME
      *
      *    DB OPEN
      *    PERFORM 100-DBOPEN-SEC
      *
           PERFORM 101-ASSIN-GET-SEC
      *
      *    入力　初期処理
           OPEN    INPUT               IN-FILE
           IF      STS-IN       NOT =   ZERO
               MOVE    1               TO  FLG-IN
               DISPLAY "*(ORCMUP0202)* IN-FILE OPN STS[" STS-IN "]"
           END-IF
      *
      *    出力ファイル　初期処理
           PERFORM 2700-OUTLST-INIT-SEC
      *
      *    入院レセプト記載略称データ読込み処理
           IF      FLG-IN          =   ZERO
               PERFORM 900-IN-KENSAKU-SEC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    ファイル情報取得
      *****************************************************************
       101-ASSIN-GET-SEC           SECTION.
      *
           INITIALIZE                      SGETTEMP-AREA
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
      *
           IF      SGETTEMP-MW         =   SPA-PANDA
               MOVE    CONST-HOMEDIR       TO  WRK-HOMEDIR
           ELSE
               MOVE    SGETTEMP-TEMPDIR    TO  WRK-HOMEDIR
           END-IF
      *
      *    入力ファイル
           MOVE    SPACE           TO  ASS-IN
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   SPA-HOSPNUM         DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   LNK-DBFILE          DELIMITED  BY  SPACE
                   "/"                 DELIMITED  BY  SIZE
                   LNK-UPFILE          DELIMITED  BY  SPACE
                                       INTO    ASS-IN
           END-STRING
      *
      *    エラーファイル
           MOVE    SPACE           TO  ASS-ERR
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   SPA-HOSPNUM         DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   "orca-mstlog/"      DELIMITED  BY  SIZE
                   LNK-UPFILE          DELIMITED  BY  SPACE
                   ".ERR"              DELIMITED  BY  SIZE
                                       INTO    ASS-ERR
           END-STRING
      *
      *    プルーフファイル
           MOVE    SPACE           TO  ASS-PLF
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   SPA-HOSPNUM         DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   "orca-mstlog/"      DELIMITED  BY  SIZE
                   LNK-UPFILE          DELIMITED  BY  SPACE
                   ".PRF"              DELIMITED  BY  SIZE
                                       INTO    ASS-PLF
           END-STRING
      *
           .
       101-ASSIN-GET-EXT.
           EXIT.
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO            TO  WRK-ERRCD
      *
           EVALUATE    RYAKUSHOWK-SYORIKBN
      *        追加
               WHEN    "I"
                   PERFORM 2010-INSERT-SYORI-SEC
      *        更新
               WHEN    "U"
                   PERFORM 2020-UPDATE-SYORI-SEC
      *        削除
               WHEN    "D"
                   PERFORM 2030-DELETE-SYORI-SEC
           END-EVALUATE
      *
      *    入院レセプト記載略称データ読込み処理
           PERFORM 900-IN-KENSAKU-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加  処理
      *****************************************************************
       2010-INSERT-SYORI-SEC       SECTION.
      *
      *    単純追加
           IF      RYAKUSHOWK-YUKOEDYMD2
                                   =   SPACE
               PERFORM 2011-INSERT-SYORI-SEC
           ELSE
               PERFORM 2012-INSERT-SYORI-SEC
           END-IF
      *
           .
       2010-INSERT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    単純追加  処理
      *****************************************************************
       2011-INSERT-SYORI-SEC       SECTION.
      *
           INITIALIZE                  RYAKUSHO-REC
           MOVE    RYAKUSHOWK-KEY  TO  RYAKUSHO-KEY
           MOVE    RYAKUSHO-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_nyuinryakusho_master"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinryakusho_master"
                                       TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM  910-RYAKUSHO-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RYAKUSHO
           END-IF
      *
           MOVE    "tbl_nyuinryakusho_master"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    追加エラー（同一キー存在）
           IF      FLG-RYAKUSHO    =   ZERO
               MOVE    1               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           ELSE
      *        ＤＢ追加
               PERFORM 20101-RYAKUSHO-INS-SEC
           END-IF
      *
           .
       2011-INSERT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    改正追加  処理
      *****************************************************************
       2012-INSERT-SYORI-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYORI
      *
           INITIALIZE                  RYAKUSHO-REC
           MOVE    RYAKUSHOWK-KEY  TO  RYAKUSHO-KEY
           MOVE    RYAKUSHO-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_nyuinryakusho_master"
                                   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_nyuinryakusho_master"
                                       TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-RYAKUSHO-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RYAKUSHO
           END-IF
      *
           PERFORM UNTIL  (FLG-RYAKUSHO    =   1)
                     OR   (FLG-SYORI       =   1)
               IF     (RYAKUSHO-YUKOSTYMD <=  RYAKUSHOWK-YUKOEDYMD2)
                 AND  (RYAKUSHO-YUKOEDYMD >=  RYAKUSHOWK-YUKOEDYMD2)
      *            更新
                   MOVE    RYAKUSHO-KEY    TO  RYAKUSHO-UP-KEY
                   MOVE    RYAKUSHOWK-YUKOEDYMD2
                                           TO  RYAKUSHO-YUKOEDYMD
                   PERFORM 20101-RYAKUSHO-UPD-SEC
      *            ＤＢ追加
                   PERFORM 20101-RYAKUSHO-INS-SEC
                   MOVE    1               TO  FLG-SYORI
               ELSE
                   MOVE    "tbl_nyuinryakusho_master"
                                           TO  MCP-TABLE
                   MOVE    "key2"          TO  MCP-PATHNAME
                   PERFORM 910-RYAKUSHO-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_nyuinryakusho_master"
                                   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF      FLG-SYORI       =   ZERO
      *        ＤＢ追加
               PERFORM 20101-RYAKUSHO-INS-SEC
           END-IF
      *
           .
       2012-INSERT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ更新  処理
      *****************************************************************
       2020-UPDATE-SYORI-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYORI
      *
           INITIALIZE                  RYAKUSHO-REC
           MOVE    RYAKUSHOWK-KEY  TO  RYAKUSHO-KEY
           MOVE    RYAKUSHO-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_nyuinryakusho_master"
                                   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_nyuinryakusho_master"
                                       TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM  910-RYAKUSHO-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RYAKUSHO
           END-IF
      *
           PERFORM UNTIL  (FLG-RYAKUSHO    =   1)
                     OR   (FLG-SYORI       =   1)
               IF     (RYAKUSHO-YUKOSTYMD  <=  RYAKUSHOWK-YUKOSTYMD)
                 AND  (RYAKUSHO-YUKOEDYMD  >=  RYAKUSHOWK-YUKOEDYMD)
      *            ＤＢ更新
                   MOVE    RYAKUSHO-KEY    TO  RYAKUSHO-UP-KEY
                   MOVE    RYAKUSHOWK-KEY  TO  RYAKUSHO-KEY
                   MOVE    RYAKUSHOWK-RYAKUSHO
                                           TO  RYAKUSHO-RYAKUSHO
                   MOVE    RYAKUSHOWK-NAME TO  RYAKUSHO-NAME
                   MOVE    RYAKUSHOWK-TERMID
                                           TO  RYAKUSHO-TERMID
                   MOVE    RYAKUSHOWK-OPID TO  RYAKUSHO-OPID
                   PERFORM 20101-RYAKUSHO-UPD-SEC
                   MOVE    1               TO  FLG-SYORI
               ELSE
                   MOVE    "tbl_nyuinryakusho_master"
                                           TO  MCP-TABLE
                   MOVE    "key2"          TO  MCP-PATHNAME
                   PERFORM  910-RYAKUSHO-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_nyuinryakusho_master"
                                   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF      FLG-SYORI       =   ZERO
               MOVE    5               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
      *
           .
       2020-UPDATE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除  処理
      *****************************************************************
       2030-DELETE-SYORI-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYORI
      *
           INITIALIZE                  RYAKUSHO-REC
           MOVE    RYAKUSHOWK-KEY  TO  RYAKUSHO-KEY
           MOVE    RYAKUSHO-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_nyuinryakusho_master"
                                   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_nyuinryakusho_master"
                                       TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM  910-RYAKUSHO-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RYAKUSHO
           END-IF
      *
           PERFORM UNTIL  (FLG-RYAKUSHO    =   1)
                     OR   (FLG-SYORI       =   1)
               IF     (RYAKUSHO-YUKOSTYMD  <=  RYAKUSHOWK-YUKOSTYMD)
                 AND  (RYAKUSHO-YUKOEDYMD  >=  RYAKUSHOWK-YUKOEDYMD)
      *            ＤＢ削除
                   PERFORM 20101-RYAKUSHO-DEL-SEC
                   MOVE    1               TO  FLG-SYORI
               ELSE
                   MOVE    "tbl_nyuinryakusho_master"
                                           TO  MCP-TABLE
                   MOVE    "key2"          TO  MCP-PATHNAME
                   PERFORM  910-RYAKUSHO-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_nyuinryakusho_master"
                                   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF      FLG-SYORI       =   ZERO
               MOVE    6               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
      *
           .
       2030-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ追加  処理
      *****************************************************************
       20101-RYAKUSHO-INS-SEC      SECTION.
      *
           INITIALIZE                  RYAKUSHO-REC
           MOVE    RYAKUSHOWK-REC(10:)
                                   TO  RYAKUSHO-REC
           MOVE    LNK-MNTYMD      TO  RYAKUSHO-CREYMD
                                       RYAKUSHO-UPYMD
           MOVE    LNK-MNTHMS      TO  RYAKUSHO-UPHMS
           MOVE    RYAKUSHO-REC    TO  MCPDATA-REC
           MOVE    "DBINSERT"      TO  MCP-FUNC
           MOVE    "tbl_nyuinryakusho_master"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    1               TO  LNK-RC
               MOVE    2               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           IF      WRK-ERRCD       =   ZERO
               ADD     1               TO  CNT-INS
           END-IF
      *
           .
       20101-RYAKUSHO-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ更新  処理
      *****************************************************************
       20101-RYAKUSHO-UPD-SEC      SECTION.
      *
           MOVE    LNK-MNTYMD      TO  RYAKUSHO-UPYMD
           MOVE    LNK-MNTHMS      TO  RYAKUSHO-UPHMS
           MOVE    RYAKUSHO-REC    TO  MCPDATA-REC
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           MOVE    "tbl_nyuinryakusho_master"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    2               TO  LNK-RC
               MOVE    3               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           IF      WRK-ERRCD           =   ZERO
               ADD     1               TO  CNT-UPD
           END-IF
      *
           .
       20101-RYAKUSHO-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ削除  処理
      *****************************************************************
       20101-RYAKUSHO-DEL-SEC      SECTION.
      *
           MOVE    RYAKUSHO-REC    TO  MCPDATA-REC
           MOVE    "DBDELETE"      TO  MCP-FUNC
           MOVE    "tbl_nyuinryakusho_master"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    3               TO  LNK-RC
               MOVE    4               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           IF      WRK-ERRCD           =   ZERO
               ADD     1               TO  CNT-DEL
           END-IF
      *
           .
       20101-RYAKUSHO-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー処理
      *****************************************************************
       2040-ERR-SYORI-SEC          SECTION.
      *
      *    エラー出力
           MOVE    RYAKUSHOWK-REC  TO  ERR-RYAKUSHOWK-REC
           WRITE   ERR-RYAKUSHOWK-REC
           ADD     1               TO  CNT-ERR
      *
      *    エラーメッセージ出力
           PERFORM 20401-PLF-OUT-SEC
      *
           .
       2040-ERR-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ出力処理
      *****************************************************************
       20401-PLF-OUT-SEC           SECTION.
      *
           MOVE    RYAKUSHOWK-SYORIKBN
                                   TO  WRK-SYORIKBN
           MOVE    RYAKUSHOWK-SRYCD
                                   TO  WRK-SRYCD
           MOVE    RYAKUSHOWK-RYAKUSHO
                                   TO  WRK-RYAKUSHO
      *
           EVALUATE    WRK-ERRCD
               WHEN    1
                   MOVE  "入院レセ記載略称マスタ登録済み"
                                           TO  WRK-MSG-01
               WHEN    2
                   MOVE  "追加エラー"      TO  WRK-MSG-01
               WHEN    3
                   MOVE  "更新エラー"      TO  WRK-MSG-01
               WHEN    4
                   MOVE  "削除エラー"      TO  WRK-MSG-01
               WHEN    5
                   MOVE  "更新対象なし"    TO  WRK-MSG-01
               WHEN    6
                   MOVE  "削除対象なし"    TO  WRK-MSG-01
           END-EVALUATE
      *
           MOVE    WRK-MSG-AREA    TO  PLF-REC
           WRITE                       PLF-REC
           ADD     1               TO  CNT-PLF
      *
           .
       20401-PLF-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力ファイル　初期処理
      *****************************************************************
       2700-OUTLST-INIT-SEC        SECTION.
      *
           OPEN    OUTPUT              ERR-FILE
           IF      STS-ERR     NOT =   ZERO
               DISPLAY "*(ORCMUP0202)* OUTFILE OPN STS[" STS-ERR "]"
               GO  TO  2700-OUTLST-INIT-EXT
           END-IF
      *
      *    エラーレコード出力ファイル　オープン
           OPEN    OUTPUT              PLF-FILE
           IF      STS-PLF     NOT =   ZERO
               DISPLAY "*(ORCMUP0202)* PLFFILE OPN STS[" STS-PLF "]"
               GO  TO  2700-OUTLST-INIT-EXT
           END-IF
      *
           MOVE    WRK-SYY         TO  MES-TITLE1-YY
           MOVE    SYS-MM          TO  MES-TITLE1-MM
           MOVE    SYS-DD          TO  MES-TITLE1-DD
           MOVE    SPA-HOSPNUM     TO  MES-TITLE1-HOSPNUM
           MOVE    MES-TITLE1      TO  PLF-REC
           WRITE   PLF-REC
      *
           .
       2700-OUTLST-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    DB DISCONNECT
      *    PERFORM 900-DBDISCONNECT-SEC
      *
      *    出力ファイル　終了処理
           CLOSE                       IN-FILE
           CLOSE                       ERR-FILE
           CLOSE                       PLF-FILE
      *
           IF      LNK-RC          =   ZERO
               IF      CNT-IN      NOT =   LNK-COUNT
                   MOVE    99              TO  LNK-RC
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    1               TO  JOB-HOSPNUM
                   MOVE    "tbl_jobkanri"  TO  MCP-TABLE
                   MOVE    "abort"         TO  MCP-PATHNAME
                   MOVE    JOBKANRI-REC    TO  MCPDATA-REC
                   MOVE    "DBSELECT"      TO  MCP-FUNC
                   PERFORM 900-ORCDBMAIN-SEC
               END-IF
           END-IF
      *
           DISPLAY "*(ORCMUP0202)* INPUT  /I   CNT[" CNT-IN  "]"
           DISPLAY "*(ORCMUP0202)*  INSERT     CNT[" CNT-INS "]"
           DISPLAY "*(ORCMUP0202)*  UPDATE     CNT[" CNT-UPD "]"
           DISPLAY "*(ORCMUP0202)*  DELETE     CNT[" CNT-DEL "]"
           DISPLAY "*(ORCMUP0202)* ERR    /O   CNT[" CNT-ERR "]"
           DISPLAY "*(ORCMUP0202)* PLF    /O   CNT[" CNT-PLF "]"
           DISPLAY "*(ORCMUP0202)* RC             [" LNK-RC "]"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *     入院レセプト記載略称読込み処理
      *****************************************************************
       900-IN-KENSAKU-SEC          SECTION.
      *
          READ    IN-FILE
              AT  END
                  MOVE    1                TO  FLG-IN
              NOT AT  END
                  MOVE    ZERO             TO  FLG-IN
                  ADD     1                TO  CNT-IN
          END-READ
      *
          IF      FLG-IN           =   ZERO
              MOVE    SPACE            TO  RYAKUSHOWK-REC
              MOVE    IN-REC           TO  RYAKUSHOWK-REC
          END-IF
      *
          .
       900-IN-KENSAKU-EXT.
          EXIT.
      *
      *****************************************************************
      *    入院レセプト記載略称読込
      *****************************************************************
       910-RYAKUSHO-READ-SEC    SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  RYAKUSHO-REC
               MOVE    ZERO            TO  FLG-RYAKUSHO
           ELSE
               INITIALIZE                  RYAKUSHO-REC
               MOVE    1               TO  FLG-RYAKUSHO
           END-IF
      *
           .
       910-RYAKUSHO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-TBL-CLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-TBL-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC           SECTION.
      *
           CALL    "ORCDBMAIN"     USING
                                   MCPAREA
                                   MCPDATA-REC
                                   SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
