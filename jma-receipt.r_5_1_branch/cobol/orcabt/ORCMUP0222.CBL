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
       PROGRAM-ID.                 ORCMUP0222.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＤＢ作成処理
      *  コンポーネント名  : 電子点数表更新処理
      *                    : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  11.02.08    NACL-森脇     新規作成
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
      *    入力：電子点数更新データ
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
      *    入力：電子点数更新データ
       FD  IN-FILE.
       01  IN-REC                      PIC X(50200).
      *
      *    出力：エラーファイル
       FD  ERR-FILE.
       01  ERR-ETENSUWK-REC.
           03  ERR-ETENSUWK-TBLKBN     PIC 9(02).
           03  ERR-ETENSUWK-SYORIKBN   PIC X(01).
           03  ERR-ETENSUWK-YUKOEDYMD2 PIC X(08).
           03  ERR-ETENSUWK-NAIYO      PIC X(1000).
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
           03  FLG-ETENSU              PIC 9(01).
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
      *
           03  WRK-TABLE               PIC X(64).
           03  WRK-PATHNAME            PIC X(64).
           03  WRK-HOMEDIR             PIC X(128).
      *
      *    出力ファイル領域
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(26) VALUE
                                       "【PGID:ORCMUP0222.CBL】".
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
           03  WRK-TBLKBN              PIC 9(02).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-SYORIKBN            PIC X(01).
      *
       01  CONST-AREA.
           03  CONST-HOMEDIR           PIC X(28)  VALUE
               "/var/lib/jma-receipt/master/".
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
       01  ETENSUWK-REC.
           03  ETENSUWK-TBLKBN         PIC 9(02).
           03  ETENSUWK-SYORIKBN       PIC X(01).
           03  ETENSUWK-YUKOEDYMD2     PIC X(08).
           03  ETENSUWK-NAIYO          PIC X(1000).
      *
       01  ETENSU1WK-REC.
           COPY    "CPETENSU1.INC"     REPLACING  //ETENSU1//
                                              BY  //ETENSU1WK//.
       01  ETENSU2WK-REC.
           COPY    "CPETENSU2.INC"     REPLACING  //ETENSU2//
                                              BY  //ETENSU2WK//.
       01  ETNSSMPWK-REC.
           COPY    "CPETENSU2-SAMPLE.INC"
                                       REPLACING  //ETNSSMP//
                                              BY  //ETNSSMPWK//.
       01  ETENSU3WK-REC.
           COPY    "CPETENSU3.INC"     REPLACING  //ETENSU3//
                                              BY  //ETENSU3WK//.
       01  ETENSU4WK-REC.
           COPY    "CPETENSU4.INC"     REPLACING  //ETENSU4//
                                              BY  //ETENSU4WK//.
       01  ETENSU5WK-REC.
           COPY    "CPETENSU5.INC"     REPLACING  //ETENSU5//
                                              BY  //ETENSU5WK//.
      *
      *    電子点数
       01  ETENSU1-REC.
           COPY    "CPETENSU1.INC".
       01  ETENSU2-REC.
           COPY    "CPETENSU2.INC".
       01  ETNSSMP-REC.
           COPY    "CPETENSU2-SAMPLE.INC".
       01  ETENSU3-REC.
           COPY    "CPETENSU3.INC".
       01  ETENSU4-REC.
           COPY    "CPETENSU4.INC".
       01  ETENSU5-REC.
           COPY    "CPETENSU5.INC".
      *
           COPY    "ENUM-VALUE".
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
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
           IF  STS-IN              NOT =   ZERO
               MOVE    1               TO  FLG-IN
               DISPLAY "*(ORCMUP0222)* IN-FILE OPN STS[" STS-IN "]"
           END-IF
      *
      *    出力ファイル　初期処理
           PERFORM 2700-OUTLST-INIT-SEC
      *
      *    電子点数データ読込み処理
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
           PERFORM 2000-KEYSET-SYORI-SEC
      *
           EVALUATE    ETENSUWK-SYORIKBN
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
      *    電子点数データ読込み処理
           PERFORM 900-IN-KENSAKU-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    キーセット  処理
      *****************************************************************
       2000-KEYSET-SYORI-SEC       SECTION.
      *
           MOVE    SPACE           TO  WRK-TABLE
           MOVE    SPACE           TO  WRK-PATHNAME
      *
           EVALUATE    ETENSUWK-TBLKBN
               WHEN    10
                   MOVE    "tbl_etensu_1_master"   TO  WRK-TABLE
                   MOVE    "key2"                  TO  WRK-PATHNAME
               WHEN    20
                   MOVE    "tbl_etensu_2_master"   TO  WRK-TABLE
                   MOVE    "key2"                  TO  WRK-PATHNAME
               WHEN    21
                   MOVE    "tbl_etensu_2_jma_master"
                                                   TO  WRK-TABLE
                   MOVE    "key2"                  TO  WRK-PATHNAME
               WHEN    22
                   MOVE    "tbl_etensu_2_sample_master"
                                                   TO  WRK-TABLE
                   MOVE    "key2"                  TO  WRK-PATHNAME
               WHEN    31
                   MOVE    "tbl_etensu_3_1_master" TO  WRK-TABLE
                   MOVE    "key2"                  TO  WRK-PATHNAME
               WHEN    32
                   MOVE    "tbl_etensu_3_2_master" TO  WRK-TABLE
                   MOVE    "key2"                  TO  WRK-PATHNAME
               WHEN    33
                   MOVE    "tbl_etensu_3_3_master" TO  WRK-TABLE
                   MOVE    "key2"                  TO  WRK-PATHNAME
               WHEN    34
                   MOVE    "tbl_etensu_3_4_master" TO  WRK-TABLE
                   MOVE    "key2"                  TO  WRK-PATHNAME
               WHEN    40
                   MOVE    "tbl_etensu_4_master"   TO  WRK-TABLE
                   MOVE    "key2"                  TO  WRK-PATHNAME
               WHEN    50
                   MOVE    "tbl_etensu_5_master"   TO  WRK-TABLE
                   MOVE    "key2"                  TO  WRK-PATHNAME
           END-EVALUATE
      *
           .
       2000-KEYSET-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加  処理
      *****************************************************************
       2010-INSERT-SYORI-SEC       SECTION.
      *
      *    単純追加
           IF      ETENSUWK-YUKOEDYMD2
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
           MOVE    ETENSUWK-REC(12:)
                                   TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    WRK-TABLE       TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-ETENSU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ETENSU
           END-IF
      *
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    追加エラー（同一キー存在）
           IF      FLG-ETENSU      =   ZERO
               MOVE    1               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           ELSE
      *        ＤＢ追加
               PERFORM 20101-ETENSU-INS-SEC
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
           MOVE    ETENSUWK-REC(12:)
                                   TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    WRK-TABLE       TO  MCP-TABLE
               MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-ETENSU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ETENSU
           END-IF
      *
           PERFORM UNTIL  (FLG-ETENSU      =   1  )
                     OR   (FLG-SYORI       =   1  )
               PERFORM 20100-INSCHK-SYORI-SEC
               IF      FLG-SYORI       =   1
      *            ＤＢ更新
                   PERFORM 20101-ETENSU-UPD-SEC
      *            ＤＢ追加
                   PERFORM 20101-ETENSU-INS-SEC
               ELSE
                   MOVE    WRK-TABLE       TO  MCP-TABLE
                   MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 910-ETENSU-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF      FLG-SYORI       =   ZERO
      *        ＤＢ追加
               PERFORM 20101-ETENSU-INS-SEC
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
           MOVE    ETENSUWK-REC(12:)
                                   TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    WRK-TABLE       TO  MCP-TABLE
               MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-ETENSU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ETENSU
           END-IF
      *
           PERFORM UNTIL  (FLG-ETENSU      =   1  )
                     OR   (FLG-SYORI       =   1  )
               PERFORM 20100-UPDCHK-SYORI-SEC
               IF      FLG-SYORI       =   1
      *            ＤＢ更新
                   PERFORM 20101-ETENSU-UPD-SEC
               ELSE
                   MOVE    WRK-TABLE       TO  MCP-TABLE
                   MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 910-ETENSU-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
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
           MOVE    ETENSUWK-REC(12:)
                                   TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    WRK-TABLE       TO  MCP-TABLE
               MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-ETENSU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ETENSU
           END-IF
      *
           PERFORM UNTIL  (FLG-ETENSU      =   1  )
                     OR   (FLG-SYORI       =   1  )
               PERFORM 20100-DELCHK-SYORI-SEC
               IF      FLG-SYORI       =   1
      *            ＤＢ削除
                   PERFORM 20101-ETENSU-DEL-SEC
               ELSE
                   MOVE    WRK-TABLE       TO  MCP-TABLE
                   MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 910-ETENSU-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
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
      *    追加チェック  処理
      *****************************************************************
       20100-INSCHK-SYORI-SEC       SECTION.
      *
           EVALUATE    ETENSUWK-TBLKBN
               WHEN    10
                   IF     (ETENSU1-YUKOSTYMD   <=  ETENSUWK-YUKOEDYMD2)
                     AND  (ETENSU1-YUKOEDYMD   >=  ETENSUWK-YUKOEDYMD2)
                       MOVE    ETENSU1-KEY     TO  ETENSU1-UP-KEY
                       MOVE    ETENSUWK-YUKOEDYMD2
                                               TO  ETENSU1-YUKOEDYMD
                       MOVE    1               TO  FLG-SYORI
                   END-IF
               WHEN    20
               WHEN    21
                   IF     (ETENSU2-YUKOSTYMD   <=  ETENSUWK-YUKOEDYMD2)
                     AND  (ETENSU2-YUKOEDYMD   >=  ETENSUWK-YUKOEDYMD2)
                       MOVE    ETENSU2-KEY     TO  ETENSU2-UP-KEY
                       MOVE    ETENSUWK-YUKOEDYMD2
                                               TO  ETENSU2-YUKOEDYMD
                       MOVE    1               TO  FLG-SYORI
                   END-IF
               WHEN    22
                   IF     (ETNSSMP-YUKOSTYMD   <=  ETENSUWK-YUKOEDYMD2)
                     AND  (ETNSSMP-YUKOEDYMD   >=  ETENSUWK-YUKOEDYMD2)
                       MOVE    ETNSSMP-KEY     TO  ETNSSMP-UP-KEY
                       MOVE    ETENSUWK-YUKOEDYMD2
                                               TO  ETNSSMP-YUKOEDYMD
                       MOVE    1               TO  FLG-SYORI
                   END-IF
               WHEN    31
               WHEN    32
               WHEN    33
               WHEN    34
                   IF     (ETENSU3-YUKOSTYMD   <=  ETENSUWK-YUKOEDYMD2)
                     AND  (ETENSU3-YUKOEDYMD   >=  ETENSUWK-YUKOEDYMD2)
                       MOVE    ETENSU3-KEY     TO  ETENSU3-UP-KEY
                       MOVE    ETENSUWK-YUKOEDYMD2
                                               TO  ETENSU3-YUKOEDYMD
                       MOVE    1               TO  FLG-SYORI
                   END-IF
               WHEN    40
                   IF     (ETENSU4-YUKOSTYMD   <=  ETENSUWK-YUKOEDYMD2)
                     AND  (ETENSU4-YUKOEDYMD   >=  ETENSUWK-YUKOEDYMD2)
                       MOVE    ETENSU4-KEY     TO  ETENSU4-UP-KEY
                       MOVE    ETENSUWK-YUKOEDYMD2
                                               TO  ETENSU4-YUKOEDYMD
                       MOVE    1               TO  FLG-SYORI
                   END-IF
               WHEN    50
                   IF     (ETENSU5-YUKOSTYMD   <=  ETENSUWK-YUKOEDYMD2)
                     AND  (ETENSU5-YUKOEDYMD   >=  ETENSUWK-YUKOEDYMD2)
                       MOVE    ETENSU5-KEY     TO  ETENSU5-UP-KEY
                       MOVE    ETENSUWK-YUKOEDYMD2
                                               TO  ETENSU5-YUKOEDYMD
                       MOVE    1               TO  FLG-SYORI
                   END-IF
           END-EVALUATE
      *
           .
       20100-INSCHK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新チェック  処理
      *****************************************************************
       20100-UPDCHK-SYORI-SEC       SECTION.
      *
           EVALUATE    ETENSUWK-TBLKBN
               WHEN    10
                   IF     (ETENSU1-YUKOSTYMD   <=  ETENSU1WK-YUKOSTYMD)
                     AND  (ETENSU1-YUKOEDYMD   >=  ETENSU1WK-YUKOEDYMD)
                       MOVE    ETENSU1WK-REC(26:)
                                               TO  ETENSU1-REC(26:)
                       MOVE    ETENSU1-KEY     TO  ETENSU1-UP-KEY
                       MOVE    ETENSU1WK-KEY   TO  ETENSU1-KEY
                       MOVE    1               TO  FLG-SYORI
                   END-IF
               WHEN    20
               WHEN    21
                   IF     (ETENSU2-YUKOSTYMD   <=  ETENSU2WK-YUKOSTYMD)
                     AND  (ETENSU2-YUKOEDYMD   >=  ETENSU2WK-YUKOEDYMD)
                       MOVE    ETENSU2WK-REC(41:)
                                               TO  ETENSU2-REC(41:)
                       MOVE    ETENSU2-KEY     TO  ETENSU2-UP-KEY
                       MOVE    ETENSU2WK-KEY   TO  ETENSU2-KEY
                       MOVE    1               TO  FLG-SYORI
                   END-IF
               WHEN    22
                   IF     (ETNSSMP-YUKOSTYMD   <=  ETNSSMPWK-YUKOSTYMD)
                     AND  (ETNSSMP-YUKOEDYMD   >=  ETNSSMPWK-YUKOEDYMD)
                       MOVE    ETNSSMPWK-REC(35:)
                                               TO  ETNSSMP-REC(35:)
                       MOVE    ETNSSMP-KEY     TO  ETNSSMP-UP-KEY
                       MOVE    ETNSSMPWK-KEY   TO  ETNSSMP-KEY
                       MOVE    1               TO  FLG-SYORI
                   END-IF
               WHEN    31
               WHEN    32
               WHEN    33
               WHEN    34
                   IF     (ETENSU3-YUKOSTYMD   <=  ETENSU3WK-YUKOSTYMD)
                     AND  (ETENSU3-YUKOEDYMD   >=  ETENSU3WK-YUKOEDYMD)
                       MOVE    ETENSU3WK-REC(35:)
                                               TO  ETENSU3-REC(35:)
                       MOVE    ETENSU3-KEY     TO  ETENSU3-UP-KEY
                       MOVE    ETENSU3WK-KEY   TO  ETENSU3-KEY
                       MOVE    1               TO  FLG-SYORI
                   END-IF
               WHEN    40
                   IF     (ETENSU4-YUKOSTYMD   <=  ETENSU4WK-YUKOSTYMD)
                     AND  (ETENSU4-YUKOEDYMD   >=  ETENSU4WK-YUKOEDYMD)
                       MOVE    ETENSU4WK-REC(29:)
                                               TO  ETENSU4-REC(29:)
                       MOVE    ETENSU4-KEY     TO  ETENSU4-UP-KEY
                       MOVE    ETENSU4WK-KEY   TO  ETENSU4-KEY
                       MOVE    1               TO  FLG-SYORI
                   END-IF
               WHEN    50
                   IF     (ETENSU5-YUKOSTYMD   <=  ETENSU5WK-YUKOSTYMD)
                     AND  (ETENSU5-YUKOEDYMD   >=  ETENSU5WK-YUKOEDYMD)
                       MOVE    ETENSU5WK-REC(29:)
                                               TO  ETENSU5-REC(29:)
                       MOVE    ETENSU5-KEY     TO  ETENSU5-UP-KEY
                       MOVE    ETENSU5WK-KEY   TO  ETENSU5-KEY
                       MOVE    1               TO  FLG-SYORI
                   END-IF
           END-EVALUATE
      *
           .
       20100-UPDCHK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除チェック  処理
      *****************************************************************
       20100-DELCHK-SYORI-SEC       SECTION.
      *
           EVALUATE    ETENSUWK-TBLKBN
               WHEN    10
                   IF     (ETENSU1-YUKOSTYMD   <=  ETENSU1WK-YUKOSTYMD)
                     AND  (ETENSU1-YUKOEDYMD   >=  ETENSU1WK-YUKOEDYMD)
                       MOVE    1               TO  FLG-SYORI
                   END-IF
               WHEN    20
               WHEN    21
                   IF     (ETENSU2-YUKOSTYMD   <=  ETENSU2WK-YUKOSTYMD)
                     AND  (ETENSU2-YUKOEDYMD   >=  ETENSU2WK-YUKOEDYMD)
                       MOVE    1               TO  FLG-SYORI
                   END-IF
               WHEN    22
                   IF     (ETNSSMP-YUKOSTYMD   <=  ETNSSMPWK-YUKOSTYMD)
                     AND  (ETNSSMP-YUKOEDYMD   >=  ETNSSMPWK-YUKOEDYMD)
                       MOVE    1               TO  FLG-SYORI
                   END-IF
               WHEN    31
               WHEN    32
               WHEN    33
               WHEN    34
                   IF     (ETENSU3-YUKOSTYMD   <=  ETENSU3WK-YUKOSTYMD)
                     AND  (ETENSU3-YUKOEDYMD   >=  ETENSU3WK-YUKOEDYMD)
                       MOVE    1               TO  FLG-SYORI
                   END-IF
               WHEN    40
                   IF     (ETENSU4-YUKOSTYMD   <=  ETENSU4WK-YUKOSTYMD)
                     AND  (ETENSU4-YUKOEDYMD   >=  ETENSU4WK-YUKOEDYMD)
                       MOVE    1               TO  FLG-SYORI
                   END-IF
               WHEN    50
                   IF     (ETENSU5-YUKOSTYMD   <=  ETENSU5WK-YUKOSTYMD)
                     AND  (ETENSU5-YUKOEDYMD   >=  ETENSU5WK-YUKOEDYMD)
                       MOVE    1               TO  FLG-SYORI
                   END-IF
           END-EVALUATE
      *
           .
       20100-DELCHK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ追加  処理
      *****************************************************************
       20101-ETENSU-INS-SEC        SECTION.
      *
           EVALUATE    ETENSUWK-TBLKBN
               WHEN    10
                   MOVE    ETENSU1WK-REC   TO  ETENSU1-REC
                   MOVE    LNK-MNTYMD      TO  ETENSU1-CHGYMD
                   MOVE    ETENSU1-REC     TO  MCPDATA-REC
               WHEN    20
               WHEN    21
                   MOVE    ETENSU2WK-REC   TO  ETENSU2-REC
                   MOVE    LNK-MNTYMD      TO  ETENSU2-CHGYMD
                   MOVE    ETENSU2-REC     TO  MCPDATA-REC
               WHEN    22
                   MOVE    ETNSSMPWK-REC   TO  ETNSSMP-REC
                   MOVE    LNK-MNTYMD      TO  ETNSSMP-CHGYMD
                   MOVE    ETNSSMP-REC     TO  MCPDATA-REC
               WHEN    31
               WHEN    32
               WHEN    33
               WHEN    34
                   MOVE    ETENSU3WK-REC   TO  ETENSU3-REC
                   MOVE    LNK-MNTYMD      TO  ETENSU3-CHGYMD
                   MOVE    ETENSU3-REC     TO  MCPDATA-REC
               WHEN    40
                   MOVE    ETENSU4WK-REC   TO  ETENSU4-REC
                   MOVE    LNK-MNTYMD      TO  ETENSU4-CHGYMD
                   MOVE    ETENSU4-REC     TO  MCPDATA-REC
               WHEN    50
                   MOVE    ETENSU5WK-REC   TO  ETENSU5-REC
                   MOVE    LNK-MNTYMD      TO  ETENSU5-CHGYMD
                   MOVE    ETENSU5-REC     TO  MCPDATA-REC
           END-EVALUATE
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC      NOT =   ZERO
               MOVE    1               TO  LNK-RC
               MOVE    2               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           IF      WRK-ERRCD       =   ZERO
               ADD     1               TO  CNT-INS
           END-IF
      *
           .
       20101-ETENSU-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ更新  処理
      *****************************************************************
       20101-ETENSU-UPD-SEC        SECTION.
      *
           EVALUATE    ETENSUWK-TBLKBN
               WHEN    10
                   MOVE    LNK-MNTYMD      TO  ETENSU1-CHGYMD
                   MOVE    ETENSU1-REC     TO  MCPDATA-REC
               WHEN    20
               WHEN    21
                   MOVE    LNK-MNTYMD      TO  ETENSU2-CHGYMD
                   MOVE    ETENSU2-REC     TO  MCPDATA-REC
               WHEN    22
                   MOVE    LNK-MNTYMD      TO  ETNSSMP-CHGYMD
                   MOVE    ETNSSMP-REC     TO  MCPDATA-REC
               WHEN    31
               WHEN    32
               WHEN    33
               WHEN    34
                   MOVE    LNK-MNTYMD      TO  ETENSU3-CHGYMD
                   MOVE    ETENSU3-REC     TO  MCPDATA-REC
               WHEN    40
                   MOVE    LNK-MNTYMD      TO  ETENSU4-CHGYMD
                   MOVE    ETENSU4-REC     TO  MCPDATA-REC
               WHEN    50
                   MOVE    LNK-MNTYMD      TO  ETENSU5-CHGYMD
                   MOVE    ETENSU5-REC     TO  MCPDATA-REC
           END-EVALUATE
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC      NOT =   ZERO
               MOVE    2               TO  LNK-RC
               MOVE    3               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           IF      WRK-ERRCD       =   ZERO
               ADD     1               TO  CNT-UPD
           END-IF
      *
           .
       20101-ETENSU-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ削除  処理
      *****************************************************************
       20101-ETENSU-DEL-SEC        SECTION.
      *
           EVALUATE    ETENSUWK-TBLKBN
               WHEN    10
                   MOVE    ETENSU1-REC     TO  MCPDATA-REC
               WHEN    20
               WHEN    21
                   MOVE    ETENSU2-REC     TO  MCPDATA-REC
               WHEN    22
                   MOVE    ETNSSMP-REC     TO  MCPDATA-REC
               WHEN    31
               WHEN    32
               WHEN    33
               WHEN    34
                   MOVE    ETENSU3-REC     TO  MCPDATA-REC
               WHEN    40
                   MOVE    ETENSU4-REC     TO  MCPDATA-REC
               WHEN    50
                   MOVE    ETENSU5-REC     TO  MCPDATA-REC
           END-EVALUATE
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC      NOT =   ZERO
               MOVE    3               TO  LNK-RC
               MOVE    4               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           IF      WRK-ERRCD       =   ZERO
               ADD     1               TO  CNT-DEL
           END-IF
      *
           .
       20101-ETENSU-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー処理
      *****************************************************************
       2040-ERR-SYORI-SEC          SECTION.
      *
      *    エラー出力
           MOVE    ETENSUWK-REC    TO  ERR-ETENSUWK-REC
           WRITE   ERR-ETENSUWK-REC
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
           MOVE    ETENSUWK-TBLKBN
                                   TO  WRK-TBLKBN
           MOVE    ETENSUWK-SYORIKBN
                                   TO  WRK-SYORIKBN
      *
           EVALUATE    WRK-ERRCD
               WHEN    1
                   MOVE  "電子点数マスタ登録済み"
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
               DISPLAY "*(ORCMUP0222)* OUTFILE OPN STS[" STS-ERR "]"
               GO  TO  2700-OUTLST-INIT-EXT
           END-IF
      *
      *    エラーレコード出力ファイル　オープン
           OPEN    OUTPUT             PLF-FILE
           IF      STS-PLF    NOT =   ZERO
               DISPLAY "*(ORCMUP0222)* PLFFILE OPN STS[" STS-PLF "]"
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
      *
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    1               TO  JOB-HOSPNUM
                   MOVE    "tbl_jobkanri"  TO  MCP-TABLE
                   MOVE    "abort"         TO  MCP-PATHNAME
                   MOVE    JOBKANRI-REC    TO  MCPDATA-REC
                   MOVE    "DBSELECT"      TO  MCP-FUNC
                   PERFORM 900-ORCDBMAIN-SEC
               END-IF
      *
           END-IF
      *
           DISPLAY "*(ORCMUP0222)* INPUT  /I   CNT[" CNT-IN  "]"
           DISPLAY "*(ORCMUP0222)*  INSERT     CNT[" CNT-INS "]"
           DISPLAY "*(ORCMUP0222)*  UPDATE     CNT[" CNT-UPD "]"
           DISPLAY "*(ORCMUP0222)*  DELETE     CNT[" CNT-DEL "]"
           DISPLAY "*(ORCMUP0222)* ERR    /O   CNT[" CNT-ERR "]"
           DISPLAY "*(ORCMUP0222)* PLF    /O   CNT[" CNT-PLF "]"
           DISPLAY "*(ORCMUP0222)* RC             [" LNK-RC "]"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *     電子点数読込み処理
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
               INITIALIZE                  ETENSUWK-REC
               MOVE    IN-REC          TO  ETENSUWK-REC
      *
               EVALUATE    ETENSUWK-TBLKBN
                   WHEN    10
                       INITIALIZE                  ETENSU1WK-REC
                       MOVE    IN-REC(12:)     TO  ETENSU1WK-REC
                   WHEN    20
                   WHEN    21
                       INITIALIZE                  ETENSU2WK-REC
                       MOVE    IN-REC(12:)     TO  ETENSU2WK-REC
                       IF      LNK-DBRVERSION (3:6)    <   "040800"
                           MOVE    ZERO        TO  ETENSU2WK-TOKUREI
                       END-IF
                   WHEN    22
                       INITIALIZE                  ETNSSMPWK-REC
                       MOVE    IN-REC(12:)     TO  ETNSSMPWK-REC
                   WHEN    31
                   WHEN    32
                   WHEN    33
                   WHEN    34
                       INITIALIZE                  ETENSU3WK-REC
                       MOVE    IN-REC(12:)     TO  ETENSU3WK-REC
                   WHEN    40
                       INITIALIZE                  ETENSU4WK-REC
                       MOVE    IN-REC(12:)     TO  ETENSU4WK-REC
                   WHEN    50
                       INITIALIZE                  ETENSU5WK-REC
                       MOVE    IN-REC(12:)     TO  ETENSU5WK-REC
               END-EVALUATE
           END-IF
      *
          .
       900-IN-KENSAKU-EXT.
          EXIT.
      *
      *****************************************************************
      *    電子点数読込
      *****************************************************************
       910-ETENSU-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               EVALUATE    ETENSUWK-TBLKBN
                   WHEN    10
                       MOVE    MCPDATA-REC     TO  ETENSU1-REC
                   WHEN    20
                   WHEN    21
                       MOVE    MCPDATA-REC     TO  ETENSU2-REC
                   WHEN    22
                       MOVE    MCPDATA-REC     TO  ETNSSMP-REC
                   WHEN    31
                   WHEN    32
                   WHEN    33
                   WHEN    34
                       MOVE    MCPDATA-REC     TO  ETENSU3-REC
                   WHEN    40
                       MOVE    MCPDATA-REC     TO  ETENSU4-REC
                   WHEN    50
                       MOVE    MCPDATA-REC     TO  ETENSU5-REC
               END-EVALUATE
               MOVE    ZERO            TO  FLG-ETENSU
           ELSE
               INITIALIZE                  ETENSU1-REC
               INITIALIZE                  ETENSU2-REC
               INITIALIZE                  ETNSSMP-REC
               INITIALIZE                  ETENSU3-REC
               INITIALIZE                  ETENSU4-REC
               INITIALIZE                  ETENSU5-REC
               MOVE    1               TO  FLG-ETENSU
           END-IF
      *
           .
       910-ETENSU-READ-EXT.
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
      *
           .
       900-ORCDBMAIN-EXT.
           EXIT.
