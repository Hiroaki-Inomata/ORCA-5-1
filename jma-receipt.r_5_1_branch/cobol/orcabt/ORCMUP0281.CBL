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
       PROGRAM-ID.                 ORCMUP0281.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＤＢ作成処理
      *  コンポーネント名  : 医薬品傷病名更新処理
      *                    : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  15.04.28    NACL-伊藤     新規作成
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
      *    入力：医薬品傷病名更新データ
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
           SELECT  PRF-FILE        ASSIGN
                                   ASS-PRF
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PRF.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    入力：電子点数更新データ
       FD  IN-FILE.
       01  IN-REC                      PIC X(3000).
      *
      *    出力：エラーファイル
       FD  ERR-FILE.
       01  ERR-REC                     PIC X(3000).
      *
      *    出力：プルーフファイル
       FD  PRF-FILE.
       01  PRF-REC                     PIC X(1000).
      *
       WORKING-STORAGE             SECTION.
     *
           COPY    "COMMON-SPA".
      *
      *    ファイル指定領域
       01  ASS-AREA.
           03  ASS-IN                  PIC X(256).
           03  ASS-ERR                 PIC X(256).
           03  ASS-PRF                 PIC X(256).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-IN                  PIC X(02).
           03  STS-ERR                 PIC X(02).
           03  STS-PRF                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-IN                  PIC 9(01).
           03  FLG-TEKIOU              PIC 9(01).
           03  FLG-PRF                 PIC 9(01).
      *
      *    インデックス領域
      *01  IDX-AREA.
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-INS                 PIC 9(06).
           03  CNT-UPD                 PIC 9(06).
           03  CNT-DEL                 PIC 9(06).
           03  CNT-ERR                 PIC 9(06).
           03  CNT-PRF                 PIC 9(06).
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
           03  WRK-SYORI               PIC X(01).
           03  WRK-RECKBN              PIC X(01).
           03  WRK-TABLE               PIC X(64).
           03  WRK-HOMEDIR             PIC X(128).
      *
      *    出力ファイル領域
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(26) VALUE
                                       "【PGID:ORCMUP0281.CBL】".
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
           03  WRK-TBLKBN              PIC X(01).
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
      *    医薬品傷病名
       01  TEKIOU-MEDICINE-REC.
           COPY    "CPTEKIOU-MEDICINE.INC".
      *
       01  TEKIOU-DISEASE-REC.
           COPY    "CPTEKIOU-DISEASE.INC".
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
           MOVE    ZERO                TO  FLG-AREA
           MOVE    ZERO                TO  CNT-AREA
           INITIALIZE                      WRK-AREA
                                           SPA-AREA
      ****                                 IDX-AREA
      *
           MOVE    LNK-HOSPNUM         TO  SPA-HOSPNUM
      *
           ACCEPT  SYS-DATE        FROM    DATE
           COMPUTE WRK-SYY             =   SYS-YY  +   2000
           MOVE    SYS-MM              TO  WRK-SMM
           MOVE    SYS-DD              TO  WRK-SDD
           ACCEPT  SYS-TIME        FROM    TIME
      *
      *    DB OPEN
      *    PERFORM 100-DBOPEN-SEC
      *
           PERFORM 101-ASSIN-GET-SEC
      *
      *    入力　初期処理
           OPEN    INPUT                   IN-FILE
           IF  STS-IN              NOT =   ZERO
               MOVE    1               TO  FLG-IN
               DISPLAY "*(ORCMUP0281)* IN-FILE OPN STS[" STS-IN "]"
           END-IF
      *
      *    出力ファイル　初期処理
           PERFORM 2700-OUTLST-INIT-SEC
      *
      *    医薬品傷病名データ読込み処理
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
           MOVE    SPACE           TO  ASS-PRF
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   SPA-HOSPNUM         DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   "orca-mstlog/"      DELIMITED  BY  SIZE
                   LNK-UPFILE          DELIMITED  BY  SPACE
                   ".PRF"              DELIMITED  BY  SIZE
                                       INTO    ASS-PRF
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
           MOVE    ZERO                TO  WRK-ERRCD
      *
           PERFORM 2000-KEYSET-SYORI-SEC
      *
           EVALUATE    WRK-SYORI
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
      *    医薬品傷病名データ読込み処理
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
           MOVE    IN-REC (1:1)        TO  WRK-SYORI
           MOVE    IN-REC (2:1)        TO  WRK-RECKBN
      *
           MOVE    SPACE               TO  WRK-TABLE
      *
           EVALUATE    WRK-RECKBN
               WHEN    "M"
                   MOVE    "tbl_tekiou_medicine_master"
                                               TO  WRK-TABLE
               WHEN    "D"
                   MOVE    "tbl_tekiou_disease_master"
                                               TO  WRK-TABLE
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
           MOVE    IN-REC(3:)          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-TABLE           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-TEKIOU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TEKIOU
           END-IF
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    追加エラー（同一キー存在）
           IF      FLG-TEKIOU      =   ZERO
               MOVE    1                   TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           ELSE
      *        ＤＢ追加
               PERFORM 20101-TEKIOU-INS-SEC
           END-IF
      *
           .
       2010-INSERT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ更新  処理
      *****************************************************************
       2020-UPDATE-SYORI-SEC       SECTION.
      *
           MOVE    IN-REC(3:)          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-TABLE           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-TEKIOU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TEKIOU
           END-IF
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    更新エラー（同一キーなし）
           IF      FLG-TEKIOU      NOT =   ZERO
               MOVE    5                   TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           ELSE
      *        ＤＢ更新
               PERFORM 20101-TEKIOU-UPD-SEC
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
           MOVE    IN-REC(3:)          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-TABLE           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-TEKIOU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TEKIOU
           END-IF
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    削除エラー（同一キーなし）
           IF      FLG-TEKIOU      NOT =   ZERO
               MOVE    6                   TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           ELSE
      *        ＤＢ削除
               PERFORM 20101-TEKIOU-DEL-SEC
           END-IF
      *
           .
       2030-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ追加  処理
      *****************************************************************
       20101-TEKIOU-INS-SEC        SECTION.
      *
           EVALUATE    WRK-RECKBN
               WHEN    "M"
                   MOVE    IN-REC(3:)      TO  TEKIOU-MEDICINE-REC
                   MOVE    LNK-MNTYMD      TO  TEKMED-CREYMD
                                               TEKMED-UPYMD
                   MOVE    LNK-MNTHMS      TO  TEKMED-UPHMS
                   MOVE    TEKIOU-MEDICINE-REC
                                           TO  MCPDATA-REC
               WHEN    "D"
                   MOVE    IN-REC(3:)      TO  TEKIOU-DISEASE-REC
                   MOVE    LNK-MNTYMD      TO  TEKDIS-CREYMD
                                               TEKDIS-UPYMD
                   MOVE    LNK-MNTHMS      TO  TEKDIS-UPHMS
                   MOVE    TEKIOU-DISEASE-REC
                                           TO  MCPDATA-REC
           END-EVALUATE
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    1                   TO  LNK-RC
               MOVE    2                   TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           IF      WRK-ERRCD           =   ZERO
               ADD     1                   TO  CNT-INS
           END-IF
      *
           .
       20101-TEKIOU-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ更新  処理
      *****************************************************************
       20101-TEKIOU-UPD-SEC        SECTION.
      *
           EVALUATE    WRK-RECKBN
               WHEN    "M"
                   MOVE    IN-REC(3:)      TO  TEKIOU-MEDICINE-REC
                   MOVE    LNK-MNTYMD      TO  TEKMED-UPYMD
                   MOVE    LNK-MNTHMS      TO  TEKMED-UPHMS
                   MOVE    TEKIOU-MEDICINE-REC
                                           TO  MCPDATA-REC
               WHEN    "D"
                   MOVE    IN-REC(3:)      TO  TEKIOU-DISEASE-REC
                   MOVE    LNK-MNTYMD      TO  TEKDIS-UPYMD
                   MOVE    LNK-MNTHMS      TO  TEKDIS-UPHMS
                   MOVE    TEKIOU-DISEASE-REC
                                           TO  MCPDATA-REC
           END-EVALUATE
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    2                   TO  LNK-RC
               MOVE    3                   TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           IF      WRK-ERRCD           =   ZERO
               ADD     1                   TO  CNT-UPD
           END-IF
      *
           .
       20101-TEKIOU-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ削除  処理
      *****************************************************************
       20101-TEKIOU-DEL-SEC        SECTION.
      *
           EVALUATE    WRK-RECKBN
               WHEN    "M"
                   MOVE    IN-REC(3:)      TO  TEKIOU-MEDICINE-REC
                   MOVE    TEKIOU-MEDICINE-REC
                                           TO  MCPDATA-REC
               WHEN    "D"
                   MOVE    IN-REC(3:)      TO  TEKIOU-DISEASE-REC
                   MOVE    TEKIOU-DISEASE-REC
                                           TO  MCPDATA-REC
           END-EVALUATE
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    3                   TO  LNK-RC
               MOVE    4                   TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           IF      WRK-ERRCD           =   ZERO
               ADD     1                   TO  CNT-DEL
           END-IF
      *
           .
       20101-TEKIOU-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー処理
      *****************************************************************
       2040-ERR-SYORI-SEC          SECTION.
      *
      *    エラー出力
           MOVE    IN-REC              TO  ERR-REC
           WRITE   ERR-REC
           ADD     1                   TO  CNT-ERR
      *
      *    エラーメッセージ出力
           PERFORM 20401-PRF-OUT-SEC
      *
           .
       2040-ERR-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ出力処理
      *****************************************************************
       20401-PRF-OUT-SEC           SECTION.
      *
           MOVE    WRK-RECKBN          TO  WRK-TBLKBN
           MOVE    WRK-SYORI           TO  WRK-SYORIKBN
      *
           EVALUATE    WRK-ERRCD
               WHEN    1
                   MOVE  "医薬品傷病名マスタ登録済み"
                                               TO  WRK-MSG-01
               WHEN    2
                   MOVE  "追加エラー"          TO  WRK-MSG-01
               WHEN    3
                   MOVE  "更新エラー"          TO  WRK-MSG-01
               WHEN    4
                   MOVE  "削除エラー"          TO  WRK-MSG-01
               WHEN    5
                   MOVE  "更新対象なし"        TO  WRK-MSG-01
               WHEN    6
                   MOVE  "削除対象なし"        TO  WRK-MSG-01
           END-EVALUATE
      *
           MOVE    WRK-MSG-AREA        TO  PRF-REC
           WRITE   PRF-REC
           ADD     1                   TO  CNT-PRF
      *
           .
       20401-PRF-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力ファイル　初期処理
      *****************************************************************
       2700-OUTLST-INIT-SEC        SECTION.
      *
           OPEN    OUTPUT              ERR-FILE
           IF      STS-ERR         NOT =   ZERO
               DISPLAY "*(ORCMUP0281)* OUTFILE OPN STS[" STS-ERR "]"
               GO  TO  2700-OUTLST-INIT-EXT
           END-IF
      *
      *    エラーレコード出力ファイル　オープン
           OPEN    OUTPUT             PRF-FILE
           IF      STS-PRF        NOT =   ZERO
               DISPLAY "*(ORCMUP0281)* PRFFILE OPN STS[" STS-PRF "]"
               GO  TO  2700-OUTLST-INIT-EXT
           END-IF
      *
           MOVE    WRK-SYY             TO  MES-TITLE1-YY
           MOVE    SYS-MM              TO  MES-TITLE1-MM
           MOVE    SYS-DD              TO  MES-TITLE1-DD
           MOVE    SPA-HOSPNUM         TO  MES-TITLE1-HOSPNUM
           MOVE    MES-TITLE1          TO  PRF-REC
           WRITE   PRF-REC
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
           CLOSE   IN-FILE
           CLOSE   ERR-FILE
           CLOSE   PRF-FILE
      *
           IF      LNK-RC              =   ZERO
               IF      CNT-IN          NOT =   LNK-COUNT
                   MOVE    99                  TO  LNK-RC
      *
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    1                   TO  JOB-HOSPNUM
                   MOVE    "tbl_jobkanri"      TO  MCP-TABLE
                   MOVE    "abort"             TO  MCP-PATHNAME
                   MOVE    JOBKANRI-REC        TO  MCPDATA-REC
                   MOVE    "DBSELECT"          TO  MCP-FUNC
                   PERFORM 900-ORCDBMAIN-SEC
               END-IF
      *
           END-IF
      *
           DISPLAY "*(ORCMUP0281)* INPUT  /I   CNT[" CNT-IN  "]"
           DISPLAY "*(ORCMUP0281)*  INSERT     CNT[" CNT-INS "]"
           DISPLAY "*(ORCMUP0281)*  UPDATE     CNT[" CNT-UPD "]"
           DISPLAY "*(ORCMUP0281)*  DELETE     CNT[" CNT-DEL "]"
           DISPLAY "*(ORCMUP0281)* ERR    /O   CNT[" CNT-ERR "]"
           DISPLAY "*(ORCMUP0281)* PRF    /O   CNT[" CNT-PRF "]"
           DISPLAY "*(ORCMUP0281)* RC             [" LNK-RC "]"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    医薬品傷病名データ読込み処理
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
          .
       900-IN-KENSAKU-EXT.
          EXIT.
      *
      *****************************************************************
      *    医薬品傷病名読込
      *****************************************************************
       910-TEKIOU-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               EVALUATE    WRK-RECKBN
                   WHEN    "M"
                       MOVE    MCPDATA-REC     TO  TEKIOU-MEDICINE-REC
                   WHEN    "D"
                       MOVE    MCPDATA-REC     TO  TEKIOU-DISEASE-REC
               END-EVALUATE
               MOVE    ZERO            TO  FLG-TEKIOU
           ELSE
               INITIALIZE                  TEKIOU-MEDICINE-REC
               INITIALIZE                  TEKIOU-DISEASE-REC
               MOVE    1               TO  FLG-TEKIOU
           END-IF
      *
           .
       910-TEKIOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
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
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
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
