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
       PROGRAM-ID.                 ORCMUP0161.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＤＢ作成処理
      *  コンポーネント名  : プログラムオプション更新処理
      *                    : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08.02.04    NACL-伊藤     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.03.00     NACL-太田    08/05/28  エラー処理強化対応
      * 04.04.00     NACL-伊藤    09/03/25  更新データレイアウト変更
      *                                     OPTION 5000 -> 44000
      * 04.04.00     NACL-太田    09/03/27  ２重追加チェック修正
      * 04.07.01     NACL-森脇    13/07/04  OPIDセット
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    入力：プログラムオプション更新データ
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
      *    入力：プログラムオプション更新データ
       FD  IN-FILE.
       01  IN-REC                      PIC X(50200).
      *
      *    出力：エラーファイル
       FD  ERR-FILE.
       01  ERR-PRGOPTIONWK-REC.
           03  ERR-PRGOPTIONWK-SYORIKBN       PIC  X(01).
           COPY    "CPPRGOPTION.INC"   REPLACING  //PRGOPTION-//
                                              BY  //ERR-PRGOPTIONWK-//.
      *
      *    出力：プルーフファイル
       FD  PLF-FILE.
       01  PLF-REC                     PIC X(1000).
      *
       WORKING-STORAGE             SECTION.
grpsys*
grpsys     COPY    "COMMON-SPA".
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
           03  FLG-PRGOPTION           PIC 9(01).
           03  FLG-LF                  PIC 9(01).
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
           03  WRK-OPTION              PIC X(60000).
           03  WRK-OPTION-LEN          PIC 9(08).
      *----(04.07.01)--ADD-START---
           03  WRK-OPID                PIC X(16).
      *----(04.07.01)--ADD-END-----
      *
           03  WRK-MAE-NAIYO           PIC X(200).
           03  WRK-ATO-NAIYO           PIC X(200).
           03  WRK-MAE-AREA.
               05  WRK-MAE-OPTION      PIC X(200) OCCURS 100.
           03  WRK-ATO-AREA.
               05  WRK-ATO-OPTION      PIC X(200) OCCURS 100.
           03  WRK-HOMEDIR             PIC X(128).
      *
      *    旧マスタデータ退避領域
       01  PRGWK-REC.
           03  PRGWK-SYORIKBN      PIC  X(1).
           03  PRGWK-HOSPNUM       PIC  9(2).
           03  PRGWK-PRGID         PIC  X(25).
           03  PRGWK-KBNCD         PIC  X(16).
           03  PRGWK-ADDKBN        PIC  X(01).
           03  PRGWK-TERMID        PIC  X(16).
           03  PRGWK-OPID          PIC  X(16).
           03  PRGWK-CREYMD        PIC  X(08).
           03  PRGWK-UPYMD         PIC  X(08).
           03  PRGWK-UPHMS         PIC  X(06).
           03  PRGWK-OPTION        PIC  X(44000).
       01  PRGWK-REC-V01.
           03  PRGWK-V01-AREA-01       PIC X(5044).
           03  PRGWK-V01-AREA-02       PIC X(55).
      *
      *    出力ファイル領域
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(26) VALUE
                                       "【PGID:ORCMUP0161.CBL】".
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
           03  WRK-PRGID               PIC X(20).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-KBNCD               PIC X(08).
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
       01  PRGOPTIONWK-REC.
           03  PRGOPTIONWK-SYORIKBN           PIC  X(01).
           COPY    "CPPRGOPTION.INC"   REPLACING   //PRGOPTION-//
                                              BY   //PRGOPTIONWK-//.
      *    プログラムオプション
       01  PRGOPTION-REC.
           COPY    "CPPRGOPTION.INC".
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
       PROCEDURE                       DIVISION
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
                   UNTIL ( FLG-IN      =   1 )
                    OR   ( LNK-RC  NOT =   ZERO )
      *
           PERFORM 300-END-SEC
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
                                           IDX-AREA
      *
           MOVE    LNK-HOSPNUM         TO  SPA-HOSPNUM
      *
           ACCEPT  SYS-DATE            FROM    DATE
           COMPUTE WRK-SYY             =   SYS-YY    +   2000
           MOVE    SYS-MM              TO  WRK-SMM
           MOVE    SYS-DD              TO  WRK-SDD
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    DB OPEN
      *    PERFORM 100-DBOPEN-SEC
      *    IF      MCP-RC      NOT =   ZERO
      *        DISPLAY "DBOPEN ERR"
      *    END-IF.
      *
           PERFORM 101-ASSIN-GET-SEC
      *
      *    入力　初期処理
           OPEN    INPUT               IN-FILE
           IF  STS-IN              NOT =   ZERO
               MOVE    1               TO  FLG-IN
               DISPLAY "*(ORCMUP0161)* IN-FILE OPN STS["
                                       STS-IN "]"
           END-IF
      *
      *    出力ファイル　初期処理
           PERFORM 2700-OUTLST-INIT-SEC
      *
      *    プログラムオプションデータ読込み処理
           IF      FLG-IN              =   ZERO
               PERFORM 900-IN-KENSAKU-SEC
           END-IF
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
           MOVE    SPACE               TO  ASS-IN
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
           MOVE    SPACE               TO  ASS-ERR
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
           MOVE    SPACE               TO  ASS-PLF
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   SPA-HOSPNUM         DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   "orca-mstlog/"      DELIMITED  BY  SIZE
                   LNK-UPFILE          DELIMITED  BY  SPACE
                   ".PRF"              DELIMITED  BY  SIZE
                                       INTO    ASS-PLF
           END-STRING
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
           EVALUATE    PRGOPTIONWK-SYORIKBN
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
      *    プログラムオプションデータ読込み処理
           PERFORM 900-IN-KENSAKU-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加  処理
      *****************************************************************
       2010-INSERT-SYORI-SEC       SECTION.
      *
           INITIALIZE                      PRGOPTION-REC
           MOVE    PRGOPTIONWK-KEY     TO  PRGOPTION-KEY
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_prgoption" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM  910-PRGOPTION-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PRGOPTION
           END-IF
      *
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    追加エラー（同一キー存在）
           IF      FLG-PRGOPTION       =   ZERO
      *        MOVE    1               TO  WRK-ERRCD
      *        PERFORM 2040-ERR-SYORI-SEC
               CONTINUE
           ELSE
      *        ＤＢ追加
               PERFORM 20101-PRGOPTION-INS-SEC
           END-IF
           .
       2010-INSERT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ更新  処理
      *****************************************************************
       2020-UPDATE-SYORI-SEC           SECTION.
      *
           INITIALIZE                      PRGOPTION-REC
           MOVE    PRGOPTIONWK-KEY     TO  PRGOPTION-KEY
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_prgoption"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM  910-PRGOPTION-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PRGOPTION
           END-IF
      *
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF      FLG-PRGOPTION       =   ZERO
      *        ＤＢ更新
               PERFORM 20101-PRGOPTION-UPD-SEC
           ELSE
               MOVE    5               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
      *        ＤＢ追加
               PERFORM 20101-PRGOPTION-INS-SEC
           END-IF
           .
       2020-UPDATE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除  処理
      *****************************************************************
       2030-DELETE-SYORI-SEC       SECTION.
      *
           INITIALIZE                      PRGOPTION-REC
           MOVE    PRGOPTIONWK-KEY     TO  PRGOPTION-KEY
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_prgoption"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM  910-PRGOPTION-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PRGOPTION
           END-IF
      *
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF      FLG-PRGOPTION       =   ZERO
      *        ＤＢ削除
               PERFORM 20101-PRGOPTION-DEL-SEC
           ELSE
               MOVE    6               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           .
       2030-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ追加  処理
      *****************************************************************
       20101-PRGOPTION-INS-SEC  SECTION.
      *
           PERFORM 220-PRGOPTION-EDIT-SEC
      *
           MOVE    LNK-MNTYMD          TO  PRGOPTION-CREYMD
                                           PRGOPTION-UPYMD
           MOVE    LNK-MNTHMS          TO  PRGOPTION-UPHMS
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    1               TO  LNK-RC
               MOVE    2               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           IF      WRK-ERRCD       =   ZERO
               ADD     1               TO  CNT-INS
           END-IF
           .
       20101-PRGOPTION-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ更新  処理
      *****************************************************************
       20101-PRGOPTION-UPD-SEC  SECTION.
      *
      *    ユーザがオプションを編集していた場合は更新しない
           IF     (PRGOPTIONWK-KBNCD   =   "*")
             AND  (PRGOPTION-OPID     NOT = SPACE)
             AND  (PRGOPTION-UPYMD    NOT = SPACE)
               PERFORM 220-PRGOPTION-HENSYU-SEC
           ELSE
               PERFORM 220-PRGOPTION-EDIT-SEC
           END-IF
      *
               MOVE    PRGOPTION-REC       TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_prgoption"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          NOT =   ZERO
                   MOVE    2               TO  LNK-RC
                   MOVE    3               TO  WRK-ERRCD
                   PERFORM 2040-ERR-SYORI-SEC
               END-IF
               IF      WRK-ERRCD           =   ZERO
                   ADD     1               TO  CNT-UPD
               END-IF
      *     END-IF
           .
       20101-PRGOPTION-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ削除  処理
      *****************************************************************
       20101-PRGOPTION-DEL-SEC  SECTION.
      *
           INITIALIZE                      PRGOPTION-REC
           MOVE    PRGOPTIONWK-REC(2:) TO  PRGOPTION-REC
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    3               TO  LNK-RC
               MOVE    4               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           IF      WRK-ERRCD           =   ZERO
               ADD     1               TO  CNT-DEL
           END-IF
           .
       20101-PRGOPTION-DEL-EXT.
           EXIT.
      *****************************************************************
      *    オプション編集処理
      *****************************************************************
       220-PRGOPTION-EDIT-SEC         SECTION.
      *
           INITIALIZE                      PRGOPTION-REC
           MOVE    PRGOPTIONWK-REC(2:) TO  PRGOPTION-REC
      *
           MOVE    SPACE               TO  WRK-OPTION
           MOVE    1                   TO  WRK-OPTION-LEN
           STRING  PRGOPTION-OPTION    DELIMITED   BY  SIZE
           INTO    WRK-OPTION
           WITH    POINTER             WRK-OPTION-LEN
           END-STRING
      *
           COMPUTE WRK-OPTION-LEN      =   WRK-OPTION-LEN  -  1
      *
           MOVE    1                   TO  IDX-PNT1
           MOVE    1                   TO  IDX-PNT2
           MOVE    SPACE               TO  WRK-OPTION
      *
           PERFORM UNTIL ( IDX-PNT1    >   WRK-OPTION-LEN )
                    OR   ( PRGOPTION-OPTION (IDX-PNT1 : ) 
                                       =   SPACE )
      *
               MOVE    ZERO            TO  FLG-LF
      *
               IF    ( IDX-PNT1        <   WRK-OPTION-LEN )
                   IF    ( PRGOPTION-OPTION (IDX-PNT1 : 2 )
                                       =   "\n" )
                       MOVE    1       TO  FLG-LF
                   END-IF
               END-IF
      *
               IF  ( FLG-LF  =  ZERO )
                   MOVE    PRGOPTION-OPTION (IDX-PNT1 : 1)
                                        TO  WRK-OPTION (IDX-PNT2 : 1)
                   COMPUTE IDX-PNT2 =   IDX-PNT2    +   1
               ELSE
                   MOVE    X"0A"        TO  WRK-OPTION (IDX-PNT2 : 1)
                   COMPUTE IDX-PNT2 =   IDX-PNT2   +   1
                   COMPUTE IDX-PNT1 =   IDX-PNT1   +   1
               END-IF
      *
                   COMPUTE IDX-PNT1 =   IDX-PNT1   +   1
      *
           END-PERFORM
      *
           MOVE    WRK-OPTION          TO  PRGOPTION-OPTION
      *
           MOVE    LNK-MNTYMD          TO  PRGOPTION-UPYMD
           MOVE    LNK-MNTHMS          TO  PRGOPTION-UPHMS
      *
           .
       220-PRGOPTION-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    オプション編集マージ処理
      *****************************************************************
       220-PRGOPTION-HENSYU-SEC       SECTION.
      *
      *    既存データ
           MOVE    SPACE               TO  WRK-OPTION
           MOVE    1                   TO  WRK-OPTION-LEN
           STRING  PRGOPTION-OPTION    DELIMITED   BY  SIZE
           INTO    WRK-OPTION
           WITH    POINTER             WRK-OPTION-LEN
           END-STRING
      *
           COMPUTE WRK-OPTION-LEN      =   WRK-OPTION-LEN  -  1
      *
           MOVE    0                   TO  CNT-MAE
           MOVE    1                   TO  IDX-PNT1
           MOVE    1                   TO  IDX-PNT2
           MOVE    SPACE               TO  WRK-MAE-NAIYO
           MOVE    SPACE               TO  WRK-MAE-AREA
      *
           PERFORM UNTIL ( IDX-PNT1    >   WRK-OPTION-LEN )
                   OR    ( PRGOPTION-OPTION(IDX-PNT1:) =   SPACE )
      *        行が終わったとき
               IF    ( PRGOPTION-OPTION(IDX-PNT1:1)    =   X"0A" )
                   MOVE    ZERO       TO  FLG-LF
      *            コメントチェック
                   IF      WRK-MAE-NAIYO(1:1)  NOT =   "#"
                       PERFORM VARYING IDX-PNT2 FROM 1 BY 1
                               UNTIL ( IDX-PNT2 >   200 )
                               OR    ( WRK-MAE-NAIYO(IDX-PNT2:)
                                                           =   SPACE )
                           IF      WRK-MAE-NAIYO(IDX-PNT2:1)   =   "="
                               MOVE    1               TO  FLG-LF
                               MOVE    200             TO  IDX-PNT2
                           END-IF
                       END-PERFORM
                   END-IF
      *            コメントでない場合セット
                   IF      FLG-LF      =   1
                       COMPUTE CNT-MAE     =   CNT-MAE     +   1
                       MOVE    WRK-MAE-NAIYO
                                           TO  WRK-MAE-OPTION(CNT-MAE)
                   END-IF
                   MOVE    1               TO  IDX-PNT2
                   MOVE    SPACE           TO  WRK-MAE-NAIYO
               ELSE
                   MOVE    PRGOPTION-OPTION(IDX-PNT1:1)
                                           TO  WRK-MAE-NAIYO(IDX-PNT2:1)
                   COMPUTE IDX-PNT2    =   IDX-PNT2    +   1
               END-IF
               COMPUTE IDX-PNT1    =   IDX-PNT1   +   1
           END-PERFORM
      *    最終行
           MOVE    ZERO       TO  FLG-LF
      *    コメントチェック
           IF      WRK-MAE-NAIYO(1:1)  NOT =   "#"
               PERFORM VARYING IDX-PNT2 FROM 1 BY 1
                       UNTIL ( IDX-PNT2 >   200 )
                       OR    ( WRK-MAE-NAIYO(IDX-PNT2:)    =   SPACE )
                   IF      WRK-MAE-NAIYO(IDX-PNT2:1)   =   "="
                       MOVE    1               TO  FLG-LF
                       MOVE    200             TO  IDX-PNT2
                   END-IF
               END-PERFORM
           END-IF
      *    コメントでない場合セット
           IF      FLG-LF      =   1
               COMPUTE CNT-MAE     =   CNT-MAE     +   1
               MOVE    WRK-MAE-NAIYO   TO  WRK-MAE-OPTION(CNT-MAE)
           END-IF
      *----(04.07.01)--ADD-START---
           IF      CNT-MAE     =   ZERO
               MOVE    SPACE           TO  WRK-OPID
           ELSE
               MOVE    PRGOPTION-OPID  TO  WRK-OPID
           END-IF
      *----(04.07.01)--ADD-END-----
      *
      *    更新データ
           INITIALIZE                      PRGOPTION-REC
           MOVE    PRGOPTIONWK-REC(2:) TO  PRGOPTION-REC
      *
           MOVE    SPACE               TO  WRK-OPTION
           MOVE    1                   TO  WRK-OPTION-LEN
           STRING  PRGOPTION-OPTION    DELIMITED   BY  SIZE
           INTO    WRK-OPTION
           WITH    POINTER             WRK-OPTION-LEN
           END-STRING
      *
           COMPUTE WRK-OPTION-LEN      =   WRK-OPTION-LEN  -  1
      *
           MOVE    0                   TO  CNT-ATO
           MOVE    1                   TO  IDX-PNT1
           MOVE    1                   TO  IDX-PNT2
           MOVE    SPACE               TO  WRK-ATO-NAIYO
           MOVE    SPACE               TO  WRK-ATO-AREA
      *
           PERFORM UNTIL ( IDX-PNT1    >   WRK-OPTION-LEN )
                   OR    ( PRGOPTION-OPTION(IDX-PNT1:) =   SPACE )
      *        行が終わるまで
               IF    ( PRGOPTION-OPTION(IDX-PNT1:2)    =   "\n" )
                   COMPUTE CNT-ATO     =   CNT-ATO     +   1
                   MOVE    WRK-ATO-NAIYO   TO  WRK-ATO-OPTION(CNT-ATO)
                   MOVE    1               TO  IDX-PNT2
                   MOVE    SPACE           TO  WRK-ATO-NAIYO
                   COMPUTE IDX-PNT1    =   IDX-PNT1    +   1
               ELSE
                   MOVE    PRGOPTION-OPTION(IDX-PNT1:1)
                                       TO  WRK-ATO-NAIYO(IDX-PNT2:1)
                   COMPUTE IDX-PNT2    =   IDX-PNT2   +   1
               END-IF
               COMPUTE IDX-PNT1    =   IDX-PNT1   +   1
           END-PERFORM
      *    最終行
           COMPUTE CNT-ATO     =   CNT-ATO     +   1
           MOVE    WRK-ATO-NAIYO   TO  WRK-ATO-OPTION(CNT-ATO)
      *
      *    マージ処理
           MOVE    1                   TO  IDX-PNT1
           MOVE    1                   TO  IDX-PNT2
           MOVE    1                   TO  IDX-PNT3
           MOVE    SPACE               TO  WRK-OPTION
           MOVE    SPACE               TO  WRK-MAE-NAIYO
           MOVE    SPACE               TO  WRK-ATO-NAIYO
      *
           PERFORM VARYING IDX FROM 1 BY 1
                   UNTIL ( IDX >   CNT-ATO )
               MOVE    WRK-ATO-OPTION(IDX) TO  WRK-ATO-NAIYO
               MOVE    ZERO            TO  FLG-LF
               PERFORM VARYING IDX-PNT1 FROM 1 BY 1
                       UNTIL ( IDX-PNT1 >   200 )
                       OR    ( WRK-ATO-NAIYO(IDX-PNT1:)    =  SPACE )
                       OR    ( FLG-LF  =   1 )
      *            コメントチェック
                   IF      WRK-ATO-NAIYO(IDX-PNT1:1)   =   "="
                       PERFORM VARYING IDY FROM 1 BY 1
                               UNTIL ( IDY >   CNT-MAE )
                               OR    ( FLG-LF  =   1 )
                           MOVE    WRK-MAE-OPTION(IDY) TO  WRK-MAE-NAIYO
      *                    オプションチェック
                           IF      WRK-ATO-NAIYO(1:1)    =   "#"
                               COMPUTE IDX-PNT2  =   IDX-PNT1  -   1
                               IF      WRK-ATO-NAIYO(2:IDX-PNT2)
                                   =   WRK-MAE-NAIYO(1:IDX-PNT2)
                                   MOVE    1       TO  FLG-LF
                               END-IF
                           ELSE
                               IF      WRK-ATO-NAIYO(1:IDX-PNT1)
                                   =   WRK-MAE-NAIYO(1:IDX-PNT1)
                                   MOVE    1       TO  FLG-LF
                               END-IF
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
      *
               IF  ( FLG-LF  =  ZERO )
                   PERFORM VARYING IDX-PNT1 FROM 1 BY 1
                           UNTIL ( IDX-PNT1 >   200 )
                           OR    ( WRK-ATO-NAIYO(IDX-PNT1:)  =   SPACE )
                       MOVE    WRK-ATO-NAIYO(IDX-PNT1:1)
                                       TO  WRK-OPTION(IDX-PNT3:1)
                       COMPUTE IDX-PNT3    =   IDX-PNT3   +   1
                   END-PERFORM
               ELSE
                   PERFORM VARYING IDX-PNT1 FROM 1 BY 1
                           UNTIL ( IDX-PNT1 >   200 )
                           OR    ( WRK-MAE-NAIYO(IDX-PNT1:)  =   SPACE )
                       MOVE    WRK-MAE-NAIYO(IDX-PNT1:1)
                                       TO  WRK-OPTION(IDX-PNT3:1)
                       COMPUTE IDX-PNT3    =   IDX-PNT3   +   1
                   END-PERFORM
               END-IF
               MOVE    X"0A"       TO  WRK-OPTION(IDX-PNT3:1)
               COMPUTE IDX-PNT3    =   IDX-PNT3   +   1
           END-PERFORM
      *
           MOVE    WRK-OPTION          TO  PRGOPTION-OPTION
      *----(04.07.01)--ADD-START---
           MOVE    WRK-OPID            TO  PRGOPTION-OPID
      *----(04.07.01)--ADD-END-----
      *
           MOVE    LNK-MNTYMD          TO  PRGOPTION-UPYMD
           MOVE    LNK-MNTHMS          TO  PRGOPTION-UPHMS
           .
       220-PRGOPTION-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    エラー処理
      *****************************************************************
       2040-ERR-SYORI-SEC          SECTION.
      *
      *    エラー出力
           MOVE    PRGOPTIONWK-REC     TO  ERR-PRGOPTIONWK-REC
           WRITE   ERR-PRGOPTIONWK-REC
           ADD     1                   TO  CNT-ERR
      *
      *    エラーメッセージ出力
           PERFORM 20401-PLF-OUT-SEC
           .
       2040-ERR-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ出力処理
      *****************************************************************
       20401-PLF-OUT-SEC           SECTION.
      *
           MOVE    PRGOPTIONWK-SYORIKBN  TO  WRK-SYORIKBN
           MOVE    PRGOPTIONWK-PRGID     TO  WRK-PRGID
           MOVE    PRGOPTIONWK-KBNCD     TO  WRK-KBNCD
           EVALUATE    WRK-ERRCD
               WHEN    1
                   MOVE  "オプションマスタ登録済み"
                                       TO  WRK-MSG-01
               WHEN    2
                   MOVE  "追加エラー"  TO  WRK-MSG-01
               WHEN    3
                   MOVE  "更新エラー"  TO  WRK-MSG-01
               WHEN    4
                   MOVE  "削除エラー"  TO  WRK-MSG-01
               WHEN    5
                   MOVE  "更新対象なし"  TO  WRK-MSG-01
               WHEN    6
                   MOVE  "削除対象なし"  TO  WRK-MSG-01
           END-EVALUATE
      *
           MOVE    WRK-MSG-AREA        TO  PLF-REC
           WRITE                           PLF-REC
           ADD     1                   TO  CNT-PLF
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
           IF  STS-ERR             NOT =   ZERO
               DISPLAY "*(ORCMUP0161)* OUTFILE OPN STS[" STS-ERR "]"
               GO      TO              2700-OUTLST-INIT-EXT
           END-IF
      *
      *    エラーレコード出力ファイル　オープン
           OPEN    OUTPUT              PLF-FILE
           IF  STS-PLF             NOT =   ZERO
               DISPLAY "*(ORCMUP0161)* PLFFILE OPN STS[" STS-PLF "]"
               GO      TO              2700-OUTLST-INIT-EXT
           END-IF
           MOVE    WRK-SYY             TO  MES-TITLE1-YY
           MOVE    SYS-MM              TO  MES-TITLE1-MM
           MOVE    SYS-DD              TO  MES-TITLE1-DD
           MOVE    SPA-HOSPNUM         TO  MES-TITLE1-HOSPNUM
           MOVE    MES-TITLE1          TO  PLF-REC
           WRITE   PLF-REC
           .
       2700-OUTLST-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *    DB DISCONNECT
      *    PERFORM 900-DBDISCONNECT-SEC
      *
      *    出力ファイル　終了処理
           CLOSE                       IN-FILE
           CLOSE                       ERR-FILE
           CLOSE                       PLF-FILE
      *
           IF    ( LNK-RC              =   ZERO )
               IF    ( CNT-IN      NOT =   LNK-COUNT )
      *
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
           DISPLAY "*(ORCMUP0161)* INPUT  /I   CNT[" CNT-IN  "]"
           DISPLAY "*(ORCMUP0161)*  INSERT     CNT[" CNT-INS "]"
           DISPLAY "*(ORCMUP0161)*  UPDATE     CNT[" CNT-UPD "]"
           DISPLAY "*(ORCMUP0161)*  DELETE     CNT[" CNT-DEL "]"
           DISPLAY "*(ORCMUP0161)* ERR    /O   CNT[" CNT-ERR "]"
           DISPLAY "*(ORCMUP0161)* PLF    /O   CNT[" CNT-PLF "]"
           DISPLAY "*(ORCMUP0161)* RC             [" LNK-RC "]"
      **** ACCEPT  WRK-IN
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *     プログラムオプション読込み処理
      *****************************************************************
       900-IN-KENSAKU-SEC          SECTION.
      *
          READ    IN-FILE
              AT  END
                  MOVE    1               TO  FLG-IN
              NOT AT  END
                  MOVE    ZERO            TO  FLG-IN
                  ADD     1               TO  CNT-IN
          END-READ
      *
          IF   ( FLG-IN            =     ZERO )
               PERFORM 900-IN-DATA-SET-SEC
          END-IF
          .
       900-IN-KENSAKU-EXT.
          EXIT.
      *
      *****************************************************************
      *    プログラムオプションマスタ変更データ設定処理
      *****************************************************************
       900-IN-DATA-SET-SEC         SECTION.
      *
           EVALUATE    TRUE
           WHEN    LNK-DBRVERSION (3:6)    <   "040400"
               PERFORM 900-PRGWK-V01-SET
      *
           WHEN    OTHER
               PERFORM 900-PRGWK-SET
           END-EVALUATE
          .
       900-IN-DATA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更後レイアウト移送処理
      *****************************************************************
       900-PRGWK-SET              SECTION.
      *
           MOVE    SPACE               TO  PRGOPTIONWK-REC
           MOVE    IN-REC              TO  PRGWK-REC
      *
           MOVE    PRGWK-SYORIKBN      TO  PRGOPTIONWK-SYORIKBN
           MOVE    SPA-HOSPNUM         TO  PRGOPTIONWK-HOSPNUM
           MOVE    PRGWK-PRGID         TO  PRGOPTIONWK-PRGID
           MOVE    PRGWK-KBNCD         TO  PRGOPTIONWK-KBNCD
           MOVE    PRGWK-ADDKBN        TO  PRGOPTIONWK-ADDKBN
           MOVE    PRGWK-TERMID        TO  PRGOPTIONWK-TERMID
           MOVE    PRGWK-OPID          TO  PRGOPTIONWK-OPID
           MOVE    PRGWK-CREYMD        TO  PRGOPTIONWK-CREYMD
           MOVE    PRGWK-UPYMD         TO  PRGOPTIONWK-UPYMD
           MOVE    PRGWK-UPHMS         TO  PRGOPTIONWK-UPHMS
           MOVE    PRGWK-OPTION        TO  PRGOPTIONWK-OPTION
           DISPLAY "PRGID = " PRGWK-PRGID
           DISPLAY "KBNCD = " PRGWK-KBNCD
           DISPLAY "ADDKBN = " PRGWK-ADDKBN
      *
          .
       900-PRGWK-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更後レイアウト移送処理
      *****************************************************************
       900-PRGWK-V01-SET          SECTION.
      *
           MOVE    SPACE               TO  PRGOPTIONWK-REC
           MOVE    IN-REC              TO  PRGWK-REC-V01
      *
           MOVE    PRGWK-V01-AREA-01   TO  PRGOPTIONWK-REC (1 : 5044)
           MOVE    PRGWK-V01-AREA-02   TO  PRGOPTIONWK-REC (44045 : 55)
           MOVE    SPA-HOSPNUM         TO  PRGOPTIONWK-HOSPNUM
      *
          .
       900-PRGWK-V01-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション読込
      *****************************************************************
       910-PRGOPTION-READ-SEC   SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PRGOPTION-REC
               MOVE    ZERO                TO  FLG-PRGOPTION
           ELSE
               INITIALIZE                      PRGOPTION-REC
               MOVE    1                   TO  FLG-PRGOPTION
           END-IF
           .
       910-PRGOPTION-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-TBL-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-TBL-CLOSE-EXT.
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
