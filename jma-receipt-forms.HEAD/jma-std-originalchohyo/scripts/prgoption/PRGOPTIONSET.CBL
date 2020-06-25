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
       PROGRAM-ID.                 PRGOPTIONSET.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : プラグイン処理
      *  コンポーネント名  : プログラムオプション更新処理
      *                    : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  14/10/02    uchi    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  02.00.01    門間         15/04/15  END-IF漏れを修正
      *                                     グループ診療対応
      *  02.00.02    門間         16/06/17  マージ処理を修正
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    入力：システム管理更新ファイル
           SELECT  IN-FILE         ASSIGN
                                   ASS-IN
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-IN.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    入力：システム管理更新ファイル
       FD  IN-FILE.
       01  IN-REC                      PIC  X(50200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "COMMON-SPA".
      *
      *    ファイル指定領域
       01  ASS-AREA.
           03  ASS-IN                  PIC X(256).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-IN                  PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-IN                  PIC 9(01).
           03  FLG-PRGOPTION           PIC 9(01).
           03  FLG-LF                  PIC 9(01).
      *
           03  FLG-SYS                 PIC 9(01).
           03  FLG-CHECK               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX-PNT1                PIC 9(05).
           03  IDX-PNT2                PIC 9(05).
           03  IDX-PNT3                PIC 9(05).
           03  IDX                     PIC 9(03).
           03  IDY                     PIC 9(03).
           03  IDXL                    PIC 9(05).
           03  IDXP                    PIC 9(05).
           03  IDXS                    PIC 9(05).
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
           03  WRK-LAST-STYUKYMD       PIC X(08).
           03  WRK-LAST-EDYUKYMD       PIC X(08).
      *
           03  WRK-IN                  PIC X(01).
           03  WRK-OPTION              PIC X(60000).
           03  WRK-OPTION-LEN          PIC 9(08).
      *----(04.07.01)--ADD-START---
           03  WRK-OPID                PIC X(16).
      *----(04.07.01)--ADD-END-----
      *
           03  WRK-MAE-NAIYO           PIC X(500).
           03  WRK-ATO-NAIYO           PIC X(500).
           03  WRK-MAE-AREA.
               05  WRK-MAE-OPTION      PIC X(500) OCCURS 200.
           03  WRK-ATO-AREA.
               05  WRK-ATO-OPTION      PIC X(500) OCCURS 100.
      *
      *　　主キー更新用項目
           03  WRK-SYSUPD-KEY.
               05  WRK-SYSUP-HOSPNUM   PIC 9(02).
               05  WRK-SYSUP-KANRICD   PIC X(04).
               05  WRK-SYSUP-KBNCD     PIC X(08).
               05  WRK-SYSUP-STYUKYMD  PIC X(08).
               05  WRK-SYSUP-EDYUKYMD  PIC X(08).
      *
           03  WRK-FILENAME            PIC X(40).
           03  WRK-PRGID               PIC X(20).
           03  WRK-KBNCD               PIC X(20).
           03  WRK-DATA                PIC X(4000).
      *
       01  LNK-AREA.
           03  LNK-HOSPNUM             PIC  X(02).
           03  LNK-YMD                 PIC  X(08).
           03  LNK-HMS                 PIC  X(06).
           03  LNK-DIR                 PIC  X(100).
           03  LNK-FILENAME            PIC  X(30).
           03  LNK-KAKU                PIC  X(03).
      *
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *    一時
       01  PRGOPTIONWK-REC.
           03  PRGOPTIONWK-SYORIKBN          PIC  X(01).
           COPY    "CPPRGOPTION.INC"  REPLACING  //PRGOPTION-//
                                             BY  //PRGOPTIONWK-//.
      *
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
       01  COMMAND-PARAM.
           02  FILLER  PIC X(256).
      *
       PROCEDURE                       DIVISION
           USING
           COMMAND-PARAM.
      *
      ****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF  FLG-IN  =  ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
      *****STOP    RUN
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
           MOVE    ZERO            TO  IDX-AREA
           INITIALIZE                  WRK-AREA
                                       SPA-AREA
      *    引数
           UNSTRING  COMMAND-PARAM  DELIMITED  BY  ","
                                    INTO  LNK-HOSPNUM
                                          LNK-YMD
                                          LNK-HMS
                                          LNK-DIR
                                          LNK-FILENAME
                                          LNK-KAKU
           END-UNSTRING
      *
           DISPLAY "START"
      *    対象となるファイル名の作成
           STRING  LNK-FILENAME  DELIMITED  BY  SPACE
                   "."           DELIMITED  BY  SIZE
                   LNK-KAKU      DELIMITED  BY  SPACE
                                 INTO       WRK-FILENAME
           END-STRING
      *    PRGID,KBNCDを割り出す
           IF  LNK-KAKU = "opt"
               UNSTRING  LNK-FILENAME  DELIMITED  BY  "-"
                                       INTO  WRK-PRGID
                                             WRK-KBNCD
               END-UNSTRING
               IF  WRK-KBNCD = SPACE
                 MOVE  "*"  TO  WRK-KBNCD
               END-IF
           ELSE
               MOVE   LNK-FILENAME     TO  WRK-PRGID
               MOVE   "EXPLANATION"    TO  WRK-KBNCD
           END-IF
      *
           MOVE    LNK-HOSPNUM     TO  SPA-HOSPNUM
      *
      *    DB OPEN
           PERFORM 100-DBOPEN-SEC
      *
      *    ファイル情報
           PERFORM 101-ASSIN-GET-SEC
      *
      *    入力　初期処理
           OPEN    INPUT           IN-FILE
           IF  STS-IN          NOT =   ZERO
               MOVE    1           TO  FLG-IN
               DISPLAY "*(SYS3004)* IN-FILE OPN STS["
                                       STS-IN "]"
           END-IF
      *
      *    プログラムオプションデータ読込み処理
           IF      FLG-IN              =   ZERO
               PERFORM 900-IN-KENSAKU-SEC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル情報取得
      *****************************************************************
       101-ASSIN-GET-SEC                SECTION.
      *
      *    入力ファイル
           MOVE    SPACE               TO  ASS-IN
           STRING  LNK-DIR             DELIMITED  BY  SPACE
                   "/"                 DELIMITED  BY  SIZE
                   WRK-FILENAME        DELIMITED  BY  SPACE
                                       INTO    ASS-IN
           END-STRING
      *
           .
       101-ASSIN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    プログラムオプションの存在チェック
           PERFORM  2001-DATA-CHECK-SEC
           IF  FLG-CHECK  =  0
      *        データ追加
               PERFORM  20101-PRGOPTION-INS-SEC
           ELSE
      *        「EXPLANATION」「*」以外は更新しない
               IF  (PRGOPTIONWK-KBNCD  =  "EXPLANATION")
               OR  (PRGOPTIONWK-KBNCD  =  "*")
      *            データ更新
                   PERFORM  20101-PRGOPTION-UPD-SEC
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション存在チェック  処理
      *****************************************************************
       2001-DATA-CHECK-SEC               SECTION.
      *
      *    同様のPRGID,KBNCDのデータがあるかどうか
           INITIALIZE                  PRGOPTION-REC
      *
           MOVE    SPA-HOSPNUM         TO  PRGOPTION-HOSPNUM
           MOVE    PRGOPTIONWK-PRGID   TO  PRGOPTION-PRGID
           MOVE    PRGOPTIONWK-KBNCD   TO  PRGOPTION-KBNCD
      *
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_prgoption"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM  910-PRGOPTION-READ-SEC
               
               IF  FLG-SYS  =  0
                   MOVE      1             TO  FLG-CHECK
                   DISPLAY  "ALREADY  SET!"
               END-IF
           END-IF
      *
           MOVE  "tbl_prgoption"       TO  MCP-TABLE
           MOVE  "key"                 TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2001-DATA-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ追加  処理
      *****************************************************************
       20101-PRGOPTION-INS-SEC  SECTION.
      *
           PERFORM 220-PRGOPTION-EDIT-SEC
      *
           MOVE    "1"                 TO  PRGOPTION-ADDKBN
           MOVE    LNK-YMD             TO  PRGOPTION-CREYMD
           MOVE    LNK-HMS             TO  PRGOPTION-UPHMS
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *     IF      MCP-RC          NOT =   ZERO
      *         MOVE    1               TO  LNK-RC
      *         MOVE    2               TO  WRK-ERRCD
      *         PERFORM 2040-ERR-SYORI-SEC
      *     END-IF
      *     IF      WRK-ERRCD       =   ZERO
      *         ADD     1               TO  CNT-INS
      *     END-IF
           IF      MCP-RC              =   ZERO
               DISPLAY  "DATA INSERT!"
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
      *    説明文は更新データに置き換え
      *    オプションデータは既存データと比較して置き換え
           IF     (PRGOPTIONWK-KBNCD   =   "EXPLANATION")
               PERFORM 220-PRGOPTION-EDIT-SEC
           ELSE
               PERFORM 220-PRGOPTION-HENSYU-SEC
           END-IF
      *
           MOVE    "1"                 TO  PRGOPTION-ADDKBN
           MOVE    LNK-YMD             TO  PRGOPTION-CREYMD
           MOVE    LNK-HMS             TO  PRGOPTION-UPHMS
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *     IF      MCP-RC          NOT =   ZERO
      *         MOVE    2               TO  LNK-RC
      *         MOVE    3               TO  WRK-ERRCD
      *         PERFORM 2040-ERR-SYORI-SEC
      *     END-IF
      *     IF      WRK-ERRCD           =   ZERO
      *         ADD     1               TO  CNT-UPD
      *     END-IF
           IF      MCP-RC              =   ZERO
               DISPLAY  "DATA UPDATE!"
           END-IF
           .
       20101-PRGOPTION-UPD-EXT.
           EXIT.
      *
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
           MOVE    SPACE               TO  WRK-MAE-AREA
      *
      *
           PERFORM UNTIL ( IDX-PNT1    >   WRK-OPTION-LEN )
                   OR    ( PRGOPTION-OPTION(IDX-PNT1:) =   SPACE )
      *
               MOVE    SPACE               TO  WRK-MAE-NAIYO
               UNSTRING    PRGOPTION-OPTION    DELIMITED   BY  X"0A"
               INTO    WRK-MAE-NAIYO
               WITH    POINTER IDX-PNT1
               END-UNSTRING

               PERFORM VARYING IDX-PNT2 FROM 1 BY 1
                       UNTIL ( IDX-PNT2 >   500 )
                       OR    ( WRK-MAE-NAIYO(IDX-PNT2:)
                                                   =   SPACE )
                   IF      WRK-MAE-NAIYO(IDX-PNT2:1)   =   "="
                       COMPUTE CNT-MAE     =   CNT-MAE     +   1
                       MOVE    WRK-MAE-NAIYO
                                   TO  WRK-MAE-OPTION(CNT-MAE)
                       MOVE    500 TO  IDX-PNT2
                   END-IF
               END-PERFORM
           END-PERFORM
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
                       UNTIL ( IDX-PNT1 >   500 )
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
                       DISPLAY  "FLG-LF  :  "  FLG-LF
                               END-IF
                           ELSE
                               MOVE    ZERO        TO  IDXS
                                                       IDXL
                               PERFORM VARYING IDXP    FROM    1   BY  1
                                       UNTIL ( IDXP    >   500 )
                                        OR   ( WRK-ATO-NAIYO(IDXP:)
                                                   =   SPACE )
                                        OR   ( IDXL    >   ZERO )
                                   IF    ( IDXS    =   ZERO )
                                       IF    ( WRK-MAE-NAIYO(IDXP:1)
                                               NOT =   "#" )
                                           MOVE    IDXP    TO  IDXS
                                       END-IF
                                   ELSE
                                       IF    ( WRK-MAE-NAIYO(IDXP:1)
                                                   =   "=" )
                                         COMPUTE IDXL = IDXP - IDXS + 1
                                       END-IF
                                   END-IF
                               END-PERFORM
                               IF    ( IDXL    >   ZERO )
                                   IF      WRK-ATO-NAIYO(1:IDX-PNT1)
                                       =   WRK-MAE-NAIYO(IDXS:IDXL)
                                       MOVE    1       TO  FLG-LF
                       DISPLAY  "FLG-LF  :  "  FLG-LF
                                   END-IF
                               END-IF
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
      *
                       DISPLAY  "FLG-LF  CHECK:  "  FLG-LF
               IF  ( FLG-LF  =  ZERO )
                   PERFORM VARYING IDX-PNT1 FROM 1 BY 1
                           UNTIL ( IDX-PNT1 >   500 )
                           OR    ( WRK-ATO-NAIYO(IDX-PNT1:)  =   SPACE )
                       MOVE    WRK-ATO-NAIYO(IDX-PNT1:1)
                                       TO  WRK-OPTION(IDX-PNT3:1)
                       COMPUTE IDX-PNT3    =   IDX-PNT3   +   1
                   END-PERFORM
                       DISPLAY  "ATO IN"
               ELSE
                   PERFORM VARYING IDX-PNT1 FROM 1 BY 1
                           UNTIL ( IDX-PNT1 >   500 )
                           OR    ( WRK-MAE-NAIYO(IDX-PNT1:)  =   SPACE )
                       MOVE    WRK-MAE-NAIYO(IDX-PNT1:1)
                                       TO  WRK-OPTION(IDX-PNT3:1)
                       COMPUTE IDX-PNT3    =   IDX-PNT3   +   1
                   END-PERFORM
                       DISPLAY  "MAE IN"
               END-IF
               MOVE    X"0A"       TO  WRK-OPTION(IDX-PNT3:1)
               COMPUTE IDX-PNT3    =   IDX-PNT3   +   1
           END-PERFORM
      *
           MOVE    WRK-OPTION          TO  PRGOPTION-OPTION
           .
       220-PRGOPTION-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *    DB DISCONNECT
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "END"
      *    出力ファイル　終了処理
           CLOSE                       IN-FILE
      *
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
               MOVE  IN-REC               TO  PRGOPTIONWK-REC
               MOVE  SPA-HOSPNUM          TO  PRGOPTIONWK-HOSPNUM
           END-IF
          .
       900-IN-KENSAKU-EXT.
          EXIT.
      *
      *****************************************************************
      *    プログラムオプション読込
      *****************************************************************
       910-PRGOPTION-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PRGOPTION-REC
               MOVE    ZERO            TO  FLG-SYS
           ELSE
               INITIALIZE              PRGOPTION-REC
               MOVE    1               TO  FLG-SYS
           END-IF
      *
           .
       910-PRGOPTION-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
      *
       990-DBCLOSE-EXT.
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
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
