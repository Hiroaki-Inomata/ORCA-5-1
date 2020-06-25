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
       IDENTIFICATION                      DIVISION.
       PROGRAM-ID.                         ORCSTUSAN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 通算日数取得サブ
      *  返却エラーコード  : 00:正常　99:エラー
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/02/06    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03.03.12  日付４の計算方法修正
      *  01.00.02    NACL-太田    03.03.24  特定入院料の考慮を削除
      *  01.00.03    NACL-太田    03.03.12  日付５を追加
      *  01.00.04    NACL-太田    03.06.04  基準日当日の通算日数対象
      *                                     チェック追加
      *  01.00.05    NACL-太田    03.09.10  入力パラメータ特定入院料コード
      *                                     の最大数を１０に変更
      *  03.01.00    NACL-太田    06.06.30  入院料が拾えないケースを認める
      *  03.05.00    NACL-多々納  07/04/26  グループ診療対応
      *  03.05.00    NACL-太田    07/05/10  グループ診療対応
      *  04.01.00    NACL-太田    07/12/13  日数８追加
      *****************************************************************
      *
       ENVIRONMENT                         DIVISION.
       CONFIGURATION                       SECTION.
       INPUT-OUTPUT                        SECTION.
       FILE-CONTROL.
      *
      *    点数マスタ
           SELECT  CTENSU-FILE     ASSIGN   ASGN-CTENSU
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  CTNS-KEY
                                   FILE    STATUS  IS  STS-CTENSU.
      *
      *    選定コード置換
           SELECT  CSENTEICDCHG-FILE
                                   ASSIGN   ASGN-CSENTEICDCHG
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  CSENCHG-KEY
                                   FILE    STATUS  IS  STS-CSENTEICDCHG.
      *
       DATA                                DIVISION.
       FILE                                SECTION.
      *
       FD  CTENSU-FILE.
           COPY "CPTENSU.INC"  REPLACING   //TNS-//   BY //CTNS-//
                                           //TNSUP-// BY //CTNSUP-//
                                           //TNSXX-// BY //CTNSXX-//.
      *
       FD  CSENTEICDCHG-FILE.
       01  CSENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC"
                               REPLACING   //SENCHG-// BY //CSENCHG-//.
      *
       WORKING-STORAGE                     SECTION.
      *
           COPY    "CPASGNPARA.INC"    REPLACING //ASGNPARA//
                                       BY        //ASGN-CTENSU//.
      *
           COPY    "CPASGNPARA.INC"    REPLACING //ASGNPARA//
                                       BY        //ASGN-CSENTEICDCHG//.
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-CTENSU                  PIC X(02).
           03  STS-CSENTEICDCHG            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-ERR                     PIC 9(01).
           03  FLG-HIT                     PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-SENTEICDCHG             PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-TOKUTEI                 PIC 9(01).
           03  FLG-TOKUTEI-SITEI           PIC 9(01).
           03  FLG-SEISINZOUAKU            PIC 9(01).
           03  FLG-LOOPEND                 PIC 9(01).
           03  FLG-SET-DIR                 PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-YMD                     PIC X(10). 
           03  WRK-HENYMDG                 PIC X(09).
           03  WRK-SYMD.
               05  WRK-SYY                 PIC 9(04).
               05  WRK-SMM                 PIC 9(02).
               05  WRK-SDD                 PIC 9(02).
           03  WRK-STYMD                   PIC X(08).
           03  WRK-EDYMD                   PIC X(08).
           03  WRK-STSRYYM                 PIC X(06).
           03  WRK-EDSRYYM                 PIC X(06).
           03  WRK-EDSRYYMD.
               05  WRK-EDSRYYMD-YM         PIC X(06).
               05  WRK-EDSRYYMD-DD         PIC 9(02).
           03  WRK-ACCTYMD.
               05  WRK-ACCTYMD-YM          PIC X(06).
               05  WRK-ACCTYMD-DD          PIC 9(02).
           03  WRK-NISSU-G.
               05  WRK-NISSU               PIC 9(05).
               05  WRK-NISSU1              PIC 9(05).
               05  WRK-NISSU2              PIC 9(05).
               05  WRK-ACCT-NISSU          PIC 9(08).
               05  WRK-ACCT-TOK-NISSU1     PIC 9(08).
               05  WRK-ACCT-TOK-NISSU2     PIC 9(08).
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-KEISANYMD               PIC X(08).
           03  WRK-STIDX                   PIC 9(02).
           03  WRK-EDIDX                   PIC 9(02).
           03  WRK-DAY-X                   PIC X(02).
           03  WRK-DAY-R   REDEFINES   WRK-DAY-X.
               05  WRK-DAY-9               PIC 9(02).
           03  WRK-SRYYMD                  PIC X(08). 
           03  WRK-KONKAI-NYUINYMD         PIC X(08).
           03  WRK-SZYMD                   PIC X(08).
      *
       01  CONST-AREA.
           03  CONST-H180701               PIC X(08) VALUE "20060701".
           03  CONST-TOKUTEI-SRYCD-MAX     PIC 9(03) VALUE 10.
           03  CONST-NOT-FOUND             PIC X(09) VALUE "NOT_FOUND".
      *
      *    患者入院履歴ワーク
       01  WKPTNYUINRRK-REC.
           COPY  "CPPTNYUINRRK.INC"    REPLACING  //PTNYUINRRK-// 
                                       BY         //WKPTNYUINRRK-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    入院会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    選定療養費
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
       01  UIDIO-AREA.
           03  UIDIO-RET               PIC X(2).
           03  UIDIO-UID               PIC X(36).
       01  UIDIO-ST                    PIC 9(2).
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    一時ディレクトリ作成サブ
           COPY "CPORCSTEMPDIR.INC".
      *
      *    同日再入院チェックサブ
           COPY    "CPORCSDNTCHK.INC".
      *
       01  LNKNRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-// 
                                       BY         //LNKNRRK-//.
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    共通領域
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                             SECTION.
      *
           COPY    "CPORCSTUSAN.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                           DIVISION    USING
           ORCSTUSANAREA
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                        SECTION.
      *
           INITIALIZE                      STS-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      STUSAN-OUT-AREA
           INITIALIZE                      STUSAN-RC
      *
           MOVE    ZERO                TO  FLG-ERR
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( STUSAN-RC           =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    終了処理
           PERFORM 300-END-SEC
      *
      *     PERFORM 800-DISPLAY-SEC
      *
           .
       000-PROC-EXT.
           EXIT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           PERFORM 800-ORCUIDSET-SEC
      *
           MOVE    SPACE               TO  ASGN-CTENSU
           STRING  "CTENSU-"           DELIMITED BY SIZE
                   UIDIO-UID           DELIMITED BY SPACE
                   ".tmp"              DELIMITED BY SIZE
           INTO    ASGN-CTENSU
           END-STRING
      *
           MOVE    SPACE               TO  ASGN-CSENTEICDCHG
           STRING  "CSENTEICDCHG-"     DELIMITED BY SIZE
                   UIDIO-UID           DELIMITED BY SPACE
                   ".tmp"              DELIMITED BY SIZE
           INTO    ASGN-CSENTEICDCHG
           END-STRING
      *
           INITIALIZE                      SGETTEMP-AREA
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           IF    ( FUNCTION TRIM(SGETTEMP-TEMPDIR) =   "/" )
               INITIALIZE                  STEMPDIR-AREA
               CALL    "ORCSTEMPDIR" USING STEMPDIR-AREA
               MOVE     1              TO  FLG-SET-DIR
           END-IF
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    ASGN-CTENSU         TO  SGETTEMP-BASENAMES (1)
           MOVE    ASGN-CSENTEICDCHG   TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  ASGN-CTENSU
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  ASGN-CSENTEICDCHG
      *
           OPEN    OUTPUT              CTENSU-FILE
           CLOSE   CTENSU-FILE
           OPEN    I-O                 CTENSU-FILE
      *
           OPEN    OUTPUT              CSENTEICDCHG-FILE
           CLOSE   CSENTEICDCHG-FILE
           OPEN    I-O                 CSENTEICDCHG-FILE
      *
      *    パラメータチェック
           PERFORM 1001-PRM-CHK-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    パラメータチェック
      *****************************************************************
       1001-PRM-CHK-SEC                     SECTION.
      *
           IF    ( STUSAN-KJNYMD       =   SPACE )
               MOVE    01              TO  STUSAN-RC
           END-IF
      *
           IF    ( STUSAN-RC           =   ZERO )
               MOVE    STUSAN-KJNYMD   TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
           END-IF
      *
           .
       1001-PRM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                        SECTION.
      *
      *    今回入院分通算処理
           PERFORM 2100-KONKAI-NYUIN-TUSAN-SEC
           IF    ( STUSAN-RC       NOT =   ZERO )
               GO  TO  200-MAIN-EXT
           END-IF
      *
           IF    ( PTNYUINRRK-SHOKAIKBN    =   "1" )
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    過去入院分通算処理
           PERFORM 2100-KAKO-NYUIN-TUSAN-SEC
           IF    ( STUSAN-RC       NOT =   ZERO )
               GO  TO  200-MAIN-EXT
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    今回入院分通算処理
      *****************************************************************
       2100-KONKAI-NYUIN-TUSAN-SEC         SECTION.
      *
           PERFORM 2110-NYUINRRK-SEL-SEC
           IF    ( STUSAN-RC       NOT =   ZERO )
               GO  TO  2100-KONKAI-NYUIN-TUSAN-EXT
           END-IF
      *
           PERFORM 2110-DOJITU-NYUTAI-CHK-SEC
           IF    ( SDNTCHK-STYMD       =   ZERO )
               INITIALIZE              WRK-NISSU-G
               PERFORM 2100-RRK-DATA-SET-SEC
               GO  TO  2100-KONKAI-NYUIN-TUSAN-EXT
           END-IF
      *
           IF    ( WRK-SZYMD            <  SDNTCHK-STYMD )
               MOVE    SDNTCHK-STYMD       TO  WRK-SZYMD
           END-IF
      *
           IF    ( PTNYUINRRK-NYUINYMD <=  STUSAN-KJNYMD )
           AND   ( PTNYUINRRK-TAIINYMD >=  STUSAN-KJNYMD )
      *
               MOVE    PTNYUINRRK-NYUINYMD TO  WRK-KONKAI-NYUINYMD
      *
               IF    ( PTNYUINRRK-JTIKBN   =   "5" OR "6" )
                   MOVE    02              TO  STUSAN-RC
                   GO  TO  2100-KONKAI-NYUIN-TUSAN-EXT
               END-IF
      *
               MOVE    SDNTCHK-STYMD       TO  WRK-STYMD
               MOVE    STUSAN-KJNYMD       TO  WRK-EDYMD
               PERFORM 800-KIKAN-CALC-SEC
               IF    ( STUSAN-RC       NOT  =   ZERO )
                   GO  TO  2100-KONKAI-NYUIN-TUSAN-EXT
               END-IF
      *
               COMPUTE STUSAN-NISSU01
                   =   STUSAN-NISSU01      +   WRK-NISSU2
               COMPUTE STUSAN-NISSU02
                   =   STUSAN-NISSU02      +   WRK-NISSU2
               COMPUTE STUSAN-NISSU03
                   =   STUSAN-NISSU03      +   WRK-NISSU2
               MOVE    SDNTCHK-STYMD       TO  WRK-STYMD
               MOVE    STUSAN-KJNYMD       TO  WRK-EDYMD
               IF    ( STUSAN-TOKUTEI-SRYCD (1)    =   "999999999" )
                   MOVE    ZERO            TO  WRK-ACCT-NISSU
                                               WRK-ACCT-TOK-NISSU1
                                               WRK-ACCT-TOK-NISSU2
               ELSE
                   PERFORM 2120-NYUINKIKAN-SEL-SEC
               END-IF
               COMPUTE STUSAN-NISSU04
                   =   STUSAN-NISSU04 +    WRK-ACCT-NISSU
               COMPUTE STUSAN-NISSU05
                   =   STUSAN-NISSU05 +    WRK-ACCT-NISSU
               COMPUTE STUSAN-NISSU06
                   =   STUSAN-NISSU06 +    WRK-ACCT-TOK-NISSU1
               COMPUTE STUSAN-NISSU07
                   =   STUSAN-NISSU07 +    WRK-ACCT-TOK-NISSU2
               COMPUTE STUSAN-NISSU08
                   =   STUSAN-NISSU08 +    WRK-ACCT-TOK-NISSU1
               COMPUTE STUSAN-NISSU09
                   =   STUSAN-NISSU09 +    WRK-ACCT-TOK-NISSU2
           ELSE
               PERFORM 2110-TUSAN-SYUKEI-SEC
               IF    ( STUSAN-RC       NOT  =   ZERO )
                   GO  TO  2100-KONKAI-NYUIN-TUSAN-EXT
               END-IF
           END-IF
      *
           PERFORM 2100-RRK-DATA-SET-SEC
      *
           .
       2100-KONKAI-NYUIN-TUSAN-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       2110-NYUINRRK-SEL-SEC               SECTION.
      *
      *    基準日に入院中の歴を取得する
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           IF    ( STUSAN-RRKNUMKBN    =   "1" )
      *
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    STUSAN-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
               MOVE    STUSAN-PTID     TO  PTNYUINRRK-PTID
               MOVE    STUSAN-RRKNUM   TO  PTNYUINRRK-RRKNUM
               MOVE    1               TO  PTNYUINRRK-RRKEDANUM
               MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
               ELSE
                   MOVE    1           TO  FLG-PTNYUINRRK
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           ELSE
      *
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    STUSAN-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
               MOVE    STUSAN-PTID     TO  PTNYUINRRK-PTID
               MOVE    STUSAN-KJNYMD   TO  PTNYUINRRK-NYUINYMD
               MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
               MOVE    "key17"          TO  MCP-PATHNAME
               MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
               ELSE
                   MOVE    1           TO  FLG-PTNYUINRRK
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
               MOVE    "key17"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           IF    ( FLG-PTNYUINRRK  NOT =   ZERO )
               MOVE    03              TO  STUSAN-RC
               GO  TO  2110-NYUINRRK-SEL-EXT
           END-IF
      *
           MOVE    ALL "0"             TO  WRK-SZYMD
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
      *
               IF    ( PTNYUINRRK-TOKUTEI-KBN   =   "2" )
                AND  ( PTNYUINRRK-TOKUTEI-NYUIN =   "24" OR "25" )
                   MOVE    1           TO  FLG-SEISINZOUAKU
                   MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  WRK-SZYMD
               END-IF
           ELSE
                   MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           IF    ( FLG-SEISINZOUAKU    =   1 )
      *
               PERFORM UNTIL ( FLG-PTNYUINRRK   NOT =   ZERO )
                   IF    ( PTNYUINRRK-TOKUTEI-KBN   =   "2" )
                    AND  ( PTNYUINRRK-TOKUTEI-NYUIN =   "24" OR "25" )
                       MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  WRK-SZYMD
                   ELSE
                       MOVE    1       TO  FLG-PTNYUINRRK
                   END-IF
      *
                   MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
                   MOVE    "key4"          TO  MCP-PATHNAME
                   PERFORM 910-DBFETCH-SEC
                   IF    ( MCP-RC          =   ZERO )
                       MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
                   ELSE
                       MOVE    1           TO  FLG-PTNYUINRRK
                   END-IF
      *
               END-PERFORM
           END-IF
      *
      *     DISPLAY "WRK-SZYMD =" WRK-SZYMD
      *
           MOVE    "tbl_ptnyuinrrk" TO  MCP-TABLE
           MOVE    "key4"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2110-NYUINRRK-SEL-EXT.
           EXIT.
      *****************************************************************
      *    同一日入退院チェック処理
      *****************************************************************
       2110-DOJITU-NYUTAI-CHK-SEC          SECTION.
      *
           INITIALIZE                  SDNTCHK-AREA
           MOVE    PTNYUINRRK-REC      TO      LNKNRRK-REC
           MOVE    STUSAN-KJNYMD       TO      SDNTCHK-KJNYMD
           CALL    "ORCSDNTCHK"        USING   SDNTCHK-AREA
                                               LNKNRRK-REC
      *
           .
       2110-DOJITU-NYUTAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    通算日数集計処理
      *****************************************************************
       2110-TUSAN-SYUKEI-SEC               SECTION.
      *
           EVALUATE    PTNYUINRRK-JTIKBN
           WHEN    "5"
               COMPUTE STUSAN-NISSU02
                   =   STUSAN-NISSU02  +   PTNYUINRRK-TAISYONISSU
               COMPUTE STUSAN-NISSU04
                   =   STUSAN-NISSU04  +   PTNYUINRRK-TAISYONISSU
               IF    ( PTNYUINRRK-TAINRELKBN   =   "1" )
                   COMPUTE STUSAN-TAINREL-NISSU
                       =   STUSAN-TAINREL-NISSU
                       +   PTNYUINRRK-TAISYONISSU
                   COMPUTE STUSAN-NISSU08
                       =   STUSAN-NISSU08
                       +   PTNYUINRRK-TOKU-TAISYONISSU
               END-IF
           WHEN    "6"
               COMPUTE STUSAN-NISSU02
                   =   STUSAN-NISSU02  +   PTNYUINRRK-TAISYONISSU
               COMPUTE STUSAN-NISSU03
                   =   STUSAN-NISSU03  +   PTNYUINRRK-TAISYONISSU
               COMPUTE STUSAN-NISSU04
                   =   STUSAN-NISSU04  +   PTNYUINRRK-TAISYONISSU
               COMPUTE STUSAN-NISSU05
                   =   STUSAN-NISSU05  +   PTNYUINRRK-TAISYONISSU
               COMPUTE STUSAN-NISSU08
                   =   STUSAN-NISSU08  +   PTNYUINRRK-TOKU-TAISYONISSU
               IF    ( PTNYUINRRK-NYUINCHUKBN  =   "1" )
                   MOVE   PTNYUINRRK-TAIINYMD
                                       TO  WRK-YMD
                   MOVE   "1"          TO  WRK-ZOGENPTN
                   MOVE   1            TO  WRK-ZOGEN
                   PERFORM 800-CALENDAR-SEC
                   IF    ( WRK-KONKAI-NYUINYMD
                                       =   WRK-KEISANYMD )
                       COMPUTE STUSAN-NISSU01
                           =   STUSAN-NISSU01
                           +   PTNYUINRRK-TAISYONISSU
                   END-IF
               END-IF
           WHEN    OTHER
               MOVE    SDNTCHK-STYMD       TO  WRK-STYMD
               MOVE    PTNYUINRRK-TAIINYMD TO  WRK-EDYMD
               PERFORM 800-KIKAN-CALC-SEC
               IF    ( STUSAN-RC       NOT =   ZERO )
                   GO  TO  2110-TUSAN-SYUKEI-EXT
               END-IF
               COMPUTE STUSAN-NISSU02
                   =   STUSAN-NISSU02  +   WRK-NISSU2
               COMPUTE STUSAN-NISSU03
                   =   STUSAN-NISSU03  +   WRK-NISSU2
               MOVE    SDNTCHK-STYMD       TO  WRK-STYMD
               MOVE    PTNYUINRRK-TAIINYMD TO  WRK-EDYMD
               IF    ( STUSAN-TOKUTEI-SRYCD (1)    =   "999999999" )
                   MOVE    ZERO            TO  WRK-ACCT-NISSU
                                               WRK-ACCT-TOK-NISSU1
                                               WRK-ACCT-TOK-NISSU2
               ELSE
                   PERFORM 2120-NYUINKIKAN-SEL-SEC
               END-IF
               COMPUTE STUSAN-NISSU04
                   =   STUSAN-NISSU04 +    WRK-ACCT-NISSU
               COMPUTE STUSAN-NISSU05
                   =   STUSAN-NISSU05 +    WRK-ACCT-NISSU
               COMPUTE STUSAN-NISSU08
                   =   STUSAN-NISSU08 +    WRK-ACCT-TOK-NISSU1
               COMPUTE STUSAN-NISSU09
                   =   STUSAN-NISSU09 +    WRK-ACCT-TOK-NISSU2
           END-EVALUATE
           .
      *
       2110-TUSAN-SYUKEI-EXT.
           EXIT.
      *****************************************************************
      *    入院期間検索処理
      *****************************************************************
       2120-NYUINKIKAN-SEL-SEC              SECTION.
      *
           MOVE    WRK-STYMD           TO  WRK-STSRYYM
           MOVE    WRK-EDYMD           TO  WRK-EDSRYYM
      *
           MOVE    ZERO                TO  WRK-ACCT-NISSU
                                           WRK-ACCT-TOK-NISSU1
                                           WRK-ACCT-TOK-NISSU2
      *
           IF    ( STUSAN-NOT-SELECT-NACCT =   "1" )
               CONTINUE
           ELSE
               PERFORM 2121-NYUINACCT-SEL-SEC
           END-IF
      *
           .
       2120-NYUINKIKAN-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       2121-NYUINACCT-SEL-SEC              SECTION.
      *
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    WRK-STYMD(1:6)      TO  NACCT-SRYYM
           MOVE    WRK-EDYMD(1:6)      TO  NACCT-CREYMD
           MOVE    "1"                 TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key68"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    NACCT-SRYYM     TO  WRK-STSRYYM
      *
               IF    ( NACCT-ZAISKBKBN     =    "1"    )
                   PERFORM 2121-NYUINACT-SEL-SEC
                   IF    ( STUSAN-RC   NOT =   ZERO )
                       GO  TO  2121-NYUINACCT-SEL-EXT
                   END-IF
      *
                   IF    ( FLG-HIT     NOT =   ZERO )
                       PERFORM 2121-NYUINACT-TUSAN-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key68"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC         NOT  =   ZERO )
                   MOVE    1           TO  FLG-NYUINACCT
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO MCP-TABLE
           MOVE    "key68"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2121-NYUINACCT-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為検索処理
      *****************************************************************
       2121-NYUINACT-SEL-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-HIT
      *
           MOVE    ZERO                TO  FLG-NYUINACT
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           MOVE    "tbl_nyuinact"      TO MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-NYUINACT    NOT =   ZERO )
               MOVE    04              TO  STUSAN-RC
               GO  TO  2121-NYUINACT-SEL-EXT
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1        >   5 )
                OR       ( FLG-HIT NOT =   ZERO )
                OR       ( NSRY-SRYCD (IDX1)   =   SPACE )
      *
               INITIALIZE                      TNS-TENSU-REC
               MOVE    NACCT-HOSPNUM       TO  TNS-HOSPNUM
               MOVE    NSRY-SRYCD(IDX1)    TO  TNS-SRYCD
               MOVE    NACCT-SRYYM         TO  TNS-YUKOSTYMD(1:6)
               MOVE    "01"                TO  TNS-YUKOSTYMD(7:2)
               MOVE    TNS-YUKOSTYMD       TO  TNS-YUKOEDYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU           =   ZERO )
                   IF    ( TNS-DATAKBN     =   1 )
                       MOVE    1           TO  FLG-HIT
                       MOVE    NACCT-SRYYM TO  WRK-SRYYMD(1:6)
                       MOVE    "01"        TO  WRK-SRYYMD(7:2)
                       PERFORM 2121-SENTEICDCHG-CHK-SEC
                       PERFORM 2121-TOKUTEI-CHK-SEC
                   END-IF
               ELSE
                   DISPLAY "SRYCD not found TNS-KEY:" NSRY-SRYCD(IDX1)
               END-IF
      *
           END-PERFORM
      *
           IF    ( FLG-HIT             =   ZERO )
               DISPLAY "datakbn=1 not found NSRY-KEY:" NSRY-KEY
      *        MOVE    05              TO  STUSAN-RC
      *        GO  TO  2121-NYUINACT-SEL-EXT
           END-IF
      *
           .
       2121-NYUINACT-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計通算処理
      *****************************************************************
       2121-NYUINACT-TUSAN-SEC             SECTION.
      *
           IF    ( WRK-STYMD (1 : 6)   =   WRK-STSRYYM )
               MOVE    WRK-STYMD (7 : 2)   TO  WRK-STIDX
           ELSE
               MOVE    1                   TO  WRK-STIDX
           END-IF
      *
           IF    ( WRK-EDYMD (1 : 6)   =   WRK-STSRYYM )
               MOVE    WRK-EDYMD (7 : 2)   TO  WRK-EDIDX
           ELSE
               MOVE    31                  TO  WRK-EDIDX
           END-IF
      *
           MOVE    WRK-EDYMD           TO  WRK-EDSRYYMD-YM
           MOVE    WRK-EDIDX           TO  WRK-EDSRYYMD-DD
      *
           PERFORM VARYING IDX1    FROM    WRK-STIDX   BY  1
                   UNTIL ( IDX1    >       WRK-EDIDX )
               IF    ( NACCT-DAY (IDX1)    >   ZERO )
                   IF    ( FLG-SENTEICDCHG =   ZERO )
                    AND  ( FLG-TOKUTEI     =   ZERO )
      *
                       COMPUTE WRK-ACCT-NISSU
                           =   WRK-ACCT-NISSU +    1
      *
                       MOVE    IDX1        TO  WRK-DAY-9
                       IF    ( STUSAN-KJNYMD (1:6) =   NACCT-SRYYM )
                        AND  ( STUSAN-KJNYMD (7:2) =   WRK-DAY-X   )
                           MOVE    1       TO  STUSAN-KJNYMD-CHK-FLG
                       END-IF
      *
                   END-IF
      *
                   IF    ( FLG-TOKUTEI     =   1 )
                       MOVE    IDX1        TO  WRK-DAY-9
                       IF    ( STUSAN-KJNYMD (1:6) =   NACCT-SRYYM )
                        AND  ( STUSAN-KJNYMD (7:2) =   WRK-DAY-X   )
                           CONTINUE
                       ELSE
                           COMPUTE WRK-ACCT-TOK-NISSU1
                               =   WRK-ACCT-TOK-NISSU1 +   1
                       END-IF
      *
                       IF    ( FLG-TOKUTEI-SITEI   =   1 )
                           MOVE    IDX1    TO  WRK-DAY-9
                           IF    ( STUSAN-KJNYMD (1:6) =   NACCT-SRYYM )
                            AND  ( STUSAN-KJNYMD (7:2) =   WRK-DAY-X   )
                               CONTINUE
                           ELSE
                               IF    ( FLG-SEISINZOUAKU    =   ZERO )
                                   COMPUTE WRK-ACCT-TOK-NISSU2
                                       =   WRK-ACCT-TOK-NISSU2 +   1
                               ELSE
                                   MOVE    NACCT-SRYYM
                                                   TO  WRK-ACCTYMD-YM
                                   MOVE    IDX1    TO  WRK-ACCTYMD-DD
                                   IF    ( WRK-SZYMD    <=  WRK-ACCTYMD)
                                    AND  ( WRK-EDSRYYMD >=  WRK-ACCTYMD)
                                       COMPUTE WRK-ACCT-TOK-NISSU2
                                           =   WRK-ACCT-TOK-NISSU2 +   1
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
      *
               END-IF
           END-PERFORM
      *
           .
       2121-NYUINACT-TUSAN-EXT.
           EXIT.
      *****************************************************************
      *    選定療養費置換可能チェック処理
      *****************************************************************
       2121-SENTEICDCHG-CHK-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-SENTEICDCHG
      *
           IF    ( WRK-SRYYMD      >=  CONST-H180701 )
            AND  ( TNS-SRYCD       =   "190080610"
                                    OR "190080710"
                                    OR "190081310"
                                    OR "190081410"
                                    OR "190098210"
                                    OR "190098310"  )
      *
               MOVE    1           TO  FLG-SENTEICDCHG
      *
           ELSE
               INITIALIZE  SENTEICDCHG-REC
               MOVE    TNS-SRYCD       TO  SENCHG-IPNSRYCD
               PERFORM 900-SENTEICDCHG-KEY2-SEL-SEC
           END-IF
      *
           .
       2121-SENTEICDCHG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料チェック処理
      *****************************************************************
       2121-TOKUTEI-CHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-TOKUTEI
                                           FLG-TOKUTEI-SITEI
      *
           IF  ( TNS-NYUTENTTLKBN      =   920 )
               MOVE    1               TO  FLG-TOKUTEI
      *
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL ( IDX3    >   CONST-TOKUTEI-SRYCD-MAX )
      *
                   IF    ( STUSAN-TOKUTEI-SRYCD (IDX3)
                                   NOT =   SPACE )
                       IF    ( STUSAN-TOKUTEI-SRYCD (IDX3)
                                       =   TNS-SRYCD )
                           MOVE    1   TO  FLG-TOKUTEI-SITEI
                           MOVE    CONST-TOKUTEI-SRYCD-MAX
                                       TO  IDX3
                       END-IF
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       2121-TOKUTEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    過去入院分通算処理
      *****************************************************************
       2100-KAKO-NYUIN-TUSAN-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    "tbl_ptnyuinrrk"    TO MCP-TABLE
           MOVE    "key38"             TO  MCP-PATHNAME
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           PERFORM UNTIL   ( FLG-PTNYUINRRK    NOT =   ZERO )
                    OR     ( PTNYUINRRK-SHOKAIKBN  =   "1"  )
      *
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
      *
               PERFORM 2110-DOJITU-NYUTAI-CHK-SEC
               IF    ( SDNTCHK-STYMD   =   ZERO )
                   INITIALIZE              WRK-NISSU-G
               ELSE
                   PERFORM 2110-TUSAN-SYUKEI-SEC
                   IF    ( STUSAN-RC   NOT =   ZERO )
                       GO  TO  2100-KAKO-NYUIN-TUSAN-EXT
                   END-IF
               END-IF
      *
      *        入院履歴データ設定処理
               PERFORM 2100-RRK-DATA-SET-SEC
      *
               MOVE    "tbl_ptnyuinrrk"    TO MCP-TABLE
               MOVE    "key38"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    1           TO  FLG-PTNYUINRRK
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO MCP-TABLE
           MOVE    "key38"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2100-KAKO-NYUIN-TUSAN-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴データ設定処理
      *****************************************************************
       2100-RRK-DATA-SET-SEC           SECTION.
      *
           IF    ( STUSAN-RRK-CNT     >=  50 )
               MOVE    1               TO  STUSAN-RRK-FLG-OVER
               GO  TO  2100-RRK-DATA-SET-EXT
           END-IF
      *
           IF    ( PTNYUINRRK-NYUINCHUKBN  =   "1" )
               IF    ( PTNYUINRRK-JTIKBN   =   "6" )
                AND  ( STUSAN-RRK-CNT      >   ZERO )
                   MOVE    STUSAN-RRK-CNT  TO  IDX2
                   MOVE    PTNYUINRRK-NYUINYMD
                                           TO  STUSAN-RRK-NYUINYMD(IDX2)
                   COMPUTE STUSAN-RRK-NISSU   (IDX2)
                       =   STUSAN-RRK-NISSU   (IDX2)
                       +   PTNYUINRRK-TAISYONISSU
               END-IF
           ELSE
      *
               COMPUTE STUSAN-RRK-CNT
                   =   STUSAN-RRK-CNT      +   1
               MOVE    STUSAN-RRK-CNT      TO  IDX2
      *
               MOVE    PTNYUINRRK-JTIKBN   TO  STUSAN-RRK-JTIKBN  (IDX2)
               MOVE    PTNYUINRRK-TAINRELKBN
                                           TO  STUSAN-RRK-TAINRELKBN
                                                                  (IDX2)
               MOVE    PTNYUINRRK-NYUINYMD TO  STUSAN-RRK-NYUINYMD(IDX2)
               MOVE    PTNYUINRRK-TAIINYMD TO  STUSAN-RRK-TAIINYMD(IDX2)
               MOVE    SDNTCHK-DJKBN       TO  STUSAN-RRK-DJKBN   (IDX2)
               IF    ( PTNYUINRRK-JTIKBN   =   "5" OR  "6" )
                   MOVE    PTNYUINRRK-TAISYONISSU
                                           TO  STUSAN-RRK-NISSU   (IDX2)
               ELSE
                   MOVE    WRK-ACCT-NISSU  TO  STUSAN-RRK-NISSU   (IDX2)
               END-IF
           END-IF
      *
           .
       2100-RRK-DATA-SET-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           CLOSE   CTENSU-FILE
                   CSENTEICDCHG-FILE
      *
           CALL    "CBL_DELETE_FILE"   USING
                                       ASGN-CTENSU
      *
           CALL    "CBL_DELETE_FILE"   USING
                                       ASGN-CSENTEICDCHG
      *
           IF    ( FLG-SET-DIR     NOT =  ZERO )
               INITIALIZE              STEMPDIR-AREA
               MOVE    "UNSET"         TO    STEMPDIR-MODE
               CALL    "ORCSTEMPDIR"   USING STEMPDIR-AREA
               DISPLAY "UNSET DIR ORCSTUSAN"
           END-IF
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       800-HIZUKE-CHK-SEC                  SECTION.
      *
           MOVE    SPACE                   TO  WRK-HENYMDG
                                               WRK-SYMD
      *
           IF    ( WRK-YMD                 =   SPACE )
               GO  TO  800-HIZUKE-CHK-EXT
           END-IF
      *
           INITIALIZE                          ORCSGDAYAREA
           MOVE    WRK-YMD                 TO  SGDAY-INDAY
           CALL    "ORCSGDAY"                  USING
                                               ORCSGDAYAREA
           IF  ( SGDAY-RC                  =   ZERO )
               MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY          TO  WRK-SYMD
           ELSE
               MOVE    06                  TO  STUSAN-RC
           END-IF
           .
       800-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間算出処理
      *****************************************************************
       800-KIKAN-CALC-SEC                  SECTION.
      *
           INITIALIZE                      WRK-NISSU1
                                           WRK-NISSU2
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    WRK-STYMD           TO  LNK-DAY5-START
           MOVE    WRK-EDYMD           TO  LNK-DAY5-END
           CALL    "ORCSDAY"               USING
                                           STS-AREA-DAY
                                           LNK-DAY5-AREA
           IF    ( STS-DAY-RC1     NOT =   ZERO )
               MOVE    07              TO  STUSAN-RC
               GO  TO                      800-KIKAN-CALC-EXT
           END-IF
      *
           MOVE    LNK-DAY5-NISSU1     TO  WRK-NISSU1
           MOVE    LNK-DAY5-NISSU2     TO  WRK-NISSU2
      *
           .
       800-KIKAN-CALC-EXT.
           EXIT.
      *****************************************************************
      *    デバッグDISPLAY処理
      *****************************************************************
       800-DISPLAY-SEC                     SECTION.
      *
           DISPLAY "STUSAN-RC      = "  STUSAN-RC
           DISPLAY "STUSAN-PTID    = "  STUSAN-PTID
           DISPLAY "STUSAN-KJNYMD  = "  STUSAN-KJNYMD
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   CONST-TOKUTEI-SRYCD-MAX )
               IF    ( STUSAN-TOKUTEI-SRYCD (IDX1)
                               NOT =   SPACE )
               DISPLAY "STUSAN-TOKUTEI-SRYCD (" IDX1 ")= "
                        STUSAN-TOKUTEI-SRYCD (IDX1)
               END-IF
           END-PERFORM
           DISPLAY "-"
      *
           DISPLAY "STUSAN-NISSU01 = " STUSAN-NISSU01
           DISPLAY "STUSAN-NISSU02 = " STUSAN-NISSU02
           DISPLAY "STUSAN-NISSU03 = " STUSAN-NISSU03
           DISPLAY "STUSAN-NISSU04 = " STUSAN-NISSU04
           DISPLAY "STUSAN-NISSU05 = " STUSAN-NISSU05
           DISPLAY "STUSAN-NISSU06 = " STUSAN-NISSU06
           DISPLAY "STUSAN-NISSU07 = " STUSAN-NISSU07
           DISPLAY "STUSAN-NISSU08 = " STUSAN-NISSU08
           DISPLAY "STUSAN-NISSU09 = " STUSAN-NISSU09
           DISPLAY "STUSAN-TAINREL-NISSU = " STUSAN-TAINREL-NISSU
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   STUSAN-RRK-CNT )
               DISPLAY "STUSAN-RRK-JTIKBN (" IDX1 ")= "
                        STUSAN-RRK-JTIKBN (IDX1)
               DISPLAY "STUSAN-RRK-TAINRELKBN (" IDX1 ")= "
                        STUSAN-RRK-TAINRELKBN (IDX1)
               DISPLAY "STUSAN-RRK-NYUINYMD (" IDX1 ")= "
                        STUSAN-RRK-NYUINYMD (IDX1)
               DISPLAY "STUSAN-RRK-TAIINYMD (" IDX1 ")= "
                        STUSAN-RRK-TAIINYMD (IDX1)
               DISPLAY "STUSAN-RRK-NISSU (" IDX1 ")= "
                        STUSAN-RRK-NISSU (IDX1)
               DISPLAY "STUSAN-RRK-DJKBN (" IDX1 ")= "
                        STUSAN-RRK-DJKBN (IDX1)
           END-PERFORM
      *
           .
       800-DISPLAY-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                    SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"                 USING
                                           STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    UID取得処理
      *****************************************************************
       800-ORCUIDSET-SEC               SECTION.
      *
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       UIDIO-ST
                                       UIDIO-AREA
      *
           .
       800-ORCUIDSET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY-SEL-SEC               SECTION.
      *
           PERFORM 9001-CTENSU-START-SEC
           IF    ( FLG-TENSU           =   ZERO )
               CONTINUE
           ELSE
               MOVE    ZERO                TO  FLG-TENSU
      *
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
                                               CTNS-TENSU-REC
                   WRITE   CTNS-TENSU-REC
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1               TO  FLG-TENSU
               END-IF
      *
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ一時ファイルSTART処理
      *****************************************************************
       9001-CTENSU-START-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           MOVE    TNS-TENSU-REC       TO  CTNS-TENSU-REC
           MOVE    "00000000"          TO  CTNS-YUKOSTYMD
                                           CTNS-YUKOEDYMD
      *
           START   CTENSU-FILE
               KEY IS  >=   CTNS-KEY
           INVALID
               MOVE    1               TO  FLG-TENSU
           NOT INVALID
               PERFORM UNTIL ( FLG-TENSU   NOT =   ZERO )
                        OR   ( FLG-LOOPEND NOT =   ZERO )
      *
                   READ CTENSU-FILE    NEXT
                   AT END
                       MOVE    1       TO  FLG-TENSU
                   NOT AT END
                       IF    ( CTNS-HOSPNUM    =   TNS-HOSPNUM )
                        AND  ( CTNS-SRYCD      =   TNS-SRYCD )
                           IF    ( CTNS-YUKOSTYMD <= TNS-YUKOSTYMD )
                            AND  ( CTNS-YUKOEDYMD >= TNS-YUKOSTYMD )
                               MOVE    1   TO  FLG-LOOPEND
                               MOVE    CTNS-TENSU-REC
                                           TO  TNS-TENSU-REC
                           END-IF
                       ELSE
                           MOVE    1       TO  FLG-TENSU
                       END-IF
                   END-READ
                   IF    ( STS-CTENSU  >   10 )
                       MOVE    1           TO  FLG-TENSU
                   END-IF
               END-PERFORM
           END-START
      *
           .
       9001-CTENSU-START-EXT.
           EXIT.
      *****************************************************************
      *    選定コード置換検索処理
      *****************************************************************
       900-SENTEICDCHG-KEY2-SEL-SEC        SECTION.
      *
           PERFORM 9001-CSENTEICDCHG-START-SEC
           IF    ( FLG-SENTEICDCHG         =   ZERO OR 2 )
               CONTINUE
           ELSE
               MOVE    ZERO                TO  FLG-SENTEICDCHG
               MOVE    SENTEICDCHG-REC     TO  MCPDATA-REC
               MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC      =   ZERO  )
                   MOVE    MCPDATA-REC     TO  SENTEICDCHG-REC
                                               CSENTEICDCHG-REC
                   WRITE   CSENTEICDCHG-REC
               ELSE
                   MOVE    1               TO  FLG-SENTEICDCHG
                   MOVE    SENTEICDCHG-REC TO  CSENTEICDCHG-REC
                   MOVE    CONST-NOT-FOUND TO  CSENCHG-SENTEISRYCD
                   WRITE   CSENTEICDCHG-REC
               END-IF
      *
               MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       900-SENTEICDCHG-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    選定コード置換一時ファイルSTART処理
      *****************************************************************
       9001-CSENTEICDCHG-START-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-SENTEICDCHG
      *
           MOVE    SENTEICDCHG-REC     TO  CSENTEICDCHG-REC
           MOVE    ZERO                TO  CSENCHG-SENTEISRYCD
      *
           START   CSENTEICDCHG-FILE
               KEY IS  >=   CSENCHG-KEY
           INVALID
               MOVE    1               TO  FLG-SENTEICDCHG
           NOT INVALID
      *
               READ CSENTEICDCHG-FILE  NEXT
               AT END
                   MOVE    1           TO  FLG-SENTEICDCHG
               NOT AT END
                   IF    ( CSENCHG-IPNSRYCD
                                   =   SENCHG-IPNSRYCD )
                       IF    ( CSENCHG-SENTEISRYCD
                                   =   CONST-NOT-FOUND )
                           MOVE    2   TO  FLG-SENTEICDCHG
                       ELSE
                           MOVE    CSENTEICDCHG-REC
                                       TO  SENTEICDCHG-REC
                       END-IF
                   ELSE
                       MOVE    1   TO  FLG-SENTEICDCHG
                   END-IF
               END-READ
               IF    ( STS-CSENTEICDCHG  >   10 )
                   MOVE    1       TO  FLG-SENTEICDCHG
               END-IF
           END-START
      *
           .
       9001-CSENTEICDCHG-START-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                    SECTION.
      *
           MOVE    "DBSELECT"      TO      MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC  =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                     SECTION.
      *
           MOVE    "DBFETCH"       TO      MCP-FUNC
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
       910-DBCLOSECURSOR-SEC                     SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO      MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
