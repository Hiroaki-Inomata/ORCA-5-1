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
       PROGRAM-ID.                 ORCMUP0042.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＤＢ作成処理
      *  コンポーネント名  : チェックマスタ更新処理
      *                    : （新レイアウト）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/03/14    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.03.00     NACL-太田    08/05/28  エラー処理強化対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    入力：チェックマスタファイル
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
      *    入力：チェックマスタ更新データ
       FD  IN-FILE.
       01  CHKWK-REC.
           COPY    "CPCHKWK1.INC".
      *
      *    出力：エラーファイル
       FD  ERR-FILE.
       01  ERR-CHKWK-REC.
           COPY    "CPCHKWK1.INC"       REPLACING
                                       //CHKWK-// BY //ERR-CHKWK-//.
      *
      *    出力：プルーフファイル
       FD  PLF-FILE.
       01  PLF-REC                 PIC X(1000).
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
           03  STS-PARA                PIC X(02).
           03  STS-IN                  PIC X(02).
           03  STS-ERR                 PIC X(02).
           03  STS-PLF                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PARA                PIC 9(01).
           03  FLG-IN                  PIC 9(01).
           03  FLG-PLF                 PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PARA                PIC 9(06).
           03  CNT-IN                  PIC 9(06).
           03  CNT-INS                 PIC 9(06).
           03  CNT-UPD                 PIC 9(06).
           03  CNT-DEL                 PIC 9(06).
           03  CNT-HEN                 PIC 9(06).
           03  CNT-ERR                 PIC 9(06).
           03  CNT-PLF                 PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
      *
      *
       01  SYS-DATE.
           03  SYS-YY                  PIC 9(02).
           03  SYS-MM                  PIC 9(02).
           03  SYS-DD                  PIC 9(02).
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
      *
           03  WRK-ERRCD               PIC 9(01). 
           03  WRK-UPD-ERRCD           PIC 9(01). 
      *
           03  WRK-IN                  PIC X(01). 
           03  WRK-HOMEDIR             PIC X(128).
      *
      *    出力ファイル領域
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(26) VALUE
                                       "【PGID:ORCMUP0042.CBL】".
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
           03  WRK-SYORI-UPDKBN        PIC X(01).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-CHKKBN              PIC X(01).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-SRYCD               PIC X(09).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-YUKOSTYMD           PIC X(08).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-YUKOEDYMD           PIC X(08).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-YUKOEDYMD2          PIC X(08).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-CDKBN               PIC X(01).
           03  FILLER                  PIC X(01)   VALUE   ",".
      *    適用コード
           03  WRK-CD                  PIC X(09).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-NYUGAIKBN           PIC X(01).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-NYUGAIBNDKBN        PIC X(01).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-SYORIKBN            PIC X(01).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-CHKERR              PIC X(01).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-DATAKBN             PIC X(01).
           03  FILLER                  PIC X(01)   VALUE   ",".
      **** 03  WRK-NAME                PIC X(100).
      *
       01  CONST-AREA.
           03  CONST-HOMEDIR           PIC X(28)  VALUE
               "/var/lib/jma-receipt/master/".
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
           COPY    "ENUM-VALUE".
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *    チェックマスタ−
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *****COPY    "CPCHKOLD.INC".
      *
      *    ジョブ管理情報
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
      *01  MCPDATA-REC                 PIC X(5000).
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
           MOVE    ZERO            TO  IDX-AREA
           INITIALIZE                  WRK-AREA
                                       SPA-AREA
      *
           MOVE    LNK-HOSPNUM     TO  SPA-HOSPNUM
      *
      *    DB OPEN
      *    PERFORM 100-DBOPEN-SEC
      *    IF      MCP-RC      NOT =   ZERO
      *        DISPLAY "DBOPEN ERR"
      *    END-IF.
      *
      *    パラメータ情報取得
      *****PERFORM 101-LNK-GET-SEC
      *    ファイル情報
           PERFORM 101-ASSIN-GET-SEC
      *
      *    入力　初期処理
           OPEN    INPUT           IN-FILE
           IF  STS-IN          NOT =   ZERO
               MOVE    1           TO  FLG-IN
               DISPLAY "*(ORCMUP0042)* IN-FILE OPN STS["
                                       STS-IN "]"
           END-IF
      *
      *    出力ファイル　初期処理
           PERFORM 2700-OUTLST-INIT-SEC
      *
      *    チェックマスタ変更ファイル読込み処理
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
      *
      *    DISPLAY "LNK-UPFILE:" LNK-UPFILE
      *    DISPLAY "ASS-IN:" ASS-IN
      *    DISPLAY "ASS-ERR:" ASS-ERR
      *    DISPLAY "ASS-PLF:" ASS-PLF
      *
           .
       101-ASSIN-GET-EXT.
           EXIT.
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    ZERO                TO  WRK-ERRCD
      *
      *    適用コードがない時、期間変更
           IF      CHKWK-CD            =   SPACE
               PERFORM 2040-KIKAN-SYORI-SEC
           ELSE
      *
           EVALUATE    CHKWK-SYORI-UPDKBN
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
           END-IF
      *
      *    点数マスタ変更ファイル読込み処理
           PERFORM 900-IN-KENSAKU-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加  処理
      *****************************************************************
       2010-INSERT-SYORI-SEC               SECTION.
      *
      *    チェックマスタ存在チェック
           PERFORM 2010-CHK-KEYREAD-SEC
      *
      *    単純追加
           IF      CHKWK-YUKOEDYMD2    =   SPACE
      *        追加エラー
               IF      FLG-CHK             =   ZERO
                   MOVE    5               TO  WRK-ERRCD
                   PERFORM 2040-ERR-SYORI-SEC
               ELSE
      *            ＤＢ追加
                   PERFORM 20101-CHK-INS-SEC
                   IF      WRK-ERRCD       =   ZERO
                       ADD     1               TO  CNT-INS
                   END-IF
               END-IF
           ELSE
      *
      *    追加更新
      *        期間更新
               IF      FLG-CHK             =   ZERO
                   PERFORM 20102-CHK-DELUPDS-SEC
               END-IF
      *        ＤＢ追加
               PERFORM 20101-CHK-INS-SEC
               IF      WRK-ERRCD       =   ZERO
                   ADD     1               TO  CNT-INS
               END-IF
           END-IF
      *
           .
       2010-INSERT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェックマスタ存在チェック処理
      *****************************************************************
       2010-CHK-KEYREAD-SEC            SECTION.
      *
           INITIALIZE                  CHK-REC
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    CHKWK-CHKKBN        TO  CHK-CHKKBN
           MOVE    CHKWK-SRYCD         TO  CHK-SRYCD
           MOVE    CHKWK-GRPNUM        TO  CHK-GRPNUM
           MOVE    CHKWK-YUKOSTYMD     TO  CHK-YUKOSTYMD
                                           CHK-YUKOEDYMD
           MOVE    CHKWK-CDKBN         TO  CHK-CDKBN
           MOVE    CHKWK-CD            TO  CHK-CD
      *
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk_master"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_chk_master"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-CHK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-CHK
           END-IF
      *
           MOVE    "tbl_chk_master"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2010-CHK-KEYREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ削除更新  処理
      *****************************************************************
       20102-CHK-DELUPDS-SEC           SECTION.
      *
      *    有効終了年月日２で終了する
           IF     (CHK-YUKOSTYMD       <=  CHKWK-YUKOEDYMD2 ) AND
                  (CHK-YUKOEDYMD       >=  CHKWK-YUKOEDYMD2 )
      *        更新
               MOVE    CHK-KEY             TO  CHK-UPDKEY
               MOVE    CHKWK-YUKOEDYMD2    TO  CHK-YUKOEDYMD
               PERFORM 20102-CHK-UPD-SEC
           END-IF
      *
           .
       20102-CHK-DELUPDS-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ追加  処理
      *****************************************************************
       20101-CHK-INS-SEC           SECTION.
      *
      *    チェックマスタ存在チェック
           PERFORM 2010-CHK-KEYREAD-SEC
      *
           IF      FLG-CHK             =   ZERO
      *        登録済
               MOVE    5               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           ELSE
      *        登録
               PERFORM 201011-CHKINS-SYORI-SEC
           END-IF
           .
       20101-CHK-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ登録  処理
      *****************************************************************
       201011-CHKINS-SYORI-SEC           SECTION.
      *
           INITIALIZE                  CHK-REC
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    CHKWK-CHKKBN        TO  CHK-CHKKBN
           MOVE    CHKWK-SRYCD         TO  CHK-SRYCD
           MOVE    CHKWK-GRPNUM        TO  CHK-GRPNUM
           MOVE    CHKWK-YUKOSTYMD     TO  CHK-YUKOSTYMD
           MOVE    CHKWK-YUKOEDYMD     TO  CHK-YUKOEDYMD
           MOVE    CHKWK-CDKBN         TO  CHK-CDKBN
           MOVE    CHKWK-CD            TO  CHK-CD
      *
           MOVE    CHKWK-NYUGAIKBN     TO  CHK-NYUGAIKBN
           MOVE    CHKWK-NYUGAIBNDKBN  TO  CHK-NYUGAIBNDKBN
           MOVE    CHKWK-SYORIKBN      TO  CHK-SYORIKBN
           MOVE    CHKWK-CHKERR        TO  CHK-CHKERR
           MOVE    CHKWK-DATAKBN       TO  CHK-DATAKBN
      *    連番号（画面表示）
           MOVE    ZERO                TO  CHK-RENNUM
      *
           MOVE    LNK-MNTYMD          TO  CHK-CREYMD
                                           CHK-UPYMD
           MOVE    LNK-MNTHMS          TO  CHK-UPHMS
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_chk_master"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    1               TO  LNK-RC
               MOVE    1               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           .
       201011-CHKINS-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ更新  処理
      *****************************************************************
       20102-CHK-UPD-SEC           SECTION.
      *
           MOVE    LNK-MNTYMD          TO  CHK-UPYMD
           MOVE    LNK-MNTHMS          TO  CHK-UPHMS
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_chk_master"    TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    2               TO  LNK-RC
               MOVE    2               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
      *
           .
       20102-CHK-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新  処理
      *****************************************************************
       2020-UPDATE-SYORI-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *
      *    チェックマスタ存在チェック
           PERFORM 2010-CHK-KEYREAD-SEC
      *
      *    対象のデータをすべて削除する
           IF      FLG-CHK             =   ZERO
      *        ＤＢ削除
               PERFORM 20101-CHK-DEL-SEC
               MOVE    WRK-ERRCD           TO  WRK-UPD-ERRCD
           ELSE
      *        更新エラー
               MOVE    3               TO  WRK-UPD-ERRCD
           END-IF
           MOVE    WRK-UPD-ERRCD       TO  WRK-ERRCD
      *
           IF      WRK-ERRCD           =   ZERO
      *        ＤＢ追加
               PERFORM 20101-CHK-INS-SEC
               IF      WRK-ERRCD       =   ZERO
                   ADD     1               TO  CNT-UPD
               END-IF
           ELSE
      *        更新エラー
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
       2030-DELETE-SYORI-SEC           SECTION.
      *
      *    チェックマスタ存在チェック
           PERFORM 2010-CHK-KEYREAD-SEC
      *
           IF      FLG-CHK              =   ZERO
      *        ＤＢ削除
               PERFORM 20101-CHK-DEL-SEC
           ELSE
      *        削除エラー
               MOVE    3               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
      *
           .
       2030-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ削除  処理
      *****************************************************************
       20101-CHK-DEL-SEC           SECTION.
      *
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_chk_master"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    3               TO  LNK-RC
               MOVE    4               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           ELSE
               ADD     1               TO  CNT-DEL
           END-IF
      *
           .
       20101-CHK-DEL-EXT.
           EXIT.
      *****************************************************************
      *    ユーザ登録分期間変更処理
      *****************************************************************
       2040-KIKAN-SYORI-SEC            SECTION.
      *
           INITIALIZE                  CHK-REC
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    CHKWK-CHKKBN        TO  CHK-CHKKBN
           MOVE    CHKWK-SRYCD         TO  CHK-SRYCD
           MOVE    CHKWK-GRPNUM        TO  CHK-GRPNUM
           MOVE    CHKWK-YUKOSTYMD     TO  CHK-YUKOSTYMD
                                           CHK-YUKOEDYMD
      *
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk_master"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_chk_master"    TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-CHK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-CHK
           END-IF
           PERFORM UNTIL    FLG-CHK        =   1
      *
               IF      (CHKWK-YUKOSTYMD    =   CHK-YUKOSTYMD )
                   AND (CHKWK-YUKOEDYMD    =   CHK-YUKOEDYMD )
                   CONTINUE
               ELSE
      *        更新
                   MOVE    CHK-KEY             TO  CHK-UPDKEY
                   MOVE    CHKWK-YUKOEDYMD2    TO  CHK-YUKOEDYMD
                   PERFORM 20102-CHK-UPD-SEC
                   IF      WRK-ERRCD           =   SPACE
                       ADD     1               TO  CNT-HEN
                   END-IF
               END-IF
               MOVE    "tbl_chk_master"    TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-CHK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_chk_master"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2040-KIKAN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー処理
      *****************************************************************
       2040-ERR-SYORI-SEC            SECTION.
      *
      *    エラー出力
           MOVE    CHKWK-REC    TO  ERR-CHKWK-REC
           WRITE                          ERR-CHKWK-REC
           ADD     1                  TO  CNT-ERR
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
       20401-PLF-OUT-SEC            SECTION.
      *
           MOVE    CHKWK-SYORI-UPDKBN  TO  WRK-SYORI-UPDKBN
           MOVE    CHKWK-CHKKBN        TO  WRK-CHKKBN
           MOVE    CHKWK-SRYCD         TO  WRK-SRYCD
           MOVE    CHKWK-YUKOSTYMD     TO  WRK-YUKOSTYMD
           MOVE    CHKWK-YUKOEDYMD     TO  WRK-YUKOEDYMD
           MOVE    CHKWK-YUKOEDYMD2    TO  WRK-YUKOEDYMD2
           MOVE    CHKWK-CDKBN         TO  WRK-CDKBN
           MOVE    CHKWK-CD            TO  WRK-CD
           MOVE    CHKWK-NYUGAIKBN     TO  WRK-NYUGAIKBN
           MOVE    CHKWK-NYUGAIBNDKBN  TO  WRK-NYUGAIBNDKBN
           MOVE    CHKWK-SYORIKBN      TO  WRK-SYORIKBN
           MOVE    CHKWK-CHKERR        TO  WRK-CHKERR
           MOVE    CHKWK-DATAKBN       TO  WRK-DATAKBN
           EVALUATE    WRK-ERRCD
               WHEN    1
                   MOVE    "追加エラー"       TO  WRK-MSG-01
               WHEN    2
                   MOVE    "更新エラー"       TO  WRK-MSG-01
               WHEN   3
                   MOVE    "削除対象なし"     TO  WRK-MSG-01
               WHEN   4
                   MOVE    "削除エラー"
                                               TO  WRK-MSG-01
               WHEN   5
                   MOVE    "チェックマスタ登録済み"
                                              TO  WRK-MSG-01
           END-EVALUATE
      *
           MOVE    WRK-MSG-AREA       TO  PLF-REC
           WRITE                          PLF-REC
           ADD     1                  TO  CNT-PLF
      *
           .
       20401-PLF-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力ファイル　初期処理
      *****************************************************************
       2700-OUTLST-INIT-SEC            SECTION.
      *
           OPEN    OUTPUT              ERR-FILE
           IF  STS-ERR             NOT =   ZERO
               DISPLAY "*(ORCMUP0042)* OUTFILE OPN STS[" STS-ERR "]"
               GO      TO              2700-OUTLST-INIT-EXT
           END-IF
      *
      *    エラーレコード出力ファイル　オープン
           OPEN    OUTPUT              PLF-FILE
           IF  STS-PLF             NOT =   ZERO
               DISPLAY "*(ORCMUP0042)* PLFFILE OPN STS[" STS-PLF "]"
               GO      TO              2700-OUTLST-INIT-EXT
           END-IF
           ACCEPT  SYS-DATE            FROM    DATE
           COMPUTE WRK-SYY             =   SYS-YY    +   2000
           MOVE    WRK-SYY             TO  MES-TITLE1-YY
           MOVE    SYS-MM              TO  MES-TITLE1-MM
           MOVE    SYS-DD              TO  MES-TITLE1-DD
           MOVE    SPA-HOSPNUM         TO  MES-TITLE1-HOSPNUM
           MOVE    MES-TITLE1          TO  PLF-REC
           WRITE   PLF-REC
      *
      *
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
           DISPLAY "*(ORCMUP0042)* INPUT  /I   CNT[" CNT-IN  "]"
           DISPLAY "*(ORCMUP0042)*  INSERT     CNT[" CNT-INS "]"
           DISPLAY "*(ORCMUP0042)*  UPDATE     CNT[" CNT-UPD "]"
           DISPLAY "*(ORCMUP0042)*  DELETE     CNT[" CNT-DEL "]"
           DISPLAY "*(ORCMUP0042)*  UPDATE YMD CNT[" CNT-HEN "]"
           DISPLAY "*(ORCMUP0042)* ERR    /O   CNT[" CNT-ERR "]"
           DISPLAY "*(ORCMUP0042)* PLF    /O   CNT[" CNT-PLF "]"
           DISPLAY "*(ORCMUP0042)* RC             [" LNK-RC "]"
      **** ACCEPT  WRK-IN
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *     チェックマスタ変更ファイル読込み処理
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
          .
       900-IN-KENSAKU-EXT.
          EXIT.
      *
      *****************************************************************
      *    チェックマスター読込
      *****************************************************************
       910-CHK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  CHK-REC
               MOVE    ZERO                TO  FLG-CHK
           ELSE
               INITIALIZE                  CHK-REC
               MOVE    1                   TO  FLG-CHK
           END-IF
      *
           .
       910-CHK-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
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
