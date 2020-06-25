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
       PROGRAM-ID.                 ORCMUP0052MIGV51.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＤＢ作成処理
      *  コンポーネント名  : 一般老人マスタ更新処理
      *                    : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02.11.05    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-小豆沢  06/05/22  MONFUNC対応
      * 01.00.02     NACL-伊藤    07/05/08  重複チェック見直し
      * 04.03.00     NACL-太田    08/05/28  エラー処理強化対応
      * 05.01.00     NACL-伊藤    17/08/24  マスター分離対応
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
      *    老人一般コード変換
       01  SRYCDCHGWK-REC.
           COPY    "CPSRYCDCHGWK.INC".
      *
      *    出力：エラーファイル
       FD  ERR-FILE.
       01  ERR-SRYCDCHGWK-REC.
           COPY    "CPSRYCDCHGWK.INC"   REPLACING
                                       //CHGWK-// BY //ERR-CHGWK-//.
      *
      *    出力：プルーフファイル
       FD  PLF-FILE.
       01  PLF-REC                 PIC X(1000).
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
           03  FLG-SRYCDCHG            PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PARA                PIC 9(06).
           03  CNT-IN                  PIC 9(06).
           03  CNT-INS                 PIC 9(06).
           03  CNT-UPD                 PIC 9(06).
           03  CNT-DEL                 PIC 9(06).
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
           03  WRK-ERRCD               PIC 9(02). 
           03  WRK-UPD-ERRCD           PIC 9(02). 
      *
           03  WRK-IN                  PIC X(01). 
           03  WRK-HOMEDIR             PIC X(128).
      *
      *    出力ファイル領域
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(31) VALUE
                                       "【PGID:ORCMUP0052MIGV51.CBL】".
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
           03  WRK-CHKKBN              PIC X(01).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-IPNSRYCD            PIC X(09).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-RJNSRYCD            PIC X(09).
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
      *    老人・一般コード振替
       01  SRYCDCHG-REC.
           COPY    "CPSRYCDCHG.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
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
               DISPLAY "*(ORCMUP0052MIGV51)* IN-FILE OPN STS["
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
           EVALUATE    CHGWK-SYORIKBN
      *        削除
               WHEN    "D"
                   PERFORM 2030-DELETE-SYORI-SEC
      *        追加・変更
               WHEN    OTHER
                   PERFORM 2010-INSERT-SYORI-SEC
           END-EVALUATE
      *
      *    一般・老人マスタ変更ファイル読込み処理
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
           INITIALIZE                  SRYCDCHG-REC
      *****MOVE    CHGWK-KEY           TO  CHG-KEY
grpsys     MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
           MOVE    CHGWK-IPNSRYCD      TO  CHG-IPNSRYCD
           MOVE    CHGWK-RJNSRYCD      TO  CHG-RJNSRYCD
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_srycdchg_org"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srycdchg_org"  TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM                     910-SRYCDCHG-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYCDCHG
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_srycdchg_org"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
      *    あればそのまま
           IF      FLG-SRYCDCHG        =   ZERO
               MOVE    5               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
               GO      TO      2010-INSERT-SYORI-EXT
           END-IF
      *
      *    一般チェック
           INITIALIZE                  SRYCDCHG-REC
      *****MOVE    CHGWK-KEY           TO  CHG-KEY
grpsys     MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
           MOVE    CHGWK-IPNSRYCD      TO  CHG-IPNSRYCD
           MOVE    CHGWK-RJNSRYCD      TO  CHG-RJNSRYCD
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_srycdchg_org"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srycdchg_org"  TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM                     910-SRYCDCHG-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYCDCHG
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_srycdchg_org"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      FLG-SRYCDCHG        =   ZERO
               MOVE    2               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
               GO      TO      2010-INSERT-SYORI-EXT
           END-IF
      *
      *    老人チェック
           INITIALIZE                  SRYCDCHG-REC
      *****MOVE    CHGWK-KEY           TO  CHG-KEY
grpsys     MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
           MOVE    CHGWK-IPNSRYCD      TO  CHG-IPNSRYCD
           MOVE    CHGWK-RJNSRYCD      TO  CHG-RJNSRYCD
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_srycdchg_org"  TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srycdchg_org"  TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM                     910-SRYCDCHG-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYCDCHG
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_srycdchg_org"  TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF    ( FLG-SRYCDCHG            =  ZERO )
             AND ( CHGWK-RJNSRYCD (1:1)    =  "6"  )
               MOVE    1               TO  FLG-SRYCDCHG   
           END-IF
      *
           IF      FLG-SRYCDCHG        =   ZERO
               MOVE    1               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
               GO      TO      2010-INSERT-SYORI-EXT
           END-IF
      *    一般コードチェック
           MOVE    SPACE               TO  TNS-TENSU-REC
           MOVE    CHGWK-IPNSRYCD      TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   1
               MOVE    6               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
               GO      TO      2010-INSERT-SYORI-EXT
           END-IF
      *     IF      TNS-ROUTEKKBN       =   1
      *         CONTINUE
      *     ELSE
      *         MOVE    7               TO  WRK-ERRCD
      *         PERFORM 2040-ERR-SYORI-SEC
      *         GO      TO      2010-INSERT-SYORI-EXT
      *     END-IF
      *    老人コードチェック
           MOVE    SPACE               TO  TNS-TENSU-REC
           MOVE    CHGWK-RJNSRYCD      TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   1
               MOVE    6               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
               GO      TO      2010-INSERT-SYORI-EXT
           END-IF
      *     IF      TNS-ROUTEKKBN       =   2
      *         CONTINUE
      *     ELSE
      *         MOVE    8               TO  WRK-ERRCD
      *         PERFORM 2040-ERR-SYORI-SEC
      *         GO      TO      2010-INSERT-SYORI-EXT
      *     END-IF
      *
      *    ＤＢ追加
           PERFORM 20101-SRYCDCHG-INS-SEC
           IF      WRK-ERRCD       =   ZERO
               ADD     1               TO  CNT-INS
           END-IF
      *
           .
       2010-INSERT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ追加  処理
      *****************************************************************
       20101-SRYCDCHG-INS-SEC           SECTION.
      *
           INITIALIZE                  SRYCDCHG-REC
grpsys     MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
           MOVE    SRYCDCHGWK-REC(2:)
                                       TO  SRYCDCHG-REC(3:)
      *
           MOVE    LNK-MNTYMD          TO  CHG-CREYMD
                                           CHG-UPYMD
           MOVE    LNK-MNTHMS          TO  CHG-UPHMS
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_srycdchg_org"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    1               TO  LNK-RC
               MOVE    3               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           .
       20101-SRYCDCHG-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除  処理
      *****************************************************************
       2030-DELETE-SYORI-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYCDCHG
      *
      *
           INITIALIZE                  SRYCDCHG-REC
      *****MOVE    CHGWK-KEY           TO  CHG-KEY
grpsys     MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
           MOVE    CHGWK-IPNSRYCD      TO  CHG-IPNSRYCD
           MOVE    CHGWK-RJNSRYCD      TO  CHG-RJNSRYCD
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_srycdchg_org"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srycdchg_org"  TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM                     910-SRYCDCHG-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYCDCHG
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_srycdchg_org"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
      *    削除エラー
           IF      FLG-SRYCDCHG    NOT =   ZERO
               MOVE    9               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
               GO      TO      2030-DELETE-SYORI-EXT
           END-IF
      *
      *    ＤＢ削除
           PERFORM 20101-SRYCDCHG-DEL-SEC
      *
           .
       2030-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ削除  処理
      *****************************************************************
       20101-SRYCDCHG-DEL-SEC           SECTION.
      *
           INITIALIZE                  SRYCDCHG-REC
      *****MOVE    CHGWK-KEY           TO  CHG-KEY
grpsys     MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
           MOVE    CHGWK-IPNSRYCD      TO  CHG-IPNSRYCD
           MOVE    CHGWK-RJNSRYCD      TO  CHG-RJNSRYCD
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_srycdchg_org"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    2               TO  LNK-RC
               MOVE    4               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           ELSE
               ADD     1               TO  CNT-DEL
           END-IF
      *
           .
       20101-SRYCDCHG-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー処理
      *****************************************************************
       2040-ERR-SYORI-SEC            SECTION.
      *
      *    エラー出力
           MOVE    SRYCDCHGWK-REC     TO  ERR-SRYCDCHGWK-REC
           WRITE                          ERR-SRYCDCHGWK-REC
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
           MOVE    CHGWK-SYORIKBN     TO  WRK-SYORIKBN
           MOVE    SPACE              TO  WRK-CHKKBN
           MOVE    CHGWK-IPNSRYCD     TO  WRK-IPNSRYCD
           MOVE    CHGWK-RJNSRYCD     TO  WRK-RJNSRYCD
           EVALUATE    WRK-ERRCD
               WHEN    1
                   MOVE    "老人コード登録済み" 
                                               TO  WRK-MSG-01
               WHEN    2
                   MOVE    "一般コード登録済み"
                                              TO  WRK-MSG-01
               WHEN    3
                   MOVE    "追加エラー"
                                              TO  WRK-MSG-01
               WHEN    4
                   MOVE    "削除エラー"
                                               TO  WRK-MSG-01
               WHEN    5
                   MOVE    "一般老人置換登録済み"
                                               TO  WRK-MSG-01
               WHEN    6
                   MOVE    "点数マスタなし"
                                               TO  WRK-MSG-01
               WHEN    7
                   MOVE    "一般コードエラー"
                                               TO  WRK-MSG-01
               WHEN    8
                   MOVE    "老人コードエラー"
                                               TO  WRK-MSG-01
               WHEN    9
                   MOVE    "削除対象なし"
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
               DISPLAY "*(ORCMUP0052MIGV51)* OUTFILE OPN STS[" STS-ERR
                       "]"
               GO      TO              2700-OUTLST-INIT-EXT
           END-IF
      *
      *    エラーレコード出力ファイル　オープン
           OPEN    OUTPUT              PLF-FILE
           IF  STS-PLF             NOT =   ZERO
               DISPLAY "*(ORCMUP0052MIGV51)* PLFFILE OPN STS[" STS-PLF
                       "]"
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
           MOVE    SYS-MM              TO  WRK-SMM
           MOVE    SYS-DD              TO  WRK-SDD
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
           DISPLAY "*(ORCMUP0052MIGV51)* INPUT  /I   CNT[" CNT-IN  "]"
           DISPLAY "*(ORCMUP0052MIGV51)*  INSERT     CNT[" CNT-INS "]"
           DISPLAY "*(ORCMUP0052MIGV51)*  UPDATE     CNT[" CNT-UPD "]"
           DISPLAY "*(ORCMUP0052MIGV51)*  DELETE     CNT[" CNT-DEL "]"
           DISPLAY "*(ORCMUP0052MIGV51)* ERR    /O   CNT[" CNT-ERR "]"
           DISPLAY "*(ORCMUP0052MIGV51)* PLF    /O   CNT[" CNT-PLF "]"
           DISPLAY "*(ORCMUP0052MIGV51)* RC             [" LNK-RC "]"
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
      *    一般・老人振替えマスター読込
      *****************************************************************
       910-SRYCDCHG-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYCDCHG-REC
               MOVE    ZERO                TO  FLG-SRYCDCHG
           ELSE
               INITIALIZE                  SRYCDCHG-REC
               MOVE    1                   TO  FLG-SRYCDCHG
           END-IF
      *
           .
       910-CHK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
grpsys     MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    ZERO                TO  TNS-YUKOSTYMD
           MOVE    ALL "9"             TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       910-TENSU-READ-EXT.
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
