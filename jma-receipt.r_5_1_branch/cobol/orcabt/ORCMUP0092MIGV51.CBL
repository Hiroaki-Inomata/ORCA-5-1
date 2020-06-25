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
       PROGRAM-ID.                 ORCMUP0092MIGV51.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＤＢ作成処理
      *  コンポーネント名  : 保険者マスタ更新処理
      *                    : （ライセンスマスタから使用）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06.09.25    NACL-伊藤     新規作成
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
      *    入力：保険者更新データ
       FD  IN-FILE.
       01  IN-REC.
           COPY    "CPHKNJAINFWKV01.INC" REPLACING
                                         //HKNJAWK-// BY //IN-//.
      *
      *    出力：エラーファイル
       FD  ERR-FILE.
       01  ERR-REC.
           COPY    "CPHKNJAINFWKV01.INC" REPLACING
                                         //HKNJAWK-// BY //ERR-//.
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
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-HKNJA               PIC 9(01).
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
           03  WRK-ERRCD               PIC 9(02).
           03  WRK-IN                  PIC X(01).
           03  WRK-HOMEDIR             PIC X(128).
      *
      *    出力ファイル領域
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(31) VALUE
                                       "【PGID:ORCMUP0092MIGV51.CBL】".
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
           03  WRK-HOSPNUM             PIC 9(02).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-HKNJANUM            PIC X(08).
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
      *    システム管理マスタ
           COPY    "CPSK1001.INC".
      *
      *    保険者
       01  HKNJA-REC.
           COPY    "CPHKNJAINF.INC".
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
           MOVE    ZERO            TO  FLG-AREA
           MOVE    ZERO            TO  CNT-AREA
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
      *    医療機関ＩＤ編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    ZERO                TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE
               DISPLAY "SYS 001 ERR"
           END-IF
      *
      *    ファイル情報
           PERFORM 101-ASSIN-GET-SEC
      *
      *    入力　初期処理
           OPEN    INPUT           IN-FILE
           IF  STS-IN          NOT =   ZERO
               MOVE    1           TO  FLG-IN
               DISPLAY "*(ORCMUP0092MIGV51)* IN-FILE OPN STS["
                                       STS-IN "]"
           END-IF
      *
      *    出力ファイル　初期処理
           PERFORM 2700-OUTLST-INIT-SEC
      *
      *    保険者変更ファイル読込み処理
           IF      FLG-IN              =   ZERO
               PERFORM 900-IN-KENSAKU-SEC
           END-IF
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
           EVALUATE    IN-SYORIKBN
      *        追加
               WHEN    "I"
                   PERFORM 2010-INSERT-SYORI-SEC
      *       更新
               WHEN    "U"
                   PERFORM 2020-UPDATE-SYORI-SEC
      *        削除
               WHEN    "D"
                   PERFORM 2030-DELETE-SYORI-SEC
           END-EVALUATE
      *
      *    保険者変更ファイル読込み処理
           PERFORM 900-IN-KENSAKU-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加  処理
      *****************************************************************
       2010-INSERT-SYORI-SEC               SECTION.
      *
           PERFORM 910-HKNJA-CHECK-SEC
      *
           IF      FLG-HKNJA           =   ZERO
      *        既にレコードが存在するならば更新を行う
               PERFORM 20201-HKNJA-UPD-SEC
           ELSE
      *        追加
               PERFORM 20101-HKNJA-INS-SEC
           END-IF
           .
       2010-INSERT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ追加  処理
      *****************************************************************
       20101-HKNJA-INS-SEC         SECTION.
      *
           INITIALIZE                      HKNJA-REC
grpsys     MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
           MOVE    IN-REC(26:)         TO  HKNJA-REC(3:)
           MOVE    LNK-MNTYMD          TO  HKNJA-CREYMD
                                           HKNJA-UPYMD
           MOVE    LNK-MNTHMS          TO  HKNJA-UPHMS
           MOVE    HKNJA-REC           TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_hknjainf_org"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               ADD     1               TO  CNT-INS
           ELSE
               MOVE    1               TO  LNK-RC
               MOVE    2               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           .
       20101-HKNJA-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新  処理
      *****************************************************************
       2020-UPDATE-SYORI-SEC       SECTION.
      *
           PERFORM 910-HKNJA-CHECK-SEC
      *
           IF      FLG-HKNJA           =   ZERO
      *        更新
               PERFORM 20201-HKNJA-UPD-SEC
           ELSE
      *        レコードが存在しないならば追加を行う
               PERFORM 20101-HKNJA-INS-SEC
           END-IF
           .
       2020-UPDATE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ更新  処理
      *****************************************************************
       20201-HKNJA-UPD-SEC         SECTION.
      *
           PERFORM 2099-HKNJA-SET-SEC
      *
           MOVE    LNK-MNTYMD          TO  HKNJA-UPYMD
           MOVE    LNK-MNTHMS          TO  HKNJA-UPHMS
           MOVE    HKNJA-REC           TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_hknjainf_org"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               ADD     1               TO  CNT-UPD
           ELSE
               MOVE    2               TO  LNK-RC
               MOVE    5               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           .
       20201-HKNJA-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除  処理
      *****************************************************************
       2030-DELETE-SYORI-SEC       SECTION.
      *
           PERFORM 910-HKNJA-CHECK-SEC
      *
           IF      FLG-HKNJA           =   ZERO
      *        削除
               PERFORM 20301-HKNJA-DEL-SEC
           ELSE
               MOVE    7               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           .
       2030-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ削除  処理
      *****************************************************************
       20301-HKNJA-DEL-SEC         SECTION.
      *
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    HKNJA-REC           TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_hknjainf_org"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               ADD     1               TO  CNT-DEL
           ELSE
               MOVE    3               TO  LNK-RC
               MOVE    6               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           .
       20301-HKNJA-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者データセット処理
      *****************************************************************
       2099-HKNJA-SET-SEC          SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
           MOVE    IN-HKNJANUM         TO  HKNJA-HKNJANUM
           MOVE    IN-HKNJANAME        TO  HKNJA-HKNJANAME
           MOVE    IN-HKNJANAME-TAN1   TO  HKNJA-HKNJANAME-TAN1
           MOVE    IN-HKNJANAME-TAN2   TO  HKNJA-HKNJANAME-TAN2
           MOVE    IN-HKNJANAME-TAN3   TO  HKNJA-HKNJANAME-TAN3
           MOVE    IN-HKNNUM           TO  HKNJA-HKNNUM
      *給付割合はセットしない
      *    データが０、空白の場合は何もしない
      *    "***" の場合は０リセットする
      *    本人外来
           IF      IN-HON-GAIKYURATE-G =   "***"
               MOVE  ZERO              TO  HKNJA-HON-GAIKYURATE
           ELSE
             IF    IN-HON-GAIKYURATE-G =   "000"  OR  SPACE
                   CONTINUE
             ELSE
               MOVE  IN-HON-GAIKYURATE TO  HKNJA-HON-GAIKYURATE
             END-IF
           END-IF
      *    本人入院
           IF      IN-HON-NYUKYURATE-G =   "***"
               MOVE  ZERO              TO  HKNJA-HON-NYUKYURATE
           ELSE
             IF    IN-HON-NYUKYURATE-G =   "000"  OR  SPACE
                   CONTINUE
             ELSE
               MOVE  IN-HON-NYUKYURATE TO  HKNJA-HON-NYUKYURATE
             END-IF
           END-IF
      *    家族外来
           IF      IN-KZK-GAIKYURATE-G =   "***"
               MOVE  ZERO              TO  HKNJA-KZK-GAIKYURATE
           ELSE
             IF    IN-KZK-GAIKYURATE-G =   "000"  OR  SPACE
                   CONTINUE
             ELSE
               MOVE  IN-KZK-GAIKYURATE TO  HKNJA-KZK-GAIKYURATE
             END-IF
           END-IF
      *    家族入院
           IF      IN-KZK-NYUKYURATE-G =   "***"
               MOVE  ZERO              TO  HKNJA-KZK-NYUKYURATE
           ELSE
             IF    IN-KZK-NYUKYURATE-G =   "000"  OR  SPACE
                   CONTINUE
             ELSE
               MOVE  IN-KZK-NYUKYURATE TO  HKNJA-KZK-NYUKYURATE
             END-IF
           END-IF
           MOVE    IN-POST             TO  HKNJA-POST
           MOVE    IN-ADRS             TO  HKNJA-ADRS
           MOVE    IN-BANTI            TO  HKNJA-BANTI
           MOVE    IN-TEL              TO  HKNJA-TEL
      *記号はセットしない
      *    データが空白の場合は何もしない
      *    先頭３文字が"***" の場合はクリアする
           IF      IN-KIGO (1:3)       =   "***"
               MOVE  SPACE             TO  HKNJA-KIGO
           ELSE
             IF    IN-KIGO             =   SPACE
                   CONTINUE
             ELSE
               MOVE  IN-KIGO           TO  HKNJA-KIGO
             END-IF
           END-IF
           MOVE    IN-IDOKBN           TO  HKNJA-IDOKBN
           MOVE    IN-IDOYMD           TO  HKNJA-IDOYMD
      **   MOVE    IN-TERMID           TO  HKNJA-TERMID
      **   MOVE    IN-OPID             TO  HKNJA-OPID
      **   MOVE    IN-CREYMD           TO  HKNJA-CREYMD
      **   MOVE    IN-UPYMD            TO  HKNJA-UPYMD
      **   MOVE    IN-UPHMS            TO  HKNJA-UPHMS
           .
       2099-HKNJA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー処理
      *****************************************************************
       2040-ERR-SYORI-SEC          SECTION.
      *
      *    エラー出力
           MOVE    IN-REC              TO  ERR-REC
           WRITE                           ERR-REC
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
           MOVE    IN-SYORIKBN         TO  WRK-SYORIKBN
           MOVE    SPA-HOSPNUM         TO  WRK-HOSPNUM
           MOVE    IN-HKNJANUM         TO  WRK-HKNJANUM
           EVALUATE    WRK-ERRCD
               WHEN    1
                   MOVE    "保険者登録済み"
                                       TO  WRK-MSG-01
               WHEN    2
                   MOVE    "追加エラー"
                                       TO  WRK-MSG-01
               WHEN    3
                   MOVE    "更新対象なし"
                                       TO  WRK-MSG-01
               WHEN    4
                   MOVE    "更新無しエラー"
                                       TO  WRK-MSG-01
               WHEN    5
                   MOVE    "更新エラー"
                                       TO  WRK-MSG-01
               WHEN    6
                   MOVE    "削除エラー"
                                       TO  WRK-MSG-01
               WHEN    7
                   MOVE    "削除対象なし"
                                       TO  WRK-MSG-01
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
               DISPLAY "*(ORCMUP0092MIGV51)* OUTFILE OPN STS[" STS-ERR
                       "]"
               GO      TO              2700-OUTLST-INIT-EXT
           END-IF
      *
      *    エラーレコード出力ファイル　オープン
           OPEN    OUTPUT              PLF-FILE
           IF  STS-PLF             NOT =   ZERO
               DISPLAY "*(ORCMUP0092MIGV51)* PLFFILE OPN STS[" STS-PLF
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
      *
           DISPLAY "*(ORCMUP0092MIGV51)* INPUT  /I   CNT[" CNT-IN  "]"
           DISPLAY "*(ORCMUP0092MIGV51)*  INSERT     CNT[" CNT-INS "]"
           DISPLAY "*(ORCMUP0092MIGV51)*  UPDATE     CNT[" CNT-UPD "]"
           DISPLAY "*(ORCMUP0092MIGV51)*  DELETE     CNT[" CNT-DEL "]"
           DISPLAY "*(ORCMUP0092MIGV51)* ERR    /O   CNT[" CNT-ERR "]"
           DISPLAY "*(ORCMUP0092MIGV51)* PLF    /O   CNT[" CNT-PLF "]"
           DISPLAY "*(ORCMUP0092MIGV51)* RC             [" LNK-RC "]"
      **** ACCEPT  WRK-IN
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO        TO  FLG-SYSKANRI
               ELSE
                   MOVE    1           TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *     保険者変更ファイル読込み処理
      *****************************************************************
       900-IN-KENSAKU-SEC          SECTION.
      *
          READ    IN-FILE
              AT  END
                  MOVE    1            TO  FLG-IN
              NOT AT  END
                  MOVE    ZERO         TO  FLG-IN
                  ADD     1            TO  CNT-IN
          END-READ
      *
          MOVE    SYS-1001-HOSPID      TO  IN-HOSPID
          MOVE    SPACE                TO  IN-TERMID
          MOVE    SPACE                TO  IN-OPID
          MOVE    SPACE                TO  IN-CREYMD
          MOVE    SPACE                TO  IN-UPYMD
          MOVE    SPACE                TO  IN-UPHMS
          .
       900-IN-KENSAKU-EXT.
          EXIT.
      *
      *****************************************************************
      *    保険者マスター読込
      *****************************************************************
       910-HKNJA-CHECK-SEC         SECTION.
      *
           INITIALIZE                      HKNJA-REC
grpsys     MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
           MOVE    IN-HKNJANUM         TO  HKNJA-HKNJANUM
           MOVE    HKNJA-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hknjainf_org"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf_org"
                                       TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM                     910-HKNJA-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNJA
           END-IF
           MOVE    "tbl_hknjainf_org"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       910-HKNJA-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者マスター読込
      *****************************************************************
       910-HKNJA-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNJA-REC
               MOVE    SPACE           TO  HKNJA-TERMID
               MOVE    SPACE           TO  HKNJA-OPID
      **       MOVE    SPACE           TO  HKNJA-CREYMD
      **       MOVE    SPACE           TO  HKNJA-UPYMD
      **       MOVE    SPACE           TO  HKNJA-UPHMS
               MOVE    ZERO            TO  FLG-HKNJA
           ELSE
               INITIALIZE                  HKNJA-REC
               MOVE    1               TO  FLG-HKNJA
           END-IF
           .
       910-HKNJA-READ-EXT.
           EXIT.
      *****************************************************************
      *    カーソルクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       990-DBCLOSE-EXT.
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
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
