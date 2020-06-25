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
       PROGRAM-ID.                 ORCMUP0081.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＤＢ作成処理
      *  コンポーネント名  : 症状措置機序マスタ更新処理
      *                    : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03.02.21    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-小豆沢  06/05/22  MONFUNC対応
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
      *    入力：症状措置機序更新データ
       FD  IN-FILE.
       01  IN-REC.
           COPY    "CPSSKIJYOWK.INC"     REPLACING
                                         //SSKIJYOWK-// BY //IN-//.
      *
      *    出力：エラーファイル
       FD  ERR-FILE.
       01  ERR-REC.
           COPY    "CPSSKIJYOWK.INC"     REPLACING
                                         //SSKIJYOWK-// BY //ERR-//.
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
           03  FLG-SSKIJYO             PIC 9(01).
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
      *    コンスタント領域
       01  CONST-AREA.
           03  CONST-CR                PIC X(01)  VALUE  X"0A".
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
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
           03  WRK-STR1                PIC X(2048).
           03  WRK-STR2                PIC X(2048).
           03  WRK-STR                 PIC X(2048).
           03  WRK-CRCNT               PIC 9(02).
           03  WRK-CRCNT2              PIC 9(04).
      *
           03  WRK-IN                  PIC X(01).
           03  WRK-HOMEDIR             PIC X(128).
      *
      *    出力ファイル領域
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(26) VALUE
                                       "【PGID:ORCMUP0081.CBL】".
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
           03  WRK-SYOJYOUCD           PIC X(07).
      *
       01  CONST-AREA2.
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
      *    症状措置機序
       01  SSKIJYO-REC.
           COPY    "CPSSKIJYO.INC".
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
               DISPLAY "*(ORCMUP0081)* IN-FILE OPN STS["
                                       STS-IN "]"
           END-IF
      *
      *    出力ファイル　初期処理
           PERFORM 2700-OUTLST-INIT-SEC
      *
      *    症状措置機序変更ファイル読込み処理
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
                   PERFORM 2010-UPDATE-SYORI-SEC
      *        削除
               WHEN    "D"
                   PERFORM 2030-DELETE-SYORI-SEC
           END-EVALUATE
      *
      *    症状措置機序変更ファイル読込み処理
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
           INITIALIZE                  SSKIJYO-REC
           MOVE    IN-KEY              TO  SSKIJYO-KEY
           MOVE    SSKIJYO-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sskijyo_master"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sskijyo_master"
                                           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM                     910-SSKIJYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SSKIJYO
           END-IF
           MOVE    "tbl_sskijyo_master"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    あればそのまま
           IF      FLG-SSKIJYO       =   ZERO
               MOVE    1               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
               GO      TO      2010-INSERT-SYORI-EXT
           END-IF
      *
      *    ＤＢ追加
           PERFORM 20101-SSKIJYO-INS-SEC
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
       20101-SSKIJYO-INS-SEC           SECTION.
      *
           INITIALIZE                  SSKIJYO-REC
           MOVE    IN-REC(2:)          TO  SSKIJYO-REC
      *
           MOVE    LNK-MNTYMD          TO  SSKIJYO-CREYMD
                                           SSKIJYO-UPYMD
           MOVE    LNK-MNTHMS          TO  SSKIJYO-UPHMS
           MOVE    SSKIJYO-REC         TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_sskijyo_master"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    1               TO  LNK-RC
               MOVE    2               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           .
      *
       20101-SSKIJYO-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新  処理
      *****************************************************************
       2010-UPDATE-SYORI-SEC               SECTION.
      *
           INITIALIZE                  SSKIJYO-REC
           MOVE    IN-KEY              TO  SSKIJYO-KEY
           MOVE    SSKIJYO-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sskijyo_master"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sskijyo_master"
                                           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM                     910-SSKIJYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SSKIJYO
           END-IF
           MOVE    "tbl_sskijyo_master"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    なければそのまま
           IF      FLG-SSKIJYO       NOT =   ZERO
               MOVE    3               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
               GO      TO      2010-UPDATE-SYORI-EXT
           END-IF
      *
      *    ＤＢ更新
           PERFORM 20101-SSKIJYO-UPD-SEC
           IF      WRK-ERRCD       =   ZERO
               ADD     1               TO  CNT-UPD
           END-IF
      *
           .
       2010-UPDATE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ更新  処理
      *****************************************************************
       20101-SSKIJYO-UPD-SEC           SECTION.
      *
           INITIALIZE                  SSKIJYO-REC
           MOVE    IN-REC(2:)          TO  SSKIJYO-REC
      *
           MOVE    LNK-MNTYMD          TO  SSKIJYO-UPYMD
           MOVE    LNK-MNTHMS          TO  SSKIJYO-UPHMS
           MOVE    SSKIJYO-REC         TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_sskijyo_master"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    2               TO  LNK-RC
               MOVE    5               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           .
      *
       20101-SSKIJYO-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除  処理
      *****************************************************************
       2030-DELETE-SYORI-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SSKIJYO
      *
           INITIALIZE                  SSKIJYO-REC
           MOVE    IN-KEY              TO  SSKIJYO-KEY
           MOVE    SSKIJYO-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sskijyo_master"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sskijyo_master"
                                           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM                     910-SSKIJYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SSKIJYO
           END-IF
           MOVE    "tbl_sskijyo_master"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    削除エラー
           IF      FLG-SSKIJYO    NOT =   ZERO
               MOVE    7               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
               GO      TO      2030-DELETE-SYORI-EXT
           END-IF
      *
      *    ＤＢ削除
           PERFORM 20101-SSKIJYO-DEL-SEC
      *
           .
       2030-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ削除  処理
      *****************************************************************
       20101-SSKIJYO-DEL-SEC           SECTION.
      *
           MOVE    SSKIJYO-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sskijyo_master"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    3               TO  LNK-RC
               MOVE    6               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           ELSE
               ADD     1               TO  CNT-DEL
           END-IF
      *
           .
       20101-SSKIJYO-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー処理
      *****************************************************************
       2040-ERR-SYORI-SEC            SECTION.
      *
      *    エラー出力
           MOVE    IN-REC             TO  ERR-REC
           WRITE                          ERR-REC
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
           MOVE    IN-SYORIKBN      TO  WRK-SYORIKBN
           MOVE    IN-SYOJYOUCD     TO  WRK-SYOJYOUCD
           EVALUATE    WRK-ERRCD
               WHEN    1
                   MOVE    "症状措置登録済み"
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
               DISPLAY "*(ORCMUP0081)* OUTFILE OPN STS[" STS-ERR "]"
               GO      TO              2700-OUTLST-INIT-EXT
           END-IF
      *
      *    エラーレコード出力ファイル　オープン
           OPEN    OUTPUT              PLF-FILE
           IF  STS-PLF             NOT =   ZERO
               DISPLAY "*(ORCMUP0081)* PLFFILE OPN STS[" STS-PLF "]"
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
           DISPLAY "*(ORCMUP0081)* INPUT  /I   CNT[" CNT-IN  "]"
           DISPLAY "*(ORCMUP0081)*  INSERT     CNT[" CNT-INS "]"
           DISPLAY "*(ORCMUP0081)*  UPDATE     CNT[" CNT-UPD "]"
           DISPLAY "*(ORCMUP0081)*  DELETE     CNT[" CNT-DEL "]"
           DISPLAY "*(ORCMUP0081)* ERR    /O   CNT[" CNT-ERR "]"
           DISPLAY "*(ORCMUP0081)* PLF    /O   CNT[" CNT-PLF "]"
           DISPLAY "*(ORCMUP0081)* RC             [" LNK-RC "]"
     **** ACCEPT  WRK-IN
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *      症状措置機序変更ファイル読込み処理
      *****************************************************************
       900-IN-KENSAKU-SEC          SECTION.
      *
          READ    IN-FILE
              AT  END
                  MOVE    1               TO  FLG-IN
              NOT AT  END
                  MOVE    ZERO            TO  FLG-IN
                  ADD     1               TO  CNT-IN
                  PERFORM 901-IN-KENSAKU-CRCNV-SEC
          END-READ
          .
       900-IN-KENSAKU-EXT.
          EXIT.
      *
      *****************************************************************
      *      症状措置機序変更ファイル改行コード変換処理
      *****************************************************************
       901-IN-KENSAKU-CRCNV-SEC          SECTION.
      *
      * 症状
           MOVE    ZERO                TO  WRK-CRCNT
           INSPECT IN-SYOJYOU          TALLYING  WRK-CRCNT
                                       FOR ALL "￥ｎ"
           IF      WRK-CRCNT           >   0
               MOVE    SPACE           TO  WRK-STR
               PERFORM VARYING  IDX    FROM  1  BY  1
                                UNTIL  IDX  >  WRK-CRCNT
                 MOVE  SPACE           TO  WRK-STR1
                 MOVE  SPACE           TO  WRK-STR2
                 MOVE  ZERO            TO  WRK-CRCNT2
                 INSPECT IN-SYOJYOU    TALLYING   WRK-CRCNT2
                                       FOR   CHARACTERS
                                       BEFORE INITIAL "￥ｎ"
                 IF      WRK-CRCNT2    >   0
                     MOVE  IN-SYOJYOU(1:WRK-CRCNT2)
                                       TO  WRK-STR1
                     MOVE  IN-SYOJYOU(WRK-CRCNT2 + 5:)
                                       TO  WRK-STR2
                     STRING WRK-STR  DELIMITED BY SPACE
                            WRK-STR1 DELIMITED BY SPACE
                            CONST-CR DELIMITED BY SIZE
                            INTO WRK-STR
                     END-STRING
                     MOVE   WRK-STR2   TO  IN-SYOJYOU
                 END-IF
               END-PERFORM
               STRING WRK-STR  DELIMITED BY SPACE
                      WRK-STR2 DELIMITED BY SPACE
                      INTO IN-SYOJYOU
               END-STRING
           END-IF
      *
      * 作用機序
           MOVE    ZERO                TO  WRK-CRCNT
           INSPECT IN-SAYOKIJYO        TALLYING  WRK-CRCNT
                                       FOR ALL "￥ｎ"
           IF      WRK-CRCNT           >   0
               MOVE    SPACE           TO  WRK-STR
               PERFORM VARYING  IDX    FROM  1  BY  1
                                UNTIL  IDX  >  WRK-CRCNT
                 MOVE  SPACE           TO  WRK-STR1
                 MOVE  SPACE           TO  WRK-STR2
                 MOVE  ZERO            TO  WRK-CRCNT2
                 INSPECT IN-SAYOKIJYO  TALLYING   WRK-CRCNT2
                                       FOR   CHARACTERS
                                       BEFORE INITIAL "￥ｎ"
                 IF      WRK-CRCNT2    >   0
                     MOVE  IN-SAYOKIJYO(1:WRK-CRCNT2)
                                       TO  WRK-STR1
                     MOVE  IN-SAYOKIJYO(WRK-CRCNT2 + 5:)
                                       TO  WRK-STR2
                     STRING WRK-STR  DELIMITED BY SPACE
                            WRK-STR1 DELIMITED BY SPACE
                            CONST-CR DELIMITED BY SIZE
                            INTO WRK-STR
                     END-STRING
                     MOVE   WRK-STR2   TO  IN-SAYOKIJYO
                 END-IF
               END-PERFORM
               STRING WRK-STR  DELIMITED BY SPACE
                      WRK-STR2 DELIMITED BY SPACE
                      INTO IN-SAYOKIJYO
               END-STRING
           END-IF
           .
       901-IN-KENSAKU-CRCNV-EXT.
           EXIT.
      *
      *****************************************************************
      *     症状措置機序マスター読込
      *****************************************************************
       910-SSKIJYO-READ-SEC            SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SSKIJYO-REC
               MOVE    ZERO                TO  FLG-SSKIJYO
           ELSE
               INITIALIZE                  SSKIJYO-REC
               MOVE    1                   TO  FLG-SSKIJYO
           END-IF
           .
       910-SSKIJYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC               SECTION.
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
