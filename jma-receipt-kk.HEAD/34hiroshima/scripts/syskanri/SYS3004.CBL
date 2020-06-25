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
       PROGRAM-ID.                 SYS3004.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＤＢ作成処理
      *  コンポーネント名  : システム管理更新処理
      *                    : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  14/10/02    uchi    新規作成
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
       01  IN-3004-REC.
      *シェルID
           03  IN-3004-SHELLID         PIC  X(020).
      *処理名称
           03  IN-3004-SHORIMSG        PIC  X(040).
      *設定フラグ
           03  IN-3004-CHK.
               05  IN-3004-CHKFLG      PIC  X(01)
                                        OCCURS   4.
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
           03  FLG-PARA                PIC 9(01).
           03  FLG-IN                  PIC 9(01).
      *
           03  FLG-SYS                 PIC 9(01).
           03  FLG-CHECK               PIC 9(01).
           03  FLG-DATA                PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDX-LIKE                PIC 9(04).
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
           03  WRK-LAST-STYUKYMD       PIC X(08).
           03  WRK-LAST-EDYUKYMD       PIC X(08).
      *
      *　　主キー更新用項目
           03  WRK-SYSUPD-KEY.
               05  WRK-SYSUP-HOSPNUM   PIC 9(02).
               05  WRK-SYSUP-KANRICD   PIC X(04).
               05  WRK-SYSUP-KBNCD     PIC X(08).
               05  WRK-SYSUP-STYUKYMD  PIC X(08).
               05  WRK-SYSUP-EDYUKYMD  PIC X(08).
      *
           03  WRK-KBNCD               PIC X(08).
      *
       01  LNK-AREA.
           03  LNK-HOSPNUM             PIC  X(02).
           03  LNK-YMD                 PIC  X(08).
           03  LNK-HMS                 PIC  X(06).
           03  LNK-DIR                 PIC  X(100).
           03  LNK-FILENAME            PIC  X(30).
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
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
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
           END-UNSTRING
      *
           DISPLAY "START"
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
                   LNK-FILENAME        DELIMITED  BY  SPACE
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
      *    シス管「3004」データの存在チェック
           PERFORM  2001-DATA-CHECK-SEC
           IF  FLG-CHECK  =  0
      *        データ追加
               PERFORM  201-DATA-INSERT-MAIN-SEC
                   UNTIL ( FLG-IN      =   1 )
           ELSE
      *        データ更新
               PERFORM  202-DATA-UPDATE-MAIN-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    シス管「3004」データ存在チェック  処理
      *****************************************************************
       2001-DATA-CHECK-SEC               SECTION.
      *
      *    一度も参照していない場合のみデータが存在しない
           INITIALIZE                  SYSKANRI-REC
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "3004"              TO  SYS-KANRICD
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               
               IF  FLG-SYS  =  0
                   MOVE      1             TO  FLG-CHECK
               END-IF
           END-IF
      *
           MOVE  "tbl_syskanri"        TO  MCP-TABLE
           MOVE  "key5"                TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2001-DATA-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ追加メイン  処理
      *****************************************************************
       201-DATA-INSERT-MAIN-SEC               SECTION.
      *
      *    ファイル読み込み
           PERFORM  900-DATA-READ-SEC
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >    25
                    OR       FLG-IN  =  1
      *        データ登録（追加）
               PERFORM  2010-DATA-INSERT-SEC
      *
      *        ファイル読み込み
               PERFORM  900-DATA-READ-SEC
           END-PERFORM
      *
           .
       201-DATA-INSERT-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ追加  処理
      *****************************************************************
       2010-DATA-INSERT-SEC               SECTION.
      *
      *    データ追加処理
           INITIALIZE                  SYSKANRI-REC
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "3004"              TO  SYS-KANRICD
           MOVE    IDX                 TO  SYS-KBNCD
           MOVE    "00000000"          TO  SYS-STYUKYMD
           MOVE    "99999999"          TO  SYS-EDYUKYMD
           MOVE    "     0"            TO  SYS-UPHMS
           MOVE    IN-3004-REC         TO  SYS-KANRITBL
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               DISPLAY  "DATA INSERT!"
           END-IF
      *
           .
       2010-DATA-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ更新メイン  処理
      *****************************************************************
       202-DATA-UPDATE-MAIN-SEC               SECTION.
      *
      *    最新の有効期限取得
           PERFORM  2020-KIGEN-GET-SEC
      *
      *    ファイル読み込み
           PERFORM  900-DATA-READ-SEC
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >    25
                    OR       FLG-DATA   =  1
                    OR       FLG-IN     =  1
      *        shellidの重複チェック
               PERFORM  2021-TYOFUKU-CHECK-SEC
      *
               IF  FLG-CHECK  =  0
      *            登録場所チェック（空欄はあるか）
                   PERFORM  2022-SPACE-CHECK-SEC
      *
      *            空欄がないなら抜ける（自動登録処理終了）
                   IF  FLG-CHECK  =  1
      *                データ登録（更新）
                       PERFORM  2023-DATA-UPDATE-SEC
                   ELSE
                       MOVE  1  TO  FLG-DATA
                   END-IF
               END-IF
      *
      *        ファイル読み込み
               PERFORM  900-DATA-READ-SEC
      *        読み込んだデータが空白なら自動登録処理終了
               IF  IN-3004-SHELLID(1:20)  =  SPACE
                   MOVE  1  TO  FLG-DATA
               END-IF
           END-PERFORM
      *
           .
       202-DATA-UPDATE-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    最新有効期限取得  処理
      *****************************************************************
       2020-KIGEN-GET-SEC               SECTION.
      *
      *    最新取得
           INITIALIZE                  SYSKANRI-REC
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "3004"              TO  SYS-KANRICD
           MOVE    "0001"              TO  SYS-KBNCD
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               
               IF  FLG-SYS  =  ZERO
                   MOVE  SYS-STYUKYMD  TO  WRK-LAST-STYUKYMD
                   MOVE  SYS-EDYUKYMD  TO  WRK-LAST-EDYUKYMD
               ELSE
                   MOVE  1  TO  FLG-DATA
               END-IF
           ELSE
               MOVE  1  TO  FLG-DATA
           END-IF
      *
           MOVE  "tbl_syskanri"        TO  MCP-TABLE
           MOVE  "key8"                TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2020-KIGEN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    shellidの重複チェック  処理
      *****************************************************************
       2021-TYOFUKU-CHECK-SEC               SECTION.
      *
      *    重複チェック
           INITIALIZE                  SYSKANRI-REC
                                       FLG-CHECK
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "3004"              TO  SYS-KANRICD
           MOVE    WRK-LAST-STYUKYMD   TO  SYS-STYUKYMD
           MOVE    WRK-LAST-EDYUKYMD   TO  SYS-EDYUKYMD
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
      *
               PERFORM  UNTIL  FLG-SYS    =  1
                        OR     FLG-CHECK  =  1
      *            同じShellidが登録されているか
                   IF  SYS-KANRITBL(1:20)  =  IN-3004-SHELLID
                       MOVE  1  TO  FLG-CHECK
                   ELSE
                       PERFORM  910-SYSKANRI-READ-SEC
                   END-IF
               END-PERFORM
           ELSE
               MOVE  1  TO  FLG-DATA
           END-IF
      *
           MOVE  "tbl_syskanri"        TO  MCP-TABLE
           MOVE  "key2"                TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2021-TYOFUKU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録場所チェック  処理
      *****************************************************************
       2022-SPACE-CHECK-SEC               SECTION.
      *
      *    空欄はあるか
           INITIALIZE                  SYSKANRI-REC
                                       FLG-CHECK
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "3004"              TO  SYS-KANRICD
           MOVE    WRK-LAST-STYUKYMD   TO  SYS-STYUKYMD
           MOVE    WRK-LAST-EDYUKYMD   TO  SYS-EDYUKYMD
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               
               PERFORM  UNTIL  FLG-SYS    =  1
                        OR     FLG-CHECK  =  1
      *            空いているか
                   IF  SYS-KANRITBL(1:60)  =  SPACE
                       MOVE  SYS-KBNCD  TO  WRK-KBNCD
                       MOVE  1          TO  FLG-CHECK
                   ELSE
                       PERFORM  910-SYSKANRI-READ-SEC
                   END-IF
               END-PERFORM
           ELSE
               MOVE  1  TO  FLG-DATA
           END-IF
      *
           MOVE  "tbl_syskanri"        TO  MCP-TABLE
           MOVE  "key2"                TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2022-SPACE-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ登録（更新）  処理
      *****************************************************************
       2023-DATA-UPDATE-SEC               SECTION.
      *
      *    空欄はあるか
           INITIALIZE                  SYSKANRI-REC
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "3004"              TO  SYS-KANRICD
           MOVE    WRK-KBNCD           TO  SYS-KBNCD
           MOVE    WRK-LAST-STYUKYMD   TO  SYS-STYUKYMD
           MOVE    WRK-LAST-EDYUKYMD   TO  SYS-EDYUKYMD
           MOVE    "     0"            TO  SYS-UPHMS
           MOVE    IN-3004-REC         TO  SYS-KANRITBL
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               DISPLAY  "DATA UPDATE!"
           END-IF
      *
           .
       2023-DATA-UPDATE-EXT.
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
      *    システム管理更新ファイル読込み処理
      *****************************************************************
       900-DATA-READ-SEC          SECTION.
      *
          READ    IN-FILE
              AT  END
                  MOVE    1               TO  FLG-IN
              NOT AT  END
                  MOVE    ZERO            TO  FLG-IN
          END-READ
      *
          .
       900-DATA-READ-EXT.
          EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               MOVE    ZERO            TO  FLG-SYS
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1               TO  FLG-SYS
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
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
