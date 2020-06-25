       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCBMIGTENSUPLUS.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : セットアップ処理
      *  コンポーネント名  : 点数付加テーブル更新
      *                    : 点数マスタ分離に伴い点数付加テーブルの有効
      *                    : 期間を点数マスタの有効期間と一致させる必要
      *                    : がなくなったため集約させる
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/ 3/ 3    NACL-伊藤     新規作成
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
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-TNSP                PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SRYCDLIST           PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDI                     PIC 9(04).
           03  IDX                     PIC 9(04).
           03  IDT                     PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-SRYCD               PIC X(09).
           03  WRK-ERRMSG              PIC X(256).
           03  WRK-MSG-CNT             PIC ZZZ,ZZ9.
           03  SYS-DATE.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYMD2.
               05  WRK-SYY2            PIC 9(02).
               05  WRK-SMM2            PIC 9(02).
               05  WRK-SDD2            PIC 9(02).
           03  WRK-YUKOEDYMD           PIC X(08).
           03  WRK-TCNT                PIC 9(04).
           03  WRK-UCNT                PIC 9(04).
      *
       01  TENSUPLUS-AREA.
           03  TENSUPLUS-OCC           OCCURS  30.
               05  TENSUPLUS-BODY      PIC X(1000).
      *
       01  TENSUPLUS-UP-AREA.
           03  TENSUPLUS-UP-OCC        OCCURS  30.
               05  TENSUPLUS-UP-BODY   PIC X(1000).
      *
       01  TBL-ERR-AREA.
           03  TBL-ERR                 PIC 9(01)   OCCURS  31.
       01  CONST-AREA.
      **** 03  COL-MAX                 PIC 9(03) VALUE  29.
      **** 03  COL-START               PIC 9(03) VALUE  4.
           03  CONST-TBL-MAX           PIC 9(03) VALUE  31.
      *
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *    点数付加テーブル
       01  TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
       01  TENSUPLUST-REC.
           COPY    "CPTENSUPLUS.INC"   REPLACING
                                       //TNSP-// BY //TNSPT-//.
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
           COPY    "COMMON-SPA".
      *
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(1000).
      *
       PROCEDURE                   DIVISION
                                   USING   COMMAND-PARAM.
      ****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
           PERFORM 100-INIT-SEC
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END     =   1
           PERFORM 300-END-SEC
           STOP    RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
           MOVE    ZERO                TO  FLG-AREA
           MOVE    ZERO                TO  CNT-AREA
           MOVE    ZERO                TO  IDX-AREA
           INITIALIZE  WRK-AREA
           INITIALIZE  SPA-AREA
           ACCEPT  SYS-DATE        FROM    DATE
           COMPUTE WRK-SYY             =   SYS-YY  +   2000
           MOVE    SYS-MM              TO  WRK-SMM
           MOVE    SYS-DD              TO  WRK-SDD
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA-HOSPNUM
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "*(ORCBMIGTENSUPLUS)* DATABASE OPEN ERROR"
               MOVE    1                   TO  FLG-END
           END-IF
      *
           IF      FLG-END             =   ZERO
               PERFORM 900-SRYCDLIST-SELECT-SEC
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
           PERFORM 9001-TENSUPLUS-SAVE-SEC
           IF      WRK-TCNT            >   1
               PERFORM 2001-TENSUPLUS-SUM-SEC
           END-IF
           PERFORM 900-SRYCDLIST-FETCH-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    点数付加テーブル集約処理
      *****************************************************************
       2001-TENSUPLUS-SUM-SEC      SECTION.
           INITIALIZE  TENSUPLUS-UP-AREA
           MOVE    ZERO                TO  WRK-UCNT
           MOVE    ZERO                TO  IDI
           MOVE    TENSUPLUS-BODY (1)  TO  TENSUPLUS-REC
           PERFORM VARYING IDX FROM    2   BY  1
                   UNTIL   IDX >   WRK-TCNT
               INITIALIZE  TENSUPLUST-REC
               MOVE    TENSUPLUS-BODY (IDX)
                                           TO  TENSUPLUST-REC
      *        終了日の翌日を求め、期間が連続しているか判定する
               PERFORM 20011-YUKOEDYMD-P1-SEC
               IF      WRK-YUKOEDYMD       =   TNSPT-YUKOSTYMD
                   PERFORM 2001-COMP-ITEM-SEC
                   IF      TBL-ERR-AREA        =   ZERO
                       MOVE    TNSPT-YUKOEDYMD     TO  TNSP-YUKOEDYMD
                   ELSE
                       ADD     1                   TO  IDI
                       MOVE    TENSUPLUS-REC       TO
                                               TENSUPLUS-UP-BODY (IDI)
                       INITIALIZE  TENSUPLUS-REC
                       MOVE    TENSUPLUS-BODY (IDX)
                                                   TO  TENSUPLUS-REC
                   END-IF
               ELSE
                   ADD     1                   TO  IDI
                   MOVE    TENSUPLUS-REC       TO
                                               TENSUPLUS-UP-BODY (IDI)
                   INITIALIZE  TENSUPLUS-REC
                   MOVE    TENSUPLUS-BODY (IDX)
                                               TO  TENSUPLUS-REC
               END-IF
           END-PERFORM
           ADD     1                   TO  IDI
           MOVE    TENSUPLUS-REC       TO  TENSUPLUS-UP-BODY (IDI)
           MOVE    IDI                 TO  WRK-UCNT
           PERFORM 2100-TENSUPLUS-UPDATE-SEC
           .
       2001-TENSUPLUS-SUM-EXT.
           EXIT.
      *****************************************************************
      *    終了日の次の日を求める処理
      *****************************************************************
       20011-YUKOEDYMD-P1-SEC      SECTION.
           INITIALIZE  STS-AREA-DAY
           INITIALIZE  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    TNSP-YUKOEDYMD      TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    1                   TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-YUKOEDYMD
           .
       20011-YUKOEDYMD-P1-EXT.
           EXIT.
      *****************************************************************
      *    項目別比較処理
      *****************************************************************
       2001-COMP-ITEM-SEC          SECTION.
           MOVE    ZERO                TO  TBL-ERR-AREA
           IF      TNSP-SHOYUKOKETA
                                   NOT =   TNSPT-SHOYUKOKETA
               MOVE    1                   TO  TBL-ERR (4)
           END-IF
           IF      TNSP-SHONAME    NOT =   TNSPT-SHONAME
               MOVE    1                   TO  TBL-ERR (5)
           END-IF
           IF      TNSP-SAIKETUKBN NOT =   TNSPT-SAIKETUKBN
               MOVE    1                   TO  TBL-ERR (6)
           END-IF
           IF      TNSP-INPCHKKBN  NOT =   TNSPT-INPCHKKBN
               MOVE    1                   TO  TBL-ERR (7)
           END-IF
           IF      TNSP-TENPUYAKKBN
                                   NOT =   TNSPT-TENPUYAKKBN
               MOVE    1                   TO  TBL-ERR (8)
           END-IF
           IF      TNSP-ALLSURYOKBN
                                   NOT =   TNSPT-ALLSURYOKBN
               MOVE    1                   TO  TBL-ERR (9)
           END-IF
           IF      TNSP-KANZANTANICD
                                   NOT =   TNSPT-KANZANTANICD
               MOVE    1                   TO  TBL-ERR (10)
           END-IF
           IF      TNSP-KANZANTANINAME
                                   NOT =   TNSPT-KANZANTANINAME
               MOVE    1                   TO  TBL-ERR (11)
           END-IF
           IF      TNSP-KANZANCHI  NOT =   TNSPT-KANZANCHI
               MOVE    1                   TO  TBL-ERR (12)
           END-IF
           IF      TNSP-SANTEIRRKKBN
                                   NOT =   TNSPT-SANTEIRRKKBN
               MOVE    1                   TO  TBL-ERR (13)
           END-IF
           IF      TNSP-JGNCNT     NOT =   TNSPT-JGNCNT
               MOVE    1                   TO  TBL-ERR (14)
           END-IF
           IF      TNSP-JGNCNT1D   NOT =   TNSPT-JGNCNT1D
               MOVE    1                   TO  TBL-ERR (15)
           END-IF
           IF      TNSP-JGNCNT1W   NOT =   TNSPT-JGNCNT1W
               MOVE    1                   TO  TBL-ERR (16)
           END-IF
           IF      TNSP-JGNCNTERR  NOT =   TNSPT-JGNCNTERR
               MOVE    1                   TO  TBL-ERR (17)
           END-IF
           IF      TNSP-JGNCNTETCM NOT =   TNSPT-JGNCNTETCM
               MOVE    1                   TO  TBL-ERR (18)
           END-IF
           IF      TNSP-JGNCNTETC  NOT =   TNSPT-JGNCNTETC
               MOVE    1                   TO  TBL-ERR (19)
           END-IF
           IF      TNSP-SOURYOHENKBN
                                   NOT =   TNSPT-SOURYOHENKBN
               MOVE    1                   TO  TBL-ERR (20)
           END-IF
           IF      TNSP-IPN-KISAIKBN
                                   NOT =   TNSPT-IPN-KISAIKBN
               MOVE    1                   TO  TBL-ERR (21)
           END-IF
           IF      TNSP-IPN-KANZANTANICD
                                   NOT =   TNSPT-IPN-KANZANTANICD
               MOVE    1                   TO  TBL-ERR (22)
           END-IF
           IF      TNSP-IPN-KANZANTANINAME
                                   NOT =   TNSPT-IPN-KANZANTANINAME
               MOVE    1                   TO  TBL-ERR (23)
           END-IF
           IF      TNSP-IPN-KANZANCHI
                                   NOT =   TNSPT-IPN-KANZANCHI
               MOVE    1                   TO  TBL-ERR (24)
           END-IF
           IF      TNSP-ZENKNSKBN  NOT =   TNSPT-ZENKNSKBN
               MOVE    1                   TO  TBL-ERR (25)
           END-IF
           IF      TNSP-SIPPURYO1  NOT =   TNSPT-SIPPURYO1
               MOVE    1                   TO  TBL-ERR (26)
           END-IF
           IF      TNSP-SIPPUIRISU1
                                   NOT =   TNSPT-SIPPUIRISU1
               MOVE    1                   TO  TBL-ERR (27)
           END-IF
           IF      TNSP-SIPPURYO2  NOT =   TNSPT-SIPPURYO2
               MOVE    1                   TO  TBL-ERR (28)
           END-IF
           IF      TNSP-SIPPUIRISU2
                                   NOT =   TNSPT-SIPPUIRISU2
               MOVE    1                   TO  TBL-ERR (29)
           END-IF
      *    端末ＩＤ
      *    オペレーターＩＤ
      *    作成年月日
      *    更新年月日
      *    更新時間
           IF      TNSP-USERTANICD
                                   NOT =   TNSPT-USERTANICD
               MOVE    1                   TO  TBL-ERR (30)
           END-IF
           IF      TNSP-USERTANINAME
                                   NOT =   TNSPT-USERTANINAME
               MOVE    1                   TO  TBL-ERR (31)
           END-IF
           .
       2001-COMP-ITEM-EXT.
           EXIT.
      *****************************************************************
      *    点数付加テーブル登録処理
      *****************************************************************
       2100-TENSUPLUS-UPDATE-SEC   SECTION.
      * 診療行為コード単位で削除
           INITIALIZE  TENSUPLUS-REC
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    WRK-SRYCD           TO  TNSP-SRYCD
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           PERFORM 900-DELETE-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    1                   TO  FLG-END
               GO  TO  2100-TENSUPLUS-UPDATE-EXT
           END-IF
      *  更新用テーブルから登録
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >   WRK-UCNT
               INITIALIZE  TENSUPLUS-REC
               MOVE    TENSUPLUS-UP-BODY (IDX)
                                           TO  TENSUPLUS-REC
               MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-INSERT-SEC
               IF      MCP-RC              =   ZERO
                   CONTINUE
               ELSE
                   MOVE    1                   TO  FLG-END
                   MOVE    SPACE               TO  WRK-ERRMSG
                   STRING  "*(ORCBMIGTENSUPLUS)* ERROR tbl_tensuplus"
                                               DELIMITED BY SIZE
                           " insert SRYCD [ "  DELIMITED BY SIZE
                           WRK-SRYCD           DELIMITED BY SIZE
                           " ]"                DELIMITED BY SIZE
                           LOW-VALUE           DELIMITED BY SIZE
                       INTO    WRK-ERRMSG
                   END-STRING
      *            DISPLAY WRK-ERRMSG
                   CALL    "cobabort"      USING   WRK-ERRMSG
                   STOP    RUN
               END-IF
           END-PERFORM
           .
       2100-TENSUPLUS-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
           PERFORM 900-DBDISCONNECT-SEC
           MOVE    CNT-SRYCDLIST      TO  WRK-MSG-CNT
           DISPLAY "*(ORCBMIGTENSUPLUS)* SRYCD KENSU [ "
                   WRK-MSG-CNT " ]"
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    点数付加テーブル検索
      *****************************************************************
       900-SRYCDLIST-SELECT-SEC    SECTION.
           INITIALIZE  TENSUPLUS-REC
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "srycdlist"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-SRYCDLIST-FETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-SRYCDLIST-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    点数付加テーブル読込
      *****************************************************************
       900-SRYCDLIST-FETCH-SEC     SECTION.
           INITIALIZE  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "srycdlist"         TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
               MOVE    TNSP-SRYCD          TO  WRK-SRYCD
               ADD     1                   TO  CNT-SRYCDLIST
               MOVE    ZERO                TO  FLG-END
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-SRYCDLIST-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    点数付加テーブル読込
      *****************************************************************
       9001-TENSUPLUS-SAVE-SEC     SECTION.
           INITIALIZE  TENSUPLUS-AREA
           MOVE    ZERO                TO  WRK-TCNT
           INITIALIZE  TENSUPLUS-REC
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    WRK-SRYCD           TO  TNSP-SRYCD
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-TNSP
               MOVE    ZERO                TO  IDT
               PERFORM UNTIL   FLG-TNSP    =   1
                   MOVE    "tbl_tensuplus"     TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-DBFETCH-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
                       ADD     1                   TO  IDT
                       IF      IDT                 >   CONST-TBL-MAX
                           MOVE    SPACE               TO  WRK-ERRMSG
                           STRING  "*(ORCBMIGTENSUPLUS)* ERROR"
                                               DELIMITED BY SIZE
                                   " table over !"
                                               DELIMITED BY SIZE
                                   LOW-VALUE   DELIMITED BY SIZE
                               INTO    WRK-ERRMSG
                           END-STRING
      *                    DISPLAY WRK-ERRMSG
                           CALL    "cobabort"      USING   WRK-ERRMSG
                           STOP    RUN
                       END-IF
                       MOVE    TENSUPLUS-REC       TO  TENSUPLUS-BODY
                                                       (IDT)
                   ELSE
                       MOVE    1                   TO  FLG-TNSP
                   END-IF
               END-PERFORM
               MOVE    IDT                 TO  WRK-TCNT
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       9001-TENSUPLUS-SAVE-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       900-DBSELECT-SEC            SECTION.
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
      ****     CALL    "cobabort"          USING   WRK-ERRMSG
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    レコード抽出処理
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    カーソルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    レコード挿入処理
      *****************************************************************
       900-INSERT-SEC              SECTION.
           MOVE    "DBINSERT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    レコード削除処理
      *****************************************************************
       900-DELETE-SEC              SECTION.
           MOVE    "DBDELETE"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC           SECTION.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-ORCDBMAIN-EXT.
           EXIT.
