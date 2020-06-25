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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGW96.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : ＣＲＯＮ情報設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/08/19    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    コンスタント値
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
       01  SPA-W96.
           03  SPA-GMN-CUR                        PIC 9(03).
           03  SPA-GMN-B08-PUSHED                 PIC 9(03).
           03  SPA-GMN-MODE                       PIC 9(01).
           03  SPA-GMN-AREA.
               05  SPA-GMN-CRON-G.
                   07  SPA-GMN-CRON-OCC           OCCURS  10.
                       09  SPA-GMN-NAME           PIC X(30).
                       09  SPA-GMN-SCRIPT         PIC X(100).
                       09  SPA-GMN-SCHEDULE-G.
                           11  SPA-GMN-DOW-G.
                               13  SPA-GMN-DOW    PIC X(1).
                               13  SPA-GMN-DOWFL  PIC X(1).
                               13  SPA-GMN-DOWMEI PIC X(8).
                           11  SPA-GMN-MON        PIC X(2).
                           11  SPA-GMN-DOM        PIC X(2).
                           11  SPA-GMN-HOUR       PIC X(2).
                           11  SPA-GMN-EXEC       PIC X(1).
           03  SPA-GMN-COMBO-AREA.
               05  SPA-GMN-DOWLST-G.
                   07  SPA-GMN-DOW-MAX            PIC 9(03).
                   07  SPA-GMN-DOWLST             OCCURS   8.
                       09  SPA-GMN-DOWL           PIC X(01).
                       09  SPA-GMN-DOWFLL         PIC X(01).
                       09  SPA-GMN-DOWMEIL        PIC X(08).
               05  SPA-GMN-MONLST-G.
                   07  SPA-GMN-MON-MAX            PIC 9(03).
                   07  SPA-GMN-MONLST             OCCURS  13
                                                  PIC X(02).
               05  SPA-GMN-DOMLST-G.
                   07  SPA-GMN-DOM-MAX            PIC 9(03).
                   07  SPA-GMN-DOMLST             OCCURS  32
                                                  PIC X(02).
               05  SPA-GMN-HOURLST-G.
                   07  SPA-GMN-HOUR-MAX           PIC 9(03).
                   07  SPA-GMN-HOURLST            OCCURS  25
                                                  PIC X(02).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-CRON                            PIC 9(01).
           03  FLG-KANACHK-ERR                     PIC 9(01).
      *
       01  CNT-AREA.
           03  CNT-JMAID                           PIC 9(04).
           03  CNT-USRID                           PIC 9(04).
           03  CNT-CRONID                          PIC 9(04).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX1                                PIC 9(05).
           03  IDX-CRON                            PIC 9(05).
           03  IDX-CMB                             PIC 9(05).
           03  IDX-JMA                             PIC 9(05).
           03  IDX-USR                             PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
      *    入力項目名
           03  WRK-MCP-WIDGET                      PIC X(64).
           03  WRK-WIDMSG                          PIC X(100).
           03  WRK-CURSOR                          PIC 9(03).
           03  WRK-KANACHK-STR                     PIC X(100).
           03  WRK-CMB-CD                          PIC X(10).
           03  WRK-CMB-ITM-G.
               07  WRK-CMB-ITM                     PIC X(200).
           03  WRK-NUM99-X.
               05  WRK-NUM99                       PIC 9(02).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-CRON-MAX                      PIC 9(03)   VALUE 10.
           03  CONST-JMA-MAX                       PIC 9(03)   VALUE 5.
           03  CONST-MAINTENANCE                   PIC 9(03)   VALUE 1.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
      *    CRON情報
           COPY  "CPCRON.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W96
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"         ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W96         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "WERR" )
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "W96    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC                  SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "WID1"
               PERFORM 330-WID1-SET-SEC
               MOVE    SPACE           TO  SPA-WID1-FLG
               MOVE    SPACE           TO  SPA-WIDCD
               GO  TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-W96
      *
           PERFORM 420-CLEAR-SEC
      *
           PERFORM 310-CMB-SYOKI-SEC
      *
           PERFORM 900-CRON-ALL-SEL-SEC
      *
           MOVE    ZERO            TO  IDX-JMA
           MOVE    5               TO  IDX-USR
      *
           PERFORM UNTIL ( FLG-CRON    NOT =   ZERO )
      *
               IF    ( CRON-CRONID >=  1000 )
                   COMPUTE IDX-USR =   IDX-USR     +   1
                   MOVE    IDX-USR TO  IDX-CRON
               ELSE
                   COMPUTE IDX-JMA =   IDX-JMA     +   1
                   MOVE    IDX-JMA TO  IDX-CRON
               END-IF
      *
               IF    ( CRON-RUN    =   ZERO )
                   MOVE    "F"         TO  SPA-GMN-EXEC (IDX-CRON)
               ELSE
                   MOVE    "T"         TO  SPA-GMN-EXEC (IDX-CRON)
               END-IF
               MOVE    CRON-NAME       TO  SPA-GMN-NAME (IDX-CRON)
               MOVE    CRON-CRNHOUR    TO  SPA-GMN-HOUR (IDX-CRON)
               MOVE    CRON-CRNDOM     TO  SPA-GMN-DOM  (IDX-CRON)
               MOVE    CRON-CRNMON     TO  SPA-GMN-MON  (IDX-CRON)
               MOVE    CRON-CRNDOW     TO  SPA-GMN-DOW  (IDX-CRON)
               MOVE    CRON-SCRIPT     TO  SPA-GMN-SCRIPT (IDX-CRON)
      *
               PERFORM 900-CRON-ALL-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_cron"          TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM 4102-KIHON-CHK-SEC
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    036                 TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コンボボックス初期処理
      *****************************************************************
       310-CMB-SYOKI-SEC               SECTION.
      *
      *    曜日コンボ
           PERFORM 3101-DOWLST-SET-SEC
      *
      *    月コンボ
           PERFORM 3101-MONLST-SET-SEC
      *
      *    日コンボ
           PERFORM 3101-DOMLST-SET-SEC
      *
      *    時コンボ
           PERFORM 3101-HOURLST-SET-SEC
      *
           .
       310-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    曜日コンボ初期処理
      *****************************************************************
       3101-DOWLST-SET-SEC             SECTION.
      *
           MOVE    "*"             TO  SPA-GMN-DOWL   (1)
           MOVE    " "             TO  SPA-GMN-DOWMEIL(1)
      *
           MOVE    "0"             TO  SPA-GMN-DOWL   (2)
           MOVE    "日曜日"        TO  SPA-GMN-DOWMEIL(2)
      *
           MOVE    "1"             TO  SPA-GMN-DOWL   (3)
           MOVE    "月曜日"        TO  SPA-GMN-DOWMEIL(3)
      *
           MOVE    "2"             TO  SPA-GMN-DOWL   (4)
           MOVE    "火曜日"        TO  SPA-GMN-DOWMEIL(4)
      *
           MOVE    "3"             TO  SPA-GMN-DOWL   (5)
           MOVE    "水曜日"        TO  SPA-GMN-DOWMEIL(5)
      *
           MOVE    "4"             TO  SPA-GMN-DOWL   (6)
           MOVE    "木曜日"        TO  SPA-GMN-DOWMEIL(6)
      *
           MOVE    "5"             TO  SPA-GMN-DOWL   (7)
           MOVE    "金曜日"        TO  SPA-GMN-DOWMEIL(7)
      *
           MOVE    "6"             TO  SPA-GMN-DOWL   (8)
           MOVE    "土曜日"        TO  SPA-GMN-DOWMEIL(8)
      *
           MOVE    8               TO  SPA-GMN-DOW-MAX
      *
           .
       3101-DOWLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    月コンボ初期処理
      *****************************************************************
       3101-MONLST-SET-SEC             SECTION.
      *
           MOVE    "*"             TO  SPA-GMN-MONLST (1)
      *
           PERFORM VARYING IDX-CMB FROM    2   BY  1
                   UNTIL ( IDX-CMB >   13 )
      *
               COMPUTE WRK-NUM99   =   IDX-CMB     -   1
               MOVE    WRK-NUM99-X TO  SPA-GMN-MONLST (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    13              TO  SPA-GMN-MON-MAX
      *
           .
       3101-MONLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    日コンボ初期処理
      *****************************************************************
       3101-DOMLST-SET-SEC             SECTION.
      *
           MOVE    "*"             TO  SPA-GMN-DOMLST (1)
      *
           PERFORM VARYING IDX-CMB FROM    2   BY  1
                   UNTIL ( IDX-CMB >   32 )
      *
               COMPUTE WRK-NUM99   =   IDX-CMB     -   1
               MOVE    WRK-NUM99-X TO  SPA-GMN-DOMLST (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    32              TO  SPA-GMN-DOM-MAX
      *
           .
       3101-DOMLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    時コンボ初期処理
      *****************************************************************
       3101-HOURLST-SET-SEC             SECTION.
      *
           MOVE    "*"             TO  SPA-GMN-HOURLST (1)
      *
           PERFORM VARYING IDX-CMB FROM    2   BY  1
                   UNTIL ( IDX-CMB >   25 )
      *
               COMPUTE WRK-NUM99   =   IDX-CMB -   2
               MOVE    WRK-NUM99-X TO  SPA-GMN-HOURLST (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    25              TO  SPA-GMN-HOUR-MAX
      *
           .
       3101-HOURLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF    ( SPA-WID1-FLG    =   "OK" )
               EVALUATE    SPA-WIDCD
               WHEN "3001"
                   PERFORM 4801-TOUROKU-SEC
               END-EVALUATE
           END-IF
      *
           .
       330-WID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM  210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 420-CLEAR-SEC
           WHEN    "CLICKED"       ALSO    "B08"
      *
               COMPUTE SPA-GMN-B08-PUSHED  =   SPA-GMN-B08-PUSHED  +   1
      *
           WHEN    "CLICKED"       ALSO    "B11"
      *
               PERFORM 470-MAINTENANCE-MODE-CHK-SEC
      *
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 480-TOUROKU-SEC
      *    実行ボタン
           WHEN    "CLICKED"       ALSO    "EXEC01"    THRU   "EXEC10"
               PERFORM 4101-EXEC-CHK-SEC
      *
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           MOVE    ZERO        TO      SPA-GMN-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC           SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
             OR  ( FLG-END     NOT =   ZERO  )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           PERFORM VARYING IDX-CRON    FROM    1   BY  1
                   UNTIL ( IDX-CRON    >   CONST-CRON-MAX )
      *
               IF    ( IDX-CRON        >  CONST-JMA-MAX )
                OR   ( SPA-GMN-MODE    =  CONST-MAINTENANCE )
                   MOVE    W96-NAME   (IDX-CRON)
                                       TO  SPA-GMN-NAME   (IDX-CRON)
                   MOVE    W96-SCRIPT (IDX-CRON)
                                       TO  SPA-GMN-SCRIPT (IDX-CRON)
               END-IF
      *
               MOVE    W96-DOW    (IDX-CRON)
                                       TO  SPA-GMN-DOW-G  (IDX-CRON)
               MOVE    W96-MON    (IDX-CRON)
                                       TO  SPA-GMN-MON    (IDX-CRON)
               MOVE    W96-DOM    (IDX-CRON)
                                       TO  SPA-GMN-DOM    (IDX-CRON)
               MOVE    W96-HOUR   (IDX-CRON)
                                       TO  SPA-GMN-HOUR   (IDX-CRON)
           END-PERFORM
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
           PERFORM VARYING IDX-CRON    FROM    1   BY  1
                   UNTIL ( IDX-CRON    >   CONST-CRON-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               COMPUTE WRK-CURSOR      = ( IDX-CRON    -   1 )
                                       *   7
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 4101-NAME-CHK-SEC
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 4101-SCRIPT-CHK-SEC
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 4101-MON-CHK-SEC
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 4101-DOM-CHK-SEC
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 4101-DOW-CHK-SEC
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 4101-HOUR-CHK-SEC
               END-IF
           END-PERFORM
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    実行ボタンチェック処理
      *****************************************************************
       4101-EXEC-CHK-SEC               SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
      *
           MOVE    WRK-MCP-WIDGET(5:2) TO  WRK-NUM99-X
           IF    ( SPA-GMN-NAME  (WRK-NUM99)
                                       =   SPACE )
               MOVE    "F"             TO  SPA-GMN-EXEC (WRK-NUM99)
           ELSE
               IF    ( W96-EXEC (WRK-NUM99)    =   "F" )
                   MOVE    "T"         TO  SPA-GMN-EXEC (WRK-NUM99)
               ELSE
                   MOVE    "F"         TO  SPA-GMN-EXEC (WRK-NUM99)
               END-IF
           END-IF
      *
           .
       4101-EXEC-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理名チェック処理
      *****************************************************************
       4101-NAME-CHK-SEC               SECTION.
      *
           IF    ( SPA-GMN-NAME (IDX-CRON) =   SPACE )
               INITIALIZE                  SPA-GMN-CRON-OCC (IDX-CRON)
               MOVE    "F"             TO  SPA-GMN-EXEC     (IDX-CRON)
           ELSE
               MOVE    SPA-GMN-NAME (IDX-CRON)
                                       TO  WRK-KANACHK-STR
               PERFORM 800-ZENKAKU-CHK-SEC
               MOVE    WRK-KANACHK-STR TO  SPA-GMN-NAME (IDX-CRON)
               IF    ( FLG-KANACHK-ERR NOT =   ZERO )
                   MOVE    "0001"      TO  SPA-ERRCD
                   COMPUTE SPA-GMN-CUR =   WRK-CURSOR  +   1
               END-IF
           END-IF
      *
           .
       4101-NAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    スクリプトチェック処理
      *****************************************************************
       4101-SCRIPT-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-SCRIPT (IDX-CRON)
                                   TO  WRK-KANACHK-STR
           PERFORM 800-HANKAKU-CHK-SEC
           MOVE    WRK-KANACHK-STR TO  SPA-GMN-SCRIPT (IDX-CRON)
           IF    ( FLG-KANACHK-ERR NOT =   ZERO )
               MOVE    "0002"      TO  SPA-ERRCD
               COMPUTE SPA-GMN-CUR =   WRK-CURSOR  +   2
           END-IF
      *
           .
       4101-SCRIPT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    曜日チェック処理
      *****************************************************************
       4101-DOW-CHK-SEC                SECTION.
      *
           MOVE    SPA-GMN-DOW (IDX-CRON)
                                   TO  WRK-CMB-CD
           PERFORM 4200-DOWLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-DOW-G (IDX-CRON)
      *
           .
       4101-DOW-CHK-EXT.
           EXIT.
      *****************************************************************
      *    月チェック処理
      *****************************************************************
       4101-MON-CHK-SEC                SECTION.
      *
           MOVE    SPA-GMN-MON (IDX-CRON)
                                   TO  WRK-CMB-CD
           PERFORM 4200-MONLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-MON (IDX-CRON)
      *
           .
       4101-MON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日チェック処理
      *****************************************************************
       4101-DOM-CHK-SEC                SECTION.
      *
           MOVE    SPA-GMN-DOM (IDX-CRON)
                                   TO  WRK-CMB-CD
           PERFORM 4200-DOMLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-DOM (IDX-CRON)
      *
           .
       4101-DOM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    時間チェック処理
      *****************************************************************
       4101-HOUR-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-HOUR (IDX-CRON)
                                   TO  WRK-CMB-CD
           PERFORM 4200-HOURLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-HOUR (IDX-CRON)
      *
           .
       4101-HOUR-CHK-EXT.
           EXIT.
      *****************************************************************
      *    曜日コンボチェック処理
      *****************************************************************
       4200-DOWLST-CHK-SEC             SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-DOW-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-DOWL        (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    SPA-GMN-DOWLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-DOWLST  (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-DOWLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    月コンボチェック処理
      *****************************************************************
       4200-MONLST-CHK-SEC             SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-MON-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-MONLST  (IDX-CMB)
                                           =   WRK-CMB-CD )
                   MOVE    SPA-GMN-MONLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-MONLST  (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-MONLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日コンボチェック処理
      *****************************************************************
       4200-DOMLST-CHK-SEC             SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-DOM-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-DOMLST  (IDX-CMB)
                                           =   WRK-CMB-CD )
                   MOVE    SPA-GMN-DOMLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-DOMLST  (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-DOMLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    時間コンボチェック処理
      *****************************************************************
       4200-HOURLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-HOUR-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-HOURLST  (IDX-CMB)
                                           =   WRK-CMB-CD )
                   MOVE    SPA-GMN-HOURLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-HOURLST  (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-HOURLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W96"               TO  SPA-MOTOPG
      *
      ***  MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    "W01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    条件クリア処理
      *****************************************************************
       420-CLEAR-SEC               SECTION.
      *
           PERFORM VARYING IDX-CRON    FROM    1   BY  1
                   UNTIL ( IDX-CRON    >   CONST-CRON-MAX )
      *
               IF    ( SPA-GMN-MODE    =   CONST-MAINTENANCE )
                       INITIALIZE          SPA-GMN-CRON-OCC (IDX-CRON)
               ELSE
                   IF    ( IDX-CRON    >   CONST-JMA-MAX )
                       INITIALIZE          SPA-GMN-CRON-OCC (IDX-CRON)
                   ELSE
                       INITIALIZE          SPA-GMN-SCHEDULE-G (IDX-CRON)
                   END-IF
               END-IF
      *
               MOVE    "F"             TO  SPA-GMN-EXEC (IDX-CRON)
      *
           END-PERFORM
      *
           PERFORM 4102-KIHON-CHK-SEC
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    036                 TO  SPA-GMN-CUR
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    メンテナンスモードチェック処理
      *****************************************************************
       470-MAINTENANCE-MODE-CHK-SEC    SECTION.
      *
           IF    ( SPA-GMN-B08-PUSHED  =   2 )
               MOVE    CONST-MAINTENANCE   TO  SPA-GMN-MODE
           ELSE
               MOVE    ZERO                TO  SPA-GMN-B08-PUSHED
           END-IF
      *
           .
       470-MAINTENANCE-MODE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       480-TOUROKU-SEC                 SECTION.
      *
      *    入力個所のチェック
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           PERFORM 4900-KANRENCHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           MOVE    "3001"          TO  SPA-WIDCD
      *
           .
       480-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       4801-TOUROKU-SEC                SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           INITIALIZE                  CRON-REC
           MOVE    CRON-REC        TO  MCPDATA-REC
           MOVE    "tbl_cron"      TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4001"      TO  SPA-ERRCD
           END-IF
      *
           MOVE    ZERO            TO  CNT-JMAID
           MOVE    1000            TO  CNT-USRID
           PERFORM VARYING IDX-CRON    FROM    1   BY  1
                   UNTIL ( IDX-CRON    >   CONST-CRON-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               IF    ( SPA-GMN-NAME (IDX-CRON) NOT =   SPACE )
      *
                   IF    ( IDX-CRON    >   CONST-JMA-MAX )
                       COMPUTE CNT-USRID   =   CNT-USRID   +   1
                       MOVE    CNT-USRID   TO  CNT-CRONID
                   ELSE
                       COMPUTE CNT-JMAID   =   CNT-JMAID   +   1
                       MOVE    CNT-JMAID   TO  CNT-CRONID
                   END-IF
      *
      *
                   INITIALIZE              CRON-REC
                   MOVE    CNT-CRONID  TO  CRON-CRONID
                   IF    ( SPA-GMN-EXEC (IDX-CRON)     =   "T" )
                       MOVE    "1"     TO  CRON-RUN
                   ELSE
                       MOVE    "0"     TO  CRON-RUN
                   END-IF
                   MOVE    SPA-GMN-NAME (IDX-CRON)
                                       TO  CRON-NAME
                   MOVE    "00"        TO  CRON-CRNMIN
                   MOVE    SPA-GMN-HOUR (IDX-CRON)
                                       TO  CRON-CRNHOUR
                   MOVE    SPA-GMN-DOM  (IDX-CRON)
                                       TO  CRON-CRNDOM
                   MOVE    SPA-GMN-MON  (IDX-CRON)
                                       TO  CRON-CRNMON
                   MOVE    SPA-GMN-DOW  (IDX-CRON)
                                       TO  CRON-CRNDOW
                   MOVE    SPA-GMN-SCRIPT (IDX-CRON)
                                       TO  CRON-SCRIPT
                   MOVE    SMCNDATE-YMD
                                       TO  CRON-CREYMD
                                           CRON-UPYMD
                   MOVE    SMCNDATE-HMS
                                       TO  CRON-UPHMS
                   MOVE    CRON-REC    TO  MCPDATA-REC
                   MOVE    "tbl_cron"  TO  MCP-TABLE
                   MOVE    "key"       TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       MOVE    "4002"  TO  SPA-ERRCD
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE   "JOIN"           TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
      *
           .
       4801-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    共通関連チェック
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
           PERFORM VARYING IDX-CRON    FROM    1   BY  1
                   UNTIL ( IDX-CRON    >   CONST-CRON-MAX )
               IF    ( SPA-GMN-NAME   (IDX-CRON) NOT = SPACE )
                AND  ( SPA-GMN-SCRIPT (IDX-CRON)     = SPACE )
                   MOVE    "1001"      TO      SPA-ERRCD
                   COMPUTE SPA-GMN-CUR = ( IDX-CRON    -   1 )
                                       *   7
                                       +   2
               END-IF
           END-PERFORM
      *
           .
      *
       4900-KANRENCHK-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W96    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE                      W96
      *
           MOVE    "black"             TO  W96-LBLJMASCRIPTS-STYLE
           IF    ( SPA-GMN-MODE        =   CONST-MAINTENANCE )
               MOVE    "red"           TO  W96-LBLJMASCRIPTS-STYLE
           END-IF
      *
           PERFORM VARYING IDX-CRON    FROM    1   BY  1
                   UNTIL ( IDX-CRON    >   CONST-CRON-MAX )
      *
               MOVE    WIDGET-NORMAL   TO  W96-NAME-STATE  (IDX-CRON)
                                           W96-SCRIPT-STATE(IDX-CRON)
               IF    ( SPA-GMN-MODE    =   CONST-MAINTENANCE )
                OR   ( IDX-CRON        >   CONST-JMA-MAX )
                   CONTINUE
               ELSE
                   MOVE    WIDGET-INSENSITIVE
                                       TO  W96-NAME-STATE  (IDX-CRON)
                                           W96-SCRIPT-STATE(IDX-CRON)
               END-IF
      *
               MOVE    SPA-GMN-NAME   (IDX-CRON)
                                       TO  W96-NAME   (IDX-CRON)
               MOVE    SPA-GMN-SCRIPT (IDX-CRON)
                                       TO  W96-SCRIPT (IDX-CRON)
               MOVE    SPA-GMN-DOW-G  (IDX-CRON)
                                       TO  W96-DOW    (IDX-CRON)
               MOVE    SPA-GMN-DOW-MAX TO  W96-DOW-COUNT (IDX-CRON)
               PERFORM VARYING IDX-CMB FROM    1   BY  1
                       UNTIL ( IDX-CMB >   SPA-GMN-DOW-MAX )
                   MOVE    SPA-GMN-DOWLST (IDX-CMB)
                                       TO  W96-DOW-ITEM
                                                 (IDX-CRON IDX-CMB)
               END-PERFORM
               MOVE    SPA-GMN-MON    (IDX-CRON)
                                       TO  W96-MON    (IDX-CRON)
               MOVE    SPA-GMN-MON-MAX TO  W96-MON-COUNT (IDX-CRON)
               PERFORM VARYING IDX-CMB FROM    1   BY  1
                       UNTIL ( IDX-CMB >   SPA-GMN-MON-MAX )
                   MOVE    SPA-GMN-MONLST (IDX-CMB)
                                       TO  W96-MON-ITEM
                                                 (IDX-CRON IDX-CMB)
               END-PERFORM
               MOVE    SPA-GMN-DOM    (IDX-CRON)
                                       TO  W96-DOM    (IDX-CRON)
               MOVE    SPA-GMN-DOM-MAX TO  W96-DOM-COUNT (IDX-CRON)
               PERFORM VARYING IDX-CMB FROM    1   BY  1
                       UNTIL ( IDX-CMB >   SPA-GMN-DOM-MAX )
                   MOVE    SPA-GMN-DOMLST (IDX-CMB)
                                       TO  W96-DOM-ITEM
                                                 (IDX-CRON IDX-CMB)
               END-PERFORM
               MOVE    SPA-GMN-HOUR   (IDX-CRON)
                                       TO  W96-HOUR   (IDX-CRON)
               MOVE    SPA-GMN-HOUR-MAX TO  W96-HOUR-COUNT (IDX-CRON)
               PERFORM VARYING IDX-CMB FROM    1   BY  1
                       UNTIL ( IDX-CMB >   SPA-GMN-HOUR-MAX )
                   MOVE    SPA-GMN-HOURLST (IDX-CMB)
                                       TO  W96-HOUR-ITEM
                                                 (IDX-CRON IDX-CMB)
               END-PERFORM
      *
               MOVE    SPA-GMN-EXEC (IDX-CRON)
                                       TO  W96-EXEC (IDX-CRON)
      *
      *        IF    ( W96-EXEC (IDX-CRON) =   "T" )
                   MOVE    "実行する"      TO  W96-EXEC-LBL (IDX-CRON)
      *        ELSE
      *            MOVE    "実行しない"    TO  W96-EXEC-LBL (IDX-CRON)
      *        END-IF
      *
           END-PERFORM
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-CUR         =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    001
               MOVE    "NAME01"            TO  MCP-WIDGET
           WHEN    002
               MOVE    "SCRIPT01"          TO  MCP-WIDGET
           WHEN    003
               MOVE    "MON01"             TO  MCP-WIDGET
           WHEN    004
               MOVE    "DOM01"             TO  MCP-WIDGET
           WHEN    005
               MOVE    "DOW01"             TO  MCP-WIDGET
           WHEN    006
               MOVE    "HOUR01"            TO  MCP-WIDGET
           WHEN    007
               MOVE    "EXEC01"            TO  MCP-WIDGET
      *
           WHEN    008
               MOVE    "NAME02"            TO  MCP-WIDGET
           WHEN    009
               MOVE    "SCRIPT02"          TO  MCP-WIDGET
           WHEN    010
               MOVE    "MON02"             TO  MCP-WIDGET
           WHEN    011
               MOVE    "DOM02"             TO  MCP-WIDGET
           WHEN    012
               MOVE    "DOW02"             TO  MCP-WIDGET
           WHEN    013
               MOVE    "HOUR02"            TO  MCP-WIDGET
           WHEN    014
               MOVE    "EXEC02"            TO  MCP-WIDGET
      *
           WHEN    015
               MOVE    "NAME03"            TO  MCP-WIDGET
           WHEN    016
               MOVE    "SCRIPT03"          TO  MCP-WIDGET
           WHEN    017
               MOVE    "MON03"             TO  MCP-WIDGET
           WHEN    018
               MOVE    "DOM03"             TO  MCP-WIDGET
           WHEN    019
               MOVE    "DOW03"             TO  MCP-WIDGET
           WHEN    020
               MOVE    "HOUR03"            TO  MCP-WIDGET
           WHEN    021
               MOVE    "EXEC03"            TO  MCP-WIDGET
      *
           WHEN    022
               MOVE    "NAME04"            TO  MCP-WIDGET
           WHEN    023
               MOVE    "SCRIPT04"          TO  MCP-WIDGET
           WHEN    024
               MOVE    "MON04"             TO  MCP-WIDGET
           WHEN    025
               MOVE    "DOM04"             TO  MCP-WIDGET
           WHEN    026
               MOVE    "DOW04"             TO  MCP-WIDGET
           WHEN    027
               MOVE    "HOUR04"            TO  MCP-WIDGET
           WHEN    028
               MOVE    "EXEC04"            TO  MCP-WIDGET
      *
           WHEN    029
               MOVE    "NAME05"            TO  MCP-WIDGET
           WHEN    030
               MOVE    "SCRIPT05"          TO  MCP-WIDGET
           WHEN    031
               MOVE    "MON05"             TO  MCP-WIDGET
           WHEN    032
               MOVE    "DOM05"             TO  MCP-WIDGET
           WHEN    033
               MOVE    "DOW05"             TO  MCP-WIDGET
           WHEN    034
               MOVE    "HOUR05"            TO  MCP-WIDGET
           WHEN    035
               MOVE    "EXEC05"            TO  MCP-WIDGET
      *
           WHEN    036
               MOVE    "NAME06"            TO  MCP-WIDGET
           WHEN    037
               MOVE    "SCRIPT06"          TO  MCP-WIDGET
           WHEN    038
               MOVE    "MON06"             TO  MCP-WIDGET
           WHEN    039
               MOVE    "DOM06"             TO  MCP-WIDGET
           WHEN    040
               MOVE    "DOW06"             TO  MCP-WIDGET
           WHEN    041
               MOVE    "HOUR06"            TO  MCP-WIDGET
           WHEN    042
               MOVE    "EXEC06"            TO  MCP-WIDGET
      *
           WHEN    043
               MOVE    "NAME07"            TO  MCP-WIDGET
           WHEN    044
               MOVE    "SCRIPT07"          TO  MCP-WIDGET
           WHEN    045
               MOVE    "MON07"             TO  MCP-WIDGET
           WHEN    046
               MOVE    "DOM07"             TO  MCP-WIDGET
           WHEN    047
               MOVE    "DOW07"             TO  MCP-WIDGET
           WHEN    048
               MOVE    "HOUR07"            TO  MCP-WIDGET
           WHEN    049
               MOVE    "EXEC07"            TO  MCP-WIDGET
      *
           WHEN    050
               MOVE    "NAME08"            TO  MCP-WIDGET
           WHEN    051
               MOVE    "SCRIPT08"          TO  MCP-WIDGET
           WHEN    052
               MOVE    "MON08"             TO  MCP-WIDGET
           WHEN    053
               MOVE    "DOM08"             TO  MCP-WIDGET
           WHEN    054
               MOVE    "DOW08"             TO  MCP-WIDGET
           WHEN    055
               MOVE    "HOUR08"            TO  MCP-WIDGET
           WHEN    056
               MOVE    "EXEC08"            TO  MCP-WIDGET
      *
           WHEN    057
               MOVE    "NAME09"            TO  MCP-WIDGET
           WHEN    058
               MOVE    "SCRIPT09"          TO  MCP-WIDGET
           WHEN    059
               MOVE    "MON09"             TO  MCP-WIDGET
           WHEN    060
               MOVE    "DOM09"             TO  MCP-WIDGET
           WHEN    061
               MOVE    "DOW09"             TO  MCP-WIDGET
           WHEN    062
               MOVE    "HOUR09"            TO  MCP-WIDGET
           WHEN    063
               MOVE    "EXEC09"            TO  MCP-WIDGET
      *
           WHEN    064
               MOVE    "NAME10"            TO  MCP-WIDGET
           WHEN    065
               MOVE    "SCRIPT10"          TO  MCP-WIDGET
           WHEN    066
               MOVE    "MON10"             TO  MCP-WIDGET
           WHEN    067
               MOVE    "DOM10"             TO  MCP-WIDGET
           WHEN    068
               MOVE    "DOW10"             TO  MCP-WIDGET
           WHEN    069
               MOVE    "HOUR10"            TO  MCP-WIDGET
           WHEN    070
               MOVE    "EXEC10"            TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "NAME01"    THRU    "NAME10"
               MOVE    WRK-MCP-WIDGET (5:2)    TO  WRK-NUM99-X
               COMPUTE SPA-GMN-CUR = ( WRK-NUM99 - 1 ) *   7
                                   +   2
           WHEN    "SCRIPT01"  THRU    "SCRIPT10"
               MOVE    WRK-MCP-WIDGET (7:2)    TO  WRK-NUM99-X
               COMPUTE SPA-GMN-CUR = ( WRK-NUM99 - 1 ) *   7
                                   +   3
           WHEN    "MON01"     THRU    "MON10"
               MOVE    WRK-MCP-WIDGET (4:2)    TO  WRK-NUM99-X
               COMPUTE SPA-GMN-CUR = ( WRK-NUM99 - 1 ) *   7
                                   +   4
           WHEN    "DOM01"     THRU    "DOM10"
               MOVE    WRK-MCP-WIDGET (4:2)    TO  WRK-NUM99-X
               COMPUTE SPA-GMN-CUR = ( WRK-NUM99 - 1 ) *   7
                                   +   5
           WHEN    "DOW01"     THRU    "DOW10"
               MOVE    WRK-MCP-WIDGET (4:2)    TO  WRK-NUM99-X
               COMPUTE SPA-GMN-CUR = ( WRK-NUM99 - 1 ) *   7
                                   +   6
           WHEN    "HOUR01"    THRU    "HOUR10"
               MOVE    WRK-MCP-WIDGET (5:2)    TO  WRK-NUM99-X
               COMPUTE SPA-GMN-CUR = ( WRK-NUM99 - 1 ) *   7
                                   +   7
           WHEN    "EXEC01"    THRU    "EXEC09"
               MOVE    WRK-MCP-WIDGET (5:2)    TO  WRK-NUM99-X
               IF    ( SPA-GMN-MODE    =   CONST-MAINTENANCE )
                OR   ( WRK-NUM99       >   CONST-JMA-MAX )
                   COMPUTE SPA-GMN-CUR = ( WRK-NUM99 - 1 ) *   7
                                   +   8
               ELSE
                   COMPUTE SPA-GMN-CUR = ( WRK-NUM99 - 1 ) *   7
                                   +   10
               END-IF
           WHEN    "EXEC10"
               COMPUTE SPA-GMN-CUR =   212
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "処理名はすべて全角で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "実行スクリプトはすべて半角で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "実行スクリプトが未入力です"
                                       TO  SPA-ERRMSG
           WHEN    "4001"  THRU    "4999"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W96"               TO  SPA-MOTOPG
           MOVE    "WERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-WIDCD
           WHEN    "3001"
               MOVE    "登録します。よろしいですか"
                                        TO  WRK-WIDMSG
           END-EVALUATE
      *
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W96"                TO  SPA-MOTOPG
           MOVE    "WID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    全角文字チェック
      *****************************************************************
       800-ZENKAKU-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-KANACHK-ERR
      *
           IF  ( WRK-KANACHK-STR       =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    WRK-KANACHK-STR TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-KANACHK-STR
               IF      ( KANACHK-RC    =   ZERO )
                   IF      ( KANACHK-SYUBETU   NOT =   2 )
                       MOVE    1       TO  FLG-KANACHK-ERR
                   END-IF
               ELSE
                       MOVE    1       TO  FLG-KANACHK-ERR
               END-IF
           END-IF
      *
           .
       800-ZENKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    半角文字チェック
      *****************************************************************
       800-HANKAKU-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-KANACHK-ERR
      *
           IF  ( WRK-KANACHK-STR       =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    WRK-KANACHK-STR TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-KANACHK-STR
               IF      ( KANACHK-RC    =   ZERO )
                   IF      ( KANACHK-SYUBETU   NOT =   1 )
                       MOVE    1       TO  FLG-KANACHK-ERR
                   END-IF
               ELSE
                       MOVE    1       TO  FLG-KANACHK-ERR
               END-IF
           END-IF
      *
           .
       800-HANKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    CRON情報検索処理
      *****************************************************************
       900-CRON-ALL-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-CRON
      *
           MOVE    CRON-REC        TO  MCPDATA-REC
           MOVE    "tbl_cron"      TO  MCP-TABLE
           MOVE    "all"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CRON-REC
           ELSE
               MOVE    1           TO  FLG-CRON
               INITIALIZE              CRON-REC
           END-IF
      *
           .
       900-CRON-ALL-SEL-EXT.
            EXIT.
      *****************************************************************
      *    CRON情報FETCH処理
      *****************************************************************
       900-CRON-ALL-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-CRON
      *
           MOVE    "tbl_cron"      TO  MCP-TABLE
           MOVE    "all"           TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CRON-REC
           ELSE
               MOVE    1           TO  FLG-CRON
               INITIALIZE              CRON-REC
           END-IF
      *
           .
       900-CRON-ALL-FET-EXT.
            EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

