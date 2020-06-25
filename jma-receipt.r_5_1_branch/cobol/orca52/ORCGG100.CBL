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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCGG100.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次統計
      *  コンポーネント名  : 並び順変更
      *  管理者            : 
      *  作成日付    作業者        記述
      *  12/08/02    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    日次統計共通パラメタ
           COPY    "G01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "G01SCR-SPA".
      *
      *    コンスタント値
           COPY    "ENUM-VALUE".
      *
       01  SPA-G100-AREA.
           03  SPA-GMN-CUR                     PIC 9(03).
      *
           03  SPA-GMN-AREA.
               05  SPA-GMN-SELNUM              PIC 9(03).
      *
           03  SPA-GMN-TYOHYOLST-AREA.
               05  SPA-GMN-TYOHYOLST-MAX       PIC 9(05).
               05  SPA-GMN-TYOHYOLST-SEL       PIC 9(05).
               05  SPA-GMN-TYOHYOLST-OCC       OCCURS  100.
                   07  SPA-GMN-TNUM            PIC 9(03).
                   07  SPA-GMN-THCNAME         PIC X(80).
                   07  SPA-GMN-TPRGNAME        PIC X(20).
           03  SPA-NAI-AREA.
               05  SPA-NAI-KANRICD             PIC X(04).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-TOUKEIMEMO              PIC 9(01).
           03  FLG-TMP                     PIC 9(01).
           03  FLG-TYOHYOLST-ROW           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDXL                        PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-GIDMSG                  PIC X(70).
           03  WRK-ZZ9                     PIC ZZ9.
           03  WRK-KANRICD                 PIC X(04).
      *    入力項目名
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-PGSRH-G.
               05  WRK-PGSRH-MAX           PIC 9(05).
           03  WRK-KBNCD-G.
               05  WRK-KBNCD               PIC 9(03).
           03  WRK-BF-KBNCD                PIC X(08).
           03  WRK-AF-KBNCD                PIC X(08).
           03  WRK-KBNCD-S9                PIC S9(03).
           03  WRK-STKBNCD                 PIC X(08).
           03  WRK-EDKBNCD                 PIC X(08).
           03  WRK-TMPKBNCD-G.
               05  WRK-TMPKBNCD-LBL        PIC X(03).
               05  WRK-TMPKBNCD            PIC 9(03).
           03  WRK-SELNUM                  PIC 9(03).
      *
       01  WRK-ROW.
           05  WRK-TYOHYOLST-ROW           PIC S9(9)   BINARY.
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-TYOHYOLST-MAX         PIC 9(05)   VALUE   100.
      *
       01  TMEMO-REC.
           COPY  "CPTOUKEIMEMO.INC"        REPLACING    //TKIMEMO-//
                                           BY           //TMEMO-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK0043.INC".
           COPY    "CPSK0044.INC".
           COPY    "CPSK3001.INC".
           COPY    "CPSK3002.INC".
           COPY    "CPSK3102.INC".
      *
      *    統計メモ
       01  TOUKEIMEMO-REC.
           COPY  "CPTOUKEIMEMO.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    ユーザー情報取得
           COPY    "CPORCSLDUSER.INC".
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
           COPY    "ORCA51SCRAREA.INC".
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
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-G01FREE
           MOVE    SPA-G01-OTHERS  TO  SPA-G100-AREA
           MOVE    SPA-WORK        TO  SPA-G01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
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
           MOVE    SPA-G01KYOUTU   TO  SPA-WORK
           MOVE    SPA-G100-AREA   TO  SPA-G01-OTHERS
           MOVE    SPA-G01FREE     TO  SPA-FREE
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
           IF    ( SPA-MOTOPG          =   "GERR" )
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
           MOVE    "G100"              TO  MCP-WINDOW.
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
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "GID1"
               PERFORM 330-GID1-SET-SEC
           WHEN    "G07"
               IF    ( SPA-NAI-G07-RET =   1 )
                   PERFORM 311-SPASET-SEC
               END-IF
           WHEN    OTHER
               PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                      SPA-G100-AREA
      *
           MOVE    "3002"              TO  SPA-NAI-KANRICD
      *
           MOVE    1                   TO  FLG-TYOHYOLST-ROW
      *
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   CONST-TYOHYOLST-MAX )
      *
               MOVE    IDXL            TO  SPA-GMN-TNUM (IDXL)
      *
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    SPA-NAI-KANRICD TO  SYS-KANRICD
               MOVE    IDXL            TO  WRK-KBNCD
               MOVE    WRK-KBNCD-G     TO  SYS-KBNCD
               MOVE    SPA-SYSYMD      TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-3002-REC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    SYS-3002-HCNAME
                                       TO  SPA-GMN-THCNAME (IDXL)
                   MOVE    SYS-3002-PRGNAME
                                       TO  SPA-GMN-TPRGNAME(IDXL)
               END-IF
      *
           END-PERFORM
      *
           MOVE    CONST-TYOHYOLST-MAX
                                       TO  SPA-GMN-TYOHYOLST-MAX
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       311-SPASET-SEC              SECTION.
      *
           MOVE    SPA-GMN-TYOHYOLST-SEL
                                   TO  WRK-SELNUM
           PERFORM 310-SPASET-SEC
           MOVE    2               TO  FLG-TYOHYOLST-ROW
           MOVE    WRK-SELNUM      TO  G100-LIST-ROW
      *
           .
       311-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＧＩＤ１）ＯＫ処理
      *****************************************************************
       330-GID1-SET-SEC            SECTION.
      *
           IF    ( SPA-GID1-FLG    =   "OK" )
               EVALUATE    SPA-GIDCD
               WHEN    "1001"
                   PERFORM 800-LDUSER-CHK-SEC
                   IF    ( SLDUSER-OTHERS  =   ZERO )
                       PERFORM 2301-DEL-SEC
                       IF    ( SPA-ERRCD   =   SPACE )
                           PERFORM 311-SPASET-SEC
                       END-IF
                   ELSE
                       MOVE    "0104"      TO  SPA-ERRCD
                   END-IF
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE               TO  SPA-G99-FLG
           MOVE    SPACE               TO  SPA-GID1-FLG
           MOVE    SPACE               TO  SPA-GIDCD
      *
           .
       330-GID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           MOVE    MCP-WIDGET              TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               PERFORM  210-BACK
      *    削除
           WHEN    "CLICKED"       ALSO    "B03"
               PERFORM 230-DEL-SEC
           WHEN    "CLICKED"       ALSO    "B05"
           WHEN    "CLICKED"       ALSO    "B05R"
      *
           WHEN    "CLICKED"       ALSO    "B06"
           WHEN    "CLICKED"       ALSO    "B06R"
      *
           WHEN    "CLICKED"       ALSO    "B07"
           WHEN    "CLICKED"       ALSO    "B07R"
      *
           WHEN    "CLICKED"       ALSO    "B08"
           WHEN    "CLICKED"       ALSO    "B08R"
      *    並べ替え
               PERFORM  480-TOUROKU-SEC
      *    追加
           WHEN    "CLICKED"       ALSO    "B09"
               PERFORM  480-G07-SENI-SEC
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
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
           WHEN    "ACTIVATE"      ALSO    "SELNUM"
               PERFORM 4100-SELNUM-CHK-SEC
           WHEN    ANY             ALSO    "LIST"
               PERFORM 4100-TYOHYOLST-CHK-SEC
      *
      *    WHEN    "XXXX"
      *        入力個所のセット
      *        PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
      *        PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC           SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
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
           PERFORM 4101-SELNUM-CHK-SEC
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    G100-SELNUM         TO  SPA-GMN-SELNUM
      *
           PERFORM 4101-SELNUM-CHK-SEC
      *
           .
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    帳票リストチェック処理
      *****************************************************************
       4100-TYOHYOLST-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-SELNUM
                                       SPA-GMN-TYOHYOLST-SEL
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-TYOHYOLST-MAX )
      *
               IF    ( G100-LIST-SELECT (IDX0) =   "T" )
                   MOVE    SPA-GMN-TNUM (IDX0)
                                   TO  SPA-GMN-SELNUM
                   MOVE    IDX0    TO  SPA-GMN-TYOHYOLST-SEL
                   MOVE    1       TO  SPA-GMN-CUR
      *
                   MOVE    SPA-GMN-TYOHYOLST-MAX
                                   TO  IDX0
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4100-TYOHYOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4101-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-CUR
           MOVE    ZERO                TO  SPA-GMN-TYOHYOLST-SEL
      *
           MOVE    ZERO                TO  IDXL
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-TYOHYOLST-MAX )
                    OR   ( IDXL    >   ZERO )
      *
               IF    ( SPA-GMN-SELNUM  =   SPA-GMN-TNUM (IDX0))
                   MOVE    IDX0        TO  IDXL
               END-IF
      *
           END-PERFORM
      *
           IF    ( IDXL                >   ZERO )
               MOVE    IDXL            TO  SPA-GMN-TYOHYOLST-SEL
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
      *
      *
           .
       4101-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "G01"               TO  SPA-SAKIPG
           MOVE    "G100"              TO  SPA-MOTOPG
      *
           MOVE   "JOIN  "             TO  MCP-PUTTYPE
           MOVE    "G01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    統計帳票一覧画面画面遷移処理
      *****************************************************************
       480-G07-SENI-SEC                SECTION.
      *
           PERFORM 800-LDUSER-CHK-SEC
           IF    ( SLDUSER-OTHERS  =   ZERO )
               PERFORM 4801-G07-SENI-SEC
           ELSE
               MOVE    "0104"      TO  SPA-ERRCD
           END-IF
      *
           .
       480-G07-SENI-EXT.
           EXIT.
      *****************************************************************
      *    統計帳票一覧画面画面遷移処理
      *****************************************************************
       4801-G07-SENI-SEC                SECTION.
      *
           MOVE   "G100"           TO  SPA-G01-MOTOPG
      *
           COMPUTE SPA-G01-SEL     =   SPA-GMN-TYOHYOLST-SEL
      *
           MOVE    SPA-G01KYOUTU   TO  SPA-WORK
           MOVE    SPA-G100-AREA   TO  SPA-G01-OTHERS
           MOVE    SPA-G01FREE     TO  SPA-FREE
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGG07"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-G01FREE
           MOVE    SPA-G01-OTHERS  TO  SPA-G100-AREA
           MOVE    SPA-WORK        TO  SPA-G01KYOUTU
      *
           MOVE    "G100"              TO  SPA-MOTOPG
           MOVE    "G07"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "G07"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4801-G07-SENI-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       230-DEL-SEC                     SECTION.
      *
           PERFORM 800-LDUSER-CHK-SEC
           IF    ( SLDUSER-OTHERS  =   ZERO )
               MOVE    "1001"      TO  SPA-GIDCD
           ELSE
               MOVE    "0104"      TO  SPA-ERRCD
           END-IF
      *
           .
       230-DEL-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       2301-DEL-SEC                     SECTION.
      *
           INITIALIZE                SYSKANRI-REC
           MOVE    SPA-HOSPNUM   TO  SYS-HOSPNUM
           MOVE    "3002"        TO  SYS-KANRICD
           MOVE    SPA-GMN-TYOHYOLST-SEL
                                 TO  WRK-KBNCD
           MOVE    WRK-KBNCD-G   TO  SYS-KBNCD
           MOVE    SPA-SYSYMD    TO  SYS-STYUKYMD
                                     SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI  = ZERO )
      *
               MOVE    SYSKANRI-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "4007"      TO  SPA-ERRCD
               END-IF
      *
               IF    ( SPA-ERRCD     =   SPACE )
                   INITIALIZE                  SYS-3102-REC
                   MOVE    SPA-HOSPNUM     TO  SYS-3102-HOSPNUM
                   MOVE    "3102"          TO  SYS-3102-KANRICD
                   MOVE    SYS-KBNCD       TO  SYS-3102-KBNCD
                   MOVE    SYS-STYUKYMD    TO  SYS-3102-STYUKYMD
                   MOVE    SYS-EDYUKYMD    TO  SYS-3102-EDYUKYMD
                   MOVE    SYS-3102-REC    TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBDELETE-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       MOVE    "4006"      TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD     =   SPACE )
                   INITIALIZE                  TOUKEIMEMO-REC
                   MOVE    SPA-HOSPNUM     TO  TKIMEMO-HOSPNUM
                   MOVE    SYS-KANRICD     TO  TKIMEMO-KANRICD
                   MOVE    SYS-KBNCD       TO  TKIMEMO-KBNCD
                   MOVE    SPA-SYSYMD      TO  TKIMEMO-STYUKYMD
                                               TKIMEMO-EDYUKYMD
                   MOVE    TOUKEIMEMO-REC  TO  MCPDATA-REC
                   MOVE    "tbl_toukeimemo"    TO  MCP-TABLE
                   MOVE    "del2"              TO  MCP-PATHNAME
                   PERFORM 910-DBDELETE-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       MOVE    "4008"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       2301-DEL-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       480-TOUROKU-SEC                 SECTION.
      *
           PERFORM 800-LDUSER-CHK-SEC
           IF    ( SLDUSER-OTHERS  =   ZERO )
               PERFORM 4801-TOUROKU-SEC
           ELSE
               MOVE    "0104"      TO  SPA-ERRCD
           END-IF
      *
           .
       480-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       4801-TOUROKU-SEC                SECTION.
      *
           MOVE    SPA-GMN-TYOHYOLST-SEL   TO  WRK-KBNCD
           MOVE    WRK-KBNCD-G             TO  WRK-BF-KBNCD
      *
           MOVE    SPA-GMN-TYOHYOLST-SEL   TO  WRK-KBNCD-S9
           EVALUATE    MCP-WIDGET
           WHEN    "B05"
           WHEN    "B05R"
               COMPUTE WRK-KBNCD-S9 = WRK-KBNCD-S9 - 10
           WHEN    "B06"
           WHEN    "B06R"
               COMPUTE WRK-KBNCD-S9 = WRK-KBNCD-S9 - 1
           WHEN    "B07"
           WHEN    "B07R"
               COMPUTE WRK-KBNCD-S9 = WRK-KBNCD-S9 + 1
           WHEN    "B08"
           WHEN    "B08R"
               COMPUTE WRK-KBNCD-S9 = WRK-KBNCD-S9 + 10
           END-EVALUATE
      *
           IF    ( WRK-KBNCD-S9     < 1 )
               COMPUTE WRK-KBNCD-S9 = 1
           END-IF
      *
           IF    ( WRK-KBNCD-S9     > 100 )
               COMPUTE WRK-KBNCD-S9 = 100
           END-IF
      *
           MOVE    WRK-KBNCD-S9    TO  WRK-KBNCD
           MOVE    WRK-KBNCD-G     TO  WRK-AF-KBNCD
      *
           IF    ( WRK-BF-KBNCD    =   WRK-AF-KBNCD )
               CONTINUE
           ELSE
               MOVE    SPA-NAI-KANRICD
                                   TO  WRK-KANRICD
               PERFORM 4801-SYSKANRI-TOUROKU-SEC
               IF    ( SPA-ERRCD           =   SPACE )
                   MOVE    "3102"  TO  WRK-KANRICD
                   PERFORM 4801-SYSKANRI-TOUROKU-SEC
               END-IF
               IF    ( SPA-ERRCD           =   SPACE )
                   PERFORM 4801-TOUKEIMEMO-TOUROKU-SEC
               END-IF
               IF    ( SPA-ERRCD   =   SPACE )
                   PERFORM 310-SPASET-SEC
                   MOVE    WRK-KBNCD-S9
                                   TO  SPA-GMN-TYOHYOLST-SEL
                                       SPA-GMN-SELNUM
                                       G100-LIST-ROW
                   MOVE    2       TO  FLG-TYOHYOLST-ROW
               END-IF
           END-IF
      *
           .
       4801-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    システム管理登録処理
      *****************************************************************
       4801-SYSKANRI-TOUROKU-SEC       SECTION.
      *
           PERFORM 48011-SYSKANRI-DEL-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    1                   TO  FLG-TMP
               IF    ( WRK-BF-KBNCD     <  WRK-AF-KBNCD )
                   MOVE    WRK-BF-KBNCD    TO  WRK-STKBNCD
                   MOVE    WRK-AF-KBNCD    TO  WRK-EDKBNCD
               ELSE
                   MOVE    WRK-AF-KBNCD    TO  WRK-STKBNCD
                   MOVE    WRK-BF-KBNCD    TO  WRK-EDKBNCD
               END-IF
               PERFORM 48011-SYSKANRI-TOUROKU-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    ZERO            TO  FLG-TMP
               MOVE    "TMP001"        TO  WRK-STKBNCD
               MOVE    "TMP999"        TO  WRK-EDKBNCD
               PERFORM 48011-SYSKANRI-TOUROKU-SEC
           END-IF
      *
           .
       4801-SYSKANRI-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    システム管理削除処理
      *****************************************************************
       48011-SYSKANRI-DEL-SEC        SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    WRK-KANRICD     TO  SYS-KANRICD
           MOVE    "TMP001"        TO  SYS-KBNCD
           MOVE    "TMP999"        TO  SYS-UPYMD
           MOVE    "99999999"      TO  SYS-STYUKYMD
           MOVE    "00000000"      TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "del2"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4001"      TO  SPA-ERRCD
           END-IF
      *
           .
       48011-SYSKANRI-DEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理登録処理
      *****************************************************************
       48011-SYSKANRI-TOUROKU-SEC           SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    WRK-KANRICD         TO  SYS-KANRICD
           MOVE    WRK-STKBNCD         TO  SYS-KBNCD
           MOVE    WRK-EDKBNCD         TO  SYSUP-KBNCD
           MOVE    "99999999"          TO  SYS-STYUKYMD
           MOVE    "00000000"          TO  SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY25-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI  NOT =   ZERO )
                    OR   ( SPA-ERRCD       NOT =   SPACE )
      *
               MOVE    SYSKANRI-REC      TO  MCPDATA-REC
               MOVE    "tbl_syskanri"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "4002"          TO  SPA-ERRCD
               ELSE
                   IF    ( FLG-TMP         =   1 )
                       MOVE    "TMP"       TO  WRK-TMPKBNCD-LBL
                       MOVE    SYS-KBNCD   TO  WRK-KBNCD-G
                       IF    ( WRK-KBNCD-G =   WRK-BF-KBNCD )
                           MOVE    WRK-AF-KBNCD    TO  WRK-KBNCD-G
                           MOVE    WRK-KBNCD-G     TO  WRK-TMPKBNCD
                       ELSE
                           IF    ( WRK-BF-KBNCD    <  WRK-AF-KBNCD )
                               COMPUTE WRK-TMPKBNCD = WRK-KBNCD    -   1
                           ELSE
                               COMPUTE WRK-TMPKBNCD = WRK-KBNCD    +   1
                           END-IF
                       END-IF
                       MOVE    WRK-TMPKBNCD-G  TO  SYS-KBNCD
                   ELSE
                       MOVE    SYS-KBNCD       TO  WRK-TMPKBNCD-G
                       MOVE    WRK-TMPKBNCD    TO  WRK-KBNCD
                       MOVE    WRK-KBNCD-G     TO  SYS-KBNCD
                   END-IF
      *
                   MOVE    SYSKANRI-REC      TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"    TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       MOVE    "4003"          TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY25-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"    TO  MCP-TABLE
           MOVE    "key25"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       48011-SYSKANRI-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    統計メモ登録処理
      *****************************************************************
       4801-TOUKEIMEMO-TOUROKU-SEC         SECTION.
      *
           PERFORM 48011-TOUKEIMEMO-DEL-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    1                   TO  FLG-TMP
               IF    ( WRK-BF-KBNCD     <  WRK-AF-KBNCD )
                   MOVE    WRK-BF-KBNCD    TO  WRK-STKBNCD
                   MOVE    WRK-AF-KBNCD    TO  WRK-EDKBNCD
               ELSE
                   MOVE    WRK-AF-KBNCD    TO  WRK-STKBNCD
                   MOVE    WRK-BF-KBNCD    TO  WRK-EDKBNCD
               END-IF
               PERFORM 48011-TOUKEIMEMO-TOUROKU-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    ZERO            TO  FLG-TMP
               MOVE    "TMP001"        TO  WRK-STKBNCD
               MOVE    "TMP999"        TO  WRK-EDKBNCD
               PERFORM 48011-TOUKEIMEMO-TOUROKU-SEC
           END-IF
      *
           .
       4801-TOUKEIMEMO-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    統計メモ削除処理
      *****************************************************************
       48011-TOUKEIMEMO-DEL-SEC        SECTION.
      *
           INITIALIZE                  TOUKEIMEMO-REC
           MOVE    SPA-HOSPNUM     TO  TKIMEMO-HOSPNUM
           MOVE    SPA-NAI-KANRICD TO  TKIMEMO-KANRICD
           MOVE    "TMP001"        TO  TKIMEMO-KBNCD
           MOVE    "TMP999"        TO  TKIMEMO-UPYMD
           MOVE    "99999999"      TO  TKIMEMO-STYUKYMD
           MOVE    "00000000"      TO  TKIMEMO-EDYUKYMD
           MOVE    TOUKEIMEMO-REC  TO  MCPDATA-REC
           MOVE    "tbl_toukeimemo"
                                   TO  MCP-TABLE
           MOVE    "del3"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4004"      TO  SPA-ERRCD
           END-IF
      *
           .
       48011-TOUKEIMEMO-DEL-EXT.
           EXIT.
      *****************************************************************
      *    統計メモ登録処理
      *****************************************************************
       48011-TOUKEIMEMO-TOUROKU-SEC    SECTION.
      *
           INITIALIZE                      TOUKEIMEMO-REC
           MOVE    SPA-HOSPNUM         TO  TKIMEMO-HOSPNUM
           MOVE    SPA-NAI-KANRICD     TO  TKIMEMO-KANRICD
           MOVE    WRK-STKBNCD         TO  TKIMEMO-KBNCD
           MOVE    WRK-EDKBNCD         TO  TKIMEMO-UPYMD
           MOVE    "99999999"          TO  TKIMEMO-STYUKYMD
           MOVE    "00000000"          TO  TKIMEMO-EDYUKYMD
           PERFORM 900-TOUKEIMEMO-KEY3-SEL-SEC
      *
           PERFORM UNTIL ( FLG-TOUKEIMEMO  NOT =   ZERO )
                    OR   ( SPA-ERRCD       NOT =   SPACE )
      *
               MOVE    TOUKEIMEMO-REC      TO  MCPDATA-REC
               MOVE    "tbl_toukeimemo"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "4005"          TO  SPA-ERRCD
               ELSE
                   IF    ( FLG-TMP         =   1 )
                       MOVE    "TMP"       TO  WRK-TMPKBNCD-LBL
                       IF    ( TKIMEMO-KBNCD   =   WRK-BF-KBNCD )
                           MOVE    WRK-AF-KBNCD    TO  WRK-KBNCD-G
                           MOVE    WRK-KBNCD-G     TO  WRK-TMPKBNCD
                       ELSE
                           MOVE    TKIMEMO-KBNCD
                                           TO  WRK-KBNCD
                           IF    ( WRK-BF-KBNCD    <  WRK-AF-KBNCD )
                               COMPUTE WRK-TMPKBNCD = WRK-KBNCD    -   1
                           ELSE
                               COMPUTE WRK-TMPKBNCD = WRK-KBNCD    +   1
                           END-IF
                       END-IF
                       MOVE    WRK-TMPKBNCD-G  TO  TKIMEMO-KBNCD
                   ELSE
                       MOVE    TKIMEMO-KBNCD   TO  WRK-TMPKBNCD-G
                       MOVE    WRK-TMPKBNCD    TO  WRK-KBNCD
                       MOVE    WRK-KBNCD-G     TO  TKIMEMO-KBNCD
                   END-IF
      *
                   MOVE    TOUKEIMEMO-REC      TO  MCPDATA-REC
                   MOVE    "tbl_toukeimemo"    TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       MOVE    "4006"          TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               PERFORM 900-TOUKEIMEMO-KEY3-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_toukeimemo"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       48011-TOUKEIMEMO-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    共通関連チェック
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
           PERFORM 4101-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4900-KANRENCHK-EXT
           END-IF
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
           IF    ( SPA-GIDCD       NOT =   SPACE )
               PERFORM 520-GIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "G100    "          TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    G100-LIST-ROW       TO  WRK-TYOHYOLST-ROW
      *
           INITIALIZE                      G100
      *
           EVALUATE    FLG-TYOHYOLST-ROW
           WHEN    1
               MOVE    ZERO            TO  G100-LIST-ROW
               MOVE    ZERO            TO  G100-LIST-ROWATTR
           WHEN    2
               MOVE    WRK-TYOHYOLST-ROW
                                       TO  G100-LIST-ROW
               MOVE    2               TO  G100-LIST-ROWATTR
           WHEN    OTHER
               MOVE    WRK-TYOHYOLST-ROW
                                       TO  G100-LIST-ROW
               MOVE    ZERO            TO  G100-LIST-ROWATTR
           END-EVALUATE
      *
           MOVE    SPA-GMN-SELNUM TO  G100-SELNUM
      *
           MOVE    SPA-GMN-TYOHYOLST-MAX
                                       TO  G100-LIST-COUNT
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-TYOHYOLST-MAX )
      *
               MOVE    SPA-GMN-TNUM(IDX0)
                                       TO  WRK-ZZ9
               MOVE    WRK-ZZ9         TO  G100-LIST-TNUM (IDX0)
      *
               MOVE    SPA-GMN-THCNAME (IDX0)
                                       TO  G100-LIST-TPGNAME (IDX0)
               MOVE    SPA-GMN-TPRGNAME (IDX0)
                                       TO  G100-LIST-TPGID (IDX0)
      *
               IF    ( IDX0        =   SPA-GMN-TYOHYOLST-SEL )
                   MOVE    "T"         TO  G100-LIST-SELECT (IDX0)
               ELSE
                   MOVE    "F"         TO  G100-LIST-SELECT (IDX0)
               END-IF
      *
           END-PERFORM
      *
      *    項目状態編集
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
            MOVE    WIDGET-INSENSITIVE     TO  G100-B03-STATE
                                               G100-B05-STATE
                                               G100-B06-STATE
                                               G100-B07-STATE
                                               G100-B08-STATE
                                               G100-B09-STATE
                                               G100-B05R-STATE
                                               G100-B06R-STATE
                                               G100-B07R-STATE
                                               G100-B08R-STATE
      *
            IF    ( SPA-GMN-TYOHYOLST-SEL      >   ZERO )
               IF    ( SPA-GMN-THCNAME (SPA-GMN-TYOHYOLST-SEL) 
                                           NOT =   SPACE )
                    MOVE    WIDGET-NORMAL  TO  G100-B03-STATE
                                               G100-B05-STATE
                                               G100-B06-STATE
                                               G100-B07-STATE
                                               G100-B08-STATE
                                               G100-B05R-STATE
                                               G100-B06R-STATE
                                               G100-B07R-STATE
                                               G100-B08R-STATE
               ELSE
                    MOVE    WIDGET-NORMAL  TO  G100-B09-STATE
               END-IF
            END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
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
               MOVE    "SELNUM"        TO  MCP-WIDGET
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
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SELNUM"
               MOVE    001             TO  SPA-GMN-CUR
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
               MOVE    "選択番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0104"
               STRING  "他端末で月次統計業務使用中の為、"
                                               DELIMITED BY  SIZE
                       "実行できません"
                                               DELIMITED BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "4001" THRU "4009"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  GERR
           MOVE    SPA-ERRCD           TO  GERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  GERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "G100"              TO  SPA-MOTOPG
           MOVE    "GERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "GERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-GIDSET-SEC              SECTION.
      *
           MOVE    SPACE                TO  WRK-GIDMSG
      *
           MOVE    "B12"                TO  MCP-WIDGET
      *
           EVALUATE    SPA-GIDCD
           WHEN    "1001"
               MOVE    "帳票の登録を削除します。"
                                        TO  WRK-GIDMSG
               MOVE    "B01"            TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    SPACE                TO  GID1
           INITIALIZE                       GID1
           MOVE    SPA-GIDCD            TO  GID1-ID1CODE
           MOVE    WRK-GIDMSG           TO  GID1-ID1MSG
      *
           MOVE    "G100"               TO  SPA-MOTOPG
           MOVE    "GID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "GID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-GIDSET-EXT.
           EXIT.
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
      *    ＬＤユーザーチェック処理
      *****************************************************************
       800-LDUSER-CHK-SEC              SECTION.
      *
           INITIALIZE                  SLDUSER-AREA
           MOVE    52              TO  SLDUSER-MY-LD
           MOVE    1               TO  SLDUSER-KBN
           CALL    "ORCSLDUSER"        USING
                                       SLDUSER-AREA
                                       SPA-AREA
      *
           .
       800-LDUSER-CHK-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY25-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key25"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-KEY25-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY25-FET-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key25"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-KEY25-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-TOUKEIMEMO-KEY3-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUKEIMEMO
      *
           MOVE    TOUKEIMEMO-REC        TO  MCPDATA-REC
           MOVE    "tbl_toukeimemo"   TO  MCP-TABLE
           MOVE    "key3"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TOUKEIMEMO-REC
           ELSE
               INITIALIZE                  TOUKEIMEMO-REC
               MOVE    1               TO  FLG-TOUKEIMEMO
           END-IF
      *
           .
      *
       900-TOUKEIMEMO-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-TOUKEIMEMO-KEY3-FET-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUKEIMEMO
      *
           MOVE    "tbl_toukeimemo"   TO  MCP-TABLE
           MOVE    "key3"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TOUKEIMEMO-REC
           ELSE
               INITIALIZE                  TOUKEIMEMO-REC
               MOVE    1               TO  FLG-TOUKEIMEMO
           END-IF
      *
           .
      *
       900-TOUKEIMEMO-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    統計メモテーブル検索処理
      *****************************************************************
       900-TOUKEIMEMO-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUKEIMEMO
      *
           MOVE    "tbl_toukeimemo"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  TOUKEIMEMO-REC
           ELSE
               INITIALIZE              TOUKEIMEMO-REC
               MOVE    1           TO  FLG-TOUKEIMEMO
           END-IF
      *
           MOVE    "tbl_toukeimemo"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TOUKEIMEMO-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
       910-DBFETCH-SEC                  SECTION.
      *
           MOVE    "DBFETCH"       TO   MCP-FUNC
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
       910-DBCLOSECURSOR-SEC                  SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                  SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

