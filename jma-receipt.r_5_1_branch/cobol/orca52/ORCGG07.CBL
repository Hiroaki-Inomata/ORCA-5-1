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
       PROGRAM-ID.                     ORCGG07.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次統計
      *  コンポーネント名  : 統計帳票一覧
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
      *    システム管理情報設定用共通パラメタ
           COPY    "G01COMMON-SPA".
      *
      *    コンスタント値
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
           COPY    "G01SCR-SPA".
      *
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-TOUKEIMEMO              PIC 9(01).
           03  FLG-ENTRY                   PIC 9(01).
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
           03  WRK-SYMD                    PIC X(10).
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-ZZ9                     PIC ZZ9.
           03  WRK-KANRICD                 PIC X(04).
           03  WRK-TYOHYOKBN               PIC X(01).
      *    入力項目名
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-PGSRH-G.
               05  WRK-PGSRH-CHK-PGNAME    PIC X(20).
               05  WRK-PGSRH-MAX           PIC 9(05).
               05  WRK-PGSRH-OCC           OCCURS  200.
                   07  WRK-PGSRH-PGNAME    PIC X(20).
           03  WRK-PATHNAME                PIC X(64).
           03  WRK-KBNCD-G.
               05  WRK-KBNCD               PIC 9(03).
           03  WRK-STYUKYMD                PIC X(08).
           03  WRK-EDYUKYMD                PIC X(08).
      *
       01  WRK-ROW.
           05  WRK-TYOHYOLST-ROW           PIC S9(9)   BINARY.
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-TYOHYOLST-MAX         PIC 9(05)   VALUE   200.
           03  CONST-KANRICD               PIC X(04)   VALUE   "3002".
      *
           COPY    "CPSK0043.INC"          REPLACING    //0043//
                                           BY           //TOUKEI//.
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
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *    COPY    "ORCA-DBPATH".
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
           MOVE    SPA-WORK        TO  SPA-G01KYOUTU
           MOVE    SPA-FREE        TO  SPA-G01FREE
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
           MOVE    SPA-G01FREE     TO  SPA-FREE
           MOVE    SPA-G01KYOUTU   TO  SPA-WORK 
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
           MOVE    "G07    "           TO  MCP-WINDOW.
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
           PERFORM 310-SPASET-SEC
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
           INITIALIZE                  SPA-G07
      *
      *    帳票区分コンボ編集処理
           PERFORM 3101-TYOHYOKBNLST-HEN-SEC
      *
           MOVE    SPA-GMN-G07-TYOHYOKBNLST (1)
                                       TO  SPA-GMN-G07-TYOHYOKBN-G
      *
      *    帳票リスト編集処理
           PERFORM 3101-TYOHYOLST-HEN-SEC
      *
           MOVE    1                   TO  SPA-GMN-G07-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票区分コンボ編集処理
      *****************************************************************
       3101-TYOHYOKBNLST-HEN-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-G07-TYOHYOKBNLST-G
      *
           MOVE    "1"             TO  SPA-GMN-G07-TYOHYOKBNL (1)
           MOVE    "標準帳票"      TO  SPA-GMN-G07-TYOHYOKBNMEIL (1)
           MOVE    "2"             TO  SPA-GMN-G07-TYOHYOKBNL (2)
           MOVE    "登録済帳票"    TO  SPA-GMN-G07-TYOHYOKBNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-G07-TYOHYOKBN-MAX
      *
           .
       3101-TYOHYOKBNLST-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票リスト編集処理
      *****************************************************************
       3101-TYOHYOLST-HEN-SEC           SECTION.
      *
           MOVE    1                   TO  FLG-TYOHYOLST-ROW
      *
           MOVE    ZERO                TO  SPA-GMN-G07-SELNUM
      *
           INITIALIZE                  SPA-GMN-G07-TYOHYOLST-AREA
                                       SPA-NAI-G07-TYOHYOLST-AREA
      *
           MOVE    SPACE               TO  WRK-KANRICD
      *
           IF    ( CONST-KANRICD     =   "3001" )
               IF    ( SPA-GMN-G07-TYOHYOKBN   =   "1" )
                   PERFORM 3101-ENTRYPRT-GET-SEC
                   MOVE    "0043"      TO  WRK-KANRICD
               ELSE
                   MOVE    "3001"      TO  WRK-KANRICD
               END-IF
           ELSE
               IF    ( SPA-GMN-G07-TYOHYOKBN   =   "1" )
                   PERFORM 3101-ENTRYPRT-GET-SEC
                   MOVE    "0044"      TO  WRK-KANRICD
               ELSE
                   MOVE    "3002"      TO  WRK-KANRICD
               END-IF
           END-IF
      *
           MOVE    ZERO                TO FLG-SYSKANRI
      *
           INITIALIZE                  SYSKANRI-REC
      *
      *
           MOVE    WRK-KANRICD         TO  SYS-KANRICD
           MOVE    SPA-SYSYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
      *
           MOVE    "key2"              TO  WRK-PATHNAME
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX1
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( IDX1        >   CONST-TYOHYOLST-MAX )
      *
               COMPUTE IDX1    =   IDX1    +   1
      *
      *
               EVALUATE    WRK-KANRICD
               WHEN    "0043"
                   MOVE  SYSKANRI-REC    TO  SYS-0043-REC
                   MOVE  IDX1            TO  SPA-GMN-G07-TNUM    (IDX1)
                   MOVE  SYS-0043-KANRICD
                                         TO  SPA-NAI-G07-TKANRICD(IDX1)
                   MOVE  SYS-0043-KBNCD  TO  SPA-NAI-G07-TKBNCD  (IDX1)
                   MOVE  SYS-0043-HCNAME TO  SPA-GMN-G07-THCNAME (IDX1)
                   MOVE  SYS-0043-PRGNAME
                                         TO  SPA-GMN-G07-TPRGNAME(IDX1)
                                               WRK-PGSRH-CHK-PGNAME
                   PERFORM 3101-ENTRYPRT-CHK-SEC
                   IF    ( FLG-ENTRY       =   1 )
                       MOVE    1         TO  SPA-GMN-G07-TSTATUS (IDX1)
                   END-IF
               WHEN    "0044"
                   MOVE  SYSKANRI-REC    TO  SYS-0044-REC
                   MOVE  IDX1            TO  SPA-GMN-G07-TNUM    (IDX1)
                   MOVE  SYS-0044-KANRICD
                                         TO  SPA-NAI-G07-TKANRICD(IDX1)
                   MOVE  SYS-0044-KBNCD  TO  SPA-NAI-G07-TKBNCD  (IDX1)
                   MOVE  SYS-0044-HCNAME TO  SPA-GMN-G07-THCNAME (IDX1)
                   MOVE  SYS-0044-PRGNAME
                                         TO  SPA-GMN-G07-TPRGNAME(IDX1)
                                             WRK-PGSRH-CHK-PGNAME
                   PERFORM 3101-ENTRYPRT-CHK-SEC
                   IF    ( FLG-ENTRY       =   1 )
                       MOVE    1         TO  SPA-GMN-G07-TSTATUS (IDX1)
                   END-IF
               WHEN    "3001"
                   MOVE  SYSKANRI-REC    TO  SYS-3001-REC
                   MOVE  SYS-3001-KBNCD  TO  WRK-KBNCD-G
                   MOVE  WRK-KBNCD       TO  SPA-GMN-G07-TNUM    (IDX1)
                   MOVE  SYS-3001-KANRICD
                                         TO  SPA-NAI-G07-TKANRICD(IDX1)
                   MOVE  SYS-3001-KBNCD  TO  SPA-NAI-G07-TKBNCD  (IDX1)
                   MOVE  SYS-3001-HCNAME TO  SPA-GMN-G07-THCNAME (IDX1)
                   MOVE  SYS-3001-PRGNAME
                                         TO  SPA-GMN-G07-TPRGNAME(IDX1)
                   MOVE    1             TO  SPA-GMN-G07-TSTATUS (IDX1)
               WHEN    "3002"
                   MOVE  SYSKANRI-REC    TO  SYS-3002-REC
                   MOVE  SYS-3002-KBNCD  TO  WRK-KBNCD-G
                   MOVE  WRK-KBNCD       TO  SPA-GMN-G07-TNUM    (IDX1)
                   MOVE  SYS-3002-KANRICD
                                         TO  SPA-NAI-G07-TKANRICD(IDX1)
                   MOVE  SYS-3002-KBNCD  TO  SPA-NAI-G07-TKBNCD  (IDX1)
                   MOVE  SYS-3002-HCNAME TO  SPA-GMN-G07-THCNAME (IDX1)
                   MOVE  SYS-3002-PRGNAME
                                         TO  SPA-GMN-G07-TPRGNAME(IDX1)
                   MOVE    1             TO  SPA-GMN-G07-TSTATUS (IDX1)
               END-EVALUATE
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  SYSKANRI-REC
               ELSE
                   INITIALIZE              SYSKANRI-REC
                   MOVE    1           TO  FLG-SYSKANRI
               END-IF
      *
           END-PERFORM
      *
           MOVE    IDX1                TO  SPA-GMN-G07-TYOHYOLST-MAX
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-TYOHYOLST-HEN-EXT.
           EXIT.
      *****************************************************************
      *    登録済帳票取得処理
      *****************************************************************
       3101-ENTRYPRT-GET-SEC           SECTION.
      *
           INITIALIZE                     WRK-PGSRH-G
      *
           MOVE    ZERO                TO FLG-SYSKANRI
      *
           INITIALIZE                     SYSKANRI-REC
           MOVE    CONST-KANRICD     TO SYS-KANRICD
           MOVE    SPA-SYSYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( WRK-PGSRH-MAX       >   CONST-TYOHYOLST-MAX )
      *
               COMPUTE WRK-PGSRH-MAX   =   WRK-PGSRH-MAX   +   1
      *
               IF    ( CONST-KANRICD =   "3001" )
                   MOVE    SYSKANRI-REC    TO  SYS-3001-REC
                   MOVE    SYS-3001-PRGNAME
                                           TO  WRK-PGSRH-PGNAME
                                                         (WRK-PGSRH-MAX)
               ELSE
                   MOVE    SYSKANRI-REC    TO  SYS-3002-REC
                   MOVE    SYS-3002-PRGNAME
                                           TO  WRK-PGSRH-PGNAME
                                                         (WRK-PGSRH-MAX)
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  SYSKANRI-REC
               ELSE
                   INITIALIZE              SYSKANRI-REC
                   MOVE    1           TO  FLG-SYSKANRI
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-ENTRYPRT-GET-EXT.
           EXIT.
      *****************************************************************
      *    登録済帳票チェック処理
      *****************************************************************
       3101-ENTRYPRT-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-ENTRY
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   WRK-PGSRH-MAX )
                    OR   ( FLG-ENTRY   =   1 )
      *
               IF    ( WRK-PGSRH-CHK-PGNAME
                                       =   WRK-PGSRH-PGNAME (IDX0) )
      *
                   MOVE    1           TO  FLG-ENTRY
      *
               END-IF
      *
           END-PERFORM
      *
           .
       3101-ENTRYPRT-CHK-EXT.
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
               MOVE   ZERO                 TO  SPA-NAI-G07-RET
               PERFORM  210-BACK
      *    確定
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 480-KAKUTEI-SEC
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
           WHEN    "ACTIVATE"      ALSO    "TYOHYOKBN"
               PERFORM 4100-TYOHYOKBN-CHK-SEC
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
           MOVE    ZERO            TO  SPA-GMN-G07-CUR
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
           MOVE    G07-SELNUM          TO  SPA-GMN-G07-SELNUM
      *
           PERFORM 4101-SELNUM-CHK-SEC
      *
           .
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    帳票区分チェック処理
      *****************************************************************
       4100-TYOHYOKBN-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-G07-CUR
      *
           MOVE    G07-TYOHYOKBN       TO  SPA-GMN-G07-TYOHYOKBN-G
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-G07-TYOHYOKBN-MAX )
                    OR   ( SPA-GMN-G07-TYOHYOKBN
                                   =   SPA-GMN-G07-TYOHYOKBNL (IDX0))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0    >   SPA-GMN-G07-TYOHYOKBN-MAX )
               MOVE    2               TO  SPA-GMN-G07-CUR
               MOVE    "0002"          TO  SPA-ERRCD
           ELSE
               MOVE    SPA-GMN-G07-TYOHYOKBNLST (IDX0)
                                       TO  SPA-GMN-G07-TYOHYOKBN-G
      *
               PERFORM 3101-TYOHYOLST-HEN-SEC
           END-IF
      *
      *
           .
       4100-TYOHYOKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    帳票リストチェック処理
      *****************************************************************
       4100-TYOHYOLST-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-G07-SELNUM
                                       SPA-GMN-G07-TYOHYOLST-SEL
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-G07-TYOHYOLST-MAX )
      *
               IF    ( G07-LIST-SELECT (IDX0)  =   "T" )
                   MOVE    SPA-GMN-G07-TNUM (IDX0)
                                   TO  SPA-GMN-G07-SELNUM
                   MOVE    IDX0    TO  SPA-GMN-G07-TYOHYOLST-SEL
                   MOVE    SPA-NAI-G07-TKANRICD(IDX0)
                                   TO  SPA-NAI-G07-KANRICD
                   MOVE    SPA-NAI-G07-TKBNCD  (IDX0)
                                   TO  SPA-NAI-G07-KBNCD
                   MOVE    1       TO  SPA-GMN-G07-CUR
      *
                   MOVE    SPA-GMN-G07-TYOHYOLST-MAX
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
           MOVE    ZERO                TO  SPA-GMN-G07-CUR
           MOVE    ZERO                TO  SPA-GMN-G07-TYOHYOLST-SEL
      *
           MOVE    ZERO                TO  IDXL
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-G07-TYOHYOLST-MAX )
                    OR   ( IDXL    >   ZERO )
      *
               IF    ( SPA-GMN-G07-SELNUM  =   SPA-GMN-G07-TNUM (IDX0))
                   MOVE    IDX0        TO  IDXL
               END-IF
      *
           END-PERFORM
      *
           IF    ( IDXL                >   ZERO )
               MOVE    IDXL            TO  SPA-GMN-G07-TYOHYOLST-SEL
      *
               MOVE    SPA-NAI-G07-TKANRICD(IDXL)
                                       TO  SPA-NAI-G07-KANRICD
               MOVE    SPA-NAI-G07-TKBNCD  (IDXL)
                                       TO  SPA-NAI-G07-KBNCD
           ELSE
               MOVE    1               TO  SPA-GMN-G07-CUR
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
           MOVE    SPA-G01-MOTOPG      TO  SPA-SAKIPG
           MOVE    "G07"               TO  SPA-MOTOPG
      *
           MOVE   "JOIN  "             TO  MCP-PUTTYPE
           MOVE    SPA-G01-MOTOPG      TO  MCP-WINDOW
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
      *    確定処理
      *****************************************************************
       480-KAKUTEI-SEC             SECTION.
      *
           PERFORM 4900-KANRENCHK-SEC
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4801-TOUROKU-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               MOVE   1            TO  SPA-NAI-G07-RET
               PERFORM 210-BACK
           END-IF
      *
           .
       480-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       4801-TOUROKU-SEC            SECTION.
      *
           INITIALIZE              SYS-TOUKEI-REC
                                   SYS-3001-REC
      *
           PERFORM 800-MCNDATE-SEC
      *
           PERFORM 48011-SYSKANRI-DEL-SEC
      *
           IF    ( SPA-ERRCD         =   SPACE )
               PERFORM 48011-KIKAN-EDIT-SEC
           END-IF
      *
           IF    ( SPA-ERRCD         =   SPACE )
               INITIALIZE                      SYSKANRI-REC
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    SPA-NAI-G07-KANRICD TO  SYS-KANRICD
               MOVE    SPA-NAI-G07-KBNCD   TO  SYS-KBNCD
               MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI        NOT = ZERO )
                   MOVE    "4004"          TO  SPA-ERRCD
               ELSE
                   MOVE    SYSKANRI-REC    TO  SYS-TOUKEI-REC
                                               SYS-3001-REC
                   MOVE    SPA-HOSPNUM     TO  SYS-3001-HOSPNUM
                   MOVE    CONST-KANRICD   TO  SYS-3001-KANRICD
                   MOVE    SPA-G01-SEL     TO  WRK-KBNCD
                   MOVE    WRK-KBNCD-G     TO  SYS-3001-KBNCD
                   MOVE    WRK-STYUKYMD    TO  SYS-3001-STYUKYMD
                   MOVE    WRK-EDYUKYMD    TO  SYS-3001-EDYUKYMD
                   MOVE    SMCNDATE-YMD    TO  SYS-3001-CREYMD
                                               SYS-3001-UPYMD
                   MOVE    SMCNDATE-HMS    TO  SYS-3001-UPHMS
                   MOVE    SYS-3001-REC    TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC      NOT =    ZERO )
                       MOVE    "4005"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
            AND  ( SPA-NAI-G07-KANRICD     =   "3002" )
               PERFORM 48011-SYS-3102-INS-SEC
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               PERFORM 48011-TOUKEIMEMO-INS-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           NOT =   SPACE )
               INITIALIZE                  SYSKANRI-REC
               MOVE    SYSKANRI-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "abort"         TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
           END-IF
      *
           .
       4801-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    システム管理削除処理
      *****************************************************************
       48011-SYSKANRI-DEL-SEC          SECTION.
      *
           INITIALIZE                SYSKANRI-REC
           MOVE    SPA-HOSPNUM   TO  SYS-HOSPNUM
           MOVE    CONST-KANRICD TO  SYS-KANRICD
           MOVE    SPA-G01-SEL   TO  WRK-KBNCD
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
                   MOVE    "4002"      TO  SPA-ERRCD
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
                       MOVE    "4007"      TO  SPA-ERRCD
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
                       MOVE    "4003"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       48011-SYSKANRI-DEL-EXT.
           EXIT.
      *****************************************************************
      *    有効期間編集処理
      *****************************************************************
       48011-KIKAN-EDIT-SEC            SECTION.
      *
           MOVE    "00000000"      TO  WRK-STYUKYMD
           MOVE    "99999999"      TO  WRK-EDYUKYMD
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    CONST-KANRICD   TO  SYS-KANRICD
           MOVE    SPA-G01-SEL     TO  WRK-KBNCD
           MOVE    WRK-KBNCD-G     TO  SYS-KBNCD
           PERFORM 900-SYSKANRI-KEY8-SEL-SEC
      *
           DISPLAY "FLG-SYSKANRI= " FLG-SYSKANRI
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               IF    ( SPA-SYSYMD  >   SYS-EDYUKYMD )
                   IF    ( WRK-STYUKYMD    <   SYS-EDYUKYMD )
                       MOVE    SYS-EDYUKYMD    TO  WRK-STYUKYMD
                   END-IF
               END-IF
      *
               IF    ( SPA-SYSYMD  <   SYS-STYUKYMD )
                   IF    ( WRK-EDYUKYMD    >   SYS-STYUKYMD )
                       MOVE    SYS-STYUKYMD    TO  WRK-EDYUKYMD
                   END-IF
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY8-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( WRK-STYUKYMD    NOT =   "00000000" )
               MOVE    "1"             TO   WRK-ZOGENPTN
               MOVE    1               TO   WRK-ZOGEN
               MOVE    WRK-STYUKYMD    TO   WRK-SYMD
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-SYMD        TO   WRK-STYUKYMD
           END-IF
      *
           IF    ( WRK-EDYUKYMD    NOT =   "99999999" )
               MOVE    "1"             TO   WRK-ZOGENPTN
               MOVE    -1              TO   WRK-ZOGEN
               MOVE    WRK-EDYUKYMD    TO   WRK-SYMD
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-SYMD        TO   WRK-EDYUKYMD
           END-IF
      *
           .
       48011-KIKAN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    月次付加情報更新処理
      *****************************************************************
       48011-SYS-3102-INS-SEC        SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "3102"              TO  SYS-KANRICD
           MOVE    SPA-NAI-G07-KBNCD   TO  SYS-KBNCD
           MOVE    SYS-TOUKEI-STYUKYMD TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        NOT = ZERO )
               CONTINUE
           ELSE
      *
               MOVE    SYSKANRI-REC    TO  SYS-3102-REC
               MOVE    SPA-HOSPNUM     TO  SYS-3102-HOSPNUM
               MOVE    SPA-G01-SEL     TO  WRK-KBNCD
               MOVE    WRK-KBNCD-G     TO  SYS-3102-KBNCD
               MOVE    WRK-STYUKYMD    TO  SYS-3102-STYUKYMD
               MOVE    WRK-EDYUKYMD    TO  SYS-3102-EDYUKYMD
               MOVE    SMCNDATE-YMD    TO  SYS-3102-CREYMD
                                           SYS-3102-UPYMD
               MOVE    SMCNDATE-HMS    TO  SYS-3102-UPHMS
               MOVE    SYS-3102-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC      NOT =    ZERO )
                   MOVE    "4008"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       48011-SYS-3102-INS-EXT.
           EXIT.
      *****************************************************************
      *    統計メモ更新処理
      *****************************************************************
       48011-TOUKEIMEMO-INS-SEC        SECTION.
      *
           INITIALIZE                  TMEMO-REC
      *
           IF    ( SYS-TOUKEI-KANRICD  =   "3001" OR "3002" )
               INITIALIZE                      TOUKEIMEMO-REC
               MOVE    SPA-HOSPNUM         TO  TKIMEMO-HOSPNUM
               MOVE    SYS-TOUKEI-PRGNAME  TO  TKIMEMO-PGID
               MOVE    SYS-TOUKEI-KANRICD  TO  TKIMEMO-KANRICD
               MOVE    SYS-TOUKEI-KBNCD    TO  TKIMEMO-KBNCD
               MOVE    SYS-TOUKEI-STYUKYMD TO  TKIMEMO-STYUKYMD
               MOVE    SYS-TOUKEI-EDYUKYMD TO  TKIMEMO-EDYUKYMD
               MOVE    TOUKEIMEMO-REC      TO  MCPDATA-REC
               PERFORM 900-TOUKEIMEMO-KEY-SEL-SEC
               IF    ( FLG-TOUKEIMEMO      =   ZERO )
                   MOVE    TOUKEIMEMO-REC  TO  TMEMO-REC
               END-IF
           END-IF
      *
           IF    ( TMEMO-HOSPNUM   =   ZERO )
               INITIALIZE                      TOUKEIMEMO-REC
               MOVE    SPA-HOSPNUM         TO  TKIMEMO-HOSPNUM
               MOVE    SYS-TOUKEI-PRGNAME  TO  TKIMEMO-PGID
               MOVE    "0000"              TO  TKIMEMO-KANRICD
               MOVE    "000"               TO  TKIMEMO-KBNCD
               MOVE    "00000000"          TO  TKIMEMO-STYUKYMD
               MOVE    "99999999"          TO  TKIMEMO-EDYUKYMD
               MOVE    TOUKEIMEMO-REC      TO  MCPDATA-REC
               PERFORM 900-TOUKEIMEMO-KEY-SEL-SEC
               IF    ( FLG-TOUKEIMEMO      =   ZERO )
                   MOVE    TOUKEIMEMO-REC  TO  TMEMO-REC
                   MOVE    "1"             TO  TMEMO-SAVEPARA
               END-IF
           END-IF
      *
           MOVE    TMEMO-REC       TO  TOUKEIMEMO-REC
           MOVE    SPA-HOSPNUM     TO  TKIMEMO-HOSPNUM
           MOVE    SYS-TOUKEI-PRGNAME
                                   TO  TKIMEMO-PGID
           MOVE    CONST-KANRICD   TO  TKIMEMO-KANRICD
           MOVE    SPA-G01-SEL     TO  WRK-KBNCD
           MOVE    WRK-KBNCD-G     TO  TKIMEMO-KBNCD
           MOVE    WRK-STYUKYMD    TO  TKIMEMO-STYUKYMD
           MOVE    WRK-EDYUKYMD    TO  TKIMEMO-EDYUKYMD
           MOVE    SMCNDATE-YMD    TO  SYS-3001-CREYMD
                                       SYS-3001-UPYMD
           MOVE    SMCNDATE-HMS    TO  SYS-3001-UPHMS
           MOVE    TOUKEIMEMO-REC  TO  MCPDATA-REC
           MOVE    "tbl_toukeimemo" TO MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4006"      TO  SPA-ERRCD
           END-IF
      *
           .
       48011-TOUKEIMEMO-INS-EXT.
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
           PERFORM 800-LDUSER-CHK-SEC
           IF    ( SLDUSER-OTHERS  >   ZERO )
               MOVE    "0104"      TO  SPA-ERRCD
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
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "G07    "           TO  MCP-WINDOW.
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
           MOVE    G07-LIST-ROW    TO  WRK-TYOHYOLST-ROW
      *
           INITIALIZE                  G07
      *
           EVALUATE    FLG-TYOHYOLST-ROW
           WHEN    1
               MOVE    ZERO            TO  G07-LIST-ROW
               MOVE    ZERO            TO  G07-LIST-ROWATTR
           WHEN    OTHER
               MOVE    WRK-TYOHYOLST-ROW
                                       TO  G07-LIST-ROW
               MOVE    ZERO            TO  G07-LIST-ROWATTR
           END-EVALUATE
      *
           IF    ( CONST-KANRICD     =   "3001" )
               MOVE    "日次統計帳票一覧"
                                       TO  G07-LBLTOUKEI
           ELSE
               MOVE    "月次統計帳票一覧"
                                       TO  G07-LBLTOUKEI
           END-IF
      *
           MOVE    SPA-GMN-G07-SELNUM  TO  G07-SELNUM
           MOVE    SPA-GMN-G07-TYOHYOKBN-G
                                       TO  G07-TYOHYOKBN
      *
           MOVE    SPA-GMN-G07-TYOHYOKBN-MAX
                                       TO  G07-TYOHYOKBN-COUNT
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-G07-TYOHYOKBN-MAX )
      *
               MOVE    SPA-GMN-G07-TYOHYOKBNLST (IDX0)
                                       TO  G07-TYOHYOKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-G07-TYOHYOLST-MAX
                                       TO  G07-LIST-COUNT
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-G07-TYOHYOLST-MAX )
      *
               MOVE    SPA-GMN-G07-TNUM(IDX0)
                                       TO  WRK-ZZ9
               MOVE    WRK-ZZ9         TO  G07-LIST-TNUM (IDX0)
      *
               MOVE    SPA-GMN-G07-THCNAME (IDX0)
                                       TO  G07-LIST-TPGNAME (IDX0)
               MOVE    SPA-GMN-G07-TPRGNAME (IDX0)
                                       TO  G07-LIST-TPGID (IDX0)
               IF    ( SPA-GMN-G07-TSTATUS (IDX0)  =   1 )
                   MOVE    "　○"      TO  G07-LIST-TSTATUS (IDX0)
               END-IF
      *
               IF    ( IDX0        =   SPA-GMN-G07-TYOHYOLST-SEL )
                   MOVE    "T"         TO  G07-LIST-SELECT (IDX0)
               ELSE
                   MOVE    "F"         TO  G07-LIST-SELECT (IDX0)
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
                  (SPA-GMN-G07-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-G07-CUR
           WHEN    001
               MOVE    "SELNUM"        TO  MCP-WIDGET
           WHEN    002
               MOVE    "TYOHYOKBN"     TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"           TO  MCP-WIDGET
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
               MOVE    212             TO  SPA-GMN-G07-CUR
           WHEN    "TYOHYOKBN"
               MOVE    001             TO  SPA-GMN-G07-CUR
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
           WHEN    "0002"
               MOVE    "帳票区分入力エラー"
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
           MOVE    "G07"               TO  SPA-MOTOPG
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
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
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
       900-SYSKANRI-KEY8-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
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
       900-SYSKANRI-KEY8-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY8-FET-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
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
       900-SYSKANRI-KEY8-FET-EXT.
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

