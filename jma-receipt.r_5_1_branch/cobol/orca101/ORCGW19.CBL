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
       PROGRAM-ID.                     ORCGW19.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 統計帳票一覧
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/08/04    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/05/02  グループ診療対応
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
           COPY    "W01COMMON-SPA".
      *
      *    コンスタント値
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
           COPY    "W10SCR-SPA".
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
      *
       01  WRK-ROW.
           05  WRK-TYOHYOLST-ROW           PIC S9(9)   BINARY.
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-TYOHYOLST-MAX         PIC 9(05)   VALUE   200.
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
      *
      *    統計メモ
       01  TOUKEIMEMO-REC.
           COPY  "CPTOUKEIMEMO.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *    COPY    "ORCA-DBPATH".
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
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W10FREE
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
           MOVE    SPA-W10FREE     TO  SPA-FREE
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
           MOVE    "W19    "           TO  MCP-WINDOW.
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
           INITIALIZE                  SPA-W19
      *
      *    帳票区分コンボ編集処理
           PERFORM 3101-TYOHYOKBNLST-HEN-SEC
      *
           MOVE    SPA-GMN-W19-TYOHYOKBNLST (1)
                                       TO  SPA-GMN-W19-TYOHYOKBN-G
      *
      *    帳票リスト編集処理
           PERFORM 3101-TYOHYOLST-HEN-SEC
      *
           MOVE    1                   TO  SPA-GMN-W19-CUR
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
           INITIALIZE                  SPA-GMN-W19-TYOHYOKBNLST-G
      *
           IF    ( SPA-NAI-W10-W19-SENI-FLG
                                       =   ZERO )
               MOVE    "1"             TO  SPA-GMN-W19-TYOHYOKBNL (1)
               MOVE    "標準帳票"      TO  SPA-GMN-W19-TYOHYOKBNMEIL (1)
               MOVE    "2"             TO  SPA-GMN-W19-TYOHYOKBNL (2)
               MOVE    "登録済帳票"    TO  SPA-GMN-W19-TYOHYOKBNMEIL (2)
      *
               MOVE    2               TO  SPA-GMN-W19-TYOHYOKBN-MAX
           ELSE
               MOVE    "1"             TO  SPA-GMN-W19-TYOHYOKBNL (1)
               MOVE    "標準帳票"      TO  SPA-GMN-W19-TYOHYOKBNMEIL (1)
      *
               MOVE    1               TO  SPA-GMN-W19-TYOHYOKBN-MAX
           END-IF
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
           MOVE    ZERO                TO  SPA-GMN-W19-SELNUM
      *
           INITIALIZE                  SPA-GMN-W19-TYOHYOLST-AREA
                                       SPA-NAI-W19-TYOHYOLST-AREA
      *
           MOVE    SPACE               TO  WRK-KANRICD
      *
           IF    ( SPA-W01-KANRICD     =   "3001" )
               IF    ( SPA-GMN-W19-TYOHYOKBN   =   "1" )
                   PERFORM 3101-ENTRYPRT-GET-SEC
                   MOVE    "0043"      TO  WRK-KANRICD
               ELSE
                   MOVE    "3001"      TO  WRK-KANRICD
               END-IF
           ELSE
               IF    ( SPA-GMN-W19-TYOHYOKBN   =   "1" )
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
           IF    ( SPA-NAI-W10-W19-SENI-FLG    =   ZERO )
      *
               MOVE    WRK-KANRICD         TO  SYS-KANRICD
               MOVE    SPA-W01-SEL-STYUKYMD
                                           TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
      *
               MOVE    "key2"              TO  WRK-PATHNAME
           ELSE
               MOVE    WRK-KANRICD         TO  SYS-KANRICD
               MOVE    SPA-W10-KBNCD(1:1)  TO  SYS-KBNCD (1:1)
               MOVE    "%"                 TO  SYS-KBNCD (2:1)
               MOVE    SPA-W01-SEL-STYUKYMD
                                           TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
      *
               MOVE    "key"               TO  WRK-PATHNAME
           END-IF
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
               EVALUATE    WRK-KANRICD
               WHEN    "0043"
                   MOVE  SYSKANRI-REC    TO  SYS-0043-REC
                   MOVE  IDX1            TO  SPA-GMN-W19-TNUM    (IDX1)
                   MOVE  SYS-0043-KANRICD
                                         TO  SPA-NAI-W19-TKANRICD(IDX1)
                   MOVE  SYS-0043-KBNCD  TO  SPA-NAI-W19-TKBNCD  (IDX1)
                   MOVE  SYS-0043-HCNAME TO  SPA-GMN-W19-THCNAME (IDX1)
                   MOVE  SYS-0043-PRGNAME
                                         TO  SPA-GMN-W19-TPRGNAME(IDX1)
                                               WRK-PGSRH-CHK-PGNAME
                   PERFORM 3101-ENTRYPRT-CHK-SEC
                   IF    ( FLG-ENTRY       =   1 )
                       MOVE    1         TO  SPA-GMN-W19-TSTATUS (IDX1)
                   END-IF
               WHEN    "0044"
                   MOVE  SYSKANRI-REC    TO  SYS-0044-REC
                   MOVE  IDX1            TO  SPA-GMN-W19-TNUM    (IDX1)
                   MOVE  SYS-0044-KANRICD
                                         TO  SPA-NAI-W19-TKANRICD(IDX1)
                   MOVE  SYS-0044-KBNCD  TO  SPA-NAI-W19-TKBNCD  (IDX1)
                   MOVE  SYS-0044-HCNAME TO  SPA-GMN-W19-THCNAME (IDX1)
                   MOVE  SYS-0044-PRGNAME
                                         TO  SPA-GMN-W19-TPRGNAME(IDX1)
                                             WRK-PGSRH-CHK-PGNAME
                   PERFORM 3101-ENTRYPRT-CHK-SEC
                   IF    ( FLG-ENTRY       =   1 )
                       MOVE    1         TO  SPA-GMN-W19-TSTATUS (IDX1)
                   END-IF
               WHEN    "3001"
                   MOVE  SYSKANRI-REC    TO  SYS-3001-REC
                   MOVE  SYS-3001-KBNCD  TO  WRK-KBNCD-G
                   MOVE  WRK-KBNCD       TO  SPA-GMN-W19-TNUM    (IDX1)
                   MOVE  SYS-3001-KANRICD
                                         TO  SPA-NAI-W19-TKANRICD(IDX1)
                   MOVE  SYS-3001-KBNCD  TO  SPA-NAI-W19-TKBNCD  (IDX1)
                   MOVE  SYS-3001-HCNAME TO  SPA-GMN-W19-THCNAME (IDX1)
                   MOVE  SYS-3001-PRGNAME
                                         TO  SPA-GMN-W19-TPRGNAME(IDX1)
                   MOVE    1             TO  SPA-GMN-W19-TSTATUS (IDX1)
               WHEN    "3002"
                   MOVE  SYSKANRI-REC    TO  SYS-3002-REC
                   MOVE  SYS-3002-KBNCD  TO  WRK-KBNCD-G
                   MOVE  WRK-KBNCD       TO  SPA-GMN-W19-TNUM    (IDX1)
                   MOVE  SYS-3002-KANRICD
                                         TO  SPA-NAI-W19-TKANRICD(IDX1)
                   MOVE  SYS-3002-KBNCD  TO  SPA-NAI-W19-TKBNCD  (IDX1)
                   MOVE  SYS-3002-HCNAME TO  SPA-GMN-W19-THCNAME (IDX1)
                   MOVE  SYS-3002-PRGNAME
                                         TO  SPA-GMN-W19-TPRGNAME(IDX1)
                   MOVE    1             TO  SPA-GMN-W19-TSTATUS (IDX1)
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
           MOVE    IDX1                TO  SPA-GMN-W19-TYOHYOLST-MAX
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBCLOSE-SEC
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
           MOVE    SPA-W01-KANRICD     TO SYS-KANRICD
           MOVE    SPA-W01-SEL-STYUKYMD
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
               IF    ( SPA-W01-KANRICD =   "3001" )
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
           PERFORM 910-DBCLOSE-SEC
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
               MOVE   ZERO                 TO  SPA-NAI-W19-RET
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
           MOVE    ZERO            TO  SPA-GMN-W19-CUR
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
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
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
           MOVE    W19-SELNUM          TO  SPA-GMN-W19-SELNUM
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
           MOVE    ZERO                TO  SPA-GMN-W19-CUR
      *
           MOVE    W19-TYOHYOKBN       TO  SPA-GMN-W19-TYOHYOKBN-G
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-W19-TYOHYOKBN-MAX )
                    OR   ( SPA-GMN-W19-TYOHYOKBN
                                   =   SPA-GMN-W19-TYOHYOKBNL (IDX0))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0    >   SPA-GMN-W19-TYOHYOKBN-MAX )
               MOVE    2               TO  SPA-GMN-W19-CUR
               MOVE    "0002"          TO  SPA-ERRCD
           ELSE
               MOVE    SPA-GMN-W19-TYOHYOKBNLST (IDX0)
                                       TO  SPA-GMN-W19-TYOHYOKBN-G
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
           MOVE    ZERO            TO  SPA-GMN-W19-SELNUM
                                       SPA-GMN-W19-TYOHYOLST-SEL
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-W19-TYOHYOLST-MAX )
      *
               IF    ( W19-LIST-SELECT (IDX0)  =   "T" )
                   MOVE    SPA-GMN-W19-TNUM (IDX0)
                                   TO  SPA-GMN-W19-SELNUM
                   MOVE    IDX0    TO  SPA-GMN-W19-TYOHYOLST-SEL
                   MOVE    SPA-NAI-W19-TKANRICD(IDX0)
                                   TO  SPA-NAI-W19-KANRICD
                   MOVE    SPA-NAI-W19-TKBNCD  (IDX0)
                                   TO  SPA-NAI-W19-KBNCD
                   MOVE    1       TO  SPA-GMN-W19-CUR
      *
                   MOVE    SPA-GMN-W19-TYOHYOLST-MAX
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
           MOVE    ZERO                TO  SPA-GMN-W19-CUR
      *
           MOVE    ZERO                TO  IDXL
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-W19-TYOHYOLST-MAX )
                    OR   ( IDXL    >   ZERO )
      *
               IF    ( SPA-GMN-W19-SELNUM  =   SPA-GMN-W19-TNUM (IDX0))
                   MOVE    IDX0        TO  IDXL
               END-IF
      *
           END-PERFORM
      *
           IF    ( IDXL                >   ZERO )
               MOVE    IDXL            TO  SPA-GMN-W19-TYOHYOLST-SEL
      *
               MOVE    SPA-NAI-W19-TKANRICD(IDXL)
                                       TO  SPA-NAI-W19-KANRICD
               MOVE    SPA-NAI-W19-TKBNCD  (IDXL)
                                       TO  SPA-NAI-W19-KBNCD
           ELSE
               MOVE    1               TO  SPA-GMN-W19-CUR
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
      *
           .
       4101-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W10"               TO  SPA-SAKIPG
           MOVE    "W19"               TO  SPA-MOTOPG
      *
           MOVE   "JOIN  "             TO  MCP-PUTTYPE
           MOVE    "W10"               TO  MCP-WINDOW
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
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           MOVE   1                    TO  SPA-NAI-W19-RET
      *
           PERFORM 210-BACK
      *
           .
       480-TOUROKU-EXT.
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
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W19    "           TO  MCP-WINDOW.
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
           MOVE    W19-LIST-ROW    TO  WRK-TYOHYOLST-ROW
      *
           INITIALIZE                  W19
      *
           EVALUATE    FLG-TYOHYOLST-ROW
           WHEN    1
               MOVE    ZERO            TO  W19-LIST-ROW
               MOVE    ZERO            TO  W19-LIST-ROWATTR
           WHEN    OTHER
               MOVE    WRK-TYOHYOLST-ROW
                                       TO  W19-LIST-ROW
               MOVE    ZERO            TO  W19-LIST-ROWATTR
           END-EVALUATE
      *
           IF    ( SPA-W01-KANRICD     =   "3001" )
               MOVE    "日次統計帳票一覧"
                                       TO  W19-LBLTOUKEI
           ELSE
               MOVE    "月次統計帳票一覧"
                                       TO  W19-LBLTOUKEI
           END-IF
      *
           MOVE    SPA-GMN-W19-SELNUM  TO  W19-SELNUM
           MOVE    SPA-GMN-W19-TYOHYOKBN-G
                                       TO  W19-TYOHYOKBN
      *
           MOVE    SPA-GMN-W19-TYOHYOKBN-MAX
                                       TO  W19-TYOHYOKBN-COUNT
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-W19-TYOHYOKBN-MAX )
      *
               MOVE    SPA-GMN-W19-TYOHYOKBNLST (IDX0)
                                       TO  W19-TYOHYOKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-W19-TYOHYOLST-MAX
                                       TO  W19-LIST-COUNT
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-W19-TYOHYOLST-MAX )
      *
               MOVE    SPA-GMN-W19-TNUM(IDX0)
                                       TO  WRK-ZZ9
               MOVE    WRK-ZZ9         TO  W19-LIST-TNUM (IDX0)
      *
               MOVE    SPA-GMN-W19-THCNAME (IDX0)
                                       TO  W19-LIST-TPGNAME (IDX0)
               MOVE    SPA-GMN-W19-TPRGNAME (IDX0)
                                       TO  W19-LIST-TPGID (IDX0)
               IF    ( SPA-GMN-W19-TSTATUS (IDX0)  =   1 )
                   MOVE    "　○"      TO  W19-LIST-TSTATUS (IDX0)
               END-IF
      *
               IF    ( IDX0        =   SPA-GMN-W19-TYOHYOLST-SEL )
                   MOVE    "T"         TO  W19-LIST-SELECT (IDX0)
               ELSE
                   MOVE    "F"         TO  W19-LIST-SELECT (IDX0)
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
                  (SPA-GMN-W19-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-W19-CUR
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
               MOVE    212             TO  SPA-GMN-W19-CUR
           WHEN    "TYOHYOKBN"
               MOVE    001             TO  SPA-GMN-W19-CUR
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
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W19"               TO  SPA-MOTOPG
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
       910-DBCLOSE-SEC                  SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSE-EXT.
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

