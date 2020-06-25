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
       PROGRAM-ID.                               ORCGW66.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 公害医療機関情報設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/12/04    NACL−太田    新規作成
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
       01  SPA-W66.
           03  SPA-GMN-CUR                         PIC 9(03).
           03  SPA-GMN-AREA.
               05  SPA-GMN-KGIIRYOCD                  PIC X(20).
               05  SPA-GMN-KGISKYKBN-G.
                   07  SPA-GMN-KGISKYKBN             PIC X(02).
                   07  SPA-GMN-KGISKYKBNF            PIC X(01).
                   07  SPA-GMN-KGISKYKBNMEI          PIC X(20).
           03  SPA-GMN-COMBO-AREA.
               05  SPA-GMN-KGISKYKBNLST-G.
                   07  SPA-GMN-KGISKYKBN-MAX         PIC 9(03).
                   07  SPA-GMN-KGISKYKBNLST          OCCURS  20.
                       09  SPA-GMN-KGISKYKBNL        PIC X(02).
                       09  SPA-GMN-KGISKYKBNFL       PIC X(01).
                       09  SPA-GMN-KGISKYKBNMEIL     PIC X(20).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-ENTRYCHK                        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                                PIC 9(05).
           03  IDX-CMB                             PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
      *    入力項目名
           03  WRK-MCP-WIDGET                      PIC X(64).
           03  WRK-WIDMSG                          PIC X(200).
           03  WRK-KANACHK-INPUT                   PIC X(100).
           03  WRK-CMB-CD                          PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM                     PIC X(200).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
      *    公害医療機関情報設定
           COPY  "CPSK4100.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
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
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W66
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
           MOVE    SPA-W66         TO  SPA-FREE
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
           MOVE    "W66    "           TO  MCP-WINDOW.
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
           INITIALIZE                  SPA-W66
      *
      *    画面初期化処理
           PERFORM 420-CLEAR-SEC
      *
           PERFORM 3100-COMBO-EDIT-SEC
      *
      *    公害医療機関情報設定
           PERFORM 3101-SYS4100-SEL-SEC
      *****IF    ( FLG-SYSKANRI    =   ZERO )
               MOVE    SYS-4100-KGIIRYOCD
                                   TO  SPA-GMN-KGIIRYOCD
               MOVE    SYS-4100-KGISKYKBN
                                   TO  WRK-CMB-CD
               PERFORM 4200-KGISKYKBNLST-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-KGISKYKBN-G
      *****END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コンボボックス編集処理
      *****************************************************************
       3100-COMBO-EDIT-SEC             SECTION.
      *
           PERFORM 3101-KGISKYKBNLST-SEC
      *
           .
       3100-COMBO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    公害診療報酬請求書区分コンボ編集処理
      *****************************************************************
       3101-KGISKYKBNLST-SEC             SECTION.
      *
           INITIALIZE                  SPA-GMN-KGISKYKBNLST-G
      *
           MOVE    "00"            TO  SPA-GMN-KGISKYKBNL   (1)
           MOVE    "標準"          TO  SPA-GMN-KGISKYKBNMEIL(1)
      *
           MOVE    "01"            TO  SPA-GMN-KGISKYKBNL   (2)
           MOVE    "名古屋市"      TO  SPA-GMN-KGISKYKBNMEIL(2)
      *
           MOVE    "02"            TO  SPA-GMN-KGISKYKBNL   (3)
           MOVE    "北九州市"      TO  SPA-GMN-KGISKYKBNMEIL(3)
      *
           MOVE    3               TO  SPA-GMN-KGISKYKBN-MAX
      *
           .
       3100-COMBO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    公害医療機関情報設定
      *****************************************************************
       3101-SYS4100-SEL-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "4100"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    SYSKANRI-REC    TO  SYS-4100-REC
           ELSE
               INITIALIZE                  SYS-4100-REC
           END-IF
      *
           IF      SYS-4100-KGISKYKBN  =   SPACE
               MOVE    "00"                TO  SYS-4100-KGISKYKBN
           END-IF
      *
           .
       3101-SYS4100-SEL-EXT.
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
               WHEN "3002"
                   PERFORM 4301-DEL-SEC
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
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM  210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM 420-CLEAR-SEC
      *    削除
           WHEN    "CLICKED"       ALSO    "B03"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM 430-DEL-SEC
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM 480-TOUROKU-SEC
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
           MOVE    W66-KGIIRYOCD      TO  SPA-GMN-KGIIRYOCD
           MOVE    W66-KGISKYKBN      TO  SPA-GMN-KGISKYKBN-G
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
           PERFORM 4101-KGIIRYOCD-CHK-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KGISKYKBN-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公害医療機関コードチェック処理
      *****************************************************************
       4101-KGIIRYOCD-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-KGIIRYOCD  =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-KGIIRYOCD   TO  WRK-KANACHK-INPUT
               PERFORM 800-ENRTRY-CHK-SEC
               MOVE    WRK-KANACHK-INPUT   TO  SPA-GMN-KGIIRYOCD
               IF    ( FLG-ENTRYCHK        NOT =   1 )
                   MOVE    "0001"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4101-KGIIRYOCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公害診療報酬請求書区分チェック処理
      *****************************************************************
       4101-KGISKYKBN-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-KGISKYKBN-G =   SPACE )
               MOVE    SPA-GMN-KGISKYKBNLST (1)
                                           TO  SPA-GMN-KGISKYKBN-G
           ELSE
               MOVE    SPA-GMN-KGISKYKBN   TO  WRK-CMB-CD
               PERFORM 4200-KGISKYKBNLST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   MOVE    "0002"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-GMN-CUR
               ELSE
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-KGISKYKBN-G
               END-IF
           END-IF
      *
           .
       4101-KGISKYKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    月途中受給者証変更時負担計算対象区分コンボボックスチェック処理
      *****************************************************************
       4200-KGISKYKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KGISKYKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KGISKYKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KGISKYKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-KGISKYKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W66"               TO  SPA-MOTOPG
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
           INITIALIZE  SPA-GMN-AREA
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       430-DEL-SEC                 SECTION.
      *
           MOVE    "3002"              TO  SPA-WIDCD
      *
           .
       430-DEL-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       4301-DEL-SEC                SECTION.
      *
           INITIALIZE                      SYS-4100-REC
           MOVE    SPA-HOSPNUM         TO  SYS-4100-HOSPNUM
           MOVE    "4100"              TO  SYS-4100-KANRICD
           MOVE    "*"                 TO  SYS-4100-KBNCD
           MOVE    SPA-W01-SEL-STYUKYMD
                                       TO  SYS-4100-STYUKYMD
           MOVE    SPA-W01-SEL-EDYUKYMD
                                       TO  SYS-4100-EDYUKYMD
           MOVE    SYS-4100-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "2003"          TO  SPA-ERRCD
               GO  TO  4301-DEL-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
      *
           .
       4301-DEL-EXT.
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
           INITIALIZE                  SYS-4100-REC
      *
           MOVE    SPA-HOSPNUM         TO  SYS-4100-HOSPNUM
           MOVE    "4100"              TO  SYS-4100-KANRICD
           MOVE    "*"                 TO  SYS-4100-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-4100-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-4100-EDYUKYMD
           MOVE    SPA-GMN-KGIIRYOCD   TO  SYS-4100-KGIIRYOCD
           MOVE    SPA-GMN-KGISKYKBN   TO  SYS-4100-KGISKYKBN
           MOVE    SYS-4100-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYS-4100-REC        TO  SYSKANRI-REC
               MOVE    SYS-4100-CREYMD     TO  SYS-CREYMD
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
               MOVE    SYS-4100-HOSPNUM    TO  SYSUP-HOSPNUM
               MOVE    SYS-4100-KANRICD    TO  SYSUP-KANRICD
               MOVE    SYS-4100-KBNCD      TO  SYSUP-KBNCD
               MOVE    SYS-4100-STYUKYMD   TO  SYSUP-STYUKYMD
               MOVE    SYS-4100-EDYUKYMD   TO  SYSUP-EDYUKYMD
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "2001"      TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    SYS-4100-REC        TO  SYSKANRI-REC
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  SYS-CREYMD
                                               SYS-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "2002"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
      *
           .
       4801-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    共通関連チェック
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
      **   IF    ( SPA-GMN-KGIIRYOCD   =   SPACE )
      **       MOVE    "1001"      TO  SPA-ERRCD
      **       MOVE    1           TO  SPA-GMN-CUR
      **   END-IF
      *
      **   IF    ( SPA-ERRCD           =   SPACE )
      **       IF    ( SPA-GMN-KGISKYKBN-G =   SPACE )
      **           MOVE    "1002"  TO  SPA-ERRCD
      **           MOVE    2       TO  SPA-GMN-CUR
      **       END-IF
      **   END-IF
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
           MOVE    "W66    "           TO  MCP-WINDOW.
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
           MOVE    SPACE               TO  W66
           INITIALIZE                      W66
      *
           MOVE    SPA-W01-STYUKYMDH   TO  W66-YUKOSTYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  W66-YUKOEDYMD
      *
           MOVE    SPA-GMN-KGIIRYOCD   TO  W66-KGIIRYOCD
           MOVE    SPA-GMN-KGISKYKBN-G TO  W66-KGISKYKBN
      *
           MOVE    SPA-GMN-KGISKYKBN-MAX   TO  W66-KGISKYKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KGISKYKBN-MAX )
                   MOVE    SPA-GMN-KGISKYKBNLST (IDX-CMB)
                                   TO  W66-KGISKYKBN-ITEM (IDX-CMB)
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
           MOVE    WIDGET-INSENSITIVE  TO  W66-B03-STATE
      *
           IF    ( SPA-W01-SELNUM      >   ZERO )
               MOVE    WIDGET-NORMAL   TO  W66-B03-STATE
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
               MOVE    "KGIIRYOCD"         TO  MCP-WIDGET
           WHEN    002
               MOVE    "KGISKYKBN"         TO  MCP-WIDGET
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
           WHEN    "KGIIRYOCD"
               MOVE    002                 TO  SPA-GMN-CUR
           WHEN    "KGISKYKBN"
               MOVE    212                 TO  SPA-GMN-CUR
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
               MOVE    "公害医療機関コード入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "公害診療報酬請求書区分入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "公害医療機関コードを入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "公害診療報酬請求書区分を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "2001"
           WHEN    "2002"
           WHEN    "2003"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W66"               TO  SPA-MOTOPG
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
           WHEN    "3002"
               MOVE    "削除します。よろしいですか"
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
           MOVE    "W66"                TO  SPA-MOTOPG
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
      *    全角半角混在チェック
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-INPUT
                                   =   SPACE )
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-INPUT
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-INPUT
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    9           TO  FLG-ENTRYCHK
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    KANACHK-SYUBETU TO  FLG-ENTRYCHK
      *
           .
       800-ENRTRY-CHK-EXT.
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
      *    システム管理マスタ検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
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
           CALL    "ORCDBMAIN"      USING      MCPAREA
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
           CALL    "ORCDBMAIN"      USING      MCPAREA
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
           CALL    "ORCDBMAIN"      USING      MCPAREA
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
           CALL    "ORCDBMAIN"      USING      MCPAREA
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
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
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
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

