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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGW33.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 印刷データ等削除機能情報（Ｗ３３）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/05/25    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.01.01    NACL-藤原    07/11/02  期間変更時のデータの初期化
      *  04.08.00    NACL-太田    15/03/06  入院オーダー追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-W33.
           03  SPA-W33-AREA.
               05  SPA-GMN-CUR         PIC 9(02).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-STYUKYMD       PIC X(09).
                   07  SPA-GMN-EDYUKYMD       PIC X(09).
                   07  SPA-GMN-KOUMOKU.
                       09  SPA-GMN-MONTHLY    PIC 9(03).
                       09  SPA-GMN-DAILY      PIC 9(03).
                       09  SPA-GMN-NORDER     PIC 9(03).
      *
               05  SPA-NAI-AREA.
                   07  SPA-NAI-STYUKYMD       PIC 9(08).
                   07  SPA-NAI-EDYUKYMD       PIC 9(08).
                   07  SPA-NAI-KOUMOKU.
                       09  SPA-NAI-8000-MONTHLY
                                              PIC 9(03).
                       09  SPA-NAI-8000-DAILY PIC 9(03).
                       09  SPA-NAI-8000-NORDER
                                              PIC 9(03).
      *
           COPY    "ENUM-VALUE".
      *
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-WIDMSG          PIC X(70). 
           03  WRK-YMD             PIC X(09). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
      *    印刷データ等削除機能情報
           COPY  "CPSK8000.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
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
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W33
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W33         TO  SPA-FREE
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   ZERO
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      * 
               MOVE    SPACE               TO  MCP-PUTTYPE
               MOVE    "W33"               TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
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
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF      SPA-WID1-FLG    =   "OK"
               PERFORM 490-KOUSHIN-SEC
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD      
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                      SPA-W33-AREA
      *
           MOVE    SPA-W01-STYUKYMDH   TO  SPA-GMN-STYUKYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  SPA-GMN-EDYUKYMD
      *
           INITIALIZE                      SPA-GMN-KOUMOKU
      * 
           MOVE    SPACE               TO  SYS-8000-REC
           INITIALIZE                      SYS-8000-REC
           MOVE    "8000"              TO  SYS-8000-KANRICD
           MOVE    "*"                 TO  SYS-8000-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-8000-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-8000-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-8000-HOSPNUM
           MOVE    SYS-8000-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-8000-REC
               MOVE    SYS-8000-DAILY      TO  SPA-GMN-DAILY
                                               SPA-NAI-8000-DAILY 
               MOVE    SYS-8000-MONTHLY    TO  SPA-GMN-MONTHLY
                                               SPA-NAI-8000-MONTHLY
               MOVE    SYS-8000-NORDER     TO  SPA-NAI-8000-NORDER
               IF    ( SYS-8000-NORDER     IS  NUMERIC )
                   MOVE    SYS-8000-NORDER TO  SPA-GMN-NORDER
               ELSE
                   MOVE    36              TO  SPA-GMN-NORDER
               END-IF
           ELSE
               MOVE    1                   TO  SPA-GMN-DAILY
               MOVE    36                  TO  SPA-GMN-MONTHLY
               MOVE    36                  TO  SPA-GMN-NORDER
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
           . 
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    W33-DAILY       TO  SPA-GMN-DAILY
           MOVE    W33-MONTHLY     TO  SPA-GMN-MONTHLY 
           MOVE    W33-NORDER      TO  SPA-GMN-NORDER
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC             SECTION.
      *
           PERFORM 41021-DAILY-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41022-MONTHLY-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41023-NORDER-CHK-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科名チェック
      *****************************************************************
       41021-DAILY-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-DAILY       =   ZERO
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "0001"              TO  SPA-ERRCD
           END-IF
           .
       41021-DAILY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科名１チェック処理
      *****************************************************************
       41022-MONTHLY-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-MONTHLY     =   ZERO
               MOVE    2                   TO  SPA-GMN-CUR
               MOVE    "0002"              TO  SPA-ERRCD
           END-IF
           .
       41022-MONTHLY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院オーダーチェック処理
      *****************************************************************
       41023-NORDER-CHK-SEC            SECTION.
      *
           IF      SPA-GMN-NORDER     =   ZERO
               MOVE    3                   TO  SPA-GMN-CUR
               MOVE    "0003"              TO  SPA-ERRCD
           END-IF
      *
           .
       41023-NORDER-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W33"               TO  SPA-MOTOPG
      *
      *     INITIALIZE                      SPA-W01KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
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
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC              SECTION.
      *
           PERFORM 4900-KANRENCHK-SEC
           IF      SPA-ERRCD        =   SPACE
              MOVE    "1001"           TO  SPA-WIDCD
           END-IF
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
      *    マシン日付取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    SPACE               TO  SYS-8000-REC
           INITIALIZE                      SYS-8000-REC
           MOVE    "8000"              TO  SYS-8000-KANRICD
           MOVE    "*"                 TO  SYS-8000-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-8000-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-8000-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-8000-HOSPNUM
           MOVE    SYS-8000-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-8000-REC
           ELSE
               MOVE    SPACE               TO  SYS-8000-REC
               INITIALIZE                      SYS-8000-REC
               MOVE    "8000"              TO  SYS-8000-KANRICD
               MOVE    "*"                 TO  SYS-8000-KBNCD
grpsys         MOVE    SPA-HOSPNUM         TO  SYS-8000-HOSPNUM
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD    TO  SYS-8000-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-8000-EDYUKYMD
           MOVE    SPA-GMN-DAILY       TO  SYS-8000-DAILY
           MOVE    SPA-GMN-MONTHLY     TO  SYS-8000-MONTHLY
           MOVE    SPA-GMN-NORDER      TO  SYS-8000-NORDER
      *     
           IF      FLG-SYSKANRI        =   ZERO
               IF      SPA-GMN-DAILY   NOT =   SPA-NAI-8000-DAILY
                   MOVE    SPACE           TO  SYS-8000-DAILY-YM
                   MOVE    SMCNDATE-YMD    TO  SYS-8000-DAILY-UPYMD
               END-IF
               IF      SPA-GMN-MONTHLY NOT =   SPA-NAI-8000-MONTHLY
                   MOVE    SPACE           TO  SYS-8000-MONTHLY-YM
                   MOVE    SMCNDATE-YMD    TO  SYS-8000-MONTHLY-UPYMD
               END-IF
               IF      SPA-GMN-NORDER  NOT =   SPA-NAI-8000-NORDER
                   MOVE    SPACE           TO  SYS-8000-NORDER-YM
                   MOVE    SMCNDATE-YMD    TO  SYS-8000-NORDER-UPYMD
               END-IF
               MOVE    SYS-8000-REC        TO  SYSKANRI-REC
               MOVE    SMCNDATE-YMD        TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
               MOVE    SYS-KANRICD         TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD           TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD
                                           TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD
                                           TO  SYSUP-EDYUKYMD
grpsys         MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
      *
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
           ELSE
               MOVE    SYS-8000-REC        TO  SYSKANRI-REC
               MOVE    SMCNDATE-YMD        TO  SYS-CREYMD
                                               SYS-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
      *
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
           END-IF
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1003"              TO  SPA-ERRCD
               GO  TO  490-KOUSHIN-EXT
           END-IF      
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC           SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRCD
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "W33"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPA-GMN-STYUKYMD    TO  W33-YUKOSTYMD
           MOVE    SPA-GMN-EDYUKYMD    TO  W33-YUKOEDYMD
      * 
           MOVE    SPA-GMN-DAILY       TO  W33-DAILY
           MOVE    SPA-GMN-MONTHLY     TO  W33-MONTHLY
           MOVE    SPA-GMN-NORDER      TO  W33-NORDER
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
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
                   MOVE    "１ヵ月以上で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "１ヵ月以上で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "更新ができませんでした"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W33"               TO  SPA-MOTOPG
           MOVE    "WERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
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
           MOVE    SPACE               TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "印刷データ等削除機能情報を登録します"
                                       TO  WRK-WIDMSG
               WHEN    "1002"
                   MOVE    "印刷データ等削除機能情報を削除します"
                                       TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WID1
           INITIALIZE                      WID1
           MOVE    SPA-WIDCD           TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG          TO  WID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "W33"               TO  SPA-MOTOPG
           MOVE    "WID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END              
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理    
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-GMN-CUR         =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
                WHEN    1
                    MOVE  "DAILY"          TO  MCP-WIDGET
                WHEN    2
                    MOVE  "MONTHLY"        TO  MCP-WIDGET
                WHEN    3
                    MOVE  "NORDER"         TO  MCP-WIDGET
                WHEN    99
                    MOVE  "B12"            TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "DAILY"
                   MOVE    2               TO  SPA-GMN-CUR
               WHEN    "MONTHLY"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "NORDER"
                   MOVE    99              TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                  SECTION.
      *
           MOVE   "PUTWINDOW"           TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
