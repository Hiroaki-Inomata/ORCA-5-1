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
      *
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGQ04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者照会
      *  コンポーネント名  : 検索条件保存画面
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07.07.20    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "Q01SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-SHOUKAIKANRI            PIC 9(01).
           03  FLG-KANACHK-ERR             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
      *
      *    作業領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-KANACHK-STR             PIC X(100).
           03  WRK-RENNUM                  PIC 9(03).
           03  WRK-LBLRENNUM.
               05  WRK-LBLRENNUM-ST        PIC Z.
               05  WRK-LBLRENNUM-SEP       PIC X(04).
               05  WRK-LBLRENNUM-ED        PIC ZZZ.
               05  WRK-LBLRENNUM-MSG       PIC X(20).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者照会検索条件管理情報
       01  SHOUKAIKANRI-REC.
           COPY    "CPSHOUKAIKANRI.INC".
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
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
        LINKAGE                     SECTION.
      *
      *     テーブルアクセス
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA13SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *****************************************************************
      *    主    処    理
      *****************************************************************
      *
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-Q01FREE
      *
           MOVE    SPACE           TO  SPA-ERRCD.
           MOVE    ZERO            TO  FLG-END.
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
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF  FLG-END   =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF.
      *
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-Q01FREE     TO  SPA-FREE
           .
      *
       000-PROC-SEC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                   SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "QERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
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
           MOVE    "Q04"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
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
           INITIALIZE                  SPA-Q04
      *
           MOVE    SPA-Q01-Q04-RENNUM
                               TO  SPA-GMN-Q04-RENNUM
      *
           MOVE    SPA-Q01-Q04-NAME
                                   TO  SPA-GMN-Q04-NAME
      *
           IF    ( SPA-Q01-Q04-TITLEKBN    =   "0" )
               MOVE    "F"     TO  SPA-GMN-Q04-TITLEKBN
           ELSE
               MOVE    "T"     TO  SPA-GMN-Q04-TITLEKBN
           END-IF
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               MOVE   SPACE            TO  SPA-Q01-Q04-NAME
               PERFORM 210-BACK
           WHEN    "CLICKED"       ALSO    "TITLEKBN"
               PERFORM 4100-TITLEKBN-CHK-SEC
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 220-TOUROKU-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
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
           MOVE    ZERO        TO      SPA-GMN-Q04-CUR
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
           MOVE    Q04-RENNUM      TO  SPA-GMN-Q04-RENNUM
           MOVE    Q04-NAME        TO  SPA-GMN-Q04-NAME
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
      *    表示順番チェック処理
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-RENNUM-CHK-SEC
           END-IF
      *
      *    登録名チェック処理
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-NAME-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    表題区分チェック処理
      *****************************************************************
       4100-TITLEKBN-CHK-SEC               SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    Q04-TITLEKBN    TO  SPA-GMN-Q04-TITLEKBN
      *
           MOVE    212             TO  SPA-GMN-Q04-CUR
      *
           .
       4100-TITLEKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    表題順番チェック処理
      *****************************************************************
       4101-RENNUM-CHK-SEC               SECTION.
      *
           EVALUATE    TRUE
           WHEN      ( SPA-GMN-Q04-RENNUM  <   1 )
               MOVE    1               TO  SPA-GMN-Q04-RENNUM
           WHEN      ( SPA-GMN-Q04-RENNUM  >   SPA-Q01-Q04-RENNUM-MAX )
               MOVE    SPA-Q01-Q04-RENNUM-MAX
                                       TO  SPA-GMN-Q04-RENNUM
           END-EVALUATE
      *
           .
       4101-RENNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    登録名チェック処理
      *****************************************************************
       4101-NAME-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-Q04-NAME    TO  WRK-KANACHK-STR
           PERFORM 800-ZENKAKU-CHK-SEC
           MOVE    WRK-KANACHK-STR     TO  SPA-GMN-Q04-NAME
           IF    ( FLG-KANACHK-ERR     NOT =   ZERO )
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-Q04-CUR
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 900-SHOUKAIKANRI-KEY3-SEL-SEC
               IF    ( FLG-SHOUKAIKANRI    =   ZERO )
                   MOVE    "0002"  TO  SPA-ERRCD
                   MOVE    1       TO  SPA-GMN-Q04-CUR
               END-IF
           END-IF
      *
           .
       4101-NAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       4900-KANREN-CHK-SEC             SECTION.
      *
           IF    ( SPA-ERRCD              =    SPACE )
               IF    ( SPA-GMN-Q04-NAME   =    SPACE )
                   MOVE    "1001" TO   SPA-ERRCD
                   MOVE    1      TO   SPA-GMN-Q04-CUR
               END-IF
           END-IF
      *
           .
       4900-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "Q01"               TO  SPA-SAKIPG
           MOVE    "Q04"               TO  SPA-MOTOPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       220-TOUROKU-SEC                 SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    (  SPA-ERRCD      =   SPACE )
               PERFORM 4900-KANREN-CHK-SEC
           END-IF
      *
           IF    (  SPA-ERRCD      =   SPACE )
               MOVE    SPA-GMN-Q04-RENNUM
                                   TO  SPA-Q01-Q04-RENNUM
               MOVE    SPA-GMN-Q04-NAME
                                   TO  SPA-Q01-Q04-NAME
               IF    ( SPA-GMN-Q04-TITLEKBN    =   "T" )
                   MOVE    "1"     TO  SPA-Q01-Q04-TITLEKBN
               ELSE
                   MOVE    "0"     TO  SPA-Q01-Q04-TITLEKBN
               END-IF
               MOVE    "CHANGE"    TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
      *
           .
      *
       220-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Q04    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE           TO  Q04
           INITIALIZE                  Q04
      *
      *    登録名
           MOVE    SPA-GMN-Q04-NAME    TO  Q04-NAME
      *
      *    表題順番
           MOVE    SPA-GMN-Q04-LBLRENNUM
                                       TO  Q04-LBLRENNUM
           MOVE    SPA-GMN-Q04-RENNUM
                                       TO  Q04-RENNUM
      *
      *    表題区分
           MOVE    SPA-GMN-Q04-TITLEKBN
                                      TO  Q04-TITLEKBN
      *
           IF    ( SPA-Q01-Q04-UPDKBN =   "INS" )
               MOVE    "条件保存"     TO  Q04-TITLE
           ELSE
               MOVE    "条件変更"     TO  Q04-TITLE
           END-IF
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
           MOVE    "T"                 TO  Q04-RENNUM-VISIBLE
           MOVE    WIDGET-NORMAL       TO  Q04-RENNUM-STATE
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC              SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-Q04-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-SKIP-CUR-SEC
      *
           EVALUATE    SPA-GMN-Q04-CUR
           WHEN    001
               MOVE    "NAME"          TO  MCP-WIDGET
           WHEN    002
               MOVE    "RENNUM"        TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
      *
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "RENNUM"
               MOVE    001             TO  SPA-GMN-Q04-CUR
           WHEN    "NAME"
               MOVE    212             TO  SPA-GMN-Q04-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5012-SKIP-CUR-SEC               SECTION.
      *
           IF    ( SPA-GMN-Q04-CUR     =   2 )
               IF    ( Q04-RENNUM-VISIBLE  =   "F" )
                   MOVE    1           TO  SPA-GMN-Q04-CUR
               END-IF
           END-IF
      *
           .
      *
       5012-SKIP-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "登録名は全角で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "同じ登録名が既に存在します"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "登録名を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "4001"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  QERR
           MOVE    SPA-ERRCD            TO  QERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  QERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "Q04"                TO  SPA-MOTOPG
           MOVE    "QERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "QERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRSET-EXT.
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
      *    患者照会検索条件管理情報検索処理
      *****************************************************************
       900-SHOUKAIKANRI-KEY3-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SHOUKAIKANRI
      *
           INITIALIZE                      SHOUKAIKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SHOKANRI-HOSPNUM
           MOVE    SPA-Q01-Q04-UID     TO  SHOKANRI-UID
           MOVE    SPA-GMN-Q04-NAME    TO  SHOKANRI-NAME
           MOVE    "0"                 TO  SHOKANRI-NYUGAIKBN
           MOVE    SHOUKAIKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_shoukaikanri"  TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC     TO  SHOUKAIKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SHOUKAIKANRI
               INITIALIZE                  SHOUKAIKANRI-REC
           END-IF
      *
           MOVE    "tbl_shoukaikanri"  TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SHOUKAIKANRI-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索条件管理情報検索処理
      *****************************************************************
       900-SHOUKAIKANRI-KEY4-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SHOUKAIKANRI
      *
           INITIALIZE                      SHOUKAIKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SHOKANRI-HOSPNUM
           MOVE    "0"                 TO  SHOKANRI-NYUGAIKBN
           MOVE    SHOUKAIKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_shoukaikanri"  TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC     TO  SHOUKAIKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SHOUKAIKANRI
               INITIALIZE                  SHOUKAIKANRI-REC
           END-IF
      *
           MOVE    "tbl_shoukaikanri"  TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SHOUKAIKANRI-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
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
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
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
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

