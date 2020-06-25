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
       PROGRAM-ID.                 ORCGQ03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者照会
      *  コンポーネント名  : 他業務連携確認画面（Ｑ０３）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/06/11    MCC−太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03/06/10 一括入力対応
      *  03.05.00    NACL-太田    07.05.23 グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "Q01SCR-SPA".
      *
      *    コンスタント値
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-HIZUKE-ERR      PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
           03  IDX-CMB             PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-SYMD            PIC X(10).
           03  WRK-HENYMDG         PIC X(10).
           03  WRK-CMB-CD          PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-IDX     PIC 9(05).
               05  WRK-CMB-ITM     PIC X(100).
      *
       01  CONST-AREA.
           03  CONST-H1504         PIC X(06)   VALUE "200304".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
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
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-Q01FREE
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
           IF  ( FLG-END           =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-Q01FREE     TO  SPA-FREE
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
                                       IDX-AREA
                                       WRK-AREA
      *
      *    エラー画面より
           IF  ( SPA-MOTOPG            =   "QERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "Q03"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
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
           INITIALIZE                      SPA-Q03
      *
           MOVE    212                     TO  SPA-GMN-Q03-CUR
      *
           IF    ( SPA-Q02-Q03-LNKKBN      =   "1" )
               PERFORM 3101-COMBO-SYOKI-SEC
      *
               MOVE    SPA-Q02-Q03-SRYYM   TO  WRK-SYMD
               PERFORM 800-NENGETU-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-Q03-SRYYM
               MOVE    WRK-SYMD            TO  SPA-NAI-Q03-SRYYM
      *
               MOVE    SPA-Q02-Q03-NYUGAIKBN
                                           TO  WRK-CMB-CD
               PERFORM 4200-NYUGAIKBNLST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-Q03-NYUGAIKBN-G
      *
           END-IF
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    コンボボックス初期処理
      *****************************************************************
       3101-COMBO-SYOKI-SEC            SECTION.
      *
           PERFORM 3101-NYUGAIKBNLST-SET-SEC
      *
           .
       3101-COMBO-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    入外区分コンボ編集処理
      *****************************************************************
       3101-NYUGAIKBNLST-SET-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-Q03-NYUGAIKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-Q03-NYUGAIKBNLST-MAX
      *
           MOVE    "1"             TO  SPA-GMN-Q03-NYUGAIKBNL (1)
      *
           MOVE    "2"             TO  SPA-GMN-Q03-NYUGAIKBNL (2)
      *
           MOVE    "入院"          TO  SPA-GMN-Q03-NYUGAIKBNMEIL (1)
      *
           MOVE    "外来"          TO  SPA-GMN-Q03-NYUGAIKBNMEIL (2)
      *
           .
      *
       3101-NYUGAIKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           MOVE    MCP-WIDGET          TO        WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               PERFORM 210-BACK
      *    確定
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 490-KAKUTEI
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           MOVE    MCP-WIDGET          TO        WRK-MCP-WIDGET
      *
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *
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
       400-GMN-INPUT-SEC           SECTION.
      *
           MOVE    ZERO        TO      SPA-GMN-Q03-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC           SECTION.
      *
      *    画面をＳＰＡにセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           IF    ( SPA-Q02-Q03-LNKKBN      =   "1" )
               IF    ( SPA-ERRCD   =   SPACE )
      *            診療年月チェック
                   PERFORM 4100-SRYYM-CHK-SEC
               END-IF
      *
               IF    ( SPA-ERRCD   =   SPACE )
      *            入外区分チェック
                   PERFORM 4100-NYUGAIKBN-CHK-SEC
               END-IF
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC        SECTION.
      *
      *    診療年月
           MOVE    Q03-SRYYM      TO  SPA-GMN-Q03-SRYYM
      *
      *    入外区分
           MOVE    Q03-NYUGAIKBN  TO  SPA-GMN-Q03-NYUGAIKBN-G
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    診療年月チェック
      *****************************************************************
       4100-SRYYM-CHK-SEC              SECTION.
      *
           MOVE    SPA-GMN-Q03-SRYYM   TO  WRK-SYMD
           PERFORM 800-NENGETU-HEN-SEC
           IF    ( FLG-HIZUKE-ERR      =   ZERO )
               MOVE    WRK-SYMD        TO  SPA-NAI-Q03-SRYYM
               MOVE    WRK-HENYMDG     TO  SPA-GMN-Q03-SRYYM
               IF    ( SPA-NAI-Q03-SRYYM   >=  CONST-H1504 )
                   CONTINUE
               ELSE
                   MOVE    "0002"      TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-Q03-CUR
           END-IF
      *
           .
       4100-SRYYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外区分チェック
      *****************************************************************
       4100-NYUGAIKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-Q03-NYUGAIKBN   TO  WRK-CMB-CD
           PERFORM 4200-NYUGAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-Q03-NYUGAIKBN-G
      *
           .
       4100-NYUGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外区分コンボチェック処理
      *****************************************************************
       4200-NYUGAIKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-Q03-NYUGAIKBNLST-MAX)
                    OR   ( WRK-CMB-ITM 
                                   NOT =   SPACE )
      *
               IF    ( SPA-GMN-Q03-NYUGAIKBNL (IDX-CMB)  =   WRK-CMB-CD)
                   MOVE    SPA-GMN-Q03-NYUGAIKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NYUGAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    ZERO                TO  SPA-Q02-Q03-RTOK-FLG
      *
           MOVE    SPA-Q01-SAKIPG      TO  SPA-SAKIPG
           MOVE    "Q03"               TO  SPA-MOTOPG
      *
      *****MOVE    "BACK"              TO  MCP-PUTTYPE
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
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
      *    確定　処理
      *****************************************************************
       490-KAKUTEI                 SECTION.
      *
      *    入力時チェック
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  490-KAKUTEI-EXT
           END-IF
      *
           MOVE    SPA-NAI-Q03-SRYYM   TO  SPA-Q02-Q03-SRYYM
           MOVE    SPA-GMN-Q03-NYUGAIKBN
                                       TO  SPA-Q02-Q03-NYUGAIKBN
      *
           MOVE    1                   TO  SPA-Q02-Q03-RTOK-FLG
      *
           MOVE    SPA-Q01-SAKIPG      TO  SPA-SAKIPG
           MOVE    "Q03"               TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-Q01-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-KAKUTEI-EXT.
           EXIT.
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Q03"               TO  MCP-WINDOW.
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
           INITIALIZE                  Q03
      *
           MOVE    "3101"                  TO  Q03-ID1CODE
      *
           IF    ( SPA-Q02-Q03-LNKKBN      =   "1" )
               MOVE    "明細書連携データを作成します"
                                           TO  Q03-ID1MSG
               MOVE    "診療年月"          TO  Q03-LBLSRYYM
           ELSE
               MOVE    "月次統計連携データを作成します"
                                           TO  Q03-ID1MSG
           END-IF
      *
           MOVE    SPA-GMN-Q03-SRYYM       TO  Q03-SRYYM
      *
           MOVE    SPA-GMN-Q03-NYUGAIKBN-G TO  Q03-NYUGAIKBN
           MOVE    SPA-GMN-Q03-NYUGAIKBNLST-MAX
                                           TO  Q03-CMB-NYUGAIKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-Q03-NYUGAIKBNLST-MAX )
      *
               MOVE    SPA-GMN-Q03-NYUGAIKBNLST (IDX1)
                                           TO  Q03-CMB-NYUGAIKBN-ITEM
                                                                (IDX1)
           END-PERFORM
      *
      *    項目状態編集
           PERFORM 5001-GMNHEN-STATE-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面項目状態編集処理
      *****************************************************************
       5001-GMNHEN-STATE-SEC           SECTION.
      *
           MOVE    WIDGET-NORMAL           TO  Q03-SRYYM-STATE
                                               Q03-NYUGAIKBN-STATE
                                               Q03-NYUGAIKBN-ITEM-STATE
           IF    ( SPA-Q02-Q03-LNKKBN      =   "1" )
               CONTINUE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  Q03-SRYYM-STATE
                                               Q03-NYUGAIKBN-STATE
                                               Q03-NYUGAIKBN-ITEM-STATE
           END-IF
      *
           .
       5001-GMNHEN-STATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-Q03-CUR    =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-SKIP-CUR-SEC
      *
           EVALUATE    SPA-GMN-Q03-CUR
           WHEN   001
               MOVE  "SRYYM"       TO  MCP-WIDGET
           WHEN   002
               MOVE  "NYUGAIKBN"   TO  MCP-WIDGET
           WHEN   212
               MOVE  "B12"         TO  MCP-WIDGET
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
           WHEN    "SRYYM"
               MOVE    002         TO  SPA-GMN-Q03-CUR
           WHEN    "NYUGAIKBN"
               MOVE    212         TO  SPA-GMN-Q03-CUR
      *
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5012-SKIP-CUR-SEC           SECTION.
      *
           IF    ( SPA-GMN-Q03-CUR =   1   )
               IF  ( Q03-SRYYM-STATE   =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-Q03-CUR =   2
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-Q03-CUR =   2   )
               IF  ( Q03-NYUGAIKBN-STATE   =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-Q03-CUR =   212
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
               MOVE    "診療年月入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "平成１５年４月以降を指定してください"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  QERR
           MOVE    SPA-ERRCD            TO  QERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  QERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "Q03"               TO  SPA-MOTOPG
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
      *
      *****************************************************************
      *    年月日本語変換処理
      *****************************************************************
       800-NENGETU-HEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-HIZUKE-ERR
           MOVE    SPACE               TO  WRK-HENYMDG
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    "1"                 TO  SGDAY-INTYPE
           MOVE    WRK-SYMD            TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY (1:6)
                                       TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY  (1:6)
                                       TO  WRK-SYMD
           ELSE
               MOVE    1               TO  FLG-HIZUKE-ERR
           END-IF
      *
           .
       800-NENGETU-HEN-EXT.
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
      *
