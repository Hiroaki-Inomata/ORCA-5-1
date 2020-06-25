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
       PROGRAM-ID.                               ORCGI041.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院業務
      *  コンポーネント名  : 院外処方せんドクター設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/08/27    NACL−多々納  新規作成
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
       DATA                                      DIVISION.
      *FILE                                      SECTION.
      *
      *
       WORKING-STORAGE                           SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    入退院登録共通領域
           COPY    "I0COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "I0SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-TOROKU             PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDX-D                   PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-MEI           PIC X(20).
           03  WRK-DRCD                PIC X(04).
      *    入力項目名
           03  WRK-MCP-WIDGET              PIC X(64).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    ドクター
           COPY    "CPSK1010.INC".
      *    診療科コード
           COPY    "CPSK1005.INC".
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *     
           COPY    "CPORCMCP.INC".  
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                                  SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA31SCRAREA.INC".
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
           MOVE    SPA-FREE        TO  SPA-I0FREE
           MOVE    SPA-WORK        TO  SPA-I0KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
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
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-I0KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I0FREE      TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                              SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "I0ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE
           MOVE   "I041"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
      *
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
           IF      SPA-NAI-I041-KBN    =   ZERO
      *        初期設定
               PERFORM 310-SPA-SET-SEC
           END-IF
           MOVE    1                   TO  SPA-NAI-I041-KBN
           MOVE    1                   TO  SPA-GMN-I041-CUR
           .
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                 SECTION.
      *
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   5   )
                         OR   (SPA-NAI-I041-SRYKA (IDX-D)  =   SPACE )
               MOVE    SPA-NAI-I041-SRYKA (IDX-D)
                                           TO  WRK-SRYKA
               PERFORM 3101-SRYKA-MEI-SEC
               MOVE    WRK-SRYKA-MEI       TO  SPA-GMN-I041-SRYKA-MEI
                                                             (IDX-D)
               MOVE    IDX-D               TO  SPA-GMN-I041-MAX
               IF      SPA-NAI-I041-DRCD (IDX-D)   =   SPACE
                   MOVE    SPA-GMN-I04-DR-CD
                                           TO  SPA-NAI-I041-DRCD (IDX-D)
               END-IF
           END-PERFORM
           INITIALIZE                  SPA-GMN-I041-AREA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-I041-MAX )
               MOVE    SPA-NAI-I041-DRCD (IDX)
                                       TO  SPA-GMN-I041-DRCD (IDX)
               MOVE    SPA-GMN-I041-DRCD (IDX) TO  WRK-DRCD
               PERFORM 41021-DRCD-CHK-SEC
           END-PERFORM
           .
       310-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    診療科名編集処理
      *****************************************************************
       3101-SRYKA-MEI-SEC                           SECTION.
      *
      *    入院科
           INITIALIZE                      SYS-1005-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    SPA-I01-I04-KJNYMD  TO  SYS-1005-STYUKYMD
                                           SYS-1005-EDYUKYMD
           MOVE    SYS-1005-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1005-REC
      *
           MOVE    SYS-1005-SRYKANAME  TO  WRK-SRYKA-MEI
      *
           .
        3101-SRYKA-MEI-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                               SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 220-TOUROKU-SEC
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-I041-CUR
      *
           MOVE    ZERO                TO  FLG-TOROKU
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-I041-MAX
               MOVE    I041-DRCD (IDX)     TO  SPA-GMN-I041-DRCD-G (IDX)
           END-PERFORM
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-I041-MAX )
                          OR  (SPA-ERRCD   NOT =   SPACE    )
               MOVE    SPA-GMN-I041-DRCD (IDX) TO  WRK-DRCD
               PERFORM 41021-DRCD-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    IDX             TO  SPA-GMN-I041-CUR
               END-IF
           END-PERFORM
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ドクターコードチェック処理
      *****************************************************************
       41021-DRCD-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-I041-DRCD-MEI (IDX)
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   SPA-GMN-I04-DR-CNT )
                           OR (FLG-OK  =   1                  )
               IF      WRK-DRCD        =   SPA-GMN-I04-DRL (IDX-D)
                   MOVE    SPA-GMN-I04-DR-ITM (IDX-D)
                                           TO  SPA-GMN-I041-DRCD-G
                                                            (IDX)
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
      *        システム管理検索
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "1010"          TO  SYS-KANRICD
               MOVE    "1"             TO  SYS-KBNCD (1:1)
               MOVE    WRK-DRCD        TO  SYS-KBNCD (2:4)
               MOVE    SPA-I01-I04-SKYSTYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF      FLG-SYSKANRI    =   ZERO
                   MOVE    SYSKANRI-REC    TO  SYS-1010-REC
                   MOVE    SYS-1010-NAME   TO  SPA-GMN-I041-DRCD-MEI
                                                                (IDX)
                   MOVE    1               TO  FLG-OK
               END-IF
           END-IF
           IF      FLG-OK              =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
           .
       41021-DRCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       220-TOUROKU-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-MCP-WIDGET
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
      *        登録処理
               PERFORM 2201-TOUROKU-SYORI-SEC
           END-IF
           .
       220-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       2201-TOUROKU-SYORI-SEC            SECTION.
     *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-I041-MAX )
               MOVE    SPA-GMN-I041-DRCD (IDX) TO  SPA-NAI-I041-DRCD
                                                               (IDX)
           END-PERFORM
           MOVE    1                   TO  SPA-GMN-I041-FLG
      *    戻る
           PERFORM 210-BACK
           .
       2201-TOUROKU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                                  SECTION.
      *
           MOVE    "I04"               TO  SPA-SAKIPG
           MOVE    "I041"              TO  SPA-MOTOPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       210-BACK-EXT.
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
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I041"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  I041
           INITIALIZE                      I041
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-I041-MAX )
               MOVE    SPA-GMN-I041-SRYKA-MEI (IDX)
                                           TO  I041-SRYKA (IDX)
               MOVE    SPA-GMN-I041-DRCD-G (IDX)
                                           TO  I041-DRCD  (IDX)
               PERFORM VARYING     IDX-D   FROM    1   BY  1
                       UNTIL      (IDX-D   >   SPA-GMN-I04-DR-CNT )
                   MOVE    SPA-GMN-I04-DR-ITM (IDX-D)
                                           TO  I041-DRCD-ITEM
                                                            (IDX IDX-D)
                   MOVE    IDX-D           TO  I041-DRCD-COUNT (IDX)
               END-PERFORM
           END-PERFORM
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *    カーソル指定のない時、入力した項目の次ぎへ
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-I041-CUR    =   ZERO )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-I041-CUR
               WHEN    01
                   MOVE    "DRCD01"        TO  MCP-WIDGET
               WHEN    02
                   MOVE    "DRCD02"        TO  MCP-WIDGET
               WHEN    03
                   MOVE    "DRCD03"        TO  MCP-WIDGET
               WHEN    04
                   MOVE    "DRCD04"        TO  MCP-WIDGET
               WHEN    05
                   MOVE    "DRCD05"        TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "DRCD01"
                   MOVE    02              TO  SPA-GMN-I041-CUR
               WHEN    "DRCD02"
                   MOVE    03              TO  SPA-GMN-I041-CUR
               WHEN    "DRCD03"
                   MOVE    04              TO  SPA-GMN-I041-CUR
               WHEN    "DRCD04"
                   MOVE    05              TO  SPA-GMN-I041-CUR
               WHEN    "DRCD05"
                   MOVE    99              TO  SPA-GMN-I041-CUR
           END-EVALUATE
           IF      SPA-GMN-I041-CUR    >   SPA-GMN-I041-MAX
               MOVE    99              TO  SPA-GMN-I041-CUR
           END-IF
           .
       50011-INPUT-CUR-EXT.
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
               MOVE    "ドクターコードエラー"    TO  SPA-ERRMSG
           WHEN    OTHER
               MOVE    SPA-ERRCD           TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  I0ERR
           MOVE    SPA-ERRCD           TO  I0ERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  I0ERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "I041"              TO  SPA-MOTOPG
           MOVE    "I0ERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I0ERR"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC    =     ZERO )
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               ELSE
                   INITIALIZE              SYSKANRI-REC
                   MOVE    1           TO  FLG-SYSKANRI
               END-IF
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYSKANRI-KEY10-SEL-EXT.
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
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
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
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                            SECTION.
      *
           MOVE   "PUTWINDOW"                    TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA.
      *
       900-PUT-WINDOW-EXT.
           EXIT.
      *
