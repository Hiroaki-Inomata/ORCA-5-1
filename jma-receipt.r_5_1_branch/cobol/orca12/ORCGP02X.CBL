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
       PROGRAM-ID.                 ORCGP02X.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録 
      *  コンポーネント名  : ＣＬＡＩＭ接続設定（Ｐ０２Ｘ）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/11/19    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  05/11/14  MONFUNC 対応 他
      *  03.05.00    NACL-多々納  07/05/XX  グループ診療対応
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
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *    患者登録全画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
      *
       01  INDEX-AREA.
           03  IDX                 PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-MCP-WIDGET          PIC X(64).
           03  WRK-GMN-SELNUM          PIC 9(02).
      *
      *    送信コード
       01  WRK-SOUSIN-AREA.
           03  FILLER              PIC X(02)   VALUE   "1 ".
           03  FILLER              PIC X(18)   VALUE   "ＥＵＣ".
      *
           03  FILLER              PIC X(02)   VALUE   "2 ".
           03  FILLER              PIC X(18)   VALUE   "Ｓ−ｊｉｓ".
      *
           03  FILLER              PIC X(02)   VALUE   "3 ".
           03  FILLER              PIC X(18)   VALUE   "ＵＴＦ−８".
      *
       01  WRK-COMBO-TBL           REDEFINES   WRK-SOUSIN-AREA.
           03  WRK-COMBO-TBLO          OCCURS   3.
               05  WRK-COMBO-SCODEL        PIC X(01).
               05  WRK-COMBO-FIL           PIC X(01).
               05  WRK-COMBO-SCODEMEIL     PIC X(18).
      *
       01  WRK-COMBO-SCODE-MAX         PIC 9(03)   VALUE   3.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *　　（ＣＬＡＩＭ接続）
           COPY    "CPSK9000.INC".
           COPY    "CPSK9001.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
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
           COPY    "ORCA12SCRAREA.INC".
      *    COPY    "P02.INC".
      *    COPY    "P02A.INC".
      *    COPY    "P02D.INC".
      *    COPY    "P02E.INC".
      *    COPY    "P02F.INC".
      *    COPY    "P014.INC".
      *    COPY    "P015.INC".
      *    COPY    "P98.INC".
      *    COPY    "P981.INC".
      *    COPY    "P0201.INC".
      *    COPY    "P982.INC".
      *    COPY    "P100.INC".
      *    COPY    "P02X.INC".
      *    COPY    "P02S.INC".
      *    COPY    "P02Q.INC".
      *    COPY    "P02Q1.INC".
      *    COPY    "P02Y.INC".
      *    COPY    "P02Y1.INC".
      *    COPY    "P02G.INC".
      *    COPY    "P02G2.INC".
      *    COPY    "P02K.INC".
      *    COPY    "P02H.INC".
      *    COPY    "P02T.INC".
      *    COPY    "PERR.INC".
      *    COPY    "PID1.INC".
      *    COPY    "PDMY01.INC".
      *    COPY    "CPAPIMOD.INC".
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
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
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
           END-EVALUATE
      *
      *    画面編集後、表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
      *
           END-IF
      *
           MOVE    SPA-P02KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA  TO  SPA-FREE
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
           IF      SPA-MOTOPG          =   "PERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5011-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "P02X"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    メインでクリア
      *****INITIALIZE                  SPA-P02X-AREA
      *
           MOVE    SPACE               TO  SYS-9001-REC
           INITIALIZE                  SYS-9001-REC
           MOVE    "9001"              TO  SYS-9001-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-9001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-9001-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-9001-HOSPNUM
           MOVE    SYS-9001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-9001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  SPA-P02X-GMN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   20   )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC           TO  SYS-9001-REC
               MOVE    SYS-9001-KBNCD (1:2)  TO  WRK-GMN-SELNUM     
               MOVE    WRK-GMN-SELNUM        TO  SPA-P02X-GMN-SELNOL
                                                                 (IDX)
               MOVE    SYS-9001-ADDRESS      TO  SPA-P02X-GMN-ADRL
                                                                 (IDX)
               MOVE    SYS-9001-PORT1        TO  SPA-P02X-GMN-PORTL
                                                                  (IDX)
               MOVE    SYS-9001-MEMO         TO  SPA-P02X-GMN-MEMOL
                                                                  (IDX)
               MOVE    IDX                   TO  SPA-P02X-GMN-MAX
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    送信コード
           MOVE    SPA-P02X-CODE       TO  SPA-GMN-SCODE
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-COMBO-SCODE-MAX
               IF      SPA-GMN-SCODE   =   WRK-COMBO-SCODEL (IDX)
                   MOVE    WRK-COMBO-TBLO (IDX)    TO  SPA-GMN-SCODE-G
               END-IF
           END-PERFORM
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *     戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    SPACE           TO  SPA-P02X-FLG
                   PERFORM 210-BACK
      *     登録のみ（送信しない）
               WHEN    "CLICKED"       ALSO    "B11"
                   MOVE    "NO"            TO  SPA-P02X-FLG
                   PERFORM 400-TOUROKU-SEC
      *     登録＆送信
               WHEN    "CLICKED"       ALSO    "B12"
                   MOVE    "OK"            TO  SPA-P02X-FLG
                   PERFORM 400-TOUROKU-SEC
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
           MOVE    ZERO                TO  SPA-P02X-CUR
      *
           MOVE    SPACE               TO  SPA-P02X-FLG
      *    入力チェック処理
           PERFORM 210-INPUT-CHK-SEC
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       210-INPUT-CHK-SEC       SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        行選択
               WHEN    ANY             ALSO    "LIST"
                   PERFORM 203-SELECT-SEC
           END-EVALUATE
      *
      *    画面をＳＰＡセット
           PERFORM 2101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 2102-KIHON-CHK-SEC
      *
           .
       210-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       2101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    P02X-SCODE          TO  SPA-GMN-SCODE-G
           MOVE    P02X-SELNUM         TO  SPA-P02X-GMN-SEL
      *
           .
       2101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       2102-KIHON-CHK-SEC       SECTION.
      *
      *    検索項目入力(確定)
           IF      SPA-ERRCD           =   SPACE
               PERFORM 201-SCODE-SEC
           END-IF
      *    番号選択入力
           IF      SPA-ERRCD           =   SPACE
               PERFORM 202-SELNUM-SEC
           END-IF
      *
           .
       2102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       201-SCODE-SEC                SECTION.
      *
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       WRK-COMBO-SCODE-MAX
               IF      SPA-GMN-SCODE-G(1:1)
                                       =   WRK-COMBO-SCODEL (IDX)
                   MOVE    WRK-COMBO-TBLO (IDX) 
                                           TO  SPA-GMN-SCODE-G
                   MOVE    WRK-COMBO-SCODE-MAX
                                           TO  IDX
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    1                   TO  SPA-P02X-CUR
               MOVE    "0001"              TO  SPA-ERRCD
           END-IF
      *
           .
       201-SCODE-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       202-SELNUM-SEC               SECTION.
      *
           IF      SPA-P02X-FLG        =   "OK"
               IF      SPA-P02X-GMN-SEL    =   ZERO
                   MOVE    "0002"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-P02X-CUR
               END-IF
           END-IF
      *
      *    選択番号チェック
           IF      SPA-P02X-GMN-SEL    >   SPA-P02X-GMN-MAX
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-P02X-CUR
           END-IF
      *
           .
       202-SELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面選択入力　処理
      *****************************************************************
       203-SELECT-SEC               SECTION.
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-P02X-GMN-MAX
               IF      P02X-LIST-SELECT(IDX)        =  "T"
                   MOVE    SPA-P02X-GMN-SELNOL (IDX)
                                           TO  SPA-P02X-GMN-SEL
               END-IF
           END-PERFORM
      *
           MOVE    SPA-P02X-GMN-SEL    TO  P02X-SELNUM
      *
           MOVE    5                   TO  SPA-P02X-CUR
           .
       203-SELNO-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       400-TOUROKU-SEC              SECTION.
      *
      *    入力チェック処理
           PERFORM 210-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 210-BACK
           END-IF
      *
           .
       400-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 502-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "P02X"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  P02X
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX        >   WRK-COMBO-SCODE-MAX)
               MOVE    WRK-COMBO-TBLO (IDX)
                                       TO  P02X-SCODE-ITEM (IDX)
           END-PERFORM
           MOVE    WRK-COMBO-SCODE-MAX TO  P02X-SCODE-COUNT
           MOVE    SPA-GMN-SCODE-G     TO  P02X-SCODE
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   SPA-P02X-GMN-MAX
               MOVE    SPA-P02X-GMN-SELNOL (IDX)
                                            TO  P02X-NO (IDX)
               MOVE    SPA-P02X-GMN-ADRL (IDX)
                                            TO  P02X-ADR (IDX)
               MOVE    SPA-P02X-GMN-PORTL (IDX)
                                            TO  P02X-PORT (IDX)
               MOVE    SPA-P02X-GMN-MEMOL (IDX)
                                            TO  P02X-MEMO (IDX)
               IF      SPA-P02X-GMN-SEL     =   IDX
                   MOVE    "T"              TO  P02X-LIST-SELECT (IDX)
               ELSE
                   MOVE    "F"              TO  P02X-LIST-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-P02X-GMN-MAX    TO  P02X-LIST-COUNT
      *
           MOVE    SPA-P02X-GMN-SEL    TO  P02X-SELNUM 
      *
           PERFORM 5011-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5011-MAPCUR-SEC             SECTION.
      *
           IF      SPA-P02X-CUR        =   ZERO
               PERFORM 50111-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-P02X-CUR
               WHEN    01
                   MOVE    "SCODE"         TO  MCP-WIDGET
               WHEN    02
                   MOVE    "SELNUM"        TO  MCP-WIDGET
               WHEN    03
                   MOVE    "B01"           TO  MCP-WIDGET
               WHEN    04
                   MOVE    "B11"           TO  MCP-WIDGET
               WHEN    05
                   MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
           .
       5011-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       50111-CUR-SET-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SCODE"
                   MOVE    2               TO  SPA-P02X-CUR
               WHEN    "SELNUM"
                   MOVE    5               TO  SPA-P02X-CUR
               WHEN    OTHER
                   MOVE    2               TO  SPA-P02X-CUR
           END-EVALUATE
           .
       50111-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       502-ERRSET-SEC              SECTION.
      *
            EVALUATE    SPA-ERRCD
                WHEN    "0001"
                    MOVE    "送信コード入力エラー"
                                                TO  SPA-ERRMSG
                WHEN    "0002"
                    MOVE    "選択番号が有りません"
                                                TO  SPA-ERRMSG
                WHEN    OTHER
                    MOVE    SPACE               TO  SPA-ERRMSG
            END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P02X"              TO  SPA-MOTOPG
           MOVE    "PERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       502-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "P02"               TO  SPA-SAKIPG
           MOVE    "P02X"              TO  SPA-MOTOPG
      *
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
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
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
           MOVE    "DBFETCH"       TO  MCP-FUNC
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
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
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
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
