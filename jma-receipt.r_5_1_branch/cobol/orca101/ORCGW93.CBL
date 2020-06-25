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
       PROGRAM-ID.                               ORCGW93.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 医療情報代理機関情報設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/12/18    NACL−森脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
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
       01  SPA-W93.
           03  SPA-GMN-CUR                     PIC 9(03).
           03  SPA-GMN-MODE                    PIC 9(01).
           03  SPA-GMN-AREA.
               05  SPA-GMN-RECEDEN-G.
                   07  SPA-GMN-RECEDEN         PIC X(01).
                   07  SPA-GMN-RECEDEN-F1      PIC X(01).
                   07  SPA-GMN-RECEDEN-MEI     PIC X(18).
               05  SPA-GMN-RECEDEN-LST-G.
                   07  SPA-GMN-RECEDEN-LST     PIC X(20)
                                               OCCURS   10.
               05  SPA-GMN-RECEDEN-MAX         PIC 9(02).
      *
               05  SPA-GMN-JMAMR-G.
                   07  SPA-GMN-JMAMR           PIC X(01).
                   07  SPA-GMN-JMAMR-F1        PIC X(01).
                   07  SPA-GMN-JMAMR-MEI       PIC X(18).
               05  SPA-GMN-JMAMR-LST-G.
                   07  SPA-GMN-JMAMR-LST       PIC X(20)
                                               OCCURS   10.
               05  SPA-GMN-JMAMR-MAX           PIC 9(02).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-OK                          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                             PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
      *    入力項目名
           03  WRK-MCP-WIDGET                  PIC X(64).
           03  WRK-WIDMSG                      PIC X(100).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
      *    医療情報代理機関情報
           COPY  "CPSK9104.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
           MOVE    SPA-FREE        TO  SPA-W93
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
           MOVE    SPA-W93         TO  SPA-FREE
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
           MOVE    "W93    "           TO  MCP-WINDOW.
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
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-W93
      *
      *    電子レセプトデータ
           MOVE    "0 作成しない"      TO  SPA-GMN-RECEDEN-LST(1)
           MOVE    "1 作成する"        TO  SPA-GMN-RECEDEN-LST(2)
           MOVE    2                   TO  SPA-GMN-RECEDEN-MAX
      *    日医診療情報データ
           MOVE    "0 作成しない"      TO  SPA-GMN-JMAMR-LST(1)
           MOVE    "1 作成する"        TO  SPA-GMN-JMAMR-LST(2)
           MOVE    2                   TO  SPA-GMN-JMAMR-MAX
      *
      *    システム管理検索
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-W01-KANRICD     TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-9104-REC
      *        電子レセプトデータ
               MOVE    SYS-9104-RECEDEN    TO  SPA-GMN-RECEDEN
      *        日医診療情報データ
               MOVE    SYS-9104-JMAMR      TO  SPA-GMN-JMAMR
           END-IF
      *    電子レセプトデータ初期
           IF      SPA-GMN-RECEDEN     =   SPACE
               MOVE    "0"             TO  SPA-GMN-RECEDEN
           END-IF
           PERFORM 4102-RECEDEN-CHK-SEC
      *    日医診療情報データ初期
           IF      SPA-GMN-JMAMR       =   SPACE
               MOVE    "0"             TO  SPA-GMN-JMAMR
           END-IF
           PERFORM 4102-JMAMR-CHK-SEC
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＷＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
      *        登録処理
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 4801-TOUROKU-SEC
                   ELSE
                       MOVE    1           TO  SPA-GMN-CUR
                   END-IF
               WHEN    "1003"
      *        削除処理
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 4301-DELETE-SYORI-SEC
                   ELSE
                       MOVE    1           TO  SPA-GMN-CUR
                   END-IF
           END-EVALUATE
      *
           .
       330-WID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE    "CHANGE"        TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 420-CLEAR-SEC
      *    削除
           WHEN    "CLICKED"       ALSO    "B03"
               PERFORM 430-DELETE-SEC
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 480-TOUROKU-SEC
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
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC           SECTION.
      *
      *    画面をＳＰＡセット
           MOVE    W93-RECEDEN     TO  SPA-GMN-RECEDEN-G
           MOVE    W93-JMAMR       TO  SPA-GMN-JMAMR-G
      *
      *    基本チェック処理
           PERFORM 4102-RECEDEN-CHK-SEC
      *    日医診療情報データ
           PERFORM 4102-JMAMR-CHK-SEC
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    電子レセプトデータチェック処理
      *****************************************************************
       4102-RECEDEN-CHK-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-RECEDEN-MAX
               IF      SPA-GMN-RECEDEN =   SPA-GMN-RECEDEN-LST
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-RECEDEN-LST (IDX)
                                           TO  SPA-GMN-RECEDEN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK          =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           .
       4102-RECEDEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日医診療情報データチェック処理
      *****************************************************************
       4102-JMAMR-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-JMAMR-MAX
               IF      SPA-GMN-JMAMR   =   SPA-GMN-JMAMR-LST
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-JMAMR-LST (IDX)
                                           TO  SPA-GMN-JMAMR-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK          =   ZERO
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
           END-IF
      *
           .
       4102-JMAMR-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"           TO  SPA-SAKIPG
           MOVE    "W93"           TO  SPA-MOTOPG
      *
           MOVE    "W01"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    条件クリア処理
      *****************************************************************
       420-CLEAR-SEC               SECTION.
      *
           PERFORM 310-SPASET-SEC
           MOVE    SPACE           TO  SPA-ERRCD
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       430-DELETE-SEC              SECTION.
      *
           MOVE    "1003"          TO  SPA-WIDCD
      *
           .
       430-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       4301-DELETE-SYORI-SEC       SECTION.
      *
      *    システム管理検索
           MOVE    SPACE           TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-W01-KANRICD TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD
                                   TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD
                                   TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
      *
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    SYSKANRI-REC    TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "9003"          TO  SPA-ERRCD
                   DISPLAY "W93 DEL ERR SYSKANRI-KEY =>" SYS-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE   "JOIN"           TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
      *
           .
       4301-DELETE-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       480-TOUROKU-SEC             SECTION.
      *
      *    入力個所のチェック
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD       =   SPACE
               MOVE    "1001"          TO  SPA-WIDCD
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
      *    システム管理検索
           MOVE    SPACE           TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-W01-KANRICD TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD
                                   TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD
                                   TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    SYSKANRI-REC    TO  SYS-9104-REC
           ELSE
               MOVE    SPACE           TO  SYS-9104-REC
               INITIALIZE                  SYS-9104-REC
               MOVE    "9104"          TO  SYS-9104-KANRICD
               MOVE    SPA-W01-KANRICD TO  SYS-9104-KANRICD
               MOVE    "*"             TO  SYS-9104-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD
                                   TO  SYS-9104-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD
                                   TO  SYS-9104-EDYUKYMD
           MOVE    SPA-GMN-RECEDEN TO  SYS-9104-RECEDEN
           MOVE    SPA-GMN-JMAMR   TO  SYS-9104-JMAMR
      *
           MOVE    SYS-9104-REC    TO  SYSKANRI-REC
      *    ＤＢ更新処理
           PERFORM 4910-DBKOUSIN-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE   "JOIN"           TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
      *
           .
       4801-TOUROKU-EXT.
           EXIT.
      ******************************************************************
      *       登録処理
      *****************************************************************
       4910-DBKOUSIN-SEC           SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    SPA-OPID        TO  SYS-OPID
               MOVE    SMCNDATE-YMD    TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
      *
               MOVE    SYS-KANRICD     TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD       TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD
                                       TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD
                                       TO  SYSUP-EDYUKYMD
               MOVE    SPA-HOSPNUM     TO  SYSUP-HOSPNUM
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC    TO  MCPDATA-REC
      *
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "upd1"          TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC      NOT =   ZERO
                   MOVE    "9002"          TO  SPA-ERRCD
                   DISPLAY "W93 UPD ERR SYSKANRI-KEY =>" SYS-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF 
           ELSE
               MOVE    SPA-OPID        TO  SYS-OPID
               MOVE    SMCNDATE-YMD    TO  SYS-CREYMD
                                           SYS-UPYMD
               MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC    TO  MCPDATA-REC
      *
               MOVE    "DBINSERT"      TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC      NOT =   ZERO
                   MOVE    "9001"          TO  SPA-ERRCD
                   DISPLAY "W93 INS ERR SYSKANRI-KEY =>" SYS-KEY
                           ",MCP-RC:" MCP-RC
               END-IF
           END-IF
      *
           .
       4910-DBKOUSIN-EXT.
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
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"       TO  MCP-PUTTYPE.
           MOVE    "W93    "       TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE           TO  W93
           INITIALIZE                  W93
      *
      *    電子レセプトデータ作成区分
           MOVE    SPA-GMN-RECEDEN-G   TO  W93-RECEDEN
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-RECEDEN-MAX
               MOVE    SPA-GMN-RECEDEN-LST(IDX)
                                       TO  W93-RECEDEN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-RECEDEN-MAX TO  W93-RECEDEN-COUNT
      *
      *    日医診療情報データ作成区分がありません
           MOVE    SPA-GMN-JMAMR-G TO  W93-JMAMR
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-JMAMR-MAX
               MOVE    SPA-GMN-JMAMR-LST(IDX)
                                       TO  W93-JMAMR-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-JMAMR-MAX   TO  W93-JMAMR-COUNT
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
           IF     (SPA-ERRCD       =   SPACE)  AND
                  (SPA-GMN-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    01
                   MOVE    "RECEDEN"       TO  MCP-WIDGET
               WHEN    02
                   MOVE    "JMAMR"         TO  MCP-WIDGET
               WHEN    90
                   MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "RECEDEN"
                   MOVE    2               TO  SPA-GMN-CUR
               WHEN    "JMAMR"
                   MOVE    90              TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "電子レセプトデータ作成区分がありません"
                                           TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "日医診療情報データ作成区分がありません"
                                           TO  SPA-ERRMSG
               WHEN    "9001"
                   MOVE    "登録処理に失敗しました"
                                           TO  SPA-ERRMSG
               WHEN    "9002"
                   MOVE    "更新処理に失敗しました"
                                           TO  SPA-ERRMSG
               WHEN    "9003"
                   MOVE    "削除処理に失敗しました"
                                           TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  WERR
           MOVE    SPA-ERRCD       TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  WERR-ERRMSG
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "W93"           TO  SPA-MOTOPG
           MOVE    "WERR"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "WERR"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "登録します。よろしいですか"
                                            TO  WRK-WIDMSG
               WHEN    "1003"
                   MOVE    "削除します。よろしいですか"
                                            TO  WRK-WIDMSG
           END-EVALUATE
      *
      *
           MOVE    SPACE           TO  WID1
           INITIALIZE                  WID1
           MOVE    SPA-WIDCD       TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG      TO  WID1-ID1MSG
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "W93"           TO  SPA-MOTOPG
           MOVE    "WID1"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "WID1"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC             SECTION.
      *
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           .
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC        SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC       SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
