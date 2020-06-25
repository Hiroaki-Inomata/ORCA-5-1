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
       PROGRAM-ID.                               ORCGW16.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 高額請求資料用薬剤分類情報設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/08/04    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/05/02  グループ診療対応
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
         01  SPA-W16.
           03  SPA-W16-AREA.
             05  SPA-GMN-W16-CUR                   PIC 9(03).
             05  SPA-GMN-W16-AREA.
               07  SPA-GMN-W16-STYUKYMD            PIC X(09).
               07  SPA-GMN-W16-EDYUKYMD            PIC X(09).
               07  SPA-GMN-W16-KBNNUM-G.
                 09  SPA-GMN-W16-KBNNUM            PIC 9(02)
                                                   OCCURS  20.
               07  SPA-GMN-W16-BUNRUIMEI-G.
                 09  SPA-GMN-W16-BUNRUIMEI         PIC X(40)
                                                   OCCURS  20.
      *
      *    カウント領域
       01  COUNT-AREA.
           03  COUNT-DAT                           PIC 9(05).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-ERR                             PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDX3                                PIC 9(05).
           03  IDXA                                PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-KBNCD.
               05  WRK-KBNNUM                      PIC 9(02).
               05  WRK-KBNNUMFL                    PIC X(06).
           03  WRK-WIDMSG                          PIC X(70). 
           03  WRK-YMD                             PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY                         PIC 9(04).
               05  WRK-SMM                         PIC 9(02).
               05  WRK-SDD                         PIC 9(02).
           03  WRK-UPDKBN                          PIC X(03).
      *
      *    入力項目名
           03  WRK-MCP-WIDGET                      PIC X(64).
           03  WRK-MAE-INPUT                       PIC X(400).
           03  WRK-ATO-INPUT                       PIC X(400).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-MAX               PIC 9(05)   VALUE   20.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    高額請求資料用薬剤分類情報
           COPY  "CPSK1036.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
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
           INITIALIZE                  COUNT-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W16
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
           MOVE    SPA-W16         TO  SPA-FREE
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
           MOVE    "W16    "           TO  MCP-WINDOW.
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
       310-SPASET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-W16
                                           W16
           INITIALIZE                  SPA-W16
                                           W16
      *
           MOVE    "00000000"
                                   TO  SPA-GMN-W16-STYUKYMD
           MOVE    "99999999"
                                   TO  SPA-GMN-W16-EDYUKYMD
      *    区分番号設定
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-MAX )
               MOVE    IDX1            TO  SPA-GMN-W16-KBNNUM(IDX1)
      *
      *        分類名設定
               MOVE    IDX1            TO  WRK-KBNNUM
               PERFORM 3101-SYS1036-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    SYS-1036-BUNRUIMEI
                                       TO  SPA-GMN-W16-BUNRUIMEI(IDX1)
               END-IF
           END-PERFORM
      *
           MOVE    1                   TO  SPA-GMN-W16-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    高額請求資料用薬剤分類情報検索処理
      *****************************************************************
       3101-SYS1036-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1036"              TO  SYS-KANRICD
           MOVE    WRK-KBNCD           TO  SYS-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-1036-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-SYS1036-SEL-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF    ( SPA-WID1-FLG    =   "OK" )
               EVALUATE    SPA-WIDCD
               WHEN "3001"
                   PERFORM 481-TOUROKU-SEC
               END-EVALUATE
           END-IF
      *
      *
           .
       330-WID1-SET-EXT.
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
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM  210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 420-CLEAR-SEC
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-TOUROKU-SEC
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
      *    EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    WHEN    "XXXX"
      *        入力個所のセット
      *        PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
      *        PERFORM 410-INPUT-CHK-SEC
      *    END-EVALUATE
      *
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
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
           MOVE    ZERO            TO  SPA-GMN-W16-CUR
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
      *    分類名称
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-MAX )
      *
               MOVE    W16-BUNRUIMEI(IDX1)
                                   TO  SPA-GMN-W16-BUNRUIMEI(IDX1)
      *
           END-PERFORM
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
      *    分類名称チェック処理
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   CONST-MAX )
               PERFORM 4100-BUNRUIMEI-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           END-PERFORM
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W16"               TO  SPA-MOTOPG
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
           INITIALIZE  SPA-GMN-W16-BUNRUIMEI-G
      *
           MOVE    1           TO  SPA-GMN-W16-CUR
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       480-TOUROKU-SEC             SECTION.
      *
      *    入力個所のチェック
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
             OR  ( FLG-END     NOT =   ZERO  )
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
       481-TOUROKU-SEC                 SECTION.
      *
           MOVE    ZERO            TO  COUNT-DAT
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   CONST-MAX )
      *
               MOVE    SPACE           TO  WRK-UPDKBN
      *
      *        高額請求資料用薬剤分類情報検索
               MOVE    IDXA            TO  WRK-KBNNUM
               PERFORM 3101-SYS1036-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   IF    ( SPA-GMN-W16-BUNRUIMEI (IDXA)
                                       =   SPACE )
                       MOVE    "DEL"   TO  WRK-UPDKBN
                   ELSE
                       MOVE    "UPD"   TO  WRK-UPDKBN
                   END-IF
               ELSE
                   IF    ( SPA-GMN-W16-BUNRUIMEI (IDXA)
                                       =   SPACE )
                       CONTINUE
                   ELSE
                       MOVE    "INS"   TO  WRK-UPDKBN
                   END-IF
               END-IF
      *
               IF    ( WRK-UPDKBN      NOT =   SPACE )
      *
      *            高額請求資料用薬剤分類情報設定
                   PERFORM 4811-SYS1036-HEN-SEC
      *
      *            高額請求資料用薬剤分類情報更新
                   PERFORM 4811-SYS1036-UPD-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       MOVE    IDXA    TO  SPA-GMN-W16-CUR
                       GO  TO  481-TOUROKU-EXT
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
      *
           .
       481-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    高額請求資料用薬剤分類情報設定処理
      *****************************************************************
       4811-SYS1036-HEN-SEC          SECTION.
      *
           INITIALIZE                      SYS-1036-REC
           MOVE    "1036"              TO  SYS-1036-KANRICD
           MOVE    WRK-KBNCD           TO  SYS-1036-KBNCD
           MOVE    "00000000"          TO  SYS-1036-STYUKYMD
           MOVE    "99999999"          TO  SYS-1036-EDYUKYMD
           MOVE    SPA-GMN-W16-BUNRUIMEI (IDXA)
                                       TO  SYS-1036-BUNRUIMEI
      *
           .
       4811-SYS1036-HEN-EXT.
           EXIT.
      *****************************************************************
      *    高額請求資料用薬剤分類情報更新処理
      *****************************************************************
       4811-SYS1036-UPD-SEC          SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1036-HOSPNUM
           MOVE    SYS-1036-REC        TO MCPDATA-REC
           MOVE    "tbl_syskanri"      TO MCP-TABLE
           MOVE    "key"               TO MCP-PATHNAME
      *
           EVALUATE    WRK-UPDKBN
           WHEN    "UPD"
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "1201"      TO SPA-ERRCD
                   GO  TO  4811-SYS1036-UPD-EXT
               END-IF
           WHEN    "INS"
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "1201"      TO SPA-ERRCD
                   GO  TO  4811-SYS1036-UPD-EXT
               END-IF
           WHEN    "DEL"
               PERFORM 910-DBDELETE-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "1201"      TO SPA-ERRCD
                   GO  TO  4811-SYS1036-UPD-EXT
               END-IF
           END-EVALUATE
      *
           .
       4811-SYS1036-UPD-EXT.
           EXIT.
      *****************************************************************
      *    分類名称チェック処理
      *****************************************************************
       4100-BUNRUIMEI-CHK-SEC          SECTION.
      *
      *    全角文字で入力されていること
           MOVE    SPA-GMN-W16-BUNRUIMEI (IDXA)
                                       TO  WRK-MAE-INPUT
           PERFORM 800-ZENCHK-SEC
           IF    ( FLG-ERR NOT =   ZERO )
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    IDXA            TO  SPA-GMN-W16-CUR
               GO  TO  4100-BUNRUIMEI-CHK-EXT
           END-IF
      *
           MOVE    WRK-ATO-INPUT       TO  SPA-GMN-W16-BUNRUIMEI (IDXA)
      *
           .
      *
       4100-BUNRUIMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    共通関連チェック
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
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
           MOVE    "W16    "           TO  MCP-WINDOW.
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
      *    有効開始日
           MOVE    SPA-GMN-W16-STYUKYMD
                                       TO  W16-STYUKYMD
      *    有効終了日
           MOVE    SPA-GMN-W16-EDYUKYMD
                                       TO  W16-EDYUKYMD
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1        >   CONST-MAX )
      *
      *        区分
               MOVE    SPA-GMN-W16-KBNNUM (IDX1)
                                       TO  W16-KBNNUM (IDX1)
      *
      *        分類名
               MOVE    SPA-GMN-W16-BUNRUIMEI (IDX1)
                                       TO  W16-BUNRUIMEI (IDX1)
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
                  (SPA-GMN-W16-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-W16-CUR
           WHEN    001
               MOVE    "BUNRUIMEI01"   TO  MCP-WIDGET
           WHEN    002
               MOVE    "BUNRUIMEI02"   TO  MCP-WIDGET
           WHEN    003
               MOVE    "BUNRUIMEI03"   TO  MCP-WIDGET
           WHEN    004
               MOVE    "BUNRUIMEI04"   TO  MCP-WIDGET
           WHEN    005
               MOVE    "BUNRUIMEI05"   TO  MCP-WIDGET
           WHEN    006
               MOVE    "BUNRUIMEI06"   TO  MCP-WIDGET
           WHEN    007
               MOVE    "BUNRUIMEI07"   TO  MCP-WIDGET
           WHEN    008
               MOVE    "BUNRUIMEI08"   TO  MCP-WIDGET
           WHEN    009
               MOVE    "BUNRUIMEI09"   TO  MCP-WIDGET
           WHEN    010
               MOVE    "BUNRUIMEI10"   TO  MCP-WIDGET
           WHEN    011
               MOVE    "BUNRUIMEI11"   TO  MCP-WIDGET
           WHEN    012
               MOVE    "BUNRUIMEI12"   TO  MCP-WIDGET
           WHEN    013
               MOVE    "BUNRUIMEI13"   TO  MCP-WIDGET
           WHEN    014
               MOVE    "BUNRUIMEI14"   TO  MCP-WIDGET
           WHEN    015
               MOVE    "BUNRUIMEI15"   TO  MCP-WIDGET
           WHEN    016
               MOVE    "BUNRUIMEI16"   TO  MCP-WIDGET
           WHEN    017
               MOVE    "BUNRUIMEI17"   TO  MCP-WIDGET
           WHEN    018
               MOVE    "BUNRUIMEI18"   TO  MCP-WIDGET
           WHEN    019
               MOVE    "BUNRUIMEI19"   TO  MCP-WIDGET
           WHEN    020
               MOVE    "BUNRUIMEI20"   TO  MCP-WIDGET
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
           WHEN    "BUNRUIMEI01"
               MOVE    001             TO  SPA-GMN-W16-CUR
           WHEN    "BUNRUIMEI02"
               MOVE    002             TO  SPA-GMN-W16-CUR
           WHEN    "BUNRUIMEI03"
               MOVE    003             TO  SPA-GMN-W16-CUR
           WHEN    "BUNRUIMEI04"
               MOVE    004             TO  SPA-GMN-W16-CUR
           WHEN    "BUNRUIMEI05"
               MOVE    005             TO  SPA-GMN-W16-CUR
           WHEN    "BUNRUIMEI06"
               MOVE    006             TO  SPA-GMN-W16-CUR
           WHEN    "BUNRUIMEI07"
               MOVE    007             TO  SPA-GMN-W16-CUR
           WHEN    "BUNRUIMEI08"
               MOVE    008             TO  SPA-GMN-W16-CUR
           WHEN    "BUNRUIMEI09"
               MOVE    009             TO  SPA-GMN-W16-CUR
           WHEN    "BUNRUIMEI10"
               MOVE    010             TO  SPA-GMN-W16-CUR
           WHEN    "BUNRUIMEI11"
               MOVE    011             TO  SPA-GMN-W16-CUR
           WHEN    "BUNRUIMEI12"
               MOVE    012             TO  SPA-GMN-W16-CUR
           WHEN    "BUNRUIMEI13"
               MOVE    013             TO  SPA-GMN-W16-CUR
           WHEN    "BUNRUIMEI14"
               MOVE    014             TO  SPA-GMN-W16-CUR
           WHEN    "BUNRUIMEI15"
               MOVE    015             TO  SPA-GMN-W16-CUR
           WHEN    "BUNRUIMEI16"
               MOVE    016             TO  SPA-GMN-W16-CUR
           WHEN    "BUNRUIMEI17"
               MOVE    017             TO  SPA-GMN-W16-CUR
           WHEN    "BUNRUIMEI18"
               MOVE    018             TO  SPA-GMN-W16-CUR
           WHEN    "BUNRUIMEI19"
               MOVE    019             TO  SPA-GMN-W16-CUR
           WHEN    "BUNRUIMEI20"
               MOVE    020             TO  SPA-GMN-W16-CUR
           END-EVALUATE
      *
      *    次カーソル位置を設定
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "BUNRUIMEI20"
               MOVE    212             TO  SPA-GMN-W16-CUR
           WHEN    OTHER
               COMPUTE SPA-GMN-W16-CUR =   SPA-GMN-W16-CUR
                                           +   1
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
               MOVE    "分類名は全角で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1201"
               MOVE    "更新できませんでした"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W16"               TO  SPA-MOTOPG
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
               MOVE    "登録処理をします"
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
           MOVE    "W16"                TO  SPA-MOTOPG
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
      *    半角チェック処理
      *****************************************************************
       800-ZENCHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF    ( KANACHK-RC      NOT =   ZERO  )
            OR   ( KANACHK-SYUBETU     =   1     )
      *        全角半角混在エラー
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
           .
      *
       800-ZENCHK-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
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

