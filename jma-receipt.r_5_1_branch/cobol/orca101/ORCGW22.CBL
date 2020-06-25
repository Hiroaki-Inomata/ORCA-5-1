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
       PROGRAM-ID.                               ORCGW22.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 室料差額設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17.12.08    NaCl-太田     新規作成
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
       01  SPA-W22.
           03  SPA-GMN-CUR                         PIC 9(03).
           03  SPA-GMN-AREA.
               05  SPA-GMN-KJNYMD                  PIC X(10).
               05  SPA-GMN-INPUT-AREA.
                   07  SPA-GMN-SELNUM              PIC 9(02).
                   07  SPA-GMN-KBNCD               PIC X(02).
                   07  SPA-GMN-SAGAKU              PIC X(06).
                   07  SPA-GMN-STYUKYMD            PIC X(10).
                   07  SPA-GMN-EDYUKYMD            PIC X(10).
               05  SPA-GMN-SAGAKULST-AREA.
                   07 SPA-GMN-SAGAKULST-MAX        PIC 9(03).
                   07 SPA-GMN-SAGAKULST-SEL        PIC 9(03).
                   07 SPA-GMN-SAGAKULST-OCC        OCCURS  100.
                      09  SPA-GMN-TKBNCD           PIC X(02).
                      09  SPA-GMN-TSAGAKU          PIC 9(06).
                      09  SPA-GMN-TSTYUKYMD        PIC X(10).
                      09  SPA-GMN-TEDYUKYMD        PIC X(10).
           03  SPA-NAI-AREA.
               05  SPA-NAI-KJNYMD                  PIC X(08).
               05  SPA-NAI-INPUT-AREA.
                   07  SPA-NAI-SAGAKU              PIC 9(06).
                   07  SPA-NAI-STYUKYMD            PIC X(08).
                   07  SPA-NAI-EDYUKYMD            PIC X(08).
               05  SPA-NAI-SAGAKULST-AREA.
                   07 SPA-NAI-SAGAKULST-OCC        OCCURS  100.
                      09  SPA-NAI-TSTYUKYMD        PIC X(08).
                      09  SPA-NAI-TEDYUKYMD        PIC X(08).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-KIKAN-DABURI                    PIC 9(01).
           03  FLG-UPD-KBN                         PIC 9(01).
           03  FLG-SAGAKULST-ROW                   PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX1                                PIC 9(05).
           03  IDXA                                PIC 9(05).
           03  IDXB                                PIC 9(05).
           03  IDXL                                PIC 9(05).
           03  IDXM                                PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SAGAKULST-ROW                   PIC S9(09).
           03  WRK-MCP-WIDGET                      PIC X(64). 
           03  WRK-WIDMSG                          PIC X(70). 
           03  WRK-MAX                             PIC 9(05).
           03  WRK-YMD                             PIC X(10). 
           03  WRK-ZOGENPTN                        PIC X(01).
           03  WRK-ZOGEN                           PIC S9(05).
           03  WRK-KEISANYMD                       PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY                         PIC 9(04).
               05  WRK-SMM                         PIC 9(02).
               05  WRK-SDD                         PIC 9(02).
           03  WRK-HENYMDG                         PIC X(09).
           03  WRK-YUKOSTYMD                       PIC X(08).
           03  WRK-YUKOEDYMD                       PIC X(08).
           03  WRK-KANACHK-INPUT                   PIC X(400).
           03  WRK-SAGAKUZ                         PIC Z(5)9.
           03  WRK-KBNCD-G.
               05  WRK-KBNCD9                      PIC 9(2).
           03  WRK-STHENYMDG                       PIC X(9).
           03  WRK-EDHENYMDG                       PIC X(9).
           03  WRK-ERRMSG-PLUS                     PIC X(32).
           03  WRK-Z9                              PIC Z9.
           03  WRK-TSAGAKUZ                        PIC ZZZ,ZZ9.
      *
       01  CONST-AREA.
           03  CONST-SAGAKULST-MAX                 PIC 9(03) VALUE 99.
           03  CONST-INSERT                        PIC 9(01) VALUE 1.
           03  CONST-UPDATE                        PIC 9(01) VALUE 2.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY  "CPSYSKANRI.INC".
      *    室料差額
           COPY  "CPSK5113.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
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
           INITIALIZE                  IDX-AREA
                                       FLG-AREA
                                       WRK-AREA
      *
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W22
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN     "PUTG"         ALSO    "CLICKED"
               PERFORM 200-GMNSENI
      *    入力
           WHEN      OTHER
               PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF  FLG-END   =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W22         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           EXIT    PROGRAM
      *
           .
      *
       000-PROC-EXT.
           EXIT.
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG      =   "WERR" )
               MOVE    SPACE       TO  SPA-MOTOPG
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
           MOVE    "W22"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
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
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-W22
      *
           MOVE    SPA-SYSYMD      TO  SPA-NAI-KJNYMD
           MOVE    SPA-SYSYMDWH    TO  SPA-GMN-KJNYMD
      *
           PERFORM 420-CLEAR-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       311-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-GMN-SAGAKULST-AREA
                                   SPA-NAI-SAGAKULST-AREA
                                   SPA-GMN-INPUT-AREA
                                   SPA-NAI-INPUT-AREA
      *
           PERFORM 3110-SAGAKULST-EDIT-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       311-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    室料差額リスト編集処理
      *****************************************************************
       3110-SAGAKULST-EDIT-SEC     SECTION.
      *
           MOVE    1               TO  FLG-SAGAKULST-ROW
      *
           INITIALIZE              SPA-GMN-SAGAKULST-AREA
                                   SPA-NAI-SAGAKULST-AREA
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5113"          TO  SYS-KANRICD
           MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5113-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( SPA-GMN-SAGAKULST-MAX
                                               >=  CONST-SAGAKULST-MAX )
      *
               COMPUTE SPA-GMN-SAGAKULST-MAX
                   =   SPA-GMN-SAGAKULST-MAX  +   1
      *
               MOVE    SPA-GMN-SAGAKULST-MAX
                                           TO  IDXL
               MOVE    SYS-5113-KBNCD      TO  SPA-GMN-TKBNCD(IDXL)
               MOVE    SYS-5113-BRM-SAGAKU-NM
                                           TO  SPA-GMN-TSAGAKU(IDXL)
      *
               MOVE    SYS-5113-STYUKYMD   TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TSTYUKYMD(IDXL)
      *
               MOVE    SYS-5113-EDYUKYMD   TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TEDYUKYMD(IDXL)
      *
               MOVE    SYS-5113-STYUKYMD   TO  SPA-NAI-TSTYUKYMD(IDXL)
               MOVE    SYS-5113-EDYUKYMD   TO  SPA-NAI-TEDYUKYMD(IDXL)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5113-REC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3110-SAGAKULST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF    ( SPA-WID1-FLG    =   "OK" )
               EVALUATE    SPA-WIDCD
               WHEN "2001"
                       PERFORM 4201-SAKUJO-SEC
               WHEN "2002"
                       PERFORM 4201-EDYUKYMD-UPDATE-SEC
                       IF    ( SPA-ERRCD   =   SPACE )
                           PERFORM 4201-TUIKA-SEC
                       END-IF
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD      
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
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"           ALSO    "B01"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 420-CLEAR-SEC
      *    基準日
           WHEN    "CLICKED"       ALSO    "B04"
               MOVE    6               TO  SPA-GMN-CUR
      *    追加
           WHEN    "CLICKED"       ALSO    "B06"
               PERFORM 420-TUIKA-SEC
      *    変更
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 420-HENKOU-SEC
      *    削除
           WHEN    "CLICKED"       ALSO    "B08"
               PERFORM 420-SAKUJO-SEC
      *    基準日
           WHEN    "CLICKED"       ALSO    "B04"
               PERFORM 4101-GMN-SPA-SET-SEC
               MOVE    1           TO      SPA-GMN-CUR
      *
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
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    基準日
           WHEN    ANY             ALSO    "KJNYMD"
               PERFORM 4100-KJNYMD-CHK-SEC
      *    選択番号
           WHEN    "ACTIVATE"      ALSO    "SELNUM"
               PERFORM 4100-SELNUM-CHK-SEC
           WHEN    ANY             ALSO    "SAGAKULST"
               PERFORM 4100-SAGAKULST-CHK-SEC
           WHEN    OTHER
      *
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
      *
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
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    W22-KBNCD           TO  SPA-GMN-KBNCD
           MOVE    W22-SAGAKU          TO  SPA-GMN-SAGAKU
           MOVE    W22-STYUKYMD        TO  SPA-GMN-STYUKYMD
           MOVE    W22-EDYUKYMD        TO  SPA-GMN-EDYUKYMD
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
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SELNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-SAGAKULST-SEL   =   ZERO )
                   PERFORM 4101-KBNCD-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SAGAKU-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-STYUKYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-EDYUKYMD-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W22"               TO  SPA-MOTOPG
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
      *    条件クリア処理
      *****************************************************************
       420-CLEAR-SEC               SECTION.
      *
           MOVE    1               TO  FLG-SAGAKULST-ROW
           PERFORM 311-SPASET-SEC
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加処理
      *****************************************************************
       420-TUIKA-SEC               SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    CONST-INSERT    TO  FLG-UPD-KBN
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
      *        既存の履歴データをチェックし、期間の重複したデータが
      *        存在しないこと
               MOVE    CONST-INSERT    TO  FLG-UPD-KBN
               PERFORM 4100-RIREKI-CHK-SEC
               IF    ( SPA-ERRCD       =   SPACE )
                   IF    ( FLG-KIKAN-DABURI    =   ZERO )
                       PERFORM 4201-TUIKA-SEC
                   ELSE
                       MOVE    "2002"  TO  SPA-WIDCD
                   END-IF
               END-IF
           END-IF
      *
           .
       420-TUIKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加処理
      *****************************************************************
       4201-TUIKA-SEC                  SECTION.
      *
           INITIALIZE                          SYS-5113-REC
           MOVE    SPA-HOSPNUM             TO  SYS-5113-HOSPNUM
           MOVE    "5113"                  TO  SYS-5113-KANRICD
           MOVE    SPA-GMN-KBNCD           TO  SYS-5113-KBNCD
           MOVE    SPA-NAI-STYUKYMD        TO  SYS-5113-STYUKYMD
           MOVE    SPA-NAI-EDYUKYMD        TO  SYS-5113-EDYUKYMD
           MOVE    SPA-NAI-SAGAKU          TO  SYS-5113-BRM-SAGAKU-NM
           MOVE    SPA-OPID                TO  SYS-5113-OPID
           MOVE    SMCNDATE-YMD            TO  SYS-5113-CREYMD
                                               SYS-5113-UPYMD
           MOVE    SMCNDATE-HMS            TO  SYS-5113-UPHMS
           MOVE    SYS-5113-REC            TO  MCPDATA-REC
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC  NOT =   ZERO )
               MOVE    "4002"              TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 311-SPASET-SEC
           END-IF
      *
           .
       4201-TUIKA-EXT.
           EXIT.
      *****************************************************************
      *    有効期間変更処理
      *****************************************************************
       4201-EDYUKYMD-UPDATE-SEC        SECTION.
      *
           INITIALIZE                          SYSKANRI-REC
           MOVE    SPA-HOSPNUM             TO  SYS-HOSPNUM
           MOVE    "5113"                  TO  SYS-KANRICD
           MOVE    SPA-GMN-KBNCD           TO  SYS-KBNCD
      *
           MOVE    SPA-NAI-STYUKYMD        TO  WRK-YMD
           MOVE    "1"                     TO  WRK-ZOGENPTN
           MOVE    -1                      TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-KEISANYMD           TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
      *
           MOVE    SPA-OPID                TO  SYS-OPID
           MOVE    SMCNDATE-YMD            TO  SYS-UPYMD
           MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
           MOVE    SYSKANRI-REC            TO  MCPDATA-REC
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "upd3"                  TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC  NOT =   ZERO )
               MOVE    "4001"              TO  SPA-ERRCD
           END-IF
      *
           .
       4201-EDYUKYMD-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    変更処理
      *****************************************************************
       420-HENKOU-SEC                  SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    CONST-UPDATE    TO  FLG-UPD-KBN
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
      *        既存の履歴データをチェックし、期間の重複したデータが
      *        存在しないこと
               MOVE    CONST-UPDATE    TO  FLG-UPD-KBN
               PERFORM 4100-RIREKI-CHK-SEC
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 4201-HENKOU-SEC
               END-IF
           END-IF
      *
           .
       420-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    変更処理
      *****************************************************************
       4201-HENKOU-SEC                  SECTION.
      *
           INITIALIZE                          SYS-5113-REC
           MOVE    SPA-HOSPNUM             TO  SYS-5113-HOSPNUM
           MOVE    "5113"                  TO  SYS-5113-KANRICD
           MOVE    SPA-GMN-KBNCD           TO  SYS-5113-KBNCD
           MOVE    SPA-NAI-KJNYMD          TO  SYS-5113-STYUKYMD
                                               SYS-5113-EDYUKYMD
           MOVE    SYS-5113-REC            TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI            =   ZERO )
               MOVE    SYSKANRI-REC        TO  SYS-5113-REC
               MOVE    SPA-NAI-STYUKYMD    TO  SYS-5113-STYUKYMD
               MOVE    SPA-NAI-EDYUKYMD    TO  SYS-5113-EDYUKYMD
               MOVE    SPA-NAI-SAGAKU      TO  SYS-5113-BRM-SAGAKU-NM
               MOVE    SPA-OPID            TO  SYS-5113-OPID
               MOVE    SMCNDATE-YMD        TO  SYS-5113-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-5113-UPHMS
               MOVE    SYS-KEY             TO  SYS-5113-UPD-KEY
               MOVE    SYS-5113-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC  NOT =   ZERO )
                   MOVE    "4003"          TO  SPA-ERRCD
               END-IF
           ELSE
      *
               MOVE    "4004"              TO  SPA-ERRCD
      *
               INITIALIZE                  SYSKANRI-REC
               MOVE    SYSKANRI-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "abort"         TO  MCP-PATHNAME
               PERFORM 911-DBSELECT-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 311-SPASET-SEC
           END-IF
      *
           .
       4201-HENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       420-SAKUJO-SEC                  SECTION.
      *
           PERFORM 4101-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               CONTINUE
           ELSE
               MOVE    1           TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-SELNUM  =   ZERO )
                   MOVE    "1007"  TO  SPA-ERRCD
                   MOVE    1       TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "2001"      TO  SPA-WIDCD
           END-IF
      *
           .
       420-SAKUJO-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       4201-SAKUJO-SEC                  SECTION.
      *
           INITIALIZE                          SYS-5113-REC
           MOVE    SPA-HOSPNUM             TO  SYS-5113-HOSPNUM
           MOVE    "5113"                  TO  SYS-5113-KANRICD
           MOVE    SPA-GMN-KBNCD           TO  SYS-5113-KBNCD
           MOVE    SPA-NAI-KJNYMD          TO  SYS-5113-STYUKYMD
                                               SYS-5113-EDYUKYMD
           MOVE    SYS-5113-REC            TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI            =   ZERO )
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
               IF    ( MCP-RC  NOT =   ZERO )
                   MOVE    "4005"          TO  SPA-ERRCD
               END-IF
           ELSE
      *
               MOVE    "4006"              TO  SPA-ERRCD
      *
               INITIALIZE                  SYSKANRI-REC
               MOVE    SYSKANRI-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "abort"         TO  MCP-PATHNAME
               PERFORM 911-DBSELECT-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 311-SPASET-SEC
           END-IF
      *
           .
       4201-SAKUJO-EXT.
           EXIT.
      *
      *****************************************************************
      *    基準日チェック処理
      *****************************************************************
       4100-KJNYMD-CHK-SEC             SECTION.
      *
           MOVE    ZERO        TO  SPA-GMN-CUR
      *
           MOVE    W22-KJNYMD  TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           IF    ( WRK-HENYMDG NOT =   SPACE )
      *
               INITIALIZE              SPA-W22
      *
               MOVE    WRK-HENYMDG TO  SPA-GMN-KJNYMD
               MOVE    WRK-SYMD    TO  SPA-NAI-KJNYMD
      *
               PERFORM 311-SPASET-SEC
      *
           ELSE
               MOVE    "0001"      TO  SPA-ERRCD
               MOVE    6           TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       4100-KJNYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4100-SELNUM-CHK-SEC            SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
           MOVE    ZERO            TO  SPA-GMN-SAGAKULST-SEL
      *
           MOVE    W22-SELNUM      TO  SPA-GMN-SELNUM
      *
           IF    ( SPA-GMN-SELNUM  =  ZERO )
               PERFORM 4201-INPUT-AREA-CLEAR-SEC
               MOVE    ZERO            TO  SPA-GMN-CUR
           ELSE
      *
               PERFORM 4101-SELNUM-CHK-SEC
               IF    ( SPA-ERRCD   =   SPACE )
                   MOVE    SPA-GMN-SELNUM
                                       TO  SPA-GMN-SAGAKULST-SEL
                   PERFORM 41001-SELNUM-CHK-SEC
               ELSE
                   PERFORM 4201-INPUT-AREA-CLEAR-SEC
                   MOVE    W22-SELNUM  TO  SPA-GMN-SELNUM
               END-IF
           END-IF
      *
           .
      *
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額リスト
      *****************************************************************
       4100-SAGAKULST-CHK-SEC          SECTION.
      *
           PERFORM VARYING     IDXM    FROM    1   BY  1
                   UNTIL     ( IDXM    >   CONST-SAGAKULST-MAX )
      *
               IF    ( W22-SAGAKULST-SELECT  (IDXM)  =   "T" )
                   MOVE    IDXM    TO  W22-SELNUM
      *
                   PERFORM 4100-SELNUM-CHK-SEC
      *
                   MOVE    CONST-SAGAKULST-MAX
                                   TO  IDXM
      *
                   MOVE    3       TO  SPA-GMN-CUR
      *
               END-IF
           END-PERFORM
      *
           .
       4100-SAGAKULST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       41001-SELNUM-CHK-SEC           SECTION.
      *
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           PERFORM 4100-INPUT-AREA-EDIT-SEC
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           .
       41001-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4101-SELNUM-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-SELNUM      =   ZERO )
               CONTINUE
           ELSE
               IF    ( SPA-GMN-SELNUM  >   SPA-GMN-SAGAKULST-MAX )
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    1           TO  SPA-GMN-CUR
               END-IF
      *
           END-IF
      *
           .
      *
       4101-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額コードチェック処理
      *****************************************************************
       4101-KBNCD-CHK-SEC              SECTION.
      *
           IF    ( SPA-GMN-KBNCD   =   SPACE )
               CONTINUE
           ELSE
      *
               INITIALIZE                  ORCSNUMAREA
               MOVE   SPA-GMN-KBNCD    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF  ( SNUM-RC           =   ZERO  )
                AND( SNUM-MINKBN       =   SPACE )
                AND( SNUM-SYOKBN       =   SPACE )
                   MOVE    SNUM-OUTNUM TO  WRK-KBNCD9
                   MOVE    WRK-KBNCD-G TO  SPA-GMN-KBNCD
                   IF    ( SPA-GMN-KBNCD   >=  "01" )
                    AND  ( SPA-GMN-KBNCD   <=  "99" )
                       CONTINUE
                   ELSE
                       MOVE    2       TO  SPA-GMN-CUR
                       MOVE    "0007"  TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    2           TO  SPA-GMN-CUR
                   MOVE    "0007"      TO  SPA-ERRCD
               END-IF
      *
               MOVE    ZERO            TO  IDXL
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   SPA-GMN-SAGAKULST-MAX )
                        OR   ( IDXL    >   ZERO )
                        OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                   IF    ( SPA-GMN-TKBNCD (IDX1)
                                       =   SPA-GMN-TKBNCD (IDXL))
                       MOVE    IDX1    TO  IDXL
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXL    >   ZERO )
                   MOVE    "0003"      TO  SPA-ERRCD
                   MOVE    2           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4101-KBNCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額チェック処理
      *****************************************************************
       4101-SAGAKU-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-SAGAKU
      *
           IF    ( SPA-GMN-SAGAKU      =   SPACE )
               MOVE    ZERO            TO  SPA-NAI-SAGAKU
           ELSE
      *
               INITIALIZE                  ORCSNUMAREA
               MOVE   SPA-GMN-SAGAKU   TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF  ( SNUM-RC           =   ZERO  )
                AND( SNUM-MINKBN       =   SPACE )
                AND( SNUM-SYOKBN       =   SPACE )
                   MOVE    SNUM-OUTNUM TO  SPA-NAI-SAGAKU
                   IF    ( SPA-NAI-SAGAKU  >   300000 )
                       MOVE    3       TO  SPA-GMN-CUR
                       MOVE    "0005"  TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    3           TO  SPA-GMN-CUR
                   MOVE    "0004"      TO  SPA-ERRCD
               END-IF
          END-IF
      *
           .
       4101-SAGAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効期間開始日チェック処理
      *****************************************************************
       4101-STYUKYMD-CHK-SEC           SECTION.
      *
          MOVE    SPACE                TO  SPA-NAI-STYUKYMD
      *
          IF    ( SPA-GMN-STYUKYMD     =   SPACE )
               IF    ( WRK-MCP-WIDGET  =   "STYUKYMD" )
                   MOVE    "00000000"  TO  SPA-GMN-STYUKYMD
                                           SPA-NAI-STYUKYMD
               END-IF
          ELSE
               MOVE    SPA-GMN-STYUKYMD    TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               IF    ( WRK-HENYMDG NOT =   SPACE )
                AND  ( WRK-HENYMDG NOT =   "99999999" )
                   MOVE    WRK-HENYMDG TO  SPA-GMN-STYUKYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-STYUKYMD
               ELSE
                   MOVE    4           TO  SPA-GMN-CUR
                   MOVE    "0006"      TO  SPA-ERRCD
               END-IF
          END-IF
      *
           .
       4101-STYUKYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効期間終了日チェック処理
      *****************************************************************
       4101-EDYUKYMD-CHK-SEC           SECTION.
      *
          MOVE    SPACE                TO  SPA-NAI-EDYUKYMD
      *
          IF    ( SPA-GMN-EDYUKYMD     =   SPACE )
               IF    ( WRK-MCP-WIDGET  =   "EDYUKYMD" )
                   MOVE    "99999999"  TO  SPA-GMN-EDYUKYMD
                                           SPA-NAI-EDYUKYMD
               END-IF
          ELSE
               MOVE    SPA-GMN-EDYUKYMD    TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               IF    ( WRK-HENYMDG NOT =   SPACE )
                AND  ( WRK-HENYMDG NOT =   "00000000" )
                   MOVE    WRK-HENYMDG TO  SPA-GMN-EDYUKYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-EDYUKYMD
               ELSE
                   MOVE    5           TO  SPA-GMN-CUR
                   MOVE    "0008"      TO  SPA-ERRCD
               END-IF
          END-IF
      *
           .
       4101-EDYUKYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力情報領域初期化処理
      *****************************************************************
       4201-INPUT-AREA-CLEAR-SEC       SECTION.
      *
           INITIALIZE  SPA-GMN-INPUT-AREA
                       SPA-NAI-INPUT-AREA
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
      *
       4201-INPUT-AREA-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    入力項目領域編集処理
      *****************************************************************
       4100-INPUT-AREA-EDIT-SEC        SECTION.
      *
      *    選択番号
           MOVE    SPA-GMN-SAGAKULST-SEL
                                   TO  IDXL
                                       SPA-GMN-SELNUM
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5113"          TO  SYS-KANRICD
           MOVE    SPA-GMN-TKBNCD (IDXL)
                                   TO  SYS-KBNCD
           MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5113-REC
      *
           MOVE    SPA-GMN-TKBNCD (IDXL)
                                   TO  SPA-GMN-KBNCD
      *
           MOVE    SYS-5113-BRM-SAGAKU-NM
                                   TO  SPA-NAI-SAGAKU
                                       WRK-SAGAKUZ
           MOVE    FUNCTION TRIM (WRK-SAGAKUZ)
                                   TO  SPA-GMN-SAGAKU
      *
           MOVE    SYS-5113-STYUKYMD
                                   TO  SPA-NAI-STYUKYMD
                                       WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-STYUKYMD
      *
           MOVE    SYS-5113-EDYUKYMD
                                   TO  SPA-NAI-EDYUKYMD
                                       WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-EDYUKYMD
      *
           .
       4100-INPUT-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    共通関連チェック
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
           IF   ( SPA-ERRCD            =   SPACE )
               IF   ( FLG-UPD-KBN      =   CONST-UPDATE )
                AND ( SPA-GMN-SELNUM   =   ZERO )
                   MOVE    "1007"      TO  SPA-ERRCD
                   MOVE    1           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF   ( SPA-ERRCD            =   SPACE )
               IF   ( SPA-GMN-KBNCD    =   SPACE )
                   MOVE    "1001"      TO  SPA-ERRCD
                   MOVE    2           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF   ( SPA-ERRCD                =   SPACE )
               IF    ( SPA-GMN-SAGAKU  =   SPACE )
                   MOVE    "1002"          TO  SPA-ERRCD
                   MOVE    3               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF   ( SPA-ERRCD                =   SPACE )
               IF    ( SPA-GMN-STYUKYMD    =   SPACE )
                   MOVE    "1003"          TO  SPA-ERRCD
                   MOVE    4               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF   ( SPA-ERRCD            =   SPACE )
               IF    ( SPA-GMN-EDYUKYMD    =   SPACE )
                   MOVE    "1004"          TO  SPA-ERRCD
                   MOVE    5               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF   ( SPA-ERRCD            =   SPACE )
               IF    ( SPA-NAI-STYUKYMD    >   SPA-NAI-EDYUKYMD )
                   MOVE    "1005"          TO  SPA-ERRCD
                   MOVE    4               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *****************************************************************
      *    履歴チェック処理
      *****************************************************************
       4100-RIREKI-CHK-SEC             SECTION.
      *
           MOVE    ZERO                    TO  FLG-KIKAN-DABURI
      *
           INITIALIZE                          SYSKANRI-REC
           MOVE    SPA-HOSPNUM             TO  SYS-HOSPNUM
           MOVE    "5113"                  TO  SYS-KANRICD
           MOVE    SPA-GMN-KBNCD           TO  SYS-KBNCD
           MOVE    SPA-NAI-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SPA-NAI-EDYUKYMD        TO  SYS-STYUKYMD
           PERFORM 900-SYSKANRI-KEY26-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( SPA-ERRCD       NOT =   SPACE )
      *
               EVALUATE    FLG-UPD-KBN
               WHEN    CONST-INSERT
                   EVALUATE    TRUE
                   WHEN  ( SPA-NAI-STYUKYMD    <=  SYS-STYUKYMD )
      *
                       PERFORM 41001-ERRMSG-PLUS-EDIT-SEC
      *
                       MOVE    "1006"      TO  SPA-ERRCD
                       MOVE    4           TO  SPA-GMN-CUR
                   END-EVALUATE
      *
                   IF    ( SPA-NAI-STYUKYMD    >   SYS-STYUKYMD )
                       MOVE    1           TO  FLG-KIKAN-DABURI
                   END-IF
               WHEN    CONST-UPDATE
                   IF    ( SPA-NAI-KJNYMD      >=  SYS-STYUKYMD )
                    AND  ( SPA-NAI-KJNYMD      <=  SYS-EDYUKYMD )
                       CONTINUE
                   ELSE
      *
                       PERFORM 41001-ERRMSG-PLUS-EDIT-SEC
      *
                       MOVE    "1006"      TO  SPA-ERRCD
                       MOVE    4           TO  SPA-GMN-CUR
                   END-IF
               END-EVALUATE
      *
               PERFORM 900-SYSKANRI-KEY26-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4100-RIREKI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ補足編集処理
      *****************************************************************
       41001-ERRMSG-PLUS-EDIT-SEC      SECTION.
      *
           MOVE    SYS-STYUKYMD    TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  WRK-STHENYMDG
      *
           MOVE    SYS-EDYUKYMD    TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  WRK-EDHENYMDG
      *
           MOVE    SPACE           TO  WRK-ERRMSG-PLUS
           STRING  "（"
                   WRK-STHENYMDG   DELIMITED BY SIZE
                   " - "           DELIMITED BY SIZE
                   WRK-EDHENYMDG   DELIMITED BY SIZE
                   "）"            DELIMITED BY SIZE
           INTO    WRK-ERRMSG-PLUS
           END-STRING
      *
           .
       41001-ERRMSG-PLUS-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-WIDCD       NOT =   SPACE )
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W22"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    W22-SAGAKULST-ROW      TO  WRK-SAGAKULST-ROW
      *
           MOVE    SPACE                  TO   W22
           INITIALIZE                          W22
      *
           EVALUATE    FLG-SAGAKULST-ROW
           WHEN    1
               MOVE    ZERO               TO  W22-SAGAKULST-ROW
               MOVE    ZERO               TO  W22-SAGAKULST-ROWATTR
           WHEN    OTHER
               MOVE    WRK-SAGAKULST-ROW  TO  W22-SAGAKULST-ROW
               MOVE    ZERO               TO  W22-SAGAKULST-ROWATTR
           END-EVALUATE
      *
           MOVE    SPA-GMN-SELNUM          TO  W22-SELNUM
           MOVE    SPA-GMN-KJNYMD          TO  W22-KJNYMD
           MOVE    SPA-GMN-KBNCD           TO  W22-KBNCD
           MOVE    WIDGET-NORMAL           TO  W22-KBNCD-STATE
           IF    ( SPA-GMN-SAGAKULST-SEL   NOT =   ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  W22-KBNCD-STATE
           END-IF
           MOVE    SPA-GMN-SAGAKU          TO  W22-SAGAKU
           MOVE    SPA-GMN-STYUKYMD        TO  W22-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD        TO  W22-EDYUKYMD
      *
           MOVE    SPA-GMN-SAGAKULST-MAX   TO  W22-SAGAKULST-COUNT
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL            >   SPA-GMN-SAGAKULST-MAX )
      *
               MOVE    IDXL                TO  WRK-Z9
               MOVE    WRK-Z9              TO  W22-TNUM (IDXL)
               MOVE    SPA-GMN-TKBNCD (IDXL)
                                           TO  W22-TKBNCD (IDXL)
               MOVE    SPA-GMN-TSAGAKU (IDXL)
                                           TO  WRK-TSAGAKUZ
               MOVE    WRK-TSAGAKUZ        TO  W22-TSAGAKU (IDXL)
               MOVE    SPA-GMN-TSTYUKYMD (IDXL)
                                           TO  W22-TSTYUKYMD (IDXL)
               MOVE    SPA-GMN-TEDYUKYMD (IDXL)
                                           TO  W22-TEDYUKYMD (IDXL)
               IF    ( SPA-GMN-SAGAKULST-SEL
                                           =   IDXL )
                   MOVE    "T"         TO  W22-SAGAKULST-SELECT (IDXL)
               ELSE
                   MOVE    "F"         TO  W22-SAGAKULST-SELECT (IDXL)
               END-IF
           END-PERFORM
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
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
           PERFORM 5012-SKIP-CUR-SEC
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    1
               MOVE    "SELNUM"        TO   MCP-WIDGET
           WHEN    2
               MOVE    "KBNCD"         TO   MCP-WIDGET
           WHEN    3
               MOVE    "SAGAKU"        TO   MCP-WIDGET
           WHEN    4
               MOVE    "STYUKYMD"      TO   MCP-WIDGET
           WHEN    5
               MOVE    "EDYUKYMD"      TO   MCP-WIDGET
           WHEN    6
               MOVE  "KJNYMD"          TO   MCP-WIDGET
           WHEN    206
               MOVE  "B06"             TO   MCP-WIDGET
           WHEN    207
               MOVE  "B07"             TO   MCP-WIDGET
           END-EVALUATE.
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
           WHEN    "SELNUM"
               MOVE    002             TO  SPA-GMN-CUR
           WHEN    "KBNCD"
               MOVE    003             TO  SPA-GMN-CUR
           WHEN    "SAGAKU"
               MOVE    004             TO  SPA-GMN-CUR
           WHEN    "STYUKYMD"
               MOVE    005             TO  SPA-GMN-CUR
           WHEN    "EDYUKYMD"
               MOVE    206             TO  SPA-GMN-CUR
           WHEN    "KJNYMD"
               MOVE    001             TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルスキップ処理
      *****************************************************************
       5012-SKIP-CUR-SEC            SECTION.
      *
           IF    ( SPA-GMN-CUR =   1 )
               IF    ( SPA-GMN-SAGAKULST-MAX   =   ZERO )
                   MOVE    2       TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR =   2 )
               IF    ( W22-KBNCD-STATE =   WIDGET-INSENSITIVE )
                   MOVE    3       TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
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
               MOVE    "基準日の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "選択番号の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "入力された室料差額コードは既に登録済みです"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "室料差額の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "室料差額は３０万円以内で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "有効期間開始日の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0007"
            MOVE "室料差額コードは０１から９９の範囲で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "有効期間終了日の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "室料差額コードを入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "室料差額を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1003"
               MOVE    "有効期間開始日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "有効期間終了日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1005"
               MOVE    "有効期間開始日＞有効期間終了日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1006"
               STRING  "有効期間の重複するデータが存在します"
                                           DELIMITED BY SIZE
                       WRK-ERRMSG-PLUS     DELIMITED BY SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1007"
               MOVE    "選択番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "4001"      THRU    "4006"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W22"               TO  SPA-MOTOPG
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
           WHEN    "2001"
               MOVE   "選択中の室料差額情報を削除します。よろしいですか"
                                        TO  WRK-WIDMSG
           WHEN    "2002"
               STRING  "有効期間の重複するデータが存在します。"
                                            DELIMITED   BY  SIZE
                       "前の期間を区切りますか"
                                            DELIMITED   BY  SIZE
               INTO    WRK-WIDMSG
               END-STRING
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W22"                TO  SPA-MOTOPG
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
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       800-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE    SPACE           TO  WRK-HENYMDG
                                       WRK-SYMD
      *
           IF    ( WRK-YMD         =   "00000000" 
                                   OR  "99999999" )
               MOVE    WRK-YMD     TO  WRK-HENYMDG
                                       WRK-SYMD
           ELSE
               INITIALIZE                  ORCSGDAYAREA
               MOVE    WRK-YMD         TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
               IF  ( SGDAY-RC          =   ZERO )
                   MOVE    SGDAY-OTDAY TO  WRK-HENYMDG
                   MOVE    SGDAY-SDAY  TO  WRK-SYMD
               END-IF
           END-IF
      *
           .
       800-HIZUKE-CHK-EXT.
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
      *    システム管理マスタ検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY8)
      *****************************************************************
       900-SYSKANRI-KEY8-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY8-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY8)
      *****************************************************************
       900-SYSKANRI-KEY8-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY8-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY26-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key26"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY26-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理
      *****************************************************************
       900-SYSKANRI-KEY26-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key26"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY26-FET-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC          SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING       MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF        ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       910-DBFETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING       MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING       MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING       MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                  SECTION.
      *
           MOVE   LOW-VALUE         TO  MCP-TABLE
                                        MCP-PATHNAME
           MOVE   "PUTWINDOW"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
