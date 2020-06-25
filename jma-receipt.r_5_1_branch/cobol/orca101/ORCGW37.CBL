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
       PROGRAM-ID.                               ORCGW37.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 短期滞在手術等基本料情報
      *  管理者            : 
      *  作成日付    作業者        記述
      *  14/02/24    NACL−太田    新規作成
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
       DATA                        DIVISION.
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
       01  SPA-W37.
           03  SPA-GMN-PAGE                       PIC 9(03).
           03  SPA-GMN-PAGE-MAX                   PIC 9(03).
           03  SPA-GMN-CUR                        PIC 9(03).
           03  SPA-GMN-AREA.
               05  SPA-GMN-TANKI3-G.
                   07  SPA-GMN-TANKI3-MAX         PIC 9(03).
                   07  SPA-GMN-TANKI3-OCC         OCCURS  100.
                       09  SPA-GMN-CHK-TANKI3     PIC X(01).
                       09  SPA-GMN-CHK-LBLTANKI3  PIC X(100).
           03  SPA-NAI-AREA.
               05  SPA-GMN-NAI-TANKI3-G.
                   07  SPA-NAI-TANKI3-OCC         OCCURS  100.
                       09  SPA-NAI-TANKI3-SRYCD   PIC X(09).
                       09  SPA-NAI-TANKI3-TOKCD   PIC X(02).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-TENSU                           PIC 9(01).
           03  FLG-ALL                             PIC 9(01).
           03  FLG-HIZUKE-ERR                      PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDXL                                PIC 9(05).
           03  IDXP                                PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
      *    入力項目名
           03  WRK-MCP-WIDGET                      PIC X(64).
           03  WRK-YMD                             PIC X(10).
           03  WRK-YMD-R REDEFINES WRK-YMD.
               05  WRK-KEISANYMD                   PIC 9(08).
               05  WRK-KEISANYMD-FL                PIC X(02).
           03  WRK-ZOGENPTN                        PIC X(01).
           03  WRK-ZOGEN                           PIC S9(05).
           03  WRK-SYMD.
               05  WRK-SYY                         PIC 9(04).
               05  WRK-SMM                         PIC 9(02).
               05  WRK-SDD                         PIC 9(02).
           03  WRK-HENYMDG                         PIC X(09).
           03  WRK-WIDMSG                          PIC X(200).
           03  WRK-TNS-SRYCD                       PIC X(09).
           03  WRK-TNS-KJNYMD                      PIC X(08).
           03  WRK-WIDGET-TANKI3-G.
               05  WRK-WIDGET-TANKI3-LBL           PIC X(11).
               05  WRK-WIDGET-TANKI3               PIC 9(02).
           03  WRK-CHK-TANKI3                      PIC X.
           03  WRK-CMB-CD                          PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM                     PIC X(200).
           03  WRK-SELNUM                          PIC 9(05).
           03  WRK-STYUKYMD                        PIC X(08).
           03  WRK-EDYUKYMD                        PIC X(08).
           03  WRK-SEL-STYUKYMD                    PIC X(08).
           03  WRK-SEL-EDYUKYMD                    PIC X(08).
           03  WRK-KBNCD-G.
               05  WRK-KBNCD                       PIC 9(08).
           03  WRK-KAISEISTYMD                     PIC X(08).
           03  WRK-KAISEIEDYMD                     PIC X(08).
           03  WRK-DELYMD                          PIC X(08).
           03  WRK-FROM                            PIC 9(05).
           03  WRK-TO                              PIC 9(05).
      *
       01  CONST-AREA.
           03  CONST-R020401               PIC X(08) VALUE "20200401".
           03  CONST-H300401               PIC X(08) VALUE "20180401".
           03  CONST-H280401               PIC X(08) VALUE "20160401".
           03  CONST-GMN-TANKI3-MAX        PIC 9(03) VALUE 25.
           03  CONST-TANKI3-MAX            PIC 9(03) VALUE 100.
      *
           COPY  "CPTANKI3.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
      *    短期滞在手術等入院基本料３情報
           COPY  "CPSK5003.INC".
      *
      *    点数マスタ
           COPY  "CPTENSU.INC".
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
           MOVE    SPA-FREE        TO  SPA-W37
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
           MOVE    SPA-W37         TO  SPA-FREE
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
           MOVE    "W37    "           TO  MCP-WINDOW.
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
           PERFORM 800-KAISEI-CHK-SEC
      *
           PERFORM 3101-SPASET-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       3101-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-W37
      *
           MOVE    1               TO  SPA-GMN-PAGE
      *
           MOVE    ZERO            TO  IDX2
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   TANKI3-MAX )
      *
               IF    ( TANKI3-LIFEKBN (IDX1)   =   ZERO )
                AND  ( TANKI3-YUKOSTYMD (IDX1)
                                 <=   SPA-W01-EDYUKYMD )
                AND  ( TANKI3-YUKOEDYMD (IDX1)
                                 >=   SPA-W01-STYUKYMD )
                   PERFORM 31011-SPASET-SEC
               END-IF
      *
           END-PERFORM
      *
           COMPUTE SPA-GMN-PAGE-MAX
               = ( SPA-GMN-TANKI3-MAX / CONST-GMN-TANKI3-MAX ) + 1
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5003"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "5003"          TO  SYS-KANRICD
               MOVE    "*"             TO  SYS-KBNCD
               MOVE    SPA-W01-STYUKYMD TO SYS-STYUKYMD
               PERFORM 900-SYSKANRI-KEY16-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5003-REC
           END-IF
      *
           IF    ( SYS-STYUKYMD    NOT =   SPACE )
               PERFORM 31012-SPASET-SEC
           END-IF
      *
           .
       3101-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       31011-SPASET-SEC                     SECTION.
      *
           COMPUTE IDX2 = IDX2 + 1
      *
           MOVE    IDX2                TO  SPA-GMN-TANKI3-MAX
      *
           MOVE    "F"                 TO  SPA-GMN-CHK-TANKI3 (IDX2)
      *
           MOVE    TANKI3-TANKICD (IDX1)
                                       TO  SPA-NAI-TANKI3-SRYCD (IDX2)
                                           WRK-TNS-SRYCD
           MOVE    TANKI3-YUKOEDYMD (IDX1)
                                       TO  WRK-TNS-KJNYMD
           PERFORM 900-TENSU-KEY34-SEL-SEC
      *
           MOVE    SPACE               TO  SPA-GMN-CHK-LBLTANKI3 (IDX2)
           STRING  TNS-NAME                DELIMITED BY SPACE
           INTO    SPA-GMN-CHK-LBLTANKI3 (IDX2)
           END-STRING
      *
           MOVE    TANKI3-TOKCD (IDX1)
                                       TO  SPA-NAI-TANKI3-TOKCD (IDX2)
      *
           .
       31011-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       31012-SPASET-SEC                     SECTION.
      *
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5003-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               IF    ( SYS-5003-KBNCD  NOT =   "*" )
                   PERFORM VARYING IDX2    FROM    1   BY  1
                           UNTIL ( IDX2    >   SPA-GMN-TANKI3-MAX )
      *
                       IF    ( SYS-5003-TANKICD
                                   =   SPA-NAI-TANKI3-SRYCD (IDX2))
                           MOVE    "T" TO  SPA-GMN-CHK-TANKI3 (IDX2)
                       END-IF
      *
                   END-PERFORM
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC TO SYS-5003-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       31012--SPASET-EXT.
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
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    "CHK_TANKI3_"       TO  WRK-WIDGET-TANKI3-LBL
           MOVE    CONST-GMN-TANKI3-MAX
                                       TO  WRK-WIDGET-TANKI3
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM  210-BACK
      *    前頁
           WHEN    "CLICKED"       ALSO    "B06"
               IF    ( SPA-GMN-PAGE    >   1 )
                 COMPUTE   SPA-GMN-PAGE
                       =   SPA-GMN-PAGE    -   1
               END-IF
      *    次頁
           WHEN    "CLICKED"       ALSO    "B07"
               IF    ( SPA-GMN-PAGE    <   SPA-GMN-PAGE-MAX )
                 COMPUTE   SPA-GMN-PAGE
                       =   SPA-GMN-PAGE    +   1
               END-IF
      *    全選択
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 420-CLEAR-SEC
      *    削除
           WHEN    "CLICKED"       ALSO    "B03"
               PERFORM 430-DEL-SEC
      *    短期３選択チェック
           WHEN    "CLICKED"       ALSO    "CHK_TANKI3_01"
                                   THRU    WRK-WIDGET-TANKI3-G
               PERFORM 4101-TANKI3-CHK-SEC
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
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
       410-INPUT-CHK-SEC           SECTION.
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
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    短期滞在手術等基本料３チェック処理
      *****************************************************************
       4101-TANKI3-CHK-SEC             SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
      *
           MOVE    WRK-MCP-WIDGET      TO  WRK-WIDGET-TANKI3-G
      *
           COMPUTE IDXL  = WRK-WIDGET-TANKI3
      *
           COMPUTE IDX2  = CONST-GMN-TANKI3-MAX
                         * ( SPA-GMN-PAGE - 1 )
                         + IDXL
      *
           IF    ( SPA-GMN-CHK-LBLTANKI3 (IDX2)  NOT = SPACE )
               MOVE    W37-CHK-TANKI3 (IDXL)
                                       TO  SPA-GMN-CHK-TANKI3 (IDX2)
           END-IF
      *
           .
       4101-TANKI3-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W37"               TO  SPA-MOTOPG
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
           MOVE    1               TO  FLG-ALL
      *
           PERFORM VARYING IDX2 FROM   1   BY  1
                   UNTIL ( IDX2 >   SPA-GMN-TANKI3-MAX )
                    OR   ( FLG-ALL =   ZERO )
      *
               IF    ( SPA-GMN-CHK-TANKI3 (IDX2)
                               NOT =   "T" )
                   MOVE    ZERO    TO  FLG-ALL
               END-IF
      *
           END-PERFORM
      *
           IF    ( FLG-ALL         =   1 )
               MOVE    "F"         TO  WRK-CHK-TANKI3
           ELSE
               MOVE    "T"         TO  WRK-CHK-TANKI3
           END-IF
      *
           PERFORM VARYING IDX2 FROM   1   BY  1
                   UNTIL ( IDX2 >   SPA-GMN-TANKI3-MAX )
      *
               MOVE    WRK-CHK-TANKI3
                                   TO  SPA-GMN-CHK-TANKI3 (IDX2)
      *
           END-PERFORM
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       430-DEL-SEC                     SECTION.
      *
           MOVE    "3002"          TO  SPA-WIDCD
      *
           .
       430-DEL-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       4301-DEL-SEC                    SECTION.
      *
           MOVE    SPA-W01-SEL-STYUKYMD TO WRK-DELYMD
      *
           PERFORM 43011-DEL-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    "JOIN"          TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
      *
           .
       4301-DEL-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       43011-DEL-SEC                   SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SPA-W01-KANRICD     TO  SYS-KANRICD
           MOVE    WRK-DELYMD          TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "4001"          TO  SPA-ERRCD
           END-IF
      *
           .
       43011-DEL-EXT.
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
           PERFORM 800-MCNDATE-SEC
      *
           PERFORM 48011-TOUROKU-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE   "JOIN"           TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
      *
           .
       4801-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       48011-TOUROKU-SEC               SECTION.
      *
           MOVE    SPA-W01-STYUKYMD    TO  WRK-DELYMD
           PERFORM 43011-DEL-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 480111-TOUROKU-SEC
           END-IF
      *
           .
       48011-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       480111-TOUROKU-SEC               SECTION.
      *
           INITIALIZE                      SYS-5003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-5003-HOSPNUM
           MOVE    "5003"              TO  SYS-5003-KANRICD
           MOVE    "*"                 TO  SYS-5003-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-5003-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-5003-EDYUKYMD
           MOVE    SMCNDATE-YMD        TO  SYS-5003-CREYMD
                                           SYS-5003-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYS-5003-UPHMS
      *
           MOVE    SYS-5003-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4002"          TO  SPA-ERRCD
           END-IF
      *
           MOVE    ZERO                TO  WRK-KBNCD
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-TANKI3-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               IF    ( SPA-GMN-CHK-TANKI3 (IDX2)
                                       =   "T" )
                   COMPUTE WRK-KBNCD = WRK-KBNCD   +   1
                   MOVE    WRK-KBNCD-G TO  SYS-5003-KBNCD
                   MOVE    SPA-NAI-TANKI3-SRYCD (IDX2)
                                       TO  SYS-5003-TANKICD
                   MOVE    SPA-NAI-TANKI3-TOKCD (IDX2)
                                       TO  SYS-5003-TOKCD
                   MOVE    SYS-5003-REC
                                       TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"
                                       TO  MCP-TABLE
                   MOVE    "key"       TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       MOVE    "4003"  TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       480111--TOUROKU-EXT.
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
           MOVE    "W37    "           TO  MCP-WINDOW.
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
           MOVE    SPACE               TO  W37
           INITIALIZE                      W37
      *
           COMPUTE WRK-FROM
                 = CONST-GMN-TANKI3-MAX
                 * ( SPA-GMN-PAGE - 1 )  + 1
      *
           COMPUTE WRK-TO
                 = CONST-GMN-TANKI3-MAX  * SPA-GMN-PAGE
      *
           MOVE    SPA-W01-STYUKYMDH   TO  W37-STYUKYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  W37-EDYUKYMD
      *
           MOVE    1                   TO  FLG-ALL
           MOVE    ZERO                TO  IDXL
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2     >   CONST-TANKI3-MAX )
      *
               IF    ( IDX2         >=  WRK-FROM )
                AND  ( IDX2         <=  WRK-TO )
                   COMPUTE IDXL = IDXL  +  1
                   MOVE    SPA-GMN-CHK-TANKI3 (IDX2)
                                   TO  W37-CHK-TANKI3 (IDXL)
                   MOVE    SPA-GMN-CHK-LBLTANKI3 (IDX2)
                                   TO  W37-CHK-LBLTANKI3 (IDXL)
      *
                   IF    ( IDX2    >   SPA-GMN-TANKI3-MAX )
                       MOVE    "F" TO  W37-CHK-TANKI3 (IDXL)
                   END-IF
               END-IF
      *
               IF    ( SPA-GMN-CHK-TANKI3 (IDX2)   NOT =   "T" )
                   MOVE    ZERO    TO  FLG-ALL
               END-IF
      *
           END-PERFORM
      *
           IF    ( FLG-ALL         =   ZERO )
               MOVE    "全選択"    TO  W37-B02-LABEL
           ELSE
               MOVE    "全解除"    TO  W37-B02-LABEL
           END-IF
      *
           MOVE    "削除"          TO  W37-B03-LABEL
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
           MOVE    WIDGET-NORMAL   TO  W37-B03-STATE
                                       W37-B06-STATE
                                       W37-B07-STATE
      *
           IF    ( SPA-W01-STYUKYMD    =   "00000000" )
            OR   ( SPA-W01-SELNUM      =   ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  W37-B03-STATE
           END-IF
      *
           IF    ( SPA-GMN-PAGE        <=  1 )
               MOVE    WIDGET-INSENSITIVE  TO  W37-B06-STATE
           END-IF
      *
           IF    ( SPA-GMN-PAGE        >=  SPA-GMN-PAGE-MAX )
               MOVE    WIDGET-INSENSITIVE  TO  W37-B07-STATE
           END-IF
      *
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   CONST-GMN-TANKI3-MAX )
      *
               MOVE    WIDGET-NORMAL  TO  W37-CHK-TANKI3-STATE (IDXL)
               IF    ( W37-CHK-LBLTANKI3 (IDXL)    =   SPACE )
                   MOVE    WIDGET-INSENSITIVE
                                      TO  W37-CHK-TANKI3-STATE (IDXL)
               END-IF
      *
           END-PERFORM
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
           PERFORM 5012-SKIP-CUR-SEC
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    001 THRU    040
               MOVE    "CHK_TANKI3_"       TO  WRK-WIDGET-TANKI3-LBL
               MOVE    SPA-GMN-CUR         TO  WRK-WIDGET-TANKI3
               MOVE    WRK-WIDGET-TANKI3-G TO  MCP-WIDGET
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
           WHEN    "CHK_TANKI3_01"     THRU    "CHK_TANKI3_24"
               MOVE    WRK-MCP-WIDGET  TO  WRK-WIDGET-TANKI3-G
               COMPUTE SPA-GMN-CUR =   WRK-WIDGET-TANKI3   +   1
           WHEN    "CHK_TANKI3_25"
               MOVE    212             TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5012-SKIP-CUR-SEC           SECTION.
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   CONST-GMN-TANKI3-MAX )
      *
               IF    ( SPA-GMN-CUR    =   IDX2 )
                 AND ( W37-CHK-TANKI3-STATE (IDX2)
                                      =    WIDGET-INSENSITIVE )
                   IF    ( IDX2       =    CONST-GMN-TANKI3-MAX )
                       COMPUTE SPA-GMN-CUR =   212
                   ELSE
                       COMPUTE SPA-GMN-CUR =   SPA-GMN-CUR +   1
                   END-IF
               END-IF
      *
           END-PERFORM
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
           WHEN    "4001"  THRU    "4003"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W37"               TO  SPA-MOTOPG
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
           MOVE    "W37"                TO  SPA-MOTOPG
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
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HIZUKE-ERR
           MOVE    SPACE               TO  WRK-SYMD
                                           WRK-HENYMDG
      *
           IF    ( WRK-YMD             =   "00000000" 
                                       OR  "99999999" )
               MOVE    WRK-YMD         TO  WRK-HENYMDG
                                           WRK-SYMD
           ELSE
      *
               INITIALIZE                      ORCSGDAYAREA
               MOVE    WRK-YMD         TO  SGDAY-INDAY
               CALL    "ORCSGDAY"      USING
                                       ORCSGDAYAREA
               IF  ( SGDAY-RC          =   ZERO )
                   MOVE    SGDAY-OTDAY TO  WRK-HENYMDG
                   MOVE    SGDAY-SDAY  TO  WRK-SYMD
               ELSE
                   MOVE    1           TO  FLG-HIZUKE-ERR
                END-IF
           END-IF
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    改正チェック処理
      *****************************************************************
       800-KAISEI-CHK-SEC              SECTION.
      *
           MOVE    CONST-H280401   TO  WRK-KAISEISTYMD
           PERFORM 8001-KAISEI-CHK-SEC
      *
           MOVE    CONST-H300401   TO  WRK-KAISEISTYMD
           PERFORM 8001-KAISEI-CHK-SEC
      *
           MOVE    CONST-R020401   TO  WRK-KAISEISTYMD
           PERFORM 8001-KAISEI-CHK-SEC
      *
           .
      *
       800-KAISEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    改正チェック処理
      *****************************************************************
       8001-KAISEI-CHK-SEC           SECTION.
      *
           MOVE    WRK-KAISEISTYMD         TO  WRK-YMD
           MOVE    "1"                     TO  WRK-ZOGENPTN
           MOVE    -1                      TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-YMD                 TO  WRK-KAISEIEDYMD
      *
           PERFORM 800-MCNDATE-SEC
      *
           IF    ( SPA-W01-SELNUM          >   ZERO )
               IF    ( WRK-KAISEISTYMD     >   SPA-W01-SEL-STYUKYMD )
                AND  ( WRK-KAISEISTYMD    <=   SPA-W01-EDYUKYMD )
      *
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    "5003"          TO  SYS-KANRICD
                   MOVE    "*"             TO  SYS-KBNCD
                   MOVE    SPA-W01-STYUKYMD TO SYS-STYUKYMD
                                               SYS-EDYUKYMD
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   IF    ( FLG-SYSKANRI    =   ZERO )
                       MOVE    SYS-EDYUKYMD        TO  WRK-EDYUKYMD
                       MOVE    WRK-KAISEIEDYMD     TO  SYS-EDYUKYMD
                       MOVE    SMCNDATE-YMD        TO  SYS-UPYMD
                       MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
                       MOVE    SYS-HOSPNUM         TO  SYSUP-HOSPNUM
                       MOVE    SYS-KANRICD         TO  SYSUP-KANRICD
                       MOVE    SYS-STYUKYMD        TO  SYSUP-STYUKYMD
                       MOVE    WRK-EDYUKYMD        TO  SYSUP-EDYUKYMD
                       MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "upd2"              TO  MCP-PATHNAME
                       PERFORM 910-DBUPDATE-SEC
                       IF    ( MCP-RC  NOT =   ZERO )
                           DISPLAY "1:DBUPDATE ERR"
                       END-IF
      *
                       MOVE    SPA-W01-STYUKYMD    TO  WRK-STYUKYMD
                       MOVE    SPA-W01-EDYUKYMD    TO  WRK-EDYUKYMD
                       MOVE    SPA-W01-SELNUM      TO  WRK-SELNUM
                       MOVE    SPA-W01-SEL-STYUKYMD
                                                   TO  WRK-SEL-STYUKYMD
                       MOVE    SPA-W01-SEL-EDYUKYMD
                                                   TO  WRK-SEL-EDYUKYMD
      *
                       INITIALIZE                  SPA-W01-SEL-STYUKYMD
                                                   SPA-W01-SEL-EDYUKYMD
      *
                       MOVE    WRK-KAISEISTYMD     TO  SPA-W01-STYUKYMD
                       PERFORM 3101-SPASET-SEC
                       PERFORM 48011-TOUROKU-SEC
                       MOVE    SPACE               TO  SPA-ERRCD
      *
                       COMPUTE SPA-W01-SELNUM
                           =   SPA-W01-SELNUM  +   1
                       MOVE    WRK-EDYUKYMD    TO  SPA-W01-EDYUKYMD
                                                   SPA-W01-SEL-EDYUKYMD
      *
                       MOVE    WRK-KAISEISTYMD TO  SPA-W01-SEL-STYUKYMD
                                                   SPA-W01-STYUKYMD
      *
                       MOVE    SPA-W01-STYUKYMD    TO  WRK-YMD
                       PERFORM 800-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG TO  SPA-W01-STYUKYMDH
      *
                       MOVE    SPA-W01-EDYUKYMD    TO  WRK-YMD
                       PERFORM 800-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG TO  SPA-W01-EDYUKYMDH
      *
                   END-IF
               END-IF
           ELSE
               IF    ( WRK-KAISEISTYMD      >  SPA-W01-STYUKYMD )
                AND  ( WRK-KAISEISTYMD     <=  SPA-W01-EDYUKYMD )
                   MOVE    SPA-W01-EDYUKYMD    TO  WRK-EDYUKYMD
                   MOVE    WRK-KAISEIEDYMD     TO  SPA-W01-EDYUKYMD
                   PERFORM 3101-SPASET-SEC
                   PERFORM 48011-TOUROKU-SEC
                   MOVE    SPACE               TO  SPA-ERRCD
      *
                   MOVE    WRK-KAISEISTYMD TO  SPA-W01-STYUKYMD
                   MOVE    WRK-EDYUKYMD    TO  SPA-W01-EDYUKYMD
                   PERFORM 3101-SPASET-SEC
                   PERFORM 48011-TOUROKU-SEC
                   MOVE    SPACE               TO  SPA-ERRCD
      *
                   MOVE    WRK-KAISEISTYMD TO  SPA-W01-STYUKYMD
                                               SPA-W01-SEL-STYUKYMD
                   MOVE    WRK-EDYUKYMD    TO  SPA-W01-EDYUKYMD
                                               SPA-W01-SEL-EDYUKYMD
      *
                   COMPUTE SPA-W01-SELNUM
                       =   SPA-W01-SELNUM  +   1
      *
                   MOVE    SPA-W01-STYUKYMD    TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG TO  SPA-W01-STYUKYMDH
      *
                   MOVE    SPA-W01-EDYUKYMD    TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG TO  SPA-W01-EDYUKYMDH
      *
               END-IF
           END-IF
      *
           .
      *
       8001-KAISEI-CHK-EXT.
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
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
            EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY16-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY16-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
            EXIT.
      *****************************************************************
      *    システム管理FETCH処理
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
            EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY34-SEL-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
      *
           MOVE    WRK-TNS-SRYCD   TO  TNS-SRYCD
           MOVE    SPA-W01-EDYUKYMD  TO TNS-YUKOSTYMD
           MOVE    SPA-W01-STYUKYMD  TO TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key34"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               INITIALIZE              TNS-TENSU-REC
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key34"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-TENSU-KEY34-SEL-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
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
      *    ＤＢ更新処理
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
      *    ＤＢ追加処理
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
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
       910-DBCLOSECURSOR-SEC           SECTION.
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
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

