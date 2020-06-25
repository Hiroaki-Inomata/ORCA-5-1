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
       PROGRAM-ID.                               ORCGW28.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 病棟選定入院料情報設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/03/26    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    04/01/07  更新日付編集対応
      *  03.05.00    NACL-太田    07.05.18  グループ診療対応
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
       01  SPA-W28FREE.
      *  病棟選定入院料情報設定画面ＳＰＡ
         03  SPA-W28.
           05  SPA-W28-AREA.
             07  SPA-GMN-W28-PAGE                      PIC 9(02).
             07  SPA-GMN-W28-LINE                      PIC 9(02).
             07  SPA-GMN-W28-MAX                       PIC 9(04).
             07  SPA-GMN-W28-CUR                       PIC 9(03).
             07  SPA-GMN-W28-AREA.
      *        入力項目領域
               09  SPA-GMN-W28-INPUT-AREA.
      *              開始選択番号
                 11  SPA-GMN-W28-STSELNUM              PIC S9(3).
      *              終了選択番号
                 11  SPA-GMN-W28-EDSELNUM              PIC S9(3).
      *              開始病棟番号
                 11  SPA-GMN-W28-STBTUNUM              PIC 9(2).
      *              終了病棟番号
                 11  SPA-GMN-W28-EDBTUNUM              PIC 9(2).
      *          負担計算区分
                 11  SPA-GMN-W28-SENTEI-FTNKBN.
                   13  SPA-GMN-W28-SENTEI-FTNKBN-CD    PIC X(01).
                   13  SPA-GMN-W28-SENTEI-FTNKBN-FL    PIC X(01).
                   13  SPA-GMN-W28-SENTEI-FTNKBN-NM    PIC X(20).
      *          点数単価
                 11  SPA-GMN-W28-SENTEI-TANKA          PIC S9(4).
      *          点数単価（一般）
                 11  SPA-GMN-W28-SENTEI-TANKAIP        PIC S9(4).
      *          点数単価（老人）
                 11  SPA-GMN-W28-SENTEI-TANKARJ        PIC S9(4).
      *          消費税
                 11  SPA-GMN-W28-SENTEI-TAX.
                   13  SPA-GMN-W28-SENTEI-TAX-CD       PIC X(01).
                   13  SPA-GMN-W28-SENTEI-TAX-FL       PIC X(01).
                   13  SPA-GMN-W28-SENTEI-TAX-NM       PIC X(18).
      *
      *        基準日
               09    SPA-GMN-W28-KJNYMD-AREA.
                 11  SPA-GMN-W28-KJNYMD                PIC X(10).
                 11  SPA-NAI-W28-KJNYMD              PIC X(08).
      *        病棟情報リスト領域
               09  SPA-GMN-W28-BTULST-AREA.
                 11  SPA-GMN-W28-BTULST-CNT            PIC S9(9).
                 11  SPA-GMN-W28-BTULST-ITM            OCCURS  400.
                   13  SPA-GMN-W28-TYUKO-FLG           PIC 9(1).
                   13  SPA-GMN-W28-TSELNUM             PIC 9(3).
                   13  SPA-GMN-W28-TBTUNUM             PIC 9(2).
                   13  SPA-GMN-W28-TBTUNAME            PIC X(40).
                   13  SPA-GMN-W28-TKHNRYO             PIC X(60).
                   13  SPA-GMN-W28-TTANKA              PIC 9(04).
                   13  SPA-GMN-W28-TTANKAIP            PIC 9(04).
                   13  SPA-GMN-W28-TTANKARJ            PIC 9(04).
                   13  SPA-GMN-W28-TTAXNAME            PIC X(10).
                   13  SPA-GMN-W28-TSTYUKYMD           PIC X(9).
                   13  SPA-GMN-W28-TEDYUKYMD           PIC X(9).
      *
                 11  SPA-GMN-W28-BTULST-SEL            PIC 9(05).
      *
      *        入力項目内部領域
               09  SPA-NAI-W28-INPUT-AREA.
                 11  SPA-NAI-W28-DUMMY                 PIC X(01).
      *
      *        病棟情報リスト内部領域
               09  SPA-NAI-W28-BTULST-AREA.
                 11  SPA-NAI-W28-BTULST-ITM            OCCURS  400.
                   13  SPA-NAI-W28-TFTNKBN             PIC X(1).
                   13  SPA-NAI-W28-TTAXKBN             PIC X(1).
                   13  SPA-NAI-W28-TSTYUKYMD           PIC X(8).
                   13  SPA-NAI-W28-TEDYUKYMD           PIC X(8).
      *
      *        コンボ項目領域
               09  SPA-GMN-W28-CMB-DAT-AREA.
      *          選定入院料−負担区分
                 11  SPA-GMN-W28-CMB-SENTEI-FTNKBN.
                   13  SPA-GMN-W28-SENTEI-FTNKBN-CNT   PIC S9(9)
                                                       BINARY.
                   13  SPA-GMN-W28-SENTEI-FTNKBN-ITM   PIC X(22)
                                                       OCCURS 10.
      *
      *          選定入院料−消費税
                 11  SPA-GMN-W28-CMB-SENTEI-TAX.
                   13  SPA-GMN-W28-SENTEI-TAX-CNT      PIC S9(9)
                                                       BINARY.
                   13  SPA-GMN-W28-SENTEI-TAX-ITM      PIC X(20)
                                                       OCCURS  5.
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-KANJI                           PIC 9(01).
           03  FLG-TENSU                           PIC 9(01).
           03  FLG-ENTRYCHK                        PIC 9(01).
           03  FLG-YUKO                            PIC 9(01).
           03  FLG-SYS5001                         PIC 9(01).
           03  FLG-SYS5110                         PIC 9(01).
           03  FLG-SRH-END                         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDX3                                PIC 9(05).
           03  IDX4                                PIC 9(05).
           03  IDX5                                PIC 9(05).
           03  IDXI                                PIC 9(05).
           03  IDX-CMB                             PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDMSG                          PIC X(70). 
           03  WRK-YMD                             PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY                         PIC 9(04).
               05  WRK-SMM                         PIC 9(02).
               05  WRK-SDD                         PIC 9(02).
           03  WRK-HENYMDG                         PIC X(09).
           03  WRK-KANACHK-MAE-INPUT               PIC X(400).
           03  WRK-CMB-CD                          PIC X(10).
           03  WRK-COMBO-DAT.
               05  WRK-CMB-IDX                     PIC 9(03).
               05  WRK-CMB-ITM                     PIC X(100).
           03  WRK-KANRICD                         PIC X(04).
           03  WRK-KBNCD                           PIC X(08).
           03  WRK-IDX                             PIC 9(05).
           03  WRK-KJNYMD                          PIC X(08).
           03  WRK-KJNYMD-BAK                      PIC X(08).
           03  WRK-NUM-EDIT.
               05  WRK-99                  PIC 9(02).
               05  WRK-99999               PIC 9(05).
               05  WRK-999999              PIC 9(06).
               05  WRK-Z9                  PIC Z9.
               05  WRK-ZZ9                 PIC ZZ9.
               05  WRK-ZZZ9                PIC ZZZ9.
               05  WRK-ZZZZ9               PIC ZZZZ9.
               05  WRK-ZZZZZ9              PIC ZZZZZ9.
               05  WRK-ZZZZZZZZZ9          PIC ZZZZZZZZZ9.
           03  WRK-IPN-RJN-EDT.
               05  WRK-TANKAIP             PIC ZZZZ9.
               05  WRK-LBLSEP              PIC X(02).
               05  WRK-TANKARJ             PIC ZZZZ9.
           03  WRK-STSELNUM                PIC 9(05).
           03  WRK-EDSELNUM                PIC 9(03).
           03  WRK-EDBTUNUM                PIC X(06).
           03  WRK-STIDX                   PIC 9(05).
           03  WRK-EDIDX                   PIC 9(05).
           03  WRK-SELNUM                  PIC 9(03).
           03  WRK-KHNRYO                  PIC X(60).
      *    入力項目名
           03  WRK-MCP-WIDGET              PIC X(64).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    病棟情報
           COPY  "CPSK5001.INC".
      *    病棟特定入院料
           COPY  "CPSK5110.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
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
           MOVE    SPA-FREE        TO  SPA-W28FREE
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
           MOVE    SPA-W28FREE     TO  SPA-FREE
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
           MOVE    "W28    "           TO  MCP-WINDOW.
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
           MOVE    SPACE           TO  SPA-W28
                                           W28
           INITIALIZE                  SPA-W28
                                           W28
      *
      *    画面初期化処理
           PERFORM 420-CLEAR-SEC
      *
      *    コンボボックス初期化
           PERFORM 310-CMB-SYOKI-SEC
      *
      *    病棟リスト編集処理
           PERFORM 310-BTULST-EDIT-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コンボボックス初期処理
      *****************************************************************
       310-CMB-SYOKI-SEC               SECTION.
      *
           INITIALIZE              SPA-GMN-W28-CMB-DAT-AREA
      *
      *    負担区分
           PERFORM 310-CMB-SENTEI-FTNKBN-SET-SEC
      *
      *    消費税区分
           PERFORM 310-CMB-SENTEI-TAX-SET-SEC
      *
           .
       310-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    選定療養費−負担区分コンボ
      *****************************************************************
       310-CMB-SENTEI-FTNKBN-SET-SEC       SECTION.
      *
           MOVE    2               TO  SPA-GMN-W28-SENTEI-FTNKBN-CNT
      *
           MOVE    "1 定率設定"    TO  SPA-GMN-W28-SENTEI-FTNKBN-ITM (1)
      *
           MOVE    "2 定額設定"    TO  SPA-GMN-W28-SENTEI-FTNKBN-ITM (2)
      *
           .
       310-CMB-SENTEI-FTNKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    選定療養費−消費税コンボ
      *****************************************************************
       310-CMB-SENTEI-TAX-SET-SEC       SECTION.
      *
           MOVE    2               TO  SPA-GMN-W28-SENTEI-TAX-CNT
      *
           MOVE    "0 なし"        TO  SPA-GMN-W28-SENTEI-TAX-ITM (1)
      *
           MOVE    "1 あり"        TO  SPA-GMN-W28-SENTEI-TAX-ITM (2)
      *
           .
       310-CMB-SENTEI-TAX-SET-EXT.
           EXIT.
      *****************************************************************
      *    病棟リスト編集処理
      *****************************************************************
       310-BTULST-EDIT-SEC             SECTION.
      *
           INITIALIZE  SPA-GMN-W28-BTULST-AREA
                       SPA-NAI-W28-BTULST-AREA
      *
           MOVE    ZERO            TO  FLG-SYS5001
      *
           INITIALIZE  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1           TO  FLG-SYS5001
           END-IF
      *
           PERFORM UNTIL ( FLG-SYS5001         NOT =   ZERO )
                    OR   ( SPA-GMN-W28-BTULST-CNT >=   400 )
      *
               MOVE    MCPDATA-REC     TO  SYS-5001-REC
      *
               PERFORM 3101-BTULST-EDIT-SEC
      *
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    1           TO  FLG-SYS5001
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       310-BTULST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟リスト編集処理
      *****************************************************************
       3101-BTULST-EDIT-SEC            SECTION.
      *
           COMPUTE SPA-GMN-W28-BTULST-CNT
               =   SPA-GMN-W28-BTULST-CNT  +   1
      *
           MOVE    SPA-GMN-W28-BTULST-CNT
                                       TO  IDX1
      *
      *    有効
           IF    ( SYS-5001-EDYUKYMD  >=   SPA-SYSYMD )
            AND  ( SYS-5001-STYUKYMD  <=   SPA-NAI-W28-KJNYMD )
            AND  ( SYS-5001-EDYUKYMD  >=   SPA-NAI-W28-KJNYMD )
               MOVE    1               TO  SPA-GMN-W28-TYUKO-FLG (IDX1)
           ELSE
               MOVE    ZERO            TO  SPA-GMN-W28-TYUKO-FLG (IDX1)
           END-IF
      *
      *    番号
           MOVE    IDX1                TO  SPA-GMN-W28-TSELNUM (IDX1)
      *
      *    病棟番号
           MOVE    SYS-5001-BTU-NUM    TO  SPA-GMN-W28-TBTUNUM (IDX1)
      *
      *    病棟名称
           MOVE    SYS-5001-BTU-NAME   TO  SPA-GMN-W28-TBTUNAME(IDX1)
      *
      *    入院料
           PERFORM 3101-KHNRYO-EDIT-SEC
           MOVE    WRK-KHNRYO          TO  SPA-GMN-W28-TKHNRYO (IDX1)
      *
      *    負担計算・単価（金額）・消費税
           MOVE    SYS-5001-SENTEI-FTNKBN
                                       TO  SPA-NAI-W28-TFTNKBN (IDX1)
           EVALUATE SPA-NAI-W28-TFTNKBN (IDX1)
           WHEN    SPACE
               MOVE    "1"             TO  SPA-NAI-W28-TFTNKBN (IDX1)
               MOVE     10             TO  SPA-GMN-W28-TTANKA  (IDX1)
               MOVE     ZERO           TO  SPA-GMN-W28-TTANKAIP(IDX1)
               MOVE     ZERO           TO  SPA-GMN-W28-TTANKARJ(IDX1)
               MOVE     "0"            TO  SPA-NAI-W28-TTAXKBN (IDX1)
           WHEN    "1"
               MOVE    SYS-5001-SENTEI-TANKA
                                       TO  SPA-GMN-W28-TTANKA  (IDX1)
               MOVE    ZERO            TO  SPA-GMN-W28-TTANKAIP(IDX1)
               MOVE    ZERO            TO  SPA-GMN-W28-TTANKARJ(IDX1)
               MOVE    SYS-5001-SENTEI-TAXKBN
                                       TO  SPA-NAI-W28-TTAXKBN (IDX1)
           WHEN    "2"
               IF   (  SYS-5001-SENTEI-KINGAKU-X     IS    NUMERIC  )
                   MOVE    SYS-5001-SENTEI-KINGAKU
                                       TO  SPA-GMN-W28-TTANKA   (IDX1)
               END-IF
               IF   (  SYS-5001-SENTEI-IPN-KINGAKU-X IS    NUMERIC  )
                   MOVE    SYS-5001-SENTEI-IPN-KINGAKU
                                       TO  SPA-GMN-W28-TTANKAIP (IDX1)
               ELSE
                   MOVE    ZERO        TO  SPA-GMN-W28-TTANKAIP (IDX1)
               END-IF
               IF   (  SYS-5001-SENTEI-RJN-KINGAKU-X IS    NUMERIC  )
                   MOVE    SYS-5001-SENTEI-RJN-KINGAKU
                                       TO  SPA-GMN-W28-TTANKARJ (IDX1)
               ELSE
                   MOVE    ZERO        TO  SPA-GMN-W28-TTANKARJ (IDX1)
               END-IF
      *
               MOVE    SYS-5001-SENTEI-TAXKBN
                                       TO  SPA-NAI-W28-TTAXKBN (IDX1)
           END-EVALUATE
      *
           MOVE    SPA-NAI-W28-TTAXKBN (IDX1)
                                       TO  WRK-CMB-CD
           PERFORM 4103-CMB-SENTEI-TAX-SRH-SEC
           MOVE    WRK-CMB-ITM (3: )   TO  SPA-GMN-W28-TTAXNAME(IDX1)
      *
      *    開始有効年月日
           MOVE    SYS-5001-STYUKYMD   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-W28-TSTYUKYMD (IDX1)
           MOVE    WRK-SYMD            TO  SPA-NAI-W28-TSTYUKYMD (IDX1)
      *
      *    終了有効年月日
           MOVE    SYS-5001-EDYUKYMD   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-W28-TEDYUKYMD (IDX1)
           MOVE    WRK-SYMD            TO  SPA-NAI-W28-TEDYUKYMD (IDX1)
      *
           .
       3101-BTULST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院料編集処理
      *****************************************************************
       3101-KHNRYO-EDIT-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-KHNRYO
      *
           IF    ( SYS-5001-BTU-KHNSRYCD   NOT =   SPACE )
      *        点数マスタを検索し、入院基本料名を設定する
               INITIALIZE              TNS-TENSU-REC
               MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
               MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  TNS-SRYCD
               MOVE    SPA-NAI-W28-KJNYMD
                                       TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
               MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
               MOVE    "tbl_tensu" TO  MCP-TABLE
               MOVE    "key"       TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  TNS-TENSU-REC
                   MOVE    TNS-NAME    TO  WRK-KHNRYO
               END-IF
      *
               MOVE    "tbl_tensu" TO  MCP-TABLE
               MOVE    "key"       TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           ELSE
      *
      *        システム管理マスタを検索し、特定入院料名称を設定する
               INITIALIZE  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "5110"          TO  SYS-KANRICD
               MOVE    SYS-5001-BTU-TOKTEI-NYUIN
                                       TO  SYS-KBNCD
               MOVE    SPA-NAI-W28-KJNYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               MOVE    SYSKANRI-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  SYS-5110-REC
                   MOVE    SYS-5110-BTU-TOKUTEINYUIN-NM
                                       TO  WRK-KHNRYO
               END-IF
      *
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       3101-KHNRYO-EDIT-EXT.
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
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM  210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
      *        PERFORM 420-CLEAR-SEC
               PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *    基準日
           WHEN    "CLICKED"       ALSO    "B03"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 4101-GMN-SPA-SET-SEC
               MOVE    100                 TO  SPA-GMN-W28-CUR
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
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
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *
      *    基準日
           WHEN    "ACTIVATE"      ALSO    "KJNYMD"
               PERFORM 4100-KJNYMD-CHK-SEC
      *
      *    開始選択番号
           WHEN    "ACTIVATE"      ALSO    "STSELNUM"
               PERFORM 4100-STSELNUM-CHK-SEC
      *
      *    病棟リスト
           WHEN    ANY             ALSO    "BTULST"
               PERFORM 4100-BTULST-CHK-SEC
      *
           WHEN    OTHER
      *
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
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
           MOVE    ZERO        TO      SPA-GMN-W28-CUR
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
      *    終了選択番号
           MOVE    W28-EDSELNUM        TO  SPA-GMN-W28-EDSELNUM
      *
      *    選定療養費−負担区分
           MOVE    W28-SENTEI-FTNKBN   TO  SPA-GMN-W28-SENTEI-FTNKBN
      *
      *    選定療養費−点数単価
           MOVE    W28-SENTEI-TANKA    TO  SPA-GMN-W28-SENTEI-TANKA
      *
      *    選定療養費−点数単価（一般）
           MOVE    W28-SENTEI-TANKAIP  TO  SPA-GMN-W28-SENTEI-TANKAIP
      *
      *    選定療養費−点数単価（老人）
           MOVE    W28-SENTEI-TANKARJ  TO  SPA-GMN-W28-SENTEI-TANKARJ
      *
      *    選定療養費−消費税
           MOVE    W28-SENTEI-TAX      TO  SPA-GMN-W28-SENTEI-TAX
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
      *    開始選択番号
           PERFORM 4101-STSELNUM-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               MOVE    1      TO   SPA-GMN-W28-CUR
               PERFORM 4201-INPUT-AREA-CLEAR2-SEC
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    開始選択番号
           IF    ( SPA-GMN-W28-TYUKO-FLG (SPA-GMN-W28-STSELNUM)
                                           NOT =  1 )
               MOVE    "0009"      TO  SPA-ERRCD
               MOVE   1            TO   SPA-GMN-W28-CUR
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    終了選択番号
           PERFORM 4100-EDSELNUM-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    選定療養費−負担区分
           PERFORM 4100-SENTEI-FTNKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    選定療養費−点数単価
           PERFORM 4100-SENTEI-TANKA-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    選定療養費−点数単価（一般）
           PERFORM 4100-SENTEI-TANKAIP-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    選定療養費−点数単価（老人）
           PERFORM 4100-SENTEI-TANKARJ-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    選定療養費−消費税
           PERFORM 4100-SENTEI-TAX-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
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
           MOVE    "W28"               TO  SPA-MOTOPG
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
           INITIALIZE  SPA-GMN-W28-BTULST-AREA
                       SPA-NAI-W28-BTULST-AREA
                       SPA-GMN-W28-INPUT-AREA
                       SPA-NAI-W28-INPUT-AREA
                       SPA-GMN-W28-KJNYMD-AREA
      *
           MOVE    SPA-SYSYMD      TO  SPA-NAI-W28-KJNYMD
           MOVE    SPA-SYSYMDWH    TO  SPA-GMN-W28-KJNYMD
      *
           MOVE    1               TO  SPA-GMN-W28-CUR
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
           MOVE    SPA-GMN-W28-STSELNUM
                                       TO  WRK-STIDX
           IF    ( SPA-GMN-W28-EDSELNUM
                                       =   ZERO )
               MOVE    SPA-GMN-W28-STSELNUM
                                       TO  WRK-EDIDX
           ELSE
               MOVE    SPA-GMN-W28-EDSELNUM
                                       TO  WRK-EDIDX
           END-IF
      *
           PERFORM VARYING IDX1    FROM    WRK-STIDX   BY  1
                   UNTIL ( IDX1        >   WRK-EDIDX )
               IF    ( SPA-GMN-W28-TYUKO-FLG (IDX1)
                                       =   1 )
                   INITIALIZE              SYSKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
                   MOVE    "5001"          TO  SYS-KANRICD
                   MOVE    SPA-GMN-W28-TBTUNUM   (IDX1)
                                           TO  SYS-KBNCD
                   MOVE    SPA-NAI-W28-TSTYUKYMD (IDX1)
                                           TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key10"         TO  MCP-PATHNAME
                   MOVE    SYSKANRI-REC    TO  MCPDATA-REC
      *            更新対象のデータが存在するかチェック
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       MOVE    "4001"      TO  SPA-ERRCD
                       GO  TO  481-TOUROKU-EXT
                   END-IF
      *
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key10"         TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
      *
                   MOVE    MCPDATA-REC     TO  SYS-5001-REC
      *
                   PERFORM 4811-SYS5001-EDIT-SEC
      *
                   PERFORM 800-MCNDATE-SEC
                   MOVE    SMCNDATE-YMD    TO  SYS-5001-UPYMD
                   MOVE    SMCNDATE-HMS    TO  SYS-5001-UPHMS
      *
                   MOVE    SYS-5001-REC    TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       MOVE    "4002"      TO  SPA-ERRCD
                       GO  TO  481-TOUROKU-EXT
                   END-IF
               END-IF
           END-PERFORM
      *
      *    病棟リスト編集処理
           PERFORM 310-BTULST-EDIT-SEC
      *
      *    画面処理化
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           .
       481-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    病棟選定入院料編集処理
      *****************************************************************
       4811-SYS5001-EDIT-SEC       SECTION.
      *
      *    選定入院負担区分
           MOVE    SPA-GMN-W28-SENTEI-FTNKBN-CD
                                   TO  SYS-5001-SENTEI-FTNKBN
      *
      *    選定入院消費税区分
           MOVE    SPA-GMN-W28-SENTEI-TAX-CD
                                   TO  SYS-5001-SENTEI-TAXKBN
      *
      *    選定入院単価（金額）
           EVALUATE    SPA-GMN-W28-SENTEI-FTNKBN-CD
           WHEN    "1"
               MOVE    SPA-GMN-W28-SENTEI-TANKA
                                   TO  SYS-5001-SENTEI-TANKA
               MOVE    ZERO        TO  SYS-5001-SENTEI-KINGAKU
               MOVE    ZERO        TO  SYS-5001-SENTEI-IPN-KINGAKU
               MOVE    ZERO        TO  SYS-5001-SENTEI-RJN-KINGAKU
           WHEN    "2"
               MOVE    ZERO        TO  SYS-5001-SENTEI-TANKA
               MOVE    SPA-GMN-W28-SENTEI-TANKA
                                   TO  SYS-5001-SENTEI-KINGAKU
               MOVE    SPA-GMN-W28-SENTEI-TANKAIP
                                   TO  SYS-5001-SENTEI-IPN-KINGAKU
               MOVE    SPA-GMN-W28-SENTEI-TANKARJ
                                   TO  SYS-5001-SENTEI-RJN-KINGAKU
           WHEN    OTHER
               MOVE    ZERO        TO  SYS-5001-SENTEI-KINGAKU
               MOVE    ZERO        TO  SYS-5001-SENTEI-IPN-KINGAKU
               MOVE    ZERO        TO  SYS-5001-SENTEI-RJN-KINGAKU
           END-EVALUATE
      *
           .
      *
       4811-SYS5001-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    基準日チェック処理
      *****************************************************************
       4100-KJNYMD-CHK-SEC             SECTION.
      *
           DISPLAY "SPA-NAI-W28-KJNYMD = " SPA-NAI-W28-KJNYMD
      *
           MOVE    SPA-NAI-W28-KJNYMD
                                   TO  WRK-KJNYMD-BAK
      *
           MOVE    SPACE           TO  SPA-NAI-W28-KJNYMD
      *
           MOVE    W28-KJNYMD      TO  SPA-GMN-W28-KJNYMD
      *
           IF    ( SPA-GMN-W28-KJNYMD  =   SPACE )
               MOVE    SPA-SYSYMD      TO  SPA-NAI-W28-KJNYMD
               MOVE    SPA-SYSYMDWH    TO  SPA-GMN-W28-KJNYMD
               MOVE    1               TO  SPA-GMN-W28-CUR
               PERFORM 4201-INPUT-AREA-CLEAR-SEC
               PERFORM 310-BTULST-EDIT-SEC
               GO  TO  4100-KJNYMD-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-W28-KJNYMD
                                   TO   WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF    ( SPA-ERRCD   NOT =    SPACE )
               MOVE    WRK-KJNYMD-BAK
                                   TO   WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-W28-KJNYMD
               MOVE    WRK-SYMD    TO  SPA-NAI-W28-KJNYMD
               MOVE    100         TO  SPA-GMN-W28-CUR
               GO  TO  4100-KJNYMD-CHK-EXT
           END-IF
      *
           MOVE    WRK-HENYMDG     TO  SPA-GMN-W28-KJNYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-W28-KJNYMD
      *
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
           PERFORM 310-BTULST-EDIT-SEC
      *
           MOVE    1               TO  SPA-GMN-W28-CUR
      *
           .
      *
       4100-KJNYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始選択番号チェック処理
      *****************************************************************
       4100-STSELNUM-CHK-SEC           SECTION.
      *
           MOVE   ZERO         TO  SPA-GMN-W28-STBTUNUM
      *
           MOVE   W28-STSELNUM TO  SPA-GMN-W28-STSELNUM
      *
           PERFORM 4101-STSELNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    1       TO   SPA-GMN-W28-CUR
               PERFORM 4201-INPUT-AREA-CLEAR2-SEC
               GO  TO  4100-STSELNUM-CHK-EXT
           END-IF
      *
      *    入力領域編集処理
           PERFORM 4100-INPUT-AREA-EDIT-SEC
           MOVE    SPA-GMN-W28-STSELNUM
                                   TO  SPA-GMN-W28-BTULST-SEL
      *
      *
      *    現在有効な病棟が選択されていること
           IF    ( SPA-GMN-W28-TYUKO-FLG (SPA-GMN-W28-STSELNUM)
                                           NOT =  1 )
               MOVE    "0009"      TO  SPA-ERRCD
               MOVE   1            TO   SPA-GMN-W28-CUR
               GO  TO  4100-STSELNUM-CHK-EXT
           END-IF
      *
           MOVE    2               TO   SPA-GMN-W28-CUR
      *
           .
       4100-STSELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始選択番号チェック処理
      *****************************************************************
       4101-STSELNUM-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-W28-STSELNUM    =   ZERO )
               MOVE    "1001" TO   SPA-ERRCD
               GO  TO  4101-STSELNUM-CHK-EXT
           END-IF
      *
      *    範囲内の選択番号が入力されていること
           IF    ( SPA-GMN-W28-STSELNUM   <=  SPA-GMN-W28-BTULST-CNT )
               CONTINUE
           ELSE
               MOVE    "0006"      TO  SPA-ERRCD
               GO  TO  4101-STSELNUM-CHK-EXT
           END-IF
      *
           .
       4101-STSELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了選択番号チェック処理
      *****************************************************************
       4100-EDSELNUM-CHK-SEC           SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-W28-EDBTUNUM
      *
           IF    ( SPA-GMN-W28-EDSELNUM    =   ZERO )
               GO  TO  4100-EDSELNUM-CHK-EXT
           END-IF
      *
      *    範囲内の選択番号が入力されていること
           IF    ( SPA-GMN-W28-EDSELNUM   <=  SPA-GMN-W28-BTULST-CNT )
               CONTINUE
           ELSE
               MOVE    "0006"      TO  SPA-ERRCD
               MOVE    2           TO  SPA-GMN-W28-CUR
               GO  TO  4100-EDSELNUM-CHK-EXT
           END-IF
      *
      *    終了選択番号が入力されてる場合、
      *    開始選択番号≦終了選択番号であること
           IF    ( SPA-GMN-W28-STSELNUM
                                       <=  SPA-GMN-W28-EDSELNUM )
               CONTINUE
           ELSE
               MOVE    "1003"  TO  SPA-ERRCD
               MOVE    2       TO  SPA-GMN-W28-CUR
               GO  TO  4100-EDSELNUM-CHK-EXT
           END-IF
      *
           MOVE   SPA-GMN-W28-EDSELNUM
                                   TO  IDX1
           MOVE   SPA-GMN-W28-TBTUNUM (IDX1)
                                   TO  SPA-GMN-W28-EDBTUNUM
      *
           .
       4100-EDSELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選定療養費−負担区分
      *****************************************************************
       4100-SENTEI-FTNKBN-CHK-SEC         SECTION.
      *
      *    該当コードのデータが存在するかチェック
           MOVE    SPA-GMN-W28-SENTEI-FTNKBN-CD
                                       TO  WRK-CMB-CD
           PERFORM 4103-CMB-SENTEI-FTNKBN-SRH-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W28-SENTEI-FTNKBN
           ELSE
               MOVE    SPA-GMN-W28-SENTEI-FTNKBN-ITM (1)
                                       TO  SPA-GMN-W28-SENTEI-FTNKBN
           END-IF
      *
           IF    ( SPA-GMN-W28-SENTEI-FTNKBN-CD    NOT =   "2" )
               MOVE    ZERO            TO  SPA-GMN-W28-SENTEI-TANKAIP
               MOVE    ZERO            TO  SPA-GMN-W28-SENTEI-TANKARJ
           END-IF
      *
           .
       4100-SENTEI-FTNKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選定療養費−点数単価
      *****************************************************************
       4100-SENTEI-TANKA-CHK-SEC       SECTION.
      *
      *
           .
       4100-SENTEI-TANKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選定療養費−点数単価（一般）
      *****************************************************************
       4100-SENTEI-TANKAIP-CHK-SEC     SECTION.
      *
      *
           .
       4100-SENTEI-TANKAIP-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選定療養費−点数単価（老人）
      *****************************************************************
       4100-SENTEI-TANKARJ-CHK-SEC     SECTION.
      *
      *
           .
       4100-SENTEI-TANKARJ-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選定療養費−消費税
      *****************************************************************
       4100-SENTEI-TAX-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-W28-SENTEI-TAX =   SPACE )
               MOVE    SPA-GMN-W28-SENTEI-TAX-ITM (1)
                                       TO  SPA-GMN-W28-SENTEI-TAX
               GO  TO  4100-SENTEI-TAX-CHK-EXT
           END-IF
      *
      *    該当コードのデータが存在するかチェック
           MOVE    SPA-GMN-W28-SENTEI-TAX-CD
                                       TO  WRK-CMB-CD
           PERFORM 4103-CMB-SENTEI-TAX-SRH-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W28-SENTEI-TAX
           ELSE
               MOVE    SPA-GMN-W28-SENTEI-TAX-ITM (1)
                                       TO  SPA-GMN-W28-SENTEI-TAX
           END-IF
      *
           .
       4100-SENTEI-TAX-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力領域編集処理
      *****************************************************************
       4100-INPUT-AREA-EDIT-SEC        SECTION.
      *
           MOVE    SPA-GMN-W28-STSELNUM
                                       TO  WRK-STSELNUM
      *
           PERFORM 4201-INPUT-AREA-CLEAR2-SEC
      *
           MOVE    WRK-STSELNUM        TO  IDXI
      *
      *    開始選択番号
           MOVE    WRK-STSELNUM        TO  SPA-GMN-W28-STSELNUM
      *
      *    開始病棟番号
           MOVE    SPA-GMN-W28-TBTUNUM (IDXI)
                                       TO  SPA-GMN-W28-STBTUNUM
      *
           MOVE    SPA-NAI-W28-TFTNKBN (IDXI)
                                       TO  WRK-CMB-CD
           PERFORM 4103-CMB-SENTEI-FTNKBN-SRH-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W28-SENTEI-FTNKBN
      *
           MOVE    SPA-GMN-W28-TTANKA  (IDXI)
                                       TO  SPA-GMN-W28-SENTEI-TANKA
           MOVE    SPA-GMN-W28-TTANKAIP(IDXI)
                                       TO  SPA-GMN-W28-SENTEI-TANKAIP
           MOVE    SPA-GMN-W28-TTANKARJ(IDXI)
                                       TO  SPA-GMN-W28-SENTEI-TANKARJ
      *
           MOVE    SPA-NAI-W28-TTAXKBN (IDXI)
                                       TO  WRK-CMB-CD
           PERFORM 4103-CMB-SENTEI-TAX-SRH-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-W28-SENTEI-TAX
      *
           .
      *
       4100-INPUT-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟リストチェック処理
      *****************************************************************
       4100-BTULST-CHK-SEC             SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-W28-BTULST-CNT )
               IF    ( W28-BTULST-SELECT (IDX1)    =   "T" )
      *
      *
                   MOVE    IDX1        TO  SPA-GMN-W28-STSELNUM
      *
      *            入力領域編集処理
                   PERFORM 4100-INPUT-AREA-EDIT-SEC
                   MOVE    SPA-GMN-W28-STSELNUM
                                       TO  SPA-GMN-W28-BTULST-SEL
      *
      *            現在有効な病棟が選択されていること
                   IF    ( SPA-GMN-W28-TYUKO-FLG (IDX1)    NOT =  1 )
                       MOVE    "0009"      TO  SPA-ERRCD
                       MOVE    1           TO  SPA-GMN-W28-CUR
                       GO  TO  4100-BTULST-CHK-EXT
                   END-IF
      *
                   MOVE    2           TO  SPA-GMN-W28-CUR
                   MOVE    SPA-GMN-W28-BTULST-CNT
                                       TO  IDX1
               END-IF
      *
           END-PERFORM
      *
           .
       4100-BTULST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力情報領域初期化処理
      *****************************************************************
       4201-INPUT-AREA-CLEAR-SEC       SECTION.
      *
           INITIALIZE  SPA-GMN-W28-INPUT-AREA
                       SPA-NAI-W28-INPUT-AREA
           MOVE   ZERO         TO  SPA-GMN-W28-BTULST-SEL
           MOVE    1           TO  SPA-GMN-W28-CUR
      *
           .
      *
       4201-INPUT-AREA-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    入力情報領域初期化処理２（終了選択番号は初期化しない）
      *****************************************************************
       4201-INPUT-AREA-CLEAR2-SEC      SECTION.
      *
           MOVE    SPA-GMN-W28-EDSELNUM
                                   TO  WRK-EDSELNUM
           MOVE    SPA-GMN-W28-EDBTUNUM
                                   TO  WRK-EDBTUNUM
      *
           INITIALIZE  SPA-GMN-W28-INPUT-AREA
                       SPA-NAI-W28-INPUT-AREA
           MOVE   ZERO         TO  SPA-GMN-W28-BTULST-SEL
           MOVE   1            TO  SPA-GMN-W28-CUR
      *
           MOVE    WRK-EDSELNUM    TO  SPA-GMN-W28-EDSELNUM
           MOVE    WRK-EDBTUNUM    TO  SPA-GMN-W28-EDBTUNUM
      *
           .
      *
       4201-INPUT-AREA-CLEAR2-EXT.
           EXIT.
      *****************************************************************
      *    選定入院料−負担区分サーチ処理
      *****************************************************************
       4103-CMB-SENTEI-FTNKBN-SRH-SEC  SECTION.
      *
           INITIALIZE                  WRK-COMBO-DAT
           MOVE    ZERO                TO  FLG-SRH-END
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W28-SENTEI-FTNKBN-CNT )
                    OR   ( FLG-SRH-END NOT =   ZERO )
      *
               IF    ( SPA-GMN-W28-SENTEI-FTNKBN-ITM (IDX-CMB) (1 : 1)
                                       =   WRK-CMB-CD )
      *
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
      *
                   MOVE    SPA-GMN-W28-SENTEI-FTNKBN-ITM (IDX-CMB)
                                       TO  WRK-CMB-ITM
      *
                   MOVE    1           TO  FLG-SRH-END
      *
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4103-CMB-SENTEI-FTNKBN-SRH-EXT.
           EXIT.
      *****************************************************************
      *    選定入院料−消費税サーチ処理
      *****************************************************************
       4103-CMB-SENTEI-TAX-SRH-SEC     SECTION.
      *
           INITIALIZE                  WRK-COMBO-DAT
           MOVE    ZERO                TO  FLG-SRH-END
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W28-SENTEI-TAX-CNT )
                    OR   ( FLG-SRH-END NOT =   ZERO )
      *
               IF    ( SPA-GMN-W28-SENTEI-TAX-ITM (IDX-CMB) (1 : 1)
                                       =   WRK-CMB-CD )
      *
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
      *
                   MOVE    SPA-GMN-W28-SENTEI-TAX-ITM (IDX-CMB)
                                       TO  WRK-CMB-ITM
      *
                   MOVE    1           TO  FLG-SRH-END
      *
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4103-CMB-SENTEI-TAX-SRH-EXT.
           EXIT.
      *****************************************************************
      *    共通関連チェック
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
      *    基準日が入力されていること
           IF    ( SPA-GMN-W28-KJNYMD  =   SPACE )
               MOVE    "0010"      TO  SPA-ERRCD
               MOVE    100         TO  SPA-GMN-W28-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
           
      *    開始選択番号が入力されていること
           IF    ( SPA-GMN-W28-STSELNUM    =   ZERO )
               MOVE    "1001"      TO  SPA-ERRCD
               MOVE    1           TO  SPA-GMN-W28-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    終了選択番号が入力されていない場合、
      *    開始選択番号の対象データが有効なデータであること
           IF    ( SPA-GMN-W28-EDSELNUM    =   ZERO )
               MOVE    SPA-GMN-W28-STSELNUM
                                   TO  IDX1
               IF    ( SPA-GMN-W28-TYUKO-FLG (IDX1)
                                           =   1 )
                   CONTINUE
               ELSE
                   MOVE    "1002"  TO  SPA-ERRCD
                   MOVE    1       TO  SPA-GMN-W28-CUR
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
           END-IF
      *
      *    終了選択番号が入力されてる場合、
      *    選択範囲に有効な病棟が存在していること
           IF    ( SPA-GMN-W28-EDSELNUM    >   ZERO )
               MOVE    SPA-GMN-W28-STSELNUM    TO  WRK-STIDX
               MOVE    SPA-GMN-W28-EDSELNUM    TO  WRK-EDIDX
               MOVE    ZERO                    TO  FLG-YUKO
               PERFORM VARYING IDX1    FROM    WRK-STIDX   BY  1
                       UNTIL ( IDX1    >       WRK-EDIDX )
                   IF    ( SPA-GMN-W28-TYUKO-FLG (IDX1)    =   1 )
                       MOVE    1               TO  FLG-YUKO
                       MOVE    WRK-EDIDX       TO  IDX1
                   END-IF
               END-PERFORM
      *
               IF    ( FLG-YUKO    =   ZERO )
                   MOVE    "1004"  TO  SPA-ERRCD
                   MOVE    1       TO  SPA-GMN-W28-CUR
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
      *
           END-IF
      *
      *    選定入院料−負担区分が入力されていること
           IF    ( SPA-GMN-W28-SENTEI-FTNKBN   NOT =   SPACE )
               CONTINUE
           ELSE
               MOVE    3             TO  SPA-GMN-W28-CUR
               MOVE    "1006"        TO  SPA-ERRCD
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    選定入院料−負担区分が定率設定の時、
      *    点数単価が９９点以下であること
           IF    ( SPA-GMN-W28-SENTEI-FTNKBN-CD   =   "1" )
               IF    ( SPA-GMN-W28-SENTEI-TANKA  <=   99  )
                   CONTINUE
               ELSE
                   MOVE    4             TO  SPA-GMN-W28-CUR
                   MOVE    "1007"        TO  SPA-ERRCD
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
           END-IF
      *
      *    選定入院料−負担区分が定額設定の時、
      *    一般／老人と一般１日・老人１日と一緒に設定されていないこと
           IF    ( SPA-GMN-W28-SENTEI-FTNKBN-CD   =   "2" )
               IF    ( SPA-GMN-W28-SENTEI-TANKA   >   ZERO  )
                   IF    ( SPA-GMN-W28-SENTEI-TANKAIP =    ZERO )
                    AND  ( SPA-GMN-W28-SENTEI-TANKARJ =    ZERO )
                       CONTINUE
                   ELSE
                       MOVE    4       TO  SPA-GMN-W28-CUR
                       MOVE    "1008"  TO  SPA-ERRCD
                       GO  TO  4900-KANRENCHK-EXT
                   END-IF
               END-IF
           END-IF
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
           MOVE    "W28    "           TO  MCP-WINDOW.
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
           INITIALIZE                          W28
      *
      *    開始選択番号
           MOVE    SPA-GMN-W28-STSELNUM    TO  W28-STSELNUM
      *    終了選択番号
           MOVE    SPA-GMN-W28-EDSELNUM    TO  W28-EDSELNUM
      *    開始病棟番号
           MOVE    SPA-GMN-W28-STBTUNUM    TO  WRK-Z9
           MOVE    WRK-Z9                  TO  W28-STBTUNUM
      *    終了病棟番号
           MOVE    SPA-GMN-W28-EDBTUNUM    TO  WRK-Z9
           MOVE    WRK-Z9                  TO  W28-EDBTUNUM
      *    基準日
           MOVE    SPA-GMN-W28-KJNYMD      TO  W28-KJNYMD
      *
      *
      *    選定療養費−負担区分
           MOVE    SPA-GMN-W28-SENTEI-FTNKBN
                                   TO  W28-SENTEI-FTNKBN
           MOVE    SPA-GMN-W28-SENTEI-FTNKBN-CNT
                                   TO  W28-SENTEI-FTNKBN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   W28-SENTEI-FTNKBN-CNT )
      *
               MOVE    SPA-GMN-W28-SENTEI-FTNKBN-ITM (IDX1)
                                   TO  W28-SENTEI-FTNKBN-ITM (IDX1)
      *
           END-PERFORM
      *
      *    選定療養費−点数単価
           MOVE    SPA-GMN-W28-SENTEI-TANKA
                                   TO  W28-SENTEI-TANKA
      *    選定療養費−点数単価（一般）
           MOVE    SPA-GMN-W28-SENTEI-TANKAIP
                                   TO  W28-SENTEI-TANKAIP
      *    選定療養費−点数単価（老人）
           MOVE    SPA-GMN-W28-SENTEI-TANKARJ
                                   TO  W28-SENTEI-TANKARJ
      *
      *    選定療養費−ラベル
           EVALUATE    SPA-GMN-W28-SENTEI-FTNKBN-CD
           WHEN    "1"
               MOVE    "点数単価"  TO  W28-LBL-SENTEI-TANKA
               MOVE    SPACE       TO  W28-LBL-SENTEI-EN
           WHEN    "2"
               MOVE    "一般／老人"
                                   TO  W28-LBL-SENTEI-TANKA
               MOVE    "円"        TO  W28-LBL-SENTEI-EN
               MOVE    "一般１日"  TO  W28-LBL-SENTEI-TANKAIP
               MOVE    "円"        TO  W28-LBL-SENTEI-ENIP
               MOVE    "老人１日"  TO  W28-LBL-SENTEI-TANKARJ
               MOVE    "円"        TO  W28-LBL-SENTEI-ENRJ
           END-EVALUATE
      *
      *    選定療養費−消費税
           MOVE    SPA-GMN-W28-SENTEI-TAX
                                   TO  W28-SENTEI-TAX
           MOVE    SPA-GMN-W28-SENTEI-TAX-CNT
                                   TO  W28-SENTEI-TAX-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   W28-SENTEI-TAX-CNT )
      *
               MOVE    SPA-GMN-W28-SENTEI-TAX-ITM (IDX1)
                                   TO  W28-SENTEI-TAX-ITM (IDX1)
      *
           END-PERFORM
      *
      *    病棟情報リスト領域
           MOVE    SPA-GMN-W28-BTULST-CNT  TO  W28-BTULST-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-W28-BTULST-CNT )
      *
      *        有効マーク
               IF    ( SPA-GMN-W28-TYUKO-FLG (IDX1)
                                           =   1 )
                   MOVE    "  ◎"          TO  W28-TYUKO (IDX1)
               ELSE
                   MOVE    SPACE           TO  W28-TYUKO (IDX1)
               END-IF
      *
      *        選択番号
               MOVE    SPA-GMN-W28-TSELNUM (IDX1)
                                           TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9            TO  W28-TSELNUM (IDX1)
      *
      *        病棟番号
               MOVE    SPA-GMN-W28-TBTUNUM (IDX1)
                                           TO  WRK-ZZZZZ9
               MOVE    WRK-ZZZZZ9          TO  W28-TBTUNUM (IDX1)
      *
      *        病棟名称
               MOVE    SPA-GMN-W28-TBTUNAME (IDX1)
                                           TO  W28-TBTUNAME (IDX1)
      *
      *        入院料
               MOVE    SPA-GMN-W28-TKHNRYO  (IDX1)
                                           TO  W28-TKHNRYO  (IDX1)
      *
               EVALUATE SPA-NAI-W28-TFTNKBN (IDX1)
               WHEN    SPACE
                   MOVE    "定率"      TO  W28-TFTNNAME (IDX1)
               WHEN    "1"
                   MOVE    "定率"      TO  W28-TFTNNAME (IDX1)
               WHEN    "2"
                   IF    ( SPA-GMN-W28-TTANKAIP (IDX1)     >   ZERO )
                    OR   ( SPA-GMN-W28-TTANKARJ (IDX1)     >   ZERO )
                       MOVE    "定額(一：老)"
                                       TO  W28-TFTNNAME (IDX1)
                   ELSE
                       MOVE    "定額"  TO  W28-TFTNNAME (IDX1)
                   END-IF
               END-EVALUATE
      *
      *        単価（金額）
               IF    ( SPA-GMN-W28-TTANKAIP (IDX1)     >   ZERO )
                OR   ( SPA-GMN-W28-TTANKARJ (IDX1)     >   ZERO )
                   MOVE    SPACE           TO  WRK-IPN-RJN-EDT
                   MOVE    SPA-GMN-W28-TTANKAIP (IDX1)
                                           TO  WRK-TANKAIP
                   MOVE    "："             TO  WRK-LBLSEP
                   MOVE    SPA-GMN-W28-TTANKARJ (IDX1)
                                           TO  WRK-TANKARJ
                   MOVE    WRK-IPN-RJN-EDT TO  W28-TTANKA (IDX1)
               ELSE
                   MOVE    SPA-GMN-W28-TTANKA (IDX1)
                                           TO  WRK-ZZZZZZZZZ9
                   MOVE    WRK-ZZZZZZZZZ9
                                           TO  W28-TTANKA (IDX1)
               END-IF
      *
      *        消費税
               MOVE    SPA-GMN-W28-TTAXNAME(IDX1)
                                           TO  W28-TTAXNAME(IDX1)
      *        開始有効年月日
               MOVE    SPA-GMN-W28-TSTYUKYMD (IDX1)
                               TO  W28-TSTYUKYMD (IDX1)
      *
      *        終了有効年月日
               MOVE    SPA-GMN-W28-TEDYUKYMD (IDX1)
                               TO  W28-TEDYUKYMD (IDX1)
      *
               IF    ( SPA-GMN-W28-BTULST-SEL  =   IDX1 )
                   MOVE    "T"             TO  W28-BTULST-SELECT (IDX1)
               ELSE
                   MOVE    "F"             TO  W28-BTULST-SELECT (IDX1)
               END-IF
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
           MOVE    WIDGET-NORMAL       TO  W28-SENTEI-TANKAIP-STATE
           MOVE    WIDGET-NORMAL       TO  W28-SENTEI-TANKARJ-STATE
      *
           IF    ( SPA-GMN-W28-SENTEI-FTNKBN-CD    =   "1" )
               MOVE    WIDGET-INSENSITIVE
                                       TO  W28-SENTEI-TANKAIP-STATE
                                           W28-SENTEI-TANKARJ-STATE
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
                  (SPA-GMN-W28-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           IF    ( SPA-GMN-W28-CUR     =    006 )
               IF    ( W28-SENTEI-TANKAIP-STATE =  WIDGET-INSENSITIVE )
                   MOVE    007             TO  SPA-GMN-W28-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-W28-CUR     =    007 )
               IF    ( W28-SENTEI-TANKARJ-STATE =  WIDGET-INSENSITIVE )
                   MOVE    005             TO  SPA-GMN-W28-CUR
               END-IF
           END-IF
      *
           EVALUATE    SPA-GMN-W28-CUR
           WHEN    001
               MOVE    "STSELNUM"          TO  MCP-WIDGET
           WHEN    002
               MOVE    "EDSELNUM"          TO  MCP-WIDGET
           WHEN    003
               MOVE    "SENTEI_FTNKBN"     TO  MCP-WIDGET
           WHEN    004
               MOVE    "SENTEI_TANKA"      TO  MCP-WIDGET
           WHEN    005
               MOVE    "SENTEI_TAX"        TO  MCP-WIDGET
           WHEN    006
               MOVE    "SENTEI_TANKAIP"    TO  MCP-WIDGET
           WHEN    007
               MOVE    "SENTEI_TANKARJ"    TO  MCP-WIDGET
           WHEN    100
               MOVE    "KJNYMD"            TO  MCP-WIDGET
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
           WHEN    "STSELNUM"
               MOVE    002             TO  SPA-GMN-W28-CUR
           WHEN    "EDSELNUM"
               MOVE    003             TO  SPA-GMN-W28-CUR
           WHEN    "SENTEI_FTNKBN"
               MOVE    004             TO  SPA-GMN-W28-CUR
           WHEN    "SENTEI_TANKA"
               MOVE    006             TO  SPA-GMN-W28-CUR
           WHEN    "SENTEI_TANKAIP"
               MOVE    007             TO  SPA-GMN-W28-CUR
           WHEN    "SENTEI_TANKARJ"
               MOVE    005             TO  SPA-GMN-W28-CUR
           WHEN    "SENTEI_TAX"
               MOVE    212             TO  SPA-GMN-W28-CUR
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
                   MOVE    "入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "病棟名称を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "入力番号に該当する病棟は存在しません"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "範囲内の選択番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE    "選択された病棟は現在有効な病棟ではありません"
                                       TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE    "基準日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "選択番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "選択された病棟は現在有効な病棟でないため"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "更新できません"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1003"
               MOVE    "範囲選択の指定に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "選択範囲に現在有効な病棟がありません"
                                       TO  SPA-ERRMSG
           WHEN    "1006"
               MOVE    "負担計算を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1007"
               MOVE    "点数単価は９９点以下で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1008"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "一般／老人と一般１日（老人１日）はいずれ"
                                           DELIMITED   BY  SIZE
                       "かを入力してください"
                                           DELIMITED   BY  SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "4001"  THRU    "4999"
               MOVE    "更新できませんでした"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W28"               TO  SPA-MOTOPG
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
               MOVE    "選択範囲内の有効な病棟について登録処理を"
                                        TO  WRK-WIDMSG (1 : 40)
               MOVE    "します"
                                        TO  WRK-WIDMSG (41 : )
           END-EVALUATE
      *
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W28"                TO  SPA-MOTOPG
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
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC             SECTION.
      *
           IF    ( WRK-YMD         =   "00000000" 
                                   OR  "99999999" )
               MOVE    WRK-YMD     TO  WRK-HENYMDG
                                       WRK-SYMD
               GO  TO  5002-HIZUKE-CHK-EXT
           END-IF
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全角半角混在チェック
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-MAE-INPUT
                                   =   SPACE )
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    1           TO  FLG-ENTRYCHK
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
      *    全角でない場合はエラーとする。
           IF      ( KANACHK-SYUBETU   NOT =   2 )
               MOVE    1           TO  FLG-ENTRYCHK
           END-IF
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
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC          SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
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
      *    テーブル読込処理
      *****************************************************************
       910-DBFETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
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
           CALL    "ORCDBMAIN"      USING      MCPAREA
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
           CALL    "ORCDBMAIN"      USING      MCPAREA
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
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

