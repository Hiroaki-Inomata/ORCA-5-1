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
       PROGRAM-ID.             ORCGW30.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 入院指示せん設定（Ｗ３０）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/02/07    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/05/02  グループ診療対応
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
       01  SPA-W30.
           03  SPA-W30-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(03).
               05  SPA-GMN-CUR         PIC 9(02).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-STYUKYMD    PIC X(09).
                   07  SPA-GMN-EDYUKYMD    PIC X(09).
      *            入院指示せん区分
                   07  SPA-GMN-SIJISENKBN-G.
                       09  SPA-GMN-SIJISENKBN      PIC X(01).
                       09  SPA-GMN-SIJISENKBN-H    PIC X(01).
                       09  SPA-GMN-SIJISENKBN-MEI  PIC X(10).
                   07  SPA-GMN-SIJISENKBN-COMB.
                       09  SPA-GMN-SIJISENKBN-LST  PIC X(12)
                                                   OCCURS   2.
                   07  SPA-SIJISEN-MAX             PIC 9(04).
      *            入院処方箋
                   07  SPA-GMN-SYOHOU-AREA.
                       09  SPA-GMN-SYOHOU-TBL          OCCURS  4.
                           11  SPA-GMN-SYOHOU-HEAD     PIC X(30).
                           11  SPA-GMN-SYOHOU-PRT      PIC 9(02).
      *            入院麻薬処方箋
                   07  SPA-GMN-MAYAKU-AREA.
                       09  SPA-GMN-MAYAKU-TBL          OCCURS  4.
                           11  SPA-GMN-MAYAKU-HEAD     PIC X(30).
                           11  SPA-GMN-MAYAKU-PRT      PIC 9(02).
      *            注射処方箋
                   07  SPA-GMN-CHUSYA-AREA.
                       09  SPA-GMN-CHUSYA-TBL          OCCURS  4.
                           11  SPA-GMN-CHUSYA-HEAD     PIC X(30).
                           11  SPA-GMN-CHUSYA-PRT      PIC 9(02).
      *            注射麻薬処方箋
                   07  SPA-GMN-CHUMAYA-AREA.
                       09  SPA-GMN-CHUMAYA-TBL        OCCURS  4.
                           11  SPA-GMN-CHUMAYA-HEAD   PIC X(30).
                           11  SPA-GMN-CHUMAYA-PRT    PIC 9(02).
      *            指示箋
                   07  SPA-GMN-SIJISEN-AREA.
                       09  SPA-GMN-SIJISEN-TBL         OCCURS  4.
                           11  SPA-GMN-SIJISEN-HEAD    PIC X(30).
                           11  SPA-GMN-SIJISEN-PRT     PIC 9(02).
      *
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-STYUKYMD       PIC 9(08).
                   07  SPA-NAI-EDYUKYMD       PIC 9(08).
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-INIT            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
           03  IDT                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-HEAD-INPUT           PIC X(30).
      *
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
      *
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
      *    入院指示せん設定情報
           COPY  "CPSK5007.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
      *
           COPY  "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
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
           MOVE    SPA-FREE        TO  SPA-W30
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
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W30         TO  SPA-FREE
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
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
                   PERFORM 510-GSRAUTH-SEC
               END-IF
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
               END-IF
           END-IF
      *
           IF     (FLG-END             =   ZERO ) AND
                  (SPA-ERRCD       NOT =   SPACE)
               MOVE    SPACE                TO  LINKAREA
               MOVE    SPACE                TO  MCP-PUTTYPE
               MOVE    "W30"                TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                    TO  FLG-END
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
           EVALUATE      SPA-WIDCD
               WHEN   "1001"
      *            更新
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM  491-KOUSHIN-SYORI-SEC
                   ELSE
                       MOVE     1              TO  SPA-GMN-CUR
                   END-IF
               WHEN   "1002"
      *            削除
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM  4032-DEL-SYORI-SEC
                   ELSE
                       MOVE     1              TO  SPA-GMN-CUR
                   END-IF
           END-EVALUATE
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
           INITIALIZE                  SPA-W30
      *
           MOVE    SPA-W01-STYUKYMDH   TO  SPA-GMN-STYUKYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  SPA-GMN-EDYUKYMD
      *
           MOVE    "0 発行しない"      TO  SPA-GMN-SIJISENKBN-LST(1)
           MOVE    "1 発行する"        TO  SPA-GMN-SIJISENKBN-LST(2)
           MOVE    2                   TO  SPA-SIJISEN-MAX
      *
           MOVE    ZERO                TO  FLG-INIT
      *
           MOVE    SPACE               TO  SYS-5007-REC
           INITIALIZE                      SYS-5007-REC
           MOVE    "5007"              TO  SYS-5007-KANRICD
           MOVE    SPA-HOSPNUM         TO  SYS-5007-HOSPNUM
           MOVE    SPA-W01-STYUKYMD    TO  SYS-5007-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-5007-EDYUKYMD
           MOVE    SYS-5007-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-5007-REC
               INITIALIZE                  SYS-5007-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           PERFORM
                   UNTIL       FLG-SYSKANRI    =   1
               MOVE    MCPDATA-REC         TO  SYS-5007-REC
               IF      FLG-INIT            =   ZERO
                   MOVE    SYS-5007-SIJISENKBN TO  SPA-GMN-SIJISENKBN
                   MOVE    1                   TO  FLG-INIT
               END-IF
      *
               EVALUATE    SYS-5007-KBNCD
                   WHEN    "01"
      *                入院処方箋
                       PERFORM VARYING IDX     FROM    1   BY  1
                               UNTIL   IDX     >   4
                           MOVE    SYS-5007-SIJISEN-HEAD(IDX)
                                           TO  SPA-GMN-SYOHOU-HEAD(IDX)
                           MOVE    SYS-5007-SIJISEN-PRT (IDX)
                                           TO  SPA-GMN-SYOHOU-PRT (IDX)
                       END-PERFORM
                   WHEN    "02"
      *                入院麻薬処方箋
                       PERFORM VARYING IDX     FROM    1   BY  1
                               UNTIL   IDX     >   4
                           MOVE    SYS-5007-SIJISEN-HEAD(IDX)
                                           TO  SPA-GMN-MAYAKU-HEAD(IDX)
                           MOVE    SYS-5007-SIJISEN-PRT (IDX)
                                           TO  SPA-GMN-MAYAKU-PRT (IDX)
                       END-PERFORM
                   WHEN    "03"
      *                注射処方箋
                       PERFORM VARYING IDX     FROM    1   BY  1
                               UNTIL   IDX     >   4
                           MOVE    SYS-5007-SIJISEN-HEAD(IDX)
                                           TO  SPA-GMN-CHUSYA-HEAD(IDX)
                           MOVE    SYS-5007-SIJISEN-PRT (IDX)
                                           TO  SPA-GMN-CHUSYA-PRT (IDX)
                       END-PERFORM
                   WHEN    "04"
      *                注射麻薬処方箋
                       PERFORM VARYING IDX     FROM    1   BY  1
                               UNTIL   IDX     >   4
                           MOVE    SYS-5007-SIJISEN-HEAD(IDX)
                                           TO  SPA-GMN-CHUMAYA-HEAD(IDX)
                           MOVE    SYS-5007-SIJISEN-PRT (IDX)
                                           TO  SPA-GMN-CHUMAYA-PRT (IDX)
                       END-PERFORM
                   WHEN    "05"
      *                指示箋
                       PERFORM VARYING IDX     FROM    1   BY  1
                               UNTIL   IDX     >   4
                           MOVE    SYS-5007-SIJISEN-HEAD(IDX)
                                           TO  SPA-GMN-SIJISEN-HEAD(IDX)
                           MOVE    SYS-5007-SIJISEN-PRT (IDX)
                                           TO  SPA-GMN-SIJISEN-PRT (IDX)
                       END-PERFORM
               END-EVALUATE
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 901-SYSKANRI-CLOSE-SEC
      *
           IF      SPA-GMN-SIJISENKBN  =   SPACE
               MOVE    ZERO            TO  SPA-GMN-SIJISENKBN
           END-IF
           EVALUATE   SPA-GMN-SIJISENKBN
               WHEN   "0"
                   MOVE    SPA-GMN-SIJISENKBN-LST(1)
                                           TO  SPA-GMN-SIJISENKBN-G
               WHEN   "1"
                   MOVE    SPA-GMN-SIJISENKBN-LST(2) 
                                           TO  SPA-GMN-SIJISENKBN-G
           END-EVALUATE
      *
           IF      FLG-INIT            =   ZERO
               PERFORM 3101-INIT-HEN-SEC
           END-IF
           MOVE    1                   TO  SPA-GMN-CUR
           . 
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面初期編集処理
      *****************************************************************
       3101-INIT-HEN-SEC         SECTION.
      *
           MOVE    "入院処方箋"            TO  SPA-GMN-SYOHOU-HEAD(1)
           MOVE    "入院処方箋（控）"      TO  SPA-GMN-SYOHOU-HEAD(2)
           MOVE    "入院処方箋（中止）"    TO  SPA-GMN-SYOHOU-HEAD(3)
           MOVE    SPACE                   TO  SPA-GMN-SYOHOU-HEAD(4)
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               MOVE    1                   TO  SPA-GMN-SYOHOU-PRT(IDX)
           END-PERFORM
      *
           MOVE    "入院麻薬処方箋"        TO  SPA-GMN-MAYAKU-HEAD(1)
           MOVE    "入院麻薬処方箋（控）"  TO  SPA-GMN-MAYAKU-HEAD(2)
           MOVE    "入院麻薬処方箋（中止）"
                                           TO  SPA-GMN-MAYAKU-HEAD(3)
           MOVE    SPACE                   TO  SPA-GMN-MAYAKU-HEAD(4)
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               MOVE    1                   TO  SPA-GMN-MAYAKU-PRT(IDX)
           END-PERFORM
      *
           MOVE    "注射処方箋"            TO  SPA-GMN-CHUSYA-HEAD(1)
           MOVE    "注射処方箋（控）"      TO  SPA-GMN-CHUSYA-HEAD(2)
           MOVE    "注射処方箋（中止）"    TO  SPA-GMN-CHUSYA-HEAD(3)
           MOVE    SPACE                   TO  SPA-GMN-CHUSYA-HEAD(4)
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               MOVE    1                   TO  SPA-GMN-CHUSYA-PRT(IDX)
           END-PERFORM
      *
           MOVE    "注射麻薬処方箋"        TO  SPA-GMN-CHUMAYA-HEAD(1)
           MOVE    "注射麻薬処方箋（控）"  TO  SPA-GMN-CHUMAYA-HEAD(2)
           MOVE    "注射麻薬処方箋（中止）"
                                           TO  SPA-GMN-CHUMAYA-HEAD(3)
           MOVE    SPACE                   TO  SPA-GMN-CHUMAYA-HEAD(4)
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               MOVE    1                   TO  SPA-GMN-CHUMAYA-PRT(IDX)
           END-PERFORM
      *
           MOVE    "指示箋"                TO  SPA-GMN-SIJISEN-HEAD(1)
           MOVE    "指示箋（控）"          TO  SPA-GMN-SIJISEN-HEAD(2)
           MOVE    "指示箋（中止）"        TO  SPA-GMN-SIJISEN-HEAD(3)
           MOVE    SPACE                   TO  SPA-GMN-SIJISEN-HEAD(4)
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               MOVE    1                   TO  SPA-GMN-SIJISEN-PRT(IDX)
           END-PERFORM
      *
           . 
       3101-INIT-HEN-EXT.
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
      *    削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM  403-DEL-SEC
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
       4101-GMN-SPA-SET-SEC        SECTION.
      *
           MOVE    W30-SIJISENKBN      TO  SPA-GMN-SIJISENKBN-G
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
               MOVE    W30-SYOHOUHEAD(IDX) TO  SPA-GMN-SYOHOU-HEAD (IDX)
               MOVE    W30-SYOHOUPRT (IDX) TO  SPA-GMN-SYOHOU-PRT (IDX)
      *
               MOVE    W30-MAYAKUHEAD(IDX) TO  SPA-GMN-MAYAKU-HEAD (IDX)
               MOVE    W30-MAYAKUPRT (IDX) TO  SPA-GMN-MAYAKU-PRT (IDX)
      *
               MOVE    W30-CHUSYAHEAD(IDX) TO  SPA-GMN-CHUSYA-HEAD (IDX)
               MOVE    W30-CHUSYAPRT (IDX) TO  SPA-GMN-CHUSYA-PRT (IDX)
      *
               MOVE    W30-CHUMAYAHEAD(IDX)
                                           TO  SPA-GMN-CHUMAYA-HEAD(IDX)
               MOVE    W30-CHUMAYAPRT(IDX) TO  SPA-GMN-CHUMAYA-PRT (IDX)
      *
               MOVE    W30-SIJISENHEAD(IDX)
                                           TO  SPA-GMN-SIJISEN-HEAD(IDX)
               MOVE    W30-SIJISENPRT(IDX) TO  SPA-GMN-SIJISEN-PRT (IDX)
           END-PERFORM
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
      *    入院指示せん区分
           PERFORM 4100-SIJISENKBN-CHK-SEC
      *
      *    入院処方箋
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4102-SYOHOU-CHK-SEC
           END-IF
      *    入院麻薬処方箋
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4103-MAYAKU-CHK-SEC
           END-IF
      *    注射処方箋
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4104-CHUSYA-CHK-SEC
           END-IF
      *    注射麻薬処方箋
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4104-CHUMAYA-CHK-SEC
           END-IF
      *    指示箋
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4105-SIJISEN-CHK-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院指示せん区分チェック
      *****************************************************************
       4100-SIJISENKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-SIJISEN-MAX )
                   OR         (FLG-OK  =   1   )
               IF      SPA-GMN-SIJISENKBN  =   SPA-GMN-SIJISENKBN-LST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-SIJISENKBN-LST (IDX)
                                           TO  SPA-GMN-SIJISENKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0001"              TO SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           .
       4100-SIJISENKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院処方箋チェック処理
      *****************************************************************
       4102-SYOHOU-CHK-SEC       SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   4   )
                   OR         (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-SYOHOU-HEAD(IDX)    NOT =   SPACE
                   MOVE    SPA-GMN-SYOHOU-HEAD(IDX)
                                               TO  WRK-HEAD-INPUT
                   PERFORM 41021-KANACHK-SEC
                   MOVE    WRK-HEAD-INPUT      TO  SPA-GMN-SYOHOU-HEAD
                                                                (IDX)
                   IF      FLG-ERR         =   1
                       MOVE    "0002"          TO  SPA-ERRCD
                       COMPUTE SPA-GMN-CUR     =   IDX   *  2
                   END-IF
               END-IF
      *
               IF      SPA-GMN-SYOHOU-PRT (IDX)    NOT =   ZERO
                   IF      SPA-GMN-SYOHOU-HEAD(IDX)    =   SPACE
                       MOVE    "0003"          TO  SPA-ERRCD
                       COMPUTE SPA-GMN-CUR     =   IDX   *  2
                                               +  1
                   END-IF
               END-IF
           END-PERFORM
      *
          .
       4102-SYOHOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角文字チェック処理
      *****************************************************************
       41021-KANACHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-HEAD-INPUT      TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               IF      KANACHK-RC2         =   7
                   MOVE    KANACHK-OUT-INPUT   TO  WRK-HEAD-INPUT
               END-IF
               MOVE    1                   TO  FLG-ERR
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  WRK-HEAD-INPUT
           END-IF
      *
          .
       41021-KANACHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院麻薬処方箋チェック処理
      *****************************************************************
       4103-MAYAKU-CHK-SEC       SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   4   )
                   OR         (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-MAYAKU-HEAD(IDX)    NOT =   SPACE
                   MOVE    SPA-GMN-MAYAKU-HEAD(IDX)
                                               TO  WRK-HEAD-INPUT
                   PERFORM 41021-KANACHK-SEC
                   MOVE    WRK-HEAD-INPUT      TO  SPA-GMN-MAYAKU-HEAD
                                                                (IDX)
                   IF      FLG-ERR         =   1
                       MOVE    "0004"          TO  SPA-ERRCD
                       COMPUTE SPA-GMN-CUR     =   IDX   *  2
                                               +   8
                   END-IF
               END-IF
      *
               IF      SPA-GMN-MAYAKU-PRT (IDX)    NOT =   ZERO
                   IF      SPA-GMN-MAYAKU-HEAD(IDX)    =   SPACE
                       MOVE    "0005"          TO  SPA-ERRCD
                       COMPUTE SPA-GMN-CUR     =   IDX   *  2
                                               +   9
                   END-IF
               END-IF
           END-PERFORM
      *
          .
       4103-MAYAKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    注射処方箋チェック処理
      *****************************************************************
       4104-CHUSYA-CHK-SEC       SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   4   )
                   OR         (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-CHUSYA-HEAD(IDX)    NOT =   SPACE
                   MOVE    SPA-GMN-CHUSYA-HEAD(IDX)
                                               TO  WRK-HEAD-INPUT
                   PERFORM 41021-KANACHK-SEC
                   MOVE    WRK-HEAD-INPUT      TO  SPA-GMN-CHUSYA-HEAD
                                                                (IDX)
                   IF      FLG-ERR         =   1
                       MOVE    "0006"          TO  SPA-ERRCD
                       COMPUTE SPA-GMN-CUR     =   IDX   *  2
                                               +   16
                   END-IF
               END-IF
      *
               IF      SPA-GMN-CHUSYA-PRT (IDX)    NOT =   ZERO
                   IF      SPA-GMN-CHUSYA-HEAD(IDX)    =   SPACE
                       MOVE    "0007"          TO  SPA-ERRCD
                       COMPUTE SPA-GMN-CUR     =   IDX   *  2
                                               +   17
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       4104-CHUSYA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    注射麻薬処方箋チェック処理
      *****************************************************************
       4104-CHUMAYA-CHK-SEC       SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   4   )
                   OR         (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-CHUMAYA-HEAD(IDX)   NOT =   SPACE
                   MOVE    SPA-GMN-CHUMAYA-HEAD(IDX)
                                               TO  WRK-HEAD-INPUT
                   PERFORM 41021-KANACHK-SEC
                   MOVE    WRK-HEAD-INPUT      TO  SPA-GMN-CHUMAYA-HEAD
                                                                (IDX)
                   IF      FLG-ERR         =   1
                       MOVE    "0008"          TO  SPA-ERRCD
                       COMPUTE SPA-GMN-CUR     =   IDX   *  2
                                               +   24
                   END-IF
               END-IF
      *
               IF      SPA-GMN-CHUMAYA-PRT (IDX)    NOT =   ZERO
                   IF      SPA-GMN-CHUMAYA-HEAD(IDX)    =   SPACE
                       MOVE    "0009"          TO  SPA-ERRCD
                       COMPUTE SPA-GMN-CUR     =   IDX   *  2
                                               +   25
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       4104-CHUMAYA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院麻薬処方箋チェック処理
      *****************************************************************
       4105-SIJISEN-CHK-SEC       SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   4   )
                   OR         (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-SIJISEN-HEAD(IDX)    NOT =   SPACE
                   MOVE    SPA-GMN-SIJISEN-HEAD(IDX)
                                               TO  WRK-HEAD-INPUT
                   PERFORM 41021-KANACHK-SEC
                   MOVE    WRK-HEAD-INPUT      TO  SPA-GMN-SIJISEN-HEAD
                                                                (IDX)
                   IF      FLG-ERR         =   1
                       MOVE    "0010"          TO  SPA-ERRCD
                       COMPUTE SPA-GMN-CUR     =   IDX   *  2
                                               +   32
                   END-IF
               END-IF
      *
               IF      SPA-GMN-SIJISEN-PRT (IDX)    NOT =   ZERO
                   IF      SPA-GMN-SIJISEN-HEAD(IDX)    =   SPACE
                       MOVE    "0011"          TO  SPA-ERRCD
                       COMPUTE SPA-GMN-CUR     =   IDX   *  2
                                               +   33
                   END-IF
               END-IF
           END-PERFORM
      *
          .
       4105-SIJISEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"           TO  SPA-SAKIPG
           MOVE    "W30"           TO  SPA-MOTOPG
      *
      *     INITIALIZE                  SPA-W01KYOUTU
      *
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                 SECTION.
      *
           MOVE    "1002"              TO  SPA-WIDCD
      *
           .    
       403-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理１００７
      *****************************************************************
       4032-DEL-SYORI-SEC          SECTION.
      *
      *
           MOVE    SPACE               TO  SYS-5007-REC
           INITIALIZE                      SYS-5007-REC
           MOVE    "5007"              TO  SYS-5007-KANRICD
           MOVE    SPA-HOSPNUM         TO  SYS-5007-HOSPNUM
           MOVE    SPA-W01-STYUKYMD    TO  SYS-5007-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-5007-EDYUKYMD
           MOVE    SYS-5007-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-5007-REC
               INITIALIZE                  SYS-5007-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           PERFORM
                   UNTIL       FLG-SYSKANRI    =   1
               MOVE    MCPDATA-REC         TO  SYS-5007-REC
      *
               MOVE    SPA-HOSPNUM         TO  SYS-5007-HOSPNUM
               MOVE    SYS-5007-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "1002"              TO  SPA-ERRCD
                   DISPLAY "DELETE ERR MCP-RC:"MCP-RC 
                               ",SYSKANRI-KEY =>" SYS-5007-KEY 
               END-IF
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 901-SYSKANRI-CLOSE-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
           .
       4032-DEL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC             SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
           IF      SPA-ERRCD        =   SPACE
               MOVE    "1001"           TO  SPA-WIDCD
           END-IF
      *
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      ******************************************************************
      *       登録処理
      *****************************************************************
       491-KOUSHIN-SYORI-SEC        SECTION.
      *
      *    更新日付取得
           CALL    "ORCSMCNDATE"       USING   ORCSMCNDATEAREA
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   5   )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               MOVE    SPACE               TO  SYS-5007-REC
               INITIALIZE                      SYS-5007-REC
               MOVE    "5007"              TO  SYS-5007-KANRICD
               EVALUATE    IDY
                   WHEN    1
                       MOVE    "01"        TO  SYS-5007-KBNCD
                   WHEN    2
                       MOVE    "02"        TO  SYS-5007-KBNCD
                   WHEN    3
                       MOVE    "03"        TO  SYS-5007-KBNCD
                   WHEN    4
                       MOVE    "04"        TO  SYS-5007-KBNCD
                   WHEN    5
                       MOVE    "05"        TO  SYS-5007-KBNCD
               END-EVALUATE
               MOVE    SPA-HOSPNUM         TO  SYS-5007-HOSPNUM
               MOVE    SPA-W01-STYUKYMD    TO  SYS-5007-STYUKYMD
               MOVE    SPA-W01-EDYUKYMD    TO  SYS-5007-EDYUKYMD
               MOVE    SYS-5007-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key10"             TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               ELSE
                   MOVE    SPACE               TO  SYS-5007-REC
                   INITIALIZE                  SYS-5007-REC
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 901-SYSKANRI-CLOSE-SEC
      *
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-5007-REC
               ELSE
      *            新規初期編集
                   PERFORM 4910-SYS5007-INIT-SEC
               END-IF
      *        共通編集
               PERFORM 4911-DB-HENSYU-SEC
      *
               MOVE    SYS-5007-REC            TO  SYSKANRI-REC
               IF      FLG-SYSKANRI            =   ZERO
                   MOVE    SMCNDATE-YMD            TO  SYS-UPYMD
                   MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
      *
                   MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
                   MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
                   MOVE    SPA-W01-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
                   MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
                   MOVE    SPA-HOSPNUM             TO  SYSUP-HOSPNUM
                   MOVE    SYSKANRI-REC            TO  MCPDATA-REC
                   MOVE    "DBUPDATE"              TO  MCP-FUNC
                   MOVE    "tbl_syskanri"          TO  MCP-TABLE
                   MOVE    "upd1"                  TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                  IF      MCP-RC              NOT =   ZERO 
                       MOVE    "1001"                  TO  SPA-ERRCD
                       DISPLAY "SYS-5007 UPD ERR =>" SYS-KEY
                               ",MCP-RC:" MCP-RC 
                   END-IF
               ELSE
                   MOVE    SMCNDATE-YMD            TO  SYS-CREYMD
                   MOVE    SMCNDATE-YMD            TO  SYS-UPYMD
                   MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
                   MOVE    SPA-HOSPNUM             TO  SYS-HOSPNUM
      *
                   MOVE    SYSKANRI-REC            TO  MCPDATA-REC
                   MOVE    "DBINSERT"              TO  MCP-FUNC
                   MOVE    "tbl_syskanri"          TO  MCP-TABLE
                   MOVE    "key"                   TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC              NOT =   ZERO 
                       MOVE    "1001"                  TO  SPA-ERRCD
                       DISPLAY "SYS-5007 INS ERR =>"  SYS-KEY
                               ",MCP-RC:" MCP-RC 
                   END-IF
               END-IF
            END-PERFORM
      *
            MOVE   "JOIN"               TO  MCP-PUTTYPE
            PERFORM 210-BACK 
           .
       491-KOUSHIN-SYORI-EXT.
           EXIT.
      *
      ******************************************************************
      *       新規初期編集処理
      *****************************************************************
       4910-SYS5007-INIT-SEC        SECTION.
      *
           MOVE    SPACE               TO  SYS-5007-REC
           INITIALIZE                      SYS-5007-REC
           MOVE    "5007"              TO  SYS-5007-KANRICD
           EVALUATE    IDY
               WHEN    1
                   MOVE    "01"        TO  SYS-5007-KBNCD
               WHEN    2
                   MOVE    "02"        TO  SYS-5007-KBNCD
               WHEN    3
                   MOVE    "03"        TO  SYS-5007-KBNCD
               WHEN    4
                   MOVE    "04"        TO  SYS-5007-KBNCD
               WHEN    5
                   MOVE    "05"        TO  SYS-5007-KBNCD
           END-EVALUATE
           .
       4910-SYS5007-INIT-EXT.
           EXIT.
      *
      ******************************************************************
      *      マスタ編集処理
      *****************************************************************
       4911-DB-HENSYU-SEC        SECTION.
      *
           MOVE    SPA-W01-STYUKYMD    TO  SYS-5007-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-5007-EDYUKYMD
           MOVE    SPA-GMN-SIJISENKBN  TO  SYS-5007-SIJISENKBN
      *
           EVALUATE    SYS-5007-KBNCD
      *        入院処方箋
               WHEN    "01"
                   PERFORM VARYING IDX     FROM    1   BY  1
                           UNTIL   IDX     >   4
                       MOVE    SPA-GMN-SYOHOU-HEAD(IDX)
                                       TO  SYS-5007-SIJISEN-HEAD(IDX)
                       MOVE    SPA-GMN-SYOHOU-PRT (IDX)
                                       TO  SYS-5007-SIJISEN-PRT (IDX)
                   END-PERFORM
      *        入院麻薬処方箋
               WHEN    "02"
                   PERFORM VARYING IDX     FROM    1   BY  1
                           UNTIL   IDX     >   4
                       MOVE    SPA-GMN-MAYAKU-HEAD(IDX)
                                       TO  SYS-5007-SIJISEN-HEAD(IDX)
                       MOVE    SPA-GMN-MAYAKU-PRT (IDX)
                                       TO  SYS-5007-SIJISEN-PRT (IDX)
                   END-PERFORM
      *        注射箋
               WHEN    "03"
                   PERFORM VARYING IDX     FROM    1   BY  1
                           UNTIL   IDX     >   4
                       MOVE    SPA-GMN-CHUSYA-HEAD(IDX)
                                       TO  SYS-5007-SIJISEN-HEAD(IDX)
                       MOVE    SPA-GMN-CHUSYA-PRT (IDX)
                                       TO  SYS-5007-SIJISEN-PRT (IDX)
                   END-PERFORM
      *        注射麻薬箋
               WHEN    "04"
                   PERFORM VARYING IDX     FROM    1   BY  1
                           UNTIL   IDX     >   4
                       MOVE    SPA-GMN-CHUMAYA-HEAD(IDX)
                                       TO  SYS-5007-SIJISEN-HEAD(IDX)
                       MOVE    SPA-GMN-CHUMAYA-PRT (IDX)
                                       TO  SYS-5007-SIJISEN-PRT (IDX)
                   END-PERFORM
      *        指示箋
               WHEN    "05"
                   PERFORM VARYING IDX     FROM    1   BY  1
                           UNTIL   IDX     >   4
                       MOVE    SPA-GMN-SIJISEN-HEAD(IDX)
                                       TO  SYS-5007-SIJISEN-HEAD(IDX)
                       MOVE    SPA-GMN-SIJISEN-PRT (IDX)
                                       TO  SYS-5007-SIJISEN-PRT (IDX)
                   END-PERFORM
           END-EVALUATE
      *
           .
       4911-DB-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC           SECTION.
      *
      *    発行ありの時、印刷部数があること
           IF      SPA-GMN-SIJISENKBN  =   "1"
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   4   )   OR
                                  (FLG-OK  =   1   )
                   IF      (SPA-GMN-SYOHOU-HEAD(IDX)   NOT =   SPACE)
                       AND (SPA-GMN-SYOHOU-PRT (IDX)   >   ZERO     )
                       MOVE    1                   TO  FLG-OK
                   END-IF
               END-PERFORM 
               IF      FLG-OK               =   ZERO
                   MOVE    "0012"               TO  SPA-ERRCD
                   MOVE    1                    TO  SPA-GMN-CUR
               END-IF
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
           PERFORM 510-GSRAUTH-SEC
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
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W30    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  W30
      *
           INITIALIZE                      W30
           MOVE    SPA-GMN-STYUKYMD    TO  W30-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD    TO  W30-EDYUKYMD
      *
           MOVE    SPA-GMN-SIJISENKBN-G    TO  W30-SIJISENKBN
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-SIJISEN-MAX
               MOVE    SPA-GMN-SIJISENKBN-LST(IDX) 
                                           TO  W30-SIJISENKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-SIJISEN-MAX     TO  W30-SIJISENKBN-COUNT
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       4
               MOVE    SPA-GMN-SYOHOU-HEAD(IDX)
                                       TO  W30-SYOHOUHEAD (IDX)
               MOVE    SPA-GMN-SYOHOU-PRT (IDX)
                                       TO  W30-SYOHOUPRT  (IDX)
      *
               MOVE    SPA-GMN-MAYAKU-HEAD(IDX)
                                       TO  W30-MAYAKUHEAD (IDX)
               MOVE    SPA-GMN-MAYAKU-PRT (IDX)
                                       TO  W30-MAYAKUPRT  (IDX)
      *
               MOVE    SPA-GMN-CHUSYA-HEAD(IDX)
                                       TO  W30-CHUSYAHEAD (IDX)
               MOVE    SPA-GMN-CHUSYA-PRT (IDX)
                                       TO  W30-CHUSYAPRT  (IDX)
      *
               MOVE    SPA-GMN-CHUMAYA-HEAD(IDX)
                                       TO  W30-CHUMAYAHEAD(IDX)
               MOVE    SPA-GMN-CHUMAYA-PRT (IDX)
                                       TO  W30-CHUMAYAPRT (IDX)
      *
               MOVE    SPA-GMN-SIJISEN-HEAD(IDX)
                                       TO  W30-SIJISENHEAD (IDX)
               MOVE    SPA-GMN-SIJISEN-PRT (IDX)
                                       TO  W30-SIJISENPRT  (IDX)
           END-PERFORM
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務権限非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           MOVE    WIDGET-INSENSITIVE  TO  W30-B03-STATE
      *
           IF      SPA-W01-SELNUM          >   ZERO
               MOVE    WIDGET-NORMAL   TO  W30-B03-STATE
           END-IF 
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入院指示せん区分エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "入院処方の見出しは全角で入力して下さい。"
                                                TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "入院処方の見出しがありません。"
                                                TO  SPA-ERRMSG
                   MOVE    "印刷部数は入力できません。"
                                                TO  SPA-ERRMSG(31:)
               WHEN    "0004"
                   MOVE   "入院麻薬処方の見出しは全角で入力して下さい。"
                                                TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "入院麻薬処方の見出しがありません。"
                                                TO  SPA-ERRMSG
                   MOVE    "印刷部数は入力できません。"
                                                TO  SPA-ERRMSG(35:)
               WHEN    "0006"
                   MOVE    "注射処方の見出しは全角で入力して下さい。"
                                                TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "注射処方の見出しがありません。"
                                                TO  SPA-ERRMSG
                   MOVE    "印刷部数は入力できません。"
                                                TO  SPA-ERRMSG(31:)
               WHEN    "0008"
                   MOVE
                        "注射麻薬処方の見出しは全角で入力して下さい。"
                                                TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "注射麻薬処方の見出しがありません。"
                                                TO  SPA-ERRMSG
                   MOVE    "印刷部数は入力できません。"
                                                TO  SPA-ERRMSG(35:)
               WHEN    "0010"
                   MOVE    "指示箋の見出しは全角で入力して下さい。"
                                                TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "指示箋の見出しがありません。"
                                                TO  SPA-ERRMSG
                   MOVE    "印刷部数は入力できません。"
                                                TO  SPA-ERRMSG(29:)
               WHEN    "0012"
                   MOVE    "入院指示せんの内容設定がありません。"
                                                TO  SPA-ERRMSG
      *
               WHEN    "1001"
                   MOVE    "更新ができませんでした"    TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除ができませんでした"    TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  WERR
           MOVE    SPA-ERRCD       TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  WERR-ERRMSG
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "W30"           TO  SPA-MOTOPG
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
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "入院指示せん設定情報を登録します。" 
                                                   TO  WRK-WIDMSG
               WHEN    "1002"
                   MOVE    "入院指示せん設定情報を削除します。" 
                                                   TO  WRK-WIDMSG
               WHEN    OTHER
                   MOVE    SPA-WIDCD               TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WID1
           INITIALIZE                      WID1
           MOVE    SPA-WIDCD           TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG          TO  WID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "W30"               TO  SPA-MOTOPG
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
           IF      SPA-GMN-CUR     =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE  "SIJISENKBN"      TO   MCP-WIDGET
               WHEN    2
                   MOVE  "SYOHOUHEAD1"     TO   MCP-WIDGET
               WHEN    3
                   MOVE  "SYOHOUPRT1"      TO   MCP-WIDGET
               WHEN    4
                   MOVE  "SYOHOUHEAD2"     TO   MCP-WIDGET
               WHEN    5
                   MOVE  "SYOHOUPRT2"      TO   MCP-WIDGET
               WHEN    6
                   MOVE  "SYOHOUHEAD3"     TO   MCP-WIDGET
               WHEN    7
                   MOVE  "SYOHOUPRT3"      TO   MCP-WIDGET
               WHEN    8
                   MOVE  "SYOHOUHEAD4"     TO   MCP-WIDGET
               WHEN    9
                   MOVE  "SYOHOUPRT4"      TO   MCP-WIDGET
      *
               WHEN    10
                   MOVE  "MAYAKUHEAD1"     TO   MCP-WIDGET
               WHEN    11
                   MOVE  "MAYAKUPRT1"      TO   MCP-WIDGET
               WHEN    12
                   MOVE  "MAYAKUHEAD2"     TO   MCP-WIDGET
               WHEN    13
                   MOVE  "MAYAKUPRT2"      TO   MCP-WIDGET
               WHEN    14
                   MOVE  "MAYAKUHEAD3"     TO   MCP-WIDGET
               WHEN    15
                   MOVE  "MAYAKUPRT3"      TO   MCP-WIDGET
               WHEN    16
                   MOVE  "MAYAKUHEAD4"     TO   MCP-WIDGET
               WHEN    17
                   MOVE  "MAYAKUPRT4"      TO   MCP-WIDGET
      *
               WHEN    18
                   MOVE  "CHUSYAHEAD1"     TO   MCP-WIDGET
               WHEN    19
                   MOVE  "CHUSYAPRT1"      TO   MCP-WIDGET
               WHEN    20
                   MOVE  "CHUSYAHEAD2"     TO   MCP-WIDGET
               WHEN    21
                   MOVE  "CHUSYAPRT2"      TO   MCP-WIDGET
               WHEN    22
                   MOVE  "CHUSYAHEAD3"     TO   MCP-WIDGET
               WHEN    23
                   MOVE  "CHUSYAPRT3"      TO   MCP-WIDGET
               WHEN    24
                   MOVE  "CHUSYAHEAD4"     TO   MCP-WIDGET
               WHEN    25
                   MOVE  "CHUSYAPRT4"      TO   MCP-WIDGET
      *
               WHEN    26
                   MOVE  "CHUMAYAHEAD1"    TO   MCP-WIDGET
               WHEN    27
                   MOVE  "CHUMAYAPRT1"     TO   MCP-WIDGET
               WHEN    28
                   MOVE  "CHUMAYAHEAD2"    TO   MCP-WIDGET
               WHEN    29
                   MOVE  "CHUMAYAPRT2"     TO   MCP-WIDGET
               WHEN    30
                   MOVE  "CHUMAYAHEAD3"    TO   MCP-WIDGET
               WHEN    31
                   MOVE  "CHUMAYAPRT3"     TO   MCP-WIDGET
               WHEN    32
                   MOVE  "CHUMAYAHEAD4"    TO   MCP-WIDGET
               WHEN    33
                   MOVE  "CHUMAYAPRT4"     TO   MCP-WIDGET
      *
               WHEN    34
                   MOVE  "SIJISENHEAD1"    TO   MCP-WIDGET
               WHEN    35
                   MOVE  "SIJISENPRT1"     TO   MCP-WIDGET
               WHEN    36
                   MOVE  "SIJISENHEAD2"    TO   MCP-WIDGET
               WHEN    37
                   MOVE  "SIJISENPRT2"     TO   MCP-WIDGET
               WHEN    38
                   MOVE  "SIJISENHEAD3"    TO   MCP-WIDGET
               WHEN    39
                   MOVE  "SIJISENPRT3"     TO   MCP-WIDGET
               WHEN    40
                   MOVE  "SIJISENHEAD4"    TO   MCP-WIDGET
               WHEN    41
                   MOVE  "SIJISENPRT4"     TO   MCP-WIDGET
      *
               WHEN    90
                   MOVE  "B12"             TO   MCP-WIDGET
           END-EVALUATE
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC           SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SIJISENKBN"
                   IF     SPA-GMN-SIJISENKBN   =   "0"
                       MOVE    90              TO  SPA-GMN-CUR
                   ELSE
                       MOVE    2               TO  SPA-GMN-CUR
                   END-IF
               WHEN    "SYOHOUHEAD1"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "SYOHOUPRT1"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "SYOHOUHEAD2"
                   MOVE    5               TO  SPA-GMN-CUR
               WHEN    "SYOHOUPRT2"
                   MOVE    6               TO  SPA-GMN-CUR
               WHEN    "SYOHOUHEAD3"
                   MOVE    7               TO  SPA-GMN-CUR
               WHEN    "SYOHOUPRT3"
                   MOVE    8               TO  SPA-GMN-CUR
               WHEN    "SYOHOUHEAD4"
                   MOVE    9               TO  SPA-GMN-CUR
               WHEN    "SYOHOUPRT4"
                   MOVE    10              TO  SPA-GMN-CUR
      *
               WHEN    "MAYAKUHEAD1"
                   MOVE    11              TO  SPA-GMN-CUR
               WHEN    "MAYAKUPRT1"
                   MOVE    12              TO  SPA-GMN-CUR
               WHEN    "MAYAKUHEAD2"
                   MOVE    13              TO  SPA-GMN-CUR
               WHEN    "MAYAKUPRT2"
                   MOVE    14              TO  SPA-GMN-CUR
               WHEN    "MAYAKUHEAD3"
                   MOVE    15              TO  SPA-GMN-CUR
               WHEN    "MAYAKUPRT3"
                   MOVE    16              TO  SPA-GMN-CUR
               WHEN    "MAYAKUHEAD4"
                   MOVE    17              TO  SPA-GMN-CUR
               WHEN    "MAYAKUPRT4"
                   MOVE    18              TO  SPA-GMN-CUR
      *
               WHEN    "CHUSYAHEAD1"
                   MOVE    19              TO  SPA-GMN-CUR
               WHEN    "CHUSYAPRT1"
                   MOVE    20              TO  SPA-GMN-CUR
               WHEN    "CHUSYAHEAD2"
                   MOVE    21              TO  SPA-GMN-CUR
               WHEN    "CHUSYAPRT2"
                   MOVE    22              TO  SPA-GMN-CUR
               WHEN    "CHUSYAHEAD3"
                   MOVE    23              TO  SPA-GMN-CUR
               WHEN    "CHUSYAPRT3"
                   MOVE    24              TO  SPA-GMN-CUR
               WHEN    "CHUSYAHEAD4"
                   MOVE    25              TO  SPA-GMN-CUR
               WHEN    "CHUSYAPRT4"
                   MOVE    26              TO  SPA-GMN-CUR
      *
               WHEN    "CHUMAYAHEAD1"
                   MOVE    27              TO  SPA-GMN-CUR
               WHEN    "CHUMAYAPRT1"
                   MOVE    28              TO  SPA-GMN-CUR
               WHEN    "CHUMAYAHEAD2"
                   MOVE    29              TO  SPA-GMN-CUR
               WHEN    "CHUMAYAPRT2"
                   MOVE    30              TO  SPA-GMN-CUR
               WHEN    "CHUMAYAHEAD3"
                   MOVE    31              TO  SPA-GMN-CUR
               WHEN    "CHUMAYAPRT3"
                   MOVE    32              TO  SPA-GMN-CUR
               WHEN    "CHUMAYAHEAD4"
                   MOVE    33              TO  SPA-GMN-CUR
               WHEN    "CHUMAYAPRT4"
                   MOVE    34              TO  SPA-GMN-CUR
      *
               WHEN    "SIJISENHEAD1"
                   MOVE    35              TO  SPA-GMN-CUR
               WHEN    "SIJISENPRT1"
                   MOVE    36              TO  SPA-GMN-CUR
               WHEN    "SIJISENHEAD2"
                   MOVE    37              TO  SPA-GMN-CUR
               WHEN    "SIJISENPRT2"
                   MOVE    38              TO  SPA-GMN-CUR
               WHEN    "SIJISENHEAD3"
                   MOVE    39              TO  SPA-GMN-CUR
               WHEN    "SIJISENPRT3"
                   MOVE    40              TO  SPA-GMN-CUR
               WHEN    "SIJISENHEAD4"
                   MOVE    41              TO  SPA-GMN-CUR
               WHEN    "SIJISENPRT4"
                   MOVE    90              TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込処理
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
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
      *****************************************************************
      *    管理マスターＣＬＯＳＥ処理
      *****************************************************************
       901-SYSKANRI-CLOSE-SEC          SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       901-SYSKANRI-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
