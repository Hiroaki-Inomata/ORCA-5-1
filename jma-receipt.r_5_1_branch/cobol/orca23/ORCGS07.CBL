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
       PROGRAM-ID.                 ORCGS07.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 収納業務
      *  コンポーネント名  : 月別請求書兼領収書印刷指示画面
      *  管理者            : 
      *  作成日付    作業者        記述
      *  10.05.11    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    収納共通パラメタ
           COPY    "S01COMMON-SPA".
      *
           COPY    ENUM-VALUE.
      *
      *    スパ領域
      *
      *    スパ領域
       01  SPA-S07.
           03  SPA-S07-AREA.
               05  SPA-GMN-CUR                     PIC 9(03).
               05  SPA-GMN-AREA.
                   07  SPA-GMN-SRYYM               PIC X(10).
                   07  SPA-GMN-KIKANKBN-G.
                       09  SPA-GMN-KIKANKBN        PIC X(01).
                       09  SPA-GMN-KIKANKBNFL      PIC X(01).
                       09  SPA-GMN-KIKANKBNMEI     PIC X(18).
                   07  SPA-GMN-STDAY               PIC 9(02).
                   07  SPA-GMN-EDDAY               PIC 9(02).
                   07  SPA-GMN-TUKIGRPKBN-G.
                       09  SPA-GMN-TUKIGRPKBN       PIC X(01).
                       09  SPA-GMN-TUKIGRPKBNFL     PIC X(01).
                       09  SPA-GMN-TUKIGRPKBNMEI    PIC X(28).
               05  SPA-GMN-COMBO-AREA.
                   07  SPA-GMN-KIKANKBNLST-G.
                       09  SPA-GMN-KIKANKBN-MAX    PIC 9(03).
                       09  SPA-GMN-KIKANKBNLST     OCCURS  5.
                           11  SPA-GMN-KIKANKBNL
                                                   PIC X(01).
                           11  SPA-GMN-KIKANKBNFLL
                                                   PIC X(01).
                           11  SPA-GMN-KIKANKBNMEIL
                                                   PIC X(18).
                   07  SPA-GMN-TUKIGRPKBNLST-G.
                       09  SPA-GMN-TUKIGRPKBN-MAX   PIC 9(03).
                       09  SPA-GMN-TUKIGRPKBNLST    OCCURS  5.
                           11  SPA-GMN-TUKIGRPKBNL
                                                   PIC X(01).
                           11  SPA-GMN-TUKIGRPKBNFLL
                                                   PIC X(01).
                           11  SPA-GMN-TUKIGRPKBNMEIL
                                                   PIC X(28).
               05  SPA-NAI-AREA.
                   07  SPA-NAI-SRYYM               PIC X(06).
                   07  SPA-NAI-TUKISUMKBN          PIC X(01).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-SYUNOU                  PIC 9(01).
           03  FLG-HIT                     PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SPACE                   PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-CMB-CD                  PIC X(05).
           03  WRK-CMB-G.
               05  WRK-CMB-ITM             PIC X(100).
           03  WRK-KANACHK-STR             PIC X(100).
           03  WRK-SRYYMD.
               05  WRK-SRYYM               PIC X(06).
               05  WRK-SRYDD               PIC 9(02).
           03  WRK-YMD                     PIC X(10). 
           03  WRK-SYMD                    PIC X(08). 
           03  WRK-HENYMDG                 PIC X(09).
           03  WRK-STYMD.
               05  WRK-STYM                PIC X(06).
               05  WRK-STDD                PIC 9(02).
           03  WRK-EDYMD.
               05  WRK-EDYM                PIC X(06).
               05  WRK-EDDD                PIC 9(02).
           03  WRK-DENPNUM                 PIC 9(07).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1039.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA23SCRAREA.INC".
      *
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE2       TO  SPA-S07
           MOVE    SPA-WORK        TO  SPA-S01KYOUTU
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
           IF  FLG-END   =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-S01KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA         TO  SPA-COMMON.
           MOVE    SPA-S07          TO  SPA-FREE2
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
           IF    ( SPA-MOTOPG          =   "SERR" )
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
           MOVE    "S07"               TO  MCP-WINDOW
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
           MOVE    SPACE           TO  SPA-S07
           INITIALIZE                  SPA-S07
      *
           INITIALIZE                  SYS-1039-REC
           MOVE    "1039"          TO  SYS-1039-KANRICD
           MOVE    "*"             TO  SYS-1039-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1039-STYUKYMD
                                       SYS-1039-EDYUKYMD
           MOVE    SYS-1039-REC    TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1039-REC
      *
           MOVE    SYS-1039-TUKISUMKBN
                                   TO  SPA-NAI-TUKISUMKBN
      *
           IF    ( SPA-S01-TUKI-SRYYM
                                   =   SPACE )
               MOVE    SPA-SYSYMD  TO  WRK-YMD
               PERFORM 800-NENGETU-CHK-SEC
               MOVE    WRK-SYMD(1:6)       TO  SPA-NAI-SRYYM
               MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM
           ELSE
               MOVE    SPA-S01-TUKI-SRYYM
                                   TO  WRK-YMD
               PERFORM 800-NENGETU-CHK-SEC
               MOVE    WRK-SYMD(1:6)       TO  SPA-NAI-SRYYM
               MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM
           END-IF
      *
      *    コンボボックス編集処理
           PERFORM 310-COMBO-EDIT-SEC
      *
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-KIKANKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-KIKANKBN-G
      *
           MOVE    SYS-1039-TUKIGRPKBN
                                   TO  WRK-CMB-CD
           PERFORM 4200-TUKIGRPKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-TUKIGRPKBN-G
           ELSE
               MOVE    "4"             TO  WRK-CMB-CD
               PERFORM 4200-TUKIGRPKBNLST-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-TUKIGRPKBN-G
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コンボボックス編集処理
      *****************************************************************
       310-COMBO-EDIT-SEC              SECTION.
      *
      *    期間区分
           PERFORM 3101-KIKANKBNLST-SEC
      *
      *    発行方法
           PERFORM 3101-TUKIGRPKBNLST-SEC
      *
           .
       310-COMBO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    期間区分コンボ初期処理
      *****************************************************************
       3101-KIKANKBNLST-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-KIKANKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-KIKANKBNL   (1)
           MOVE    "しない"        TO  SPA-GMN-KIKANKBNMEIL(1)
           MOVE    "1"             TO  SPA-GMN-KIKANKBNL   (2)
           MOVE    "する"          TO  SPA-GMN-KIKANKBNMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-KIKANKBN-MAX
      *
           .
       3101-KIKANKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    発行方法コンボ初期処理
      *****************************************************************
       3101-TUKIGRPKBNLST-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-TUKIGRPKBNLST-G
      *
           MOVE    "1"             TO  SPA-GMN-TUKIGRPKBNL    (1)
           MOVE    "診療科・保険組合せ別に発行"
                                   TO  SPA-GMN-TUKIGRPKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-TUKIGRPKBNL    (2)
           MOVE    "保険組合せ別に発行"
                                   TO  SPA-GMN-TUKIGRPKBNMEIL (2)
      *
           MOVE    "3"             TO  SPA-GMN-TUKIGRPKBNL    (3)
           MOVE    "診療科別に発行"
                                   TO  SPA-GMN-TUKIGRPKBNMEIL (3)
      *
           MOVE    "4"             TO  SPA-GMN-TUKIGRPKBNL    (4)
           MOVE    "全体をまとめて発行"
                                   TO  SPA-GMN-TUKIGRPKBNMEIL (4)
      *
           MOVE    4               TO  SPA-GMN-TUKIGRPKBN-MAX
      *
           .
       3101-TUKIGRPKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
           WHEN    "CLICKED"       ALSO    "B01"
               INITIALIZE              SPA-S01-TUKI-G
               PERFORM 210-BACK
           WHEN    "CLICKED"       ALSO    "B12S"
           WHEN    "CLICKED"       ALSO    "B10"
           WHEN    "CLICKED"       ALSO    "B11"
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 220-KAKUTEI-SEC
      *
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
           MOVE    S07-SRYYM       TO  SPA-GMN-SRYYM
           MOVE    S07-KIKANKBN    TO  SPA-GMN-KIKANKBN-G
           MOVE    S07-STDAY       TO  SPA-GMN-STDAY
           MOVE    S07-EDDAY       TO  SPA-GMN-EDDAY
           MOVE    S07-TUKIGRPKBN  TO  SPA-GMN-TUKIGRPKBN-G
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
           PERFORM 4101-SRYYM-CHK-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-KIKANKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-STDAY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-EDDAY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-TUKIGRPKBN-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療年月チェック処理
      *****************************************************************
       4101-SRYYM-CHK-SEC              SECTION.
      *
           MOVE    SPA-NAI-SRYYM       TO  WRK-SRYYM
           MOVE    SPACE               TO  SPA-NAI-SRYYM
      *
           MOVE    SPA-GMN-SRYYM       TO  WRK-YMD
           PERFORM 800-NENGETU-CHK-SEC
           IF    ( WRK-SYMD            =   SPACE )
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
               MOVE    WRK-SRYYM       TO  WRK-YMD
               PERFORM 800-NENGETU-CHK-SEC
               MOVE    WRK-SYMD(1:6)       TO  SPA-NAI-SRYYM
               MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM
           ELSE
               MOVE    WRK-SYMD(1:6)       TO  SPA-NAI-SRYYM
               MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM
           END-IF
      *
           .
       4101-SRYYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間区分チェック処理
      *****************************************************************
       4101-KIKANKBN-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-KIKANKBN    TO  WRK-CMB-CD
           PERFORM 4200-KIKANKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-KIKANKBNLST (1)
                                       TO  SPA-GMN-KIKANKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-KIKANKBN-G
           END-IF
      *
           IF    ( SPA-GMN-KIKANKBN    =   "0" )
               MOVE    ZERO            TO  SPA-GMN-STDAY
               MOVE    99              TO  SPA-GMN-EDDAY
           END-IF
      *
           .
       4101-KIKANKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始日チェック処理
      *****************************************************************
       4101-STDAY-CHK-SEC               SECTION.
      *
           MOVE    SPA-NAI-SRYYM    TO  WRK-YMD
           PERFORM 800-EOM-SEC
           MOVE    WRK-YMD          TO  WRK-SRYYMD
      *
           IF    ( SPA-GMN-STDAY        <    1 )
            OR   ( SPA-GMN-STDAY        >    WRK-SRYDD )
               MOVE    1            TO   SPA-GMN-STDAY
           END-IF
      *
           .
       4101-STDAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了日チェック処理
      *****************************************************************
       4101-EDDAY-CHK-SEC               SECTION.
      *
           MOVE    SPA-NAI-SRYYM    TO  WRK-YMD
           PERFORM 800-EOM-SEC
           MOVE    WRK-YMD          TO  WRK-SRYYMD
      *
           IF    ( SPA-GMN-EDDAY        <    1 )
            OR   ( SPA-GMN-EDDAY        >    WRK-SRYDD )
               MOVE    WRK-SRYDD    TO   SPA-GMN-EDDAY
           END-IF
      *
           .
       4101-EDDAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    発行方法区分チェック処理
      *****************************************************************
       4101-TUKIGRPKBN-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-TUKIGRPKBN  TO  WRK-CMB-CD
           PERFORM 4200-TUKIGRPKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-TUKIGRPKBNLST (4)
                                       TO  SPA-GMN-TUKIGRPKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-TUKIGRPKBN-G
           END-IF
      *
           .
       4101-TUKIGRPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間区分コンボチェック処理
      *****************************************************************
       4200-KIKANKBNLST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-KIKANKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KIKANKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-KIKANKBNLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-KIKANKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    発行方法区分コンボチェック処理
      *****************************************************************
       4200-TUKIGRPKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                   WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB      FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-TUKIGRPKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-TUKIGRPKBNL (IDX-CMB)
                                        =   WRK-CMB-CD )
                   MOVE    SPA-GMN-TUKIGRPKBNLST  (IDX-CMB)
                                        TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-TUKIGRPKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       411-KANREN-CHK-SEC              SECTION.
      *
           IF    ( SPA-GMN-KIKANKBN    =   "1" )
               IF    ( SPA-GMN-STDAY   >   SPA-GMN-EDDAY )
                   MOVE    "1001"      TO  SPA-ERRCD
                   MOVE    3           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-HIT
      *
           PERFORM 900-SYUNOU-KEY73-SEL-SEC
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( FLG-SYUNOU  NOT =   ZERO )
                    OR   ( FLG-HIT         =   1 )
      *
               MOVE    1           TO  FLG-HIT
               MOVE    SYU-DENPNUM TO  WRK-DENPNUM
               MOVE    SYU-SRYYMD  TO  WRK-SRYYMD
      *
               IF    ( SPA-GMN-KIKANKBN    =   "1" )
                   MOVE    SPA-NAI-SRYYM   TO  WRK-STYM
                                               WRK-EDYM
                   MOVE    SPA-GMN-STDAY   TO  WRK-STDD
                   MOVE    SPA-GMN-EDDAY   TO  WRK-EDDD
                   IF    ( SYU-SRYYMD      >=  WRK-STYMD )
                    AND  ( SYU-SRYYMD      <=  WRK-EDYMD )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-HIT
                   END-IF
               END-IF
      *
               IF    ( SPA-NAI-TUKISUMKBN  =   "1" )
                   IF    ( SYU-SKYMONEY    >   SYU-NYUKIN-TOTAL )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-HIT
                   END-IF
               END-IF
      *
               PERFORM 900-SYUNOU-KEY73-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key73"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-HIT =   ZERO )
               MOVE    "1002"          TO  SPA-ERRCD
           END-IF
      *
           .
      *
       411-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "S02"               TO  SPA-SAKIPG
           MOVE    "S07"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    確定処理
      *****************************************************************
       220-KAKUTEI-SEC                 SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 411-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *
               INITIALIZE              SPA-S01-TUKI-G
      *
               MOVE    WRK-DENPNUM     TO  SPA-S01-TUKI-DENPNUM
      *
               MOVE    SPA-NAI-SRYYM   TO  SPA-S01-TUKI-SRYYM
               MOVE    SPA-GMN-KIKANKBN
                                       TO  SPA-S01-TUKI-KIKANKBN
               IF    ( SPA-GMN-KIKANKBN    =   "1" )
                   MOVE    SPA-NAI-SRYYM   TO  WRK-STYM
                                               WRK-EDYM
                   MOVE    SPA-GMN-STDAY   TO  WRK-STDD
                   MOVE    SPA-GMN-EDDAY   TO  WRK-EDDD
                   MOVE    WRK-STYMD       TO  SPA-S01-TUKI-STYMD
                   MOVE    WRK-EDYMD       TO  SPA-S01-TUKI-EDYMD
               END-IF
      *
               MOVE        SPA-GMN-TUKIGRPKBN
                                           TO  SPA-S01-TUKI-GRPKBN
      *
               EVALUATE    WRK-MCP-WIDGET
               WHEN    "B12S"
                   MOVE    "2"             TO  SPA-S01-TUKI-SKYPRT
                   MOVE    "2"             TO  SPA-S01-TUKI-SRYPRT
               WHEN    "B10"
                   MOVE    "1"             TO  SPA-S01-TUKI-SKYPRT
               WHEN    "B11"
                   MOVE    "1"             TO  SPA-S01-TUKI-SRYPRT
               WHEN    OTHER
                   MOVE    "1"             TO  SPA-S01-TUKI-SKYPRT
                   MOVE    "1"             TO  SPA-S01-TUKI-SRYPRT
               END-EVALUATE
      *
               PERFORM 210-BACK
      *
           END-IF
      *
           .
      *
       220-KAKUTEI-EXT.
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
           MOVE    "S07    "           TO  MCP-WINDOW.
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
           MOVE    SPACE                   TO  S07
           INITIALIZE                          S07
      *
           MOVE    SPA-GMN-SRYYM           TO  S07-SRYYM
      *
           MOVE    SPA-GMN-KIKANKBN-G      TO  S07-KIKANKBN
           MOVE    SPA-GMN-KIKANKBN-MAX    TO  S07-KIKANKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-KIKANKBN-MAX )
               MOVE    SPA-GMN-KIKANKBNLST (IDX0)
                                           TO  S07-KIKANKBN-ITEM (IDX0)
           END-PERFORM
      *
           IF    ( SPA-GMN-KIKANKBN        =   "0" )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-STDAY       TO  S07-STDAY
               MOVE    SPA-GMN-EDDAY       TO  S07-EDDAY
           END-IF
      *
           MOVE    SPA-GMN-TUKIGRPKBN-G    TO  S07-TUKIGRPKBN
           MOVE    SPA-GMN-TUKIGRPKBN-MAX  TO  S07-TUKIGRPKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-TUKIGRPKBN-MAX )
               MOVE    SPA-GMN-TUKIGRPKBNLST (IDX0)
                                           TO  S07-TUKIGRPKBN-ITEM(IDX0)
           END-PERFORM
      *
           EVALUATE SPA-S01-SKYPRT
           WHEN    "0"
               MOVE    "【請求書・明細書不要】"
                                   TO  S07-LBLSKYPRT
           WHEN    "1"
               MOVE    "【請求書・明細書必要】"
                                   TO  S07-LBLSKYPRT
           WHEN    "2"
               MOVE    "【請求書・明細書必要（請求あり）】"
                                   TO  S07-LBLSKYPRT
           WHEN    "3"
               MOVE    "【請求書・明細書必要（訂正時なし）】"
                                   TO  S07-LBLSKYPRT
           WHEN    "4"
               MOVE    "【請求書必要（明細書不要）】"
                                   TO  S07-LBLSKYPRT
           WHEN    "5"
               MOVE    "【請求書必要（請求あり）（明細書不要）】"
                                   TO  S07-LBLSKYPRT
           WHEN    "6"
               MOVE    "【請求書必要（訂正時なし）（明細書不要）】"
                                   TO  S07-LBLSKYPRT
           WHEN    "7"
               MOVE    "【請求書必要（請求あり）（明細書必要）】"
                                   TO  S07-LBLSKYPRT
           END-EVALUATE
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
           IF    ( SPA-GMN-KIKANKBN        =   "0" )
               MOVE    WIDGET-INSENSITIVE  TO  S07-STDAY-STATE
                                               S07-EDDAY-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  S07-STDAY-STATE
                                               S07-EDDAY-STATE
           END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC                 SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-CUR         =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-SKIP-CUR-SEC
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    001
               MOVE    "SRYYM"         TO  MCP-WIDGET
           WHEN    002
               MOVE    "KIKANKBN"      TO  MCP-WIDGET
           WHEN    003
               MOVE    "STDAY"         TO  MCP-WIDGET
           WHEN    004
               MOVE    "EDDAY"         TO  MCP-WIDGET
           WHEN    005
               MOVE    "TUKIGRPKBN"    TO  MCP-WIDGET
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
       5011-INPUT-CUR-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SRYYM"
               MOVE    002             TO  SPA-GMN-CUR
           WHEN    "KIKANKBN"
               MOVE    003             TO  SPA-GMN-CUR
           WHEN    "STDAY"
               MOVE    004             TO  SPA-GMN-CUR
           WHEN    "EDDAY"
               MOVE    005             TO  SPA-GMN-CUR
           WHEN    "TUKIGRPKBN"
               MOVE    001             TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5012-SKIP-CUR-SEC               SECTION.
      *
           IF    ( SPA-GMN-CUR         =   3 )
               IF    ( S07-STDAY-STATE =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-CUR =   4
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   4 )
               IF    ( S07-EDDAY-STATE =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-CUR =   5
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
           WHEN    "1001"
               MOVE    "期間指定が開始日＞終了日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE "対象となる外来収納データが存在しません"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SERR
           MOVE    SPA-ERRCD           TO  SERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  SERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "S07"               TO  SPA-MOTOPG
           MOVE    "SERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "SERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRSET-EXT.
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
      *    年月チェック・編集処理
      *****************************************************************
       800-NENGETU-CHK-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-HENYMDG
                                       WRK-SYMD
      *
           INITIALIZE                  ORCSGDAYAREA
           MOVE    WRK-YMD         TO  SGDAY-INDAY
           MOVE    "2"             TO  SGDAY-INTYPE
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           END-IF
      *
           .
       800-NENGETU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       800-EOM-SEC                     SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"            TO  LNK-DAY7-IRAI
           MOVE    WRK-YMD (1:6)   TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI TO  WRK-YMD
      *
           .
       800-EOM-EXT.
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
      **************************************************************
      *    収納マスター検索処理
      **************************************************************
       900-SYUNOU-KEY73-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    "2"                 TO  SYU-NYUGAIKBN
           MOVE    SPA-S01-PTID        TO  SYU-PTID
           MOVE    SPA-NAI-SRYYM       TO  SYU-SRYYMD
           MOVE    "%"                 TO  SYU-SRYYMD(7:1)
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key73"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY73-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納マスターFETCH処理
      **************************************************************
       900-SYUNOU-KEY73-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key73"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY73-FET-EXT.
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
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

