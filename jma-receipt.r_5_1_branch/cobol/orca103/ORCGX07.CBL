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
       PROGRAM-ID.             ORCGX07.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 病名登録
      *  コンポーネント名  : 自院病名検索（Ｘ０７）
      *  管理者            : 
      *  12/09/27    NACL−太田   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    画面活性非活性
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    チェックマスタ設定用共通パラメタ
           COPY    "X01COMMON-SPA".
      *
      *    チェックマスタ情報用画面スパ
           COPY    "X01SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-USERBYOMEI      PIC 9(01).
           03  FLG-BYOMEI          PIC 9(01).
           03  FLG-ROW             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX1                PIC 9(05).
           03  IDXI                PIC 9(05).
           03  IDXJ                PIC 9(05).
           03  IDXB                PIC 9(05).
           03  IDXU                PIC 9(05).
           03  IDXS                PIC 9(05).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-NUM                     PIC ZZ9.
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-TABLE                   PIC X(64).
           03  WRK-PATHNAME                PIC X(64).
           03  WRK-STIDX                   PIC 9(05).
           03  WRK-BYOMEILST-ROW           PIC 9(05).
           03  WRK-MAE-INPUT               PIC X(500).
           03  WRK-OUT-INPUT               PIC X(500).
           03  WRK-INPUTCD-CUR-G.
               05  WRK-INPUTCD-CUR-SPC     PIC 9(03).
               05  WRK-INPUTCD-CUR-MAX     PIC 9(03).
      *
       01  WRK-WIDGET-AREA.
           03  WRK-WIDGET-INPUTCD-G.
               05  WRK-WIDGET-INPUTCD-STR PIC X(07).
               05  WRK-WIDGET-INPUTCD-NUM PIC 9(01).
      *
       01  CONST-AREA.
           03  CONST-BUNLST-MAX    PIC 9(05)   VALUE   100.
           03  CONST-UBYOLST-MAX   PIC 9(05)   VALUE   100.
           03  CONST-BYOCD-MAX     PIC 9(05)   VALUE   6.
           03  CONST-HOSOKUCD-MAX  PIC 9(05)   VALUE   3.
           03  CONST-MKCD-MAX      PIC 9(05)   VALUE   21.
           03  CONST-SELKBN-BYOMEI           PIC X(01)     VALUE "1".
           03  CONST-SELKBN-HOSOKU           PIC X(01)     VALUE "2".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    自院病名マスタ
       01  USERBYOMEI-REC.
           COPY    "CPUSERBYOMEI.INC".
      *
      *    病名マスタ
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    チェックマスタ業務サブ１（病名欄編集）
           COPY    "CPXSUB01.INC".
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
           COPY    "X01.INC".
           COPY    "X02.INC".
           COPY    "X03.INC".
           COPY    "X04.INC".
           COPY    "X05.INC".
           COPY    "X06.INC".
           COPY    "X07.INC".
           COPY    "X08.INC".
           COPY    "X91.INC".
           COPY    "X97.INC".
           COPY    "X98.INC".
           COPY    "X99.INC".
           COPY    "X100.INC".
           COPY    "XERR.INC".
           COPY    "XID1.INC".
           COPY    "XID2.INC".
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-X01KYOUTU
           MOVE    SPA-FREE        TO  SPA-X01FREE
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
           IF    ( FLG-END             =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-X01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-X01FREE     TO  SPA-FREE
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
           IF    ( SPA-MOTOPG          =   "XERR" )
               MOVE    SPACE               TO  SPA-MOTOPG
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
           MOVE    "X07"               TO  MCP-WINDOW.
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
           INITIALIZE                  SPA-GMN-X07-AREA
                                       SPA-NAI-X07-AREA
      *
           MOVE    1               TO  SPA-GMN-X07-CUR
      *
           IF    ( SPA-X01-SELKBN  =   CONST-SELKBN-BYOMEI )
               MOVE    CONST-BYOCD-MAX
                                   TO  SPA-GMN-X07-BYOCD-MAX
           ELSE
               MOVE    CONST-HOSOKUCD-MAX
                                   TO  SPA-GMN-X07-BYOCD-MAX
           END-IF
      *
           PERFORM VARYING IDXI    FROM    1   BY  1
                   UNTIL ( IDXI    >       SPA-GMN-X07-BYOCD-MAX )
               MOVE    SPA-X01-INPUTCD (IDXI)
                                   TO  SPA-GMN-X07-BYOCD (IDXI)
           END-PERFORM
      *
           PERFORM 4101-BYOMEICD-CHK-SEC
           MOVE    SPACE           TO  SPA-ERRCD
      *
           IF    ( SPA-X01-INDEX   =   ZERO )
      *
      *        分類一覧
               PERFORM 3102-BUNRUI-LST-SEC
      *
      *        自院病名一覧
               MOVE    1               TO  SPA-GMN-X07-UBYOLST-PAGE
      *
      *        分類が選択されている場合
               IF    ( SPA-X01-BUNRUICD    >   ZERO )
                   MOVE    SPA-X01-BUNRUICD    TO  X07-BUNRUICD
                   PERFORM 4100-BUNRUICD-CHK-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       MOVE    SPACE           TO  SPA-ERRCD
                       MOVE    ZERO            TO  SPA-GMN-X07-BUNRUICD
                   END-IF
               END-IF
               IF    ( SPA-GMN-X07-BUNRUICD    =   ZERO )
                   INITIALIZE                      USERBYOMEI-REC
                   MOVE    "tbl_userbyomei"    TO  WRK-TABLE
                   MOVE    "key3"              TO  WRK-PATHNAME
                   PERFORM 3101-USERBYOMEI-LST-SEC
                   MOVE    9                   TO  SPA-GMN-X07-CUR
               END-IF
           ELSE
      *
               MOVE    SPACE       TO  SPA-GMN-X07-BYOCD (SPA-X01-INDEX)
               MOVE    SPA-X01-BYOMEIINPUTCD
                                   TO  SPA-GMN-X07-SELBYOMEI
      *
      *        自院病名一覧
               MOVE    1                   TO  SPA-GMN-X07-UBYOLST-PAGE
      *
               INITIALIZE                      USERBYOMEI-REC
               STRING  SPA-X01-BYOMEIINPUTCD   DELIMITED   BY  SPACE
                       "%"                     DELIMITED   BY  SIZE
               INTO    USBYO-BYOMEIINPUTCD
               END-STRING
               MOVE    "tbl_userbyomei"    TO  WRK-TABLE
               MOVE    "key11"             TO  WRK-PATHNAME
               PERFORM 3101-USERBYOMEI-LST-SEC
               MOVE    2                   TO  SPA-GMN-X07-CUR
           END-IF
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    分類一覧 編集処理
      *****************************************************************
       3102-BUNRUI-LST-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDXB
           INITIALIZE                  SPA-GMN-X07-BUNLST-AREA
      *
           PERFORM 900-USERBYOMEI-KEY4-SEL-SEC
      *
           PERFORM UNTIL ( FLG-USERBYOMEI  NOT =   ZERO )
                    OR   ( IDXB               >=   CONST-BUNLST-MAX  )
      *
               COMPUTE IDXB    =   IDXB    +   1
      *
               IF    ( USBYO-BYOMEIINPUTCD NOT =   SPACE )
                   MOVE    USBYO-BYOMEIINPUTCD(1:3)
                                       TO  SPA-GMN-X07-BBUNRUICD (IDXB)
               END-IF                            
      *
               MOVE    USBYO-BYOMEI    TO  SPA-GMN-X07-BBUNRUIMEI(IDXB)
      *
               PERFORM 900-USERBYOMEI-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    IDXB                TO  SPA-GMN-X07-BUNLST-MAX
           .
       3102-BUNRUI-LST-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               PERFORM 210-BACK
      *    全分類検索
           WHEN    "CLICKED"       ALSO    "B04"
               PERFORM 450-BUNRUI-HENSYU-SEC
      *    前頁
           WHEN    "CLICKED"       ALSO    "B06"
               PERFORM 460-MAEGMN-SEC
      *    次頁
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 470-ATOGMN-SEC
      *    確定
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 450-KAKUTEI-SEC
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
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           MOVE    ZERO            TO  SPA-GMN-X07-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
           WHEN    "ACTIVATE"      ALSO    "BUNRUICD"
               PERFORM 4100-BUNRUICD-CHK-SEC
           WHEN    "ACTIVATE"      ALSO    "SELNUM2"
               PERFORM 4100-SELNUM2-CHK-SEC
           WHEN    "ACTIVATE"      ALSO    "INPUTCD1"
           WHEN    "ACTIVATE"      ALSO    "INPUTCD2"
           WHEN    "ACTIVATE"      ALSO    "INPUTCD3"
           WHEN    "ACTIVATE"      ALSO    "INPUTCD4"
           WHEN    "ACTIVATE"      ALSO    "INPUTCD5"
           WHEN    "ACTIVATE"      ALSO    "INPUTCD6"
               PERFORM 4100-INPUTCD-CHK-SEC
           WHEN    "ACTIVATE"      ALSO    "KENBYOMEI"
               IF    ( SPA-X01-INDEX   =   ZERO )
                   PERFORM 4100-SELBYOMEI-CHK-SEC
               END-IF
      *    病名選択
           WHEN    ANY             ALSO    "BYOMEILST"
               PERFORM 4100-BYOMEISEL-SEC
      *    分類選択
           WHEN    ANY             ALSO    "BUNRUILST"
               PERFORM 4100-BUNRUILST-SEC
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    分類名選択処理
      *****************************************************************
       4100-BUNRUICD-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-X07-BUNLST-SEL
      *
           MOVE    X07-BUNRUICD        TO  SPA-GMN-X07-BUNRUICD
      *
           IF    ( SPA-GMN-X07-BUNRUICD    =   ZERO )
               MOVE    2               TO  SPA-GMN-X07-CUR
           ELSE
               PERFORM VARYING IDXB     FROM    1   BY  1
                       UNTIL ( IDXB     >   SPA-GMN-X07-BUNLST-MAX )
                        OR   ( SPA-GMN-X07-BUNLST-SEL   >  ZERO )
      *
                   IF    ( SPA-GMN-X07-BUNRUICD
                                       =   SPA-GMN-X07-BBUNRUICD (IDXB))
                       MOVE    IDXB    TO  SPA-GMN-X07-BUNLST-SEL
                   END-IF
      *
               END-PERFORM
               IF    ( SPA-GMN-X07-BUNLST-SEL  =   ZERO )
                   MOVE    "0012"      TO  SPA-ERRCD
                   MOVE    9           TO  SPA-GMN-X07-CUR
               ELSE
                   PERFORM 41001-BUNRUICD-SELECT-SEC
               END-IF
           END-IF
      *
           .
       4100-BUNRUICD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名番号選択処理
      *****************************************************************
       4100-SELNUM2-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-X07-CUR
      *
           MOVE    X07-SELNUM2         TO  SPA-GMN-X07-SELNUM
      *
      *    検索時対象が１件のときは選択番号なしで確定
           IF    ( SPA-GMN-X07-SELNUM  =   ZERO )
               IF    ( SPA-X01-INDEX   >   ZERO )
                AND  ( SPA-GMN-X07-UBYOLST-MAX     =   1    )
                AND  ( SPA-GMN-X07-UBYOLST-PAGE    =   1    )
                   MOVE    1           TO  SPA-GMN-X07-SELNUM
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-X07-SELNUM  >   ZERO )
               MOVE    ZERO            TO      IDXU
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SPA-GMN-X07-UBYOLST-MAX )
                        OR   ( IDXU    >   ZERO )
      *
                   IF    ( SPA-GMN-X07-TBANGO (IDX0)
                                       =   SPA-GMN-X07-SELNUM )
                       MOVE    IDX0    TO  IDXU
                       PERFORM 4101-BYOMEI-SELECT-SEC
                   END-IF
               END-PERFORM
      *
               IF    ( IDXU    =   ZERO )
                   MOVE    "0012"      TO  SPA-ERRCD
                   MOVE    2           TO  SPA-GMN-X07-CUR
               END-IF
      *
           ELSE
               IF    ( SPA-X01-INDEX   >   ZERO )
                   MOVE    "0108"      TO  SPA-ERRCD
                   MOVE    2           TO  SPA-GMN-X07-CUR
               END-IF
           END-IF
      *
           .
      *
       4100-SELNUM2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    自院病名行選択処理
      *****************************************************************
       4100-BYOMEISEL-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDXU
           PERFORM VARYING   IDX0  FROM    1   BY  1
                   UNTIL   ( IDX0  >   SPA-GMN-X07-UBYOLST-MAX )
                    OR     ( IDXU  NOT =   ZERO )
      *
               IF    ( X07-BYOMEILST-SELECT (IDX0) =   "T" )
                   MOVE    IDX0        TO  IDXU
                   PERFORM 4101-BYOMEI-SELECT-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       4100-BYOMEISEL-EXT.
           EXIT.
      *****************************************************************
      *    自院病名行選択処理（病名検索時）
      *****************************************************************
       4101-BYOMEI-SELECT-SEC          SECTION.
      *
           MOVE    ZERO            TO  IDXJ
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-X07-BYOCD-MAX )
                    OR   ( IDXJ    >   ZERO )
      *
               IF    ( SPA-GMN-X07-BYOCD (IDX1)    =   SPACE )
                   MOVE    IDX1    TO  IDXJ
               END-IF
      *
           END-PERFORM
      *
           IF    ( IDXJ    =   ZERO )
               MOVE    3           TO  SPA-GMN-X07-CUR
               MOVE    "0013"      TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 41011-BYOMEI-SELECT-SEC
           END-IF
      *
           .
       4101-BYOMEI-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    自院病名行選択処理（病名検索時）
      *****************************************************************
       41011-BYOMEI-SELECT-SEC          SECTION.
      *
           IF    ( SPA-X01-INDEX       =   ZERO )
               MOVE    SPA-GMN-X07-TINPUTCD (IDXU)
                                       TO  SPA-GMN-X07-BYOCD (IDXJ)
               PERFORM 4101-BYOMEICD-CHK-SEC
           ELSE
               MOVE    SPA-X01-INDEX   TO  IDXJ
               MOVE    SPA-GMN-X07-TINPUTCD (IDXU)
                                       TO  SPA-GMN-X07-BYOCD (IDXJ)
               PERFORM 4101-BYOMEICD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    (  SPA-X01-INDEX  =   ZERO )
                   MOVE    ZERO        TO  SPA-GMN-X07-SELNUM
                   MOVE    9           TO  SPA-GMN-X07-CUR
               ELSE
                   PERFORM 450-KAKUTEI-SEC
               END-IF 
           END-IF
      *
           .
       41011-BYOMEI-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    選択分類コード編集処理
      *****************************************************************
       41001-BUNRUICD-SELECT-SEC      SECTION.
      *
           INITIALIZE                  SPA-GMN-X07-SELBYOMEI
           MOVE    1                   TO  SPA-GMN-X07-UBYOLST-PAGE
      *
           INITIALIZE                  USERBYOMEI-REC
           MOVE    SPA-GMN-X07-BBUNRUICD 
                           (SPA-GMN-X07-BUNLST-SEL)
                                       TO  USBYO-BUNRUICD
           MOVE    "tbl_userbyomei"    TO  WRK-TABLE
           MOVE    "key7"              TO  WRK-PATHNAME
           PERFORM 3101-USERBYOMEI-LST-SEC
           MOVE    2                   TO  SPA-GMN-X07-CUR
      *
           .
       41001-BUNRUICD-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    自院病名一覧 編集処理
      *****************************************************************
       3101-USERBYOMEI-LST-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-X07-UBYOLST-AREA
                                       SPA-GMN-X07-SELNUM
      *
           MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           PERFORM 900-USERBYOMEI-SEL-SEC
      *
           COMPUTE WRK-STIDX   = ( SPA-GMN-X07-UBYOLST-PAGE  -  1 )
                               *   CONST-UBYOLST-MAX
                               +   1
      *
           MOVE    ZERO                TO  CNT-LINE
                                           IDXU
           PERFORM UNTIL ( CNT-LINE       >=   CONST-UBYOLST-MAX )
                    OR   ( FLG-USERBYOMEI  NOT =   ZERO )
      *
               COMPUTE IDXU    =   IDXU    +   1
      *
               IF    ( IDXU   >=   WRK-STIDX )
      *
                   COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
                   MOVE    IDXU    TO  SPA-GMN-X07-TBANGO  (CNT-LINE)
                   MOVE    USBYO-BYOMEIINPUTCD
                                   TO  SPA-GMN-X07-TINPUTCD(CNT-LINE)
                   MOVE    USBYO-BYOMEI
                                   TO  SPA-GMN-X07-TBYOMEI (CNT-LINE)
                   MOVE    CNT-LINE    TO  SPA-GMN-X07-UBYOLST-MAX
               END-IF
      *
               MOVE    WRK-TABLE           TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 900-USERBYOMEI-FET-SEC
      *
           END-PERFORM
      *
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-USERBYOMEI  =   ZERO )
               MOVE    1           TO  SPA-GMN-X07-UBYOLST-NEXT
           ELSE
               MOVE    ZERO        TO  SPA-GMN-X07-UBYOLST-NEXT
           END-IF
      *
           MOVE    1               TO  FLG-ROW
      *
           .
       3101-USERBYOMEI-LST-EXT.
           EXIT.
      *****************************************************************
      *    分類名行選択処理
      *****************************************************************
       4100-BUNRUILST-SEC              SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  SPA-GMN-X07-BUNLST-SEL
      *
           PERFORM VARYING     IDXB    FROM    1   BY  1
                   UNTIL     ( IDXB    >   SPA-GMN-X07-BUNLST-MAX )
                    OR       ( SPA-GMN-X07-BUNLST-SEL  NOT =   ZERO )
      *
               IF    ( X07-BUNRUI-SELECT (IDXB)    =   "T" )
                   MOVE    IDXB        TO  SPA-GMN-X07-BUNLST-SEL
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-X07-BUNLST-SEL  >   ZERO )
               MOVE    SPA-GMN-X07-BBUNRUICD (SPA-GMN-X07-BUNLST-SEL)
                                           TO SPA-GMN-X07-BUNRUICD
               PERFORM 41001-BUNRUICD-SELECT-SEC
           END-IF
      *
           .
       4100-BUNRUISEL-EXT.
           EXIT.
      *****************************************************************
      *    自院病名コードチェック処理
      *****************************************************************
       4100-INPUTCD-CHK-SEC            SECTION.
      *
           PERFORM VARYING IDXI    FROM    1   BY  1
                   UNTIL ( IDXI    >       SPA-GMN-X07-BYOCD-MAX )
      *
               MOVE    X07-INPUTCD (IDXI)
                                   TO  SPA-GMN-X07-BYOCD (IDXI)
           END-PERFORM
      *
           PERFORM 4101-BYOMEICD-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
            OR   ( SPA-ERRCD       =   "0006" )
               IF    ( SPA-ERRCD   =   "0006" )
                   MOVE    2       TO  SPA-GMN-X07-CUR
               ELSE
                   PERFORM 41001-INPUTCD-CUR-HEN-SEC
               END-IF
           END-IF
      *
           .
       4100-INPUTCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索病名チェック処理
      *****************************************************************
       4100-SELBYOMEI-CHK-SEC          SECTION.
      *
           MOVE    X07-KENBYOMEI       TO  SPA-GMN-X07-SELBYOMEI
      *
           IF    ( SPA-GMN-X07-SELBYOMEI   =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-X07-SELBYOMEI
                                       TO  WRK-MAE-INPUT
               PERFORM 800-KANA-CHK-SEC
               MOVE    WRK-OUT-INPUT   TO  SPA-GMN-X07-SELBYOMEI
               IF    ( FLG-ERR     NOT =   ZERO )
                   MOVE    "0071"      TO  SPA-ERRCD
                   MOVE    1           TO  SPA-GMN-X07-CUR
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
      *
                   MOVE    1               TO  SPA-GMN-X07-UBYOLST-PAGE
      *
                   INITIALIZE                      USERBYOMEI-REC
                   STRING  "%"                     DELIMITED  BY  SIZE
                           SPA-GMN-X07-SELBYOMEI   DELIMITED  BY  SPACE
                           "%"                     DELIMITED  BY  SIZE
                   INTO    USBYO-BYOMEI
                   END-STRING
      *
                   MOVE    "tbl_userbyomei"    TO  WRK-TABLE
                   MOVE    "key8"              TO  WRK-PATHNAME
                   PERFORM 3101-USERBYOMEI-LST-SEC
      *
               END-IF
           END-IF
      *
           .
       4100-SELBYOMEI-CHK-EXT.
           EXIT.
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
           PERFORM 4101-BYOMEICD-CHK-SEC
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    前頁処理
      *****************************************************************
       4101-PREV-PAGE-SEC              SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    1           TO  SPA-GMN-X07-CUR
      *
           .
       4101-PREV-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    後頁処理
      *****************************************************************
       4101-NEXT-PAGE-SEC              SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    1           TO  SPA-GMN-X07-CUR
      *
           .
       4101-NEXT-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "X04"               TO  SPA-SAKIPG
           MOVE    "X07"               TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
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
      *    全分類編集処理
      *****************************************************************
       450-BUNRUI-HENSYU-SEC           SECTION.
      *
           INITIALIZE                      SPA-GMN-X07-SELBYOMEI
           MOVE    ZERO                TO  SPA-GMN-X07-BUNRUICD
                                           SPA-GMN-X07-BUNLST-SEL
      *    検索処理
           MOVE    "tbl_userbyomei"    TO  WRK-TABLE
           MOVE    "key3"              TO  WRK-PATHNAME
           INITIALIZE                  USERBYOMEI-REC
           PERFORM 3101-USERBYOMEI-LST-SEC
      *
           MOVE    9                   TO  SPA-GMN-X07-CUR
      *
           .
       450-BUNRUI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    前頁処理
      *****************************************************************
       460-MAEGMN-SEC             SECTION.
      *
           IF    ( SPA-GMN-X07-UBYOLST-PAGE    >   1 )
               COMPUTE SPA-GMN-X07-UBYOLST-PAGE
                   =   SPA-GMN-X07-UBYOLST-PAGE    -   1
               PERFORM 4601-PAGE-CHANGE-SEC
           END-IF
      *
           .
       460-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁変更処理
      *****************************************************************
       4601-PAGE-CHANGE-SEC            SECTION.
      *
           INITIALIZE                  SPA-GMN-X07-SELBYOMEI
      *
      *    検索処理
           IF    ( SPA-GMN-X07-BUNRUICD    =   ZERO )
               IF    ( SPA-X01-INDEX   =   ZERO )
                   INITIALIZE                  USERBYOMEI-REC
                   MOVE    "tbl_userbyomei"    TO  WRK-TABLE
                   MOVE    "key3"              TO  WRK-PATHNAME
               ELSE
                   INITIALIZE                  USERBYOMEI-REC
                   STRING  SPA-X01-BYOMEIINPUTCD   DELIMITED   BY  SPACE
                           "%"                     DELIMITED   BY  SIZE
                   INTO    USBYO-BYOMEIINPUTCD
                   END-STRING
                   MOVE    "tbl_userbyomei"    TO  WRK-TABLE
                   MOVE    "key11"             TO  WRK-PATHNAME
               END-IF
           ELSE
               MOVE    "tbl_userbyomei"    TO  WRK-TABLE
               MOVE    "key7"              TO  WRK-PATHNAME
               INITIALIZE                  USERBYOMEI-REC
               MOVE    SPA-GMN-X07-BUNRUICD
                                           TO  USBYO-BUNRUICD
           END-IF
      *
           PERFORM 3101-USERBYOMEI-LST-SEC
      *
           MOVE    2                   TO  SPA-GMN-X07-CUR
      *
           .
       4601-PAGE-CHANGE-EXT.
           EXIT.
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       470-ATOGMN-SEC             SECTION.
      *
           IF    ( SPA-GMN-X07-UBYOLST-NEXT    NOT =   ZERO )
               COMPUTE SPA-GMN-X07-UBYOLST-PAGE
                   =   SPA-GMN-X07-UBYOLST-PAGE    +   1
               PERFORM 4601-PAGE-CHANGE-SEC
           END-IF
      *
           .
       470-ATOGMN-EXT.
           EXIT.
      *****************************************************************
      *    確定処理
      *****************************************************************
       450-KAKUTEI-SEC             SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4501-KAKUTEI-SEC
           END-IF
      *
           .
       450-KAKUTEI-EXT.
           EXIT.
      *****************************************************************
      *    確定処理
      *****************************************************************
       4501-KAKUTEI-SEC             SECTION.
      *
           INITIALIZE                      SPA-X01-INPUTCD-G
           PERFORM VARYING IDXI    FROM    1   BY  1
                   UNTIL ( IDXI    >   SPA-GMN-X07-BYOCD-MAX )
               MOVE    SPA-GMN-X07-BYOCD (IDXI)
                                   TO  SPA-X01-INPUTCD (IDXI)
           END-PERFORM
      *
           MOVE    1                   TO  SPA-X01-UERBYOMEI-KAKUTEI
      *
           MOVE    "X04"               TO  SPA-SAKIPG
           MOVE    "X07"               TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4501-KAKUTEI-EXT.
           EXIT.
      *****************************************************************
      *    病名コードチェック処理
      *****************************************************************
       4101-BYOMEICD-CHK-SEC           SECTION.
      *
           INITIALIZE                  XSUB01-LNK
           PERFORM VARYING IDXI    FROM    1   BY  1
                   UNTIL ( IDXI    >       SPA-GMN-X07-BYOCD-MAX )
      *
               MOVE    SPA-GMN-X07-BYOCD (IDXI)
                                       TO  WRK-MAE-INPUT
               PERFORM 800-KANA-CHK-SEC
               MOVE    WRK-OUT-INPUT   TO  SPA-GMN-X07-BYOCD (IDXI)
                                           XSUB01-BYOCD (IDXI)
      *
           END-PERFORM
      *
           IF    ( SPA-X01-BYOSEL      =   1 )
               MOVE    1               TO  XSUB01-SINGLE
           END-IF
      *
           CALL    "ORCGXSUB01"    USING   SPA-AREA
                                           XSUB01-LNK
      *
           INITIALIZE                      SPA-NAI-X07-INPUT-AREA
           PERFORM VARYING IDXS    FROM    1   BY  1
                   UNTIL ( IDXS    >       SPA-GMN-X07-BYOCD-MAX )
      *
               MOVE    XSUB01-O-BYOCD (IDXS)
                                   TO  SPA-GMN-X07-BYOCD (IDXS)
      *
               IF    ( XSUB01-O-TOP (IDXS) =   "1" )
      *
                   COMPUTE SPA-NAI-X07-BYOINF-MAX
                       =   SPA-NAI-X07-BYOINF-MAX  +   1
                   MOVE    SPA-NAI-X07-BYOINF-MAX
                                   TO  IDXI
                   MOVE    IDXS    TO  SPA-NAI-X07-BYOIDX (IDXI)
                   MOVE    XSUB01-O-BYOSTATUS (IDXS)
                                   TO  SPA-NAI-X07-BYOSTATUS (IDXI)
                   MOVE    XSUB01-O-BYOMEI(IDXS)
                                   TO  SPA-NAI-X07-BYOMEI (IDXI)
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   CONST-MKCD-MAX )
      *
                       MOVE    XSUB01-O-RECECD (IDXS IDX0)
                                   TO  SPA-NAI-X07-BYOCD (IDXI IDX0)
      *
                   END-PERFORM
      *
               END-IF
           END-PERFORM
      *
           MOVE    XSUB01-O-BYOMEIALL  TO  SPA-GMN-X07-BYOMEI
      *
           EVALUATE    XSUB01-O-RC
           WHEN    1
               MOVE    "0005"          TO  SPA-ERRCD
           WHEN    2
               MOVE    "0006"          TO  SPA-ERRCD
           WHEN    3
               MOVE    "0007"          TO  SPA-ERRCD
           WHEN    4
               MOVE    "0008"          TO  SPA-ERRCD
           END-EVALUATE
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
            AND  ( SPA-ERRCD       NOT =   "0006" )
               IF    ( SPA-X01-INDEX   >   ZERO )
                AND  ( XSUB01-O-ERRIDX >   SPA-X01-INDEX )
                   MOVE    SPACE       TO  SPA-ERRCD
               ELSE
                   IF    ( XSUB01-O-ERRIDX >   ZERO )
                    OR   ( XSUB01-O-ERRIDX <=  SPA-GMN-X07-BYOCD-MAX )
                       COMPUTE SPA-GMN-X07-CUR
                           =   XSUB01-O-ERRIDX
                           +   2
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-X01-BYOSEL      =   1 )
                AND  ( XSUB01-O-BYOMEI-CNT >   1 )
                   MOVE    "0103"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4101-BYOMEICD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カーソル位置決定処理
      *****************************************************************
       41001-INPUTCD-CUR-HEN-SEC       SECTION.
      *
           MOVE    WRK-MCP-WIDGET  TO  WRK-WIDGET-INPUTCD-G
      *
           INITIALIZE                  WRK-INPUTCD-CUR-G
           PERFORM VARYING IDXI    FROM    1   BY  1
                   UNTIL ( IDXI    >   SPA-GMN-X07-BYOCD-MAX )
               IF    ( SPA-GMN-X07-BYOCD (IDXI)
                                   =   SPACE )
                   IF    ( WRK-INPUTCD-CUR-SPC     =   ZERO )
                       MOVE    IDXI    TO  WRK-INPUTCD-CUR-SPC
                   END-IF
               ELSE
                       MOVE    IDXI    TO  WRK-INPUTCD-CUR-MAX
               END-IF
           END-PERFORM
      *
           IF    ( WRK-INPUTCD-CUR-MAX >   WRK-INPUTCD-CUR-SPC )
                   COMPUTE SPA-GMN-X07-CUR
                       =   WRK-INPUTCD-CUR-SPC   +   2
           ELSE
               IF    ( WRK-INPUTCD-CUR-MAX >=  WRK-WIDGET-INPUTCD-NUM )
                   COMPUTE SPA-GMN-X07-CUR
                       =   WRK-WIDGET-INPUTCD-NUM    +   3
               ELSE
                   IF    ( X07-INPUTCD (WRK-WIDGET-INPUTCD-NUM)
                                       NOT =   SPACE )
                       COMPUTE SPA-GMN-X07-CUR
                           =   WRK-INPUTCD-CUR-MAX   +   3
                   ELSE
                       MOVE    2       TO  SPA-GMN-X07-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
       41001-INPUTCD-CUR-HEN-EXT.
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
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "X07    "           TO  MCP-WINDOW.
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
           IF    ( FLG-ROW             =   1 )
               MOVE    ZERO                TO  WRK-BYOMEILST-ROW
           ELSE
               MOVE    X07-BYOMEILST-ROW   TO  WRK-BYOMEILST-ROW
           END-IF
      *
           MOVE    SPACE                   TO  X07
           INITIALIZE                          X07
      *
           PERFORM VARYING IDXU        FROM    1   BY  1
                   UNTIL ( IDXU     >  SPA-GMN-X07-UBYOLST-MAX )
      *
               MOVE    SPA-GMN-X07-TBANGO  (IDXU)
                                           TO  WRK-NUM
               MOVE    WRK-NUM             TO  X07-TBANGO    (IDXU)
               MOVE    SPA-GMN-X07-TINPUTCD(IDXU)
                                           TO  X07-TBYOMEIINPUTCD(IDXU)
               MOVE    SPA-GMN-X07-TBYOMEI (IDXU)
                                           TO  X07-TBYOMEI   (IDXU)
           END-PERFORM
           MOVE    SPA-GMN-X07-UBYOLST-MAX TO  X07-BYOMEILST-COUNT
           MOVE    WRK-BYOMEILST-ROW       TO  X07-BYOMEILST-ROW
      *
           MOVE    SPA-GMN-X07-SELBYOMEI   TO  X07-KENBYOMEI
           MOVE    SPA-GMN-X07-BUNRUICD    TO  X07-BUNRUICD
           MOVE    SPA-GMN-X07-SELNUM      TO  X07-SELNUM2
           PERFORM VARYING     IDX0    FROM   1   BY  1
                   UNTIL     ( IDX0    >      SPA-GMN-X07-BYOCD-MAX )
               MOVE    SPA-GMN-X07-BYOCD (IDX0)
                                           TO  X07-INPUTCD (IDX0)
           END-PERFORM
           MOVE    SPA-GMN-X07-BYOMEI      TO  X07-BYOMEI
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   SPA-GMN-X07-BUNLST-MAX )
      *
               MOVE    SPA-GMN-X07-BBUNRUICD (IDXB)
                                           TO  X07-BBUNRUICD (IDXB)
               MOVE    SPA-GMN-X07-BBUNRUIMEI(IDXB)
                                           TO  X07-BBUNRUIMEI(IDXB)
      *
               IF    ( IDXB        =   SPA-GMN-X07-BUNLST-SEL )
                   MOVE    "T"     TO  X07-BUNRUI-SELECT (IDXB)
               ELSE
                   MOVE    "F"     TO  X07-BUNRUI-SELECT (IDXB)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-X07-BUNLST-MAX  TO  X07-BUNRUI-COUNT
           MOVE    ZERO                    TO  X07-BUNRUI-ROW
      *
           MOVE    WIDGET-NORMAL           TO  X07-B04-STATE
                                               X07-B06-STATE
                                               X07-B07-STATE
                                               X07-B12-STATE
                                               X07-BUNRUICD-STATE
                                               X07-KENBYOMEI-STATE
      *
      *
           PERFORM VARYING     IDX0    FROM    1   BY  1
                   UNTIL     ( IDX0    >       6 )
      *
               MOVE    WIDGET-NORMAL   TO  X07-INPUTCD-STATE (IDX0)
               MOVE    "T"             TO  X07-INPUTCD-VISIBLE (IDX0)
      *
           END-PERFORM
      *
           IF    ( SPA-X01-INDEX   =   ZERO )
               CONTINUE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  X07-B04-STATE
                                               X07-B12-STATE
                                               X07-BUNRUICD-STATE
                                               X07-KENBYOMEI-STATE
               PERFORM VARYING     IDX0    FROM    1   BY  1
                       UNTIL     ( IDX0    >       6 )
                   MOVE    WIDGET-INSENSITIVE
                                           TO  X07-INPUTCD-STATE (IDX0)
               END-PERFORM
           END-IF
      *
           IF    ( SPA-GMN-X07-UBYOLST-PAGE   <=   1 )
               MOVE    WIDGET-INSENSITIVE  TO  X07-B06-STATE
           END-IF
      *
           IF    ( SPA-GMN-X07-UBYOLST-NEXT    =   ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  X07-B07-STATE
           END-IF
      *
           IF    ( SPA-X01-INDEX   =   ZERO )
               MOVE    "検索病名"      TO  X07-TITLE
           ELSE
               MOVE    "検索コード"    TO  X07-TITLE
           END-IF
      *
           IF    ( SPA-X01-SELKBN  =   CONST-SELKBN-HOSOKU )
               MOVE    "補足コメント" TO   X07-LBLBYOMEI
               MOVE    "F"            TO   X07-INPUTCD-VISIBLE (4)
                                           X07-INPUTCD-VISIBLE (5)
                                           X07-INPUTCD-VISIBLE (6)
           ELSE
               MOVE    "病　名"       TO   X07-LBLBYOMEI
           END-IF
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
           IF    ( SPA-ERRCD           =   SPACE )
             AND ( SPA-GMN-X07-CUR     =   ZERO  )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-SKIP-CUR-SEC
      *
           EVALUATE    SPA-GMN-X07-CUR
           WHEN    1
               MOVE  "KENBYOMEI"   TO  MCP-WIDGET
           WHEN    2
               MOVE  "SELNUM2"     TO  MCP-WIDGET
           WHEN    3
               MOVE  "INPUTCD1"    TO  MCP-WIDGET
           WHEN    4
               MOVE  "INPUTCD2"    TO  MCP-WIDGET
           WHEN    5
               MOVE  "INPUTCD3"    TO  MCP-WIDGET
           WHEN    6
               MOVE  "INPUTCD4"    TO  MCP-WIDGET
           WHEN    7
               MOVE  "INPUTCD5"    TO  MCP-WIDGET
           WHEN    8
               MOVE  "INPUTCD6"    TO  MCP-WIDGET
           WHEN    9
               MOVE  "BUNRUICD"    TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "KENBYOMEI"
               MOVE    2           TO  SPA-GMN-X07-CUR
           WHEN    "BUNRUICD"
               MOVE    2           TO  SPA-GMN-X07-CUR
           WHEN    "SELNUM2"
               MOVE    2           TO  SPA-GMN-X07-CUR
           WHEN    "INPUTCD1"
               MOVE    3           TO  SPA-GMN-X07-CUR
           WHEN    "INPUTCD2"
               MOVE    4           TO  SPA-GMN-X07-CUR
           WHEN    "INPUTCD3"
               MOVE    5           TO  SPA-GMN-X07-CUR
           WHEN    "INPUTCD4"
               MOVE    6           TO  SPA-GMN-X07-CUR
           WHEN    "INPUTCD5"
               MOVE    7           TO  SPA-GMN-X07-CUR
           WHEN    "INPUTCD6"
               MOVE    8           TO  SPA-GMN-X07-CUR
           END-EVALUATE
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5012-SKIP-CUR-SEC               SECTION.
      *
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
           MOVE    SPACE                          TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0005"
               STRING  "未コード化傷病名は他のコードといっしょに"
                                                   DELIMITED   BY  SIZE
                       "入力できません"            DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0006"
               STRING  "病名コードが２２個以上になりますので、編"
                                                   DELIMITED   BY  SIZE
                       "集病名とします"            DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0007"
               MOVE    "病名コードがありません"
                                       TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "自院病名コードがありません"
                                       TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE    "選択番号入力エラー"        TO  SPA-ERRMSG
           WHEN    "0013"
               MOVE    "これ以上選択できません"    TO  SPA-ERRMSG
           WHEN    "0071"
               MOVE    "全角入力して下さい"        TO  SPA-ERRMSG
           WHEN    "0103"
               MOVE
                   "変更処理中なので病名はひとつしか入力できません"
                                                   TO  SPA-ERRMSG
           WHEN    "0108"
               MOVE    "項目に入力がありません"    TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  XERR
           MOVE    SPA-ERRCD           TO  XERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  XERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "X07"               TO  SPA-MOTOPG
           MOVE    "XERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XERR"              TO  MCP-WINDOW
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
      *    カナチェック処理
      *****************************************************************
       800-KANA-CHK-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    1                   TO  FLG-ERR
               MOVE    WRK-MAE-INPUT       TO  WRK-OUT-INPUT
               IF    ( KANACHK-RC2     =   7 )
                   MOVE    KANACHK-OUT-INPUT
                                           TO  WRK-OUT-INPUT
               END-IF
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-OUT-INPUT
           END-IF
      *
           .
       800-KANA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    自院病名検索処理
      *****************************************************************
       900-USERBYOMEI-KEY4-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-USERBYOMEI
      *
      *    分類コード ’Ｃ’のもの
           INITIALIZE                  USERBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  USERBYOMEI-REC
           ELSE
               INITIALIZE                  USERBYOMEI-REC
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
      *
           .
      *
       900-USERBYOMEI-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    自院病名検索処理
      *****************************************************************
       900-USERBYOMEI-KEY4-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-USERBYOMEI
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  USERBYOMEI-REC
           ELSE
               INITIALIZE                  USERBYOMEI-REC
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
      *
           .
      *
       900-USERBYOMEI-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    自院病名検索処理
      *****************************************************************
       900-USERBYOMEI-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-USERBYOMEI
      *
           MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  USERBYOMEI-REC
           ELSE
               INITIALIZE                  USERBYOMEI-REC
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
      *
           .
      *
       900-USERBYOMEI-SEL-EXT.
           EXIT.
      *****************************************************************
      *    自院病名検索処理
      *****************************************************************
       900-USERBYOMEI-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-USERBYOMEI
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  USERBYOMEI-REC
           ELSE
               INITIALIZE                  USERBYOMEI-REC
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
      *
           .
      *
       900-USERBYOMEI-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
