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
       PROGRAM-ID.                 ORCGN41.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ヘルプマスタ
      *  コンポーネント名  : ヘルプマスタ登録（Ｎ４１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/01/21    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.06.00    NACL-多々納  11/03/22  LD104 ,LD105,LD106,LD108,LD109　統合
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面スパ領域
       01  SPA-N41.
           03  SPA-N41GMN-AREA.
               05  SPA-GMN-PAGE                  PIC 9(04).
               05  SPA-GMN-NEXT                  PIC 9(04).
               05  SPA-GMN-PAGE2                 PIC 9(04).
               05  SPA-GMN-NEXT2                 PIC 9(04).
               05  SPA-GMN-CUR                   PIC 9(02).
      *
               05  SPA-GMN-HELPKBN-G.
                   07  SPA-GMN-HELPKBN             PIC X(01).
                   07  SPA-GMN-HELPKBNH1           PIC X(01).
                   07  SPA-GMN-HELPKBN-MEI         PIC X(10).
               05  SPA-GMN-HELPKBN-COMB.
                   07  SPA-GMN-HELPKBN-ITEM        PIC X(12)
                                                   OCCURS   5.
                   07  SPA-HELPKBN-MAX             PIC 9(04).
      *
               05  SPA-GMN-LDNAME-G.
                   07  SPA-GMN-LDNAME              PIC X(40).
                   07  SPA-GMN-LDNAME-LD           PIC X(20).
               05  SPA-GMN-LDNAME-COMB-G.
                   07  SPA-GMN-LDNAME-COMB         OCCURS   50.
                       09  SPA-GMN-LDNAME-ITEM     PIC X(40).
                       09  SPA-GMN-LDNAME-LDC      PIC X(20).
                   07  SPA-LDNAME-MAX              PIC 9(04).
      *        複写先
               05  SPA-GMN-COPY-AREA.
                   07  SPA-GMN-HELPKBN-CP-G.
                       09  SPA-GMN-HELPKBN-CP          PIC X(01).
                       09  SPA-GMN-HELPKBN-CPH1        PIC X(01).
                       09  SPA-GMN-HELPKBN-CPMEI       PIC X(10).
                   07  SPA-GMN-LDNAME-CP-G.
                       09  SPA-GMN-LDNAME-CP           PIC X(40).
                       09  SPA-GMN-LDNAME-LD-CP        PIC X(20).
                   07  SPA-GMN-MENUNUM1-CP             PIC 9(04).
                   07  SPA-GMN-MENU1-CP                PIC X(30).
                   07  SPA-GMN-MENUNUM2-CP             PIC 9(04).
                   07  SPA-GMN-MENU2-CP                PIC X(30).
      *
               05  SPA-GMN-HEADMENU                PIC X(30).
               05  SPA-GMN-MENUTBL.
                   07  SPA-GMN-TBLREC              OCCURS  300.
                       09  SPA-GMN-TMENUNUM        PIC 9(04).
                       09  SPA-GMN-TMENU           PIC X(30).
                       09  SPA-GMN-TMENU2FLG       PIC 9(01).
                   07  SPA-GMN-TBLMAX              PIC 9(04).
               05  SPA-GMN-SELNUM            PIC 9(04).
               05  SPA-GMN-MENU              PIC X(30).
      *
               05  SPA-GMN-MENU2TBL.
                   07  SPA-GMN-TBLREC2              OCCURS  300.
                       09  SPA-GMN-TMENUNUM2        PIC 9(04).
                       09  SPA-GMN-TMENU2           PIC X(30).
                   07  SPA-GMN-TBLMAX2              PIC 9(04).
               05  SPA-GMN-SELNUM2           PIC 9(04).
               05  SPA-GMN-MENU2             PIC X(30).
      *
               05  SPA-GMN-HELP              PIC X(2000).
      *
           03  SPA-NAI-AREA.
               05  SPA-SYORIFLG            PIC 9(01).
               05  SPA-SYORIFLG2           PIC 9(01).
               05  SPA-COPYFLG             PIC 9(01).
      *
               05  SPA-NAI-MENU1FLG        PIC 9(01).
               05  SPA-NAI-MENU2FLG        PIC 9(01).
      *
               05  SPA-NAI-MENUNUM1        PIC 9(04).
               05  SPA-NAI-MENUNUM2        PIC 9(04).
      *
               05  SPA-NAI-COPY-AREA.
                   07  SPA-NAI-MENUNUM1-CP        PIC 9(04).
                   07  SPA-NAI-MENUNUM2-CP        PIC 9(04).
                   07  SPA-NAI-MENU1-CP           PIC X(30).
      *
               05  SPA-NAI-HELP-G.
                   07  SPA-NAI-HELP              PIC X(50)
                                                 OCCURS   40.
      *    ヘルプ共通パラメタ
           COPY    "N41COMMON-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-HELP            PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-KOUSIN          PIC 9(01).
           03  FLG-HELP-ARI        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX-H               PIC 9(04).
           03  IDX-H1              PIC 9(04).
           03  IDX-H2              PIC 9(04).
           03  IDX-H3              PIC 9(04).
           03  IDX-TBL             PIC 9(04).
           03  IDX-ST              PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-K2              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ORC-DBPATH          PIC X(64).
           03  WRK-MCP-WIDGET          PIC X(60).
           03  WRK-SELNUM              PIC 9(04).
      *
           03  WRK-KEY-AREA.
               05  WRK-HELPKBN             PIC X(01).
               05  WRK-KEY-OPID            PIC X(16).
               05  WRK-LDNAME-LD           PIC X(20).
               05  WRK-MENUNUM1            PIC 9(04).
               05  WRK-MENUNUM2            PIC 9(04).
               05  WRK-EDANUM              PIC 9(04).
           03  WRK-MENU                    PIC X(30).
           03  WRK-INPUT-MENU              PIC X(30).
      *
           03  WRK-HELP-AREA.
               05  WRK-HELP            PIC X(50)   OCCURS   40.
      *
           03  WRK-N41IDMSG            PIC X(80).
      *
      *    コラムリスト位置
           03  WRK-N41-ROW             PIC S9(09).
           03  WRK-N41-ROWATTR         PIC S9(09).
           03  WRK-N41-ROW2            PIC S9(09).
           03  WRK-N41-ROWATTR2        PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
           03  FLG-GMN-INIT2           PIC  9(01).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    ヘルプマスタ−
       01  HELP-REC.
           COPY    "CPHELP.INC".
      *
      *    ヘルプマスタ−ワーク
       01  WRK-HELP-REC.
           COPY    "CPHELP.INC"        REPLACING
                                       //HELP-// BY //WRK-HELP-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    全角変換サブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    業務名
           COPY    "CPLOCKCONST.INC".
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
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
      *orca104
           COPY    "V01.INC".
           COPY    "V02.INC".
           COPY    "V03.INC".
           COPY    "VERR.INC".
           COPY    "VID1.INC".
      *orca105
           COPY    "N01.INC".
           COPY    "NERR.INC".
           COPY    "NID1.INC".
      *orca106
           COPY    "N11.INC".
           COPY    "N11ERR.INC".
           COPY    "N11ID1.INC".
      *orca108
           COPY    "N31.INC".
           COPY    "N31ERR.INC".
           COPY    "N31ID1.INC".
      *orca109
           COPY    "N41.INC".
           COPY    "N41ERR.INC".
           COPY    "N41ID1.INC".
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
           MOVE    SPA-WORK        TO  SPA-N41KYOUTU
           MOVE    SPA-FREE        TO  SPA-N41
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
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-N41KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-N41         TO  SPA-FREE
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
           IF      SPA-MOTOPG          =   "N41ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      *
               MOVE   SPACE                TO  MCP-PUTTYPE
               MOVE   "N41"                TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
           IF      LNK-KYOUTU      NOT =  SPACE
               MOVE    LNK-KYOUTU      TO  SPA-AREA
           END-IF
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "N41ID1"
      *            確認画面より
                   PERFORM 3002-N41ID1-SET-SEC
               WHEN    OTHER
                   PERFORM 3001-SPA-SET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面編集処理
      *****************************************************************
       3001-SPA-SET-SEC             SECTION.
      *
           INITIALIZE                  SPA-N41
           INITIALIZE                  SPA-N41KYOUTU
      *    リスト初期編集
           MOVE    "0 標準"            TO  SPA-GMN-HELPKBN-ITEM(1)
           MOVE    "1 自院"            TO  SPA-GMN-HELPKBN-ITEM(2)
           MOVE    2                   TO  SPA-HELPKBN-MAX
      *    業務名編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   ORC-LOCKCONST-COUNT
               MOVE    ORC-LOCKCONST-LD(IDX)
                                       TO  SPA-GMN-LDNAME-LDC(IDX)
               MOVE    ORC-LOCKCONST-NAME(IDX)
                                       TO  SPA-GMN-LDNAME-ITEM(IDX)
               MOVE    IDX             TO  SPA-LDNAME-MAX
           END-PERFORM
           ADD     1                   TO  SPA-LDNAME-MAX
           MOVE    "9999"              TO  SPA-GMN-LDNAME-LDC
                                                      (SPA-LDNAME-MAX)
           MOVE    "共通"              TO  SPA-GMN-LDNAME-ITEM
                                                      (SPA-LDNAME-MAX)
      ***平成１７年４月は診療行為のみとする
           MOVE    1                   TO  SPA-LDNAME-MAX
           MOVE    "ORCA21"            TO  SPA-GMN-LDNAME-LDC
                                                      (SPA-LDNAME-MAX)
           MOVE    "診療行為入力"      TO  SPA-GMN-LDNAME-ITEM
                                                      (SPA-LDNAME-MAX)
      *****
      *    初期表示
           MOVE    SPA-GMN-LDNAME-LDC(1)   TO  SPA-GMN-LDNAME-LD
           MOVE    SPA-GMN-LDNAME-ITEM(1)  TO  SPA-GMN-LDNAME
      *??
      **   MOVE    SPA-GMN-HELPKBN-ITEM(1) TO  SPA-GMN-HELPKBN-G
      *??
           MOVE    SPA-GMN-HELPKBN-ITEM(2) TO  SPA-GMN-HELPKBN-G
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-SYORIFLG
           MOVE    ZERO                TO  SPA-SYORIFLG2
           PERFORM 3001-MENU-HENSYU-SEC
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       3001-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    メニュウ内容編集処理
      *****************************************************************
       3001-MENU-HENSYU-SEC          SECTION.
      *
           IF      SPA-SYORIFLG        =   1
               PERFORM 30011-MENU1-HENSYU-SEC
      *        クリア
               INITIALIZE                  SPA-GMN-MENU2TBL
               MOVE    ZERO                TO  SPA-GMN-NEXT2
               MOVE    ZERO                TO  SPA-GMN-PAGE2
               MOVE    ZERO                TO  SPA-NAI-MENU1FLG
               MOVE    ZERO                TO  SPA-NAI-MENU2FLG
               MOVE    ZERO                TO  SPA-GMN-SELNUM
               MOVE    ZERO                TO  SPA-NAI-MENUNUM1
               MOVE    SPACE               TO  SPA-GMN-MENU
           ELSE
               PERFORM 30012-MENU2-HENSYU-SEC
           END-IF
           MOVE    SPACE               TO  SPA-GMN-HELP
           INITIALIZE                  SPA-GMN-COPY-AREA
           INITIALIZE                  SPA-NAI-COPY-AREA
           MOVE    ZERO                TO  SPA-COPYFLG
           MOVE    ZERO                TO  SPA-SYORIFLG2
           MOVE    ZERO                TO  SPA-NAI-MENUNUM2
      *
           .
       3001-MENU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    メニュウ１内容編集処理
      *****************************************************************
       30011-MENU1-HENSYU-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-MENUTBL
           MOVE    1                   TO  FLG-GMN-INIT
      *
           MOVE    SPACE               TO  HELP-REC
           INITIALIZE                  HELP-REC
           MOVE    SPA-GMN-HELPKBN     TO  HELP-HELPKBN
           MOVE    SPACE               TO  HELP-KEY-OPID
           MOVE    SPA-GMN-LDNAME-LD   TO  HELP-LDNAME
      *
           MOVE    "tbl_help"          TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
           MOVE    SPA-HOSPNUM         TO  HELP-HOSPNUM
           MOVE    HELP-REC            TO  MCPDATA-REC
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-HELP-READ-SEC
           ELSE
               INITIALIZE                  HELP-REC
               MOVE    1               TO  FLG-HELP
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDX-ST
           IF      SPA-GMN-PAGE        =   ZERO
               MOVE    1                   TO  IDX-ST
           ELSE
               COMPUTE IDX-ST          =   (SPA-GMN-PAGE   -   1)
                                       *   300
                                       +   1
           END-IF
           PERFORM
               UNTIL   (FLG-HELP           =   1  )  OR
                       (IDX                >=  300)
               ADD     1                   TO  IDY
               IF      IDX-ST              <=  IDY
                   ADD     1                   TO  IDX
                   MOVE    HELP-MENUNUM1   TO  SPA-GMN-TMENUNUM (IDX)
                   MOVE    HELP-MENU2FLG   TO  SPA-GMN-TMENU2FLG(IDX)
                   MOVE    HELP-MENU       TO  SPA-GMN-TMENU    (IDX)
               END-IF
      *
               MOVE    "tbl_help"          TO  MCP-TABLE
               MOVE    "key1"              TO  MCP-PATHNAME
               PERFORM 900-HELP-READ-SEC
           END-PERFORM
           MOVE    "tbl_help"          TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    次頁判定
           IF     (IDX                 >=  300 )  AND
                  (FLG-HELP            =   ZERO)
               MOVE    1               TO  SPA-GMN-NEXT
           ELSE
               MOVE    ZERO            TO  SPA-GMN-NEXT
           END-IF
           MOVE    IDX                 TO  SPA-GMN-TBLMAX
           .
       30011-MENU1-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    メニュウ２内容編集処理
      *****************************************************************
       30012-MENU2-HENSYU-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-MENU2TBL
           MOVE    ZERO                TO  SPA-GMN-NEXT2
           MOVE    1                   TO  FLG-GMN-INIT2
      *
           MOVE    SPACE               TO  HELP-REC
           INITIALIZE                  HELP-REC
           MOVE    SPA-GMN-HELPKBN     TO  HELP-HELPKBN
           MOVE    SPACE               TO  HELP-KEY-OPID
           MOVE    SPA-GMN-LDNAME-LD   TO  HELP-LDNAME
           MOVE    SPA-NAI-MENUNUM1    TO  HELP-MENUNUM1
      *
           MOVE    "tbl_help"          TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           MOVE    SPA-HOSPNUM         TO  HELP-HOSPNUM
           MOVE    HELP-REC            TO  MCPDATA-REC
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-HELP-READ-SEC
           ELSE
               INITIALIZE                  HELP-REC
               MOVE    1               TO  FLG-HELP
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDX-ST
           IF      SPA-GMN-PAGE2       =   ZERO
               MOVE    1                   TO  IDX-ST
           ELSE
               COMPUTE IDX-ST          =   (SPA-GMN-PAGE2  -   1)
                                       *   300
                                       +   1
           END-IF
           PERFORM
               UNTIL   (FLG-HELP           =   1  )  OR
                       (IDX                >=  300)
               IF      HELP-MENUNUM2       =   ZERO
                   CONTINUE
               ELSE
                   ADD     1                   TO  IDY
                   IF      IDX-ST              <=  IDY
                       ADD     1                   TO  IDX
                       MOVE    HELP-MENUNUM2   TO  SPA-GMN-TMENUNUM2
                                                                  (IDX)
                       MOVE    HELP-MENU       TO  SPA-GMN-TMENU2 (IDX)
                   END-IF
                END-IF
      *
               MOVE    "tbl_help"          TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-HELP-READ-SEC
           END-PERFORM
           MOVE    "tbl_help"          TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    次頁判定
           IF     (IDX                 >=  300 )  AND
                  (FLG-HELP            =   ZERO)
               MOVE    1               TO  SPA-GMN-NEXT2
           ELSE
               MOVE    ZERO            TO  SPA-GMN-NEXT2
           END-IF
           MOVE    IDX                 TO  SPA-GMN-TBLMAX2
      *    クリア
           MOVE    ZERO                TO  SPA-GMN-SELNUM2
           MOVE    ZERO                TO  SPA-NAI-MENUNUM2
           MOVE    SPACE               TO  SPA-GMN-MENU2
           MOVE    ZERO                TO  SPA-NAI-MENU2FLG
           .
       30012-MENU2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（Ｎ４１ＩＤ１）ＯＫ処理
      *****************************************************************
       3002-N41ID1-SET-SEC          SECTION.
      *
           EVALUATE    SPA-N41IDCD
      *        削除確認
               WHEN    "0101"
               WHEN    "0102"
                   IF      SPA-N41ID1-FLG        =   "OK"
                       PERFORM 4031-DELETE-SYORI-SEC
                   ELSE
                       MOVE    3               TO  SPA-GMN-CUR
                   END-IF
                   MOVE    SPACE           TO  SPA-N41IDCD
      *        目次切替え
               WHEN    "0103"
                   IF      SPA-N41ID1-FLG        =   "OK"
                       MOVE    1               TO  SPA-SYORIFLG
                       MOVE    1               TO  SPA-GMN-PAGE
                       PERFORM 3001-MENU-HENSYU-SEC
                       MOVE    3               TO  SPA-GMN-CUR
                   END-IF
                   MOVE    SPACE           TO  SPA-N41IDCD
           END-EVALUATE
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
           END-IF
      *
           .
       3002-N41ID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                    PERFORM 402-CLREA-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                    PERFORM 403-DELETE-SEC
      *        目次１検索
               WHEN    "CLICKED"       ALSO    "B04"
                    PERFORM 404-KENSAKU-SEC
      *        前頁
               WHEN    "CLICKED"       ALSO    "B06"
                    PERFORM 406-PAGE-BACK-SEC
      *        次頁
               WHEN    "CLICKED"       ALSO    "B07"
                    PERFORM 407-PAGE-NEXT-SEC
      ****     移動
      *        WHEN    "CLICKED"       ALSO    "B08"
      ****          PERFORM 408-MOVE-SEC
      *        複写
               WHEN    "CLICKED"       ALSO    "B09"
                    PERFORM 409-COPY-SEC
      *        入力
               WHEN    "CLICKED"       ALSO    "B10"
                    PERFORM 420-HELP-IN-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                    PERFORM 412-TOROKU-SEC
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        区分入力
               WHEN    "ACTIVATE"      ALSO    "HELPKBN"
                   PERFORM 4001-HELPKBN-CHK-SEC
      *        業務入力
               WHEN    "ACTIVATE"      ALSO    "LDNAME"
                   PERFORM 4002-LDNAME-CHK-SEC
      *        目次１選択
               WHEN    ANY             ALSO    "HEADLIST"
                   PERFORM 4003-MENULIST-SEL-SEC
      *        目次２選択
               WHEN    ANY             ALSO    "MENU2LIST"
                   PERFORM 4004-MENU2LIST-SEL-SEC
      *        目次１文字検索入力
               WHEN    "ACTIVATE"      ALSO    "HEADMENU"
                   PERFORM 4005-HEADMENU-CHK-SEC
      *
               WHEN    OTHER
                   MOVE    ZERO            TO  FLG-KOUSIN
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    区分チェック処理
      *****************************************************************
       4001-HELPKBN-CHK-SEC          SECTION.
      *
           MOVE    N41-HELPKBN         TO  SPA-GMN-HELPKBN-G
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-HELPKBN-MAX )  OR
                              (FLG-OK  =   1   )
               IF      SPA-GMN-HELPKBN     =   SPA-GMN-HELPKBN-ITEM
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-HELPKBN-ITEM(IDX)
                                           TO  SPA-GMN-HELPKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               MOVE    SPACE               TO  SPA-GMN-HEADMENU
               MOVE    1                   TO  SPA-GMN-PAGE
               MOVE    1                   TO  SPA-SYORIFLG
               MOVE    ZERO                TO  SPA-SYORIFLG2
               PERFORM 3001-MENU-HENSYU-SEC
           END-IF
      *
           .
       4001-HELPKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務チェック処理
      *****************************************************************
       4002-LDNAME-CHK-SEC          SECTION.
      *
           MOVE    N41-LDNAME          TO  SPA-GMN-LDNAME
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-LDNAME-MAX )  OR
                              (FLG-OK  =   1   )
               IF      SPA-GMN-LDNAME     =   SPA-GMN-LDNAME-ITEM
                                                          (IDX)
                   MOVE    SPA-GMN-LDNAME-ITEM(IDX)
                                           TO  SPA-GMN-LDNAME
                   MOVE    SPA-GMN-LDNAME-LDC (IDX)
                                           TO  SPA-GMN-LDNAME-LD
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-PAGE
               MOVE    1                   TO  SPA-SYORIFLG
               MOVE    SPACE               TO  SPA-GMN-HEADMENU
               PERFORM 3001-MENU-HENSYU-SEC
           END-IF
      *
           .
       4002-LDNAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    目次１選択処理
      *****************************************************************
       4003-MENULIST-SEL-SEC          SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TBLMAX
               IF      N41-HEADLIST-SELECT (IDX)   =   "T"
                   MOVE    SPA-GMN-TMENUNUM (IDX)  TO  WRK-SELNUM
               END-IF
           END-PERFORM
      *
           IF     (WRK-SELNUM          >   ZERO ) AND
                  (WRK-SELNUM      NOT =   SPA-GMN-SELNUM)
               MOVE    WRK-SELNUM      TO  SPA-GMN-SELNUM
               PERFORM 4101-SELNUM-CHK-SEC
               IF      SPA-GMN-SELNUM  =   ZERO
                   MOVE    3               TO  SPA-GMN-CUR
               ELSE
                   MOVE    4               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       4003-MENULIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    目次２選択処理
      *****************************************************************
       4004-MENU2LIST-SEL-SEC          SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TBLMAX2
               IF      N41-MENU2LIST-SELECT (IDX)  =   "T"
                   MOVE    SPA-GMN-TMENUNUM2 (IDX) TO  WRK-SELNUM
               END-IF
           END-PERFORM
      *
           IF     (WRK-SELNUM          >   ZERO ) AND
                  (WRK-SELNUM      NOT =   SPA-GMN-SELNUM2)
               MOVE    WRK-SELNUM      TO  SPA-GMN-SELNUM2
               PERFORM 4101-SELNUM2-CHK-SEC
               IF      SPA-GMN-SELNUM2     =   ZERO
                   MOVE    5               TO  SPA-GMN-CUR
               ELSE
                   MOVE    6               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4004-MENU2LIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    目次１文字検索入力処理
      *****************************************************************
       4005-HEADMENU-CHK-SEC          SECTION.
      *
           MOVE    N41-HEADMENU        TO  SPA-GMN-HEADMENU
      *
           IF      SPA-GMN-HEADMENU    =   SPACE
               CONTINUE
           ELSE
               PERFORM 4005-HEADMENU-KENSAKU-SEC
           END-IF
           .
      *
       4005-HEADMENU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    目次１文字検索入力処理
      *****************************************************************
       4005-HEADMENU-KENSAKU-SEC          SECTION.
      *
      *    部分参照とする
           MOVE    ZERO                TO  IDX-K
           PERFORM VARYING     IDX     FROM    30  BY  -1
                   UNTIL       IDX     <       1
               IF      SPA-GMN-HEADMENU(IDX:1) NOT =   SPACE
                   MOVE    IDX                 TO  IDX-K
                   MOVE    1                   TO  IDX
               END-IF
           END-PERFORM
           MOVE    SPACE               TO  WRK-INPUT-MENU
           IF      IDX-K               =   30
               MOVE    SPA-GMN-HEADMENU        TO  WRK-INPUT-MENU
           ELSE
               IF      IDX-K           =   29
                   MOVE    SPA-GMN-HEADMENU    TO  WRK-INPUT-MENU
                   MOVE    "%"                 TO  WRK-INPUT-MENU
                                                             (30:1)
               ELSE
                   COMPUTE IDX-K2              =   IDX-K   +  2
                   MOVE    "%"                 TO  WRK-INPUT-MENU
                                                             (1:1)
                   MOVE    SPA-GMN-HEADMENU    TO  WRK-INPUT-MENU
                                                             (2:)
                   MOVE    "%"                 TO  WRK-INPUT-MENU
                                                             (IDX-K2:)
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  HELP-REC
           INITIALIZE                  HELP-REC
           MOVE    SPA-GMN-HELPKBN     TO  HELP-HELPKBN
           MOVE    SPACE               TO  HELP-KEY-OPID
           MOVE    SPA-GMN-LDNAME-LD   TO  HELP-LDNAME
           MOVE    SPA-NAI-MENUNUM1    TO  HELP-MENUNUM1
           MOVE    WRK-INPUT-MENU      TO  HELP-MENU
      *
           MOVE    SPA-HOSPNUM         TO  HELP-HOSPNUM
           MOVE    HELP-REC            TO  MCPDATA-REC
           MOVE    "tbl_help"          TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_help"          TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 900-HELP-READ-SEC
           ELSE
               INITIALIZE                  HELP-REC
               MOVE    1               TO  FLG-HELP
           END-IF
      *
           INITIALIZE                  SPA-GMN-MENUTBL
           MOVE    1                   TO  FLG-GMN-INIT
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDX-ST
           IF      SPA-GMN-PAGE        =   ZERO
               MOVE    1                   TO  IDX-ST
           ELSE
               COMPUTE IDX-ST          =   (SPA-GMN-PAGE   -   1)
                                       *   300
                                       +   1
           END-IF
           PERFORM
               UNTIL   (FLG-HELP           =   1  )  OR
                       (IDX                >=  300)
               ADD     1                   TO  IDY
               IF      IDX-ST              <=  IDY
                   ADD     1                   TO  IDX
                   MOVE    HELP-MENUNUM1   TO  SPA-GMN-TMENUNUM (IDX)
                   MOVE    HELP-MENU2FLG   TO  SPA-GMN-TMENU2FLG(IDX)
                   MOVE    HELP-MENU       TO  SPA-GMN-TMENU    (IDX)
               END-IF
      *
               MOVE    "tbl_help"          TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 900-HELP-READ-SEC
           END-PERFORM
           MOVE    "tbl_help"          TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    次頁判定
           IF     (IDX                 >=  300 )  AND
                  (FLG-HELP            =   ZERO)
               MOVE    1               TO  SPA-GMN-NEXT
           ELSE
               MOVE    ZERO            TO  SPA-GMN-NEXT
           END-IF
           MOVE    IDX                 TO  SPA-GMN-TBLMAX
      *    クリア
           INITIALIZE                  SPA-GMN-MENU2TBL
           MOVE    ZERO                TO  SPA-GMN-NEXT2
           MOVE    ZERO                TO  SPA-GMN-PAGE2
           MOVE    ZERO                TO  SPA-NAI-MENU1FLG
           MOVE    ZERO                TO  SPA-NAI-MENU2FLG
           MOVE    ZERO                TO  SPA-GMN-SELNUM
           MOVE    ZERO                TO  SPA-GMN-SELNUM2
           MOVE    SPACE               TO  SPA-GMN-MENU
           MOVE    SPACE               TO  SPA-GMN-MENU2
           MOVE    SPACE               TO  SPA-GMN-HELP
           INITIALIZE                  SPA-GMN-COPY-AREA
           INITIALIZE                  SPA-NAI-COPY-AREA
           MOVE    ZERO                TO  SPA-COPYFLG
           MOVE    ZERO                TO  SPA-SYORIFLG2
           MOVE    ZERO                TO  SPA-NAI-MENUNUM1
           MOVE    ZERO                TO  SPA-NAI-MENUNUM2
      *
           IF      SPA-GMN-TBLMAX      >   ZERO
               MOVE    3                   TO  SPA-GMN-CUR
           ELSE
               MOVE    "0026"              TO  SPA-ERRCD
               MOVE    8                   TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       4005-HEADMENU-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    N41-SELNUM          TO  SPA-GMN-SELNUM
           MOVE    N41-MENU            TO  SPA-GMN-MENU
           MOVE    N41-SELNUM2         TO  SPA-GMN-SELNUM2
           MOVE    N41-MENU2           TO  SPA-GMN-MENU2
           MOVE    N41-HELP            TO  SPA-GMN-HELP
      *
           MOVE    N41-HELPKBN-CP      TO  SPA-GMN-HELPKBN-CP-G
           MOVE    N41-LDNAME-CP       TO  SPA-GMN-LDNAME-CP
           MOVE    N41-MENUNUM1-CP     TO  SPA-GMN-MENUNUM1-CP
           MOVE    N41-MENU1-CP        TO  SPA-GMN-MENU1-CP
           MOVE    N41-MENUNUM2-CP     TO  SPA-GMN-MENUNUM2-CP
           MOVE    N41-MENU2-CP        TO  SPA-GMN-MENU2-CP
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    選択番号
           PERFORM 4101-SELNUM-CHK-SEC
      *    目次１
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4102-MENU-CHK-SEC
           END-IF
      *    選択番号２
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4101-SELNUM2-CHK-SEC
           END-IF
      *    目次２
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4102-MENU2-CHK-SEC
           END-IF
      *    内容
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4103-HELP-CHK-SEC
           END-IF
      *    複写先
           IF      SPA-COPYFLG     NOT =   ZERO
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 4104-HELPKBN-CP-CHK-SEC
               END-IF
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 4105-LDNAME-CP-CHK-SEC
               END-IF
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 4106-MENUNUM1-CP-CHK-SEC
               END-IF
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 4107-MENU1-CP-CHK-SEC
               END-IF
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 4108-MENUNUM2-CP-CHK-SEC
               END-IF
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 4109-MENU2-CP-CHK-SEC
               END-IF
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4101-SELNUM-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  IDX-TBL
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   SPA-GMN-TBLMAX) OR
                              (FLG-OK          =   1     )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-SELNUM      =   SPA-GMN-TMENUNUM(IDX)
                   MOVE    1                   TO  FLG-OK
                   MOVE    IDX                 TO  IDX-TBL
               END-IF
           END-PERFORM
      *
           IF      SPA-GMN-SELNUM      =   SPA-NAI-MENUNUM1
               CONTINUE
           ELSE
               IF      FLG-OK              =   ZERO
                   MOVE    SPA-GMN-SELNUM      TO  WRK-MENUNUM1
                   MOVE    ZERO                TO  WRK-MENUNUM2
      *            登録済みかの判定
                   PERFORM 41011-HELP-HENSYU-SEC
                   IF      FLG-HELP-ARI        =   1
                       MOVE    SPACE           TO  SPA-GMN-HELP
                       MOVE    "0020"          TO  SPA-ERRCD
                       MOVE    3               TO  SPA-GMN-CUR
                   ELSE
      *                新規
                       MOVE    SPA-GMN-SELNUM  TO  SPA-NAI-MENUNUM1
                       MOVE    1               TO  SPA-NAI-MENU1FLG
                   END-IF
               ELSE
      *        対象あり
                   MOVE    ZERO                TO  SPA-NAI-MENU1FLG
                   MOVE    SPA-GMN-SELNUM      TO  SPA-NAI-MENUNUM1
                   MOVE    SPA-GMN-TMENU(IDX-TBL)
                                               TO  SPA-GMN-MENU
                   MOVE    ZERO                TO  SPA-NAI-MENUNUM2
                   MOVE    SPA-GMN-SELNUM      TO  WRK-MENUNUM1
                   MOVE    ZERO                TO  WRK-MENUNUM2
      *            ヘルプ内容編集
                   PERFORM 41011-HELP-HENSYU-SEC
                   MOVE    1                   TO  SPA-GMN-PAGE2
                   MOVE    2                   TO  SPA-SYORIFLG
      *            目次２編集
                   PERFORM 3001-MENU-HENSYU-SEC
                   MOVE    WRK-HELP-AREA       TO  SPA-GMN-HELP
               END-IF
           END-IF
           .
       4101-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ヘルプ内容編集処理
      *****************************************************************
       41011-HELP-HENSYU-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-HELP-AREA
           MOVE    SPACE               TO  SPA-NAI-HELP-G
           MOVE    ZERO                TO  IDX-H
      *
           MOVE    SPACE               TO  HELP-REC
           INITIALIZE                  HELP-REC
           MOVE    SPA-GMN-HELPKBN     TO  HELP-HELPKBN
           MOVE    SPACE               TO  HELP-KEY-OPID
           MOVE    SPA-GMN-LDNAME-LD   TO  HELP-LDNAME
           MOVE    WRK-MENUNUM1        TO  HELP-MENUNUM1
           MOVE    WRK-MENUNUM2        TO  HELP-MENUNUM2
      *
           MOVE    SPA-HOSPNUM         TO  HELP-HOSPNUM
           MOVE    HELP-REC            TO  MCPDATA-REC
           MOVE    "tbl_help"          TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-HELP-READ-SEC
           ELSE
               INITIALIZE                  HELP-REC
               MOVE    1               TO  FLG-HELP
           END-IF
           MOVE    ZERO                TO  FLG-HELP-ARI
           PERFORM UNTIL       (FLG-HELP   =   1  )  OR
                               (IDX-H      >=  40 )
               IF      HELP-EDANUM         >   ZERO
                   ADD     1                   TO  IDX-H
                   MOVE    HELP-HELP           TO  WRK-HELP (IDX-H)
                   MOVE    HELP-HELP           TO  SPA-NAI-HELP (IDX-H)
               END-IF
               MOVE    1                   TO  FLG-HELP-ARI
      *
               MOVE    "tbl_help"          TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-HELP-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_help"          TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    改行コードでテーブル編集
           MOVE    SPACE               TO  WRK-HELP-AREA
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  IDX-H2
           PERFORM VARYING     IDX-H   FROM   1   BY  1
                    UNTIL      IDX-H       >  40
               PERFORM VARYING     IDX-H1  FROM   1   BY  1
                        UNTIL      IDX-H1      >  50
                   ADD     1                   TO  IDX-H2
                   MOVE    SPA-NAI-HELP(IDX-H)(IDX-H1:1)
                                            TO  WRK-HELP-AREA(IDX-H2:1)
                   IF      (SPA-NAI-HELP(IDX-H)(IDX-H1:1)  =   X"0A")
                       AND (IDX-H1             <   50   )
                        COMPUTE IDX-H3         =   IDX-H1  +  1
                        IF    SPA-NAI-HELP(IDX-H)(IDX-H3:) =  SPACE
                            MOVE   50                TO  IDX-H1
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
           MOVE    WRK-HELP-AREA       TO  SPA-GMN-HELP
      *
           .
       41011-HELP-HENSYU-EXT.
      *
      *****************************************************************
      *    選択番号２チェック処理
      *****************************************************************
       4101-SELNUM2-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  IDX-TBL
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   SPA-GMN-TBLMAX2) OR
                              (FLG-OK          =   1     )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-SELNUM2     =   SPA-GMN-TMENUNUM2(IDX)
                   MOVE    1                   TO  FLG-OK
                   MOVE    IDX                 TO  IDX-TBL
               END-IF
           END-PERFORM
      *
      *    見出２
           IF      SPA-GMN-SELNUM2     =   SPA-NAI-MENUNUM2
               CONTINUE
           ELSE
      *        複写中はエラー
               IF      SPA-COPYFLG         NOT =   ZERO
                   MOVE    "0018"              TO  SPA-ERRCD
                   MOVE    11                  TO  SPA-GMN-CUR
                   MOVE    SPA-NAI-MENUNUM2    TO  SPA-GMN-SELNUM2
               ELSE
      *            ヘルプ内容編集
                   MOVE    SPA-NAI-MENUNUM1    TO  WRK-MENUNUM1
                   MOVE    SPA-GMN-SELNUM2     TO  WRK-MENUNUM2
                   PERFORM 41011-HELP-HENSYU-SEC
                   IF      FLG-OK              =   1
                       MOVE    SPA-GMN-TMENU2(IDX-TBL)
                                                   TO  SPA-GMN-MENU2
                       MOVE    WRK-HELP-AREA       TO  SPA-GMN-HELP
                       MOVE    ZERO                TO  SPA-NAI-MENU2FLG
                   ELSE
                       MOVE    SPACE               TO  SPA-GMN-HELP
                       MOVE    1                   TO  SPA-NAI-MENU2FLG
                       IF      FLG-HELP-ARI        =   1
                           MOVE    SPACE           TO  SPA-GMN-HELP
                           MOVE    "0020"          TO  SPA-ERRCD
                           MOVE    5               TO  SPA-GMN-CUR
                           MOVE    ZERO            TO  SPA-NAI-MENU2FLG
                       END-IF
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    6                   TO  SPA-GMN-CUR
                       MOVE    SPA-GMN-SELNUM2     TO  SPA-NAI-MENUNUM2
                   END-IF
               END-IF
           END-IF
           .
       4101-SELNUM2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ヘルプマスタ存在チェック処理
      *****************************************************************
       410112-HELP-KENSAKU-SEC      SECTION.
      *
           MOVE    SPACE               TO  HELP-REC
           INITIALIZE                  HELP-REC
           MOVE    WRK-HELPKBN         TO  HELP-HELPKBN
           MOVE    WRK-KEY-OPID        TO  HELP-KEY-OPID
           MOVE    WRK-LDNAME-LD       TO  HELP-LDNAME
           MOVE    WRK-MENUNUM1        TO  HELP-MENUNUM1
           MOVE    WRK-MENUNUM2        TO  HELP-MENUNUM2
           MOVE    WRK-EDANUM          TO  HELP-EDANUM
      *
           MOVE    SPA-HOSPNUM         TO  HELP-HOSPNUM
           MOVE    HELP-REC            TO  MCPDATA-REC
           MOVE    "tbl_help"          TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-HELP-READ-SEC
           ELSE
               INITIALIZE                  HELP-REC
               MOVE    1               TO  FLG-HELP
           END-IF
           IF      FLG-HELP            =   ZERO
               MOVE    1               TO  FLG-ARI
           ELSE
               MOVE    ZERO            TO  FLG-ARI
           END-IF
      *
           MOVE    "tbl_help"          TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       410112-HELP-KENSAKU-EXT.
           EXIT.
      *****************************************************************
      *    目次チェック処理
      *****************************************************************
       4102-MENU-CHK-SEC              SECTION.
      *
           IF      SPA-GMN-MENU        =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-MENU        TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC      NOT =   ZERO  )  OR
                      (KANACHK-SYUBETU     =   1     )
                   IF      KANACHK-RC2         =   7
                       MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MENU
                   END-IF
                   MOVE    "0004"          TO  SPA-ERRCD
                   MOVE    4               TO  SPA-GMN-CUR
               ELSE
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MENU
               END-IF
           END-IF
      *
           .
       4102-MENU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    目次２チェック処理
      *****************************************************************
       4102-MENU2-CHK-SEC              SECTION.
      *
           IF      SPA-GMN-MENU2       =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-MENU2       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC      NOT =   ZERO  )  OR
                      (KANACHK-SYUBETU     =   1     )
                   IF      KANACHK-RC2         =   7
                       MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MENU2
                   END-IF
                   MOVE    "0019"          TO  SPA-ERRCD
                   MOVE    6               TO  SPA-GMN-CUR
               ELSE
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MENU2
               END-IF
           END-IF
      *
           .
       4102-MENU2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    内容チェック処理
      *****************************************************************
       4103-HELP-CHK-SEC              SECTION.
      *
           IF      SPA-GMN-HELP        =   SPACE
               MOVE    SPACE               TO  SPA-NAI-HELP-G
           ELSE
      *        ２５文字ずつのチェック
               MOVE    SPACE               TO  WRK-HELP-AREA
               MOVE    ZERO                TO  FLG-CHK
               MOVE    ZERO                TO  IDX-H
               MOVE    1                   TO  IDX-H2
               PERFORM UNTIL  (FLG-CHK     =   1   )  OR
                              (IDX-H       >=  40  )
                   ADD     1                   TO  IDX-H
                   MOVE    SPA-GMN-HELP(IDX-H2:50)
                                               TO  WRK-HELP (IDX-H)
                   PERFORM 41031-HALF-MOJI-SEC
                   IF      KANACHK-RC2         =   7
                       ADD     49              TO  IDX-H2
                   ELSE
                       ADD     50              TO  IDX-H2
                   END-IF
                   IF     (IDX-H2              >   2000)  OR
                          (SPA-GMN-HELP(IDX-H2:)   =   SPACE)
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-PERFORM
      *        改行コードでテーブル編集
               MOVE    SPACE               TO  WRK-HELP-AREA
               MOVE    ZERO                TO  FLG-CHK
               MOVE    ZERO                TO  IDX-H
               MOVE    1                   TO  IDX-H1
               MOVE    1                   TO  IDX-H2
               MOVE    ZERO                TO  IDX-H3
               PERFORM UNTIL  (FLG-CHK     =   1   )  OR
                              (IDX-H       >=  40  )
                   ADD     1                   TO  IDX-H3
                   IF     (IDX-H3              =   50  )  OR
                          (SPA-GMN-HELP(IDX-H1:1)  =   X"0A")
                       ADD     1                   TO  IDX-H
                       MOVE    SPA-GMN-HELP(IDX-H2:IDX-H3)
                                               TO  WRK-HELP (IDX-H)
                       IF      SPA-GMN-HELP(IDX-H1:1)  =   X"0A"
                           CONTINUE
                       ELSE
                           PERFORM 41031-HALF-MOJI-SEC
                           IF      KANACHK-RC2         =   7
                               COMPUTE IDX-H3          =   IDX-H3  -  1
                           END-IF
                       END-IF
                       COMPUTE IDX-H2              =   IDX-H2
                                                   +   IDX-H3
                       MOVE    IDX-H2              TO  IDX-H1
                       COMPUTE IDX-H1              =   IDX-H1  -  1
                       MOVE    ZERO                TO  IDX-H3
                   END-IF
                   ADD     1                   TO  IDX-H1
      *
                   IF     (IDX-H1              >   2000)  OR
                          (SPA-GMN-HELP(IDX-H1:)   =   SPACE)
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      IDX-H3                  >   ZERO
                   ADD     1                   TO  IDX-H
                   MOVE    SPA-GMN-HELP(IDX-H2:IDX-H3)
                                               TO  WRK-HELP (IDX-H)
               END-IF
               MOVE    WRK-HELP-AREA       TO  SPA-NAI-HELP-G
      *        改行コードでテーブル編集
               MOVE    SPACE               TO  WRK-HELP-AREA
               MOVE    ZERO                TO  FLG-CHK
               MOVE    ZERO                TO  IDX-H2
               PERFORM VARYING     IDX-H   FROM   1   BY  1
                        UNTIL      IDX-H       >  40
                   PERFORM VARYING     IDX-H1  FROM   1   BY  1
                            UNTIL      IDX-H1      >  50
                       ADD     1                   TO  IDX-H2
                       MOVE    SPA-NAI-HELP(IDX-H)(IDX-H1:1)
                                           TO  WRK-HELP-AREA(IDX-H2:1)
                       IF   SPA-NAI-HELP(IDX-H)(IDX-H1:1)   =   X"0A"
                            MOVE   50               TO  IDX-H1
                       END-IF
                   END-PERFORM
               END-PERFORM
               MOVE    WRK-HELP-AREA   TO  SPA-GMN-HELP
           END-IF
      *
           .
       4103-HELP-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角チェック処理
      *****************************************************************
       41031-HALF-MOJI-SEC              SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-HELP(IDX-H)     TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC2         =   7
               MOVE    KANACHK-OUT-INPUT   TO  WRK-HELP(IDX-H)
           END-IF
           .
       41031-HALF-MOJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    複写先区分チェック処理
      *****************************************************************
       4104-HELPKBN-CP-CHK-SEC              SECTION.
      *
      *    標準は対象外
           MOVE    2                   TO  IDY
      *??
      **** MOVE    1                   TO  IDY
      *??
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    IDY   BY  1
                   UNTIL      (IDX     >   SPA-HELPKBN-MAX )  OR
                              (FLG-OK  =   1   )
               IF      SPA-GMN-HELPKBN-CP  =   SPA-GMN-HELPKBN-ITEM
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-HELPKBN-ITEM(IDX)
                                           TO  SPA-GMN-HELPKBN-CP-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
               MOVE    10                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       4104-HELPKBN-CP-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    複写先業務ＬＤチェック処理
      *****************************************************************
       4105-LDNAME-CP-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-LDNAME-MAX )  OR
                              (FLG-OK  =   1   )
               IF      SPA-GMN-LDNAME-CP  =   SPA-GMN-LDNAME-ITEM
                                                          (IDX)
                   MOVE    SPA-GMN-LDNAME-ITEM(IDX)
                                           TO  SPA-GMN-LDNAME-CP
                   MOVE    SPA-GMN-LDNAME-LDC (IDX)
                                           TO  SPA-GMN-LDNAME-LD-CP
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    11                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       4105-LDNAME-CP-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    複写先目次１チェック処理
      *****************************************************************
       4106-MENUNUM1-CP-CHK-SEC              SECTION.
      *
           IF     (SPA-GMN-MENUNUM1-CP =   SPA-NAI-MENUNUM1-CP )  OR
                  (SPA-GMN-MENUNUM1-CP =   ZERO       )
               CONTINUE
           ELSE
              MOVE    SPA-NAI-MENUNUM1     TO  WRK-MENUNUM1
              MOVE    SPA-GMN-MENUNUM1-CP  TO  SPA-NAI-MENUNUM1-CP
      *
              INITIALIZE                   WRK-KEY-AREA
              MOVE    SPA-GMN-HELPKBN-CP   TO  WRK-HELPKBN
              MOVE    SPA-GMN-LDNAME-LD-CP TO  WRK-LDNAME-LD
              MOVE    SPA-GMN-MENUNUM1-CP  TO  WRK-MENUNUM1
              MOVE    ZERO                 TO  WRK-MENUNUM2
              MOVE    ZERO                 TO  WRK-EDANUM
              PERFORM 410112-HELP-KENSAKU-SEC
              IF      FLG-ARI              =   1
                  MOVE    HELP-MENU            TO  SPA-GMN-MENU1-CP
                  MOVE    HELP-MENU            TO  SPA-NAI-MENU1-CP
                  MOVE    13                   TO  SPA-GMN-CUR
              ELSE
                  MOVE    SPACE                TO  SPA-GMN-MENU1-CP
                  MOVE    SPACE                TO  SPA-NAI-MENU1-CP
                  MOVE    12                   TO  SPA-GMN-CUR
              END-IF
           END-IF
      *
           .
       4106-MENUNUM1-CP-CHK-EXT.
           EXIT.
      *****************************************************************
      *    複写先目次名１チェック処理
      *****************************************************************
       4107-MENU1-CP-CHK-SEC              SECTION.
      *
      *    登録済みの時、変更不可
           IF      SPA-NAI-MENU1-CP    NOT =   SPACE
               MOVE    SPA-NAI-MENU1-CP    TO  SPA-GMN-MENU1-CP
           END-IF
      *
           IF      SPA-GMN-MENU1-CP    =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-MENU1-CP    TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC      NOT =   ZERO  )  OR
                      (KANACHK-SYUBETU     =   1     )
                   IF      KANACHK-RC2         =   7
                       MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MENU1-CP
                   END-IF
                   MOVE    "0008"          TO  SPA-ERRCD
                   MOVE    13              TO  SPA-GMN-CUR
               ELSE
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MENU1-CP
               END-IF
           END-IF
      *
           .
       4107-MENU1-CP-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    複写先目次２チェック処理
      *****************************************************************
       4108-MENUNUM2-CP-CHK-SEC              SECTION.
      *
           IF     (SPA-GMN-MENUNUM2-CP =   SPA-NAI-MENUNUM2-CP )  OR
                  (SPA-GMN-MENUNUM2-CP =   ZERO                )
               CONTINUE
           ELSE
              MOVE    SPA-GMN-MENUNUM2-CP  TO  SPA-NAI-MENUNUM2-CP
      *
              INITIALIZE                   WRK-KEY-AREA
              MOVE    SPA-GMN-HELPKBN-CP   TO  WRK-HELPKBN
              MOVE    SPA-GMN-LDNAME-LD-CP TO  WRK-LDNAME-LD
              MOVE    SPA-GMN-MENUNUM1-CP  TO  WRK-MENUNUM1
              MOVE    SPA-GMN-MENUNUM2-CP  TO  WRK-MENUNUM2
              MOVE    1                    TO  WRK-EDANUM
              PERFORM 410112-HELP-KENSAKU-SEC
              IF      FLG-ARI              =   1
                  MOVE    "0009"               TO  SPA-ERRCD
                  MOVE    14                   TO  SPA-GMN-CUR
              ELSE
                  MOVE    15                   TO  SPA-GMN-CUR
              END-IF
          END-IF
      *
           .
       4108-MENUNUM2-CP-CHK-EXT.
           EXIT.
      *****************************************************************
      *    複写先目次名２チェック処理
      *****************************************************************
       4109-MENU2-CP-CHK-SEC              SECTION.
      *
           IF      SPA-GMN-MENU2-CP    =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-MENU2-CP    TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC      NOT =   ZERO  )  OR
                      (KANACHK-SYUBETU     =   1     )
                   IF      KANACHK-RC2         =   7
                       MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MENU2-CP
                   END-IF
                   MOVE    "0008"          TO  SPA-ERRCD
                   MOVE    15              TO  SPA-GMN-CUR
               ELSE
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MENU2-CP
               END-IF
           END-IF
      *
           .
       4109-MENU2-CP-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "M02"               TO  SPA-SAKIPG
           MOVE    "N41"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       402-CLREA-SEC       SECTION.
      *
           IF      SPA-COPYFLG     NOT =   ZERO
               INITIALIZE                  SPA-GMN-COPY-AREA
               INITIALIZE                  SPA-NAI-COPY-AREA
               MOVE    ZERO                TO  SPA-COPYFLG
           END-IF
      *
           MOVE    ZERO                TO  SPA-SYORIFLG2
           MOVE    ZERO                TO  SPA-GMN-SELNUM
           MOVE    ZERO                TO  SPA-GMN-SELNUM2
           MOVE    SPACE               TO  SPA-GMN-MENU
           MOVE    SPACE               TO  SPA-GMN-MENU2
           MOVE    SPACE               TO  SPA-GMN-HELP
           MOVE    ZERO                TO  SPA-NAI-MENUNUM1
           MOVE    ZERO                TO  SPA-NAI-MENUNUM2
           INITIALIZE                  SPA-GMN-MENU2TBL
           MOVE    ZERO                TO  SPA-GMN-NEXT2
           MOVE    ZERO                TO  SPA-GMN-PAGE2
      *
           IF      SPA-GMN-HEADMENU    NOT =   SPACE
               MOVE    SPACE           TO  SPA-GMN-HEADMENU
               MOVE    1               TO  SPA-SYORIFLG
               PERFORM 3001-MENU-HENSYU-SEC
           END-IF
      *
           MOVE    3               TO  SPA-GMN-CUR
      *
           .
       402-CLREA-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       403-DELETE-SEC       SECTION.
      *
           IF     (SPA-NAI-MENU1FLG    =   1  )  OR
                  (SPA-NAI-MENU2FLG    =   1  )
               MOVE    "0030"          TO  SPA-ERRCD
           END-IF
           IF      SPA-COPYFLG         =   1
               MOVE    "0032"          TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-MENUNUM1    =   ZERO
                   MOVE    "0031"          TO  SPA-ERRCD
               ELSE
                   IF      SPA-NAI-MENUNUM2    =   ZERO
                       MOVE    "0101"          TO  SPA-N41IDCD
                   ELSE
                       MOVE    "0102"          TO  SPA-N41IDCD
                   END-IF
               END-IF
           END-IF
      *
           .
       403-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DELETE-SYORI-SEC       SECTION.
      *
           MOVE    SPACE               TO  HELP-REC
           INITIALIZE                  HELP-REC
           MOVE    SPA-GMN-HELPKBN     TO  HELP-HELPKBN
           MOVE    SPACE               TO  HELP-KEY-OPID
           MOVE    SPA-GMN-LDNAME-LD   TO  HELP-LDNAME
           MOVE    SPA-NAI-MENUNUM1    TO  HELP-MENUNUM1
           MOVE    SPA-NAI-MENUNUM2    TO  HELP-MENUNUM2
      *
           MOVE    SPA-HOSPNUM         TO  HELP-HOSPNUM
           MOVE    HELP-REC            TO  MCPDATA-REC
           IF      SPA-NAI-MENUNUM2    =   ZERO
               MOVE    "tbl_help"          TO  MCP-TABLE
               MOVE    "del2"              TO  MCP-PATHNAME
           ELSE
               MOVE    "tbl_help"          TO  MCP-TABLE
               MOVE    "del1"              TO  MCP-PATHNAME
           END-IF
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1001"          TO  SPA-ERRCD
               DISPLAY "N41 HELP DEL ERR:" MCP-RC
                       ",KEY:" HELP-KEY
           END-IF
      *
           IF      SPA-NAI-MENUNUM2    =   ZERO
               MOVE    1               TO  SPA-SYORIFLG
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           PERFORM 3001-MENU-HENSYU-SEC
           MOVE    3                   TO  SPA-GMN-CUR
      *
           IF      SPA-SYORIFLG        =   2
      *        目次１変更
               IF      SPA-GMN-TBLMAX2     =   ZERO
                   PERFORM 450111-HELP-MENU1-SEC
               END-IF
           END-IF
           .
       4031-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    目次１検索処理
      *****************************************************************
       404-KENSAKU-SEC       SECTION.
      *
           MOVE    8                   TO  SPA-GMN-CUR
      *
           .
       404-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       406-PAGE-BACK-SEC       SECTION.
      *
           IF      SPA-SYORIFLG        =   1
               IF      SPA-GMN-PAGE        =   1
                   MOVE    "0023"          TO  SPA-ERRCD
               ELSE
                   COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE   -   1
                   PERFORM 3001-MENU-HENSYU-SEC
               END-IF
           ELSE
               IF      SPA-GMN-PAGE2       =   1
                   MOVE    "0023"          TO  SPA-ERRCD
               ELSE
                   COMPUTE SPA-GMN-PAGE2   =   SPA-GMN-PAGE2  -   1
                   PERFORM 3001-MENU-HENSYU-SEC
               END-IF
           END-IF
      *
           .
       406-PAGE-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁処理
      *****************************************************************
       407-PAGE-NEXT-SEC       SECTION.
      *
           IF      SPA-SYORIFLG        =   1
               IF      SPA-GMN-NEXT        =   ZERO
                   MOVE    "0024"          TO  SPA-ERRCD
               ELSE
                   ADD     1               TO  SPA-GMN-PAGE
                   PERFORM 3001-MENU-HENSYU-SEC
               END-IF
           ELSE
               IF      SPA-GMN-NEXT2       =   ZERO
                   MOVE    "0024"          TO  SPA-ERRCD
               ELSE
                   ADD     1               TO  SPA-GMN-PAGE2
                   PERFORM 3001-MENU-HENSYU-SEC
               END-IF
           END-IF
      *
           .
       407-PAGE-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    目次移動処理
      *****************************************************************
       408-MOVE-SEC       SECTION.
      *
      *
           .
       408-MOVE-EXT.
           EXIT.
      *
      *****************************************************************
      *    複写処理
      *****************************************************************
       409-COPY-SEC       SECTION.
      *
           IF      SPA-COPYFLG         =   1
               MOVE    ZERO                TO  SPA-COPYFLG
               INITIALIZE                  SPA-GMN-COPY-AREA
               INITIALIZE                  SPA-NAI-COPY-AREA
               MOVE    7                   TO  SPA-GMN-CUR
           ELSE
      *
           IF     (SPA-NAI-MENU1FLG    =   1  )  OR
                  (SPA-NAI-MENU2FLG    =   1  )
               MOVE    "0025"          TO  SPA-ERRCD
           ELSE
               IF      SPA-NAI-MENUNUM1    =   ZERO
                   MOVE    "0022"          TO  SPA-ERRCD
               END-IF
           END-IF
      *    初期表示
           IF      SPA-ERRCD           =   SPACE
               INITIALIZE                  SPA-GMN-COPY-AREA
               INITIALIZE                  SPA-NAI-COPY-AREA
      *        自院
               MOVE    SPA-GMN-HELPKBN-ITEM(2)
                                           TO  SPA-GMN-HELPKBN-CP-G
      *        業務は複写元と同じ
               MOVE    SPA-GMN-LDNAME      TO  SPA-GMN-LDNAME-CP
               MOVE    SPA-GMN-LDNAME-LD   TO  SPA-GMN-LDNAME-LD-CP
      *        目次名２
               IF      SPA-NAI-MENUNUM2    >   ZERO
                   MOVE    SPA-GMN-MENU2       TO  SPA-GMN-MENU2-CP
               END-IF
      *
               MOVE    1                   TO  SPA-COPYFLG
               MOVE    12                  TO  SPA-GMN-CUR
      *??
      ***      MOVE    10                  TO  SPA-GMN-CUR
      *??
           END-IF
      *
           END-IF
      *
           .
       409-COPY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力　処理
      *****************************************************************
       420-HELP-IN-SEC             SECTION.
      *    入力
           PERFORM 410-INPUT-CHK-SEC
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-GMN-HELP    NOT =   SPACE )
               MOVE    99                  TO  SPA-GMN-CUR
           END-IF
           IF      SPA-GMN-HELPKBN     =   "0"
               MOVE    3               TO  SPA-GMN-CUR
           END-IF
      *
           .
       420-HELP-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       412-TOROKU-SEC             SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
      *    更新前チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 420-KOUSIN-CHK-SEC
           END-IF
      *
      *    更新処理
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4501-KOUSIN-SYORI-SEC
           END-IF
      *
           .
       412-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新前チェック処理
      *****************************************************************
       420-KOUSIN-CHK-SEC              SECTION.
      *
           IF      SPA-GMN-HELPKBN     =   SPACE
               MOVE    "0010"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           IF      SPA-GMN-LDNAME      =   SPACE
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "0011"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF      SPA-NAI-MENUNUM1    =   ZERO
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "0012"              TO  SPA-ERRCD
                   MOVE    3                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *    目次１の追加
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-NAI-MENUNUM1    NOT =   ZERO) AND
                  (SPA-COPYFLG         =   ZERO  )
               IF      SPA-GMN-MENU        =   SPACE
                   MOVE    "0013"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF     (SPA-NAI-MENUNUM1    =   ZERO )  AND
                  (SPA-NAI-MENUNUM2    NOT =   ZERO)
              IF      SPA-ERRCD           =   SPACE
                   MOVE    "0112"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *    目次２の追加
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-NAI-MENUNUM1    NOT =   ZERO) AND
                  (SPA-NAI-MENUNUM2    NOT =   ZERO) AND
                  (SPA-COPYFLG         =   ZERO  )
               IF      SPA-GMN-MENU2       =   SPACE
                   MOVE    "0113"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN-CUR
               END-IF
               IF      SPA-GMN-HELP        =   SPACE
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "0014"              TO  SPA-ERRCD
                       MOVE    6                   TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *    複写
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-COPYFLG     NOT =   ZERO  )
               IF    ((SPA-GMN-HELPKBN-CP  =   SPACE )  OR
                      (SPA-GMN-LDNAME-CP   =   SPACE )  OR
                      (SPA-GMN-MENUNUM1-CP =   ZERO  )  OR
                      (SPA-GMN-MENU1-CP    =   SPACE ))    OR
                     ((SPA-NAI-MENUNUM2    >   ZERO  )  AND
                     ((SPA-GMN-MENUNUM2-CP =   ZERO  )  OR
                      (SPA-GMN-MENU2-CP    =   SPACE )))
                   MOVE    "0015"              TO  SPA-ERRCD
                   MOVE    10                  TO  SPA-GMN-CUR
               END-IF
               IF     (SPA-GMN-HELPKBN-CP  =   SPA-GMN-HELPKBN) AND
                      (SPA-GMN-LDNAME-CP   =   SPA-GMN-LDNAME ) AND
                      (SPA-GMN-MENUNUM1-CP =   SPA-NAI-MENUNUM1)
                  IF     ((SPA-NAI-MENUNUM2    >   ZERO  )  AND
                          (SPA-GMN-MENUNUM2-CP =   SPA-NAI-MENUNUM2))
                      OR  (SPA-NAI-MENUNUM2    =   ZERO  )
                       MOVE    "0016"              TO  SPA-ERRCD
                       MOVE    10                  TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
           .
      *
       420-KOUSIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       4501-KOUSIN-SYORI-SEC             SECTION.
      *
      *    更新日取得
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           EVALUATE    SPA-COPYFLG
               WHEN    ZERO
      *            目次１登録
                   PERFORM 45011-MENU1-KOUSIN-SEC
      *            目次２登録
                   IF      SPA-NAI-MENUNUM2    >   ZERO
                       PERFORM 45012-MENU2-KOUSIN-SEC
                   END-IF
               WHEN    1
      *            複写
                   PERFORM 45013-COPY-SYORI-SEC
               WHEN    2
      *            移動
                   PERFORM 45014-MOVE-SYORI-SEC
           END-EVALUATE
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    ZERO                TO  SPA-COPYFLG
               MOVE    SPACE               TO  SPA-GMN-HELP
               INITIALIZE                  SPA-GMN-COPY-AREA
               INITIALIZE                  SPA-NAI-COPY-AREA
      *        目次１
               MOVE    SPA-NAI-MENUNUM1    TO  WRK-MENUNUM1
               MOVE    SPA-GMN-MENU        TO  WRK-MENU
               MOVE    1                   TO  SPA-SYORIFLG
               PERFORM 30011-MENU1-HENSYU-SEC
               MOVE    WRK-MENUNUM1        TO  SPA-NAI-MENUNUM1
               MOVE    WRK-MENUNUM1        TO  SPA-GMN-SELNUM
               MOVE    WRK-MENU            TO  SPA-GMN-MENU
      *        目次２
               IF      SPA-NAI-MENUNUM2    NOT =   ZERO
                   MOVE    2                   TO  SPA-SYORIFLG
                   PERFORM 3001-MENU-HENSYU-SEC
               END-IF
      *        目次１の内容編集
               IF      SPA-GMN-SELNUM      NOT =   ZERO
                   MOVE    SPA-GMN-SELNUM      TO  WRK-MENUNUM1
                   MOVE    ZERO                TO  WRK-MENUNUM2
                   PERFORM 41011-HELP-HENSYU-SEC
               END-IF
               MOVE    ZERO                TO  SPA-NAI-MENUNUM2
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4501-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    目次１更新処理
      *****************************************************************
       45011-MENU1-KOUSIN-SEC       SECTION.
      *
           MOVE    SPA-NAI-MENUNUM1    TO  WRK-MENUNUM1
           MOVE    ZERO                TO  WRK-MENUNUM2
           MOVE    ZERO                TO  WRK-EDANUM
           IF      SPA-NAI-MENUNUM2    =   ZERO
               MOVE    SPA-NAI-HELP-G      TO  WRK-HELP-AREA
      *        内容登録
               PERFORM 450112-HELP-KOUSIN-SEC
           ELSE
               MOVE    SPACE               TO  WRK-HELP-AREA
      *        内容変更なし登録
               PERFORM 450111-HELP-MENU1-SEC
           END-IF
           .
       45011-MENU1-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    マスタ新規処理
      *****************************************************************
       450111-HELP-MENU1-SEC       SECTION.
      *
           MOVE    SPACE               TO  HELP-REC
           INITIALIZE                  HELP-REC
           MOVE    SPA-GMN-HELPKBN     TO  HELP-HELPKBN
           MOVE    SPACE               TO  HELP-KEY-OPID
           MOVE    SPA-GMN-LDNAME-LD   TO  HELP-LDNAME
           MOVE    SPA-NAI-MENUNUM1    TO  HELP-MENUNUM1
           MOVE    ZERO                TO  HELP-MENUNUM2
      *
           MOVE    SPA-HOSPNUM         TO  HELP-HOSPNUM
           MOVE    HELP-REC            TO  MCPDATA-REC
           MOVE    "tbl_help"          TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-HELP-READ-SEC
           ELSE
               INITIALIZE                  HELP-REC
               MOVE    1               TO  FLG-HELP
           END-IF
           IF      FLG-HELP            =   1
               PERFORM 4900-HELP-INI-SEC
               MOVE    ZERO                TO  HELP-MENUNUM2
               MOVE    ZERO                TO  HELP-EDANUM
               MOVE    SPA-GMN-MENU        TO  HELP-MENU
      *        目次２フラグ
               PERFORM 4903-HELP-MENU2FLG-SEC
               PERFORM 4901-HELP-ADD-SEC
           ELSE
      *        メニュウ名称
               MOVE    SPA-GMN-MENU        TO  HELP-MENU
      *        目次２フラグ
               PERFORM 4903-HELP-MENU2FLG-SEC
               PERFORM 4902-HELP-UPD-SEC
           END-IF
      *
           MOVE    "tbl_help"          TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
     *
           .
       450111-HELP-MENU1-EXT.
           EXIT.
      *
      *****************************************************************
      *    マスタ新規処理
      *****************************************************************
       450112-HELP-KOUSIN-SEC       SECTION.
      *
      *    削除後追加する
           MOVE    SPACE               TO  HELP-REC
           INITIALIZE                  HELP-REC
           MOVE    SPA-GMN-HELPKBN     TO  HELP-HELPKBN
           MOVE    SPACE               TO  HELP-KEY-OPID
           MOVE    SPA-GMN-LDNAME-LD   TO  HELP-LDNAME
           MOVE    SPA-NAI-MENUNUM1    TO  HELP-MENUNUM1
           MOVE    WRK-MENUNUM2        TO  HELP-MENUNUM2
      *
           MOVE    SPA-HOSPNUM         TO  HELP-HOSPNUM
           MOVE    HELP-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_help"          TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
      *    追加処理
           MOVE    ZERO                TO  WRK-EDANUM
           PERFORM VARYING     IDX-H   FROM    1   BY  1
                   UNTIL      (IDX-H   >   40  )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      WRK-HELP (IDX-H)    NOT =   SPACE
                   PERFORM 4900-HELP-INI-SEC
                   ADD     1                   TO  WRK-EDANUM
                   MOVE    WRK-MENUNUM2        TO  HELP-MENUNUM2
                   MOVE    WRK-EDANUM          TO  HELP-EDANUM
                   IF      WRK-EDANUM          =   1
                       IF      WRK-MENUNUM2        =   ZERO
                           MOVE    SPA-GMN-MENU        TO  HELP-MENU
      *                    目次２フラグ
                           PERFORM 4903-HELP-MENU2FLG-SEC
                       ELSE
                           MOVE    SPA-GMN-MENU2       TO  HELP-MENU
                       END-IF
                   END-IF
                   MOVE    WRK-HELP (IDX-H)    TO  HELP-HELP
      *
                   PERFORM 4901-HELP-ADD-SEC
               END-IF
           END-PERFORM
      *
           IF    (WRK-MENUNUM2         =   ZERO )  AND
                 (WRK-EDANUM           =   ZERO )
      *        目次１で内容が無い時の追加
               PERFORM 4900-HELP-INI-SEC
               MOVE    ZERO                TO  HELP-MENUNUM2
               MOVE    ZERO                TO  HELP-EDANUM
               MOVE    SPA-GMN-MENU        TO  HELP-MENU
      *        目次２フラグ
               PERFORM 4903-HELP-MENU2FLG-SEC
               PERFORM 4901-HELP-ADD-SEC
           END-IF
      *
           .
       450112-HELP-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    マスタ共通新規編集処理
      *****************************************************************
       4900-HELP-INI-SEC       SECTION.
      *
           MOVE    SPACE               TO  HELP-REC
           INITIALIZE                  HELP-REC
           MOVE    SPA-GMN-HELPKBN     TO  HELP-HELPKBN
           MOVE    SPACE               TO  HELP-KEY-OPID
           MOVE    SPA-GMN-LDNAME-LD   TO  HELP-LDNAME
           MOVE    SPA-NAI-MENUNUM1    TO  HELP-MENUNUM1
      *
           .
       4900-HELP-INI-EXT.
           EXIT.
      *
      *****************************************************************
      *    マスタ目次２フラグ編集処理
      *****************************************************************
       4903-HELP-MENU2FLG-SEC       SECTION.
      *
           IF     (SPA-GMN-TBLMAX2     >   ZERO )  OR
                  (SPA-NAI-MENUNUM2    >   ZERO )
               MOVE    1               TO  HELP-MENU2FLG
           ELSE
               MOVE    ZERO            TO  HELP-MENU2FLG
           END-IF
      *
           .
       4903-HELP-MENU2FLG-EXT.
           EXIT.
      *
      *****************************************************************
      *    ヘルプマスタ新規登録処理
      *****************************************************************
       4901-HELP-ADD-SEC       SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  HELP-CREYMD
           MOVE    SMCNDATE-YMD        TO  HELP-UPYMD
           MOVE    SMCNDATE-HMS        TO  HELP-UPHMS
      *
           MOVE    SPA-HOSPNUM         TO  HELP-HOSPNUM
           MOVE    HELP-REC            TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_help"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1000"          TO  SPA-ERRCD
               DISPLAY "N41 HELP INS ERR:" MCP-RC
                       ",KEY:" HELP-KEY
           END-IF
      *
           .
       4901-HELP-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ヘルプマスタ更新処理
      *****************************************************************
       4902-HELP-UPD-SEC       SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  HELP-UPYMD
           MOVE    SMCNDATE-HMS        TO  HELP-UPHMS
      *
           MOVE    SPA-HOSPNUM         TO  HELP-HOSPNUM
           MOVE    HELP-REC            TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_help"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1000"          TO  SPA-ERRCD
               DISPLAY "N41 HELP UPD ERR:" MCP-RC
           END-IF
      *
           .
       4902-HELP-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    目次２更新処理
      *****************************************************************
       45012-MENU2-KOUSIN-SEC       SECTION.
      *
           MOVE    SPA-NAI-MENUNUM1    TO  WRK-MENUNUM1
           MOVE    SPA-NAI-MENUNUM2    TO  WRK-MENUNUM2
           MOVE    ZERO                TO  WRK-EDANUM
           MOVE    SPA-GMN-HELP        TO  WRK-HELP-AREA
           MOVE    SPA-NAI-HELP-G      TO  WRK-HELP-AREA
      *    内容登録
           PERFORM 450112-HELP-KOUSIN-SEC
           .
       45012-MENU2-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    複写処理
      *****************************************************************
       45013-COPY-SYORI-SEC       SECTION.
      *
      *    登録済みでないかのチェック
           INITIALIZE                   WRK-KEY-AREA
           MOVE    SPA-GMN-HELPKBN-CP   TO  WRK-HELPKBN
           MOVE    SPA-GMN-LDNAME-LD-CP TO  WRK-LDNAME-LD
           MOVE    SPA-GMN-MENUNUM1-CP  TO  WRK-MENUNUM1
           MOVE    SPA-GMN-MENUNUM2-CP  TO  WRK-MENUNUM2
           MOVE    1                    TO  WRK-EDANUM
           PERFORM 410112-HELP-KENSAKU-SEC
           IF      FLG-ARI              =   1
               MOVE    "0009"          TO  SPA-ERRCD
               MOVE    10              TO  SPA-GMN-CUR
           END-IF
      *
      *    目次１の登録
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-MENU1-CP    =   SPACE
                   MOVE    SPACE               TO  HELP-REC
                   INITIALIZE                  HELP-REC
                   PERFORM 450131-COPY-INI-HEN-SEC
                   MOVE    ZERO                TO  WRK-MENUNUM2
                   PERFORM 450132-COPY-MENU2-SEC
               ELSE
      *            目次１のフラグ
                   PERFORM 450133-COPY-MENU1-SEC
               END-IF
           END-IF
      *
      *    目次２の登録
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-MENUNUM2    >   ZERO
                   MOVE    SPA-NAI-MENUNUM2    TO  WRK-MENUNUM2
                   PERFORM 450132-COPY-MENU2-SEC
               END-IF
           END-IF
      *
           .
       45013-COPY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    目次１の更新処理
      *****************************************************************
       450133-COPY-MENU1-SEC       SECTION.
      *
           INITIALIZE                   WRK-KEY-AREA
           MOVE    SPA-GMN-HELPKBN-CP   TO  WRK-HELPKBN
           MOVE    SPA-GMN-LDNAME-LD-CP TO  WRK-LDNAME-LD
           MOVE    SPA-GMN-MENUNUM1-CP  TO  WRK-MENUNUM1
           MOVE    ZERO                 TO  WRK-MENUNUM2
           MOVE    1                    TO  WRK-EDANUM
           PERFORM 410112-HELP-KENSAKU-SEC
           IF      FLG-ARI              =   1
               IF      HELP-MENU2FLG    =   ZERO
                   MOVE    1                TO  HELP-MENU2FLG
                   PERFORM 4902-HELP-UPD-SEC
               END-IF
           END-IF
      *
           .
       450133-COPY-MENU1-EXT.
           EXIT.
      *
      *****************************************************************
      *    複写マスタ新規処理
      *****************************************************************
       450132-COPY-MENU2-SEC       SECTION.
      *
      *    複写元検索
           MOVE    SPACE               TO  HELP-REC
           INITIALIZE                  HELP-REC
           MOVE    SPA-GMN-HELPKBN     TO  HELP-HELPKBN
           MOVE    SPACE               TO  HELP-KEY-OPID
           MOVE    SPA-GMN-LDNAME-LD   TO  HELP-LDNAME
           MOVE    SPA-NAI-MENUNUM1    TO  HELP-MENUNUM1
           MOVE    WRK-MENUNUM2        TO  HELP-MENUNUM2
      *
           MOVE    SPA-HOSPNUM         TO  HELP-HOSPNUM
           MOVE    HELP-REC            TO  MCPDATA-REC
           MOVE    "tbl_help"          TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-HELP-READ-SEC
           ELSE
               INITIALIZE                  HELP-REC
               MOVE    1               TO  FLG-HELP
           END-IF
           PERFORM UNTIL       (FLG-HELP   =   1  )  OR
                               (SPA-ERRCD  NOT =   SPACE )
      *
               PERFORM 450131-COPY-INI-HEN-SEC
               IF      WRK-MENUNUM2        =   ZERO
                   MOVE    ZERO                TO  HELP-MENUNUM2
                   MOVE    SPA-GMN-MENU1-CP    TO  HELP-MENU
               ELSE
                   MOVE    SPA-GMN-MENUNUM2-CP TO  HELP-MENUNUM2
                   MOVE    SPA-GMN-MENU2-CP    TO  HELP-MENU
               END-IF
      *
               PERFORM 4901-HELP-ADD-SEC
      *
               MOVE    "tbl_help"          TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-HELP-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_help"          TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       450132-COPY-MENU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    複写マスタ共通新規編集処理
      *****************************************************************
       450131-COPY-INI-HEN-SEC       SECTION.
      *
           MOVE    SPA-GMN-HELPKBN-CP  TO  HELP-HELPKBN
           MOVE    SPACE               TO  HELP-KEY-OPID
           MOVE    SPA-GMN-LDNAME-LD-CP
                                       TO  HELP-LDNAME
           MOVE    SPA-NAI-MENUNUM1-CP TO  HELP-MENUNUM1
      *
           .
       450131-COPY-INI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    移動処理
      *****************************************************************
       45014-MOVE-SYORI-SEC       SECTION.
      *
      *    複写処理
           PERFORM 45013-COPY-SYORI-SEC
      *
           IF      SPA-ERRCD           =   SPACE
      *        削除処理
               PERFORM 4031-DELETE-SYORI-SEC
           END-IF
      *
           .
       45014-MOVE-SYORI-EXT.
           EXIT.
      *
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
           IF      SPA-N41IDCD       NOT =   SPACE
               PERFORM 520-N41IDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "N41"               TO  MCP-WINDOW.
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
           IF     (FLG-GMN-INIT        =   1   )  OR
                  (SPA-GMN-TBLMAX      =   ZERO )
               MOVE    ZERO                TO  WRK-N41-ROW
           ELSE
               MOVE    N41-HEADLIST-ROW    TO  WRK-N41-ROW
           END-IF
           IF     (FLG-GMN-INIT2       =   1   )  OR
                  (SPA-GMN-TBLMAX2     =   ZERO )
               MOVE    ZERO                TO  WRK-N41-ROW2
           ELSE
               MOVE    N41-MENU2LIST-ROW   TO  WRK-N41-ROW2
           END-IF
      *
           MOVE    SPACE               TO  N41
      *
      *    コラムリストの位置指定
           INITIALIZE                      N41
           MOVE    ZERO                TO  N41-HEADLIST-ROW
           MOVE    ZERO                TO  N41-HEADLIST-ROWATTR
           IF      WRK-N41-ROW     NOT =   ZERO
               MOVE    WRK-N41-ROW         TO  N41-HEADLIST-ROW
           END-IF
           MOVE    ZERO                TO  N41-MENU2LIST-ROW
           MOVE    ZERO                TO  N41-MENU2LIST-ROWATTR
           IF      WRK-N41-ROW2    NOT =   ZERO
               MOVE    WRK-N41-ROW2        TO  N41-MENU2LIST-ROW
           END-IF
      *
           MOVE    SPA-GMN-HELPKBN-G   TO  N41-HELPKBN
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX     >   SPA-HELPKBN-MAX
               MOVE    SPA-GMN-HELPKBN-ITEM(IDX)   TO  N41-HELPKBN-ITEM
                                                                   (IDX)
           END-PERFORM
           MOVE    SPA-HELPKBN-MAX     TO  N41-HELPKBN-COUNT
      *
           MOVE    SPA-GMN-LDNAME-G    TO  N41-LDNAME
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX     >   SPA-LDNAME-MAX
               MOVE    SPA-GMN-LDNAME-ITEM(IDX)   TO  N41-LDNAME-ITEM
                                                                   (IDX)
           END-PERFORM
           MOVE    SPA-LDNAME-MAX      TO  N41-LDNAME-COUNT
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX     >   SPA-GMN-TBLMAX
               MOVE    SPA-GMN-TMENUNUM(IDX)   TO  N41-TMENUNUM (IDX)
               MOVE    SPA-GMN-TMENU   (IDX)   TO  N41-TMENU    (IDX)
               IF      SPA-GMN-TMENUNUM (IDX)  =   SPA-GMN-SELNUM
                   MOVE    "T"             TO  N41-HEADLIST-SELECT(IDX)
                   MOVE    IDX             TO  N41-HEADLIST-ROW
                   MOVE    2               TO  N41-HEADLIST-ROWATTR
               ELSE
                   MOVE    "F"             TO  N41-HEADLIST-SELECT(IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-TBLMAX      TO  N41-HEADLIST-COUNT
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX     >   SPA-GMN-TBLMAX2
               MOVE    SPA-GMN-TMENUNUM2(IDX)  TO  N41-TMENUNUM2(IDX)
               MOVE    SPA-GMN-TMENU2   (IDX)  TO  N41-TMENU2   (IDX)
               IF      SPA-GMN-TMENUNUM2 (IDX) =   SPA-GMN-SELNUM2
                   MOVE    "T"             TO  N41-MENU2LIST-SELECT(IDX)
                   MOVE    IDX             TO  N41-MENU2LIST-ROW
                   MOVE    2               TO  N41-MENU2LIST-ROWATTR
               ELSE
                   MOVE    "F"             TO  N41-MENU2LIST-SELECT(IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-TBLMAX2     TO  N41-MENU2LIST-COUNT
      *
           MOVE    SPA-GMN-HEADMENU    TO  N41-HEADMENU
           MOVE    SPA-GMN-SELNUM      TO  N41-SELNUM
           MOVE    SPA-GMN-MENU        TO  N41-MENU
           MOVE    SPA-GMN-SELNUM2     TO  N41-SELNUM2
           MOVE    SPA-GMN-MENU2       TO  N41-MENU2
      *
           MOVE    SPA-GMN-HELP        TO  N41-HELP
      *
           IF      SPA-COPYFLG         =   1
               MOVE    SPA-GMN-HELPKBN-CP-G    TO  N41-HELPKBN-CP
      *        標準は対象外（開始を２）
               MOVE    1                   TO  IDY
      *??
      ***      MOVE    ZERO                TO  IDY
      *??
               MOVE    ZERO                TO  N41-HELPKBN-CP-COUNT
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   SPA-HELPKBN-MAX
                   ADD     1               TO  IDY
                   MOVE    SPA-GMN-HELPKBN-ITEM(IDY)
                                           TO  N41-HELPKBN-CP-ITEM
                                                                 (IDX)
                   MOVE    IDX             TO  N41-HELPKBN-CP-COUNT
               END-PERFORM
      *
               MOVE    SPA-GMN-LDNAME-CP-G TO  N41-LDNAME-CP
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   SPA-LDNAME-MAX
                   MOVE    SPA-GMN-LDNAME-ITEM(IDX)
                                           TO  N41-LDNAME-CP-ITEM(IDX)
               END-PERFORM
               MOVE    SPA-LDNAME-MAX      TO  N41-LDNAME-CP-COUNT
      *
               MOVE    SPA-GMN-MENUNUM1-CP TO  N41-MENUNUM1-CP
               MOVE    SPA-GMN-MENU1-CP    TO  N41-MENU1-CP
               MOVE    SPA-GMN-MENUNUM2-CP TO  N41-MENUNUM2-CP
               MOVE    SPA-GMN-MENU2-CP    TO  N41-MENU2-CP
           END-IF
      *    非活性処理
           PERFORM 510-GSRAUTH-SEC
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *****************************************************************
      *    非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           IF      SPA-GMN-HELPKBN     =   "0"
      *        標準は更新不可
               MOVE    WIDGET-INSENSITIVE  TO  N41-B12-STATE
               MOVE    WIDGET-INSENSITIVE  TO  N41-B03-STATE
      *??
      *        MOVE    WIDGET-NORMAL       TO  N41-B12-STATE
      *        MOVE    WIDGET-NORMAL       TO  N41-B03-STATE
      *??
           ELSE
               MOVE    WIDGET-NORMAL       TO  N41-B12-STATE
               MOVE    WIDGET-NORMAL       TO  N41-B03-STATE
           END-IF
      *
           IF      SPA-COPYFLG         =   1
               MOVE    WIDGET-NORMAL       TO  N41-B12-STATE
               MOVE    WIDGET-NORMAL       TO  N41-HELPKBN-CP-STATE
               MOVE    WIDGET-NORMAL       TO  N41-LDNAME-CP-STATE
               MOVE    WIDGET-NORMAL       TO  N41-MENUNUM1-CP-STATE
               MOVE    WIDGET-NORMAL       TO  N41-MENU1-CP-STATE
               MOVE    WIDGET-NORMAL       TO  N41-MENUNUM2-CP-STATE
               MOVE    WIDGET-NORMAL       TO  N41-MENU2-CP-STATE
               IF      SPA-NAI-MENUNUM2    =   ZERO
                   MOVE    WIDGET-INSENSITIVE  TO  N41-MENUNUM2-CP-STATE
                   MOVE    WIDGET-INSENSITIVE  TO  N41-MENU2-CP-STATE
               END-IF
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  N41-HELPKBN-CP-STATE
               MOVE    WIDGET-INSENSITIVE  TO  N41-LDNAME-CP-STATE
               MOVE    WIDGET-INSENSITIVE  TO  N41-MENUNUM1-CP-STATE
               MOVE    WIDGET-INSENSITIVE  TO  N41-MENU1-CP-STATE
               MOVE    WIDGET-INSENSITIVE  TO  N41-MENUNUM2-CP-STATE
               MOVE    WIDGET-INSENSITIVE  TO  N41-MENU2-CP-STATE
           END-IF
      *
           .
       51O-GSRAUTH-EXT.
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
               WHEN    01
                   MOVE    "HELPKBN"         TO  MCP-WIDGET
               WHEN    02
                   MOVE    "LDNAME"          TO  MCP-WIDGET
               WHEN    03
                   MOVE    "SELNUM"          TO  MCP-WIDGET
               WHEN    04
                   MOVE    "MENU"            TO  MCP-WIDGET
               WHEN    05
                   MOVE    "SELNUM2"         TO  MCP-WIDGET
               WHEN    06
                   MOVE    "MENU2"           TO  MCP-WIDGET
               WHEN    07
                   MOVE    "HELP"            TO  MCP-WIDGET
               WHEN    08
                   MOVE    "HEADMENU"        TO  MCP-WIDGET
               WHEN    10
                   MOVE    "HELPKBN_CP"      TO  MCP-WIDGET
               WHEN    11
                   MOVE    "LDNAME_CP"       TO  MCP-WIDGET
               WHEN    12
                   MOVE    "MENUNUM1_CP"     TO  MCP-WIDGET
               WHEN    13
                   MOVE    "MENU1_CP"        TO  MCP-WIDGET
               WHEN    14
                   MOVE    "MENUNUM2_CP"     TO  MCP-WIDGET
               WHEN    15
                   MOVE    "MENU2_CP"        TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B12"             TO  MCP-WIDGET
           END-EVALUATE
      *
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
               WHEN    "HELPKBN"
                   MOVE    2               TO  SPA-GMN-CUR
      * 業務が選択可能になるまで
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "LDNAME"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "SELNUM"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "MENU"
                   IF      SPA-NAI-MENU1FLG    =   1
                       MOVE    7               TO  SPA-GMN-CUR
                   ELSE
                       MOVE    5               TO  SPA-GMN-CUR
                   END-IF
               WHEN    "SELNUM2"
                   MOVE    6               TO  SPA-GMN-CUR
               WHEN    "MENU2"
                   MOVE    7               TO  SPA-GMN-CUR
               WHEN    "HELP"
                   MOVE    99              TO  SPA-GMN-CUR
      *
               WHEN    "HELPKBN_CP"
                   MOVE    11              TO  SPA-GMN-CUR
      * 業務が選択可能になるまで
                   MOVE    12              TO  SPA-GMN-CUR
               WHEN    "LDNAME_CP"
                   MOVE    12              TO  SPA-GMN-CUR
               WHEN    "MENUNUM1_CP"
                   MOVE    13              TO  SPA-GMN-CUR
               WHEN    "MENU1_CP"
                   IF      SPA-NAI-MENUNUM2    =   ZERO
                       MOVE    99              TO  SPA-GMN-CUR
                   ELSE
                       MOVE    14              TO  SPA-GMN-CUR
                   END-IF
               WHEN    "MENUNUM2_CP"
                   MOVE    15              TO  SPA-GMN-CUR
               WHEN    "MENU2_CP"
                   MOVE    99              TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
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
                   MOVE    "区分選択入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "業務選択入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "番号１入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "目次１は全角で入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "番号２入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "目次２は全角で入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "複写先区分がエラーです。"
                                       TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "複写先業務がエラーです。"
                                       TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "複写先目次名は全角で入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "複写先は既に登録済みです。"
                                       TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "区分が未入力です。入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "業務ＬＤが未入力です。入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "目次１番号が未入力です。入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0112"
                   MOVE    "目次１番号が未入力です。"
                                       TO  SPA-ERRMSG
                   MOVE    "目次２番号だけでは登録できません。"
                                       TO  SPA-ERRMSG(25:)
               WHEN    "0013"
                   MOVE    "目次１名称が未入力です。入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0113"
                   MOVE    "目次２名称が未入力です。入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "ヘルプ内容が未入力です。入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "複写先はすべて入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "複写元と複写先が同じです。複写できません"
                                       TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "複写中です。目次切替えはできません。"
                                       TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "複写中です。選択はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE    "既に登録済みの番号です。"
                                       TO  SPA-ERRMSG
                   MOVE    "一覧表に表示してから選択して下さい"
                                       TO  SPA-ERRMSG(25:)
               WHEN    "0021"
                   MOVE    "複写中です。目次名変更はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "0022"
                   MOVE    "複写は詳細毎となります。"
                                       TO  SPA-ERRMSG
                   MOVE    "目次２まで選択して下さい。"
                                       TO  SPA-ERRMSG(25:)
               WHEN    "0025"
                   MOVE    "新規登録中です。複写できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0023"
                   MOVE    "前頁はありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0024"
                   MOVE    "次頁はありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0026"
                   MOVE    "目次１に該当がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0030"
                   MOVE    "新規分です。削除できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0031"
                   MOVE    "番号が選択されていません。"
                                       TO  SPA-ERRMSG
               WHEN    "0032"
                   MOVE    "複写中です。削除できません。"
                                       TO  SPA-ERRMSG
               WHEN    "1000"
                   MOVE    "マスタ更新エラーです。"
                                       TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD   TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  N41ERR
           MOVE    SPA-ERRCD           TO  N41ERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  N41ERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "N41"               TO  SPA-MOTOPG
           MOVE    "N41ERR"            TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "N41ERR"            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-N41IDSET-SEC              SECTION.
      *
           EVALUATE    SPA-N41IDCD
               WHEN    "0101"
                   MOVE    "目次１の内容をすべて削除します。"
                                       TO  WRK-N41IDMSG
                   MOVE    "よろしいですか？"
                                       TO  WRK-N41IDMSG(33:)
               WHEN    "0102"
                   MOVE    "ヘルプ明細を削除します。"
                                       TO  WRK-N41IDMSG
                   MOVE    "よろしいですか？"
                                       TO  WRK-N41IDMSG(25:)
               WHEN    "0103"
                   MOVE    "新規登録中です。現在の内容は削除されます。"
                                       TO  WRK-N41IDMSG
                   MOVE    "よろしいですか？"
                                       TO  WRK-N41IDMSG(43:)
               WHEN    OTHER
                   MOVE    SPA-N41IDCD
                                       TO  WRK-N41IDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  N41ID1
           INITIALIZE                      N41ID1
           MOVE    SPA-N41IDCD         TO  N41ID1-ID1CODE
           MOVE    WRK-N41IDMSG        TO  N41ID1-ID1MSG
      *
           MOVE    SPACE               TO  SPA-N41ID1-FLG
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "N41"               TO  SPA-MOTOPG
           MOVE    "N41ID1"            TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "N41ID1"            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-N41IDSET-EXT.
           EXIT.
      *****************************************************************
      *    ヘルプマスター読込
      *****************************************************************
       900-HELP-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HELP-REC
               MOVE    ZERO            TO  FLG-HELP
           ELSE
               INITIALIZE                  HELP-REC
               MOVE    1               TO  FLG-HELP
           END-IF
      *
           .
       900-HELP-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING
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
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
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
           CALL   "ORCDBMAIN"       USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
