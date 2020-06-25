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
       PROGRAM-ID.                 ORCGKHELP.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : ヘルプ画面
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/01/24    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  05/11/17  MONFUNC 対応
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    会計照会共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "KHELPSCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-GMN-INIT        PIC  9(01).
      *
           03  FLG-HELP            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-ST              PIC 9(04).
           03  IDX-H               PIC 9(04).
           03  IDX-H1              PIC 9(04).
           03  IDX-H2              PIC 9(04).
           03  IDX-H3              PIC 9(04).
           03  IDX-TBL             PIC 9(04).
      *
           03  IDX-K               PIC 9(04).
           03  IDX-K2              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDGET              PIC X(64).
           03  WRK-ORC-DBPATH          PIC X(64).
      *
           03  WRK-KEY-MENUNUM1        PIC 9(04).
           03  WRK-KEY-LDNAME          PIC X(20).
      *
           03  TBL-HELP-AREA.
               05  TBL-HELP-OCC        OCCURS   100.
                   07  TBL-GMN-TMENUNUM-G.
                       09  TBL-GMN-TMENUBUN    PIC X(01).
                       09  TBL-GMN-TMENUNUM    PIC 9(04).
                   07  TBL-GMN-TMENU       PIC X(30).
      *
           03  WRK-HELP-AREA.
               05  WRK-HELP            PIC X(50)  OCCURS  40.
           03  WRK-HELP-AREA2.
               05  WRK-HELP2           PIC X(50)  OCCURS  40.
           03  WRK-SELNUM              PIC 9(04).
      *
           03  WRK-INPUT-MENU          PIC X(30).
      *
      *    コラムリスト位置
           03  WRK-KHELP-ROW           PIC S9(09).
      *
           03  WRK-MCP-PATH.
               05  WRK-MCP-BLOCKS      PIC S9(9)   BINARY.
               05  WRK-MCP-RNAME       PIC S9(9)   BINARY.
               05  WRK-MCP-PNAME       PIC S9(9)   BINARY.
      *
      *
       01  WRK-TBLMAX                  PIC 9(04)   VALUE    100.
      *
      *    業務名
           COPY    "CPLOCKCONST.INC".
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
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
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
      **** COPY    "ORCA-DBPATH".
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
      *
       01  SCRAREA.
           COPY    "ORCA21SCRAREA.INC".
      *!!!     COPY    "K01.INC".
      *!!!     COPY    "K02.INC".
      *!!!     COPY    "K02N.INC".
      *!!!     COPY    "K021.INC".
      *!!!     COPY    "K023.INC".
      *!!!     COPY    "K03.INC".
      *!!!     COPY    "K04.INC".
      *!!!     COPY    "K05.INC".
      *!!!     COPY    "K051.INC".
      *!!!     COPY    "K052.INC".
      *!!!     COPY    "K06.INC".
      *!!!     COPY    "K061.INC".
      *!!!     COPY    "K062.INC".
      *!!!     COPY    "K07.INC".
      *!!!     COPY    "K08.INC".
      *!!!     COPY    "K081.INC".
      *!!!     COPY    "K09.INC".
      *!!!     COPY    "K10.INC".
      *!!!     COPY    "K97.INC".
      *!!!     COPY    "K98.INC".
      *!!!     COPY    "K99.INC".
      *!!!     COPY    "K03X.INC".
      *!!!     COPY    "KERR.INC".
      *!!!     COPY    "KERR2.INC".
      *!!!     COPY    "KID1.INC".
      *!!!     COPY    "KID2.INC".
      *!!!     COPY    "KHELP.INC".
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
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *    ヘルプ・発行指示共通
           MOVE    SPA-ETCKYOUTU-AREA  TO  SPA-KHELP
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
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
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-KHELP           TO  SPA-ETCKYOUTU-AREA
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "KERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE
           MOVE   "KHELP"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
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
           INITIALIZE                  SPA-HELP-AREA
      *
      *    業務ＬＤ
           MOVE    "ORCA21"            TO  SPA-KHELP-LDNAME
      *
           PERFORM                     310-SPA-SET-SEC
      *
           MOVE    1                   TO  SPA-KHELP-CUR
      *
           .
       3OO-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           MOVE    1                   TO  SPA-KHELP-SYORIFLG
      *    自院分を優先とする
           MOVE    "1"                 TO  SPA-KHELP-HELPKBN
           MOVE    ZERO                TO  SPA-KHELP-MENUNUM1
           MOVE    ZERO                TO  SPA-KHELP-MENUNUM2
           MOVE    1                   TO  SPA-KHELP-PAGE
           PERFORM 320-MENU-HENSYU-SEC
      *    自院分が無い時、標準
           IF      SPA-KHELP-TBLMAX    =   ZERO
               MOVE    "0"                 TO  SPA-KHELP-HELPKBN
               PERFORM 320-MENU-HENSYU-SEC
           END-IF
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    目次編集処理
      *****************************************************************
       320-MENU-HENSYU-SEC                  SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
           INITIALIZE                  TBL-HELP-AREA
           INITIALIZE                  SPA-KHELP-TBL-AREA
           MOVE    ZERO                TO  SPA-KHELP-TBLMAX
      *
           MOVE    SPACE               TO  HELP-REC
           INITIALIZE                  HELP-REC
           MOVE    SPA-KHELP-HELPKBN   TO  HELP-HELPKBN
           MOVE    SPACE               TO  HELP-KEY-OPID
           MOVE    SPA-KHELP-LDNAME    TO  HELP-LDNAME
      *
           IF      SPA-KHELP-MENUNUM1  =   ZERO
               CONTINUE
           ELSE
               MOVE    SPA-KHELP-MENUNUM1  TO  HELP-MENUNUM1
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  HELP-HOSPNUM
           MOVE    HELP-REC            TO  MCPDATA-REC
           MOVE    "tbl_help"          TO  MCP-TABLE
           IF      SPA-KHELP-MENUNUM1  =   ZERO
               MOVE    "key1"              TO  MCP-PATHNAME
           ELSE
               MOVE    "key2"              TO  MCP-PATHNAME
           END-IF
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
           IF      SPA-KHELP-PAGE      =   ZERO
               MOVE    1                   TO  IDX-ST
           ELSE
               COMPUTE IDX-ST          =   (SPA-KHELP-PAGE   -   1)
                                       *   WRK-TBLMAX
                                       +   1
           END-IF
           PERFORM
               UNTIL   (FLG-HELP           =   1  )  OR
                       (IDX                >=  WRK-TBLMAX)
                   ADD     1                   TO  IDY
                   IF      IDX-ST              <=  IDY
                       ADD     1               TO  IDX
                       IF     (SPA-KHELP-MENUNUM1  =   ZERO )  AND
                              (HELP-MENU2FLG       =   1    )
                           MOVE    "+"             TO  TBL-GMN-TMENUBUN
                                                                 (IDX)
                       END-IF
                       IF     (SPA-KHELP-MENUNUM1  NOT =   ZERO )  AND
                              (HELP-MENUNUM2       =   ZERO )
                           MOVE    ">"             TO  TBL-GMN-TMENUBUN
                                                                 (IDX)
                       END-IF
                       MOVE    IDY             TO  SPA-KHELP-TRENNUM
                                                                  (IDX)
                       MOVE    HELP-MENU       TO  TBL-GMN-TMENU(IDX)
      *
                       MOVE    HELP-HELPKBN    TO  SPA-KHELP-THELPKBN
                                                                  (IDX)
                       MOVE    HELP-MENUNUM1   TO  SPA-KHELP-TMENUNUM1
                                                                  (IDX)
                       MOVE    HELP-MENUNUM2   TO  SPA-KHELP-TMENUNUM2
                                                                  (IDX)
                       IF     (HELP-MENU2FLG       =   1    )  AND
                              (HELP-MENUNUM2       =   ZERO )
                           MOVE    1               TO  SPA-KHELP-TEDANUM
                                                                (IDX)
                       ELSE
                           MOVE    ZERO            TO  SPA-KHELP-TEDANUM
                                                                (IDX)
                       END-IF
                   END-IF
      *
               IF      SPA-KHELP-MENUNUM1  =   ZERO
                   MOVE    "key1"              TO  MCP-PATHNAME
               ELSE
                   MOVE    "key2"              TO  MCP-PATHNAME
               END-IF
               PERFORM 900-HELP-READ-SEC
           END-PERFORM
           MOVE    "tbl_help"          TO  MCP-TABLE
           IF      SPA-KHELP-MENUNUM1  =   ZERO
               MOVE    "key1"              TO  MCP-PATHNAME
           ELSE
               MOVE    "key2"              TO  MCP-PATHNAME
           END-IF
           PERFORM 990-DBCLOSE-SEC
      *
      *    次頁判定
           IF     (IDX                 >=  WRK-TBLMAX )  AND
                  (FLG-HELP            =   ZERO)
               MOVE    1               TO  SPA-KHELP-NEXT
           ELSE
               MOVE    ZERO            TO  SPA-KHELP-NEXT
           END-IF
           MOVE    IDX                 TO  SPA-KHELP-TBLMAX
           MOVE    ZERO                TO  SPA-KHELP-SELNUM
      *
           .
       320-MENU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    コラムリスト位置保存
           IF      FLG-GMN-INIT        =   1
               MOVE    ZERO                TO  WRK-KHELP-ROW
           ELSE
               MOVE    KHELP-ROW           TO  WRK-KHELP-ROW
           END-IF
      *
           IF      FLG-GMN-INIT        =   1
               MOVE    SPACE               TO  KHELP
           END-IF
      *
           IF      WRK-HELP-AREA       =   SPACE
               MOVE    KHELP-HELP          TO  WRK-HELP-AREA
               IF     (SPA-KHELP-SELNUM    =   ZERO  )  OR
                      (SPA-ERRCD           =   "0001")
                  MOVE    SPACE               TO  WRK-HELP-AREA
               END-IF
           END-IF
      *
      *    コラムリストの位置指定
           MOVE    ZERO                TO  KHELP-ROW
           MOVE    ZERO                TO  KHELP-ROWATTR
           IF      WRK-KHELP-ROW       NOT =   ZERO
               MOVE    WRK-KHELP-ROW       TO  KHELP-ROW
           END-IF
      *
           MOVE    SPA-KHELP-GMN-INPUT TO  KHELP-INPUT
           MOVE    SPA-KHELP-SELNUM    TO  KHELP-SELNUM
      *
           MOVE    WRK-HELP-AREA       TO  KHELP-HELP
      *
      *    目次
           IF      FLG-GMN-INIT        =   1
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   SPA-KHELP-TBLMAX
                   MOVE    TBL-GMN-TMENUBUN(IDX)
                                           TO  KHELP-TMENUNUM (IDX)
                   MOVE    SPA-KHELP-TRENNUM (IDX)
                                           TO  KHELP-TMENUNUM (IDX)
                                                              (2:)
                   MOVE    TBL-GMN-TMENU   (IDX)
                                           TO  KHELP-TMENU    (IDX)
               END-PERFORM
               MOVE    SPA-KHELP-TBLMAX    TO  KHELP-COUNT
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KHELP-TBLMAX
               IF      SPA-KHELP-SELNUM    =   SPA-KHELP-TRENNUM (IDX)
                   MOVE    "T"             TO  KHELP-SELECT   (IDX)
                   MOVE    IDX             TO  KHELP-ROW
                   MOVE    2               TO  KHELP-ROWATTR
               ELSE
                   MOVE    "F"             TO  KHELP-SELECT   (IDX)
               END-IF
           END-PERFORM
      *
           IF      SPA-KHELP-HELPKBN   =   "0"
               MOVE    "【標準】"          TO  KHELP-HEAD-VALUE
               MOVE    "blue"              TO  KHELP-HEAD-STYLE
           ELSE
               MOVE    "【自院】"          TO  KHELP-HEAD-VALUE
               MOVE    "black"             TO  KHELP-HEAD-STYLE
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-KHELP-CUR        =   ZERO
               EVALUATE    WRK-WIDGET
                   WHEN    "INPUT"
                       MOVE    1                   TO  SPA-KHELP-CUR
                   WHEN    "SELNUM"
                       MOVE    1                   TO  SPA-KHELP-CUR
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-KHELP-CUR
               WHEN    1
                   MOVE    "SELNUM"            TO  MCP-WIDGET
               WHEN    2
                   MOVE    "INPUT"             TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        検索
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 404-KENSAKU-SEC
      *        検索
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 405-KIRIKAE-SEC
      *        前頁
               WHEN    "CLICKED"       ALSO    "B06"
                    PERFORM 406-PAGE-BACK-SEC
      *        次頁
               WHEN    "CLICKED"       ALSO    "B07"
                    PERFORM 407-PAGE-NEXT-SEC
      *        標準
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 409-HYOUJUN-SEC
      *        自院
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 410-JIIN-SEC
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
           MOVE    ZERO                TO  SPA-KHELP-CUR
      *    入力項目の決定
           MOVE    MCP-WIDGET          TO  WRK-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        キー入力
               WHEN    "ACTIVATE"      ALSO    "INPUT"
                   PERFORM 401-INPUT-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 402-SELNUM-CHK-SEC
               WHEN    ANY             ALSO    "MENULIST"
                   PERFORM 403-MENULIST-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    目次選択処理
      *****************************************************************
       403-MENULIST-CHK-SEC          SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KHELP-TBLMAX
               IF      KHELP-SELECT (IDX)  =   "T"
                   MOVE    SPA-KHELP-TRENNUM(IDX)  TO  WRK-SELNUM
               END-IF
           END-PERFORM
      *
           IF     (WRK-SELNUM          >   ZERO ) AND
                  (WRK-SELNUM      NOT =   SPA-KHELP-SELNUM)
               MOVE    WRK-SELNUM      TO  KHELP-SELNUM 
               PERFORM 402-SELNUM-CHK-SEC
           END-IF
      *
           .
      *
       403-MENULIST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号処理
      *****************************************************************
       402-SELNUM-CHK-SEC               SECTION.
      *
           MOVE    KHELP-SELNUM        TO  SPA-KHELP-SELNUM
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  IDX-TBL
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-KHELP-TBLMAX) OR
                              (FLG-OK      =   1     )
               IF      SPA-KHELP-SELNUM    =   SPA-KHELP-TRENNUM(IDX)
                   MOVE    1                   TO  FLG-OK
                   MOVE    IDX                 TO  IDX-TBL
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-KHELP-CUR
           ELSE
               MOVE    SPA-KHELP-TMENUNUM1 (IDX-TBL)
                                           TO  SPA-KHELP-MENUNUM1
               MOVE    SPA-KHELP-TMENUNUM2 (IDX-TBL)
                                           TO  SPA-KHELP-MENUNUM2
               MOVE    SPA-KHELP-THELPKBN  (IDX-TBL)
                                           TO  SPA-KHELP-HELPKBN
               IF     (SPA-KHELP-MENUNUM2  =   ZERO  )  AND
                      (SPA-KHELP-TEDANUM (IDX-TBL) =   1    )
      *            見出１
                   MOVE    2                   TO  SPA-KHELP-SYORIFLG
                   MOVE    1                   TO  SPA-KHELP-PAGE
                   PERFORM 320-MENU-HENSYU-SEC
                   MOVE    ZERO                TO  SPA-KHELP-SELNUM
      *            １件目を初期表示とする
                   IF      SPA-KHELP-TBLMAX    >   ZERO
                       MOVE    SPA-KHELP-TRENNUM (1)
                                               TO  SPA-KHELP-SELNUM
                       MOVE    SPA-KHELP-TMENUNUM1 (1)
                                               TO  SPA-KHELP-MENUNUM1
                       MOVE    SPA-KHELP-TMENUNUM2 (1)
                                               TO  SPA-KHELP-MENUNUM2
                      MOVE    SPA-KHELP-THELPKBN  (1)
                                               TO  SPA-KHELP-HELPKBN
                   END-IF
               END-IF
      *        見出２
               PERFORM 4021-MENUNUM2-SYORI-SEC
               MOVE    1                   TO  SPA-KHELP-CUR
           END-IF
           .
       402-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    キー入力処理
      *****************************************************************
       4021-MENUNUM2-SYORI-SEC               SECTION.
      *
           MOVE    SPACE               TO  WRK-HELP-AREA
           MOVE    ZERO                TO  IDX-H
      *    新規かどうかの判定
           MOVE    SPACE               TO  HELP-REC
           INITIALIZE                  HELP-REC
           MOVE    SPA-KHELP-HELPKBN   TO  HELP-HELPKBN
           MOVE    SPACE               TO  HELP-KEY-OPID
           MOVE    SPA-KHELP-LDNAME    TO  HELP-LDNAME
           MOVE    SPA-KHELP-MENUNUM1  TO  HELP-MENUNUM1
           MOVE    SPA-KHELP-MENUNUM2  TO  HELP-MENUNUM2
           MOVE    SPA-HOSPNUM         TO  HELP-HOSPNUM
      *
           MOVE    HELP-REC            TO  MCPDATA-REC
           MOVE    "tbl_help"          TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-HELP-READ-SEC
           ELSE
               INITIALIZE                  HELP-REC
               MOVE    1               TO  FLG-HELP
           END-IF
           PERFORM UNTIL       (FLG-HELP   =   1  )  OR
                               (IDX-H      >=  40 )
               ADD     1                   TO  IDX-H
               MOVE    HELP-HELP           TO  WRK-HELP (IDX-H)
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
           MOVE    SPACE               TO  WRK-HELP-AREA2
           MOVE    ZERO                TO  IDX-H2
           PERFORM VARYING     IDX-H   FROM   1   BY  1
                    UNTIL      IDX-H       >  40
               PERFORM VARYING     IDX-H1  FROM   1   BY  1
                        UNTIL      IDX-H1      >  50
                   ADD     1                   TO  IDX-H2
                   MOVE    WRK-HELP(IDX-H)(IDX-H1:1)
                                           TO  WRK-HELP-AREA2(IDX-H2:1)
                   IF      (WRK-HELP(IDX-H)(IDX-H1:1)  =   X"0A")
                       AND (IDX-H1             <   50   )
                       COMPUTE IDX-H3      =   IDX-H1  +  1
                       IF    WRK-HELP(IDX-H)(IDX-H3:)  =  SPACE
                            MOVE   50               TO  IDX-H1
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
           MOVE    WRK-HELP-AREA2      TO  WRK-HELP-AREA
      *
           .
       4021-MENUNUM2-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    キー入力処理
      *****************************************************************
       401-INPUT-CHK-SEC               SECTION.
      *
           MOVE    KHELP-INPUT         TO  SPA-KHELP-GMN-INPUT
      *
           IF     SPA-KHELP-GMN-INPUT  =   SPACE
               CONTINUE
               MOVE    1                   TO  FLG-GMN-INIT
               INITIALIZE                  TBL-HELP-AREA
               INITIALIZE                  SPA-KHELP-TBL-AREA
               MOVE    ZERO                TO  SPA-KHELP-TBLMAX
               MOVE    2                   TO  SPA-KHELP-CUR
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-KHELP-GMN-INPUT TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC      NOT =   ZERO  )  OR
                      (KANACHK-SYUBETU     =   1     )
                   IF      KANACHK-RC2         =   7
                       MOVE    KANACHK-OUT-INPUT
                                           TO  SPA-KHELP-GMN-INPUT
                   END-IF
                   MOVE    "0002"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-KHELP-CUR
               ELSE
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-KHELP-GMN-INPUT
               END-IF
      *
               IF      SPA-ERRCD           =   SPACE
                   MOVE    1                   TO  SPA-KHELP-PAGE
                   PERFORM 4011-HELP-KENSAKU-SEC
               END-IF
           END-IF
           .
       401-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    目次名称からの編集処理
      *****************************************************************
       4011-HELP-KENSAKU-SEC               SECTION.
      *
           MOVE    3                   TO  SPA-KHELP-SYORIFLG
      *    部分参照とする
           MOVE    ZERO                TO  IDX-K
           PERFORM VARYING     IDX     FROM    30  BY  -1
                   UNTIL       IDX     <       1
               IF      SPA-KHELP-GMN-INPUT(IDX:1)  NOT =   SPACE
                   MOVE    IDX                 TO  IDX-K
                   MOVE    1                   TO  IDX
               END-IF
           END-PERFORM
           MOVE    SPACE               TO  WRK-INPUT-MENU
           IF      IDX-K               =   30
               MOVE    SPA-KHELP-GMN-INPUT     TO  WRK-INPUT-MENU
           ELSE
               IF      IDX-K           =   29
                   MOVE    SPA-KHELP-GMN-INPUT TO  WRK-INPUT-MENU
                   MOVE    "%"                 TO  WRK-INPUT-MENU
                                                             (30:1)
               ELSE
                   COMPUTE IDX-K2              =   IDX-K   +  2
                   MOVE    "%"                 TO  WRK-INPUT-MENU
                                                             (1:1)
                   MOVE    SPA-KHELP-GMN-INPUT TO  WRK-INPUT-MENU
                                                             (2:)
                   MOVE    "%"                 TO  WRK-INPUT-MENU
                                                             (IDX-K2:)
               END-IF
           END-IF
      *
           MOVE    1                   TO  FLG-GMN-INIT
           INITIALIZE                  TBL-HELP-AREA
           INITIALIZE                  SPA-KHELP-TBL-AREA
           MOVE    ZERO                TO  SPA-KHELP-TBLMAX
      *
           MOVE    SPACE               TO  HELP-REC
           INITIALIZE                  HELP-REC
           MOVE    SPA-KHELP-HELPKBN   TO  HELP-HELPKBN
           MOVE    SPACE               TO  HELP-KEY-OPID
           MOVE    SPA-KHELP-LDNAME    TO  HELP-LDNAME
           MOVE    WRK-INPUT-MENU
                                       TO  HELP-MENU
           MOVE    SPA-HOSPNUM         TO  HELP-HOSPNUM
      *
           MOVE    HELP-REC            TO  MCPDATA-REC
           MOVE    "tbl_help"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-HELP-READ-SEC
           ELSE
               INITIALIZE                  HELP-REC
               MOVE    1               TO  FLG-HELP
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDX-ST
           IF      SPA-KHELP-PAGE      =   ZERO
               MOVE    1                   TO  IDX-ST
           ELSE
               COMPUTE IDX-ST          =   (SPA-KHELP-PAGE   -   1)
                                       *   WRK-TBLMAX
                                       +   1
           END-IF
           PERFORM
               UNTIL   (FLG-HELP           =   1  )  OR
                       (IDX                >=  WRK-TBLMAX)
               ADD     1                   TO  IDY
               IF      IDX-ST              <=  IDY
                   ADD     1                   TO  IDX
                   MOVE    IDY                 TO  SPA-KHELP-TRENNUM
                                                                 (IDX)
                   IF     (HELP-MENUNUM2       =   ZERO )  AND
                          (HELP-MENU2FLG       =   1    )
                      MOVE    "+"             TO  TBL-GMN-TMENUBUN
                                                                 (IDX)
                   END-IF
                   MOVE    HELP-MENU           TO  TBL-GMN-TMENU (IDX)
      *
                   MOVE    HELP-HELPKBN    TO  SPA-KHELP-THELPKBN (IDX)
                   MOVE    HELP-MENUNUM1   TO  SPA-KHELP-TMENUNUM1(IDX)
                   MOVE    HELP-MENUNUM2   TO  SPA-KHELP-TMENUNUM2(IDX)
                   IF     (HELP-MENUNUM2       =   ZERO )  AND
                          (HELP-MENU2FLG       =   1    )
                       MOVE    1               TO  SPA-KHELP-TEDANUM
                                                                (IDX)
                   ELSE
                       MOVE    ZERO            TO  SPA-KHELP-TEDANUM
                                                                (IDX)
                   END-IF
                   IF     (HELP-MENUNUM2       =   ZERO )  AND
                          (HELP-EDANUM         >   ZERO )
      *                目次１の時、目次２の有無判定
                       PERFORM 40111-MENUNUM2-CHK-SEC
                   END-IF
               END-IF
      *
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-HELP-READ-SEC
           END-PERFORM
           MOVE    "tbl_help"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    次頁判定
           IF     (IDX                 >=  300 )  AND
                  (FLG-HELP            =   ZERO)
               MOVE    1               TO  SPA-KHELP-NEXT
           ELSE
               MOVE    ZERO            TO  SPA-KHELP-NEXT
           END-IF
           MOVE    IDX                 TO  SPA-KHELP-TBLMAX
           MOVE    ZERO                TO  SPA-KHELP-SELNUM
      *
           IF      SPA-KHELP-TBLMAX    =   ZERO
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-KHELP-CUR
           ELSE
               IF      SPA-KHELP-TMENUNUM2 (1) >   ZERO
                   MOVE    SPA-KHELP-TRENNUM  (1)
                                               TO  SPA-KHELP-SELNUM
                   MOVE    SPA-KHELP-TMENUNUM1 (1)
                                               TO  SPA-KHELP-MENUNUM1
                   MOVE    SPA-KHELP-TMENUNUM2 (1)
                                               TO  SPA-KHELP-MENUNUM2
                   MOVE    SPA-KHELP-THELPKBN  (1)
                                               TO  SPA-KHELP-HELPKBN
      *            見出２
                   PERFORM 4021-MENUNUM2-SYORI-SEC
               END-IF
           END-IF
           MOVE    1                   TO  SPA-KHELP-CUR
      *
           .
       4011-HELP-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    目次２判定処理
      *****************************************************************
       40111-MENUNUM2-CHK-SEC            SECTION.
      *
           MOVE    HELP-REC            TO  WRK-HELP-REC
           MOVE    ZERO                TO  FLG-CHK
      *
           MOVE    HELP-REC            TO  MCPDATA-REC
           MOVE    "tbl_help"          TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-HELP-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HELP
           END-IF
           PERFORM UNTIL   (FLG-HELP   =   1   )  OR
                           (FLG-CHK    =   1   )
               IF      HELP-MENUNUM2   NOT =   ZERO
                   MOVE    1               TO  FLG-CHK
               ELSE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-HELP-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_help"          TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-CHK             =   1
               MOVE    "+"             TO  TBL-GMN-TMENUBUN(IDX)
               MOVE    ZERO            TO  SPA-KHELP-TEDANUM(IDX)
           END-IF
           MOVE    WRK-HELP-REC        TO  HELP-REC
      *
           .
       40111-MENUNUM2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           IF      SPA-ETC-MOTOPG      =   SPACE
               MOVE    "K02"               TO  SPA-SAKIPG
           ELSE
               MOVE    SPA-ETC-MOTOPG      TO  SPA-SAKIPG
           END-IF
           MOVE    "KHELP"             TO  SPA-MOTOPG
      *
           INITIALIZE                  SPA-HELP-AREA
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
      *    検索処理
      *****************************************************************
       404-KENSAKU-SEC               SECTION.
      *
           MOVE    2                   TO  SPA-KHELP-CUR
           .
       404-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    目次切替え処理
      *****************************************************************
       405-KIRIKAE-SEC       SECTION.
      *
           IF      SPA-KHELP-GMN-INPUT     =   SPACE
               IF      SPA-KHELP-SYORIFLG  =   2
                   MOVE    1                   TO  SPA-KHELP-SYORIFLG
                   MOVE    ZERO                TO  SPA-KHELP-SELNUM
                   MOVE    ZERO                TO  SPA-KHELP-MENUNUM1
                   MOVE    ZERO                TO  SPA-KHELP-MENUNUM2
                   MOVE    1                   TO  SPA-KHELP-PAGE
                   PERFORM 320-MENU-HENSYU-SEC
                   MOVE    1                   TO  SPA-KHELP-CUR
               END-IF
           ELSE
               IF      SPA-KHELP-SYORIFLG      =   2
                   MOVE    1                   TO  SPA-KHELP-SYORIFLG
                   MOVE    ZERO                TO  SPA-KHELP-SELNUM
                   MOVE    ZERO                TO  SPA-KHELP-MENUNUM1
                   MOVE    ZERO                TO  SPA-KHELP-MENUNUM2
                   MOVE    1                   TO  SPA-KHELP-PAGE
                   PERFORM 4011-HELP-KENSAKU-SEC
                   MOVE    1                   TO  SPA-KHELP-CUR
               END-IF
           END-IF
      *
           .
       405-KIRIKAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       406-PAGE-BACK-SEC       SECTION.
      *
           IF      SPA-KHELP-PAGE        =   1
               MOVE    "0004"          TO  SPA-ERRCD
           ELSE
               COMPUTE SPA-KHELP-PAGE  =   SPA-KHELP-PAGE   -   1
               IF     (SPA-KHELP-GMN-INPUT =   SPACE )  OR
                      (SPA-KHELP-SYORIFLG  NOT =   3 )
                   PERFORM 320-MENU-HENSYU-SEC
               ELSE
                   PERFORM 4011-HELP-KENSAKU-SEC
               END-IF
               MOVE    1                   TO  SPA-KHELP-CUR
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
           IF      SPA-KHELP-NEXT      =   ZERO
               MOVE    "0005"          TO  SPA-ERRCD
           ELSE
               ADD     1               TO  SPA-KHELP-PAGE
               IF     (SPA-KHELP-GMN-INPUT =   SPACE )  OR
                      (SPA-KHELP-SYORIFLG  NOT =   3 )
                   PERFORM 320-MENU-HENSYU-SEC
               ELSE
                   PERFORM 4011-HELP-KENSAKU-SEC
               END-IF
               MOVE    1                   TO  SPA-KHELP-CUR
           END-IF
      *
           .
       407-PAGE-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    標準処理
      *****************************************************************
       409-HYOUJUN-SEC               SECTION.
      *
           MOVE    1                   TO  SPA-KHELP-CUR
      *
           MOVE    SPACE               TO  SPA-KHELP-GMN-INPUT
           MOVE    1                   TO  SPA-KHELP-SYORIFLG
           MOVE    ZERO                TO  SPA-KHELP-HELPKBN
           MOVE    ZERO                TO  SPA-KHELP-MENUNUM1
           MOVE    ZERO                TO  SPA-KHELP-MENUNUM2
           MOVE    1                   TO  SPA-KHELP-PAGE
           PERFORM 320-MENU-HENSYU-SEC
           MOVE    ZERO                TO  SPA-KHELP-SELNUM
      *
           .
       409-HYOUJUN-EXT.
           EXIT.
      *****************************************************************
      *    自院処理
      *****************************************************************
       410-JIIN-SEC               SECTION.
      *
           MOVE    1                   TO  SPA-KHELP-CUR
      *
           MOVE    SPACE               TO  SPA-KHELP-GMN-INPUT
           MOVE    1                   TO  SPA-KHELP-SYORIFLG
           MOVE    "1"                 TO  SPA-KHELP-HELPKBN
           MOVE    ZERO                TO  SPA-KHELP-MENUNUM1
           MOVE    ZERO                TO  SPA-KHELP-MENUNUM2
           MOVE    1                   TO  SPA-KHELP-PAGE
           PERFORM 320-MENU-HENSYU-SEC
           MOVE    ZERO                TO  SPA-KHELP-SELNUM
      *
           .
       410-JIIN-EXT.
           EXIT.
      *
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "KHELP"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "目次名称は全角で入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "対象の目次がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "前頁はありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "次頁はありません。"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KERR
           MOVE    SPA-ERRCD           TO  KERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  KERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "KHELP"             TO  SPA-MOTOPG
           MOVE    "KERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    ヘルプマスター読込
      *****************************************************************
       900-HELP-READ-SEC         SECTION.
      *
           MOVE    "tbl_help"          TO  MCP-TABLE
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           MOVE    ZERO            TO  MCP-RC
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
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
