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
       PROGRAM-ID.             ORCGT03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 総括表・公費請求書
      *  コンポーネント名  : プリンタ設定
      *  管理者            : 
      *  06/06/05    NaCl−藤原   新規作成
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
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    レセプト作成指示共通パラメタ
           COPY    "T01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "T01SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).  
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
           03  IDXX                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-STR             PIC 9(05).
           03  WRK-SELNUM          PIC S9(05).
           03  WRK-RENNUM          PIC 9(04).
           03  WRK-NUM             PIC ZZ9.
           03  WRK-MOTOPG          PIC X(08).
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-GMN-PRTNM-G.
               05  WRK-GMN-PRTNM   PIC X(02).
               05  WRK-GMN-PRTNM-FIL
                                   PIC X(01).
               05  WRK-GMN-PRTNMMEI
                                   PIC X(16).
           03  WRK-PRTNMMEI        PIC X(16).
      *
       01  WRK-CONS-AREA.
      *    最大行
           03  WRK-CONS-SHELLIDLST-MAX
                                   PIC 9(03)   VALUE   30.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
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
           COPY    "ORCA44SCRAREA.INC".
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
                                       WRK-AREA
      *                                 
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-T01FREE
           MOVE    SPA-WORK        TO  SPA-T01KYOUTU
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
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-T01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-T01FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "TERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "T03    "           TO  MCP-WINDOW.
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
           PERFORM 310-SPASET-SEC
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-T03-KOUMOKU
                                       SPA-NAI-T03-KOUMOKU
      *
      *    プリンタ指定画面編集
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       SPA-3004-MAX     
               MOVE    IDZ             TO  SPA-GMN-T03-TRENNUM (IDZ)
               MOVE    SPA-3004-KBNCD   (IDZ)
                                       TO  SPA-NAI-T03-TKBNCD  (IDZ)
               MOVE    SPA-3004-SHELLID (IDZ) 
                                       TO  SPA-NAI-T03-TSHELLID(IDZ)
               MOVE    SPA-3004-SHORIMSG (IDZ) 
                                       TO  SPA-GMN-T03-TSHORIMSG
                                                               (IDZ)
               MOVE    SPA-3004-PRTNM   (IDZ)
                                       TO  SPA-GMN-T03-TPRTNM  (IDZ)
               MOVE    IDZ             TO  SPA-GMN-T03-SHELLID-MAX
           END-PERFORM
      *
           MOVE    1               TO  SPA-GMN-T03-CUR
                                       SPA-GMN-T03-PAGE
           .
       310-SPASET-EXT.
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
                   PERFORM 210-BACK
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 41001-SELNUM-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "PRTNM"
                   PERFORM 41002-PRTNM-CHK-SEC
      *
               WHEN    ANY             ALSO    "SHELLIDLST"
                   PERFORM 41020-SHELLIDLST-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象帳票選択処理
      *****************************************************************
       41001-SELNUM-CHK-SEC            SECTION.
      *
           MOVE    1                   TO  SPA-GMN-T03-CUR
      *
           INITIALIZE                      SPA-GMN-T03-KOUMOKU
                                           SPA-NAI-T03-KOUMOKU
      *
           MOVE    T03-SELNUM         TO  SPA-GMN-T03-SELNUM
      *
           IF      SPA-GMN-T03-SELNUM =   ZERO
               MOVE    99                  TO  SPA-GMN-T03-CUR
           ELSE
               MOVE    ZERO                TO  FLG-CHK  
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-T03-SHELLID-MAX
                       OR      FLG-CHK     =   1
                   IF      SPA-GMN-T03-SELNUM =
                                       SPA-GMN-T03-TRENNUM (IDX)
                       MOVE    IDX             TO  SPA-NAI-T03-SELNUM
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO                           
                   MOVE    "0001"          TO  SPA-ERRCD
               ELSE
                   PERFORM 410011-SHELLIDLST-SELECT-SEC
               END-IF
           END-IF
           .
       41001-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象帳票選択処理
      *****************************************************************
       410011-SHELLIDLST-SELECT-SEC    SECTION.
      *
           MOVE    SPA-GMN-T03-TSHORIMSG (SPA-NAI-T03-SELNUM)
                                       TO  SPA-GMN-T03-SHORIMSG
           MOVE    SPA-GMN-T03-TPRTNM    (SPA-NAI-T03-SELNUM)
                                       TO  SPA-GMN-T03-PRTNMMEI
           MOVE    SPA-GMN-T03-PRTNMMEI
                                       TO  WRK-PRTNMMEI
           PERFORM 5001-PRTNM-HENSYU-SEC
           MOVE    WRK-GMN-PRTNM-G     TO  SPA-GMN-T03-PRTNM-G
      *
           MOVE    2                   TO  SPA-GMN-T03-CUR
           .
       410011-SHELLIDLST-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プリンタチェック処理
      *****************************************************************
       41002-PRTNM-CHK-SEC          SECTION.
      *
           MOVE    2                   TO  SPA-GMN-T03-CUR
      *
           IF      SPA-NAI-T03-SELNUM =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-T03-CUR
           END-IF
      *         
           IF      SPA-ERRCD           =   SPACE
               MOVE    T03-PRTNM           TO  SPA-GMN-T03-PRTNM-G
      *
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       SPA-GMN-T03-PRTNM-MAX
                   IF      SPA-GMN-T03-PRTNM   =   SPA-GMN-T03-PRTNML
                                                               (IDXX)
                       MOVE    SPA-GMN-T03-PRTNMLST (IDXX)
                                              TO  SPA-GMN-T03-PRTNM-G
                       MOVE    SPA-GMN-T03-PRTNM-MAX
                                              TO  IDXX
                   END-IF
               END-PERFORM
               IF     SPA-GMN-T03-PRTNMMEI    =   SPACE
                   MOVE    "0002"          TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-GMN-T03-PRTNMMEI
                                           TO  SPA-GMN-T03-TPRTNM
                                               (SPA-NAI-T03-SELNUM)
      *
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >   SPA-GMN-T02-SHELLID-MAX
                   IF      SPA-NAI-T03-TKBNCD  (SPA-NAI-T03-SELNUM)
                                       =   SPA-NAI-T02-TKBNCD  (IDXX)
                       MOVE    SPA-GMN-T03-PRTNMMEI
                                               TO  SPA-NAI-T02-TPRTNM
                                                               (IDXX)
                       MOVE    SPA-GMN-T02-SHELLID-MAX
                                               TO  IDXX
                   END-IF
               END-PERFORM
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >   SPA-3004-MAX
                   IF      SPA-NAI-T03-TKBNCD  (SPA-NAI-T03-SELNUM)
                                       =   SPA-3004-KBNCD  (IDXX)
                       MOVE    SPA-GMN-T03-PRTNMMEI
                                               TO  SPA-3004-PRTNM
                                                            (IDXX)
                       MOVE    SPA-3004-MAX    TO  IDXX
                   END-IF
               END-PERFORM
      *
               INITIALIZE                      SPA-GMN-T03-KOUMOKU
                                               SPA-NAI-T03-KOUMOKU
               MOVE    1                   TO  SPA-GMN-T03-CUR
           END-IF
           .
       41002-PRTNM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象プリンタ選択処理
      *****************************************************************
       41020-SHELLIDLST-SEC       SECTION.
      *          
           MOVE    ZERO                TO  WRK-SELNUM
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-T03-SHELLID-MAX
               IF      T03-SHELLIDLST-SELECT (IDX) =   "T"
                   IF      IDX             =   SPA-NAI-T03-SELNUM
                       CONTINUE
                   ELSE        
                       MOVE    IDX                 TO  WRK-SELNUM
                       MOVE    SPA-GMN-T03-SHELLID-MAX
                                                   TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               INITIALIZE                      SPA-GMN-T03-KOUMOKU
                                               SPA-NAI-T03-KOUMOKU
      *
               MOVE    WRK-SELNUM          TO  SPA-NAI-T03-SELNUM
               MOVE    SPA-GMN-T03-TRENNUM (SPA-NAI-T03-SELNUM) 
                                           TO SPA-GMN-T03-SELNUM
      *
               PERFORM 410011-SHELLIDLST-SELECT-SEC
           END-IF
           .
       41021-BUNRUISEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
      *
           MOVE    "T01"               TO  SPA-SAKIPG
           MOVE    "T03"               TO  SPA-MOTOPG
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
      *    前頁処理
      *****************************************************************
       460-MAEGMN-SEC             SECTION.
      *
           IF      SPA-GMN-T03-PAGE        =   1
               GO  TO  460-MAEGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-T03-PAGE    =   SPA-GMN-T03-PAGE    -   1
      *
           MOVE    1                   TO  SPA-GMN-T03-CUR     
      *
           .
       460-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       470-ATOGMN-SEC             SECTION.
      *
           IF      SPA-GMN-T03-NEXT    =   ZERO
               GO  TO  470-ATOGMN-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-T03-PAGE
      *
           MOVE    1                   TO  SPA-GMN-T03-CUR     
      *
           .
       470-ATOGMN-EXT.
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
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "T03    "           TO  MCP-WINDOW
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
           INITIALIZE                  T03
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-T03-PRTNM-MAX
               MOVE    SPA-GMN-T03-PRTNMLST (IDX)
                                           TO  T03-PRTNM-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-T03-PRTNM-MAX
                                       TO  T03-PRTNM-COUNT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-T03-SHELLID-MAX
               MOVE    SPA-GMN-T03-TRENNUM (IDX)
                                       TO  WRK-NUM
               MOVE    WRK-NUM         TO  T03-SHELLIDLST-RENNUM  (IDX)
               MOVE    SPA-GMN-T03-TSHORIMSG (IDX)
                                       TO  T03-SHELLIDLST-SHORIMSG(IDX)
               MOVE    SPA-GMN-T03-TPRTNM (IDX)
                                       TO  T03-SHELLIDLST-PRTNM   (IDX)
               IF      IDX             =   SPA-NAI-T03-SELNUM
                   MOVE    "T"         TO  T03-SHELLIDLST-SELECT  (IDX)
               ELSE
                   MOVE    "F"         TO  T03-SHELLIDLST-SELECT  (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-T03-SHELLID-MAX
                                       TO  T03-SHELLIDLST-COUNT
           MOVE    ZERO                TO  T03-SHELLIDLST-ROW
      *
           MOVE    SPA-GMN-T03-SELNUM  TO  T03-SELNUM
           MOVE    SPA-GMN-T03-PRTNM-G
                                       TO  T03-PRTNM
           MOVE    SPA-GMN-T03-SHORIMSG
                                       TO  T03-SHORIMSG  
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
         EVALUATE    SPA-GMN-T03-CUR
             WHEN    1
                     MOVE  "SELNUM"    TO  MCP-WIDGET
             WHEN    2
                     MOVE  "PRTNM"     TO  MCP-WIDGET
             WHEN    99
                     MOVE  "B01"       TO  MCP-WIDGET
         END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
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
                   MOVE    "選択番号エラー"            TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "プリンタ名エラー"          TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  TERR
           MOVE    SPA-ERRCD            TO  TERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  TERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "T03"                TO  SPA-MOTOPG
           MOVE    "TERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "TERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    プリンタ編集処理
      *****************************************************************
       5001-PRTNM-HENSYU-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-GMN-PRTNM-G
      *
           IF      WRK-PRTNMMEI        =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       SPA-GMN-T03-PRTNM-MAX
                   IF      WRK-PRTNMMEI    =   SPA-GMN-T03-PRTNMMEIL
                                                               (IDXX)
                       MOVE    SPA-GMN-T03-PRTNMLST (IDXX)
                                              TO  WRK-GMN-PRTNM-G
                       MOVE    SPA-GMN-T03-PRTNM-MAX
                                              TO  IDXX
                   END-IF
               END-PERFORM
           END-IF
           .
       5001-PRTNM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC
           CALL   "ORCDBMAIN"       USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
