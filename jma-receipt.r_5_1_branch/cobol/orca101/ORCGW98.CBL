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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGW98.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 排他制御情報（Ｗ９８）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.03.00    NACL-多々納  06/10/24  初期設定修正
      *  03.04.00    NACL-多々納  07/01/23  排他削除修正
      *                                     MONFUNC対応
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
           COPY    "W98SCR-SPA".
      *01  SPA-W98.
      *    03  SPA-W98-AREA.
      *        05  SPA-GMN-PAGE        PIC 9(02).
      *        05  SPA-GMN-LINE        PIC 9(02).
      *        05  SPA-GMN-MAX         PIC 9(03).
      *        05  SPA-GMN-CUR         PIC 9(02).
      *
      *        05  SPA-GMN-AREA.
      *                                        
      *            07  SPA-GMN-HAITA-G.   
      *                09  SPA-GMN-HAITA
      *                                       PIC X(01).
      *                09  SPA-GMN-F1         PIC X(01).
      *                09  SPA-GMN-HAITAMEI
      *                                       PIC X(18).
      *                                        
      *            07  SPA-GMN-HAITALST      OCCURS  2.
      *                09  SPA-GMN-HAITAL
      *                                       PIC X(01).
      *                09  SPA-GMN-F1L        PIC X(01).
      *                09  SPA-GMN-HAITAMEIL
      *                                       PIC X(18).
      *            07  SPA-GMN-HAITA-MAX
      *                                       PIC 9(02).    
      *                                        
      *            07  SPA-GMN-FLG-TABLE.
      *              08  SPA-GMN-FLG-TABLE1    OCCURS  26.
      *                09  SPA-GMN-FLG-TABLE2  OCCURS  26.
      *                  10  SPA-GMN-FLG-VALUE PIC X(2).
      *            07  SPA-GMN-CHK-TABLE.
      *              08  SPA-GMN-CHK-TABLE1    OCCURS  26.
      *                09  SPA-GMN-CHK-TABLE2  OCCURS  26.
      *                  10  SPA-GMN-CHK-VALUE PIC X(1).
      *            07  SPA-GMN-CHK-ON-TABLE.
      *              08  SPA-GMN-CHK-ON-TABLE1    OCCURS  26.
      *                09  SPA-GMN-CHK-ON-VALUE   PIC X.
      *             
      *            07  SPA-GMN-YOKO-TABLE.
      *              08  SPA-GMN-YOKO-TABLE1    OCCURS  26.
      *                09  SPA-GMN-YOKO-VALUE   PIC X(3).
      *            07  SPA-GMN-TATE-TABLE.
      *              08  SPA-GMN-TATE-TABLE1    OCCURS  26.
      *                09  SPA-GMN-TATE-VALUE   PIC X(15).
      *             
      *        05  SPA-NAIB-AREA.
      *            07  SPA-LDNAME-TABLE.
      *              08  SPA-NAIB-LDNAME-TABLE1    OCCURS  26.
      *                09  SPA-NAIB-LDNAME-TABLE2  OCCURS  26.
      *                  10  SPA-NAIB-LDNAME-VALUE PIC X(20).
      *             
      *            07  SPA-NAIB-TATELD-TABLE.
      *              08  SPA-NAIB-TATELD-TABLE1    OCCURS  26.
      *                09  SPA-NAIB-TATELD-VALUE   PIC X(20).
      *             
      *        05  SPA-NAI-KAIJYO-CNT              PIC 9(02).
      *        05  SPA-NAI-KAIJYO-FLG              PIC 9(01).
      *        05  SPA-NAI-TATE-MAX                PIC 9(03).
      ******   05  SPA-NAI-YOKO-MAX                PIC 9(03).
      *
           COPY    "ENUM-VALUE".
      *
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-LDMAP           PIC 9(01).
           03  FLG-READ-SKIP       PIC 9(01).
      *
      *    カウント領域
      *
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-PATH            PIC X(64).
           03  WRK-SELNO           PIC 9(03).
           03  WRK-NUM1            PIC 9(02).
           03  WRK-NUM2            PIC 9(02).
           03  WRK-GMN-CUR         PIC 9(02).
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
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
       01  WRK-ADDRESS.
           03  WRK-ADDRESS1        PIC X(08).
           03  WRK-ADDRESS2        PIC 9(02).
       01  WRK-PORT.
           03  WRK-PORT1           PIC X(05).
           03  WRK-PORT2           PIC 9(02).
       01  WRK-MEMO.
           03  WRK-MEMO1           PIC X(05).
           03  WRK-MEMO2           PIC 9(02).
      *
       01  WRK-CONS-LINE-MAX       PIC 9(02)   VALUE   26.
       01  WRK-KBNCD               PIC 9(02).
      *             
       01  WRK-SPA.
           05  WRK-NAIB-AREA.
               07  WRK-LDNAME-TABLE.
                 08  WRK-NAIB-LDNAME-TABLE1    OCCURS  26.
                   09  WRK-NAIB-LDNAME-TABLE2  OCCURS  26.
                     10  WRK-NAIB-LDNAME-VALUE PIC X(20).
      *             
               07  WRK-NAIB-TATELD-TABLE.
                 08  WRK-NAIB-TATELD-TABLE1    OCCURS  26.
                   09  WRK-NAIB-TATELD-VALUE   PIC X(20).
      *             
                07  WRK-GMN-YOKO-TABLE.
                  08  WRK-GMN-YOKO-TABLE1    OCCURS  26.
                    09  WRK-GMN-YOKO-VALUE   PIC X(3).
                07  WRK-GMN-TATE-TABLE.
                  08  WRK-GMN-TATE-TABLE1    OCCURS  26.
                    09  WRK-GMN-TATE-VALUE   PIC X(15).
      *               
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    排他制御情報
           COPY  "CPSK9800.INC".
      *    排他制御情報（詳細）
           COPY  "CPLDMAP.INC".
      *    排他管理情報（詳細）
           COPY  "CPTBLLOCK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付サブルーチン領域
      *
           COPY  "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *     
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *D   COPY    "CPORCMCP.INC".
      *D   COPY    "ORCA-DBPATH".
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
      *
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W98
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
           MOVE    SPA-W98         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
           DISPLAY "take (1 1)= " SPA-NAIB-LDNAME-VALUE(1 1)
           DISPLAY "take (1 2)= " SPA-NAIB-LDNAME-VALUE(1 2)
           DISPLAY "take (2 1)= " SPA-NAIB-LDNAME-VALUE(2 1)
           DISPLAY "take (2 2)= " SPA-NAIB-LDNAME-VALUE(2 2)
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
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF     (FLG-END             =   1 )  OR
                  (SPA-ERRCD       NOT =   SPACE)
               CONTINUE
           ELSE
               MOVE    SPACE               TO  LINKAREA
      * 
               MOVE    SPACE                TO  MCP-PUTTYPE
               MOVE    "W98"                TO  MCP-WINDOW
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
           EVALUATE    SPA-WIDCD
               WHEN    "3001"
               PERFORM 490-KOUSHIN-SEC
               WHEN    "3002"
               PERFORM 470-LOCK-DEL-SEC
           END-EVALUATE.
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
           INITIALIZE   SPA-W98-AREA
           MOVE    SPACE               TO  WRK-SPA
           INITIALIZE                      WRK-SPA
      *     
           MOVE    "0"                 TO  SPA-GMN-HAITAL    (1)
           MOVE    "しない"            TO  SPA-GMN-HAITAMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-HAITAL    (2)
           MOVE    "する"              TO  SPA-GMN-HAITAMEIL (2)
           MOVE    2                   TO  SPA-GMN-HAITA-MAX 
      *     
           MOVE    SPACE               TO  SYS-9800-REC
           INITIALIZE                      SYS-9800-REC
           MOVE    "9800"              TO  SYS-9800-KANRICD
           MOVE    "*"                 TO  SYS-9800-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-9800-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-9800-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-9800-HOSPNUM
           MOVE    SYS-9800-REC        TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY"      TO  WRK-PATH
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-9800-REC
               MOVE    SYS-9800-HAITA  TO  SPA-GMN-HAITA
             IF      SPA-GMN-HAITA     =   "0"
               MOVE    "しない"        TO  SPA-GMN-HAITAMEI
             ELSE
               MOVE    "する"          TO  SPA-GMN-HAITAMEI
             END-IF             
           ELSE
               MOVE    "1"              TO  SPA-GMN-HAITA
               MOVE    "する"           TO  SPA-GMN-HAITAMEI
           END-IF             
      *
           MOVE    SPACE               TO  TBL-LDMAP
           INITIALIZE                      TBL-LDMAP
           MOVE    SPA-HOSPNUM         TO  LDMAP-HOSPNUM
           MOVE    TBL-LDMAP           TO  MCPDATA-REC
           MOVE    "tbl_ldmap"         TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ldmap"         TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-TBL-LDMAP-READ-SEC
               MOVE    MCPDATA-REC     TO  TBL-LDMAP
           ELSE
               INITIALIZE                  TBL-LDMAP
               MOVE    1               TO  FLG-LDMAP
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-LDMAP   =   1
                               OR      IDX         >= 
                                       WRK-CONS-LINE-MAX
               MOVE    MCPDATA-REC        TO  TBL-LDMAP
               ADD     1              TO  IDX
               PERFORM 3101-WRK-HAITA-MAKE-SEC
      *
               MOVE    "tbl_ldmap"         TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-TBL-LDMAP-READ-SEC
      *
               DISPLAY "take LDMAP READ"
           END-PERFORM
      *
           PERFORM 3102-SPA-HAITA-MAKE-SEC
      *    カーソルクロース
           MOVE    "tbl_ldmap"         TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *                                  
           MOVE    IDX                 TO  SPA-GMN-MAX
      *
           MOVE    1                    TO  SPA-GMN-CUR
           . 
       31O-SPASET-EXT.
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
                   MOVE   ZERO         TO  SPA-NAI-KAIJYO-CNT
                   MOVE   "CHANGE"     TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    強制変更
               WHEN    "CLICKED"       ALSO    "B10"
                   ADD    1            TO  SPA-NAI-KAIJYO-CNT
                   IF  SPA-NAI-KAIJYO-CNT      =  3
                     IF  SPA-NAI-KAIJYO-FLG    =  0
                         MOVE    "0002"        TO  SPA-ERRCD
                     END-IF
                       MOVE   1        TO  SPA-NAI-KAIJYO-FLG
                   END-IF
      *    情報削除
               WHEN    "CLICKED"       ALSO    "B11"
                   MOVE   ZERO         TO  SPA-NAI-KAIJYO-CNT
      *!!!!!       MOVE  "3002"        TO  SPA-WIDCD
                   PERFORM 480-W981-SYORI-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   MOVE   ZERO         TO  SPA-NAI-KAIJYO-CNT
                   PERFORM 490-TOUROKU-SEC
           END-EVALUATE
      *
      *    IF  SPA-NAI-KAIJYO-FLG      NOT  =  1
      *        GO     TO       200-GMNSENI-EXT
      *    END-IF
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET(1:4)
               WHEN    "CLICKED"       ALSO    "CHK_"
                   MOVE   ZERO         TO  SPA-NAI-KAIJYO-CNT
                 IF  SPA-NAI-KAIJYO-FLG      NOT  =  1
                     GO     TO       200-GMNSENI-EXT
                 END-IF
      *
                 IF    MCP-WIDGET(1:6) =       "CHK_ON"
                    DISPLAY "take MCP = " MCP-WIDGET
                 ELSE
                   MOVE   MCP-WIDGET(5:2)      TO  WRK-NUM1
                   MOVE   MCP-WIDGET(8:2)      TO  WRK-NUM2
                   PERFORM 480-HAITA-HENKO-SEC
                 END-IF
           END-EVALUATE
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "HAITAUMU"
                   PERFORM 4100-HAITAUMU-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *   排他制御テーブル生成処理 
      *****************************************************************
       3101-WRK-HAITA-MAKE-SEC    SECTION.
0     * 10        20        30        40        50        60
+---+-*--+----+----+----+----+----+----+----+----+----+----+----+
      *
               DISPLAY "take nai= " TBL-LDMAP  
           MOVE    LDMAP-LDNAME        TO
                                       WRK-NAIB-TATELD-VALUE (IDX) 
           MOVE    LDMAP-LDMSG         TO
                                       WRK-GMN-TATE-VALUE (IDX) 
           MOVE    LDMAP-LDMSG(1:3)    TO
                                       WRK-GMN-YOKO-VALUE (IDX) 
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       26
               MOVE    LDMAP-CHK-LDNAME (IDY)
                                       TO  
                                       WRK-NAIB-LDNAME-VALUE(IDX IDY)
           END-PERFORM
           .
       3101-WRK-HAITA-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *   排他制御テーブル生成処理 
      *****************************************************************
       3102-SPA-HAITA-MAKE-SEC    SECTION.
0     * 10        20        30        40        50        60
+---+-*--+----+----+----+----+----+----+----+----+----+----+----+
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       26
               MOVE    WRK-NAIB-TATELD-VALUE (IDX) 
                                       TO
                                       SPA-NAIB-TATELD-VALUE (IDX) 
               MOVE    WRK-GMN-TATE-VALUE (IDX) 
                                       TO
                                       SPA-GMN-TATE-VALUE (IDX) 
               MOVE    WRK-GMN-YOKO-VALUE (IDX) 
                                       TO
                                       SPA-GMN-YOKO-VALUE (IDX) 
               PERFORM 31021-SPA-YOKO-MAKE-SEC
               IF      WRK-NAIB-TATELD-VALUE (IDX)
                                       NOT =   SPACE
                       ADD   1         TO  SPA-NAI-TATE-MAX
               END-IF 
           END-PERFORM
           MOVE     SPA-NAI-TATE-MAX   TO  SPA-NAI-YOKO-MAX
      *
           .
       3102-SPA-HAITA-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *   排他制御テーブル生成処理 
      *****************************************************************
       31021-SPA-YOKO-MAKE-SEC    SECTION.
0     * 10        20        30        40        50        60
+---+-*--+----+----+----+----+----+----+----+----+----+----+----+
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       26
             IF    WRK-NAIB-LDNAME-VALUE(IDX IDY)
                                       NOT  =   SPACE
               PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       26
                 IF    WRK-NAIB-TATELD-VALUE(IDZ)
                                       =
                                       WRK-NAIB-LDNAME-VALUE(IDX IDY)
                 MOVE    "○"          TO  
                                       SPA-GMN-FLG-VALUE(IDX IDZ)
                 MOVE    "T"           TO  
                                       SPA-GMN-CHK-VALUE(IDX IDZ)
                 MOVE    WRK-NAIB-LDNAME-VALUE(IDX IDY)
                                       TO
                                       SPA-NAIB-LDNAME-VALUE(IDX IDZ)
                 MOVE    99            TO  
                                       IDZ
                 END-IF
               END-PERFORM
             END-IF
           END-PERFORM
           .
       31021-SPA-YOKO-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他有無区分チェック
      *****************************************************************
       4100-HAITAUMU-CHK-SEC       SECTION.
      *
           MOVE    1                      TO  SPA-GMN-CUR
      *
           MOVE    W98-HAITAUMU        TO  SPA-GMN-HAITA-G
      *
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-HAITA-MAX
               IF      SPA-GMN-HAITA
                                   =   SPA-GMN-HAITAL (IDY)
                   MOVE    SPA-GMN-HAITALST (IDY)
                                           TO  SPA-GMN-HAITA-G
                   MOVE    SPA-GMN-HAITA-MAX
                                           TO  IDY
               END-IF                                     
           END-PERFORM
      *
           IF      SPA-GMN-HAITAMEI =   SPACE
               MOVE    "0001"                  TO  SPA-ERRCD
           END-IF    
      *
           IF      SPA-ERRCD               =   SPACE
               MOVE    2                      TO  SPA-GMN-CUR
           END-IF
           .
       4100-HAITAUMU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W98"               TO  SPA-MOTOPG
      *
      *     INITIALIZE                  SPA-W01KYOUTU
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
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC              SECTION.
      *
           PERFORM 4900-KANRENCHK-SEC
           IF      SPA-ERRCD        =   SPACE
              MOVE    "3001"           TO  SPA-WIDCD
           END-IF
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
0     * 10        20        30        40        50        60
+---+-*--+----+----+----+----+----+----+----+----+----+----+----+
       480-HAITA-HENKO-SEC         SECTION.
      *
      * 同一業務の変更は認めない
      *    IF      WRK-NUM1            =   WRK-NUM2
      *            GO        TO        480-HAITA-HENKO-EXT
      *    END-IF
      *
      * 範囲外
           IF      WRK-NUM1            >   SPA-NAI-TATE-MAX
                   GO        TO        480-HAITA-HENKO-EXT
           END-IF
      *
           IF      WRK-NUM2            >   SPA-NAI-YOKO-MAX
                   GO        TO        480-HAITA-HENKO-EXT
           END-IF
      *
           IF      SPA-GMN-CHK-VALUE(WRK-NUM1 WRK-NUM2)
                                       =   "T"
                   MOVE  "F"           TO
                         SPA-GMN-CHK-VALUE(WRK-NUM1 WRK-NUM2)
                   MOVE  SPACE         TO
                         SPA-GMN-FLG-VALUE(WRK-NUM1 WRK-NUM2)
                   MOVE  SPACE
                                       TO
                         SPA-NAIB-LDNAME-VALUE(WRK-NUM1 WRK-NUM2)
                   MOVE  "F"           TO
                         SPA-GMN-CHK-VALUE(WRK-NUM2 WRK-NUM1)
                   MOVE  SPACE         TO
                         SPA-GMN-FLG-VALUE(WRK-NUM2 WRK-NUM1)
                   MOVE  SPACE
                                       TO
                         SPA-NAIB-LDNAME-VALUE(WRK-NUM2 WRK-NUM1)
           ELSE
                   MOVE  "T"           TO
                         SPA-GMN-CHK-VALUE(WRK-NUM1 WRK-NUM2)
                   MOVE  "○"          TO
                         SPA-GMN-FLG-VALUE(WRK-NUM1 WRK-NUM2)
                   MOVE  SPA-NAIB-TATELD-VALUE(WRK-NUM2)
                                       TO
                         SPA-NAIB-LDNAME-VALUE(WRK-NUM1 WRK-NUM2)
                   MOVE  "T"           TO
                         SPA-GMN-CHK-VALUE(WRK-NUM2 WRK-NUM1)
                   MOVE  "○"          TO
                         SPA-GMN-FLG-VALUE(WRK-NUM2 WRK-NUM1)
                   MOVE  SPA-NAIB-TATELD-VALUE(WRK-NUM1)
                                       TO
                         SPA-NAIB-LDNAME-VALUE(WRK-NUM2 WRK-NUM1)
           END-IF
           .
       480-HAITA-HENKO-EXT.
           EXIT.
      *
      *****************************************************************
      *    状況一覧画面へ
      *****************************************************************
       480-W981-SYORI-SEC           SECTION.
      *
      *    初期画面セット
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-W98             TO  SPA-FREE
           MOVE    SPA-W01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGW981"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-W98
           MOVE    SPA-WORK            TO  SPA-W01KYOUTU
      *
           MOVE    "W981"              TO  SPA-SAKIPG
           MOVE    "W98"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           IF      SPA-W981-LIST-MAX   =   ZERO
               MOVE    "B01"               TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM"            TO  MCP-WIDGET
           END-IF
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "W981"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       480-W981-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
      *    排他管理情報
           MOVE    SPACE               TO  SYS-9800-REC
           INITIALIZE                      SYS-9800-REC
           MOVE    SPA-HOSPNUM         TO  SYS-9800-HOSPNUM
           MOVE    "9800"              TO  SYS-9800-KANRICD
           MOVE    "99999999"          TO  SYS-9800-STYUKYMD
           MOVE    "00000000"          TO  SYS-9800-EDYUKYMD
           MOVE    SYS-9800-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    "1003"                  TO  SPA-ERRCD
               GO  TO  490-KOUSHIN-EXT
           END-IF
      *
      *    排他管理情報
           MOVE    SPACE               TO  SYS-9800-REC
           INITIALIZE                      SYS-9800-REC
           MOVE    "9800"              TO  SYS-9800-KANRICD
           MOVE    "*"                 TO  SYS-9800-KBNCD
           MOVE    "00000000"          TO  SYS-9800-STYUKYMD
           MOVE    "99999999"          TO  SYS-9800-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-9800-HOSPNUM
           MOVE    SPA-GMN-HAITA       TO  SYS-9800-HAITA
           MOVE    SYS-9800-REC        TO  SYSKANRI-REC
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    (  MCP-RC             NOT =   ZERO )
               MOVE    "1003"                  TO  SPA-ERRCD
               GO  TO  490-KOUSHIN-EXT
           END-IF
      *
           PERFORM 4901-LDMAP-KOUSHIN-SEC
      *    カーソルクロース
           MOVE    "tbl_ldmap"         TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK 
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *      ＭＡＰＤＢ更新処理 
      *****************************************************************
       4901-LDMAP-KOUSHIN-SEC      SECTION.
0     * 10        20        30        40        50        60        70
+---+-*--+----+----+----+----+----+----+----+----+----+----+----+----+
      *
           MOVE    SPACE               TO  TBL-LDMAP
           INITIALIZE                      TBL-LDMAP
           MOVE    SPA-HOSPNUM         TO  LDMAP-HOSPNUM
           MOVE    TBL-LDMAP           TO  MCPDATA-REC
           MOVE    "tbl_ldmap"         TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ldmap"         TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-TBL-LDMAP-READ-SEC
               MOVE    MCPDATA-REC     TO  TBL-LDMAP
           ELSE
               INITIALIZE              TBL-LDMAP
               MOVE    1               TO  FLG-LDMAP
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-LDMAP   =   1
                               OR      IDX         >= 
                                       WRK-CONS-LINE-MAX
               MOVE    MCPDATA-REC        TO  TBL-LDMAP
               ADD     1              TO  IDX
               PERFORM 49011-LDMAP-MAKE-SEC
      *
               MOVE    "tbl_ldmap"         TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-TBL-LDMAP-READ-SEC
           END-PERFORM
      *                                  
           .
       4901-LDMAP-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *      ＭＡＰＤＢ更新処理 
      *****************************************************************
       49011-LDMAP-MAKE-SEC        SECTION.
0     * 10        20        30        40        50        60        70
+---+-*--+----+----+----+----+----+----+----+----+----+----+----+----+
      *                                  
           MOVE    ZERO                TO  IDZ
           MOVE    SPACE               TO  LDMAP-CHK-LDMAP
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       26
             IF    SPA-NAIB-LDNAME-VALUE(IDX IDY)
                                       NOT  =   SPACE
                 ADD     1             TO   IDZ
                 MOVE    SPA-NAIB-LDNAME-VALUE(IDX IDY)
                                       TO  
                                       LDMAP-CHK-LDNAME(IDZ)
           DISPLAY "LDMAP = " LDMAP-CHK-LDNAME(IDZ)
             END-IF
           END-PERFORM
      *                                  
           MOVE    SPA-HOSPNUM         TO  LDMAP-HOSPNUM
           MOVE    TBL-LDMAP           TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_ldmap"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  FLG-LDMAP
           END-IF
      *                                  
           .
       49011-LDMAP-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC           SECTION.
      *
           MOVE    SPACE                TO  SPA-ERRCD
           MOVE    SPA-GMN-CUR          TO  WRK-GMN-CUR 
      *
           PERFORM 4100-HAITAUMU-CHK-SEC
      *
           IF      SPA-ERRCD            =   SPACE
               MOVE    WRK-GMN-CUR          TO  SPA-GMN-CUR
           END-IF             
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *      ＬＯＣＫＤＢ削除処理 
      *****************************************************************
       470-LOCK-DEL-SEC      SECTION.
0     * 10        20        30        40        50        60        70
+---+-*--+----+----+----+----+----+----+----+----+----+----+----+----+
      *                                  
           MOVE    SPACE               TO  TBL-LOCK
           INITIALIZE                      TBL-LOCK
           MOVE    SPA-HOSPNUM         TO  LOCK-HOSPNUM
           MOVE    TBL-LOCK            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  FLG-LDMAP
           END-IF
      *                                  
           .
       470-LOCK-DEL-EXT.
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
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W98    "           TO  MCP-WINDOW.
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
           MOVE    SPACE               TO   W98
           INITIALIZE                       W98
      *
           MOVE    SPA-GMN-HAITA-G     TO  W98-HAITAUMU
      *    MOVE    WIDGET-INSENSITIVE  TO  W98-FIXED2-STATE
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-HAITA-MAX
               MOVE    SPA-GMN-HAITALST (IDX)
                                       TO  W98-ITEM(IDX)
               MOVE    SPA-GMN-HAITA-MAX
                                       TO  W98-COUNT
           END-PERFORM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       26
               MOVE    SPA-GMN-YOKO-VALUE (IDX)
                                       TO  W98-YOKO-VALUE(IDX)
               MOVE    SPA-GMN-TATE-VALUE (IDX)
                                       TO  W98-TATE-VALUE(IDX)
             PERFORM VARYING IDY FROM    1   BY  1
                     UNTIL   IDY >       26
               MOVE    "whitebutton"   TO  W98-CHK-STYLE(IDX IDY)
               MOVE    SPA-GMN-FLG-VALUE (IDX IDY)
                                       TO  W98-FLG-VALUE(IDX IDY)
             END-PERFORM
           END-PERFORM
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
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
                   MOVE    "排他制御の入力に誤りがあります"
                                                         TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "排他情報の変更が可能です"
                                                         TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "全ての排他情報を削除します。"
                                                         TO  SPA-ERRMSG
               WHEN    "0099"
                   MOVE    "未入力の項目があります"
                                               TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "更新ができませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
               WHEN    "1003"
                   MOVE    "削除ができませんでした"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W98"                TO  SPA-MOTOPG
           MOVE    "WERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
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
           MOVE    SPACE               TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "3001"
                   MOVE    "排他制御情報を登録します"
                                                TO  WRK-WIDMSG
               WHEN    "3002"
                   STRING    "処理中の排他情報も全て削除します。"
                             "処理中の端末がないことを確認して下さい。"
                                                DELIMITED  BY  SIZE
                                                INTO   WRK-WIDMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
           EVALUATE    SPA-WIDCD
               WHEN    "3002"
                   MOVE    "B01"                TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "W98"                TO  SPA-MOTOPG
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
      *    画面カーソルセット処理    
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
          EVALUATE    SPA-GMN-CUR
                WHEN    1
                    MOVE  "HAITAUMU"       TO   MCP-WIDGET
                WHEN    2
                    MOVE  "B12"            TO   MCP-WIDGET
                WHEN    3
                    MOVE  "B12"            TO   MCP-WIDGET
                WHEN    4
                    MOVE  "B12"            TO   MCP-WIDGET
            END-EVALUATE
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           IF      WRK-YMD  (1:7)  NOT NUMERIC
               INSPECT     WRK-YMD      REPLACING   ALL " " BY  "0"
               MOVE    WRK-YMD(1:1)     TO  WRK-WGO
               MOVE    WRK-YMD(2:2)     TO  WRK-WYY
               MOVE    WRK-YMD(5:2)     TO  WRK-WMM
               MOVE    WRK-YMD(8:2)     TO  WRK-WDD
           ELSE
               MOVE    WRK-YMD(1:7)     TO  WRK-WYMD
           END-IF
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "12"                TO  LNK-DAY1-IRAI
           MOVE    ZERO                TO  LNK-DAY1-YMD
           MOVE    WRK-WYMD            TO  LNK-DAY1-YMD(2:7)
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
               MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御マスター読込（キー）
      *****************************************************************
       900-TBL-LDMAP-INV-SEC        SECTION.
      *
           MOVE    "tbl_ldmap"         TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ldmap"         TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-LDMAP
               ELSE
                   MOVE    1                   TO  FLG-LDMAP
               END-IF
           ELSE
               MOVE    1                   TO  FLG-LDMAP
           END-IF
      *
           MOVE    "tbl_ldmap"         TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-TBL-LDMAP-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御マスター読込（キー）
      *****************************************************************
       900-TBL-LDMAP-READ-SEC        SECTION.
      *
           MOVE    1                   TO  FLG-READ-SKIP
      *
           PERFORM UNTIL     ( FLG-READ-SKIP   =   ZERO )
                   OR        ( FLG-LDMAP   NOT =   ZERO )
               MOVE    "tbl_ldmap"         TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                     MOVE    ZERO          TO  FLG-LDMAP
                     MOVE    MCPDATA-REC   TO  TBL-LDMAP
      *
      *            無床診療所の場合、入院情報は読み飛ばす
                   IF    ( SPA-BEDSU       <=  ZERO )
                     AND ( LDMAP-GROUPNUM   =  2 )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-READ-SKIP
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-LDMAP
               END-IF
           END-PERFORM
      *
           .
       900-TBL-LDMAP-READ-EXT.
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
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
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
