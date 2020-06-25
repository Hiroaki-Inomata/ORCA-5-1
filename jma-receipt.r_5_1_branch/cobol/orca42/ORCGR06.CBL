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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGR06.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト印刷指示
      *  コンポーネント名  : プリンタ一括設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/01/16    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
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
           COPY    "R01COMMON-SPA".
      *
      *    レセプト作成−印刷指示画面ＳＰＡ
           COPY    "R02SCR-SPA".
      *   
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDY                 PIC 9(04).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
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
           COPY    "ORCA42SCRAREA.INC".
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
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-R02
           MOVE    SPA-WORK        TO  SPA-R01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *        画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *        入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-R01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-R02         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "RERR"
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
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "R06"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
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
           INITIALIZE              SPA-R06-AREA
      *
      *    全体設定を初期設定
           MOVE    "T"             TO  SPA-GMN-R06-SETTEI1
           MOVE    "F"             TO  SPA-GMN-R06-SETTEI2
      *
           EVALUATE    SPA-R01-MOTOPG
               WHEN    "R02"
                   MOVE    SPA-R01-PRTNM-G
                                   TO  SPA-GMN-R06-PRTNM-G
                   MOVE    SPA-R01-PRTNM-N-G
                                   TO  SPA-GMN-R06-PRTNM-N-G
               WHEN    "R04"
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY >       SPA-GMN-PRTNM-MAX
                       IF      SPA-R01-PRTNMMEI
                                           =   SPA-GMN-PRTNMMEIL (IDY)
                           MOVE    SPA-GMN-PRTNMLST (IDY)
                                          TO  SPA-GMN-R06-PRTNM-G
                           MOVE    SPA-GMN-PRTNM-MAX
                                              TO  IDY
                       END-IF
                   END-PERFORM
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY >       SPA-GMN-PRTNM-MAX
                       IF      SPA-R01-PRTNM-NMEI
                                           =   SPA-GMN-PRTNMMEIL (IDY)
                           MOVE    SPA-GMN-PRTNMLST (IDY)
                                          TO  SPA-GMN-R06-PRTNM-N-G
                           MOVE    SPA-GMN-PRTNM-MAX
                                              TO  IDY
                       END-IF
                   END-PERFORM
           END-EVALUATE      
      *
           MOVE    1               TO  SPA-GMN-R06-CUR
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
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        設定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-SETTEI-SEC
      *        全体設定
               WHEN    "CLICKED"       ALSO    "SETTEI1"
                   PERFORM 400-SETTEI1-SEC
      *        頁設定
               WHEN    "CLICKED"       ALSO    "SETTEI2"
                   PERFORM 400-SETTEI2-SEC
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
               WHEN    "ACTIVATE"      ALSO    "PRTNM"
                   PERFORM 4110-PRTNM-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "PRTNM_N"
                   PERFORM 4120-PRTNM-N-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    プリンタ入力処理
      *****************************************************************
       4110-PRTNM-CHK-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-GMN-R06-CUR
      *
           MOVE    R06-PRTNM           TO  SPA-GMN-R06-PRTNM-G
      * 
           IF      SPA-GMN-R06-PRTNM   =   SPACE   
               MOVE    SPACE           TO  SPA-GMN-R06-PRTNM-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PRTNM-MAX
                   IF      SPA-GMN-R06-PRTNM
                                           =   SPA-GMN-PRTNML (IDY)
                       MOVE    SPA-GMN-PRTNMLST (IDY)
                                              TO  SPA-GMN-R06-PRTNM-G
                       MOVE    SPA-GMN-PRTNM-MAX
                                              TO  IDY
                   END-IF
               END-PERFORM
           END-IF
           IF      SPA-GMN-R06-PRTNMMEI   =   SPACE
               MOVE    SPA-GMN-PRTNMLST (1)   TO  SPA-GMN-R06-PRTNM-G
           END-IF    
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    2                   TO  SPA-GMN-R06-CUR
           END-IF 
           .
       4110-PRTNM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    プリンタ（続紙）入力処理
      *****************************************************************
       4120-PRTNM-N-CHK-SEC             SECTION.
      *
           MOVE    2                   TO  SPA-GMN-R06-CUR
      *
           MOVE    R06-PRTNM-N         TO  SPA-GMN-R06-PRTNM-N-G
      * 
           IF      SPA-GMN-R06-PRTNM-N =   SPACE   
               MOVE    SPACE           TO  SPA-GMN-R06-PRTNM-N-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PRTNM-MAX
                   IF      SPA-GMN-R06-PRTNM-N
                                           =   SPA-GMN-PRTNML (IDY)
                       MOVE    SPA-GMN-PRTNMLST (IDY)
                                              TO  SPA-GMN-R06-PRTNM-N-G
                       MOVE    SPA-GMN-PRTNM-MAX
                                              TO  IDY
                   END-IF
               END-PERFORM
           END-IF
           IF      SPA-GMN-R06-PRTNM-NMEI =   SPACE
               MOVE    SPA-GMN-PRTNMLST (1)   TO  SPA-GMN-R06-PRTNM-N-G
           END-IF    
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    199                 TO  SPA-GMN-R06-CUR
           END-IF 
           .
       4120-PRTNM-N-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    全体設定選択処理
      *****************************************************************
       400-SETTEI1-SEC          SECTION.
      *
           MOVE    R06-SETTEI1         TO  SPA-GMN-R06-SETTEI1
      *
           MOVE    1                   TO  SPA-GMN-R06-CUR
           .
       400-SETTEI1-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁設定選択処理
      *****************************************************************
       400-SETTEI2-SEC          SECTION.
      *
           MOVE    R06-SETTEI2         TO  SPA-GMN-R06-SETTEI2
      *
           MOVE    1                   TO  SPA-GMN-R06-CUR
           .
       400-SETTEI2-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPACE               TO  SPA-RID1-FLG
           MOVE    SPACE               TO  SPA-R01-PRTNM-G
                                           SPA-R01-PRTNM-N-G
      *
           MOVE    SPA-R01-MOTOPG      TO  SPA-SAKIPG
           MOVE    "R06"               TO  SPA-MOTOPG
     *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
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
      *    設定処理
      *****************************************************************
       450-SETTEI-SEC                  SECTION.
      *
           EVALUATE    TRUE
               WHEN    SPA-GMN-R06-SETTEI1 =   "T"
                   MOVE    "AL"                TO  SPA-RID1-FLG
               WHEN    SPA-GMN-R06-SETTEI2 =   "T"
                   MOVE    "PG"                TO  SPA-RID1-FLG
           END-EVALUATE
           MOVE    SPA-GMN-R06-PRTNM-G
                                       TO  SPA-R01-PRTNM-G
           MOVE    SPA-GMN-R06-PRTNM-N-G
                                       TO  SPA-R01-PRTNM-N-G
      *
           MOVE    SPA-R01-MOTOPG      TO  SPA-SAKIPG
           MOVE    "R06"               TO  SPA-MOTOPG
     *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       450-SETTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "R06"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE                      R06
      *
           EVALUATE    SPA-R01-MOTOPG
               WHEN    "R02"
                   MOVE    WIDGET-NORMAL   TO  R06-SETTEI1-STATE
                                               R06-SETTEI2-STATE
               WHEN    "R04"
                   MOVE    WIDGET-INSENSITIVE
                                           TO  R06-SETTEI1-STATE
                                               R06-SETTEI2-STATE
           END-EVALUATE      
           MOVE    SPA-GMN-R06-SETTEI1 TO  R06-SETTEI1
           MOVE    SPA-GMN-R06-SETTEI2 TO  R06-SETTEI2
      *
           MOVE    SPA-GMN-R06-PRTNM-G TO  R06-PRTNM
           MOVE    SPA-GMN-R06-PRTNM-N-G
                                       TO  R06-PRTNM-N
      *
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-PRTNM-MAX
               MOVE    SPA-GMN-PRTNMLST (IDY)
                                           TO  R06-PRTNMLST   (IDY)
                                               R06-PRTNM-NLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-PRTNM-MAX   TO  R06-PRTNM-COUNT
                                           R06-PRTNM-N-COUNT
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
           EVALUATE    SPA-GMN-R06-CUR
               WHEN    1
                   MOVE    "PRTNM"     TO  MCP-WIDGET
               WHEN    2
                   MOVE    "PRTNM_N"   TO  MCP-WIDGET
               WHEN    199
                   MOVE    "B12"       TO  MCP-WIDGET
           END-EVALUATE  
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
