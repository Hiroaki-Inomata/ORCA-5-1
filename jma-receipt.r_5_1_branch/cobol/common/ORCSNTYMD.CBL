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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCSNTYMD.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 入退院日取得
      *                      
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/09/18    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07.05.17  グループ診療対応
      *****************************************************************
      *
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
      *FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTNYUINRRK                  PIC 9(01).
           03  FLG-SKIP                        PIC 9(01).
           03  FLG-NYUINCHU                    PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                            PIC 9(05).
           03  IDX2                            PIC 9(05).
           03  IDX3                            PIC 9(05).
           03  IDXA                            PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD                        PIC X(08).
           03  WRK-ZOGENPTN                    PIC X(01).
           03  WRK-ZOGEN                       PIC S9(05).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                        SECTION.
      *
      *    入退院日取得サブパラメタ
           COPY    "CPORCSNTYMD.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                       DIVISION    USING
           ORCSNTYMDAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
      *    初期処理
           PERFORM 150-INIT-SEC
      *    主処理
           PERFORM 200-MAIN-SEC
      *    終了処理
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    パラメータチェック
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    処理区分が1,2,3のいずれかであること
           IF    ( SNTYMD-KBN          =   "1" OR "2" OR "3" )
               CONTINUE
           ELSE
               MOVE    1               TO  SNTYMD-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       150-INIT-SEC                    SECTION.
      *
           INITIALIZE                  SNTYMD-OT
      *
           .
       150-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SNTYMD-HOSPNUM      TO  PTNYUINRRK-HOSPNUM
           MOVE    SNTYMD-PTID         TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    1               TO  FLG-PTNYUINRRK
           ELSE
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
                    OR   ( SNTYMD-MAX         >=   50 )
      *
               PERFORM 2001-SKIP-CHK-SEC
      *
               IF    ( FLG-SKIP        =   ZERO )
                   PERFORM 2001-NTYMD-HEN-SEC
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              NOT =   ZERO )
                   MOVE    1               TO  FLG-PTNYUINRRK
               ELSE
                   MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    読み飛ばしチェック処理
      *****************************************************************
       2001-SKIP-CHK-SEC               SECTION.
      *
           MOVE    1                   TO  FLG-SKIP
      *
           EVALUATE    SNTYMD-KBN
           WHEN    "1"
               MOVE    ZERO            TO  FLG-SKIP
           WHEN    "2"
               IF    ( PTNYUINRRK-JTIKBN   NOT =   "5" )
                   MOVE    ZERO        TO  FLG-SKIP
               END-IF
           WHEN    "3"
               IF     ( PTNYUINRRK-JTIKBN  NOT =   "5" )
                  AND ( PTNYUINRRK-JTIKBN  NOT =   "6" )
                   MOVE    ZERO        TO  FLG-SKIP
               ELSE
                   IF    ( PTNYUINRRK-JTIKBN   =   "6" )
                    AND  ( PTNYUINRRK-NYUINCHUKBN
                                               =   "1" )
                       MOVE    ZERO        TO  FLG-SKIP
                   END-IF
               END-IF
           END-EVALUATE
      *
           .
       2001-SKIP-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入退院日編集処理
      *****************************************************************
       2001-NTYMD-HEN-SEC              SECTION.
      *
           MOVE    ZERO                    TO  FLG-NYUINCHU
           IF    ( PTNYUINRRK-JTIKBN   =   "6" )
            AND  ( PTNYUINRRK-NYUINCHUKBN
                                       =   "1" )
               MOVE    1                   TO  FLG-NYUINCHU
           END-IF
      *
           IF    ( SNTYMD-MAX          =   ZERO )
               IF    ( FLG-NYUINCHU    =    1  )
                   IF    ( SNTYMD-KBN  NOT =   "3" )
                       PERFORM 2001-NTYMD-SET-SEC
                   END-IF
               ELSE
                   PERFORM 2001-NTYMD-SET-SEC
               END-IF
           ELSE
               IF    ( FLG-NYUINCHU    =    1  )
                   MOVE    PTNYUINRRK-TAIINYMD TO  WRK-SYMD
                   MOVE    "1"                 TO  WRK-ZOGENPTN
                   MOVE    1                   TO  WRK-ZOGEN
                   PERFORM 800-CALENDAR-SEC
                   IF    ( WRK-SYMD            =   SNTYMD-NYUINYMD
                                                           (SNTYMD-MAX))
                    AND  ( PTNYUINRRK-SHONUM   =   SNTYMD-SHONUM
                                                           (SNTYMD-MAX))
                       MOVE    PTNYUINRRK-NYUINYMD
                                               TO  SNTYMD-NYUINYMD
                                                           (SNTYMD-MAX)
                   ELSE
                       IF    ( SNTYMD-KBN  NOT =   "3" )
                           PERFORM 2001-NTYMD-SET-SEC
                       END-IF
                   END-IF
               ELSE
                   PERFORM 2001-NTYMD-SET-SEC
               END-IF
           END-IF
      *
           .
       2001-NTYMD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入退院日設定処理
      *****************************************************************
       2001-NTYMD-SET-SEC              SECTION.
      *
           COMPUTE SNTYMD-MAX  =   SNTYMD-MAX  +   1
      *
           MOVE    SNTYMD-MAX          TO  IDXA
           MOVE    PTNYUINRRK-RRKNUM   TO  SNTYMD-RRKNUM  (IDXA)
           MOVE    PTNYUINRRK-NYUINYMD TO  SNTYMD-NYUINYMD(IDXA)
           MOVE    PTNYUINRRK-TAIINYMD TO  SNTYMD-TAIINYMD(IDXA)
           MOVE    PTNYUINRRK-SHONUM   TO  SNTYMD-SHONUM  (IDXA)
           MOVE    PTNYUINRRK-JTIKBN   TO  SNTYMD-JTIKBN  (IDXA)
      *
           .
      *
       2001-NTYMD-SET-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    ZERO                TO  SNTYMD-RC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
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
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           DISPLAY "ORCSNTYMD ERRCD [" SNTYMD-RC "]"
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
