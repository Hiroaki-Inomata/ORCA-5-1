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
       PROGRAM-ID.                     ORCHCM31S01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 支払金額編集サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/11/11    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田     07/05/24  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *
       WORKING-STORAGE                 SECTION.
      *
       01  FLG-AREA.
           03  FLG-SYUMEI              PIC 9(01).
           03  FLG-TAISYO              PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDX3                    PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-SIHARAI-AREA.
               05  WRK-SIHARAI-MAX     PIC 9(05).
               05  WRK-SIHARAI-OCC     OCCURS  100.
                   07  WRK-SIHARAI-YMD PIC X(08).
                   07  WRK-SIHARAI-KIN PIC S9(10).
                   07  WRK-SIHARAI-BFKIN PIC S9(10).
      *
       01  CONST-AREA.
           03  CONST-SIHARAI-MAX       PIC 9(05)   VALUE   100.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    収納明細
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                        SECTION.
      *
           COPY    "CPORCHCM31S01.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                       DIVISION    USING
           HCM31S01-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       WRK-AREA.
      *
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 2001-SYUMEI-HEN-SEC
      *
           PERFORM 2001-SIHARAI-HEN-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細編集処理
      *****************************************************************
       2001-SYUMEI-HEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
           MOVE    ZERO                TO  IDX1
           INITIALIZE                      WRK-SIHARAI-AREA
      *
           INITIALIZE                      SYUMEI-REC
           MOVE    HCM31S01-HOSPNUM    TO  SMEI-HOSPNUM
           MOVE    HCM31S01-NYUGAIKBN  TO  SMEI-NYUGAIKBN
           MOVE    HCM31S01-PTID       TO  SMEI-PTID
           MOVE    HCM31S01-DENPNUM    TO  SMEI-DENPNUM
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               MOVE    1               TO  FLG-SYUMEI
               INITIALIZE                  SYUMEI-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-SYUMEI  NOT =   ZERO )
                    OR   ( IDX1        >=  CONST-SIHARAI-MAX )
      *
               MOVE    ZERO            TO  FLG-TAISYO
               EVALUATE    HCM31S01-PROCKBN
               WHEN    "1"
                       MOVE    1       TO  FLG-TAISYO
               WHEN    "2"
                   IF    ( SMEI-NYUHEN-YMD <=  HCM31S01-EDYMD )
                       MOVE    1       TO  FLG-TAISYO
                   END-IF
               WHEN    "3"
                   IF    ( SMEI-NYUHEN-YMD >=  HCM31S01-STYMD )
                    AND  ( SMEI-NYUHEN-YMD <=  HCM31S01-EDYMD )
                       MOVE    1       TO  FLG-TAISYO
                   END-IF
               END-EVALUATE
      *
               IF    ( SMEI-NYUHEN-MONEY   =   ZERO )
                   MOVE    ZERO        TO  FLG-TAISYO
               END-IF
      *
               IF    ( FLG-TAISYO      =   1 )
                   COMPUTE IDX1    =   IDX1    +   1
                   MOVE    SMEI-NYUHEN-YMD
                                       TO  WRK-SIHARAI-YMD (IDX1)
                   IF    ( HCM31S01-SUMKBN =   "1" )
                       IF    ( SMEI-NYUHEN-YMD
                                       >=  HCM31S01-STYMD )
                           MOVE    SMEI-NYUHEN-MONEY
                                       TO  WRK-SIHARAI-KIN (IDX1)
                       ELSE
                           MOVE    SMEI-NYUHEN-MONEY
                                       TO  WRK-SIHARAI-BFKIN (IDX1)
                       END-IF
                   ELSE
                       MOVE    SMEI-NYUHEN-MONEY
                                       TO  WRK-SIHARAI-KIN (IDX1)
                   END-IF
               END-IF
      *
               MOVE    "tbl_syumei"    TO  MCP-TABLE
               MOVE    "key6"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  SYUMEI-REC
               ELSE
                   MOVE    1           TO  FLG-SYUMEI
                   INITIALIZE              SYUMEI-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSE-SEC
      *
           MOVE    IDX1                TO  WRK-SIHARAI-MAX
      *
           .
       2001-SYUMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    支払額編集処理
      *****************************************************************
       2001-SIHARAI-HEN-SEC            SECTION.
      *
           IF    ( HCM31S01-PROCKBN    =   "3" )
               PERFORM VARYING IDX1
                       FROM    WRK-SIHARAI-MAX BY  -1
                       UNTIL ( IDX1    <   2 )
      *
                   COMPUTE IDX2    =   IDX1    -   1
      *
                   IF    ( WRK-SIHARAI-KIN (IDX1)  <   ZERO )
                       COMPUTE WRK-SIHARAI-KIN (IDX2)
                           =   WRK-SIHARAI-KIN (IDX2)
                           +   WRK-SIHARAI-KIN (IDX1)
      *
                       MOVE    ZERO    TO  WRK-SIHARAI-KIN (IDX1)
      *
                   END-IF
      *
               END-PERFORM
      *
               IF    ( WRK-SIHARAI-KIN (1) <   ZERO )
                   MOVE    ZERO        TO  WRK-SIHARAI-KIN (1)
               END-IF
      *
               INITIALIZE                  HCM31S01-OT
               MOVE        ZERO        TO  IDX3
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   WRK-SIHARAI-MAX )
      *
                   IF    ( WRK-SIHARAI-KIN (IDX0)  >   ZERO )
      *
                       COMPUTE IDX3    =   IDX3    +   1
      *
                       MOVE    WRK-SIHARAI-YMD (IDX0)
                                       TO  HCM31S01-NYKNYMD(IDX3)
                       MOVE    WRK-SIHARAI-KIN (IDX0)
                                       TO  HCM31S01-KINGAKU(IDX3)
                   END-IF
      *
               END-PERFORM
      *
               MOVE    IDX3            TO  HCM31S01-OT-MAX
      *
           ELSE
      *
               INITIALIZE                  HCM31S01-OT
      *
               MOVE    HCM31S01-SRYYMD TO  HCM31S01-NYKNYMD (1)
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   WRK-SIHARAI-MAX )
      *
                   COMPUTE HCM31S01-KINGAKU (1)
                       =   HCM31S01-KINGAKU (1)
                       +   WRK-SIHARAI-KIN (IDX0)
      *
                   COMPUTE HCM31S01-BFKINGAKU (1)
                       =   HCM31S01-BFKINGAKU (1)
                       +   WRK-SIHARAI-BFKIN (IDX0)
      *
               END-PERFORM
      *
               MOVE    1               TO  HCM31S01-OT-MAX
      *
           END-IF
      *
           .
       2001-SIHARAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
           .
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSE-EXT.
           EXIT.
           .
