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
       PROGRAM-ID.                 ORCSHYOUKETU.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 標欠情報取得サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/08/10    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/05/09  グループ診療対応
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI                PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
      *
      *    コンスタント値
       01  CONSTANT-AREA.
      *    標欠（１００分の１０減額）
           03  CONS-HYOUKETSU1             PIC X(09) VALUE "190107890".
      *    標欠（１００分の１５減額）
           03  CONS-HYOUKETSU2             PIC X(09) VALUE "190107990".
      *    標欠（１００分の２減額）（離島等）
           03  CONS-HYOUKETSU3             PIC X(09) VALUE "190108690".
      *    標欠（１００分の３減額）（離島等）
           03  CONS-HYOUKETSU4             PIC X(09) VALUE "190108590".
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-SANTEIYM.
             05  WRK-SANTEI-YY             PIC 9(04).
             05  WRK-SANTEI-MM             PIC 9(02).
      *
           03  WRK-HYOUKETUKBN             PIC X(01).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK5000.INC".
      *
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    標欠取得用パラメタ
           COPY    "CPORCSHYOUKETU.INC".
      *
       PROCEDURE                   DIVISION    USING
      *     SPA-AREA
            ORCSHYOUKETU-AREA
            SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-HYOUKETU-HENKYAKU-AREA
           MOVE    ZERO            TO  LNK-HYOUKETU-RC
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-HYOUKETU-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    主処理
           IF      LNK-HYOUKETU-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
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
      *    算定年月チェック
           IF    ( LNK-HYOUKETU-SANTEIYM    =   SPACE )
               MOVE    1               TO  LNK-HYOUKETU-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主取得
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *標欠情報の取得（システム管理テーブルより）
           MOVE    SPACE               TO  WRK-HYOUKETUKBN
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-HYOUKETU-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF  FLG-SYSKANRI     =      ZERO
                   IF   SYS-5000-HYOUKETUKBN  =  SPACE OR "0"
                       CONTINUE
                   ELSE
                       MOVE  SYS-5000-HYOUKETUKBN  TO  WRK-HYOUKETUKBN
                       EVALUATE  SYS-5000-HYOUKETUKBN
                           WHEN  "1"
                               MOVE  CONS-HYOUKETSU1   TO  
                                                LNK-HYOUKETU-SRYCD(1)
                           WHEN  "2"
                               MOVE  CONS-HYOUKETSU2   TO  
                                                LNK-HYOUKETU-SRYCD(1)
                           WHEN  "3"
                               MOVE  CONS-HYOUKETSU3   TO  
                                                LNK-HYOUKETU-SRYCD(1)
                           WHEN  "4"
                               MOVE  CONS-HYOUKETSU4   TO  
                                                LNK-HYOUKETU-SRYCD(1)
                       END-EVALUATE
                       MOVE    LNK-HYOUKETU-SANTEIYM   TO
                                                LNK-HYOUKETU-STARTYM(1)
                       MOVE    SYS-5000-EDYUKYMD(1:6)   TO
                                                LNK-HYOUKETU-ENDYM(1)
                   END-IF
               ELSE
                   MOVE  2     TO    LNK-HYOUKETU-RC
               END-IF
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *標欠情報の取得（システム管理テーブルより翌月分を取得）
           MOVE    LNK-HYOUKETU-SANTEIYM  
                                       TO  WRK-SANTEIYM
           IF      LNK-HYOUKETU-SANTEIYM(5:2)  = "12"
               ADD    1                TO  WRK-SANTEI-YY
               MOVE   1                TO  WRK-SANTEI-MM
           ELSE
               ADD    1                TO  WRK-SANTEI-MM
           END-IF
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-SANTEIYM        TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF  FLG-SYSKANRI     =      ZERO
                   IF   SYS-5000-HYOUKETUKBN  =  SPACE OR "0"
                       CONTINUE
                   ELSE
                       IF    SYS-5000-HYOUKETUKBN  =  WRK-HYOUKETUKBN
                           CONTINUE
                       ELSE
                         EVALUATE  SYS-5000-HYOUKETUKBN
                           WHEN  "1"
                               MOVE  CONS-HYOUKETSU1   TO  
                                                LNK-HYOUKETU-SRYCD(2)
                           WHEN  "2"
                               MOVE  CONS-HYOUKETSU2   TO  
                                                LNK-HYOUKETU-SRYCD(2)
                           WHEN  "3"
                               MOVE  CONS-HYOUKETSU3   TO  
                                                LNK-HYOUKETU-SRYCD(2)
                           WHEN  "4"
                               MOVE  CONS-HYOUKETSU4   TO  
                                                LNK-HYOUKETU-SRYCD(2)
                         END-EVALUATE
                         MOVE    WRK-SANTEIYM   TO
                                                LNK-HYOUKETU-STARTYM(2)
                         MOVE    SYS-5000-EDYUKYMD(1:6)   TO
                                                LNK-HYOUKETU-ENDYM(2)
                       END-IF
                   END-IF
               ELSE
                   MOVE  2     TO    LNK-HYOUKETU-RC
               END-IF
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           IF    (LNK-HYOUKETU-SRYCD(1)    =   SPACE)   AND
                 (LNK-HYOUKETU-SRYCD(2)    =   SPACE)
                 MOVE   3        TO  LNK-HYOUKETU-RC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター読込
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-5000-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                      SYS-5000-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
