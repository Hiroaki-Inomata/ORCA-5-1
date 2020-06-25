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
       PROGRAM-ID.             ORCDBSPATMP.
      *****************************************************************
      *  システム名        : ORCA
      *  サブシステム名    : 
      *  コンポーネント名  : ORCA DB パラメタアクセスモジュール
      *  返却エラーコード  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  14/05/XX    NACL-多々納     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-ERR             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATHNAME            PIC X(64).
           03  WRK-FUNC                PIC X(64).
           03  WRK-TABLE               PIC X(64).
           03  WRK-RENNUMMAX           PIC 9(03).
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
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *    共通領域
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA2.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
           MCPAREA
           MCPDATA2-REC
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    MCP-TABLE           TO  WRK-TABLE
           MOVE    MCP-PATHNAME        TO  WRK-PATHNAME
           MOVE    MCP-FUNC            TO  WRK-FUNC
           MOVE    ZERO                TO  MCP-RC
      *
           EVALUATE    MCP-FUNC
               WHEN    "DBOPEN"
               WHEN    "DBDISCONNECT"
               WHEN    "DBSTART"
               WHEN    "DBCOMMIT"
               WHEN    "PUTWINDOW"
                   MOVE    LOW-VALUE       TO  MCP-TABLE
                                               MCP-PATHNAME
               WHEN    OTHER
                   IF  MCP-FUNC            =   "DBFETCH"
                       MOVE    LOW-VALUE   TO  MCPDATA2-REC
                   END-IF
                   IF    ( SPA-NOCHK-HOSPNUM   =   "1" )
                       CONTINUE
                   ELSE
                       IF    ( SPA-HOSPNUM     =   ZERO )
                        OR   ( SPA-HOSPNUM(1:2)    IS  NOT NUMERIC )
      *
                           MOVE   1            TO  FLG-ERR
                           MOVE   9            TO  MCP-RC
                       END-IF
                   END-IF
           END-EVALUATE
      *
           IF    ( FLG-ERR         =   ZERO )
      *
               MOVE    1           TO  MCP-VERSION
               MOVE    1           TO  MCP-REDIRECT
      *
               CALL    "MONFUNC"       USING
                                       MCPAREA
                                       MCPDATA2-REC
           END-IF
      *
           .
      *
           EXIT PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           .
      *
       100-INIT-EXT.
           EXIT.
