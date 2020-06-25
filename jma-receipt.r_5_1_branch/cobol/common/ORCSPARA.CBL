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
       PROGRAM-ID.                     ORCSPARA.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : パラメタ保存、読込、削除
      *  管理者            :
      *  作成日付    作業者        記述
      *  19/06/10    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *
       WORKING-STORAGE                 SECTION.
      *
       01  WRK-AREA.
           03  WRK-YMD                             PIC X(10). 
           03  WRK-ZOGENPTN                        PIC X(01).
           03  WRK-ZOGEN                           PIC S9(05).
           03  WRK-KEISANYMD                       PIC X(08).
           03  WRK-DELYMDHMS.
               05  WRK-DELYMDHMS-YMD               PIC X(08).
               05  WRK-DELYMDHMS-HMS               PIC X(06).
      *
       01  CONST-AREA.
           03  CONST-GYOUMUID          PIC X(04)   VALUE "0000".
           03  CONST-FILEMEI           PIC X(20)   VALUE "ORCSPARA".
           03  CONST-EDANUM            PIC 9(01)   VALUE 1.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ＳＰＡ一時データ
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
       01  UIDIO-AREA.
           03  UIDIO-RET               PIC X(2).
           03  UIDIO-UID               PIC X(36).
       01  UIDIO-ST                    PIC 9(2).
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                         SECTION.
           COPY    "CPORCSPARA.INC".
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       PROCEDURE                       DIVISION
           USING
           SPARA-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  WRK-AREA
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           PERFORM 990-EXIT-PROGRAM-SEC
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    ZERO            TO  SPARA-RC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           EVALUATE    SPARA-REQUEST
           WHEN  "SAVE"
               PERFORM 2001-SAVE-SEC
           WHEN  "LOAD"
               PERFORM 2001-LOAD-SEC
           WHEN  "UPDATE"
               PERFORM 2001-UPDATE-SEC
           WHEN  "LOAD_AND_DELETE"
               PERFORM 2001-LOAD-SEC
               PERFORM 2001-DELETE-SEC
           WHEN  "DELETE"
               PERFORM 2001-DELETE-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ保存処理
      *****************************************************************
       2001-SAVE-SEC                       SECTION.
      *
           INITIALIZE                      SPARA-ID
      *
           PERFORM 800-ORCUIDSET-SEC
      *
           INITIALIZE                      SPATMP-REC
      *
           PERFORM 20011-COMMON-PARA-SEC
      *
           MOVE    UIDIO-UID           TO  SPATMP-TERMID
           MOVE    SPARA-DATA          TO  SPATMP-DATA-REC
           MOVE    SMCNDATE-YMD        TO  SPATMP-UPYMD
           MOVE    SMCNDATE-HMS        TO  SPATMP-UPHMS
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBINSERT"          TO  MCP-FUNC
           PERFORM 20011-MONFUNC-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    UIDIO-UID       TO  SPARA-ID
           ELSE
               MOVE    MCP-RC          TO  SPARA-RC
           END-IF
      *
           .
       2001-SAVE-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ読込処理
      *****************************************************************
       2001-LOAD-SEC                       SECTION.
      *
           INITIALIZE                      SPARA-DATA
      *
           INITIALIZE                      SPATMP-REC
      *
           PERFORM 20011-COMMON-PARA-SEC
      *
           MOVE    SPARA-ID            TO  SPATMP-TERMID
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 20011-MONFUNC-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    "tbl_spa_tmp"   TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               MOVE    "DBFETCH"       TO  MCP-FUNC
               PERFORM 20011-MONFUNC-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE     SPATMP-DATA-REC
                                       TO  SPARA-DATA
               ELSE
                   MOVE    MCP-RC      TO  SPARA-RC
               END-IF
           ELSE
               MOVE    MCP-RC          TO  SPARA-RC
           END-IF
      *
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 20011-MONFUNC-SEC
      *
           .
       2001-LOAD-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ更新処理
      *****************************************************************
       2001-UPDATE-SEC                     SECTION.
      *
           INITIALIZE                      SPATMP-REC
      *
           PERFORM 20011-COMMON-PARA-SEC
      *
           MOVE    SPARA-ID            TO  SPATMP-TERMID
           MOVE    SPARA-DATA          TO  SPATMP-DATA-REC
           MOVE    SMCNDATE-YMD        TO  SPATMP-UPYMD
           MOVE    SMCNDATE-HMS        TO  SPATMP-UPHMS
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           PERFORM 20011-MONFUNC-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    MCP-RC          TO  SPARA-RC
           END-IF
      *
           .
       2001-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       2001-DELETE-SEC                 SECTION.
      *
           INITIALIZE                      SPATMP-REC
      *
           PERFORM 20011-COMMON-PARA-SEC
      *
           MOVE    SPARA-ID            TO  SPATMP-TERMID
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBDELETE"          TO  MCP-FUNC
           PERFORM 20011-MONFUNC-SEC
           MOVE    MCP-RC              TO  SPARA-RC
      *
           IF    ( SPARA-RC            =   ZERO )
            AND  ( SMCNDATE-HMS(6:1)   =   "0" )
               MOVE    SMCNDATE-YMD    TO  WRK-YMD
               COMPUTE WRK-ZOGEN   =   -2
               MOVE    "1"             TO  WRK-ZOGENPTN
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-KEISANYMD   TO  WRK-DELYMDHMS-YMD
               MOVE    SMCNDATE-HMS    TO  WRK-DELYMDHMS-HMS
               MOVE    WRK-DELYMDHMS   TO  SPATMP-DATA-REC
               MOVE    "tbl_spa_tmp"   TO  MCP-TABLE
               MOVE    "del5"          TO  MCP-PATHNAME
               MOVE    "DBDELETE"          TO  MCP-FUNC
               PERFORM 20011-MONFUNC-SEC
               MOVE    MCP-RC          TO  SPARA-RC
           END-IF
      *
           .
       2001-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    共通引数編集処理
      *****************************************************************
       20011-COMMON-PARA-SEC                   SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    CONST-GYOUMUID      TO  SPATMP-GYOUMUID
           MOVE    CONST-FILEMEI       TO  SPATMP-FILEMEI
           MOVE    CONST-EDANUM        TO  SPATMP-EDANUM
      *
           .
       20011-COMMON-PARA-EXT.
           EXIT.
      *****************************************************************
      *    ＭＯＮＦＵＮＣ呼出（SPATMP-REC指定）
      *****************************************************************
       20011-MONFUNC-SEC                 SECTION.
      *
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       SPATMP-REC
      *
           .
      *
       20011-MONFUNC-EXT.
           EXIT.
      *****************************************************************
      *    終了　処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    ＳＰＡ一時データ検索処理
      *****************************************************************
       900-SPATMP-KEY-SEL-SEC          SECTION.
      *
      *
           .
      *
       900-SPATMP-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    UID取得処理
      *****************************************************************
       800-ORCUIDSET-SEC               SECTION.
      *
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       UIDIO-ST
                                       UIDIO-AREA
      *
           .
       800-ORCUIDSET-EXT.
           EXIT.
      *****************************************************************
      *    EXIT PROGRAM
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
