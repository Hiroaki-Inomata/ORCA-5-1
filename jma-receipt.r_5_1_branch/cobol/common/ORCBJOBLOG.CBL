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
       PROGRAM-ID.             ORCBJOBLOG.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ジョブ管理
      *  コンポーネント名  : 終了情報書き込み
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/11/21    NACL−竹田　  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       DATA                    DIVISION.
       WORKING-STORAGE             SECTION.
grpsys*
grpsys COPY    "COMMON-SPA".
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-SYSYMD-8.
               05  WRK-SYSYY           PIC 9(02).
               05  WRK-SYSYMD          PIC 9(06).
           03  WRK-TIME                PIC 9(08).
      *
       01  WRK-PARA.
           03  WRK-PARA-PROGNAME         PIC  X(20).
           03  WRK-PARA-KBNID            PIC  X(01).
           03  WRK-PARA-RESULT           PIC  X(01).
           03  WRK-PARA-OBJECTYYM        PIC  X(06).
           03  WRK-PARA-ERRCD            PIC  X(04).
           03  WRK-PARA-MSG              PIC  X(200).
           03  WRK-PARA-HOSPNUM          PIC  9(02).
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-JOBLOG                              PIC 9(01).
      *    ジョブ管理マスタ
       01  JOBLOG-REC.
           COPY    "CPJOBLOG.INC".
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      **************************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       FLG-AREA 
                                       WRK-AREA
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA-PROGNAME
                                               WRK-PARA-KBNID
                                               WRK-PARA-RESULT
                                               WRK-PARA-OBJECTYYM
                                               WRK-PARA-ERRCD
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-MSG
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           PERFORM 500-SYSYMD-HENSYU-SEC
           ACCEPT  WRK-TIME      FROM TIME
           PERFORM 100-DBOPEN-SEC
	   .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           DISPLAY "ORCBJOBLOG:" WRK-PARA-PROGNAME "##"
           DISPLAY "ORCBJOBLOG:" WRK-PARA-KBNID "##"
           DISPLAY "ORCBJOBLOG:" WRK-PARA-RESULT "##"
           DISPLAY "ORCBJOBLOG:" WRK-PARA-OBJECTYYM "##"
           DISPLAY "ORCBJOBLOG:" WRK-PARA-ERRCD "##"
           DISPLAY "ORCBJOBLOG:" WRK-PARA-MSG "##"
           DISPLAY "ORCBJOBLOG:" WRK-SYSYMD-8 "##"
           DISPLAY "ORCBJOBLOG:" WRK-TIME "##"
      *
           PERFORM 2001-JOBLOG-SET-SEC
      *
           IF    ( WRK-PARA-PROGNAME   =  "das-check"
                                      OR  "das-profile" )
           AND   ( WRK-PARA-KBNID      =  "D" )
               PERFORM 2002-JOBLOG-DELETE-1-SEC
           ELSE
      *
               PERFORM 2002-JOBLOG-READ-SEC
      *
               IF      FLG-JOBLOG          =  ZERO
                   PERFORM 2001-JOBLOG-SET-SEC
                   MOVE    "key"              TO  MCP-PATHNAME
                   MOVE    "DBUPDATE"         TO  MCP-FUNC
                   PERFORM 900-JOBLOG-ACC-SEC
               ELSE
                   PERFORM 2001-JOBLOG-SET-SEC
                   MOVE    "key"              TO  MCP-PATHNAME
                   MOVE    "DBINSERT"         TO  MCP-FUNC
                   PERFORM 900-JOBLOG-ACC-SEC
               END-IF
           END-IF
           .
       200-MAIN-EXT.
           EXIT.             
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.             
      *****************************************************************
      *    システム日付編集処理
      *****************************************************************
       500-SYSYMD-HENSYU-SEC       SECTION.
      *
           ACCEPT  WRK-SYSYMD          FROM DATE
           MOVE    20                  TO  WRK-SYSYY
      *
           .
       500-SYSYMD-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    JOBLOG明細セット
      *****************************************************************
       2001-JOBLOG-SET-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  JOBLOG-HOSPNUM
           MOVE    WRK-SYSYMD-8        TO  JOBLOG-LOGYMD
           MOVE    WRK-TIME            TO  JOBLOG-LOGTIME
           MOVE    WRK-PARA-PROGNAME   TO  JOBLOG-PROGNAME
           MOVE    WRK-PARA-KBNID      TO  JOBLOG-KBNID
           MOVE    WRK-PARA-RESULT     TO  JOBLOG-RESULT
           MOVE    WRK-PARA-OBJECTYYM  TO  JOBLOG-OBJECTYYM
           MOVE    WRK-PARA-ERRCD      TO  JOBLOG-ERRCD
           MOVE    WRK-PARA-MSG        TO  JOBLOG-MSG
      *
           .
       2001-JOBLOG-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    JOBLOG読込
      *****************************************************************
       2002-JOBLOG-READ-SEC        SECTION.
      *
           MOVE    JOBLOG-REC          TO  MCPDATA-REC
           MOVE    "tbl_joblog"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_joblog"       TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               MOVE    "DBFETCH"          TO  MCP-FUNC
               PERFORM 900-JOBLOG-ACC-SEC
           ELSE
               MOVE    1                   TO  FLG-JOBLOG
               INITIALIZE                      JOBLOG-REC
           END-IF
      *
           MOVE    "tbl_joblog"        TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2002-JOBLOG-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    JOBLOG削除
      *****************************************************************
       2002-JOBLOG-DELETE-1-SEC        SECTION.
      *
           MOVE    JOBLOG-REC          TO  MCPDATA-REC
           MOVE    "tbl_joblog"        TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           MOVE    "DBDELETE"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       2002-JOBLOG-DELETE-1-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-JOBLOG-ACC-SEC             SECTION.
      *
           MOVE    JOBLOG-REC          TO  MCPDATA-REC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JOBLOG-REC
               MOVE    ZERO                TO  FLG-JOBLOG
           ELSE
               MOVE    1                   TO  FLG-JOBLOG
               INITIALIZE                      JOBLOG-REC
           END-IF
      *
           .
       900-JOBLOG-ACC-EXT.
           EXIT.
      *
      *****************************************************************
      *    クローズ処理
      *****************************************************************
       900-CLOSE-SEC                  SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           MOVE    "DBSTART"           TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-PATH
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-PATH
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
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
