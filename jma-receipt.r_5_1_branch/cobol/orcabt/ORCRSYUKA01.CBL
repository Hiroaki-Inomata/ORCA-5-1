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
       PROGRAM-ID.             ORCRSYUKA01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 主科一括取得
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/12/01    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
      *
           03  FLG-READ                PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM      PIC X(06).
               05  WRK-PARA-NYUGAI     PIC X(01).
               05  WRK-PARA-PREFNUM    PIC X(02).
               05  WRK-PARA-JOBID      PIC 9(07).
               05  WRK-PARA-SHELLID    PIC X(08).
               05  WRK-PARA-HOSPNUM    PIC 9(02).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
           COPY    "MCPAREA".
      *     
           COPY    "ORCA-DBMETA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
           COPY  "CPSK2009.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    主科設定サブ
       01  ORCSSYUACCAREA.
           COPY    "CPORCSSYUACC.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1 
      *
           PERFORM 300-TERM-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       FLG-AREA
                                       SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            WRK-PARA-JOBID
                                       WRK-PARA-SHELLID
                                       WRK-PARA-HOSPNUM
                                       WRK-PARA-SRYYM
                                       WRK-PARA-NYUGAI
                                       WRK-PARA-PREFNUM
           END-UNSTRING 
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ＤＢオープン処理
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCRSYUKA01"   TO  JOB-PGID
           MOVE    "主科設定処理"
                                   TO  JOB-SHELLMSG
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    主科設定情報
           INITIALIZE                      SPA-SYUKA-INFO
           MOVE    SPACE               TO  SYS-2009-REC
           INITIALIZE                  SYS-2009-REC
           MOVE    "2009"              TO  SYS-2009-KANRICD
           MOVE    "*"                 TO  SYS-2009-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-2009-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2009-STYUKYMD (7:2)
           MOVE    SYS-2009-STYUKYMD   TO  SYS-2009-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-2009-HOSPNUM
           MOVE    SYS-2009-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      FLG-READ            =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-2009-REC
                   MOVE    SYS-2009-SYUKA-KBN  TO  SPA-SYUKA-KBN
                   MOVE    SYS-2009-SYUKA-NYUIN
                                               TO  SPA-SYUKA-NYUIN
                   MOVE    SYS-2009-SYUKA-GAIRAI
                                               TO  SPA-SYUKA-GAIRAI
                   MOVE    SYS-2009-SYUKA-MODE TO  SPA-SYUKA-MODE
                   MOVE    SYS-2009-SYUKA-SELECT
                                               TO  SPA-SYUKA-SELECT
               ELSE
                   MOVE    "0"                 TO  SPA-SYUKA-KBN
               END-IF
           ELSE
               MOVE    "0"                 TO  SPA-SYUKA-KBN
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF    WRK-PARA-NYUGAI       =  "2"
               INITIALIZE                      JYURRK-REC
grpsys         MOVE    WRK-PARA-HOSPNUM    TO  JYURRK-HOSPNUM
               MOVE    WRK-PARA-SRYYM      TO  JYURRK-SRYYMD(1:6)
               MOVE    "% "                TO  JYURRK-SRYYMD(7:)
               MOVE    WRK-PARA-NYUGAI     TO  JYURRK-NYUGAIKBN
               MOVE    JYURRK-REC          TO  MCPDATA-REC
               MOVE    "tbl_jyurrk"        TO  WRK-MCP-TABLE
               MOVE    "key34"             TO  WRK-MCP-PATHNAME
           ELSE
               INITIALIZE                      PTNYUINRRK-REC
grpsys         MOVE    WRK-PARA-HOSPNUM    TO  PTNYUINRRK-HOSPNUM
               MOVE    WRK-PARA-SRYYM      TO  PTNYUINRRK-TENSTUYMD(1:6)
               MOVE    "01"                TO  PTNYUINRRK-TENSTUYMD(7:)
               MOVE    WRK-PARA-SRYYM      TO  PTNYUINRRK-TENNYUYMD(1:6)
               MOVE    "31"                TO  PTNYUINRRK-TENNYUYMD(7:)
               MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
               MOVE    "tbl_ptnyuinrrk"    TO  WRK-MCP-TABLE
               MOVE    "key43"             TO  WRK-MCP-PATHNAME
           END-IF    
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               MOVE    FLG-READ            TO  FLG-END
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
      *
           PERFORM UNTIL       FLG-END     =   1
               PERFORM 20011-IKKATU-HENSYU-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       2001-IKKATU-SYORI-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    一括取得処理
      *****************************************************************
       20011-IKKATU-HENSYU-SEC              SECTION.
      *
           INITIALIZE                    PTINF-REC
           IF      WRK-PARA-NYUGAI       =  "2"
               MOVE    MCPDATA-REC          TO  JYURRK-REC
               MOVE    JYURRK-PTID          TO  PTINF-PTID
           ELSE
               MOVE    MCPDATA-REC          TO  PTNYUINRRK-REC
               MOVE    PTNYUINRRK-PTID      TO  PTINF-PTID
           END-IF
grpsys     MOVE    WRK-PARA-HOSPNUM         TO  PTINF-HOSPNUM
           PERFORM 900-PTINF-READ-SEC
      *
           IF      FLG-PTINF           =   ZERO
      *
               DISPLAY "PTID=" PTINF-PTID 
      *
               INITIALIZE                      ORCSSYUACCAREA
               MOVE    WRK-PARA-PREFNUM    TO  SPA-PREFNUM
               MOVE    WRK-PARA-SRYYM      TO  LNK-SYUACC-I-SRYYM
               MOVE    PTINF-BIRTHDAY      TO  SPA-BIRTHDAY
               MOVE    PTINF-PTID          TO  SPA-PTID
               MOVE    WRK-PARA-NYUGAI     TO  SPA-NYUGAIKBN
               MOVE   "05"                 TO  LNK-SYUACC-I-KBN
               CALL   "ORCSSYUKAMAIN"      USING   SPA-AREA
                                                   ORCSSYUACCAREA
     
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           MOVE    FLG-READ            TO  FLG-END
           .
       20011-IKKATU-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *     
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      FLG-READ            =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-READ
           ELSE
               MOVE    1                   TO  FLG-READ
           END-IF
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
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
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
