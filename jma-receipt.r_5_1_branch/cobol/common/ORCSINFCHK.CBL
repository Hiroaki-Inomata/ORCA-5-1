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
       PROGRAM-ID.             ORCSINFCHK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 感染症サーベイランス処理チェック処理
      *  返却エラーコード  : 00:正常
      *                      10:データ作成なし(システム管理)
      *                      20:６時より前
      *                      30:データ作成処理中
      *                      40:データ作成処理済み
      *                      50:参加意思なし
      *                      90:データ作成処理中(開始日が当日でない)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09.12.09    伊藤          新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *  04.06.01    NACL-伊藤    11/01/28  リアルタイム送信
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-DASCONF         PIC 9(01).
      *
      *    ワーク領域
       01  WRK-AREA.
      *    ジョブ管理ＤＢのキー値（感染症データ）
           03  WRK-CONS-JOB-SHELLID-INFECT
                                   PIC X(08)   VALUE   "INFECTIO".
           03  WRK-CONS-JOB-JOBID-INFECT
                                   PIC 9(07)   VALUE   1.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    定点設定
       01  DASCF-REC.
           COPY    "CPDASCONF.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSINFCHK.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSINFCHKAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    ZERO                TO  INFCHK-RC
      *
      *    ジョブ管理チェック（データ収集）
           IF      SPA-DATAINFECTIONKBN    NOT =   2
               MOVE    10                  TO  INFCHK-RC
           END-IF
      *
      *    マシン日付取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF      INFCHK-RC           =   ZERO
      *        ６時以降？
               IF      SMCNDATE-HMS      <=    "060000"
                   MOVE    20                  TO  INFCHK-RC
               END-IF
           END-IF
      *
           IF      INFCHK-RC           =   ZERO
               PERFORM 100-JOBEXEC-CHK-SEC
           END-IF
      *
      **   IF      INFCHK-RC           =   ZERO
      **       PERFORM 200-AGREEMENT-CHK-SEC
      **   END-IF
           .
      *
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    感染症データ作成処理チェック処理
      *****************************************************************
       100-JOBEXEC-CHK-SEC         SECTION.
      *
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID-INFECT
                                       TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID-INFECT
                                       TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           IF      SJOBKANRI-RETURN    =   ZERO
               IF      JOB-ENDYMD          =   SPACE
                   MOVE    30              TO  INFCHK-RC
                   IF      JOB-STARTYMD    NOT =   SMCNDATE-YMD
                       MOVE    90          TO  INFCHK-RC
                   END-IF
               ELSE
                   IF      JOB-ENDYMD      =   SMCNDATE-YMD
                       MOVE    40          TO  INFCHK-RC
                   END-IF
               END-IF
           END-IF
           .
       100-JOBEXEC-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    収集データファイル有無チェック処理
      *****************************************************************
       200-AGREEMENT-CHK-SEC       SECTION.
      *
           PERFORM 900-DASCONF-READ-SEC
           IF      FLG-DASCONF         =   ZERO
               IF    ( DASCF-AGREEMENT      =   "1"   )  AND
                     ( DASCF-SENDRESULT     =   "000" )
                   CONTINUE
               ELSE
                   MOVE    50              TO  INFCHK-RC
               END-IF
           ELSE
               MOVE    50              TO  INFCHK-RC
           END-IF
           .
       200-AGREEMENT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    定点設定読込
      *****************************************************************
       900-DASCONF-READ-SEC            SECTION.
      *
           INITIALIZE                      DASCF-REC
           MOVE    SPA-HOSPNUM         TO  DASCF-HOSPNUM
           MOVE    DASCF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_das_conf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_das_conf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-DASCONF
                   MOVE    MCPDATA-REC         TO  DASCF-REC
               ELSE
                   MOVE    1                   TO  FLG-DASCONF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-DASCONF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_das_conf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DASCONF-READ-EXT.
           EXIT.
      *
