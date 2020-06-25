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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBJOBCHK1.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : ジョブ実行前チェック
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/11/16    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    RESULTファイル
           SELECT  RESULT-FILE     ASSIGN  RESULT
                                   FILE    STATUS  IS  STS-RESULT.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    出力ファイル
       FD  RESULT-FILE.
       01  RESULT-REC              PIC X(100).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPASGNPARA.INC"    REPLACING   //ASGNPARA//
                                       BY          //RESULT//.
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-RESULT              PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-RESULT              PIC 9(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSKEY.
               05  WRK-KANRICD         PIC  X(04).
               05  WRK-KBNCD           PIC  X(08).
           03  WRK-REC.
               05  WRK-RENKEI1         PIC X(01).
               05  WRK-RENKEI2         PIC X(01).
               05  WRK-RENKEI3         PIC X(01).
               05  WRK-RENKEI4         PIC X(01).
               05  WRK-RENKEI5         PIC X(01).
               05  WRK-RENKEI6         PIC X(01).
      *
       01  WRK-PARA.
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-JOBNAME        PIC X(20).
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    医療情報連携基盤用情報
           COPY    "CPSK9101.INC".
      *    レセプト編集区分
           COPY    "CPSK2005.INC".
           COPY    "CPSK200501.INC".
      *    医療情報代理機関情報
           COPY    "CPSK9104.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(400).
      *
      ******************************************************************
       PROCEDURE                   DIVISION
                                       USING
                                       COMMAND-PARAM.
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END         =       ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
           STOP    RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
      *
           INITIALIZE                  RESULT
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                               INTO    WRK-PARA-HOSPNUM
                                       WRK-PARA-JOBID
                                       WRK-PARA-SHELLID
                                       WRK-PARA-JOBNAME
                                       RESULT
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM
                                   TO  SPA-HOSPNUM
           MOVE    "T"             TO  SPA-SUPERVISOR
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RESULT          TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RESULT
      *
           PERFORM 100-DBOPEN-SEC
      *
           PERFORM 800-MCNDATE-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    SPACE           TO  WRK-REC
      *
           PERFORM 800-CHK-ORCSJOB-SEC
           IF    ( FLG-RESULT      =   ZERO )
               MOVE    "0"             TO  WRK-RENKEI1
           ELSE
               MOVE    "1"             TO  WRK-RENKEI1
           END-IF
      *
           EVALUATE    WRK-PARA-JOBNAME
               WHEN    "renkei1"
               WHEN    "renkei2"
                   MOVE    "0"             TO  WRK-RENKEI2
                   MOVE    "9101"          TO  WRK-KANRICD
                   MOVE    "*"             TO  WRK-KBNCD
                   PERFORM 900-SYSKANRI-READ-SEC
                   MOVE    SYSKANRI-REC    TO  SYS-9101-REC
                   IF    ( FLG-SYSKANRI    =   ZERO )
                       IF      WRK-PARA-JOBNAME    =   "renkei1"
                           MOVE    SYS-9101-COOPBASE
                                                   TO  WRK-RENKEI2
                       END-IF
                       IF      WRK-PARA-JOBNAME    =   "renkei2"
                           MOVE    SYS-9101-COOPBASE2
                                                   TO  WRK-RENKEI2
                       END-IF
                   END-IF
               WHEN    "mis"
                   MOVE    "0"             TO  WRK-RENKEI2
                                               WRK-RENKEI3
                   MOVE    "9104"          TO  WRK-KANRICD
                   MOVE    "*"             TO  WRK-KBNCD
                   PERFORM 900-SYSKANRI-READ-SEC
                   MOVE    SYSKANRI-REC    TO  SYS-9104-REC
                   IF    ( FLG-SYSKANRI    =   ZERO )
                       MOVE    SYS-9104-RECEDEN
                                               TO  WRK-RENKEI2
                       MOVE    SYS-9104-JMAMR  TO  WRK-RENKEI3
                   END-IF
               WHEN    "receipt"
                   MOVE    "0"             TO  WRK-RENKEI2
                                               WRK-RENKEI3
                                               WRK-RENKEI4
                                               WRK-RENKEI5
                                               WRK-RENKEI6
                   MOVE    "2005"          TO  WRK-KANRICD
                   MOVE    "*"             TO  WRK-KBNCD
                   PERFORM 900-SYSKANRI-READ-SEC
                   MOVE    SYSKANRI-REC    TO  SYS-2005-REC
                   IF    ( FLG-SYSKANRI    =   ZERO )
                       MOVE    SYS-2005-SYARECEKBN
                                               TO  WRK-RENKEI2
                       MOVE    SYS-2005-KKHRECEKBN
                                               TO  WRK-RENKEI3
                   END-IF
                   MOVE    "2005"          TO  WRK-KANRICD
                   MOVE    "01"            TO  WRK-KBNCD
                   PERFORM 900-SYSKANRI-READ-SEC
                   MOVE    SYSKANRI-REC    TO  SYS-200501-REC
                   IF    ( FLG-SYSKANRI    =   ZERO )
                       MOVE    SYS-200501-KOUIKIRECEKBN
                                               TO  WRK-RENKEI4
                       MOVE    SYS-200501-ROURECEKBN
                                               TO  WRK-RENKEI5
                       MOVE    SYS-200501-KOUIKIKBN
                                               TO  WRK-RENKEI6
                   END-IF
           END-EVALUATE
      *
           OPEN    OUTPUT              RESULT-FILE
           MOVE    WRK-REC         TO  RESULT-REC
           WRITE   RESULT-REC
           CLOSE   RESULT-FILE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC             SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       800-CHK-ORCSJOB-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-RESULT
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               IF    ( JOB-ENDTIME  NOT =   SPACE )
                AND  ( JOB-ENDYMD   NOT =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    1       TO  FLG-RESULT
               END-IF
           END-IF
      *
           .
       800-CHK-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    WRK-KANRICD     TO  SYS-KANRICD
           MOVE    WRK-KBNCD       TO  SYS-KBNCD
           MOVE    SMCNDATE-YMD    TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC       SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       100-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　切断処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
