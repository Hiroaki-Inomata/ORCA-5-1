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
       PROGRAM-ID.             ORCBMNT01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : マスタ更新
      *  コンポーネント名  : マスタ更新実行前チェック
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/08/29    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  MNTERRFL
                                   FILE    STATUS  IS  STS-RECEERR.
      *
      *    HOSPIDファイル
           SELECT  HOSPID-FILE     ASSIGN  HOSPIDFL
                                   FILE    STATUS  IS  STS-HOSPIDFL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
      *    エラーファイル
       FD  HOSPID-FILE.
       01  HOSPID-REC              PIC X(24).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC"     REPLACING   //RECEERR//
                                       BY          //MNTERRFL//.
      *
           COPY    "CPASGNPARA.INC"    REPLACING   //ASGNPARA//
                                       BY          //HOSPIDFL//.
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-HOSPIDFL        PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-JOBRUNNING      PIC 9(01).
           03  FLG-JOBMNT          PIC 9(01).
           03  FLG-JOBMLI          PIC 9(01).
      *
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR         PIC X(200).
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "COMMON-SPA".
     *
        01  WRK-PARA.
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *
       01  CONST-AREA.
           03  CONST-MNTJOBID      PIC 9(07)  VALUE    1.
           03  CONST-MNTSHELLID    PIC X(08)  VALUE    "MSTMNT".
           03  CONST-MLIJOBID      PIC 9(07)  VALUE    1.
           03  CONST-MLISHELLID    PIC X(08)  VALUE    "MSTMLI".
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
           COPY    "CPSK1001.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
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
      *
       PROCEDURE                       DIVISION
                                       USING
                                       COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END     =       ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
      *
           .
       000-PROC-EXT.
      *
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
           INITIALIZE                  MNTERRFL
                                       HOSPIDFL
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                               INTO    WRK-PARA-HOSPNUM
                                       MNTERRFL
                                       HOSPIDFL
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    "T"                 TO  SPA-SUPERVISOR
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    MNTERRFL        TO  SGETTEMP-BASENAMES (1)
           MOVE    HOSPIDFL        TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  MNTERRFL
           MOVE    SGETTEMP-FULLNAMES (2)
                                   TO  HOSPIDFL
      *
           PERFORM 100-DBOPEN-SEC
      *
           PERFORM 800-MCNDATE-SEC
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
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1001"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SMCNDATE-YMD    TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1001-REC
      *
           PERFORM 800-CHK-ORCSJOB-SEC
           IF    ( FLG-JOBRUNNING  =   1 )
               MOVE    "マスタ更新実行中です。"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF    ( FLG-END         =   ZERO )
               PERFORM 2001-JOBSTART-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ実行処理
      *****************************************************************
       2001-JOBSTART-SEC               SECTION.
      *
           PERFORM 800-CHK-ORCSJOB-SEC
      *
           IF    ( FLG-JOBMNT     =   ZERO )
               MOVE    "DEL"               TO  SJOBKANRI-MODE
               PERFORM 800-MNT-ORCSJOB-SEC
           END-IF
      *
           MOVE    "JBS"               TO  SJOBKANRI-MODE
           PERFORM 800-MNT-ORCSJOB-SEC
      *
           IF    ( FLG-JOBMLI     =   ZERO )
               MOVE    "DEL"               TO  SJOBKANRI-MODE
               PERFORM 800-MLI-ORCSJOB-SEC
           END-IF
      *
           MOVE    "JBS"               TO  SJOBKANRI-MODE
           PERFORM 800-MLI-ORCSJOB-SEC
      *
           .
      *
       2001-JOBSTART-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC              SECTION.
      *
           OPEN    INPUT               RECEERR-FILE
           IF    ( STS-RECEERR     =   ZERO )
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR     TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
           END-IF
      *
      *
      *    DISPLAY WRK-RECEERR
      *
           MOVE    1                   TO  FLG-END
           MOVE    1                   TO  FLG-ERR
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       800-CHK-ORCSJOB-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-JOBRUNNING
                                       FLG-JOBMNT
                                       FLG-JOBMLI
      *
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           PERFORM 800-MNT-ORCSJOB-SEC
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               IF    ( JOB-ENDTIME  NOT =   SPACE )
                AND  ( JOB-ENDYMD   NOT =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    1       TO  FLG-JOBRUNNING
               END-IF
           ELSE
                   MOVE    1       TO  FLG-JOBMNT
           END-IF
      *
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           PERFORM 800-MLI-ORCSJOB-SEC
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               IF    ( JOB-ENDTIME  NOT =   SPACE )
                AND  ( JOB-ENDYMD   NOT =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    1       TO  FLG-JOBRUNNING
               END-IF
           ELSE
                   MOVE    1       TO  FLG-JOBMLI
           END-IF
      *
           .
       800-CHK-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理編集処理
      *****************************************************************
       800-MNT-ORCSJOB-SEC             SECTION.
      *
           INITIALIZE                  JOBKANRI-REC
           MOVE    CONST-MNTJOBID      TO  JOB-JOBID
           MOVE    CONST-MNTSHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
       800-MNT-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理編集処理
      *****************************************************************
       800-MLI-ORCSJOB-SEC             SECTION.
      *
           INITIALIZE                  JOBKANRI-REC
           MOVE    CONST-MLIJOBID      TO  JOB-JOBID
           MOVE    CONST-MLISHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
       800-MLI-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF    ( FLG-ERR         =   ZERO )
               OPEN    OUTPUT      HOSPID-FILE
               MOVE    SYS-1001-HOSPID TO  HOSPID-REC
               WRITE   HOSPID-REC
               CLOSE   HOSPID-FILE
           END-IF
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
      
           .
       300-END-EXT.
           EXIT.
      *
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
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC       SECTION.
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
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
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
       910-DBCLOSECURSOR-SEC                SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
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
      *
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
