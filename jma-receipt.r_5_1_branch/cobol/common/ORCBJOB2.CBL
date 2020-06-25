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
       PROGRAM-ID.             ORCBJOB2.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ジョブ管理
      *  コンポーネント名  : 終了情報書き込み
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/05/30    NACL−太田    新規作成
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
           SELECT  MSTERR-FILE     ASSIGN  RECEERR
                                   FILE    STATUS IS  STS-MSTERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *    エラーファイル
       FD  MSTERR-FILE.
       01  MSTERR-REC              PIC X(200).
      *
       WORKING-STORAGE         SECTION.
grpsys*
grpsys COPY    "COMMON-SPA".
grpsys*
       COPY    "CPRECEERR.INC".
      *
       01  STS-AREA.
           03  STS-MSTERR              PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-JOBKANRI            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX1                    PIC 9(03). 
      *
       01  WRK-AREA.
           03  WRK-MSTERR              PIC X(200).
      *
      *    パラメタ領域
       01  WRK-PARA-AREA.
           03  WRK-PARA.
               05  WRK-PARA-MODE       PIC X(03).
               05  WRK-PARA-JOBID      PIC 9(07).
               05  WRK-PARA-SHELLID    PIC X(08).
               05  WRK-PARA-ERRCD      PIC X(04).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-ERRFILE        PIC X(100).
           03  WRK-PARA-SHELLMSG       PIC X(100).
           03  WRK-PARA-YOBI           PIC X(100).
           03  WRK-PARA-UPDCNT-X.
               05  WRK-PARA-UPDCNT     PIC 9(10).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(400).
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
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-TERM-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       SPA-AREA
                                       WRK-AREA
                                       WRK-PARA-AREA
      *
           MOVE    SPACE           TO  RECEERR
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-HOSPNUM
                                               RECEERR
                                               WRK-PARA-SHELLMSG
                                               WRK-PARA-YOBI
                                               WRK-PARA-UPDCNT
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      D    DISPLAY "ORCBJOB=" WRK-PARA-MODE "##"
      *
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
           MOVE    ZERO                TO  FLG-END
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
           IF    ( WRK-PARA-MODE       =   "STS" )
      *
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN    =   ZERO )
                   IF  ( JOB-STOPFLG       =   1    )
                       PERFORM 500-CAN-HENSYU-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF    ( FLG-END             =   ZERO )
               MOVE    WRK-PARA-MODE       TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               MOVE    WRK-PARA-ERRCD      TO  JOB-ERRCD
               MOVE    WRK-PARA-SHELLMSG   TO  JOB-SHELLMSG
               MOVE    WRK-PARA-YOBI       TO  JOB-YOBI
      *
               IF    ( WRK-PARA-MODE       =   "STE" )
                AND  ( WRK-PARA-UPDCNT-X   IS  NUMERIC )
                   MOVE    WRK-PARA-UPDCNT TO  JOB-UPDCNT
               END-IF
      *
               PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( WRK-PARA-MODE       =   "CHK" )
                AND  ( SJOBKANRI-RETURN    =   ZERO )
                AND  ( JOB-ENDYMD          =   SPACE )
                   MOVE    "処理が実行中です"
                                       TO  WRK-MSTERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ジョブキャンセル処理
      *****************************************************************
       500-CAN-HENSYU-SEC              SECTION.
      *
           MOVE    "処理が中止されました"
                                       TO  WRK-MSTERR
      *
           OPEN    INPUT   MSTERR-FILE
           IF    ( STS-MSTERR          =   ZERO )
               CLOSE   MSTERR-FILE
           ELSE
               OPEN    OUTPUT              MSTERR-FILE
      *
               MOVE    WRK-MSTERR      TO  MSTERR-REC
               WRITE   MSTERR-REC
               CLOSE   MSTERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "CAN"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               MOVE    "8888"              TO  JOB-ERRCD
               MOVE    WRK-MSTERR          TO  JOB-YOBI
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
               MOVE    2                   TO  JOB-STOPFLG
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           .
       500-CAN-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   MSTERR-FILE
           IF    ( STS-MSTERR          =   ZERO )
               CLOSE   MSTERR-FILE
           ELSE
               OPEN    OUTPUT              MSTERR-FILE
      *
               MOVE    WRK-MSTERR      TO  MSTERR-REC
               WRITE   MSTERR-REC
               CLOSE   MSTERR-FILE
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
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
