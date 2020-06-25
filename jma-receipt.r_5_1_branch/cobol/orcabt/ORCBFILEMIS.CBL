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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCBFILEMIS.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 診療情報システム用ファイル情報管理の更新
      *                    : 主にクラウド環境でクライアント保存をするため
      *                    : CSVデータをmonblobへ格納し、ファイル情報、
      *                    : UUIDを格納する
      *  管理者            : 
      *  作成日付    作業者        記述
      *  18/02/01    NACL−伊藤    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  WRK-AREA.
           03  WRK-TITLE               PIC X(100).
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-TEISYUTUSAKI   PIC X(01).
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC X(02).
           03  WRK-PARA-NYUGAIKBN      PIC X(01).
           03  WRK-PARA-FOLDER         PIC X(200).
           03  WRK-PARA-FILENM         PIC X(100).
           03  WRK-PARA-COUNT          PIC 9(06).
           03  WRK-PARA-MONBLOB-ID     PIC X(37).
      *
       01  WRK-ERRMSG-AREA.
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-MCP-RC              PIC 9(9).
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *    ジョブ管理マスタ
      *01  JOBKANRI-REC.
      *    COPY    "CPJOBKANRI.INC".
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
           COPY    "CPORCSGETTEMP.INC".
      *    COPY    "CPORCSJOBKANRI.INC".
           COPY    "CPORCSFILEMISSV.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
           COPY    "ORCA-BLOB".
           COPY    "COMMON-SPA".
      *****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(1000).
      *****************************************************************
       PROCEDURE                   DIVISION
                                   USING   COMMAND-PARAM.
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
           PERFORM 200-MAIN-SEC
           PERFORM 300-END-SEC
           STOP    RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE  WRK-PARA
           INITIALIZE  SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID
                                               LNK-PRTKANRI-PRTNM
                                               WRK-PARA-TEISYUTUSAKI
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-FOLDER
                                               WRK-PARA-FILENM
                                               WRK-PARA-COUNT
                                               WRK-PARA-MONBLOB-ID
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *     DISPLAY "LNK-PRTKANRI-RENNUM       [ "
      *              LNK-PRTKANRI-RENNUM        " ]"
      *     DISPLAY "LNK-PRTKANRI-TBL-KEY      [ "
      *              LNK-PRTKANRI-TBL-KEY       " ]"
      *     DISPLAY "LNK-PRTKANRI-TBL-GROUP    [ "
      *              LNK-PRTKANRI-TBL-GROUP     " ]"
      *     DISPLAY "LNK-PRTKANRI-SHORI-RENNUM [ "
      *              LNK-PRTKANRI-SHORI-RENNUM  " ]"
      *     DISPLAY "LNK-PRTKANRI-SRYYM        [ "
      *              LNK-PRTKANRI-SRYYM         " ]"
      *     DISPLAY "LNK-PRTKANRI-SKYYMD       [ "
      *              LNK-PRTKANRI-SKYYMD        " ]"
      *     DISPLAY "LNK-PRTKANRI-SHELLID      [ "
      *              LNK-PRTKANRI-SHELLID       " ]"
      *     DISPLAY "LNK-PRTKANRI-PRIORITY     [ "
      *              LNK-PRTKANRI-PRIORITY      " ]"
      *     DISPLAY "LNK-PRTKANRI-TERMID       [ "
      *              LNK-PRTKANRI-TERMID        " ]"
      *     DISPLAY "LNK-PRTKANRI-OPID         [ "
      *              LNK-PRTKANRI-OPID          " ]"
      *     DISPLAY "LNK-PRTKANRI-PRTNM        [ "
      *              LNK-PRTKANRI-PRTNM         " ]"
      *     DISPLAY "WRK-PARA-TEISYUTUSAKI     [ "
      *              WRK-PARA-TEISYUTUSAKI      " ]"
      *     DISPLAY "WRK-PARA-JOBID            [ "
      *              WRK-PARA-JOBID             " ]"
      *     DISPLAY "WRK-PARA-SHELLID          [ "
      *              WRK-PARA-SHELLID           " ]"
      *     DISPLAY "WRK-PARA-HOSPNUM          [ "
      *              WRK-PARA-HOSPNUM           " ]"
      *     DISPLAY "WRK-PARA-NYUGAIKBN        [ "
      *              WRK-PARA-NYUGAIKBN         " ]"
      *     DISPLAY "WRK-PARA-FOLDER           [ "
      *              WRK-PARA-FOLDER            " ]"
      *     DISPLAY "WRK-PARA-FILENM           [ "
      *              WRK-PARA-FILENM            " ]"
      *     DISPLAY "WRK-PARA-COUNT            [ "
      *              WRK-PARA-COUNT             " ]"
      *     DISPLAY "WRK-PARA-MONBLOB-ID       [ "
      *              WRK-PARA-MONBLOB-ID        " ]"
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    MOVE    "STS"               TO  SJOBKANRI-MODE
      *    INITIALIZE  JOBKANRI-REC
      *    MOVE    "ORCBFILEMIS"       TO  JOB-PGID
      *    EVALUATE    WRK-PARA-SHELLID
      *        WHEN    "RENKEI1"
      *            MOVE    "連携基盤データ処理"
      *                                        TO  JOB-SHELLMSG
      *        WHEN    OTHER
      *            MOVE    "不明データ処理"    TO  JOB-SHELLMSG
      *    END-EVALUATE
      *    PERFORM 900-CALL-ORCSJOB-SEC
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    INITIALIZE  SGETTEMP-AREA
      *    MOVE    WRK-PARA-FILENM     TO  SGETTEMP-BASENAME
      *    CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
      *
           PERFORM 2001-FILE-MIS-INSERT-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ファイル保存情報更新処理
      *****************************************************************
       2001-FILE-MIS-INSERT-SEC    SECTION.
      *
           INITIALIZE  ORCSFILEMISSVAREA
           MOVE    "I"                 TO  FLMISSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  FLMISSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM TO  FLMISSV-RENNUM(1)
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  FLMISSV-TBL-GROUP(1)
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  FLMISSV-SHORI-RENNUM(1)
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  FLMISSV-SHELLID(1)
           MOVE    LNK-PRTKANRI-SRYYM  TO  FLMISSV-SRYYM(1)
           MOVE    LNK-PRTKANRI-SKYYMD TO  FLMISSV-SRYYMD(1)
           MOVE    WRK-PARA-COUNT      TO  FLMISSV-CNT(1)
           MOVE    WRK-PARA-NYUGAIKBN  TO  FLMISSV-NYUGAIKBN(1)
           MOVE    WRK-PARA-FOLDER     TO  FLMISSV-TO-FOLDER(1)
           MOVE    WRK-PARA-FILENM     TO  FLMISSV-TO-DATA(1)
           MOVE    "2"                 TO  FLMISSV-CODE-TYPE(1)
           MOVE    SPACE               TO  FLMISSV-PUTFLG(1)
           MOVE    SPACE               TO  WRK-TITLE
           EVALUATE    LNK-PRTKANRI-TBL-KEY
               WHEN    "RENKEI1"
                   PERFORM 20011-RENKEI1-TITLE-SEC
               WHEN    "MISRECE1"
                   PERFORM 20011-MISRECE1-TITLE-SEC
               WHEN    "MISJMAMR"
                   PERFORM 20011-MISJMAMR-TITLE-SEC
               WHEN    "MIGRATE1"
                   PERFORM 20011-MIGRATE1-TITLE-SEC
           END-EVALUATE
           MOVE    WRK-TITLE           TO  FLMISSV-TITLE(1)
           MOVE    WRK-PARA-MONBLOB-ID TO  FLMISSV-TBL-UUID(1)
      *
           CALL    "ORCSFILEMISSV" USING   ORCSFILEMISSVAREA
                                           SPA-AREA
           .
       2001-FILE-MIS-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    データ内容（タイトル）の編集（連携基盤）処理
      *****************************************************************
       20011-RENKEI1-TITLE-SEC     SECTION.
      *
           MOVE    "連携基盤・"        TO  WRK-TITLE
           IF      WRK-PARA-NYUGAIKBN  =   "1"
               STRING  WRK-TITLE           DELIMITED BY SPACE
                       "入院・"            DELIMITED BY SIZE
                       INTO    WRK-TITLE
               END-STRING
           ELSE
               STRING  WRK-TITLE           DELIMITED BY SPACE
                       "外来・"            DELIMITED BY SIZE
                       INTO    WRK-TITLE
               END-STRING
           END-IF
           IF      WRK-PARA-TEISYUTUSAKI   =   "1"
               STRING  WRK-TITLE           DELIMITED BY SPACE
                       "社保"              DELIMITED BY SIZE
                       INTO    WRK-TITLE
               END-STRING
           ELSE
               STRING  WRK-TITLE           DELIMITED BY SPACE
                       "国保"              DELIMITED BY SIZE
                       INTO    WRK-TITLE
               END-STRING
           END-IF
           IF      LNK-PRTKANRI-SHELLID    =   "recept4_renkei2.sh"
               STRING  WRK-TITLE           DELIMITED BY SPACE
                       "（当日受診分）"    DELIMITED BY SIZE
                       INTO    WRK-TITLE
               END-STRING
           END-IF
           .
       20011-RENKEI1-TITLE-EXT.
           EXIT.
      *****************************************************************
      *    データ内容（タイトル）の編集（レセ電）処理
      *****************************************************************
       20011-MISRECE1-TITLE-SEC    SECTION.
      *
           MOVE    "レセ電・"          TO  WRK-TITLE
           IF      WRK-PARA-NYUGAIKBN  =   "1"
               STRING  WRK-TITLE           DELIMITED BY SPACE
                       "入院・"            DELIMITED BY SIZE
                       INTO    WRK-TITLE
               END-STRING
           ELSE
               STRING  WRK-TITLE           DELIMITED BY SPACE
                       "外来・"            DELIMITED BY SIZE
                       INTO    WRK-TITLE
               END-STRING
           END-IF
           IF      LNK-PRTKANRI-RENNUM =   101 OR  102
                                       OR  111 OR  112
               STRING  WRK-TITLE           DELIMITED BY SPACE
                       "労災"              DELIMITED BY SIZE
                       INTO    WRK-TITLE
               END-STRING
           ELSE
               IF      WRK-PARA-TEISYUTUSAKI   =   "1"
                   STRING  WRK-TITLE           DELIMITED BY SPACE
                           "社保"              DELIMITED BY SIZE
                           INTO    WRK-TITLE
                   END-STRING
               ELSE
                   IF      WRK-PARA-TEISYUTUSAKI   =   "4"
                     STRING  WRK-TITLE           DELIMITED BY SPACE
                             "国保"              DELIMITED BY SIZE
                             INTO    WRK-TITLE
                     END-STRING
                   END-IF
               END-IF
           END-IF
           IF      LNK-PRTKANRI-RENNUM =   1   OR  11
                                       OR  101 OR  111
               STRING  WRK-TITLE           DELIMITED BY SPACE
                       "（日次）"          DELIMITED BY SIZE
                       INTO    WRK-TITLE
               END-STRING
           ELSE
               STRING  WRK-TITLE           DELIMITED BY SPACE
                       "（確定）"          DELIMITED BY SIZE
                       INTO    WRK-TITLE
               END-STRING
           END-IF
           .
       20011-MISRECE1-TITLE-EXT.
           EXIT.
      *****************************************************************
      *    データ内容（タイトル）の編集（データフォーマット）処理
      *****************************************************************
       20011-MISJMAMR-TITLE-SEC    SECTION.
      *
           MOVE    "診療情報・"        TO  WRK-TITLE
           IF      WRK-PARA-NYUGAIKBN  =   "1"
               STRING  WRK-TITLE           DELIMITED BY SPACE
                       "入院"              DELIMITED BY SIZE
                       INTO    WRK-TITLE
               END-STRING
           ELSE
               STRING  WRK-TITLE           DELIMITED BY SPACE
                       "外来"              DELIMITED BY SIZE
                       INTO    WRK-TITLE
               END-STRING
           END-IF
           IF      LNK-PRTKANRI-TBL-GROUP(1:8)
                                           =   LNK-PRTKANRI-SKYYMD
               STRING  WRK-TITLE           DELIMITED BY SPACE
                       "・当日分"          DELIMITED BY SIZE
                       INTO    WRK-TITLE
               END-STRING
           ELSE
               STRING  WRK-TITLE           DELIMITED BY SPACE
                       "（訂正分）"        DELIMITED BY SIZE
                       INTO    WRK-TITLE
               END-STRING
           END-IF
           .
       20011-MISJMAMR-TITLE-EXT.
           EXIT.
      *****************************************************************
      *    データ内容（タイトル）の編集（マスタデータ移行）処理
      *****************************************************************
       20011-MIGRATE1-TITLE-SEC    SECTION.
      *
           MOVE    SPACE               TO  WRK-TITLE
           EVALUATE    WRK-PARA-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    "点数マスタ"        TO  WRK-TITLE
               WHEN    "2"
                   MOVE    "チェックマスタ"    TO  WRK-TITLE
               WHEN    "3"
                   MOVE    "一般老人置換マスタ"
                                               TO  WRK-TITLE
               WHEN    "4"
                   MOVE    "保険者マスタ"      TO  WRK-TITLE
               WHEN    "5"
                   MOVE    "住所マスタ"        TO  WRK-TITLE
           END-EVALUATE
           .
       20011-MIGRATE1-TITLE-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *    MOVE    "STE"               TO  SJOBKANRI-MODE
      *    INITIALIZE  JOBKANRI-REC
      *    PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    エラー処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           INITIALIZE  WRK-ERRMSG-AREA
           MOVE    MCP-RC              TO  WRK-MCP-RC
      *
           STRING  "ORCBFILEMIS RC= "  DELIMITED   BY  SIZE
                   WRK-MCP-RC          DELIMITED   BY  SIZE
                   LOW-VALUE           DELIMITED   BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"      USING   WRK-ERRMSG
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC        SECTION.
      *
      *    MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
      *    MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
      *    MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
      *    CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
      *                                    JOBKANRI-REC
      *                                    SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC           SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-ORCDBMAIN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
