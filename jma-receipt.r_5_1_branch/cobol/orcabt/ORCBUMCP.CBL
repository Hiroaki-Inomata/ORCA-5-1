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
       PROGRAM-ID.             ORCBUMCP.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : マスタチェック
      *  管理者            : 森脇
      *  作成日付    作業者        記述
      *  19/03/06    NACL-森脇     新規作成
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
      *    エラーファイル
           SELECT  BUMCPERR-FILE   ASSIGN  BUMCP-ERRFNAME
                                   FILE    STATUS  IS  STS-BUMCPERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  BUMCPERR-FILE.
       01  BUMCPERR-REC            PIC X(200).
      *
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPCOMMONDAT3.INC".
      *
       01  STS-AREA.
           03  STS-BUMCPERR            PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-ERR                 PIC 9(01).
      *
       01  WRK-AREA.
           03  WRK-ERRMSG              PIC X(200).
      *
      *    パラメタ領域
       01  BUMCP-AREA.
           COPY    "CPORCSPRTLNK.INC".
           COPY    "CPBUMCPPARA.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
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
           02  FILLER                  PIC X(256).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
                                       USING
                                       COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-ERR         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
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
           INITIALIZE              BUMCP-AREA
                                   SPA-AREA
                                   STS-AREA
                                   WRK-AREA
                                   FLG-AREA
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            LNK-PRTKANRI-RENNUM
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
                                       BUMCP-JOBID
                                       BUMCP-SHELLID
                                       BUMCP-HOSPNUM
                                       BUMCP-ERRFNAME
                                       BUMCP-MSTKBN
                                       BUMCP-CSVKBN
                                       BUMCP-MSTYMD
           END-UNSTRING
      *
           MOVE    BUMCP-HOSPNUM       TO  SPA-HOSPNUM
           MOVE    LNK-PRTKANRI-TERMID TO  SPA-TERMID
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPA-SYSYMD
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    BUMCP-ERRFNAME      TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  BUMCP-ERRFNAME

      *
           EVALUATE    BUMCP-MSTKBN
               WHEN    "1"
                   MOVE    "点数ユーザーマスタチェックリスト"
                                           TO  BUMCP-SHELLMSG
               WHEN    "2"
                   MOVE    "チェックユーザーマスタチェックリスト"
                                           TO  BUMCP-SHELLMSG
               WHEN    "3"
                   MOVE    "一般老人置換ユーザーマスタチェックリスト"
                                           TO  BUMCP-SHELLMSG
               WHEN    "4"
                   MOVE    "保険者ユーザーマスタチェックリスト"
                                           TO  BUMCP-SHELLMSG
               WHEN    "5"
                   MOVE    "住所ユーザーマスタチェックリスト"
                                           TO  BUMCP-SHELLMSG
           END-EVALUATE
      *
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           PERFORM 800-ORCSJOB-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    点数マスタ
           IF    ( BUMCP-RC     =   ZERO )
               IF    ( BUMCP-MSTKBN        =     "1" )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCBUMCTENSU"
                   CALL    "ORCBUMCTENSU"      USING
                                               BUMCP-AREA
                                               SPA-AREA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCBUMCTENSU"
               END-IF
           END-IF
      *
      *    チェクマスタ
           IF    ( BUMCP-RC     =   ZERO )
               IF    ( BUMCP-MSTKBN        =     "2" )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCBUMCCHK"
                   CALL    "ORCBUMCCHK"        USING
                                               BUMCP-AREA
                                               SPA-AREA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCBUMCCHK"
               END-IF
           END-IF
      *
      *    一般・老人コードマスタ
           IF    ( BUMCP-RC     =   ZERO )
               IF    ( BUMCP-MSTKBN        =     "3" )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCBUMCSRYCDCHG"
                   CALL    "ORCBUMCSRYCDCHG"   USING
                                               BUMCP-AREA
                                               SPA-AREA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCBUMCSRYCDCHG"
               END-IF
           END-IF
      *
      *    保険者マスタ
           IF    ( BUMCP-RC     =   ZERO )
               IF    ( BUMCP-MSTKBN        =     "4" )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCBUMCHKNJAINF"
                   CALL    "ORCBUMCHKNJAINF"   USING
                                               BUMCP-AREA
                                               SPA-AREA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCBUMCHKNJAINF"
               END-IF
           END-IF
      *
      *    住所マスタ
           IF    ( BUMCP-RC     =   ZERO )
               IF    ( BUMCP-MSTKBN        =     "5" )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCBUMCADRS"
                   CALL    "ORCBUMCADRS"       USING
                                               BUMCP-AREA
                                               SPA-AREA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCBUMCADRS"
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF    ( BUMCP-PAGE          =   ZERO )
               MOVE    "処理対象のデータがありませんでした"
                                   TO  WRK-ERRMSG
 　              PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF    ( FLG-ERR             =   ZERO )
               MOVE    "STM"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    BUMCP-PAGE      TO  JOB-UPDCNT
           PERFORM 800-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   BUMCPERR-FILE
           IF      STS-BUMCPERR        =   ZERO
               CLOSE   BUMCPERR-FILE
           ELSE
               OPEN    OUTPUT              BUMCPERR-FILE
      *
               MOVE    WRK-ERRMSG      TO  BUMCPERR-REC
               WRITE   BUMCPERR-REC
               CLOSE   BUMCPERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-ERRMSG      TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-ERR
      *
           .
       500-ERR-HENSYU-EXT.
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
      *    ジョブ管理サブ
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    BUMCP-JOBID         TO  JOB-JOBID
           MOVE    BUMCP-SHELLID       TO  JOB-SHELLID
           MOVE    "ORCBUMCP"          TO  JOB-PGID
           MOVE    BUMCP-SHELLMSG      TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA


 
           .
       800-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
