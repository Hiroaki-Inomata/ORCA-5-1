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
       PROGRAM-ID.             ORCBHXLST.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 随時帳票
      *  コンポーネント名  : チェクマスタ一覧表印刷（一括）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/08/08    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL−太田   05.02.08  ORCHXLSTに編集処理を移行
      *  03.05.00    NACL-太田    07/06/04  グループ診療対応
      *  04.08.01    NACL-森脇    17/01/16  適応病名対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
      *----(04.08.01)--ADD-START---
           03  FLG-CHKSND              PIC 9(01).
      *----(04.08.01)--ADD-END-----
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-JOB-UPDCNT                PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM         PIC 9(02).
           03  WRK-PARA-CHKKBN         PIC X(01).
           03  WRK-PARA-SRYCD          PIC X(09).
           03  WRK-PARA-SRYCD-END      PIC X(09).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *----(04.08.01)--ADD-START--
       01  CHKSND-REC.
           COPY    "CPCHKSND.INC".
      *----(04.08.01)--ADD-END----
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *----(04.08.01)--ADD-START--
      *    一時チェックマスタ編集サブ
           COPY    "CPORCSS013.INC".
      *
           COPY    "CPORCSS013.INC"    REPLACING
                                       //SS013-// BY //TSS013-//.
      *----(04.08.01)--ADD-END----
      *
      *    一覧印刷変換領域
           COPY    "CPORCHXLST.INC".
      *
      *    全角変換
           COPY    "CPORCSKANACHK.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           INITIALIZE                  RECEERR
      *
           MOVE    ZERO            TO  CNT-AREA
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
                                       WRK-PARA-JOBID
                                       WRK-PARA-SHELLID
                                       WRK-PARA-HOSPNUM
                                       RECEERR
                                       WRK-PARA-CHKKBN
                                       WRK-PARA-SRYCD
                                       WRK-PARA-SRYCD-END
           END-UNSTRING
      *
      *    初期値セット
           PERFORM 110-SYOKI-SET-SEC
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBHXLST"         TO  JOB-PGID
           MOVE    "チェックマスタ一覧一括印刷"
                                       TO  JOB-SHELLMSG
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           PERFORM 800-ORCSJOB-SEC
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RECEERR
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期値セット
      *****************************************************************
       110-SYOKI-SET-SEC           SECTION.
      *
      *    ＳＰＡエリアセット処理
           PERFORM 110-SPA-SET-SEC
      *
      *    印刷パラメタエリアセット処理
           PERFORM 110-ORCSPRTAREA-SET-SEC
      *
           .
       110-SYOKI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡエリアセット処理
      *****************************************************************
       110-SPA-SET-SEC                 SECTION.
      *
      *    システム日付セット
           INITIALIZE                      SPA-AREA
           MOVE    LNK-PRTKANRI-SKYYMD
                                       TO  SPA-SYSYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    LNK-PRTKANRI-TERMID TO  SPA-TERMID
      *
           .
       110-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    印刷パラメタエリアセット処理
      *****************************************************************
       110-ORCSPRTAREA-SET-SEC         SECTION.
      *
      *    印刷パラメタ編集
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    "チェックマスタ一覧一括印刷"
                                       TO  SPRT-TITLE
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
      *
           .
       110-ORCSPRTAREA-SET-EXT.
           EXIT.
      *----(04.08.01)--UPD-START---
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF    ( WRK-PARA-CHKKBN =   "Y" )   OR
                 ( WRK-PARA-CHKKBN =   "Z" )
               GO  TO  200-MAIN-EXT
           END-IF
      *
           IF    ( WRK-PARA-CHKKBN =   "1" )   OR
                 ( WRK-PARA-CHKKBN =   "2" )
               PERFORM 2002-TEKBYO-START-SEC
           END-IF
      *
           INITIALIZE                      ORCHXLSTAREA
           MOVE    WRK-PARA-CHKKBN     TO  HXLST-CHKKBN
           MOVE    WRK-PARA-SRYCD      TO  HXLST-SRYCD
           MOVE    WRK-PARA-SRYCD-END  TO  HXLST-SRYCD-END
           MOVE    LNK-PRTKANRI-SKYYMD TO  HXLST-KJNYMD
           MOVE    "1"                 TO  HXLST-SRTKBN
           MOVE    "2"                 TO  HXLST-PROCKBN
           MOVE    ORCSPRTAREA         TO  HXLST-PRTAREA
           MOVE    TSS013-TEMP-NAME    TO  HXLST-TEMP-NAME
           MOVE    TSS013-TEMP-KEY     TO  HXLST-TEMP-KEY
      *
           CALL    "ORCHXLST"          USING
                                       SPA-AREA
                                       ORCHXLSTAREA
      *
           MOVE    HXLST-OUT-CNT       TO  CNT-JOB-UPDCNT
      *
           MOVE    1                   TO  FLG-END
      *
           IF    ( WRK-PARA-CHKKBN     =   "1" OR "2" )
               PERFORM 2001-TEKBYO-END-SEC
           END-IF
      *
             .
       200-MAIN-EXT.
           EXIT.
      *
      *----(04.08.01)--UPD-END-----
      *----(04.08.01)--ADD-START---
      *****************************************************************
      *    適応病名マスタ一覧表開始処理
      *****************************************************************
       2002-TEKBYO-START-SEC           SECTION.
      *
           INITIALIZE                      SS013-AREA
           MOVE     "DBCREATE"         TO  SS013-FUNC
           MOVE     WRK-PARA-CHKKBN    TO  SS013-CHKKBN
           MOVE     "2"                TO  SS013-CREATEKBN
           MOVE     LNK-PRTKANRI-SKYYMD
                                       TO  SS013-KJNYMD
           CALL     "ORCSS013"         USING
                                           SS013-AREA
                                           SPA-AREA
           MOVE     SS013-AREA         TO  TSS013-AREA
      *
           PERFORM 900-CHKSND-KEY23-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
      *
               INITIALIZE                      SS013-AREA
               MOVE     "DBINSERT"         TO  SS013-FUNC
               MOVE     WRK-PARA-CHKKBN    TO  SS013-CHKKBN
               MOVE     CHKSND-SRYCD       TO  SS013-SRYCD
               MOVE     LNK-PRTKANRI-SKYYMD
                                           TO  SS013-KJNYMD
               MOVE     TSS013-TEMP-NAME   TO  SS013-TEMP-NAME
               MOVE     TSS013-TEMP-KEY    TO  SS013-TEMP-KEY
               CALL     "ORCSS013"         USING
                                           SS013-AREA
                                           SPA-AREA
      *
               PERFORM 900-CHKSND-KEY23-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "key23"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2002-TEKBYO-START-EXT.
           EXIT.
      *****************************************************************
      *    適応病名マスタ一覧表終了処理
      *****************************************************************
       2001-TEKBYO-END-SEC             SECTION.
      *
           MOVE    "STM"       TO  SJOBKANRI-MODE
           INITIALIZE              JOBKANRI-REC
           PERFORM 800-ORCSJOB-SEC
      *
           INITIALIZE                      SS013-AREA
           MOVE     "DBDROP"           TO  SS013-FUNC
           CALL     "ORCSS013"         USING
                                       SS013-AREA
                                       SPA-AREA
      *
           .
       2001-TEKBYO-END-EXT.
           EXIT.
      *----(04.08.01)--ADD-END-----
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      CNT-JOB-UPDCNT            =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBHXLST PAGE:" CNT-JOB-UPDCNT  " ***"
           DISPLAY "*** ORCBHXLST END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-JOB-UPDCNT  TO  JOB-UPDCNT
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           PERFORM 800-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END     
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *----(04.08.01)--ADD-START---
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY23-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    WRK-PARA-CHKKBN     TO  CHKSND-CHKKBN
           MOVE    "%"                 TO  CHKSND-SRYCD
           MOVE    "0"                 TO  CHKSND-CDKBN
           MOVE    LNK-PRTKANRI-SKYYMD TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-KEY23-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY23-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-KEY23-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN" USING   MCPAREA
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN" USING   MCPAREA
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
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN" USING   MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN" USING   MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN" USING   MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *----(04.08.01)--ADD-END-----
      *****************************************************************
      *    ジョブ管理サブ
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
      *
       800-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC.
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
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
