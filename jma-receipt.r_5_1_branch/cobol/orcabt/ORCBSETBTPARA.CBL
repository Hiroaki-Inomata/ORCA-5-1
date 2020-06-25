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
       PROGRAM-ID.             ORCBSETBTPARA.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : パラメタテーブル設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  18/10/12    NACL-伊藤     新規作成
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
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
      *
       01  WRK-PARA.
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-INFO-KBN       PIC X(02).
           03  WRK-PARA-INFO-PARA      PIC X(500).
      *
       01  WRK-ERR-AREA.
           03  WRK-ERRMSG              PIC X(300).
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
           COPY    "CPORCSMCNDATE.INC".
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
           COPY    "COMMON-SPA".
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(600).
      ******************************************************************
       PROCEDURE                   DIVISION
                                       USING
                                       COMMAND-PARAM.
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
           PERFORM 100-INIT-SEC
           PERFORM 200-MAIN-SEC
           PERFORM 300-END-SEC
           .
       000-PROC-EXT.
           STOP    RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
           INITIALIZE  WRK-PARA
           INITIALIZE  SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                               INTO    WRK-PARA-HOSPNUM
                                       WRK-PARA-JOBID
                                       WRK-PARA-SHELLID
                                       WRK-PARA-INFO-KBN
                                       WRK-PARA-INFO-PARA
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    "orcabatch"         TO  SPA-OPID
           PERFORM 100-DBOPEN-SEC
           PERFORM 800-MCNDATE-SEC
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *    パラメタＤＢ更新
           INITIALIZE  BTPARA-REC
           MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
           MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           INITIALIZE  BTPARA-REC
           MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
           MOVE    WRK-PARA-JOBID      TO  BTPARA-JOBID
           MOVE    WRK-PARA-INFO-KBN   TO  BTPARA-INFO-KBN
           MOVE    WRK-PARA-INFO-PARA  TO  BTPARA-INFO-PARA
           MOVE    SPA-TERMID          TO  BTPARA-TERMID
           MOVE    SPA-OPID            TO  BTPARA-OPID
           MOVE    SMCNDATE-YMD        TO  BTPARA-CREYMD
           MOVE    SMCNDATE-YMD        TO  BTPARA-UPYMD
           MOVE    SMCNDATE-HMS        TO  BTPARA-UPHMS
           MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBINSERT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    "BTPARA INSERT ERROR"
                                       TO  WRK-ERRMSG
               PERFORM 500-COBABORT-SEC
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
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
           CALL    "cobabort"          USING   WRK-ERRMSG
           .
       500-COBABORT-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC             SECTION.
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
           .
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC           SECTION.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-ORCDBMAIN-EXT.
           EXIT.
