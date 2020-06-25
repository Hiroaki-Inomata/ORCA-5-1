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
       PROGRAM-ID.             ORCBMRCSV10B.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療情報データ作成
      *  コンポーネント名  : 該当患者リスト作成
      *                    : 診療日リスト作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/12/04    NACL-伊藤     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *****************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    該当患者リスト
           SELECT  PTLIST-FILE     ASSIGN  ASS-PTLIST
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PTLIST.
      *    診療日リスト
           SELECT  SRYYMD-FILE     ASSIGN  ASS-SRYYMD
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-SRYYMD.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    該当患者リスト
       FD  PTLIST-FILE.
       01  PTLIST-REC.
           03  PTLIST-NYUGAIKBN    PIC X(01).
           03  PTLIST-SRYYMD       PIC X(08).
           03  PTLIST-PTID         PIC 9(10).
      *
      *    診療日リスト
       FD  SRYYMD-FILE.
       01  SRYYMD-REC.
           03  SRYYMD-YMD          PIC X(08).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPASGNPARA.INC"    REPLACING   //ASGNPARA//
                                       BY          //ASS-PTLIST//.
           COPY    "CPASGNPARA.INC"    REPLACING   //ASGNPARA//
                                       BY          //ASS-SRYYMD//.
      *
       01  STS-AREA.
           03  STS-PTLIST          PIC X(02).
           03  STS-SRYYMD          PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
      *
       01  WRK-AREA.
           03  WRK-KEY-OLD.
               05  WRK-KEY-OLD-SRYYMD  PIC X(08).
               05  WRK-KEY-OLD-PTID    PIC 9(10).
           03  WRK-KEY-NEW.
               05  WRK-KEY-NEW-SRYYMD  PIC X(08).
               05  WRK-KEY-NEW-PTID    PIC 9(10).
           05  WRK-PTID                PIC 9(10).
      *
       01  CNT-AREA.
           03  CNT-PTLIST          PIC 9(04).
           03  CNT-SRYYMD          PIC 9(04).
      *
       01  PARA-AREA.
           03  PARA-PTLIST         PIC X(128).
           03  PARA-SRYYMD         PIC X(128).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    受診履歴マスタ
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSGETTEMP.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      *
       PROCEDURE                   DIVISION
                                   USING
                                       COMMAND-PARAM.
      *
      ****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE  FLG-AREA
           INITIALIZE  STS-AREA
           INITIALIZE  WRK-AREA
           INITIALIZE  CNT-AREA
           INITIALIZE  SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    SPA-HOSPNUM
                                               SPA-NYUGAIKBN
                                               SPA-SRYYMD
                                               PARA-PTLIST
                                               PARA-SRYYMD
           END-UNSTRING
      *
           PERFORM 100-DBOPEN-SEC
      *
           INITIALIZE  SGETTEMP-AREA
           MOVE    PARA-PTLIST         TO  SGETTEMP-BASENAMES (1)
           MOVE    PARA-SRYYMD         TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  ASS-PTLIST
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  ASS-SRYYMD
      *
           OPEN    OUTPUT  PTLIST-FILE
           OPEN    OUTPUT  SRYYMD-FILE
      *
           IF      SPA-NYUGAIKBN       =   "1"
               PERFORM 400-SRYYMD-WRITE-SEC
               PERFORM 100-PTNYUINRRK-SEC
           ELSE
               PERFORM 100-JYURRK-SEC
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      ****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM UNTIL   FLG-END =   1
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
                   IF      PTINF-TSTPTNUMKBN   =   "0"
                       IF      SPA-NYUGAIKBN       =   "1"
                           MOVE    PTNYUINRRK-PTID
                                           TO  WRK-KEY-NEW-PTID
                       ELSE
                           MOVE    JYURRK-SRYYMD
                                           TO  WRK-KEY-NEW-SRYYMD
                           MOVE    JYURRK-PTID
                                           TO  WRK-KEY-NEW-PTID
                           IF      WRK-KEY-NEW-SRYYMD
                                       NOT =   WRK-KEY-OLD-SRYYMD
                               PERFORM 400-SRYYMD-WRITE-SEC
                           END-IF
                       END-IF
                       IF      WRK-KEY-NEW     NOT =   WRK-KEY-OLD
                           PERFORM 400-PTLIST-WRITE-SEC
                           MOVE    WRK-KEY-NEW         TO  WRK-KEY-OLD
                       END-IF
                   END-IF
               END-IF
               PERFORM 900-TABLE-FETCH-SEC
           END-PERFORM
           .
       200-MAIN-EXT.
           EXIT.
      *
      ****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
           ELSE
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key70"             TO  MCP-PATHNAME
           END-IF
           PERFORM 990-DBCLOSE-SEC
      *
           CLOSE   PTLIST-FILE
           CLOSE   SRYYMD-FILE
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "ORCBMRCSV10B PTLIST [ " CNT-PTLIST " ]"
           DISPLAY "             SRYYMD [ " CNT-SRYYMD " ]"
           DISPLAY "*** ORCBMRCSV10B END ***"
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴読込処理
      *****************************************************************
       100-JYURRK-SEC              SECTION.
      *
           INITIALIZE  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-SRYYMD          TO  JYURRK-UPYMD
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key70"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               PERFORM 900-TABLE-FETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-JYURRK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴読込処理
      *****************************************************************
       100-PTNYUINRRK-SEC          SECTION.
      *
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-SRYYMD          TO  PTNYUINRRK-NYUINYMD
           MOVE    SPA-SRYYMD          TO  PTNYUINRRK-TAIINYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               PERFORM 900-TABLE-FETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-PTNYUINRRK-EXT.
           EXIT.
      *
      *****************************************************************
      *    該当患者リスト出力処理
      *****************************************************************
       400-PTLIST-WRITE-SEC        SECTION.
      *
           INITIALIZE  PTLIST-REC
           MOVE    SPA-NYUGAIKBN       TO  PTLIST-NYUGAIKBN
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    SPA-SRYYMD          TO  PTLIST-SRYYMD
           ELSE
               MOVE    WRK-KEY-NEW-SRYYMD  TO  PTLIST-SRYYMD
           END-IF
           MOVE    WRK-KEY-NEW-PTID    TO  PTLIST-PTID
           WRITE   PTLIST-REC
           COMPUTE CNT-PTLIST          =   CNT-PTLIST  +   1
           .
       400-PTLIST-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日リスト出力処理
      *****************************************************************
       400-SRYYMD-WRITE-SEC        SECTION.
      *
           INITIALIZE  SRYYMD-REC
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    SPA-SRYYMD          TO  SRYYMD-YMD
           ELSE
               MOVE    JYURRK-SRYYMD       TO  SRYYMD-YMD
           END-IF
           WRITE   SRYYMD-REC
           COMPUTE CNT-SRYYMD          =   CNT-SRYYMD  +   1
           .
       400-SRYYMD-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴読込
      *****************************************************************
       900-TABLE-FETCH-SEC         SECTION.
      *
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-END
                   MOVE    MCPDATA-REC         TO  PTNYUINRRK-REC
                   MOVE    PTNYUINRRK-PTID     TO  WRK-PTID
               ELSE
                   MOVE    1                   TO  FLG-END
                   INITIALIZE  PTNYUINRRK-REC
               END-IF
           ELSE
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key70"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-END
                   MOVE    MCPDATA-REC         TO  JYURRK-REC
                   MOVE    JYURRK-PTID         TO  WRK-PTID
               ELSE
                   MOVE    1                   TO  FLG-END
                   INITIALIZE  JYURRK-REC
               END-IF
           END-IF
           .
       900-TABLE-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報読込
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           INITIALIZE  PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    WRK-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   INITIALIZE  PTINF-REC
                   MOVE    1                   TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE  PTINF-REC
               MOVE    1                   TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル読み込み処理
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
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
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
