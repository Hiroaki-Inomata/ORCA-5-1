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
       PROGRAM-ID.             ORCDBACCT.
      *****************************************************************
      *  システム名        : ORCA
      *  サブシステム名    : 
      *  コンポーネント名  : ORCA DB 診療会計アクセスモジュール
      *  返却エラーコード  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/02/12    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  13/01/11  診療回数テーブル１０対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SRYACCTS        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
           03  IDX-DAY             PIC 9(05).
      *
           03  IDZ                 PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATHNAME            PIC X(64).
           03  WRK-FUNC                PIC X(64).
           03  WRK-TABLE               PIC X(64).
           03  WRK-RENNUMMAX           PIC 9(03).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    新診療会計（主）
       01  SRYACCTM-REC.
           COPY    "CPSRYACCTM.INC".
      *
      *    新診療会計（従）
       01  SRYACCTS-REC.
           COPY    "CPSRYACCTS.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *    共通領域
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
           MCPAREA
           MCPDATA-REC
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    MCP-TABLE           TO  WRK-TABLE
           MOVE    MCP-PATHNAME        TO  WRK-PATHNAME
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-EDIT-SRYACCT-SEC
      *
           .
      *
           EXIT PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           .
      *
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計編集処理
      *****************************************************************
       200-EDIT-SRYACCT-SEC            SECTION.
      *
           MOVE    "tbl_sryacct_main"  TO  MCP-TABLE
           MOVE    MCPDATA-REC         TO  SRYACCT-REC
      *
           EVALUATE    MCP-FUNC
           WHEN    "DBSELECT"
               PERFORM 2001-EDIT-SRYACCTM-SEC
               MOVE    SRYACCTM-REC    TO  MCPDATA-REC
               CALL    "MONFUNC"   USING
                                   MCPAREA
                                   MCPDATA-REC
           WHEN    "DBCLOSECURSOR"
               PERFORM 910-DBCLOSECURSOR-SEC
           WHEN    "DBFETCH"
               CALL    "MONFUNC"   USING
                                   MCPAREA
                                   MCPDATA-REC
               IF    ( MCP-RC  =   ZERO )
                   PERFORM 2001-SRYACCTM-FET-SEC
               END-IF
           WHEN    "DBUPDATE"
               PERFORM 2001-SRYACCTM-UPD-SEC
           WHEN    "DBDELETE"
               PERFORM 2001-SRYACCTM-DEL-SEC
           WHEN    "DBINSERT"
               PERFORM 2001-SRYACCTM-INS-SEC
           END-EVALUATE
      *
           MOVE    SRYACCT-REC     TO  MCPDATA-REC
      *
           .
       200-EDIT-SRYACCT-EXT.
           EXIT.
      *****************************************************************
      *    新診療会計（主）編集処理
      *****************************************************************
       2001-EDIT-SRYACCTM-SEC          SECTION.
      *
           INITIALIZE                     SRYACCTM-REC
           MOVE    ACCT-HOSPNUM       TO  ACCTM-HOSPNUM
           MOVE    ACCT-NYUGAIKBN     TO  ACCTM-NYUGAIKBN
           MOVE    ACCT-PTID          TO  ACCTM-PTID
           MOVE    ACCT-SRYKA         TO  ACCTM-SRYKA
           MOVE    ACCT-SRYYM         TO  ACCTM-SRYYM
           MOVE    ACCT-SRYKBN        TO  ACCTM-SRYKBN
           MOVE    ACCT-ZAINUM        TO  ACCTM-ZAINUM
           MOVE    ACCT-HKNCOMBI      TO  ACCTM-HKNCOMBI
           MOVE    ACCT-JIHOKBN       TO  ACCTM-JIHOKBN
           MOVE    ACCT-ZAITEN        TO  ACCTM-ZAITEN
           MOVE    ACCT-SRYCDTOTAL    TO  ACCTM-SRYCDTOTAL
           MOVE    ACCT-SURYOUTOTAL   TO  ACCTM-SURYOUTOTAL
           MOVE    ACCT-MEISAISU      TO  ACCTM-MEISAISU
           MOVE    ACCT-ZAIKAISU      TO  ACCTM-ZAIKAISU
           MOVE    ACCT-SYUTEN1       TO  ACCTM-SYUTEN1
           MOVE    ACCT-SYUTEN2       TO  ACCTM-SYUTEN2
           MOVE    ACCT-YKZTEN        TO  ACCTM-YKZTEN
           MOVE    ACCT-KIZAITEN      TO  ACCTM-KIZAITEN
           MOVE    ACCT-DRCD          TO  ACCTM-DRCD
           MOVE    ACCT-ZAIREQFLG     TO  ACCTM-ZAIREQFLG
           MOVE    ACCT-TEIGENRATE    TO  ACCTM-TEIGENRATE
           MOVE    ZERO               TO  ACCTM-RENNUMMAX
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
      *
               MOVE    ACCT-DAY (1 IDX-DAY)
                                       TO  ACCTM-DAY (IDX-DAY)
      *
           END-PERFORM
      *
           MOVE    ACCT-TERMID        TO  ACCTM-TERMID
           MOVE    ACCT-OPID          TO  ACCTM-OPID
           MOVE    ACCT-CREYMD        TO  ACCTM-CREYMD
           MOVE    ACCT-UPYMD         TO  ACCTM-UPYMD
           MOVE    ACCT-UPHMS         TO  ACCTM-UPHMS
           MOVE    ACCT-STSRYYM       TO  ACCTM-STSRYYM
           MOVE    ACCT-EDSRYYM       TO  ACCTM-EDSRYYM
      *
           .
       2001-EDIT-SRYACCTM-EXT.
           EXIT.
      *****************************************************************
      *    新診療会計（主）FETCH処理
      *****************************************************************
       2001-SRYACCTM-FET-SEC            SECTION.
      *
           MOVE    MCPDATA-REC         TO  SRYACCTM-REC
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    ACCTM-HOSPNUM       TO  ACCT-HOSPNUM
           MOVE    ACCTM-NYUGAIKBN     TO  ACCT-NYUGAIKBN
           MOVE    ACCTM-PTID          TO  ACCT-PTID
           MOVE    ACCTM-SRYKA         TO  ACCT-SRYKA
           MOVE    ACCTM-SRYYM         TO  ACCT-SRYYM
           MOVE    ACCTM-SRYKBN        TO  ACCT-SRYKBN
           MOVE    ACCTM-ZAINUM        TO  ACCT-ZAINUM
           MOVE    ACCTM-HKNCOMBI      TO  ACCT-HKNCOMBI
           MOVE    ACCTM-JIHOKBN       TO  ACCT-JIHOKBN
           MOVE    ACCTM-ZAITEN        TO  ACCT-ZAITEN
           MOVE    ACCTM-SRYCDTOTAL    TO  ACCT-SRYCDTOTAL
           MOVE    ACCTM-SURYOUTOTAL   TO  ACCT-SURYOUTOTAL
           MOVE    ACCTM-MEISAISU      TO  ACCT-MEISAISU
           MOVE    ACCTM-ZAIKAISU      TO  ACCT-ZAIKAISU
           MOVE    ACCTM-SYUTEN1       TO  ACCT-SYUTEN1
           MOVE    ACCTM-SYUTEN2       TO  ACCT-SYUTEN2
           MOVE    ACCTM-YKZTEN        TO  ACCT-YKZTEN
           MOVE    ACCTM-KIZAITEN      TO  ACCT-KIZAITEN
           MOVE    ACCTM-DRCD          TO  ACCT-DRCD
           MOVE    ACCTM-ZAIREQFLG     TO  ACCT-ZAIREQFLG
           MOVE    ACCTM-TEIGENRATE    TO  ACCT-TEIGENRATE
      *
           MOVE    ACCTM-TERMID        TO  ACCT-TERMID
           MOVE    ACCTM-OPID          TO  ACCT-OPID
           MOVE    ACCTM-CREYMD        TO  ACCT-CREYMD
           MOVE    ACCTM-UPYMD         TO  ACCT-UPYMD
           MOVE    ACCTM-UPHMS         TO  ACCT-UPHMS
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
               MOVE    ACCTM-DAY (IDX-DAY)
                                       TO  ACCT-DAY (1 IDX-DAY)
                                           ACCT-DAY (2 IDX-DAY)
      *
           END-PERFORM
      *
           IF    ( ACCTM-RENNUMMAX     IS  NOT NUMERIC )
               MOVE    ZERO            TO  ACCTM-RENNUMMAX
           END-IF
      *
           IF    ( ACCTM-RENNUMMAX     >   0 )
      *
               PERFORM 900-SRYACCTS-KEY2-SEL-SEC
      *
               PERFORM UNTIL ( FLG-SRYACCTS    NOT =   ZERO )
      *
      ****         COMPUTE ACCT-DAY (ACCTS-RENNUM + 1 ACCTS-DATE-HI)
      ****             =   ACCTS-DAY-CNT
                   COMPUTE IDZ             =   ACCTS-RENNUM + 1
                   MOVE    ACCTS-DAY-CNT   TO  ACCT-DAY
                                                  (IDZ ACCTS-DATE-HI)
      *
                   COMPUTE ACCT-DAY (2 ACCTS-DATE-HI)
                       =   ACCT-DAY (2 ACCTS-DATE-HI)
                       -   ACCTS-DAY-CNT
      *
                   PERFORM 900-SRYACCTS-KEY2-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_sryacct_sub"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
      *    PERFORM 990-SRYACC-DAY-DISPLAY-SEC
      *
           MOVE    ZERO                    TO  MCP-RC
      *
           .
       2001-SRYACCTM-FET-EXT.
           EXIT.
      *****************************************************************
      *    新診療会計（主）更新処理
      *****************************************************************
       2001-SRYACCTM-UPD-SEC            SECTION.
      *
           PERFORM 2001-EDIT-SRYACCTM-SEC
      *
           PERFORM 2001-SRYACCTS-INS-SEC
      *
           IF    ( MCP-RC          =    ZERO )
               MOVE    WRK-RENNUMMAX       TO  ACCTM-RENNUMMAX
               MOVE    SRYACCTM-REC        TO  MCPDATA-REC
               MOVE    "tbl_sryacct_main"  TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
           END-IF
      *
           .
       2001-SRYACCTM-UPD-EXT.
           EXIT.
      *****************************************************************
      *    新診療会計（主）追加処理
      *****************************************************************
       2001-SRYACCTM-INS-SEC           SECTION.
      *
           PERFORM 2001-EDIT-SRYACCTM-SEC
      *
           PERFORM 2001-SRYACCTS-INS-SEC
      *
           IF    ( MCP-RC          =    ZERO )
               MOVE    WRK-RENNUMMAX       TO  ACCTM-RENNUMMAX
               MOVE    SRYACCTM-REC        TO  MCPDATA-REC
               MOVE    "tbl_sryacct_main"  TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
           END-IF
      *
           .
       2001-SRYACCTM-INS-EXT.
           EXIT.
      *****************************************************************
      *    新診療会計（主）削除処理
      *****************************************************************
       2001-SRYACCTM-DEL-SEC           SECTION.
      *
           PERFORM 2001-EDIT-SRYACCTM-SEC
      *
           PERFORM 2001-SRYACCTS-DEL-SEC
      *
           IF    ( MCP-RC          =    ZERO )
               MOVE    SRYACCTM-REC        TO  MCPDATA-REC
               MOVE    "tbl_sryacct_main"  TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
           END-IF
      *
           .
       2001-SRYACCTM-INS-EXT.
           EXIT.
      *****************************************************************
      *    新診療会計（従）削除処理
      *****************************************************************
       2001-SRYACCTS-DEL-SEC            SECTION.
      *
           INITIALIZE                   SRYACCTS-REC
           MOVE    ACCTM-HOSPNUM   TO   ACCTS-HOSPNUM
           MOVE    ACCTM-PTID      TO   ACCTS-PTID
           MOVE    ACCTM-ZAINUM    TO   ACCTS-ZAINUM
           MOVE    ACCTM-NYUGAIKBN TO   ACCTS-VNYUGAIKBN
           MOVE    ACCTM-SRYYM     TO   ACCTS-VSRYYM
           IF    ( MCP-PATHNAME    =    "del1" OR "del2" OR "del3" )
               CONTINUE
           ELSE
               MOVE    "del"           TO   MCP-PATHNAME
           END-IF
      *
           MOVE    SRYACCTS-REC        TO  MCPDATA-REC
           MOVE    "tbl_sryacct_sub"   TO  MCP-TABLE
           PERFORM 910-DBDELETE-SEC
      *
           .
       2001-SRYACCTS-DEL-EXT.
           EXIT.
      *****************************************************************
      *    新診療会計（従）追加処理
      *****************************************************************
       2001-SRYACCTS-INS-SEC            SECTION.
      *
           MOVE    "del"           TO   MCP-PATHNAME
           PERFORM 2001-SRYACCTS-DEL-SEC
      *
           INITIALIZE                   SRYACCTS-REC
           MOVE    ACCTM-HOSPNUM   TO   ACCTS-HOSPNUM
           MOVE    ACCTM-PTID      TO   ACCTS-PTID
           MOVE    ACCTM-ZAINUM    TO   ACCTS-ZAINUM
      *
           MOVE    ZERO            TO   WRK-RENNUMMAX
           PERFORM VARYING IDX1    FROM 3   BY  1
      *!!          UNTIL ( IDX1    >    4 )
                   UNTIL ( IDX1    >    10)
                    OR   ( MCP-RC  NOT  =   ZERO )
      *
               PERFORM VARYING IDX-DAY  FROM    1  BY  1
                       UNTIL ( IDX-DAY  >   31 )
                    OR   ( MCP-RC  NOT  =   ZERO )
      *
                    IF    ( ACCT-DAY (IDX1 IDX-DAY)    >   ZERO )
                        COMPUTE ACCTS-RENNUM   = IDX1    -   1
                        COMPUTE ACCTS-DATE-HI  = IDX-DAY
                        COMPUTE ACCTS-DAY-CNT  = ACCT-DAY (IDX1 IDX-DAY)
                        MOVE    SRYACCTS-REC        TO  MCPDATA-REC
                        MOVE    "tbl_sryacct_sub"   TO  MCP-TABLE
                        MOVE    "key"               TO  MCP-PATHNAME
                        PERFORM 910-DBINSERT-SEC
      *
                        MOVE    ACCTS-RENNUM        TO  WRK-RENNUMMAX
      *
                    END-IF
      *
               END-PERFORM
      *
           END-PERFORM
      *
           .
       2001-SRYACCTS-INS-EXT.
           EXIT.
      *****************************************************************
      *    新診療会計テーブル（従）検索処理
      *****************************************************************
       900-SRYACCTS-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCTS
      *
           INITIALIZE                      SRYACCTS-REC
           MOVE    ACCTM-HOSPNUM       TO  ACCTS-HOSPNUM
           MOVE    ACCTM-PTID          TO  ACCTS-PTID
           MOVE    ACCTM-ZAINUM        TO  ACCTS-ZAINUM
           MOVE    SRYACCTS-REC        TO  MCPDATA-REC
           MOVE    "tbl_sryacct_sub"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACCTS-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCTS
               INITIALIZE                  SRYACCTS-REC
           END-IF
      *
           .
       900-SRYACCTS-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    新診療会計テーブル（従）FETCH処理
      *****************************************************************
       900-SRYACCTS-KEY2-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCTS
      *
           MOVE    "tbl_sryacct_sub"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACCTS-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCTS
               INITIALIZE                  SRYACCTS-REC
           END-IF
      *
           .
       900-SRYACCTS-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
      *
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
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC       SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
      *
           .
       990-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
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
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    診療会計日付回数テーブルDISPLAY処理
      *****************************************************************
       990-SRYACC-DAY-DISPLAY-SEC      SECTION.
      *
           DISPLAY "============="
           DISPLAY ACCT-PTID ":" ACCT-SRYKBN ":" ACCT-ZAINUM
                             ":DAY-TBL  (1) =" 
                                    ACCT-DAY (1 1) ","
                                    ACCT-DAY (1 2) ","
                                    ACCT-DAY (1 3) ","
                                    ACCT-DAY (1 4) ","
                                    ACCT-DAY (1 5) ","
                                    ACCT-DAY (1 6) ","
                                    ACCT-DAY (1 7) ","
                                    ACCT-DAY (1 8) ","
                                    ACCT-DAY (1 9) ","
                                    ACCT-DAY (1 10) ","
                                    ACCT-DAY (1 11) ","
                                    ACCT-DAY (1 12) ","
                                    ACCT-DAY (1 13) ","
                                    ACCT-DAY (1 14) ","
                                    ACCT-DAY (1 15) ","
                                    ACCT-DAY (1 16) ","
                                    ACCT-DAY (1 17) ","
                                    ACCT-DAY (1 18) ","
                                    ACCT-DAY (1 19) ","
                                    ACCT-DAY (1 10) ","
                                    ACCT-DAY (1 21) ","
                                    ACCT-DAY (1 22) ","
                                    ACCT-DAY (1 23) ","
                                    ACCT-DAY (1 24) ","
                                    ACCT-DAY (1 25) ","
                                    ACCT-DAY (1 26) ","
                                    ACCT-DAY (1 27) ","
                                    ACCT-DAY (1 28) ","
                                    ACCT-DAY (1 29) ","
                                    ACCT-DAY (1 20) ","
                                    ACCT-DAY (1 31) ","
           DISPLAY ACCT-PTID ":" ACCT-SRYKBN ":" ACCT-ZAINUM
                             ":DAY-TBL  (2) =" 
                                    ACCT-DAY (2 1) ","
                                    ACCT-DAY (2 2) ","
                                    ACCT-DAY (2 3) ","
                                    ACCT-DAY (2 4) ","
                                    ACCT-DAY (2 5) ","
                                    ACCT-DAY (2 6) ","
                                    ACCT-DAY (2 7) ","
                                    ACCT-DAY (2 8) ","
                                    ACCT-DAY (2 9) ","
                                    ACCT-DAY (2 10) ","
                                    ACCT-DAY (2 11) ","
                                    ACCT-DAY (2 12) ","
                                    ACCT-DAY (2 13) ","
                                    ACCT-DAY (2 14) ","
                                    ACCT-DAY (2 15) ","
                                    ACCT-DAY (2 16) ","
                                    ACCT-DAY (2 17) ","
                                    ACCT-DAY (2 18) ","
                                    ACCT-DAY (2 19) ","
                                    ACCT-DAY (2 10) ","
                                    ACCT-DAY (2 21) ","
                                    ACCT-DAY (2 22) ","
                                    ACCT-DAY (2 23) ","
                                    ACCT-DAY (2 24) ","
                                    ACCT-DAY (2 25) ","
                                    ACCT-DAY (2 26) ","
                                    ACCT-DAY (2 27) ","
                                    ACCT-DAY (2 28) ","
                                    ACCT-DAY (2 29) ","
                                    ACCT-DAY (2 20) ","
                                    ACCT-DAY (2 31) ","
           DISPLAY ACCT-PTID ":" ACCT-SRYKBN ":" ACCT-ZAINUM
                             ":DAY-TBL  (3) =" 
                                    ACCT-DAY (3 1) ","
                                    ACCT-DAY (3 2) ","
                                    ACCT-DAY (3 3) ","
                                    ACCT-DAY (3 4) ","
                                    ACCT-DAY (3 5) ","
                                    ACCT-DAY (3 6) ","
                                    ACCT-DAY (3 7) ","
                                    ACCT-DAY (3 8) ","
                                    ACCT-DAY (3 9) ","
                                    ACCT-DAY (3 10) ","
                                    ACCT-DAY (3 11) ","
                                    ACCT-DAY (3 12) ","
                                    ACCT-DAY (3 13) ","
                                    ACCT-DAY (3 14) ","
                                    ACCT-DAY (3 15) ","
                                    ACCT-DAY (3 16) ","
                                    ACCT-DAY (3 17) ","
                                    ACCT-DAY (3 18) ","
                                    ACCT-DAY (3 19) ","
                                    ACCT-DAY (3 10) ","
                                    ACCT-DAY (3 21) ","
                                    ACCT-DAY (3 22) ","
                                    ACCT-DAY (3 23) ","
                                    ACCT-DAY (3 24) ","
                                    ACCT-DAY (3 25) ","
                                    ACCT-DAY (3 26) ","
                                    ACCT-DAY (3 27) ","
                                    ACCT-DAY (3 28) ","
                                    ACCT-DAY (3 29) ","
                                    ACCT-DAY (3 20) ","
                                    ACCT-DAY (3 31) ","
           DISPLAY ACCT-PTID ":" ACCT-SRYKBN ":" ACCT-ZAINUM
                             ":DAY-TBL  (4) =" 
                                    ACCT-DAY (4 1) ","
                                    ACCT-DAY (4 2) ","
                                    ACCT-DAY (4 3) ","
                                    ACCT-DAY (4 4) ","
                                    ACCT-DAY (4 5) ","
                                    ACCT-DAY (4 6) ","
                                    ACCT-DAY (4 7) ","
                                    ACCT-DAY (4 8) ","
                                    ACCT-DAY (4 9) ","
                                    ACCT-DAY (4 10) ","
                                    ACCT-DAY (4 11) ","
                                    ACCT-DAY (4 12) ","
                                    ACCT-DAY (4 13) ","
                                    ACCT-DAY (4 14) ","
                                    ACCT-DAY (4 15) ","
                                    ACCT-DAY (4 16) ","
                                    ACCT-DAY (4 17) ","
                                    ACCT-DAY (4 18) ","
                                    ACCT-DAY (4 19) ","
                                    ACCT-DAY (4 10) ","
                                    ACCT-DAY (4 21) ","
                                    ACCT-DAY (4 22) ","
                                    ACCT-DAY (4 23) ","
                                    ACCT-DAY (4 24) ","
                                    ACCT-DAY (4 25) ","
                                    ACCT-DAY (4 26) ","
                                    ACCT-DAY (4 27) ","
                                    ACCT-DAY (4 28) ","
                                    ACCT-DAY (4 29) ","
                                    ACCT-DAY (4 20) ","
                                    ACCT-DAY (4 31) ","
      *
           .
      *
       990-SRYACC-DAY-DISPLAY-EXT.
           EXIT.
