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
       PROGRAM-ID.             ORCSGETFTNKBN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（情報連携）
      *  コンポーネント名  : 負担区分取得
      *  管理者            :
      *  作成日付    作業者        記述
      *  14/ 7/ 7    NACL−伊藤    新規作成
      *****************************************************************
      *
      *  平成２０年４月改正以降の負担区分を返却
      *  平成２０年３月までは空白を返却
      *
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       DATA                        DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  FLG-AREA.
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
      *
       01  INDEX-AREA.
           03  IDX                     PIC 9(03).
           03  IDY                     PIC 9(03).
      *
       01  WRK-AREA.
           03  WRK-FTNKBN-AREA.
               05  WRK-FTNKBN          PIC X(01).
               05  WRK-KOHNUM          PIC 9(01)
                                           OCCURS  4.
      *
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
       LINKAGE                    SECTION.
           COPY    "CPORCSGETFTNKBN.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSGETFTNKBNAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE  WRK-AREA
      *
           PERFORM 100-FTNKBN-HENSYU-SEC
           MOVE    WRK-FTNKBN          TO  ORCSGETFK-FTNKBN
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    負担区分編集処理
      *****************************************************************
       100-FTNKBN-HENSYU-SEC       SECTION.
      *
      *    主保険＋３種公費のとき該当の負担区分がないので
      *    ＋２種公費のときと同じ負担区分を編集する
      *
           INITIALIZE  WRK-FTNKBN-AREA
      *
      *    保険組合せ読込み
           INITIALIZE  HKNCOMBI-REC
           MOVE    ORCSGETFK-HOSPNUM   TO  COMB-HOSPNUM
           MOVE    ORCSGETFK-PTID      TO  COMB-PTID
           MOVE    ORCSGETFK-HKNCOMBINUM
                                       TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           PERFORM 900-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   1
               DISPLAY "HKNCONBI READ ERROR"
               GO  TO  100-FTNKBN-HENSYU-EXT
           END-IF
      *
      *    平成２０年４月改正以降の負担区分設定
           IF      ORCSGETFK-SRYYM     >=  "200804"
               PERFORM 200-FTNKBN-HENSYU-200804-SEC
               PERFORM 300-HKNJA-FTNJA-SEC
           END-IF
      *
      *???
      **   IF      WRK-FTNKBN          =   SPACE
      **       DISPLAY "FTNKBN ERR ##"
      **   END-IF
      *???
           .
       100-FTNKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    平成２０年４月改正以降の負担区分設定処理
      *****************************************************************
       200-FTNKBN-HENSYU-200804-SEC    SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
               IF      COMB-KOHHKNNUM (IDX)
                                       NOT =   SPACE
                   MOVE    1                   TO  WRK-KOHNUM (IDX)
               END-IF
           END-PERFORM
      *
      *???
      **     DISPLAY "RECE02(1)=" RECE02-KOHNUM (1) "(2)="
      **                          RECE02-KOHNUM (2)
      **             "(3)=" RECE02-KOHNUM (3) "##"
      **     DISPLAY "(1)=" WRK-KOHNUM (1) "(2)=" WRK-KOHNUM (2)
      **             "(3)=" WRK-KOHNUM (3) "##"
      *???
           IF      COMB-HKNNUM     NOT =   SPACE
               EVALUATE    WRK-KOHNUM (1)
                               ALSO    WRK-KOHNUM (2)
                                           ALSO    WRK-KOHNUM (3)
                                                   ALSO    WRK-KOHNUM(4)
      *            １者
                   WHEN    0   ALSO    0   ALSO    0   ALSO    0
                       MOVE    "1"            TO  WRK-FTNKBN
      *            ２者
                   WHEN    1   ALSO    0   ALSO    0   ALSO    0
                       MOVE    "2"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    0   ALSO    0
                       MOVE    "3"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    0   ALSO    1   ALSO    0
                       MOVE    "E"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    0   ALSO    0   ALSO    1
                       MOVE    "G"            TO  WRK-FTNKBN
      *            ３者
                   WHEN    1   ALSO    1   ALSO    0   ALSO    0
                       MOVE    "4"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    0   ALSO    1   ALSO    0
                       MOVE    "M"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    0   ALSO    0   ALSO    1
                       MOVE    "N"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    1   ALSO    0
                       MOVE    "O"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    0   ALSO    1
                       MOVE    "P"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    0   ALSO    1   ALSO    1
                       MOVE    "Q"            TO  WRK-FTNKBN
      *            ４者
                   WHEN    1   ALSO    1   ALSO    1   ALSO    0
                       MOVE    "V"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    1   ALSO    0   ALSO    1
                       MOVE    "W"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    0   ALSO    1   ALSO    1
                       MOVE    "X"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    1   ALSO    1
                       MOVE    "Y"            TO  WRK-FTNKBN
      *            ５者
                   WHEN    1   ALSO    1   ALSO    1   ALSO    1
                       MOVE    "9"            TO  WRK-FTNKBN
               END-EVALUATE
           ELSE
               EVALUATE    WRK-KOHNUM (1)
                               ALSO    WRK-KOHNUM (2)
                                           ALSO    WRK-KOHNUM (3)
                                                   ALSO    WRK-KOHNUM(4)
      *            １者
                   WHEN    1   ALSO    0   ALSO    0   ALSO    0
                       MOVE    "5"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    0   ALSO    0
                       MOVE    "6"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    0   ALSO    1   ALSO    0
                       MOVE    "B"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    0   ALSO    0   ALSO    1
                       MOVE    "C"            TO  WRK-FTNKBN
      *            ２者
                   WHEN    1   ALSO    1   ALSO    0   ALSO    0
                       MOVE    "7"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    0   ALSO    1   ALSO    0
                       MOVE    "H"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    0   ALSO    0   ALSO    1
                       MOVE    "I"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    1   ALSO    0
                       MOVE    "J"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    0   ALSO    1
                       MOVE    "K"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    0   ALSO    1   ALSO    1
                       MOVE    "L"            TO  WRK-FTNKBN
      *            ３者
                   WHEN    1   ALSO    1   ALSO    1   ALSO    0
                       MOVE    "R"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    1   ALSO    0   ALSO    1
                       MOVE    "S"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    0   ALSO    1   ALSO    1
                       MOVE    "T"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    1   ALSO    1
                       MOVE    "U"            TO  WRK-FTNKBN
      *            ４者
                   WHEN    1   ALSO    1   ALSO    1   ALSO    1
                       MOVE    "Z"            TO  WRK-FTNKBN
               END-EVALUATE
           END-IF
      *???
      **     IF      WRK-FTNKBN          =   SPACE
      **         DISPLAY "FTNKBN ERR ##"
      **     END-IF
      *???
           .
       200-FTNKBN-HENSYU-200804-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号・負担者番号設定処理
      *****************************************************************
       300-HKNJA-FTNJA-SEC         SECTION.
      *
           INITIALIZE  PTHKNINF-REC
           MOVE    COMB-HOSPNUM        TO  PTHKN-HOSPNUM
           MOVE    COMB-PTID           TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
           MOVE    COMB-TEKSTYMD       TO  PTHKN-TEKSTYMD
           MOVE    COMB-TEKSTYMD       TO  PTHKN-TEKEDYMD
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           PERFORM 900-PTHKNINF-READ-SEC
           IF      FLG-PTHKNINF        =   ZERO
               MOVE    PTHKN-HKNJANUM      TO  ORCSGETFK-HKNJANUM
           END-IF
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   4
                   OR      COMB-KOHID (IDX)    =   ZERO
               MOVE    COMB-HOSPNUM        TO  PTKOH-HOSPNUM
               MOVE    COMB-PTID           TO  PTKOH-PTID
               MOVE    COMB-KOHID (IDX)    TO  PTKOH-KOHID
               MOVE    COMB-TEKSTYMD       TO  PTKOH-TEKSTYMD
               MOVE    COMB-TEKSTYMD       TO  PTKOH-TEKEDYMD
               MOVE    PTKOHINF-REC        TO  MCPDATA-REC
               PERFORM 900-PTKOHINF-READ-SEC
               IF      FLG-PTKOHINF        =   ZERO
                   MOVE    PTKOH-FTNJANUM      TO
                                           ORCSGETFK-FTNJANUM (IDX)
               END-IF
           END-PERFORM
           .
       300-HKNJA-FTNJA-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読込
      *****************************************************************
       900-HKNCOMBI-READ-SEC       SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE  HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE  HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスタ読込
      *****************************************************************
       900-PTHKNINF-READ-SEC       SECTION.
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTHKNINF
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
                   IF      PTHKN-SAKJOKBN      =   "1"
                       MOVE    1                   TO  FLG-PTHKNINF
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報マスタ読込
      *****************************************************************
       900-PTKOHINF-READ-SEC       SECTION.
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTKOHINF
                   MOVE    MCPDATA-REC         TO  PTKOHINF-REC
                   IF      PTKOH-SAKJOKBN      =   "1"
                       MOVE    1                   TO  FLG-PTKOHINF
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-PTKOHINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTKOHINF
           END-IF
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-CLOSE-EXT.
           EXIT.
      *
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
      *
