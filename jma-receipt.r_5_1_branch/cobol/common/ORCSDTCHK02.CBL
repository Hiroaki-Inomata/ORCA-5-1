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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSDTCHK02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : 保険組合せ取得
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/04/18    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/06/04  グループ診療対応
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
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
           03  FLG-END             PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-TARGET          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX1                PIC 9(05).
           03  IDX-MAX             PIC 9(05).
           03  IDX-PNT             PIC 9(05).
           03  IDX-KOH             PIC 9(05).
           03  IDX-HKNNUM          PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-HKNCOMBI            PIC X(100).
           03  WRK-HKNKBN-MEI          PIC X(02).
           03  WRK-SYUHKNNUM           PIC X(03).
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-RSI-HKNKBN          PIC X(01).
      *
       01  CONST-AREA.
           03  CONST-HKNCOMBI-MAX      PIC 9(05)   VALUE   1000.
           03  CONST-HKNINF-MAX        PIC 9(05)   VALUE   50.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                        SECTION.
      *
           COPY    "CPORCSDTCHK02.INC".
           01  LNK-HKNCOMBI-AREA.
             02  LNK-HKNCOMBI-OCC      OCCURS  1000.
           COPY    "CPHKNCOMBI.INC"    REPLACING  //COMB-//
                                       BY         //LNK-COMB-//.
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SDTCHK02AREA
           LNK-HKNCOMBI-AREA
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SDTCHK02-OT
                                       SDTCHK02-OT-HKNINF
                                       LNK-HKNCOMBI-AREA
      *
      *    保険組合せ編集処理
           PERFORM 100-HKNCOMBI-HENSYU-SEC
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       100-HKNCOMBI-HENSYU-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
      *
           PERFORM 900-HKNCOMBI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-HKNCOMBI    =   1 )
                    OR   ( IDX-MAX        >=   CONST-HKNCOMBI-MAX )
      *
               MOVE    ZERO            TO  FLG-TARGET
      *
               IF    ( COMB-DELKBN         =   "1" )
                   IF    ( SDTCHK02-ALL    =   1   )
                       MOVE    1       TO  FLG-TARGET
                   END-IF
               ELSE
                       MOVE    1       TO  FLG-TARGET
               END-IF
      *
               IF    ( FLG-TARGET      =   1 )
                   COMPUTE IDX-MAX =   IDX-MAX +   1
                   MOVE    HKNCOMBI-REC TO  LNK-HKNCOMBI-OCC (IDX-MAX)
                   PERFORM 1101-HKNCOMBIMEI-HEN-SEC
                   MOVE    WRK-HKNCOMBI TO  SDTCHK02-O-HKNCOMBIMEI
                                                             (IDX-MAX)
                   MOVE    WRK-RSI-HKNKBN
                                        TO  SDTCHK02-O-RSI-HKNKBN
                                                             (IDX-MAX)
                   PERFORM 1101-HKNINF-HEN-SEC
               END-IF
      *
               PERFORM 900-HKNCOMBI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    最大件数
           MOVE    IDX-MAX             TO  SDTCHK02-O-MAX
      *
      *
           .
       100-HKNCOMBI-HENSYU-EXT.
      *
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       1101-HKNCOMBIMEI-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI
                                           WRK-RSI-HKNKBN
      *
           MOVE    1                   TO  IDX-PNT
      *
           STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
           INTO    WRK-HKNCOMBI
           WITH    POINTER IDX-PNT
           END-STRING
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   4 )
      *
               IF    ( COMB-KOH-TANSEIDONAME (IDX0)   NOT =    SPACE )
                   IF    ( IDX-PNT >   1 )
                       STRING  "　"        DELIMITED  BY  SIZE
                               COMB-KOH-TANSEIDONAME (IDX0)
                                           DELIMITED  BY  SPACE
                       INTO    WRK-HKNCOMBI
                       WITH    POINTER IDX-PNT
                       END-STRING
                   ELSE
                       STRING  COMB-KOH-TANSEIDONAME (IDX0)
                                           DELIMITED  BY  SPACE
                       INTO    WRK-HKNCOMBI
                       WITH    POINTER IDX-PNT
                       END-STRING
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *    労災の時、コメントの追加
           IF    ( COMB-HKNNUM         =   "971"  OR  "973" )
      *
               PERFORM 900-PTRSIINF-KEY-SEL-SEC
      *
               MOVE    PTRSI-HKNKBN    TO  WRK-RSI-HKNKBN
      *
               MOVE    SPACE           TO  WRK-HKNKBN-MEI
               EVALUATE    PTRSI-HKNKBN
               WHEN    "1"
                   MOVE    "短"        TO  WRK-HKNKBN-MEI
               WHEN    "2"
                   MOVE    "傷"        TO  WRK-HKNKBN-MEI
               WHEN    "3"
                   MOVE    "ア"        TO  WRK-HKNKBN-MEI
               WHEN    "5"
                   MOVE    "公"        TO  WRK-HKNKBN-MEI
               END-EVALUATE
      *
               IF    ( PTRSI-COMMENT       =   SPACE )
                   MOVE    SPACE               TO  WRK-HKNCOMBI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           "　"                    DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                   INTO    WRK-HKNCOMBI
                   END-STRING
               ELSE
                   MOVE    SPACE               TO  WRK-HKNCOMBI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           "　"                    DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           "（"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT(1:24)     DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                   INTO              WRK-HKNCOMBI
                   END-STRING
               END-IF
           END-IF
           .
       1101-HKNCOMBIMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険情報編集処理
      *****************************************************************
       1101-HKNINF-HEN-SEC             SECTION.
      *
           IF    ( CONST-HKNINF-MAX    >   SDTCHK02-O-HKNINF-MAX )
               IF    ( COMB-KOHHKNNUM (1)  =   "970" )
                   MOVE    "970"       TO  WRK-SYUHKNNUM
                   MOVE    COMB-HKNNUM TO  WRK-HKNNUM
                   PERFORM 11011-HKNINF-HEN-SEC
                   MOVE    COMB-KOHHKNNUM (1)
                                       TO  WRK-SYUHKNNUM
                                           WRK-HKNNUM
                   PERFORM 11011-HKNINF-HEN-SEC
               ELSE
                   MOVE    COMB-HKNNUM TO  WRK-SYUHKNNUM
                                           WRK-HKNNUM
                   PERFORM 11011-HKNINF-HEN-SEC
               END-IF
           END-IF
      *
           PERFORM VARYING IDX-KOH     FROM    1   BY  1
                   UNTIL ( IDX-KOH     >   4 )
      *
               IF    ( COMB-KOHHKNNUM (IDX-KOH)    NOT =   SPACE )
                AND  ( COMB-KOHHKNNUM (IDX-KOH)    NOT =   "970" )
                   IF    ( CONST-HKNINF-MAX
                                       >   SDTCHK02-O-HKNINF-MAX)
                       MOVE    COMB-HKNNUM TO  WRK-SYUHKNNUM
                       MOVE    COMB-KOHHKNNUM (IDX-KOH)
                                           TO  WRK-HKNNUM
                       PERFORM 11011-HKNINF-HEN-SEC
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       1101-HKNINF-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険情報編集処理
      *****************************************************************
       11011-HKNINF-HEN-SEC            SECTION.
      *
           MOVE    ZERO            TO  IDX-HKNNUM
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1            >   SDTCHK02-O-HKNINF-MAX )
                    OR   ( IDX-HKNNUM  NOT =   ZERO )
      *
               IF    ( WRK-HKNNUM  =   SDTCHK02-O-HKNNUM (IDX1))
                   MOVE    IDX1    TO  IDX-HKNNUM
               END-IF
      *
           END-PERFORM
      *
           IF    ( IDX-HKNNUM      =   ZERO )
               COMPUTE SDTCHK02-O-HKNINF-MAX
                   =   SDTCHK02-O-HKNINF-MAX   +   1
               MOVE    SDTCHK02-O-HKNINF-MAX
                                   TO  IDX-HKNNUM
               MOVE    WRK-HKNNUM  TO  SDTCHK02-O-HKNNUM (IDX-HKNNUM)
           END-IF
      *
           IF    ( IDX-HKNNUM      >   ZERO )
               EVALUATE    WRK-SYUHKNNUM
               WHEN      "060"
               WHEN      "067"
               WHEN      "068"
               WHEN      "069"
                   MOVE    1       TO  SDTCHK02-O-KOKUHO(IDX-HKNNUM)
               WHEN      "971"
                   MOVE    1       TO  SDTCHK02-O-ROSAI (IDX-HKNNUM)
               WHEN      "973"
                   MOVE    1       TO  SDTCHK02-O-JIBAI (IDX-HKNNUM)
               WHEN      "970"
                   MOVE    1       TO  SDTCHK02-O-DAISAN (IDX-HKNNUM)
               WHEN      "975"
                   MOVE    1       TO  SDTCHK02-O-KOUGAI (IDX-HKNNUM)
               WHEN      "040"
               WHEN      "039"
                   MOVE    1       TO  SDTCHK02-O-KOUKI  (IDX-HKNNUM)
               WHEN      OTHER
                   EVALUATE    WRK-SYUHKNNUM (1:2)
                   WHEN  "98"
                       MOVE    1   TO  SDTCHK02-O-JIHI  (IDX-HKNNUM)
                   WHEN  "90"
                   WHEN  "91"
                       MOVE    1   TO  SDTCHK02-O-CHIKEN(IDX-HKNNUM)
                   WHEN  OTHER
                       MOVE    1   TO  SDTCHK02-O-SYAHO (IDX-HKNNUM)
                   END-EVALUATE
               END-EVALUATE
           END-IF
      *
           .
       11011-HKNINF-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ取得処理(KEY2)
      *****************************************************************
       900-HKNCOMBI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SDTCHK02-HOSPNUM     TO  COMB-HOSPNUM
           MOVE    SDTCHK02-PTID       TO  COMB-PTID
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       900-HKNCOMBI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せFETCH処理(KEY2)
      *****************************************************************
       900-HKNCOMBI-KEY2-FET-SEC       SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       900-HKNCOMBI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者労災保険情報検索処理(KEY)
      *****************************************************************
       900-PTRSIINF-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTRSIINF
      *
           INITIALIZE                      PTRSIINF-REC
           MOVE    SDTCHK02-HOSPNUM     TO  PTRSI-HOSPNUM
           MOVE    SDTCHK02-PTID       TO  PTRSI-PTID
           MOVE    COMB-HKNID          TO  PTRSI-HKNID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
               INITIALIZE                  PTRSIINF-REC
           END-IF
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTRSIINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
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
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
