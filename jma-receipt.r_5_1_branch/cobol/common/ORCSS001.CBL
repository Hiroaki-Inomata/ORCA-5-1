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
       PROGRAM-ID.             ORCSS001.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 保険分類
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/07/14    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *****************************************************************
      *
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
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-HOKEN-G.
               05  FLG-SYAHO       PIC 9(01).
               05  FLG-KOKUHO      PIC 9(01).
               05  FLG-JIHI        PIC 9(01).
           03  FLG-TBLCOMBI        PIC 9(01).
           03  FLG-DOUJITSU-DAY-G.
               05  FLG-DOUJITSU-DAY PIC 9(03)
                                   OCCURS  31.
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX-NYUGAI          PIC 9(05).
           03  IDX-SRYKA           PIC 9(05).
           03  IDX-DAY             PIC 9(02).
           03  IDX-TBLCOMBI        PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-HKNCOMBINUM         PIC 9(04).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-STIDX               PIC 9(02).
           03  WRK-EDIDX               PIC 9(02).
           03  WRK-STDAY               PIC 9(02).
           03  WRK-EDDAY               PIC 9(02).
           03  WRK-YMD.
               05  WRK-YYYYMM          PIC X(06).
               05  WRK-DD              PIC 9(02).
      *
       01  TNYURRK-AREA.
           03  TNYURRK-OCC             OCCURS  31.
               05  TNYURRK-SRYKA       PIC X(02).
      *
       01  TCOMBI-AREA.
           03  TCOMBI-MAX              PIC 9(05).
           03  TCOMBI-OCC              OCCURS  31.
               05  TCOMBI-HKNCOMBI     PIC 9(04).
      *
       01  CONST-AREA.
           03  CONST-ROUSAI            PIC X(03)   VALUE   "971".
           03  CONST-JIBAI             PIC X(03)   VALUE   "973".
           03  CONST-KOUGAI            PIC X(03)   VALUE   "975".
           03  CONST-DAI3SHA           PIC X(03)   VALUE   "970".
           03  CONST-HOUKATU           PIC 9(04)   VALUE   9999.
           03  CONST-TCOMBI-MAX        PIC 9(05)   VALUE   31.
           03  CONST-SRYKA-MAX         PIC 9(05)   VALUE   50.
           03  CONST-TBLCOMBI-MAX      PIC 9(05)   VALUE   100.
      *
      *    保険組合せ
       01  TBLCOMBI-AREA.
           02  TBLCOMBI-MAX            PIC 9(05).
           02  TBLCOMBI-OCC            OCCURS      100.
           COPY    "CPHKNCOMBI.INC"    REPLACING   //COMB-//
                                       BY          //TBLCOMBI-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK4001.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    入院診療会計マスター
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
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
           COPY  "CPORCSS001.INC".
grpsys*
grpsys     COPY  "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SS001-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  TNYURRK-AREA
           INITIALIZE                  TBLCOMBI-AREA
           INITIALIZE                  SS001-HKNGRP-G
           INITIALIZE                  SS001-SRYKA-G
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           IF    ( SS001-KIKANKBN  =   "1" )
               MOVE    SS001-STDAY TO  WRK-STDAY
               MOVE    SS001-EDDAY TO  WRK-EDDAY
           ELSE
               MOVE    1           TO  WRK-STDAY
               MOVE    31          TO  WRK-EDDAY
           END-IF
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "4001"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SS001-SRYYM         TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-4001-REC
           IF    ( SYS-4001-JIBAIKBN   =   SPACE )
               MOVE    "1"             TO  SYS-4001-JIBAIKBN
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-HKNGRP-EDIT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       2001-HKNGRP-EDIT-SEC        SECTION.
      *
           PERFORM 20011-GAIHKN-EDIT-SEC
      *
           PERFORM 20011-NYUHKN-EDIT-SEC
      *
           .
       2001-HKNGRP-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    外来保険情報編集処理
      *****************************************************************
       20011-GAIHKN-EDIT-SEC           SECTION.
      *
           PERFORM 900-JYURRK-KEY42-SEL-SEC
      *
           PERFORM UNTIL ( FLG-JYURRK  NOT =   ZERO )
      *
               IF    ( JYURRK-NYUGAIKBN    =   "1" )
                   MOVE    1           TO  IDX-NYUGAI
               ELSE
                   MOVE    2           TO  IDX-NYUGAI
               END-IF
      *
               MOVE    JYURRK-HKNCOMBINUM  TO  WRK-HKNCOMBINUM
               IF    ( WRK-HKNCOMBINUM     =   "9999" )
                   MOVE    ZERO        TO  FLG-HKNCOMBI
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    SS001-HOSPNUM   TO  COMB-HOSPNUM
                   MOVE    SS001-PTID      TO  COMB-PTID
                   MOVE    9999            TO  COMB-HKNCOMBINUM
                   MOVE    SPACE           TO  COMB-HKNNUM
               ELSE
                   PERFORM 900-HKNCOMBI-KEY-SEL-SEC
               END-IF
               IF    ( FLG-HKNCOMBI    =   ZERO )
      *
                   PERFORM 200111-HKNBUNRUI-SEC
      *
                   IF    ( SS001-SRYKAKBN  =   "1" )
                       MOVE    JYURRK-SRYKA    TO  WRK-SRYKA
                       PERFORM 200111-SRYKA-HKNBUNRUI-SEC
                       IF    ( IDX-SRYKA   >   ZERO )
                           PERFORM 200111-GAIHKN-EDIT-SEC
                       END-IF
                   ELSE
                       MOVE    ZERO            TO  WRK-SRYKA
                       PERFORM 200111-SRYKA-HKNBUNRUI-SEC
                       IF    ( IDX-SRYKA   >   ZERO )
                           PERFORM 200111-GAIHKN-EDIT-SEC
                       END-IF
      *
                       MOVE    JYURRK-SRYKA    TO  WRK-SRYKA
                       PERFORM 200111-SRYKA-HKNBUNRUI-SEC
                       IF    ( IDX-SRYKA   >   ZERO )
                           PERFORM 200111-GAIHKN-EDIT-SEC
                       END-IF
      *
                   END-IF
               END-IF
      *
               PERFORM 900-JYURRK-KEY42-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-GAIHKN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    外来保険情報編集処理
      *****************************************************************
       200111-GAIHKN-EDIT-SEC          SECTION.
      *
           MOVE    JYURRK-SRYYMD   TO  WRK-YMD
           IF    ( JYURRK-NYUGAIKBN    =   "1" )
               MOVE    1    TO  SS001-SRYKA-NYU-DAY(IDX-SRYKA WRK-DD)
           ELSE
               MOVE    1    TO  SS001-SRYKA-GAI-DAY(IDX-SRYKA WRK-DD)
           END-IF
      *
           .
       200111-GAIHKN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院保険情報編集処理
      *****************************************************************
       20011-NYUHKN-EDIT-SEC           SECTION.
      *
           MOVE    1           TO  IDX-NYUGAI
      *
           PERFORM 200111-TNYURRK-SRYKA-SEC
      *
           PERFORM 900-NYUINACCT-KEY17-SEL-SEC
           IF    ( FLG-NYUINACCT       =   ZERO )
      *
               PERFORM 200111-TCOMBI-EDIT-SEC
      *
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   TCOMBI-MAX )
      *
                   MOVE    TCOMBI-HKNCOMBI (IDX1)
                                   TO  WRK-HKNCOMBINUM
                   PERFORM 900-HKNCOMBI-KEY-SEL-SEC
                   IF    ( FLG-HKNCOMBI    =   ZERO )
      *
                       PERFORM 200111-HKNBUNRUI-SEC
      *
                       PERFORM VARYING IDX-DAY FROM    WRK-STDAY   BY  1
                               UNTIL ( IDX-DAY >       WRK-EDDAY )
      *
                           IF    ( TNYURRK-SRYKA (IDX-DAY)
                                               NOT =   SPACE )
                            AND (( NACCT-DAY (IDX-DAY)
                                               =   WRK-HKNCOMBINUM )
                               OR( FLG-DOUJITSU-DAY (IDX-DAY)
                                               =   WRK-HKNCOMBINUM ))
                               IF    ( SS001-SRYKAKBN  =   "1" )
                                   MOVE    TNYURRK-SRYKA (IDX-DAY)
                                                   TO  WRK-SRYKA
                                   PERFORM 200111-SRYKA-HKNBUNRUI-SEC
                                   IF    ( IDX-SRYKA   >   ZERO )
                                       PERFORM 200111-NYUHKN-EDIT-SEC
                                   END-IF
                               ELSE
                                   MOVE    ZERO    TO  WRK-SRYKA
                                   PERFORM 200111-SRYKA-HKNBUNRUI-SEC
                                   IF    ( IDX-SRYKA   >   ZERO )
                                       PERFORM 200111-NYUHKN-EDIT-SEC
                                   END-IF
      *
                                   MOVE    TNYURRK-SRYKA (IDX-DAY)
                                                   TO  WRK-SRYKA
                                   PERFORM 200111-SRYKA-HKNBUNRUI-SEC
                                   IF    ( IDX-SRYKA   >   ZERO )
                                       PERFORM 200111-NYUHKN-EDIT-SEC
                                   END-IF
                               END-IF
                           END-IF
      *
                       END-PERFORM
      *
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       20011-NYUHKN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院保険情報編集処理
      *****************************************************************
       200111-NYUHKN-EDIT-SEC          SECTION.
      *
           MOVE    1               TO  SS001-SRYKA-NYURRK-DAY
                                           (IDX-SRYKA IDX-DAY)
                                       SS001-SRYKA-NYU-DAY
                                           (IDX-SRYKA IDX-DAY)
      *
           .
       200111-NYUHKN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴退避領域診療科編集処理
      *****************************************************************
       200111-TNYURRK-SRYKA-SEC        SECTION.
      *
           PERFORM 900-PTNYUINRRK-KEY41-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
      *
               IF    ( PTNYUINRRK-TENNYUYMD (1:6)  <   SS001-SRYYM )
                   MOVE    1           TO  WRK-STIDX
               ELSE
                   MOVE    PTNYUINRRK-TENNYUYMD (7:2)
                                       TO  WRK-STIDX
               END-IF
      *
               IF    ( PTNYUINRRK-TENSTUYMD (1:6)  >   SS001-SRYYM )
                   MOVE    31          TO  WRK-EDIDX
               ELSE
                   MOVE    PTNYUINRRK-TENSTUYMD (7:2)
                                       TO  WRK-EDIDX
               END-IF
      *
               IF    ( WRK-STDAY       >   WRK-STIDX )
                   MOVE    WRK-STDAY   TO  WRK-STIDX
               END-IF
      *
               IF    ( WRK-EDDAY       <   WRK-EDIDX )
                   MOVE    WRK-EDDAY   TO  WRK-EDIDX
               END-IF
      *
               PERFORM VARYING IDX-DAY     FROM    WRK-STIDX   BY  1
                       UNTIL ( IDX-DAY     >       WRK-EDIDX )
                   MOVE    PTNYUINRRK-NYUINKA
                                       TO  TNYURRK-SRYKA (IDX-DAY)
               END-PERFORM
      *
               PERFORM 900-PTNYUINRRK-KEY41-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200111-TNYURRK-SRYKA-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ退避領域編集処理
      *****************************************************************
       200111-TCOMBI-EDIT-SEC          SECTION.
      *
           INITIALIZE                  TCOMBI-AREA
                                       FLG-DOUJITSU-DAY-G
      *
           PERFORM VARYING IDX1    FROM    WRK-STDAY   BY  1
                   UNTIL ( IDX1    >       WRK-EDDAY )
      *
               IF    ( NACCT-DAY (IDX1)    >   ZERO )
                   MOVE    NACCT-DAY (IDX1)    TO  WRK-HKNCOMBINUM
                   PERFORM 2001111-TCOMBI-EDIT-SEC
               END-IF
      *
           END-PERFORM
      *
           INITIALIZE  ORCSNYUINDAY-AREA
           MOVE    NACCT-HOSPNUM       TO  LNK-NYUINDAY-HOSPNUM
           MOVE    NACCT-PTID          TO  LNK-NYUINDAY-PTID
           MOVE    NACCT-SRYYM         TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"          USING
                                           ORCSNYUINDAY-AREA
                                           SPA-AREA
           IF    ( LNK-NYUINDAY-RC     NOT =   ZERO )
               INITIALIZE              ORCSNYUINDAY-AREA
           END-IF
      *
           PERFORM VARYING IDX1   FROM    1   BY  1
                   UNTIL ( IDX1   >       31 )
      *
               IF    ( LNK-NYUINDAY-DOUJITSU-KBN(IDX1) = "1" )
                   MOVE    LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX1)
                                       TO  FLG-DOUJITSU-DAY(IDX1)
                                           WRK-HKNCOMBINUM
                   PERFORM 2001111-TCOMBI-EDIT-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       200111-TCOMBI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ退避領域編集処理
      *****************************************************************
       2001111-TCOMBI-EDIT-SEC          SECTION.
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   TCOMBI-MAX )
                   OR    ( WRK-HKNCOMBINUM
                                   =   TCOMBI-HKNCOMBI (IDX2) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    (     IDX2                >   TCOMBI-MAX )
               IF    ( CONST-TCOMBI-MAX    >   TCOMBI-MAX )
                   COMPUTE TCOMBI-MAX  =   TCOMBI-MAX  +   1
                   MOVE    WRK-HKNCOMBINUM
                                   TO  TCOMBI-HKNCOMBI
                                               (TCOMBI-MAX)
               END-IF
           END-IF
      *
           .
       2001111-TCOMBI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険分類処理
      *****************************************************************
       200111-HKNBUNRUI-SEC            SECTION.
      *
           IF    ( COMB-HKNCOMBINUM        =   CONST-HOUKATU )
                   MOVE    1               TO  SS001-HKT-FLG(IDX-NYUGAI)
           ELSE
               IF    ( COMB-HKNNUM         =   CONST-ROUSAI
                                            OR CONST-JIBAI
                                            OR CONST-KOUGAI )
                   CONTINUE
               ELSE
                   MOVE    1               TO  SS001-IHO-FLG(IDX-NYUGAI)
               END-IF
      *
               IF    ( COMB-KOHHKNNUM (1)  =   "970" )
                   MOVE    1               TO  SS001-DI3-FLG(IDX-NYUGAI)
               END-IF
      *
               EVALUATE    COMB-HKNNUM
               WHEN    SPACE
                   MOVE    1               TO  SS001-SYA-FLG(IDX-NYUGAI)
               WHEN    CONST-ROUSAI
                       MOVE    1           TO  SS001-RSI-FLG(IDX-NYUGAI)
               WHEN    CONST-JIBAI
                       MOVE    1           TO  SS001-JBI-FLG(IDX-NYUGAI)
                       IF    ( COMB-HOJOKBN    =   "1" )
                           MOVE    1       TO SS001-JBIR-FLG(IDX-NYUGAI)
                       ELSE
                           MOVE    1       TO SS001-JBIK-FLG(IDX-NYUGAI)
                       END-IF
               WHEN    CONST-KOUGAI
                       MOVE    1           TO  SS001-KGI-FLG(IDX-NYUGAI)
               WHEN    "060"
               WHEN    "067"
               WHEN    "068"
               WHEN    "069"
                   MOVE    1               TO  SS001-KOK-FLG(IDX-NYUGAI)
               WHEN    "040"
               WHEN    "039"
                   MOVE    1               TO  SS001-KOUKI-FLG
                                                            (IDX-NYUGAI)
               WHEN    OTHER
                   EVALUATE    COMB-HKNNUM (1:2)
                   WHEN    "90"
                   WHEN    "91"
                       MOVE    1           TO  SS001-CHIKEN-FLG
                                                            (IDX-NYUGAI)
                   WHEN    "98"
                       MOVE    1           TO  SS001-JHI-FLG(IDX-NYUGAI)
                   WHEN    OTHER
                       MOVE    1           TO  SS001-SYA-FLG(IDX-NYUGAI)
                   END-EVALUATE
               END-EVALUATE
           END-IF
      *
           .
       200111-HKNBUNRUI-EXT.
           EXIT.
      *****************************************************************
      *    診療科別保険分類処理
      *****************************************************************
       200111-SRYKA-HKNBUNRUI-SEC      SECTION.
      *
           PERFORM VARYING IDX-SRYKA   FROM    1   BY  1
                   UNTIL ( IDX-SRYKA   >   SS001-SRYKA-MAX )
                    OR   ( SS001-SRYKA (IDX-SRYKA)
                                       =   WRK-SRYKA )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-SRYKA       >   SS001-SRYKA-MAX )
               IF    ( CONST-SRYKA-MAX
                                   >   SS001-SRYKA-MAX )
                   COMPUTE SS001-SRYKA-MAX
                       =   SS001-SRYKA-MAX
                       +   1
      *
                   MOVE    WRK-SRYKA
                                   TO  SS001-SRYKA (SS001-SRYKA-MAX)
      *
                   MOVE    SS001-SRYKA-MAX
                                   TO  IDX-SRYKA
      *
               ELSE
                   MOVE    ZERO    TO  IDX-SRYKA
               END-IF
           END-IF
      *
           IF    ( IDX-SRYKA       >   ZERO )
      *
               IF    ( COMB-HKNCOMBINUM    =   CONST-HOUKATU )
                       MOVE    1       TO  SS001-SRYKA-HKT-FLG
                                                  (IDX-SRYKA IDX-NYUGAI)
               ELSE
                   IF    ( COMB-HKNNUM    =    CONST-ROUSAI
                                           OR  CONST-JIBAI
                                           OR  CONST-KOUGAI  )
                       CONTINUE
                   ELSE
                       MOVE    1       TO  SS001-SRYKA-IHO-FLG
                                                  (IDX-SRYKA IDX-NYUGAI)
                   END-IF
      *
                   IF    ( COMB-KOHHKNNUM (1)  =   "970" )
                       MOVE    1       TO  SS001-SRYKA-DI3-FLG
                                                  (IDX-SRYKA IDX-NYUGAI)
                   END-IF
      *
                   EVALUATE    COMB-HKNNUM
                   WHEN    SPACE
                       MOVE    1       TO  SS001-SRYKA-SYA-FLG
                                                  (IDX-SRYKA IDX-NYUGAI)
                   WHEN    CONST-ROUSAI
                       MOVE    1       TO  SS001-SRYKA-RSI-FLG 
                                                  (IDX-SRYKA IDX-NYUGAI)
                   WHEN    CONST-JIBAI
                       MOVE    1       TO  SS001-SRYKA-JBI-FLG
                                                  (IDX-SRYKA IDX-NYUGAI)
                       IF    ( COMB-HOJOKBN    =   "1" )
                           MOVE    1       TO  SS001-SRYKA-JBIR-FLG
                                                  (IDX-SRYKA IDX-NYUGAI)
                       ELSE
                           MOVE    1       TO  SS001-SRYKA-JBIK-FLG
                                                  (IDX-SRYKA IDX-NYUGAI)
                       END-IF
                   WHEN    CONST-KOUGAI
                       MOVE    1       TO  SS001-SRYKA-KGI-FLG
                                                  (IDX-SRYKA IDX-NYUGAI)
                   WHEN    "060"
                   WHEN    "067"
                   WHEN    "068"
                   WHEN    "069"
                       MOVE    1       TO  SS001-SRYKA-KOK-FLG
                                                  (IDX-SRYKA IDX-NYUGAI)
                   WHEN    "040"
                   WHEN    "039"
                       MOVE    1       TO  SS001-SRYKA-KOUKI-FLG
                                                  (IDX-SRYKA IDX-NYUGAI)
                   WHEN    OTHER
                       EVALUATE    COMB-HKNNUM (1:2)
                       WHEN    "90"
                       WHEN    "91"
                           MOVE    1   TO  SS001-SRYKA-CHIKEN-FLG
                                                  (IDX-SRYKA IDX-NYUGAI)
                       WHEN    "98"
                           MOVE    1   TO  SS001-SRYKA-JHI-FLG
                                                  (IDX-SRYKA IDX-NYUGAI)
                       WHEN    OTHER
                           MOVE    1   TO  SS001-SRYKA-SYA-FLG
                                                  (IDX-SRYKA IDX-NYUGAI)
                       END-EVALUATE
                   END-EVALUATE
               END-IF
           END-IF
      *
           .
       200111-SRYKA-HKNBUNRUI-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                SECTION.
      *
           IF    ( SS001-SRYKAKBN  NOT =   "1" )
               IF    ( SS001-SRYKA-MAX     >   1 )
                   MOVE    1       TO  SS001-SRYKA-MAX
               END-IF
           END-IF
      *
      D    PERFORM 800-DISPLAY-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    DISPLAY処理
      *****************************************************************
       800-DISPLAY-SEC             SECTION.
      *
           DISPLAY "HOSPNUM = " SS001-HOSPNUM
           DISPLAY "PTID    = " SS001-PTID
           DISPLAY "SRYYM   = " SS001-SRYYM
      *
           DISPLAY "GIHO-FLG   = " SS001-GIHO-FLG
           DISPLAY "GRSI-FLG   = " SS001-GRSI-FLG
           DISPLAY "GJBI-FLG   = " SS001-GJBI-FLG
           DISPLAY "GJBIR-FLG  = " SS001-GJBIR-FLG
           DISPLAY "GJBIK-FLG  = " SS001-GJBIK-FLG
           DISPLAY "GSYA-FLG   = " SS001-GSYA-FLG
           DISPLAY "GKOK-FLG   = " SS001-GKOK-FLG
           DISPLAY "GJHI-FLG   = " SS001-GJHI-FLG
           DISPLAY "GKGI-FLG   = " SS001-GKGI-FLG
           DISPLAY "GDI3-FLG   = " SS001-GDI3-FLG
           DISPLAY "GHKT-FLG   = " SS001-GHKT-FLG
      *
           DISPLAY "NIHO-FLG   = " SS001-NIHO-FLG
           DISPLAY "NRSI-FLG   = " SS001-NRSI-FLG
           DISPLAY "NJBI-FLG   = " SS001-NJBI-FLG
           DISPLAY "NJBIR-FLG  = " SS001-NJBIR-FLG
           DISPLAY "NJBIK-FLG  = " SS001-NJBIK-FLG
           DISPLAY "NSYA-FLG   = " SS001-NSYA-FLG
           DISPLAY "NKOK-FLG   = " SS001-NKOK-FLG
           DISPLAY "NJHI-FLG   = " SS001-NJHI-FLG
           DISPLAY "NKGI-FLG   = " SS001-NKGI-FLG
           DISPLAY "NDI3-FLG   = " SS001-NDI3-FLG
           DISPLAY "NHKT-FLG   = " SS001-NHKT-FLG
      *
           DISPLAY "SRYKA-MAX  = " SS001-SRYKA-MAX
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SS001-SRYKA-MAX )
      *
               DISPLAY IDX0 ":SRYKA = " SS001-SRYKA (IDX0)
      *
               DISPLAY "SRYKA (" IDX0  ")= " SS001-SRYKA (IDX0)
      *
               DISPLAY       "SRYKA-IHO-FLG ( " IDX0 "1 )="
                        SS001-SRYKA-IHO-FLG (IDX0 1)
               DISPLAY       "SRYKA-IHO-FLG ( " IDX0 "2 )="
                        SS001-SRYKA-IHO-FLG (IDX0 2)
      *
               DISPLAY       "SRYKA-RSI-FLG ( " IDX0 "1 )="
                        SS001-SRYKA-RSI-FLG (IDX0 1)
               DISPLAY       "SRYKA-RSI-FLG ( " IDX0 "2 )="
                        SS001-SRYKA-RSI-FLG (IDX0 2)
      *
               DISPLAY       "SRYKA-JBI-FLG ( " IDX0 "1 )="
                        SS001-SRYKA-JBI-FLG (IDX0 1)
               DISPLAY       "SRYKA-JBI-FLG ( " IDX0 "2 )="
                        SS001-SRYKA-JBI-FLG (IDX0 2)
      *
               DISPLAY       "SRYKA-JBIR-FLG ( " IDX0 "1 )="
                        SS001-SRYKA-JBIR-FLG (IDX0 1)
               DISPLAY       "SRYKA-JBIR-FLG ( " IDX0 "2 )="
                        SS001-SRYKA-JBIR-FLG (IDX0 2)
      *
               DISPLAY       "SRYKA-JBIK-FLG ( " IDX0 "1 )="
                        SS001-SRYKA-JBIK-FLG (IDX0 1)
               DISPLAY       "SRYKA-JBIK-FLG ( " IDX0 "2 )="
                        SS001-SRYKA-JBIK-FLG (IDX0 2)
      *
               DISPLAY       "SRYKA-SYA-FLG ( " IDX0 "1 )="
                        SS001-SRYKA-SYA-FLG (IDX0 1)
               DISPLAY       "SRYKA-SYA-FLG ( " IDX0 "2 )="
                        SS001-SRYKA-SYA-FLG (IDX0 2)
      *
               DISPLAY       "SRYKA-KOK-FLG ( " IDX0 "1 )="
                        SS001-SRYKA-KOK-FLG (IDX0 1)
               DISPLAY       "SRYKA-KOK-FLG ( " IDX0 "2 )="
                        SS001-SRYKA-KOK-FLG (IDX0 2)
      *
               DISPLAY       "SRYKA-KOUKI-FLG ( " IDX0 "1 )="
                        SS001-SRYKA-KOUKI-FLG (IDX0 1)
               DISPLAY       "SRYKA-KOUKI-FLG ( " IDX0 "2 )="
                        SS001-SRYKA-KOUKI-FLG (IDX0 2)
      *
               DISPLAY       "SRYKA-JHI-FLG ( " IDX0 "1 )="
                        SS001-SRYKA-JHI-FLG (IDX0 1)
               DISPLAY       "SRYKA-JHI-FLG ( " IDX0 "2 )="
                        SS001-SRYKA-JHI-FLG (IDX0 2)
      *
               DISPLAY       "SRYKA-HKT-FLG ( " IDX0 "1 )="
                        SS001-SRYKA-HKT-FLG (IDX0 1)
               DISPLAY       "SRYKA-HKT-FLG ( " IDX0 "2 )="
                        SS001-SRYKA-HKT-FLG (IDX0 2)
      *
               DISPLAY       "SRYKA-DI3-FLG ( " IDX0 "1 )="
                        SS001-SRYKA-DI3-FLG (IDX0 1)
               DISPLAY       "SRYKA-DI3-FLG ( " IDX0 "2 )="
                        SS001-SRYKA-DI3-FLG (IDX0 2)
      *
               DISPLAY       "SRYKA-DAY-OCC ( " IDX0 "1 )="
                        SS001-SRYKA-DAY-OCC (IDX0 1)
               DISPLAY       "SRYKA-DAY-OCC ( " IDX0 "2 )="
                        SS001-SRYKA-DAY-OCC (IDX0 2)
               DISPLAY       "SRYKA-NYURRK-DAY-G ( " IDX0 ")="
                        SS001-SRYKA-NYURRK-DAY-G (IDX0)
      *
           END-PERFORM
      *
           .
       800-DISPLAY-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴検索処理
      *****************************************************************
       900-JYURRK-KEY42-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-JYURRK
      *
           INITIALIZE                  JYURRK-REC
grpsys     MOVE    SS001-HOSPNUM       TO  JYURRK-HOSPNUM
           MOVE    SS001-PTID          TO  JYURRK-PTID
           STRING  SS001-SRYYM         DELIMITED   BY  SIZE
                   "%"                 DELIMITED   BY  SIZE
           INTO    JYURRK-SRYYMD
           END-STRING
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  JYURRK-REC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-KEY42-SEL-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴FETCH処理
      *****************************************************************
       900-JYURRK-KEY42-FET-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-JYURRK
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  JYURRK-REC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-KEY42-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理(KEY17)
      *****************************************************************
       900-NYUINACCT-KEY17-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                  NYUINACCT-REC
      *
grpsys     MOVE    SS001-HOSPNUM       TO  NACCT-HOSPNUM
           MOVE    SS001-PTID          TO  NACCT-PTID
           MOVE    SS001-SRYYM         TO  NACCT-SRYYM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUINACCT-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                    TO  FLG-HKNCOMBI
      *
           PERFORM 9001-TBLCOMBI-CHK-SEC
           IF   (  FLG-TBLCOMBI    =       ZERO )
               INITIALIZE                      HKNCOMBI-REC
grpsys         MOVE    SS001-HOSPNUM       TO  COMB-HOSPNUM
               MOVE    SS001-PTID          TO  COMB-PTID
               MOVE    WRK-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   IF    ( COMB-HKNNUM     =   "973" )
                       PERFORM 900-PTRSIINF-KEY-SEL-SEC
                       IF    ( PTRSI-JIBAIKBN  =   "1" OR "2" )
                           MOVE    PTRSI-JIBAIKBN
                                           TO  COMB-HOJOKBN
                       ELSE
                           MOVE    SYS-4001-JIBAIKBN
                                           TO  COMB-HOJOKBN
                       END-IF
                   END-IF
                   IF    ( CONST-TBLCOMBI-MAX  >   TBLCOMBI-MAX )
                       COMPUTE TBLCOMBI-MAX
                           =   TBLCOMBI-MAX    +   1
                       MOVE    HKNCOMBI-REC TO TBLCOMBI-OCC
                                                  (TBLCOMBI-MAX)
      *
      *
                       MOVE    TBLCOMBI-MAX TO SS001-HKNCOMBI-MAX
                       MOVE    COMB-HKNCOMBINUM
                                            TO SS001-HKNCOMBINUM
                                                  (TBLCOMBI-MAX)
      *
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
                   INITIALIZE                  HKNCOMBI-REC
               END-IF
      *
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
      *
           .
       900-HKNCOMBI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       9001-TBLCOMBI-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TBLCOMBI
      *
           PERFORM VARYING IDX-TBLCOMBI    FROM    1   BY  1
                   UNTIL ( IDX-TBLCOMBI    >   TBLCOMBI-MAX )
                    OR   ( FLG-TBLCOMBI    =   1 )
      *
               IF    ( TBLCOMBI-HOSPNUM  (IDX-TBLCOMBI)
                                           =   SS001-HOSPNUM )
                AND  ( TBLCOMBI-PTID     (IDX-TBLCOMBI)
                                           =   SS001-PTID )
                AND  ( TBLCOMBI-HKNCOMBINUM (IDX-TBLCOMBI)
                                           =   WRK-HKNCOMBINUM )
      *
                   MOVE    1           TO  FLG-TBLCOMBI
      *
                   MOVE    TBLCOMBI-OCC (IDX-TBLCOMBI)
                                       TO  HKNCOMBI-REC
               END-IF
      *
           END-PERFORM
      *
           .
       9001-TBLCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理(KEY41)
      *****************************************************************
       900-PTNYUINRRK-KEY41-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE                      PTNYUINRRK-REC
      *
           MOVE    SS001-HOSPNUM       TO  PTNYUINRRK-HOSPNUM
           MOVE    SS001-PTID          TO  PTNYUINRRK-PTID
           MOVE    SS001-SRYYM         TO  PTNYUINRRK-TENNYUYMD
           MOVE    "31"                TO  PTNYUINRRK-TENNYUYMD
           MOVE    SS001-SRYYM         TO  PTNYUINRRK-TENSTUYMD
           MOVE    "01"                TO  PTNYUINRRK-TENSTUYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY41-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴FETCH処理(KEY41)
      *****************************************************************
       900-PTNYUINRRK-KEY41-FET-SEC     SECTION.
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY41-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者労災情報検索処理
      *****************************************************************
       900-PTRSIINF-KEY-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-PTRSIINF
      *
           INITIALIZE                      PTRSIINF-REC
           MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
           MOVE    COMB-PTID           TO  PTRSI-PTID
           MOVE    COMB-HKNID          TO  PTRSI-HKNID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
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
      *
       900-PTRSIINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
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
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
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
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
