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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCSS009.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 科別・保険別入院会計編集サブ
      *  管理者            :
      *  作成日付    作業者        記述
      *  06/06/20    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/05/28  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *
       WORKING-STORAGE                 SECTION.
      *
      *
       01  FLG-AREA.
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-LOOP-END                PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ZAINUM                  PIC 9(08).
      *
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX4                        PIC 9(05).
           03  IDX5                        PIC 9(05).
           03  IDX6                        PIC 9(05).
           03  IDX7                        PIC 9(05).
           03  IDX8                        PIC 9(05).
           03  IDX9                        PIC 9(05).
           03  IDX-DAY                     PIC 9(02).
      *
       01  WRK-AREA.
           03  WRK-COMB-HKNCOMBINUM        PIC 9(04).
           03  WRK-HKNCOMBINUM-G.
               05  WRK-HKNCOMBINUM         PIC 9(04).
               05  WRK-HKNCOMBINUM-SKJHKN  PIC X(01).
           03  WRK-HBTSRYKA-G.
               05  WRK-HBTSRYKA-HBTKA      PIC X(02)   OCCURS  31.
               05  WRK-HBTSRYKA-MAX        PIC 9(05).
               05  WRK-HBTSRYKA-OCC        OCCURS  100.
                   07  WRK-HBTSRYKA        PIC X(02).
                   07  WRK-HBTSRYKA-DAY    PIC 9(01)   OCCURS  31.
           03  WRK-STIDX                   PIC 9(02).
           03  WRK-EDIDX                   PIC 9(02).
           03  WRK-DAY-G.
               05  WRK-DAY                 PIC 9(03)   OCCURS  31.
      *
       01  TNRRK-AREA.
           03  TNRRK-MAX                   PIC 9(05).
           03  TNRRK-OCC                   OCCURS  100.
               05  TNRRK-SRYKA             PIC X(02).
               05  TNRRK-DAY-OCC           OCCURS  31.
                   07  TNRRK-SRYKA-DAY     PIC 9(01).
                   07  TNRRK-BRMNUM        PIC X(08).
      *
       01  TCOMBI-AREA.
           03  TCOMBI-MAX                  PIC 9(05).
           03  TCOMBI-HKNCOMBI-OCC         OCCURS  50.
               05  TCOMBI-HKNCOMBI         PIC 9(04).
               05  TCOMBI-HKNCOMBI-CNT     PIC 9(03).
               05  TCOMBI-HKNCOMBI-DAY     PIC 9(01)
                                           OCCURS  31.
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-H2910                 PIC X(06) VALUE "201710".
           03  CONST-H180401               PIC X(08) VALUE "20060401".
           03  CONST-ZAINUM-MAX            PIC 9(05) VALUE  200.
           03  CONST-TNRRK-MAX             PIC 9(05) VALUE  100.
           03  CONST-SRYCD-MAX             PIC 9(05) VALUE 20.
           03  CONST-TCOMBI-MAX            PIC 9(05) VALUE 50.
           03  CONST-OTHER                 PIC X(01) VALUE " ".
           03  CONST-NSRYSRH               PIC X(01) VALUE "1".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    入院診療会計マスター
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
           COPY    "CPORCSS006.INC".
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    入院会計編集サブ
           COPY    "CPORCHCM34S01.INC".
      *
      *    入院料と異なる食事算定保険組み合わせ取得サブ
           COPY    "CPORCSSKJHKN.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                         SECTION.
           COPY    "CPORCSS009.INC".
       01  LNKNACCT-REC.
           COPY    "CPNYUINACCT.INC"
                                   REPLACING   //NACCT//
                                   BY          //LNKNACCT//.
           COPY    "COMMON-SPA".
      ****************************************************************
       PROCEDURE                       DIVISION
               USING
           SS009-AREA
           LNKNACCT-REC
           SPA-AREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           MOVE    FLG-ERR         TO  SS009-RC
      *
           EXIT PROGRAM
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  IDX-AREA
                                       CNT-AREA
                                       FLG-AREA
                                       WRK-AREA
                                       SS009-OT-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    LNKNACCT-REC        TO  NYUINACCT-REC
      *
           PERFORM 2001-TNRRK-HEN-SEC
      *
           PERFORM 2001-TCOMBI-HEN-SEC
      *
           PERFORM 2001-NACCT-EDIT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    日別診療科編集処理
      *****************************************************************
       2001-TNRRK-HEN-SEC              SECTION.
      *
           INITIALIZE                  TNRRK-AREA
      *
           PERFORM 900-PTNYUINRRK-KEY41-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
      *
               IF    ( PTNYUINRRK-TENNYUYMD (1:6)  <   NACCT-SRYYM )
                   MOVE    1           TO  WRK-STIDX
               ELSE
                   MOVE    PTNYUINRRK-TENNYUYMD (7:2)
                                       TO  WRK-STIDX
               END-IF
      *
               IF    ( PTNYUINRRK-TENSTUYMD (1:6)  >   NACCT-SRYYM )
                   MOVE    31          TO  WRK-EDIDX
               ELSE
                   MOVE    PTNYUINRRK-TENSTUYMD (7:2)
                                       TO  WRK-EDIDX
               END-IF
      *
               PERFORM VARYING IDX-DAY     FROM    WRK-STIDX   BY  1
                       UNTIL ( IDX-DAY     >       WRK-EDIDX )
      *
                   MOVE    PTNYUINRRK-NYUINKA
                                   TO  SS009-SRYKA-DAY(IDX-DAY)
                   MOVE    PTNYUINRRK-BRMNUM
                                   TO  SS009-BRMNUM-DAY(IDX-DAY)
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
           PERFORM VARYING IDX-DAY     FROM    1   BY  1
                   UNTIL ( IDX-DAY     >       31 )
      *
               IF    ( SS009-SRYKA-DAY(IDX-DAY)
                                   NOT =   SPACE )
      *
                   PERFORM VARYING IDX5    FROM    1   BY  1
                       UNTIL ( IDX5    >   TNRRK-MAX )
                       OR    ( TNRRK-SRYKA (IDX5)
                                       = SS009-SRYKA-DAY(IDX-DAY))
      *
                       CONTINUE
      *
                   END-PERFORM
      *
                   MOVE    ZERO        TO  IDX6
      *
                   IF    ( IDX5    >   TNRRK-MAX )
      *
                       IF    ( TNRRK-MAX       <   CONST-TNRRK-MAX )
                           COMPUTE TNRRK-MAX   =   TNRRK-MAX   +   1
      *
                           MOVE    TNRRK-MAX   TO  IDX6
      *
                           MOVE    SS009-SRYKA-DAY(IDX-DAY)
                                       TO  TNRRK-SRYKA (IDX6)
                       END-IF
                   ELSE
                       MOVE    IDX5    TO  IDX6
                   END-IF
      *
                   IF    ( IDX6        >   ZERO )
                       MOVE    1   TO  TNRRK-SRYKA-DAY(IDX6 IDX-DAY)
                       MOVE    SS009-BRMNUM-DAY(IDX-DAY)
                                   TO  TNRRK-BRMNUM   (IDX6 IDX-DAY)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       2001-TNRRK-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ情報編集処理
      *****************************************************************
       2001-TCOMBI-HEN-SEC             SECTION.
      *
           INITIALIZE                  TCOMBI-AREA
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   31 )
      *
               IF    ( NACCT-DAY (IDX1)    >   ZERO )
                   INITIALIZE                      WRK-HKNCOMBINUM-G
                   MOVE    NACCT-DAY (IDX1)    TO  WRK-HKNCOMBINUM
                   PERFORM 20011-TCOMBI-HEN-SEC
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
                   INITIALIZE              WRK-HKNCOMBINUM-G
                   MOVE    LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI(IDX1)
                                       TO  WRK-HKNCOMBINUM
                   PERFORM 20011-TCOMBI-HEN-SEC
               END-IF
      *
           END-PERFORM
      *
           IF    ( NACCT-SRYYM         >= CONST-H2910 )
               INITIALIZE              ORCSSKJHKN-AREA
               MOVE    NACCT-HOSPNUM   TO  LNK-SKJHKN-HOSPNUM
               MOVE    NACCT-PTID      TO  LNK-SKJHKN-PTID
               MOVE    NACCT-SRYYM     TO  LNK-SKJHKN-SANTEIYM
               CALL    "ORCSSKJHKN"    USING
                                       ORCSSKJHKN-AREA
                                       ORCSNYUINDAY-AREA
                                       SPA-AREA
               IF    ( LNK-SKJHKN-RC   =   ZERO )
                   PERFORM VARYING IDX1   FROM    1   BY  1
                           UNTIL ( IDX1   >       31 )
                       PERFORM VARYING IDX9    FROM    1   BY  1
                               UNTIL ( IDX9    >   2 )
                           IF    ( LNK-SKJHKN-HKNCOMBINUM  (IDX1 IDX9)
                                       NOT =   SPACE )
                               MOVE LNK-SKJHKN-HKNCOMBINUM (IDX1 IDX9)
                                       TO  WRK-HKNCOMBINUM-G
                               MOVE "1"
                                       TO  WRK-HKNCOMBINUM-SKJHKN
                               PERFORM 20011-TCOMBI-HEN-SEC
                           END-IF
                       END-PERFORM
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       2001-TCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ情報編集処理
      *****************************************************************
       20011-TCOMBI-HEN-SEC            SECTION.
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2   >   TCOMBI-MAX )
                   OR    ( WRK-HKNCOMBINUM
                                   =   TCOMBI-HKNCOMBI (IDX2) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    (     IDX2                >   TCOMBI-MAX )
               IF    ( CONST-TCOMBI-MAX    >   TCOMBI-MAX )
                   MOVE    IDX2            TO  TCOMBI-MAX
                   MOVE    WRK-HKNCOMBINUM
                                           TO  TCOMBI-HKNCOMBI
                                                       (TCOMBI-MAX)
               ELSE
                   MOVE    ZERO            TO  IDX2
               END-IF
           END-IF
      *
           IF    ( IDX2    >   ZERO )
               IF    ( TCOMBI-HKNCOMBI-DAY (IDX2 IDX1)
                                           =    ZERO )
                   COMPUTE TCOMBI-HKNCOMBI-CNT (IDX2)
                       =   TCOMBI-HKNCOMBI-CNT (IDX2)
                       +   1
                   IF    ( WRK-HKNCOMBINUM-SKJHKN =   SPACE )
                       MOVE    1   TO  TCOMBI-HKNCOMBI-DAY (IDX2 IDX1)
                   END-IF
               END-IF
           END-IF
      *
           .
       20011-TCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院会計情報編集処理
      *****************************************************************
       2001-NACCT-EDIT-SEC             SECTION.
      *
           MOVE    ZERO                TO  CNT-ZAINUM
      *
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL ( IDX3    >   TCOMBI-MAX )
      *
               MOVE    TCOMBI-HKNCOMBI (IDX3)  TO  WRK-COMB-HKNCOMBINUM
               PERFORM 900-HKNCOMBI-KEY-SEL-SEC
      *
               INITIALIZE                  HCM34S01-AREA
               MOVE    NACCT-HOSPNUM       TO  HCM34S01-HOSPNUM
               MOVE    NACCT-PTID          TO  HCM34S01-PTID
               MOVE    NACCT-SRYYM         TO  HCM34S01-SANTEIYM
               MOVE    COMB-HKNCOMBINUM    TO  HCM34S01-HKNCOMBI
               MOVE    COMB-HKNNUM         TO  HCM34S01-HKNNUM
               MOVE    SS009-EXGAIHAKU     TO  HCM34S01-EXGAIHAKU
               IF    ( SS009-REQUEST       =   CONST-NSRYSRH )
                   MOVE    "1"             TO  HCM34S01-KIKANKBN
                   MOVE    SS009-STYMD     TO  HCM34S01-SKYSTYMD
                   MOVE    SS009-EDYMD     TO  HCM34S01-SKYEDYMD
      *
                   INITIALIZE                  SS006-AREA
                   MOVE    SS009-STYMD     TO  SS006-SKYSTYMD
                   MOVE    SS009-EDYMD     TO  SS006-SKYEDYMD
                   MOVE    "1"             TO  SS006-SKYKBN
                   CALL    "ORCSS006"          USING
                                               SS006-AREA
                                               SPA-AREA
                   IF    ( SS009-STYMD     <   CONST-H180401 )
                       CONTINUE
                   ELSE
                       MOVE    SS006-DOJITUFLG-ST
                                           TO  HCM34S01-DOJITUFLG-ST
                       MOVE    SS006-TIMEKBN-ST
                                           TO  HCM34S01-TIMEKBN-ST
                   END-IF
                   IF    ( SS009-EDYMD     <  CONST-H180401 )
                       CONTINUE
                   ELSE
                       MOVE    SS006-DOJITUFLG-ED
                                           TO  HCM34S01-DOJITUFLG-ED
                       MOVE    SS006-TIMEKBN-ED
                                           TO  HCM34S01-TIMEKBN-ED
                   END-IF
               END-IF
      *
               IF    ( COMB-HKNNUM         =   "971" OR "973" )
      *
                   CALL    "ORCHCM34S02"       USING HCM34S01-AREA
                                                     SPA-AREA
               ELSE
                   CALL    "ORCHCM34S01"       USING HCM34S01-AREA
                                                     SPA-AREA
               END-IF
               MOVE    HCM34S01-RC         TO  FLG-ERR
      *
               PERFORM 20011-SS009-OT-EDIT-SEC
      *
           END-PERFORM
      *
           .
       2001-NACCT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院会計情報追加処理
      *****************************************************************
       20011-SS009-OT-EDIT-SEC             SECTION.
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4    >   HCM34S01-HENKYAKU-MAX )
      *
               IF    ( HCM34S01-ZAISKBKBN (IDX4)   NOT =   "3" )
                AND  ( HCM34S01-ZAISKBKBN (IDX4)   NOT =   "5" )
                   PERFORM VARYING IDX7    FROM    1   BY  1
                           UNTIL ( IDX7    >   TNRRK-MAX )
      *
                       MOVE    ZERO    TO  WRK-DAY-G
      *
                       PERFORM VARYING IDX-DAY FROM    1   BY  1
                               UNTIL ( IDX-DAY >   31 )
                           IF    ( TNRRK-SRYKA-DAY (IDX7 IDX-DAY )
                                               > ZERO )
                            AND  ( HCM34S01-DAY     (IDX4 IDX-DAY )
                                               > ZERO )
                               MOVE    HCM34S01-DAY     (IDX4 IDX-DAY )
                                       TO  WRK-DAY (IDX-DAY)
                           END-IF
                       END-PERFORM
      *
                       IF    ( WRK-DAY-G   NOT =   ZERO )
                           IF    ( CONST-ZAINUM-MAX     >   CNT-ZAINUM )
                               PERFORM 200111-SS009-OT-EDIT-SEC
                           ELSE
                               DISPLAY "NACCT ZAINUM OVER:" NACCT-PTID
                           END-IF
                       END-IF
      *
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
      *
               IF    ( TCOMBI-HKNCOMBI-DAY (IDX3 IDX-DAY )
                                           > ZERO )
                   MOVE    TCOMBI-HKNCOMBI (IDX3)
                                       TO  SS009-HKNCOMBI-DAY (IDX-DAY)
               END-IF
      *
               IF    ( HCM34S01-SAGAKUCD (IDX-DAY) NOT =   SPACE )
                   MOVE    HCM34S01-SAGAKUCD (IDX-DAY)
                                       TO  SS009-SAGAKUCD-DAY (IDX-DAY)
               END-IF
      *
           END-PERFORM
      *
           .
       20011-SS009-OT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院会計情報追加処理
      *****************************************************************
       200111-SS009-OT-EDIT-SEC    SECTION.
      *
           COMPUTE CNT-ZAINUM  =   CNT-ZAINUM  +   1
      *
           MOVE    CNT-ZAINUM  TO  SS009-HENKYAKU-MAX
      *
           MOVE    TNRRK-SRYKA    (IDX7)
                               TO  SS009-SRYKA (CNT-ZAINUM)
      *
           MOVE    COMB-HKNCOMBINUM
                               TO  SS009-HKNCOMBI
                                                   (CNT-ZAINUM)
           MOVE    HCM34S01-SRYKBN (IDX4)
                               TO  SS009-SRYKBN
                                                   (CNT-ZAINUM)
      *
           MOVE    HCM34S01-SRYSYUKBN (IDX4)
                               TO  SS009-SRYSYUKBN
                                                   (CNT-ZAINUM)
      *
           MOVE    HCM34S01-ZAISKBKBN (IDX4)
                               TO  SS009-ZAISKBKBN
                                                   (CNT-ZAINUM)
      *
           MOVE    HCM34S01-ZAITEN (IDX4)
                               TO  SS009-ZAITEN
                                                   (CNT-ZAINUM)
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
      *
               COMPUTE SS009-ZAIKAISU (CNT-ZAINUM)
                   =   SS009-ZAIKAISU (CNT-ZAINUM)
                   +   WRK-DAY (IDX-DAY)
      *
               MOVE    WRK-DAY (IDX-DAY)
                               TO  SS009-DAY
                                          (CNT-ZAINUM IDX-DAY)
      *
           END-PERFORM
      *
           MOVE    HCM34S01-MEISAISU (IDX4)
                               TO  SS009-MEISAISU
                                                   (CNT-ZAINUM)
      *
           MOVE    HCM34S01-SRYCDTOTAL (IDX4)
                               TO  SS009-SRYCDTOTAL
                                                   (CNT-ZAINUM)
      *
           MOVE    HCM34S01-SURYOUTOTAL (IDX4)
                               TO  SS009-SURYOUTOTAL
                                                   (CNT-ZAINUM)
      *
           MOVE    HCM34S01-RUIKEITOTAL (IDX4)
                               TO  SS009-RUIKEITOTAL
                                                   (CNT-ZAINUM)
      *
           PERFORM VARYING IDX8    FROM    1   BY  1
                   UNTIL ( IDX8    >   CONST-SRYCD-MAX )
                    OR   ( HCM34S01-SRYCD (IDX4 IDX8)
                                   =   SPACE )
      *
               MOVE    HCM34S01-SURYO (IDX4 IDX8)
                                   TO  SS009-SURYO
                                                   (CNT-ZAINUM IDX8)
      *
               MOVE    HCM34S01-SRYCD (IDX4 IDX8)
                                   TO  SS009-SRYCD
                                                   (CNT-ZAINUM IDX8)
      *
           END-PERFORM
      *
           .
       200111-SS009-OT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理(KEY41)
      *****************************************************************
       900-PTNYUINRRK-KEY41-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE                  PTNYUINRRK-REC
      *
           MOVE    NACCT-HOSPNUM        TO  PTNYUINRRK-HOSPNUM
           MOVE    NACCT-PTID          TO  PTNYUINRRK-PTID
           MOVE    NACCT-SRYYM         TO  PTNYUINRRK-TENNYUYMD
           MOVE    "31"                TO  PTNYUINRRK-TENNYUYMD(7:2)
           MOVE    NACCT-SRYYM         TO  PTNYUINRRK-TENSTUYMD
           MOVE    "01"                TO  PTNYUINRRK-TENSTUYMD(7:2)
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 900-PTNYUINRRK-KEY41-FET-SEC
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
           MOVE    ZERO                TO  FLG-LOOP-END
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key41"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
      *
                   MOVE    1               TO  FLG-LOOP-END
      *
                   IF    ( SS009-REQUEST   =   CONST-NSRYSRH )
                       IF    ( PTNYUINRRK-RRKNUM
                                           =   SS009-RRKNUM )
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-LOOP-END
                       END-IF
                   END-IF
      *
               ELSE
                   MOVE    1               TO  FLG-PTNYUINRRK
                   INITIALIZE                  PTNYUINRRK-REC
               END-IF
           END-PERFORM
      *
           .
       900-PTNYUINRRK-KEY41-FET-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                    TO  FLG-HKNCOMBI
      *
           INITIALIZE                          HKNCOMBI-REC
           MOVE    NACCT-HOSPNUM           TO  COMB-HOSPNUM
           MOVE    NACCT-PTID              TO  COMB-PTID
           MOVE    WRK-COMB-HKNCOMBINUM    TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC            TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC                  =   ZERO )
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                      HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNCOMBI-KEY-SEL-EXT.
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
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
