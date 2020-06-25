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
       PROGRAM-ID.             ORCSORCAPG.
      *****************************************************************
      *  システム名        : ORCA
      *  サブシステム名    : 
      *  コンポーネント名  : 選択式プログラム一覧取得
      *  返却エラーコード  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  18/07/02    NACL-太田     新規作成
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
      *
       WORKING-STORAGE             SECTION.
      *
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDXT                PIC 9(05).
      *
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-EXISTS          PIC 9(01).
      *
       01  WRK-AREA.
           03  WRK-DUMMY           PIC 9(01).
           03  WRK-TYPE            PIC X(16).
           03  WRK-KANRICD         PIC X(04).
           03  WRK-SITEKBN         PIC X(01).
      *
       01  CONST-AREA.
           03  CONST-INFO-VAL.
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000001".
               05  FILLER          PIC X(20)   VALUE "ORCHC01".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE "1".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000001".
               05  FILLER          PIC X(20)   VALUE "A21000H01A4".
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000001".
               05  FILLER          PIC X(20)   VALUE "A21000H01B5".
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000002".
               05  FILLER          PIC X(20)   VALUE "ORCHCM19".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE "1".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000002".
               05  FILLER          PIC X(20)   VALUE "ORCHC02".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000002".
               05  FILLER          PIC X(20)   VALUE "ORCHC02Q".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000002".
               05  FILLER          PIC X(20)   VALUE "A00000H02".
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000002".
               05  FILLER          PIC X(20)   VALUE "A21000H02".
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000003".
               05  FILLER          PIC X(20)   VALUE "ORCHC03".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE "1".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000003".
               05  FILLER          PIC X(20)   VALUE "ORCHC03V02".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000003".
               05  FILLER          PIC X(20)   VALUE "ORCHC03V03".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000003".
               05  FILLER          PIC X(20)   VALUE "ORCHC03V04".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000003".
               05  FILLER          PIC X(20)   VALUE "ORCHC03V03A5".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000003".
               05  FILLER          PIC X(20)   VALUE "ORCHC03V05".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000003".
               05  FILLER          PIC X(20)   VALUE "A00000H03".
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000003".
               05  FILLER          PIC X(20)   VALUE "A00000H03A4".
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000003".
               05  FILLER          PIC X(20)   VALUE "A00000H03A5".
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000003".
               05  FILLER          PIC X(20)   VALUE "A21000H03".
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000003".
               05  FILLER          PIC X(20)   VALUE "A00000H03A5P".
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000003".
               05  FILLER          PIC X(20)   VALUE "A00000H03A5L".
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000004".
               05  FILLER          PIC X(20)   VALUE "ORCHC30".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE "1".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000004".
               05  FILLER          PIC X(20)   VALUE "ORCHC31".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000005".
               05  FILLER          PIC X(20)   VALUE "ORCHC33".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE "1".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000006".
               05  FILLER          PIC X(20)   VALUE "ORCHCM31".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE "1".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000007".
               05  FILLER          PIC X(20)   VALUE "ORCHCM32".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE "1".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000008".
               05  FILLER          PIC X(20)   VALUE "ORCHC04".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE "1".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000009".
               05  FILLER          PIC X(20)   VALUE "ORCHC62".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE "1".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000009".
               05  FILLER          PIC X(20)   VALUE "A00000RA01".
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000010".
               05  FILLER          PIC X(20)   VALUE "ORCHC67".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE "1".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000011".
               05  FILLER          PIC X(20)   VALUE "ORCHC14".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE "1".
      *
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(08)   VALUE "00000012".
               05  FILLER          PIC X(20)   VALUE "ORCHC15".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE "1".
      *
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(08)   VALUE "00000001".
               05  FILLER          PIC X(20)   VALUE "ORCHCN01".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE "1".
      *
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(08)   VALUE "00000002".
               05  FILLER          PIC X(20)   VALUE "ORCHCN02".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE "1".
      *
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(08)   VALUE "00000003".
               05  FILLER          PIC X(20)   VALUE "ORCHCN03".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE "1".
      *
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(08)   VALUE "00000003".
               05  FILLER          PIC X(20)   VALUE "ORCHCN03V02".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(08)   VALUE "00000003".
               05  FILLER          PIC X(20)   VALUE "ORCHCN03V03".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(08)   VALUE "00000003".
               05  FILLER          PIC X(20)   VALUE "ORCHCN03V03A5".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(08)   VALUE "00000003".
               05  FILLER          PIC X(20)   VALUE "A00000H03NA4".
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(08)   VALUE "00000003".
               05  FILLER          PIC X(20)   VALUE "A00000H03NA5".
               05  FILLER          PIC X(01)   VALUE "2".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(08)   VALUE "00000004".
               05  FILLER          PIC X(20)   VALUE "ORCHCN33".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE "1".
      *
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(08)   VALUE "00000005".
               05  FILLER          PIC X(20)   VALUE "ORCHC501".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE "1".
      *
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(08)   VALUE "00000006".
               05  FILLER          PIC X(20)   VALUE "ORCHC502".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE "1".
      *
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(08)   VALUE "00000007".
               05  FILLER          PIC X(20)   VALUE "ORCHC503".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE "1".
      *
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(08)   VALUE "00000008".
               05  FILLER          PIC X(20)   VALUE "ORCHCN04".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE "1".
      *
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(08)   VALUE "00000009".
               05  FILLER          PIC X(20)   VALUE "ORCHCN30".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE "1".
      *
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(08)   VALUE "00000009".
               05  FILLER          PIC X(20)   VALUE "ORCHCN31".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE " ".
      *
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(08)   VALUE "00000010".
               05  FILLER          PIC X(20)   VALUE "ORCHCN62".
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(01)   VALUE "1".
      *
           03  CONST-INFO-R  REDEFINES   CONST-INFO-VAL.
               05  CONST-INFO-OCC    OCCURS  47.
                   07  CONST-INFO-NYUGAIKBN  PIC X(01).
                   07  CONST-INFO-KBNCD      PIC X(08).
                   07  CONST-INFO-PGID       PIC X(20).
                   07  CONST-INFO-SITEKBN    PIC X(01).
                   07  CONST-INFO-DEFAULT    PIC X(01).
           03  CONST-INFO-MAX    PIC 9(03)   VALUE 47.
           03  CONST-PGLST-MAX     PIC 9(03)   VALUE 200.
           03  CONST-DAILY         PIC X(16)   VALUE "DAILY".
           03  CONST-MONTHLY       PIC X(16)   VALUE "MONTHLY".
           03  CONST-ONLINE        PIC X(16)   VALUE "ONLINE".
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK0043.INC"  REPLACING
                                   //SYS-0043//  BY //SYS-STAT//.
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
           COPY    "CPORCSORCAPG.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           SORCAPG-AREA
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SORCAPG-SITEKBN
                                           SORCAPG-PGLST-MAX
                                           SORCAPG-PGLST
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
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
           IF    ( SORCAPG-NYUGAIKBN   NOT =   "1" )
               MOVE    "2"             TO  SORCAPG-NYUGAIKBN
           END-IF
      *
           MOVE    FUNCTION UPPER-CASE(SORCAPG-TYPE)
                                       TO  WRK-TYPE
           MOVE    WRK-TYPE            TO  SORCAPG-TYPE
      *
           .
      *
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           EVALUATE    SORCAPG-TYPE
           WHEN    CONST-ONLINE
               IF    ( SORCAPG-SET-DEFAULT     =   "1" )
                   PERFORM 2002-ONLINE-SEC
               ELSE
                   PERFORM 2001-ONLINE-SEC
               END-IF
           WHEN    CONST-DAILY
           WHEN    CONST-MONTHLY
               PERFORM 2002-STAT-SEC
           WHEN    OTHER
               MOVE    1               TO  SORCAPG-RC
           END-EVALUATE
      *
           .
      *
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    オンライン帳票　処理
      *****************************************************************
       2001-ONLINE-SEC                     SECTION.
      *
           MOVE    ZERO            TO  IDXT
           MOVE    ZERO            TO  FLG-EXISTS
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-INFO-MAX )
                    OR   ( IDXT    >=  CONST-PGLST-MAX )
      *
               IF    ( SORCAPG-NYUGAIKBN
                                   =   CONST-INFO-NYUGAIKBN (IDX0))
                AND  ( SORCAPG-KBNCD
                                   =   CONST-INFO-KBNCD (IDX0))
                   COMPUTE IDXT =  IDXT    +   1
                   MOVE    IDXT    TO  SORCAPG-PGLST-MAX
                   MOVE    CONST-ONLINE
                                   TO  SORCAPG-TTYPE (IDXT)
                   MOVE    CONST-INFO-PGID (IDX0)
                                   TO  SORCAPG-TPGID (IDXT)
                   MOVE    CONST-INFO-SITEKBN (IDX0)
                                   TO  SORCAPG-TSITEKBN (IDXT)
      *
                   IF    ( FLG-EXISTS      =   ZERO )
                       IF    ( SORCAPG-PGID    =   SPACE )
                           IF    ( CONST-INFO-DEFAULT (IDX0)
                                           =   "1" )
                               MOVE   1    TO  FLG-EXISTS
                               MOVE    CONST-INFO-PGID (IDX0)
                                           TO  SORCAPG-PGID
                               MOVE    CONST-INFO-SITEKBN (IDX0)
                                           TO  SORCAPG-SITEKBN
                           END-IF
                       ELSE
                           IF    ( SORCAPG-PGID
                                           =   CONST-INFO-PGID (IDX0))
                               MOVE    1   TO  FLG-EXISTS
                               MOVE    CONST-INFO-SITEKBN (IDX0)
                                           TO  SORCAPG-SITEKBN
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( FLG-EXISTS  =   ZERO )
               IF    ( SORCAPG-PGID    =   SPACE )
                   MOVE    2           TO  SORCAPG-RC
               ELSE
                   MOVE    3           TO  SORCAPG-RC
               END-IF
           END-IF
      *
           .
      *
       2001-ONLINE-EXT.
           EXIT.
      *****************************************************************
      *    オンライン帳票　処理
      *****************************************************************
       2002-ONLINE-SEC                     SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-INFO-MAX )
      *
               IF    ( SORCAPG-NYUGAIKBN
                                   =   CONST-INFO-NYUGAIKBN (IDX0))
                AND  ( CONST-INFO-DEFAULT (IDX0)
                                   =   "1" )
                   MOVE    FUNCTION NUMVAL(CONST-INFO-KBNCD(IDX0))
                                   TO  IDXT
                   MOVE    CONST-ONLINE
                                   TO  SORCAPG-TTYPE (IDXT)
                   MOVE    CONST-INFO-PGID (IDX0)
                                   TO  SORCAPG-TPGID (IDXT)
                   MOVE    CONST-INFO-SITEKBN (IDX0)
                                   TO  SORCAPG-TSITEKBN (IDXT)
                   IF    ( IDXT    >   SORCAPG-PGLST-MAX )
                       MOVE    IDXT    TO  SORCAPG-PGLST-MAX
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
      *
       2002-ONLINE-EXT.
           EXIT.
      *****************************************************************
      *    統計帳票　処理
      *****************************************************************
       2002-STAT-SEC                     SECTION.
      *
           IF    ( SORCAPG-PGID    =   SPACE )
               PERFORM 20021-STAT-ALL-SEC
           ELSE
               PERFORM 20011-STAT-CHECK-SEC
           END-IF
      *
           .
      *
       2002-STAT-EXT.
           EXIT.
      *****************************************************************
      *    統計帳票返却処理
      *****************************************************************
       20021-STAT-ALL-SEC                  SECTION.
      *
           IF    ( SORCAPG-TYPE        =   "DAILY" )
               MOVE    CONST-DAILY     TO  WRK-TYPE
               PERFORM 200211-STAT-ALL-SEC
           ELSE
               MOVE    CONST-MONTHLY   TO  WRK-TYPE
               PERFORM 200211-STAT-ALL-SEC
           END-IF
      *
           .
      *
       20021-STAT-ALL-EXT.
           EXIT.
      *****************************************************************
      *    統計帳票返却処理
      *****************************************************************
       200211-STAT-ALL-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-EXISTS
      *
           MOVE    SPACE               TO  WRK-KANRICD
           IF    ( WRK-TYPE            =   CONST-DAILY )
               MOVE    "0043"          TO  WRK-KANRICD
           ELSE
               MOVE    "0044"          TO  WRK-KANRICD
           END-IF
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    WRK-KANRICD         TO  SYS-KANRICD
           MOVE    SORCAPG-SYSYMD      TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
      *
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-STAT-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( FLG-EXISTS      NOT =   ZERO )
                    OR   ( IDXT        >=  CONST-PGLST-MAX )
      *
               COMPUTE IDXT    =   IDXT +  1
      *
               MOVE    IDXT            TO  SORCAPG-PGLST-MAX
               MOVE    WRK-TYPE        TO  SORCAPG-TTYPE (IDXT)
               MOVE    SYS-STAT-PRGNAME
                                       TO  SORCAPG-TPGID (IDXT)
               MOVE    SPACE           TO  WRK-SITEKBN
               IF    ( SYS-STAT-KBNCD (1:1)    =   "1" )
                   MOVE    "1"         TO  WRK-SITEKBN
               ELSE
                   MOVE    "2"         TO  WRK-SITEKBN
               END-IF
               MOVE    WRK-SITEKBN     TO  SORCAPG-TSITEKBN (IDXT)
      *
               IF    ( SORCAPG-PGID    NOT =   SPACE )
                AND  ( SORCAPG-PGID        =   SYS-STAT-PRGNAME )
                   MOVE    1           TO  FLG-EXISTS
                   MOVE    WRK-SITEKBN TO  SORCAPG-SITEKBN
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC        TO  SYS-STAT-REC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       200211-STAT-ALL-EXT.
           EXIT.
      *****************************************************************
      *    統計帳票返却処理
      *****************************************************************
       20011-STAT-CHECK-SEC            SECTION.
      *
           IF    ( SORCAPG-TYPE        =   "DAILY" )
               MOVE    CONST-DAILY     TO  WRK-TYPE
               PERFORM 200211-STAT-ALL-SEC
           ELSE
               MOVE    CONST-MONTHLY   TO  WRK-TYPE
               PERFORM 200211-STAT-ALL-SEC
           END-IF
      *
           INITIALIZE                      SORCAPG-PGLST-MAX
                                           SORCAPG-PGLST
      *
           IF    ( FLG-EXISTS      =   ZERO )
               MOVE    4           TO  SORCAPG-RC
           END-IF
      *
           .
      *
       20011-STAT-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    終了　処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      D    DISPLAY "SORCAPG-PGID:" SORCAPG-PGID ":" SORCAPG-SITEKBN
      D            "[" SORCAPG-RC "]"
      *
           .
      *
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
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
           MOVE    "DBFETCH"       TO  MCP-FUNC
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
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
