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
       PROGRAM-ID.             ORCHCN68S01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院会計照会
      *  コンポーネント名  : 医療区分・ＡＤＬ評価票編集サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/09/17    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-NYUINACT        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-RYOCOM          PIC 9(01).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX-DAY             PIC 9(05).
           03  IDX-LST             PIC 9(05).
           03  IDX-SRY             PIC 9(05).
           03  IDX-MED             PIC 9(05).
           03  IDXK                PIC 9(05).
           03  IDXM                PIC 9(05).
           03  IDXR                PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RYOYO-DAY-G.
               05  WRK-RYOYO-DAY           PIC S9(02)
                                           OCCURS  31.
           03  WRK-RYOYO-G.
               05  WRK-RYOYO-CD            PIC X(09)
                                           OCCURS  31.
           03  WRK-RYOYO-KHN-G.
               05  WRK-RYOYO-KHN           PIC 9(02)
                                           OCCURS  31.
           03  WRK-TNS-SRYCD               PIC X(09).
           03  WRK-TNS-KJNYMD              PIC X(08).
           03  WRK-SRYYM                   PIC X(06).
           03  WRK-ZAINUM-G.
               05  WRK-ZAINUM              PIC 9(08).
           03  WRK-NUM-ZEN                 PIC X(02).
           03  WRK-NUM-HAN                 PIC X(01).
      *
       01  MEDINF-AREA.
           03  MEDINF-MAX                  PIC 9(05).
           03  MEDINF-OCC                  OCCURS       100.
               09  MEDINF-MEDKBN           PIC X(01).
               09  MEDINF-LIMIT            PIC 9(03).
               09  MEDINF-ALLKBN           PIC 9(01).
               09  MEDINF-ZAINUM           PIC 9(04).
               09  MEDINF-SRYCD            PIC X(09).
               09  MEDINF-ID               PIC X(02).
               09  MEDINF-NAME             PIC X(100).
      *
       01  CONST-AREA.
           03  CONST-H300401           PIC X(08)   VALUE   "20180401".
           03  CONST-H240401           PIC X(08)   VALUE   "20120401".
           03  CONST-H220401           PIC X(08)   VALUE   "20100401".
           03  CONST-H200401           PIC X(08)   VALUE   "20080401".
           03  CONST-NSRYACT-MAX       PIC 9(05)   VALUE 5.
           03  CONST-RYOLIST-MAX       PIC 9(04)   VALUE   100.
      *
           03  CONST-ADLKBN-G.
               05  CONST-ADLTEN-MAX        PIC 9(03).
               05  CONST-ADLKBN-OCC    OCCURS  4.
                   07  CONST-ADLKBN-ALLKBN PIC 9(01).
                   07  CONST-ADLKBN-ZAINUM PIC 9(04).
                   07  CONST-ADLKBN-ID     PIC X(02).
                   07  CONST-ADLKBN-NAME   PIC X(100).
               05  CONST-ADLKBN-MAX        PIC 9(03).
      *
           COPY    "CPRYOYOCD.INC".
           COPY    "CPMEDKBN.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK5014.INC".
      *
       01  RYOCOM-REC.
           COPY    "CPRYOCOM.INC".
      *
           COPY    "CPTENSU.INC".
      *
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "CPORCSS011.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       COPY    "COMMON-SPA".
       COPY    "CPORCHCN68S01.INC".
      *
      ******************************************************************
       PROCEDURE               DIVISION
                                   USING
                                   SPA-AREA
                                   HCN68S01-AREA
                                   .
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
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  MEDINF-AREA
      *
           IF    ( HCN68S01-RANGE      =   SPACE )
               MOVE    "0"             TO  HCN68S01-RANGE
           END-IF
      *
           MOVE    6                   TO  CONST-ADLTEN-MAX
           MOVE    4                   TO  CONST-ADLKBN-MAX
      *
           MOVE    0                   TO  CONST-ADLKBN-ALLKBN (1)
           MOVE    1001                TO  CONST-ADLKBN-ZAINUM (1)
           MOVE    "a"                 TO  CONST-ADLKBN-ID     (1)
           MOVE    "ａ　ベッドの可動性"
                                       TO  CONST-ADLKBN-NAME   (1)
      *
           MOVE    0                   TO  CONST-ADLKBN-ALLKBN (2)
           MOVE    1002                TO  CONST-ADLKBN-ZAINUM (2)
           MOVE    "b"                 TO  CONST-ADLKBN-ID     (2)
           MOVE    "ｂ　移乗"          TO  CONST-ADLKBN-NAME   (2)
      *
           MOVE    0                   TO  CONST-ADLKBN-ALLKBN (3)
           MOVE    1003                TO  CONST-ADLKBN-ZAINUM (3)
           MOVE    "c"                 TO  CONST-ADLKBN-ID     (3)
           MOVE    "ｃ　食事"          TO  CONST-ADLKBN-NAME   (3)
      *
           MOVE    0                   TO  CONST-ADLKBN-ALLKBN (4)
           MOVE    1004                TO  CONST-ADLKBN-ZAINUM (4)
           MOVE    "d"                 TO  CONST-ADLKBN-ID     (4)
           MOVE    "ｄ　トイレの使用"  TO  CONST-ADLKBN-NAME   (4)
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
           PERFORM 210-EDIT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    編集処理
      *****************************************************************
       210-EDIT-SEC                SECTION.
      *
           PERFORM 2101-EDIT-NYUIN-DAY-SEC
      *
           PERFORM 2101-EDIT-RYOYO-DAY-SEC
      *
           PERFORM 2101-EDIT-RYOLST-SEC
      *
           IF    ( HCN68S01-RYOYO-FLG  =   1 )
               PERFORM 2101-EDIT-PTSTS-SEC
           END-IF
      *
           .
       210-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院期間編集処理
      *****************************************************************
       2101-EDIT-NYUIN-DAY-SEC         SECTION.
      *
           PERFORM 900-NYUINACCT-KEY19-SEL-SEC
           IF    ( FLG-NYUINACCT   =   ZERO )
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
      *
                   MOVE    NACCT-DAY (IDX-DAY)
                                   TO  HCN68S01-COMB-DAY  (IDX-DAY)
      *
                   IF    ( NACCT-DAY (IDX-DAY) >   ZERO )
                       MOVE    1   TO  HCN68S01-NYUIN-DAY (IDX-DAY)
                       COMPUTE HCN68S01-NYUIN-NISSU
                           =   HCN68S01-NYUIN-NISSU    +   1
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       2101-EDIT-NYUIN-DAY-EXT.
           EXIT.
      *****************************************************************
      *    療養型入院基本料算定期間編集処理
      *****************************************************************
       2101-EDIT-RYOYO-DAY-SEC         SECTION.
      *
           PERFORM 900-NYUINACCT-KEY35-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               INITIALIZE      WRK-RYOYO-DAY-G
      *
               PERFORM 21011-EDIT-RYOYO-DAY-SEC
      *
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
      *
                   IF    ( WRK-RYOYO-DAY (IDX-DAY) NOT =   ZERO )
                       MOVE    WRK-RYOYO-DAY (IDX-DAY)
                                       TO  HCN68S01-RYOYO-DAY (IDX-DAY)
                   END-IF
      *
                   IF    ( WRK-RYOYO-CD  (IDX-DAY) NOT =   SPACE )
                       MOVE    WRK-RYOYO-CD  (IDX-DAY)
                                       TO  HCN68S01-RYOYO-CD  (IDX-DAY)
                       MOVE    WRK-RYOYO-KHN (IDX-DAY)
                                       TO  HCN68S01-RYOYO-KHN (IDX-DAY)
                       MOVE    1       TO  HCN68S01-RYOYO-FLG
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-NYUINACCT-KEY35-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key35"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2101-EDIT-RYOYO-DAY-EXT.
           EXIT.
      *****************************************************************
      *    療養型入院基本料算定期間編集処理
      *****************************************************************
       21011-EDIT-RYOYO-DAY-SEC         SECTION.
      *
           PERFORM 900-NYUINACT-KEY9-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACT    NOT =   ZERO )
                    OR   ( WRK-RYOYO-DAY-G NOT =   ZERO )
      *
               PERFORM VARYING IDX-SRY FROM    1   BY  1
                       UNTIL ( IDX-SRY >   CONST-NSRYACT-MAX )
                        OR   ( WRK-RYOYO-DAY-G NOT =   ZERO )
      *
                   PERFORM 210111-RYOYOCD-CHK-SEC
      *
               END-PERFORM
      *
               PERFORM 900-NYUINACT-KEY9-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       21011-EDIT-RYOYO-DAY-EXT.
           EXIT.
      *****************************************************************
      *    療養型入院基本料診療行為コードチェック処理
      *****************************************************************
       210111-RYOYOCD-CHK-SEC          SECTION.
      *
           PERFORM VARYING IDXK    FROM    1   BY  1
                   UNTIL ( IDXK    >   CONST-RYOYOCD-KHN-MAX )
                    OR   ( WRK-RYOYO-DAY-G NOT =   ZERO )
      *
               PERFORM VARYING IDXR    FROM    1   BY  1
                   UNTIL ( IDXR    >   CONST-RYOYOCD-CD-MAX  )
                    OR   ( WRK-RYOYO-DAY-G NOT =   ZERO )
      *
                   IF    ( NSRY-SRYCD (IDX-SRY)    =   CONST-RYOYOCD
                                                            (IDXK IDXR))
      *
                       PERFORM VARYING IDX-DAY FROM    1   BY  1
                               UNTIL ( IDX-DAY >   31 )
      *
                           IF    ( NACCT-DAY (IDX-DAY) >   ZERO )
                               MOVE    IDXR
                                           TO  WRK-RYOYO-DAY (IDX-DAY)
                               MOVE    CONST-RYOYOCD  (IDXK IDXR)
                                           TO  WRK-RYOYO-CD  (IDX-DAY)
                               MOVE    IDXK
                                           TO  WRK-RYOYO-KHN(IDX-DAY)
                           END-IF
      *
                       END-PERFORM
      *
                   END-IF
      *
               END-PERFORM
      *
           END-PERFORM
      *
           IF    ( WRK-RYOYO-DAY-G         =   ZERO )
               MOVE    NSRY-SRYCD (IDX-SRY)    TO  WRK-TNS-SRYCD
               MOVE    HCN68S01-SRYYMD        TO  WRK-TNS-KJNYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( TNS-DATAKBN     =       1 )
                   PERFORM VARYING IDX-DAY FROM    1   BY  1
                           UNTIL ( IDX-DAY >   31 )
      *
                       IF    ( NACCT-DAY (IDX-DAY) >   ZERO )
                           MOVE    -1      TO  WRK-RYOYO-DAY (IDX-DAY)
                       END-IF
      *
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       210111-RYOYOCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ＡＤＬ区分・医療区分リスト編集処理
      *****************************************************************
       2101-EDIT-RYOLST-SEC            SECTION.
      *
           INITIALIZE                  HCN68S01-RYOLIST-AREA
      *
           PERFORM 21011-EDIT-MEDINF-SEC
      *
           MOVE    ZERO            TO  IDX-LST
      *
           PERFORM VARYING IDX0        FROM    1   BY  1
                   UNTIL ( IDX0    >   MEDINF-MAX )
                    OR   ( IDX-LST >=  CONST-RYOLIST-MAX )
      *
               MOVE    HCN68S01-SRYYM  TO  WRK-SRYYM
               MOVE    MEDINF-ZAINUM (IDX0)
                                       TO  WRK-ZAINUM
               PERFORM 900-RYOCOM-KEY-SEL-SEC
               IF    ( FLG-RYOCOM  =   ZERO )
                   COMPUTE IDX-LST =   IDX-LST +   1
      *
                   PERFORM 21011-EDIT-RYOLST-MEDKBN-SEC
      *
                   MOVE    RYOCOM-MONTH
                                   TO  HCN68S01-TDAYALL (IDX-LST)
      *
                   PERFORM VARYING IDX-DAY FROM    1   BY  1
                           UNTIL ( IDX-DAY >   31 )
      *
                       EVALUATE    TRUE
                       WHEN  ( HCN68S01-RANGE      =   "1" )
                         AND ( HCN68S01-RYOYO-CD (IDX-DAY)
                                               NOT =   SPACE )
                       WHEN  ( HCN68S01-RANGE      =   "0" )
                         AND ( HCN68S01-NYUIN-DAY (IDX-DAY)
                                                   >   ZERO )
                           MOVE    RYOCOM-DAY (IDX-DAY)
                                   TO  HCN68S01-TDAY (IDX-LST IDX-DAY)
                       END-EVALUATE
      *
                   END-PERFORM
      *
               ELSE
                    COMPUTE IDX-LST =   IDX-LST +   1
                    PERFORM 21011-EDIT-RYOLST-MEDKBN-SEC
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX0        FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-ADLKBN-MAX )
                    OR   ( IDX-LST >=  CONST-RYOLIST-MAX )
      *
               COMPUTE IDX-LST =   IDX-LST +   1
      *
               IF    ( IDX0    =   1 )
                   MOVE    IDX-LST TO  HCN68S01-RYOLIST-ADL-STIDX
               END-IF
      *
               MOVE    "1"         TO  HCN68S01-TZAISKBKBN
                                                   (IDX-LST)
      *
               MOVE    CONST-ADLKBN-ALLKBN (IDX0)
                                   TO  HCN68S01-TALLKBN (IDX-LST)
               MOVE    CONST-ADLKBN-ZAINUM (IDX0)
                                   TO  HCN68S01-TZAINUM (IDX-LST)
               MOVE    CONST-ADLKBN-ID     (IDX0)
                                   TO  HCN68S01-TID     (IDX-LST)
               MOVE    CONST-ADLKBN-NAME   (IDX0)
                                   TO  HCN68S01-TNAME   (IDX-LST)
      *
               MOVE    HCN68S01-SRYYM  TO  WRK-SRYYM
               MOVE    HCN68S01-TZAINUM (IDX-LST)
                                   TO  WRK-ZAINUM
               PERFORM 900-RYOCOM-KEY-SEL-SEC
               IF    ( FLG-RYOCOM  =   ZERO )
                   IF    ( RYOCOM-MONTH    >   ZERO )
                    AND  ( RYOCOM-DAY-TBL2 =   ZERO )
                       PERFORM VARYING IDX-DAY FROM    1   BY  1
                               UNTIL ( IDX-DAY >   31 )
      *
                           EVALUATE    TRUE
                           WHEN  ( HCN68S01-RANGE      =   "1" )
                             AND ( HCN68S01-RYOYO-CD (IDX-DAY)
                                               NOT =   SPACE )
                           WHEN  ( HCN68S01-RANGE      =   "0" )
                             AND ( HCN68S01-NYUIN-DAY (IDX-DAY)
                                                   >   ZERO )
                               MOVE    RYOCOM-MONTH
                                   TO  HCN68S01-TDAY (IDX-LST IDX-DAY)
                           END-EVALUATE
      *
                       END-PERFORM
                   ELSE
                       PERFORM VARYING IDX-DAY FROM    1   BY  1
                               UNTIL ( IDX-DAY >   31 )
      *
                           EVALUATE    TRUE
                           WHEN  ( HCN68S01-RANGE      =   "1" )
                             AND ( HCN68S01-RYOYO-CD (IDX-DAY)
                                               NOT =   SPACE )
                           WHEN  ( HCN68S01-RANGE      =   "0" )
                             AND ( HCN68S01-NYUIN-DAY (IDX-DAY)
                                                   >   ZERO )
                               MOVE    RYOCOM-DAY (IDX-DAY)
                                   TO  HCN68S01-TDAY (IDX-LST IDX-DAY)
                           END-EVALUATE
      *
                       END-PERFORM
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( HCN68S01-SRYYMD >=  CONST-H300401 )
               PERFORM 21011-EDIT-RYOLST-DESIGNR-SEC
           END-IF
      *
           MOVE    IDX-LST         TO  HCN68S01-RYOLIST-MAX
      *
           .
       2101-EDIT-RYOLST-EXT.
           EXIT.
      *****************************************************************
      *    ＡＤＬ区分・医療区分リスト編集処理 （医療区分編集）
      *****************************************************************
       21011-EDIT-RYOLST-MEDKBN-SEC    SECTION.
      *
           MOVE    "2"             TO  HCN68S01-TZAISKBKBN  (IDX-LST)
           MOVE    MEDINF-MEDKBN (IDX0)
                                   TO  HCN68S01-TMEDKBN (IDX-LST)
           MOVE    MEDINF-LIMIT  (IDX0)
                                   TO  HCN68S01-TLIMIT  (IDX-LST)
           MOVE    MEDINF-ALLKBN (IDX0)
                                   TO  HCN68S01-TALLKBN (IDX-LST)
           MOVE    MEDINF-ZAINUM (IDX0)
                                   TO  HCN68S01-TZAINUM (IDX-LST)
           MOVE    MEDINF-SRYCD  (IDX0)
                                   TO  HCN68S01-TSRYCD  (IDX-LST)
           MOVE    MEDINF-SRYCD  (IDX0)
                                   TO  HCN68S01-TSRYCD  (IDX-LST)
           MOVE    MEDINF-NAME   (IDX0)
                                   TO  HCN68S01-TNAME   (IDX-LST)
           MOVE    MEDINF-ID     (IDX0)
                                   TO  HCN68S01-TID     (IDX-LST)
      *
           .
       21011-EDIT-RYOLST-MEDKBN-EXT.
           EXIT.
      *****************************************************************
      *    医療区分情報編集処理
      *****************************************************************
       21011-EDIT-MEDINF-SEC           SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( HCN68S01-SRYYMD  >=  CONST-H200401 )
               MOVE    ZERO            TO  MEDINF-MAX
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   MEDH20-MAX )
      *
                   IF    ( MEDH20-STYMD  (IDX0) <= HCN68S01-SRYYMD )
                     AND ( MEDH20-EDYMD  (IDX0) >= HCN68S01-SRYYMD )
      *
                       COMPUTE MEDINF-MAX  =   MEDINF-MAX  +   1
                       MOVE    MEDINF-MAX  TO  IDXM
      *
                       MOVE  MEDH20-MEDKBN (IDX0)
                                           TO  MEDINF-MEDKBN (IDXM)
                       MOVE  MEDH20-LIMIT (IDX0)
                                           TO  MEDINF-LIMIT  (IDXM)
                       MOVE  MEDH20-ALLKBN(IDX0)
                                           TO  MEDINF-ALLKBN (IDXM)
                       MOVE  MEDH20-ZAINUM(IDX0)
                                           TO  MEDINF-ZAINUM (IDXM)
                       MOVE  MEDH20-SRYCDL (IDX0)
                                           TO  MEDINF-SRYCD  (IDXM)
                       MOVE  MEDH20-SRYCDL (IDX0)
                                             TO WRK-TNS-SRYCD
                       MOVE  HCN68S01-SRYYMD TO WRK-TNS-KJNYMD
                       PERFORM 900-TENSU-KEY-SEL-SEC
      * 暫定対応
      *                MOVE    TNS-NAME    TO MEDINF-NAME (IDXM)
      *                IF    ( MEDINF-NAME (IDXM)  =   SPACE )
                           MOVE    MEDH20-MEIL (IDX0)
                                           TO MEDINF-NAME (IDXM)
      *                END-IF
                       MOVE    MEDH20-ID   (IDX0)
                                           TO MEDINF-ID (IDXM)
                   END-IF
               END-PERFORM
           END-EVALUATE
      *
           .
       21011-EDIT-MEDINF-EXT.
           EXIT.
      *****************************************************************
      *    ＡＤＬ区分・医療区分リスト編集処理 （ＤＥＳＩＧＮＲ編集）
      *****************************************************************
       21011-EDIT-RYOLST-DESIGNR-SEC    SECTION.
      *
           COMPUTE IDX-LST =   IDX-LST +   1
      *
           MOVE    IDX-LST TO  HCN68S01-RYOLIST-DESIGNR-STIDX
      *
           MOVE    "A"             TO  HCN68S01-TZAISKBKBN  (IDX-LST)
      *
           MOVE    DESIGNR-ALLKBN  TO  HCN68S01-TALLKBN (IDX-LST)
           MOVE    DESIGNR-ZAINUM  TO  HCN68S01-TZAINUM (IDX-LST)
           MOVE    DESIGNR-SRYCD   TO  HCN68S01-TSRYCD  (IDX-LST)
           MOVE    DESIGNR-ID      TO  HCN68S01-TID     (IDX-LST)
           MOVE    DESIGNR-MEI     TO  HCN68S01-TNAME   (IDX-LST)
      *
           MOVE    HCN68S01-SRYYM  TO  WRK-SRYYM
           MOVE    DESIGNR-ZAINUM  TO  WRK-ZAINUM
           PERFORM 900-RYOCOM-KEY-SEL-SEC
           IF    ( FLG-RYOCOM  =   ZERO )
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
                   IF    ( HCN68S01-RYOYO-CD (IDX-DAY)
                                               NOT =   SPACE )
                       MOVE    RYOCOM-DAY (IDX-DAY)
                                   TO  HCN68S01-TDAY (IDX-LST IDX-DAY)
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    HCN68S01-SRYYM  TO  WRK-SRYYM
           MOVE    DESIGNR-ZAINUM-EVAL
                                   TO  WRK-ZAINUM
           PERFORM 900-RYOCOM-KEY-SEL-SEC
           IF    ( FLG-RYOCOM  =   ZERO )
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
                   IF    ( HCN68S01-RYOYO-CD (IDX-DAY)
                                               NOT =   SPACE )
                       MOVE    RYOCOM-DAY (IDX-DAY)
                               TO  HCN68S01-DESIGNR-EVAL-DAY (IDX-DAY)
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       21011-EDIT-RYOLST-DESIGNR-EXT.
           EXIT.
      *****************************************************************
      *    患者状態編集処理
      *****************************************************************
       2101-EDIT-PTSTS-SEC            SECTION.
      *
           PERFORM 21011-EDIT-MEDADL-SEC
      *
           INITIALIZE                  SS011-AREA
           MOVE    HCN68S01-PTID   TO  SS011-PTID
           MOVE    HCN68S01-SRYYMD TO  SS011-SRYYM
           MOVE    1               TO  SS011-DAYFLG
           MOVE    1               TO  SS011-MEDADLFLG
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
               IF    ( HCN68S01-RYOYO-DAY (IDX-DAY) >  ZERO )
                   MOVE    HCN68S01-RYOYO-DAY (IDX-DAY)
                                   TO  SS011-DAY (IDX-DAY)
                   MOVE    HCN68S01-RYOYO-KHN (IDX-DAY)
                                   TO  SS011-KHN (IDX-DAY)
                   EVALUATE    TRUE
                   WHEN  (HCN68S01-MEDKBN-DAYALL (1)        =  1 )
                   WHEN  (HCN68S01-MEDKBN-DAY (1 IDX-DAY)   =  1 )
                       MOVE    3   TO  SS011-MEDKBN (IDX-DAY)
                   WHEN  (HCN68S01-MEDKBN-DAYALL (2)        =  1 )
                   WHEN  (HCN68S01-MEDKBN-DAY (2 IDX-DAY)   =  1 )
                       MOVE    2   TO  SS011-MEDKBN (IDX-DAY)
                   WHEN    OTHER
                       MOVE    1   TO  SS011-MEDKBN (IDX-DAY)
                   END-EVALUATE
               END-IF
      *
               COMPUTE SS011-ADLTEN (IDX-DAY)
                    =  HCN68S01-ADLTEN-DAY (IDX-DAY)
           END-PERFORM
      *
           CALL    "ORCSS011"  USING   SS011-AREA
                                       SPA-AREA
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
      *
               IF    ( HCN68S01-RYOYO-CD (IDX-DAY)
                                       NOT =   SPACE )
                   MOVE    SS011-DAY  (IDX-DAY)
                                TO  HCN68S01-PTSTS-DAY (IDX-DAY)
                   MOVE    SS011-DAY2  (IDX-DAY)
                                TO  HCN68S01-PTSTS-DAY2(IDX-DAY)
                   MOVE    SS011-DAY3  (IDX-DAY)
                                TO  HCN68S01-PTSTS-DAY3(IDX-DAY)
                   MOVE    SS011-SRYCD (IDX-DAY)
                                TO  HCN68S01-PTSTS-SRYCD (IDX-DAY)
               END-IF
      *
           END-PERFORM
      *
           .
       2101-EDIT-PTSTS-EXT.
           EXIT.
      *****************************************************************
      *    医療区分、ＡＤＬ点数編集処理
      *****************************************************************
       21011-EDIT-MEDADL-SEC               SECTION.
      *
           INITIALIZE                  HCN68S01-MEDKBN-DAY-AREA
                                       HCN68S01-ADLTEN-DAY-AREA
      *
           PERFORM VARYING IDX-LST     FROM    1   BY  1
                   UNTIL ( IDX-LST     >   HCN68S01-RYOLIST-MAX )
      *
               IF    ( HCN68S01-TZAISKBKBN (IDX-LST)    =   "1" )
                   IF    ( HCN68S01-TDAYALL (IDX-LST)   >   ZERO )
                    AND  ( HCN68S01-TDAY-G  (IDX-LST)   =   ZERO )
                       PERFORM VARYING IDX-DAY FROM    1   BY  1
                               UNTIL ( IDX-DAY >   31 )
                           IF    ( HCN68S01-RYOYO-CD (IDX-DAY)
                                       NOT =   SPACE )
                               COMPUTE HCN68S01-ADLTEN-DAY (IDX-DAY)
                                   =   HCN68S01-ADLTEN-DAY (IDX-DAY)
                                   +   HCN68S01-TDAYALL (IDX-LST)
                           END-IF
                       END-PERFORM
                   ELSE
                       PERFORM VARYING IDX-DAY FROM    1   BY  1
                               UNTIL ( IDX-DAY >   31 )
                           IF    ( HCN68S01-RYOYO-CD (IDX-DAY)
                                       NOT =   SPACE )
                               COMPUTE HCN68S01-ADLTEN-DAY (IDX-DAY)
                                   =   HCN68S01-ADLTEN-DAY (IDX-DAY)
                                   +   HCN68S01-TDAY  (IDX-LST IDX-DAY)
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
      *
               MOVE    ZERO            TO  IDX-MED
               IF    ( HCN68S01-TZAISKBKBN (IDX-LST)   =   "2" )
                   IF    ( HCN68S01-TMEDKBN(IDX-LST)   =   3 )
                       MOVE    1       TO  IDX-MED
                   END-IF
                   IF    ( HCN68S01-TMEDKBN(IDX-LST)   =   2 )
                       MOVE    2       TO  IDX-MED
                   END-IF
               END-IF
      *
               IF    ( IDX-MED >       ZERO )
      *
                   IF    ( HCN68S01-TDAYALL(IDX-LST)    >   ZERO )
                    AND  ( HCN68S01-TDAY-G (IDX-LST)    =   ZERO )
                       MOVE    HCN68S01-TDAYALL(IDX-LST)
                                   TO HCN68S01-MEDKBN-DAYALL (IDX-MED)
                   END-IF
      *
                   PERFORM VARYING IDX-DAY FROM    1   BY  1
                           UNTIL ( IDX-DAY >   31 )
                       IF    ( HCN68S01-TDAY   (IDX-LST IDX-DAY)
                                           >   ZERO )
                        AND  ( HCN68S01-RYOYO-CD (IDX-DAY)
                                       NOT =   SPACE )
                           MOVE    HCN68S01-TDAY   (IDX-LST IDX-DAY)
                               TO  HCN68S01-MEDKBN-DAY (IDX-MED IDX-DAY)
                       END-IF
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           .
       21011-EDIT-MEDADL-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *
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
      *    数字半角処理
      *****************************************************************
       800-NUM-HANKAKU-SEC         SECTION.
      *
           MOVE    ZERO            TO  WRK-NUM-HAN
           EVALUATE    WRK-NUM-ZEN
           WHEN    "０"
               MOVE    "0"         TO  WRK-NUM-HAN
           WHEN    "１"
               MOVE    "1"         TO  WRK-NUM-HAN
           WHEN    "２"
               MOVE    "2"         TO  WRK-NUM-HAN
           WHEN    "３"
               MOVE    "3"         TO  WRK-NUM-HAN
           WHEN    "４"
               MOVE    "4"         TO  WRK-NUM-HAN
           WHEN    "５"
               MOVE    "5"         TO  WRK-NUM-HAN
           WHEN    "６"
               MOVE    "6"         TO  WRK-NUM-HAN
           WHEN    "７"
               MOVE    "7"         TO  WRK-NUM-HAN
           WHEN    "８"
               MOVE    "8"         TO  WRK-NUM-HAN
           WHEN    "９"
               MOVE    "9"         TO  WRK-NUM-HAN
           END-EVALUATE
      *
           .
      *
       800-NUM-HANKAKU-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
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
      *
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY19-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE  NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    HCN68S01-PTID       TO  NACCT-PTID
           MOVE    HCN68S01-SRYYM      TO  NACCT-SRYYM
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  NYUINACCT-REC
           ELSE
               INITIALIZE              NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUINACCT-KEY19-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY35-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE                  NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    HCN68S01-PTID       TO  NACCT-PTID
           MOVE    HCN68S01-SRYYM      TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key35"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  NYUINACCT-REC
           ELSE
               INITIALIZE              NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
      *
           .
       900-NYUINACCT-KEY35-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計FETCH処理
      *****************************************************************
       900-NYUINACCT-KEY35-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key35"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  NYUINACCT-REC
           ELSE
               INITIALIZE              NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
      *
           .
       900-NYUINACCT-KEY35-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為検索処理
      *****************************************************************
       900-NYUINACT-KEY9-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACT
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           .
       900-NYUINACT-KEY9-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為FETCH処理
      *****************************************************************
       900-NYUINACT-KEY9-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACT
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           .
       900-NYUINACT-KEY9-FET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理(KEY)
      *****************************************************************
       900-TENSU-KEY-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
      *
           MOVE    WRK-TNS-SRYCD   TO  TNS-SRYCD
           MOVE    WRK-TNS-KJNYMD  TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               INITIALIZE              TNS-TENSU-REC
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
      *    点数マスタクローズ
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    療養レセプトコメントマスタ検索処理
      *****************************************************************
       900-RYOCOM-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-RYOCOM
      *
           INITIALIZE                  RYOCOM-REC
           MOVE    SPA-HOSPNUM     TO  RYOCOM-HOSPNUM
           MOVE    "1"             TO  RYOCOM-NYUGAIKBN
           MOVE    HCN68S01-PTID   TO  RYOCOM-PTID
           MOVE    WRK-SRYYM       TO  RYOCOM-SRYYM
           MOVE    WRK-ZAINUM      TO  RYOCOM-ZAINUM
           MOVE    RYOCOM-REC      TO  MCPDATA-REC
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  RYOCOM-REC
           ELSE
               MOVE    1           TO  FLG-RYOCOM
               INITIALIZE              RYOCOM-REC
           END-IF
      *
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-RYOCOM-KEY-SEL-EXT.
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
      *    ＤＢ検索処理（FHETCHを行わない)
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
