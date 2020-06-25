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
       PROGRAM-ID.             ORCSS005.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 療養レセコメントデータ作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/08/21    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *
       WORKING-STORAGE                 SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-RYOCOM                      PIC 9(01).
           03  FLG-PTNYUINRRK                  PIC 9(01).
           03  FLG-NYUINACCT                   PIC 9(01).
           03  FLG-NYUINACT                    PIC 9(01).
           03  FLG-TENSU                       PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-END                         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                            PIC 9(05).
           03  IDXK                            PIC 9(05).
           03  IDXR                            PIC 9(05).
           03  IDX-SRY                         PIC 9(05).
           03  IDX-DAY                         PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ZENGETU                     PIC X(06).
           03  WRK-SRYYM                       PIC X(08).
           03  WRK-SRYYMD                      PIC X(08).
           03  WRK-ZENGETUMATU                 PIC X(08).
           03  WRK-YMD                         PIC X(08).
           03  WRK-ZOGENPTN                    PIC X(01).
           03  WRK-ZOGEN                       PIC S9(05).
           03  WRK-DAY-X.
               05  WRK-DAY                     PIC 9(02).
           03  WRK-RYOYO-DAY-G.
               05  WRK-RYOYO-DAY               PIC S9(02)
                                               OCCURS  31.
           03  WRK-TNS-SRYCD                   PIC X(09).
           03  WRK-TNS-KJNYMD                  PIC X(08).
           03  WRK-ZAINUM-G.
               05  WRK-ZAINUM                  PIC 9(08).
           03  WRK-ZAISKBKBN                   PIC X(01).
      *
       01  CALENDAR-AREA.
           03  CL-RYOYO-FLG                    PIC 9(01).
           03  CL-DAY-G.
               05  CL-DAY                      PIC 9(01)
                                               OCCURS  31.
           03  CL-ZENGETU-END                  PIC 9(02).
           03  CL-ZENGETU-RYOYO-FLG            PIC 9(01).
           03  CL-ZENGETU-DAY-G.
               05  CL-ZENGETU-DAY              PIC 9(01)
                                               OCCURS  31.
           03  CL-KONGETU-END                  PIC 9(02).
           03  CL-KONGETU-RYOYO-FLG            PIC 9(01).
           03  CL-KONGETU-DAY-G.
               05  CL-KONGETU-DAY              PIC 9(01)
                                               OCCURS  31.
      *
       01  CONST-AREA.
           03  CONST-H220401           PIC X(08)   VALUE "20100401".
           03  CONST-NSRYACT-MAX       PIC 9(05)   VALUE 5.
           03  CONST-ADL-TTLZAINUM     PIC 9(04)   VALUE 3000.
      *
           COPY    "CPRYOYOCD.INC".
      *
       01  WKRYOCOM-REC.
           COPY    "CPRYOCOM.INC"      REPLACING
                                       //RYOCOM-// BY //WKRYOCOM-//.
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  RYOCOM-REC.
           COPY    "CPRYOCOM.INC".
      *
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
           COPY    "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK5014.INC".
      *
           COPY    "CPSK5015.INC".
      *
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                        SECTION.
      *
           COPY    "CPORCSS005.INC".
      *
           COPY    "COMMON-SPA".
      *
       PROCEDURE                       DIVISION    USING
           SS005-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SS005-OT
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    終了処理
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  CALENDAR-AREA
      *
           MOVE    ZERO            TO  FLG-END
      *
           MOVE    SS005-SRYYM     TO  WRK-SRYYM
      *
           MOVE    SS005-SRYYM     TO  WRK-SRYYMD
           MOVE    "01"            TO  WRK-SRYYMD (7:2)
      *
           MOVE    SS005-SRYYM     TO  WRK-YMD
           MOVE    "01"            TO  WRK-YMD (7:2)
           MOVE    "1"             TO  WRK-ZOGENPTN
           COMPUTE WRK-ZOGEN   =   -1
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-YMD (1:6)   TO  WRK-ZENGETU
      *
           MOVE    WRK-YMD (7:2)   TO  WRK-DAY-X
           MOVE    WRK-DAY         TO  CL-ZENGETU-END
      *
           MOVE    WRK-YMD         TO  WRK-ZENGETUMATU
      *
           MOVE    SS005-SRYYM     TO  WRK-SRYYM
           PERFORM 900-RYOCOM-KEY2-SEL-SEC
           IF    ( FLG-RYOCOM      =   ZERO )
               MOVE    1           TO  FLG-END
           END-IF
      *
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    "5"             TO  WRK-ZAISKBKBN
           PERFORM 900-NYUINACCT-KEY17-SEL-SEC
           IF    ( FLG-NYUINACCT   NOT =   ZERO )
               MOVE    1           TO  FLG-END
           END-IF
      *
           PERFORM 900-PTNYUINRRK-KEY24-SEL-SEC
           IF    ( FLG-PTNYUINRRK  =   ZERO )
               IF    ( PTNYUINRRK-TAIINYMD =   "99999999" )
                   MOVE    31      TO  CL-KONGETU-END
               ELSE
                   MOVE    PTNYUINRRK-TAIINYMD (7:2)
                                   TO  WRK-DAY-X
                   MOVE    WRK-DAY TO  CL-KONGETU-END
               END-IF
           ELSE
               MOVE    1           TO  FLG-END
           END-IF
      *
           MOVE    WRK-ZENGETUMATU(1:6) TO  WRK-SRYYM
           PERFORM 1001-NYUINACCT-GET-SEC
           MOVE    CL-RYOYO-FLG    TO  CL-ZENGETU-RYOYO-FLG
           MOVE    CL-DAY-G        TO  CL-ZENGETU-DAY-G
      *
           MOVE    SS005-SRYYM     TO  WRK-SRYYM
           PERFORM 1001-NYUINACCT-GET-SEC
           MOVE    CL-RYOYO-FLG    TO  CL-KONGETU-RYOYO-FLG
           MOVE    CL-DAY-G        TO  CL-KONGETU-DAY-G
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5015"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    WRK-SRYYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5015-REC
      *
           IF    ( WRK-SRYYMD     >=   CONST-H220401 )
               MOVE    "1"         TO  SYS-5015-ADLINPUTKBN
           END-IF
      *
           PERFORM 800-MCNDATE-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    入院会計情報取得処理
      *****************************************************************
       1001-NYUINACCT-GET-SEC          SECTION.
      *
           INITIALIZE                  CL-DAY-G
                                       CL-RYOYO-FLG
      *
           PERFORM 10011-EDIT-RYOYO-DAY-SEC
      *
           .
       1001--NYUINACCT-GET-EXT.
           EXIT.
      *****************************************************************
      *    療養型入院基本料算定期間編集処理
      *****************************************************************
       10011-EDIT-RYOYO-DAY-SEC         SECTION.
      *
           PERFORM 900-NYUINACCT-KEY35-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               INITIALIZE      WRK-RYOYO-DAY-G
      *
               PERFORM 100111-EDIT-RYOYO-DAY-SEC
      *
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
      *
                   IF    ( WRK-RYOYO-DAY (IDX-DAY) NOT =   ZERO )
                       MOVE    WRK-RYOYO-DAY (IDX-DAY)
                                       TO  CL-DAY (IDX-DAY)
                       IF    ( WRK-RYOYO-DAY (IDX-DAY) >   ZERO )
                           MOVE    1   TO  CL-RYOYO-FLG
                       END-IF
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
       10011-EDIT-RYOYO-DAY-EXT.
           EXIT.
      *****************************************************************
      *    療養型入院基本料算定期間編集処理
      *****************************************************************
       100111-EDIT-RYOYO-DAY-SEC         SECTION.
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
                   PERFORM 1001111-RYOYOCD-CHK-SEC
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
       100111-EDIT-RYOYO-DAY-EXT.
           EXIT.
      *****************************************************************
      *    療養型入院基本料診療行為コードチェック処理
      *****************************************************************
       1001111-RYOYOCD-CHK-SEC          SECTION.
      *
           PERFORM VARYING IDXK    FROM    1   BY  1
                   UNTIL ( IDXK    >   CONST-RYOYOCD-KHN-MAX )
                    OR   ( WRK-RYOYO-DAY-G NOT =   ZERO )
               PERFORM VARYING IDXR    FROM    1   BY  1
                   UNTIL ( IDXR    >   CONST-RYOYOCD-CD-MAX  )
                    OR   ( WRK-RYOYO-DAY-G NOT =   ZERO )
                   IF    ( NSRY-SRYCD (IDX-SRY)
                                   =   CONST-RYOYOCD  (IDXK IDXR))
                       PERFORM VARYING IDX-DAY FROM    1   BY  1
                               UNTIL ( IDX-DAY >   31 )
      *
                           IF    ( NACCT-DAY (IDX-DAY) >   ZERO )
                               MOVE    1   TO  WRK-RYOYO-DAY (IDX-DAY)
                           END-IF
      *
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           IF    ( WRK-RYOYO-DAY-G         =   ZERO )
               MOVE    NSRY-SRYCD (IDX-SRY)    TO  WRK-TNS-SRYCD
               MOVE    WRK-SRYYM               TO  WRK-TNS-KJNYMD
               MOVE    "01"                    TO  WRK-TNS-KJNYMD (7:2)
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
       1001111-RYOYOCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    WRK-ZENGETU     TO  WRK-SRYYM
           PERFORM 900-RYOCOM-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-RYOCOM  NOT =   ZERO )
                    OR   ( SS005-RC    NOT =   ZERO )
      *
               IF    ( RYOCOM-MONTH        =   ZERO )
                AND  ( RYOCOM-DAY-TBL      =   ZERO )
                   CONTINUE
               ELSE
      *
                   IF    ( RYOCOM-ZAISKBKBN    =   "1" )
                       IF    ( SYS-5015-ADLINPUTKBN
                                           =   SPACE OR "1" )
                           IF    ( RYOCOM-ZAINUM
                                           =   CONST-ADL-TTLZAINUM )
                               CONTINUE
                           ELSE
                               PERFORM 2001-ADLKBN-DATA-INS-SEC
                           END-IF
                       ELSE
                           IF    ( RYOCOM-ZAINUM
                                           =   CONST-ADL-TTLZAINUM )
                               PERFORM 2001-ADLKBN-DATA-INS-SEC
                           END-IF
                       END-IF
                   END-IF
      *
                   IF    ( RYOCOM-ZAISKBKBN    =   "2" )
                       IF    ( SYS-5015-SETMEDKBN  =   "2" )
                           INITIALIZE                  SYSKANRI-REC
                           MOVE    "5014"          TO  SYS-KANRICD
                           MOVE    RYOCOM-ZAINUM   TO  WRK-ZAINUM
                           MOVE    WRK-ZAINUM-G    TO  SYS-KBNCD
                           MOVE    WRK-SRYYMD      TO  SYS-STYUKYMD
                                                       SYS-EDYUKYMD
                           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                           MOVE    SYSKANRI-REC    TO  SYS-5014-REC
                           IF    ( FLG-SYSKANRI    =   ZERO )
                               CONTINUE
                           ELSE
                               MOVE    "1"         TO  SYS-5014-DISPKBN
                           END-IF
                           IF    ( SYS-5014-DISPKBN    =   "1" )
                            AND  ( RYOCOM-MONTH    NOT =   ZERO )
                               PERFORM 2001-IRYOKBN-DATA-INS-SEC
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
               PERFORM 900-RYOCOM-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療区分データ作成処理
      *****************************************************************
       2001-IRYOKBN-DATA-INS-SEC       SECTION.
      *
           INITIALIZE                  RYOCOM-DAY-AREA
      *
           MOVE    SPA-HOSPNUM     TO  RYOCOM-HOSPNUM
      *
           IF    ( CL-ZENGETU-RYOYO-FLG    =   ZERO )
            OR   ( CL-KONGETU-RYOYO-FLG    =   ZERO )
               MOVE    ZERO        TO  RYOCOM-MONTH
           END-IF
      *
           MOVE    SS005-SRYYM     TO  RYOCOM-SRYYM
      *
           MOVE    SMCNDATE-YMD    TO  RYOCOM-CREYMD
                                       RYOCOM-UPYMD
           MOVE    SMCNDATE-HMS    TO  RYOCOM-UPHMS
      *
           MOVE    RYOCOM-REC      TO  MCPDATA-REC
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    1           TO  SS005-RC
           END-IF
      *
           .
       2001-IRYOKBN-DATA-INS-EXT.
           EXIT.
      *****************************************************************
      *    ADL区分データ作成処理
      *****************************************************************
       2001-ADLKBN-DATA-INS-SEC        SECTION.
      *
           MOVE    RYOCOM-REC      TO  WKRYOCOM-REC
      *
           INITIALIZE                  RYOCOM-DAY-AREA
      *
           MOVE    SPA-HOSPNUM     TO  RYOCOM-HOSPNUM
      *
           IF    ( CL-ZENGETU-RYOYO-FLG    =   ZERO )
            OR   ( CL-KONGETU-RYOYO-FLG    =   ZERO )
               MOVE    ZERO        TO  RYOCOM-MONTH
           ELSE
               IF    ( RYOCOM-MONTH    >   ZERO )
                   CONTINUE
               ELSE
                   IF    ( CL-ZENGETU-DAY (CL-ZENGETU-END) >   ZERO )
                       PERFORM VARYING IDX-DAY FROM    1   BY  1
                               UNTIL ( IDX-DAY >   CL-KONGETU-END )
      *
                           IF    ( CL-KONGETU-DAY(IDX-DAY) >   ZERO )
                               MOVE    WKRYOCOM-DAY (CL-ZENGETU-END)
                                       TO  RYOCOM-DAY (IDX-DAY)
                           END-IF
      *
                       END-PERFORM
                   END-IF
               END-IF
           END-IF
      *
           MOVE    SS005-SRYYM     TO  RYOCOM-SRYYM
      *
           MOVE    SMCNDATE-YMD    TO  RYOCOM-CREYMD
                                       RYOCOM-UPYMD
           MOVE    SMCNDATE-HMS    TO  RYOCOM-UPHMS
      *
           MOVE    RYOCOM-REC      TO  MCPDATA-REC
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    2           TO  SS005-RC
           END-IF
      *
           .
       2001-ADLKBN-DATA-INS-EXT.
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
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-YMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    療養レセプトコメントマスタ検索処理
      *****************************************************************
       900-RYOCOM-KEY2-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-RYOCOM
      *
           INITIALIZE                  RYOCOM-REC
           MOVE    SPA-HOSPNUM      TO  RYOCOM-HOSPNUM
           MOVE    "1"             TO  RYOCOM-NYUGAIKBN
           MOVE    SPA-PTID        TO  RYOCOM-PTID
           MOVE    WRK-SRYYM       TO  RYOCOM-SRYYM
           MOVE    RYOCOM-REC      TO  MCPDATA-REC
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  RYOCOM-REC
           ELSE
               MOVE    1           TO  FLG-RYOCOM
               INITIALIZE              RYOCOM-REC
           END-IF
      *
           .
       900-RYOCOM-KEY2-SEL-EXT.
            EXIT.
      *****************************************************************
      *    療養レセプトコメントマスタFETCH処理
      *****************************************************************
       900-RYOCOM-KEY2-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-RYOCOM
      *
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  RYOCOM-REC
           ELSE
               MOVE    1           TO  FLG-RYOCOM
               INITIALIZE              RYOCOM-REC
           END-IF
      *
           .
       900-RYOCOM-KEY2-FET-EXT.
            EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY24-SEL-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNYUINRRK
      *
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM      TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID        TO  PTNYUINRRK-PTID
           MOVE    WRK-ZENGETUMATU TO  PTNYUINRRK-NYUINYMD
           MOVE    SS005-SRYYM     TO  PTNYUINRRK-TAIINYMD
           MOVE    "01"            TO  PTNYUINRRK-TAIINYMD (7:2)
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
           MOVE    "key24"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               MOVE    1           TO  FLG-PTNYUINRRK
               INITIALIZE              PTNYUINRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
           MOVE    "key24"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY24-SEL-EXT.
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
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    WRK-TNS-SRYCD   TO  TNS-SRYCD
           MOVE    WRK-TNS-KJNYMD  TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
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
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY17-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE  NYUINACCT-REC
           MOVE    SPA-HOSPNUM          TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    WRK-SRYYM           TO  NACCT-SRYYM
           MOVE    WRK-ZAISKBKBN       TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  NYUINACCT-REC
           ELSE
               INITIALIZE              NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
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
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY35-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE                  NYUINACCT-REC
           MOVE    SPA-HOSPNUM          TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    WRK-SRYYM           TO  NACCT-SRYYM
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
           MOVE    NACCT-HOSPNUM        TO  NSRY-HOSPNUM
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
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYSKANRI-KEY10-SEL-EXT.
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
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC            SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
