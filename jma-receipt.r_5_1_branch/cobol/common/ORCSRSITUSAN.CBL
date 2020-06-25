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
       IDENTIFICATION                      DIVISION.
       PROGRAM-ID.                         ORCSRSITUSAN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 労災通算日数取得サブ
      *  返却エラーコード  : 00:正常　99:エラー
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/02/06    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    04.09.09  新規作成
      *  03.05.00    NACL-太田    07/05/10  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                         DIVISION.
       CONFIGURATION                       SECTION.
       INPUT-OUTPUT                        SECTION.
       FILE-CONTROL.
      *
      *
       DATA                                DIVISION.
      *FILE                                SECTION.
      *
      *
       WORKING-STORAGE                     SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINRYODAY-G.
               05  FLG-NYUINRYODAY         PIC 9(01)
                                           OCCURS  31.
           03  FLG-HKNCOMBIDAY-G.
               05  FLG-HKNCOMBIDAY         PIC 9(01)
                                           OCCURS  31.
           03  FLG-SHONUMDAY-G.
               05  FLG-SHONUMDAY           PIC 9(03)
                                           OCCURS  31.
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX-DAY                     PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYYM                   PIC X(06).
           03  WRK-YMD                     PIC X(10). 
           03  WRK-HENYMDG                 PIC X(09).
           03  WRK-SYMD.
               05  WRK-SYY                 PIC 9(04).
               05  WRK-SMM                 PIC 9(02).
               05  WRK-SDD                 PIC 9(02).
           03  WRK-STYMD                   PIC X(08).
           03  WRK-EDYMD                   PIC X(08).
           03  WRK-STSRYYM                 PIC X(06).
           03  WRK-EDSRYYM                 PIC X(06).
           03  WRK-NISSU                   PIC 9(05).
           03  WRK-NISSU1                  PIC 9(05).
           03  WRK-NISSU2                  PIC 9(05).
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-KEISANYMD               PIC X(08).
           03  WRK-STIDX                   PIC 9(02).
           03  WRK-EDIDX                   PIC 9(02).
           03  WRK-DAY-X                   PIC X(02).
           03  WRK-DAY-R   REDEFINES   WRK-DAY-X.
               05  WRK-DAY-9               PIC 9(02).
           03  WRK-TUKISTYMD               PIC X(08).
           03  WRK-TUKIEDYMD               PIC X(08).
           03  WRK-STDAY                   PIC 9(02).
           03  WRK-EDDAY                   PIC 9(02).
           03  WRK-SHONUM                  PIC 9(03).
           03  WRK-GETUMATUYMD             PIC X(08).
           03  WRK-NYUINYMD                PIC X(08).
           03  WRK-SRYYMD.
               05  WRK-SRYYMD-YM           PIC X(06).
               05  WRK-SRYYMD-DD           PIC 9(02).
      *
       01  TSHONUM-AREA.
           03  TSHONUM-IDX                 PIC 9(03).
           03  TSHONUM-MAX                 PIC 9(03).
           03  TSHONUM-OCC                 OCCURS  31.
               05  TSHONUM-SHONUM          PIC 9(03).
               05  TSHONUM-NYUINYMD        PIC X(08).
      *
       01  CONST-AREA.
           03  CONST-H280401           PIC X(08)   VALUE "20160401".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    入院会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    同日再入院チェックサブ
           COPY    "CPORCSDNTCHK.INC".
      *
       01  LNKNRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-// 
                                       BY         //LNKNRRK-//.
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    共通領域
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                             SECTION.
      *
           COPY    "CPORCSRSITUSAN.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                           DIVISION    USING
           ORCSRSITUSANAREA
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                        SECTION.
      *
           INITIALIZE                      WRK-AREA
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      TSHONUM-AREA
           INITIALIZE                      RSITUSAN-OUT-AREA
           INITIALIZE                      RSITUSAN-RC
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
           IF    ( RSITUSAN-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    主処理
           PERFORM 200-MAIN-SEC
           IF    ( RSITUSAN-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *
           PERFORM 990-EXIT-PROGRAM-SEC
      *
           .
       000-PROC-EXT.
           EXIT.
           .
      *
      *****************************************************************
      *    パラメータチェック
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
           IF    ( RSITUSAN-SRYYM      =   SPACE )
               MOVE    01              TO  RSITUSAN-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           IF    ( RSITUSAN-HKNCOMBI   =   SPACE )
               MOVE    02              TO  RSITUSAN-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           MOVE    RSITUSAN-SRYYM      TO  WRK-YMD
           MOVE    "01"                TO  WRK-YMD (7:2)
           PERFORM 800-HIZUKE-CHK-SEC
           IF    ( RSITUSAN-RC     NOT =   ZERO )
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                        SECTION.
      *
      *    月末日取得
           MOVE    RSITUSAN-SRYYM  TO  WRK-SYMD
           PERFORM 800-GETUMATU-SEC
           MOVE    WRK-SYMD        TO  WRK-GETUMATUYMD
      *
      *    初回番号取得処理
           PERFORM 2100-SHONUM-GET-SEC
      *
           PERFORM VARYING TSHONUM-IDX FROM    1   BY  1
                   UNTIL ( TSHONUM-IDX >   TSHONUM-MAX )
      *
               MOVE    TSHONUM-SHONUM (TSHONUM-IDX)
                                   TO  WRK-SHONUM
               MOVE    TSHONUM-NYUINYMD (TSHONUM-IDX)
                                   TO  WRK-NYUINYMD
      *
      *        日数取得処理
               IF    ( RSITUSAN-SRYYM >=   CONST-H280401 (1:6) )
                   PERFORM 2101-NISSU-GET-SEC
               ELSE
                   PERFORM 2100-NISSU-GET-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    初回番号取得処理
      *****************************************************************
       2100-SHONUM-GET-SEC                 SECTION.
      *
      *    入院料チェック処理
           MOVE    RSITUSAN-SRYYM      TO  WRK-SRYYM
           PERFORM 2110-NYUINRYO-CHK-SEC
      *
      *    保険組合せチェック処理
           MOVE    RSITUSAN-SRYYM      TO  WRK-SRYYM
           PERFORM 2110-HKNCOMBI-CHK-SEC
      *
      *    診療年月初回番号編集処理
           PERFORM 2110-SHONUMDAY-HEN-SEC
      *
      *    初回番号取得処理
           PERFORM 2110-SHONUM-GET-SEC
      *
           .
       2100-SHONUM-GET-EXT.
           EXIT.
      *****************************************************************
      *    入院料チェック処理
      *****************************************************************
       2110-NYUINRYO-CHK-SEC           SECTION.
      *
           INITIALIZE                          FLG-NYUINRYODAY-G
      *
           MOVE    ZERO                    TO  FLG-NYUINACCT
           INITIALIZE                          NYUINACCT-REC
           MOVE    RSITUSAN-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    RSITUSAN-PTID           TO  NACCT-PTID
           MOVE    WRK-SRYYM               TO  NACCT-SRYYM
           MOVE    "1"                     TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC           TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    1                   TO  FLG-NYUINACCT
           ELSE
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
      *        選定入院料は読み飛ばし
               IF    ( NACCT-SRYCDTOTAL    =    099999916 )
                   CONTINUE
               ELSE
      *
      *            入院料を算定している日にちを退避
                   PERFORM VARYING     IDX1    FROM    1   BY  1
                           UNTIL     ( IDX1    >   31 )
      *
                       IF   ( NACCT-DAY(IDX1)  >   ZERO )
                           MOVE    1       TO  FLG-NYUINRYODAY (IDX1)
                       END-IF
      *
                   END-PERFORM
               END-IF
      *
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              NOT =   ZERO )
                   MOVE    1               TO  FLG-NYUINACCT
               ELSE
                   MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSE-SEC
      *
           .
       2110-NYUINRYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       2110-HKNCOMBI-CHK-SEC               SECTION.
      *
           MOVE    ZERO                    TO  FLG-HKNCOMBIDAY-G
      *
           MOVE    ZERO                    TO  FLG-NYUINACCT
           INITIALIZE                          NYUINACCT-REC
           MOVE    RSITUSAN-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    RSITUSAN-PTID           TO  NACCT-PTID
           MOVE    WRK-SRYYM               TO  NACCT-SRYYM
           MOVE    "5"                     TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC           TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    1                   TO  FLG-NYUINACCT
           ELSE
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           END-IF
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSE-SEC
      *
           IF    ( FLG-NYUINACCT       NOT =   ZERO )
               MOVE    03                  TO  RSITUSAN-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL     ( IDX1    >   31 )
                IF   ( RSITUSAN-HKNCOMBI   =   NACCT-DAY(IDX1) )
                   MOVE    1               TO  FLG-HKNCOMBIDAY (IDX1)
                END-IF
           END-PERFORM
      *
           IF    ( WRK-SRYYM               =   RSITUSAN-SRYYM )
               IF    ( FLG-HKNCOMBIDAY-G   =   ZERO )
                   MOVE    04                  TO  RSITUSAN-RC
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
           END-IF
      *
           .
      *
       2110-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療年月初回番号編集処理
      *****************************************************************
       2110-SHONUMDAY-HEN-SEC          SECTION.
      *
           INITIALIZE                  FLG-SHONUMDAY-G
           MOVE    SPACE           TO  WRK-TUKISTYMD
                                       WRK-TUKIEDYMD
      *
           MOVE    RSITUSAN-SRYYM  TO  WRK-TUKISTYMD
           MOVE    "01"            TO  WRK-TUKISTYMD (7:2)
           MOVE    WRK-GETUMATUYMD TO  WRK-TUKIEDYMD
      *
      *
      *    診療月内の入退院情報
           MOVE    ZERO            TO  FLG-PTNYUINRRK
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    RSITUSAN-HOSPNUM TO  PTNYUINRRK-HOSPNUM
           MOVE    RSITUSAN-PTID   TO  PTNYUINRRK-PTID
           MOVE    WRK-TUKIEDYMD   TO  PTNYUINRRK-NYUINYMD
           MOVE    WRK-TUKISTYMD   TO  PTNYUINRRK-TAIINYMD
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
           MOVE    "key23"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1           TO  FLG-PTNYUINRRK
           ELSE
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
      *
               IF    ( PTNYUINRRK-NYUINYMD (1:6)
                                       <   WRK-TUKISTYMD )
                   MOVE    01          TO  WRK-STDAY
               ELSE
                   MOVE    PTNYUINRRK-NYUINYMD (7:2)
                                       TO  WRK-STDAY
               END-IF
      *
               IF    ( PTNYUINRRK-TAIINYMD (1:6)
                                       >   WRK-TUKIEDYMD )
                   MOVE    31          TO  WRK-EDDAY
               ELSE
                   MOVE    PTNYUINRRK-TAIINYMD (7:2)
                                       TO  WRK-EDDAY
               END-IF
      *
               PERFORM VARYING IDX-DAY FROM    WRK-STDAY   BY  1
                       UNTIL ( IDX-DAY >   WRK-EDDAY )
                   MOVE    PTNYUINRRK-SHONUM
                                       TO  FLG-SHONUMDAY (IDX-DAY)
               END-PERFORM
      *
               MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
               MOVE    "key23"         TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    1               TO  FLG-PTNYUINRRK
               ELSE
                   MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
           MOVE    "key23"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSE-SEC
      *
           .
       2110-SHONUMDAY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    初回番号取得処理
      *****************************************************************
       2110-SHONUM-GET-SEC             SECTION.
      *
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL ( IDX3    >   31 )
      *
               IF    ( FLG-HKNCOMBIDAY (IDX3)  >   ZERO )
                AND  ( FLG-NYUINRYODAY (IDX3)  >   ZERO
                 OR    RSITUSAN-SRYYM          >=  CONST-H280401 (1:6))
                AND  ( FLG-SHONUMDAY   (IDX3)  >   ZERO )
      *
                   MOVE    FLG-SHONUMDAY (IDX3)
                                   TO  WRK-SHONUM
      *
                   PERFORM 21101-SHONUM-GET-SEC
      *
               END-IF
           END-PERFORM
      *
           .
       2110-SHONUM-GET-EXT.
           EXIT.
      *****************************************************************
      *    初回番号取得処理
      *****************************************************************
       21101-SHONUM-GET-SEC            SECTION.
      *
           MOVE    ZERO            TO  TSHONUM-IDX
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0 >  TSHONUM-MAX )
                    OR   ( TSHONUM-IDX NOT =   ZERO )
      *
               IF    ( TSHONUM-SHONUM (IDX0)   =  WRK-SHONUM )
                   MOVE    IDX0    TO  TSHONUM-IDX
               END-IF
      *
           END-PERFORM
      *
           IF    ( TSHONUM-IDX     =   ZERO )
               IF    ( TSHONUM-MAX <   31 )
      *
                   COMPUTE TSHONUM-MAX =   TSHONUM-MAX +   1
                   MOVE    WRK-SHONUM
                                   TO  TSHONUM-SHONUM (TSHONUM-MAX)
                   PERFORM 900-PTNYUINRRK-KEY52-SEL-SEC
                   MOVE    PTNYUINRRK-NYUINYMD
                                   TO  TSHONUM-NYUINYMD(TSHONUM-MAX)
      *
               END-IF
           END-IF
      *
           .
       21101-SHONUM-GET-EXT.
           EXIT.
      *****************************************************************
      *    日数取得処理
      *****************************************************************
       2100-NISSU-GET-SEC                  SECTION.
      *
           MOVE    ZERO            TO  WRK-NISSU
      *
           MOVE    ZERO            TO  FLG-PTNYUINRRK
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    RSITUSAN-HOSPNUM TO  PTNYUINRRK-HOSPNUM
           MOVE    RSITUSAN-PTID   TO  PTNYUINRRK-PTID
           MOVE    WRK-GETUMATUYMD TO  PTNYUINRRK-NYUINYMD
           MOVE    WRK-SHONUM      TO  PTNYUINRRK-SHONUM
      *
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
           MOVE    "key52"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    1           TO  FLG-PTNYUINRRK
           ELSE
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
      *
               IF    ( PTNYUINRRK-RRKEDANUM    =   1 )
                   IF    ( PTNYUINRRK-JTIKBN   =   "6" )
                    OR   ( PTNYUINRRK-JTIKBN   =   "5" 
                      AND  PTNYUINRRK-TAINRELKBN   =   "1" )
                       IF    ( PTNYUINRRK-SHONUM   =   WRK-SHONUM )
                           PERFORM 2110-JIINRRK-NISSU-HEN-SEC
                       END-IF
                   ELSE
                       PERFORM 2110-NISSU-HEN-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
               MOVE    "key52"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    1           TO  FLG-PTNYUINRRK
               ELSE
                   MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
           MOVE    "key52"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSE-SEC
      *
           .
       2100-NISSU-GET-EXT.
           EXIT.
      *****************************************************************
      *    日数取得処理
      *****************************************************************
       2101-NISSU-GET-SEC                  SECTION.
      *
           MOVE    ZERO                TO  WRK-NISSU
      *
           IF    ( WRK-NYUINYMD    NOT =   SPACE )
            AND  ( WRK-NYUINYMD (1:6)  <   RSITUSAN-SRYYM )
               MOVE    WRK-NYUINYMD    TO  WRK-STYMD
               MOVE    WRK-TUKISTYMD   TO  WRK-EDYMD
               PERFORM 800-NISSU-SEC
               COMPUTE WRK-NISSU = WRK-NISSU1
           END-IF
      *
           MOVE    RSITUSAN-SRYYM      TO  WRK-SRYYMD
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       31 )
      *
               MOVE    IDX1            TO  WRK-SRYYMD-DD
      *
               IF    ( WRK-NYUINYMD    NOT =   SPACE )
                 AND ( WRK-NYUINYMD       <=   WRK-SRYYMD )
      *
                   COMPUTE WRK-NISSU   =   WRK-NISSU   +   1
      *
                   IF    ( FLG-SHONUMDAY (IDX1)    =   WRK-SHONUM )
                       IF   ( FLG-HKNCOMBIDAY (IDX1)  =   1 )
                           IF    ( WRK-NISSU   <=  14 )
                               MOVE    "1" TO  RSITUSAN-DAY (IDX1)
                           ELSE
                               MOVE    "2" TO  RSITUSAN-DAY (IDX1)
                           END-IF
                       END-IF
                  END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           .
       2101-NISSU-GET-EXT.
           EXIT.
      *****************************************************************
      *    自院歴より日数編集処理
      *****************************************************************
       2110-JIINRRK-NISSU-HEN-SEC          SECTION.
      *
           COMPUTE WRK-NISSU
               =   WRK-NISSU   +   PTNYUINRRK-TAISYONISSU
      *
           .
       2110-JIINRRK-NISSU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    日数通算処理
      *****************************************************************
       2110-NISSU-HEN-SEC              SECTION.
      *
           INITIALIZE                  SDNTCHK-AREA
           MOVE    PTNYUINRRK-REC      TO      LNKNRRK-REC
           CALL    "ORCSDNTCHK"        USING   SDNTCHK-AREA
                                               LNKNRRK-REC
      *
           IF    ( SDNTCHK-STYMD   NOT =   ZERO )
               MOVE    SDNTCHK-STYMD    TO  WRK-STYMD
               IF    ( PTNYUINRRK-TAIINYMD >   WRK-GETUMATUYMD )
                   MOVE    WRK-GETUMATUYMD     TO  WRK-EDYMD
               ELSE
                   MOVE    PTNYUINRRK-TAIINYMD TO  WRK-EDYMD
               END-IF
      *
               PERFORM 2120-NYUINKIKAN-SEL-SEC
           END-IF
      *
           .
       2110-NISSU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院期間検索処理
      *****************************************************************
       2120-NYUINKIKAN-SEL-SEC              SECTION.
      *
           MOVE    WRK-STYMD           TO  WRK-STSRYYM
           MOVE    WRK-EDYMD           TO  WRK-EDSRYYM
      *
           PERFORM UNTIL ( WRK-STSRYYM >   WRK-EDSRYYM )
      *
               PERFORM 2121-NYUINACCT-SEL-SEC
      *
               MOVE   WRK-STSRYYM      TO  WRK-YMD
               MOVE   "01"             TO  WRK-YMD (7 : 2)
               MOVE   "2"              TO  WRK-ZOGENPTN
               MOVE   1                TO  WRK-ZOGEN
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-KEISANYMD   TO  WRK-STSRYYM
      *
           END-PERFORM
      *
           .
       2120-NYUINKIKAN-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       2121-NYUINACCT-SEL-SEC              SECTION.
      *
      *    入院料チェック処理
           MOVE    WRK-STSRYYM         TO  WRK-SRYYM
           PERFORM 2110-NYUINRYO-CHK-SEC
      *
      *    保険組合せチェック処理
           MOVE    WRK-STSRYYM         TO  WRK-SRYYM
           PERFORM 2110-HKNCOMBI-CHK-SEC
      *
           IF    ( WRK-STYMD (1 : 6)   =   WRK-STSRYYM )
               MOVE    WRK-STYMD (7 : 2)   TO  WRK-STIDX
           ELSE
               MOVE    1                   TO  WRK-STIDX
           END-IF
      *
           IF    ( WRK-EDYMD (1 : 6)   =   WRK-STSRYYM )
               MOVE    WRK-EDYMD (7 : 2)   TO  WRK-EDIDX
           ELSE
               MOVE    31                  TO  WRK-EDIDX
           END-IF
      *
           PERFORM VARYING IDX1    FROM    WRK-STIDX   BY  1
                   UNTIL ( IDX1    >       WRK-EDIDX )
      *
               IF    ( FLG-NYUINRYODAY (IDX1)  >   ZERO )
      *
                   IF   ( PTNYUINRRK-SHONUM    =   WRK-SHONUM )
      *
                       COMPUTE WRK-NISSU   =   WRK-NISSU   +   1
      *
                       IF    ( RSITUSAN-SRYYM  =   WRK-STSRYYM )
                        AND  ( FLG-HKNCOMBIDAY (IDX1)
                                               =   1 )
                           IF    ( WRK-NISSU   <=  14 )
                               MOVE    "1" TO  RSITUSAN-DAY (IDX1)
                           ELSE
                               MOVE    "2" TO  RSITUSAN-DAY (IDX1)
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       2121-NYUINACCT-SEL-EXT.
           EXIT.
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       800-GETUMATU-SEC      SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       800-GETUMATU-EXT.
            EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       800-HIZUKE-CHK-SEC                  SECTION.
      *
           MOVE    SPACE                   TO  WRK-HENYMDG
                                               WRK-SYMD
      *
           IF    ( WRK-YMD                 =   SPACE )
               GO  TO  800-HIZUKE-CHK-EXT
           END-IF
      *
           INITIALIZE                          ORCSGDAYAREA
           MOVE    WRK-YMD                 TO  SGDAY-INDAY
           CALL    "ORCSGDAY"                  USING
                                               ORCSGDAYAREA
           IF  ( SGDAY-RC                  =   ZERO )
               MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY          TO  WRK-SYMD
           ELSE
               MOVE    06                  TO  RSITUSAN-RC
           END-IF
           .
       800-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日数処理
      *****************************************************************
       800-NISSU-SEC                       SECTION.
      *
           INITIALIZE                      WRK-NISSU1
                                           WRK-NISSU2
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    WRK-STYMD           TO  LNK-DAY5-START
           MOVE    WRK-EDYMD           TO  LNK-DAY5-END
           CALL    "ORCSDAY"               USING
                                           STS-AREA-DAY
                                           LNK-DAY5-AREA
           IF    ( STS-DAY-RC1         =   ZERO )
               MOVE    LNK-DAY5-NISSU1 TO  WRK-NISSU1
               MOVE    LNK-DAY5-NISSU2 TO  WRK-NISSU2
           END-IF
      *
      *
           .
       800-NISSU-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                    SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"                 USING
                                           STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY52-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    RSITUSAN-PTID       TO  PTNYUINRRK-PTID
           MOVE    WRK-GETUMATUYMD     TO  PTNYUINRRK-NYUINYMD
           MOVE    WRK-SHONUM          TO  PTNYUINRRK-SHONUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key52"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk" TO     MCP-TABLE
           MOVE    "key52"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSE-SEC
      *
           .
       900-PTNYUINRRK-KEY52-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                    SECTION.
      *
           MOVE    "DBSELECT"      TO      MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF    ( MCP-RC  =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                     SECTION.
      *
           MOVE    "DBFETCH"       TO      MCP-FUNC
           CALL    "ORCDBMAIN"             USING
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
       910-DBCLOSE-SEC                     SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO      MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
      *
       910-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           DISPLAY "I:RSITUSAN-HOSPNUM     = " RSITUSAN-HOSPNUM
           DISPLAY "I:RSITUSAN-PTID        = " RSITUSAN-PTID
           DISPLAY "I:RSITUSAN-HKNCOMBI    = " RSITUSAN-HKNCOMBI
           DISPLAY "I:RSITUSAN-SRYYM       = " RSITUSAN-SRYYM
           DISPLAY "-"
           DISPLAY "O:RSITUSAN-OUT-AREA    [" RSITUSAN-OUT-AREA "]"
           DISPLAY "O:RSITUSAN-RC = " RSITUSAN-RC
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
