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
       PROGRAM-ID.                     ORCSI02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 分娩管理情報編集
      *  管理者            : 
      *  作成日付    作業者        記述
      *  15/11/17    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
       WORKING-STORAGE                 SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-BUNBEN              PIC 9(01).
      *
      *    カウント領域
      *01  CNT-AREA.
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-KJNYMD.
               05  WRK-KJNYEAR         PIC 9(04).
               05  WRK-KJNMMDD         PIC X(04).
           03  WRK-BIRTHDAY.
               05  WRK-BIRTHYEAR       PIC 9(04).
               05  WRK-BIRTHMMDD       PIC X(04).
           03  WRK-AGE                 PIC 9(03).
           03  WRK-BASE-MONEY          PIC S9(08).
      *
       01  CONST-AREA.
           03  CONST-H211001           PIC X(08)   VALUE "20091001".
           03  CONST-H270101           PIC X(08)   VALUE "20150101".
      *
       01  TNRRK-AREA.
           COPY    "CPPTNYUINRRK.INC"  REPLACING   //PTNYUINRRK-//
                                       BY          //TNRRK-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
           COPY    "CPSK1005.INC".
      *
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    分娩負担金計算サブ
       01  LNKBUNBEN-REC.
           COPY  "CPBUNBEN.INC"    REPLACING  //BUNBEN-//
                                   BY       //LNKBUNBEN-//.
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                         SECTION.
      *
           COPY    "CPORCSI02.INC".
       01  SNRRK-AREA.
           COPY    "CPPTNYUINRRK.INC"  REPLACING   //PTNYUINRRK-//
                                       BY          //SNRRK-//.
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING   SI02-AREA
                                               SNRRK-AREA
                                               MCPAREA
                                               SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
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
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
      *    INITIALIZE                  CNT-AREA
           INITIALIZE                  IDX-AREA
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SNRRK-HOSPNUM   TO  SYS-HOSPNUM
           MOVE    "1005"          TO  SYS-KANRICD
           MOVE    SNRRK-NYUINKA   TO  SYS-KBNCD
           MOVE    SNRRK-NYUINYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1005-REC
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           EVALUATE    SI02-REQID
           WHEN    "1"
               PERFORM 2001-BUNBEN-CHK-SEC
           WHEN    "2"
               PERFORM 2001-BUNBEN-UPD-SEC
           WHEN    "3"
               PERFORM 2001-BUNBEN-CHK-SEC
               PERFORM 2001-BUNBEN-UPD-SEC
           WHEN    "4"
               PERFORM 2002-BUNBEN-UPD-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    分娩チェック処理
      *****************************************************************
       2001-BUNBEN-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  SI02-BUNBENCHK
           MOVE    ZERO            TO  SI02-BUNBENNUM
           MOVE    SPACE           TO  SI02-BUNBENKBN
           MOVE    SPACE           TO  SI02-RIYOUKBN
      *
           MOVE    SNRRK-NYUINYMD  TO  WRK-KJNYMD
           PERFORM 800-AGE-SEC
           IF    ( SYS-1005-RECESRYKA  =   "23" OR "24" )
            AND  ( PTINF-SEX       =   "2" )
            AND  ( WRK-AGE         >   10 )
               MOVE    1           TO  SI02-BUNBENCHK
           END-IF
      *
           IF    ( SNRRK-RRKNUM    NOT =   ZERO )
               PERFORM 900-BUNBEN-KEY3-SEL-SEC
               IF    ( FLG-BUNBEN      =   ZERO )
                   MOVE    1       TO  SI02-BUNBENCHK
                   MOVE    BUNBEN-RRKNUM
                                   TO  SI02-BUNBENNUM
                   MOVE    BUNBEN-BUNBENKBN
                                   TO  SI02-BUNBENKBN
                   MOVE    BUNBEN-RIYOUKBN
                                   TO  SI02-RIYOUKBN
               END-IF
           END-IF
      *
           .
       2001-BUNBEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    分娩テーブル更新処理
      *****************************************************************
       2001-BUNBEN-UPD-SEC             SECTION.
      *
           PERFORM 900-BUNBEN-KEY3-SEL-SEC
           IF    ( FLG-BUNBEN      =   ZERO )
               MOVE    "000000"    TO  BUNBEN-SKYYM
               INITIALIZE              BUNBEN-TEISYUTUBIKBN
                                       BUNBEN-SKYKBN
                                       BUNBEN-NYUIN-DAY
                                       BUNBEN-NYU-MONEY
                                       BUNBEN-RMSGK-MONEY
                                       BUNBEN-KAIJO-MONEY
                                       BUNBEN-BUNBEN-MONEY
                                       BUNBEN-SHIN-MONEY
                                       BUNBEN-KNS-MONEY
                                       BUNBEN-SYC-MONEY
                                       BUNBEN-SNK-MONEY
                                       BUNBEN-ETC-MONEY
                                       BUNBEN-ICBFTN-MONEY
                                       BUNBEN-NINPUFTN-MONEY
                                       BUNBEN-DAIRI-MONEY
               IF    ( BUNBEN-UPDKBN   NOT =   ZERO )
                   MOVE    "3"     TO  BUNBEN-UPDKBN
               END-IF
           ELSE
               INITIALIZE              BUNBEN-REC
               MOVE    SNRRK-HOSPNUM
                                   TO  BUNBEN-HOSPNUM
               MOVE    SNRRK-PTID  TO  BUNBEN-PTID
               MOVE    "1"         TO  BUNBEN-NYUGAIKBN
               MOVE    "0"         TO  BUNBEN-UPDKBN
               MOVE    SNRRK-UPYMD TO BUNBEN-CREYMD
               MOVE    SNRRK-RRKNUM TO  BUNBEN-RRKNUM
               MOVE    "000000"    TO  BUNBEN-SKYYM
           END-IF
      *
           MOVE    SI02-BUNBENKBN
                                   TO  BUNBEN-BUNBENKBN
      *
           IF    ( SI02-BUNBENKBN  =   "0" )
               MOVE    SPACE       TO  BUNBEN-RIYOUKBN
           ELSE
               IF    ( SI02-RIYOUKBN   =   SPACE )
                   MOVE    "1"     TO  BUNBEN-RIYOUKBN
               ELSE
                   MOVE    SI02-RIYOUKBN
                                   TO  BUNBEN-RIYOUKBN
               END-IF
           END-IF
           MOVE    SNRRK-UPYMD     TO  BUNBEN-UPYMD
           MOVE    SNRRK-UPHMS     TO  BUNBEN-UPHMS
      *
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "del3"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1           TO  SI02-RC
           END-IF
      *
           IF    ( SI02-RC         =   ZERO )
               MOVE    SNRRK-AREA      TO   TNRRK-AREA
               CALL    "ORCGI0SUB03"   USING
                                       TNRRK-AREA
                                       SPA-AREA
               IF    ( TNRRK-AREA  >   ZERO )
                AND  ( SI02-BUNBENKBN   =   "0" OR "1" OR "2" )
      *
                   MOVE    BUNBEN-REC      TO  MCPDATA-REC
                   MOVE    "tbl_bunben"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC  =   ZERO )
                       CONTINUE
                   ELSE
                       MOVE    2   TO  SI02-RC
                   END-IF
               END-IF
           END-IF
      *
           .
       2001-BUNBEN-UPD-EXT.
           EXIT.
      *****************************************************************
      *    分娩テーブル更新処理（退院登録時）
      *****************************************************************
       2002-BUNBEN-UPD-SEC             SECTION.
      *
           PERFORM 900-BUNBEN-KEY3-SEL-SEC
           IF    ( FLG-BUNBEN       =  ZERO )
             AND ( BUNBEN-BUNBENKBN =  "1" OR "2" )
             AND ( SNRRK-TAIINYMD   >=  CONST-H211001 )
               IF    ( BUNBEN-UPDKBN    =  ZERO )
                   PERFORM 20021-BUNBEN-UPD-SEC
               ELSE
                   PERFORM 20022-BUNBEN-UPD-SEC
               END-IF
           END-IF
      *
           .
       2002-BUNBEN-UPD-EXT.
           EXIT.
      *****************************************************************
      *    分娩テーブル更新処理（退院登録時、事前入力無し）
      *****************************************************************
       20021-BUNBEN-UPD-SEC             SECTION.
      *
           PERFORM 800-ORCSBUNBENFTN-SEC
      *
           MOVE    BUNBEN-RIYOUKBN TO  LNKBUNBEN-RIYOUKBN
           MOVE    BUNBEN-BUNBENKBN TO LNKBUNBEN-BUNBENKBN
           MOVE    "1"             TO  LNKBUNBEN-UPDKBN
           MOVE    BUNBEN-CREYMD   TO  LNKBUNBEN-CREYMD
           MOVE    LNKBUNBEN-REC   TO  BUNBEN-REC
      *
           MOVE    SNRRK-UPYMD     TO  BUNBEN-UPYMD
           MOVE    SNRRK-UPHMS     TO  BUNBEN-UPHMS
      *
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "del3"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    3           TO  SI02-RC
           END-IF
      *
           IF    ( SI02-RC         =   ZERO )
               MOVE    BUNBEN-REC      TO  MCPDATA-REC
               MOVE    "tbl_bunben"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC          =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    4       TO  SI02-RC
               END-IF
           END-IF
      *
           .
       20021-BUNBEN-UPD-EXT.
           EXIT.
      *****************************************************************
      *    分娩テーブル更新処理（退院登録時、事前入力あり）
      *****************************************************************
       20022-BUNBEN-UPD-SEC             SECTION.
      *
           PERFORM 800-ORCSBUNBENFTN-SEC
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1001"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SNRRK-TAIINYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1001-REC
           IF    ( SYS-1001-SANKANUM   IS  NOT NUMERIC )
            OR   ( SYS-1001-SANKANUM   =   ZERO    )
               MOVE    ZERO            TO  SYS-1001-SANKANUM
           END-IF
      *
           MOVE    BUNBEN-RIYOUKBN     TO  LNKBUNBEN-RIYOUKBN
           MOVE    BUNBEN-SKYYM        TO  LNKBUNBEN-SKYYM
           MOVE    BUNBEN-SKYKBN       TO  LNKBUNBEN-SKYKBN
           MOVE    BUNBEN-BUNBENKBN    TO  LNKBUNBEN-BUNBENKBN
           MOVE    BUNBEN-KANYUKBN     TO  LNKBUNBEN-KANYUKBN
           MOVE    BUNBEN-HONKZKKBN    TO  LNKBUNBEN-HONKZKKBN
           MOVE    BUNBEN-HKNJANUM     TO  LNKBUNBEN-HKNJANUM
           MOVE    BUNBEN-HKNJANUM-S   TO  LNKBUNBEN-HKNJANUM-S
           MOVE    BUNBEN-KIGO         TO  LNKBUNBEN-KIGO
           MOVE    BUNBEN-NUM          TO  LNKBUNBEN-NUM
           MOVE    BUNBEN-BIRTHDAY     TO  LNKBUNBEN-BIRTHDAY
           MOVE    BUNBEN-ZAIWEEK      TO  LNKBUNBEN-ZAIWEEK
           MOVE    BUNBEN-SIZANKBN     TO  LNKBUNBEN-SIZANKBN
           MOVE    BUNBEN-BIRTHCNT     TO  LNKBUNBEN-BIRTHCNT
           IF    ( BUNBEN-NYUIN-DAY    NOT =   ZERO )
               MOVE    BUNBEN-NYUIN-DAY TO LNKBUNBEN-NYUIN-DAY
           END-IF
           MOVE    BUNBEN-SANKAKBN     TO  LNKBUNBEN-SANKAKBN
           MOVE    BUNBEN-BIKOU        TO  LNKBUNBEN-BIKOU
           MOVE    "1"                 TO  LNKBUNBEN-UPDKBN
           MOVE    BUNBEN-CREYMD       TO  LNKBUNBEN-CREYMD
           MOVE    BUNBEN-TEISYUTUBIKBN TO LNKBUNBEN-TEISYUTUBIKBN
      *
           MOVE    LNKBUNBEN-REC       TO  BUNBEN-REC
      *
           MOVE    ZERO                TO  BUNBEN-DAIRI-MONEY
           MOVE    ZERO                TO  WRK-BASE-MONEY
           IF    ( BUNBEN-RIYOUKBN     =   "1" )
      *
               EVALUATE    TRUE
               WHEN  ( SYS-1001-SANKANUM    =   ZERO )
               WHEN  ( BUNBEN-ZAIWEEK     <   22 )
               WHEN  ( BUNBEN-SANKAKBN    =   "2" OR "3" )
                   IF    ( BUNBEN-BIRTHDAY   NOT =   SPACE )
                       EVALUATE   TRUE
                       WHEN    ( BUNBEN-BIRTHDAY
                                          >= CONST-H270101 )
                           MOVE    404000  TO  WRK-BASE-MONEY
                       WHEN    OTHER
                           MOVE    390000  TO  WRK-BASE-MONEY
                       END-EVALUATE
                   END-IF
               WHEN    OTHER
                   MOVE    420000      TO  WRK-BASE-MONEY
               END-EVALUATE
      *
               IF    ( BUNBEN-SANKAKBN    =   "3" )
                   COMPUTE BUNBEN-DAIRI-MONEY
                       =   WRK-BASE-MONEY  *   BUNBEN-BIRTHCNT
                       +   BUNBEN-SNK-MONEY
               ELSE
                   COMPUTE BUNBEN-DAIRI-MONEY
                       =   WRK-BASE-MONEY  *   BUNBEN-BIRTHCNT
               END-IF
      *
           END-IF
      *
           MOVE    SNRRK-UPYMD     TO  BUNBEN-UPYMD
           MOVE    SNRRK-UPHMS     TO  BUNBEN-UPHMS
      *
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "del3"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    5           TO  SI02-RC
           END-IF
      *
           IF    ( SI02-RC         =   ZERO )
               MOVE    BUNBEN-REC      TO  MCPDATA-REC
               MOVE    "tbl_bunben"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC          =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    6       TO  SI02-RC
               END-IF
           END-IF
      *
           .
       20022-BUNBEN-UPD-EXT.
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
      *    年齢編集処理
      *****************************************************************
       800-AGE-SEC                     SECTION.
      *
           MOVE    ZERO                TO  WRK-AGE
      *
           MOVE    PTINF-BIRTHDAY      TO  WRK-BIRTHDAY
           COMPUTE WRK-AGE         =   WRK-KJNYEAR
                                   -   WRK-BIRTHYEAR
           IF    ( PTINF-BIRTHDAY (5:4)    >   WRK-KJNYMD (5:4) )
                COMPUTE WRK-AGE    =   WRK-AGE -   1
           END-IF
      *
           .
       800-AGE-EXT.
           EXIT.
      *****************************************************************
      *    分娩負担金サブ
      *****************************************************************
       800-ORCSBUNBENFTN-SEC           SECTION.
      *
           INITIALIZE                  LNKBUNBEN-REC
           MOVE    SNRRK-HOSPNUM   TO  LNKBUNBEN-HOSPNUM
           MOVE    SNRRK-PTID      TO  LNKBUNBEN-PTID
           MOVE    "1"             TO  LNKBUNBEN-NYUGAIKBN
           MOVE    SNRRK-RRKNUM    TO  LNKBUNBEN-RRKNUM
           CALL    "ORCSBUNBENFTN"     USING
                                       LNKBUNBEN-REC
                                       SPA-AREA
      *
           .
      *
       800-ORCSBUNBENFTN-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SNRRK-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    SNRRK-PTID      TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               MOVE    1           TO  FLG-PTINF
               INITIALIZE              PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理検索処理
      *****************************************************************
       900-BUNBEN-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BUNBEN
           INITIALIZE                  BUNBEN-REC
      *
           MOVE    SNRRK-HOSPNUM   TO  BUNBEN-HOSPNUM
           MOVE    SNRRK-PTID      TO  BUNBEN-PTID
           MOVE    "1"             TO  BUNBEN-NYUGAIKBN
           MOVE    SNRRK-RRKNUM    TO  BUNBEN-RRKNUM
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BUNBEN-REC
           ELSE
               INITIALIZE              BUNBEN-REC
               MOVE    1           TO  FLG-BUNBEN
           END-IF
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-BUNBEN-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
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
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
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
       910-DB-SELECT-EXT.
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
      *****************************************************************
      *    ＤＢインサート処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
