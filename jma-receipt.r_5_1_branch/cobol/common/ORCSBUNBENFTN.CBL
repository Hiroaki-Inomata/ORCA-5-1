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
       PROGRAM-ID.             ORCSBUNBENFTN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 出産育児一時金テーブル編集サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-BUNBEN          PIC 9(01).
           03  FLG-BUNBENFTN       PIC 9(01).
           03  FLG-BUNBENHKN       PIC 9(01).
           03  FLG-LOOPEND         PIC 9(01).
           03  FLG-KYUSEIRRK       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
           03  IDX-BUNBEN          PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-HKNNUM          PIC X(03).
           03  WRK-HOJOKBN         PIC X(01).
           03  WRK-KYUSEINAME      PIC X(100).
           03  WRK-SRYYMD          PIC X(08).
           03  WRK-UPDNYUGAIKBN    PIC X(01).
           03  WRK-NYUGAIKBN       PIC X(01).
           03  WRK-SAGAKU          PIC S9(10).
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    収納退避領域
       01  TSYU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING //SYU-// BY //TSYU-//.
       01  WSYU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING //SYU-// BY //WSYU-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    旧姓履歴
       01  KYUSEIRRK-REC.
           COPY  "CPKYUSEIRRK.INC".
      *
      *    分娩管理
       01  BUNBEN-REC.
           COPY  "CPBUNBEN.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者保険
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
           COPY    "CPSK1046.INC".
      *
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    収納再計算
           COPY  "CPORCSSAIKEISAN.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    共通領域
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                         SECTION.
      *
       01  LNKBUNBEN-REC.
           COPY  "CPBUNBEN.INC"        REPLACING  //BUNBEN-//
                                              BY  //LNKBUNBEN-//.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                       DIVISION    USING
           LNKBUNBEN-REC
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           IF    ( LNKBUNBEN-NYUGAIKBN =   SPACE )
               MOVE    "1"             TO  LNKBUNBEN-NYUGAIKBN
           END-IF
      *
           PERFORM 900-BUNBEN-KEY3-SEL-SEC
      *
           IF    ( LNKBUNBEN-NYUGAIKBN =   "1" )
               PERFORM 900-PTNYUINRRK-KEY4-SEL-SEC
               MOVE    PTNYUINRRK-NYUINYMD TO  WRK-SRYYMD
               IF    ( PTNYUINRRK-TAIINYMD NOT =   "99999999" )
                   MOVE    PTNYUINRRK-TAIINYMD TO  WRK-SRYYMD
               END-IF
           ELSE
               PERFORM 900-SYUNOU-KEY-SEL-SEC
               MOVE    SYU-SRYYMD          TO  WRK-SRYYMD
           END-IF
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           MOVE    SPACE               TO  WRK-KYUSEINAME
           PERFORM 900-KYUSEIRRK-KEY3-SEL-SEC
           IF    ( FLG-KYUSEIRRK   =   ZERO )
               MOVE    KYUSEI-KANANAME
                                       TO  WRK-KYUSEINAME
           END-IF
      *
           PERFORM 900-PTNUM-KEY-SEL-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主取得
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
           MOVE    PTNUM-PTNUM         TO  LNKBUNBEN-PTNUM
           MOVE    "000000"            TO  LNKBUNBEN-SKYYM
      *
           IF    ( WRK-KYUSEINAME      =   SPACE )
               MOVE    PTINF-KANANAME  TO  LNKBUNBEN-NAME
           ELSE
               MOVE    WRK-KYUSEINAME  TO  LNKBUNBEN-NAME
           END-IF
      *
           PERFORM 2001-EDIT-HKNINF-SEC
      *
           IF    ( LNKBUNBEN-NYUGAIKBN =   "1" )
               PERFORM 2001-EDIT-NYUININF-SEC
      *
               PERFORM 2001-EDIT-SYUINF-SEC
           ELSE
      *
               PERFORM 2002-EDIT-GAIRAIINF-SEC
      *
               PERFORM 2002-EDIT-SYUINF-SEC
           END-IF
      *
           IF  ( BUNBEN-TEISYUTUSAKI   NOT =   SPACE )
               MOVE    BUNBEN-TEISYUTUSAKI
                                       TO  LNKBUNBEN-TEISYUTUSAKI
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院情報編集処理
      *****************************************************************
       2001-EDIT-NYUININF-SEC          SECTION.
      *
           MOVE    PTNYUINRRK-RRKNUM   TO  LNKBUNBEN-RRKNUM
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    PTNYUINRRK-NYUINYMD TO  LNK-DAY5-START
           MOVE    PTNYUINRRK-TAIINYMD TO  LNK-DAY5-END
           CALL    "ORCSDAY"               USING
                                           STS-AREA-DAY
                                           LNK-DAY5-AREA
      *
           MOVE    LNK-DAY5-NISSU2     TO  LNKBUNBEN-NYUIN-DAY
      *
           .
       2001-EDIT-NYUININF-EXT.
           EXIT.
      *****************************************************************
      *    収納情報編集処理（入院）
      *****************************************************************
       2001-EDIT-SYUINF-SEC            SECTION.
      *
           PERFORM 900-SYUNOU-KEY45-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
      *
               MOVE    SYU-SYUHKNNUM   TO  WRK-HKNNUM
               PERFORM 900-HKNCOMBI-KEY-SEL-SEC
               MOVE    COMB-HOJOKBN    TO  WRK-HOJOKBN
               PERFORM 2100-CHK-HOKEN-SEC
               IF    ( FLG-BUNBENFTN       =   1 )
                   PERFORM 20011-EDIT-SYUINF-SEC
               END-IF
      *
               PERFORM 900-SYUNOU-KEY45-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key45"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM VARYING IDX-BUNBEN  FROM    1   BY  1
                   UNTIL ( IDX-BUNBEN  >   10 )
      *
               COMPUTE LNKBUNBEN-NINPUFTN-MONEY
                   =   LNKBUNBEN-NINPUFTN-MONEY
                   +   LNKBUNBEN-MONEY (IDX-BUNBEN)
      *
           END-PERFORM
      *
           .
       2001-EDIT-SYUINF-EXT.
           EXIT.
      *****************************************************************
      *    収納情報編集処理
      *****************************************************************
       20011-EDIT-SYUINF-SEC           SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1046"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SYU-SKYEDYMD    TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1046-REC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   16 )
      *
               IF    ( SYS-1046-MEISAIKBN (IDX1)   IS  NUMERIC )
                   MOVE    SYS-1046-MEISAIKBN (IDX1)
                                   TO  IDX-BUNBEN
               ELSE
                   MOVE    09      TO  IDX-BUNBEN
               END-IF
      *
               COMPUTE LNKBUNBEN-MONEY (IDX-BUNBEN)
                   =   LNKBUNBEN-MONEY (IDX-BUNBEN)
                   +   SYU-TGMONEY  (IDX1)
                   +   SYU-TGMONEY-TAX (IDX1)
      *
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   10 )
      *
               IF    ( LNKBUNBEN-NYUGAIKBN =   "1" )
                   IF    ( SYS-1046-ETCJIHIKBN (IDX1)   IS  NUMERIC )
                       MOVE    SYS-1046-ETCJIHIKBN (IDX1)
                                   TO  IDX-BUNBEN
                   ELSE
                       MOVE    09  TO  IDX-BUNBEN
                   END-IF
               ELSE
                   IF    ( SYS-1046-GETCJIHIKBN (IDX1)  IS  NUMERIC )
                       MOVE    SYS-1046-GETCJIHIKBN (IDX1)
                                   TO  IDX-BUNBEN
                   ELSE
                       MOVE    09  TO  IDX-BUNBEN
                   END-IF
               END-IF
      *
               COMPUTE LNKBUNBEN-MONEY (IDX-BUNBEN)
                   =   LNKBUNBEN-MONEY (IDX-BUNBEN)
                   +   SYU-JIHI-TAXNASI (IDX1)
                   +   SYU-JIHI-TAXARI  (IDX1)
      *
           END-PERFORM
      *
           COMPUTE LNKBUNBEN-NYU-MONEY
               =   LNKBUNBEN-NYU-MONEY
               +   SYU-SKYMONEY-SKJ-JIHI-KEI
               +   SYU-SKYMONEY-LIFE-JIHI-KEI
      *
           COMPUTE LNKBUNBEN-RMSGK-MONEY
               =   LNKBUNBEN-RMSGK-MONEY
               +   SYU-RMSAGAKU
      *
           IF    ( SYU-SYUHKNNUM (1:2) =   "98" )
               COMPUTE LNKBUNBEN-ETC-MONEY
                   =   LNKBUNBEN-ETC-MONEY
                   +   SYU-HKNTEKMONEY
               COMPUTE LNKBUNBEN-NYU-MONEY
                   =   LNKBUNBEN-NYU-MONEY
                   +   SYU-SKYMONEY-SKJ-KEI
                   +   SYU-SKYMONEY-LIFE-KEI
           ELSE
               COMPUTE LNKBUNBEN-ICBFTN-MONEY
                   =   LNKBUNBEN-ICBFTN-MONEY
                   +   SYU-HKNTEKMONEY
                   +   SYU-KOH-FTN
                   +   SYU-SKYMONEY-SKJ-KEI
                   +   SYU-SKYMONEY-LIFE-KEI
           END-IF
      *
           .
       20011-EDIT-SYUINF-EXT.
           EXIT.
      *****************************************************************
      *    外来情報編集処理（外来）
      *****************************************************************
       2002-EDIT-GAIRAIINF-SEC         SECTION.
      *
           MOVE    1               TO  LNKBUNBEN-NYUIN-DAY
      *
           .
       2002-EDIT-GAIRAIINF-EXT.
           EXIT.
      *****************************************************************
      *    収納情報編集処理（外来）
      *****************************************************************
       2002-EDIT-SYUINF-SEC            SECTION.
      *
           PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
           IF    ( SYU-GRP-DENPNUM =   ZERO )
      *
               COMPUTE WRK-SAGAKU
                   =   WRK-SAGAKU  + ( SYU-SKYMONEY - SYU-CHOSEI )
      *
               MOVE    SYUNOU-REC  TO   TSYU-REC
               PERFORM 20021-SAIKEISAN-SEC
               PERFORM 901-SYUNOU-KEY-SEL-SEC
      *
               COMPUTE WRK-SAGAKU
                   =   WRK-SAGAKU  - ( SYU-SKYMONEY - SYU-CHOSEI )
      *
               PERFORM 20021-EDIT-SYUINF-SEC
      *
               MOVE    TSYU-REC    TO   MCPDATA-REC
               MOVE    "tbl_syunou" TO  MCP-TABLE
               MOVE    "key"        TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC       NOT =  ZERO )
                   MOVE    "tbl_syunou"    TO  MCP-TABLE
                   MOVE    "abort"         TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
               END-IF
           ELSE
      *
               MOVE    SYUNOU-REC  TO   TSYU-REC
      *
               PERFORM 900-SYUNOU-KEY74-SEL-SEC
      *
               PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
      *
                   COMPUTE WRK-SAGAKU
                       =   WRK-SAGAKU  + ( SYU-SKYMONEY - SYU-CHOSEI )
      *
                   MOVE    SYUNOU-REC  TO   WSYU-REC
                   MOVE    "4"         TO   WSYU-NYUGAIKBN
                   MOVE    WSYU-REC    TO   MCPDATA-REC
                   MOVE    "tbl_syunou" TO  MCP-TABLE
                   MOVE    "key"        TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC       NOT =  ZERO )
                       MOVE    "tbl_syunou"    TO  MCP-TABLE
                       MOVE    "abort"         TO  MCP-PATHNAME
                       PERFORM 910-DBSELECT-SEC
                   END-IF
      *
                   PERFORM 900-SYUNOU-KEY74-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_syunou"    TO  MCP-TABLE
               MOVE    "key74"         TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               MOVE    TSYU-REC        TO  SYUNOU-REC
               PERFORM 20021-SAIKEISAN-SEC
               PERFORM 20022-EDIT-SYUINF-SEC
      *
      *
               MOVE    TSYU-REC    TO   SYUNOU-REC
               MOVE    "4"         TO   SYU-NYUGAIKBN
               PERFORM 900-SYUNOU-KEY74-SEL-SEC
      *
               PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
      *
                   MOVE    "2"          TO  SYU-NYUGAIKBN
                   MOVE    SYUNOU-REC  TO   MCPDATA-REC
                   MOVE    "tbl_syunou" TO  MCP-TABLE
                   MOVE    "key"       TO  MCP-PATHNAME
                   PERFORM 910-DBDELETE-SEC
                   IF    ( MCP-RC       NOT =  ZERO )
                       MOVE    "tbl_syunou"    TO  MCP-TABLE
                       MOVE    "abort"         TO  MCP-PATHNAME
                       PERFORM 910-DBSELECT-SEC
                   END-IF
      *
                   MOVE    SYUNOU-REC  TO   WSYU-REC
                   MOVE    "2"         TO   WSYU-NYUGAIKBN
                   MOVE    "4"         TO   WSYU-SELNYUGAIKBN
                   MOVE    WSYU-REC    TO   MCPDATA-REC
                   MOVE    "tbl_syunou" TO  MCP-TABLE
                   MOVE    "upd3"       TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   IF    ( MCP-RC       NOT =  ZERO )
                       MOVE    "tbl_syunou"    TO  MCP-TABLE
                       MOVE    "abort"         TO  MCP-PATHNAME
                       PERFORM 910-DBSELECT-SEC
                   END-IF
      *
                   PERFORM 900-SYUNOU-KEY74-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_syunou"    TO  MCP-TABLE
               MOVE    "key74"         TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           END-IF
      *
           COMPUTE LNKBUNBEN-ETC-MONEY
               =   LNKBUNBEN-ETC-MONEY
               +   WRK-SAGAKU
      *
      *
           PERFORM VARYING IDX-BUNBEN  FROM    1   BY  1
                   UNTIL ( IDX-BUNBEN  >   10 )
      *
               COMPUTE LNKBUNBEN-NINPUFTN-MONEY
                   =   LNKBUNBEN-NINPUFTN-MONEY
                   +   LNKBUNBEN-MONEY (IDX-BUNBEN)
      *
           END-PERFORM
      *
           .
       2002-EDIT-SYUINF-EXT.
           EXIT.
      *****************************************************************
      *    収納情報編集処理（外来）
      *****************************************************************
       20021-EDIT-SYUINF-SEC           SECTION.
      *
           MOVE    SYU-SYUHKNNUM   TO  WRK-HKNNUM
           PERFORM 900-HKNCOMBI-KEY-SEL-SEC
           MOVE    COMB-HOJOKBN    TO  WRK-HOJOKBN
           PERFORM 2100-CHK-HOKEN-SEC
           IF    ( FLG-BUNBENFTN       =   1 )
               PERFORM 20011-EDIT-SYUINF-SEC
           END-IF
      *
           .
       20021-EDIT-SYUINF-EXT.
           EXIT.
      *****************************************************************
      *    収納情報編集処理（外来）
      *****************************************************************
       20022-EDIT-SYUINF-SEC           SECTION.
      *
           PERFORM 900-SYUNOU-KEY74-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
      *
               COMPUTE WRK-SAGAKU
                   =   WRK-SAGAKU  - ( SYU-SKYMONEY - SYU-CHOSEI )
      *
               PERFORM 20021-EDIT-SYUINF-SEC
      *
               PERFORM 900-SYUNOU-KEY74-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key74"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20022-EDIT-SYUINF-EXT.
           EXIT.
      *****************************************************************
      *    自費分再計算処理
      *****************************************************************
       20021-SAIKEISAN-SEC             SECTION.
      *
           INITIALIZE                   LNK-ORCSSAIKEISAN-REC
      *
           MOVE     SYU-HOSPNUM    TO   LNK-ORCSSKS-HOSPNUM
           MOVE     SYU-NYUGAIKBN  TO   LNK-ORCSSKS-NYUGAIKBN
           MOVE     SYU-PTID       TO   LNK-ORCSSKS-PTID
           MOVE     SYU-DENPNUM    TO   LNK-ORCSSKS-DENPNUM
           MOVE     SYU-SRYKA      TO   LNK-ORCSSKS-SRYKA
           MOVE     "1"            TO   LNK-ORCSSKS-BUNBENJIHIKBN
      *
           CALL    "ORCSSAIKEISAN"     USING
                                       SPA-AREA
                                       LNK-ORCSSAIKEISAN-REC
      *
           .
       20021-SAIKEISAN-EXT.
           EXIT.
      *****************************************************************
      *    保険情報編集処理
      *****************************************************************
       2001-EDIT-HKNINF-SEC            SECTION.
      *
           PERFORM 900-PTHKNINF-KEY8-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTHKNINF        NOT =   ZERO )
                    OR   ( LNKBUNBEN-KANYUKBN  NOT =   SPACE )
      *
               MOVE    PTHKN-HKNNUM        TO  WRK-HKNNUM
               MOVE    PTHKN-HOJOKBN       TO  WRK-HOJOKBN
               PERFORM 2100-CHK-HOKEN-SEC
               IF    ( FLG-BUNBENHKN       =   1 )
                   IF    ( PTHKN-HONKZKKBN =   "2" )
                       MOVE    "5"         TO  LNKBUNBEN-HONKZKKBN
                   ELSE
                       MOVE    "1"         TO  LNKBUNBEN-HONKZKKBN
                   END-IF
                   MOVE    PTHKN-HKNJANUM  TO  LNKBUNBEN-HKNJANUM
                   IF    ( PTHKN-HKNNUM    =   "067" )
                       MOVE    PTHKN-HKNJANUM (3:)
                                               TO  LNKBUNBEN-HKNJANUM-S
                   ELSE
                       MOVE    PTHKN-HKNJANUM  TO  LNKBUNBEN-HKNJANUM-S
                   END-IF
                   MOVE    PTHKN-KIGO      TO  LNKBUNBEN-KIGO
                   MOVE    PTHKN-NUM       TO  LNKBUNBEN-NUM
                   IF    ( PTHKN-HKNNUM    =   "060" OR "067"
                                            OR "068" OR "069" )
                       MOVE    "2"         TO  LNKBUNBEN-KANYUKBN
                   ELSE
                       MOVE    "1"         TO  LNKBUNBEN-KANYUKBN
                   END-IF
               END-IF
      *
               PERFORM 900-PTHKNINF-KEY8-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    IF    ( BUNBEN-RIYOUKBN         =   "1" )
      *        IF    ( BUNBEN-BUNBENKBN    =   "1" )
      *                MOVE    "2"     TO  LNKBUNBEN-TEISYUTUSAKI
      *        ELSE
      *            IF    ( LNKBUNBEN-KANYUKBN  =    "2" )
      *                MOVE    "2"     TO  LNKBUNBEN-TEISYUTUSAKI
      *            ELSE
      *                MOVE    "1"     TO  LNKBUNBEN-TEISYUTUSAKI
      *            END-IF
      *        END-IF
      *    END-IF
      *
           .
       2001-EDIT-HKNINF-EXT.
           EXIT.
      *****************************************************************
      *    保険チェック処理
      *****************************************************************
       2100-CHK-HOKEN-SEC              SECTION.
      *
           MOVE    1               TO  FLG-BUNBENFTN
                                       FLG-BUNBENHKN
      *
           EVALUATE    WRK-HKNNUM
           WHEN    "002"
               IF   (  WRK-HOJOKBN NOT =   SPACE )
                   MOVE    ZERO    TO  FLG-BUNBENFTN
                                       FLG-BUNBENHKN
               END-IF
           WHEN    "039"
           WHEN    "040"
           WHEN    "971"
           WHEN    "973"
           WHEN    "975"
               MOVE    ZERO        TO  FLG-BUNBENFTN
                                       FLG-BUNBENHKN
           WHEN    "980"   THRU    "989"
               MOVE    ZERO        TO  FLG-BUNBENHKN
           END-EVALUATE
      *
           .
       2100-CHK-HOKEN-EXT.
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
      *    収納マスター検索
      *****************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    LNKBUNBEN-HOSPNUM   TO  SYU-HOSPNUM
           MOVE    LNKBUNBEN-NYUGAIKBN TO  SYU-NYUGAIKBN
           MOVE    LNKBUNBEN-PTID      TO  SYU-PTID
           MOVE    LNKBUNBEN-RRKNUM    TO  SYU-DENPNUM
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター検索
      *****************************************************************
       901-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       901-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納マスター検索
      *****************************************************************
       900-SYUNOU-KEY45-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    LNKBUNBEN-HOSPNUM   TO  SYU-HOSPNUM
           MOVE    LNKBUNBEN-NYUGAIKBN TO  SYU-NYUGAIKBN
           MOVE    LNKBUNBEN-PTID      TO  SYU-PTID
           MOVE    LNKBUNBEN-RRKNUM    TO  SYU-NYUIN-RRKNUM
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key45"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY45-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       900-SYUNOU-KEY45-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
                                           FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key45"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  SYUNOU-REC
      *
                   MOVE    1               TO  FLG-LOOPEND
                   IF    ( SYU-DENPJTIKBN      =   "3" )
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
               ELSE
                   INITIALIZE                  SYUNOU-REC
                   MOVE    1               TO  FLG-SYUNOU
               END-IF
      *
           END-PERFORM
      *
           .
       900-SYUNOU-KEY45-FET-EXT.
           EXIT.
      *****************************************************************
      *    収納マスター検索
      *****************************************************************
       900-SYUNOU-KEY74-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key74"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY74-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       900-SYUNOU-KEY74-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key74"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY74-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスター検索
      *****************************************************************
       900-PTHKNINF-KEY8-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-PTHKNINF
      *
           INITIALIZE                      PTHKNINF-REC
           MOVE    LNKBUNBEN-HOSPNUM   TO  PTHKN-HOSPNUM
           MOVE    LNKBUNBEN-PTID      TO  PTHKN-PTID
           MOVE    WRK-SRYYMD          TO  PTHKN-TEKSTYMD
                                           PTHKN-TEKEDYMD
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-PTHKNINF-KEY8-FET-SEC
           END-IF
      *
           .
       900-PTHKNINF-KEY8-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスター読込
      *****************************************************************
       900-PTHKNINF-KEY8-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTHKNINF
                                           FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-PTHKNINF    NOT =   ZERO )
                    OR   ( FLG-LOOPEND     NOT =   ZERO )
      *
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  PTHKNINF-REC
                   MOVE    1               TO  FLG-LOOPEND
                   IF    ( PTHKN-SAKJOKBN  =   "1" )
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
               ELSE
                   INITIALIZE                  PTHKNINF-REC
                   MOVE    1               TO  FLG-PTHKNINF
               END-IF
      *
           END-PERFORM
      *
           .
       900-PTHKNINF-KEY8-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY4-SEL-SEC     SECTION.
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    LNKBUNBEN-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
           MOVE    LNKBUNBEN-PTID      TO  PTNYUINRRK-PTID
           MOVE    LNKBUNBEN-RRKNUM    TO  PTNYUINRRK-RRKNUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
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
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報読み込み処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    LNKBUNBEN-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNKBUNBEN-PTID      TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               INITIALIZE              PTINF-REC
               MOVE    1           TO  FLG-PTINF
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
      *    旧姓履歴検索処理
      *****************************************************************
       900-KYUSEIRRK-KEY3-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-KYUSEIRRK
      *
           INITIALIZE                      KYUSEIRRK-REC
           MOVE    LNKBUNBEN-HOSPNUM   TO  KYUSEI-HOSPNUM
           MOVE    LNKBUNBEN-PTID      TO  KYUSEI-PTID
           MOVE    WRK-SRYYMD          TO  KYUSEI-CHGYMD
           MOVE    KYUSEIRRK-REC       TO  MCPDATA-REC
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  KYUSEIRRK-REC
           ELSE
               INITIALIZE                  KYUSEIRRK-REC
               MOVE    1               TO  FLG-KYUSEIRRK
           END-IF
      *
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-KYUSEIRRK-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SYU-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SYU-PTID            TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNCOMBI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者番号情報読み込み処理
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNUM
      *
           INITIALIZE                      PTNUM-REC
           MOVE    LNKBUNBEN-HOSPNUM   TO  PTNUM-HOSPNUM
           MOVE    LNKBUNBEN-PTID      TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNUM-REC
           ELSE
               INITIALIZE              PTNUM-REC
               MOVE    1           TO  FLG-PTNUM
           END-IF
      *
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理検索処理
      *****************************************************************
       900-BUNBEN-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BUNBEN
      *
           MOVE    LNKBUNBEN-REC   TO  MCPDATA-REC
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
       910-DBSELECT-EXT.
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
