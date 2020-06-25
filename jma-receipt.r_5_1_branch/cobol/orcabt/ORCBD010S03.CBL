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
       PROGRAM-ID.             ORCBD010S03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次・月次統計
      *  コンポーネント名  : 収納明細編集サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/06/20    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYUMEI                  PIC 9(01).
           03  FLG-SYURRK                  PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-TCOMB                   PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX-MAX                     PIC 9(05).
           03  IDX-JIHI                    PIC 9(05).
           03  IDX-HOHO                    PIC 9(05).
           03  IDX-TCOMB                   PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYKA                   PIC  X(02).
           03  WRK-HKNCOMBI                PIC  X(04).
           03  WRK-HKNNUM                  PIC  X(03).
           03  WRK-HONKZKKBN               PIC  X(01).
           03  WRK-KOHNUM                  PIC  X(03).
           03  WRK-HKNSEIKYU               PIC S9(10).
           03  WRK-JIHSEIKYU               PIC S9(10).
           03  WRK-HKNGENMEN               PIC S9(10).
           03  WRK-JIHGENMEN               PIC S9(10).
           03  WRK-HKNCHOSEI               PIC S9(10).
           03  WRK-JIHCHOSEI               PIC S9(10).
           03  WRK-SYUEDANUM               PIC  9(02).
           03  WRK-SAGAKU                  PIC S9(10).
           03  WRK-HOHO                    PIC  X(02).
           03  WRK-CHOSEI                  PIC S9(10).
           03  WRK-SHO                     PIC S9(10).
           03  WRK-JIHHASU                 PIC S9(10).
           03  WRK-HKNHASU                 PIC S9(10).
      *
       01  TSKY-AREA.
           03  TSKY-OCC                    OCCURS   2.
               05  TSKY-HKNSEIKYU          PIC S9(10).
               05  TSKY-JIHSEIKYU          PIC S9(10).
               05  TSKY-HKNSKYAF           PIC S9(10).
               05  TSKY-JIHSKYAF           PIC S9(10).
               05  TSKY-GENMEN             PIC S9(10).
               05  TSKY-HKNGENMEN          PIC S9(10).
               05  TSKY-JIHGENMEN          PIC S9(10).
               05  TSKY-CHOSEI             PIC S9(10).
               05  TSKY-CHOSEI1            PIC S9(10).
               05  TSKY-CHOSEI2            PIC S9(10).
               05  TSKY-HKNCHOSEI          PIC S9(10).
               05  TSKY-HKNCHOSEI1         PIC S9(10).
               05  TSKY-HKNCHOSEI2         PIC S9(10).
               05  TSKY-JIHCHOSEI          PIC S9(10).
               05  TSKY-JIHCHOSEI1         PIC S9(10).
               05  TSKY-JIHCHOSEI2         PIC S9(10).
      *
       01  TNYUKIN-AREA.
           03  TNYUKIN-OCC                 OCCURS   2.
               05  TNYUKIN-NYUKIN          PIC S9(10).
               05  TNYUKIN-HKNNYUKIN       PIC S9(10).
               05  TNYUKIN-JIHNYUKIN       PIC S9(10).
      *
       01  TSRRK-AREA.
           03  TSRRK-HKNTEN                PIC S9(10).
           03  TSRRK-SEIKYU                PIC S9(10).
           03  TSRRK-CHOSEI                PIC S9(10).
           03  TSRRK-CHOSEI1               PIC S9(10).
           03  TSRRK-CHOSEI2               PIC S9(10).
           03  TSRRK-GENMEN                PIC S9(10).
           03  TSRRK-HKNSEIKYU             PIC S9(10).
           03  TSRRK-JIHSEIKYU             PIC S9(10).
           03  TSRRK-HKNGENMEN             PIC S9(10).
           03  TSRRK-JIHGENMEN             PIC S9(10).
           03  TSRRK-HKNCHOSEI             PIC S9(10).
           03  TSRRK-HKNCHOSEI1            PIC S9(10).
           03  TSRRK-HKNCHOSEI2            PIC S9(10).
           03  TSRRK-JIHCHOSEI             PIC S9(10).
           03  TSRRK-JIHCHOSEI1            PIC S9(10).
           03  TSRRK-JIHCHOSEI2            PIC S9(10).
           03  TSRRK-ETCJIHI               PIC S9(10)
                                           OCCURS  10.
           03  TSRRK-SYURRKNUM             PIC  9(02).
           03  TSRRK-SYUEDANUM             PIC  9(02).
      *
       01  THOHO-AREA.
           03  THOHO-MAX                   PIC 9(05).
           03  THOHO-OCC                   OCCURS  100.
               05  THOHO-HOHO              PIC X(02).
               05  THOHO-HOHOMONEY         PIC S9(10).
      *
      *    保険組合せ
       01  TCOMB-AREA.
           02  TCOMB-MAX                   PIC 9(05).
           02  TCOMB-OCC                   OCCURS      100.
           COPY    "CPHKNCOMBI.INC"        REPLACING   //COMB-//
                                           BY          //TCOMB-//.
      *
       01  CONST-AREA.
           03  CONST-H190401           PIC X(08)   VALUE   "20070401".
           03  CONST-TCOMB-MAX         PIC 9(05)   VALUE   100.
           03  CONST-THOHO-MAX         PIC 9(05)   VALUE   100.
           03  CONST-ETCJIHI-MAX       PIC 9(05)   VALUE   10.
           03  CONST-HOHO-HENKIN       PIC X(02) VALUE "@1".
           03  CONST-HOHO-TORIKESI     PIC X(02) VALUE "@2".
           03  CONST-TEISEI            PIC X(01) VALUE "8".
           03  CONST-HENKIN            PIC X(01) VALUE "4".
           03  CONST-TORIKESI          PIC X(02) VALUE "6".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    収納明細マスタ
       01  SMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    収納履歴マスタ
       01  SYURRK-REC.
           COPY    "CPSYURRK.INC".
      *
      *    保険組合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
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
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
           COPY    "CPORCBD010S.INC".
           COPY    "CPORCBD010S03.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
                                                SYUNOU-REC
                                                SBD010-AREA
                                                SBD010S03-AREA
                                                SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC.
      *
           PERFORM 200-MAIN-SEC.
      *
           PERFORM 300-END-SEC.
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
                                       TCOMB-AREA
                                       SBD010S03-AREA
                                       TSKY-AREA
                                       TNYUKIN-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 900-SYUMEI-KEY6-SEL-SEC
      *
           INITIALIZE                  THOHO-AREA
           MOVE    SYU-SRYKA           TO  WRK-SRYKA
           MOVE    SYU-HKNCOMBINUM     TO  WRK-HKNCOMBI
           MOVE    SYU-SYUHKNNUM       TO  WRK-HKNNUM
           MOVE    SYU-KOH-HKNNUM (1)  TO  WRK-KOHNUM
           PERFORM 900-HKNCOMBI-KEY-SEL-SEC
           MOVE    COMB-HONKZKKBN      TO  WRK-HONKZKKBN
      *
           PERFORM UNTIL ( FLG-SYUMEI  NOT =   ZERO )
      *
               PERFORM 2001-EDIT-SMEI-SEC
      *
               PERFORM 900-SYUMEI-KEY6-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    収納明細編集処理
      *****************************************************************
       2001-EDIT-SMEI-SEC          SECTION.
      *
           COMPUTE IDX-MAX =   IDX-MAX +   1
      *
           INITIALIZE              TSRRK-AREA
      *
           MOVE    TNYUKIN-OCC (1) TO  TNYUKIN-OCC (2)
           MOVE    ZERO            TO  TNYUKIN-HKNNYUKIN (1)
                                       TNYUKIN-JIHNYUKIN (1)
      *
           IF    ( SMEI-SYURRKNUM  =   ZERO )
               CONTINUE
           ELSE
               IF    ( SMEI-SYURRKUPDKBN =   "1" )
                   MOVE    1           TO  WRK-SYUEDANUM
                   PERFORM 2002-EDIT-SYURRK-SEC
      *
                   PERFORM 2001-EDIT-S03COMMON-SEC
      *
                   COMPUTE SBD010S03-NYUKIN (IDX-MAX)
                       =   TNYUKIN-NYUKIN (2)
                       *   -1
      *
                   COMPUTE SBD010S03-HKNNYUKIN (IDX-MAX)
                       =   TNYUKIN-HKNNYUKIN (2)
                       *   -1
      *
                   COMPUTE SBD010S03-JIHNYUKIN (IDX-MAX)
                       =   TNYUKIN-JIHNYUKIN (2)
                       *   -1
      *
                   MOVE    THOHO-MAX   TO  SBD010S03-HOHO-MAX (IDX-MAX)
                   PERFORM VARYING IDX-HOHO    FROM    1   BY  1
                           UNTIL ( IDX-HOHO    >   THOHO-MAX )
      *
                       MOVE    THOHO-HOHO (IDX-HOHO)
                                       TO  SBD010S03-HOHO
                                                   (IDX-MAX IDX-HOHO)
      *
                       COMPUTE SBD010S03-HOHOMONEY (IDX-MAX IDX-HOHO)
                           =   THOHO-HOHOMONEY (IDX-HOHO)
                           *   -1
      *
                   END-PERFORM
      *
                   COMPUTE IDX-MAX =   IDX-MAX     +   1
      *
                   MOVE    THOHO-MAX   TO  SBD010S03-HOHO-MAX (IDX-MAX)
                   PERFORM VARYING IDX-HOHO    FROM    1   BY  1
                           UNTIL ( IDX-HOHO    >   THOHO-MAX )
      *
                       MOVE    THOHO-HOHO (IDX-HOHO)
                                       TO  SBD010S03-HOHO
                                                   (IDX-MAX IDX-HOHO)
      *
                       COMPUTE SBD010S03-HOHOMONEY (IDX-MAX IDX-HOHO)
                           =   THOHO-HOHOMONEY (IDX-HOHO)
      *
                   END-PERFORM
      *
                   COMPUTE SBD010S03-NYUKIN (IDX-MAX)
                       =   TNYUKIN-NYUKIN (1)
                   INITIALIZE                  TNYUKIN-OCC (2)
                   MOVE    ZERO            TO  TNYUKIN-HKNNYUKIN (1)
                                               TNYUKIN-JIHNYUKIN (1)
      *
               END-IF
      *
               MOVE    SMEI-SYUEDANUM  TO  WRK-SYUEDANUM
               PERFORM 2001-EDIT-SYURRK-SEC
      *
           END-IF
      *
           PERFORM 2001-EDIT-S03COMMON-SEC
      *
           COMPUTE TNYUKIN-NYUKIN (1)
               =   TNYUKIN-NYUKIN (1)
               +   SMEI-NYUHEN-MONEY
      *
           COMPUTE SBD010S03-NYUKIN (IDX-MAX)
               =   SBD010S03-NYUKIN (IDX-MAX)
               +   SMEI-NYUHEN-MONEY
      *
           IF    ( SMEI-NYUHEN-MONEY   NOT =   ZERO )
               PERFORM 2001-EDIT-S03HOHO-SEC
           END-IF
      *
           IF    ( WRK-HKNNUM (1:2)    =   "98" OR "90" OR "91" )
      *
               COMPUTE TNYUKIN-JIHNYUKIN (1)
                   =   TNYUKIN-NYUKIN (1)
      *
           ELSE
      *
               COMPUTE WRK-SAGAKU
                   =   TNYUKIN-NYUKIN (1)
                   -   TSKY-HKNSKYAF (1)
                   -   TSKY-JIHSKYAF (1)
      *
               IF    ( WRK-SAGAKU      >=  ZERO )
                   COMPUTE TNYUKIN-HKNNYUKIN (1)
                       =   TSKY-HKNSKYAF (1)
                       +   WRK-SAGAKU
                   COMPUTE TNYUKIN-JIHNYUKIN (1)
                       =   TSKY-JIHSKYAF (1)
               ELSE
                   COMPUTE WRK-SAGAKU
                       =   TNYUKIN-NYUKIN (1)
                       -   TSKY-HKNSKYAF (1)
                   IF    ( WRK-SAGAKU  >=  ZERO )
                       COMPUTE TNYUKIN-HKNNYUKIN (1)
                           =   TSKY-HKNSKYAF (1)
                       COMPUTE TNYUKIN-JIHNYUKIN (1)
                           =   WRK-SAGAKU
                   ELSE
                       COMPUTE TNYUKIN-HKNNYUKIN (1)
                           =   TNYUKIN-NYUKIN (1)
                   END-IF
               END-IF
      *
           END-IF
      *
           COMPUTE SBD010S03-HKNNYUKIN (IDX-MAX)
               =   TNYUKIN-HKNNYUKIN (1)
               -   TNYUKIN-HKNNYUKIN (2)
      *
           COMPUTE SBD010S03-JIHNYUKIN (IDX-MAX)
               =   TNYUKIN-JIHNYUKIN (1)
               -   TNYUKIN-JIHNYUKIN (2)
      *
           .
       2001-EDIT-SMEI-EXT.
           EXIT.
      *****************************************************************
      *    収納履歴編集処理
      *****************************************************************
       2001-EDIT-SYURRK-SEC        SECTION.
      *
           INITIALIZE              TSRRK-AREA
      *
           PERFORM 900-SYURRK-KEY-SEL-SEC
      *
           MOVE    TSKY-OCC (1)    TO  TSKY-OCC (2)
           MOVE    ZERO            TO  TSKY-HKNGENMEN (1)
                                       TSKY-JIHGENMEN (1)
                                       TSKY-HKNCHOSEI (1)
                                       TSKY-HKNCHOSEI1(1)
                                       TSKY-HKNCHOSEI2(1)
                                       TSKY-JIHCHOSEI (1)
                                       TSKY-JIHCHOSEI1(1)
                                       TSKY-JIHCHOSEI2(1)
      *
      *
           MOVE    SYURRK-SRYKA    TO  WRK-SRYKA
           MOVE    SYURRK-HKNCOMBINUM
                                   TO  WRK-HKNCOMBI
           PERFORM 900-HKNCOMBI-KEY-SEL-SEC
           MOVE    COMB-HKNNUM     TO  WRK-HKNNUM
           MOVE    COMB-KOHHKNNUM (1)
                                   TO  WRK-KOHNUM
           MOVE    COMB-HONKZKKBN  TO  WRK-HONKZKKBN
      *
           MOVE    SYURRK-SYURRKNUM
                                   TO  TSRRK-SYURRKNUM
           MOVE    SYURRK-SYUEDANUM
                                   TO  TSRRK-SYUEDANUM
      *
           IF    ( WRK-HKNNUM (1:2)    =   "98" OR "90" OR "91" )
      *
               COMPUTE TSRRK-JIHSEIKYU
                   =   SYURRK-HKNTEKMONEY
                   +   SYURRK-RJN-FTN
                   +   SYURRK-KOH-FTN
                   +   SYURRK-RSI-TOTAL
      *
                   +   SYURRK-SKYMONEY-SKJ-KEI
                   +   SYURRK-SKYMONEY-LIFE-KEI
      *
                   +   SYURRK-RMSAGAKU
                   +   SYURRK-SKYMONEY-SKJ-JIHI-KEI
                   +   SYURRK-SKYMONEY-LIFE-JIHI-KEI
                   +   SYURRK-TOTAL-TGMONEY
                   +   SYURRK-TOTAL-TGMONEY-TAX
                   +   SYURRK-JIHI-TOTAL
                   +   SYURRK-JIHI-TOTAL-TAX
      *
               IF    ( SYU-SRYYMD  <   CONST-H190401 )
                   COMPUTE TSRRK-JIHSEIKYU
                       =   TSRRK-JIHSEIKYU
                       +   SYURRK-JIHI-TAX
               END-IF
           ELSE
               COMPUTE TSRRK-HKNTEN    =   SYURRK-TOTAL-HKNTEN
      *
               COMPUTE TSRRK-HKNSEIKYU =   SYURRK-HKNTEKMONEY
                                       +   SYURRK-RJN-FTN
                                       +   SYURRK-KOH-FTN
                                       +   SYURRK-RSI-TOTAL
                                       +   SYURRK-SKYMONEY-SKJ-KEI
                                       +   SYURRK-SKYMONEY-LIFE-KEI
      *
               COMPUTE TSRRK-JIHSEIKYU = SYURRK-RMSAGAKU
                                       +   SYURRK-SKYMONEY-SKJ-JIHI-KEI
                                       +   SYURRK-SKYMONEY-LIFE-JIHI-KEI
                                       +   SYURRK-TOTAL-TGMONEY
                                       +   SYURRK-TOTAL-TGMONEY-TAX
                                       +   SYURRK-JIHI-TOTAL
                                       +   SYURRK-JIHI-TOTAL-TAX
               IF    ( SYU-SRYYMD  <   CONST-H190401 )
                   COMPUTE TSRRK-JIHSEIKYU   =   TSRRK-JIHSEIKYU
                                             +   SYURRK-JIHI-TAX
               END-IF
           END-IF
      *
           COMPUTE TSRRK-SEIKYU
               =   SYURRK-SKYMONEY
      *
           PERFORM VARYING IDX-JIHI    FROM    1   BY  1
                   UNTIL ( IDX-JIHI    >   CONST-ETCJIHI-MAX )
               COMPUTE TSRRK-ETCJIHI (IDX-JIHI)
                   =   SYURRK-JIHI-TAXNASI (IDX-JIHI)
                   +   SYURRK-JIHI-TAXARI  (IDX-JIHI)
           END-PERFORM
      *
           COMPUTE TSRRK-CHOSEI
               =   SYURRK-CHOSEI
      *
           COMPUTE TSRRK-CHOSEI1
               =   SYURRK-CHOSEI1
      *
           COMPUTE TSRRK-CHOSEI2
               =   SYURRK-CHOSEI2
      *
           COMPUTE TSRRK-GENMEN
               =   SYURRK-DISCOUNT-MONEY * -1
      *
           COMPUTE WRK-SAGAKU
               =   TSRRK-SEIKYU
               - ( TSRRK-HKNSEIKYU
               +   TSRRK-JIHSEIKYU
               +   TSRRK-CHOSEI
               +   TSRRK-GENMEN )
      *
           DIVIDE  TSRRK-HKNSEIKYU    BY    10
                   GIVING  WRK-SHO    REMAINDER    WRK-HKNHASU
           DIVIDE  TSRRK-JIHSEIKYU    BY    10
                   GIVING  WRK-SHO    REMAINDER    WRK-JIHHASU
      *
           IF    ( WRK-SAGAKU              NOT =   ZERO )
      *
               IF    ( WRK-HKNHASU             =   ZERO )
                AND  ( WRK-JIHHASU         NOT =   ZERO )
                   COMPUTE TSRRK-JIHSEIKYU
                       =   TSRRK-JIHSEIKYU
                       +   WRK-SAGAKU
                   MOVE    ZERO            TO  WRK-SAGAKU
               END-IF
      *
               IF    ( WRK-HKNHASU         NOT =   ZERO )
                AND  ( WRK-JIHHASU             =   ZERO )
                   COMPUTE TSRRK-HKNSEIKYU
                       =   TSRRK-HKNSEIKYU
                       +   WRK-SAGAKU
                   MOVE    ZERO            TO  WRK-SAGAKU
               END-IF
      *
           END-IF
      *
           IF    ( WRK-SAGAKU              NOT =   ZERO )
               EVALUATE    TRUE
               WHEN    ( TSRRK-GENMEN      NOT =   ZERO )
                   COMPUTE TSRRK-GENMEN
                       =   TSRRK-GENMEN
                       +   WRK-SAGAKU
               WHEN    ( TSRRK-CHOSEI1     NOT =   ZERO )
                   COMPUTE TSRRK-CHOSEI1
                       =   TSRRK-CHOSEI1
                       +   WRK-SAGAKU
                   COMPUTE TSRRK-CHOSEI
                       =   TSRRK-CHOSEI
                       +   WRK-SAGAKU
               WHEN    ( TSRRK-CHOSEI2     NOT =   ZERO )
                   COMPUTE TSRRK-CHOSEI2
                       =   TSRRK-CHOSEI2
                       +   WRK-SAGAKU
                   COMPUTE TSRRK-CHOSEI
                       =   TSRRK-CHOSEI
                       +   WRK-SAGAKU
               WHEN    ( TSRRK-JIHSEIKYU   NOT =   ZERO )
                   COMPUTE TSRRK-JIHSEIKYU
                       =   TSRRK-JIHSEIKYU
                       +   WRK-SAGAKU
               WHEN    ( TSRRK-HKNSEIKYU   NOT =   ZERO )
                   COMPUTE TSRRK-HKNSEIKYU
                       =   TSRRK-HKNSEIKYU
                       +   WRK-SAGAKU
               WHEN    ( TSKY-GENMEN (1)   NOT =   ZERO )
                   COMPUTE TSRRK-GENMEN
                       =   TSRRK-GENMEN
                       +   WRK-SAGAKU
               WHEN    ( TSKY-CHOSEI1 (1)  NOT =   ZERO )
                   COMPUTE TSRRK-CHOSEI1
                       =   TSRRK-CHOSEI1
                       +   WRK-SAGAKU
                   COMPUTE TSRRK-CHOSEI
                       =   TSRRK-CHOSEI
                       +   WRK-SAGAKU
               WHEN    ( TSKY-CHOSEI2 (1)  NOT =   ZERO )
                   COMPUTE TSRRK-CHOSEI2
                       =   TSRRK-CHOSEI2
                       +   WRK-SAGAKU
                   COMPUTE TSRRK-CHOSEI
                       =   TSRRK-CHOSEI
                       +   WRK-SAGAKU
               WHEN    ( TSKY-JIHSEIKYU (1)  NOT   =   ZERO )
                   COMPUTE TSRRK-JIHSEIKYU
                       =   TSRRK-JIHSEIKYU
                       +   WRK-SAGAKU
               WHEN    OTHER
                   COMPUTE TSRRK-HKNSEIKYU
                       =   TSRRK-HKNSEIKYU
                       +   WRK-SAGAKU
               END-EVALUATE
           END-IF
      *
           COMPUTE TSKY-CHOSEI (1)
               =   TSKY-CHOSEI (1)
               +   TSRRK-CHOSEI
      *
           COMPUTE TSKY-CHOSEI1 (1)
               =   TSKY-CHOSEI1 (1)
               +   TSRRK-CHOSEI1
      *
           COMPUTE TSKY-CHOSEI2 (1)
               =   TSKY-CHOSEI2 (1)
               +   TSRRK-CHOSEI2
      *
           COMPUTE TSKY-GENMEN (1)
               =   TSKY-GENMEN (1)
               +   TSRRK-GENMEN
      *
           COMPUTE TSKY-HKNSEIKYU (1)
               =   TSKY-HKNSEIKYU (1)
               +   TSRRK-HKNSEIKYU
      *
           COMPUTE TSKY-JIHSEIKYU (1)
               =   TSKY-JIHSEIKYU (1)
               +   TSRRK-JIHSEIKYU
      *
           IF    ( WRK-HKNNUM (1:2)    =   "98" OR "90" OR "91" )
               COMPUTE TSKY-JIHGENMEN (1)
                   =   TSKY-JIHGENMEN (1)
                   +   TSKY-GENMEN    (1)
               COMPUTE TSKY-JIHCHOSEI (1)
                   =   TSKY-JIHCHOSEI (1)
                   +   TSKY-CHOSEI    (1)
               COMPUTE TSKY-JIHCHOSEI1(1)
                   =   TSKY-JIHCHOSEI1(1)
                   +   TSKY-CHOSEI1   (1)
               COMPUTE TSKY-JIHCHOSEI2(1)
                   =   TSKY-JIHCHOSEI2(1)
                   +   TSKY-CHOSEI2   (1)
           ELSE
      *
               COMPUTE WRK-SAGAKU
                   =   TSKY-HKNSEIKYU (1)
                   +   TSKY-GENMEN (1)
               IF    ( WRK-SAGAKU      >=  ZERO )
                   COMPUTE TSKY-HKNGENMEN (1)
                       =   TSKY-GENMEN (1)
               ELSE
                   COMPUTE TSKY-HKNGENMEN (1)
                       =   TSKY-HKNSEIKYU (1)
                       *   -1
                   COMPUTE TSKY-JIHGENMEN (1)
                       =   WRK-SAGAKU
               END-IF
      *
               COMPUTE WRK-HKNSEIKYU
                   =   TSKY-HKNSEIKYU (1)
                   +   TSKY-HKNGENMEN (1)
      *
               COMPUTE WRK-JIHSEIKYU
                   =   TSKY-JIHSEIKYU (1)
                   +   TSKY-JIHGENMEN (1)
      *
               COMPUTE WRK-JIHGENMEN
                   =   TSKY-JIHGENMEN (1)
      *
               COMPUTE WRK-CHOSEI
                   =   TSKY-CHOSEI1 (1)
               PERFORM 2001-EDIT-CHOSEI-SEC
               COMPUTE TSKY-HKNCHOSEI1 (1)
                   =   WRK-HKNCHOSEI
               COMPUTE TSKY-JIHCHOSEI1 (1)
                   =   WRK-JIHCHOSEI
      *
               COMPUTE WRK-CHOSEI
                   =   TSKY-CHOSEI2 (1)
               PERFORM 2001-EDIT-CHOSEI-SEC
               COMPUTE TSKY-HKNCHOSEI2 (1)
                   =   WRK-HKNCHOSEI
               COMPUTE TSKY-JIHCHOSEI2 (1)
                   =   WRK-JIHCHOSEI
           END-IF
      *
           COMPUTE TSKY-HKNCHOSEI (1)
               =   TSKY-HKNCHOSEI1 (1)
               +   TSKY-HKNCHOSEI2 (1)
      *
           COMPUTE TSKY-JIHCHOSEI (1)
               =   TSKY-JIHCHOSEI1 (1)
               +   TSKY-JIHCHOSEI2 (1)
      *
           COMPUTE TSKY-HKNSKYAF  (1)
               =   TSKY-HKNSEIKYU (1)
               +   TSKY-HKNGENMEN (1)
               +   TSKY-HKNCHOSEI (1)
      *
           COMPUTE TSKY-JIHSKYAF  (1)
               =   TSKY-JIHSEIKYU (1)
               +   TSKY-JIHGENMEN (1)
               +   TSKY-JIHCHOSEI (1)
      *
           COMPUTE TSRRK-HKNGENMEN
               =   TSKY-HKNGENMEN (1)
               -   TSKY-HKNGENMEN (2)
      *
           COMPUTE TSRRK-JIHGENMEN
               =   TSKY-JIHGENMEN (1)
               -   TSKY-JIHGENMEN (2)
      *
           COMPUTE TSRRK-HKNCHOSEI
               =   TSKY-HKNCHOSEI (1)
               -   TSKY-HKNCHOSEI (2)
      *
           COMPUTE TSRRK-HKNCHOSEI1
               =   TSKY-HKNCHOSEI1 (1)
               -   TSKY-HKNCHOSEI1 (2)
      *
           COMPUTE TSRRK-HKNCHOSEI2
               =   TSKY-HKNCHOSEI2 (1)
               -   TSKY-HKNCHOSEI2 (2)
      *
           COMPUTE TSRRK-JIHCHOSEI
               =   TSKY-JIHCHOSEI (1)
               -   TSKY-JIHCHOSEI (2)
      *
           COMPUTE TSRRK-JIHCHOSEI1
               =   TSKY-JIHCHOSEI1 (1)
               -   TSKY-JIHCHOSEI1 (2)
      *
           COMPUTE TSRRK-JIHCHOSEI2
               =   TSKY-JIHCHOSEI2 (1)
               -   TSKY-JIHCHOSEI2 (2)
      *
           .
       2001-EDIT-SYURRK-EXT.
           EXIT.
      *****************************************************************
      *    調整金編集処理
      *****************************************************************
       2001-EDIT-CHOSEI-SEC        SECTION.
      *
           MOVE    ZERO            TO  WRK-HKNCHOSEI
                                       WRK-JIHCHOSEI
      *
           IF    ( WRK-CHOSEI     >=   ZERO )
               IF    ( WRK-JIHGENMEN   =   ZERO )
                   COMPUTE WRK-HKNCHOSEI
                       =   WRK-CHOSEI
               ELSE
                   COMPUTE WRK-SAGAKU
                       =   WRK-CHOSEI
                       +   WRK-JIHGENMEN
                   IF    ( WRK-SAGAKU >=   ZERO )
      *
                       COMPUTE WRK-JIHCHOSEI
                           =   WRK-JIHGENMEN
                           *   -1
      *
                       COMPUTE WRK-HKNCHOSEI
                           =   WRK-SAGAKU
      *
                       COMPUTE WRK-JIHGENMEN
                           =   0
      *
                   ELSE
                       COMPUTE WRK-JIHCHOSEI
                           =   WRK-CHOSEI
                       COMPUTE WRK-JIHGENMEN
                           =   WRK-SAGAKU
                   END-IF
               END-IF
           ELSE
               IF    ( WRK-HKNSEIKYU       >=  ZERO )
                   IF    ( WRK-HKNSEIKYU   >=  WRK-CHOSEI * -1 )
                       COMPUTE WRK-HKNCHOSEI
                           =   WRK-CHOSEI
                   ELSE
                       COMPUTE WRK-HKNCHOSEI
                           =   WRK-HKNSEIKYU
                           *   -1
                       COMPUTE WRK-JIHCHOSEI
                           =   WRK-HKNSEIKYU
                           +   WRK-CHOSEI
                       COMPUTE WRK-SAGAKU
                           =   WRK-JIHSEIKYU
                           +   WRK-JIHCHOSEI
                       IF    ( WRK-SAGAKU  <   ZERO )
                           COMPUTE WRK-JIHCHOSEI
                               =   WRK-JIHSEIKYU
                               *   -1
                           COMPUTE WRK-HKNCHOSEI
                               =   WRK-HKNCHOSEI
                               +   WRK-SAGAKU
                       END-IF
                       COMPUTE WRK-JIHGENMEN
                           =   WRK-JIHGENMEN
                           +   WRK-JIHCHOSEI
                   END-IF
               ELSE
                   COMPUTE WRK-HKNCHOSEI
                       =   WRK-CHOSEI
               END-IF
           END-IF
      *
           COMPUTE WRK-HKNSEIKYU
               =   WRK-HKNSEIKYU
               +   WRK-HKNCHOSEI
      *
           COMPUTE WRK-JIHSEIKYU
               =   WRK-JIHSEIKYU
               +   WRK-JIHCHOSEI
      *
           .
       2001-EDIT-CHOSEI-EXT.
           EXIT.
      *****************************************************************
      *    収納履歴編集処理
      *****************************************************************
       2002-EDIT-SYURRK-SEC        SECTION.
      *
           PERFORM 2001-EDIT-SYURRK-SEC
      *
           COMPUTE TSRRK-HKNGENMEN
               =   TSKY-HKNGENMEN (2)
               *   -1
      *
           COMPUTE TSRRK-JIHGENMEN
               =   TSKY-JIHGENMEN (2)
               *   -1
      *
           COMPUTE TSRRK-HKNCHOSEI
               =   TSKY-HKNCHOSEI (2)
               *   -1
      *
           COMPUTE TSRRK-HKNCHOSEI1
               =   TSKY-HKNCHOSEI1 (2)
               *   -1
      *
           COMPUTE TSRRK-HKNCHOSEI2
               =   TSKY-HKNCHOSEI2 (2)
               *   -1
      *
           COMPUTE TSRRK-JIHCHOSEI
               =   TSKY-JIHCHOSEI (2)
               *   -1
      *
           COMPUTE TSRRK-JIHCHOSEI1
               =   TSKY-JIHCHOSEI1 (2)
               *   -1
      *
           COMPUTE TSRRK-JIHCHOSEI2
               =   TSKY-JIHCHOSEI2 (2)
               *   -1
      *
           INITIALIZE  TSKY-OCC (1)
      *
           .
       2002-EDIT-SYURRK-EXT.
           EXIT.
      *****************************************************************
      *    収納履歴編集処理
      *****************************************************************
       2001-EDIT-S03COMMON-SEC        SECTION.
      *
           MOVE    IDX-MAX             TO  SBD010S03-MAX
           MOVE    SMEI-DENPEDANUM     TO  SBD010S03-DENPEDANUM(IDX-MAX)
           MOVE    SMEI-JOUTAIKBN      TO  SBD010S03-JOUTAIKBN (IDX-MAX)
           MOVE    WRK-SRYKA           TO  SBD010S03-SRYKA     (IDX-MAX)
           MOVE    WRK-HKNCOMBI        TO  SBD010S03-HKNCOMBI  (IDX-MAX)
           MOVE    WRK-HKNNUM          TO  SBD010S03-HKNNUM    (IDX-MAX)
           MOVE    WRK-KOHNUM          TO  SBD010S03-KOHNUM    (IDX-MAX)
           MOVE    WRK-HONKZKKBN       TO  SBD010S03-HONKZKKBN (IDX-MAX)
           MOVE    SMEI-DAILYKEY       TO  SBD010S03-DAILYKEY  (IDX-MAX)
           MOVE    SMEI-SYURRKNUM      TO  SBD010S03-SYURRKNUM (IDX-MAX)
           MOVE    TSRRK-HKNTEN        TO  SBD010S03-HKNTEN    (IDX-MAX)
           MOVE    TSRRK-SEIKYU        TO  SBD010S03-SEIKYU    (IDX-MAX)
           MOVE    TSRRK-CHOSEI        TO  SBD010S03-CHOSEI    (IDX-MAX)
           MOVE    TSRRK-CHOSEI1       TO  SBD010S03-CHOSEI1   (IDX-MAX)
           MOVE    TSRRK-CHOSEI2       TO  SBD010S03-CHOSEI2   (IDX-MAX)
           MOVE    TSRRK-GENMEN        TO  SBD010S03-GENMEN    (IDX-MAX)
           MOVE    TSRRK-HKNSEIKYU     TO  SBD010S03-HKNSEIKYU (IDX-MAX)
           MOVE    TSRRK-JIHSEIKYU     TO  SBD010S03-JIHSEIKYU (IDX-MAX)
           MOVE    TSRRK-HKNGENMEN     TO  SBD010S03-HKNGENMEN (IDX-MAX)
           MOVE    TSRRK-JIHGENMEN     TO  SBD010S03-JIHGENMEN (IDX-MAX)
           MOVE    TSRRK-HKNCHOSEI     TO  SBD010S03-HKNCHOSEI (IDX-MAX)
           MOVE    TSRRK-JIHCHOSEI     TO  SBD010S03-JIHCHOSEI (IDX-MAX)
           MOVE    TSRRK-HKNCHOSEI1    TO  SBD010S03-HKNCHOSEI1(IDX-MAX)
           MOVE    TSRRK-JIHCHOSEI1    TO  SBD010S03-JIHCHOSEI1(IDX-MAX)
           MOVE    TSRRK-HKNCHOSEI2    TO  SBD010S03-HKNCHOSEI2(IDX-MAX)
           MOVE    TSRRK-JIHCHOSEI2    TO  SBD010S03-JIHCHOSEI2(IDX-MAX)
      *
           PERFORM VARYING IDX-JIHI    FROM    1   BY  1
                   UNTIL ( IDX-JIHI    >   CONST-ETCJIHI-MAX )
               MOVE   TSRRK-ETCJIHI (IDX-JIHI)
                                       TO  SBD010S03-ETCJIHI 
                                                      (IDX-MAX IDX-JIHI)
           END-PERFORM
      *
           MOVE    TSRRK-SYURRKNUM     TO  SBD010S03-SYURRKNUM (IDX-MAX)
           MOVE    TSRRK-SYUEDANUM     TO  SBD010S03-SYUEDANUM (IDX-MAX)
      *
           .
       2001-EDIT-S03COMMON-EXT.
           EXIT.
      *****************************************************************
      *    入金方法編集処理
      *****************************************************************
       2001-EDIT-S03HOHO-SEC           SECTION.
      *
           MOVE    SPACE                   TO  WRK-HOHO
           EVALUATE    SMEI-JOUTAIKBN
           WHEN    CONST-TEISEI
               IF    ( SMEI-NYUHEN-MONEY   <   ZERO )
                   MOVE    CONST-HOHO-HENKIN
                                           TO  WRK-HOHO
               ELSE
                   MOVE    SMEI-NYUKIN-HOHO
                                           TO  WRK-HOHO
               END-IF
           WHEN    CONST-HENKIN
               MOVE    CONST-HOHO-HENKIN
                                           TO  WRK-HOHO
           WHEN    CONST-TORIKESI
               IF    ( SMEI-NYUKIN-HOHO    NOT =   "00" )
                   MOVE    SMEI-NYUKIN-HOHO
                                           TO  WRK-HOHO
               ELSE
                   MOVE    CONST-HOHO-TORIKESI
                                           TO  WRK-HOHO
               END-IF
           WHEN    OTHER
               MOVE    SMEI-NYUKIN-HOHO    TO  WRK-HOHO
           END-EVALUATE
      *
           PERFORM VARYING IDX-HOHO    FROM    1   BY  1
                   UNTIL ( IDX-HOHO    >   CONST-THOHO-MAX )
      *
               IF    ( SBD010S03-HOHO (IDX-MAX IDX-HOHO)
                                       =   SPACE )
                   MOVE    IDX-HOHO    TO  SBD010S03-HOHO-MAX (IDX-MAX)
                   MOVE    WRK-HOHO    TO  SBD010S03-HOHO
                                                     (IDX-MAX IDX-HOHO)
                       COMPUTE SBD010S03-HOHOMONEY (IDX-MAX IDX-HOHO)
                           =   SBD010S03-HOHOMONEY (IDX-MAX IDX-HOHO)
                           +   SMEI-NYUHEN-MONEY
                       MOVE    CONST-THOHO-MAX TO  IDX-HOHO
               ELSE
                   IF    ( SBD010S03-HOHO (IDX-MAX IDX-HOHO)
                                       =    WRK-HOHO )
                       COMPUTE SBD010S03-HOHOMONEY (IDX-MAX IDX-HOHO)
                           =   SBD010S03-HOHOMONEY (IDX-MAX IDX-HOHO)
                           +   SMEI-NYUHEN-MONEY
                       MOVE    CONST-THOHO-MAX TO  IDX-HOHO
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX-HOHO    FROM    1   BY  1
                   UNTIL ( IDX-HOHO    >   CONST-THOHO-MAX )
      *
               IF    ( THOHO-HOHO (IDX-HOHO)
                                       =   SPACE )
                   MOVE    IDX-HOHO    TO  THOHO-MAX
                   MOVE    WRK-HOHO    TO  THOHO-HOHO (IDX-HOHO)
                       COMPUTE THOHO-HOHOMONEY (IDX-HOHO)
                           =   THOHO-HOHOMONEY (IDX-HOHO)
                           +   SMEI-NYUHEN-MONEY
                       MOVE    CONST-THOHO-MAX TO  IDX-HOHO
               ELSE
                   IF    ( THOHO-HOHO (IDX-HOHO)
                                       =    WRK-HOHO )
                       COMPUTE THOHO-HOHOMONEY (IDX-HOHO)
                           =   THOHO-HOHOMONEY (IDX-HOHO)
                           +   SMEI-NYUHEN-MONEY
                       MOVE    CONST-THOHO-MAX TO  IDX-HOHO
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       2001-EDIT-S03HOHO-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           DISPLAY
              ","
              ","  "DENPEDANUM"
              ","  "SRYKA"
              ","
              ","  "HKNCOMBI"
              ","  "HKNNUM"
              ","  "HONKZKKBN"
              ","  "KOHNUM"
              ","  "HKNTEN"
              ","  "SEIKYU"
              ","  "HKNSEIKYU"
              ","  "JIHSEIKYU"
              ","  "GENMEN"
              ","  "HKNGENMEN"
              ","  "JIHGENMEN"
              ","  "CHOSEI"
              ","  "CHOSEI1"
              ","  "CHOSEI2"
              ","  "HKNCHOSEI"
              ","  "JIHCHOSEI"
              ","  "NYUKIN"
              ","  "HKNNYUKIN"
              ","  "JIHNYUKIN"
              ","
              ","
              ","
              ","  "ETCJIHI(1)"
              ","  "ETCJIHI(2)"
              ","  "ETCJIHI(3)"
              ","  "ETCJIHI(4)"
              ","  "ETCJIHI(5)"
              ","  "ETCJIHI(6)"
              ","  "ETCJIHI(7)"
              ","  "ETCJIHI(8)"
              ","  "ETCJIHI(9)"
              ","  "ETCJIHI(10)"
              ","  "HOHO(1)"
              ","  "HOHOMONEY(1)"
              ","  "HOHO(2)"
              ","  "HOHOMONEY(2)"
              ","  "HOHO(3)"
              ","  "HOHOMONEY(3)"
              ","  "HOHO(4)"
              ","  "HOHOMONEY(4)"
              ","  "HOHO(5)"
              ","  "HOHOMONEY(5)"
              ","  "DAILYKEY"
              ","  "SYURRKNUM"
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SBD010S03-MAX )
      *
           DISPLAY 
              ","
              ","  SBD010S03-DENPEDANUM(IDX0)
              ","  SBD010S03-SRYKA(IDX0)
              ","
              ","  SBD010S03-HKNCOMBI(IDX0)
              ","  SBD010S03-HKNNUM(IDX0)
              ","  SBD010S03-HONKZKKBN(IDX0)
              ","  SBD010S03-KOHNUM(IDX0)
              ","  SBD010S03-HKNTEN(IDX0)
              ","  SBD010S03-SEIKYU(IDX0)
              ","  SBD010S03-HKNSEIKYU(IDX0)
              ","  SBD010S03-JIHSEIKYU(IDX0)
              ","  SBD010S03-GENMEN(IDX0)
              ","  SBD010S03-HKNGENMEN(IDX0)
              ","  SBD010S03-JIHGENMEN(IDX0)
              ","  SBD010S03-CHOSEI(IDX0)
              ","  SBD010S03-CHOSEI1(IDX0)
              ","  SBD010S03-CHOSEI2(IDX0)
              ","  SBD010S03-HKNCHOSEI(IDX0)
              ","  SBD010S03-JIHCHOSEI(IDX0)
              ","  SBD010S03-NYUKIN(IDX0)
              ","  SBD010S03-HKNNYUKIN(IDX0)
              ","  SBD010S03-JIHNYUKIN(IDX0)
              ","
              ","
              ","
              ","  SBD010S03-ETCJIHI(IDX0 1)
              ","  SBD010S03-ETCJIHI(IDX0 2)
              ","  SBD010S03-ETCJIHI(IDX0 3)
              ","  SBD010S03-ETCJIHI(IDX0 4)
              ","  SBD010S03-ETCJIHI(IDX0 5)
              ","  SBD010S03-ETCJIHI(IDX0 6)
              ","  SBD010S03-ETCJIHI(IDX0 7)
              ","  SBD010S03-ETCJIHI(IDX0 8)
              ","  SBD010S03-ETCJIHI(IDX0 9)
              ","  SBD010S03-ETCJIHI(IDX0 10)
              ","  SBD010S03-HOHO(IDX0 1)
              ","  SBD010S03-HOHOMONEY(IDX0 1)
              ","  SBD010S03-HOHO(IDX0 2)
              ","  SBD010S03-HOHOMONEY(IDX0 2)
              ","  SBD010S03-HOHO(IDX0 3)
              ","  SBD010S03-HOHOMONEY(IDX0 3)
              ","  SBD010S03-HOHO(IDX0 4)
              ","  SBD010S03-HOHOMONEY(IDX0 4)
              ","  SBD010S03-HOHO(IDX0 5)
              ","  SBD010S03-HOHOMONEY(IDX0 5)
              ","  SBD010S03-DAILYKEY(IDX0)
              ","  SBD010S03-SYURRKNUM(IDX0)
      *
           END-PERFORM
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    収納明細検索処理
      *****************************************************************
       900-SYUMEI-KEY6-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUMEI
      *
           INITIALIZE                  SMEI-REC
           MOVE    SYU-HOSPNUM     TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN   TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID        TO  SMEI-PTID
           MOVE    SYU-DENPNUM     TO  SMEI-DENPNUM
           MOVE    SMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SMEI-REC
           ELSE
               INITIALIZE              SMEI-REC
               MOVE    1           TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納明細FETCH処理
      *****************************************************************
       900-SYUMEI-KEY6-FET-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUMEI
      *
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SMEI-REC
           ELSE
               INITIALIZE              SMEI-REC
               MOVE    1           TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-KEY6-FET-EXT.
           EXIT.
      *****************************************************************
      *    収納履歴検索処理
      *****************************************************************
       900-SYURRK-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYURRK
      *
           INITIALIZE                  SYURRK-REC
           MOVE    SMEI-HOSPNUM    TO  SYURRK-HOSPNUM
           MOVE    SMEI-NYUGAIKBN  TO  SYURRK-NYUGAIKBN
           MOVE    SMEI-PTID       TO  SYURRK-PTID
           MOVE    SMEI-DENPNUM    TO  SYURRK-DENPNUM
           MOVE    SMEI-SYURRKNUM  TO  SYURRK-SYURRKNUM
           MOVE    WRK-SYUEDANUM   TO  SYURRK-SYUEDANUM
           MOVE    SYURRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_syurrk"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  SYURRK-REC
           ELSE
               INITIALIZE              SYURRK-REC
               MOVE    1           TO  FLG-SYURRK
           END-IF
      *
           MOVE    "tbl_syurrk"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYURRK-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                    TO  FLG-HKNCOMBI
      *
           PERFORM 9001-TCOMB-CHK-SEC
           IF   (  FLG-TCOMB    =       ZERO )
               INITIALIZE                      HKNCOMBI-REC
               MOVE    SYU-HOSPNUM         TO  COMB-HOSPNUM
               MOVE    SYU-PTID            TO  COMB-PTID
               MOVE    WRK-HKNCOMBI        TO  COMB-HKNCOMBINUM
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   IF    ( CONST-TCOMB-MAX >   TCOMB-MAX )
                       COMPUTE TCOMB-MAX   =   TCOMB-MAX   +   1
                       MOVE    HKNCOMBI-REC TO TCOMB-OCC
                                                  (TCOMB-MAX)
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
       9001-TCOMB-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TCOMB
      *
           PERFORM VARYING IDX-TCOMB    FROM    1   BY  1
                   UNTIL ( IDX-TCOMB    >   TCOMB-MAX )
                    OR   ( FLG-TCOMB    =   1 )
      *
               IF    ( TCOMB-HOSPNUM  (IDX-TCOMB)
                                           =   SYU-HOSPNUM )
                AND  ( TCOMB-PTID     (IDX-TCOMB)
                                           =   SYU-PTID )
                AND  ( TCOMB-HKNCOMBINUM (IDX-TCOMB)
                                           =   WRK-HKNCOMBI )
      *
                   MOVE    1           TO  FLG-TCOMB
      *
                   MOVE    TCOMB-OCC (IDX-TCOMB)
                                       TO  HKNCOMBI-REC
               END-IF
      *
           END-PERFORM
      *
           .
       9001-TCOMB-CHK-EXT.
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
      *    ＤＢ検索処理
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
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
       910-DBCLOSECURSOR-SEC                SECTION.
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
