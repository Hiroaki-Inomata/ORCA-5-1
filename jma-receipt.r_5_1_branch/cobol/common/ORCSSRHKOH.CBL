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
       PROGRAM-ID.                     ORCSSRHKOH.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 公費検索サブ
      *  管理者            :
      *  作成日付    作業者        記述
      *  06/06/20    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/05/28  グループ診療対応
      *  04.05.01    NACL-門脇    10/01/07  治験対応
      *  04.05.02    NACL-門脇    12/05/16  愛知・三重県地方公費対応
      *  04.07.01    NACL-門脇    14/11/04  平成２７年１月改正対応
      *                                     （０５４、０５２対応）
      *  04.07.02    NACL-門脇    15/01/26  平成２７年１月改正対応
      *                                     （都道府県独自難病対応）
      *  04.07.03    NACL-門脇    15/06/08  難病の実施機関番号追加対応（６０２）
      *
      *  04.08.01    NACL-門脇    15/10/21  現物給付対象外の低所得（食事）対応
      *  04.08.02    NACL-門脇    17/10/18  入院料と異なる保険組み合わせ
      *                                     での食事算定対応
      *  04.08.03    NACL-門脇    18/04/04  難病負担者番号追加対応
      *  05.00.01    NACL-門脇    20/03/26  小児慢性の実施機関番号追加対応
      *                                     （７１０・８１０）
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
       01  FLG-AREA.
           03  FLG-PTINF               PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-NACCT               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-END                 PIC 9(01).
           03  FLG-HKNHIT              PIC 9(01).
           03  FLG-KOH                 PIC 9(01).
           03  FLG-OK                  PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDX-KOH                 PIC 9(05).
           03  IDX-ZAI                 PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-JGNGET-AREA.
               04  WRK-JGNGET-OCC      OCCURS 4.
                 05  WRK-JGNGET-TEKIYOFLG
                                       PIC 9(01).
      *
           03  WRK-DAY-AREA.
               04  WRK-DAY-OCC         OCCURS 31.
                 05  WRK-DAY1          PIC 9(04).
                 05  WRK-DAY5X.
                   07  WRK-DAY5        PIC 9(04).
                 05  WRK-DAY-MI        PIC X(01).
           03  WRK-ZAISKBKBN           PIC X(01).
      *
       01  KEY-AREA.
           03  KEY-HKNNUM              PIC X(03).
           03  KEY-HKNJANUM            PIC X(08).
           03  KEY-HONKZKKBN           PIC X(01).
      *
       01  TKOH-AREA.
         02   TKOH-MAX                 PIC 9(05).
         02   TKOH-OCC                 OCCURS 4.
           COPY    "CPPTKOHINF.INC"
              REPLACING   //PTKOH-//   BY //TKOH-//.
      *
       01  TNCOMB-AREA.
         02   TNCOMB-MAX               PIC 9(05).
         02   TNCOMB-OCC               OCCURS      31.
           03 TNCOMB-HKNCOMBI          PIC X(04).
      *
       01  CONST-AREA.
           03  CONST-KOH-MAX           PIC 9(05)   VALUE 4.
           03  CONST-H2107             PIC X(06)   VALUE "200907".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    入院会計
       01  NACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
           COPY    "CPORCSS009.INC".
      *
      *    公費月上限額取得サブ
           COPY    "CPORCSJGNGET.INC".
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                         SECTION.
           COPY    "CPORCSSRHKOH.INC".
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       PROCEDURE                       DIVISION
           USING
           SSRHKOH-AREA
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
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
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
                                       FLG-AREA
                                       WRK-AREA
                                       KEY-AREA
                                       TNCOMB-AREA
      *
           MOVE    ZERO                TO  SSRHKOH-RC
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           INITIALIZE ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SSRHKOH-PTID        TO  AGECHK-PTID
           MOVE    SSRHKOH-SRYYM       TO  AGECHK-SRYYMD
           MOVE    "01"                TO  AGECHK-SRYYMD(7:2)
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"            USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
           IF    ( AGECHK-RC       =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE  ORCSAGECHKAREA
           END-IF
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1001"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SSRHKOH-SRYYM       TO  SYS-STYUKYMD
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1001-REC
      *
           MOVE    SSRHKOH-HKNCOMBI    TO  WRK-HKNCOMBI
           PERFORM 800-HKNCOMBI-CHK-SEC
           IF    ( FLG-HKNHIT          =   1 )
               MOVE    COMB-HKNNUM     TO  KEY-HKNNUM
               PERFORM 800-HOKEN-CHK-SEC
               MOVE    PTHKN-HKNJANUM  TO  KEY-HKNJANUM
               MOVE    PTHKN-HONKZKKBN TO  KEY-HONKZKKBN
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 900-SRYACCT-KEY53-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SRYACCT NOT =   ZERO )
                    OR   ( SSRHKOH-RC  NOT =   ZERO )
      *
               MOVE    ACCT-HKNCOMBI   TO  WRK-HKNCOMBI
               PERFORM 2001-H2105KAISEI-CHK-SEC
      *
               PERFORM 900-SRYACCT-KEY53-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key53"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( SSRHKOH-RC      =   ZERO )
               IF    ( SSRHKOH-NYUGAIKBN   =   "1" )
                   PERFORM 2001-NACCTHKN-CHK-SEC
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    入院会計チェック処理
      *****************************************************************
       2001-NACCTHKN-CHK-SEC           SECTION.
      *
           INITIALIZE                  TNCOMB-AREA
      *
      ***  PERFORM 900-NYUINACCT-KEY17-SEL-SEC
      *
      ***  INITIALIZE              SS009-AREA
      ***  CALL    "ORCSS009"      USING
      ***                          SS009-AREA
      ***                          NACCT-REC
      ***                          SPA-AREA
      *
      ***  PERFORM VARYING IDX-ZAI FROM    1   BY  1
      ***          UNTIL ( IDX-ZAI >   SS009-HENKYAKU-MAX )
      *
      ***      IF    ( SS009-ZAITEN(IDX-ZAI) *  SS009-ZAIKAISU(IDX-ZAI)
      ***                          >   ZERO )
      ***          PERFORM VARYING IDX0    FROM    1   BY  1
      ***                  UNTIL ( IDX0    >   31 )
      ***                   OR   ( SS009-HKNCOMBI(IDX-ZAI)
      ***                                  =   TNCOMB-HKNCOMBI(IDX0))
      *
      ***              IF    ( IDX0      >   TNCOMB-MAX )
      ***                  MOVE    IDX0  TO  TNCOMB-MAX
      ***                  MOVE    SS009-HKNCOMBI(IDX-ZAI)
      ***                                TO  TNCOMB-HKNCOMBI(TNCOMB-MAX)
      ***                  MOVE    31    TO  IDX0
      ***              END-IF
      ***          END-PERFORM
      ***      END-IF
      *
      ***  END-PERFORM
      *
           PERFORM 2001-NACCTHKN-CHK2-SEC
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TNCOMB-MAX )
                    OR   ( SSRHKOH-RC  NOT =   ZERO )
      *
               MOVE    TNCOMB-HKNCOMBI(IDX0)   TO  WRK-HKNCOMBI
               PERFORM 2001-H2105KAISEI-CHK-SEC
      *
           END-PERFORM
      *
           .
       2001-NACCTHKN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院会計チェック処理
      *****************************************************************
       2001-NACCTHKN-CHK2-SEC           SECTION.
      *
           INITIALIZE                  WRK-DAY-AREA
      *
      *    入院料
           MOVE    "1"    TO   WRK-ZAISKBKBN
           PERFORM 900-NYUINACCT-KEY17-SEL2-SEC
           PERFORM UNTIL ( FLG-NACCT   NOT =   ZERO )
             PERFORM   VARYING   IDX1    FROM    1   BY  1
                        UNTIL    IDX1     >     31
                IF     NACCT-DAY(IDX1) NOT =  ZERO 
                  MOVE NACCT-DAY(IDX1)   TO  WRK-DAY1(IDX1)
                END-IF
             END-PERFORM
             PERFORM 900-NYUINACCT-KEY17-FET2-SEC
           END-PERFORM
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    保険組み合わせ
           MOVE    "5"    TO   WRK-ZAISKBKBN
           PERFORM 900-NYUINACCT-KEY17-SEL2-SEC
           PERFORM UNTIL ( FLG-NACCT   NOT =   ZERO )
             PERFORM   VARYING   IDX1    FROM    1   BY  1
                        UNTIL    IDX1     >     31
                IF     NACCT-DAY(IDX1) NOT =  ZERO 
                  MOVE NACCT-DAY(IDX1)   TO  WRK-DAY5(IDX1)
                END-IF
             END-PERFORM
             PERFORM 900-NYUINACCT-KEY17-FET2-SEC
           END-PERFORM
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    入院料未算定日等の取得
           INITIALIZE                      ORCSNYUINDAY-AREA
           MOVE    SPA-HOSPNUM         TO  LNK-NYUINDAY-HOSPNUM
           MOVE    SSRHKOH-PTID        TO  LNK-NYUINDAY-PTID
           MOVE    SSRHKOH-SRYYM       TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"      USING   ORCSNYUINDAY-AREA
                                               SPA-AREA
           IF      LNK-NYUINDAY-RC  =  ZERO
                PERFORM   VARYING   IDX1    FROM    1   BY  1
                           UNTIL    IDX1     >     31
                   MOVE  LNK-NYUINDAY-KBN(IDX1)
                                       TO  WRK-DAY-MI(IDX1)
                END-PERFORM
           END-IF
      *
      ***
      *
           PERFORM   VARYING   IDX1    FROM    1   BY  1
                      UNTIL    IDX1     >     31
              IF  ((WRK-DAY1(IDX1)  NOT = ZERO) AND
                   (WRK-DAY5(IDX1)  NOT = ZERO))
              OR  ((WRK-DAY-MI(IDX1)    = "1" ) AND
                   (WRK-DAY5(IDX1)  NOT = ZERO))
                MOVE     ZERO      TO   FLG-OK
                PERFORM  VARYING  IDX2  FROM  1  BY  1
                          UNTIL  (IDX2                     >  31  )
                           OR    (TNCOMB-HKNCOMBI(IDX2)    = SPACE)
                           OR    (FLG-OK                   =   1  )
                  IF    WRK-DAY5X(IDX1) = TNCOMB-HKNCOMBI(IDX2)
                    MOVE    1      TO   FLG-OK
                  END-IF
                END-PERFORM
                IF    FLG-OK  =  ZERO
                AND   IDX2   <=   31
                  MOVE  WRK-DAY5X(IDX1) TO  TNCOMB-HKNCOMBI(IDX2)
                  MOVE  IDX2            TO  TNCOMB-MAX
                END-IF
              END-IF
           END-PERFORM
      *
           .
       2001-NACCTHKN-CHK2-EXT.
           EXIT.
      *****************************************************************
      *    平成２１年５月改正対象公費存在チェック処理
      *****************************************************************
       2001-H2105KAISEI-CHK-SEC        SECTION.
      *
           PERFORM 800-HKNCOMBI-CHK-SEC
           IF    ( KEY-HKNNUM      =   COMB-HKNNUM )
           AND   ( FLG-KOH         =   1           )
      *
               PERFORM 800-HOKEN-CHK-SEC
               PERFORM VARYING IDX-KOH FROM    1   BY  1
                       UNTIL ( IDX-KOH >   TKOH-MAX )
      *
                   IF    ( KEY-HKNNUM      =   PTHKN-HKNNUM )
                    AND  ( KEY-HKNJANUM    =   PTHKN-HKNJANUM )
                       EVALUATE    TRUE
                       WHEN  ( TKOH-KOHNUM  (IDX-KOH)
                                           =   "051" OR "091" )
                        AND  ( TKOH-FTNJANUM(IDX-KOH) (5:3)
                                           =   "601" OR "602" )
                        AND  ( TKOH-FTNJANUM(IDX-KOH) (1:2)
                                           =   "51"           )
                       WHEN  ( TKOH-KOHNUM  (IDX-KOH)
                                           =   "151"          )
                        AND  ( SYS-1001-PREFNUM   =  13       )
                        AND  ( TKOH-FTNJANUM(IDX-KOH) (5:3)
                                           =   "601" OR "602" )
                        AND  ( TKOH-FTNJANUM(IDX-KOH) (1:2)
                                           =   "51"           )
                       WHEN  ( TKOH-KOHNUM  (IDX-KOH)
                                           =   "054"          )
                        AND  ( TKOH-FTNJANUM(IDX-KOH) (5:3)
                                           =   "601" OR "501"
                                                     OR "602" )
                        AND  ( TKOH-FTNJANUM(IDX-KOH) (1:2)
                                           =   "54"           )
                       WHEN  ( TKOH-KOHNUM  (IDX-KOH)
                                           =   "054"          )
                        AND  ( TKOH-FTNJANUM(IDX-KOH) (5:1)
                                           =   "7" OR "8"     )
                        AND  ( TKOH-FTNJANUM(IDX-KOH) (1:2)
                                           =   "54"           )
                       WHEN  ( TKOH-KOHNUM  (IDX-KOH)
                                           =   "052"          )
                        AND  ( TKOH-FTNJANUM(IDX-KOH) (5:2)
                                           =   "60"           )
                        AND  ( TKOH-FTNJANUM(IDX-KOH) (1:2)
                                           =   "52"           )
                        AND  ( SSRHKOH-SRYYM    <=   "201412" )
                       WHEN  ( TKOH-KOHNUM  (IDX-KOH)
                                           =   "052"          )
                        AND  ( TKOH-FTNJANUM(IDX-KOH) (5:1)
                                           =   "7" OR "8"     )
                        AND  ( TKOH-FTNJANUM(IDX-KOH) (1:2)
                                           =   "52"           )
                        AND  ( SSRHKOH-SRYYM    >=   "201501" )
                       WHEN  ( TKOH-KOHNUM  (IDX-KOH)
                                           =   "152"          )
                        AND  ( SYS-1001-PREFNUM   =  23 OR 24 )
                        AND  ( TKOH-FTNJANUM(IDX-KOH) (5:2)
                                           =   "60"           )
                        AND  ( TKOH-FTNJANUM(IDX-KOH) (1:2)
                                           =   "52"           )
                        AND  ( SSRHKOH-SRYYM    <=   "201412" )
                       WHEN  ( WRK-JGNGET-TEKIYOFLG(IDX-KOH)
                                           =    1             )
                           IF    ( KEY-HKNNUM  =   "060" OR "039" )
                            OR   ( AGECHK-NENREI   >=    70 )
                               MOVE    1       TO  SSRHKOH-RC
                           ELSE
                               IF    ( KEY-HONKZKKBN
                                           =   PTHKN-HONKZKKBN )
                                   MOVE    1   TO  SSRHKOH-RC
                               END-IF
                           END-IF
                       END-EVALUATE
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       2001-H2105KAISEI-CHK-EXT.
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
      *    保険組合せチェック処理
      *****************************************************************
       800-HKNCOMBI-CHK-SEC            SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC
      *
           MOVE    ZERO            TO  FLG-HKNHIT
                                       FLG-KOH
      *
           INITIALIZE                  WRK-JGNGET-AREA
      *
           PERFORM 900-HKNCOMBI-KEY-SEL-SEC
           IF    ( COMB-HKNNUM     =   SPACE OR "971" OR "973" OR "975"
                                    OR "068" OR "069" OR "040")
            OR   ( COMB-HKNNUM(1:2)  = "98"  OR "90"  OR "91" )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-HKNHIT
           END-IF
      *
           PERFORM VARYING IDX-KOH FROM    1   BY  1
                   UNTIL ( IDX-KOH >   CONST-KOH-MAX )
                    OR   ( COMB-KOHHKNNUM (IDX-KOH)    =   SPACE )
      *
               IF  IDX-KOH  =  1
                 IF  ( COMB-KOHHKNNUM (IDX-KOH)    =   "051"
                                                   OR  "052"
                                                   OR  "091"    )
                 OR  ((COMB-KOHHKNNUM (IDX-KOH)    =   "054"   ) AND
                      (SSRHKOH-SRYYM              >=   "201501"))
                 OR  ((SYS-1001-PREFNUM            =    13     ) AND
                      (COMB-KOHHKNNUM (IDX-KOH)    =   "151"   ) AND
                      (SSRHKOH-SRYYM              >=   "200905"))
                 OR  ((SYS-1001-PREFNUM            =    23     ) AND
                      (COMB-KOHHKNNUM (IDX-KOH)    =   "191"   ) AND
                      (SSRHKOH-SRYYM              >=   "200910"))
                 OR  ((SYS-1001-PREFNUM            =   23 OR 24) AND
                      (COMB-KOHHKNNUM (IDX-KOH)    =   "152"   ) AND
                      (SSRHKOH-SRYYM              >=   "201104") AND
                      (SSRHKOH-SRYYM              <=   "201412"))
                   MOVE    1       TO  FLG-KOH
                 END-IF
                 IF   (FLG-KOH                       =  ZERO         )
                 AND  (COMB-KOHHKNNUM(IDX-KOH)(1:1) >= "1" AND <= "8")
                   PERFORM  900-PTKOHINF-KEY-SEL-SEC
                   INITIALIZE                  LNK-ORCSJGNGET-REC
                   MOVE  SPA-HOSPNUM       TO  LNK-ORCSJGNGET-HOSPNUM
                   MOVE  COMB-KOHHKNNUM(IDX-KOH)
                                           TO  LNK-ORCSJGNGET-KOHHKNNUM
                   MOVE  PTKOH-TEKSTYMD    TO  LNK-ORCSJGNGET-TEKSTYMD
                   MOVE  SYS-1001-PREFNUM  TO  LNK-ORCSJGNGET-PREFNUM
                   MOVE   "1"              TO  LNK-ORCSJGNGET-HENFLG
                   CALL  "ORCSJGNGET"      USING
                                           LNK-ORCSJGNGET-REC
                                           SPA-AREA
                   IF    LNK-ORCSJGNGET-RC   =   ZERO
                     IF   (SSRHKOH-SRYYM            >=  "201501")
                     AND  (LNK-ORCSJGNGET-TEKIYOFLG  =   1      )
                       MOVE    1   TO  FLG-KOH
                       MOVE    1   TO  WRK-JGNGET-TEKIYOFLG(IDX-KOH)
                     END-IF
                   END-IF
                 END-IF
               END-IF
      *
               IF  IDX-KOH  =  2
                 IF  ( COMB-KOHHKNNUM (IDX-KOH)    =   "051"
                                                   OR  "091"    )
                 OR  ((COMB-KOHHKNNUM (IDX-KOH)    =   "054"   ) AND
                      (SSRHKOH-SRYYM              >=   "201501"))
                   MOVE    1       TO  FLG-KOH
                 END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       800-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険チェック処理
      *****************************************************************
       800-HOKEN-CHK-SEC               SECTION.
      *
           INITIALIZE                  PTHKNINF-REC
                                       TKOH-AREA
      *
           PERFORM 900-PTHKNINF-KEY-SEL-SEC
           PERFORM VARYING IDX-KOH FROM    1   BY  1
                   UNTIL ( IDX-KOH >   CONST-KOH-MAX )
                    OR   ( COMB-KOHHKNNUM (IDX-KOH)    =   SPACE )
      *
               COMPUTE TKOH-MAX    =   TKOH-MAX    +   1
               PERFORM 900-PTKOHINF-KEY-SEL-SEC
               MOVE    PTKOHINF-REC    TO  TKOH-OCC (TKOH-MAX)
      *
           END-PERFORM
      *
           .
       800-HOKEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理(KEY17)
      *****************************************************************
       900-NYUINACCT-KEY17-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NACCT
      *
           INITIALIZE                      NACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SSRHKOH-PTID        TO  NACCT-PTID
           MOVE    SSRHKOH-SRYYM       TO  NACCT-SRYYM
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
           MOVE    NACCT-REC           TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NACCT-REC
           ELSE
               INITIALIZE                  NACCT-REC
               MOVE    1               TO  FLG-NACCT
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
       900-NYUINACCT-KEY17-SEL2-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NACCT
      *
           INITIALIZE                      NACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SSRHKOH-PTID        TO  NACCT-PTID
           MOVE    SSRHKOH-SRYYM       TO  NACCT-SRYYM
           MOVE    WRK-ZAISKBKBN       TO  NACCT-ZAISKBKBN
           MOVE    NACCT-REC           TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NACCT-REC
           ELSE
               INITIALIZE                  NACCT-REC
               MOVE    1               TO  FLG-NACCT
           END-IF
      *
           .
       900-NYUINACCT-KEY17-SEL2-EXT.
           EXIT.
      *****************************************************************
      *    入院会計FETCH処理
      *****************************************************************
       900-NYUINACCT-KEY17-FET2-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NACCT
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NACCT-REC
           ELSE
               INITIALIZE                  NACCT-REC
               MOVE    1               TO  FLG-NACCT
           END-IF
      *
           .
       900-NYUINACCT-KEY17-FET2-EXT.
           EXIT.
      *****************************************************************
      *    診療会計検索処理
      *****************************************************************
       900-SRYACCT-KEY53-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCT
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SSRHKOH-NYUGAIKBN   TO  ACCT-NYUGAIKBN
           MOVE    SSRHKOH-PTID        TO  ACCT-PTID
           MOVE    SSRHKOH-SRYYM       TO  ACCT-SRYYM
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key53"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-KEY53-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療会計FETCH処理
      *****************************************************************
       900-SRYACCT-KEY53-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCT
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key53"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-KEY53-FET-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SSRHKOH-PTID        TO  COMB-PTID
           MOVE    WRK-HKNCOMBI        TO  COMB-HKNCOMBINUM
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
      *    保険マスター読み込み
      *****************************************************************
       900-PTHKNINF-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO               TO  FLG-PTHKNINF
      *
           INITIALIZE                      PTHKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    SSRHKOH-PTID        TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
           END-IF
     *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTHKNINF-KEY-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ検索処理
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
      *    公費マスター読み込み
      *****************************************************************
       900-PTKOHINF-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-PTKOHINF
      *
           INITIALIZE                      PTKOHINF-REC
           MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
           MOVE    SSRHKOH-PTID        TO  PTKOH-PTID
           MOVE    COMB-KOHID(IDX-KOH) TO  PTKOH-KOHID
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
     *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTKOHINF-KEY-SEL-EXT.
           EXIT.
      **************************************************************
      *    患者情報検索処理(KEY)
      **************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    SSRHKOH-PTID    TO  PTINF-PTID
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
