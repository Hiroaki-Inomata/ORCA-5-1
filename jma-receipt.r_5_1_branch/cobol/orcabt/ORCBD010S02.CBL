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
       PROGRAM-ID.             ORCBD010S02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次・月次統計
      *  コンポーネント名  : 収納明細編集サブ（処理区分毎の編集）
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
           03  FLG-END                     PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX-S02                     PIC 9(05).
           03  IDX-S03                     PIC 9(05).
           03  IDX-JIHI                    PIC 9(05).
           03  IDX-HS02                    PIC 9(05).
           03  IDX-HS03                    PIC 9(05).
           03  IDX-SYAHON                  PIC 9(02).
           03  IDX-SYAKZK                  PIC 9(02).
           03  IDX-SYARJN                  PIC 9(02).
           03  IDX-KOKIPN                  PIC 9(02).
           03  IDX-KOKRJN                  PIC 9(02).
           03  IDX-KOHTAN                  PIC 9(02).
           03  IDX-JIHHKN                  PIC 9(02).
           03  IDX-ROUSAI                  PIC 9(02).
           03  IDX-JIBAI                   PIC 9(02).
           03  IDX-KOUGAI                  PIC 9(02).
           03  IDX-KOUKI                   PIC 9(02).
           03  IDX-CHIKEN                  PIC 9(02).
           03  IDX-MISYU                   PIC 9(02).
      *
       01  CNT-AREA.
           03  CNT-HOHO                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYKA                   PIC  X(02).
           03  WRK-HKNCOMBI                PIC  X(04).
           03  WRK-HOKEN                   PIC  9(02).
           03  WRK-MISYU                   PIC  S9(10).
           03  WRK-HKNMISYU                PIC  S9(10).
           03  WRK-JIHMISYU                PIC  S9(10).
           03  WRK-HS02-MAX                PIC 9(05).
           03  WRK-HS03-MAX                PIC 9(05).
           03  WRK-KAISYU                  PIC  S9(10).
           03  WRK-MISYU-KON               PIC  S9(10).
           03  WRK-MISYU-ZEN               PIC  S9(10).
           03  WRK-NYUKIN                  PIC  S9(10).
           03  WRK-SEIKYU                  PIC  S9(10).
           03  WRK-NYUKIN-KON              PIC  S9(10).
           03  WRK-NYUKIN-ZAN              PIC  S9(10).
      *
       01  KEY-AREA.
           03  KEY-SRYKA                   PIC X(02).
           03  KEY-HOKEN                   PIC 9(02).
           03  KEY-SRYKA2                  PIC X(02).
      *
       01  THOHO-AREA.
           03  THOHO-MAX                   PIC 9(05).
           03  THOHO-OCC                   OCCURS  100.
               05  THOHO-HOHO              PIC X(02).
               05  THOHO-HOHOMONEY         PIC S9(10).
      *
       01  CONST-AREA.
           03  CONST-H200401               PIC X(08)   VALUE "20080401".
           03  CONST-SBD010S02-MAX         PIC 9(05)   VALUE 200.
           03  CONST-ETCJIHI-MAX           PIC 9(05)   VALUE 10.
           03  CONST-HOHO-MAX              PIC 9(05)   VALUE 100.
      *
           COPY    "CPORCBD010S02.INC"     REPLACING   //SBD010S02-//
                                           BY          //TSBD010S02-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    収納明細編集サブ
           COPY    "CPORCBD010S03.INC".
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
           COPY    "CPORCBD010S02.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
                                                SYUNOU-REC
                                                SBD010-AREA
                                                SBD010S02-AREA
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
      *
      *
           EVALUATE    TRUE
           WHEN  ( SBD010S02-EDDAILYKEY (1:8)
                                       >=  CONST-H200401 )
      *
               MOVE    1                   TO  IDX-SYAHON
               MOVE    2                   TO  IDX-SYAKZK
               MOVE    3                   TO  IDX-KOKIPN
               MOVE    4                   TO  IDX-KOUKI
                                               IDX-SYARJN
                                               IDX-KOKRJN
               MOVE    5                   TO  IDX-KOHTAN
               MOVE    6                   TO  IDX-JIHHKN
               MOVE    7                   TO  IDX-ROUSAI
               MOVE    8                   TO  IDX-JIBAI 
               MOVE    9                   TO  IDX-KOUGAI
               MOVE    10                  TO  IDX-CHIKEN
      *
           WHEN  OTHER
      *
               MOVE    1                   TO  IDX-SYAHON
               MOVE    2                   TO  IDX-SYAKZK
               MOVE    3                   TO  IDX-SYARJN
               MOVE    4                   TO  IDX-KOKIPN
               MOVE    5                   TO  IDX-KOKRJN
               MOVE    6                   TO  IDX-KOHTAN
               MOVE    7                   TO  IDX-JIHHKN
               MOVE    8                   TO  IDX-ROUSAI
               MOVE    9                   TO  IDX-JIBAI 
               MOVE    10                  TO  IDX-KOUGAI
               MOVE    11                  TO  IDX-CHIKEN
      *
           END-EVALUATE
      *
           INITIALIZE                  SBD010S03-AREA
           CALL    "ORCBD010S03"       USING
                                       SYUNOU-REC
                                       SBD010-AREA
                                       SBD010S03-AREA
                                       SPA-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM VARYING IDX-S03  FROM    1   BY  1
                   UNTIL ( IDX-S03  >   SBD010S03-MAX )
      *
               IF      ( SBD010S03-DAILYKEY (IDX-S03)
                                           <=  SBD010S02-EDDAILYKEY )
                   COMPUTE WRK-MISYU
                       =   WRK-MISYU
                       +   SBD010S03-SEIKYU (IDX-S03)
                       -   SBD010S03-NYUKIN (IDX-S03)
      *
                   COMPUTE WRK-HKNMISYU
                       =   WRK-HKNMISYU
                       +   SBD010S03-HKNSEIKYU (IDX-S03)
                       +   SBD010S03-HKNGENMEN (IDX-S03)
                       +   SBD010S03-HKNCHOSEI (IDX-S03)
                       -   SBD010S03-HKNNYUKIN (IDX-S03)
      *
                   COMPUTE WRK-JIHMISYU
                       =   WRK-JIHMISYU
                       +   SBD010S03-JIHSEIKYU (IDX-S03)
                       +   SBD010S03-JIHGENMEN (IDX-S03)
                       +   SBD010S03-JIHCHOSEI (IDX-S03)
                       -   SBD010S03-JIHNYUKIN (IDX-S03)
               END-IF
      *
               IF    ( SBD010S03-DAILYKEY (IDX-S03)
                                           >=  SBD010S02-STDAILYKEY )
                AND  ( SBD010S03-DAILYKEY (IDX-S03)
                                           <=  SBD010S02-EDDAILYKEY )
      *
                   MOVE    SBD010S03-SRYKA    (IDX-S03)
                                           TO      KEY-SRYKA
                                                   KEY-SRYKA2
                   PERFORM 800-GET-HOKEN-SEC
                   MOVE    WRK-HOKEN       TO      KEY-HOKEN
      *
                   IF    ( SBD010-SUMKBN   =   "0" )
                       MOVE    ZERO        TO      KEY-SRYKA
                   END-IF
      *
                   IF    ( SBD010-MEISAIKBN    =   "1" )
                       MOVE    1           TO  SBD010S02-MAX
                                               IDX-S02
                       PERFORM 20011-EDIT-SMEI-SEC
                   ELSE
                       PERFORM 2001-EDIT-SMEI-SEC
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           PERFORM 2001-EDIT-MISYU-SEC
      *
           MOVE    SBD010S02-AREA      TO  TSBD010S02-AREA
      *
           PERFORM VARYING IDX-S02 FROM    1   BY  1
                   UNTIL ( IDX-S02 >   SBD010S02-MAX )
      *
               INITIALIZE              SBD010S02-HOHO-G (IDX-S02)
               MOVE    TSBD010S02-HOHO-MAX (IDX-S02)
                                       TO  WRK-HS02-MAX
               MOVE    ZERO            TO  CNT-HOHO
      *
      *
               PERFORM VARYING IDX-HS02    FROM    1   BY  1
                       UNTIL ( IDX-HS02    >   WRK-HS02-MAX )
      *
                   IF    ( SBD010-MEISAIKBN    =   "1" )
                       COMPUTE SBD010S02-HOHOMONEY  (IDX-S02 1 )
                           =   SBD010S02-HOHOMONEY  (IDX-S02 1 )
                           +   TSBD010S02-HOHOMONEY  (IDX-S02 IDX-HS02 )
                       IF    ( IDX-HS02    =   WRK-HS02-MAX )
                           IF    ( SBD010S02-HOHOMONEY  (IDX-S02 1 )
                                           NOT =   ZERO )
                               MOVE    TSBD010S02-HOHO  (IDX-S02 1)
                                       TO  SBD010S02-HOHO  (IDX-S02 1)
                               MOVE    1   TO  SBD010S02-HOHO-MAX
                                                             (IDX-S02)
                           END-IF
                       END-IF
                   ELSE
                       IF    ( TSBD010S02-HOHOMONEY (IDX-S02 IDX-HS02)
                                       NOT =   ZERO )
                           COMPUTE CNT-HOHO    =   CNT-HOHO    +   1
                           MOVE    CNT-HOHO
                                       TO  SBD010S02-HOHO-MAX (IDX-S02)
                           MOVE    TSBD010S02-HOHO  (IDX-S02 IDX-HS02)
                                       TO  SBD010S02-HOHO 
                                                    (IDX-S02 CNT-HOHO)
                           MOVE    TSBD010S02-HOHOMONEY
                                                    (IDX-S02 IDX-HS02)
                                       TO  SBD010S02-HOHOMONEY
                                                    (IDX-S02 CNT-HOHO)
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDX-S02     =   IDX-MISYU )
                   MOVE    1           TO  SBD010S02-LAST  (IDX-S02)
                   MOVE    WRK-MISYU
                                       TO  SBD010S02-MISYU (IDX-S02)
                   MOVE    WRK-HKNMISYU
                                       TO  SBD010S02-HKNMISYU
                                                           (IDX-S02)
                   MOVE    WRK-JIHMISYU
                                       TO  SBD010S02-JIHMISYU
                                                           (IDX-S02)
               END-IF
      *
               MOVE    1               TO  SBD010S02-RENNUM(IDX-S02)
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   SBD010S02-MAX )
      *
                   IF    ( IDX-S02     =   IDX1 )
                       CONTINUE
                   ELSE
                       IF    ( SBD010S02-KEYSRYKA (IDX-S02)
                                       =   SBD010S02-KEYSRYKA (IDX1))
                           IF    ( SBD010S02-DENPEDANUM-G (IDX-S02)
                                       >   SBD010S02-DENPEDANUM-G(IDX1))
      *
                               COMPUTE SBD010S02-RENNUM(IDX-S02)
                                   =   SBD010S02-RENNUM(IDX-S02)
                                   +   1
      *
                           END-IF
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
           END-PERFORM
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    収納明細編集処理
      *****************************************************************
       2001-EDIT-SMEI-SEC          SECTION.
      *
           MOVE    ZERO        TO  IDX-S02
           PERFORM VARYING IDX0 FROM    1  BY  1
                   UNTIL ( IDX0        >   CONST-SBD010S02-MAX )
                    OR   ( IDX-S02 NOT =   ZERO )
      *
               IF    ( IDX0    >   SBD010S02-MAX )
                       MOVE    IDX0    TO  IDX-S02
                       MOVE    IDX0    TO  SBD010S02-MAX
               ELSE
                   IF    ( SBD010S02-KEYSRYKA (IDX0)
                                   =   KEY-SRYKA )
                    AND  ( SBD010S02-KEYHOKEN (IDX0)
                                   =   KEY-HOKEN )
                    AND  ( SBD010S02-SRYKA (IDX0)
                                   =   KEY-SRYKA2 )
                       MOVE    IDX0    TO  IDX-S02
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( IDX-S02     >   ZERO )
               PERFORM 20011-EDIT-SMEI-SEC
           END-IF
      *
           .
       2001-EDIT-SMEI-EXT.
           EXIT.
      *****************************************************************
      *    収納明細編集処理
      *****************************************************************
       20011-EDIT-SMEI-SEC             SECTION.
      *
           MOVE    KEY-SRYKA   TO  SBD010S02-KEYSRYKA (IDX-S02)
           MOVE    KEY-HOKEN   TO  SBD010S02-KEYHOKEN (IDX-S02)
      *
           IF    ( SBD010S02-MIN-DENPEDANUM    =   ZERO )
               MOVE    SBD010S03-DENPEDANUM (IDX-S03)
                               TO  SBD010S02-MIN-DENPEDANUM
           END-IF
      *
           MOVE    SBD010S03-DENPEDANUM (IDX-S03)
                               TO  SBD010S02-DENPEDANUM (IDX-S02)
      *
           MOVE    SBD010S03-SRYKA (IDX-S03)
                               TO  SBD010S02-SRYKA (IDX-S02)
           MOVE    SBD010S03-HKNCOMBI (IDX-S03)
                               TO  SBD010S02-HKNCOMBI (IDX-S02)
           MOVE    WRK-HOKEN
                               TO  SBD010S02-HOKEN (IDX-S02)
           MOVE    SBD010S03-HKNNUM (IDX-S03)
                               TO  SBD010S02-HKNNUM (IDX-S02)
           MOVE    SBD010S03-HONKZKKBN (IDX-S03)
                               TO  SBD010S02-HONKZKKBN (IDX-S02)
           MOVE    SBD010S03-KOHNUM (IDX-S03)
                               TO  SBD010S02-KOHNUM (IDX-S02)
           COMPUTE SBD010S02-HKNTEN (IDX-S02)
               =   SBD010S02-HKNTEN (IDX-S02)
               +   SBD010S03-HKNTEN (IDX-S03)
           COMPUTE SBD010S02-SEIKYU (IDX-S02)
               =   SBD010S02-SEIKYU (IDX-S02)
               +   SBD010S03-SEIKYU (IDX-S03)
           COMPUTE SBD010S02-HKNSEIKYU (IDX-S02)
               =   SBD010S02-HKNSEIKYU (IDX-S02)
               +   SBD010S03-HKNSEIKYU (IDX-S03)
           COMPUTE SBD010S02-JIHSEIKYU (IDX-S02)
               =   SBD010S02-JIHSEIKYU (IDX-S02)
               +   SBD010S03-JIHSEIKYU (IDX-S03)
           COMPUTE SBD010S02-CHOSEI (IDX-S02)
               =   SBD010S02-CHOSEI (IDX-S02)
               +   SBD010S03-CHOSEI (IDX-S03)
           COMPUTE SBD010S02-CHOSEI1 (IDX-S02)
               =   SBD010S02-CHOSEI1 (IDX-S02)
               +   SBD010S03-CHOSEI1 (IDX-S03)
           COMPUTE SBD010S02-CHOSEI2 (IDX-S02)
               =   SBD010S02-CHOSEI2 (IDX-S02)
               +   SBD010S03-CHOSEI2 (IDX-S03)
           COMPUTE SBD010S02-HKNCHOSEI (IDX-S02)
               =   SBD010S02-HKNCHOSEI (IDX-S02)
               +   SBD010S03-HKNCHOSEI (IDX-S03)
           COMPUTE SBD010S02-HKNCHOSEI1 (IDX-S02)
               =   SBD010S02-HKNCHOSEI1 (IDX-S02)
               +   SBD010S03-HKNCHOSEI1 (IDX-S03)
           COMPUTE SBD010S02-HKNCHOSEI2 (IDX-S02)
               =   SBD010S02-HKNCHOSEI2 (IDX-S02)
               +   SBD010S03-HKNCHOSEI2 (IDX-S03)
           COMPUTE SBD010S02-JIHCHOSEI (IDX-S02)
               =   SBD010S02-JIHCHOSEI (IDX-S02)
               +   SBD010S03-JIHCHOSEI (IDX-S03)
           COMPUTE SBD010S02-JIHCHOSEI1 (IDX-S02)
               =   SBD010S02-JIHCHOSEI1 (IDX-S02)
               +   SBD010S03-JIHCHOSEI1 (IDX-S03)
           COMPUTE SBD010S02-JIHCHOSEI2 (IDX-S02)
               =   SBD010S02-JIHCHOSEI2 (IDX-S02)
               +   SBD010S03-JIHCHOSEI2 (IDX-S03)
           COMPUTE SBD010S02-GENMEN (IDX-S02)
               =   SBD010S02-GENMEN (IDX-S02)
               +   SBD010S03-GENMEN (IDX-S03)
           COMPUTE SBD010S02-HKNGENMEN (IDX-S02)
               =   SBD010S02-HKNGENMEN (IDX-S02)
               +   SBD010S03-HKNGENMEN (IDX-S03)
           COMPUTE SBD010S02-JIHGENMEN (IDX-S02)
               =   SBD010S02-JIHGENMEN (IDX-S02)
               +   SBD010S03-JIHGENMEN (IDX-S03)
           COMPUTE SBD010S02-NYUKIN (IDX-S02)
               =   SBD010S02-NYUKIN (IDX-S02)
               +   SBD010S03-NYUKIN (IDX-S03)
           COMPUTE SBD010S02-HKNNYUKIN (IDX-S02)
               =   SBD010S02-HKNNYUKIN (IDX-S02)
               +   SBD010S03-HKNNYUKIN (IDX-S03)
           COMPUTE SBD010S02-JIHNYUKIN (IDX-S02)
               =   SBD010S02-JIHNYUKIN (IDX-S02)
               +   SBD010S03-JIHNYUKIN (IDX-S03)
           PERFORM VARYING IDX-JIHI FROM    1   BY  1
                   UNTIL ( IDX-JIHI >   CONST-ETCJIHI-MAX )
               COMPUTE SBD010S02-ETCJIHI (IDX-S02 IDX-JIHI)
                   =   SBD010S02-ETCJIHI (IDX-S02 IDX-JIHI)
                   +   SBD010S03-ETCJIHI (IDX-S03 IDX-JIHI)
           END-PERFORM
      *
           MOVE    SBD010S03-HOHO-MAX (IDX-S03)
                               TO  WRK-HS03-MAX
           PERFORM VARYING IDX-HS03    FROM    1   BY  1
                   UNTIL ( IDX-HS03    >   WRK-HS03-MAX )
      *
               PERFORM VARYING IDX-HS02    FROM    1   BY  1
                       UNTIL ( IDX-HS02    >   CONST-HOHO-MAX )
      *
                   IF    ( SBD010S02-HOHO (IDX-S02 IDX-HS02)
                                       =   SPACE )
                           MOVE    IDX-HS02    TO  SBD010S02-HOHO-MAX
                                                           (IDX-S02)
                           MOVE    SBD010S03-HOHO (IDX-S03 IDX-HS03)
                                       TO  SBD010S02-HOHO
                                                      (IDX-S02 IDX-HS02)
                           COMPUTE SBD010S02-HOHOMONEY(IDX-S02 IDX-HS02)
                               =   SBD010S02-HOHOMONEY(IDX-S02 IDX-HS02)
                               +   SBD010S03-HOHOMONEY(IDX-S03 IDX-HS03)
                           MOVE    CONST-HOHO-MAX  TO  IDX-HS02
                   ELSE
                       IF    ( SBD010S02-HOHO (IDX-S02 IDX-HS02)
                                           =    SBD010S03-HOHO
                                                    (IDX-S03 IDX-HS03) )
                           COMPUTE SBD010S02-HOHOMONEY(IDX-S02 IDX-HS02)
                               =   SBD010S02-HOHOMONEY(IDX-S02 IDX-HS02)
                               +   SBD010S03-HOHOMONEY(IDX-S03 IDX-HS03)
                           MOVE    CONST-HOHO-MAX  TO  IDX-HS02
                       END-IF
                   END-IF
      *
               END-PERFORM
           END-PERFORM
      *
           IF    ( SBD010S02-DAILYKEY (IDX-S02)    =    SPACE )
               MOVE    SBD010S03-DAILYKEY (IDX-S03)
                                       TO  SBD010S02-DAILYKEY (IDX-S02)
           END-IF
      *
           IF    ( SBD010S03-SYURRKNUM (IDX-S03)   >    ZERO )
               MOVE    SBD010S03-SYURRKNUM (IDX-S03)
                                       TO  SBD010S02-SYURRKNUM (IDX-S02)
               MOVE    SBD010S03-SYUEDANUM (IDX-S03)
                                       TO  SBD010S02-SYUEDANUM (IDX-S02)
           END-IF
      *
           MOVE    IDX-S02             TO  IDX-MISYU
      *
           .
       20011-EDIT-SMEI-EXT.
           EXIT.
      *****************************************************************
      *    未収額編集処理
      *****************************************************************
       2001-EDIT-MISYU-SEC            SECTION.
      *
           PERFORM VARYING IDX-S03  FROM    1   BY  1
                   UNTIL ( IDX-S03  >   SBD010S03-MAX )
      *
               IF    ( SBD010S03-DAILYKEY (IDX-S03)
                                           <   SBD010S02-STDAILYKEY )
      *
                   MOVE    SBD010S03-SRYKA    (IDX-S03)
                                           TO  KEY-SRYKA
                                               KEY-SRYKA2
                   PERFORM 800-GET-HOKEN-SEC
                   MOVE    WRK-HOKEN   TO  KEY-HOKEN
      *
                   IF    ( SBD010-SUMKBN   =   "0" )
                       MOVE    ZERO    TO  KEY-SRYKA
                   END-IF
      *
                   IF    ( SBD010-MEISAIKBN    =   "1" )
                       MOVE    1       TO  IDX-S02
                       PERFORM 20021-EDIT-SMEI-SEC
                   ELSE
                       PERFORM 2002-EDIT-SMEI-SEC
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX-S02 FROM    1   BY  1
                   UNTIL ( IDX-S02 >   SBD010S02-MAX )
      *
               MOVE    SBD010S02-ZENHKNMISYU (IDX-S02)
                                   TO  WRK-MISYU-ZEN
               MOVE    SBD010S02-HKNNYUKIN   (IDX-S02)
                                   TO  WRK-NYUKIN
               COMPUTE WRK-SEIKYU
                   =   SBD010S02-HKNSEIKYU   (IDX-S02)
                   +   SBD010S02-HKNGENMEN   (IDX-S02)
                   +   SBD010S02-HKNCHOSEI   (IDX-S02)
      *
               PERFORM 20021-EDIT-MISYU-SEC
               COMPUTE SBD010S02-ZENHKNNYUKIN (IDX-S02)
                   =   WRK-KAISYU
               COMPUTE SBD010S02-KONHKNNYUKIN (IDX-S02)
                   =   SBD010S02-HKNNYUKIN    (IDX-S02)
                   -   WRK-KAISYU
               COMPUTE SBD010S02-KONHKNMISYU  (IDX-S02)
                   =   WRK-MISYU-KON
      *
               MOVE    SBD010S02-ZENJIHMISYU (IDX-S02)
                                   TO  WRK-MISYU-ZEN
               MOVE    SBD010S02-JIHNYUKIN   (IDX-S02)
                                   TO  WRK-NYUKIN
               COMPUTE WRK-SEIKYU
                   =   SBD010S02-JIHSEIKYU   (IDX-S02)
                   +   SBD010S02-JIHGENMEN   (IDX-S02)
                   +   SBD010S02-JIHCHOSEI   (IDX-S02)
      *
               PERFORM 20021-EDIT-MISYU-SEC
               COMPUTE SBD010S02-ZENJIHNYUKIN (IDX-S02)
                   =   WRK-KAISYU
               COMPUTE SBD010S02-KONJIHNYUKIN (IDX-S02)
                   =   SBD010S02-JIHNYUKIN    (IDX-S02)
                   -   WRK-KAISYU
               COMPUTE SBD010S02-KONJIHMISYU  (IDX-S02)
                   =   WRK-MISYU-KON
      *
               COMPUTE SBD010S02-ZENNYUKIN (IDX-S02)
                   =   SBD010S02-ZENHKNNYUKIN (IDX-S02)
                   +   SBD010S02-ZENJIHNYUKIN (IDX-S02)
      *
               COMPUTE SBD010S02-KONNYUKIN (IDX-S02)
                   =   SBD010S02-KONHKNNYUKIN (IDX-S02)
                   +   SBD010S02-KONJIHNYUKIN (IDX-S02)
      *
               COMPUTE SBD010S02-ZENMISYU (IDX-S02)
                   =   SBD010S02-ZENHKNMISYU (IDX-S02)
                   +   SBD010S02-ZENJIHMISYU (IDX-S02)
      *
               COMPUTE SBD010S02-KONMISYU (IDX-S02)
                   =   SBD010S02-KONHKNMISYU (IDX-S02)
                   +   SBD010S02-KONJIHMISYU (IDX-S02)
           END-PERFORM
      *
           .
       2001-EDIT-MISYU-EXT.
           EXIT.
      *****************************************************************
      *    未収額収納明細編集処理
      *****************************************************************
       2002-EDIT-SMEI-SEC              SECTION.
      *
           MOVE    ZERO        TO  IDX-S02
           PERFORM VARYING IDX0 FROM    1  BY  1
                   UNTIL ( IDX0        >   CONST-SBD010S02-MAX )
                    OR   ( IDX-S02 NOT =   ZERO )
      *
                   IF    ( SBD010S02-KEYSRYKA (IDX0)
                                   =   KEY-SRYKA )
                    AND  ( SBD010S02-KEYHOKEN (IDX0)
                                   =   KEY-HOKEN )
                    AND  ( SBD010S02-SRYKA (IDX0)
                                   =   KEY-SRYKA2 )
                       MOVE    IDX0    TO  IDX-S02
                   END-IF
      *
           END-PERFORM
      *
           IF    ( IDX-S02     >   ZERO )
               PERFORM 20021-EDIT-SMEI-SEC
           END-IF
      *
           .
       2002-EDIT-SMEI-EXT.
           EXIT.
      *****************************************************************
      *    未収額収納明細編集処理
      *****************************************************************
       20021-EDIT-SMEI-SEC             SECTION.
      *
           COMPUTE SBD010S02-ZENHKNMISYU (IDX-S02)
               =   SBD010S02-ZENHKNMISYU (IDX-S02)
               +   SBD010S03-HKNSEIKYU   (IDX-S03)
               +   SBD010S03-HKNGENMEN   (IDX-S03)
               +   SBD010S03-HKNCHOSEI   (IDX-S03)
               -   SBD010S03-HKNNYUKIN   (IDX-S03)
      *
           COMPUTE SBD010S02-ZENJIHMISYU (IDX-S02)
               =   SBD010S02-ZENJIHMISYU (IDX-S02)
               +   SBD010S03-JIHSEIKYU   (IDX-S03)
               +   SBD010S03-JIHGENMEN   (IDX-S03)
               +   SBD010S03-JIHCHOSEI   (IDX-S03)
               -   SBD010S03-JIHNYUKIN   (IDX-S03)
      *
           .
       20021-EDIT-SMEI-EXT.
           EXIT.
      *****************************************************************
      *    未収額編集処理
      *****************************************************************
       20021-EDIT-MISYU-SEC            SECTION.
      *
      *    MOVE    ZERO            TO  WRK-KAISYU
      *    IF    ( WRK-MISYU-ZEN   >=  ZERO )
      *        IF    ( WRK-NYUKIN  >=  ZERO )
      *            IF    ( WRK-MISYU-ZEN   >=  WRK-NYUKIN )
      *                COMPUTE WRK-KAISYU  =   WRK-NYUKIN
      *            ELSE
      *                COMPUTE WRK-KAISYU  =   WRK-MISYU-ZEN
      *            END-IF
      *        ELSE
      *                COMPUTE WRK-KAISYU  =   WRK-NYUKIN
      *        END-IF
      *    ELSE
      *        IF    ( WRK-NYUKIN  >=  ZERO )
      *            IF    ( WRK-MISYU-ZEN * -1  >=  WRK-NYUKIN )
      *                COMPUTE WRK-KAISYU  =   WRK-NYUKIN  * -1
      *            ELSE
      *                COMPUTE WRK-KAISYU  =   WRK-MISYU-ZEN
      *            END-IF
      *        ELSE
      *                COMPUTE WRK-KAISYU  =   WRK-NYUKIN
      *        END-IF
      *    END-IF
      *
      *    COMPUTE WRK-MISYU-KON
      *        =   WRK-SEIKYU  - ( WRK-NYUKIN  -   WRK-KAISYU )
      *---------
      *
      *
      *
      *
      *
           MOVE    ZERO            TO  WRK-KAISYU
           MOVE    ZERO            TO  WRK-NYUKIN-KON
      *
           IF    ( WRK-SEIKYU      >=  ZERO )
               IF    ( WRK-NYUKIN  >=  ZERO )
                   IF    ( WRK-SEIKYU      >=  WRK-NYUKIN )
                       COMPUTE WRK-NYUKIN-KON  =   WRK-NYUKIN
                   ELSE
                       COMPUTE WRK-NYUKIN-KON  =   WRK-SEIKYU
                   END-IF
               ELSE
                       COMPUTE WRK-NYUKIN-KON  =   0
               END-IF
           ELSE
               IF    ( WRK-NYUKIN  >=  ZERO )
                       COMPUTE WRK-NYUKIN-KON  =   0
               ELSE
                   IF    ( WRK-SEIKYU     >=  WRK-NYUKIN )
                       COMPUTE WRK-NYUKIN-KON  =   WRK-SEIKYU
                   ELSE
                       COMPUTE WRK-NYUKIN-KON  =   WRK-NYUKIN
                   END-IF
               END-IF
           END-IF
      *
           COMPUTE WRK-NYUKIN-ZAN   =  WRK-NYUKIN  -   WRK-NYUKIN-KON
      *
           IF    ( WRK-MISYU-ZEN   >=  ZERO )
               IF    ( WRK-NYUKIN-ZAN  >=  ZERO )
                   IF    ( WRK-MISYU-ZEN   >=  WRK-NYUKIN-ZAN )
                       COMPUTE WRK-KAISYU  =   WRK-NYUKIN-ZAN
                   ELSE
                       COMPUTE WRK-KAISYU  =   WRK-MISYU-ZEN
                   END-IF
               ELSE
                       COMPUTE WRK-KAISYU  =   0
               END-IF
           ELSE
               IF    ( WRK-NYUKIN-ZAN  >=  ZERO )
                   IF    ( WRK-MISYU-ZEN * -1  >=  WRK-NYUKIN-ZAN )
                       COMPUTE WRK-KAISYU  =   0
                   ELSE
                       COMPUTE WRK-KAISYU  =   0
                   END-IF
               ELSE
                   IF    ( WRK-MISYU-ZEN   >=  WRK-NYUKIN-ZAN )
                       COMPUTE WRK-KAISYU  =   WRK-MISYU-ZEN
                   ELSE
                       COMPUTE WRK-KAISYU  =   WRK-NYUKIN-ZAN
                   END-IF
               END-IF
           END-IF
      *
           DISPLAY "WRK-SEIKYU     = " WRK-SEIKYU
           DISPLAY "WRK-NYUKIN     = " WRK-NYUKIN
           DISPLAY "WRK-NYUKIN-KON = " WRK-NYUKIN-KON
           DISPLAY "WRK-NYUKIN-ZAN = " WRK-NYUKIN-ZAN
           DISPLAY "WRK-KAISYU     = " WRK-KAISYU
      *
           COMPUTE WRK-MISYU-KON
               =   WRK-SEIKYU  - ( WRK-NYUKIN  -   WRK-KAISYU )
      *
           .
       20021-EDIT-MISYU-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           DISPLAY 
                   "KEYSRYKA"
              ","  "KEYHOKEN"
              ","  "DENPEDANUM"
              ","  "SRYKA"
              ","  "HOKEN"
              ","  "HKNCOMBI"
              ","  "HKNNUM"
              ","  "HONKZKKBN"
              ","  "KOHNUM"
              ","  "HKNTEN"
              ","  "SEIKYU"
              ","  "NYUKIN"
              ","  "CHOSEI"
              ","  "CHOSEI1"
              ","  "CHOSEI2"
              ","  "GENMEN"
              ","  "HKNSEIKYU"
              ","  "JIHSEIKYU"
              ","  "HKNNYUKIN"
              ","  "JIHNYUKIN"
              ","  "KONNYUKIN"
              ","  "KONHKNNYUKIN"
              ","  "KONJIHNYUKIN"
              ","  "ZENNYUKIN"
              ","  "ZENHKNNYUKIN"
              ","  "ZENJIHNYUKIN"
              ","  "MISYU"
              ","  "HKNMISYU"
              ","  "JIHMISYU"
              ","  "KONMISYU"
              ","  "KONHKNMISYU"
              ","  "KONJIHMISYU"
              ","  "ZENMISYU"
              ","  "ZENHKNMISYU"
              ","  "ZENJIHMISYU"
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
              ","  "SYUEDANUM"
              ","  "RENNUM"
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SBD010S02-MAX )
      *
           DISPLAY 
                   SBD010S02-KEYSRYKA  (IDX0)
              ","  SBD010S02-KEYHOKEN  (IDX0)
              ","  SBD010S02-DENPEDANUM(IDX0)
              ","  SBD010S02-SRYKA(IDX0)
              ","  SBD010S02-HOKEN  (IDX0)
              ","  SBD010S02-HKNCOMBI(IDX0)
              ","  SBD010S02-HKNNUM(IDX0)
              ","  SBD010S02-HONKZKKBN(IDX0)
              ","  SBD010S02-KOHNUM(IDX0)
              ","  SBD010S02-HKNTEN(IDX0)
              ","  SBD010S02-SEIKYU(IDX0)
              ","  SBD010S02-NYUKIN(IDX0)
              ","  SBD010S02-CHOSEI(IDX0)
              ","  SBD010S02-CHOSEI1(IDX0)
              ","  SBD010S02-CHOSEI2(IDX0)
              ","  SBD010S02-GENMEN(IDX0)
              ","  SBD010S02-HKNSEIKYU(IDX0)
              ","  SBD010S02-JIHSEIKYU(IDX0)
              ","  SBD010S02-HKNNYUKIN(IDX0)
              ","  SBD010S02-JIHNYUKIN(IDX0)
              ","  SBD010S02-KONNYUKIN(IDX0)
              ","  SBD010S02-KONHKNNYUKIN(IDX0)
              ","  SBD010S02-KONJIHNYUKIN(IDX0)
              ","  SBD010S02-ZENNYUKIN(IDX0)
              ","  SBD010S02-ZENHKNNYUKIN(IDX0)
              ","  SBD010S02-ZENJIHNYUKIN(IDX0)
              ","  SBD010S02-MISYU   (IDX0)
              ","  SBD010S02-HKNMISYU(IDX0)
              ","  SBD010S02-JIHMISYU(IDX0)
              ","  SBD010S02-KONMISYU(IDX0)
              ","  SBD010S02-KONHKNMISYU(IDX0)
              ","  SBD010S02-KONJIHMISYU(IDX0)
              ","  SBD010S02-ZENMISYU(IDX0)
              ","  SBD010S02-ZENHKNMISYU(IDX0)
              ","  SBD010S02-ZENJIHMISYU(IDX0)
              ","  SBD010S02-ETCJIHI(IDX0 1)
              ","  SBD010S02-ETCJIHI(IDX0 2)
              ","  SBD010S02-ETCJIHI(IDX0 3)
              ","  SBD010S02-ETCJIHI(IDX0 4)
              ","  SBD010S02-ETCJIHI(IDX0 5)
              ","  SBD010S02-ETCJIHI(IDX0 6)
              ","  SBD010S02-ETCJIHI(IDX0 7)
              ","  SBD010S02-ETCJIHI(IDX0 8)
              ","  SBD010S02-ETCJIHI(IDX0 9)
              ","  SBD010S02-ETCJIHI(IDX0 10)
              ","  SBD010S02-HOHO(IDX0 1)
              ","  SBD010S02-HOHOMONEY(IDX0 1)
              ","  SBD010S02-HOHO(IDX0 2)
              ","  SBD010S02-HOHOMONEY(IDX0 2)
              ","  SBD010S02-HOHO(IDX0 3)
              ","  SBD010S02-HOHOMONEY(IDX0 3)
              ","  SBD010S02-HOHO(IDX0 4)
              ","  SBD010S02-HOHOMONEY(IDX0 4)
              ","  SBD010S02-HOHO(IDX0 5)
              ","  SBD010S02-HOHOMONEY(IDX0 5)
              ","  SBD010S02-DAILYKEY(IDX0)
              ","  SBD010S02-SYURRKNUM(IDX0)
              ","  SBD010S02-SYUEDANUM(IDX0)
              ","  SBD010S02-RENNUM   (IDX0)
      *
           END-PERFORM
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    保険の種類取得処理
      *****************************************************************
       800-GET-HOKEN-SEC               SECTION.
      *
           MOVE    IDX-SYAHON        TO  WRK-HOKEN
      *
           EVALUATE    SBD010S03-HKNNUM (IDX-S03)
           WHEN    SPACE
               MOVE    IDX-KOHTAN    TO  WRK-HOKEN
           WHEN    "975"
               MOVE    IDX-KOUGAI    TO  WRK-HOKEN
           WHEN    "973"
               MOVE    IDX-JIBAI     TO  WRK-HOKEN
           WHEN    "971"
               MOVE    IDX-ROUSAI    TO  WRK-HOKEN
           WHEN    "039"
           WHEN    "040"
               MOVE    IDX-KOUKI     TO  WRK-HOKEN
           WHEN    "060"
           WHEN    "067"
           WHEN    "068"
           WHEN    "069"
               IF    ( SBD010S03-KOHNUM (IDX-S03)
                                           =   "027" )
                   MOVE    IDX-KOKRJN TO WRK-HOKEN
               ELSE
                   MOVE    IDX-KOKIPN TO WRK-HOKEN
               END-IF
           WHEN    OTHER
               EVALUATE    TRUE
               WHEN  ( SBD010S03-HKNNUM (IDX-S03) (1:2)
                                           =   "98" )
                   MOVE    IDX-JIHHKN  TO WRK-HOKEN
               WHEN  ( SBD010S03-HKNNUM (IDX-S03) (1:2)
                                           =   "90" OR "91" )
                   MOVE    IDX-CHIKEN  TO WRK-HOKEN
               WHEN  ( SBD010S03-KOHNUM (IDX-S03)
                                           =   "027" )
                   MOVE    IDX-SYARJN  TO WRK-HOKEN
               WHEN  ( SBD010S03-HONKZKKBN (IDX-S03)
                                           =   "1" )
                   MOVE    IDX-SYAHON  TO  WRK-HOKEN
               WHEN  ( SBD010S03-HONKZKKBN (IDX-S03)
                                           =   "2" )
                   MOVE    IDX-SYAKZK  TO  WRK-HOKEN
               END-EVALUATE
           END-EVALUATE
      *
           .
       800-GET-HOKEN-EXT.
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
