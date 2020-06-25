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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSNUM.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通サブ
      *  コンポーネント名  : 数字変換
      *  管理者            : 
      *  作成日付    作業者        記述
      *  XX.XX.XX    ＮＮＮ        新規作成
      *                                                               *
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
       WORKING-STORAGE             SECTION.
       01  WK-IDX.
           02  WK-IDX01                         PIC S9(002).
           02  WK-IDX02                         PIC S9(002).
           02  WK-IDX03                         PIC S9(002).
           02  WK-IDX04                         PIC S9(002).
           02  WK-IDX05                         PIC S9(002).
       01  WK-TBL-AREA.
           02   WK-SEISUU1T.
               03  WK-SEISUU1T-AREA             PIC X(001) OCCURS 26.
           02   WK-SYOSUUT.
               03  WK-SYOSUUT-AREA              PIC X(001) OCCURS 6.
           02   WK-SEISUU2T.
               03   WK-INT1T.
                   04  WK-INT1T-AREA            PIC X(001) OCCURS 18.
               03  WK-INT2T                     PIC 9(018).
               03   WK-INT3T            REDEFINES    WK-INT2T.
                   04  WK-INT3T-AREA            PIC X(001) OCCURS 18.
               03   WK-INT4T.
                   04  WK-INT4T-AREA            PIC X(001) OCCURS 18.
           02   WK-INPT.
               03  WK-INPT-ITM                  PIC X(001) OCCURS 26.
       01   WK-SW-AREA.
           02  WK-SEISW                         PIC X(003).
           02  WK-SYOSW                         PIC X(005).
           02  WK-NUMSW                         PIC X(003).
           02  WK-KIGSW                         PIC X(003).
           02  WK-KIG                           PIC X(001).
           02  WK-INPITM                        PIC 9(002).
       01   WK-EDT.
           02  WK-EDT1                          PIC 9(018).
           02  WK-EDT2                          PIC ZZZZZZZZZZZZZZZZZ9.
           02  WK-EDT3                          PIC ZZ,ZZZ,ZZZ,ZZZ,ZZ9.
           02  WK-EDT4                          PIC ZZ,ZZZ,ZZZ,ZZZ,ZZZ.
           02  WK-EDT5                          PIC ******************.
           02  WK-EDT6                          PIC *****************9.
           02  WK-EDT7                          PIC **,***,***,***,***.
           02  WK-EDT8                          PIC **,***,***,***,**9.
       01  WK-AREA.
           02  WK-MINUS                         PIC -(14)9.
           02  WK-OUTSIGN.
               03  WK-OUTSIGN1                  PIC 9(012).
               03  WK-OUTSIGN2                  PIC 9(006).
           02  WK-OUTSIGNR             REDEFINES
               WK-OUTSIGN                       PIC 9(12)V9(06).
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY     "CPORCSNUM.INC".
      *
       PROCEDURE                   DIVISION
                                   USING   ORCSNUMAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *    初期処理
           PERFORM INIT-SEC
      *    チェック処理
           PERFORM CHECK-SEC
      *
           IF      SNUM-RC             =   ZERO
      *        整数チェック処理
               PERFORM SEI-SEC
           END-IF
      *
           IF      SNUM-RC             =   ZERO
      *        小数チェック処理
               PERFORM SYO-SEC
           END-IF
      *
           IF      SNUM-RC             =   ZERO
      *        編集処理
               PERFORM TERM-SEC
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *****************************************************************
      *    初期  処理
      *****************************************************************
       INIT-SEC                        SECTION.
      *
           INITIALIZE                   WK-IDX
           INITIALIZE                   WK-TBL-AREA
           INITIALIZE                   WK-SW-AREA
           INITIALIZE                   WK-AREA
           INITIALIZE                   WK-EDT
      *
           MOVE    ZERO                TO  WK-SYOSUUT
           MOVE    ZERO                TO  WK-INT2T
           MOVE    ALL "*"             TO  WK-INT4T
      *
           MOVE    ZERO                TO  SNUM-OUTNUM
           MOVE    ZERO                TO  SNUM-SEISUU
           MOVE    ZERO                TO  SNUM-SYOSUU
           MOVE    ZERO                TO  SNUM-KETSUU
           MOVE    SPACE               TO  SNUM-OUTEDT
           MOVE    SPACE               TO  SNUM-MINKBN
           MOVE    SPACE               TO  SNUM-SYOKBN
           MOVE    ZERO                TO  SNUM-RC
           .
       INIT-EXT.
           EXIT.
      *****************************************************************
      *    チェック 処理
      *****************************************************************
       CHECK-SEC                      SECTION.
           MOVE    SPACE               TO  WK-SEISW
           MOVE    SPACE               TO  WK-SYOSW
           MOVE    SPACE               TO  WK-NUMSW
           MOVE    SPACE               TO  WK-KIGSW
           MOVE    ZERO                TO  WK-IDX02
           MOVE    ZERO                TO  WK-IDX03
           MOVE    SNUM-INX            TO  WK-INPT
           PERFORM VARYING     WK-IDX01    FROM    1   BY  1
                   UNTIL       WK-IDX01    >   26
               PERFORM TBL-SEC
           END-PERFORM
           .
       CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック 処理
      *****************************************************************
       TBL-SEC                      SECTION.
      *
           IF      (WK-INPT-ITM (WK-IDX01) =   SPACE )
               AND (WK-IDX02               =   ZERO  )
               AND (WK-IDX03               =   ZERO  )
               AND (WK-SEISW               =   SPACE )
               AND (WK-SYOSW               =   SPACE )
               GO      TO      TBL-EXT
           END-IF
      *    数字チェック
           IF      (WK-INPT-ITM (WK-IDX01)     NUMERIC)
               OR  (WK-INPT-ITM (WK-IDX01) =  "*"   OR
                                              "+"   OR
                                              "-"   OR
                                              "\"   OR
                                              ","   OR
                                              "."   OR
                                              " " )
               CONTINUE
           ELSE
               MOVE    1               TO  SNUM-RC
               MOVE    26              TO  WK-IDX01
               GO      TO      TBL-EXT
           END-IF
      *
           IF      (WK-INPT-ITM (WK-IDX01) NUMERIC)  AND
                   (WK-NUMSW           =   SPACE  )
               MOVE    "ON"            TO  WK-NUMSW
           END-IF
           IF      (WK-INPT-ITM (WK-IDX01) =   "+"  OR
                                               "-"  OR
                                               "\" )
               IF      WK-KIGSW            =   SPACE
                   MOVE    "ON"            TO  WK-KIGSW
               ELSE
                   MOVE    1               TO  SNUM-RC
                   MOVE    26              TO  WK-IDX01
               END-IF
           END-IF
           IF      (WK-INPT-ITM (WK-IDX01) =   "*"  OR
                                               "+"  OR
                                               "-"  OR
                                               "\"  )  AND
                   (WK-NUMSW               =   "ON" )
               MOVE    1               TO  SNUM-RC
               MOVE    26              TO  WK-IDX01
           END-IF
           IF      SNUM-RC             =   1
               GO      TO      TBL-EXT
           END-IF
      *
           IF      WK-SEISW            =     SPACE
               EVALUATE    WK-INPT-ITM (WK-IDX01)
                   WHEN    "."
                       MOVE    "ON"        TO  WK-SEISW
                       MOVE    "1"         TO  SNUM-SYOKBN
                   WHEN    SPACE
                       MOVE    "ON"        TO  WK-SEISW
                       MOVE    "ON"        TO  WK-SYOSW
                   WHEN    OTHER
                       COMPUTE WK-IDX02    =   WK-IDX02    +   1
                       IF      WK-IDX02        >   18
                           MOVE    1               TO  SNUM-RC
                           MOVE    26              TO  WK-IDX01
                       ELSE
                           MOVE    WK-INPT-ITM (WK-IDX01)
                                               TO  WK-SEISUU1T-AREA
                                                             (WK-IDX02)
                       END-IF
               END-EVALUATE
           ELSE
               IF      WK-SYOSW            =     SPACE
                   EVALUATE    WK-INPT-ITM (WK-IDX01)
                       WHEN    "."
                           MOVE    1           TO  SNUM-RC
                           MOVE    26          TO  WK-IDX01
                       WHEN    SPACE
                           MOVE    "ON"        TO  WK-SYOSW
                       WHEN    "0"
                       WHEN    "1"
                       WHEN    "2"
                       WHEN    "3"
                       WHEN    "4"
                       WHEN    "5"
                       WHEN    "6"
                       WHEN    "7"
                       WHEN    "8"
                       WHEN    "9"
                           COMPUTE WK-IDX03    =   WK-IDX03    +   1
                           IF      WK-IDX03        >   6
                               MOVE    1               TO  SNUM-RC
                               MOVE    26              TO  WK-IDX01
                           ELSE
                               MOVE    WK-INPT-ITM (WK-IDX01)
                                                   TO  WK-SYOSUUT-AREA
                                                             (WK-IDX03)
                           END-IF
                       WHEN    OTHER
                           MOVE    1           TO  SNUM-RC
                           MOVE    26          TO  WK-IDX01
                   END-EVALUATE
               ELSE
                   IF      WK-INPT-ITM (WK-IDX01) NOT =  SPACE
                       MOVE    1           TO  SNUM-RC
                       MOVE    26          TO  WK-IDX01
                    END-IF
               END-IF
           END-IF
           .
       TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    整数編集処理
      *****************************************************************
       SEI-SEC                      SECTION.
      *
           IF      WK-SEISUU1T-AREA (1)    =   "*"
               PERFORM ASTA-SEC
           ELSE
               PERFORM NORMAL-SEC
               IF     (WK-SEISUU1T-AREA (1)    =   "-" )  AND
                      (SNUM-RC                 =   ZERO)
                   MOVE    "1"             TO  SNUM-MINKBN
               END-IF
           END-IF
           .
       SEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    整数編集処理
      *****************************************************************
       NORMAL-SEC                      SECTION.
      *
           MOVE    19                  TO  WK-IDX04
           MOVE    19                  TO  WK-IDX05
           PERFORM VARYING WK-IDX01    FROM    WK-IDX02  BY  -1
                   UNTIL  WK-IDX01     <   1
               IF      WK-SEISUU1T-AREA (WK-IDX01) >=  "0"  AND
                                                   <=  "9"
                   COMPUTE  WK-IDX04   =   WK-IDX04 -     1
                   MOVE    WK-SEISUU1T-AREA (WK-IDX01)
                                       TO  WK-INT1T-AREA (WK-IDX04)
                   COMPUTE  WK-IDX05   =   WK-IDX05 -     1
                   MOVE    WK-SEISUU1T-AREA (WK-IDX01)
                                       TO  WK-INT3T-AREA (WK-IDX05)
               ELSE
                   IF      WK-IDX01        NOT =  1
                       COMPUTE  WK-IDX04   =   WK-IDX04 -     1
                       MOVE    WK-SEISUU1T-AREA (WK-IDX01)
                                          TO  WK-INT1T-AREA (WK-IDX04)
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    WK-INT2T            TO  WK-EDT1
           MOVE    WK-INT2T            TO  WK-EDT2
           MOVE    WK-INT2T            TO  WK-EDT3
           MOVE    WK-INT2T            TO  WK-EDT4
           IF     (WK-INT2T            =   WK-EDT1 )  OR
                  (WK-INT1T            =   WK-EDT2 )  OR
                  (WK-INT1T            =   WK-EDT3 )  OR
                  (WK-INT1T            =   WK-EDT4 )
               PERFORM VARYING WK-IDX01    FROM    1   BY  1
                       UNTIL   WK-IDX01    >   18
                   IF      WK-INT3T-AREA (WK-IDX01) NOT =  "0"
                       COMPUTE SNUM-SEISUU =   19  -   WK-IDX01
                       MOVE    18          TO  WK-IDX01
                   END-IF
               END-PERFORM
           ELSE
               MOVE    3               TO  SNUM-RC
           END-IF
           .
       NORMAL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＊編集処理
      *****************************************************************
       ASTA-SEC                      SECTION.
      *
           MOVE    19                  TO  WK-IDX04
           MOVE    19                  TO  WK-IDX05
           PERFORM VARYING WK-IDX01    FROM    WK-IDX02  BY  -1
                   UNTIL   WK-IDX01    <   1
               COMPUTE  WK-IDX04   =   WK-IDX04 -     1
               MOVE    WK-SEISUU1T-AREA (WK-IDX01)
                                       TO  WK-INT4T-AREA (WK-IDX04)
               IF      WK-SEISUU1T-AREA (WK-IDX01) >=  "0"  AND
                                                   <=  "9"
                   COMPUTE  WK-IDX05   =   WK-IDX05 -     1
                   MOVE    WK-SEISUU1T-AREA (WK-IDX01)
                                       TO  WK-INT3T-AREA (WK-IDX05)
               END-IF
           END-PERFORM
      *
           MOVE    WK-INT2T            TO  WK-EDT5
           MOVE    WK-INT2T            TO  WK-EDT6
           MOVE    WK-INT2T            TO  WK-EDT7
           MOVE    WK-INT2T            TO  WK-EDT8
           IF     (WK-INT4T            =   WK-EDT6 )  OR
                  (WK-INT4T            =   WK-EDT7 )  OR
                  (WK-INT4T            =   WK-EDT8 )
               PERFORM VARYING WK-IDX01    FROM    1   BY  1
                       UNTIL   WK-IDX01    >   18
                   IF      WK-INT3T-AREA (WK-IDX01) NOT =  "0"
                       COMPUTE SNUM-SEISUU =   18  -   WK-IDX01
                       MOVE    18          TO  WK-IDX01
                   END-IF
               END-PERFORM
           ELSE
               MOVE    3               TO  SNUM-RC
           END-IF
           .
       ASTA-EXT.
           EXIT.
      *
      *****************************************************************
      *    小数編集処理
      *****************************************************************
       SYO-SEC                      SECTION.
           PERFORM VARYING WK-IDX01    FROM    6   BY -1
                   UNTIL   WK-IDX01    <   1
               IF      WK-SYOSUUT-AREA (WK-IDX01) NOT =  "0"
                   MOVE    WK-IDX01        TO  SNUM-SYOSUU
                   MOVE    ZERO            TO  WK-IDX01
                END-IF
           END-PERFORM
           .
       SYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    編集処理
      *****************************************************************
       TERM-SEC                      SECTION.
      *
           MOVE    WK-INT2T            TO  WK-OUTSIGN1
           MOVE    WK-SYOSUUT          TO  WK-OUTSIGN2
           MOVE    WK-OUTSIGNR         TO  SNUM-OUTNUM
           IF      SNUM-MINKBN         =   "1"
               COMPUTE SNUM-OUTNUM     =   SNUM-OUTNUM     *   -1
           END-IF
           MOVE    SNUM-OUTNUM         TO  WK-MINUS
           MOVE    WK-MINUS            TO  SNUM-OUTEDT
           INSPECT SNUM-OUTEDT CONVERTING  " "  TO  "0"
           IF      SNUM-SEISUU         >   13
               MOVE    3               TO  SNUM-RC
           END-IF
           COMPUTE SNUM-KETSUU         =   SNUM-SEISUU
                                       +   SNUM-SYOSUU
           .
       TERM-EXT.
           EXIT.
      *
      *****************************************************************
