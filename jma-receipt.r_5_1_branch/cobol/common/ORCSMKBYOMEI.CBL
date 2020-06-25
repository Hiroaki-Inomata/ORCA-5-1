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
       PROGRAM-ID.                     ORCSMKBYOMEI.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 病名編集サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/11/30    NACL−太田    新規作成
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
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *
       WORKING-STORAGE                 SECTION.
      *
       01  FLG-AREA.
           03  FLG-KYUSEI              PIC 9(01).
           03  FLG-UTAGAI              PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC S9(05).
           03  IDX1                    PIC 9(05).
           03  IDX-PNT                 PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-BYOMEI              PIC X(240).
      *
       01  CONST-AREA.
           03  CONST-DEBUG             PIC 9(01)   VALUE   0.
           03  CONST-LEN               PIC 9(05)   VALUE   160.
           03  CONST-CD-MAX            PIC 9(05)   VALUE   21.
           03  CONST-UTAGAICD          PIC X(07)   VALUE   "ZZZ8002".
           03  CONST-KYUSEICD          PIC X(07)   VALUE   "ZZZ4012".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                        SECTION.
      *
           COPY    "CPORCSMKBYOMEI.INC".
      *
       PROCEDURE                       DIVISION    USING
           ORCSMKBYOMEIAREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           IF    ( CONST-DEBUG     =   1 )
               PERFORM 800-DISPLAY-IN-SEC
           END-IF
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           PERFORM 200-MAIN-SEC
      *
      *    終了処理
           PERFORM 300-END-SEC
      *
           IF    ( CONST-DEBUG     =   1 )
               PERFORM 800-DISPLAY-OT-SEC
           END-IF
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
                                       SMKBYO-OT
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 2001-EDIT-BYOMEI-SEC
      *
           IF    ( SMKBYO-BYOMEICD (1)     NOT =   SPACE )
               PERFORM 2001-EDIT-BYOMEICD-SEC
           END-IF
      *
           IF    ( SMKBYO-HOSOKU-COMMENT
                                           NOT =   SPACE )
               PERFORM 2001-EDIT-HOSOKU-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名編集処理
      *****************************************************************
       2001-EDIT-BYOMEI-SEC            SECTION.
      *
           PERFORM 210-GET-BYO-LENGTH-SEC
      *
           PERFORM 210-CHK-UTAGAI-SEC
      *
           PERFORM 210-CHK-KYUSEI-SEC
      *
           IF   ( SMKBYO-UTAGAIFLG    =   1  OR 3 )
               IF    ( FLG-UTAGAI     =   0 )
                   MOVE    1               TO  SMKBYO-STS-MODIFY
                   MOVE    1               TO  SMKBYO-STS-UTAGAI
      *
                   MOVE    SMKBYO-BYOMEI   TO  WRK-BYOMEI
                   MOVE    SPACE           TO  SMKBYO-BYOMEI
                   MOVE    1               TO  IDX-PNT
                   STRING  WRK-BYOMEI (1:SMKBYO-LENGTH)
                                           DELIMITED   BY  SIZE
                           "の疑い"        DELIMITED   BY  SIZE
                   INTO    SMKBYO-BYOMEI
                   WITH    POINTER IDX-PNT
                   END-STRING
      *
                   COMPUTE SMKBYO-LENGTH   =   IDX-PNT -   1
      *
               END-IF
           END-IF
      *
           IF   ( SMKBYO-UTAGAIFLG    =   2  OR 3 )
               IF    ( FLG-KYUSEI     =   0 )
                   MOVE    1               TO  SMKBYO-STS-MODIFY
                   MOVE    1               TO  SMKBYO-STS-KYUSEI
      *
                   MOVE    SMKBYO-BYOMEI   TO  WRK-BYOMEI
                   MOVE    SPACE           TO  SMKBYO-BYOMEI
                   MOVE    1               TO  IDX-PNT
                   STRING  "急性"          DELIMITED   BY  SIZE
                           WRK-BYOMEI (1:SMKBYO-LENGTH)
                                           DELIMITED   BY  SIZE
                   INTO    SMKBYO-BYOMEI
                   WITH    POINTER IDX-PNT
                   END-STRING
      *
                   COMPUTE SMKBYO-LENGTH   =   IDX-PNT -   1
      *
               END-IF
           END-IF
      *
           .
       2001-EDIT-BYOMEI-EXT.
           EXIT.
      *****************************************************************
      *    病名コード編集処理
      *****************************************************************
       2001-EDIT-BYOMEICD-SEC          SECTION.
      *
           IF    ( SMKBYO-STS-UTAGAI   =   1 )
               IF    ( SMKBYO-BYOMEICD (CONST-CD-MAX ) NOT =   SPACE )
                   MOVE    "1"         TO  SMKBYO-BYOMEIHENFLG
               ELSE
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL ( IDX1    >   CONST-CD-MAX )
      *
                       IF    ( SMKBYO-BYOMEICD (IDX1)  =   SPACE )
                           MOVE    CONST-UTAGAICD
                                           TO  SMKBYO-BYOMEICD (IDX1)
                           MOVE    CONST-CD-MAX
                                           TO  IDX1
                       END-IF
      *
                   END-PERFORM
               END-IF
           END-IF
      *
           IF    ( SMKBYO-STS-KYUSEI   =   1 )
               IF    ( SMKBYO-BYOMEICD (CONST-CD-MAX ) NOT =   SPACE )
                   MOVE    "1"         TO  SMKBYO-BYOMEIHENFLG
               ELSE
                   PERFORM VARYING IDX1
                           FROM    CONST-CD-MAX    BY  -1
                           UNTIL ( IDX1    < 2 )
      *
                       MOVE    SMKBYO-BYOMEICD (IDX1 - 1)
                                       TO  SMKBYO-BYOMEICD (IDX1)
      *
                   END-PERFORM
      *
                   MOVE    CONST-KYUSEICD
                                       TO  SMKBYO-BYOMEICD (1)
      *
               END-IF
           END-IF
      *
           .
       2001-EDIT-BYOMEICD-EXT.
           EXIT.
      *****************************************************************
      *    補足病名編集処理
      *****************************************************************
       2001-EDIT-HOSOKU-SEC            SECTION.
      *
           MOVE    SMKBYO-BYOMEI       TO  WRK-BYOMEI
           MOVE    SPACE               TO  SMKBYO-BYOMEI
           MOVE    1                   TO  IDX-PNT
      *
           STRING  WRK-BYOMEI          DELIMITED BY SPACE
                   "（"                DELIMITED BY SIZE
                   SMKBYO-HOSOKU-COMMENT
                                       DELIMITED BY SPACE
                   "）"                DELIMITED BY SIZE
           INTO    SMKBYO-BYOMEI
           WITH    POINTER IDX-PNT
           END-STRING
      *
           COMPUTE SMKBYO-LENGTH   =   IDX-PNT -   1
      *
           .
       2001-EDIT-HOSOKU-EXT.
           EXIT.
      *****************************************************************
      *    病名文字列長取得処理
      *****************************************************************
       210-GET-BYO-LENGTH-SEC          SECTION.
      *
           MOVE    ZERO            TO  SMKBYO-LENGTH
      *
           PERFORM VARYING IDX0    FROM    CONST-LEN   BY  -1
                   UNTIL ( IDX0            <   1 )
                    OR   ( SMKBYO-LENGTH   >   ZERO )
      *
               IF    ( SMKBYO-BYOMEI (IDX0 : 1) NOT =   SPACE )
                   MOVE    IDX0    TO  SMKBYO-LENGTH
               END-IF
      *
           END-PERFORM
      *
           .
       210-GET-BYO-LENGTH-EXT.
           EXIT.
      *****************************************************************
      *    疑い文字チェック
      *****************************************************************
       210-CHK-UTAGAI-SEC              SECTION.
      *
      *    "疑"をチェック
           PERFORM VARYING IDX0    FROM    1   BY  2
                   UNTIL ( IDX0        >   SMKBYO-LENGTH   -   1)
                    OR   ( FLG-UTAGAI  =   1 )
      *
               IF    ( SMKBYO-BYOMEI (IDX0 :2)  =   "疑" )
                   MOVE    1           TO  FLG-UTAGAI
               END-IF
      *
           END-PERFORM
      *
           .
       210-CHK-UTAGAI-EXT.
           EXIT.
      *****************************************************************
      *    急性文字チェック
      *****************************************************************
       210-CHK-KYUSEI-SEC              SECTION.
      *
      *    "急性"をチェック
           PERFORM VARYING IDX0    FROM    1   BY  2
                   UNTIL ( IDX0        >   SMKBYO-LENGTH   -   3)
                    OR   ( FLG-KYUSEI  =   1 )
      *
               IF    ( SMKBYO-BYOMEI (IDX0:4)  =   "急性" )
                   MOVE    1           TO  FLG-KYUSEI
               END-IF
      *
           END-PERFORM
      *
           .
       210-CHK-KYUSEI-EXT.
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
      *
      *****************************************************************
      *    引数表示
      *****************************************************************
       800-DISPLAY-IN-SEC              SECTION.
      *
           DISPLAY "== IN == "
           DISPLAY "UTAGAIFLG   :" SMKBYO-UTAGAIFLG
           DISPLAY "BYOMEI      :" SMKBYO-BYOMEI
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-CD-MAX )
               DISPLAY "BYOMEICD (" IDX0 ")" SMKBYO-BYOMEICD (IDX0)
           END-PERFORM
      *
           DISPLAY "BYOMEIHENFLG:" SMKBYO-BYOMEIHENFLG
      *
           .
       800-DISPLAY-IN-EXT.
           EXIT.
      *****************************************************************
      *    引数表示
      *****************************************************************
       800-DISPLAY-OT-SEC              SECTION.
      *
           DISPLAY "== OT == "
           DISPLAY "BYOMEI      :" SMKBYO-BYOMEI
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-CD-MAX )
               DISPLAY "BYOMEICD (" IDX0 ")" SMKBYO-BYOMEICD  (IDX0)
           END-PERFORM
      *
           DISPLAY "BYOMEIHENFLG:" SMKBYO-BYOMEIHENFLG
           DISPLAY "LENGTH      :" SMKBYO-LENGTH
           DISPLAY "STS-MODIFY  :" SMKBYO-STS-MODIFY
           DISPLAY "STS-UTAGAI  :" SMKBYO-STS-UTAGAI
           DISPLAY "STS-KYUSEI  :" SMKBYO-STS-KYUSEI
      *
           .
       800-DISPLAY-OT-EXT.
           EXIT.
      *
