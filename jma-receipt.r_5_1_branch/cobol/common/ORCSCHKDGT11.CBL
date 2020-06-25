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
       PROGRAM-ID.             ORCSCHKDGT11.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    :
      *  コンポーネント名  : チェックデジット算出（チェック）
      *                      モジュラス１１
      *  処理区分          :  1:チェックディジット付番
      *                    :  2:チェックディジットチェック
      *  返却エラーコード  : 00:エラーなし
      *                      10:入力パラメータ誤り
      *                      01:入力チェックデジット誤り（処理区分＝２のみ）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−小豆沢   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01.07.10  ６桁以上のときのチェックデジット
      *                                     算出修正
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
           03  FLG-END             PIC X(03).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDX                 PIC 9(03).
           03  IDX2                PIC 9(03).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-2               PIC 9(04).
           03  CNT-3               PIC 9(04).
           03  CNT-4               PIC 9(04).
           03  CNT-5               PIC 9(04).
           03  CNT-6               PIC 9(04).
           03  CNT-7               PIC 9(04).
      *
      *    ワーク領域
       01  WRK-AREA.
      *****03  WRK-CD1             PIC 9(02).
           03  WRK-CD1             PIC 9(01).
           03  WRK-KEI             PIC 9(04).
           03  WRK-GIV             PIC 9(04).
           03  WRK-AMARI           PIC 9(03).
           03  WRK-END             PIC 9(02).
      *
       01  WRK-IN PIC X(01). 
      *    リンク領域
       LINKAGE  SECTION.
      *
           COPY     "CPORCCHKDGT11.INC".
      *
       PROCEDURE                   DIVISION    USING  LNK-CHKDGT-AREA.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   "END"
      *
           PERFORM 300-END-SEC
      *
           EXIT PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    DISPLAY "*** SBCHKDGT  START  ***"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-OUT
      *
           PERFORM     110-PARA-CHECK-SEC
           IF  LNK-O-ERRCD         NOT =   ZERO
               MOVE    "END"           TO  FLG-END
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力パラメータチェック処理
      *****************************************************************
       110-PARA-CHECK-SEC                SECTION.
      *
           IF    LNK-I-SYORIKBN        =   1   OR   2
               CONTINUE
           ELSE
               MOVE    10              TO  LNK-O-ERRCD
               GO      TO              110-PARA-CHECK-EXT
           END-IF
      *
           PERFORM     VARYING     IDX   FROM    1   BY  1
                       UNTIL      (IDX               >   100  )
                       OR         (LNK-I-CODE(IDX:1) =   SPACE)
               MOVE    IDX               TO  WRK-END
           END-PERFORM
           IF    LNK-I-CODE(1:WRK-END)   IS  NUMERIC
               CONTINUE
           ELSE
               MOVE    10              TO  LNK-O-ERRCD
               GO      TO              110-PARA-CHECK-EXT
           END-IF
           IF    (LNK-I-SYORIKBN       =   1 )  AND
                 (WRK-END              >   99)
               MOVE    10              TO  LNK-O-ERRCD
               GO      TO              110-PARA-CHECK-EXT
           END-IF
      *
           IF    (LNK-I-SYORIKBN       =   2    )  AND
                 (LNK-I-CODE(2:1)      =   SPACE)
               MOVE    10              TO  LNK-O-ERRCD
               GO      TO              110-PARA-CHECK-EXT
           END-IF
           .
       110-PARA-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    ＜チェックデジット算出＞
           IF    LNK-I-SYORIKBN    =   1
               CONTINUE
           ELSE
      *    ＜チェックデジット算出後、比較＞
      *    　　コードの最後から２番目のインデックス取得しておく
               PERFORM     VARYING     IDX   FROM    1   BY  1
                           UNTIL      (IDX               >   100  )
                           OR         (LNK-I-CODE(IDX:1) =   SPACE)
                   MOVE    IDX               TO  WRK-END
               END-PERFORM
               COMPUTE     WRK-END     =   WRK-END   -   1
               IF    WRK-END           =   ZERO
                   MOVE    "END"           TO  FLG-END
                   GO      TO              200-MAIN-EXT
               END-IF
           END-IF
      *
           MOVE    ZERO            TO  IDX2
           PERFORM     VARYING     IDX   FROM    WRK-END   BY  -1
                       UNTIL      (IDX           <     1)
      *
               ADD     1           TO    IDX2
      *---- (01.00.01) LINE MOD START ---------------------------- 
      ******** IF      IDX2        >     7
               IF      IDX2        >     6
      *---- (01.00.01) LINE MOD END   ---------------------------- 
                       MOVE   1    TO    IDX2
               END-IF
               MOVE    LNK-I-CODE (IDX:1)    TO  WRK-CD1
      *
               EVALUATE    IDX2
                   WHEN    1
                           ADD    WRK-CD1    TO   CNT-2
                   WHEN    2
                           ADD    WRK-CD1    TO   CNT-3
                   WHEN    3
                           ADD    WRK-CD1    TO   CNT-4
                   WHEN    4
                           ADD    WRK-CD1    TO   CNT-5
                   WHEN    5
                           ADD    WRK-CD1    TO   CNT-6
                   WHEN    6
                           ADD    WRK-CD1    TO   CNT-7
               END-EVALUATE
           END-PERFORM
      *
           MOVE    ZERO            TO  WRK-KEI
           COMPUTE   WRK-KEI   =  (CNT-2 * 2)    +
                                  (CNT-3 * 3)    +
                                  (CNT-4 * 4)    +
                                  (CNT-5 * 5)    +
                                  (CNT-6 * 6)    +
                                  (CNT-7 * 7)
           DIVIDE    11  INTO    WRK-KEI   GIVING      WRK-GIV
                                           REMAINDER   WRK-AMARI

           IF    WRK-AMARI         =   ZERO   OR   1
               MOVE    ZERO            TO  LNK-O-CHKDGT
           ELSE
               COMPUTE   LNK-O-CHKDGT  =   11    -   WRK-AMARI
           END-IF
      *
      *    ＜チェックデジット算出＞
           IF    LNK-I-SYORIKBN    =   1
               MOVE    LNK-I-CODE      TO  LNK-O-CODE
               ADD     1               TO  WRK-END
               MOVE    LNK-O-CHKDGT    TO  LNK-O-CODE(WRK-END:1)
               MOVE    ZERO            TO  LNK-O-ERRCD
           ELSE
      *    ＜チェックデジット算出後、比較＞
               ADD     1               TO  WRK-END
               MOVE    LNK-I-CODE (WRK-END:1)
                                           TO  WRK-CD1
               IF    LNK-O-CHKDGT      =   WRK-CD1
                   MOVE    LNK-I-CODE      TO  LNK-O-CODE
                   MOVE    ZERO            TO  LNK-O-ERRCD
               ELSE
                   MOVE    LNK-I-CODE      TO  LNK-O-CODE
                   MOVE    LNK-O-CHKDGT    TO  LNK-O-CODE (WRK-END:1)
                   MOVE    01              TO  LNK-O-ERRCD
               END-IF
           END-IF
      *
           MOVE    "END"           TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    DISPLAY "I-KBN ["  LNK-I-SYORIKBN "] CODE[" LNK-I-CODE "]"
      *    DISPLAY "O-CODE["  LNK-O-CODE "] ERR[" LNK-O-ERRCD "]"
      *    DISPLAY "*** ORCSCHKDGT11    END  ***"
      *    STOP    " "
           .
       300-END-EXT.
           EXIT.
      *
