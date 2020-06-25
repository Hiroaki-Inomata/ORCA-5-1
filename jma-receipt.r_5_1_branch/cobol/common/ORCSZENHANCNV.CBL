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
       PROGRAM-ID.             ORCSZENHANCNV.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 全角→半角変換処理
      *  管理者            : 
      *  08/09/16    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.00.00     NACL-多々納  19/04/01  新元号対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
           03  FLG-YM              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDW                 PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDXY                PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-HENKAN-AREA.
               05  WRK-HENKAN-IN   PIC X(50).
               05  WRK-HENKAN-OUT  PIC X(25).
               05  WRK-HENKAN-LEN  PIC 9(02).
      *
               05  WRK-MOJI-IN     PIC X(50).
               05  WRK-MOJI-OUT    PIC X(25).
               05  WRK-MOJI-LEN    PIC 9(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY   "CPORCXKANACONV.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSZENHANCNV.INC".
      *****************************************************************
     *
       PROCEDURE                   DIVISION    USING
                                   ORCSZENHANCNVAREA
           .
     *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    ZERO                TO  LNK-ZENHANCNV-RC    
      *    処理区分により検索処理
           EVALUATE    LNK-ZENHANCNV-SYORI
               WHEN    "1"
                   PERFORM 200-YMD-HENSYU-SEC
               WHEN    "2"
                   PERFORM 200-NUM-HENSYU-SEC
               WHEN    "3"
                   PERFORM 200-MOJI-HENSYU-SEC
               WHEN    OTHER
                   MOVE    1               TO  LNK-ZENHANCNV-RC
           END-EVALUATE
      *
           IF      LNK-ZENHANCNV-RC    >   0
               INITIALIZE                  LNK-ZENHANCNV-OUT
           END-IF
      *??
           IF      LNK-ZENHANCNV-RC    >   0
               DISPLAY "SYORI="  LNK-ZENHANCNV-SYORI
               DISPLAY "DATA ="  LNK-ZENHANCNV-IN-DATA
               DISPLAY "LEN  ="  LNK-ZENHANCNV-IN-LEN
               DISPLAY "LEN1 ="  LNK-ZENHANCNV-IN-LEN1
               DISPLAY "RC   ="  LNK-ZENHANCNV-RC
           END-IF
      ***  DISPLAY "SYMD ="  LNK-ZENHANCNV-OT-SYMD
      ***  DISPLAY "WYMD ="  LNK-ZENHANCNV-OT-WYMD
      ***  DISPLAY "GYMD ="  LNK-ZENHANCNV-OT-GYMD
      ***  DISPLAY "DATA ="  LNK-ZENHANCNV-OT-DATA
      ***  DISPLAY "LEN  ="  LNK-ZENHANCNV-OT-LEN
      ***  DISPLAY "NUM  ="  LNK-ZENHANCNV-OT-NUM
      *??
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *     
      *****************************************************************
      *    日付変換処理
      *****************************************************************
       200-YMD-HENSYU-SEC       SECTION.
      *
           IF      LNK-ZENHANCNV-IN-LEN
                                   NOT =   LNK-ZENHANCNV-IN-LEN1
              MOVE    9               TO  LNK-ZENHANCNV-RC
              GO  TO  200-YMD-HENSYU-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-YM
      *
           IF      LNK-ZENHANCNV-IN-DATA (1:4) 
                                       =   "平成"  OR  "昭和"  OR
                                           "大正"  OR  "明治"
      *
      *2019.5.1からの新元号対応（令和）
                                       OR  "令和"
      *
               PERFORM 2001-YMD-WAREKI-HENSYU-SEC
           ELSE
               IF      LNK-ZENHANCNV-IN-DATA (1:2) 
                                           =   "２"
                   PERFORM 2002-YMD-SEIREKI-HENSYU-SEC
               ELSE
                   MOVE    2               TO  LNK-ZENHANCNV-RC
                   GO  TO  200-YMD-HENSYU-EXT
               END-IF
           END-IF
           .
       200-YMD-HENSYU-EXT.
           EXIT.
      *     
      *****************************************************************
      *    和歴変換処理
      *****************************************************************
       2001-YMD-WAREKI-HENSYU-SEC     SECTION.
      *
           IF      LNK-ZENHANCNV-IN-DATA (17:4)
                                       =   SPACE
               MOVE    1                   TO  FLG-YM
           END-IF
      *
           EVALUATE    LNK-ZENHANCNV-IN-DATA (1:4) 
               WHEN    "平成"
                   MOVE    "4"         TO  LNK-ZENHANCNV-OT-WYMD (1:1)
               WHEN    "昭和"
                   MOVE    "3"         TO  LNK-ZENHANCNV-OT-WYMD (1:1)
               WHEN    "大正"
                   MOVE    "2"         TO  LNK-ZENHANCNV-OT-WYMD (1:1)
               WHEN    "明治"
                   MOVE    "1"         TO  LNK-ZENHANCNV-OT-WYMD (1:1)
      *2019.5.1からの新元号対応（令和）
               WHEN    "令和"
                   MOVE    "5"         TO  LNK-ZENHANCNV-OT-WYMD (1:1)
           END-EVALUATE
           MOVE    LNK-ZENHANCNV-IN-DATA (5:4)
                                       TO  WRK-MOJI-IN
           MOVE    4                   TO  WRK-MOJI-LEN
           PERFORM 7003-MOJI-HENKAN-SEC
           MOVE    WRK-HENKAN-OUT      TO  LNK-ZENHANCNV-OT-WYMD (2:2)
           MOVE    LNK-ZENHANCNV-IN-DATA (11:4)
                                       TO  WRK-MOJI-IN
           MOVE    4                   TO  WRK-MOJI-LEN                    
           PERFORM 7003-MOJI-HENKAN-SEC
           MOVE    WRK-HENKAN-OUT      TO  LNK-ZENHANCNV-OT-WYMD (4:2)
           IF      FLG-YM              =   ZERO
               MOVE    LNK-ZENHANCNV-IN-DATA (17:4)
                                           TO  WRK-MOJI-IN
               MOVE    4                   TO  WRK-MOJI-LEN                    
               PERFORM 7003-MOJI-HENKAN-SEC
               MOVE    WRK-HENKAN-OUT      TO
                                           LNK-ZENHANCNV-OT-WYMD (6:2)
           END-IF
      *
           INSPECT LNK-ZENHANCNV-OT-WYMD   REPLACING   ALL " "
                                                       BY  "0"
      *
      *    和歴→西暦
           INITIALIZE                      ORCSGDAYAREA
           IF      FLG-YM              =   ZERO
               MOVE    LNK-ZENHANCNV-OT-WYMD
                                           TO  SGDAY-INDAY
           ELSE
               MOVE    LNK-ZENHANCNV-OT-WYMD(1:5)
                                           TO  SGDAY-INDAY
               MOVE    "1"                 TO  SGDAY-INTYPE
           END-IF
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               IF      FLG-YM              =   ZERO
                   MOVE    SGDAY-SDAY          TO  LNK-ZENHANCNV-OT-SYMD
                   MOVE    SGDAY-OTDAY         TO  LNK-ZENHANCNV-OT-GYMD
               ELSE
                   MOVE    SGDAY-SDAY (1:6)    TO  LNK-ZENHANCNV-OT-SYMD
                   MOVE    SGDAY-OTDAY(1:6)    TO  LNK-ZENHANCNV-OT-GYMD
               END-IF
           ELSE
               MOVE    3                   TO  LNK-ZENHANCNV-RC
           END-IF                 
           .
       2001-YMD-WAREKI-HENSYU-EXT.
           EXIT.
      *     
      *****************************************************************
      *    西暦変換処理
      *****************************************************************
       2002-YMD-SEIREKI-HENSYU-SEC     SECTION.
      *
           IF      LNK-ZENHANCNV-IN-DATA (13:4)
                                       =   SPACE
               MOVE    1                   TO  FLG-YM
           END-IF
      *
           MOVE    LNK-ZENHANCNV-IN-DATA
                                       TO  WRK-MOJI-IN
           MOVE    8                   TO  WRK-MOJI-LEN
           PERFORM 7003-MOJI-HENKAN-SEC
      *
           INSPECT WRK-HENKAN-OUT (1:8)    REPLACING   ALL " "
                                                       BY  "0"
      *
      *    西暦→和歴
           INITIALIZE                      ORCSGDAYAREA
           IF      FLG-YM              =   ZERO
               MOVE    WRK-HENKAN-OUT (1:8)
                                           TO  SGDAY-INDAY
           ELSE
               MOVE    "1"                 TO  SGDAY-INTYPE
               MOVE    WRK-HENKAN-OUT (1:6)
                                           TO  SGDAY-INDAY
           END-IF
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               IF      FLG-YM              =   ZERO
                   MOVE    SGDAY-SDAY          TO  LNK-ZENHANCNV-OT-SYMD
                   MOVE    SGDAY-OTDAY         TO  LNK-ZENHANCNV-OT-GYMD
               ELSE
                   MOVE    SGDAY-SDAY (1:6)    TO  LNK-ZENHANCNV-OT-SYMD
                   MOVE    SGDAY-OTDAY(1:6)    TO  LNK-ZENHANCNV-OT-GYMD
               END-IF
           ELSE
               MOVE    3                   TO  LNK-ZENHANCNV-RC
           END-IF                 
           .
       2002-YMD-SEIREKI-HENSYU-EXT.
           EXIT.
      *     
      *****************************************************************
      *    文字変換処理
      *****************************************************************
       200-MOJI-HENSYU-SEC         SECTION.
      *
           MOVE    LNK-ZENHANCNV-IN-DATA (1:LNK-ZENHANCNV-IN-LEN)
                                   TO  WRK-MOJI-IN
           MOVE    LNK-ZENHANCNV-IN-LEN
                                   TO  WRK-MOJI-LEN
           PERFORM 7003-MOJI-HENKAN-SEC
           MOVE    WRK-HENKAN-OUT  TO  LNK-ZENHANCNV-OT-DATA
           MOVE    WRK-HENKAN-LEN  TO  LNK-ZENHANCNV-OT-LEN
           .
       200-MOJI-HENSYU-EXT.
           EXIT.
      *     
      *****************************************************************
      *    数字変換処理
      *****************************************************************
       200-NUM-HENSYU-SEC         SECTION.
      *
           MOVE    LNK-ZENHANCNV-IN-DATA (1:LNK-ZENHANCNV-IN-LEN)
                                   TO  WRK-MOJI-IN
           MOVE    LNK-ZENHANCNV-IN-LEN
                                   TO  WRK-MOJI-LEN
           PERFORM 7003-MOJI-HENKAN-SEC
           MOVE    WRK-HENKAN-OUT  TO  LNK-ZENHANCNV-OT-DATA
           MOVE    WRK-HENKAN-LEN  TO  LNK-ZENHANCNV-OT-LEN
      *
      *    
           INITIALIZE                  ORCSNUMAREA
           MOVE    LNK-ZENHANCNV-OT-DATA
                                   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF      SNUM-RC             =   ZERO
               MOVE    SNUM-OUTNUM     TO  LNK-ZENHANCNV-OT-NUM
           ELSE
               MOVE    4               TO  LNK-ZENHANCNV-RC
           END-IF
           .
       200-NUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角文字→半角文字変換処理
      *****************************************************************
       7003-MOJI-HENKAN-SEC            SECTION.
      *
           INITIALIZE                      ORCXKANACONVAREA 
           MOVE    1000                TO  KANACONV-STLEN
           MOVE    WRK-MOJI-LEN        TO  KANACONV-LEN
           MOVE    1                   TO  KANACONV-CONV-FLAG
           MOVE    112                 TO  KANACONV-CHAR-TYPE
           MOVE    WRK-MOJI-IN         TO  KANACONV-INDATA
           CALL    "kanaconv"          USING   ORCXKANACONVAREA
           IF      KANACONV-RETURN     =   0
               MOVE   KANACONV-OUTDATA (1:KANACONV-MAXLEN)
                                           TO  WRK-HENKAN-OUT
               MOVE   KANACONV-MAXLEN      TO  WRK-HENKAN-LEN
           END-IF
      *
           .
       7003-MOJI-HENKAN-EXT.
           EXIT.

