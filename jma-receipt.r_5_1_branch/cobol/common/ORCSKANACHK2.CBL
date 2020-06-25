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
       PROGRAM-ID.             ORCSKANACHK2.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 半角チェック処理（拡張漢字対応）
      *  返却エラーコード  : 0:正常　1:半角混在、２：変換エラー
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/10/27    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.06.00    NACL-多々納  10/10/01  入力・出力領域をX(5000)へ
      *  05.01.00    NACL-多々納  17/09/26  改行コード判定追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-ERR             PIC 9(01).
           03  FLG-KANA            PIC 9(01).
           03  FLG-KANA2           PIC 9(01).
           03  FLG-KANJI           PIC 9(01).
           03  FLG-KANJI-ERR       PIC 9(01).
           03  FLG-KUHAKU          PIC 9(01).
           03  FLG-ZENKAKU1        PIC 9(01).
           03  FLG-ZENKAKU2        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDX-MAX2            PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX-END             PIC 9(04).
      *
      *
        01  WRK-AREA.
           03  WRK-WYMD.
               05  WRK-WGO             PIC X(01).
               05  WRK-WYY             PIC 9(02).
               05  WRK-WMM             PIC 9(02).
               05  WRK-WDD             PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
      *
           03  WRK-INDAY.
               05  WRK-YY              PIC X(10).
               05  WRK-MM              PIC X(02).
               05  WRK-DD              PIC X(02).
      *
           03  WRK-HMM                 PIC 9(02).
           03  WRK-HDD                 PIC 9(02).
      *
           03  WRK-MOJI                PIC X(01).
           03  WRK-KIGO                PIC X(01).
           03  WRK-ZENMOJI             PIC X(02).
           03  WRK-ZENMOJI2            PIC X(02).
      *
           03  WRK-MEISYO              PIC X(5000).
      *
           03  WRK-ERRCD               PIC X(02).
      *
           03  WRK-MAX-CNT             PIC 9(04).
      *
           03  WRK-IDX1                PIC 9(04).
           03  WRK-IDX2                PIC 9(04).
      *    全角文字数
           03  WRK-MAX-MOJI            PIC S9(9)  BINARY.
      *
      *R01.8
           03  WRK-PCNT                PIC 9(04).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCXKANACONV2.INC".
      *
      *    拡張文字対応
           COPY    "CPORCSSTRING.INC".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSKANACHK2.INC".
      *
       PROCEDURE                    DIVISION    USING
           ORCSKANACHK2AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    ZERO                TO  KANACHK2-RC
           MOVE    ZERO                TO  KANACHK2-RC2
           MOVE    SPACE               TO  KANACHK2-OUT-INPUT
      *    最大桁数判定
           MOVE    ZERO                TO  IDX-MAX
           PERFORM VARYING     IDX     FROM    5000 BY  -1
                   UNTIL       IDX     <   1
               IF     KANACHK2-MAE-INPUT (IDX:1)    NOT =   SPACE
                   MOVE    IDX             TO  IDX-MAX
                   MOVE    1               TO  IDX
               END-IF
           END-PERFORM
           IF      IDX-MAX             =   ZERO
               MOVE    ZERO                TO  KANACHK2-MAX
               MOVE    SPACE               TO  KANACHK2-OUT-INPUT
           ELSE
               MOVE    IDX-MAX             TO  KANACHK2-MAX
           END-IF
      *
           IF      IDX-MAX             >   ZERO
      *        全角の内容判定
               PERFORM 1000-KOUMOKU-CHK-SEC
      *
               EVALUATE    KANACHK2-SYORI
                   WHEN    "1"
      *            全角判定処理
                   PERFORM 100-ZENKAKU-CHECK-SEC
                   WHEN    "2"
      *            全角置換処理
                   PERFORM 200-ZENKAKU-HENKAN-SEC
               END-EVALUATE
      *H29.9
      *        改行コードチェック
               PERFORM 1009-KAIGYO-CHK-SEC
      *
           END-IF
      *TEST
      *    DISPLAY "KANACHK2-MAE-INPUT:" KANACHK2-MAE-INPUT (1:50)
      *    DISPLAY "KANACHK2-OUT-INPUT:" KANACHK2-OUT-INPUT (1:50)
      *    DISPLAY "KANACHK2-RC :" KANACHK2-RC 
      *          ",KANACHK2-RC2:" KANACHK2-RC2
      *          ",KANACHK2-RC3:" KANACHK2-RC3
      *          ",KANACHK2-RC4:" KANACHK2-RC4
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    全角チェック処理
      *****************************************************************
       1000-KOUMOKU-CHK-SEC           SECTION.
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    KANACHK2-MAX-CNT     TO  KANACONV-LEN
           IF      KANACHK2-MAX-CNT     =   ZERO
               MOVE    IDX-MAX             TO  KANACONV-LEN
           END-IF
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    256                 TO  KANACONV-CHAR-TYPE
           MOVE    KANACHK2-MAE-INPUT  TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
      *
           MOVE    KANACONV-OUTDATA    TO  KANACHK2-MAE-INPUT
           MOVE    KANACONV-OUTDATA    TO  KANACHK2-OUT-INPUT
           IF      KANACONV-MAXLEN     =   IDX-MAX
               MOVE    ZERO                TO  KANACHK2-RC2
           ELSE
               MOVE    7                   TO  KANACHK2-RC2
           END-IF
           MOVE    KANACONV-MAXLEN     TO  IDX-MAX
           MOVE    KANACONV-MAXLEN     TO  KANACHK2-MAX
           IF      KANACHK2-MAX-CNT    =   ZERO
               MOVE    5000                TO  KANACHK2-MAX-CNT
           END-IF
      *    拡張漢字有無
           INITIALIZE                  ORCSSTRINGAREA
           MOVE    "check"             TO  ORCSSTR-COMMAND
           MOVE    KANACHK2-OUT-INPUT  TO  ORCSSTR-STRING1
           CALL    "orcsstring"        USING
                                       ORCSSTRINGAREA
           IF      ORCSSTR-NUM2    NOT =   ZERO
               MOVE    1               TO  KANACHK2-RC4
           END-IF
      *TEST
      *    DISPLAY "1000-KOUMOKU-CHK"
      *    DISPLAY "KANACHK2-OUT-INPUT:" KANACHK2-OUT-INPUT (1:50)
      *    DISPLAY "ORCSSTR-NUM1:" ORCSSTR-NUM1
      **         ",ORCSSTR-NUM2:"  ORCSSTR-NUM2
      *
           .
       1000-KOUMOKU-CHK-EXT.
           EXIT.
      *
      *H29.9
      *****************************************************************
      *    改行判定処理
      *****************************************************************
       1009-KAIGYO-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-PCNT
           INSPECT     KANACHK2-OUT-INPUT  TALLYING  WRK-PCNT
                                       FOR ALL  X"0A"
      *    改行コードあり
           IF      WRK-PCNT            >   ZERO
               MOVE    1                   TO  KANACHK2-RC5
           END-IF
      *
      *R01.8 変更
      *    改行判定
      *    INITIALIZE                  ORCSSTRINGAREA
      *    MOVE    "search"            TO  ORCSSTR-COMMAND
      *    MOVE    KANACHK2-OUT-INPUT  TO  ORCSSTR-STRING1
      *    MOVE    X"0A"               TO  ORCSSTR-STRING2
      *    CALL    "orcsstring"        USING
      *                                ORCSSTRINGAREA
      *    IF      ORCSSTR-NUM1    NOT =   ZERO
      *        MOVE    1                   TO  KANACHK2-RC5
      **** END-IF
           .
       1009-KAIGYO-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    全角判定処理
      *****************************************************************
       100-ZENKAKU-CHECK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-KANJI
           MOVE    ZERO                TO  FLG-KANA
           MOVE    ZERO                TO  FLG-KUHAKU
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    KANACHK2-MAX-CNT     TO  KANACONV-LEN
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    271                 TO  KANACONV-CHAR-TYPE
           MOVE    KANACHK2-MAE-INPUT   TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
           IF      KANACONV-RETURN     =   ZERO
      *        全角のみ
               MOVE    1                   TO  FLG-KANJI
               INITIALIZE                  ORCXKANACONVAREA
               MOVE    KANACHK2-MAX-CNT    TO  KANACONV-LEN
               MOVE    5000                TO  KANACONV-STLEN
               MOVE    1                   TO  KANACONV-CONV-FLAG
               MOVE    271                 TO  KANACONV-CHAR-TYPE
               MOVE    KANACHK2-MAE-INPUT   TO  KANACONV-INDATA
               CALL    "kanaconv"          USING
                                           ORCXKANACONVAREA
               IF      KANACONV-MAXLEN     =   IDX-MAX
                   CONTINUE
               ELSE
      *            半角空白の置換えあり
                   MOVE    8                   TO  KANACHK2-RC2
               END-IF
               MOVE    KANACONV-OUTDATA    TO  KANACHK2-OUT-INPUT
               MOVE    2                   TO  KANACHK2-SYUBETU
               MOVE    KANACONV-MAXLEN     TO  KANACHK2-MAX
           ELSE
      *        半角のみの判定
               INITIALIZE                  ORCXKANACONVAREA
               MOVE    KANACHK2-MAX-CNT    TO  KANACONV-LEN
               MOVE    5000                TO  KANACONV-STLEN
               MOVE    0                   TO  KANACONV-CONV-FLAG
               MOVE    113                 TO  KANACONV-CHAR-TYPE
               MOVE    KANACHK2-MAE-INPUT  TO  KANACONV-INDATA
               CALL    "kanaconv"          USING
                                           ORCXKANACONVAREA
               IF      KANACONV-RETURN     =   ZERO
                   MOVE    1                   TO  KANACHK2-SYUBETU
                   MOVE    KANACONV-MAXLEN     TO  KANACHK2-MAX
                   MOVE    KANACONV-MAXLEN     TO  IDX-MAX
      *            間に空白があるかの判定
                   PERFORM 1001-KUHAKU-CHK-SEC
               ELSE
      *            混在エラー
                   MOVE    1                   TO  KANACHK2-RC
               END-IF
           END-IF
      *
      *    全角空白の判定
           IF     (KANACHK2-SYUBETU     =   2    )  AND
                  (KANACHK2-RC          =   ZERO )
               PERFORM 1003-ZENSPACE-CHK-SEC
           END-IF
           .
       100-ZENKAKU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    間に半角空白があるかの判定処理
      *****************************************************************
       1001-KUHAKU-CHK-SEC           SECTION.
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    KANACHK2-MAX-CNT     TO  KANACONV-LEN
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    112                 TO  KANACONV-CHAR-TYPE
           MOVE    KANACHK2-MAE-INPUT   TO  KANACONV-INDATA
           CALL    "kanaconv"          USING
                                       ORCXKANACONVAREA
           IF      KANACONV-RETURN NOT =   ZERO
               MOVE    8                   TO  KANACHK2-RC2
           END-IF
      *
           .
       1001-KUHAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全角空白判定処理
      *****************************************************************
       1003-ZENSPACE-CHK-SEC           SECTION.
      *
      *    最大文字数
           INITIALIZE                  ORCSSTRINGAREA
           MOVE    "check"             TO  ORCSSTR-COMMAND
           MOVE    KANACHK2-OUT-INPUT  TO  ORCSSTR-STRING1
           CALL    "orcsstring"        USING
                                       ORCSSTRINGAREA
           MOVE    ORCSSTR-NUM1        TO  WRK-MAX-MOJI
      *
           INITIALIZE                  ORCSSTRINGAREA
           MOVE    "search"            TO  ORCSSTR-COMMAND
           MOVE    KANACHK2-OUT-INPUT  TO  ORCSSTR-STRING1
           MOVE    "　"                TO  ORCSSTR-STRING2
           CALL    "orcsstring"        USING
                                       ORCSSTRINGAREA
           IF      ORCSSTR-NUM1    NOT =   ZERO
      *        全角空白あり
               MOVE    1                   TO  KANACHK2-RC3
      *        最終文字判定
               INITIALIZE                  ORCSSTRINGAREA
               MOVE    "substr"            TO  ORCSSTR-COMMAND
               MOVE    KANACHK2-OUT-INPUT  TO  ORCSSTR-STRING1
               MOVE    WRK-MAX-MOJI        TO  ORCSSTR-NUM1
               COMPUTE ORCSSTR-NUM2        =   WRK-MAX-MOJI
                                           +   1
               CALL    "orcsstring"        USING
                                           ORCSSTRINGAREA
               IF      ORCSSTR-STRING2     =   "　"
      *            最後が全角空白
                   IF      KANACHK2-SYORIKBN    =   "1"
      *                最後の全角空白削除
                       PERFORM 10031-LASTDEL-HEN-SEC
                   ELSE
                       MOVE    3               TO  KANACHK2-RC3
                   END-IF
               END-IF
           END-IF
      *
      *    全部空白
           IF     (KANACHK2-RC4        =   ZERO  )
           AND    (KANACHK2-RC3    NOT =   ZERO  )
               MOVE    ZERO                TO  FLG-ZENKAKU1
               MOVE    ZERO                TO  FLG-ZENKAKU2
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   WRK-MAX-MOJI )
                              OR  (FLG-ZENKAKU1    =   1    )
                   INITIALIZE                  ORCSSTRINGAREA
                   MOVE    "substr"            TO  ORCSSTR-COMMAND
                   MOVE    KANACHK2-OUT-INPUT  TO  ORCSSTR-STRING1
                   MOVE    IDX                 TO  ORCSSTR-NUM1
                   MOVE    IDX                 TO  ORCSSTR-NUM2
                   CALL    "orcsstring"        USING
                                               ORCSSTRINGAREA
                   IF      ORCSSTR-STRING2     =   "　"
      *                全角空白
                       MOVE    1               TO  FLG-ZENKAKU2
                   ELSE
                       MOVE    1               TO  FLG-ZENKAKU1
                   END-IF
               END-PERFORM
               IF      FLG-ZENKAKU1        =   ZERO
      *            全角空白のみ
                   MOVE    2               TO  KANACHK2-RC3
               END-IF
           END-IF
      *
           .
       1003-ZENSPACE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    最後の全角空白削除処理
      *****************************************************************
       10031-LASTDEL-HEN-SEC           SECTION.
      *
           MOVE    ZERO                TO  IDX-END
           PERFORM VARYING     IDX     FROM    WRK-MAX-MOJI   BY  -1
                   UNTIL      (IDX     <   1   )
                          OR  (IDX-END >   ZERO)
               INITIALIZE                  ORCSSTRINGAREA
               MOVE    "substr"            TO  ORCSSTR-COMMAND
               MOVE    KANACHK2-OUT-INPUT  TO  ORCSSTR-STRING1
               MOVE    IDX                 TO  ORCSSTR-NUM1
               MOVE    IDX                 TO  ORCSSTR-NUM2
               CALL    "orcsstring"        USING
                                           ORCSSTRINGAREA
               IF      ORCSSTR-STRING2     =   "　"
                   CONTINUE
               ELSE
                   MOVE    IDX                 TO  IDX-END
               END-IF
           END-PERFORM
      *
           MOVE    KANACHK2-OUT-INPUT  TO  WRK-MEISYO
           MOVE    SPACE               TO  KANACHK2-OUT-INPUT
           IF      IDX-END             >   ZERO
               INITIALIZE                  ORCSSTRINGAREA
               MOVE    "substr"            TO  ORCSSTR-COMMAND
               MOVE    WRK-MEISYO          TO  ORCSSTR-STRING1
               MOVE    1                   TO  ORCSSTR-NUM1
               MOVE    IDX-END             TO  ORCSSTR-NUM2
               CALL    "orcsstring"        USING
                                           ORCSSTRINGAREA
               MOVE    ORCSSTR-STRING2     TO  KANACHK2-OUT-INPUT
           END-IF
      *
           MOVE    ZERO                TO  KANACHK2-RC3
           IF      KANACHK2-OUT-INPUT  =   SPACE
               MOVE    ZERO                TO  KANACHK2-MAX
           ELSE
      *        最大桁数
               MOVE    1                   TO  FLG-KANJI
               INITIALIZE                  ORCXKANACONVAREA
               MOVE    KANACHK2-MAX-CNT    TO  KANACONV-LEN
               MOVE    5000                TO  KANACONV-STLEN
               MOVE    1                   TO  KANACONV-CONV-FLAG
               MOVE    271                 TO  KANACONV-CHAR-TYPE
               MOVE    KANACHK2-OUT-INPUT  TO  KANACONV-INDATA
               CALL    "kanaconv"          USING
                                           ORCXKANACONVAREA
               MOVE    KANACONV-MAXLEN     TO  KANACHK2-MAX
      *
               INITIALIZE                  ORCSSTRINGAREA
               MOVE    "search"            TO  ORCSSTR-COMMAND
               MOVE    KANACHK2-OUT-INPUT  TO  ORCSSTR-STRING1
               MOVE    "　"                TO  ORCSSTR-STRING2
               CALL    "orcsstring"        USING
                                           ORCSSTRINGAREA
               IF      ORCSSTR-NUM1    NOT =   ZERO
      *            全角空白あり
                   MOVE    1                   TO  KANACHK2-RC3
               END-IF
           END-IF
           .
       10031-LASTDEL-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角処理
      *****************************************************************
       200-ZENKAKU-HENKAN-SEC           SECTION.
      *
           MOVE    2                   TO  KANACHK2-SYUBETU
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    KANACHK2-MAX-CNT     TO  KANACONV-LEN
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    1                   TO  KANACONV-CONV-FLAG
           MOVE    271                 TO  KANACONV-CHAR-TYPE
           MOVE    KANACHK2-MAE-INPUT  TO  KANACONV-INDATA
           CALL    "kanaconv"          USING
                                       ORCXKANACONVAREA
           IF      KANACONV-RETURN     =   ZERO
               MOVE    2                   TO  KANACHK2-SYUBETU
               MOVE    KANACONV-OUTDATA    TO  KANACHK2-OUT-INPUT
               MOVE    KANACONV-MAXLEN     TO  KANACHK2-MAX
           ELSE
               MOVE    2                   TO  KANACHK2-RC
           END-IF
      *
      *    全角空白の判定
           IF     (KANACHK2-SYUBETU     =   2    )  AND
                  (KANACHK2-RC          =   ZERO )
               PERFORM 1003-ZENSPACE-CHK-SEC
           END-IF
           .
       200-ZENKAKU-HENKAN-EXT.
           EXIT.
      *
