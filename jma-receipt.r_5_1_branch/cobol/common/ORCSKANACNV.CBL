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
       PROGRAM-ID.             ORCSKANACNV.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : カタカナ変換
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-太田      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-太田     02/05/29
      *  04.08.01    NACL-森脇    15/12/21  ひらがな変換対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-INPUTERR        PIC 9(01).
           03  FLG-EXIT            PIC 9(01).
      *    1:ひらがな／2:カタカナ／3:不明
           03  FLG-TYPE            PIC 9(01).
      *    0:1,2共通文字("ー"等)/1:ひらがな／2:カタカナ／9:不明
      *    (初期値:0/不明にはひらがな・カタカナの混在も含む)
           03  FLG-BFTYPE          PIC 9(01).
           03  FLG-HANKAKUCHK      PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MOTO-STR-AREA.
               05  WRK-MOTO-STR    PIC X(02)
                                   OCCURS 100.
           03  WRK-ATO-STR-AREA.
               05  WRK-ATO-STR     PIC X(02)
                                   OCCURS 100.
           03  WRK-KANACHK-MAE-INPUT
                                   PIC X(400).
      *
       01  CONST-AREA.
           03  CONST-KANA-AREA.
               05  FILLER          PIC X(10)   VALUE "あいうえお".
               05  FILLER          PIC X(10)   VALUE "かきくけこ".
               05  FILLER          PIC X(10)   VALUE "さしすせそ".
               05  FILLER          PIC X(10)   VALUE "たちつてと".
               05  FILLER          PIC X(10)   VALUE "なにぬねの".
               05  FILLER          PIC X(10)   VALUE "はひふへほ".
               05  FILLER          PIC X(10)   VALUE "まみむめも".
               05  FILLER          PIC X(06)   VALUE "やゆよ".
               05  FILLER          PIC X(10)   VALUE "らりるれろ".
               05  FILLER          PIC X(10)   VALUE "わゐゑをん".
               05  FILLER          PIC X(10)   VALUE "がぎぐげご".
               05  FILLER          PIC X(10)   VALUE "ざじずぜぞ".
               05  FILLER          PIC X(10)   VALUE "だちづでど".
               05  FILLER          PIC X(10)   VALUE "ばびぶべぼ".
               05  FILLER          PIC X(10)   VALUE "ぱぴぷぺぽ".
               05  FILLER          PIC X(08)   VALUE "っゃゅょ".
               05  FILLER          PIC X(10)   VALUE "ぁぃぅぇぉ".
               05  FILLER          PIC X(36)   VALUE LOW-VALUE.
               05  FILLER          PIC X(10)   VALUE "アイウエオ".
               05  FILLER          PIC X(10)   VALUE "カキクケコ".
               05  FILLER          PIC X(10)   VALUE "サシスセソ".
               05  FILLER          PIC X(10)   VALUE "タチツテト".
               05  FILLER          PIC X(10)   VALUE "ナニヌネノ".
               05  FILLER          PIC X(10)   VALUE "ハヒフヘホ".
               05  FILLER          PIC X(10)   VALUE "マミムメモ".
               05  FILLER          PIC X(06)   VALUE "ヤユヨ".
               05  FILLER          PIC X(10)   VALUE "ラリルレロ".
               05  FILLER          PIC X(10)   VALUE "ワヰヱヲン".
               05  FILLER          PIC X(10)   VALUE "ガギグゲゴ".
               05  FILLER          PIC X(10)   VALUE "ザジズゼゾ".
               05  FILLER          PIC X(10)   VALUE "ダヂヅデド".
               05  FILLER          PIC X(10)   VALUE "バビブベボ".
               05  FILLER          PIC X(10)   VALUE "パピプペポ".
               05  FILLER          PIC X(08)   VALUE "ッャュョ".
               05  FILLER          PIC X(10)   VALUE "ァィゥェォ".
               05  FILLER          PIC X(02)   VALUE "ヴ".
               05  FILLER          PIC X(34)   VALUE LOW-VALUE.
               05  FILLER          PIC X(08)   VALUE "゛゜ー　".
      *
           03  CONST-KANA-R     REDEFINES   CONST-KANA-AREA.
               05  CONST-KANA      PIC X(02)   OCCURS 204
                                   INDEXED BY IDX-K.
      *
           03  CONST-HIRAGANA-ST   PIC 9(05)   VALUE 001.
           03  CONST-HIRAGANA-ED   PIC 9(05)   VALUE 082.
      *
           03  CONST-KATAKANA-ST   PIC 9(05)   VALUE 101.
           03  CONST-KATAKANA-ED   PIC 9(05)   VALUE 183.
      *
           03  CONST-HKKYOTU-ST    PIC 9(05)   VALUE 201.
           03  CONST-HKKYOTU-ED    PIC 9(05)   VALUE 204.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY  "CPORCSKANACNV.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               KANACNV-LNK.
      *
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE              FLG-AREA
                                   IDX-AREA
                                   WRK-AREA
      *
           PERFORM 100-INPUT-CHK-SEC
           IF  ( FLG-HANKAKUCHK    NOT =   ZERO )
               MOVE    1           TO  KANACNV-RC
               MOVE    3           TO  KANACNV-TYPE
               MOVE    SPACE       TO  KANACNV-OT-STR
               GO  TO  000-PROC-EXT
           END-IF
      *
           PERFORM 100-EDIT-SEC
      *
           IF  ( FLG-BFTYPE    =   1   OR  2 )
               MOVE    ZERO        TO  KANACNV-RC
               MOVE    FLG-BFTYPE  TO  KANACNV-TYPE
               MOVE    WRK-ATO-STR-AREA
                                   TO  KANACNV-OT-STR
           ELSE
               MOVE    ZERO        TO  KANACNV-RC
               MOVE    3           TO  KANACNV-TYPE
               MOVE    SPACE       TO  KANACNV-OT-STR
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       100-INPUT-CHK-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-INPUTERR
      *
      *    全て全角で入力されていること
           MOVE    KANACNV-IN-STR  TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-ENRTRY-CHK-SEC
           IF ( FLG-HANKAKUCHK     NOT =   ZERO )
               MOVE    1           TO  FLG-INPUTERR
           END-IF
      *
           .
       100-INPUT-CHK-EXT.
          EXIT.
      *****************************************************************
      *    編集処理
      *****************************************************************
       100-EDIT-SEC                SECTION.
      *
           MOVE    KANACNV-IN-STR  TO  WRK-MOTO-STR-AREA
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   100 )
                OR       ( WRK-MOTO-STR (IDX1) =   SPACE )
      *
               SET IDX-K       TO      1
               SEARCH    CONST-KANA    VARYING IDX-K
               AT  END
                   MOVE    9           TO      FLG-BFTYPE
                   MOVE  100       TO  IDX1
               WHEN    WRK-MOTO-STR (IDX1) =   CONST-KANA (IDX-K)
      *
                   SET IDX2    TO  IDX-K
      *
                   PERFORM 101-KANA-CHK
      *
                   IF  ( FLG-EXIT  NOT =   ZERO )
                       MOVE  100       TO  IDX1
                   END-IF
      *
               END-SEARCH
      *
           END-PERFORM
      *
           .
       100-EDIT-EXT.
           EXIT.
      *----(04.08.01)--UPD-START---
      *****************************************************************
      *    かなチェック処理
      *****************************************************************
       101-KANA-CHK                SECTION.
      *
           MOVE    ZERO                TO      FLG-EXIT
      *
           EVALUATE    IDX2
      *    ひらがなかどうか判断
           WHEN    CONST-HIRAGANA-ST   THRU    CONST-HIRAGANA-ED
               IF  ( FLG-BFTYPE        =       0 OR 1 OR 3)
                   MOVE    1           TO      FLG-BFTYPE
                   IF      KANACNV-SYORI   =   "1"
                       MOVE    CONST-KANA (IDX2)
                                       TO      WRK-ATO-STR (IDX1)
                   ELSE
                       MOVE    CONST-KANA (IDX2 + 100 )
                                       TO      WRK-ATO-STR (IDX1)
                   END-IF
               ELSE
                   MOVE    9           TO      FLG-BFTYPE
                   MOVE    1           TO      FLG-EXIT
               END-IF
      *    カタカナかどうか判断
           WHEN    CONST-KATAKANA-ST   THRU    CONST-KATAKANA-ED
               IF  ( FLG-BFTYPE        =       0 OR 2 OR 3)
                   MOVE    2           TO      FLG-BFTYPE
                   IF      KANACNV-SYORI   =   "1"
                       MOVE    CONST-KANA (IDX2 - 100 )
                                       TO      WRK-ATO-STR (IDX1)
                   ELSE
                       MOVE    CONST-KANA (IDX2)
                                       TO      WRK-ATO-STR (IDX1)
                   END-IF
               ELSE
                   MOVE    9           TO      FLG-BFTYPE
                   MOVE    1           TO      FLG-EXIT
               END-IF
      *    "ー","゛"等かどうか判断
           WHEN    CONST-HKKYOTU-ST    THRU    CONST-HKKYOTU-ED
               MOVE    CONST-KANA (IDX2)
                                       TO      WRK-ATO-STR (IDX1)
           WHEN    OTHER
               MOVE        9           TO      FLG-BFTYPE
               MOVE        1           TO      FLG-EXIT
           END-EVALUATE
      *
           .
       101-KANA-EXT.
           EXIT.
      *----(04.08.01)--UPD-END-----
      *****************************************************************
      *    全角半角混在チェック
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-HANKAKUCHK
      *
           IF  ( WRK-KANACHK-MAE-INPUT
                                   =   SPACE )
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    1           TO  FLG-HANKAKUCHK
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
      *    全角でない場合はエラーとする。
           IF      ( KANACHK-SYUBETU   NOT =   2 )
               MOVE    1           TO  FLG-HANKAKUCHK
           END-IF
      *
           .
       800-ENRTRY-CHK-EXT.
           EXIT.
