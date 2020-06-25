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
      *----------------------------------------------------------*
      *    文字列チェック関数パラメタ　  COPY (CPORCSKANACONV2.INC)
      *                  入出力桁数 X(5000)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.06.00    NACL-多々納  10/10/01  入力・出力領域をX(5000)へ
      ************************************************************
      *
       01  ORCXKANACONVAREA.
      *   出力バイト数
           03 KANACONV-LEN             PIC S9(9) BINARY.
      *   返り値( 0: 正常 1: 範囲外文字 2: 外字、変換不能 )
           03 KANACONV-RETURN          PIC S9(9) BINARY.
      *   変換文字列数(バイト)
           03 KANACONV-MAXLEN          PIC S9(9) BINARY.
      *   変換フラグ (0: 変換しない,     1: 変換する
      *               2: 1+カタカナ変換, 3: 1+ひらがな変換)
      *   ※ 半角カナおよび外字は全てのフラグで変換される
      *      か捨てられる。
      *     (外字は■)
           03 KANACONV-CONV-FLAG       PIC S9(9) BINARY.
      *   出力文字種指定
      *       0: 半角カナ、外字のみ変換
      *      14: 全角   2: ひらがな  4: カタカナ
      *      112: ASCII 16: 数字     32: アルファベット
      *      15: 全角+スペース改行   113: ASCII+スペース改行 
      *   出力文字種は OR をとることで複数指定できる
      *   例: (6: ひらがな、カタカナのみ)
      *       (48: 英数字のみ)
           03 KANACONV-CHAR-TYPE       PIC S9(9) BINARY.
      *   文字領域の長さ(INDATA, OUTDATAの長さと同じであること)
           03 KANACONV-STLEN           PIC S9(9) BINARY.
      *   入力文字列
           03 KANACONV-INDATA          PIC X(5000).
      *   出力文字列
           03 KANACONV-OUTDATA         PIC X(5000).
      *
      *
