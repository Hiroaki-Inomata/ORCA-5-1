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
      *    文字列変換(sjis->euc)関数パラメタ COPY (CPORCSJISTOEUC.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     
      ************************************************************
      *
       01  ORCSJISTOEUC.
      *   変換文字列数(バイト)
           03 SJISTOEUC-INLEN           PIC S9(9) BINARY.
      *   出力バイト数
           03 SJISTOEUC-OUTLEN          PIC S9(9) BINARY.
      *   返り値( 0: 正常 1: 範囲外文字 )
           03 SJISTOEUC-RETURN          PIC S9(9) BINARY.
      *   入力文字列
           03 SJISTOEUC-INDATA          PIC X(10000).
      *   出力文字列
           03 SJISTOEUC-OUTDATA         PIC X(10000).
