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
      *    文字列チェック関数パラメタ　  COPY (CPORCSSTRING.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     
      ************************************************************
      *
       01  ORCSSTRINGAREA.
      *    コマンド
           03  ORCSSTR-COMMAND         PIC X(08).
      *    文字列１
           03  ORCSSTR-STRING1         PIC X(50000).
      *    文字列２
           03  ORCSSTR-STRING2         PIC X(50000).
      *    文字列３
           03  ORCSSTR-STRING3         PIC X(50000).
      *    数値１
           03  ORCSSTR-NUM1            PIC S9(9) BINARY.
      *    数値１
           03  ORCSSTR-NUM2            PIC S9(9) BINARY.
      *
      *
