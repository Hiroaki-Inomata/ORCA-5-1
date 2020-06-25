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
      ************************************************************
      *    チェックマスタ病名編集サブ　COPY (CPORCSS012.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SS012-AREA.
           03  SS012-KEY.
               05  SS012-I-BYOMEI           PIC X(200).
           03  SS012-SRYYM                  PIC X(06).
           03  SS012-O-BYOMEI               PIC X(200).
           03  SS012-BYOMEICD-G.
               05  SS012-BYOMEICD-OCC       OCCURS  21.
                   07  SS012-BYOMEICD       PIC X(07).
                   07  SS012-BYOMEICD-SEP   PIC X(01).
           03  SS012-BYOMEIHENFLG           PIC X(01).
           03  SS012-HAISHI                 PIC 9(01).
