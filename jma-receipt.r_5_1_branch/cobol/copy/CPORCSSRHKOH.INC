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
      *    公費検索サブ                   COPY (CPORCSSRHKOH.INC)*
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SSRHKOH-AREA.
           03  SSRHKOH-PTID                PIC 9(10).
           03  SSRHKOH-NYUGAIKBN           PIC X(01).
           03  SSRHKOH-SRYYM               PIC X(06).
           03  SSRHKOH-HKNCOMBI            PIC X(04).
      *    リターンコード 公費なし:0 公費あり:1
           03  SSRHKOH-RC                  PIC S9(01).
