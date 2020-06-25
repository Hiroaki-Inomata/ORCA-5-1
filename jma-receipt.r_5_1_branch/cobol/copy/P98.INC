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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
        02  P98.
     *******02  P98-FIXED1.
              03  P98-ADRS.
                04  P98-VALUE   PIC X(100).
              03  P98-POST.
                04  P98-VALUE   PIC X(7).
              03  P98-TADRS-G OCCURS  50.
                  04  P98-TADRS-STYLE PIC X(20).
                  04  P98-TADRS       PIC X(20).
              03  P98-SELNUM.
                04  P98-VALUE   PIC S9(3).
