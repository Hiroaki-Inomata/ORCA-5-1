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
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
         02  Q99.
            03  Q99-FIXED1.
              04  Q99-SELNUM  PIC 9(04).
              04  Q99-SCROLLEDWINDOW1.
                05  Q99-ADRSLIST.
                  06  Q99-ADRSLIST-COUNT PIC S9(9) BINARY.
                  06  Q99-ADRSLIST-ROW     PIC S9(9)   BINARY.
                  06  Q99-ADRSLIST-ROWATTR PIC S9(9)   BINARY.
                  06  Q99-ADRSLIST-ITEM  OCCURS 200 TIMES.
                    07  Q99-NUM  PIC X(4).
                    07  Q99-POST PIC X(7).
                    07  Q99-ADRS PIC X(50).
                  06  Q99-ADRSLIST-SELECT-TBL.
                    07  Q99-ADRSLIST-SELECT
                                         PIC X OCCURS 200 TIMES.
