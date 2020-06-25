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
         02  P0201.
            03  P0201-FIXED1.
              04  P0201-SELNUM    PIC X(4).
              04  P0201-SCROLLEDWINDOW1.
                05  P0201-YYKLIST.
                  06  P0201-COUNT PIC S9(9)   BINARY.
                  06  P0201-ROW   PIC S9(9)   BINARY.
                  06  P0201-ROWATTR   PIC S9(9)   BINARY.
                  06  P0201-ITEM      OCCURS  20 TIMES.
                    07  P0201-NUM       PIC X(5).
                    07  P0201-YYKTIME   PIC X(6).
                    07  P0201-NAME      PIC X(30).
                    07  P0201-SRYKA     PIC X(10).
                  06  P0201-SELECT    PIC X   OCCURS  20 TIMES.
