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
         02  P02A.
      ****  02  P02A-FIXED1.
              03  P02A-MIDASI.
                04  P02A-VALUE  PIC X(22).
              03  P02A-SCROLLEDWINDOW1.
                04  P02A-DUALLIST.
                  05  P02A-DUALLIST-COUNT  PIC S9(9)   BINARY.
                  05  P02A-DUALLIST-ROW   PIC S9(9)   BINARY.
                  05  P02A-DUALLIST-ROWATTR   PIC S9(9)   BINARY.
                  05  P02A-DUALLIST-ITEM       OCCURS  20 TIMES.
                    07  P02A-NUM   PIC X(4).
                    07  P02A-PTNUM PIC X(20).
                    07  P02A-NAME  PIC X(30).
                    07  P02A-BIRTHDAY  PIC X(9).
                    07  P02A-ADRS  PIC X(50).
                    07  P02A-TEL   PIC X(16).
                  05  P02A-DUALLIST-SELECT PIC X       OCCURS  20 TIMES.
              03  P02A-SELNUM.
                04  P02A-VALUE  PIC S9(3).
