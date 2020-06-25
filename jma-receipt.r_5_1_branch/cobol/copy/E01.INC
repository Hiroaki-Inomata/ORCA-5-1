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
        02  E01.
            03  E01-FIXED6.
              04  E01-SCROLLEDWINDOW1.
                05  E01-CLIST1.
                  06  E01-COUNT1   PIC S9(9)   BINARY.
                  06  E01-ROW1     PIC S9(9)   BINARY.
                  06  E01-ROWATTR1 PIC S9(9)   BINARY.
                  06  E01-ITEM1        OCCURS  200 TIMES.
                    07  E01-VALUE1  PIC X(25).
                  06  E01-SELECT1  PIC X       OCCURS  200 TIMES.
              04  E01-SCROLLEDWINDOW2.
                05  E01-CLIST2.
                  06  E01-COUNT2   PIC S9(9)   BINARY.
                  06  E01-ROW2     PIC S9(9)   BINARY.
                  06  E01-ROWATTR2 PIC S9(9)   BINARY.
                  06  E01-ITEM2        OCCURS  200 TIMES.
                    07  E01-VALUE2  PIC X(25).
                  06  E01-SELECT2  PIC X       OCCURS  200 TIMES.
              04  E01-CHKBTN01.
                05  E01-VALUE3  PIC X.
              04  E01-CHKMSG01.
                05  E01-VALUE4  PIC X(32).
              04  E01-CHKMSG02.
                05  E01-VALUE5  PIC X(06).
