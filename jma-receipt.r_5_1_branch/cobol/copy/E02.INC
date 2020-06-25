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
        02  E02.
            03  E02-FIXED6.
              04  E02-SRYYM.
                05  E02-VALUE   PIC X(7).
              04  E02-SCROLLEDWINDOW1.
                05  E02-CLIST1.
                  06  E02-COUNT1   PIC S9(9)   BINARY.
                  06  E02-ROW1     PIC S9(9)   BINARY.
                  06  E02-ROWATTR1 PIC S9(9)   BINARY.
                  06  E02-ITEM1        OCCURS  900 TIMES.
                    07  E02-VALUE1  PIC X(25).
                  06  E02-SELECT1  PIC X       OCCURS  900 TIMES.
              04  E02-SCROLLEDWINDOW2.
                05  E02-CLIST2.
                  06  E02-COUNT2   PIC S9(9)   BINARY.
                  06  E02-ROW2     PIC S9(9)   BINARY.
                  06  E02-ROWATTR2 PIC S9(9)   BINARY.
                  06  E02-ITEM2        OCCURS  900 TIMES.
                    07  E02-VALUE2  PIC X(25).
                  06  E02-SELECT2  PIC X       OCCURS  900 TIMES.
              04  E02-CHKMSG000.
                05  E02-VALUE   PIC X(48).
              04  E02-FILENAME.
                05  E02-VALUE   PIC X(129).
