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
        02  K10.
      ****  02  K10-FIXED1.
              03  K10-SELNUM.
                04  K10-VALUE   PIC X(3).
              03  K10-SCROLLEDWINDOW1.
                04  K10-LIST.
                  05  K10-COUNT   PIC S9(9)   BINARY.
                  05  K10-ROW     PIC S9(9)   BINARY.
                  05  K10-ROWATTR PIC S9(9)   BINARY.
                  05  K10-ITEM        OCCURS  200 TIMES.
                    06  K10-NO      PIC X(3).
                    06  K10-PTNUM   PIC X(20).
                    06  K10-NAME    PIC X(50).
                    06  K10-SRYYMD  PIC X(12).
                    06  K10-SRYKA   PIC X(10).
                    06  K10-HKNCOMBI    PIC X(50).
                    06  K10-DRCD-NAME    PIC X(50).
                    06  K10-SYORIKBN     PIC X(2).
                  05  K10-SELECT  PIC X       OCCURS  200 TIMES.
