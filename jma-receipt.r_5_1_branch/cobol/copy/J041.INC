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
         02  J041.
            03  J041-FIXED10.
              04  J041-SCROLLEDWINDOW1.
                05  J041-MED-LIST.
                  06  J041-COUNT  PIC S9(9)   BINARY.
                  06  J041-ROW    PIC S9(9)   BINARY.
                  06  J041-ROWATTR  PIC S9(9)   BINARY.
                  06  J041-ITEM       OCCURS  50 TIMES.
                    07  J041-TOUMEI PIC X(40).
                    07  J041-KNKMEI PIC X(40).
                    07  J041-TOUYM  PIC X(6).
                  06  J041-SELECT PIC X       OCCURS  50 TIMES.
