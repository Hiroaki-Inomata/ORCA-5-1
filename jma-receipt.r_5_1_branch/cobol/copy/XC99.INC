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
            02  XC99.
              03  XC99-MSG.
                04  XC99-VALUE   PIC X(80).
              03  XC99-SCROLLEDWINDOW28.
                04  XC99-JOBKANRILST.
                  05  XC99-COUNT       PIC S9(9)   BINARY.
                  05  XC99-ROW         PIC S9(9)   BINARY.
                  05  XC99-ROWATTR     PIC S9(9)   BINARY.
                  05  XC99-ITEM        OCCURS  30 TIMES.
                    06  XC99-RENNUM    PIC X(7).
                    06  XC99-SHORINM   PIC X(20).
                    06  XC99-STARTTIME PIC X(8).
                    06  XC99-ENDTIME   PIC X(8).
                    06  XC99-PAGE      PIC X(10).
                    06  XC99-ERRMSG    PIC X(100).
                  05  XC99-SELECT  PIC X       OCCURS  30 TIMES.
