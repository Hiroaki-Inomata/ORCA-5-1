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
           02  C06.
              03  C06-SCROLLEDWINDOW28.
                04  C06-SETCDLST.
                  05  C06-SETCDLST-COUNT    PIC S9(9)   BINARY.
                  05  C06-SETCDLST-ROW      PIC S9(9)   BINARY.
                  05  C06-SETCDLST-ROWATTR  PIC S9(9)   BINARY.
                  05  C06-SETCDLST-ITEM     OCCURS  100 TIMES.
                    06  C06-TRENNUM         PIC X(3).
                    06  C06-TSETCD          PIC X(10).
                    06  C06-TSETNAME        PIC X(40).
                  05  C06-SETCDLST-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  C06-SELNUM.
                04  C06-VALUE   PIC S9(3).
              03  C06-SETCD.
                04  C06-VALUE   PIC X(10).
              03  C06-SETNAME.
                04  C06-VALUE   PIC X(40).
              03  C06-INPUTCD   OCCURS  6.
                04  C06-VALUE   PIC X(10).
              03  C06-BYOMEITOP OCCURS  6.
                04  C06-VALUE   PIC X(2).
              03  C06-BYOMEI    OCCURS  6.
                04  C06-VALUE   PIC X(80).
