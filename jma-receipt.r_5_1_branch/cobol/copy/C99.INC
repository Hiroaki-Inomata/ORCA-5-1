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
        02  C99.
          03  C99-MSG.
            04  C99-VALUE          PIC X(80).
          03  C99-SCROLLEDWINDOW28.
            04  C99-JOBKANRILST.
              05  C99-COUNT        PIC S9(9)   BINARY.
              05  C99-ROW          PIC S9(9)   BINARY.
              05  C99-ROWATTR      PIC S9(9)   BINARY.
              05  C99-ITEM         OCCURS  30 TIMES.
                06  C99-RENNUM     PIC X(7).
                06  C99-SHORINM    PIC X(26).
                06  C99-STARTDATE  PIC X(9).
                06  C99-STARTTIME  PIC X(8).
                06  C99-ENDDATE    PIC X(9).
                06  C99-ENDTIME    PIC X(8).
                06  C99-PAGE       PIC X(10).
                06  C99-ERRMSG     PIC X(100).
              05  C99-SELECT       PIC X
                                   OCCURS  30 TIMES.
