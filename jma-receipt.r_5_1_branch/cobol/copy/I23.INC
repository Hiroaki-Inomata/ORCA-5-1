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
        02  I23.
          03  I23-MSG.
            04  I23-VALUE          PIC X(80).
          03  I23-SCROLLEDWINDOW28.
            04  I23-JOBKANRILST.
              05  I23-COUNT        PIC S9(9)   BINARY.
              05  I23-ROW          PIC S9(9)   BINARY.
              05  I23-ROWATTR      PIC S9(9)   BINARY.
              05  I23-ITEM         OCCURS  30 TIMES.
                06  I23-RENNUM     PIC X(7).
                06  I23-SHORINM    PIC X(26).
                06  I23-STARTDATE  PIC X(9).
                06  I23-STARTTIME  PIC X(8).
                06  I23-ENDDATE    PIC X(9).
                06  I23-ENDTIME    PIC X(8).
                06  I23-PAGE       PIC X(10).
                06  I23-ERRMSG     PIC X(100).
              05  I23-SELECT       PIC X
                                   OCCURS  30 TIMES.
          03  I23-DURATION   PIC S9(09) BINARY.
          03  I23-DOWNLOAD1.
              05  I23-OBJECTDATA   PIC X(36).
              05  I23-FILENAME     PIC X(1024).
