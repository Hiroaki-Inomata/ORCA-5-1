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
        02  Q97.
          03  Q97-MSG.
            04  Q97-VALUE          PIC X(80).
          03  Q97-SCROLLEDWINDOW28.
            04  Q97-JOBKANRILST.
              05  Q97-COUNT        PIC S9(9)   BINARY.
              05  Q97-ROW          PIC S9(9)   BINARY.
              05  Q97-ROWATTR      PIC S9(9)   BINARY.
              05  Q97-ITEM         OCCURS  30 TIMES.
                06  Q97-RENNUM     PIC X(7).
                06  Q97-SHORINM    PIC X(26).
                06  Q97-STARTDATE  PIC X(9).
                06  Q97-STARTTIME  PIC X(8).
                06  Q97-ENDDATE    PIC X(9).
                06  Q97-ENDTIME    PIC X(8).
                06  Q97-PAGE       PIC X(10).
                06  Q97-ERRMSG     PIC X(100).
              05  Q97-SELECT       PIC X
                                   OCCURS  30 TIMES.
          03  Q97-DURATION   PIC S9(09) BINARY.
          03  Q97-DOWNLOAD1.
              05  Q97-OBJECTDATA   PIC X(36).
              05  Q97-FILENAME     PIC X(1024).
