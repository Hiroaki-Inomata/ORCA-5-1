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
        02  XB01.
              03  XB01-B02-LABEL            PIC X(10).
              03  XB01-LBLMSG01-STYLE       PIC X(20).
              03  XB01-LBLMSG01             PIC X(40).
              03  XB01-SELNUM               PIC 9(4).
              03  XB01-SCROLLEDWINDOW2.
                04  XB01-JOBLIST.
                  05  XB01-JOBLIST-COUNT    PIC S9(9)   BINARY.
                  05  XB01-JOBLIST-ROW      PIC S9(9)   BINARY.
                  05  XB01-JOBLIST-ROWATTR  PIC S9(9)   BINARY.
                  05  XB01-JOBLIST-ITEM     OCCURS  100 TIMES.
                    07  XB01-TNO            PIC X(4).
                    07  XB01-TJOBNAME       PIC X(40).
                    07  XB01-TJOBID         PIC X(7).
                    07  XB01-TSHELLID       PIC X(8).
                    07  XB01-TSTYMD         PIC X(9).
                    07  XB01-TSTHMS         PIC X(8).
                    07  XB01-TEDYMD         PIC X(9).
                    07  XB01-TEDHMS         PIC X(8).
                    07  XB01-TSTEPSTHMS     PIC X(8).
                    07  XB01-TSTEPEDHMS     PIC X(8).
                    07  XB01-TERRCD         PIC X(4).
                  05  XB01-JOBLIST-SELECT   PIC X   OCCURS  100 TIMES.
              04  XB01-DURATION             PIC S9(09) BINARY.
