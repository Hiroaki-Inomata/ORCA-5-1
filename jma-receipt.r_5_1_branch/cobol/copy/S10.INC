      ******************************************************************* * Project code name "ORCA"
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
        02  S10.
            03  S10-LBLSRYYM         PIC X(20).
            03  S10-LBLMSG01         PIC X(40).
            03  S10-SELNUM           PIC S9(3).
            03  S10-SCROLLEDWINDOW1.
              04  S10-PTLIST.
                05  S10-PTLIST-COUNT    PIC S9(9)   BINARY.
                05  S10-PTLIST-ROW      PIC S9(9)   BINARY.
                05  S10-PTLIST-ROWATTR  PIC S9(9)   BINARY.
                05  S10-PTLIST-ITEM     OCCURS  100 TIMES.
                  06  S10-TNUM          PIC X(5).
                  06  S10-TPTNUM        PIC X(20).
                  06  S10-TNAME         PIC X(50).
                  06  S10-TSEX          PIC X(02).
                  06  S10-TBIRHDAY      PIC X(10).
                  06  S10-TAGE          PIC X(10).
                  06  S10-TKBNMEI       PIC X(20).
                05  S10-PTLIST-SELECT   PIC X
                                        OCCURS  100 TIMES.
