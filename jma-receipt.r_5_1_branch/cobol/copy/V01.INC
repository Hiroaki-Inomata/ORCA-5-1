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
        02  V01.
            03  V01-FIXED1.
              04  V01-B05       PIC X(10).
              04  V01-B06       PIC X(10).
              04  V01-B11-STATE PIC S9(9)   BINARY.
              04  V01-STYUKYMD.
                05  V01-VALUE   PIC X(11).
              04  V01-EDYUKYMD.
                05  V01-VALUE   PIC X(11).
              04  V01-HKNNUM.
                05  V01-VALUE   PIC X(4).
              04  V01-TANSEIDONAME.
                05  V01-VALUE   PIC X(11).
              04  V01-SELNUM.
                05  V01-VALUE   PIC 9(3).
              04  V01-PAYKBN-STATE
                                PIC S9(9)   BINARY.
              04  V01-PAYKBN   PIC X(3).
              04  V01-SCROLLEDWINDOW1.
                05  V01-CLIST1.
                  06  V01-COUNT   PIC S9(9)   BINARY.
                  06  V01-ROW     PIC S9(9)   BINARY.
                  06  V01-ROWATTR PIC S9(9)   BINARY.
                  06  V01-ITEM        OCCURS  100 TIMES.
                    07  V01-TBANGO    PIC X(4).
                    07  V01-TPAYKBN   PIC X(3).
                    07  V01-TSTYUKYMD PIC X(10).
                    07  V01-TEDYUKYMD PIC X(10).
                  06  V01-SELECT  PIC X       OCCURS  100 TIMES.
              04  V01-CMB-PREFNUM.
                05  V01-PREFNUM            PIC X(23).
                05  V01-PREFNUM-STATE      PIC S9(9)   BINARY.
                05  V01-PREFNUM-COUNT      PIC S9(9)   BINARY.
                05  V01-PREFNUMLST         PIC X(23)   OCCURS  50 TIMES.
              04  V01-CPHKNNUM-STATE
                                PIC S9(9)   BINARY.
              04  V01-CPHKNNUM  PIC X(4).
              04  V01-MESSAGE-STYLE  PIC X(20).
              04  V01-MESSAGE        PIC X(100).
              04  V01-DURATION       PIC S9(09) BINARY.
