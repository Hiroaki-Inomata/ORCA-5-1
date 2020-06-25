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
        02  MJOB.
            03  MJOB-FIXED12.
              04  MJOB-B06-STATE                PIC S9(9)   BINARY.
              04  MJOB-B07-STATE                PIC S9(9)   BINARY.
              04  MJOB-B08-STATE                PIC S9(9)   BINARY.
              04  MJOB-B11-STATE                PIC S9(9)   BINARY.
              04  MJOB-B12-STATE                PIC S9(9)   BINARY.
              04  MJOB-NOTEBOOK1.
                05  MJOB-PAGENO                 PIC S9(9)   BINARY.
                05  MJOB-FIXED16.
                  06  MJOB-SCROLLEDWINDOW26.
                    07  MJOB-HPOS    PIC S9(9)   BINARY.
                    07  MJOB-VPOS    PIC S9(9)   BINARY.
                    07  MJOB-LIST.
                      08  MJOB-COUNT   PIC S9(9)   BINARY.
                      08  MJOB-ROW     PIC S9(9)   BINARY.
                      08  MJOB-ITEM        OCCURS  100 TIMES.
                        09  MJOB-VALUE1  PIC X(16).
                        09  MJOB-VALUE2  PIC X(5).
                        09  MJOB-VALUE3  PIC X(256).
                        09  MJOB-VALUE4  PIC X(22).
                        09  MJOB-VALUE5  PIC X(22).
                        09  MJOB-VALUE6  PIC X(35).
                        09  MJOB-VALUE7  PIC X(35).
                      08  MJOB-SELECT
                              PIC X       OCCURS  100 TIMES.
                05  MJOB-FIXED17.
                  06  MJOB-SW1.
                    07  MJOB-HPOS    PIC S9(9)   BINARY.
                    07  MJOB-VPOS    PIC S9(9)   BINARY.
                    07  MJOB-DATAVIEW.
                      08  MJOB-VALUE   PIC X(63000).
                  06  MJOB-MIDASI.
                    07  MJOB-VALUE   PIC X(18).
                  06  MJOB-CNTHEN.
                    07  MJOB-VALUE   PIC X(28).
              05  MJOB-LOGDATE.
                06  MJOB-TEXTDATA   PIC X(10).
              05  MJOB-MIDASI1.
                06  MJOB-MIDASI1-VALUE   PIC X(18).
