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
        02  G100.
      *      02  L07-FIXED32.
              03  G100-B03-STATE           PIC S9(9)   BINARY.
              03  G100-B05-STATE           PIC S9(9)   BINARY.
              03  G100-B06-STATE           PIC S9(9)   BINARY.
              03  G100-B07-STATE           PIC S9(9)   BINARY.
              03  G100-B08-STATE           PIC S9(9)   BINARY.
              03  G100-B09-STATE           PIC S9(9)   BINARY.
              03  G100-B05R-STATE          PIC S9(9)   BINARY.
              03  G100-B06R-STATE          PIC S9(9)   BINARY.
              03  G100-B07R-STATE          PIC S9(9)   BINARY.
              03  G100-B08R-STATE          PIC S9(9)   BINARY.
              03  G100-SELNUM              PIC S9(3).
              03  G100-SCROLLEDWINDOW26.
                04  G100-LIST.
                  05  G100-LIST-COUNT      PIC S9(9)   BINARY.
                  05  G100-LIST-ROW        PIC S9(9)   BINARY.
                  05  G100-LIST-ROWATTR    PIC S9(9)   BINARY.
                  05  G100-LIST-ITEM       OCCURS  200 TIMES.
                    06  G100-LIST-TNUM     PIC X(3).
                    06  G100-LIST-TPGNAME  PIC X(80).
                    06  G100-LIST-TPGID    PIC X(20).
                  05  G100-LIST-SELECT     PIC X       OCCURS  200.

