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
        02  G05.
      *     02  G05-FIXED6.
              03  G05-B06-STATE        PIC S9(9)   BINARY.
              03  G05-B07-STATE        PIC S9(9)   BINARY.
              03  G05-SELNUM           PIC S9(5).
              03  G05-PTNUM.
                04  G05-VALUE          PIC X(20).
              03  G05-NAME.
                04  G05-VALUE          PIC X(100).
              03  G05-SCROLLEDWINDOW1.
                04  G05-PTLST.
                  05  G05-PTLST-COUNT       PIC S9(9)   BINARY.
                  05  G05-PTLST-ROW PIC     S9(9)   BINARY.
                  05  G05-PTLST-ROWATTR     PIC S9(9)   BINARY.
                  05  G05-PTLST-ITEM        OCCURS  200 TIMES.
                    06  G05-PTLST-TNUM      PIC X(5).
                    06  G05-PTLST-TTESTPT   PIC X(2).
                    06  G05-PTLST-TPTNUM    PIC X(20).
                    06  G05-PTLST-TNAME     PIC X(100).
                  05  G05-PTLST-SELECT
                          PIC X       OCCURS  200 TIMES.
