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
        02  W22.
            03  W22-FIXED6.
              04  W22-SELNUM                 PIC S9(2).
              04  W22-KJNYMD                 PIC X(10).
              04  W22-KBNCD                  PIC X(02).
              04  W22-KBNCD-STATE            PIC S9(9)   BINARY.
              04  W22-SAGAKU                 PIC X(6).
              04  W22-STYUKYMD               PIC X(10).
              04  W22-EDYUKYMD               PIC X(10).
              04  W22-SAGAKULST.
                05  W22-SAGAKULST-COUNT      PIC S9(9)   BINARY.
                05  W22-SAGAKULST-ROW        PIC S9(9)   BINARY.
                05  W22-SAGAKULST-ROWATTR    PIC S9(9)   BINARY.
                05  W22-SAGAKULST-OCC        OCCURS  100 TIMES.
                  06  W22-TNUM               PIC X(2).
                  06  W22-TKBNCD             PIC X(2).
                  06  W22-TSAGAKU            PIC X(7).
                  06  W22-TSTYUKYMD          PIC X(10).
                  06  W22-TEDYUKYMD          PIC X(10).
                05  W22-SAGAKULST-SELECT     PIC X OCCURS  100 TIMES.
