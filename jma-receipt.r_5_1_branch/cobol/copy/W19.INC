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
        02  W19.
      *      02  W19-FIXED32.
              03  W19-LBLTOUKEI            PIC X(20).
              03  W19-SELNUM               PIC S9(3).
              03  W19-CMB-TYOHYOKBN.
                04  W19-TYOHYOKBN          PIC X(22).
                04  W19-TYOHYOKBN-COUNT    PIC S9(9)   BINARY.
                04  W19-TYOHYOKBN-ITEM     PIC X(22)   OCCURS  2.
              03  W19-SCROLLEDWINDOW26.
                04  W19-LIST.
                  05  W19-LIST-COUNT       PIC S9(9)   BINARY.
                  05  W19-LIST-ROW         PIC S9(9)   BINARY.
                  05  W19-LIST-ROWATTR     PIC S9(9)   BINARY.
                  05  W19-LIST-ITEM        OCCURS  200 TIMES.
                    06  W19-LIST-TNUM      PIC X(3).
                    06  W19-LIST-TPGNAME   PIC X(80).
                    06  W19-LIST-TPGID     PIC X(20).
                    06  W19-LIST-TSTATUS   PIC X(4).
                  05  W19-LIST-SELECT      PIC X       OCCURS  200.
