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
        02  L07.
      *      02  L07-FIXED32.
              03  L07-LBLTOUKEI            PIC X(20).
              03  L07-SELNUM               PIC S9(3).
              03  L07-CMB-TYOHYOKBN.
                04  L07-TYOHYOKBN          PIC X(22).
                04  L07-TYOHYOKBN-COUNT    PIC S9(9)   BINARY.
                04  L07-TYOHYOKBN-ITEM     PIC X(22)   OCCURS  2.
              03  L07-SCROLLEDWINDOW26.
                04  L07-LIST.
                  05  L07-LIST-COUNT       PIC S9(9)   BINARY.
                  05  L07-LIST-ROW         PIC S9(9)   BINARY.
                  05  L07-LIST-ROWATTR     PIC S9(9)   BINARY.
                  05  L07-LIST-ITEM        OCCURS  200 TIMES.
                    06  L07-LIST-TNUM      PIC X(3).
                    06  L07-LIST-TPGNAME   PIC X(80).
                    06  L07-LIST-TPGID     PIC X(20).
                    06  L07-LIST-TSTATUS   PIC X(4).
                  05  L07-LIST-SELECT      PIC X       OCCURS  200.

