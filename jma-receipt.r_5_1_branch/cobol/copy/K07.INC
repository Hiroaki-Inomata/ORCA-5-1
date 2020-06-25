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
        02  K07.
      ****   02  K07-FIXED32.
              03  K07-SELNUM.
                04  K07-VALUE   PIC S9(4).
              03  K07-SCROLLEDWINDOW26.
                04  K07-LIST.
                  05  K07-COUNT   PIC S9(9)   BINARY.
                  05  K07-ROW     PIC S9(9)   BINARY.
                  05  K07-ROWATTR PIC S9(9)   BINARY.
                  05  K07-ITEM        OCCURS  150 TIMES.
                    06  K07-NO      PIC X(4).
                    06  K07-SRYYMD  PIC X(14).
                    06  K07-SINSATSU  PIC X(4).
                    06  K07-NAIFUKU  PIC X(4).
                    06  K07-TONPUKU PIC X(4).
                    06  K07-GAIYOU  PIC X(4).
                    06  K07-CYUSYA  PIC X(4).
                    06  K07-SYOCHI  PIC X(4).
                    06  K07-OP      PIC X(4).
                    06  K07-MASUI   PIC X(4).
                    06  K07-KENSA   PIC X(4).
                    06  K07-GAZOU  PIC X(4).
                    06  K07-SONOTA  PIC X(4).
                  05  K07-SELECT  PIC X       OCCURS  150 TIMES.
