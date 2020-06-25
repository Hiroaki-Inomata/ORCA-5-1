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
        02  K03X.
      **  02  K03X-FIXED32.
              03  K03X-SELNUM.
                04  K03X-VALUE   PIC S9(3).
              03  K03X-SCROLLEDWINDOW26.
                04  K03X-LIST.
                  05  K03X-LIST-COUNT   PIC S9(9)   BINARY.
                  05  K03X-ROW     PIC S9(9)   BINARY.
                  05  K03X-ROWATTR PIC S9(9)   BINARY.
                  05  K03X-ITEM        OCCURS  20 TIMES.
                    06  K03X-NO     PIC X(3).
                    06  K03X-ADR    PIC X(22).
                    06  K03X-PORT    PIC X(22).
                    06  K03X-MEMO   PIC X(22).
                  05  K03X-LIST-SELECT  PIC X       OCCURS  20 TIMES.
              03  K03X-SCODE-COMBO.
                04  K03X-SCODE.
                  05  K03X-VALUE   PIC X(20).
                04  K03X-SCODE-COUNT   PIC S9(9)   BINARY.
                04  K03X-SCODE-ITEM    PIC X(20)   OCCURS  10 TIMES.
