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
        02  Y03.
      **** 02  Y03-FIXED1.
              03  Y03-SRYMD.
                04  Y03-VALUE   PIC X(20).
              03  Y03-HYODAI.
                04  Y03-VALUE   PIC X(20).
              03  Y03-SCROLLEDWINDOW1.
                04  Y03-TSLIST.
                  05  Y03-COUNT   PIC S9(9)   BINARY.
                  05  Y03-ROW       PIC S9(9)   BINARY.
                  05  Y03-ROWATTR   PIC S9(9)   BINARY.
                  05  Y03-ITEM        OCCURS  200 TIMES.
                        08  Y03-TNUM        PIC X(4).
                        08  Y03-TYYKTIME    PIC X(5).
                        08  Y03-TSRYKA      PIC X(20).
                        08  Y03-TSRYNAIYO   PIC X(24).
                        08  Y03-TDRNAME     PIC X(24).
                        08  Y03-TNAME       PIC X(20).
                        08  Y03-TPTNUM      PIC X(20).
                        08  Y03-TYYKKBN     PIC X(20).
                        08  Y03-TRENRAKU    PIC X(16).
                        08  Y03-TRAIIN      PIC X(4).
                  05  Y03-SELECT  PIC X       OCCURS  200 TIMES.
              03  Y03-SRYNAIYO-COB.
                04  Y03-SRYNAIYO.
                  05  Y03-VALUE   PIC X(27).
                04  Y03-SRYNAIY-COUNT   PIC S9(9)   BINARY.
                04  Y03-SRYNAIY-ITEM    PIC X(27)   OCCURS 100 TIMES.
              03  Y03-DRNAME-COB.
                04  Y03-DRNAME.
                  05  Y03-VALUE   PIC X(29).
                04  Y03-DRNAME-COUNT   PIC S9(9)   BINARY.
                04  Y03-DRNAME-ITEM    PIC X(29)   OCCURS  100 TIMES.
