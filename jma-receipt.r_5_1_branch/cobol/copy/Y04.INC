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
        02  Y04.
      *****   02  Y04-FIXED1.
              03  Y04-TYYKYMD1.
                04  Y04-VALUE   PIC X(20).
              03  Y04-TYYKYMD2.
                04  Y04-VALUE   PIC X(20).
              03  Y04-TYYKYMD3.
                04  Y04-VALUE   PIC X(20).
              03  Y04-TYYKYMD4.
                04  Y04-VALUE   PIC X(20).
              03  Y04-TYYKYMD5.
                04  Y04-VALUE   PIC X(20).
              03  Y04-TYYKYMD6.
                04  Y04-VALUE   PIC X(20).
              03  Y04-DRNAME-COMBO.
                04  Y04-DRNAME.
                  05  Y04-VALUE   PIC X(29).
                04  Y04-S1-COUNT     PIC S9(9)   BINARY.
                04  Y04-DRNAMELST    PIC X(29)   OCCURS  100 TIMES.
              03  Y04-SRYNAIYO-COMBO.
                04  Y04-SRYNAIYO.
                  05  Y04-VALUE   PIC X(27).
                04  Y04-S2-COUNT   PIC S9(9)   BINARY.
                04  Y04-SRYNAIYOLST    PIC X(27)   OCCURS  100 TIMES.
              03  Y04-KANJA-LIST1.
                04  Y04-PT-LIST1.
                  05  Y04-LIST1-COUNT   PIC S9(9)   BINARY.
                  05  Y04-LIST1-ROW     PIC S9(9)   BINARY.
                  05  Y04-LIST1-ROWATTR PIC S9(9)   BINARY.
                  05  Y04-ITEM        OCCURS  200 TIMES.
                    06  Y04-LIST1-SRYNAIYO  PIC X(2).
                    06  Y04-LIST1-YYKTIME  PIC X(5).
                    06  Y04-LIST1-NAME  PIC X(10).
                  05  Y04-LIST1-SELECT  PIC X       OCCURS  200 TIMES.
              03  Y04-KANJA-LIST2.
                04  Y04-PT-LIST2.
                  05  Y04-LIST2-COUNT   PIC S9(9)   BINARY.
                  05  Y04-LIST2-ROW     PIC S9(9)   BINARY.
                  05  Y04-LIST2-ROWATTR PIC S9(9)   BINARY.
                  05  Y04-ITEM        OCCURS  200 TIMES.
                    06  Y04-LIST2-SRYNAIYO  PIC X(2).
                    06  Y04-LIST2-YYKTIME  PIC X(5).
                    06  Y04-LIST2-NAME  PIC X(10).
                  05  Y04-LIST2-SELECT  PIC X       OCCURS  200 TIMES.
              03  Y04-KANJA-LIST3.
                04  Y04-PT-LIST3.
                  05  Y04-LIST3-COUNT   PIC S9(9)   BINARY.
                  05  Y04-LIST3-ROW     PIC S9(9)   BINARY.
                  05  Y04-LIST3-ROWATTR PIC S9(9)   BINARY.
                  05  Y04-ITEM        OCCURS  200 TIMES.
                    06  Y04-LIST3-SRYNAIYO  PIC X(2).
                    06  Y04-LIST3-YYKTIME  PIC X(5).
                    06  Y04-LIST3-NAME  PIC X(10).
                  05  Y04-LIST3-SELECT  PIC X       OCCURS  200 TIMES.
              03  Y04-KANJA-LIST4.
                04  Y04-PT-LIST4.
                  05  Y04-LIST4-COUNT   PIC S9(9)   BINARY.
                  05  Y04-LIST4-ROW     PIC S9(9)   BINARY.
                  05  Y04-LIST4-ROWATTR PIC S9(9)   BINARY.
                  05  Y04-ITEM        OCCURS  200 TIMES.
                    06  Y04-LIST4-SRYNAIYO  PIC X(2).
                    06  Y04-LIST4-YYKTIME  PIC X(5).
                    06  Y04-LIST4-NAME  PIC X(10).
                  05  Y04-LIST4-SELECT  PIC X       OCCURS  200 TIMES.
              03  Y04-KANJA-LIST5.
                04  Y04-PT-LIST5.
                  05  Y04-LIST5-COUNT   PIC S9(9)   BINARY.
                  05  Y04-LIST5-ROW     PIC S9(9)   BINARY.
                  05  Y04-LIST5-ROWATTR PIC S9(9)   BINARY.
                  05  Y04-ITEM        OCCURS  200 TIMES.
                    06  Y04-LIST5-SRYNAIYO  PIC X(2).
                    06  Y04-LIST5-YYKTIME  PIC X(5).
                    06  Y04-LIST5-NAME  PIC X(10).
                  05  Y04-LIST5-SELECT  PIC X       OCCURS  200 TIMES.
              03  Y04-KANJA-LIST6.
                04  Y04-PT-LIST6.
                  05  Y04-LIST6-COUNT   PIC S9(9)   BINARY.
                  05  Y04-LIST6-ROW     PIC S9(9)   BINARY.
                  05  Y04-LIST6-ROWATTR PIC S9(9)   BINARY.
                  05  Y04-ITEM        OCCURS  200 TIMES.
                    06  Y04-LIST6-SRYNAIYO        PIC X(2).
                    06  Y04-LIST6-YYKTIME         PIC X(5).
                    06  Y04-LIST6-NAME      PIC X(10).
                  05  Y04-LIST6-SELECT  PIC X       OCCURS  200 TIMES.
