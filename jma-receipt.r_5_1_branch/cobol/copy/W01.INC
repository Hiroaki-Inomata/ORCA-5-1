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
        02  W01.
      ***   02  W01-FIXED1.
              03  W01-SCROLLEDWINDOW1.
                04  W01-TENSU-LIST.
                  05  W01-COUNT   PIC S9(9)   BINARY.
                  05  W01-ROW     PIC S9(9)   BINARY.
                  05  W01-ROWATTR PIC S9(9)   BINARY.
                  05  W01-ITEM        OCCURS  100 TIMES.
                    07  W01-TSELNUM     PIC X(3).
                    07  W01-TSTYUKYMD   PIC X(9).
                    07  W01-TEDYUKYMD   PIC X(9).
                  05  W01-SELECT  PIC X       OCCURS  100 TIMES.
              03  W01-STYUKYMD.
                04  W01-VALUE   PIC X(10).
              03  W01-EDYUKYMD.
                04  W01-VALUE   PIC X(10).
              03  W01-SELNUM.
                04  W01-VALUE   PIC S9(3).
              03  W01-MIDASI.
                04  W01-VALUE   PIC X(20).
              03  W01-KBNCD-COMBO.
                04  W01-KBNCD.
                  05  W01-VALUE   PIC X(35).
                04  W01-KBNCD-COUNT   PIC S9(9)   BINARY.
                04  W01-KBNCD-ITEM    PIC X(35)   OCCURS  200 TIMES.
              03  W01-KANRICD-COMBO.
                04  W01-KANRICD.
                  05  W01-VALUE   PIC X(35).
                04  W01-KANRICD-COUNT   PIC S9(9)   BINARY.
                04  W01-KANRICD-ITEM    PIC X(35)   OCCURS  100 TIMES.
