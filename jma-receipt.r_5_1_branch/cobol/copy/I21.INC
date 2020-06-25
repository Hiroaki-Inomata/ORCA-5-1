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
          02  I21.
              03  I21-B04-STATE            PIC S9(9)   BINARY.
              03  I21-B04S-STATE           PIC S9(9)   BINARY.
              03  I21-B06-LABEL            PIC X(8).
              03  I21-B06-STATE            PIC S9(9)   BINARY.
              03  I21-B07-LABEL            PIC X(8).
              03  I21-B07-STATE            PIC S9(9)   BINARY.
              03  I21-B10-STATE            PIC S9(9)   BINARY.
              03  I21-B11-STATE            PIC S9(9)   BINARY.
              03  I21-B12-STATE            PIC S9(9)   BINARY.
              03  I21-DURATION             PIC S9(9)   BINARY.
              03  I21-STSRYYMD  PIC X(10).
              03  I21-STSRYYMD-STATE       PIC S9(9)   BINARY.
              03  I21-EDSRYYMD  PIC X(10).
              03  I21-EDSRYYMD-STATE       PIC S9(9)   BINARY.
              03  I21-LBLMSG01  PIC X(100).
              03  I21-LBLMSG01-STYLE       PIC X(20).
              03  I21-SCROLLEDWINDOW1.
                04  I21-BTULIST.
                  05  I21-BTULIST-COUNT    PIC S9(9)   BINARY.
                  05  I21-BTULIST-ROW      PIC S9(9)   BINARY.
                  05  I21-BTULIST-ROWATTR  PIC S9(9)   BINARY.
                  05  I21-BTULIST-ITEM     OCCURS  100 TIMES.
                    06  I21-TBTUNUM-G.
                        07  I21-TFIL1      PIC X(2).
                        07  I21-TBTUNUM    PIC X(2).
                        07  I21-TFIL2      PIC X(2).
                    06  I21-TBTUNAME       PIC X(10).
                    06  I21-TCNT-ST        PIC X(10).
                    06  I21-TCNT-NYUIN     PIC X(10).
                    06  I21-TCNT-TENNYU    PIC X(10).
                    06  I21-TCNT-TENSTU    PIC X(10).
                    06  I21-TCNT-TAIIN     PIC X(10).
                    06  I21-TCNT-ED        PIC X(10).
                    06  I21-TCNT-TOTAL     PIC X(10).
                    06  I21-TCNT-AVG       PIC X(10).
                  05  I21-BTULIST-SELECT   PIC X
                                           OCCURS  100 TIMES.

