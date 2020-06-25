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
            02  XA01.
              03  XA01-SELNUM.
                04  XA01-VALUE  PIC S9(3).
              03  XA01-TBL-GROUP.
                04  XA01-VALUE  PIC X(14).
              03  XA01-RENNUM.
                04  XA01-VALUE  PIC S9(4).
              03  XA01-SHORI-RENNUM.
                04  XA01-VALUE  PIC S9(4).
              03  XA01-STPAGE.
                04  XA01-VALUE  PIC S9(5).
              03  XA01-EDPAGE.
                04  XA01-VALUE  PIC S9(5).
              03  XA01-SCROLLEDWINDOW26.
                04  XA01-LIST.
                  05  XA01-HEAD1.
                    06  XA01-VALUE         PIC X(8).
                  05  XA01-HEAD2.
                    06  XA01-VALUE         PIC X(10).
                  05  XA01-COUNT           PIC S9(9)   BINARY.
                  05  XA01-ROW             PIC S9(9)   BINARY.
                  05  XA01-ROWATTR         PIC S9(9)   BINARY.
                  05  XA01-ITEM            OCCURS  100 TIMES.
                    06  XA01-TNO           PIC X(3).
                    06  XA01-TTBL-GROUP    PIC X(14).
                    06  XA01-TRENNUM       PIC X(4).
                    06  XA01-TSHORI-RENNUM PIC X(4).
                    06  XA01-TPAGE         PIC X(5).
                    06  XA01-TSRYYM        PIC X(6).
                    06  XA01-TSKYYMD       PIC X(9).
                    06  XA01-TSHORI-TITLE  PIC X(20).
                    06  XA01-TPRT-TITLE    PIC X(60).
                    06  XA01-TPRTFLG       PIC X(2).
                  05  XA01-SELECT          PIC X     OCCURS  100 TIMES.

