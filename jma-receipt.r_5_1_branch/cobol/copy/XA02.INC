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
            02  XA02.
              03  XA02-B06S-STATE               PIC S9(9)   BINARY.
              03  XA02-B07S-STATE               PIC S9(9)   BINARY.
              03  XA02-SELNUM.
                04  XA02-VALUE  PIC S9(3).
              03  XA02-SKYYMD.
                04  XA02-VALUE  PIC X(10).
              03  XA02-RENNUM.
                04  XA02-VALUE  PIC S9(4).
              03  XA02-SHORI-RENNUM.
                04  XA02-VALUE  PIC S9(4).
              03  XA02-STPAGE.
                04  XA02-VALUE  PIC S9(5).
              03  XA02-EDPAGE.
                04  XA02-VALUE  PIC S9(5).
              03  XA02-SCROLLEDWINDOW26.
                04  XA02-LIST.
                  05  XA02-HEAD1.
                    06  XA02-VALUE         PIC X(8).
                  05  XA02-HEAD2.
                    06  XA02-VALUE         PIC X(10).
                  05  XA02-COUNT           PIC S9(9)   BINARY.
                  05  XA02-ROW             PIC S9(9)   BINARY.
                  05  XA02-ROWATTR         PIC S9(9)   BINARY.
                  05  XA02-ITEM            OCCURS  300 TIMES.
                    06  XA02-TNO           PIC X(3).
                    06  XA02-GYOMUNM       PIC X(12).
                    06  XA02-LPTNUM        PIC X(20).
                    06  XA02-PTNAME        PIC X(50).
                    06  XA02-TPRT-TITLE    PIC X(60).
                    06  XA02-SAKU-YMD      PIC X(9).
                    06  XA02-SAKU-TIME     PIC X(8).
                    06  XA02-TSKYYMD       PIC X(9).
                    06  XA02-TRENNUM       PIC X(4).
                    06  XA02-TSHORI-RENNUM PIC X(4).
                    06  XA02-TPAGE         PIC X(5).
                  05  XA02-SELECT          PIC X     OCCURS  300 TIMES.
              03  XA02-GYOMU-COMBO.
                04  XA02-GYOMUCD.
                  05  XA02-G-VALUE         PIC X(35).
                04  XA02-G-COUNT           PIC S9(9)   BINARY.
                04  XA02-G-ITEM            PIC X(35)
                                           OCCURS  100 TIMES.
              03  XA02-PTNUM.
                04  XA02-VALUE  PIC X(20).
              03  XA02-PTNAME-G.
                04  XA02-PTNAME-STYLE      PIC X(20).
                04  XA02-PTNAME-VALUE      PIC X(50).
              03  XA02-CREYMD.
                04  XA02-VALUE  PIC X(10).
              03  XA02-PRT-TITLE.
                04  XA02-VALUE  PIC X(60).
