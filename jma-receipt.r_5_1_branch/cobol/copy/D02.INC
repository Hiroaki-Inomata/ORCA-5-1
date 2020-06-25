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
        02  D02.
      *     02  D02-FIXED6.
              03  D02-B06-STATE        PIC S9(9)   BINARY.
              03  D02-B07-STATE        PIC S9(9)   BINARY.
              03  D02-LBLSRYYM-STYLE   PIC X(20).
              03  D02-LBLSRYYM         PIC X(30).
              03  D02-LBLNYUGAI-STYLE  PIC X(20).
              03  D02-LBLNYUGAI        PIC X(20).
              03  D02-SELNUM           PIC S9(5).
              03  D02-PTNUM.
                04  D02-VALUE          PIC X(20).
              03  D02-NAME.
                04  D02-VALUE          PIC X(100).
              03  D02-SCROLLEDWINDOW1.
                04  D02-CHKLST.
                  05  D02-CHKLST-COUNT   PIC S9(9)   BINARY.
                  05  D02-CHKLST-ROW PIC S9(9)   BINARY.
                  05  D02-CHKLST-ROWATTR PIC S9(9)   BINARY.
                  05  D02-CHKLST-ITEM        OCCURS  200 TIMES.
                    06  D02-CHKLST-TNUM      PIC X(5).
                    06  D02-CHKLST-TTESTPT   PIC X(2).
                    06  D02-CHKLST-TPTNUM    PIC X(20).
                    06  D02-CHKLST-TNAME     PIC X(100).
                  05  D02-CHKLST-SELECT
                          PIC X       OCCURS  200 TIMES.
              03  D02-CMB-TAISYO.
                04  D02-TAISYO        PIC X(32).
                04  D02-TAISYO-COUNT  PIC S9(9)   BINARY.
                04  D02-TAISYO-ITEM   PIC X(32)   OCCURS  5 TIMES.
