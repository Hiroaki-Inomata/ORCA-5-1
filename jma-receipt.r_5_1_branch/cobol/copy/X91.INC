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
        02  X91.
      *     02  FIXED2.
              03  X91-KJNYMD    PIC X(10).
              03  X91-CHKYKZBYO.
                04  X91-VALUE   PIC X.
              03  X91-CHKSRYBYO.
                04  X91-VALUE   PIC X.
              03  X91-CHKYKZHEI.
                04  X91-VALUE   PIC X.
              03  X91-CHKSRYHEI.
                04  X91-VALUE   PIC X.
              03  X91-CHKYKZKNKBYO.
                04  X91-VALUE   PIC X.
              03  X91-CHKSRYSNT.
                04  X91-VALUE   PIC X.
              03  X91-CHKSRYHEIA.
                04  X91-VALUE   PIC X.
              03  X91-CHKSRYHEIB.
                04  X91-VALUE   PIC X.
              03  X91-CHKTEKBYOYKZ.
                04  X91-VALUE   PIC X.
              03  X91-CHKTEKBYOSRY.
                04  X91-VALUE   PIC X.
              03  X91-CHKSRYRANGE.
                04  X91-VALUE   PIC X.
              03  X91-STSRYCD-G.
                04  X91-STSRYCD PIC X(9).
                04  X91-STSRYCD-STATE
                                PIC S9(9)   BINARY.
              03  X91-STSRYNAME-G.
                04  X91-STSRYNAME
                                PIC X(100).
                04  X91-STSRYNAME-STYLE
                                      PIC X(20).
                04  X91-STSRYNAME-STATE
                                PIC S9(9)   BINARY.
              03  X91-EDSRYCD-G.
                04  X91-EDSRYCD PIC X(9).
                04  X91-EDSRYCD-STATE
                                PIC S9(9)   BINARY.
              03  X91-CMB-SORT.
                04  X91-SORT.
                  05  X91-VALUE   PIC X(30).
                04  X91-SORT-COUNT   PIC S9(9)   BINARY.
                04  X91-SORT-ITEM
                        PIC X(30)   OCCURS  5 TIMES.
              03  X91-CHKBYOYKZ.
                04  X91-VALUE   PIC X.
              03  X91-CHKBYOSRY.
                04  X91-VALUE   PIC X.
              03  X91-CHKKAISU.
                04  X91-VALUE   PIC X.
              03  X91-CMB-MOREKIKAN.
                04  X91-MOREKIKAN.
                  05  X91-VALUE   PIC X(40).
                04  X91-MOREKIKAN-COUNT   PIC S9(9)   BINARY.
                04  X91-MOREKIKAN-ITEM
                        PIC X(40)   OCCURS  5 TIMES.
              03  X91-CHKMOREYKZBYO.
                04  X91-VALUE   PIC X.
              03  X91-CHKMOREKENSA.
                04  X91-VALUE   PIC X.
              03  X91-CHKMOREBUI.
                04  X91-VALUE   PIC X.
              03  X91-CMB-MORESORT.
                04  X91-MORESORT.
                  05  X91-VALUE   PIC X(30).
                04  X91-MORESORT-COUNT   PIC S9(9)   BINARY.
                04  X91-MORESORT-ITEM
                        PIC X(30)   OCCURS  5 TIMES.
