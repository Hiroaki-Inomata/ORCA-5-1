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
        02  P100.
            03  P100-FIXED1                    OCCURS 2.
              04  P100-FRA-KANJI.
                05  P100-FIXED2.
                  06  P100-KANANAME           PIC X(50).
                  06  P100-SCR-KANJILST.
                    07  P100-KANJILST.
                      08  P100-KANJILST-COUNT  PIC S9(9)   BINARY.
                      08  P100-KANJILST-ROW    PIC S9(9)   BINARY.
                      08  P100-KANJILST-ROWATTR   PIC S9(9)   BINARY.
                      08  P100-KANJILST-ITEM   OCCURS  50.
                        09  P100-KANJILST-NO   PIC X(02).
                        09  P100-KANJILST-KANJI
                                               PIC X(50).
                      08  P100-KANJILST-SELECT PIC X
                                               OCCURS  50.
                  06  P100-SELNUM              PIC S9(3).
                  06  P100-SELNUM-STATE        PIC S9(9)   BINARY.
                  06  P100-KANJINAME           PIC X(50).
