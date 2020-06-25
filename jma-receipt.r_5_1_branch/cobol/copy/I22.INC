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
        02  I22.
            03  I22-FIXED1.
              04  I22-ID1CODE         PIC X(10).
              04  I22-ID1MSG          PIC X(80).
              04  I22-LBLFILENAME     PIC X(14).
              04  I22-FILENAME        PIC X(100).
              04  I22-FILENAME-STATE  PIC S9(9)   BINARY.
              04  I22-LBLNUMSEL       PIC X(10).
              04  I22-LBLNUMSEL2      PIC X(2).
              04  I22-STNUM           PIC S9(9).
              04  I22-STNUM-STATE     PIC S9(9)   BINARY.
              04  I22-STNUM-VISIBLE   PIC X(1). 
              04  I22-EDNUM           PIC S9(9).
              04  I22-EDNUM-STATE     PIC S9(9)   BINARY.
              04  I22-EDNUM-VISIBLE   PIC X(1). 
              04  I22-FILE-COMBO.
                05  I22-FILE.
                  07  I22-VALUE    PIC X(22).
                05  I22-FILE-STATE  PIC S9(9)   BINARY.
                05  I22-FILE-COUNT PIC S9(9)   BINARY.
                05  I22-FILE-ITEM
                        PIC X(22)   OCCURS  6 TIMES.
                05  I22-FILE-ITEM-STATE  PIC S9(9)   BINARY.
              04  I22-CDKBN-COMBO.
                05  I22-CDKBN.
                  07  I22-VALUE    PIC X(22).
                05  I22-CDKBN-STATE  PIC S9(9)   BINARY.
                05  I22-CDKBN-COUNT PIC S9(9)   BINARY.
                05  I22-CDKBN-ITEM
                        PIC X(22)   OCCURS  5 TIMES.
                05  I22-CDKBN-ITEM-STATE  PIC S9(9)   BINARY.
