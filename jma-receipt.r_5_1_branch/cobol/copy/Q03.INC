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
        02  Q03.
            03  Q03-FIXED1.
              04  Q03-ID1CODE         PIC X(10).
              04  Q03-ID1MSG          PIC X(80).
              04  Q03-LBLSRYYM        PIC X(10).
              04  Q03-SRYYM           PIC X(10).
              04  Q03-SRYYM-STATE     PIC S9(9)   BINARY.
              04  Q03-CMB-NYUGAIKBN.
                  05  Q03-NYUGAIKBN   PIC X(12).
                  05  Q03-NYUGAIKBN-STATE     PIC S9(9)   BINARY.
                  05  Q03-CMB-NYUGAIKBN-COUNT
                                      PIC S9(9)   BINARY.
                  05  Q03-CMB-NYUGAIKBN-ITEM
                                      PIC X(12)   OCCURS  2.
                  05  Q03-NYUGAIKBN-ITEM-STATE
                                      PIC S9(9)   BINARY.
