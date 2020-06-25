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
        02  Z100.
            03  Z100-FIXED1.
              04  Z100-ID1CODE         PIC X(10).
              04  Z100-ID1MSG          PIC X(80).
              04  Z100-FILENAME-G      OCCURS  8 TIMES.
                05  Z100-FILENAME      PIC X(100).
                05  Z100-FILENAME-STATE    PIC S9(9)   BINARY.
              04  Z100-FILE-COMBO.
                05  Z100-FILE-STATE  PIC S9(9)   BINARY.
                05  Z100-FILE.
                  07  Z100-VALUE    PIC X(22).
                05  Z100-FILE-COUNT PIC S9(9)   BINARY.
                05  Z100-FILE-ITEM
                        PIC X(22)   OCCURS  6 TIMES.
              04  Z100-CDKBN-COMBO.
                05  Z100-CDKBN-STATE  PIC S9(9)   BINARY.
                05  Z100-CDKBN.
                  07  Z100-VALUE    PIC X(22).
                05  Z100-CDKBN-COUNT PIC S9(9)   BINARY.
                05  Z100-CDKBN-ITEM
                        PIC X(22)   OCCURS  5 TIMES.
