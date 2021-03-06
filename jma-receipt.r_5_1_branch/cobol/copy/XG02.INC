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
        02  XG02.
            03  XG02-FIXED1.
              04  XG02-FILENAME        PIC X(100).
              04  XG02-FILE-COMBO.
                05  XG02-FILE.
                  07  XG02-VALUE    PIC X(22).
                05  XG02-FILE-COUNT PIC S9(9)   BINARY.
                05  XG02-FILE-ITEM
                        PIC X(22)   OCCURS  6 TIMES.
              04  XG02-CDKBN-COMBO.
                05  XG02-CDKBN.
                  07  XG02-VALUE    PIC X(22).
                05  XG02-CDKBN-COUNT PIC S9(9)   BINARY.
                05  XG02-CDKBN-ITEM
                        PIC X(22)   OCCURS  5 TIMES.
