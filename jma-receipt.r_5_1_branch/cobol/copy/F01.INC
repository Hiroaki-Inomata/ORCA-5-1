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
        02  F01.
            03  F01-FIXED6.
              04  F01-FILENAME.
                05  F01-VALUE   PIC X(129).
              04  F01-CHKBTN    OCCURS  120  TIMES.
                05  F01-VALUE1  PIC X.
              04  F01-CHKMSG    OCCURS  120  TIMES.
                05  F01-VALUE2  PIC X(30).
              04  F01-LBLCAUTION-STYLE  PIC  X(20).
              04  F01-LBLCAUTION        PIC  X(100).
