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
        02  TID2.
            03  TID2-FIXED1.
              04  TID2-B10-STATE   PIC S9(9)   BINARY.
              04  TID2-B10-VISIBLE  PIC X(01).
              04  TID2-B10-LABEL  PIC X(14).
              04  TID2-B11-STATE   PIC S9(9)   BINARY.
              04  TID2-B11-VISIBLE  PIC X(01).
              04  TID2-B11-LABEL  PIC X(14).
              04  TID2-B12-LABEL  PIC X(14).
              04  TID2-ID1CODE.
                05  TID2-VALUE  PIC X(10).
              04  TID2-ID1MSG-STYLE  PIC X(20).
              04  TID2-ID1MSG.
                05  TID2-VALUE  PIC X(144).
              04  TID2-LABEL-STYLE  PIC X(20).
              04  TID2-LABEL        PIC X(120).
