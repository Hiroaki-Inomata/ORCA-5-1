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
        02  RID4.
            03  RID4-FIXED1.
              04  RID4-ID1CODE.
                05  RID4-VALUE  PIC X(10).
              05  RID4-LABEL-STYLE  PIC X(20).
              05  RID4-LABEL        PIC X(80).
              05  RID4-LABEL1-STYLE PIC X(20).
              05  RID4-LABEL1       PIC X(110).
              04  RID4-B01.
                05  RID4-B01-LABEL  PIC X(4).
              04  RID4-ID1MSG.
                05  RID4-VALUE  PIC X(90).
