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
        02  TID3.
            03  TID3-FIXED1.
              04  TID3-B12-LABEL  PIC X(14).
              04  TID3-ID1CODE.
                05  TID3-VALUE  PIC X(10).
              04  TID3-ID1MSG-STYLE  PIC X(20).
              04  TID3-ID1MSG.
                05  TID3-VALUE  PIC X(100).
              04  TID3-LABEL-STYLE  PIC X(20).
              04  TID3-LABEL        PIC X(80).
