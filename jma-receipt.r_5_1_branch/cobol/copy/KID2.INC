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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
        02  KID2.
    ******* 02  KID2-FIXED1.
              03  KID2-ID2CODE.
                04  KID2-VALUE  PIC X(10).
              03  KID2-ID2MSG.
                04  KID2-VALUE  PIC X(100).
              03  KID2-USERPG-COMB.
                04  KID2-USERPG-STATE   PIC S9(9)   BINARY.
                04  KID2-USERPG.
                  05  KID2-VALUE  PIC X(20).
                04  KID2-USERPG-COUNT  PIC S9(9)   BINARY.
                04  KID2-USERPG-ITEM
                        PIC X(20)   OCCURS  2 TIMES.
