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
        02  Z90.
      **    02  Z90-FIXED1.
              03  Z90-SRYNAME.
                04  Z90-VALUE   PIC X(100).
              03  Z90-SRYCDCHG      OCCURS  5.
                04  Z90-VALUE   PIC X(20).
              03  Z90-SRYCDNAME     OCCURS  5.
                04  Z90-VALUE   PIC X(100).
              03  Z90-SRYCD.
                04  Z90-VALUE   PIC X(20).
