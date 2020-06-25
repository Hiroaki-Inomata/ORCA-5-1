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
          01  HC24.
              03  HC24-PTNAME    PIC  X(20).
              03  HC24-HOSPNAME    PIC  X(30).
              03  HC24-HOSPNUM    PIC  X(30).
              03  HC24-PTAGE    PIC  X(3).
              03  HC24-TEKIYO        PIC  X(52)  OCCURS  118.
              03  HC24-SRYKBN        PIC  X(2)   OCCURS  118.
              03  HC24-SAIGAIKBN    PIC  X(8).
              03  HC24-PTNUM    PIC  X(30).
              03  HC24-NENNUM   PIC  X(9).
              03  HC24-HKNNUM   PIC  X(14).
