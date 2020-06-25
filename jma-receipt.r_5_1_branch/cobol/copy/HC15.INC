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
      01  HC15.
              03  HC15-DRCD    PIC  X(20).
              03  HC15-HAKYMD    PIC  X(22).
              03  HC15-TITLE    PIC  X(14).
              03  HC15-PAGE    PIC  X(3).
              03  HC15-TITLE2    PIC  X(20).
              03  HC15-YYKYMD     PIC  X(28)  OCCURS   6.
      *
              03  HC15-TBL           OCCURS 6.
                  05  HC15-SRYNAIYO      PIC  X(10) OCCURS   33.
                  05  HC15-YYKTIME       PIC  X(5)  OCCURS   33.
                  05  HC15-NAME          PIC  X(14) OCCURS   33.
      *
