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
          01  HC27. 
              03  HC27-PRTYM       PIC  X(16).
              03  HC27-HOSPCD      PIC  X(20).
              03  HC27-HOSPNAME    PIC  X(30).
              03  HC27-NAME        PIC  X(12)    OCCURS  84.
              03  HC27-KENSU-G     OCCURS  3.
                  05  HC27-KENSU   PIC  X(5)     OCCURS  84.
              03  HC27-KOHNAME     PIC  X(22)    OCCURS  6.
              03  HC27-KOHKENSU    PIC  X(5)     OCCURS  6.
