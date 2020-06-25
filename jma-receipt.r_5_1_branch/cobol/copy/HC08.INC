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
       01  HC08.
              03  HC08-HOSPCD      PIC  X(20).
              03  HC08-ADRS        PIC  X(50).
              03  HC08-HOSPNAME    PIC  X(30).
              03  HC08-KAISETUNAME PIC  X(30).
              03  HC08-SEIYMD      PIC  X(22).
              03  HC08-PRTYM       PIC  X(16).
              03  HC08-GKENSU      PIC  X(7).
              03  HC08-GKINGAK     PIC  X(10).
              03  HC08-HKN-TBL.
                  05  HC08-HKN-G OCCURS  23.
                      07  HC08-KENSU     PIC  X(5).
                      07  HC08-TENSU     PIC  X(10).
                      07  HC08-NISSU     PIC  X(5).
                      07  HC08-ITBFTN    PIC  X(10).
                      07  HC08-YKZFTN    PIC  X(10).
              03  HC08-GOK-TBL.
                  05  HC08-GOK-G OCCURS  3.
                      07  HC08-GOKEI     PIC  X(5).
              03  HC08-KOH-TBL.
                  05  HC08-KOH-TBL1      OCCURS  3.
                      07  HC08-KOH-G     OCCURS  6.
                          09  HC08-KOHKENSU  PIC  X(5).
                          09  HC08-KOHFTNGAK PIC  X(10).
                          09  HC08-KOHTENSU  PIC  X(10).
                          09  HC08-KOHYKZFTN PIC  X(10).
                          09  HC08-KOHKBN    PIC  X(28).
