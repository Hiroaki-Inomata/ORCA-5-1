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
       01  HC10.
              03  HC10-HOSPCD    PIC  X(20).
              03  HC10-KOH-TBL.
                  05  HC10-KOH-TBL1      OCCURS  3.
                      07  HC10-KOH-G     OCCURS  6.
                          09  HC10-KOHKENSU  PIC  X(5).
                          09  HC10-KOHFTNGAK PIC  X(10).
                          09  HC10-KOHTENSU  PIC  X(10).
                          09  HC10-KOHYKZFTN PIC  X(10).
                          09  HC10-KOHKBN    PIC  X(28).
              03  HC10-SYO-TBL.
                  05  HC10-SYO-TBL1       OCCURS  3.
                      07  HC10-SYO-G      OCCURS  6.
                          09  HC10-SYOKENSU  PIC  X(6).
                          09  HC10-SYOKINGAK PIC  X(11).
                          09  HC10-SYOFTNGAK PIC  X(11).
              03  HC10-GKENSU    PIC  X(7).
              03  HC10-GKINGAK   PIC  X(10).
              03  HC10-GOKEI     PIC  X(6).
              03  HC10-SYOGOKEI  PIC  X(6).
