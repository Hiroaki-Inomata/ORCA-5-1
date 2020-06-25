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
       01  HC04.
              03  HC04-HOSPCD      PIC  X(20).
              03  HC04-ADRS        PIC  X(50).
              03  HC04-HOSPNAME    PIC  X(100).
              03  HC04-KAISETUNAME PIC  X(30).
              03  HC04-HKN-TBL.
                  05  HC04-HKN-TBL1     OCCURS  4.
                      07  HC05-HKN-G    OCCURS  2.
                          09  HC04-KENSU    PIC  X(5).
                          09  HC04-TENSU    PIC  X(11).
                          09  HC04-NISSU    PIC  X(5).
                          09  HC04-ITBFTN   PIC  X(8).
                          09  HC04-YKZFTN   pic  X(9).
              03  HC04-KOH-TBL.
                  05  HC04-KOH-TBL1     OCCURS  2.
                      07  HC05-KOH-G    OCCURS  2.
                          09  HC04-KOHKENSU    PIC  X(5).
                          09  HC04-KOHTENSU    PIC  X(11).
                          09  HC04-KOHNISSU    PIC  X(5).
                          09  HC04-KOHITBFTN   PIC  X(8).
                          09  HC04-KOHYKZFTN   PIC  X(9).
              03  HC04-SYO-TBL.
                  05  HC04-SYO-G        OCCURS  4.
                      07  HC04-SYOKENSU     PIC  X(5).
                      07  HC04-SYONISSU     PIC  X(5).
                      07  HC04-SYOKINGAK    PIC  X(13).
                      07  HC04-SYOFTNGAK    PIC  X(9).
              03  HC04-KOHSYO-TBL.
                  05  HC04-KOHSYO-G     OCCURS  2.
                      07  HC04-KOHSYOKENSU  PIC  X(5).
                      07  HC04-KOHSYONISSU  PIC  X(5).
                      07  HC04-KOHSYOKINGAK PIC  X(13).
                      07  HC04-KOHSYOFTNGAK PIC  X(9).
              03  HC04-PRTYM      PIC  X(16).
              03  HC04-SEIYMD     PIC  X(22).
              03  HC04-HOKENJYANAME    PIC  X(20).
              03  HC04-KOHNUM-TBL.
                  05  HC04-KOHNUM-TBL1  OCCURS  2.
                      07  HC04-KOHNUM   PIC  X(3).
                      07  HC04-KOHNAME  PIC  X(12).
              03  HC04-HKNJANUM         PIC  X(08).
