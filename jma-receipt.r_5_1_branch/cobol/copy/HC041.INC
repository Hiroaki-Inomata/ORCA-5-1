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
       01  HC041.
              03  HC041-HOSPCD      PIC  X(20).
              03  HC041-ADRS        PIC  X(50).
              03  HC041-HOSPNAME    PIC  X(100).
              03  HC041-KAISETUNAME PIC  X(30).
              03  HC041-KYURATE-TBL         OCCURS  3.
                  05  HC041-KYURATE-TBL     OCCURS  2.
                      07  HC041-KYURATE-G   OCCURS  2.
                          09  HC041-KYURATE  PIC  X(1).
              03  HC041-HKN-TBL.
                  05  HC041-HKN-TBL1     OCCURS  3.
                      07  HC041-HKN-G    OCCURS  2.
                          09  HC041-HKN  OCCURS  2.
                              11  HC041-KENSU    PIC  X(5).
                              11  HC041-TENSU    PIC  X(11).
                              11  HC041-NISSU    PIC  X(5).
                              11  HC041-ITBFTN   PIC  X(10).
                              11  HC041-YKZFTN   pic  X(9).
              03  HC041-ROU-TBL.
                  07  HC041-ROU-G    OCCURS  2.
                      09  HC041-ROUKENSU    PIC  X(5).
                      09  HC041-ROUTENSU    PIC  X(11).
                      09  HC041-ROUNISSU    PIC  X(5).
                      09  HC041-ROUITBFTN   PIC  X(10).
                      09  HC041-ROUYKZFTN   pic  X(9).
              03  HC041-KOH-TBL.
                  05  HC041-KOH-TBL1     OCCURS  2.
                      07  HC041-KOH-G    OCCURS  2.
                          09  HC041-KOHKENSU    PIC  X(5).
                          09  HC041-KOHTENSU    PIC  X(11).
                          09  HC041-KOHNISSU    PIC  X(5).
                          09  HC041-KOHITBFTN   PIC  X(10).
                          09  HC041-KOHYKZFTN   PIC  X(9).
              03  HC041-SYO-TBL.
                  05  HC041-SYO-TBL1         OCCURS  3.
                      07  HC041-SYO-G        OCCURS  2.
                          09  HC041-SYOKENSU     PIC  X(5).
                          09  HC041-SYONISSU     PIC  X(5).
                          09  HC041-SYOKINGAK    PIC  X(13).
                          09  HC041-SYOFTNGAK    PIC  X(9).
              03  HC041-ROUSYO-TBL.
                  05  HC041-ROUSYOKENSU      PIC  X(5).
                  05  HC041-ROUSYONISSU      PIC  X(5).
                  05  HC041-ROUSYOKINGAK     PIC  X(13).
                  05  HC041-ROUSYOFTNGAK     PIC  X(9).
              03  HC041-KOHSYO-TBL.
                  05  HC041-KOHSYO-G     OCCURS  2.
                      07  HC041-KOHSYOKENSU  PIC  X(5).
                      07  HC041-KOHSYONISSU  PIC  X(5).
                      07  HC041-KOHSYOKINGAK PIC  X(13).
                      07  HC041-KOHSYOFTNGAK PIC  X(9).
              03  HC041-PRTYM      PIC  X(16).
              03  HC041-SEIYMD     PIC  X(22).
              03  HC041-HOKENJYANAME    PIC  X(20).
              03  HC041-KOHNUM-TBL.
                  05  HC041-KOHNUM-TBL1  OCCURS  2.
                      07  HC041-KOHNUM   PIC  X(3).
                      07  HC041-KOHNAME  PIC  X(12).
              03  HC041-HKNJANUM         PIC  X(08).
