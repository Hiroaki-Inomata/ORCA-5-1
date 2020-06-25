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
       01  HC09.
              03  HC09-HOSPCD      PIC  X(20).
              03  HC09-ADRS        PIC  X(30).
              03  HC09-HOSPNAME    PIC  X(30).
              03  HC09-KAISETUNAME PIC  X(30).
              03  HC09-SEIYMD      PIC  X(22).
              03  HC09-PRTYM       PIC  X(16).
              03  HC09-HKN-TBL.
                  05  HC09-HKN-G   OCCURS  23.
                      07  HC09-KENSU     PIC  X(6).
                      07  HC09-TENSU     PIC  X(11).
                      07  HC09-NISSU     PIC  X(5).
                      07  HC09-ITBFTN    PIC  X(11).
                      07  HC09-YKZFTN    PIC  X(11).
              03  HC09-SYO-TBL.
                  05  HC09-SYO-G    OCCURS  23.
                      07  HC09-SYOKENSU  PIC  X(6).
                      07  HC09-SYONISSU  PIC  X(5).
                      07  HC09-SYOKINGAK PIC  X(11).
                      07  HC09-SYOFTNGAK PIC  X(11).
              03  HC09-GOK-TBL.
                  05  HC09-GOK-G    OCCURS  2.
                      07  HC09-GOKEI     PIC  X(6).
              03  HC09-SYOGOK-TBL.
                  05  HC09-SYOGOK-G OCCURS  2.
                      07  HC09-SYOGOKEI  PIC  X(6).
