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
        01  SKT1701.
            03  SKT1701-HOSPCD            PIC X(14).
            03  SKT1701-SEIKYU-TBL        OCCURS  3 TIMES.
                05  SKT1701-NYUGAI-TBL    OCCURS  2 TIMES.
                    07  SKT1701-KENSU     PIC X(05).
                    07  SKT1701-NISSU     PIC X(05).
                    07  SKT1701-TENSU     PIC X(09).
                    07  SKT1701-ITBFTN    PIC X(09).
                05  SKT1701-SKENSU        PIC X(05).
                05  SKT1701-SNISSU        PIC X(05).
                05  SKT1701-SKINGK        PIC X(09).
                05  SKT1701-SFTNGK        PIC X(09).
            03  SKT1701-ZAITAKU-TBL       OCCURS  2 TIMES.
                05  SKT1701-Z-KENSU       PIC X(05).
                05  SKT1701-Z-NISSU       PIC X(05).
                05  SKT1701-Z-TENSU       PIC X(09).
                05  SKT1701-Z-ITBFTN      PIC X(09).
            03  SKT1701-KOH-SEIKYU-TBL    OCCURS  9 TIMES.
                05  SKT1701-K-KENSU       PIC X(05).
                05  SKT1701-K-NISSU       PIC X(05).
                05  SKT1701-K-TENSU       PIC X(09).
                05  SKT1701-K-YKZFTN      PIC X(07).
                05  SKT1701-K-ITBFTN      PIC X(09).
                05  SKT1701-K-SKENSU      PIC X(05).
                05  SKT1701-K-SNISSU      PIC X(05).
                05  SKT1701-K-SKINGK      PIC X(09).
                05  SKT1701-K-SFTNGK      PIC X(09).
