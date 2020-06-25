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
        01  SKT0911o.
            02  SKT0911o-HKNJANUM    PIC X(12).
            02  SKT0911o-KENNUM  PIC X(10).
            02  SKT0911o-HOSPCD  PIC X(20).
            02  SKT0911o-NO  PIC X(10).
            02  SKT0911o-HKN-TBL         OCCURS  2 TIMES.
              03  SKT0911o-HKN-G           OCCURS  2 TIMES.
                04  SKT0911o-HKN-KENSU   PIC X(6).
                04  SKT0911o-HKN-NISSU   PIC X(6).
                04  SKT0911o-HKN-TENSU   PIC X(13).
                04  SKT0911o-HKN-ITBFTN  PIC X(13).
              03  SKT0911o-HKN-SYOG.
                04  SKT0911o-HKN-SYOKENSU    PIC X(6).
                04  SKT0911o-HKN-SYONISSU    PIC X(6).
                04  SKT0911o-HKN-SYOKINGAK   PIC X(13).
                04  SKT0911o-HKN-SYOFTNGAK   PIC X(13).
            02  SKT0911o-KOH-TBL         OCCURS  2 TIMES.
              03  SKT0911o-KOH-G           OCCURS  2 TIMES.
                04  SKT0911o-KOH-KENSU   PIC X(6).
                04  SKT0911o-KOH-NISSU   PIC X(6).
                04  SKT0911o-KOH-TENSU   PIC X(13).
                04  SKT0911o-KOH-ITBFTN  PIC X(13).
              03  SKT0911o-KOH-SYOG.
                04  SKT0911o-KOH-SYOKENSU    PIC X(6).
                04  SKT0911o-KOH-SYONISSU    PIC X(6).
                04  SKT0911o-KOH-SYOKINGAK   PIC X(13).
                04  SKT0911o-KOH-SYOFTNGAK   PIC X(13).
              03  SKT0911o-KOH-NUM PIC X(3).
              03  SKT0911o-KOH-NAME    PIC X(12).
