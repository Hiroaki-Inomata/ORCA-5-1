      *******************************************************************
      * Project code name "ORCA"
      * úãWZvg\tgiJMA standard receipt softwarej
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
      *  Rs[åC³ð
      * Maj/Min/Rev  C³Ò  út        àe
      *  01.00.01    gì    2010/02/03  ÁÊÃ{ïÛÎ
      *  01.00.02    äã    2018/10/12  C³ðRgí
      ********************************************************************
        01  SKT0911.
      *Û¯ÒÔ
            02  SKT0911-HKNJANUM    PIC X(12).
      *§Ô
            02  SKT0911-KENNUM  PIC X(10).
      *ãÃ@ÖR[h
            02  SKT0911-HOSPCD  PIC X(20).
      *y[W
            02  SKT0911-NO  PIC X(10).
      *ÁÊÃ{ï
            02  SKT0911-TOKUBETU        PIC X(10).
      *öï¿ê
            02  SKT0911-KOH-TBL         OCCURS  4 TIMES.
      *Ã{ÌtFü@EO
              03  SKT0911-KOH-G           OCCURS  2 TIMES.
      *Ã{ÌtF
                04  SKT0911-KOH-KENSU   PIC X(6).
      *Ã{ÌtFú
                04  SKT0911-KOH-NISSU   PIC X(6).
      *Ã{ÌtF_
                04  SKT0911-KOH-TENSU   PIC X(13).
      *Ã{ÌtFêSà
                04  SKT0911-KOH-ITBFTN  PIC X(13).
      *****
      *HÃ{F
              03  SKT0911-KOH-SYOG.
      *HÃ{F
                04  SKT0911-KOH-SYOKENSU    PIC X(6).
      *HÃ{Fú
                04  SKT0911-KOH-SYONISSU    PIC X(6).
      *HÃ{Fàz
                04  SKT0911-KOH-SYOKINGAK   PIC X(13).
      *HÃ{FWSà
                04  SKT0911-KOH-SYOFTNGAK   PIC X(13).
      *öïÔ
              03  SKT0911-KOH-NUM PIC X(3).
      *öï¼Ì
              03  SKT0911-KOH-NAME    PIC X(20).
