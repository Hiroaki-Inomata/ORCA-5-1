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
        01  SKT0910.
      *fÃN
            02  SKT0910-PRTYM   PIC X(16).
      *Û¯ÒÔ
            02  SKT0910-HKNJANUM    PIC X(12).
      *¿Nú
            02  SKT0910-SEIYMD  PIC X(22).
      *§Ô
            02  SKT0910-KENNUM  PIC X(4).
      *ãÃ@ÖR[h
            02  SKT0910-HOSPCD  PIC X(20).
      *ãÃ@ÖÝn
            02  SKT0910-ADRS    PIC X(100).
      *ãÃ@Ö¼Ì
            02  SKT0910-HOSPNAME    PIC X(100).
      *JÝÒ¼
            02  SKT0910-KAISETUNAME PIC X(30).
      *y[WÔ
            02  SKT0910-NO  PIC X(10).
      *Ã{Ìt
            02  SKT0910-HKN-TBL.
      *ê
              03  SKT0910-HKN-TBLG        OCCURS  9 TIMES.
      *ü@EOe[u
                04  SKT0910-HKN-G           OCCURS  2 TIMES.
      *Ã{ÌtF
                  05  SKT0910-KENSU   PIC X(6).
      *Ã{ÌtFú
                  05  SKT0910-NISSU   PIC X(6).
      *Ã{ÌtF_
                  05  SKT0910-TENSU   PIC X(13).
      *Ã{ÌtFêSà
                  05  SKT0910-ITBFTN  PIC X(13).
      *HÃ{
            02  SKT0910-SYO-TBL.
      *ê
              03  SKT0910-SYO-TBLG        OCCURS  9 TIMES.
      *HÃ{F
                04  SKT0910-SYOKENSU    PIC X(6).
      *HÃ{Fú
                04  SKT0910-SYONISSU    PIC X(6).
      *HÃ{Fàz
                04  SKT0910-SYOKINGAK   PIC X(13).
      *HÃ{FWSà
                04  SKT0910-SYOFTNGAK   PIC X(13).
            02  SKT0910-TITLE1  PIC X(10).
            02  SKT0910-HYOBETU
                    PIC X(2)    OCCURS  2 TIMES.
      *ÁÊÃ{ï
            02  SKT0910-TOKUBETU        PIC X(10).
