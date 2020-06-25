      *******************************************************************
      * Project code name "ORCA"
      * Æü°åÉ¸½à¥ì¥»¥×¥È¥½¥Õ¥È¡ÊJMA standard receipt software¡Ë
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
      ************************************************************
      *    ¿ÇÎÅÊó½·ÀÁµá½ñ     COPY (CPSKT4110TMP.INC)              *
      *
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô   ÆüÉÕ        ÆâÍÆ
      * 01.00.01     Ìç´Ö     2008/11/20  ¥·¥¹´É¡Ö2007¡×¸øÈñ¤Þ¤È¤áÂÐ±þ
      * 01.00.02     ÇëÌî     2019/02/07  ½¤ÀµÍúÎò¥³¥á¥ó¥Èºï½ü
      ************************************************************
       01  SKT4110TMP-REC.
           03  SKT4110TMP-KEY.
               05  SKT4110TMP-SRYYM                 PIC  X(06).
               05  SKT4110TMP-SYUTURYOKU            PIC  9(01).
               05  SKT4110TMP-HKNJANUM              PIC  X(06).
               05  SKT4110TMP-ZAITAKU               PIC  X(01).
               05  SKT4110TMP-KYURATE               PIC  9(02).
      *
           03  SKT4110TMP-PAGE-FLG1                 PIC  9(01).
           03  SKT4110TMP-PAGE-FLG2                 PIC  9(01).
           03  SKT4110TMP-PAGE-FLG3                 PIC  9(01).
      *ÀÁµá¥Æ¡¼¥Ö¥ë
           03  SKT4110TMP-HKN-AREA  OCCURS  11.
               05  SKT4110TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT4110TMP-KENSU             PIC  9(05).
                   07  SKT4110TMP-NISSU             PIC  9(05).
                   07  SKT4110TMP-TENSU             PIC  9(09).
                   07  SKT4110TMP-YKZFTN            PIC  9(07).
                   07  SKT4110TMP-ITBFTN            PIC  9(09).
               05  SKT4110TMP-S-KENSU               PIC  9(05).
               05  SKT4110TMP-S-NISSU               PIC  9(05).
               05  SKT4110TMP-S-KINGK               PIC  9(09).
               05  SKT4110TMP-S-FTNGK               PIC  9(09).
      *¸øÈñÀÁµá¥Æ¡¼¥Ö¥ë
           03  SKT4110TMP-KOH-AREA  OCCURS  20.
               05  SKT4110TMP-KOHNUM                PIC  X(03).
               05  SKT4110TMP-KOHNAME               PIC  X(10).
               05  SKT4110TMP-KOH-NYUGAI-TBL OCCURS 2.
                   07  SKT4110TMP-KKENSU            PIC  9(05).
                   07  SKT4110TMP-KNISSU            PIC  9(05).
                   07  SKT4110TMP-KTENSU            PIC  9(09).
                   07  SKT4110TMP-KYKZFTN           PIC  9(07).
                   07  SKT4110TMP-KITBFTN           PIC  9(09).
                   07  SKT4110TMP-KKANFTN           PIC  9(09).
               05  SKT4110TMP-S-KKENSU              PIC  9(05).
               05  SKT4110TMP-S-KNISSU              PIC  9(05).
               05  SKT4110TMP-S-KKINGK              PIC  9(09).
               05  SKT4110TMP-S-KFTNGK              PIC  9(09).
      *Ï·¿Í¸øÈñÀÁµá¥Æ¡¼¥Ö¥ë£±
           03  SKT4110TMP-R1-KOH-AREA  OCCURS  7.
               05  SKT4110TMP-R1-KOH-NYUGAI-TBL OCCURS 2.
                   07  SKT4110TMP-R1-KKENSU            PIC  9(05).
                   07  SKT4110TMP-R1-KNISSU            PIC  9(05).
                   07  SKT4110TMP-R1-KTENSU            PIC  9(09).
                   07  SKT4110TMP-R1-KYKZFTN           PIC  9(07).
                   07  SKT4110TMP-R1-KITBFTN           PIC  9(09).
                   07  SKT4110TMP-R1-KKOUJO            PIC  9(09).
               05  SKT4110TMP-SR1-KKENSU              PIC  9(05).
               05  SKT4110TMP-SR1-KNISSU              PIC  9(05).
               05  SKT4110TMP-SR1-KKINGK              PIC  9(09).
               05  SKT4110TMP-SR1-KFTNGK              PIC  9(09).
      *Ï·¿Í¸øÈñÀÁµá¥Æ¡¼¥Ö¥ë£²
           03  SKT4110TMP-R2-KOH-AREA  OCCURS  13.
               05  SKT4110TMP-R2-KOHNUM                PIC  X(03).
               05  SKT4110TMP-R2-KOHNAME               PIC  X(10).
               05  SKT4110TMP-R2-KOH-NYUGAI-TBL OCCURS 2.
                   07  SKT4110TMP-R2-KKENSU            PIC  9(05).
                   07  SKT4110TMP-R2-KNISSU            PIC  9(05).
                   07  SKT4110TMP-R2-KTENSU            PIC  9(09).
                   07  SKT4110TMP-R2-KYKZFTN           PIC  9(07).
                   07  SKT4110TMP-R2-KITBFTN           PIC  9(09).
                   07  SKT4110TMP-R2-KKOUJO            PIC  9(09).
               05  SKT4110TMP-SR2-KKENSU              PIC  9(05).
               05  SKT4110TMP-SR2-KNISSU              PIC  9(05).
               05  SKT4110TMP-SR2-KKINGK              PIC  9(09).
               05  SKT4110TMP-SR2-KFTNGK              PIC  9(09).
      *
           03  SKT4110TMP-TYOUKI                    PIC  9(04).
