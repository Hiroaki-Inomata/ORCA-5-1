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
      *    ¿ÇÎÅÊó½·ÀÁµá½ñ     COPY (CPSKT3750TMP.INC)              *
      *
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      * 01.00.01     Ìç´Ö         08/10/29  ¥·¥¹´É¡Ö2007¡×¸øÈñ¤Þ¤È¤áÂÐ±þ
      * 01.00.02     µÈÀî         09/08/26  Ãæ¹ñ¶å½£ËÌÉô¹ë±«ÂÐ±þ
      * 01.00.03     Ìç´Ö         19/01/10  ½¤ÀµÍúÎò¥³¥á¥ó¥Èºï½ü
      ************************************************************
       01  SKT4010TMP-REC.
           03  SKT4010TMP-KEY.
               05  SKT4010TMP-SYUTURYOKU            PIC  9(01).
               05  SKT4010TMP-HKNJANUM              PIC  X(06).
               05  SKT4010TMP-ZAITAKU               PIC  X(01).
               05  SKT4010TMP-GENMEN                PIC  X(01).
      *
           03  SKT4010TMP-PAGE1-FLG                 PIC  9(01).
           03  SKT4010TMP-PAGE2-FLG                 PIC  9(01).
           03  SKT4010TMP-PAGE3-FLG                 PIC  9(01).
      *
           03  SKT4010TMP-HKN-AREA  OCCURS  11.
               05  SKT4010TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT4010TMP-KENSU             PIC  9(05).
                   07  SKT4010TMP-NISSU             PIC  9(05).
                   07  SKT4010TMP-TENSU             PIC  9(09).
                   07  SKT4010TMP-YKZFTN            PIC  9(09).
                   07  SKT4010TMP-ITBFTN            PIC  9(09).
               05  SKT4010TMP-S-KENSU               PIC  9(05).
               05  SKT4010TMP-S-NISSU               PIC  9(05).
               05  SKT4010TMP-S-KINGK               PIC  9(13).
               05  SKT4010TMP-S-FTNGK               PIC  9(09).
           03  SKT4010TMP-KOH-AREA  OCCURS  20.
               05  SKT4010TMP-KOHNUM                PIC  X(03).
               05  SKT4010TMP-KOHNAME               PIC  X(10).
               05  SKT4010TMP-KOH-NYUGAI-TBL OCCURS 2.
                   07  SKT4010TMP-KKENSU            PIC  9(05).
                   07  SKT4010TMP-KNISSU            PIC  9(05).
                   07  SKT4010TMP-KTENSU            PIC  9(09).
                   07  SKT4010TMP-KYKZFTN           PIC  9(09).
                   07  SKT4010TMP-KITBFTN           PIC  9(09).
                   07  SKT4010TMP-KPTFTN            PIC  9(09).
               05  SKT4010TMP-S-KKENSU              PIC  9(05).
               05  SKT4010TMP-S-KNISSU              PIC  9(05).
               05  SKT4010TMP-S-KKINGK              PIC  9(13).
               05  SKT4010TMP-S-KFTNGK              PIC  9(09).
           03  SKT4010TMP-KOH2-AREA  OCCURS  20.
               05  SKT4010TMP-KOHNUM2                PIC  X(03).
               05  SKT4010TMP-KOH2-NYUGAI-TBL OCCURS 2.
                   07  SKT4010TMP-KKENSU2            PIC  9(05).
                   07  SKT4010TMP-KNISSU2            PIC  9(05).
                   07  SKT4010TMP-KTENSU2            PIC  9(09).
                   07  SKT4010TMP-KYKZFTN2           PIC  9(09).
                   07  SKT4010TMP-KITBFTN2           PIC  9(09).
                   07  SKT4010TMP-KPTFTN2            PIC  9(09).
               05  SKT4010TMP-S-KKENSU2              PIC  9(05).
               05  SKT4010TMP-S-KNISSU2              PIC  9(05).
               05  SKT4010TMP-S-KKINGK2              PIC  9(13).
               05  SKT4010TMP-S-KFTNGK2              PIC  9(09).
           03  SKT4010TMP-GENBAKU-AREA.
               05  SKT4010TMP-KOH-NYUGAI-TBL OCCURS 2.
                   07  SKT4010TMP-GKENSU            PIC  9(05).
                   07  SKT4010TMP-GNISSU            PIC  9(05).
                   07  SKT4010TMP-GTENSU            PIC  9(09).
                   07  SKT4010TMP-GYKZFTN           PIC  9(09).
                   07  SKT4010TMP-GITBFTN           PIC  9(09).
                   07  SKT4010TMP-GPTFTN            PIC  9(09).
               05  SKT4010TMP-S-GKENSU              PIC  9(05).
               05  SKT4010TMP-S-GNISSU              PIC  9(05).
               05  SKT4010TMP-S-GKINGK              PIC  9(13).
               05  SKT4010TMP-S-GFTNGK              PIC  9(09).
