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
      *    ¿ÇÎÅÊó½·ÀÁµá½ñ     COPY (CPSKT3410TMP.INC)              *
      *
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01   ÆâÅÄ    06/11/22  Ä¢É¼ÍÍ¼°¤ÎÊÑ¹¹
      *  01.00.02   µÈÀî    10/08/24  Ãæ¹ñÃÏÊý¹ë±«ÂÐ±þ¡ÊÂÐ¾Ý¼ÔÊÌ½¸·×¡Ë
      *  01.00.03   °æ¾å    19/01/10  ½¤ÀµÍúÎò¥³¥á¥ó¥Èºï½ü
      ************************************************************
       01  SKT3410TMP-REC.
           03  SKT3410TMP-KEY.
               05  SKT3410TMP-SYUTURYOKU            PIC  9(01).
               05  SKT3410TMP-KENKBN                PIC  X(01).
               05  SKT3410TMP-HKNJANUM              PIC  X(06).
               05  SKT3410TMP-GENMEN                PIC  X(01).
           03  SKT3410TMP-HKN-AREA  OCCURS  11.
               05  SKT3410TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT3410TMP-KENSU             PIC  9(05).
                   07  SKT3410TMP-NISSU             PIC  9(05).
                   07  SKT3410TMP-TENSU             PIC  9(11).
                   07  SKT3410TMP-YKZFTN            PIC  9(09).
                   07  SKT3410TMP-ITBFTN            PIC  9(07).
                   07  SKT3410TMP-GENBAKU           PIC  9(03).
               05  SKT3410TMP-S-KENSU               PIC  9(05).
               05  SKT3410TMP-S-NISSU               PIC  9(05).
               05  SKT3410TMP-S-KINGK               PIC  9(11).
               05  SKT3410TMP-S-FTNGK               PIC  9(09).
      *¸¶Çú
           03  SKT3410TMP-GKOH-AREA.
               05  SKT3410TMP-GNYUGAI-TBL  OCCURS  2.
                   07  SKT3410TMP-GKENSU            PIC  9(05).
                   07  SKT3410TMP-GNISSU            PIC  9(05).
                   07  SKT3410TMP-GTENSU            PIC  9(11).
                   07  SKT3410TMP-GITBFTN           PIC  9(09).
               05  SKT3410TMP-S-GKENSU              PIC  9(05).
               05  SKT3410TMP-S-GNISSU              PIC  9(05).
               05  SKT3410TMP-S-GKINGK              PIC  9(11).
               05  SKT3410TMP-S-GFTNGK              PIC  9(09).
      *°ìÈÌ¡¦Âà¿¦ ¡Ü ¸øÈñ
           03  SKT3410TMP-KOH-TBL.
               05  SKT3410TMP-KOH-AREA  OCCURS  20.
                   07  SKT3410TMP-KOHNUM                PIC  X(03).
                   07  SKT3410TMP-KNYUGAI-TBL  OCCURS  2.
                     09  SKT3410TMP-KKENSU            PIC  9(05).
                     09  SKT3410TMP-KNISSU            PIC  9(05).
                     09  SKT3410TMP-KTENSU            PIC  9(11).
                     09  SKT3410TMP-KITBFTN           PIC  9(07).
                   07  SKT3410TMP-S-KKENSU            PIC  9(05).
                   07  SKT3410TMP-S-KNISSU            PIC  9(05).
                   07  SKT3410TMP-S-KKINGK            PIC  9(11).
                   07  SKT3410TMP-S-KFTNGK            PIC  9(09).
      *    »ÈÍÑ¤·¤Ê¤¤
      *Ï·¿Í ¡Ü ¸øÈñ
           03  SKT3410TMP-RKOH-TBL.
               05  SKT3410TMP-RKOH-AREA  OCCURS  20.
                   07  SKT3410TMP-RKOHNUM                PIC  X(03).
                   07  SKT3410TMP-RKNYUGAI-TBL  OCCURS  2.
                     09  SKT3410TMP-RKKENSU             PIC  9(05).
                     09  SKT3410TMP-RKNISSU             PIC  9(05).
                     09  SKT3410TMP-RKTENSU             PIC  9(11).
                     09  SKT3410TMP-RKITBFTN            PIC  9(07).
                   07  SKT3410TMP-S-RKKENSU             PIC  9(05).
                   07  SKT3410TMP-S-RKNISSU             PIC  9(05).
                   07  SKT3410TMP-S-RKKINGK             PIC  9(11).
                   07  SKT3410TMP-S-RKFTNGK             PIC  9(09).
      *Ï·¿Í ¡Ü ¸¶Çú
           03  SKT3410TMP-GRKOH-AREA.
               05  SKT3410TMP-GRNYUGAI-TBL  OCCURS  2.
                   07  SKT3410TMP-GRKENSU             PIC  9(05).
      *Ï·¿Í ¡Ü ½ÅÅÙ¿´¿È¾ã³²
           03  SKT3410TMP-JRKOH-AREA.
               05  SKT3410TMP-JRNYUGAI-TBL  OCCURS  2.
                   07  SKT3410TMP-JRKENSU             PIC  9(05).
      *°ìÈÌ¡¦Âà¿¦ ¡Ü ¸¶Çú
           03  SKT3410TMP-GKOH-AREA.
               05  SKT3410TMP-GKNYUGAI-TBL  OCCURS  2.
                   07  SKT3410TMP-GKKENSU             PIC  9(04).
                   07  SKT3410TMP-GKNISSU             PIC  9(04).
                   07  SKT3410TMP-GKTENSU             PIC  9(07).
                   07  SKT3410TMP-GKITBFTN            PIC  9(07).
               05  SKT3410TMP-S-GKKENSU               PIC  9(04).
               05  SKT3410TMP-S-GKNISSU               PIC  9(04).
               05  SKT3410TMP-S-GKKINGK               PIC  9(07).
               05  SKT3410TMP-S-GKFTNGK               PIC  9(07).
      *°ìÈÌ¡¦Âà¿¦ ¡Ü ·ë³Ë£³£´¾ò
           03  SKT3410TMP-34KOH-AREA.
               05  SKT3410TMP-34NYUGAI-TBL  OCCURS  2.
                   07  SKT3410TMP-34KENSU             PIC  9(05).
                   07  SKT3410TMP-34NISSU             PIC  9(05).
                   07  SKT3410TMP-34TENSU             PIC  9(11).
               05  SKT3410TMP-S-34KENSU               PIC  9(05).
               05  SKT3410TMP-S-34NISSU               PIC  9(05).
      *°ìÈÌ¡¦Âà¿¦ ¡Ü ·ë³Ë£³£µ¾ò
           03  SKT3410TMP-35KOH-AREA.
               05  SKT3410TMP-35KENSU                 PIC  9(05).
               05  SKT3410TMP-35NISSU                 PIC  9(05).
               05  SKT3410TMP-35TENSU                 PIC  9(11).
               05  SKT3410TMP-35ITBFTN                PIC  9(09).
               05  SKT3410TMP-S-35KENSU               PIC  9(05).
               05  SKT3410TMP-S-35NISSU               PIC  9(05).
               05  SKT3410TMP-S-35KINGK               PIC  9(11).
      *Ê¡»ã°åÎÅ
           03  SKT3410TMP-FKOH-AREA OCCURS 5.
               05  SKT3410TMP-FNYUGAI-TBL  OCCURS  2.
                   07  SKT3410TMP-FKENSU             PIC  9(05).
                   07  SKT3410TMP-FNISSU             PIC  9(05).
                   07  SKT3410TMP-FTENSU             PIC  9(11).
                   07  SKT3410TMP-FITBFTN            PIC  9(07).
      *    »ÈÍÑ¤·¤Ê¤¤
               05  SKT3410TMP-S-FKENSU               PIC  9(05).
               05  SKT3410TMP-S-FNISSU               PIC  9(05).
               05  SKT3410TMP-S-FKINGK               PIC  9(11).
               05  SKT3410TMP-S-FFTNGK               PIC  9(09).
