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
      *    ¿ÇÎÅÊó½·ÀÁµá½ñ     COPY (CPSKT0110TMP.INC)            *
      *
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô  ÆüÉÕ      ÆâÍÆ
      *  01.00.01    µÈÀî    08/10/20  ¥·¥¹´É¡Ö2007¡×¤Î¸øÈñ¤Þ¤È¤áÂÐ±þ
      *  01.00.02    µÈÀî    10/11/24  open-cobol1.0ÂÐ±þ
      *  01.00.03    °æ¾å    18/12/12  ½¤ÀµÍúÎò¥³¥á¥ó¥Èºï½ü
      ************************************************************
       01  SKT2530TMP-REC.
           03  SKT2530TMP-KEY.
               05  SKT2530TMP-SYUTURYOKU           PIC  9(02).
               05  SKT2530TMP-HKNJANUM             PIC  X(06).
               05  SKT2530TMP-HKNJANAME            PIC  X(16).
               05  SKT2530TMP-ZAITAKU              PIC  X(01).
               05  SKT2530TMP-KYURATE              PIC  9(03).
           03  SKT2530TMP-TBL	OCCURS  11.
               05  SKT2530TMP-NYUGAI-TBL	OCCURS  2.
                  07  SKT2530TMP-KENSU              PIC  9(05).
                  07  SKT2530TMP-NISSU              PIC  9(05).
                  07  SKT2530TMP-TENSU              PIC  9(09).
                  07  SKT2530TMP-YKZFTN             PIC  9(07).
                  07  SKT2530TMP-ITBFTN             PIC  9(07).
               05  SKT2530TMP-SKENSU                PIC  9(05).
               05  SKT2530TMP-SNISSU                PIC  9(05).
               05  SKT2530TMP-SKINGK                PIC  9(09).
               05  SKT2530TMP-SFTNGK                PIC  9(07).
           03  SKT2530TMP-FUKUSI-TBL.
               05  SKT2530TMP-FNYUGAI-TBL  OCCURS  2.
                   07  SKT2530TMP-F-KENSU            PIC  9(05).
                   07  SKT2530TMP-F-NISSU            PIC  9(05).
                   07  SKT2530TMP-F-TENSU            PIC  9(09).
                   07  SKT2530TMP-F-YKZFTN           PIC  9(09).
                   07  SKT2530TMP-F-ITBFTN           PIC  9(09).
               05  SKT2530TMP-F-SKENSU               PIC  9(05).
               05  SKT2530TMP-F-SNISSU               PIC  9(05).
               05  SKT2530TMP-F-SKINGK               PIC  9(09).
               05  SKT2530TMP-F-SFTNGK               PIC  9(09).
           03  SKT2530TMP-KOH-AREA.
               05  SKT2530TMP-KOH-TBL     OCCURS  20.
                   07  SKT2530TMP-KOHNUM                PIC  X(03).
                   07  SKT2530TMP-KOHNAME               PIC  X(12).
                   07  SKT2530TMP-KNYUGAI-TBL     OCCURS  2.
                       09  SKT2530TMP-K-KENSU        PIC  9(05).
                       09  SKT2530TMP-K-NISSU        PIC  9(05).
                       09  SKT2530TMP-K-TENSU        PIC  9(09).
                       09  SKT2530TMP-K-YKZFTN       PIC  9(09).
                       09  SKT2530TMP-K-ITBFTN       PIC  9(09).
                   07  SKT2530TMP-K-SKENSU           PIC  9(05).
                   07  SKT2530TMP-K-SNISSU           PIC  9(05).
                   07  SKT2530TMP-K-SKINGK           PIC  9(09).
                   07  SKT2530TMP-K-SFTNGK           PIC  9(09).
           03  SKT2530TMP-TYOKI-KENSU                PIC  9(04).
           03  SKT2530TMP-GENBK-KENSU                PIC  9(04).
