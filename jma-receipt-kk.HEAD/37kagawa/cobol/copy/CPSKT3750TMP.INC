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
      *  01.00.01    µÈÀî         10/09/24  ¼ç²ÊÂÐ±þ
      *  01.00.02    ÇëÌî         19/01/11  ½¤ÀµÍúÎò¥³¥á¥ó¥Èºï½ü
      ************************************************************
       01  SKT3750TMP-REC.
           03  SKT3750TMP-KEY.
               05  SKT3750TMP-SYUTURYOKU            PIC  9(01).
               05  SKT3750TMP-HKNJANUM              PIC  X(06).
               05  SKT3750TMP-RECEKA                PIC  X(02).
               05  SKT3750TMP-ZAITAKU               PIC  X(01).
           03  SKT3750TMP-HKN-AREA  OCCURS  11.
               05  SKT3750TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT3750TMP-KENSU             PIC  9(05).
                   07  SKT3750TMP-NISSU             PIC  9(05).
                   07  SKT3750TMP-TENSU             PIC  9(09).
                   07  SKT3750TMP-YKZFTN            PIC  9(09).
                   07  SKT3750TMP-ITBFTN            PIC  9(09).
               05  SKT3750TMP-S-KENSU               PIC  9(05).
               05  SKT3750TMP-S-NISSU               PIC  9(05).
               05  SKT3750TMP-S-KINGK               PIC  9(13).
               05  SKT3750TMP-S-FTNGK               PIC  9(09).
           03  SKT3750TMP-KOH-AREA  OCCURS  20.
               05  SKT3750TMP-KOHNUM                PIC  X(03).
               05  SKT3750TMP-KOHNAME               PIC  X(10).
               05  SKT3750TMP-KOH-NYUGAI-TBL OCCURS 2.
                   07  SKT3750TMP-KKENSU            PIC  9(05).
                   07  SKT3750TMP-KNISSU            PIC  9(05).
                   07  SKT3750TMP-KTENSU            PIC  9(09).
                   07  SKT3750TMP-KYKZFTN           PIC  9(09).
                   07  SKT3750TMP-KITBFTN           PIC  9(09).
               05  SKT3750TMP-S-KKENSU              PIC  9(05).
               05  SKT3750TMP-S-KNISSU              PIC  9(05).
               05  SKT3750TMP-S-KKINGK              PIC  9(13).
               05  SKT3750TMP-S-KFTNGK              PIC  9(09).
