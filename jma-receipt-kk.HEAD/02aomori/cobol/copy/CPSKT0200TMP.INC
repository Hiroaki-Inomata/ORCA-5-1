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
      *    ¿ÇÎÅÊó½·Áí³çÉ½     COPY (CPSKT0200TMP.INC)            
      *
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      ********************************************************************
       01  SKT0200TMP-REC.
           03  SKT0200TMP-KEY.
      *¶èÊ¬
               05  SKT0200TMP-KBN             PIC  X(01).
      *¸ºÌÈ¶èÊ¬
               05  SKT0200TMP-GENMEN          PIC  X(01).
      *ÀÁµá¥Æ¡¼¥Ö¥ë
           03    SKT0200TMP-SEIKYU-TBL  OCCURS 15.
      *Æþ³°¥Æ¡¼¥Ö¥ë
               05  SKT0200TMP-NYUGAI-TBL  OCCURS 2.
      *·ï¿ô
                   07  SKT0200TMP-KENSU       PIC 9(05).
      *Æü¿ô
                   07  SKT0200TMP-NISSU       PIC 9(05).
      *ÅÀ¿ô
                   07  SKT0200TMP-TENSU       PIC 9(09).
      *°ìÉôÉéÃ´¶â
                   07  SKT0200TMP-ITBFTN      PIC 9(09).
      *¿©»ö·ï¿ô
               05  SKT0200TMP-S-KENSU         PIC 9(05).
      *¿©»öÆü¿ô
               05  SKT0200TMP-S-NISSU         PIC 9(05).
      *¿©»öÎÅÍÜÈñ
               05  SKT0200TMP-S-KINGAKU       PIC 9(09).
      *É¸½àÉéÃ´³Û
               05  SKT0200TMP-S-FTNGAKU       PIC 9(09).
      *¸øÈñ¥Æ¡¼¥Ö¥ë
           03  SKT0200TMP-KOH-TBL  OCCURS  20.
      *¸øÈñÈÖ¹æ
               05  SKT0200TMP-KOHNUM          PIC X(03).
      *¸øÈñÌ¾¾Î
               05  SKT0200TMP-KOHNAME         PIC X(16).
      *¸øÈñ¡ÝÆþ³°¥Æ¡¼¥Ö¥ë
               05  SKT0200TMP-KNYUGAI-TBL    OCCURS 2.
      *¸øÈñ¡Ý·ï¿ô
                   07  SKT0200TMP-KKENSU      PIC 9(05).
      *¸øÈñ¡ÝÆü¿ô
                   07  SKT0200TMP-KNISSU      PIC 9(05).
      *¸øÈñ¡ÝÅÀ¿ô
                   07  SKT0200TMP-KTENSU      PIC 9(09).
      *¸øÈñ¡Ý°ìÉôÉéÃ´¶â
                   07  SKT0200TMP-KITBFTN     PIC 9(09).
      *¸øÈñ¡Ý¿©»ö·ï¿ô
               05  SKT0200TMP-S-KKENSU        PIC 9(05).
      *¸øÈñ¡Ý¿©»öÆü¿ô
               05  SKT0200TMP-S-KNISSU        PIC 9(05).
      *¸øÈñ¡Ý¿©»öÎÅÍÜÈñ
               05  SKT0200TMP-S-KKINGAKU      PIC 9(09).
      *¸øÈñ¡ÝÉ¸½àÉéÃ´³Û
               05  SKT0200TMP-S-KFTNGAKU      PIC 9(09).
      *¹ç·×·ï¿ô
           03  SKT0200TMP-GKENSU              PIC 9(09).
      *Ï·¿Í¹ç·×·ï¿ô
           03  SKT0200TMP-RGKENSU             PIC 9(09).
