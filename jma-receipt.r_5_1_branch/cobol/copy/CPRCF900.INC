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
      *    £Ä£Ð£ÃÄ´ººÂÐ±þ     ½ç½øÈÖ¹æ¾ðÊó (CPRCF900.INC)      *
      ************************************************************
      ************************************************************
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  04.00.01    NACL-¾®Æ¦Âô  xx/xx/xx  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      ************************************************************
      *01  RECE900-REC.
           03  RECE900-KEY.
      *°åÎÅµ¡´Ø£É£Ä
               05  RECE900-HOSPNUM         PIC  9(02).
      *¿ÇÎÅÇ¯·î
               05  RECE900-SRYYM           PIC  9(06).
      *´µ¼Ô£É£Ä
               05  RECE900-PTID            PIC  9(10).
      *¿ÇÎÅ¶èÊ¬        
               05  RECE900-SRYKBN          PIC  X(02).
      *½ç½øÈÖ¹æ        
           03  RECE900-RENNUM              PIC  9(04).
