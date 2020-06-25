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
      *----------------------------------------------------------------*
      *    ¥ì¥»ÅÅ¿ÇÎÅ²Ê¥³¡¼¥É¥Æ¡¼¥Ö¥ë   COPY (CMRECEDENSRYKACD.INC)      *
      *----------------------------------------------------------------*
      *****************************************************************
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  03.03.01    NACL-Æ£¸¶    06/10/16  ¿´ÎÅÆâ²Ê¤Ë½¤Àµ
      *
      *  04.05.01    NACL-Æ£¸¶    10/03/23  Ê¿À®£²£²Ç¯£´·î²þÀµÂÐ±þ
      *****************************************************************
      *
      *    ¥ì¥»ÅÅ¿ÇÎÅ²Ê¥³¡¼¥É¥Æ¡¼¥Ö¥ë
       01  TBL-RECEDENSRYKACD-AREA.
               05  FILLER          PIC X(02)   VALUE   "01". 
               05  FILLER          PIC X(20)   VALUE   "Æâ²Ê".
               05  FILLER          PIC X(02)   VALUE   "02". 
               05  FILLER          PIC X(20)   VALUE   "Àº¿À²Ê".
               05  FILLER          PIC X(02)   VALUE   "03". 
               05  FILLER          PIC X(20)   VALUE   "¿À·Ð²Ê".
               05  FILLER          PIC X(02)   VALUE   "04". 
               05  FILLER          PIC X(20)   VALUE   "¿À·ÐÆâ²Ê".
               05  FILLER          PIC X(02)   VALUE   "05". 
               05  FILLER          PIC X(20)   VALUE   "¸ÆµÛ´ï²Ê".
               05  FILLER          PIC X(02)   VALUE   "06". 
               05  FILLER          PIC X(20)   VALUE   "¾Ã²½´ï²Ê".
               05  FILLER          PIC X(02)   VALUE   "07". 
               05  FILLER          PIC X(20)   VALUE   "°ßÄ²²Ê".
               05  FILLER          PIC X(02)   VALUE   "08". 
               05  FILLER          PIC X(20)   VALUE   "½Û´Ä´ï²Ê".
               05  FILLER          PIC X(02)   VALUE   "09". 
               05  FILLER          PIC X(20)   VALUE   "¾®»ù²Ê".
               05  FILLER          PIC X(02)   VALUE   "10". 
               05  FILLER          PIC X(20)   VALUE   "³°²Ê".
               05  FILLER          PIC X(02)   VALUE   "11". 
               05  FILLER          PIC X(20)   VALUE   "À°·Á³°²Ê".
               05  FILLER          PIC X(02)   VALUE   "12". 
               05  FILLER          PIC X(20)   VALUE   "·ÁÀ®³°²Ê".
               05  FILLER          PIC X(02)   VALUE   "13". 
               05  FILLER          PIC X(20)   VALUE   "ÈþÍÆ³°²Ê".
               05  FILLER          PIC X(02)   VALUE   "14". 
               05  FILLER          PIC X(20)   VALUE   "Ç¾¿À·Ð³°²Ê".
               05  FILLER          PIC X(02)   VALUE   "15". 
               05  FILLER          PIC X(20)   VALUE   "¸ÆµÛ´ï³°²Ê".
               05  FILLER          PIC X(02)   VALUE   "16". 
               05  FILLER          PIC X(20)   VALUE   "¿´Â¡·ì´É³°²Ê".
               05  FILLER          PIC X(02)   VALUE   "17". 
               05  FILLER          PIC X(20)   VALUE   "¾®»ù³°²Ê".
               05  FILLER          PIC X(02)   VALUE   "18". 
               05  FILLER          PIC X(20)   VALUE   "ÈéÉæÈçÇ¢´ï²Ê".
               05  FILLER          PIC X(02)   VALUE   "19". 
               05  FILLER          PIC X(20)   VALUE   "ÈéÉæ²Ê".
               05  FILLER          PIC X(02)   VALUE   "20". 
               05  FILLER          PIC X(20)   VALUE   "ÈçÇ¢´ï²Ê".
               05  FILLER          PIC X(02)   VALUE   "21". 
               05  FILLER          PIC X(20)   VALUE   "À­ÉÂ²Ê".
               05  FILLER          PIC X(02)   VALUE   "22". 
               05  FILLER          PIC X(20)   VALUE   "¤³¤¦Ìç²Ê".
               05  FILLER          PIC X(02)   VALUE   "23". 
               05  FILLER          PIC X(20)   VALUE   "»ºÉØ¿Í²Ê".
               05  FILLER          PIC X(02)   VALUE   "24". 
               05  FILLER          PIC X(20)   VALUE   "»º²Ê".
               05  FILLER          PIC X(02)   VALUE   "25". 
               05  FILLER          PIC X(20)   VALUE   "ÉØ¿Í²Ê".
               05  FILLER          PIC X(02)   VALUE   "26". 
               05  FILLER          PIC X(20)   VALUE   "´ã²Ê".
               05  FILLER          PIC X(02)   VALUE   "27". 
               05  FILLER          PIC X(20)   VALUE   "¼ªÉ¡¤¤¤ó¤³¤¦²Ê".
               05  FILLER          PIC X(02)   VALUE   "28". 
               05  FILLER          PIC X(20)   VALUE   "µ¤´É¿©Æ»²Ê".
               05  FILLER          PIC X(02)   VALUE   "30". 
               05  FILLER          PIC X(20)   VALUE   "Êü¼ÍÀþ²Ê".
               05  FILLER          PIC X(02)   VALUE   "31". 
               05  FILLER          PIC X(20)   VALUE   "Ëã¿ì²Ê".
               05  FILLER          PIC X(02)   VALUE   "33". 
               05  FILLER          PIC X(20)   VALUE   "¿´ÎÅÆâ²Ê".
               05  FILLER          PIC X(02)   VALUE   "34". 
               05  FILLER          PIC X(20)   VALUE   "¥¢¥ì¥ë¥®¡¼²Ê".
               05  FILLER          PIC X(02)   VALUE   "35". 
               05  FILLER          PIC X(20)   VALUE   "¥ê¥¦¥Þ¥Á²Ê".
               05  FILLER          PIC X(02)   VALUE   "36". 
               05  FILLER          PIC X(20)   VALUE
                                               "¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó²Ê".
               05  FILLER          PIC X(02)   VALUE   "37". 
               05  FILLER          PIC X(20)   VALUE   "ÉÂÍý¿ÇÃÇ²Ê".
               05  FILLER          PIC X(02)   VALUE   "38". 
               05  FILLER          PIC X(20)   VALUE   "Î×¾²¸¡ºº²Ê".
               05  FILLER          PIC X(02)   VALUE   "39". 
               05  FILLER          PIC X(20)   VALUE   "µßµÞ²Ê".
      *
       01  TBL-RECEDENSRYKACD-AREA-R   REDEFINES
                                               TBL-RECEDENSRYKACD-AREA.
           03  TBL-RECEDENSRYKACD-OCC  OCCURS      37
                                       INDEXED TBL-RECEDENSRYKACD-IDX.
               05  TBL-RECEDEN-SRYKACD
                                   PIC X(02).
               05  TBL-RECEDEN-SRYKANAME
                                   PIC X(20).
      *
       01  TBL-RECEDENSRYKACD-MAX  PIC 9(02)   VALUE   37.

