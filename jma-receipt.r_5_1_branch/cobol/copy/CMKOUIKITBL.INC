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
      *    ¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹çÈÖ¹æ¥Æ¡¼¥Ö¥ë   COPY (CMKOUIKITBL.INC)
      *****************************************************************
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *****************************************************************
       01  CONST-KOUIKI-AREA.
           03  FILLER              PIC X(02)   VALUE   "01".
           03  FILLER              PIC X(08)   VALUE   "39010004".
           03  FILLER              PIC X(100)  VALUE
                                   "ËÌ³¤Æ»¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "02".
           03  FILLER              PIC X(08)   VALUE   "39020003".
           03  FILLER              PIC X(100)  VALUE
                                   "ÀÄ¿¹¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "03".
           03  FILLER              PIC X(08)   VALUE   "39030002".
           03  FILLER              PIC X(100)  VALUE
                                   "´ä¼ê¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "04".
           03  FILLER              PIC X(08)   VALUE   "39040001".
           03  FILLER              PIC X(100)  VALUE
                                   "µÜ¾ë¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "05".
           03  FILLER              PIC X(08)   VALUE   "39050000".
           03  FILLER              PIC X(100)  VALUE
                                   "½©ÅÄ¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "06".
           03  FILLER              PIC X(08)   VALUE   "39060009".
           03  FILLER              PIC X(100)  VALUE
                                   "»³·Á¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "07".
           03  FILLER              PIC X(08)   VALUE   "39070008".
           03  FILLER              PIC X(100)  VALUE
                                   "Ê¡Åç¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "08".
           03  FILLER              PIC X(08)   VALUE   "39080007".
           03  FILLER              PIC X(100)  VALUE
                                   "°ñ¾ë¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "09".
           03  FILLER              PIC X(08)   VALUE   "39090006".
           03  FILLER              PIC X(100)  VALUE
                                   "ÆÊÌÚ¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "10".
           03  FILLER              PIC X(08)   VALUE   "39100003".
           03  FILLER              PIC X(100)  VALUE
                                   "·²ÇÏ¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "11".
           03  FILLER              PIC X(08)   VALUE   "39110002".
           03  FILLER              PIC X(100)  VALUE
                                   "ºë¶Ì¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "12".
           03  FILLER              PIC X(08)   VALUE   "39120001".
           03  FILLER              PIC X(100)  VALUE
                                   "ÀéÍÕ¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "13".
           03  FILLER              PIC X(08)   VALUE   "39130000".
           03  FILLER              PIC X(100)  VALUE
                                   "ÅìµþÅÔ¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "14".
           03  FILLER              PIC X(08)   VALUE   "39140009".
           03  FILLER              PIC X(100)  VALUE
                                   "¿ÀÆàÀî¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "15".
           03  FILLER              PIC X(08)   VALUE   "39150008".
           03  FILLER              PIC X(100)  VALUE
                                   "¿·³ã¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "16".
           03  FILLER              PIC X(08)   VALUE   "39160007".
           03  FILLER              PIC X(100)  VALUE
                                   "ÉÙ»³¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "17".
           03  FILLER              PIC X(08)   VALUE   "39170006".
           03  FILLER              PIC X(100)  VALUE
                                   "ÀÐÀî¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "18".
           03  FILLER              PIC X(08)   VALUE   "39180005".
           03  FILLER              PIC X(100)  VALUE
                                   "Ê¡°æ¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "19".
           03  FILLER              PIC X(08)   VALUE   "39190004".
           03  FILLER              PIC X(100)  VALUE
                                   "»³Íü¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "20".
           03  FILLER              PIC X(08)   VALUE   "39200001".
           03  FILLER              PIC X(100)  VALUE
                                   "Ä¹Ìî¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "21".
           03  FILLER              PIC X(08)   VALUE   "39210000".
           03  FILLER              PIC X(100)  VALUE
                                   "´ôÉì¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "22".
           03  FILLER              PIC X(08)   VALUE   "39220009".
           03  FILLER              PIC X(100)  VALUE
                                   "ÀÅ²¬¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "23".
           03  FILLER              PIC X(08)   VALUE   "39230008".
           03  FILLER              PIC X(100)  VALUE
                                   "°¦ÃÎ¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "24".
           03  FILLER              PIC X(08)   VALUE   "39240007".
           03  FILLER              PIC X(100)  VALUE
                                   "»°½Å¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "25".
           03  FILLER              PIC X(08)   VALUE   "39250006".
           03  FILLER              PIC X(100)  VALUE
                                   "¼¢²ì¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "26".
           03  FILLER              PIC X(08)   VALUE   "39260005".
           03  FILLER              PIC X(100)  VALUE
                                   "µþÅÔÉÜ¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "27".
           03  FILLER              PIC X(08)   VALUE   "39270004".
           03  FILLER              PIC X(100)  VALUE
                                   "ÂçºåÉÜ¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "28".
           03  FILLER              PIC X(08)   VALUE   "39280003".
           03  FILLER              PIC X(100)  VALUE
                                   "Ê¼¸Ë¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "29".
           03  FILLER              PIC X(08)   VALUE   "39290002".
           03  FILLER              PIC X(100)  VALUE
                                   "ÆàÎÉ¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "30".
           03  FILLER              PIC X(08)   VALUE   "39300009".
           03  FILLER              PIC X(100)  VALUE
                                   "ÏÂ²Î»³¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "31".
           03  FILLER              PIC X(08)   VALUE   "39310008".
           03  FILLER              PIC X(100)  VALUE
                                   "Ä»¼è¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "32".
           03  FILLER              PIC X(08)   VALUE   "39320007".
           03  FILLER              PIC X(100)  VALUE
                                   "Åçº¬¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "33".
           03  FILLER              PIC X(08)   VALUE   "39330006".
           03  FILLER              PIC X(100)  VALUE
                                   "²¬»³¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "34".
           03  FILLER              PIC X(08)   VALUE   "39340005".
           03  FILLER              PIC X(100)  VALUE
                                   "¹­Åç¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "35".
           03  FILLER              PIC X(08)   VALUE   "39350004".
           03  FILLER              PIC X(100)  VALUE
                                   "»³¸ý¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "36".
           03  FILLER              PIC X(08)   VALUE   "39360003".
           03  FILLER              PIC X(100)  VALUE
                                   "ÆÁÅç¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "37".
           03  FILLER              PIC X(08)   VALUE   "39370002".
           03  FILLER              PIC X(100)  VALUE
                                   "¹áÀî¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "38".
           03  FILLER              PIC X(08)   VALUE   "39380001".
           03  FILLER              PIC X(100)  VALUE
                                   "°¦É²¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "39".
           03  FILLER              PIC X(08)   VALUE   "39390000".
           03  FILLER              PIC X(100)  VALUE
                                   "¹âÃÎ¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "40".
           03  FILLER              PIC X(08)   VALUE   "39400007".
           03  FILLER              PIC X(100)  VALUE
                                   "Ê¡²¬¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "41".
           03  FILLER              PIC X(08)   VALUE   "39410006".
           03  FILLER              PIC X(100)  VALUE
                                   "º´²ì¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "42".
           03  FILLER              PIC X(08)   VALUE   "39420005".
           03  FILLER              PIC X(100)  VALUE
                                   "Ä¹ºê¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "43".
           03  FILLER              PIC X(08)   VALUE   "39430004".
           03  FILLER              PIC X(100)  VALUE
                                   "·§ËÜ¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "44".
           03  FILLER              PIC X(08)   VALUE   "39440003".
           03  FILLER              PIC X(100)  VALUE
                                   "ÂçÊ¬¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "45".
           03  FILLER              PIC X(08)   VALUE   "39450002".
           03  FILLER              PIC X(100)  VALUE
                                   "µÜºê¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "46".
           03  FILLER              PIC X(08)   VALUE   "39460001".
           03  FILLER              PIC X(100)  VALUE
                                   "¼¯»ùÅç¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
           03  FILLER              PIC X(02)   VALUE   "47".
           03  FILLER              PIC X(08)   VALUE   "39470000".
           03  FILLER              PIC X(100)  VALUE
                                   "²­Æì¸©¸å´ü¹âÎð¼Ô°åÎÅ¹­°èÏ¢¹ç".
      *
       01  CONST-KOUIKI-AREA-R     REDEFINES   CONST-KOUIKI-AREA.
           03  CONST-KOUIKI-OCC    OCCURS   47.
               05  CONST-KOUIKI-PREFNUM
                                   PIC X(02).
               05  CONST-KOUIKI-NUM
                                   PIC X(08).
               05  CONST-KOUIKI-NAME
                                   PIC X(100).
      *
       01  CONST-KOUIKI-MAX        PIC 9(02)   VALUE   47.

